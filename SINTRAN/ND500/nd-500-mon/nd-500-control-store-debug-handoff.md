# ND-500 Control-Store / DMA Crash - Debug Handoff

Repo home: `SINTRAN/ND500/nd-500-mon/` (all repo paths below are relative to the repo root
`E:\Dev\Ronny\NDInsight`).
External source package (outside the repo - the original disk image + `.prog` original):
`/mnt/d/ND/500/ND-500(0) System Package for SINTRAN IIIVSX L/`
Emulator: RetroCore (C#), namespace `Emulated.HW.ND.*`.
Program under test: `nd-500-mon-j04.prog` ("ND-500/5000 MONITOR Version J04 88. 6.16 / 88. 8.17").

Related deliverables:
- `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.md` - monitor analysis, the `MON 60` wrapper, control-store dependency
- `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm` - full ND-100 disassembly (base 0)
- `SINTRAN/ND500/swapper/swapper-k01.pseg.md` / `SINTRAN/ND500/swapper/swapper-k01.dseg.md` - the swapper domain
Bus-interface reference (authoritative background):
- `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md`

All numbers are OCTAL when suffixed `B` or clearly ND-style; hex is prefixed `0x`.
Claims are marked FACT (read from bytes/trace/doc), COMPUTED (arithmetic shown), or
INFERRED (reasoned, not proven). Assume nothing not so marked.

---

## 1. Timeline - how we got here

1. `LOAD-SWAPPER swapper-k01` and even `STATUS` failed with `NO SUCH FILE NAME`.
   Cause (FACT, byte-traced): any operation needing the live ND-500 issues a `MON 60`;
   SINTRAN's level-12 driver reads the 3022 interface STATUS and returns `ECSLOAD`
   ("CONTROL STORE MUST BE LOADED", `002032B`); the monitor's `MON 60` wrapper at
   `146256B` catches that at `146263B` and auto-loads its default control-store file
   `(SYSTEM)CONTROL-STORE:DATA` (string at file offset `0x32950` in `nd-500-mon-j04.prog`)
   via the loader at `177152B`. The file was absent -> SINTRAN `NO SUCH FILE NAME`.
2. A `CONTROL-STORE:DATA` was copied in from another SCSI disk. The control store now
   loads far enough that the ND-500 microclock runs and the microcode executes.
3. `STATUS` now crashes the emulator (section 2) instead of failing the file open. The
   failure moved from "no microcode" to "microcode runs and does a DMA the emulator
   cannot service" - i.e. forward progress.

---

## 2. The crash (verbatim)

```
N500: status

> Loading Control StoreUnexpected ND100 CPU exception: System.Exception: Unmapped memory
   at Emulated.Machines.ND.ND100.ND100Memory.WriteMemory32W(UInt32 address, UInt16 word, Boolean isByte)
   at Emulated.Machines.ND.ND100.ND100Memory.Emulated.HW.Common.CPU.ISystemBus.WriteMemory16(UInt32 address, UInt16 value)
   at Emulated.HW.ND.CPU.NDBUS.NDBusND500IF.WriteND100Memory(UInt32 address, UInt32 data)
   at Emulated.HW.ND.CPU.NDBUS.NDBusND500IF.ProcessTagOut(UInt16 tagValue)
   at Emulated.HW.ND.CPU.NDBUS.NDBusND500IF.Write(Int32 address, UInt16 value)
   at Emulated.HW.ND.CPU.ND100.Instructions.IOXT()
   at Emulated.HW.ND.CPU.ND100.CpuND100.ExecuteDecodedInstruction(...)
```

Read bottom-up:
- The ND-100 executed an `IOXT` (register-indirect I/O) to the 3022/5015 ND-500 interface.
- `NDBusND500IF.Write` received the I/O and dispatched to `ProcessTagOut(tagValue)`.
- `ProcessTagOut` decoded a TAG-OUT that means "write into ND-100 memory" (a DMA write
  driven by the ND-500 microcode) and called `WriteND100Memory(address, data)`.
- `ND100Memory.WriteMemory32W(address, ...)` found `address` **not mapped** -> exception.

So: the ND-500 microcode asked to DMA a word into ND-100 memory, and the target
`address` is outside the emulator's mapped ND-100 physical memory.

---

## 3. Why the ND-500 writes ND-100 memory here (mechanism)

FACT (from `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md`, sections on MAR / TAG-OUT / activate):

- The ND-100<->ND-500 mailbox is a DMA channel. The ND-500 microcode initiates and
  controls DMA to/from ND-100 (I/O-processor) memory through the interface.
- The **MAR (Memory Address Register)** in the interface holds the ND-100 address the
  DMA targets. MAR is loaded two-step over the 16-bit bus: **most-significant part
  first, least-significant part read first**; "MAR bits 24-31 mirror bits 8-15."
- **TAG-OUT** codes are driven by the 5015 (ND-500 side) to the 3022 (ND-100 side):
  code 6 = "read DATA register (and ND-100 memory)", code 7 = "write DATA register
  (and then into ND-100 memory)". The crash is on the write path -> a TAG-OUT
  write-to-ND-100-memory (INFERRED: code 7 or the emulator's equivalent).
- Activation sequence `XACT500` (`MP-P2-N500.NPL`): before starting the ND-500 the driver
  writes MAR = message-bank : message-address via `LMAR5`, then CONTROL := activate.
  On activate the ND-500 microcode fetches the message buffer by DMA through MAR, does
  the work, writes the answer fields back by DMA, and raises the level-12 interrupt.

So during `STATUS` the monitor activates the ND-500 with a message; the microcode DMAs
the message buffer (and/or writes an answer) into ND-100 memory at the MAR-derived
address. That address is what came out unmapped.

---

## 4. What the target address most likely is

FACT - the ND-500 memory configuration on this system (from the `MEM-CONF` output):

```
PART      WIDTH        N100   N500P  N500D
  0B      0B-  7777B    Y      Y      Y

                        PAGE          WORD          BYTE
                   ND-100  ND-500   ND-100        ND-500
ND-500 address 0:  004100  000000   00010200000   00000000000
Register block:    004212  000112   00010424000   00000450000
Phys segment tbl:  004252  000152   00010524000   00000650000
WIP/PGU table:     004211  000111   00010422000   00000444000
```

COMPUTED - ND-100 page -> physical word/byte address (1 ND-100 page = 1024 words = 2048 bytes):

| Structure | ND-100 page | word address (page*1024) | byte address |
|-----------|-------------|--------------------------|--------------|
| ND-500 address 0  | `004100B` = 2112 | 2112*1024 = 2162688 = `0x210000` | `0x420000` |
| WIP/PGU table     | `004211B` = 2185 | 2185*1024 = 2237440 = `0x222400` | `0x444800` |
| Register block    | `004212B` = 2186 | 2186*1024 = 2238464 = `0x222800` | `0x445000` |
| Phys segment tbl  | `004252B` = 2218 | 2218*1024 = 2271232 = `0x22A400` | `0x454800` |

(Cross-check: the monitor's own "ND-100 WORD" column reads `00010200000B` for ND-500
address 0. `00010200000B` = `0x210000` word - matches the page arithmetic exactly. FACT.)

INFERRED: the DMA the microcode attempted lands in this region (ND-500 window / register
block / mailbox), i.e. an ND-100 physical **word address at or above `0x210000`**
(byte `0x420000`, ~4.3 MB in). If the emulator's ND-100 physical memory is smaller than
that, the write is necessarily "Unmapped memory".

---

## 5. Hypotheses to test in the debug session (ranked)

### H1 - ND-100 physical RAM is too small to cover the ND-500 window (most likely)
The ND-500 window / register block sit at ND-100 physical word `0x210000`+ (COMPUTED
above). If RetroCore maps fewer ND-100 pages than that, any mailbox DMA is unmapped.
- Check: what is the emulator's configured ND-100 physical memory size (pages/words)?
- Check: is it >= `004252B`+ pages (i.e. covers word `0x22A400`+)?
- Fix if confirmed: increase the emulated ND-100 physical memory so the pages named in
  `MEM-CONF` are backed, OR ensure `DEFINE-MEMORY-CONFIGURATION` on this system points
  the ND-500 window at ND-100 pages that are actually mapped. Note `MEM-CONF` here was
  set with "ND-100 page number for ND-500 physical address 0" = `004100B`; if that page
  is beyond real RAM, that is the misconfiguration.

### H2 - MAR assembly in NDBusND500IF is wrong (address is garbage, not just high)
The interface loads MAR in two 16-bit halves, MS first, with the documented mirror
(bits 24-31 mirror 8-15). If `NDBusND500IF` assembles the two `LMAR5` writes in the
wrong order, or masks/shifts the halves wrong, the DMA address is corrupt.
- Check: log the full MAR value at entry to `ProcessTagOut` and compare to what the two
  preceding `LMAR5` writes carried.
- Check: the byte address passed to `WriteND100Memory` / `WriteMemory32W` vs the MAR:
  is it MAR verbatim, MAR<<1 (word->byte), or a mis-shift?
- Reference for correct semantics: `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` sections 3.2
  (register offsets), MAR two-step access, and 5 (`XACT500` LMAR5 writes).

### H3 - message-buffer bank (5MBBANK) mismatch
The mailbox lives in SINTRAN resident; the driver computes `5MBBANK` from `5FPMAILBOX`.
If the emulator's resident image / bank base differs from what the loaded microcode+driver
expect, MAR points somewhere valid-looking but wrong.
- Check: value written to MAR MS part (the bank) vs the resident mailbox physical bank.

### H4 - TAG-OUT decode picked the write path incorrectly
`ProcessTagOut(tagValue)` may be mis-decoding the 3-bit TAG-OUT code (0..7) so a
read-MAR / read-STATUS tag is handled as "write ND-100 memory".
- Check: the raw `tagValue` at the crash, decode against the TAG-OUT table:
  0=read MAR, 1=write MAR, 2=read STATUS, 3=write STATUS, 4=read CONTROL,
  5=reset activate, 6=read DATA(+ND-100 mem), 7=write DATA(+ND-100 mem). Bit 3 = ND-100
  when 0; bit 7 = MOST bit. (Source: `SINTRAN/ND500/ND500-BUS-INTERFACE-REFERENCE.md` section 10.2.)

---

## 6. Concrete breakpoints / instrumentation for the session

In RetroCore:
- Break in `Emulated.HW.ND.CPU.NDBUS.NDBusND500IF.ProcessTagOut`. Log: `tagValue` (and
  its decoded TAG-OUT code), the current MAR, and the `address` about to be written.
- Break in `NDBusND500IF.WriteND100Memory(address, data)`. Log `address` (hex) and
  compare to the `MEM-CONF` word addresses in section 4.
- Break in `ND100Memory.WriteMemory32W`. Log the mapped-memory bounds so you can see by
  how much `address` overshoots.
- Watch the two `LMAR5` writes that precede activation (the `XACT500` path) to capture
  the intended MAR, then confirm `ProcessTagOut` uses the same value.

Also available for a native debug session on the ND-100 side: the DAP debugger MCP tools
(`debug_read_memory`, `debug_disassemble`, breakpoints) against nd100x, if you want to
watch SINTRAN's driver set up MAR / the mailbox before activation.

---

## 7. Decision the trace already settles

- The control store IS now loading and running (the crash is downstream of a running
  microengine). FACT.
- The remaining fault is on the **ND-100 memory side of the DMA**, in the emulator's
  interface model or its ND-100 memory map - NOT in `nd-500-mon-j04.prog` and NOT a
  missing-file problem anymore. FACT (from the stack trace: the exception is
  `ND100Memory ... Unmapped memory` reached through `NDBusND500IF.WriteND100Memory`).
- Start with H1 (RAM size vs the `0x210000`+ ND-500 window). It is the cheapest to
  confirm and the `MEM-CONF` numbers point straight at it.

---

## 8. Open / unverified

- Which TAG-OUT code actually fired (need the `tagValue` at the crash) - H4.
- The exact MAR value and whether the emulator's MAR two-step assembly matches the
  documented MS-first / bit-mirror rule - H2.
- Whether `CONTROL-STORE:DATA` copied from the other SCSI disk is the correct microcode
  for this CPU, or merely close enough to start the microclock. Run
  `COMPARE-CONTROL-STORE` against it once STATUS works; a clean 0-fault compare confirms
  it. (The `MICRO-5800-*.DATA` candidate is ND-5800/SAMSON, 128-bit, 16384 microwords -
  matches the loader's 40000B word count but CPU-family fit is still unconfirmed.)
