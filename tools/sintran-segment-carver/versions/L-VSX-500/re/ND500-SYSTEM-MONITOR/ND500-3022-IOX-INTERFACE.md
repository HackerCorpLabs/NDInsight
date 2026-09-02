# ND-500 3022 IOX interface driver (the real bus interface)

The low-level ND-100 <-> ND-500 hardware interface: the routines in `030-S3SM5` at `051023B`-`052070B`
that access the **3022 interface device via `IOXT`**. This is the actual bus interface the emulator
must implement (Phase 3). Disassembly: [`ND500-3022-IOX-DRIVER.ASM`](ND500-3022-IOX-DRIVER.ASM).

## This confirms Q6 (byte-level)
The ND-500 register/hardware access is **`IOXT` in the resident SINTRAN system monitor** (`030-S3SM5`),
NOT in the `nd-500-mon:prog` caller (which had zero `IOXT`). The whole `051xxx` region is a block of
IOX accesses to the interface device. Q6 answered from bytes: register access is confined to resident
SINTRAN.

**Distinction from the fabricated "TAG protocol":** `WRTAG` here is a REAL hardware **tag register**
write via `IOXT` (`dev+offset`). It is NOT the fabricated "TAG code protocol" (message codes 8/9/16 =
MonitorCall/PageFault/OperationComplete) that must be deleted from `NDBusND500IF.cs` - that protocol
does not exist. `WRTAG` is a low-level register, a different thing.

## The access pattern (VERIFIED from bytes)
Every 3022 access is:
```
LDX ,B -56        ; X := the interface descriptor (the current ND-500 CPU-DF)
LDT ,X -3         ; T := the IOX DEVICE NUMBER (stored at descriptor offset -3)
AAT <offset>      ; T := device + register offset
[SAA <value>]     ; A := value to write (for writes)
IOXT              ; do the IOX (read into A, or write A) on device T
```
So the interface is a set of IOX registers at `<device> + <offset>`, with `<device>` read from the
per-CPU interface descriptor (offset `-3`). This means the emulator's ND-500 interface is the IOX
device whose number sits at `CPU-DF[-3]`.

## Driver routines (VERIFIED - names from N500-SYMBOLS, at these addresses in 030-S3SM5)
| routine | addr | purpose (from name; body VERIFIED) |
|---------|------|-------------------------------------|
| `WADR`  | `051023B` | write address to the interface |
| `WRDAT` | `051032B` | write data (writes a sequence of registers: see below) |
| `RDATL` | `051046B` | read data (low) |
| `REDAT` | `051052B` | read data |
| `WRTAG` | `051116B` | write tag register |
| `GETMA` | `051151B` | get magic / status |
| `LOW5W` `RLPAG` `RELPA` `RLSWM` `RELAA` `RELAL` `PTOS3` `RELCB` `RELFT` `TRESP` `TSTPW` `TSTPO` `CHKST` `TSBCL` | `051164B`.. | page/release/test-present/check-status helpers |

## IOX register map (VERIFIED both from the S3SM5 driver bytes AND the hardware manual)

**Cross-validation complete (2026-07-15):** the offsets this driver uses (the `AAT <n>` before each
`IOXT`) map **exactly** onto the register table in
`SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` section 3.2 (which came independently from the
TMP/dossier hardware manuals). Two independent sources - the actual system-monitor driver code and the
hardware manual - agree on every register. This byte-validates that doc.

| offset | register (SYM) | function | driver use |
|--------|----------------|----------|------------|
| `dev+2` | **RSTA5** | Read STATUS | `RSTAT` interface-status read (the control-store-gate poll) |
| `dev+5` | **LCON5** | Load CONTROL | control writes (most-used write) |
| `dev+6` | **MCLR5** | Master Clear strobe (restarts ND-500 microcode at control-store addr 0) | `5MCLE` |
| `dev+11` | **LTAG5** | Write TAG-OUT | `WRTAG` |
| `dev+13` | **LLOW5** ("WDAT") | Write DATAX / lower limit | `WADR` / `WRDAT` |
| `dev+14` | **SLOC5** | Set locked | lock the interface |
| `dev+15` | **CLKD5** | Clock DATA | `WRDAT` data strobe |
| `dev+16` | **UNLC5** | Release locked | unlock |
| `dev+17` | **RETG5** | Return tag (bit0 reverse tag bus, bit1 stop bit) | stop sequence |
| (`dev+0/1`) | RMAR5/LMAR5 | Read/Load MAR (DMA address, 2-step) | (elsewhere in driver) |
| (`dev+10`) | RTAG5/RUPP5 | Read TAG-IN / upper limit | |

Reads at `dev-2`/`dev-3`/`dev-11` are the **descriptor** fields (device number at `-3`), not the IOX
device.

**`WRTAG` = `LTAG5` = Write TAG-OUT - a real hardware register**, exactly as the bus doc says. This is
NOT the fabricated "TAG code protocol" (message codes 8/9/16); the TAG lines are register-level
strobes. Confirmed from both sides.

**Control-store gate:** `MCLR5` (`dev+6`) restarts the ND-500 microprogram at control-store address 0;
`RSTA5` (`dev+2`, read by `RSTAT`) reports interface STATUS. The "control store loaded?" gate reads
STATUS via `RSTAT`/`dev+2` - trace `RSTAT`'s bit tests + the STATUS bit definitions
(`ND500-BUS-INTERFACE-REFERENCE.md` section 4.2) for the exact bit the emulator must return.

## Emulator relevance (Phase 3 - this IS the bus interface)
- The ND-500 interface is an **IOX device** at `CPU-DF[-3]`; the emulator implements those IOX
  registers.
- **`dev+2` = status read** (via `RSTAT`) is the prime candidate for the "control store loaded?" /
  interface-status the monitor polls - i.e. the gate that hung `VERSION`. Trace `RSTAT` fully to get
  the exact status bits the emulator must return.
- `WADR`/`WRDAT`/`RDATL`/`REDAT` = the address+data path for register/memory transfer.
- **Next step (Phase 3 item 1):** cross-check this byte-verified offset map against the manual-based
  register maps in `SINTRAN\ND500\ND500-BUS-INTERFACE-REFERENCE.md` and
  `ND500-BUS-OCTOBUS-HW-INTERFACE.md` - validate/correct those docs against these bytes, and name each
  offset. This closes the "do we understand the IOX messages/registers" question (Phase 3 item 2).

Byte source: `../../segments/030-S3SM5.bin` (base `40000B`); symbols `N500-SYMBOLS`.
