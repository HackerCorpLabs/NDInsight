# ND 211185 COSMOS TCP/IP Gateway B05 - 68000 firmware reverse engineering

Date: 2026-07-30
Target: `tcp-ser-all-banks-b05-68k.bin` (Raw Binary, 68000:BE:32, base 0x00000000, length 0x80000)
Image provenance: see [README-tcp-ser-b05-image.md](../../../../Installation/Communication/Ethernet/x/stripped/README-tcp-ser-b05-image.md)
Sibling already annotated: `encos-ser-all-banks-68k.bin` (COSMOS/ENCOS firmware for the same board)

Every claim below is tagged **VERIFIED** (read directly out of the image bytes or the
disassembly) or **INFERRED** (reasoning on top of verified bytes). Nothing here is
guessed from the sibling firmware alone.

---

## 1. Symbol table

**VERIFIED.** The embedded ND linker symbol table was located and fully parsed.

Full extraction written to:
`C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Ronny-NDInsight\cf73cf04-f140-484f-b39f-f67a220d3faa\scratchpad\tcp-symbols-full.txt`

### Record layout - confirmed exactly as briefed

32 bytes per record:

| Offset | Size | Meaning |
|---|---|---|
| +0x00 | 4 | self/next pointer, increments by 0x20 |
| +0x04 | 1 | name length (1..12) |
| +0x05 | 1 | flag byte |
| +0x06 | 1 | kind: 0x02 = defined, 0xFF = marker/undefined |
| +0x07 | 1 | segment: 0x10 = CODE, 0x16 = DRAM/data, 0x11 = other |
| +0x08 | 4 | address, big-endian |
| +0x0C | 4 | zero |
| +0x10 | 12 | name, not NUL-terminated |

The briefed note that this is 4 bytes later than the ENCOS layout is **correct** - the
address field is at +0x08 here, not +0x04.

### Extent - one correction to the briefing

The briefing said "around file offset 0x7C3A0". That is the correct **start**, but the
table is far larger than a single block and does not stop where a naive walk stops.

**VERIFIED extent:**

- `0x7C3A0` - `0x7FBA0`: 448 records, contiguous, 32-byte stride
- `0x7FBA0` - `0x7FBA8`: 8 stray bytes (`43 45 B0 01 83 5B 0A 00`), a genuine 8-byte
  misalignment in the middle of the table - **not** a parse error on my side; records
  resume 32-byte aligned at 0x7FBA8
- `0x7FBA8` - `0x7FD88`: 15 further records

**Total: 463 records.** The briefing's figure of 437 is close but low; the difference is
accounted for by the marker records.

Breakdown by segment and kind:

| Segment | Kind | Count |
|---|---|---|
| CODE (0x10) | defined | 317 |
| DRAM (0x16) | defined | 134 |
| DRAM (0x16) | marker | 5 |
| OTHER (0x11) | marker | 4 |
| CODE (0x10) | marker | 3 |

316 distinct CODE addresses (one address carries two names).

### Sanity check

All 15 known-good examples from the briefing plus all 8 named TCP/AIP routines parsed to
exactly the stated values. **VERIFIED**, no exceptions:

`PIOC_NUMBE` DRAM 0x64C, `ND100_CPU` DRAM 0x64E, `REALTIME` DRAM 0xFC2,
`BUFFER_STA` DRAM 0x12F4 (marker), `BUFFER_END` DRAM 0x1A00 (marker),
`END_PIOCOS` CODE 0x4660 (marker), `INTPROTECT` CODE 0x47F4, `STARTIO` CODE 0x4814,
`INITLANCE` CODE 0x4884, `STARTMA` CODE 0x5C46, `STOPMA` CODE 0x5C6E,
`RCVCOMPLET` CODE 0x602E, `XMTRINGAPP` CODE 0x6600, `INTLANCE` CODE 0x8198,
`LNMAINIT` CODE 0x7FBC, `AIPINIT` CODE 0xC942, `SENDARP` CODE 0xCF0E,
`ARPINPUT` CODE 0xD290, `TCPINCKSUM` CODE 0xF856, `TCPIPSEND` CODE 0xF8CE,
`INITTCPCB` CODE 0xFA48, `TCPINPUT` CODE 0x10AC8, `TCPPROCESS` CODE 0x13A82.

Note `BUFFER_STA`, `BUFFER_END` and `END_PIOCOS` are **marker** records (kind 0xFF), not
defined symbols. Per the ENCOS lesson, marker records must never be applied as labels.

---

## 2. THE BANK MAP IN THE BRIEFING IS WRONG - read this before anything else

**VERIFIED, and it contradicts the briefing directly.**

The briefing describes the image as four independent 128 KB banks, with bank 0 = PIOCOS,
bank 1 = AIP, bank 2 = TCP (sparse), bank 3 = TCP + symbol table. For **code**, that is
not what the image contains.

The symbol table places every CODE symbol in **one contiguous linked address space**
running from `0x000047F4` (`INTPROTECT`) to `0x0002287A` (`MON2`). There is no second
copy of anything at 0x20000, 0x40000 or 0x60000, and there is no per-bank relocation.

CODE symbol distribution: 267 symbols below 0x20000, 50 symbols in 0x20000-0x2287A.
Nothing at or above 0x40000 at all.

Proof that the symbol addresses are **direct file offsets** into this image, not a
separate runtime space - the PLANC-MC prologue `2F 0E 2C 56 2D 4F 00 08` is present at
the exact address the symbol table names, for a symbol drawn from each of the three
subsystems the briefing said lived in different banks:

```
0x04814  STARTIO   2f 0e 2c 56 2d 4f 00 08 45 ee 00 14 2c 8a ...
0x0C942  AIPINIT   2f 0e 2c 56 2d 4f 00 08 45 ee 00 38 2c 8a ...
0x10AC8  TCPINPUT  2f 0e 2c 56 2d 4f 00 08 45 ee 00 70 2c 8a ...
```

So AIP is at ~0xBA7C-0xEB00 and TCP at ~0xF38E-0x1BCxx, both **inside what the briefing
calls bank 0**. The 128 KB boundaries are EPROM device boundaries; the linked program
simply spans them.

**INFERRED:** file 0x2287A-0x7C3A0 is data and read-only tables, not a second program.
Verified data anchors inside that span: the AIP port-name table at 0x24A5C, the AIP text
block at 0x3CCB8-0x3D400, the global message pool at 0x26EB4.

**Consequence for anyone reusing this:** do not add 0x20000/0x60000 to anything, and do
not treat banks 2 and 3 as "mostly sparse TCP". Bank 2 and most of bank 3 are data plus
the symbol table.

---

## 3. Interrupt vector table - CONFIRMED

**VERIFIED.** The hypothesis holds. 0x00000-0x003FF is a real, fully populated 68000
vector table, statically present in the image.

Evidence - the first 0x100 bytes read as plausible code pointers, all inside 0x0-0x7FFFF,
and the architecturally-significant slots carry meaningful distinct values while the
unused slots point into a dense run of 2-byte stubs (0x1F24, 0x1F26, 0x1F28 ... each
vector getting its own stub address so the stub identifies which vector fired). That
stub-run pattern is the ENCOS house pattern and is by itself strong confirmation.

| Vector | Address | Value | Meaning |
|---|---|---|---|
| 0 | 0x000 | 0x000005C8 | initial SSP |
| 1 | 0x004 | 0x00001CFE | **initial PC - the firmware entry point** |
| 2 | 0x008 | 0x0000211C | bus error |
| 3 | 0x00C | 0x00002136 | address error |
| 4 | 0x010 | 0x00001F24 | illegal instruction (stub) |
| 5 | 0x014 | 0x00001F26 | divide by zero (stub) |
| 6 | 0x018 | 0x00001F28 | CHK (stub) |
| 7 | 0x01C | 0x00001F2A | TRAPV (stub) |
| 8 | 0x020 | 0x00001F2C | privilege violation (stub) |
| 9 | 0x024 | 0x00001E38 | trace - **real handler, not a stub** |
| 10 | 0x028 | 0x00001F30 | line-A (stub) |
| 11 | 0x02C | 0x00001F32 | line-F (stub) |
| 24 | 0x060 | 0x00001F4C | spurious (stub) |
| 25-29 | 0x064-0x074 | 0x1F4E..0x1F56 | autovector level 1-5 (stubs) |
| 30 | 0x078 | 0x00001B00 | **autovector level 6 - real handler** |
| 31 | 0x07C | 0x00001DD8 | **autovector level 7 - real handler** |
| 32 | 0x080 | 0x00003498 | TRAP #0 |
| 33 | 0x084 | 0x0000215C... (`00 00 21 60`) | TRAP #1 - fault reporter |
| **34** | **0x088** | **0x00003498** | **TRAP #2 - the PIOC-OS kernel entry** |
| 35 | 0x08C | 0x00002606 | TRAP #3 |
| 42 | 0x0A8 | 0x00001E00 | TRAP #10 - real handler |

**The briefed TRAP #2 hypothesis holds. VERIFIED:** vector 34 at 0x088 = 0x00003498, and
TRAP #0 points at the same dispatcher. This is byte-identical in address to the ENCOS
firmware's `PiocOsTrap2Dispatch`, so the D0-function-code / A0-argument-block kernel ABI
carries over. I did **not** re-verify the 27-entry dispatch table contents for this
image - that is listed as open work in section 7.

**I did not hand-place the vector labels.** Per the mid-task correction, `M68kVectorTable.java`
from the toolkit does this typing properly and creates the references that expose
otherwise-unreferenced handlers. Running it is the right way to land these. The table
above is the reading, ready to check the script's output against.

---

## 4. Functions created and renamed

Starting state: 171 functions, all `FUN_*` except thunks.

**94 functions renamed to their vendor names** via one batch operation, all succeeded.
**4 new functions created and named** (`PORTOPEN` 0x2061A, `PORTCONNEC` 0x206AC,
`PONAREGIST` 0x20ACC, `PONALOOKUP` 0x20C08), with their code disassembled.

222 further CODE symbols have a vendor name but no Ghidra function yet. Per the mid-task
correction I deliberately did **not** grind through those by hand - `PlancFixFlow.java`
will carve them properly (it took ENCOS from 187 to 474 functions), and the name list is
sitting ready in `tcp-symbols-full.txt` to be applied afterwards.

The most significant renamed routines:

**PIOC-OS port / message layer (the priority target)**

| Name | Addr | What it does |
|---|---|---|
| `PORTCREATE` | 0x203C8 | create a port object |
| `PORTOPEN` | 0x2061A | open an existing port |
| `PORTCONNEC` | 0x206AC | connect to a named remote port |
| `PORTRECEIV` | 0x20702 | dequeue a message from a port |
| `PORTSEND` | 0x2081A | **enqueue a message on a port - fully decoded, section 5** |
| `PONAREGIST` | 0x20ACC | register a port under a name |
| `PONALOOKUP` | 0x20C08 | look a port up by name |
| `POMSGETMES` | 0x20D2A | **allocate a message buffer - fully decoded, section 5** |
| `POMSRELEAS` | 0x20DFA | release a message buffer back to the pool |
| `POMSREAD` / `POMSWRITE` | 0x20F9A / 0x2101E | read/write message buffer payload |
| `POWAITFORL` | 0x20316 | block waiting for a lock (event mask argument) |
| `POLKLOCK` / `POLKUNLOCK` | 0x1FFD8 / 0x200A2 | lock primitives |
| `PIOCOS` | 0x200BE | PIOC-OS module init |

**LANCE / media-access driver**

`INITLANCE` 0x4884, `STARTMA` 0x5C46, `STOPMA` 0x5C6E, `HARDWAREER` 0x5CC2,
`RCVRINGAPP` 0x5EE6, `RCVCOMPLET` 0x602E, `XMTRINGAPP` 0x6600, `XMTCOMPLET` 0x68A8,
`INTLANCE` 0x8198, `LNMAINIT` 0x7FBC, `WATCHDOGDE` 0x5E4C, `REINITRING` 0x5AB8.
These match the ENCOS firmware's driver one-for-one by name, so the ENCOS analysis of the
LANCE ring handling and the `LNMASTATIS` statistics block should transfer - **INFERRED,
not re-verified here.**

**AIP - ARPA Internet Protocol layer**

`AIPINIT` 0xC942, `SENDARP` 0xCF0E, `ROUTE` 0xD154, `ARPINPUT` 0xD290, `SNPSEND` 0xD6EE,
`FRAGMENTAN` 0xD732, `LOCALDELIV` 0xD976, `NETSEND` 0xDAA0, `REASSEMBLY` 0xDC1A,
`REMOTEDELI` 0xDF46, `NETDELIVER` 0xE494, `SNPDELIVER` 0xBA7C, `SNPRESPONS` 0xBFCE.

**TCP**

`TCPINIT` 0x138EE, `TCPPROCESS` 0x13A82, `MAINLOOP` 0x134B0, `TCPINPUT` 0x10AC8,
`TCPINCKSUM` 0xF856, `TCPIPSEND` 0xF8CE, `INITTCPCB` 0xFA48, `TCPSNDTCP` 0x18F5E,
`TCPRCVTCP` 0x19F7E, `TCPRESPOND` 0x188C8, `TCPTIMERS` 0x1B7D8, `TCPCLOSE` 0x18F24,
`UDPINPUT` 0x13484, `UDPOUTPUT` 0x1346E, plus a complete TCP state-machine family
(`TCPLICLS`, `TCPSYCLS`, `TCPCLOPN`, `TCPCL2CLW`, `TCPCLRWT`, `TCPFW1SYR`, `TCPSSSYN`,
`TCPSSSND`, `TCPSSRCV`, `TCPCLNSY`, `TCPCLSYN`, `TCPCLACT`, `TCPCLERR`) and a
per-state network-receive family (`TCPSYSNETR`, `TCPCL1NETR`, `TCPCL2NETR`, `TCPFW1NETR`,
`TCPSYRNETR`, `TCPESNETR`, `TCPFW2NETR`, `TCPCWNETR`, `TCPRWNETR`).

**Two layers the briefing never mentioned - a genuinely new finding**

- `SKP*` / `SKU*` (0x1BC9A-0x1E7xx): a socket-like session layer -
  `SKPOPENPOR`, `SKPHOMEPOR`, `SKPGETMESS`, `SKPSEND`, `SKPRECEIVE`, `SKPREAD`,
  `SKPWRITE`, `SKPWAIT`, `SKPSCHEDUL`, `SKPCANCEL`, `SKPREMOTEA`, `SKUPROCESS`,
  `SKURECEIVE`, `SKUCONVERT`, `SKUXMSGMUL`.
- `XMSG*` / `XMP*` (0x1E19C-0x1FC26): **an XMSG client inside the card**.
  `XMSGWRITE`, `XMSGREAD`, `XMSGSEND`, then a family of four-letter XMSG primitives -
  `XMPOPNM`, `XMPROUT`, `XMPFOPN`, `XMPFCLS`, `XMPFGET`, `XMPFREL`, `XMPFREA`, `XMPFWRI`,
  `XMPFSND`, `XMPFRCV`, `XMPFRRE`, `XMPFPST`, `XMPFMST`, `XMPFSCM`, `XMPFPRV`, `XMPFDMM`,
  `XMPFALM`, `XMPFM2P`, `XMPCONF`, `XMPFGSM`, `XMPBINI`, `XMPBRDY`, `XMPBAST`, `XMPXETS`,
  `XMPXRTS`, `XMPBLENGTH`.
  `SKUXMSGMUL` bridges the socket layer onto XMSG. **INFERRED:** this is how a SINTRAN
  host program talks to a TCP socket on the card - over XMSG, not over a bespoke datagram
  API. Directly relevant to the XMSG work in this repo; worth a dedicated pass.

**PLANC leaf runtime** (one copy only in this image, 0x21146-0x226E8): `PLANC_IMU` 0x22518,
`PLANC_IDV` 0x2255E, `PLANC_APPD` 0x22618, `PLANC_REMV` 0x22632, `PLANC_XRET` 0x226DA,
`#ERET` 0x226C8, `#PRERR` 0x226E8, plus `#NEW`, `#DISPOSE`, `#MOVE`, `#ENTR`, `#LEAV`.
Note this **differs from ENCOS**, which had three copies of the leaf runtime.
`MON0`/`MON1`/`MON2`/`MON64`/`MON65` at 0x22820-0x2287A close the image.

Names containing `#` were applied with the `#` replaced by `PLANC_` (Ghidra symbol rules).

---

## 5. The port / message protocol - the priority target

This is the main result. **VERIFIED** from the disassembly of `PORTSEND` (0x2081A) and
`POMSGETMES` (0x20D2A). Both routines now carry full plate comments in the Ghidra database.

### Port object layout

| Offset | Size | Field |
|---|---|---|
| +0x04 | word | **magic 0xAAAA**, validated on entry to every port operation |
| +0x06 | byte | spinlock, taken with `TAS` |
| +0x0A | byte | port type. **2 = bounded ring port**; other values = linked-list port |
| +0x12 | word | producer / write index (ring ports) |
| +0x14 | word | consumer / read index (ring ports) |
| +0x16 | 64 x long | the message-slot ring |

**Ring slot encoding:** each slot is one longword holding the message-buffer pointer with
**bit 31 as the occupied flag**. `btst #31` tests it, `bset #31` marks the slot full. The
index is advanced as `(idx + 1) AND 0x3F`, so the ring is exactly **64 entries**.

Lock contention calls `POWAITFORL` (0x20316) with mask `0x40000000` and then retries the
`TAS`, so this is a spin-with-blocking-wait, not a bare spinlock.

### Message buffer header - this is the "RB" candidate

| Offset | Size | Field |
|---|---|---|
| +0x00 | long | free-list link. Cleared on send; used as the `#REMV` link (D0 = 0) on allocate |
| +0x08 | long | reply-to / answer port |
| +0x0C | long | home (owner) port, stamped in at allocation time |
| +0x14 | word | buffer capacity |
| +0x18 | word | actual data length |

### PORTSEND parameters

PLANC-MC puts the first "further" parameter at +0x14 off A6:

| Slot | Type | Meaning |
|---|---|---|
| (0x14,A6) | long | reply-to port. If 0, defaults to the message's own home port |
| (0x18,A6) | long | destination selector. **0** -> use msg+0x0C (home port); **-1** -> use msg+0x08 (reply port); otherwise it is the destination port address itself |
| (0x1C,A6) | long | pointer to the message buffer |
| (0x20,A6) | word | data length; must be <= msg+0x14 or the call fails |

That 0 / -1 / explicit-address convention is the send-to-home / send-back-to-sender /
send-to-named-port idiom, and it is what makes request-response over these ports work
without the caller tracking who sent what.

### The message pool

**VERIFIED.** The global pool object is at absolute address **0x00026EB4**. Same header
shape as a port (magic 0xAAAA at +4, TAS lock at +6). At +0x12 it holds the head of a
**size-class list**:

| Offset | Field |
|---|---|
| +0x00 | next size class |
| +0x0E | buffer size this class provides (long) |
| +0x12 | head of this class's free list |

`POMSGETMES` walks the classes and takes the first whose size is **strictly greater** than
the requested size, pops a buffer with `#REMV`, zeroes buffer+0x00 and writes the caller's
home port into buffer+0x0C. The lock is released on every exit path including failure.

### Error code family

`0x49xx` is the PIOC-OS port-system error family. Confirmed values:

| Code | Meaning |
|---|---|
| 0x4953 | bad port or bad pool - magic word at +4 was not 0xAAAA |
| 0x4956 | no message buffer available in any size class |
| 0x495C | destination port ring full (slot bit 31 already set) |
| 0x495D | length argument exceeds the message buffer capacity |

### The AIP port names

**VERIFIED.** At file offset **0x24A5C** there is a concatenated fixed-length name table
with no separators:

```
ENMA  LNMASPcommand  LNMASPdata  LNLLSPcommand  LNLLSPdata
```

`ENMA` is the media-access service; the rest are the sub-process ports. The symbol table
independently carries `LNMASPCOMM`, `LNMASPDATA`, `LNLLSPCOMM` as DRAM symbols, which
confirms the slicing.

The matching AIP progress text block is at **0x3CCB8**:

```
Command port created.$Data port created.$Transmit port created.
LNMASPcommand$Connected to MA command port
LNMASPdata$Connected to MA data port
$AIP: collecting response RBs from transmit SP
$AIP: sending data RB no more responses.
$AIP: sending transmit RB no more responses.
$AIP: attaching to media-access
$AIP: FATAL PORT SYSTEM ERROR
$AIP: FATAL RB ERROR
$AIP: no more xmit messages
$AIP: AIPbad NUMBER of buffers in response RB
```

`$` is the PLANC text-block separator. This confirms the briefed three-port model
(command / data / transmit) **VERIFIED**, and confirms that "RB" objects are what travel
over those ports.

**INFERRED, and explicitly not proven:** an "RB" is a message buffer as laid out above,
carrying an AIP-specific payload after the 0x1C-byte header. I did not decode the payload
- see section 7.

Also at 0x3CF58, the media-access control strings including **`set DIX mode OK`** and
`BAD value`, alongside `MEDIA-ACCESS STOPPED` / `MEDIA-ACCESS RESTARTING`. So the DIX-mode
command exists in this firmware too; its command encoding was not decoded.

---

## 6. PLANC-MC idioms confirmed in this image

**VERIFIED**, and they match `../../../../tools/ghidra-planc/README.md` exactly:

- Prologue `2F 0E 2C 56 2D 4F 00 08 4? EE 00 NN 2C 8A` at every routine entry.
- Epilogue `2C 5F 24 5F 4E EA 00 02` - `jmp (2,A2)`, the **skip return**. It IS the return.
- `ON ROUTINEERROR` compiled as a `bra.b` over an inline handler placed between prologue
  and body. `AIPINIT` at 0xC942 does exactly this: `bra.b 0xC9A4` jumping over the handler
  at 0xC952.
- The error slot after a call. This image shows a clean variant worth recording: rather
  than `4E D5` (`jmp (A5)`), AIP uses a **pair of 2-byte branches**:

```
  bsr.w   TARGET
  bra.b   L_err     ; 60 02 - the ERROR slot, taken only on error return
  bra.b   L_ok      ; 60 04 - the SUCCESS path (return lands here, at retaddr+2)
L_err:
  bsr.w   LocalErrorHandler
L_ok:
```

  So the skip-return lands on the *second* branch. Testing the LENGTH of the slot rather
  than the opcode, as the toolkit README insists, is what makes this readable.

- `jmp (A5)` appears as the error unwind (e.g. at 0x20D72 inside `POMSGETMES`).

---

## 7. PHASE 2 - database state, and why bulk function creation was NOT done

**VERIFIED: `PlancFixFlow.java` had NOT been run when phase 2 started.** Two independent checks:

1. Function count was **175** - exactly the 171 phase 1 found plus the 4 phase 1 created.
   A `PlancFixFlow` run on the sibling took it from 187 to 474.
2. At 0x208EE, inside `PORTSEND`, the skip-return slot bytes `60 04` were still rendered as
   two `<undefined>` bytes with no fallthrough override - the exact artefact the script fixes.

### The prologue audit (what you asked for)

Of the **316 distinct CODE symbol addresses**, **299 sit on the exact PLANC-MC prologue**
`2F0E 2C56 2D4F 0008`. The **17 that do not are all explainable and are a finding, not an
error in the symbol table.** They fall into four groups:

| Symbol | Addr | First bytes | What it is |
|---|---|---|---|
| `INTPROTECT` | 0x047F4 | `48e7 8000 4280` | hand-written; `movem.l D0,-(SP)` |
| `INTLANCE` | 0x08198 | `4e56 ffec 2e8e` | `link A6,#-0x14` - a **NATIVE** PLANC routine (A6/A7 coincide at entry, ND-820026.1 Figure 9). This is the LANCE ISR, and per the toolkit README a NATIVE entry is compiled PLANC, not hand-written assembler |
| `POMNPROCES` | 0x09BB2 | `204f 2c7c 0002 4c6c 4fee` | **process entry point** - `movea.l A7,A0 ; movea.l #imm,A6 ; lea (d,A6),A7`. Builds its own stack; no caller ever returns into it |
| `AUTO_START` | 0x09EA0 | `204f 2c7c 0002 63f4` | process entry point |
| `TCPPROCESS` | 0x13A82 | `204f 2c7c 0003 dcf8` | process entry point |
| `#IMU` `#INSE` `#APPD` `#REMV` `#INIT` `#ENTR` `#LEAV` `#ERET` `#XRET` `#PRERR` | 0x22518-0x226E8 | various | the hand-written PLANC leaf runtime, exactly as the toolkit README describes. **Do not apply `__planc` to these.** |
| `MON65` / `#QUIT` | 0x2283E / 0x22842 | `4eba ff82` / `7000 4e42` | monitor-call stubs - see section 9 |

That is a fourth routine shape the toolkit does not currently list: the **process entry point**
(`204f 2c7c ...`). Worth adding to `PlancFixFlow`'s recognisers, since all three instances are
top-level process bodies and therefore prime disassembly seeds.

### Why I did not create the remaining ~206 functions

**I tried it, measured the result, and stopped.** I created `MACMDPORTH` at 0x6D2E and then
disassembled at that address. Ghidra reported `Disassembled 28 address units` and the function
body came back as:

```
Body: [[00006d2e, 00006d2e]]      <- ONE BYTE
```

The cause is exactly the failure mode the toolkit README documents: flow dies at the first
`jmp (A5)` (here at 0x6D48, six instructions in), so Ghidra never lays down the body. Creating
206 functions in this state would produce **206 one-byte stub functions** with wrong bodies and
wrong sizes, which is worse than leaving the addresses bare - it would litter the database and
give `PlancFixFlow` incorrect pre-existing boundaries to work around.

**The correct order is: run `PlancFixFlow.java` first, then apply the names.** The complete
name list is already extracted and sitting in `tcp-symbols-full.txt`; applying it afterwards is
one batch operation, as phase 1 demonstrated with 94/94 successes.

What I did create in phase 2 (5 functions, each one I actually needed to read):
`MACMDPORTH` 0x6D2E, `MADATAPORT` 0x7298, `LLCMDPORTH` 0x75B8, `LLDATAPORT` 0x7B78,
`PiocOsTrap2Dispatch` 0x3498, plus `STARTIO`, `LNMAINIT`, `RCVCOMPLET`, `XMTCOMPLET`.

---

## 8. THE AIP REQUEST BLOCK - decoded

**VERIFIED.** This is the phase 2 headline. Full plate comments are now on `MACMDPORTH`
(0x6D2E) and on the dispatch table at 0x24A86.

### How an RB reaches the media-access layer

`MACMDPORTH` is the server on the `LNMASPcommand` port. Its first act is:

```
move.l  (0x10,A0),(0x18,A6)    ; msg+0x10 -> the RB body pointer
```

So **the RB is not the message - it hangs off the message at msg+0x10.** That completes the
container-to-payload chain from phase 1: port ring slot -> message buffer -> `msg+0x10` -> RB.

### RB header layout

| Offset | Size | Field |
|---|---|---|
| +0x00 | word | **TYPE in bits 15..10**; bits 9..0 preserved |
| +0x02 | word | **STATUS**, signed |
| +0x06 | long | (AIP side) free-list linkage pointer |
| +0x08 | - | **argument / result area**, command specific |

### The type field and the request/response rule

The opcode extraction is:

```
move.b  (A1),D0b        ; RB[0], the high byte of the type word
lsr.w   #2,D0w
and.l   #0x3F,D0        ; opcode = (RB[0] >> 2) & 0x3F
```

and every handler ends by stamping its reply type back into the same word:

```
andi.w  #0x03FF,(A0)          ; keep the low 10 bits
ori.w   #(type << 10),(A0)
```

**VERIFIED RULE: request types are EVEN and the response type is request + 1.** I confirmed
this mechanically by scanning the whole image for the `025003ff0050` stamp pattern and reading
the immediate at each of the 54 sites; every media-access handler's stamp is exactly its table
index + 1. This also explains why all 13 odd slots in the dispatch table point at the single
reject stub 0x727C - **a response never arrives on the command port.**

The reply is written into the **same RB**, in place. An RB is its own response.

### Status codes

| Value | Meaning |
|---|---|
| 0 | OK |
| -16 (0xFFF0) | no active MA / DIX user |
| -18 (0xFFEE) | BAD value - argument failed validation |
| -20 (0xFFEC) | BAD - media access stopped |

The AIP text block prints these as `OK` / `BAD - MEDIA-ACCESS STOPPED` /
`MEDIA-ACCESS RESTARTING` / `BAD value`.

### The command set - 11 implemented, not 8

Table at **0x24A86**, 27 entries, bounded by the byte at **0x24A84 = 0x1A**. The bounds check
at 0x6D76 is **correct** - unlike the two ENCOS table over-runs, index 27+ is unreachable here.

| op | RB[0] | handler | reply type | command |
|---|---|---|---|---|
| 0 | 0x00 | 0x6D8E | 1 | attach |
| 2 | 0x08 | 0x6DF8 | 3 | statistics |
| 4 | 0x10 | 0x6E3C | 5 | change address |
| 6 | 0x18 | 0x6E8C | 7 | start |
| 8 | 0x20 | 0x6F46 | 9 | stop |
| 10 | 0x28 | 0x6FAC | 11 | activate group address |
| **12** | **0x30** | **0x7096** | **13** | **set DIX mode** |
| 14 | 0x38 | 0x7162 | 15 | DIX attach |
| 22 | 0x58 | 0x71EA | 23 | **unidentified** |
| 24 | 0x60 | 0x6FF2 | 25 | **unidentified** |
| 26 | 0x68 | 0x7050 | 27 | **unidentified** |

Opcodes 16, 18, 20 and every odd opcode are unimplemented.

**VERIFIED**: the 11 handler addresses, their reply types, and the bounds.
**INFERRED**: the eight *names*. The AIP response-type text block at 0x3CCB8 lists exactly
eight names - attach, statistics, change address, start, stop, "acitvate group address" [sic,
vendor typo], set DIX mode, DIX attach - and the first eight even opcodes are implemented
contiguously, so the pairing is ordinal. Opcodes 22/24/26 have no AIP text at all, which is
consistent with AIP not using them. **I did not find an independent confirmation of the
name-to-opcode pairing for the middle six**; see the caveat under `set DIX mode` below, which
does have independent confirmation.

### set DIX mode - the one that matters

**Request type 12 (RB[0] = 0x30), handler 0x7096.**

Independent confirmation that 0x7096 really is the DIX handler, not merely the 7th in order:
the handler tests the globals `ACTIVEMAUS` (0x2493E, "active MA users") and **`ACTIVEDIXU`
(0x24942, "active DIX users")**, and gates on `LNMAIOACTI` (0x24860, "MA I/O active"). Those
three names come from the **vendor symbol table**, not from me, and `ACTIVEDIXU` appears in
only this family of handlers. That is confirmation from a source independent of the text
ordering.

**Argument: 6 bytes at RB+0x08.**

The AIP-side builder is at **0xC6FE**. It pops an RB from `AIP_freeRbListHead` (0x3A372) with
`#REMV`, then:

```
movea.l (0x18,A6),A0            ; A0 = the RB
andi.w  #0x03FF,(A0)
ori.w   #0x3000,(A0)            ; type 12 = set DIX mode
lea     (0x3C4FE).l,A1          ; AIP_setDixModeArgument_6bytes
lea     (0x8,A0),A2             ; dest = RB+0x08
move.w  (A1)+,(A2)+             ; 2 bytes
move.l  (A1)+,(A2)+             ; 4 bytes  -> 6 bytes total
```

and `AIPINIT` (0xC942) initialises those 6 bytes at 0xC9E0-0xCA22 to **FF FF FF FF FF FF**
(loop bounds 0..5, storing `#0xFF`).

The handler validates **only bit 0 of the first byte**:

```
lea     (0x8,A1),A0
move.b  (0,A0,D2.l),D1b         ; D2 = 0, so RB[8]
and.b   #1,D1b
cmpi.b  #1,D1b
bne     -> status -18 BAD value
```

So **the DIX enable flag is RB[8] bit 0**, and AIP's all-ones pattern turns it on. On success
the handler copies all 6 bytes onward into a structure, and if `LNMAIOACTI` is nonzero it calls
`STOPMA` (0x5C6E) then `STARTMA` (0x5C46) - which is precisely the `MEDIA-ACCESS RESTARTING`
message. Changing DIX mode restarts the LANCE.

**What I could NOT establish:** the meaning of the other 5 argument bytes. Six bytes is
MAC-address-shaped and the handler copies all six into a structure at `(0x1C,A6)+4`, but I did
not trace that destination. Do not assume the remaining bytes are padding. Note also the image
is **zero** at 0x3C4FE - the FF pattern is written at run time, so a static dump shows nothing.

---

## 9. TRAP #2 dispatch - and the ENCOS comparison, measured

**VERIFIED.** Function `PiocOsTrap2Dispatch` created at 0x3498 with a full plate comment.

**ABI, verified in THIS image rather than assumed:** `D0` = function code, `A0` = argument
block, `trap #2`. The proof is the vendor's own `MON1` stub at 0x22846:

```
lea    (0x14,A6),A0        ; A0 = argument block
move.w #1,D0w              ; D0 = function code
trap   #2
tst.w  D0 / beq +2 / jmp (A5)    ; nonzero D0 = error -> PLANC unwind
```

`MON0` (aliased with `#QUIT`) at 0x22842 is the bare 4-byte gate `moveq #0,D0 ; trap #2`.
That also resolves phase 1's open item 10: the `MONn` symbols are **PIOC-OS trap wrappers**,
not SINTRAN monitor calls.

Dispatcher structure: fast paths for `D0 == 0x09` (-> 0x25B0) and `D0 == 0x1B` (-> 0x25C4)
taken *before* the table; register save `movem.l D0-D7/A0-A6,(0x30,A0)` into the current
process descriptor at **0x650**, so saved D0 is at desc+0x30 and saved A0 at desc+0x50; kernel
stack `0x666 + 0x3FE`; PLANC frame via `jsr 0x4456` with **skip distance 8** (the 8 bytes at
0x34FE are an inline frame descriptor, not code); then bounds check `0 <= code <= 0x1A` and
dispatch through the table at **0x0C6A**, **27 entries**.

### Does the numbering match ENCOS? YES - and this is measured, not assumed

I hexdumped **0x0C68-0x0CE7 from both programs** and the bytes are **identical**:

```
0c68  4f 31 00 00 2e 72 00 00  2d ec 00 00 2d 94 00 00
0c78  2e 98 00 00 30 46 00 00  30 f2 00 00 31 76 00 00
...
0cc8  1f 08 00 00 40 3a 00 00  33 48 00 00 31 50 00 00
```

Every fixed address the dispatcher uses is also identical between the two firmwares:
dispatcher 0x3498, descriptor pointer 0x650, stack limit 0x654, kernel stack 0x666+0x3FE,
frame allocator 0x4456, fast paths 0x25B0 / 0x25C4. **The two images embed the same PIOC-OS
build**, so the ENCOS function-code table (0x00 resolve-current-process ... 0x1A terminate)
applies here unchanged. This is now proven rather than inherited.

---

## 10. The in-card XMSG client

Symbols present (all CODE, all on valid prologues): `XMSGWRITE` 0x1E19C, `XMSGREAD` 0x1E220,
`XMSGSEND` 0x1E2C8, `SKUXMSGMUL` 0x1DFC4, plus 26 `XMP*` primitives 0x1ED04-0x1FC26.

**Same primitive family as ENCOS. VERIFIED by name comparison:** every one of the eight ENCOS
LOC-XMSG primitives - `XMPFOPN`, `XMPFGET`, `XMPFWRI`, `XMPFSND`, `XMPFRCV`, `XMPFREA`,
`XMPFREL`, `XMPFRRE` - is present here at a CODE address. The PIOC-OS module directory at
0x05CC also lists **`LOC-XMSG`** (dated `APRIL 21, 1986`, string at 0x0634), the same module
ENCOS carries. So the XMSG client is the shared PIOC-OS LOC-XMSG layer, not a TCP-specific
reimplementation.

This image adds 18 primitives beyond the ENCOS eight: `XMPOPNM`, `XMPROUT`, `XMPFCLS`,
`XMPFPST`, `XMPFMST`, `XMPFSCM`, `XMPFPRV`, `XMPFDMM`, `XMPFALM`, `XMPFM2P`, `XMPCONF`,
`XMPFGSM`, `XMPBINI`, `XMPBRDY`, `XMPBAST`, `XMPXETS`, `XMPXRTS`, `XMPBLENGTH`.
**INFERRED** from the names only: `XMPROUT` is routing, `XMPCONF` configuration, the `XMPB*`
group buffer management, `XMPXETS`/`XMPXRTS` transmit/receive timestamps or states. None of
these bodies were read.

### Does the card originate XMSG conversations, and under what name?

**Undetermined, and the negative evidence is worth recording.** I searched the entire 512 KB
image for `XM-`, `ENNS`, `*XM` and `XMSG` as literal strings:

- `XM-`: **zero hits**
- `ENNS`: **zero hits**
- `*XM`: **zero hits**
- `XMSG`: 5 hits, all accounted for - one is the `LOC-XMSG` module-directory record at 0x0634,
  the other four are `XMSGWRITE` / `XMSGREAD` / `XMSGSEND` / `SKUXMSGMUL` inside the **symbol
  table** itself at 0x7E273+

So **this firmware contains no XMSG service-name string at all**, in sharp contrast to ENCOS,
whose "Unknown name" bring-up problem traces to it registering `*XM-ENNS0`. Three
possibilities I cannot presently distinguish: the name is composed at run time from the PIOC
number (`PIOC_NUMBE`, 0x64C) the way ENCOS builds `ENNS<n>`; the name is supplied by the host
in the board configuration record; or this firmware only ever *answers* and never registers.
The vendor symbol `TCPPORTNAM` (two instances, 0x7B91A and 0x7BB0A) is the obvious place to
look next - it is a name buffer, and it is empty in the image, so it is filled at run time.

### An unexplained structural finding

**VERIFIED, unexplained:** ten DRAM symbols occur as **two identical-name instances at two
addresses** - `POCSINTERN`, `FROMTCPTOA`, `FROMAIPTOT`, `MYAIPEVMAS`, `NOTAIPEVMA` at both
0x78AFC.. and 0x78CEC.., and `TCPPORTNAM`, `MYPENDINGE`, `LOCKEDBYAL`, `LOCKIDLETI`,
`TCPMBSTATU` at both 0x7B91A.. and 0x7BB0A.., plus `STATICSKST` at 0x78FCA and 0x791BA. The
offsets *within* each pair match exactly (e.g. +0x04, +0x24, +0x54 in the first group). That is
the signature of **two instances of one structure**, most likely two TCP/AIP contexts or a
double-buffer. I applied them with `_A` / `_B` suffixes. **I did not determine what the two
instances are.**

---

## 11. DRAM symbols - the boundary is SETTLED, and phase 1's caution was wrong

**VERIFIED, and this reverses both the briefing's warning and my own phase 1 conclusion.**

The briefing warned that CODE (0x10) and DRAM (0x16) are separate overlapping address spaces,
citing `END_PIOCOS` CODE 0x4660 against `BUFFER_END` DRAM 0x1A00. **That is not a collision -
0x1A00 is simply below 0x4660.** There is one flat address space.

Decisive test: **of the 134 defined DRAM symbols, ZERO fall inside the code span
0x47F4-0x2287A.** The distribution is 3 below the code (0x64C, 0x64E, 0xFC2), 74 in
0x22882-0x40000, and 57 at 0x40000 and above. Not one overlap.

Corroborating evidence that these numeric addresses are exactly how the code addresses them:

- `lea (0x24A86).l,A2` at 0x6D82 reaches the MA command jump table, whose 27 entries are all
  valid code pointers and whose length matches the bound byte at 0x24A84.
- `lea (0x3C4FE).l,A1` at 0xC764 reaches the DIX argument buffer that `AIPINIT` initialises.
- `lea (0x3A372).l,A0` at 0xC718 reaches the RB free-list head.
- The low symbol `PIOC_NUMBE` 0x64C sits immediately below **0x650**, which I independently
  proved is the current-process-descriptor pointer used by the TRAP #2 dispatcher.

Some DRAM addresses read as all-zero in the image (`RCVRING` 0x24000, `POCSINTERN_B` 0x78CEC,
`AIP_setDixModeArgument_6bytes` 0x3C4FE). **That is BSS, not evidence of a wrong address** -
the linked image simply has initialised data and zero-filled data in one segment. Phase 1's
worry that "0x24000+ might not be file-resident" is answered: the region is addressed at these
exact numbers by absolute `lea` instructions, some of it is initialised and some is BSS.

**Action taken: all 132 non-duplicate DRAM symbols applied, 132/132 succeeded** (the other 2 of
134 were already applied by hand earlier as `POMS_messagePool` etc.). The 3 low ones were
applied too, since they do not collide. Names hitting the same address twice were suffixed
`_A` / `_B` as described above.

Also renamed to meaningful names rather than vendor abbreviations, where I established the
meaning: `tbl_maCommandDispatch` (0x24A86), `maCommandDispatch_maxIndex` (0x24A84),
`AIP_setDixModeArgument_6bytes` (0x3C4FE), `AIP_freeRbListHead` (0x3A372),
`POMS_messagePool` (0x26EB4).

Note on the `createLabel` hazard: `rename_symbol_batch` with `target_type: data` **replaces**
where a `DAT_*` symbol already exists and **creates** where none does. The result messages
distinguish the two. These batches must not be re-run, or the create cases will duplicate.

---

## 12. PHASE 3 - post-PlancFixFlow

### 12.1 State after the script, and a tooling problem I created

`PlancFixFlow` **worked**. Flow past `jmp (A5)` is fixed: 0x6D4A onward, which was raw
`<undefined>` bytes in phase 2, now disassembles correctly. A fresh `create_function` at
0xD6EE (`SNPSEND`) produced a proper two-range body.

But function count only moved **175 -> 185**, and the bodies of functions that already
existed are **stale**: `MACMDPORTH` is still 1 byte, `PORTSEND` still only 16 bytes (its
prologue block). **Ghidra does not recompute an existing function's body when flow changes
later**, and `create_function` on an existing entry point returns
`Function already exists` without recomputing.

**The correct recipe, learned the hard way: `disassemble` the address FIRST, then
`create_function`.** Creating first at an address whose bytes are still raw pins a
permanent 1-byte body.

**I made this mistake before diagnosing it and pinned roughly 20 bad function bodies**
(`POMNERHAND` 0x8232, `SENDERROR` 0x8334, `GETALLERRO` 0x8836, `INITERRORS` 0x8A0E,
`INITRESOUR` 0x8C6A, `ANALYZEEVE` 0x9ACE, `PLANC_OUTBYT` 0x9FF8, `SNPDELIVER` 0xBA7C,
`ARPINPUT` 0xD290, `REASSEMBLY` 0xDC1A, `REMOTEDELI` 0xDF46, `NETDELIVER` 0xE494,
`LNNDTOMAAP` 0xE5E2, `LNNDFROMMA` 0xE636, `LNEXNDFROM` 0xE70C, `LNINHEXMAA` 0xE7E0,
`LNOUTHEXMA` 0xE970, `LNPRINTMAS` 0xEAF8, `INITTCPCB` 0xFA48, `TCPINPUT` 0x10AC8,
`MAINLOOP` 0x134B0, plus `AIPINIT` 0xC942 at 16 bytes). The underlying bytes are now
disassembled, so the bodies are recoverable - but **not through the MCP surface, which
exposes no `delete_function`.** These need a `removeFunction` + re-create pass in a script.

**Recommendation: do the remaining bulk in a Ghidra script, not over MCP.** ~197 symbols
needing disassemble-then-create is roughly 400 round trips; a short script doing
`disassemble(addr); removeFunction(addr); createFunction(addr, name)` over
`tcp-symbols-full.txt` does the whole job correctly, including repairing the ~20 I pinned.

**Applied in phase 3 anyway: 34 further functions**, of which 14 got proper multi-range
bodies (`REINITRING` 2 ranges, `WATCHDOGDE` 4, `IOCOMPLETI` 5, `LNMAEVENTS` 10,
`SNPRESPONS` 5, `SENDARP` 4, `ROUTE` 3, `NETSEND` 2, `SNPSEND` 2, `CHECKSUM`,
`TCPINCKSUM` 2, `TCPIPSEND` 3).

### 12.2 The frame-offset contradiction - RESOLVED, and BOTH sources are right

**VERIFIED against the binary. This is the most portable finding in phase 3.**

The disagreement (`PlancFixFlow` says first parameter at 0x12, the toolkit README says 0x14)
is real, and neither is wrong. **The offset depends on the width of ERRCODE at +0x10, and
this image contains BOTH conventions in different modules.**

I scanned the whole code span for the two ERRCODE stores an `ON ROUTINEERROR` prologue makes:

| Pattern | Meaning | Sites | Address range |
|---|---|---|---|
| `move.w D0w,(0x10,A6)` (`3d400010`) | **16-bit** ERRCODE | 4 | **0x023E0 - 0x03516** |
| `move.l D0,(0x10,A6)` (`2d400010`) | **32-bit** ERRCODE | 49 | **0x07E9E - 0x2082E** |

**Zero overlap.** The split is clean and it falls exactly on the module boundary:

- **PIOC-OS kernel region (~0x23E0-0x3516): ERRCODE is 16 bits, so the first parameter is at
  `(0x12,A6)`.** This is the region that is byte-identical to ENCOS. The 16-bit sites include
  **0x2EAE**, which is precisely the address `PlancFixFlow`'s note cites.
- **Everything else in this image (0x7E9E upward - LANCE driver, AIP, TCP, SKP, XMSG, the
  port library): ERRCODE is 32 bits, so the first parameter is at `(0x14,A6)`.**

Confirmed on both sides of the call, at addresses in each region:

*PIOC-OS side, first parameter at 0x12* - callee `0x28E6` reads it directly:
```
0x28E6:  jsr 0x4492 ; dc.l 0x1E        (frame allocator, skip 4, inline frame size)
0x28FC:  lea (0x12,A6),A0              <-- first parameter
0x2900:  movea.l (0x4,A6),A2
```
and the callers that stage into `(0x4,A6)` write their first argument at +0x12: sites
**0x27E8, 0x285E, 0x2ED2, 0x31E8** (offsets 0x12, 0x16, 0x1A, 0x1E) and **0x32AA, 0x32EE**
(offsets 0x12).

*TCP/IP side, first parameter at 0x14* - `PORTSEND` (0x2081A) reads its four parameters at
`(0x14,A6)`, `(0x18,A6)`, `(0x1C,A6)`, `(0x20,A6)`, and its own handler at 0x2082E does
`move.l D0,(0x10,A6)` - a **longword** ERRCODE, leaving 0x10-0x13 occupied. `POMSGETMES`
(0x20D2A) and the `MON1` stub (0x22846) agree.

**Mechanism (INFERRED, but it is the documented one):** ERRCODE is declared as one PLANC
*word*, and ND-60.117.5 Appendix C records that **from version F of MC68000 PLANC one word
became 4 bytes where previously it was 2**. PIOC-OS is dated APRIL 1986 and was compiled by a
pre-F compiler; the TCP/IP B05 program by version F or later. The two were then linked
together, so **one image legitimately contains two frame layouts.**

**Practical rule:** do not hard-code either offset. Read the routine's own `ON ROUTINEERROR`
prologue - `move.w` at (0x10,A6) means parameters start at 0x12, `move.l` means 0x14. For
this image, the address decides: below ~0x4000 use 0x12, above ~0x7000 use 0x14.
`PlancFrameTypes` should set the field width per routine rather than globally.

### 12.3 The 9 sites PlancFixFlow declined - it was RIGHT to decline

**VERIFIED.** All nine are the same thing, and it is not a mis-identified callee.

The five distinct targets (0x1BCAE, 0x1BCF2, 0x1CEA8, 0x1D320, 0x1DACE) are **inline
`ON ROUTINEERROR` handlers, not routines**. Each begins with `2D 5F` = `move.l (SP)+,(d16,A6)`
and each is preceded by a `bra.b` that jumps over it. From `SKPINIT` (0x1BC9A):

```
0x1BCA8:  move.l D0,(0x34,A6)
0x1BCAC:  bra.b  0x1BCC8            ; jump OVER the handler
0x1BCAE:  move.l (SP)+,(0x6a,A6)    ; <-- HANDLER. Pops the return address into the frame
0x1BCB2:  move.l D0,(0x10,A6)       ; ERRCODE (32-bit -> this module uses 0x14 params)
0x1BCB6:  move.l (0x4a,A6),D0
0x1BCBA:  movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (2,A2)    ; skip return
0x1BCC2:  movea.l (0x6a,A6),A0 ; jmp (A0)                     ; resume via stashed link
```

The critical detail: the handler's **first instruction pops the `bsr` return address off the
stack** into `(0x6a,A6)`. The epilogue then pops the *original* caller's saved A6 and return
link and skip-returns to **the grandparent**. So a `bsr` into one of these handlers **never
returns to the instruction after the bsr** - it either unwinds one level or resumes via the
stashed link at 0x1BCC2.

That is why the bytes after those nine calls are 4 or 8 bytes long and look like ordinary
code: **they are** ordinary code, reached by other paths. There is no error slot to size,
because the call is non-returning. `PlancFixFlow` correctly refused to invent one.

**Rule the script could learn:** a `bsr` whose target begins `2D 5F` (`move.l (SP)+,(d16,A6)`)
and which is immediately preceded by a `bra.b` jumping over it, is a call into the routine's
own `ON ROUTINEERROR` handler. Mark it **non-returning** and apply no fallthrough override.
All nine sites cluster in 0x1BC00-0x1DC00 because that is the `SKP*` session layer, whose
routines each carry two or three stacked handlers.

### 12.4 The 3 sites that would not disassemble - all CODE, none data

**VERIFIED. None of the three is data.** All three are the same 6-byte shape, sitting
immediately after a function's skip-return epilogue:

| Address | Preceded by | Bytes | Decoded |
|---|---|---|---|
| 0x0CF88 | epilogue at 0xCF80 | `20 6E 00 32 4E D0` | `movea.l (0x32,A6),A0 ; jmp (A0)` |
| 0x1C650 | epilogue at 0x1C648 | `20 6E 00 38 4E D0` | `movea.l (0x38,A6),A0 ; jmp (A0)` |
| 0x1C8B4 | epilogue at 0x1C8AC | `20 6E 00 50 4E D0` | `movea.l (0x50,A6),A0 ; jmp (A0)` |

These are **`ON ROUTINEERROR` resume tails** - the "resume via the stashed error link" step
documented in the toolkit README. They are unreachable by linear flow because they sit
directly after a return instruction, and they are only entered by a branch from inside the
handler body. **Do not type them as data.**

**Rule the script could learn:** after an epilogue `2C 5F 24 5F 4E EA 00 02`, if the next 6
bytes match `20 6E xx xx 4E D0`, disassemble them as a handler resume.

### 12.5 The media-access command set - the DIX parallel family

**VERIFIED** from the handler heads. The three previously unidentified opcodes are not
miscellaneous: **ops 22/24/26 are the DIX-mode counterparts of the classic commands**, and
they are identifiable because they test `ACTIVEDIXU` (0x24942) where the classic command tests
`ACTIVEMAUS` (0x2493E).

The cleanest proof is op 26 against op 10. They are the same code with one global swapped:

```
op 10 (activate group address, 0x6FAC):  tst.l (0x2493E)   ; ACTIVEMAUS
op 26 (0x7050):                          tst.l (0x24942)   ; ACTIVEDIXU
```
and from there both continue identically: `bne` past the error, else status **-16** into
RB+0x02, then `move.l #-16,(0x48,A6) ; clr.w (0x4c,A6) ; bsr ...`.

So **op 26 = activate group address, DIX flavour** (INFERRED from that equivalence, but the
equivalence itself is byte-level).

Per-command entry guards, all **VERIFIED**:

| op | command | guard | failure status |
|---|---|---|---|
| 0 | attach | global 0x2487A, then `LNMAIOACTI` | -10 (0xFFF6) |
| 2 | statistics | requires `LNMAIOACTI` nonzero, then attached | -9 (0xFFF7), then -10 |
| 4 | change address | requires `LNMAIOACTI` **zero** (`bne` -> error) - cannot change the MAC while running | -9 |
| 6 | start | requires `LNMAIOACTI` zero - fails if already started | -9 |
| 8 | stop | requires `ACTIVEDIXU`, `ACTIVEMAUS` and `QACTIVELLU` all clear | -17 (0xFFEF) |
| 10 | activate group address | requires `ACTIVEMAUS` | -16 (0xFFF0) |
| 12 | set DIX mode | RB[8] bit 0 must be 1 | -18 / -20 / -16 |
| 14 | DIX attach | RB[8] bit 0 (same shape as op 12) | -18 |
| 24 | **DIX detach (inferred)** | `ACTIVEMAUS` then `ACTIVEDIXU` | -17 |
| 26 | **activate group address, DIX (inferred)** | `ACTIVEDIXU` | -16 |

**op 22 (0x71EA) is structurally different** and is the one genuinely new shape:

```
0x71EA:  movea.l (0x18,A6),A1        ; RB
         lea     (0x0A,A1),A0        ; *** RB+0x0A, not RB+0x08 ***
         movea.l ...,A2
         move.w  (A2),D0w
         btst    #15,D0
         beq     ...
         move.w  (2,A2),D1w
         andi.l  #0x0000FFFF,D1
         add.l   A1,D1               ; RB base + 16-bit offset -> pointer
```
It reads a **descriptor at RB+0x0A** whose bit 15 is a valid flag and whose next word is a
**16-bit offset relative to the RB base**. That is a self-relative buffer reference - a
scatter/gather or data-descriptor element. **INFERRED:** op 22 is a data-carrying command
rather than a control command. Its argument area starts at **RB+0x0A**, not RB+0x08 - so the
argument offset is NOT uniform across the command set.

### 12.6 THE HOST SEAM - the card's port name is `*TCP`

**VERIFIED.** This answers the phase 2 open question, and the phase 2 negative result was
correct rather than misleading: the name is not XMSG-shaped at all.

At **0x7B90A** sit four ASCII bytes: **`*TCP`** (`2A 54 43 50`). Immediately after, at
**0x7B90E**, is a 12-byte PLANC array descriptor over them:

```
+0x00  long  virtualOrigo = 0x0007B90A
+0x04  long  lowerLimit   = 0
+0x08  long  upperLimit   = 3          -> 4 characters
```

`TCPNETINIT` (0x1422A) installs it at **0x1441A** by copying the descriptor into
`TCPPORTNAM_A`:

```
lea     (0x0007B90E).l,A0
lea     (0x0007B91A).l,A1        ; TCPPORTNAM_A
move.l  (A0)+,(A1)+
move.l  (A0)+,(A1)+
move.l  (A0)+,(A1)+
```

and it is read back at 0x11D4C-0x11D52. The descriptor is also referenced at 0x143C2.

**Why phase 2 found nothing:** `TCPPORTNAM_A` and `_B` are zero in the image because they are
filled at run time from this template. Searching the name *buffers* was never going to work.

**Four characters is the PIOC-OS port-name width** - the same width as the sibling firmware's
`RTC `, `FREE`, `PRO1` - so `*TCP` is a **PIOC-OS port name, not an XMSG service name**. The
leading `*` is ND's usual system/service marker.

**Corroborating: PIOC-OS has an explicit ND-100-side port and message pool.** Among the DRAM
symbols applied in phase 2 are `PO100PORTS` (0x2692C), `PORT100POO` (0x26946), `PO100MSGTY`
(0x26ECE), `POMS100POO` (0x26F02), `POMS100BUF` (0x27C36) and `POSKRG100T` (0x24BC4). The
"100" is **ND-100**. So the host seam is the PIOC-OS port system itself, with a separate
ND-100 port pool and message-buffer pool - the ND-100 opens the port named `*TCP` and
exchanges PIOC-OS messages with it, exactly as AIP does with `LNMASPcommand` internally.

**INFERRED, not proven:** that the ND-100 side performs a `PONALOOKUP`-equivalent on `*TCP`.
I did not trace the ND-100-facing registration call, and I did not confirm whether `*TCP` is
registered via `PONAREGIST` (0x20ACC) or only used as a lookup key.

---

## 13. What I could NOT determine - explicit list

Resolved in phase 2: the RB container and header (section 8), `set DIX mode` (section 8), the
TRAP #2 table and its ENCOS equivalence (section 9), the DRAM boundary (section 11), the
`MONn` stubs (section 9).
Resolved in phase 3: the frame-offset contradiction (12.2), the 9 declined call sites (12.3),
the 3 non-disassembling sites (12.4), the identity of ops 24/26 and the guards for all 11
commands (12.5), and the card's port name `*TCP` (12.6).

Still open after phase 3:

A. **~163 CODE symbols still have no function**, and ~20 more have a pinned 1-byte body I
   cannot repair over MCP (12.1). Needs a Ghidra script doing
   `disassemble; removeFunction; createFunction` from `tcp-symbols-full.txt`.
B. **Op 22's payload** (12.5). I established it reads a self-relative descriptor at RB+0x0A
   with a bit-15 valid flag, but not what it carries or what the command is called. It is the
   only command whose argument area does not start at RB+0x08.
C. **The names of ops 24 and 26** are inferred from a byte-level equivalence with ops 8 and 10
   plus the ACTIVEMAUS/ACTIVEDIXU swap. No vendor text names them.
D. **The name-to-opcode pairing for the middle six commands** is still ordinal inference from
   the AIP text block. Only `set DIX mode`, and now (by guard structure) `change address`,
   `start` and `stop`, have independent corroboration.
E. **The other 5 bytes of the `set DIX mode` argument.** Unchanged from phase 2.
F. **Whether the ND-100 registers or looks up `*TCP`**, and the ND-100-side message format on
   that port. `PO100PORTS` / `POMS100POO` / `POMS100BUF` are the structures to read next -
   this is now the single highest-value remaining target.
G. **The data-port RB layout** - `MADATAPORT` (0x7298), `LLCMDPORTH` (0x75B8), `LLDATAPORT`
   (0x7B78) bodies still unread. Frame buffers are handed over here, not on the command port.
H. **The duplicated `_A` / `_B` structure pair** (section 10). Still unexplained. Note
   `TCPPORTNAM_B` (0x7BB0A) is referenced only from the symbol table, never from code, whereas
   `_A` is referenced from three code sites - so the pair is not symmetric in use.
I. **The 18 `XMP*` primitives beyond the ENCOS eight** - named only, no body read.
J. **`TCPINCKSUM`, `SENDARP`, `ARPINPUT`** now have functions but were still not analysed.
K. **The five longwords before the port-name table at 0x24A4C**; **the 8 stray bytes at
   0x7FBA0**; **bank 2 and most of bank 3** outside the DRAM symbols. All unchanged.

1. **The other 5 bytes of the `set DIX mode` argument.** Bit 0 of byte 0 is the enable flag -
   proven. The handler copies all 6 bytes into a structure at `(0x1C,A6)+4` and I did not
   trace that destination. Six bytes is MAC-address-shaped; I am **not** guessing what it means.
2. **Media-access opcodes 22, 24 and 26.** Handlers located (0x71EA, 0x6FF2, 0x7050) and reply
   types confirmed (23, 25, 27), but they have no AIP text and were not read. AIP calls
   type 24 from 0xC5F6 and type 18 from 0xC500 - note **type 18 has no handler in the command
   table**, so it must be served by a different port. Unexplained.
3. **The name-to-opcode pairing for the middle six commands.** Ordinal inference from the AIP
   text block. Only `set DIX mode` (op 12) has independent confirmation, via the
   `ACTIVEDIXU` global. `attach` (op 0) and `DIX attach` (op 14) are plausible from their
   distinct status-string sets but unproven.
4. **`MADATAPORT` (0x7298), `LLCMDPORTH` (0x75B8), `LLDATAPORT` (0x7B78).** Named and created,
   bodies not read. The **data**-port RB layout is very likely different from the command RB
   and is where actual frame buffers are handed over - this is the obvious next target.
5. **Whether the card registers an XMSG service name, and what it is.** No `XM-`, `ENNS` or
   `*XM` string exists anywhere in the image (searched, zero hits). `TCPPORTNAM` at 0x7B91A /
   0x7BB0A is a run-time-filled name buffer and is the place to look.
6. **The 18 `XMP*` primitives beyond the ENCOS eight.** Named only; no body read.
7. **The duplicated DRAM structure pair** (section 10). Two instances of the same layout at
   0x78AFC/0x78CEC and 0x7B91A/0x7BB0A. Purpose unknown.
8. **`PORTRECEIV` (0x20702) and `PORTCREATE` (0x203C8) bodies.** `PORTRECEIV` would confirm the
   consumer-side use of the read index at port+0x14, which is still inferred from the producer
   side only.
9. **`TCPINCKSUM` (0xF856), `SENDARP` (0xCF0E), `ARPINPUT` (0xD290).** Located and named, not
   analysed.
10. **The five longwords before the port-name table at 0x24A4C** (0x492A, 0x4900, 0x492A,
    0x491A, 0x492A). Same numeric range as the port-system error codes. Still unidentified -
    note they are **not** in the code span, which phase 1 stated incorrectly; the code span
    ends at 0x2287A.
11. **The 8 stray bytes at 0x7FBA0** in the middle of the symbol table. Present, purpose unknown.
12. **Bank 2 and most of bank 3** (0x40000-0x7C3A0 outside the DRAM symbols). Large regions
    were never examined at all.

---

## 14. Corrections to the briefing, collected

| Briefing said | Reality |
|---|---|
| Four independent banks: 0 = PIOCOS, 1 = AIP, 2 = TCP sparse, 3 = TCP + symtab | **Wrong for code.** One contiguous linked CODE segment 0x47F4-0x2287A. AIP and TCP both live below 0x20000. Banks 2 and 3 are data and the symbol table. |
| ~437 symbols | 463 records (317 CODE defined, 134 DRAM defined, 12 markers) |
| Symbol table "around 0x7C3A0" | Starts there, runs to 0x7FD88 with an 8-byte break at 0x7FBA0 |
| Symbol record +0x08 address | Correct - and correctly noted as 4 bytes later than ENCOS |
| Vector table at 0x0-0x3FF, TRAP #2 = kernel gate | **Confirmed.** Reset PC 0x1CFE, TRAP #2 -> 0x3498 |
| `jmp (A5)` is a continuation / A5 holds a continuation address | Superseded by the coordinator's own correction: A5 is the PLANC error vector, `jmp (A5)` is the error unwind |
| The `4E EA 00 02` epilogue is not the end of the function | Superseded by the coordinator's own correction: it IS the return (skip return to RETLINK+2) |
| CODE (0x10) and DRAM (0x16) are separate OVERLAPPING address spaces; do not label DRAM addresses | **Wrong.** One flat space. Zero of the 134 defined DRAM symbols fall inside the code span 0x47F4-0x2287A. The cited example is not a collision: `BUFFER_END` 0x1A00 is simply below `END_PIOCOS` 0x4660. All 132 were applied safely. My own phase 1 caution was wrong too. |
| (phase 1, mine) `MONn` are probably SINTRAN monitor-call stubs | **Wrong.** They are PIOC-OS `trap #2` wrappers; `MON0` is literally `moveq #0,D0 ; trap #2`. |
| The MA command set is 8 commands | 8 are named by AIP, but **11** are implemented (ops 22, 24, 26 additionally). |
| (toolkit README + briefing) PLANC first parameter is at `(0x14,A6)` | **Right for this program, wrong for the PIOC-OS kernel inside it.** Both offsets occur - see 12.2. |
| (`PlancFixFlow` output) first parameter is at `(0x12,A6)`, not 0x14 | **Right for the PIOC-OS kernel, wrong for the TCP/IP program.** Both offsets occur - see 12.2. |
| (implied) `PlancFixFlow`'s 9 skipped sites are a limitation to fix | **No - the script was correct.** Those calls are non-returning; there is no error slot to size. See 12.3. |
| (implied) the 3 sites that would not disassemble might be data | **All three are code** - `ON ROUTINEERROR` resume tails. See 12.4. |
| (mine, phase 2) no XMSG/service name exists in the image, so the card may not advertise one | **The search was right but the conclusion was too broad.** The card does advertise a name - `*TCP` - as a 4-character PIOC-OS **port** name built at run time from a descriptor, not as an XMSG service name. See 12.6. |

---

## 15. Related documents

- [ENCOS firmware symbol table](../../../../Installation/Communication/Ethernet/x/stripped/docs/ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md)
- [PLANC-MC 68K frame facts](../../../../tools/ghidra-planc/PLANC-MC-68K-FRAME-FACTS-MANUAL-SOURCED.md)
- [ghidra-planc toolkit README](../../../../tools/ghidra-planc/README.md)
- [Ethernet Basic Software Programmer Guide](../../../../Reference-Manuals/ND-60.197.01%20EN%20Ethernet%20Basic%20Software%20Programmer%20Guide.md)
