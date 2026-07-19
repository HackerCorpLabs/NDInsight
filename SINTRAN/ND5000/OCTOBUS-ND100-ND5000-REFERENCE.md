# OCTOBUS: ND-100 <-> ND-5000 (SAMSON) Communication Reference

**Status**: Consolidated reference, 2026-07-15
**Evidence classes**: **[V]** = verified from primary source (NPL source, hardware manual, microcode binary/disassembly), **[I]** = interpretation/inference (marked with reasoning), **[C]** = contradiction between sources (unresolved).

**Primary sources**
| Source | Path |
|---|---|
| ND-5000 Hardware Description (ch. 5 Access Module, App. 2 Octobus Protocol v5) | `E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware Description.md` (also `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\`) |
| SINTRAN NPL boot/driver source | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\PH-P2-OPPSTART.NPL`, `MP-P2-N500.NPL` |
| Symbol tables | `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\N5000-SYMBOLS.SYMB.TXT`, `s3vs-4.symb` |
| Octobus protocol carve docs | `E:\Dev\Ronny\NDInsight\SINTRAN\Devices\Octobus\OCTOBUS-PROTOCOL-REFERENCE.md`, `octobus_protocol_frame_format_and_introduction.md` |
| ND-500/5000 interface carves | `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ND500-BUS-OCTOBUS-HW-INTERFACE.md`, `ND500-BUS-INTERFACE-REFERENCE.md`, `ND5000-SAMSON-ARCHITECTURE.md` |
| ND-5800 microcode disassemblies | `E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md` (WM500), `MICRO-5800-A30.md` (WM406) |
| C# emulator | `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusOctobus.cs`, `OctobusFabric.cs`, `OctobusND5000Station.cs` |

---

## 1. Big picture

On the ND-5000 (SAMSON) generation the old ND-500/1 interface pair — PCB 3022 (ND-100 side) and PCB 5015 "CONTROL II" (ND-500 side) with their 64-wire cable — is **gone**. **[V]** (ND-05.020.01 p. 31): the octobus replaces both the "ND-500 interface card in the ND-100" and the "ND-100 interface on the Control II card".

Replacement architecture:

```
ND-100/110/120                                    ND-5000 CPU (SAMSON)
+--------------+     OCTOBUS (serial, 4 pairs)    +---------------------------+
| 3109/324118  |<================================>| Access Module (ACCP card) |
| Octobus/MPM  |      messages/kicks only         |  OCTC gate array + 68000  |
| Line Driver  |                                  |  AIB/AOB <-> microprogram |
+--------------+                                  +---------------------------+
       |                                                       |
       |                MFbus / MPM-5 shared memory            |
       +===========  (ALL data goes through here)  ============+
                   mailbox + X5FIF FIFO + X5SEMA semaphore
```

**Key principle [V]** (ND-05.020.01 ch. 5.3): *the octobus normally carries no data* — it is a control/wake-up path ("look in the mailbox"). Data moves through MFbus shared memory. The mailbox model is the same Control/Status/Address register triple as the 3022 generation, carried over the new transport; answers go through a shared-memory FIFO (X5HEN/X5FYL/X5FIF) guarded by the X5SEMA test-and-set semaphore, and wake-ups are octobus kicks instead of the old MAR+CONTROL activate.

Old-generation mapping (for readers coming from the 5015 docs):

| ND-500/1 (3022/5015) | ND-5000 (octobus/ACCP) |
|---|---|
| DATA-IN / DATA-OUT registers | AIB / AOB buffers |
| TAG-IN / TAG-OUT handshake | AIBF / AOBF / ATRAP / OMESS flags |
| WA / BREAK / CSCNT control-store load | APR/ASR serial shadow-register loop, ACCP `LOAD CONTROL STORE` (LOCSD/LOCSM) |
| MAR + CONTROL "activate" | Octobus kick (XKICK500) |
| Level-12 interrupt + MAILINK chain walk | Octobus input ident -> XN500 driver -> X5FIF FIFO drain |

---

## 2. Octobus physical / wire level [V]

(ND-05.020.01 Appendix 2, pages 330-331)

- Four differential signal pairs, converted to TTL on the local bus: **XREQ** (request), **XCLK** (clock), **XDAT** (data), **XRFO** (master refresh, 15 µs period).
- Up to 62 stations (station numbers 1-76 octal; 0 and 77B illegal). Buses can be bridged.
- **Master**: any node can be master (it supplies XCLK and XRFO). If XRFO stops pulsing, stations auto-arbitrate; the **lowest station number becomes master**.
- Speed: 4 MHz clock = 1 Mbit/s (8 µs per frame), 1 MHz = 250 kbit/s, 0.5 MHz = 125 kbit/s.
- **Wire frame** = start bit + 30 bits + stop bit:
  `| Priority(4) | Destination(6) | C | B | Source(6) | Information(8) | Parity(2) | Ack(2) |`
- **Arbitration**: all requesters transmit simultaneously; a station transmitting '0' that reads back '1' stops and increments its **lost-access (priority) counter** — starvation-free by hardware.
- **Ack codes**: 00 = timeout (15 retries), 01 = received OK, 10 = destination busy (255 retries), 11 = parity error (B=0, 15 retries) / ambiguous response (B=1). Emergency messages transmit with the priority counter forced to max.

**Station numbers (octal) [V]** (App. 2 p. 329):

| Octal | Decimal | Device |
|---|---|---|
| 1 | 1 | ND-120 CPU |
| 2-7 | 2-7 | MFbus controllers |
| 10-13 | 8-11 | SCSI controllers (disk) |
| 14-15 | 12-13 | Matra VME |
| 16-17 | 14-15 | Multifunction communication |
| 20 | 16 | Hyperchannel |
| 21-23 | 17-19 | FDDI (fibernet) |
| 24-27 | 20-23 | FPS-5000 |
| 30-33 | 24-27 | Graphic controller |
| 34-67 | 28-55 | Free for expansion |
| **70-76** | **56-62** | **ND-5000 CPUs** |

SINTRAN assigns each SAMSON CPU station `CPUNO + FN5DEST - 1` **[V]** (ND5000-SAMSON-ARCHITECTURE.md §3-5).

---

## 3. The 16-bit software frame — THE format (and a debunk)

**[V]** (App. 2 §2.5 + NPL PH-P2-OPPSTART.NPL:3929-3932). This is the single frame layout used everywhere software touches the octobus: written to the ND-100 card's transmit register (100405), read from its receive FIFO (100400), and seen by the ND-5000 microprogram in AOB/AIB:

```
| 15 | 14 |  13 - 8            |  7 |  6 |  5 |  4 |  3 - 0        |
|  C |  B | DEST(send)/        |  E |  K |  M |  S | number / data |
|    |    | SOURCE(receive)    |    |    |    |    |               |
```

- **C** (NPL symbol `CBIT` = bit 15): control frame (1) vs data frame (0)
- **B** (bit 14): broadcast to station TYPE (BT register, 3 bits — the six type codes are documented NOWHERE we have found **[C/open]**)
- **Station field**: destination when sending, **rewritten to source by the bus on receive**
- **E** (`EBIT` = bit 7): emergency; **K** (bit 6): kick; **M** (bit 5): multibyte SOMB/EOMB; **S** (bit 4): start(1)/end(0) of multibyte

Frame decode table **[V]** (App. 2 fig. 64):

| C | E | K | M | S | Low bits | Meaning |
|---|---|---|---|---|---|---|
| 1 | 1 | - | - | - | emergency code (with E, bits 7-0 read as octal code 2xxB) | EMESS emergency |
| 1 | 0 | 1 | 0 | - | kick number (bits 5-0) | KICK |
| 1 | 0 | 0 | 0 | - | ident number | IDF ident |
| 1 | 0 | 0 | 1 | 1 | OMD number (bits 3-0) | SOMB start of multibyte |
| 1 | 0 | 0 | 1 | 0 | OMD number | EOMB end of multibyte |
| 0 | - | - | - | - | data byte (bits 7-0) | DATA (multibyte body) |

NPL frame construction **[V]** (CH5CPUPRESENT):

```npl
ASTATION\/COMD=:5STATION
A SH 10 BONE CBIT BONE EBIT=:X       % SH 10 (octal!) = shift left 8: station -> bits 13-8
T:=100405; A\/CMMACLE; *IOXT         % masterclear Samson  (CMMAC = 041B)
A:=X\/CMACONT; *IOXT                 % continue accp       (CMACO = 042B)
```

### 3.1 DEBUNK: the "TRANSMIT-ON-OCTOBUS trace" header format

An earlier analysis (preserved in old versions of `NDBusOctobus.cs`) claimed a conflicting write format: C=bit 9, B=bit 8, dest=bits 7-2, "verified from trace" with octal values 1404/1424/0450/0074. **Resolution [V]**: those trace values are the correct frames with their two trailing octal zeros dropped:

| Dest | C | B | "trace" | Real value | Real frame |
|---|---|---|---|---|---|
| 1 | 1 | 1 | 1404 | **140400B** = 0xC100 | C, B, dest=1 |
| 5 | 1 | 1 | 1424 | **142400B** = 0xC500 | C, B, dest=5 |
| 10 | 0 | 1 | 0450 | **045000B** = 0x4A00 | B, dest=10 |
| 15 | 0 | 0 | 0074 | **007400B** = 0x0F00 | dest=15 |

`(C<<9)|(B<<8)|(dest<<2)` is exactly the true frame shifted right 6 bits. The NPL and the trace **agree**; the alternate bit layout was an artifact of octal truncation. Do not reintroduce it.

### 3.2 DEBUNK: the fabricated `OctobusCommand` enum

The old C# enum values (MasterClear=0x0010, ContinueACCP=0x0020, CpuReset=0x0040, IdleKick=0x0080, ClearKick=0x0100, N100Kick=0x0200) were invented. 0x0010 = 020B is the **DCONT "clear interface"** value written to control registers +3/+7 (a different register and mechanism); the rest continued the power-of-two pattern. The decode (`value & 0xFF00` when dest==0) was structurally dead code. **[V]** real values in §5 below.

---

## 4. Emergency codes — three-source cross-check [V]

The hardware manual (ch. 5.3.9) quotes emergency codes as whole octal info bytes; the NPL builds them as `EBIT | CM-code`. They are the same numbers:

| Info byte (octal) | = EBIT + | NPL symbol | Effect on ND-5000 |
|---|---|---|---|
| **241B** (0xA1) | 041B | CMMAC(LE) | Master clear: resets ACCP **and** ND-5000 CPU (full power-up sequence, self-test) |
| **242B** (0xA2) | 042B | CMACO(NT) | Continue ACCP (sent right after master clear by CH5CPUPRESENT) |
| **244B** (0xA4) | 044B | (CMATE in symbol dump) | Terminate ACCP -> level-7 interrupt -> firmware idle loop. Used by the ND-500 monitor on ACCP timeout; followed by Alive Check; if that fails, only 241B helps |

Emergency messages carry R/H interpretation bits (R=0,H=0 software-handled; R=0,H=1 hardware-decoded — the ACCP card's MCL PAL decodes master clear in hardware, print version C+ required for octobus decoding).

---

## 5. CM* command codes [V]

From `N5000-SYMBOLS.SYMB.TXT` (5-char symbol truncation: CMMACLE→CMMAC, CMACONT→CMACO, CMCPURES→CMCPU). All octal. Full table in `OCTOBUS-PROTOCOL-REFERENCE.md` §3; the important ones:

| Symbol | Octal | Hex | Use |
|---|---|---|---|
| CMACK | 000 | 0x00 | Acknowledge |
| CMSYS(PAR) | 016 | 0x0E | System parameter (multibyte MCOMMAND high byte) |
| CMREA | 020 | 0x10 | Read address |
| CMRUN / CMSTO / CMCON / CMRES | 033/034/035/036 | 0x1B-0x1E | Run / Stop / Continue / Reset |
| **CMMAC** | **041** | **0x21** | Master clear (as emergency: 241B) |
| **CMACO** | **042** | **0x22** | Continue ACCP (as emergency: 242B) |
| CMENK | 061 | 0x31 | Enable kick |
| CMMIC | 066 | 0x36 | Microcode command |
| **CMCPU(RES)** | **071** | **0x39** | CPU reset — sent as a **multibyte message to OMD 3**, NOT a raw frame (XRS5CPU, MP-P2-N500.NPL:3328-3341: `CMCPURES SHZ 10 =: X.MCOMMAND; CALL MBSEND`) |
| CMHWF | 200 | 0x80 | Hardware fault (error code in fault messages) |

**Kick numbers** — triple-verified (NPL symbols / manual p. 336 / microcode OCB_DEC_K dispatch):

| # | NPL symbol | Manual (p. 336) | Microcode handler (B30) |
|---|---|---|---|
| 1 | **N100KICK** | activate ND-5000 process | → ACTIVATE |
| 2 | — | activate ND-5000 process | → ACTIVATE |
| 3 | **CLRKICK** | clear flag — continue process | → OCB_KICK03 (sync via message-region semaphore, resume or idle) |
| 4 | — | update internal clock | → OCB_KICK05 handler (shared with 5) |
| 5 | — | NUCLEUS kick | → OCB_KICK05 (set idle, clean queue) |
| 6 | **IDLEKICK** | save context — go IDLE | → OCB_KICK06/KICK06 (CNTXTSAVE, cleanup, IDLE) |
| 0, 7-63 | — | — | → TRAP_NOTREC (error report 204B back over octobus) |

Broadcast is **forbidden** for kicks and idents (manual p. 336). Idents are routed by *source + ident number* to IDENT ENTRIES (process activation), not an echo protocol.

---

## 6. ND-100 side: the 3109/324118 card and SINTRAN driver

### 6.1 Hardware [V]

(ND500-EVIDENCE-AND-CONTRADICTIONS.md lines 855-923; ND-05.020.01 App. 1 p. 325)

- "N-100 Octobus & MPM Line Driver": **PCB 3109 = part 324118** (OBCON gate array 36600B, "ND-O2CON") or **PCB 3096 = part 324133** (36600D "NDOBCON" ASIC). It is a Multiport (MPM) Line Driver with the octobus controller added.
- Thumbwheels: TH5 = octobus speed, TH4/TH3 = station number low/high, TH2 = octobus device number. LEDs: LD1 = XREQ, LD2 = MASTER. Connectors A/B to the MFbus/MPM cabinet, C to the ND-100 bus.
- Small-cabinet alternative: Double Bus Controller (324244) = MFbus controller + line driver + port on one card.
- The IOX device numbers are **not** in any hardware manual — software-verified (NPL) only.

### 6.2 IOX register map [V from NPL usage]

Base 100400 octal (interface 0); interfaces 1-3 at +10B steps (100410/100420/100430).

| Reg | Addr | Dir | Function | NPL evidence |
|---|---|---|---|---|
| +0 | 100400 | R | Input data (pops 16-word receive FIFO) | |
| +1 | 100401 | W | Input data write (rare) | |
| +2 | 100402 | R | Input status (presence probe: IOX error A=7 = no card) | OCSTART `T:=HDEV+2` |
| +3 | 100403 | W | Input control (DCONT=3; value 020B = clear/master-clear, bit 0 = interrupt enable) | OCSTART `20; *IOXT` |
| +4 | 100404 | R | Output data (loopback readback) | |
| +5 | 100405 | W | **Transmit frame** (one 16-bit software frame per write, §3) | CH5CPUPRESENT |
| +6 | 100406 | R | Output status (bit 3 = ready-for-transfer; polled `WHILE A NBIT 3`) | CH5CPUPRESENT |
| +7 | 100407 | W | Output control (same DCONT semantics) | OCSTART `T+4` |

Receive FIFO is 16 x 16 bits (matches the OCTC FIFO on the ND-5000 side). Input status carries the **source station in bits 13-8** [I — from emulator trace conventions; register bit semantics beyond bit 3/bit 7 are inferred from polling loops only].

**Interrupt idents [V — LIVE-VERIFIED by two ND diagnostics, 2026-07-15]**: the hardware idents are **40B (input/receive) and 41B (output/transmit) on level 13**; interfaces 2-4 use 42B/43B, 44B/45B, 46B/47B at 100410/100420/100430. Proof one: TPE OCTOBUS B00 `LIST-OCTOBUS-DEVICES` prints the full table ("IDENT CODES LVL 13, Receive - Transmit: 100400 40 41 / 100410 42 43 / 100420 44 45 / 100430 46 47"). Proof two: CONFIGURATION D05 states it outright when the emulator answered with the wrong codes: *"Wrong identcode(s) found on level 13D. Expected identcodes: 40B and 41B. Found identcodes: 37B and 40B."* The NEC-01 course table was therefore RIGHT; the "60B/61B" ident-formula claim stays refuted.

The earlier "37B/40B" reading of the ITB13 byte evidence (`ITB13 = 154075`; `MEM[154134] = 123511 = IOCT0`, `MEM[154135] = 123537 = OOCT0`, i.e. slots +37B/+40B) is superseded: the slot index is evidently not the ident code itself. [I] Plausible reconciliation: if ITB13 holds the entry for ident N at `ITB13+N-1` (table indexed from ident 1), slot 37B belongs to ident 40B and slot 40B to ident 41B — matching the live values exactly; this offset interpretation is UNVERIFIED (check where the RTC ident-1 datafield sits relative to ITB13). The kick path's "level 12" (`LV12KICK`) is the *driver* level, distinct from the card's interrupt level.

### 6.3 SINTRAN boot and driver flow [V]

- **OCSTART** (PH-P2-OPPSTART.NPL:4029-4086): probe +2 (IOX error = no octobus), clear both controllers (020B to +3 and +7), allocate buffer pool (CBPOOL: ONOBU buffers) and entry tables (CENTRY: 16 OMD entries, 16 kick entries, per-source-station ident entries), hook level-link elements for levels 10/11/12. *Handles only interface 0.*
- **CH5CPUPRESENT** (3895-3945): old 3022 probe first; if absent, probe 100406; wait bit 3; assign `5STATION`; send CMMACLE + CMACONT emergency frames; set MUDOM flag, mark CPU SAMSON.
- **CON5IDENT / MFPREPARE** (MP-P2-N500.NPL:3575-3634): connect the octobus ident entry, then send the LOAD-SYSTEM-PARAMETERS multibyte (CMSYSPAR, host station/OMD + error station/OMD, 4-byte payload) to the ACCP — this is the manual's LSYSPAR handshake, which the microprogram fetches via micro-command 1 immediately after starting at CS address 0.
- **XKICK500** (3270-3316): forces itself onto level 12, then `T:=5STATION; X:=OCTORING; A:=CKICKTYPE; CALL SKICK`. Callers use kick types N100KICK (activate), CLRKICK, IDLEKICK.
- **XRS5CPU / RS5CPU** (3319-3369): CPU reset via multibyte CMCPURES to OMD 3 (ACCP command CPURES — resets the ND-5000 CPU, not the ACCP).
- **5OMBREAD** (3372-3551): handles multibyte messages *from* the SAMSON on the reserved OMD: CPU-available acks (sets `5ALIVE`), microprogram HW-fault/trap reports (CSTS 200B/201B + register dump — the message the microcode's TRAP_OCBM machinery builds), MF-controller memory-error records (SEC codes 20B/30B/31B/50B/51B/77B), with MFACK replies via MBSEND.
- **Talk-back interrupt path**: SAMSON writes its answer into the shared-memory FIFO, then sends an octobus frame to station 1; the input controller interrupts; SINTRAN's ident entry dispatches to the **XN500** driver branch which drains X5FIF under the X5SEMA semaphore (`ND500-BUS-INTERFACE-REFERENCE.md` §7.5). Monitor calls from the ND-500 reach SINTRAN through DECOMESS -> MCHANDLE exactly as on the old generation.
- **NUCLEUS API** (L-version release notes §15.8): NKSEND / NKRECEIVE / NKMOVE / NKGETINFO monitor calls ride on this transport (kick 5); kernel error `nke_KICKLOCK = 101042B` = kick-queue lock timeout.

### 6.4 Carved driver bodies [V — carved 2026-07-15]

The routine bodies missing from the loose NPL files were located in the **MPIT overlay**:
`E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm` (load base 32000B; byte-identical twin in `017-S3SMPIT` over 035400-041100). Earlier claims that the driver "is not present in any carved segment" (`324B-OctobusFunction/README.md`) and the bogus symbol overlay in `006-S3FS.annotated.dis` are both **wrong** — the symbols resolve against the PIT-mapped image, not commoncode. Ten sibling symbols land on coherent cross-referencing code: SOCTO=035546, IOCTO=035551, OCTOI=035553, OCTOT=035456, KICKS=036047, OMDRE=036054, SOCTW=036342, SKICK=037254, MBSEN(D)=037425, OMBRE(AD)=037660, CONOM(D)=040062, ECONI(D)=040467. (Killer cross-check: SOCTO's kick dispatch pointer `[035647]=036047` = KICKS.)

What the bodies prove:

- **Frame model byte-confirmed**: SOCTO's receive dispatch tests C=bit 15, E=bit 7, K=bit 6, M=bit 5, S=bit 4, extracts station from bits 13-8. The §3 frame layout is no longer only NPL/manual-verified — it is machine-code-verified.
- **SKICK** (037254) builds exactly `C | K | station<<8 | kicktype` (`SHT ZIN 10` for the station shift, `BSET ONE 60` for K, `BSET ONE 170` for C), with a second entry at 037256 that sends the same frame WITHOUT the K bit. If the transmitter is idle it writes control:=4, frame to write-data, control:=1; otherwise it queues the frame in a ring buffer (full ring = error exit).
- **SOCTO** (input): polls status (+2), bit 3 = FIFO non-empty, reads frames from +0; dispatches E-frames (discard - hardware handled), K-frames -> KICKS (per-code jump table), M/S-frames -> per-station multibyte open/close state machine, C=0 data bytes -> per-station collection.
- **SOCTW** (output): interrupt-driven transmit service; ring buffer for single frames; multibyte state machine sends SOMB (descriptor | C | S), two data words per service pass, then EOMB (descriptor | C); re-enables with control:=1 after each pass.
- **MBSEND** (037425): builds the message descriptor - sets M (bit 5) always, B (bit 14) conditionally on a descriptor field, length-validates, MOVEWs the payload, hands off to the output datafield.
- **Register semantics confirmed**: +0 read FIFO, +1 write frame, +2 status (bit 3 = ready/non-empty), +3 control with values **4** (= transmit enable, matches ControlWordBits bit 2) and **1** (= interrupt enable, bit 0).
- **Device base**: the MPIT bytes never contain a literal 1004xx (datafields are runtime-initialized), but the resident commoncode sender at 063247-063263 IOXTs literal **100405** (write) / **100406** (status) while building `C|E|station<<8|code` frames with codes 41B/42B - byte-proof of the output base 100404; input base 100400 follows from the +4 spacing [I].

Remaining unknowns from the carve: datafield initialization chain (how B is loaded at interrupt entry), higher-level meaning of the M=0 vs S=0 receive paths, MBSEND's B-bit trigger condition.

---

## 7. ND-5000 side: the Access Module (ACCP)

### 7.1 Hardware [V] (ND-05.020.01 ch. 5)

- Baby card **324702 (print 5602)**, later **324716 (print 5616)** [the 5616 replacement is from board-catalog data; it does not appear in ND-05.020.01].
- MC68000 @10 MHz (clock-locked to the ND-5000), 128 KB EPROM (2x27512, vector table at 0), 32 KB SRAM, OCTC octobus gate array, 16x16 receive FIFO, dual UART (console/RS232), MCL PAL (hardware-decoded master clear), interrupt PAL (autovectors, 8 strap-selectable sources; octobus emergency = level 7).
- Device selection by MA(23-20); UART above 0xC00000 (5 wait states).
- The ACCP firmware is 68000 machine code — **there is no "ACCP microcode"**. No EPROM dump is currently available (open item).

### 7.2 The AIB/AOB interface [V]

- **AOB** (ACCP -> microprogram): ACCP writes AOB + sets AOBF, optionally ATRAP (+ OMESS for octobus traffic). **AOBF and ATRAP auto-clear when the microprogram reads AOB** (p. 108).
- **AIB** (microprogram -> ACCP): microprogram write sets AIBF; ACCP clears it (RAIBF) after reading.
- **Routing rules** (ch. 5.3.1): with kicks enabled and ACCPTRAP clear, AIB words go **straight to the octobus** (no ACCP protocol handling). Kicks/idents/multibyte-to-OMD>3 from the octobus go **straight to the microprogram** via AOB with ATRAP+OMESS. Multibyte to OMD 0/3 is consumed by the ACCP itself (OMD 0 = octobus test, OMD 3 = ACCP command library).
- **OMESS** distinguishes octobus traffic from ACCP-originated traps (AMICTRAP command: ATRAP without OMESS; microtrap subcodes 1=redefine system parameters, 2=halt, 3=run).
- **Micro-commands** (ch. 5.3.7, microprogram sets ACCPTRAP + writes AIB): 1 = get system parameters (3 words via AOB), 2 = get ASTS+BADAP status (2 words), 3 = get CPU type/model. Replies come **without ATRAP**.
- **ASTS** (p. 111): bit 0 AIBF, 1 AOBF, 2 OBREC (octobus FIFO), 3 OSTOP (emergency, used by TERMINATE), 10 ALIVE, 11 ACCPTRAP, 12 STOP, 13 POWFAIL, ...
- **ACCP command library** (ch. 5.3.11+, multibyte to OMD 3): ECHO, LSYSPAR, AMICTRAP, LPARP/VPARP (parameter pointer in shared memory), LOCSD/LOCSM (**control store load**: 128-bit microwords = 16 bytes + checksum, direct or via memory), DCSD/DCSM dump, MAR/MIR access, START/STOP/CONTINUE MICROPROGRAM, SCOPE loop, **ENKICK/DISKICK**, CPURES, TERM, ARES, RECO, PROM version. Replies: Messack / Messnak (codes -2..9; -2 = illegal when kicks enabled, 0 = microprogram not started, 9 = CS not initialized...). No reply -> ND-120 sends TERMINATE.
- **ENKICK** (ch. 5.3.47): issued when the OS starts; afterwards all hardware-affecting ACCP commands are disabled and octobus traffic flows to/from the microprogram. **DISKICK** returns to test/init mode where everything except multibyte-to-OMD-3 is ignored.

### 7.3 Microcode counterpart (ND-5800 B30) — [V from disassembly]

Full catalog in section 9. The primitives (B30 addresses, octal):

| Routine | CS addr | Function |
|---|---|---|
| ACCP_READ | 016371 | spin on AFLAG bit 9, read `A,SPEC,AOB` -> SC13/Q, return |
| ACCP_WAITI | 016375 | same wait, return AFLAG without consuming AOB |
| ACCP_WRITE | 016402 | spin until AFLAG bit 10 clears, write SC12 -> `D,SPEC,AIB` |
| ACCP_WAITO | 016406 | wait for AIB drained |
| ACCP_RDYW | 017073 | full command/response exchange (command in SC11), parks async messages for ATRAP_CHK |
| SCAN_ACCP | 016554 | poll AFLAG, dispatch bit12->power fail, bit5->OCB kick, bit6->async trap, bit11->other trap |
| TRAP_OMESS | 016412 | octobus-message trap: read AOB, dispatch OCB_DECODE (microcode kicks) or OCTO_SOFT (software delivery) |

**AFLAG bit map** (assembled from all test sites; bits 9/10 are the handshake, the rest [I] from dispatch targets): bit 5 = OCB kick/message pending, bit 6 = async trap pending, bit 7/8 = data/instruction fault, bit 9 = **AOB has data**, bit 10 = **AIB busy**, bit 11 = other trap, bit 12 = power-fail. *These bit meanings are inferred from loop polarity and dispatch targets — not documented anywhere.*

**Message framing seen by the microcode [V]**: ACCP->CPU messages are word streams from AOB where **bit 15 marks the last word** (OCB_MES_M drain loop tests BM17). Outbound multi-word status/error messages (codes 202B CPU-available, 203B CPU-unavailable, 204B-210B error reports) are assembled via ACCP_XWRITE into the shared message region (base 020000 physical) and terminated with a `...|100040`-pattern word — the ACCP fetches them from memory, matching 5OMBREAD's record formats on the SINTRAN side.

**A30 vs B30 [V]**: identical ACCP primitive layer; B30 (work mode 500) adds the whole NK multiprocessor nucleus (SEND/RECVE/GETINF/MHOLE macro-instructions, LOCK_DH spin-locks, NK_TRACE, SENKICK) and real handlers for kicks 3-6; A30 stubs kicks 3-6 to "not recognised" and vectors SEND/RECVE/GETINF/WHOLE to ILLEG.

---

## 8. C# emulator mapping (RetroCore)

Implemented 2026-07-15 in `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\`:

| File | Role |
|---|---|
| `NDBusOctobus.cs` | ND-100 card (3109): IOX map 100400-100437, dual controllers, 16-word FIFO, level-triggered interrupts, loopback for TPE tests. Registers itself on the fabric as station 1 (talk-back path). Corrected: octal station enum, on-bus frame parse, CM* command codes (fabricated enum removed). |
| `OctobusFabric.cs` | The bus: 64-slot station registry, unicast routing with dest->source rewrite, broadcast (simplified: all stations, type codes unknown), Ack-timeout as null. |
| `OctobusND5000Station.cs` | The Access Module: AIB/AOB with AIBF/AOBF/ATRAP/OMESS semantics, emergency 241B/242B/244B, kick delivery + KickReceived event, multibyte OMD routing (0/3 to ACCP, >3 to AOB), micro-commands 1-3, inferred AFLAG word, ENKICK/DISKICK. `AttachCpu(IND500Cpu)` follows the NDBusND500IF pattern (TagWritten event = microcode AIB write, emulator convention). |

Tests: `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests.ND100\ControllerOctobus\` (`OctobusControllerTests.cs` legacy TPE tests + `OctobusND5000Tests.cs` fabric/station/end-to-end; 50 passing).

**Not yet implemented**: shared-memory mailbox + X5FIF/X5SEMA protocol, multibyte MBSEND state machine on the ND-100 side, microcode-accurate AOBASR boot reads, ACCP command library beyond micro-commands 1-3, broadcast type codes, ident-entry process activation.

---

## 9. Corrections to prior analyses (read this before trusting older docs/code)

1. **Frame format**: the +5 write format is C=15/B=14/dest=13-8/E=7 (NPL-verified). The "C=9/B=8/dest=7-2 trace format" was an octal-truncation artifact (§3.1). Both the old code comment AND any doc quoting it are wrong; the old *code* was right.
2. **OctobusCommand enum**: values 0x10/0x20/0x40/... were fabricated (0x10 conflated with DCONT clear). Real codes are the CM* symbols (§5); CMMACLE/CMACONT are *emergency frames*, CMCPURES is a *multibyte message*, kicks are *K-frames* — three different mechanisms, not one command register.
3. **Kick constants**: IDLEKICK=6, CLRKICK=3, N100KICK=1 (kick numbers on the wire), NOT command codes. Verified three ways (§5).
4. **"ACCP microcode" does not exist** — the ACCP runs 68000 firmware from EPROM; the only microcode is the ND-5000 control store (which the ACCP loads via LOCSD/LOCSM).
5. **Interrupt ident/level for the 3109**: ~~genuinely contradicted~~ **RESOLVED live 2026-07-15** (section 6.2) — idents are **40B/41B on level 13** (interfaces 2-4: 42B/43B, 44B/45B, 46B/47B), verified by TPE OCTOBUS B00 LIST-OCTOBUS-DEVICES AND CONFIGURATION D05 ("Expected identcodes: 40B and 41B"). The ITB13 "37B/40B" reading confused slot index with ident code (plausibly a table-indexed-from-ident-1 offset); the 60B/61B formula stays refuted.
6. **5015 CONTROL II is not part of any ND-5000 system** — it is the ND-500/1 CPU controller #2. On ND-5000 its ND-100-interface role is taken by the octobus/ACCP; its other roles (prefetch control, micro clock, arithmetic control) are absorbed by the ND-5000's own IAC/IDU/MIC units.

## 10. Open questions

1. ~~Octobus ident codes / interrupt level~~ **RESOLVED live 2026-07-15** (section 6.2): level 13, idents 40B/41B for interface 0 and 42B-47B for interfaces 2-4 (TPE B00 + CONFIGURATION D05; all four interfaces covered). Remaining sub-question: verify the ITB13 slot-to-ident offset (entry for ident N at ITB13+N-1?) against a known device, e.g. the RTC (ident 1).
2. Broadcast type (BT) code assignments — referenced "OCTObus Protocol Specification" document never found. (MBSEND sets the B bit when descriptor word X+2 != 0 — mechanism carved, meaning still unknown.)
3. ~~SOCTO/SOCTW/SKICK/MBSEND/OMBREAD routine bodies~~ **RESOLVED** (§6.4): carved from 026-S3IMPIT.
4. ACCP EPROM dump (would allow full firmware analysis; ch. 5.3 documents the command surface).
5. Receive-side status register bits beyond bit 3 (carve confirmed bit 3 only; control values 4/1 match transmit-enable/interrupt-enable bits).
6. What runs on octobus levels 10/11 (OCSTART wires OCD10/OCD11/OCD12; OCD12/OCD10/OCD11 bytes in the carved image show no octobus link-element role).
7. Exact kick-word encodings the microcode SENDS (SENKICK builds `100102|cpu`-pattern words; mapping to receiving-side kick numbers is [I]).
8. Datafield initialization chain: how the octobus datafield B-pointer is established at interrupt entry (ident-entry vector at 123501 points into zeroed memory in this image — filled at boot).
9. Stale docs to correct: `324B-OctobusFunction/README.md` ("driver not present in any carved segment" — now false). (`OCTOBUS-PROTOCOL-REFERENCE.md` section 9's idents 40/41 turned out to be CORRECT — live-verified 2026-07-15 — so that entry is withdrawn.)
