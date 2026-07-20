# SINTRAN-Side Octobus Message Catalog (ND-100 -> ND-5000 / MF-controller)

What SINTRAN III (M06 generation) actually sends and expects on the octobus,
cataloged from the NPL source, the M06 symbol tables and the carved L-VSX-500
driver bodies. This is the "what will the OS throw at our emulated stations"
reference for RetroCore.

Evidence markers: [V] = verified in source/symbols/carve, [I] = interpretation,
[UNCERTAIN] = explicitly open.

Companion docs:
- [OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md) - frame format, CM* codes, kicks, idents
- [OCTOBUS-TEST-PROTOCOL-RE.md](OCTOBUS-TEST-PROTOCOL-RE.md) - the OMD-0 test protocol (TPE, not SINTRAN)
- [HANDOFF-OCTOBUS-EMULATION.md](HANDOFF-OCTOBUS-EMULATION.md) - emulation plan phases 1-4

Primary sources (all paths repo-relative):
- `../NPL-SOURCE/NPL/MP-P2-N500.NPL` - kick sender, multibyte builders, receive dispatcher
- `../NPL-SOURCE/NPL/RP-P2-N500.NPL` - 5PIT bring-up ladder (XX5CONOMD)
- `../NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL` - SINTR startup probe (CH5CPUPRESENT), OCSTART tables
- `../NPL-SOURCE/NPL/CC-P2-N500.NPL` - monitor-level entry points (CALLROUT), 5OCTOSWITCH
- `../NPL-SOURCE/SYMBOLS/M06/` - constant values (SYMBOL-1-LIST, N5000-SYMBOLS)
- `../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/026-S3IMPIT/026-S3IMPIT.asm` - carved L driver bodies

---

## 1. The multibyte message descriptor (LMFIELD record) [V]

Every SINTRAN-built octobus multibyte message is described by a word record
(built in the working field "LMFIELD", handed to MBSEND). Word offsets from
the M06 symbol tables (values octal):

| Offset | Symbol (5-char) | Field | Notes |
|---|---|---|---|
| 0 | MOCTS | MOCTSTATION | destination (or reported source) station number |
| 1 | MOCTO | MOCTOMD | destination OMD number |
| 2 | MBROA | MBROADCAST | 0 = not broadcast |
| 3 | MMSGL | MMSGLENGTH | message length in BYTES |
| 4 | MCOMM / MSTS | MCOMMAND / MSTS | command or status byte in the HIGH byte (NPL idiom `CMxxx SHZ 10 =: X.MCOMMAND`) |
| 5 | MDP1 | MDP1 | first parameter word |
| 4 (recv) | ETYPE | ETYPE | received: high byte = SEC/status, low byte = reporting source |

Received-record extras (5OMBREAD / 9FLER): the L07 carve (2026-07-19,
OCTOBUS-DRIVER-ROUTINES-CARVE.md) byte-proves the 9FLER record stores the
SEC code at **LMREC+2** and the source station at **LMREC+3** [V]. The
earlier "EOCTSOURCE = offset 1 / ESECCODE = offset 0" reading here came
from M06 symbols and may be relative to a different record base -
[UNCERTAIN] until re-verified against M06 bytes; for L07 use +2/+3.
`N5SECCODE` = 2000B is OR-ed into the SEC code as the "ND-500 class" tag. [V]

MBSEND takes the record plus a physical address/bank (`"LMFIELD+DPITPHYS"=:D;
A:=DPITBANK`) and puts the message on the wire; the multibyte wire envelope
itself (SOMB 0x30|OMD, srcOMD, byte count, payload bytes, EOMB 0x20|OMD) is
byte-verified in [OCTOBUS-TEST-PROTOCOL-RE.md](OCTOBUS-TEST-PROTOCOL-RE.md)
section 1. [V]

## 2. Station and OMD assignments [V]

From M06 SYMBOL-1-LIST / N5000-SYMBOLS (values octal):

| Symbol | Value | Meaning |
|---|---|---|
| (card) | 1 | ND-100 CPU station |
| FMFDEST | 2 | first MF-controller station |
| LMFDEST | 6 | last MF-controller station |
| FN5DEST | 70 | first ND-5000 (SAMSON) CPU station |
| LN5DEST | 73 | last ND-5000 CPU station in M06 (4 SAMSONs). **L07 = 77B** [V 2026-07-19 carve, 5OMBREAD range check] - the hardware station table allows 70-76B; per-build constant |
| OMDACCP | 3 | OMD on the SAMSON consumed by the ACCP command library |
| MFOMDNO | 4 | OMD on the MF-controller for host messages |
| N100IDENT | 1 | the ND-100's octobus IDENT number |
| CMSYSPAR | 16 | command: write system parameters |
| CMCPURES | 71 | command: reset CPU |
| CMMACLE | 41 | (emergency base) master clear |
| CMACONT | 42 | (emergency base) continue ACCP |
| MFACK | 0 | ack status |
| MFNACK | 377 | nack status |
| N100KICK | 1 | kick: activate ND-5000 process |
| CLRKICK | 3 | kick: execute clear-functions mask |
| IDLEKICK | 6 | kick: save context / go idle |
| N5SECCODE | 2000 | SEC-code class tag for ND-500 errors |

Per-CPU station assignment: `X.CPUNO + FN5DEST - 1 =: X.5STATION`
(RP-P2-N500.NPL:976), i.e. SAMSON CPU n sits at station 70B + n - 1. [V]

The ND-100's own receive OMD (`5OMDNO`, offset 0 of the driver datafield LMDF)
is allocated at runtime by CONOMD - it is NOT a fixed constant. [V]

## 3. Kicks (single control frames) [V]

Send path: `XKICK500` (MP-P2-N500.NPL:3278-3316). If not already on level 12
it switches there (IRW LV12B, MST PID), then `LV12KICK` runs
`T:=5STATION; X:=OCTORING; A:=CKICKTYPE; CALL SKICK`. Errors log 9ERR(#99).
Return to the calling level goes via a level-14 helper (LV14KICK) that
temporarily masks lower levels. [V]

| Kick | Value | Sent by | When |
|---|---|---|---|
| N100KICK | 1 | XACTRDY (ACT52, MP-P2-N500.NPL:3032) | activate the ND-500: a message is waiting and a CPU is idle or lower-priority |
| CLRKICK | 3 | LMPCLR (MP-P2-N500.NPL:1231), ST0PSYS (MP-P2-N500.NPL:3769) | execute the clear-functions mask just written to mailbox word X5CLR (clear tsb/cache/dump/forget-process; ST0PSYS writes mask 77B) |
| IDLEKICK | 6 | XTER500 TER51 loop (MP-P2-N500.NPL:2950) | ask a running CPU to save context and go idle (termination), polled with GETC5PROC |

Kick frame format (C=1, K=1, kick number in low bits) and the microcode
dispatch (OCB_DEC_K) are in
[OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md). [V]

## 4. Multibyte messages the ND-100 SENDS [V]

### 4.1 XRS5CPU - "Reset CPU" (MP-P2-N500.NPL:3328-3343)

To (5STATION, OMDACCP=3), not broadcast, MMSGLENGTH=1:
`MCOMMAND = CMCPURES(071B) << 8`. Sent via MBSEND on level 12. Used by
RS5CPU (level-1 5PIT) for every alive-but-inactive SAMSON. [V]

### 4.2 MFPREPARE - give the MF-controller our OMD (MP-P2-N500.NPL:3586-3598)

To (MF station 2..6, MFOMDNO=4), MMSGLENGTH=3:
`MCOMMAND = CMSYSPAR(016B) << 8 | N100IDENT(1)`, `MDP1 = 5OMDNO << 8`.
"Sends OMD number to mf-controller on which N100 can receive messages."
Ack/Nack answer handled by 5OMBREAD. [V]

### 4.3 CON5IDENT - the SAMSON LSYSPAR handshake (MP-P2-N500.NPL:3614-3634)

Per alive SAMSON CPU, from level 1 via level 12:
1. `ECONID` connects octobus IDENT N100IDENT(1) to the LV12 driver
   (`N100IDENT=:D; LV12B SHZ -3; CALL ECONID`). [V]
2. Builds to (5STATION, OMDACCP=3), MMSGLENGTH=7:
   `MCOMMAND = CMSYSPAR(016B) << 8 | N100IDENT(1)`,
   `S5 = 5OMDNO << 8`, `S6 = S7 = 0`; CALL MBSEND. [V]

This is the "write system parameters" (manual LSYSPAR, ND-05.020.01 ch.
5.3.13) message: it tells the ACCP which station/ident/OMD the host listens
on. Comment in source: "Send 'alive' message to the ACCP to verify that
it's present". [V]

**The ACCP MUST ANSWER this message with an Ack** - 5OMBREAD treats the
Ack/Nack "on 'WriteSysPar' message" as "I'm present" and only then sets
`CPUAVAILABLE BONE 5ALIVE` (MP-P2-N500.NPL:3467-3471). No ack = SAMSON
never becomes alive to SINTRAN. [V]

### 4.4 MF error-record acknowledge (MP-P2-N500.NPL:3523-3532)

After forwarding an MF-controller error record to 9FLER, if the source was
an MF station (2..6): reply to (source station, MFOMDNO=4), MMSGLENGTH=1,
`MSTS = MFACK(0) << 8`. "Mf-controller requires ack message." [V]

## 5. Messages the ND-100 RECEIVES - 5OMBREAD dispatch [V]

5OMBREAD (MP-P2-N500.NPL:3453-3536) is activated when a multibyte message
arrives on the reserved OMD (5OMDNO). It reads the message with OMBREAD and
dispatches on the SOURCE station in MOCTSTATION:

**Source in FN5DEST..LN5DEST (a SAMSON, 70B-73B):**
- `ETYPE` high byte =: CSTS (status/SEC), low byte =: CMICP (reporting source).
- CSTS=MFACK(0) or MFNACK(377B): Ack/Nack on WriteSysPar -> set 5ALIVE, done. [V]
- Known error codes (CSTS masked 17B < 17B) indexed into MPFATAL
  `(1,0, 0,1, 0,0, 0,0, 0,1, 1,0)` - a nonzero entry forces
  `XRSTARTALL` with `CSTS \/ N5SECCODE`. [V]
- CMICP=1 (microprogram) with CSTS=200B (HW-fault) or 201B (general trap):
  the record carries the extended mp error record (process no, trapping/
  restart P, trap no, MMS status, logical/physical address, ...); the
  shadow-process id is patched in and the record goes to 9FLER. [V]
  Record layouts are spelled out in the 5OMBREAD header comment
  (MP-P2-N500.NPL:3390-3450): accp record (errcode 200B hwfault,
  errtype 1=accp), mp record (errtype 2), mf record (etype + emainstat +
  errlog1-4 + master/slave/address/syndrome). [V]

**Source in FMFDEST..LMFDEST (MF-controller, 2-6):**
- ETYPE=MFACK: ignored (ack for something we sent). [V]
- Else: error record -> 9FLER; SEC code = etype; MF codes documented in the
  header comment: 14B+ prefix, then 20B corrected memory error, 30B memory
  timeout, 31B unknown error, 50B memory write parity, 51B memory I/O error,
  77B fatal MF-controller error. Reply MFACK (section 4.4). [V]
- Octobus read errors from OMBREAD itself (A != 101410B) are logged as SEC
  code with station 0. [I on the 101410B magic - looks like the "no message"
  status, not verified]

## 6. Startup sequences [V]

### 6.1 SINTR cold start - CH5CPUPRESENT (PH-P2-OPPSTART.NPL:3903-3945)

Per generated ND-500 CPU datafield:
1. Try the OLD ND-500 interface first (IOXT on HDEV+RSTA5 with IOX-error
   trapped): success -> mark OLD500 + 5ALIVE. [V]
2. Otherwise probe the octobus card: IOXT read 100406 (output controller
   status) with IOX-error trapped. Present -> busy-wait until status bit 3
   (ReadyForTransfer) is set, then:
   - `5STATION := ASTATION \/ COMD` (station for this CPU df),
   - send emergency `(station << 8) | CBIT | EBIT | CMMACLE` (= master clear
     Samson system) via IOXT write 100405,
   - send `... | CMACONT` (= continue accp, starts the ACCP self-test),
   - set MIFLAG MUDOM bit and CPUAVAILABLE := SAMSON. [V]

This is the origin of the live-verified LIST-HA-CO probe pattern (242B
emergencies) and of the "kick 1/3/6 three-way" bring-up. Note SINTRAN talks
to the card DIRECTLY with IOXT here - the driver tables are not up yet. [V]

### 6.2 OCSTART - octobus driver tables (PH-P2-OPPSTART.NPL:4032-4082 + CENTRY/CBPOOL)

OCSTART (called from the startup chain at PH-P2-OPPSTART.NPL:1409) handles
ONLY interface 0 ("THIS OCTOBUS DRIVER ONLY HANDLE ONE OCTOBUS INTERFACE
(DEVICE 0)", source comment). It allocates physical memory for:
- receive buffer pool (CBPOOL: MBYA1/MBYA2/MBLINK descriptor chain, OBSIZE
  bytes per buffer),
- ident-entry tables per source station (SOUIDEN, stations 0..77B, each
  entry = OLINK/DLEVE/DFADD triplet),
- 16 OMD entries (OMDENT(0..17B)) and 16 kick entries (KICKENT(0..17B)),
  each also OLINK/DLEVE/DFADD. [V]

The OMDENT/KICKENT tables are how incoming multibyte messages and kicks are
routed to driver datafields on level 12; CONOMD allocates an OMD entry and
returns its number (=> 5OMDNO), ECONID binds an ident entry to a level. [V,
table mechanics; exact entry semantics beyond the three zeroed words are
[UNCERTAIN] pending carve analysis of CONOMD/ECONID bodies]

### 6.3 5PIT warm bring-up - XX5CONOMD (RP-P2-N500.NPL:944-999)

Runs on the 5PIT (level 1) with per-step level-12 calls and MON 2HOLD waits:
1. `CON5OMD` once (if 5OMDNO=0): connect the receive OMD. [V]
2. If connected: `MFPREPARE(station)` for EVERY MF station 2..6 -
   absent controllers simply never answer. [V]
3. Per SAMSON CPU df: set 5STATION, then `CON5IDENT` (section 4.3);
   the Ack routed through 5OMBREAD sets 5ALIVE. [V]
4. Count NSAMSON / N5CPU; a CPU with HDEV=660B is disabled on logical
   device number overlap (546D terminal group 4 check). [V]

## 7. Monitor-level entry points and the level switch [V]

- CC-P2-N500.NPL:179-203: monitor-level names KICK500 / TER500 / IACT500 /
  ACTRDY / RSTARTALL / 5CONOMD dispatch through CALLROUT to the MP-P2
  routines (0KICK500, 05CONOMD, ...). [V]
- CC-P2-N500.NPL:612-622: 5OCTOSWITCH - the register-preserving trampoline
  used to jump between the octobus command context and Mpit routines. [V]
- ST0PSYS (STOP-SYSTEM command, MP-P2-N500.NPL:3759+): writes clear-mask 77B
  into mailbox X5CLR then CLRKICK per active SAMSON, then polls X5CLR until
  the ND-5000 has executed the functions (bounded loop 1000B). [V]

## 8. Carved driver bodies (L-VSX-500, 026-S3IMPIT) [V addresses]

In-kernel L addresses (carve) vs M06 symbol addresses (all octal):

| Routine | L carve @ | M06 symbol | Role |
|---|---|---|---|
| SOCTO | 035546 | SOCTO=036176 | send single octobus frame |
| SOCTW | 036342 | SOCTW=036772 | send octobus frame + wait [I from name] |
| SKICK | 037254 | SKICK=037704 | send kick; carve shows `BSET ONE SSK` then a body SHARED with SIDEN (037256 `BSET ZRO SSK`) - kick vs ident is a one-flag variant of the same sender [V] |
| MBSEND | 037425 | MBSEN=040055 | send multibyte message. **NO IOXT in MBSEND** [V 2026-07-19]: validates (station 1..76B, OMD <=17B, length 1..255 bytes), pops a CBPOOL buffer, queues on the TX datafield, and if idle fires level 13 with P:=SOCTW (036342) to transmit. The IOXT block at 037320+ belongs to SKICK's direct-TX path (control:=4 / frame / control:=1), not MBSEND - the earlier reading of this row was wrong. Full body: OCTOBUS-DRIVER-ROUTINES-CARVE.md |
| OMBREAD | 037660 | OMBRE=040310 | read received multibyte message into buffer |
| CONOMD | 040062 | CONOM=040512 | connect/allocate an OMD entry |
| ECONID | 040467 | ECONI=041117 | connect an ident entry to a level |

Full instruction-level RE of these bodies is still OPEN - the calling
contracts above (registers in/out, record layouts) are what the emulator
needs and are NPL-verified.

## 9. ND-500-MON J04 [I]

The ND-500 monitor J04 symbol area contains `AEOCTOBUS` (address-error
source enum SINTRAN/OCTOBUS/NUCLEUS) and `PMMESSACK`/`PMMESSNAK` - the
octobus generation is represented in its error/ack model, but no octobus
driver paths in J04 have been analyzed yet. See
`../ND500/nd-500-mon/nd-500-mon-j04.prog.md`. [I]

## 10. What this means for the RetroCore emulation

Ordered by when SINTRAN will hit it:

1. **CH5CPUPRESENT probe**: card must IOXT-respond on 100406 with RFT
   (bit 3) and accept emergency 241B/242B to the SAMSON station.
   IMPLEMENTED (OctobusND5000Station handles both emergencies). [V]
2. **CONOMD/ECONID**: pure ND-100-side table management - no wire traffic
   expected [I from NPL; verify when carving the bodies].
3. **MFPREPARE to stations 2-6**: no MF-controller emulated -> frames to
   absent stations must produce Ack=00 timeout (implemented in
   OctobusFabric). [V]
4. **CON5IDENT / CMSYSPAR to (SAMSON, OMD 3)**: OctobusND5000Station
   already parses CMSYSPAR and stores the system parameters - BUT it does
   NOT yet send the Ack multibyte back. **Without that Ack, 5OMBREAD never
   sets 5ALIVE and the SAMSON stays dead to SINTRAN** ("No ND-500(0) CPU
   found"). **Reply shape RESOLVED [V], 2026-07-18** - see
   [CARVE-ANSWER-OCTOBUS-CPU-PRESENCE-2026-07-18.md](CARVE-ANSWER-OCTOBUS-CPU-PRESENCE-2026-07-18.md):
   send a multibyte message on OMD **5OMDNO** (= the CMSYSPAR body S5 field
   = byte[4] = 3) from the SAMSON station whose command/ETYPE word high byte
   = **MFACK(0)** (MMSGLENGTH=1, ETYPE=0x0000 suffices). 5OMBREAD then does
   `CPUAVAILABLE BONE 5ALIVE` at MP-P2-N500.NPL:3470 (146616). Boot's
   CH5CPUPRESENT sets only CPUAVAILABLE:=SAMSON (interface type), never
   5ALIVE - the alive handshake is @nd-500's CON5IDENT, not boot. [V]
5. **Kicks 1/3/6** to the SAMSON station: delivered via AOB with
   ATRAP+OMESS when kicks enabled. IMPLEMENTED. [V]
6. **ND-5000-originated error records** (hw-fault 200B / trap 201B) and
   **Messack/Messnak**: handoff phase 4.
7. **ACCP command library on OMD 3/4 (CS-load → microstart)**: RESOLVED [V]
   2026-07-18 — see
   [CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md](CARVE-ANSWER-OCTOBUS-CSLOAD-MICROSTART-2026-07-18.md).
   Two commands read back STATE and cannot be canned:
   - **VPARP (022B/0x12, §5.3.16)** must return **ack + the 32-bit word read
     back from MFbus memory at the LPARP (021B/0x11) pointer** (trace pointer =
     0x00018000). Canned/zero answer → "Verification of ACCP parameter pointer
     failed". Store the LPARP pointer, echo `_mpmRam[pointer]`.
   - **CMALI / ALIVE CHECK (037B/0x1F, §5.3.26)** encodes running-state:
     **Messack = micro RUNNING, Messnak-7 = NOT running**. After a 244B
     TERMINATE SINTRAN expects **Messnak-7**; a canned Messack → "ACCP was
     terminated; Microprogram is running". Track a `_microRunning` flag
     (TERMINATE/STOPMIC 034B/master-clear → false; STARTMIC 033B → true).
   - Observed OMD-3 init prologue (all else = Messack): DISKICK 062B, STOPMIC
     034B, CPURES 071B, LPARP 021B, VPARP 022B; then LOCSM, STARTMIC 033B.

---

*Created: 2026-07-17. Sources: SINTRAN III s3vs-4 NPL + M06 symbols +
L-VSX-500 carve. Author of record for follow-up: see
[HANDOFF-OCTOBUS-EMULATION.md](HANDOFF-OCTOBUS-EMULATION.md).*
