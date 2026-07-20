# ACCP DUCS / CMRWC / CMWWC / LPARP / VPARP — manual parameter layout (verbatim)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\ACCP-DUCS-COMMAND-MANUAL-SPEC-2026-07-19.md`
**Date:** 2026-07-19

## Primary source (the ONE manual that documents the ACCP command interface)

`E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md`
— Norsk Data **ND-05.020.1 EN**, *ND-5000 Hardware Description*, **Chapter 5 "THE ACCESS MODULE"**,
sections **5.3.10 – 5.3.20** (manual pages 123–131). This is the only manual in the trees searched that
specifies the ACCP↔ND-120 command bodies. Grade **[V]** = verbatim manual quote; **[I]** = inferred.

Command-code cross-reference (the numeric ACCP command bytes are NOT in the manual — they come from the
SINTRAN NPL symbol file `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\M06\N5000-SYMBOLS.SYMB.TXT`
lines 4467-4468: `CMRWC=000025`, `CMWWC=000023`, and the ACCP-library carve in
`030-S3SM5.asm` — see `DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md`):

| Manual name | SINTRAN name | Octal | Hex | Manual §  |
|---|---|---|---|---|
| Load Parameter Pointer (LPARP)     | CMLPA  | 021B | 0x11 | 5.3.15 |
| Verify Parameter Pointer (VPARP)   | CMVER  | 022B | 0x12 | 5.3.16 |
| Load Control Store Via Memory (LOCSM) | **CMWWC** ("Write Control Store") | **023B** | **0x13** | 5.3.18 |
| Dump Control Store Via Memory (**DUCS**) | **CMRWC** ("Dump/Read Control Store") | **025B** | **0x15** | 5.3.20 |

> Note: the manual's *own* name for the DUCS command is **"Dump Control Store Via Memory (DUCS)"**;
> SINTRAN's symbolic alias for the same 025B command is **CMRWC**. Likewise the manual calls the write
> path **"Load Control Store Via Memory (LOCSM)"** = SINTRAN 023B **CMWWC**. The manual DOES NOT print
> the numeric byte codes; those are established by the carve, not the manual.

---

## 1. The command-structure preamble — how parameters are passed (§5.3.10, page 123) [V]

> "The commands sent by the ND-120 to the ACCP can have parameters sent directly over the octobus, or
> transferred via shared memory in the MFbus memory. Most commands have short parameters, and are
> transferred directly. For those commands which can use both methods (i.e. directly or via the MFbus),
> the command itself is always passed directly. **The command LOAD PARAMETER POINTER is used during
> initialization to specify the location of the parameter area in shared memory.** The ND-120 can check
> that the ACCP agrees on this address by writing a 32-bit word in the parameter area and sending the
> command VERIFY PARAMETER POINTER."

> "The ACCP then reads its parameter area and returns the first 32-bit word found there. (The ACCP cannot
> access the MFbus memory when the microprogram is running, and an attempt to give an ACCP command with the
> parameters in the MFbus memory when the microprogram is running results in an error message from the
> ACCP.)"

> "The two types of parameters passing between the ACCP and the ND-120 is called *directly* and *via
> memory*. Both types are activated as multibyte messages. For commands with long parameters (as LOAD
> CONTROL STORE), the parameters are normally passed via memory since this takes the shortest time and it
> produces the least amount of traffic on the octobus."

§5.3.11 (page 124) [V]:
> "Most commands with parameters in the MFbus memory have only a single command byte in the message body.
> Some commands have direct parameters and instruct the ACCP to put data in the parameter field as a
> response (e.g. dump control store). **The parameter field is organized in 16-bit words**, since both the
> ND-120 and the ACCP are 16-bit processors. Direct parameters with several bytes always have the most
> significant byte first."

**Load-bearing consequence:** the "via memory" commands (LOCSM/023B and DUCS/025B) carry **NO address in
the command body**. The single 16-bit **parameter-area base address is established once, out of band, by
LPARP (021B)**, and every subsequent "via memory" command reads/writes *the parameter field at that
LPARP-established base*.

---

## 2. LPARP — Load Parameter Pointer (§5.3.15, page 128) [V]

> **5.3.15 Load Parameter Pointer (LPARP)**
> - **Direct parameters:** Parameter pointer (4 bytes)
> - **Memory parameters:** None
>
> "The address of the parameter area in the MFbus memory is given."
> - **Messack parameters:** None

So LPARP hands the ACCP a **4-byte (32-bit) MFbus physical address** = the base of the "parameter area".
This is the ONLY place a memory address is conveyed to the ACCP. (On the wire this was observed as
`00 01 80 00` = `0x00018000` = the X5OCT octobus-buffer window — see
`DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md` §Q2.)

## 3. VPARP — Verify Parameter Pointer (§5.3.16, page 128) [V]

> **5.3.16 Verify Parameter Pointer (VPARP)**
> - **Direct parameters:** None
> - **Memory parameters:** None
>
> "This command is used to verify that the ND-120 and the ACCP agree on where the parameter area is.
> Before the command is given, the ND-120 writes a 32-bit word in the parameter area. The ACCP reads and
> returns the word from its parameter area, and the ND-120 should then check if they are equal."
> - **Messack parameters:** Test pattern (4 byte)
>
> | Messnak Error Codes | Description |
> |---|---|
> | -1 | Illegal when microprogram is running. |
> | 1  | No parameter pointer is given |

VPARP is a pure self-consistency echo of the FIRST 32-bit word of the LPARP-pointed parameter area. It
carries no address and does not read control store. (Confirms the skill's "VPARP is a §5.3.16
self-consistency echo, NOT a version read".)

---

## 4. CMWWC / LOCSM — Load (Write) Control Store Via Memory (§5.3.18, page 129) [V]

> **5.3.18 Load Control Store Via Memory (LOCSM)**  [= SINTRAN 023B CMWWC "Write Control Store"]
> - **Direct parameters:** None
> - **Memory parameters:**
>
> | 15 … 0 |
> |---|
> | μI word count (N) |
> | Control store address |
> | μI word 0, Bits 127-112 |
> | μI word 0, Bits 15-0 |
> | μI word 1, Bits 127-112 |
> | μI word 1, Bits 15-0 |
> | ... |
> | μI word N, Bits 127-112 |
> | μI word N, Bits 15-0 |
> | Checksum addend |
>
> "Load control store via memory. While loading, the checksum is calculated by 16-bit addition of all the
> words including the checksum addend. If the result is zero, the loading is assumed to be OK and *Messack*
> is returned."
> - **Messack parameters:** None
> - **Messnak error codes:** -1 Illegal when microprogram is running / 1 No parameter pointer is given /
>   4 Checksum error

**No address in the command.** The whole block — count N, target CS address, the N microwords (8×16-bit =
128 bits each), and the checksum addend — lives in the **parameter area** located by the LPARP pointer.

---

## 5. CMRWC / DUCS — Dump (Read) Control Store Via Memory (§5.3.20, page 131) — THE TARGET COMMAND [V]

> **5.3.20 Dump Control Store Via Memory (DUCS)**  [= SINTRAN 025B CMRWC]
> - **Direct parameters:** None
> - **Memory parameters:**
>
>   | 15 … 0 |
>   |---|
>   | μI word count (N) |
>   | Control store address |
>
> "Dump control store via memory. While dumping, the checksum is calculated as for loading. **The ND-120
> should read the memory parameter field after receiving *Messack*.**"
>
> Dumped data in memory:
>
>   | 15 … 0 |
>   |---|
>   | μI word 0, Bits 127-112 |
>   | ... |
>   | μI word 0, Bits 15-0 |
>   | μI word 1, Bits 127-112 |
>   | ... |
>   | μI word 1, Bits 15-0 |
>   | μI word N, Bits 127-112 |
>   | ... |
>   | μI word N, Bits 15-0 |
>   | (Checksum addend) |
>
> - *Messnak* error codes: -1 Illegal when microprogram is running / 1 No parameter pointer is given /
>   5 Control store error in buffered CI-bits
>
> "NOTE: Microinstruction (16 bytes) and checksum addend (2 bytes) are written to memory at *Messnak* 5."

### Sibling for contrast — DCSD, Dump Control Store DIRECTLY (§5.3.19, page 130) [V]
> - **Direct parameters:** CS address (2 bytes) — (this is the *direct* variant; ONE microinstruction is
>   returned inline in the octobus reply, no memory buffer. DUCS is the *via-memory* variant.)

---

## 6. ANSWER: how is the read-back buffer ("region-25") ADDRESS passed to the ACCP?

**DUCS does NOT carry a memory address.** Verbatim, its only parameters are two 16-bit words placed in the
parameter field: **μI word count (N)** and **Control store address** (the *source* CS address to dump
FROM — a microstore index 0..64K, not a memory address). There is no buffer-pointer field in the command.

**The destination buffer address is the LPARP parameter pointer, dereferenced by the ACCP.** Mechanism,
straight from §5.3.10 + §5.3.15 + §5.3.20:

1. **Once, at init:** `LPARP (021B)` gives the ACCP a **4-byte MFbus physical address = the parameter-area
   base**. ("The address of the parameter area in the MFbus memory is given." §5.3.15) [V]
2. `VPARP (022B)` round-trips a 32-bit word through that same area to confirm both sides agree on the
   base. [V]
3. **Per DUCS call:** the ND-120 writes `{N, CS-address}` into the parameter field (at the LPARP base),
   then sends the bare `DUCS (025B)` command byte. [V]
4. The ACCP dumps the N microwords **into the parameter field** — i.e. into the SAME LPARP-pointed area,
   as the "Dumped data in memory" table above — followed by the checksum addend, then returns *Messack*.
   "The ND-120 should read the memory parameter field after receiving *Messack*." [V]
5. The ND-120 reads the dumped block back out of that area. [V]

So: **the buffer address is NOT in DUCS; it is the pointer established by LPARP, which the ACCP
dereferences.** This is exactly the parameter-pointer/dereference model the GOAL asked about. "region-25"
(the SINTRAN-side name for the read-back block) is a sub-region *inside* the LPARP-pointed parameter area.

### What the manual does NOT pin down (firmware-internal) [stated explicitly]
The manual gives the parameter-area layout as an ordered list of 16-bit words (count, CS-addr, then the
dumped words). It does **not** document:
- the exact byte OFFSET of the read-back block ("region-25") vs. the input `{N,CS-addr}` header inside the
  parameter area — the "Dumped data in memory" table restarts at word 0, so whether the dump overwrites the
  2-word header or follows it is **not stated in the manual**;
- any secondary descriptor/pointer list inside the parameter area (SINTRAN's carve sees a
  `{descriptor,base}` pair list at the LPARP base that separately names regions 23/24/25/26 — see
  `DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md` §Q2 `[UNVERIFIED]`). That descriptor format is a
  SINTRAN/ACCP-firmware convention, **not** in ND-05.020.1.
- the ACCP's 128 KB MC68000 EPROM firmware has **never been dumped** (per the octobus-nd5000 skill), so the
  exact in-parameter-area addressing is only knowable from the SINTRAN side (the carve), not from any
  Norsk Data manual.

**Bottom line for the emulator:** service DUCS/CMRWC the same way CMWWC is already serviced — locate the
parameter area from the LPARP pointer (the base VPARP already validates), read `{N, CS-addr}`, write the
N×(8×16-bit) microwords + checksum addend back into that area, reply Messack. The manual authorizes exactly
this; the precise sub-offset of the read-back block is a SINTRAN-carve detail, not a manual-documented one.

---

## Manuals checked (and result)

| File | Result |
|---|---|
| `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.020.01 EN ND-5000 Hardware Description.md` | **HIT** — Ch.5 §5.3.10-5.3.20 fully documents LPARP/VPARP/LOCSM/DUCS parameter layouts (quoted above). |
| `E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware Description.md` | Same manual (duplicate copy in ND5000UC tree). |
| `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md` | Mentions control store / octobus but does NOT specify the ACCP command bodies. |
| `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md` | Cross-refs the manual; confirms LPARP/VPARP/LOCSD/LOCSM naming, no independent param layout. |
| SINTRAN NPL symbols `...\SYMBOLS\M06\N5000-SYMBOLS.SYMB.TXT` | Source of the numeric codes `CMRWC=000025`, `CMWWC=000023`. |
| Carve `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\DUCS-READBACK-REGION-OWNERSHIP-CARVE-2026-07-19.md` | SINTRAN-side region-25/26/23/24 ownership + LPARP=0x18000 wire evidence (corroborates §6). |
