# ENNS0 START-NETWORK-SERVER: the 68K PRKEY fix point - RESOLVED

Date: 2026-07-23. Builds on `ENNS0-POLL-FINDINGS.md` and
`ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`.
Legend: **[V]=VERIFIED** (decoded bytes / read source) - **[I]=INFERRED**.

Firmware image analysed:
`E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`
(MC68000, big-endian, base 0x0, 512 KB, all banks loaded).

---

## THE ONE-LINE ANSWER (the fix)

[V] The firmware writes PRKEY (0x5473) into DRAM word 1002B (68K byte **0x404**) from
**exactly one instruction**, `0x1CF4: move.w #$5473,($00000404).L`, inside the
datafield-pointer-setup subroutine at **0x1C6A**. That subroutine is called from
**exactly one site**, `0x1DA6: jsr $1C6A`, which sits inside **reset_entry (0x1CFE = the
68K reset vector, vec1)**. reset_entry reaches 0x1DA6 **unconditionally on both the warm
and cold restart paths**. There is **no command / handshake / mailbox path** that ever
reposts PRKEY.

[V]+[I] Therefore the firmware reposts PRKEY **if and only if the 68K CPU actually takes
its reset vector (restarts at 0x1CFE)**. The hang means PISTA's `PWCR=60B` (HALT AND
RESET) is **not causing the emulated 68K to restart at 0x1CFE**; the CPU stays
halted/stopped, reset_entry never runs, and PRKEY (and the STARTED flag at 0x4C0) are
never refreshed, so PISTA's 3-second readiness poll on word 1002B times out.

**FIX POINT (emulator, `NDBusEthernetII.cs`):** the ND-100 control-register (`HDEV+3` =
PWCR) write handler must, on `PWCR=60B` (HALT+RESET) followed by `PWCR=0` (INITIATE),
perform a **real 68K CPU reset** - reload SSP from vector 0 (0x000005C8) and PC from
vector 1 (0x00001CFE) and resume execution. That single change re-runs reset_entry ->
`jsr $1C6A` -> reposts PRKEY@0x404, which is exactly what PISTA is waiting for. DRAM must
**not** be wiped (see "warm vs cold" - the firmware clears its own warm-boot magic and
still reaches the PRKEY call on either path); a plain reset-vector restart is sufficient
and correct.

---

## Task B - byte-level evidence

### B1. The single PRKEY write and its address cell [V]
Byte-search of the whole 512 KB image:
- immediate `0x5473` (PRKEY) appears **once**, at file offset 0x1CF6.
- abs32 operand `0x00000404` appears **once**, at 0x1CF8.
- abs32 `0x00000402` (datafield-ptr cell, word 1001B): at 0x2690, 0x3508 (unrelated
  producer/consumer of the datafield pointer; NOT the PRKEY gate).
- abs32 `0x0000040A` (monitor postbox): 20 refs (the postbox handler - not PRKEY).

Decoded instruction (VERIFIED bytes `33FC 5473 0000 0404`):
```
0x1CF4: move.w #$5473,($00000404).L    ; post PRKEY into PIOC word 1002B
0x1CFC: rts
```

### B2. The routine that posts it: 0x1C6A "setup_datafield_pointers_and_post_prkey" [V]
Disassembled 0x1C6A..0x1CFC. It builds a pointer table at A0 (=0x4CA) whose fields point
at the postbox/datafield cells, then posts PRKEY as its final act, then RTS:
```
0x1C6A: lea ($4CA).L,A0            ; A0 = pointer-table base
0x1C70: lea ($40A).L,A1  ; move.l A1,(4,A0)     ; -> monitor postbox
0x1C7A: lea ($406).L,A2  ; move.l A2,(A0)       ; -> REQUEST cell (0x406)
0x1C82: lea ($454).L,A3  ; move.l A3,(8,A0)     ; -> CPU-dump frame
        ... (0x49E, 0x4B6, 0x4BA, 0x4BE, 0x4C0, 0xBB6, 0x4C2, 0x4C6, 0xA8A) ...
0x1CC0: lea ($4C0).L,A1  ; move.l A1,(28,A0)    ; -> STARTED flag cell
0x1CF4: move.w #$5473,($404).L                  ; **POST PRKEY**
0x1CFC: rts
```

### B3. Its one caller is reset_entry [V]
Full-image scan for jsr/bsr/jmp targeting 0x1C60..0x1CFC returns **one hit**:
`0x1DA6: jsr (-318,PC) -> $1C6A`. 0x1DA6 is inside reset_entry.

### B4. reset_entry (0x1CFE) reaches 0x1DA6 on BOTH restart paths [V]
Reset vectors (VERIFIED): vec0 SSP=0x000005C8, vec1 PC=0x00001CFE.
Branch trace of reset_entry:
```
0x1D26: cmpi.l #$55555555,($4BA).L        ; warm-boot magic test
0x1D30: bne   $1D58                        ; magic ABSENT -> cold path @1D58
  -- warm path (magic present, falls through) --
0x1D32: clr.l ($4BA).L                      ; clear warm magic
0x1D38: addq.w #1,($4BE).L                  ; restart counter++
0x1D3E: jsr $1A30 (nd_monitor_set_flag)
0x1D4C: move.w #4,(2,$40A)                  ; postbox code = 4
0x1D50: jsr $1A48 (post_and_signal_nd100_scip)  ; SCIP -> INT12, code 4
0x1D54: jsr $1A12
        (falls through into 0x1D58)
  -- common path @1D58 (cold target AND warm fall-through) --
0x1D58..: build 0x454 dump frame; clear MERRSTAT (0x1D84 move.b #0,$EF0040);
          install vector 0x78; 0x1D9C jsr $1AD4; 0x1DA0 jsr $396A (init_mfp);
0x1DA6:  jsr $1C6A                            ; **-> posts PRKEY@0x404** (ALWAYS reached)
0x1DAA:  clr.l ($4BA).L
0x1DB0:  move.w #1,($4C0).L                   ; STARTED flag = 1
0x1DDC:  move.l #$55555555,($4BA).L           ; re-arm warm magic for next reset
```
So the warm/cold distinction only decides whether the firmware first signals the ND-100
with monitor-code 4 and bumps a restart counter; **both paths call 0x1C6A and repost
PRKEY.** The earlier hypothesis that PRKEY is "gated behind a one-time warm-boot flag" is
**refuted** [V]: warm restart still reposts PRKEY.

### B5. Conclusion on the crux question [V]+[I]
- [V] There is NO command-driven repost of PRKEY (0x404 has a single writer, reachable
  only from the reset vector).
- [V] A genuine 68K reset (PC<-0x1CFE) reposts PRKEY every time, warm or cold.
- [I] Since the live trace shows PRKEY posted once (the initial `rt enns0` cold boot) and
  never again after PISTA's `PWCR=60B`+`PWCR=0`, the emulator is not restarting the 68K at
  0x1CFE on that HALT+RESET. That is the missing behaviour and the fix. (Confirming the
  exact current PWCR handling requires reading `NDBusEthernetII.cs` in the RetroCore repo,
  which is outside this analysis image; the handoff note "68K STOPs and needs OPCOM to
  wake / control-bit reset paths reverted" is consistent with the reset line not being
  driven.)

---

## Task A - the .prog loader and identity (premise corrected)

### A1. .prog format [V]
`encos-in-b01.prog` = 183174 bytes = 91587 ND words, big-endian. A ~256-word header
(mostly zero; nonzero control words at word 1=1, word 3=0x88DD, word 5=0x64C2 - undocumented
per `SINTRAN\File-Formats\PROG-FILE-FORMAT.md` which leaves the header layout UNVERIFIED),
then the memory image begins at word 256 (octal 400). A trivial BE-word reader is enough to
scan it; no special loader beyond skipping to word 256.

### A2. Identity: encos-in is XCOM, the INSTALLER command processor - NOT ENNS0 [V]
Raw ASCII strings decisively identify it:
```
(PACK-FOUR-6728:ENCOS-B00)ENCOS-IN-B01:XCOM
#XCOM Extended command processor
^type --> Checking Ethernet Controller in SINTRAN.
^type *  COSMOS Ethernet Option version <VER> ...
RENAME-BRF ENCOS-ERR-<ENCH>-<VER><REV>:BRF ENNS0 ENNS<ENCH>
```
So `encos-in-b01.prog` is the **XCOM extended-command-file interpreter** that runs the
COSMOS-Ethernet **installation** script (it renames `ENCOS-ERR-*:BRF` to the runtime name
`ENNS0`). It is the install driver, not the network server.

### A3. Consequence for the "T=6 START_P caller" hunt [V]
- [V] `encos-in-b01.prog` contains **zero** `MON 255` (0o153255) instructions in either
  byte order. It therefore does NOT itself issue the PIOCM START.
- [V] The actual runtime server ENNS0 (the linked `encos-err-i-b01.brf` image) is where the
  START orchestration lives; that was already decoded in the prior findings (server-start
  region 030200-030710, the 10 PIOCM wrappers at 032703-033147, `START_P`=T=6 at 033124).
  Nothing in encos-in supersedes that. The prior note that speculated "the START_P caller is
  in encos-in-b01.prog" is corrected here: encos-in is the installer (XCOM), and the START
  path is in the ENNS0/encos-err image already analysed.

Net: Task A adds identity/format facts but does not change the start-sequence picture -
the START-NETWORK-SERVER flow (SEGLOAD firmware banks -> START_P) is in ENNS0 as previously
documented, and the request word is kernel-supplied by PISTA (MPIOC=5), gated on PRKEY.

---

## Handoff to the emulator author
1. In `NDBusEthernetII.cs`, on the control-register write path (`HDEV+3`/PWCR):
   - `PWCR=60B` (0x30) = HALT AND RESET  -> assert 68K RESET (and hold).
   - `PWCR=0`         = INITIATE         -> release, and restart the 68K at its reset
     vector: SSP=word[0]=0x000005C8, PC=word[1]=0x00001CFE.
2. Do NOT clear the 68K DRAM on this reset - reset_entry manages the warm-boot magic at
   0x4BA itself and still reposts PRKEY on the warm path.
3. Expected result: reset_entry runs -> `jsr $1C6A` -> `move.w #$5473,($404)` reposts
   PRKEY; PISTA's readiness poll on word 1002B sees PRKEY, writes MPIOC=5+TRIG=1 and rings
   the start doorbell (PWCR=11B); START-NETWORK-SERVER proceeds.
4. Regression guard: TPE ETHERNET-TWO tests 1-11 must still pass (esp. test 11).

## Source paths
- Firmware: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`
- Installer image: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\encos-in-b01.prog` (=XCOM)
- Kernel driver (PISTA): `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL`
- Tooling (scratchpad): `m68kdis.py` (minimal MC68000 disassembler, this session),
  `nd100dis.py`, `brf_link.py`
- Prior findings (scratchpad): `ENNS0-POLL-FINDINGS.md`,
  `ENNS0-PIOCM-START-FINDINGS-2026-07-23.md`
