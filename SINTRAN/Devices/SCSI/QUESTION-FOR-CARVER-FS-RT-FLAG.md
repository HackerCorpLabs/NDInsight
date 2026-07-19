# Question for the SINTRAN carver: find the FILSYS RT "work-in-progress" flag and its DONE transition

Context: SINTRAN III L07 (L-VSX-500) on RetroCore. Live-verified bug: at boot from the
SCSI disk, the FILE SYSTEM RT program re-runs a RELEASE-USER operation forever (~4/s),
decrementing user SYSTEM's enter count in the user file (block 61438) and rewriting it,
because a state flag never leaves the value -1. Everything below is live-verified with
DAP watchpoints unless marked otherwise. We need the STATIC side: names, octal addresses,
and the code that performs the missing flag transition.

## What we know (runtime, virtual addresses in the FS RT program's own context)

1. FS RT program MAIN LOOP at runtime 0x649B-0x64C3. Live disassembly (code words):

```
649B  21CB  STD -$35,B
649C  01D6  STZ -$2A,B
649D  49BD  LDA -$43,B        ; A := STATE FLAG   [B-0x43], B=0xB080 -> virt 0xB03D
649E  F201  SAT $1
649F  C035  SKP IF DA EQL ST  ; flag == 1 ?
64A0  A816  JMP *$16 -> 64B6  ; flag != 1 (== -1): SKIP request check, re-run release
64A1  4826  LDA *$26
64A2  BBE3  JPL I -$1D,B
64A3  BA25  JPL I *$25        ; (x3)
64A6  599A  LDX -$66,B        ; X := [B-0x66] = 0x4635
64A7  5C0A  LDX $A,X          ; X := [X+0xA]  = 0x4658 (request element)
64A8  4C13  LDA $13,X         ; A := request word [X+0x13]
64A9  F250  SAT $50
64AA  C435  SKP IF DA UEQ ST  ; request == 0120B ?
64AB  A806  JMP *$6
64AC  F251  SAT $51           ; or 0121B ?
64AD  C435  SKP IF DA UEQ ST
64AE  A803  JMP *$3
64AF  F10C  SAA $C            ; else error code 14B
64B0  BA1B  JPL I *$1B
64B1  BA1B  JPL I *$1B
64B4  BA15  JPL I *$15
64B5  BA18  JPL I *$18
64B6  F1FF  SAA -$1           ; flag := -1  ("work in progress")
64B7  09BD  STA -$43,B
64B8  599A  LDX -$66,B
64B9  0416  STZ $16,X
64BA  5C0A  LDX $A,X
64BB  0413  STZ $13,X         ; clear request word
64BC  BA12  JPL I *$12
64BD  BA0F  JPL I *$F         ; -> paging gate -> FILSYS RELEASE-USER
64BF  CC45  RCLR DA
64C0  599A  LDX -$66,B
64C1  0412  STZ $12,X
64C2  040D  STZ $D,X
64C3  AA0C  JMP I *$C
```

LIVE FACT: 900 watched write-stops on the request word (phys 0x9E6B) show it NEVER
receives 0120B/0121B during the loop - the loop runs PURELY on the flag==-1 fast path
(649D -> 64A0 -> 64B6). The flag is re-asserted -1 by the loop itself at 64B6/64B7.
Nothing ever writes a non-(-1) value.

2. The RELEASE it dispatches lands in FILSYS code at runtime 0x8B26+, with the
   enter-count decrement at 0x8B3D (code words for fingerprinting):

```
8B34  1908  STX $8,B
8B35  4C03  LDA $3,X
8B36  BA54  JPL I *$54
8B37  4A52  LDA I *$52
8B38  BA55  JPL I *$55
8B39  5107  LDT $7,B
8B3A  4906  LDA $6,B
8B3B  BA53  JPL I *$53        ; check call; skip-return = proceed
8B3C  A838  JMP *$38
8B3D  4C15  LDA $15,X         ; X=0x0EF5; entry word0 at [X+0x15] (virt 0xF0A)
8B3E  F5FF  AAA -$1           ; DECREMENT enter count
8B3F  7050  AND *$50          ; & 0x00FF
8B40  CC69  COPY SA DD
8B41  4C15  LDA $15,X
8B42  704E  AND *$4E          ; & 0xFF00
8B43  CC0D  RADD SD DA
8B44  0C15  STA $15,X
8B45  4906  LDA $6,B
8B46  BA4B  JPL I *$4B        ; queue user-file write-back
```

3. Anchors: RT descriptions in resident memory: RTLO symbols WSRTF=043111, WARTF=043145,
   DELRT=043162, DSRTF=043230, FSYRT=043262, RRTEL=043367. FILSYS symbols (FILSYS's own
   link space): RLUSE=115020, RRLUS=115016, MRUSE=105010, ENSYS=114356, ENFUS=114340,
   MENSY=114350, MRENU=114353, ENUSE=114361, GUIOI (near 0x8B1A runtime). The runtime
   FILSYS overlay relocation is unknown - fingerprint the code word sequences above in
   the carved segments to anchor.

## The questions

1. Which routine is the main loop at runtime 0x649B (name + octal address in the carve /
   FILSYS listing)? It looks like the FILSYS RT program's request dispatcher.
2. What is the STATE FLAG at frame offset B-0x43 called, and what values does it take
   (we observe 1 = idle/check-request, -1 = work in progress)?
3. WHERE does SINTRAN set that flag to a non-(-1) value ("done"/idle)? Find every writer.
   That transition never happens in our emulator - we need the exact code and its
   preconditions (what result/status/condition it tests before declaring done).
4. In the release path (the 0x8B26 routine, presumably RLUSE-family): what is the
   intended TERMINATION of a stale-enter cleanup - does it stop on enter-count == 0,
   on an error return from a check (the call at 0x8B3B whose skip-return currently
   always says "proceed"), or does the RT program count iterations?
5. Byte evidence please: carved words + octal addresses for each claim, per the usual
   VERIFIED/INFERRED discipline.

Deliverable: answers + the octal addresses, so the live session can breakpoint the DONE
transition and see why its precondition never satisfies in the emulator.
