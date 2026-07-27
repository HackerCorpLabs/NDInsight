# ENNS0 RX-forward root cause: why a received Ethernet frame never becomes an XMSG message

Date: 2026-07-24
Subsystem: ND Ethernet II (PCB 3094 / ND-110063), COSMOS network server ENNS0
Scope: the RECEIVE-forward path - LANCE RX -> 68000 firmware RCVCOMPLETE -> ND-100 / XMSG

Convention: **[V]** = byte-verified from the firmware disassembly / emulator source / a captured
frame. **[I]** = inferred (strongly supported, not fully traced). Firmware addresses are hex,
MC68000 big-endian, base 0x0, binary
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\NDBusDevices\TestData\encos-ser-all-banks-68k.bin`.

> **CORRECTION (2026-07-25) - this doc's original verdict is SUPERSEDED.**
> The RX discard WAS a real emulator bug, and it was on the TRANSMIT side, not an unmet
> firmware precondition. The sender's Am7990 LANCE appended a 4-byte FCS to the frame handed
> to the network backend, while the receiver's LANCE RX assumes backend frames are FCS-less
> and adds 4 to MCNT - so the FCS was DOUBLE-COUNTED and every COSMOS frame arrived 4 bytes
> too long. That failed the firmware's 802.3 length-consistency check **gate 2**
> (RCVCOMPLETE 0x5D18: received-data-length must equal the 802.3 length field), NOT the
> empty-pool **gate 3** this doc originally blamed. Sections 1-2 describe the discard
> MECHANISM correctly, but the CAUSE is the FCS double-count and the "not a bug in the
> emulator" conclusion is wrong. Fixed in `Emulated.HW\AMD\LANCE\Am7990\Am2990Lance.cs`
> (hand the backend the FCS-less frame; keep FCS on the internal-loopback path). Reproduced
> and validated offline by `Emulated.Tests\ND100\Nd100TwoNodeEthernetHarnessTests.cs`:
> `conn-to d102` now returns CONNECTION ESTABLISHED (was "Remote system not available").
> Consolidated write-up: `SINTRAN\XMSG\DOC\COSMOS-RE\COSMOS-MULTI-NODE-NETWORK-2026-07-25.md`.

---

## 1. Verdict (one line)

**The received frame is DISCARDED inside the 68000 firmware RX-completion handler
RCVCOMPLETE (0x5C42), not lost between the LANCE and the ND-100. On any discard the
firmware clears its "message built" pointer, takes the branch at 0x601A that only
re-appends the RX buffer to the LANCE ring and yields - it never calls the deliver
routine (0x134E6) and therefore never rings SCIP/INT12. That is exactly the observed
symptom: RX ring pointers updated, then straight on to transmit, no SCIP, no superkick.**

For a correctly-addressed IEEE-802.3 COSMOS frame (the real connect-to case) the specific
discard is **gate 3: the card's free XMSG receive-buffer pool at DRAM 0x188C6 is empty**,
so RCVCOMPLETE has nowhere to put the inbound message and drops it.

This is NOT a bug in the emulator's LANCE RX, descriptor write-back, RINT delivery, or the
68K interrupt path - those are all correct and were verified. The frame is dropped by
CORRECT firmware logic because a precondition is not met.

---

## 2. The RX-completion handler RCVCOMPLETE @ 0x5C42 - full decode [V]

Entry is reached from the LANCE level-2 RX interrupt. Register `A6` is a coroutine frame
pointer; the important local is **`$18(A6)` = the message buffer this handler builds** (0 =
"no message built" = the frame is being dropped).

```
5C54  lea   $18000,A0            ; RX software ring header (FREE@+0, PROD@+2, CONS@+4)
5C5A  move.w $4(A0),D0           ; D0 = CONS index
5C62  lea   $8(A0,D0*8),A1       ; A1 = &descriptor[CONS]  (RMDs start at 0x18008)
5C6E  move.w $2(A2),D1           ; RMD1 (flags)
5C72  btst  #15,D1               ; OWN ?
5C76  bne   $604C                ;   OWN set (chip still owns) -> yield to scheduler
5C7A  move.w $4(A2),D2 / tst.l
5C82  beq   $604C                ;   zero byte-count -> yield
...   copy dst-MAC (6 bytes) to $30(A6); length d5 = (MCNT & 0x0FFF) - 4  (FCS stripped)
5CC8  btst  #14,D6 / bne $5EFC   ; RMD1.ERR -> error-counter path -> discard
```

### Frame-format classification -> sets `$36(A6)`  [V]
```
5CD0  cmp.w #60,D5 / bcs $5CDE   ; runt (<60)  -> $36 handling below
5CDA  cmp.w D5,D7(=1514)/bcc 5D0A; in [60..1514] -> length/type discrimination
5D0A  tst.w $1888A / beq $5D58   ; if card "802.3 mode" word == 0 -> treat as 802.3 ($36=1)
5D14  cmp.w D5,#60 / bcc $5D58   ; frame <= 60 bytes -> $36=1 (accept as 802.3)
5D18  sub.w #14,D5               ; else D5 = payload length
5D1C  move.w $C(A4),D1           ; D1 = frame bytes 12-13 (the 802.3 length / DIX type field)
5D20  cmp.w D5,D1 / bne $5D2C    ; length field == payload length ?
5D24    move.w #1,$36(A6)        ;   YES -> 802.3 length frame     ($36 = 1)
5D2C    clr.w  $36(A6)           ;   NO  -> Ethernet-II TYPE frame ($36 = 0)
```
`$36(A6) = 1` only for **IEEE-802.3 length-encapsulated** frames. A DIX Ethernet-II frame
whose bytes 12-13 are an EtherType (e.g. 0x0800 IP, 0x0806 ARP) that is larger than the
payload length gets `$36 = 0`.

### "Is it for us?" -> sets `$38(A6)`  [V]
```
5D5E  test dst-MAC bit0 (multicast/broadcast?) -> $5DC8 else unicast
      unicast: compare 6 bytes at $30(A6) (frame dst) vs $1885E (card MAC)
               match -> $38=1 ; mismatch (and not promiscuous $18888) -> $38=0
      mcast/bcast: bsr $542C multicast filter -> $38 from result
```

### The three discard gates -> all clear `$18(A6)` and jump to 0x5EF4  [V]
```
5DF2  tst.w $38(A6) / beq $5EF4  ; GATE 1: not addressed to us            -> DISCARD
5DFA  tst.w $36(A6) / beq $5EF4  ; GATE 2: not an 802.3 length frame      -> DISCARD
5E02  lea $188C6,A0 / tst.w (A0) / beq $5EF4   ; GATE 3a: rx-buffer pool count == 0
5E0E  move.l $4(A0),D0 / tst.l / beq $5ECA     ; GATE 3b: rx-buffer free-list head == 0
                                               ;         -> DISCARD (no free receive buffer)
5E1C..5EC8  ACCEPT: pop a free buffer, tag it (ori #$4C00 at msg+0x0A = "received message"),
            copy the frame in, store the buffer pointer in $18(A6).
5EF4  clr.l $18(A6)              ; DISCARD marker: "no message built"
5EF8  bra   $5FF2                ; common tail
```

### The decisive branch - deliver vs silent drop @ 0x601A  [V]
The common tail advances the RX ring (this is the "updates RX ring pointers" the log shows)
and then:
```
5FFC  addq.w #1,(A0)            ; FREE++            (ring header 0x18000)
6008  CONS = (CONS+1) mod 128   ; consumer advanced (0x18004)
601A  tst.l $18(A6) / beq $6034 ; was a message actually built ?
6020    lea $188D6,A0           ; YES  ->
6024    jsr $134E6              ;   ENQUEUE to the ND-100 ready-ring ($188D6) + SIGNAL.
6032    bra $6048 -> $5C54      ;   (this call is the ONLY route to a SCIP / superkick)
6034    bsr $5B60 (RCVRINGAPP)  ; NO   -> just re-append the RX buffer to the LANCE ring
6046    jmp (A5)                ;         and YIELD.  NO $134E6, NO SCIP, NO INT12.
```

So: **deliver-to-ND-100 (and the SCIP that wakes ENNS0/PDRIV/XRTEN) happens only when
RCVCOMPLETE successfully built a message buffer (`$18(A6) != 0`), i.e. only when the frame
passed all three gates.** Every discard takes the 0x6034 branch: re-append the buffer, yield -
which is precisely the reported trace (ring pointers move, then the scheduler goes on to
transmit the next routing frame, with no notification to the ND-100).

> **CORRECTION (2026-07-25, Ghidra RE pass) - two structural details above are wrong.**
> 1. **Ring advance is NOT part of the discard branch.** Accept and discard both reach the shared
>    tail `0x5ff2` -> `LanceRxDescriptorClear` (0x553c) -> `0x5ffc` FREE++ / CONS=(CONS+1) mod 128.
>    The two paths diverge only at `0x601a`. So "advance the ring" is common, not discard-specific;
>    what the discard branch uniquely does is re-arm the buffer via `0x5b60` and yield.
> 2. **`0x5ffc` is NOT dead code**, despite Ghidra reporting no xrefs to it. Execution RESUMES at the
>    instruction following the `jmp (A5)`. The same applies at `0x5dd0` (after the group-filter call)
>    where `0x5dd2` clearly consumes the returned D0.
>
>    **CORRECTION (2026-07-26) - the REASON given here was wrong.** This is not a "coroutine yield".
>    It is the PLANC-MC **two-entry return convention**, now verified byte-level across the image:
>    - Every compiled routine ends `movea.l (SP)+,A6 ; movea.l (SP)+,A2 ; jmp (0x2,A2)` - the second
>      pop takes the RETURN ADDRESS into A2, and the jump goes to **retaddr + 2**, skipping the
>      2-byte `4E D5 jmp (A5)`. Byte search finds 400+ of these epilogues and **zero** routines that
>      return to +0.
>    - `jmp (A5)` is therefore the **error-unwind trampoline**, reached only when the runtime returns
>      to +0. A5 holds `#XRET` (0x135A8) normally, or `#ERET` (0x13596) at an outermost frame.
>      `#XRET` is the only +0 returner in the image; it pops one frame, re-arms A5 to itself, and
>      lands on the caller's `jmp (A5)` - a one-frame-at-a-time stack unwind.
>    - A5 is loaded in exactly 5 places image-wide: the three process roots (`POMNPROCES` 0x7BBA,
>      `AUTO_START` 0x7E26, `POCSPROCES` 0xE398) set it to `#XRET`; `#XRET` and `#ERET` re-arm it.
>
>    Note this firmware DOES also have a genuine cooperative-coroutine scheduler (the PIOCOS task
>    system with STOP / `movem` save-restore, see `ENNS0-Startup-RE-2026-07-23\FIRST-SUPERKICK-BRIDGE-DECODE-2026-07-23.md`).
>    The error above was attributing `bsr; jmp (A5)` to that scheduler. The two are unrelated.
>
> Also note the gate-2 mechanism is better understood now: `0x1888a` is a "length field present"
> switch, and clearing it disables the gate-2 length test entirely, which is what makes Ethernet II
> traffic possible. Full decode:
> [ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md](ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md).

---

## 3. Mapping the decode onto the observed symptom

| Observed in the controller log | Explanation (from the decode) |
|---|---|
| `LANCE RX accepted ... rmd1=0x0303 (OWN cleared, RINT raised)` | LANCE RX + descriptor write-back correct; firmware RX ISR entered at 0x5C42. |
| firmware writes RX_FREE/CONS/PROD then moves on | the common tail (0x5FFC-0x6016) FREE++/CONS++ - runs on BOTH accept and discard. |
| RX_PROD == RX_CONS (both 2) | ring fully returned to the LANCE after consuming - normal post-consume state, not the bug. |
| **no SCIP, no INT12, no superkick after RX** | the frame hit a discard gate -> `$18(A6)=0` -> 0x6034 branch -> no 0x134E6 -> no SCIP. |
| "goes straight on to TRANSMIT its next routing frame" | 0x6034 `jmp (A5)` yields to the scheduler, whose next work item is the pending TX. |

---

## 4. Which gate fires for the real connect-to frame

The boot-harness RX probe injects a **real captured COSMOS connect-to frame** (node 102 ->
node 100), `Nd100SintranEthernetIIBootHarnessTests.cs:476`:

```
dst = 08:00:26:64:00:00   (= this card's MAC, node 100)
src = 08:00:26:66:00:00   (node 102)
bytes 12-13 = 00 0E       (= 14 : an IEEE-802.3 LENGTH field, NOT a DIX EtherType)
payload = a8 a8 03 0b 02 0f 00 00 ...   (XMSG/COSMOS LLC data), padded to 60 bytes
```

Walking the gates for this 60-byte frame:
- **Gate 1 ($38, MAC):** dst == card MAC 08:00:26:64:00:00 -> `$38 = 1`. **PASS.** [V]
- **Gate 2 ($36, 802.3):** frame length <= 60 -> the short-frame branch (0x5D14 `bcc $5D58`)
  sets `$36 = 1` unconditionally. **PASS.** [V] (Independently, bytes 12-13 = 0x000E is a
  genuine 802.3 length, confirming COSMOS uses IEEE-802.3, not DIX Ethernet-II framing -
  despite the product name "Ethernet II".)
- **Gate 3 ($188C6, free rx buffer):** this is the only remaining gate -> **the discard is
  here: the card has no free XMSG receive buffer.** [V that it is the only reachable gate;
  [I] that the pool is actually empty at runtime - confirm with the instrumentation in section 6.]

Corollary, already seen: the earlier `[RX-INJECT]` probe used a broadcast **ARP** frame
(EtherType 0x0806). That frame is > 60 bytes with bytes 12-13 = 0x0806 != payload length ->
**Gate 2** discards it (`$36 = 0`). The memory note "wrong EtherType, not COSMOS -> no
superkick" is exactly gate 2. COSMOS traffic must be 802.3-framed to survive gate 2.

### Who fills the 0x188C6 pool
`0x188C6` is a buffer-pool control block: `+0` = free count word, `+4` = RX free-buffer
list head, `+8` = TX free-buffer list head (the TX side is popped by XMTRINGAPP at 0x6216).
[V] The free receive buffers are XMSG receive credits; the card can only deliver an inbound
message when the ND-100 / XMSG side has posted receive buffers into this pool. If ENNS0's
receive-buffer posting never runs (historically ENNS0 hangs in a device INPUT wait on LU
2240B and force-reserves the device - see `ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md`),
the pool stays empty and **every** inbound frame is discarded at gate 3 with no notification.
Whether the pool is empty at the moment a frame arrives is a runtime fact that the section-6
instrumentation resolves definitively on the next run.

---

## 5. Is there an emulator bug to fix here?

Statically, **no bug was found in the emulator's RX-forward path**:
- `Am2990Lance.ReceivePacket` / `ReceiveComplete`
  (`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\AMD\LANCE\Am7990\Am2990Lance.cs:1359,1492`)
  filter, DMA into the descriptor, clear OWN, write MCNT (+4 for the stripped FCS), and raise
  RINT correctly.
- The 68K level-2 interrupt reaches RCVCOMPLETE (the trace shows PC 0x5C42), the descriptor
  is well-formed (rmd1=0x0303), and the firmware runs its real logic.
- The firmware then DROPS the frame by design (gate 3 / gate 2). The "missing notify" is a
  correct consequence of the drop, not a lost SCIP.

Therefore the fix is NOT a patch to the notify/SCIP path. It is one of:
1. **Ensure inbound COSMOS frames are IEEE-802.3 framed** (bytes 12-13 = LLC data length).
   Real card-to-card traffic already is (the firmware TX builds 802.3), so a two-emulator
   bridge is fine; a hand-built or foreign DIX frame is dropped at gate 2.
2. **Ensure the ND-100 / ENNS0 side posts receive buffers into the card pool (0x188C6)
   before frames arrive.** This is an XMSG/driver-sequence interaction, one layer above the
   controller emulation. Until buffers are posted, gate 3 drops everything.

Because both remaining causes are in guest/higher-layer behaviour (not the C# controller),
no behavioural controller-code change is warranted; making one blind would risk masking the
real gate. What IS added is precise instrumentation to pin the exact gate on the next run.

---

## 6. Emulator change made (instrumentation only)

File: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs`
(the 68K PC-watch, `_pcWatch` + `Watch68KPc`). Added, gated by the existing Device log level
(zero cost when Device logging is off):

- Two new watch PCs at the decisive branch targets:
  - `0x6020` -> logs `[68K-RX] DELIVERED` (RCVCOMPLETE built a message -> `jsr 0x134E6` ->
    ND-100 ready-ring + SCIP).
  - `0x6034` -> logs `[68K-RX] DISCARDED` (frame dropped by gate1/2/3 -> re-append ring, NO SCIP).
- At the RCVCOMPLETE entry `0x5C42`, dumps the free XMSG receive-buffer pool state:
  `[68K-RX] RCVCOMPLETE 0x5C42: XMSG rx-buffer pool $188C6 count=<n> freeHead=0x<addr>`
  with an explicit `POOL EMPTY: frame WILL be discarded (gate3), no SCIP` when the pool is
  empty. This DIRECTLY confirms or refutes the gate-3 hypothesis of section 4.

Build: `dotnet build Emulated.HW/Emulated.HW.csproj` -> 0 errors. [V]
Tests: EthernetII / LANCE suite = **150 passed, 2 failed**; the 2 failures
(`Test_LANCE_TDMD_TransmitDemand`, `Test6_SoftwareInterruptController`) are the documented
**pre-existing** failures (fail identically on the pre-edit baseline, per
`ethii-emulator-fixes` memory). No regression from this change. [V]

---

## 7. How to validate end-to-end

Run with Device logging on and watch the `[68K-RX]` lines:

1. **Single-node harness RX probe** (`Boot_Login_StartXmsg_And_EthernetServer_CaptureControllerLog`,
   [Explicit]; needs `D:\BIGDISK0-L.IMG`, do not block if locked). The injected frame at
   `Nd100SintranEthernetIIBootHarnessTests.cs:476` is a real 802.3 COSMOS frame. Expected with
   this instrumentation: `[68K-RX] RCVCOMPLETE ... POOL EMPTY ...` followed by `[68K-RX]
   DISCARDED`. That confirms gate 3 (no receive buffer posted) as the live blocker.
2. **Two-node bridge** (the real connect-to case): two emulators joined via
   `device add ETH 0 --net=tcp:...` / `--net=listen:...` (or pcap). Drive `connect-to` from
   one node. On the receiver, a `[68K-RX] DELIVERED` line means the frame reached XMSG (fix
   works); a `[68K-RX] DISCARDED` with `POOL EMPTY` means the receiver never posted receive
   buffers - chase the ND-100/ENNS0 receive-buffer posting (the LU-2240B input path).

The gate the log names tells you exactly where to continue:
- `DISCARDED` + `POOL EMPTY` -> ND-100/ENNS0 must post receive buffers (XMSG credit path).
- `DISCARDED` + pool non-empty -> gate 1 (MAC mismatch: check card MAC vs frame dst) or gate 2
  (frame not 802.3: bytes 12-13 are a DIX EtherType, not a length).
- `DELIVERED` -> RX-forward is fixed; any remaining failure is downstream in XMSG/XROUT.

---

## 8. Key addresses (quick reference)

| Addr | Role |
|------|------|
| 0x5C42 | RCVCOMPLETE entry (LANCE RX complete, 68K level 2) [V] |
| 0x5DF2 | gate 1: dst-MAC-for-us (`$38`) `beq 0x5EF4` [V] |
| 0x5DFA | gate 2: 802.3-length-frame (`$36`) `beq 0x5EF4` [V] |
| 0x5E02 / 0x5E0E | gate 3: free rx-buffer pool 0x188C6 (+0 count / +4 head) [V] |
| 0x5EF4 | discard marker: `clr.l $18(A6)` [V] |
| 0x601A | deliver-vs-drop branch `tst.l $18(A6); beq 0x6034` [V] |
| 0x6020 | DELIVERED: `jsr 0x134E6` -> ready-ring 0x188D6 + SCIP [V] |
| 0x6034 | DISCARDED: `bsr 0x5B60` RCVRINGAPP + `jmp (A5)`, no SCIP [V] |
| 0x134E6 | generic "enqueue buffer on list + signal" (deliver / return) [V] |
| 0x18000/2/4 | RX software ring header FREE/PROD/CONS [V] |
| 0x18008 | RX descriptors (RMD), 8 bytes each [V] |
| 0x1885E | card MAC (dst recheck target) [V] |
| 0x1888A | card 802.3-vs-typed mode word (gate-2 discrimination) [V] |
| 0x1888C | card statistics/counter control block base [V] |
| 0x188C6 | XMSG receive-buffer pool: +0 count, +4 RX free-list, +8 TX free-list [V] |
| 0x188D6 | ND-100 delivered/ready message ring (target of 0x134E6 on RX) [V] |

---

## 9. Files

- Firmware binary: `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\NDBusDevices\TestData\encos-ser-all-banks-68k.bin`
- Emulator change: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs` (`_pcWatch` / `Watch68KPc`)
- LANCE RX (verified correct): `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\AMD\LANCE\Am7990\Am2990Lance.cs` (`ReceivePacket` 1359, `ReceiveComplete` 1492)
- Harness + RX probe frame: `E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs:476`
- Related prior RE: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\` (ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B, FIRST-SUPERKICK-BRIDGE-DECODE, MAKE-RUNNABLE-TRIGGER-AND-FIX)
- Master reference: `E:\Dev\Repos\Ronny\RetroCore\DOCS\ND_EthernetII_MASTER_REFERENCE_2026-07-23.md`
</content>
</invoke>
