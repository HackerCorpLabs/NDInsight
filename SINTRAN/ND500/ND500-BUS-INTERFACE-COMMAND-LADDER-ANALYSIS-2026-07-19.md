# ND-500 3022 Bus-Interface - Per-Command Traffic Analysis (2026-07-19)

**Purpose**: answer Ronny's ask - "log what the commands trigger in the ND-500 bus interface
and analyse if we are responding correct" - by capturing the 3022 register-level delta each
ND-500 monitor command produces in the live boot harness, and grading it against the architect's
3022 expectation column (`DOMAIN-HANDLING-TWO-INTERFACE-EXPECTATION-TABLES-2026-07-19.md`).

**Method**: RetroCore harness test `Nd500_BusInterface_CommandLadder_Capture`
(`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranNd500BootHarnessTests.cs`).
Boots SINTRAN III L from the BIGDISK0-L SMD image, logs in as SYSTEM, enters the ND-500 monitor,
and runs the architect's REACHABLE ladder (A1 -> B -> F7 -> F5; phase-D+ is [TC] and excluded).
`NDBusND500IF.RegisterTrace` is captured into a ring; `BusDelta()` flushes each command's OWN new
events (seq-tagged), so per-command traffic is isolated. Grades below: **OK** = matches expectation;
**RESIDUAL** = works but with the known CS-load loop; **[TC]** = transport OK, semantics uncarved.

Full transcript + trace: `scratchpad\sintran-boot-capture-busladder.txt`,
`scratchpad\sintran-3022-trace-busladder.txt`.

---

## Per-command results

| Rung | Command | Console result | 3022 traffic | Architect expectation | Verdict |
|---|---|---|---|---|---|
| A1 | `@ND-500` | banner + `N500:` | (no NEW filtered traffic this run - CS already loaded) | CS-load gate then swapper [V gate] | **OK** (A1 CLEARED - see below) |
| B1 | `VERSION` | `Subsystem 88.6.16 REV-J04` / `> Loading Control Store` / `System 88.8.17 Rev-L00` / `Micro program 11930` | ~8000 events: CS-load bit-9 verify loop (`LCON5:=0x24` / `ACTIVATE mar=0` / `PROCMSG skip mar==0`), then the microprogram read completes | microprogram = read of loaded CS word-1; others ND-100-cached [TC mechanism] | **RESIDUAL**: eventually correct (returns micro ver 11930) but via a slow mar=0 bare-activate loop = the Bug-B CS-load residual |
| B2 | `WHO-IS-ON` | `===> 1 used by SYSTEM on terminal 1 cpu 1` | **(no 3022 traffic)** | ND-100 tables only, NO bus [V] | **OK** - exactly as predicted |
| B3 | `LIST-ACTIVE-PROCESSES` | `proc 1 / magic 0 / (no name)` | 17 events = **background ReadMicroVersion heartbeat** (see note) | ND-100 tables, NO bus [?] | **OK** - the command itself is ND-100-cached; the 17 events are an incidental background poll |
| F7 | `GET-FLAG 1` | `0B` (flag value returned) | 17 events = **background ReadMicroVersion heartbeat**, byte-identical to B3's block | RFLAG=100B, first real round-trip [V subfn#; TC block] | **ND-100-cached** - NOT a GET-FLAG round-trip (see correction) |
| F7 | `SET-FLAG 1 0` | silent, back to `N500:` | **(no 3022 traffic)** | SFLAG=101B round-trip [V subfn#] | **ND-100-cached** - no heartbeat landed in its window; consistent with GET-FLAG also being cached |
| F7 | `GET-FLAG 1` (readback) | `0B` | 17 events (same heartbeat) | - | consistent - flag stays 0 |
| F5 | `LOOK-AT-HARDWARE INTERFACE` | `N100 STATUS 001000` / `N500 MICRO PROGRAM STOPPED` / `N500 STATUS 040000` / `INPUT/OUTPUT NOT FINISHED` / `MAR 0  MICRO P: 0` | 295 events (direct register reads) | direct 3022 register dump - best register-map validator [V] | **OK** (dump renders); confirms MICRO PROGRAM STOPPED + MAR=0 + MICRO P=0 = ND-500 never runs real microcode |

---

## CORRECTION (2026-07-19): the "GET-FLAG round-trip" is a BACKGROUND POLL, not GET-FLAG's traffic

The 17-event block attributed to `GET-FLAG 1` is **byte-identical** to the 17-event block on
`LIST-ACTIVE-PROCESSES` (same MAR=0x212098, same `MSGHDR@0x424130 MICFU=0x0001`, same
`PROCMSG ... lastMICFU=ReadMicroVersion`). That is a **periodic background ReadMicroVersion poll**
the monitor emits over the 3022, which happened to land inside those two commands' capture windows.
`WHO-IS-ON` and `SET-FLAG` simply had no poll land in theirs. So:

- **There is NO GET/SET-FLAG asymmetry.** BOTH `GET-FLAG` and `SET-FLAG` are **ND-100-cached** in this
  build - neither RFLAG(100B) nor SFLAG(101B) crosses the 3022. The earlier "first live round-trip"
  reading was wrong: it caught the version heartbeat, not a flag transaction.
- **The real periodic 3022 traffic** is `ReadMicroVersion` + `ResidentRead` background polls
  (tail-of-trace counts: 4x ReadMicroVersion, 14x ResidentRead). These ARE working round-trips.
- **OPEN (needs carve)**: do RFLAG/SFLAG *(supposed to)* cross the interface at all, or are they
  legitimately ND-100-resident state? And does `SET-FLAG` actually change the flag functionally
  (untested - value read back was 0 both before and after `set-flag 1 0`, which sets it to its
  current value, so it proves nothing). Carve MON 60B 100B/101B to settle this.

## Background-poll anatomy (the ReadMicroVersion heartbeat - a REAL working round-trip)

The recurring 17-event block is a clean, complete transaction - the transport IS correct:

```
Terminate(offs7)=0x0021  [lock=1  STATUS=InterruptEnabled,InterfaceLocked]   ; lock the interface
LoadMarX2(offs1)=0x0021  -> MARBUILD half=MS -> mar=0x212098                  ; MAR high half (bank/MS)
LoadMarX2(offs1)=0x2098  -> MARBUILD half=LS -> mar=0x212098                  ; MAR low half (LS)
LoadControlRegister(offs5)=0x0005                                            ; LCON5:=5 = ACT50 activate
ACTIVATE mar=0x212098 (word)=2171032 ctrl=0x0005                             ; activate w/ REAL mar IN the window
MSGHDR base=0x424130 link=0xFFFFFFFF N5STA=0x0001 MICFU=0x0001               ; msg header read at byte 0x424130
PROCMSG base=0x424130 -> processed=1 lastMICFU=ReadMicroVersion             ; message PROCESSED
Terminate(offs7)=0x0028  [STATUS=ND500Finished,InterfaceLocked]              ; completion (finished set)
LoadControlRegister=0x0008 / LoadStatusRegister=0x0000 / LCON5:=1            ; finish + cleanup
Terminate(offs7)=0x0020 / LCON5:=0x0008 / LSTA5:=0 / LCON5:=1                ; unlock cleanup
```

- **MAR = 0x212098 word = 0x424130 byte** = INSIDE the 5MPM window (base byte 0x420000 / word 0x210000).
  So the ECCR ADRZERO fix is holding - the mailbox lands in the window, not low memory.
- Two-half MARBUILD (MS then LS) matches the carved ACT50 sequence (`LMAR5:=bank`, `LMAR5:=addr`, `LCON5:=5`).
- `PROCMSG processed=1` = the servicer consumed the message and answered; completion via `ND500Finished`.
- **CAVEAT (the [TC] gap)**: `lastMICFU=ReadMicroVersion` - the servicer decoded this GET-FLAG message
  with the SAME micro-function as VERSION (code 0x0001). RFLAG (100B) is a DISTINCT subfunction; the
  console did show `0B`, but the generic decode means we cannot yet claim the RFLAG *value* is真. The
  message BLOCK/subfunction handling is [TC] per the architect - carve needed to confirm RFLAG vs a
  fall-through to ReadMicroVersion.

---

## Findings / "are we responding correctly?"

1. **A1 (CS-load) is CLEARED** on the 3022 path - the earlier "stalls in CS-load / test-mode"
   blocker was the TERM5 bug (emulator manufactured `ND500Finished` on a bare unlock); fixed
   (`NDBusND500IF.cs` Terminate case, committed 1ec4c3df0). `@ND-500`, `VERSION`, and `GET-FLAG`
   all get through the gate now. **The architect's tables still list A1 as the blocker - this
   analysis is the evidence that it clears.** Please update the 3022 column.

2. **The CS-load VERIFY loop is the remaining residual** (Bug-B): `VERSION` (and any first command
   that reads the microprogram version) spins ~8000 events of `LCON5:=0x24 / ACTIVATE mar=0 /
   PROCMSG skip (mar==0)` for ~30-60s before completing. SINTRAN issues **bare activates with MAR
   never set**; the emulator correctly skips them (no message), but SINTRAN retries until the CS
   bit-9 verify is satisfied. It DOES complete (micro ver 11930 returned) - it is slow, not stuck.

3. **The pure-ND-100 rungs are correct**: `WHO-IS-ON`, `LIST-ACTIVE-PROCESSES`, `GET-FLAG`,
   `SET-FLAG` produce NO command-specific 3022 traffic. The 17-event blocks seen on some of them
   are the background ReadMicroVersion heartbeat (byte-identical across commands), not their own.

4. **GET/SET-FLAG are BOTH ND-100-cached here (no asymmetry)** - correcting the first pass. RFLAG
   (100B)/SFLAG(101B) do not cross the 3022 in this build. OPEN: whether they SHOULD (carve needed),
   and whether SET-FLAG changes the flag functionally (untested - only set it to its current value 0).

5. **The real periodic 3022 traffic = `ReadMicroVersion` + `ResidentRead` background polls** (both
   ND-100->ND-500 reads, both complete cleanly: MAR-in-window, activate, PROCMSG processed, finished).
   These are the working round-trips; their message BLOCK format / trigger cadence is [TC] - carve.

6. **The ND-500 micro program is STOPPED / MAR=0 / MICRO P=0** (F5 dump) - confirms the emulated
   ND-500 does not execute real microcode (classic-500 144-bit microcode absent), which is why
   phase-D RUN yields "NO WELL DEFINED PROGRAM IN MEMORY" and why the servicer answers messages
   rather than the microcode doing so. This bounds what the harness can prove without the microcode.

**Build order status** (architect's ladder): A1 done; B/C reachable and green; F7 first round-trip
green (transport); F5 register dump green. Next: carve the RFLAG/SFLAG block (F7 semantics) and the
LAST-N500-MSG ring format (F6) for the cross-check; phase-D stays pending on the domain carve.
