# Octobus MAILBOX / MICFU message sequence - annotated live reference

**What this is:** the message-by-message record of what SINTRAN actually sends over the
octobus MAILBOX (MICFU) layer during ND-500 bring-up, with each message explained, captured
live from RetroCore on 2026-07-28.

**Why it exists:** the two existing catalogs cover DIFFERENT layers and neither answers
"what does message N mean and what should we have replied":

| File | Layer |
|---|---|
| `SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` | ND-100 side, octobus FRAME / OMD (LMFIELD, kicks, CON5IDENT, 5OMBREAD) |
| `ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md` | microcode side, B30 routines (AFLAG, polling spine, TRAP_OCBM) |
| **this file** | **mailbox MICFU layer (3RMICV / CACHE / PHYSWR / PHYSRD) with operands** |

Without this, the raw trace is easy to misread. It was misread twice on 2026-07-28 - see
"Corrections" at the end. Read this before drawing conclusions from a MICFU trace.

## How to capture it

```
dotnet test Emulated.Tests --no-build --filter "FullyQualifiedName~Nd100SintranNd5000OctobusBootHarnessTests.FullFlow"
```

Look for `----- servicer MICFU trace (octobus-fullflow) -----`. The harness logs the copy-family
operands (`addrA`, `addrB`, `nrbyt`) via `OnServicerMessage`; without those the trace is not
interpretable. Machine state at capture: run thread ON, SINTRAN fully initialised
(`N5CPU=1`, `NSAMSON=4`, `5MBBANK=41B`, `5FPMAILBOX=0x851`).

## The captured sequence: 56 messages

| Count | MICFU | Name | Role |
|---|---|---|---|
| 30 | `0x01` | `3RMICV` | **WATCHDOG ping** (not a version query - see below) |
| 1 | `0x0A` | `CACHE` (12B) | cache-clear, `nrbyt=2048` (one page) |
| 13 | `0x19` | `PHYSWR` (31B) | physical write, `nrbyt=4` (one 32-bit word) each |
| 12 | `0x18` | `PHYSRD` (30B) | physical read, `nrbyt=4` each |

### Phase 1 - probe (msgs 1-3)

```
1  3RMICV @0x00428E30
2  3RMICV @0x0042C130
3  3RMICV @0x0042C130
```

Two distinct message buffers appear and stay fixed for the whole run:
`0x00428E30` = the general ND-500 message buffer, `0x0042C130` = the WATCHDOG buffer.

### Phase 2 - cache clear (msg 4)

```
4  CACHE addrA=0xCFCF4000 addrB=0x00216400 nrbyt=2048
```

`addrA` is not a plausible address - for `CACHE` these operand slots are almost certainly not
address operands at all. **UNVERIFIED:** what CACHE's operand fields mean.

### Phase 3 - WRITE a 13-word block (msgs 5-27, interleaved with the watchdog)

```
PHYSWR addrA=0x000000BC addrB=0x0000CC00 nrbyt=4
PHYSWR addrA=0x000000C0 ...
PHYSWR addrA=0x000000C4
PHYSWR addrA=0x000000B6
PHYSWR addrA=0x00000096
PHYSWR addrA=0x0000009A
PHYSWR addrA=0x0000009E
PHYSWR addrA=0x000000A2
PHYSWR addrA=0x000000A6
PHYSWR addrA=0x000000AA
PHYSWR addrA=0x000000AE
PHYSWR addrA=0x000000B2
```

Every write is FOUR BYTES from the SAME source buffer `addrB=0x0000CC00`. Targets step by 4
across `0x96..0xC4` - a contiguous block of 32-bit words, written in the order
`BC C0 C4 B6 96 9A 9E A2 A6 AA AE B2`.

### Phase 4 - READ THE SAME WORDS BACK (msgs 29+)

```
PHYSRD addrA=0x000000BC addrB=0x0000D400 nrbyt=4
PHYSRD addrA=0x000000C0 ...
PHYSRD addrA=0x000000C4
PHYSRD addrA=0x000000B6
PHYSRD addrA=0x00000096
PHYSRD addrA=0x0000009A
...
```

**Same addresses, SAME ORDER**, into a different buffer `0x0000D400`. This is a
**write-then-read-back VERIFY** of a 13-word block, not a data transfer.

### Phase 5 - nothing but watchdog

After the read-back the trace contains ONLY `3RMICV` pings. SINTRAN issues no further work.

## What each MICFU means here

- **`3RMICV` (0x01) is the WATCHDOG, not a version query.** `MP-P2-N500.NPL:1209` (`LCLTSB`)
  stamps `MICFU=3RMICV` into the WATCHDOG buffer and arms a timer (`LTTMR=:TMR`). The timeout
  check is `RP-P2-N500.NPL:127642`: when the timer fires, if the watchdog message's `N5STA` is
  not `ANSWER`, SINTRAN checks `X5BRK` and otherwise raises `N5TIMOUT` ("ACCP was terminated;
  Microprogram has stopped") and calls `RSTARTALL`. **Do not read a burst of 3RMICV as a version
  negotiation - it is a heartbeat, and its count is just elapsed time.**
- **`PHYSWR`/`PHYSRD` are the copy family.** `Nd500MicrocodeServicer.PerformOctobusBlockCopy`:
  `addrA` = ND-500/target side (SC3), `addrB` = buffer side (SC7), `nrbyt` = byte count (SC4),
  both resolved as `host.Nd500AddressBase + addr`. WR copies B->A, RD copies A->B.

## Emulator behaviour on this sequence (2026-07-28)

Every message IS serviced and answered - `N5STA` transitions `1 (MSGN5) -> 2 (WAITING) ->
3 (ANSWER)` are observed live, and 914 station->ND-100 frames were read by SINTRAN. The
sequence completes. Then SINTRAN waits forever and the ND-5000 stays `PC=0 stopMode=WAIT`:
**no activation (`3START`, MICFU `0x13`) is ever sent**, so the swapper is never started.

## OPEN - the next question, and do NOT guess it

**What is ND-500 physical `0x96..0xC4`?** 13 consecutive 32-bit words that SINTRAN writes and
immediately verifies during `start-swapper`. Identifying this block is the next step, because
whatever SINTRAN expects to happen after a successful verify is not happening.

Also UNVERIFIED: the meaning of `CACHE`'s operand fields; whether `addrA` on the copy family
should resolve to ND-500 LOCAL memory rather than through `Nd500AddressBase` into the MPM
window (a self-consistent round-trip would hide the difference, so the passing verify does NOT
prove the target is right).

## Corrections - misreadings this file exists to prevent

1. **"Only 4 messages were serviced."** WRONG - an artifact of reading a truncated trace view.
   56 messages were serviced. Count the real trace section, do not eyeball a window.
2. **"The PHYSWR addresses cycle - it is a livelock."** WRONG - they ADVANCE by 4 across
   `0x96..0xC4`. A 16-line window straddled the write and read-back phases and looked like a
   loop. The only repetition is the watchdog.
3. **"`MicroVersion=0x2E9A` is being rejected."** Not supported by any evidence here - `3RMICV`
   is the watchdog and its payload is not implicated.

**Raw ordered capture:** regenerate with the command above; the run used for this file is
`run_ops.txt` in the session scratchpad (transient - re-capture rather than relying on it).
