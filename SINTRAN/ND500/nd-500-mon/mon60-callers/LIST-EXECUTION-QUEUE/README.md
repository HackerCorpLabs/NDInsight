# LIST-EXECUTION-QUEUE

MON 60 subfunction **LSTEXQ = 133B** (octal) = **0x5B** = 91 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **LIST ND-500 EX-QUEUE**
(server handler `ILI5EXQ`). yaml: LSTEXQ "List execution queue", params `(none)`.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Handler | **STANDALONE ENTER-routine at 111430** (framesize 000407 = 263 locals), spans 111430..111603 | PROVEN |
| MON 60 call site | **111445** `JPL I 123` -> ptr 111570, `bank1[111570]=146723` = thunk `SAA 133` | PROVEN |
| Error path | 111446 -> ptr 111571 = LEAVE(value) 177327 | PROVEN |
| Success path | 111447 (fall-through); routine success -> LEAVE-SKIP 177335 at 111564/111603 | PROVEN |

## MON call(s) used

| Call | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|
| MON 60 LSTEXQ | 133B / 0x5B | 146723 | `(none)` | `,X 6` = 3-word descriptor (work-buffer &qbuf @B-165 + count) - see inference | err=111446->LEAVE(val); ok=111447 |
| MON 104 HOLD | 104B (MON, not MON 60) | - | - | pauses between screenfuls | - |

Byte citations: `111570`=`146723` (thunk, `SAA 133`); parameter store `111444`
(`STF ,X 6`); `MON 104` at `111562` (`153104`).

## What it does

1. Sets up a work buffer (`@B-165`) and issues `MON 60` LSTEXQ (133B) to obtain a
   snapshot of the ND-500 execution queue. (`111433-111445`)
2. Prints a heading and then walks the returned queue entries in a loop
   (`111456-111554`), printing each entry's fields via output helpers (`016507`,
   `054452`, etc.; none are MON calls). The loop terminates on a `-1` sentinel.
3. If there is more to show, it issues `MON 104` (HOLD/SuspendProgram) at `111562` to
   pause, then loops back to `111440` to re-request and continue the listing.
4. Returns skip (success) via LEAVE-SKIP. (`111564`)

## How it fits ND-500 init

`LIST-EXECUTION-QUEUE` is an operator monitoring command: it shows which ND-500
processes are queued for execution. It is a status/inspection command rather than an
init step. It is the closest match in this binary to a "list status" of ND-500
activity (see the LIST-STATUS note in the base folder).

## Unknown / inferred

- **INFERRED**: LSTEXQ's parameter. The yaml lists `(none)`, but the handler stores a
  3-word `F` descriptor into slot 1 (`STF ,X 6` at `111444`), built from `&(B-165)`
  and a count. The most consistent reading is a caller-supplied result buffer +
  length; the store is PROVEN, the "buffer+length" interpretation is inferred.
- **INFERRED**: the `-1` loop sentinel and the "more to show" test drive the
  screen-pause/HOLD; the `SAT -1` comparisons at `111460`/`111514`/`111557` are PROVEN.
- **INFERRED (role)**: `016507`/`054452` are output/formatting helpers; addresses and
  call sites PROVEN, internals not carved.
- **PROVEN**: exactly one MON 60 (LSTEXQ) plus one MON 104 (HOLD) in 111430..111603.
