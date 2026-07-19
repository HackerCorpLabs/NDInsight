# STATUS

MON 60 subfunction **RSTAT = 41B** (octal) = **0x21** = 33 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ ND-500 INTERFACE STATUS**.
yaml: RSTAT "Read communication status", params `<status (bits 16:31 ND-500,
bits 0:15 ND-100)> <MAR>`.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Handler | **STANDALONE ENTER-routine at 127551** (framesize 000010 = 8 locals) | PROVEN |
| Body | 127551 .. 127732; local pointer pool 127733 .. 127755 | PROVEN |
| MON 60 call site | **127566** `JPL I 146` -> ptr 127734, `bank1[127734]=146461` = thunk `SAA 41` | PROVEN |
| Error path | 127567 -> ptr 127735 = LEAVE(value) 177327 | PROVEN |
| Success path | 127570 (fall-through), formats output, ends 127732 -> LEAVE-SKIP 177335 | PROVEN |

## MON 60 subfunction(s) used

| Subfn | Octal / Hex | Thunk | Params (yaml) | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|
| RSTAT | 41B / 0x21 | 146461 | `<status> <MAR>` | `,X 6` = &status[2] (@B-171); `,X 7` = &mar (@B-167); `,X 10` = &extra (@B-165) | err=127567->LEAVE(val); ok=127732->LEAVE-SKIP |

Byte citations: `127734` = `146461` (thunk); thunk `146461`=`SAA 41`,
`146462`=`JMP I 1`, `146463`=`146244`. Parameter stores at `127557`/`127562`/`127565`.

## What it does

1. Marshals **three** output addresses into gateway param slots 1/2/3 and issues
   `MON 60` RSTAT to read the ND-500/ND-100 interface status word, the MAR, and a
   third word. (`127554-127566`)
2. On error, returns to the caller with the MON 60 status code (LEAVE-value).
3. On success, formats the status for the operator terminal: it prints a heading and
   the raw value, then walks the status bits in two loops (`127606-127634` over the
   ND-100 half, `127653-127701` over the ND-500 half), printing a message-table
   string for each set bit, then prints the MAR and the third word. (`127570-127731`)
   Output uses helper routines `000052` (emit text), `122441` and `122405` (emit
   numeric values). None of these are MON calls.
4. Returns skip (success). (`127732`)

Only **one** MON 60 (RSTAT) and **no** other MON calls.

## How it fits ND-500 init

`STATUS` is the operator's window into the 3022/5015 communication interface: it shows
whether the ND-500 is present/ready and the raw interface + MAR state. It is a
diagnostic/monitoring command rather than an init step, but it is the command an
operator uses to check that the ND-500 came up.

## Unknown / inferred

- **UNKNOWN**: the identity of the third returned word (`,X 10`, `@B-165`). The yaml
  lists only two RSTAT parameters (`<status> <MAR>`), but the handler passes **three**
  address slots. The third is read and printed; its meaning is not established here.
- **PROVEN**: the status is a 2-word quantity (`@B-171`, `LDD`-width) split ND-500 /
  ND-100 halves per the two decode loops (SAA 5/limit 11, and SAA 12/limit 17).
- **INFERRED (role)**: `000052` = text emitter, `122441`/`122405` = value emitters.
  Their addresses and call sites are PROVEN; internals not carved.
- **INFERRED**: the per-bit message strings live in the bank-2 message table; the
  literal texts were not extracted.
