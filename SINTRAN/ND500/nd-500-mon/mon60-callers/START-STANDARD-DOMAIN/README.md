# START-STANDARD-DOMAIN

MON 60 subfunction **130B** (octal) = **0x58** = 88 decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **START STANDARD DOMAIN**
(server handler `ISFSYDOM`). yaml name for 130B: PLADOM "Place standard domain".

> **Naming note.** The task's original label "PLACE-DOMAIN" for this call site was
> corrected against the authoritative NPL source: subfunction 130B is *START*
> standard domain, not "place domain". The operator command that reaches call site
> 043171 was **not** verified against the bank-2 command-string table, so this folder
> is named by the authoritative subfunction PURPOSE (130B). The distinct operator
> "PLACE" bracket uses subfunctions 055B (start-place) / 006B (place one segment) /
> 056B (end-place) - and this same routine issues 055B/056B as part of its work.

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location

| Item | Value | Status |
|---|---|---|
| Handler | **STANDALONE ENTER-routine at 043011** (framesize 000717 = 463 locals), spans 043011..045462 | PROVEN |
| MON 60 call site (130B) | **043171** `JPL I 32` -> ptr 043223, `bank1[043223]=146712` = thunk `SAA 130` | PROVEN |
| Error path (130B) | 043172 -> 043071 (local), ends at LEAVE(value) 177327 via ptr 043217 | PROVEN |
| Success path | 043173 (fall-through); routine success -> LEAVE-SKIP via 043442 | PROVEN |

## MON 60 subfunction(s) used (all PROVEN by thunk resolution)

| Subfn | Octal / Hex | Thunk | Purpose (NPL) | Site | Parameter-block layout | Skip/Error |
|---|---|---|---|---|---|---|
| (130B) | 130B / 0x58 | 146712 | START STANDARD DOMAIN | 043171 | `,X 6` = name word (@B-162, from F @B-171); `,X 7` = [33] flag | err=043172; ok=043173 |
| SRESPL | 140B / 0x60 | 146737 | Start residual place | 043547 | (set up in body) | err->043321 |
| SPLAC | 55B / 0x2D | 146530 | Start place | 043552 | (none marshalled at site) | err->043321 |
| EPLAC | 56B / 0x2E | 146533 | End place | 044062 | `,X 6` = name (@B+76) | err->043321 |

Byte citations: `043223`=`146712` (130B thunk, `SAA 130`); `043674`=`146737`
(SRESPL); `043676`=`146530` (SPLAC); `044117`=`146533` (EPLAC). yaml param for 130B:
`<name>`.

## What it does

1. Copies the domain-name descriptor (F-image at `B-171`) and issues `MON 60` 130B
   to **start the standard domain**, passing the name and a flag read from global
   `[33]`. (`043160-043172`)
2. Runs a large body (443 locals) that builds a place descriptor and formats operator
   output. On a local flag (`@B-163`) it selects **SRESPL** (140B, start *residual*
   place) or **SPLAC** (55B, start place). (`043545-043553`)
3. Places segment(s) and finally issues **EPLAC** (56B, end place). (`044062`)
4. Any MON 60 error unwinds to the routine's LEAVE(value) error return; full success
   takes LEAVE-SKIP.

So a single operator action here both *starts the standard domain* and *brackets a
place operation* around it.

## How it fits ND-500 init

Standard domains are the ND-500's loadable program environments. Starting a standard
domain and placing its segments in ND-500 memory is a core part of preparing the
ND-500 to run a program.

## Unknown / inferred

- **UNKNOWN**: the exact meaning of param 2 for 130B (`,X 7` = global `[33]`). It is a
  program global read at `043167`; its identity/role was not established.
- **UNKNOWN (operator command name)**: which operator keyword reaches 043011. The
  folder is named by subfunction PURPOSE per the coordinator's instruction.
- **PROVEN**: this routine issues exactly the four MON 60 subfunctions above; no other
  MON 60 thunks resolve inside 043011..045462, and no MON <n> instructions appear in
  the carved regions.
- **INFERRED (role)**: `043321`/`043071` are local error joins; `043442` is the
  success LEAVE-SKIP join. Addresses and reachability PROVEN.
- **SUMMARISED**: the 1322-line body's formatting/placement logic is not carved
  line-by-line; the `.asm` here carries the four MON 60 regions plus the entry.
