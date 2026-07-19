# TASK-03 — Per-handler RE of the GOTAB MON table

Do the [shared setup](README.md#shared-setup-every-task-does-this-first) first.

## Load
| Program | File | Base | Symbols |
|---|---|---|---|
| MON dispatch | `segments\116-S3SERWD.bin` | `0x600` (oct 3000) | `re\116-S3SERWD.ghidra-symbols.txt` |

The resident level-14 dispatch code was captured under this segment (S3MPIT/S3RPIT
have `madr=0` and don't carve; found here by a `GOTAB` content signature).

**Verify:** `0x74B0` (oct 072260) = `JMP ,X` (the dispatch); `0x74B6` (oct 072266)
= `000377` (the call-number mask); label `ENT14` = oct 072167. If those match, the
base is right.

## Goal
Walk the `GOTAB` jump table entry by entry and document each monitor call's
handler: what it does, and whether it finishes on level 14, hops to B-level
(level 4), or defers to an RT program.

## Verified so far (see `..\..\SINTRAN\OS\23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md`)
- `MON nnn` (oct 161000+nnn) → level 14 → mask to `T`=call number → `X:=GOTAB(T)`
  → `JMP ,X` at oct 072260.
- `GOTAB` = oct 071233, 256 words. `GOTAB(0)=MFELL=072114` (illegal),
  `GOTAB(1)=M1=071633` (read), `GOTAB(2)=M2=071635` (write), `M21..M24` at oct 21-24.
- I/O handlers (`M1`,`M2`,…) arm B-level via `IOB14=071660`; `MFELL/ACTMON` arm
  monitor level.
- Reference-manual list of MON calls: `..\..\Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md`.

## Steps
1. Define `GOTAB` (oct 071233) as a 256-word pointer array in Ghidra; each word is
   a handler address. Cross-reference each against the symbol file (`M1`, `M2`, …).
2. For each defined (non-`MFELL`) slot, follow the handler and classify its level
   (14 / 4 / RT program) and summarize its function; match to the MON-call name
   from the reference manual.
3. Flag the RT-program handoff path (`RWRT*`) and any ND-500 calls that jump out
   to the ND-500 monitor (TASK-02).

## Deliverable
Write `versions\L-VSX-500\re\TASK-03-results.md`: a table
`MON# → name → handler addr (hex+oct) → level → summary` for every defined GOTAB
slot, VERIFIED/UNCERTAIN. This upgrades doc 23 from "dispatch mechanism" to a full
per-call reference.
