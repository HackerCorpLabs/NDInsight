# MON 60B 100B/101B - RRFLAG / WWFLAG (read / write ND-500 process flags)

Reads or writes the flag word in an ND-500 process's data segment.
- `100B` RRFLAG - read flags (RFLAG)
- `101B` WWFLAG - write flags (SPFLAG)

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`) - summary
- `5D12` = process number (`-1` = own process); else validate (`5SWPROC < A <= MX5PROCS`, `RTRES!=0`)
  and resolve the process descriptor `X`.
- Map that process's ND-500 data segment (`M1MEXY`, `FF500`/`FT500` flag words).
- READ (`5FUNCTION=5RFLAG`): `AD:=FF500.DS0 -> 5DD2`, then `STDS0` copies flags to user `5P2`.
- WRITE: guard (only RT-program or user SYSTEM may write another proc's flags), `5DD2 -> FT500.DS0`.
- Restore caller's segments; `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[100/101]. From NPL: body. PENDING: L07 body address. `M1MEXY` = map ND-500
segment (resident, not carved).
