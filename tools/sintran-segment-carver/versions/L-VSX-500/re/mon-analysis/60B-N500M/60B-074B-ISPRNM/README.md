# MON 60B 074B/136B - ISPRNM / IPRACTIVE (set process name / activate stopped process)

Shared body: copies a process name (<=50B bytes) from the user into the MON60 buffer.
- `074B` ISPRNM - SET-PROCESS-NAME
- `136B` IPRACTIVE - activate stopped process (by name)

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`A:=5P1; T:=50; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy the process name (param1, <=50B bytes), then common path.

## Byte status
VERIFIED: dispatch + 5IFUNC[074/136]. From NPL: body. PENDING: L07 body address.
