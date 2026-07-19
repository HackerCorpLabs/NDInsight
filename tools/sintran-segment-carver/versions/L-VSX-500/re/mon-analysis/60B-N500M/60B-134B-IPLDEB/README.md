# MON 60B 134B - IPLDEB (place debugger)

Places the ND-500 debugger domain; copies the debugger name into the MON60 buffer.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`A:=5P2; T:=200; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy the debugger name (param2, <=200B bytes), then common path.

## Byte status
VERIFIED: dispatch + 5IFUNC[134]. From NPL: body. PENDING: L07 body address.
