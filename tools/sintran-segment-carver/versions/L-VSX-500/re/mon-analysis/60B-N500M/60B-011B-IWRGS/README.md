# MON 60B 011B/056B - IWRGS / IEPLACE (write registers / end-place)

Shared body: copies the ND-500 register block from the user into the MON60 buffer.
- `011B` IWRGS - write registers (WRREG_BLOCK)
- `056B` IEPLACE - end-place (same register-copy prep)

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`T:=NREGS SH 2; A:=5P1; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy NREGS*4 bytes (NREGS 32-bit registers) from user (param1), then common path.

## Byte status
VERIFIED: dispatch + 5IFUNC[011/056]. From NPL: body. PENDING: L07 body address.
