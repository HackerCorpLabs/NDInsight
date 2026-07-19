# MON 60B 075B - ITSTUSER (USYST: check if current user is user SYSTEM)

Privilege check: succeeds only if the caller is user SYSTEM (`5PASSTYPE=2`), else returns
"not authorised".

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF 5PASSTYPE=2 GO FAR 5OKRET` else `GO FAR EENAUTORISED`. (`5PASSTYPE`: 2 = user SYSTEM.)

## Byte status
VERIFIED: dispatch + 5IFUNC[075]. From NPL: body. PENDING: L07 body address.
