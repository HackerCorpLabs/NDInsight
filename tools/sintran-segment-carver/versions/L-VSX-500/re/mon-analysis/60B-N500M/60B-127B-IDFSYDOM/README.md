# MON 60B 127B/161B - IDFSYDOM / INDFSYDOM (DEFINE-STANDARD-DOMAIN)

Defines a standard domain. `127B` = old domain format, `161B` = new domain format; both copy the
standard-domain info block into the MON60 buffer.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`A:=5P1; T:=4000; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy the domain info (param1, <=4000B bytes), then common path.

## Byte status
VERIFIED: dispatch + 5IFUNC[127/161]. From NPL: body. PENDING: L07 body address.
