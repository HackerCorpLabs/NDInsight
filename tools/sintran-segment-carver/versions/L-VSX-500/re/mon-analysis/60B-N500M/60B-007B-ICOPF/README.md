# MON 60B subfunctions 007B/046B/047B/067B/071B/130B/131B - ICOPF (shared file-name-copy handler)

Seven subfunctions share ONE handler body (`ICOPF`) that copies a file/segment name from the caller
into the MON60 buffer, then hands off to the common path:

| code | name | operator command / purpose |
|------|------|----------------------------|
| 007B | IPLSWAPPER | LOAD-SWAPPER (place swapper) - **CPU-team priority** |
| 046B | IDEFSWAP | DEFINE-SWAP-FILE |
| 047B | IDELSWAP | DELETE-SWAP-FILE |
| 067B | ISPRTE | read process entry from name segment |
| 071B | ISSGTE | read phys-segment entry from name segment |
| 130B | ISFSYDOM | START STANDARD DOMAIN (NOT "PLACE-DOMAIN" - keyword unconfirmed; see cross-analysis) |
| 131B | IDLSYDOM | delete standard domain |

**Status:** dispatch byte-verified; body logic from `5P-P2-MON60.NPL` (see `60B-007B-ICOPF.npl`);
L07 body byte-location pending the bank-2 5IFUNC table (see `../60B-5IFUNC-dispatch-table.md`).

## Handler (verbatim NPL in `60B-007B-ICOPF.npl`)
`ICOPF: A:=5P1; T:=200; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy the file/segment name (param 1, <=200B
bytes) from user space into the MON60 buffer, then the common system-monitor path does the real work.

## Contract
- `A` -> param list; `params[0]` = one of the seven codes.
- `params[1]` (`5P1`) = pointer to the file/segment-name string; copied <=`200B` (128) bytes via `FRUSMOVE`.
- return: skip=success / direct=error (MON 60B convention).

## Byte status
VERIFIED: dispatch to N500M + range check + these seven 5IFUNC slots map to ICOPF (3-way).
From NPL: the ICOPF body. PENDING: L07 body address (bank-2 5IFUNC).
