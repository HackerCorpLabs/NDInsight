# MON 60B 145B - ISSTDOM (start system domain from SINTRAN command)

Starts a system domain invoked from a SINTRAN III command; marks that on ESCAPE control returns to
SINTRAN OPCOM. This is the byte-visible `SAT 145B` boundary in the L07 dispatcher (last valid code
before `ILLFUNC`).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`0=:5P1; GO FAR 5NOPAR` - zero param1 (mark return-to-OPCOM-on-escape), then common path.

## Byte status
VERIFIED: dispatch + 5IFUNC[145]; matches L07 dispatcher `SAT 145B` boundary. From NPL: body. PENDING: L07 body address.
