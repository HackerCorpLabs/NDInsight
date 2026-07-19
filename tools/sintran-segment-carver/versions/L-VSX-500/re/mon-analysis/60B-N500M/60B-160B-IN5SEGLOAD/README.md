# MON 60B 160B - IN5SEGLOAD (load/place one segment, new domain format)

The new-domain-format version of `006B ISEGLOAD`: copies a 12-byte segment name and, if shared parts
exist, a shared-info block.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `A:=5P1; T:=12; CALL FRUSMOVE` - copy 12-byte segment name.
- `IF 5D41><0` -> `A:=5P4; T:=300; X:=5; CALL XFRUSMOVE` - copy the shared-info block (300B bytes).
- `GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC[160]. From NPL: body. PENDING: L07 body address. Compare `006B ISEGLOAD` (old format).
