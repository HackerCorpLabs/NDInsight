# MON 60B subfunction 006B - ISEGLOAD (PLACE / load one segment)

Loads (places) one ND-500 segment. Copies the segment name, and if the segment has ND-100/ND-500
shared parts, also copies the shared-part info.

**Status:** dispatch byte-verified; body from `5P-P2-MON60.NPL` (`60B-006B-ISEGLOAD.npl`); L07 body
byte-location pending (bank-2 5IFUNC).

## Handler (verbatim NPL in `.npl`)
1. `A:=5P1; T:=200; CALL FRUSMOVE` - copy segment name (param1, <=200B bytes) user->MON60 buffer.
2. `IF 5D51><0` - if there are shared parts, `A:=5P5; T:=40; X:=100; CALL XFRUSMOVE` copies the
   shared-info block (param5, 40B bytes) too.
3. `GO FAR 5NOPAR` - common path performs the place.

## Contract
- `params[1]` (5P1) = segment-name string (<=200B bytes).
- `5D51` (a field of param5) != 0 => shared parts present.
- `params[5]` (5P5) = shared-info block (40B bytes) when shared.

## Byte status
VERIFIED: dispatch + 5IFUNC[006]=ISEGLOAD. From NPL: body. PENDING: L07 body address.
