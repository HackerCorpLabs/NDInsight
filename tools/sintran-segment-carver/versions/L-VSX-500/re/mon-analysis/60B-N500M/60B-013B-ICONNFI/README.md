# MON 60B subfunction 013B - ICONNFI (CONNECT-FILE)

Connects a file for the ND-500 (operator CONNECT-FILE). Copies the file name and the file type into
the MON60 buffer, then hands off.

**Status:** dispatch byte-verified; body from `5P-P2-MON60.NPL` (`60B-013B-ICONNFI.npl`); L07 body
byte-location pending (bank-2 5IFUNC).

## Handler (verbatim NPL in `.npl`)
1. `T:=200; A:=5P1; CALL FRUSMOVE` - copy file name (param1, <=200B bytes).
2. `T:=4; A:=5P3; X:=100; CALL XFRUSMOVE` - copy file type (param3, 4 bytes).
3. `GO FAR 5NOPAR` - common path connects the file.

## Contract
- `params[1]` (5P1) = file-name string (<=200B bytes).
- `params[3]` (5P3) = 4-byte file type.

## Byte status
VERIFIED: dispatch + 5IFUNC[013]=ICONNFI. From NPL: body. PENDING: L07 body address.
