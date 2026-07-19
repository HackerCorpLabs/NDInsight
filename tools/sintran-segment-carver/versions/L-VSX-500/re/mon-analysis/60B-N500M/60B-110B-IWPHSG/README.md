# MON 60B 110B - IWPHSG (write into a physical segment)

Copies a data block from the user into the MON60 buffer for a physical-segment write.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `AD:=5DD3` byte count; reject if > `4000B` (`EBIGBUFF`).
- `T:=5D32; A:=5P4; CALL FRUSMOV` - copy `5D32` bytes from user (param4) into MON60 buffer.
- `GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC[110]. From NPL: body. PENDING: L07 body address.
