# MON 60B 154B - IDBUGSW (DEBUG-SWAPPER on/off)

Turns swapper debugging on or off. When turning on (`5D12=1`), checks that no one else is using the ND-500.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF 5D12=1 THEN CALL XTUSON; GO FAR ERET` (on: fail if in use); else `GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC (154 not in 0..142 doc range; live-but-undocumented, matches caller's >142B note). From NPL: body. PENDING: L07 body address.
