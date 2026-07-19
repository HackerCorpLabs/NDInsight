# MON 60B 125B - IPRIMLOG (PRINT-MONCALL-LOG: read moncall log data)

Copies the moncall-log data buffer (`3000B` bytes) to the user.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Require reserved by caller (`5MLOG=RTREF`), else `ELOGNRESERVED`.
- `XSUPDWINDOW` maps the log buffer (`5FBUM60`); `TOUSMOVE` copies `3000B` bytes to user `5P1`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[125]. From NPL: body. PENDING: L07 body address.
