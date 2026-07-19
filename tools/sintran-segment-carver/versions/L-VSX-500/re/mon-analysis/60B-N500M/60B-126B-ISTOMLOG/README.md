# MON 60B 126B - ISTOMLOG (STOP-MONCALL-LOG: stop and release)

Stops and releases the moncall-log facility.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF 5MLOG><RTREF THEN ELOGNRESERVED`; else `0=:5MLOG=:5MLOPROC`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[126]. From NPL: body. PENDING: L07 body address.
