# MON 60B 114B - IRELLOG (release logging facility)

Stops logging and releases the facility (clears both the started flag and the reservation).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF 5HRTP><RTREF THEN ELOGNRESERVED`; else `0=:5HIFLAG=:5HRTP`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[114]. From NPL: body. PENDING: L07 body address.
