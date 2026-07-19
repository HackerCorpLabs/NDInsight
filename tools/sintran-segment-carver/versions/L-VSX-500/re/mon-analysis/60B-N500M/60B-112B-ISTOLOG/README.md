# MON 60B 112B - ISTOLOG (STOP LOGGING)

Stops process/moncall logging (clears the started flag). Facility stays reserved.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF 5HRTP><RTREF THEN ELOGNRESERVED`; else `0=:5HIFLAG` (logging stopped). `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[112]. From NPL: body. PENDING: L07 body address.
