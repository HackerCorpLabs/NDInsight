# MON 60B 066B - IRELHIST (STOP AND RELEASE HISTOGRAM)

Stops (if running) and releases the histogram facility. Like `064B` but releases regardless of the
started flag when non-zero.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Require reserved by caller, else `EHNRESERVED`.
- If `5HIFLAG><0` -> `CALL RLHILOG`. Clear `5HRTP`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[066]. From NPL: body. PENDING: L07 body address.
