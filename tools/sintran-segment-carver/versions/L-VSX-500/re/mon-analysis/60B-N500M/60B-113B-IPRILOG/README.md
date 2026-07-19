# MON 60B 113B - IPRILOG (READ LOG DATA / PRINT LOG INFO)

Copies sampled process-log data to the user; optionally clears the log buffer afterwards.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Require reserved by caller (`5HRTP=RTREF`), else `ELOGNRESERVED`.
- Size: PROCESS-LOG-ONE (`5HIFLAG=2`) -> 16 words; PROCESS-LOG-ALL -> `(MX5PROCS+1+3)*2`.
- `MOVUS` copies from `5HIDATA` to user `5P2`.
- If `5DD1=0` -> clear the log buffer. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[113]. From NPL: body. PENDING: L07 body address.
