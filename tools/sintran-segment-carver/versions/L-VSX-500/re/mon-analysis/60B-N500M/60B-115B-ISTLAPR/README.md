# MON 60B 115B - ISTLAPR (START-PROCESS-LOG-ALL)

Starts logging for all ND-500 processes (`5HIFLAG:=3`).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Facility free or caller's, and no other logging function active (`5HIFLAG` != 3), else error.
- Reserve (`RTREF=:5HRTP`), clear the log buffer, `5HIFLAG:=3` (log-all started). `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[115]. From NPL: body. PENDING: L07 body address.
