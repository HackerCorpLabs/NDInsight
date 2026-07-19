# MON 60B 111B - ISTAPRLOG (START-PROCESS-LOG-ONE)

Starts process logging for one specified ND-500 process (shares the histogram facility/buffer).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Facility must be free or the caller's (`5HRTP`), and no other logging function active (`5HIFLAG` != 2).
- `5DD1` = process number to log (`<=MX5PROCS`) -> `5LOGPROC`; reserve (`RTREF=:5HRTP`).
- `5HIFLAG:=2` (logging started); clear the log buffer. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[111]. From NPL: body. PENDING: L07 body address.
