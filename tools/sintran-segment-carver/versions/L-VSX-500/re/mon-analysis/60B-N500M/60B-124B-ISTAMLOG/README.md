# MON 60B 124B - ISTAMLOG (START-MONCALL-LOG)

Starts monitor-call logging, for all processes (user SYSTEM only) or for the caller's own process.
The first MON60 buffer is repurposed as the moncall-log buffer.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`) - summary
- If moncall-log in use by another (`5MLOG><RTREF AND A><0`) -> `ELOGINUSE`.
- `5DD1` selects scope: all procs (needs user SYSTEM, else `EENAUTORISED`) -> `5LOGPROC:=-1`; else own proc (compute proc no).
- `RTREF=:5MLOG`; repurpose the first MON60 buffer (`5FBUM60`) as the log buffer and clear it. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[124]. From NPL: body. PENDING: L07 body address.
