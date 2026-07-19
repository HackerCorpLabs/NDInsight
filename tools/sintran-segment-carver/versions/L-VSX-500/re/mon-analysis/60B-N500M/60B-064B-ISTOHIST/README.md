# MON 60B 064B - ISTOHIST (STOP HISTOGRAM)

Stops histogram sampling (removes the histogram message from the exec-queue via `RLHILOG`).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Require reserved by caller (`RTREF=5HRTP`), else `EHNRESERVED`.
- If started (`5HIFLAG=1`) -> `CALL RLHILOG` (release/dequeue). Clear `5HRTP`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[064]. From NPL: body. PENDING: L07 body address.
