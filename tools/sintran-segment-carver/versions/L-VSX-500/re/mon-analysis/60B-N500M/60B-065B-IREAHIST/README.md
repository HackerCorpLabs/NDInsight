# MON 60B 065B - IREAHIST (READ HISTOGRAM)

Copies the sampled histogram data (2 words per channel) plus the outside-range count back to the user.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- Require reserved by caller (`RTREF=5HRTP`), else `EHNRESERVED`.
- `MOVUS` copies `200B` from `5HIDATA` to user buffer `5P1` (2 words/channel).
- `STDS0` copies the `5HIOUTSIDE` outside-range count to `5P1+200`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[065]. From NPL: body. PENDING: L07 body address.
