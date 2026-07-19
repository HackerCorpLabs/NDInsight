# MON 60B 062B - IDEFHIST (DEFINE HISTOGRAM)

Defines a PC-sampling histogram over the ND-500: number of intervals (channels), interval size, and
start address. Reserves the histogram facility for the caller and clears the sampling buffer.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- If facility in use by another (`5HRTP><0 AND A><RTREF`) -> `EHIUSED`.
- Reserve: `RTREF=:5HRTP; 0=:5HIFLAG`.
- `5DD3` = number of intervals (channels): 1..`100B`, else `EEILPAR`; -> `5HICHANNELS`.
- `5DD2` = interval size: !=0, else `EEILPAR`; -> `5HINTERVAL`. `5DD1` = start -> `5HISTART`.
- Clear the sampling buffer `5HIDATA..5HIOUTSIDE`. `GO FAR 5OKRET`.

## Contract
- `5DD3` channels (<=100B), `5DD2` interval size, `5DD1` start address.

## Byte status
VERIFIED: dispatch + 5IFUNC[062]. From NPL: body. PENDING: L07 body address.
