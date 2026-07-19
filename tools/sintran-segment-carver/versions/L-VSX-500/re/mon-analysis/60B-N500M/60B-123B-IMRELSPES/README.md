# MON 60B 123B - IMRELSPES (release ND-500 + memory from the test-monitor)

Releases memory + the ND-500 reserved by `061B IMRESSPES` (RELMEM). Only proceeds if reserved by the caller.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`IF CCPUDF.SPREF><RTREF GO FAR 5OKRET` (not caller's -> just ok); else `GO FAR 5NOPAR` (common path releases memory).

## Byte status
VERIFIED: dispatch + 5IFUNC[123]. From NPL: body. PENDING: L07 body address.
