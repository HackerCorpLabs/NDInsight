# MON 60B 061B - IMRESSPES (RESER: reserve memory for the ND-500 test-monitor)

Reserves memory / a CPU for the ND-500 test-monitor (RESER).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- If `5D11=-1 AND 5D12><-1` (specific CPU named): validate CPU number (`A-1>>NCPU-1` -> EEILPAR); set `CCPUDF` = that CPU descriptor; if already special-reserved by another (`X.SPREF><0 AND A><RTREF`) -> EESPRES.
- `OUT: CALL TUSON; GO FAR ERET` - fail if anyone else using this CPU now.
- `GO FAR 5NOPAR` - reserved for caller.

## Byte status
VERIFIED: dispatch + 5IFUNC[061]. From NPL: body. PENDING: L07 body address.
