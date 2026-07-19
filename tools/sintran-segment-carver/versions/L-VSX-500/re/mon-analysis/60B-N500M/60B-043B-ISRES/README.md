# MON 60B 043B - ISRES (reserve ND-500 CPU/system for special use)

Reserves the ND-500 CPU (or whole system) for the calling RT program (SPRES).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- If `5D11=0 AND 5D12=0` (CPU only): if already reserved by caller (`CCPUDF.SPREF=RTREF`) -> ok; else `CALL TUSON` (fail if anyone else using this CPU) -> error.
- Else (whole system): if already reserved by caller (`RTREF=NSPREF`) -> ok; else set `NSPREF:=A`, `CALL XTUSON` (mark unavailable + check other logged-on users) -> error.
- `GO FAR 5OKRET` on success.

## Byte status
VERIFIED: dispatch + 5IFUNC[043]. From NPL: body. PENDING: L07 body address. TUSON/XTUSON/RELCPU are resident helpers (not carved).
