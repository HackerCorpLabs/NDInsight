# MON 60B 044B - ISREL (release ND-500 CPU/system from special use)

Releases a special-use reservation made by `043B ISRES` (SPREL).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `IF CCPUDF.SPREF=RTREF THEN CALL RELCPU` - if this CPU was special-reserved by the caller, release it.
- `IF NSPREF=RTREF THEN 0=:NSPREF` - if the system was special-reserved by the caller, clear it.
- `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[044]. From NPL: body. PENDING: L07 body address.
