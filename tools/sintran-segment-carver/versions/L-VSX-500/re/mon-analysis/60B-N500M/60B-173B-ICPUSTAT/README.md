# MON 60B 173B - ICPUSTAT (SET-CPU-STATUS)

Sets a CPU's status/availability (mark a CPU present/absent or excluded) by patching the 5PIT page
tables (`5IDPIT`/`5SDPIT`).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`) - summary
- Validate: `5D11=0`, CPU number `5D12-1 <= NCPU-1`, `5D41=0`, `5D42<=3`.
- Point `X` at the target CPU-DF `CPUAVAILABLE` word.
- If `5D22><0`: patch the `5IDPIT` PIT entry (`5NOTP` if A=0 else `5EXCL`), set 5PIT as alt-PIT, `MON 2WSEG`, alt off.
- If `5D32><0`: same for `5SDPIT`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[173]. From NPL: body. PENDING: L07 body address. Manipulates the 5PIT page tables directly.
