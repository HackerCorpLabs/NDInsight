# MON 60B 144B - ICHACPU (CHANGE-CPU)

Binds the caller's ND-500 process to a different ND-500 CPU (multi-CPU systems).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. Touches the
5MPM message buffer (Phase 3 relevant).

## Handler (verbatim in `.npl`) - summary
- Validate CPU number (`5D12-1 <= NCPU-1`); target CPU must be alive (`5ALIVE`), else `ENOCPU`.
- Public users cannot select an excluded CPU (`5EXCLUDE`) -> `ENAUTHORISED`. Reject if reserved for special use by another (`EESPRES`).
- If the old CPU-DF was special-reserved by the caller and the CPU is changing -> `RELCPU`.
- Write the new CPU-DF address + set `5CPUBOUND` in the process message buffer. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[144]. From NPL: body. PENDING: L07 body address.
