# MON 60B 133B/150B - ILI5EXQ / ILI5TQU (LIST-EXECUTION-QUEUE / LIST-TIME-QUEUE)

Two subfunctions share `ILI5F`, differing by switch `K`: `133B` lists the ND-500 exec-queue (K=0),
`150B` lists the time-queue (K=1). **CPU-team caller call site for 133B = 111445.**

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. COMPLEX -
walks the 5MPM exec/time queue in IOF (relevant to Phase 3 bus interface).

## Handler (verbatim in `.npl`) - summary
- In IOF, map the MON60 buffer physical page into the PIT window.
- Seed the output buffer: time-queue -> `5ATIME` + start of time-queue (`X5BTI`); exec-queue -> current
  active proc + start of exec-queue (`MAILINK`, `X5BEX`).
- Walk the linked queue (`LDDTX` until `-1`), skipping `DUMMESS`; for each real entry with proc.no != -1,
  append: time-queue = process number + start-time (`D5TIM`); exec-queue = priority (`5PRIO`) + descriptor.
- Terminate with `-1`; `TOUSMOVE` copies the collected bytes to user `5P1`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[133/150] (+ caller call site 111445 for 133B). From NPL: body. PENDING: L07
body address. GETC5PROC/CNVBYADR/TOUSMOVE resident; walks the ND-500 exec/time queue.
