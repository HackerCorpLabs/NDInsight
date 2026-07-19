# MON 60B 063B - ISTAHIST (START HISTOGRAM)

Starts the histogram sampling defined by `062B IDEFHIST`, by inserting a histogram message into the
ND-500 execution queue.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. COMPLEX -
touches the 5MPM message buffer and the ND-500 exec-queue (relevant to Phase 3 bus interface).

## Handler (verbatim in `.npl`) - summary
- Require the facility reserved by the caller (`RTREF=5HRTP`), else `EHNRESERVED`.
- If already started (`5HIFLAG><0`) -> ok.
- Compute caller process number (RDIV of `5PRDESCR-S500S` by `5PRDSIZE`).
- Under IOF+SLOCK: ensure the histogram message is not already queued, set CPU-bound flags in the
  message buffer, write the process number into `HIMESS`, then `ITO500XQ` inserts it into the ND-500
  exec-queue. Mark `5HIFLAG:=1` (started). `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[063]. From NPL: body. PENDING: L07 body address. Uses resident helpers
HIMFEXQUEUE/SLOCK/GCPUDF/ITO500XQ/SUNLOCK (not carved) + the 5MPM message buffer.
