# MON 60B 102B - IFORGET (STOP-ND-500: abort all active procs, release buffers)

Stops the whole ND-500 subsystem: reserves the CPU for special use, sets the STOP-ND-500 mode flag,
removes all processes from the exec-queue, and aborts every active ND-500 process not owned by the caller.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. COMPLEX -
touches the 5MPM message buffers and the exec-queue (Phase 3 relevant).

## Handler (verbatim in `.npl`) - summary
- `RTREF=:NSPREF` (reserve system), `SYSINITFLAG |= B5STOP` (STOP mode), `RSTARTALL` (empty exec-queue).
- Loop all process descriptors `S500S..MX5PROCS`: for each in use and not the caller's, mark
  `5SYSABORT` in `PSTAT`, set `5IBRK|52ESCSET` in the message-buffer flags under SLOCK, and trigger
  the abort on the process's level (`IRW MLEVB`, SYSABORT). `GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC[102]. From NPL: body. PENDING: L07 body address. RSTARTALL/SLOCK/SUNLOCK resident.
