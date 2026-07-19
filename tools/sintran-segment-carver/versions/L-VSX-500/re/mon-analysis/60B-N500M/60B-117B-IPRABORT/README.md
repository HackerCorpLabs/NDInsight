# MON 60B 117B/122B - IPRABORT / ILOGOFF (abort / logoff process)

Aborts (`117B`, ABORT-PROCESS) or logs off (`122B`, LOGOUT-PROCESS) a specified ND-500 process.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. COMPLEX -
touches 5MPM message buffer + drives the abort on the process's level.

## Handler (verbatim in `.npl`) - summary
- Validate process number (`5D11=0`, `5SWPROC < 5D12`, `A<=MX5PROCS`), else `EEILPAR`. If not reserved -> ok.
- ABORT (`5FUNCTION=PRSTOP`): if target is caller -> switch function to `ABREL`, `GO FAR 5NOPAR`; else
  set `5SYSABORT` (clear `SOFFLOGG`) in `PSTAT`.
- LOGOFF: if target is caller -> set `SOFFLOGG`, switch function to `XN5REL`, `GO FAR 5NOPAR`; else set `SOFFLOGG`.
- `INPRABORT` (shared with `135B IABLOG`): set `5IBRK|52ESCSET` in message-buffer flags under SLOCK,
  then trigger on the process's level (`IRW MLEVB`, SYSABORT). `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[117/122]. From NPL: body. PENDING: L07 body address. `INPRABORT` is a shared tail.
