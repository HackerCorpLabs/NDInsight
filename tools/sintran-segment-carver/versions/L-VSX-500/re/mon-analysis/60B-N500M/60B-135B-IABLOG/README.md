# MON 60B 135B - IABLOG (logoff process and abort RT-program)

Logs off an ND-500 process and aborts the owning RT-program.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `5D11=0` required; `5D12` = RT-descriptor addr (`0` = caller). Only RT-programs (`5BACKGR` clear) may be aborted; validate via `GOODRT`.
- Under IOF: if the RT-program reserved an ND-500 proc (`FSEMA`), set `5SYSABORT|SOFFLOG` in `PSTAT` and `GO FAR INPRABORT` (shared abort tail with `117B`); else `5ABPROG`.

## Byte status
VERIFIED: dispatch + 5IFUNC[135]. From NPL: body. PENDING: L07 body address. Shares `INPRABORT` tail with 117B.
