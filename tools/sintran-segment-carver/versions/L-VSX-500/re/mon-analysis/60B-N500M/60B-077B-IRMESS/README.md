# MON 60B 077B - IRMESS (READ MESSAGE)

Reads a process message buffer (from the 5MPM message area) back to the user - own process (-1) or a
specified ND-500 process number.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending. Touches the
5MPM message buffer (relevant to Phase 3 bus interface).

## Handler (verbatim in `.npl`)
- If `5D11=0 AND 5D12>>=5SWPROC` (a specific message request): `A=-1` -> own process (`5PRDESCR`);
  else validate proc no (`A>>MX5PROCS` -> `EEILPAR`), `X = A-5SWPROC * 5PRDSIZE + S500S`. `X.MESSBUFF`.
- Else `GO FAR ILLFUNC`.
- `XSUPDWINDOW` maps the message (bank `5MBBANK`); `TOUSMOVE` copies `(55MESSIZE-55MSNEGSIZE-3)*2`
  bytes to the user buffer `5P2`. `GO FAR 5OKRET`.

## Byte status
VERIFIED: dispatch + 5IFUNC[077]. From NPL: body. PENDING: L07 body address. Uses XSUPDWINDOW/TOUSMOVE
(resident, not carved).
