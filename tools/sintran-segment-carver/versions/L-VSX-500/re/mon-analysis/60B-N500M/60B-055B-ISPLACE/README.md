# MON 60B 055B - ISPLACE (START-PLACE)

Begins a place operation by clearing the `55REP` bit in the caller process message-buffer flags word (in the 5MPM message buffer).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `X:=5PRDESCR.MESSBUFF` - caller process descriptor message buffer.
- `*AAX 5MSFL; IOF; LDATX` - address flags word `5MSFL` in bank `5MBBANK`; load it (interrupts off).
- `A BZERO 55REP; *STATX; ION` - clear the `55REP` bit and store back.
- `GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC[055]. From NPL: body. PENDING: L07 body address. Touches the 5MPM message buffer - relevant to Phase 3 bus interface.
