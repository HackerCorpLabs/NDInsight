# MON 60B 076B - ITOSWP (send message to the swapper)

Copies a message body from the user into the MON60 buffer and hands off to the common path, which
delivers it to the ND-500 swapper.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`A:=5P1+7; T:=55MESSIZE-7 SH 1; CALL FRUSMOVE ; GO FAR 5NOPAR` - copy `(55MESSIZE-7)*2` bytes starting
at `5P1+7` (the first 7 words are a header set elsewhere) into the MON60 buffer.

## Byte status
VERIFIED: dispatch + 5IFUNC[076]. From NPL: body. PENDING: L07 body address. `55MESSIZE` = swapper
message size in words. Relevant to swapper interface (see other session).
