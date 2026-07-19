# MON 60B 104B - IWSYSP (write system parameters)

Copies a 16-word system-parameter block from the user into `N500DF+SYSPAR`, then common path.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`T:=N500DF+SYSPAR; X:=16; 5P1=:D; MOVUS (K=0)` - copy 16 words from user `5P1` into `N500DF+SYSPAR`.
`GO FAR 5NOPAR`.

## Byte status
VERIFIED: dispatch + 5IFUNC[104]. From NPL: body. PENDING: L07 body address.
