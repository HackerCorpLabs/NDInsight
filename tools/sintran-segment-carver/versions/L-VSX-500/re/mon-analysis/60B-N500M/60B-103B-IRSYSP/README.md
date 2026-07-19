# MON 60B 103B - IRSYSP (LIST-SYSTEM-PARAMETERS: read system variables)

Copies the 16-word ND-500 system-parameter block (`N500DF+SYSPAR`) to the user. Operator command
LIST-SYSTEM-PARAMETERS; **CPU-team caller call site 073132**.

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
`D:=N500DF+SYSPAR; T:=5P1; X:=16; MOVUS` - copy 16 words from `N500DF+SYSPAR` to user buffer `5P1`.
`GO FAR 5OKRET`.

## Contract
- out: 16-word system-parameter block copied to `params[1]` (5P1).

## Byte status
VERIFIED: dispatch + 5IFUNC[103] (+ caller call site 073132). From NPL: body. PENDING: L07 body address.
