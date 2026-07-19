# MON 60B - error entry points + ERET

The `5IFUNC` dispatcher and handlers branch to these labels on error. Each loads an error code and
falls to `ERET`, which saves the code and returns (direct return = error per the MON 60B convention).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Error entry points (verbatim in `.npl`)
| label | error code | meaning |
|-------|-----------|---------|
| `EENIMPLEMENT` | `ENIMPLEMENT` | not implemented |
| `EEFUNRTP` | `EFUNRTP` | function not legal for RT-program |
| `EENPROC` | `ENOPROC` | no ND-500 process |
| `EENPCOMMU` | `ENOPCOMMU` | no ND-500 communication |
| `EENAUTHORIZED` | `ENAUTHORISED` | not authorised (not user SYSTEM) |
| `EESPRES` | `ESPRES` | reserved for special use by another |
| `EEILPAR` | `EC174` | illegal parameter (error 174B) |
| `ILLFUNC` | `EILFUNC` | illegal function code |

## ERET (verbatim in `.npl`)
`ERET` sets `B=N500DF`, saves the error code in `ZAREG`, does the level switch, and returns via `RET5`
(direct return = error). Special case: for `N5REL` on first access it returns ok (`5OKRET`).

## Byte status
VERIFIED: dispatch reaches these on the error branches (byte-visible `JMP I 133` etc. in the L07
dispatcher). From NPL: bodies. PENDING: L07 addresses.
