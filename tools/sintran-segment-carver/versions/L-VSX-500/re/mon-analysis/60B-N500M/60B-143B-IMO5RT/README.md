# MON 60B 143B - IMO5RT (activate program in ND-500 or ND-100)

Activates a program: if the RT-descriptor has an ND-500 proc reserved, activates the ND-500 proc;
otherwise falls through to the ND-100 RT activation (`IIM5RT` executes an RT/abort MON).

Status: dispatch byte-verified; body from `5P-P2-MON60.NPL` (`.npl`); L07 body loc pending.

## Handler (verbatim in `.npl`)
- `5DD1` = RT-descriptor addr (`0` = caller). Validate via `GOODRT`.
- Under IOF: if it reserved an ND-500 proc (`FSEMA`) -> `5PRACTIVATE` (activate ND-500 proc). Then `IIM5RT` (`A:=153100; EXR SD` = execute MON RT).

## Byte status
VERIFIED: dispatch + 5IFUNC[143]. From NPL: body. PENDING: L07 body address. `IIM5RT`/`5ABFELL` execute an RT MON via EXR.
