# FUNCS routine bodies - annotated disassemblies

Per-routine disassembly of the `FUNCS` operation routines (the ND-500 operations dispatched by
`FUNCS[code]`; see `../FUNCS-dispatch-table.md`). All from `030-S3SM5`, base `040000B`, byte-verified.
Grouped by category into annotated `.ASM` files (a folder-per-routine would be ~60 folders; grouped
listings are equivalent and complete).

## Common pattern (VERIFIED)
Each `FUNCS[code]` routine: validates the request, reads/writes fields of the **message block**
(`LDX ,B -11` = the message pointer; `,X <off>` = message fields per `../ND500-5MPM-MESSAGE-AND-ACTIVATION.md`),
and `JPL`s to the shared IOX helpers (`WADR`/`WRDAT`/`RDATL`/`REDAT` in the `051023B` driver) to move
data across the 3022. The reads/writes to the ND-500 register block and memory all funnel through that
IOX driver (`../ND500-3022-IOX-INTERFACE.md`).

## Groups carved - ALL FUNCS ROUTINE BODIES DONE (2026-07-15)
| file | routines | codes |
|------|----------|-------|
| [`FUNCS-register-memory.ASM`](FUNCS-register-memory.ASM) | REGRE REGWR PMREA DMREA AMEMR PMWRI DMWRI AMEMW | 000-005,032,033 - register + memory R/W |
| [`FUNCS-controlstore-micro.ASM`](FUNCS-controlstore-micro.ASM) | CSREA CSWRI MPSTA MPSTO 5MCLE CSLOA | 023,024,025,034,035,037 - control store + micro control |
| [`FUNCS-segload-primitives.ASM`](FUNCS-segload-primitives.ASM) | SGLOA NSGLO MOVxx PLACE LDATX..STDTX LDSWA | 006,160,007 + message-move + 5MPM primitives |
| [`FUNCS-file-process.ASM`](FUNCS-file-process.ASM) | REGSR REGSW PROGS CONFI CLOSF RESR5 REL50 LIOPF CTUSE WHOIS SETER | 010-022 |
| [`FUNCS-memconfig-reserve.ASM`](FUNCS-memconfig-reserve.ASM) | DEFMC RSTAT SWFDE SWFDL TSTFU RINTE G5PAG T5PAG RUNSW SPLAC EPLAC RMVER LIMEM MEMSP | 040-061 |
| [`FUNCS-nameseg-process.ASM`](FUNCS-nameseg-process.ASM) | SPRTE GPRTE SSGTE GSGTE RPHSG SPRNM TOSWP 500SR WSYSP S1PRI LINKT WPHSG SOUTF MRELS | 067-110,120,123 |
| [`FUNCS-domain-trace-cpu.ASM`](FUNCS-domain-trace-cpu.ASM) | 5DFSY 5NDFS 5SFSY 5DLSY 5LISY PLDEB GPRMA 5SRES COMSB SINPD SISTD DBUGS STSEL READS MONAC INITR CLRTR ARMTR DISAR DUMPT CLRAD GETCP SCACH RSCRR SETCU | 127-177 |

**COMPLETE:** all ~60 distinct `FUNCS` operation routines are disassembled (byte-verified vs
`030-S3SM5`, base `40000B`; ~11,000 lines total). Spot-checked coherent (`PROGS` clean dispatch;
`RSTAT` `IOXT` present). Every routine follows the common pattern above (message-block field access +
`JPL` to the shared IOX driver). The emulator-critical ops (register/memory transfer, control-store,
micro control, RSTAT status) are in the first two + memconfig files.

This completes the ND-500 system-monitor carve: dispatch, IOX interface + register map, control-store
gate, 5MPM message + activation, level-12 return path, AND every operation routine body.
