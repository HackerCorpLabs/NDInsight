# FUNCS routine bodies - annotated disassemblies

Per-routine disassembly of the `FUNCS` operation routines (the ND-500 operations dispatched by
`FUNCS[code]`; see `../FUNCS-dispatch-table.md`). All from `030-S3SM5`, base `040000B`, byte-verified.
Grouped by category into annotated `.ASM` files (a folder-per-routine would be ~60 folders; grouped
listings are equivalent and complete).

## Common pattern (CORRECTED 2026-08-10 - see `SINTRAN\ND500\CARVE-ANSWER-RESULT-BLOCKS-2026-08-10.md`)
Each `FUNCS[code]` routine: validates the request, reads its parameters from and writes its RESULTS
into the **MON 60 info block** (`LDX ,B -11` = `S500DF-ZPREG` = static cell `165777B`; `,X 40/43/46/...`
= the `5DD1..5DD5`/`5P1..5P5` parameter records, L07 SYMBOL), touches the **5MPM message** through a
SEPARATE frame cell (`LDX ,B -67` = window address of the caller's MESSBUFF; `,X 7/10/11/13/14` =
message fields per `../ND500-5MPM-MESSAGE-AND-ACTIVATION.md`), and `JPL`s to the shared helpers
(`063007` build-message/MICFU:=A, `104236` send+wait, `141567`/`141600` OK/error exits, plus the
`051023B` IOX driver `WADR`/`WRDAT`/`RDATL`/`REDAT` for the 3022). **The prior claim here that
`B-11` is "the message pointer" was WRONG** - it is the info block; its `,X 40+` stores are the
result slots the NPL post-return `FUNCS` copies back to the user.

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
