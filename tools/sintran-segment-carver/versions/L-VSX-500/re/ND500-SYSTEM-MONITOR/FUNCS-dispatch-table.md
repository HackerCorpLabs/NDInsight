# ND-500 System Monitor - the FUNCS dispatch table (the actual ND-500 operations)

`FUNCS = 142031B` in `030-S3SM5` is the system monitor's 128-entry operation table, **indexed by the
MON 60B subfunction code** - the server-side twin of the worker's `5IFUNC`. Where `5IFUNC[N]` marshals
the caller's parameters, **`FUNCS[N]` performs the actual ND-500 operation**. `5FP2E` (the entry) reads
the packaged function code and jumps through `FUNCS[code]`.

**Byte-verified:** dumped from `030-S3SM5.bin` (base `40000B`); every entry lands on a named
N500-SYMBOLS routine; `REGRE` (entry 0) and the entry code are coherent. Overlay validated (segment's
own symbol file + coherent code + release-doc PIT).

`ERRFP = 141574B` = the error/not-serviced-here handler. Codes that point at `ERRFP` are handled
entirely on the ND-100 side (in `N500M`/`5IFUNC` or the resident driver) and have no ND-500-side
operation - e.g. reserve/release for special use (`043`-`045`), histogram (`062`-`066`), most logging
(`111`-`115`, `124`-`126`), abort (`117`), which manipulate ND-100 process state, not the ND-500.

## The full path (all hops named)
```
command  -> [caller] thunk SAA <code>  -> gateway -> MON 60
         -> [worker] N500M 5IFUNC[code] (param marshalling) -> 5NOPAR -> FPT2ENTRY
         -> [sysmon] 5FP2E -> FUNCS[code] -> the ND-500 operation (REGRE / PMWRI / CSLOA / ...)
```

## FUNCS table (code -> routine @addr -> operation)

| code | routine | addr | operation |
|------|---------|------|-----------|
| `000` | REGRE | 142365 | read a register |
| `001` | REGWR | 142410 | write a register |
| `002` | PMREA | 142456 | logical program-memory read |
| `003` | DMREA | 142465 | logical data-memory read |
| `004` | PMWRI | 142527 | logical program-memory write |
| `005` | DMWRI | 142536 | logical data-memory write |
| `006` | SGLOA | 142637 | load (place) one segment |
| `007` | LDSWA | 143551 | load swapper |
| `010` | REGSR | 146410 | read register block |
| `011` | REGSW | 146426 | write register block |
| `012` | PROGS | 146451 | start ND-500 program (RUNN) |
| `013` | CONFI | 146761 | connect file |
| `014` | CLOSF | 147003 | close file |
| `015` | RESR5 | 147014 | reserve ND-500 process |
| `016` | REL50 | 147727 | release ND-500 process |
| `017` | LIOPF | 150533 | list open files |
| `020` | CTUSE | 151132 | time used |
| `021` | WHOIS | 151763 | who is on |
| `022` | SETER | 152160 | set error flag |
| `023` | CSREA | 152165 | read control store |
| `024` | CSWRI | 152373 | write control store |
| `025` | MPSTA | 152616 | micro start |
| `026` | DMEXA | 153051 | data-memory examine (4 bytes) |
| `027` | DMDEP | 153107 | data-memory deposit |
| `030` | PMEXA | 153042 | program-memory examine (4 bytes) |
| `031` | PMDEP | 153100 | program-memory deposit |
| `032` | AMEMR | 142474 | physical (absolute) data-memory read |
| `033` | AMEMW | 142545 | physical (absolute) data-memory write |
| `034` | MPSTO | 153146 | micro stop |
| `035` | 5MCLE | 153247 | master clear |
| `036` | MONSA | 153356 | (monitor save / undocumented) |
| `037` | CSLOA | 153441 | **LOAD CONTROL STORE (microcode load - the emulator gate)** |
| `040` | DEFMC | 155742 | define memory configuration |
| `041` | RSTAT | 156064 | read ND-500 interface status |
| `042` | EXAM  | 150416 | examine (log off + terminate) |
| `043`-`045` | ERRFP | 141574 | (not serviced here; ND-100-side reserve/special) |
| `046` | SWFDE | 156207 | define swap file |
| `047` | SWFDL | 156535 | delete swap file |
| `050` | TSTFU | 156701 | test function |
| `051` | RINTE | 162604 | read interface (IODATUT) register |
| `052` | G5PAG | 162670 | give ND-500 pages |
| `053` | T5PAG | 163372 | take ND-500 pages |
| `054` | RUNSW | 163621 | start (run) swapper |
| `055` | SPLAC | 164137 | start-place |
| `056` | EPLAC | 164407 | end-place |
| `057` | RMVER | 164652 | read micro-program version |
| `060` | LIMEM | 164776 | list memory configuration |
| `061` | MEMSP | 165041 | reserve memory (test-monitor) |
| `062`-`066` | ERRFP | 141574 | (histogram; ND-100-side) |
| `067` | SPRTE | 165511 | read process entry from name segment |
| `070` | GPRTE | 165771 | get process table entry |
| `071` | SSGTE | 166110 | read phys-segment entry from name segment |
| `072` | GSGTE | 166352 | get phys-segment table entry |
| `073` | RPHSG | 166537 | read from a physical segment |
| `074` | SPRNM | 166645 | set process name |
| `075` | ERRFP | 141574 | (user-SYSTEM check; ND-100-side) |
| `076` | TOSWP | 166733 | message to swapper |
| `077` | ERRFP | 141574 | (read message; ND-100-side) |
| `100`-`101` | ERRFP | 141574 | (flags; ND-100-side) |
| `102` | 500SR | 167127 | stop ND-500 system |
| `103` | ERRFP | 141574 | (read system vars; ND-100-side) |
| `104` | WSYSP | 167277 | write system parameters |
| `105` | S1PRI | 167325 | set priority |
| `106` | LINKT | 167523 | link to process |
| `107` | ERRFP | 141574 | (undocumented) |
| `110` | WPHSG | 167550 | write into a physical segment |
| `111`-`115` | ERRFP | 141574 | (process logging; ND-100-side) |
| `116` | REL50 | 147727 | log off own process (= release) |
| `117` | ERRFP | 141574 | (abort; ND-100-side) |
| `120` | SOUTF | 167723 | set output device |
| `121` | FSWPR | 142576 | read from swapper data memory |
| `122` | ERRFP | 141574 | (logout; ND-100-side) |
| `123` | MRELS | 167730 | release memory (test-monitor) |
| `124`-`126` | ERRFP | 141574 | (moncall logging; ND-100-side) |
| `127` | 5DFSY | 167736 | define standard domain |
| `130` | 5SFSY | 171122 | start standard domain |
| `131` | 5DLSY | 172015 | delete standard domain |
| `132` | 5LISY | 172263 | list standard domains |
| `133` | ERRFP | 141574 | (list exec-queue; ND-100-side) |
| `134` | PLDEB | 172660 | place debugger |
| `135` | ABLOG | 147734 | log off + abort RT-program |
| `136` | GPRMA | 173217 | activate stopped process |
| `137` | ERRFP | 141574 | (unused) |
| `140` | 5SRES | 173243 | (reserve/special-place pair) |
| `141` | COMSB | 173301 | set block size of a file |
| `142` | SINPD | 173370 | redefine default infant file |
| `143`-`144` | ERRFP | 141574 | (activate / change-cpu; ND-100-side) |
| `145` | SISTD | 173377 | start standard domain from S3 opcom |
| `146` | ERRFP | 141574 | (illegal) |
| `147` | RELS3 | 147744 | (escape while running std domain) |
| `150`-`153` | ERRFP | 141574 | (list-time-queue / illegal) |
| `154` | DBUGS | 173536 | debug swapper |
| `155` | STSEL | 174061 | (set select / free-for-patching) |
| `156` | READS | 174106 | read system info |
| `157` | MONAC | 174261 | write control store (alias 024) / monitor access |
| `160` | NSGLO | 142753 | load segment (new domain format) |
| `161` | 5NDFS | 167744 | define standard domain (new format) |
| `162` | INITR | 174526 | init tracer |
| `163` | CLRTR | 174562 | clear trace |
| `164` | ARMTR | 174576 | arm tracer |
| `165` | DISAR | 174612 | disarm tracer |
| `166` | DUMPT | 174626 | dump trace memory |
| `167` | CLRAD | 174654 | clear trace address |
| `170` | GETCP | 174676 | read ND-500 CPU-type + micro version |
| `171` | SCACH | 174723 | set cache |
| `172` | RSCRR | 174776 | read HW scratch register file |
| `173`-`176` | ERRFP | 141574 | (set-cpu-status / illegal; ND-100-side) |
| `177` | SETCU | 175051 | (set CU / FUNCMAX boundary) |

## Why this matters for the emulator
`FUNCS` is the implementation list for the ND-500 side of MON 60B. The routines that touch the ND-500
hardware / 5MPM message live here: `REGRE`/`REGWR` (register access via the memory-mapped register
block), `PMREA`/`PMWRI`/`AMEMR`/`AMEMW` (memory transfer), `CSLOA` (control-store load - the gate that
hung the emulator), `MPSTA`/`MPSTO`/`5MCLE` (micro start/stop/master-clear), `RSTAT` (interface
status - what the "control store loaded?" check reads). These are the concrete routines to carve for
Phase 1 (5MPM handshake) and Phase 3 (bus interface / bring-up).

**Next carve priority:** `CSLOA` (037, control-store load), `RSTAT` (041, interface status),
`REGRE`/`REGWR` (register access), `MPSTA`/`MPSTO`/`5MCLE` (micro control), and the `LDATX`/`STATX`
5MPM primitives at `143300B` - these are the emulator's control-store gate + register/memory interface.
