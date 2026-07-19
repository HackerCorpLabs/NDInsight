# MON 60 (N500M) Subfunction Table - Authoritative

Every `MON 60` subfunction code `000B`-`177B`, with its purpose and server-side handler,
taken from the SINTRAN worker source:
`SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL`

- **Purpose** column = the verbatim `% FUNCTION=nnn:` comment in that source (authoritative).
- **Handler** column = the routine named in the `INTEGER ARRAY 5IFUNC` dispatch (line 1319).
  `5NOPAR` = generic path (no special input-parameter marshalling; the message is forwarded
  to the ND-500 through the common `5NOPAR` routine). `ILLFUNC` = illegal/unimplemented
  (`EILFUNC`). `0` = table slot unused.
- **Client** column = the name used by the ND-100 monitor program's thunk (from
  `Developer/MON/calls/60B_N500M.yaml`), and the thunk address
  in `nd-500-mon-j04.prog` (see
  `SINTRAN/ND500/nd-500-mon/mon60-callers/INDEX.md`).

IMPORTANT: NPL is a different build revision than the L07 binary. Treat purposes/handler
names as authoritative LOGIC, confirmed where the L07 thunk exists. `FUNCMAX=177`
(`5P-P2-MON60.NPL:287`); codes `155`-`167` are annotated "free for patching". All codes octal.

This table is the reference for the per-command folders under
`SINTRAN/ND500/nd-500-mon/mon60-callers/`.

---

## Full table (000B - 177B)

| Code | Handler (5IFUNC) | Purpose (NPL FUNCTION= comment) | Client name (yaml) |
|------|------------------|--------------------------------|--------------------|
| `000` | 5NOPAR | READ A REGISTER | RRREG |
| `001` | 5NOPAR | (write a register) | WRREG |
| `002` | 5NOPAR | LOGICAL PROGRAM MEMORY READ | RPROG |
| `003` | 5NOPAR | LOGICAL DATA MEMORY READ | RDATA |
| `004` | IPMWRITE | LOGICAL PROGRAM MEMORY WRITE | WPROG |
| `005` | IDMWRITE | LOGICAL DATA MEMORY WRITE | WDATA |
| `006` | ISEGLOAD | LOAD (PLACE), ONE SEGMENT | PLACE |
| `007` | IPLSWAPPER | PLACE SWAPPER | SWLOD |
| `010` | 5NOPAR | READ ALL REGISTERS | RRREG_BLOCK |
| `011` | IWRGS | WRITE REGISTERS | WRREG_BLOCK |
| `012` | 5NOPAR | START ND-500 PROGRAM | RUNN |
| `013` | ICONNFI | CONNECT FILE | CNCFI |
| `014` | 5NOPAR | (close file) | CLSFI |
| `015` | 5NOPAR | RESERVE ND-500 PROCESS | RESRV |
| `016` | 5NOPAR | RELEASE ND-500 PROCESS | RELIS |
| `017` | 5NOPAR | (list open files) | LISOP |
| `020` | 5NOPAR | (time used) | TIMUS |
| `021` | 5NOPAR | (who is on) | WHO |
| `022` | 5NOPAR | (set error flag) | ERRFL |
| `023` | 5NOPAR | READ CONTROL STORE (equal for func=157) | REACS |
| `024` | IWCNT | WRITE CONTROL STORE | WRICS |
| `025` | 5NOPAR | (micro start) | MICST |
| `026` | 5NOPAR | DATA MEMORY EXAMINE (4 BYTES) | DMEXAM |
| `027` | 5NOPAR | (data memory deposit) | DMDEP |
| `030` | 5NOPAR | PROGRAM MEMORY EXAMINE (4 BYTES) | PMEXAM |
| `031` | 5NOPAR | (program memory deposit) | PMDEP |
| `032` | 5NOPAR | PHYSICAL DATA MEMORY READ | ABSMR |
| `033` | IDAMW | PHYSICAL DATA MEMORY WRITE | ABSMW |
| `034` | 5NOPAR | (micro stop) | MSTOP |
| `035` | 5NOPAR | (master clear) | MSTCL |
| `036` | 5NOPAR | (undocumented; live thunk exists) | -- |
| `037` | ICSLOAD | LOAD CONTROL STORE (LOAD A FILE INTO CS) | LDCS |
| `040` | 5NOPAR | (define memory configuration) | DEFM |
| `041` | 5NOPAR | READ ND-500 INTERFACE STATUS | RSTAT |
| `042` | 5NOPAR | LOG OFF PROC. AND TERMINATE ND-100 PROGR. | -- |
| `043` | ISRES | RESERVE ND-500 CPU/SYSTEM FOR SPECIAL USE | SPRES |
| `044` | ISREL | RELEASE ND-500 CPU/SYSTEM FROM SPECIAL USE | SPREL |
| `045` | 5NOPAR | (see 046/047 swap-file pair) | DEFSW |
| `046` | IDEFSWAP | DEFINE SWAP FILE | -- |
| `047` | IDELSWAP | DELETE SWAP FILE | DELSW |
| `050` | 5NOPAR | (test function) | TESTF |
| `051` | 5NOPAR | READ INTERFACE (COMMUNICATION), IODATUT REGISTER | RIFRG |
| `052` | 5NOPAR | (give ND-500 pages) | G500P |
| `053` | 5NOPAR | (take ND-500 pages) | T500P |
| `054` | 5NOPAR | (start swapper) | STSWP |
| `055` | ISPLACE | START-PLACE | SPLAC |
| `056` | IEPLACE | END-PLACE | EPLAC |
| `057` | 5NOPAR | READ MICRO PROGRAM VERSION | MPVER |
| `060` | 5NOPAR | LIST MEMORY CONFIGURATION | LIMEM |
| `061` | IMRESSPES | RESERVE MEMORY FOR THE ND-500 TEST-MONITOR | -- |
| `062` | IDEFHIST | DEFINE HISTOGRAM | HIDEF |
| `063` | ISTAHIST | START HISTOGRAM | HISTA |
| `064` | ISTOHIAT | STOP HISTOGRAM | HISTP |
| `065` | IREAHIST | READ HISTOGRAM | HISTN |
| `066` | IRELHIST | STOP AND RELEASE HISTOGRAM | HIREL |
| `067` | ISPRTE | READ PROCESS ENTRY FROM NAME SEGMENT | SPRTE |
| `070` | 5NOPAR | READ A PROCESS TABLE ENTRY FROM THE SYS.MON | GPRTE |
| `071` | ISSGTE | READ PHYS.SEGMENT ENTRY FROM NAME SEGMENT | SSGTE |
| `072` | 5NOPAR | READ A PHYS.SEGMENT TABLE ENTRY FROM SYS.MON | GSGTE |
| `073` | 5NOPAR | READ FROM A PHYSICAL SEGMENT | RPHSG |
| `074` | ISPRNM | SET NAME ON CURRENT PROCESS | SPRNM |
| `075` | ITSTUSER | CHECK IF CURRENT USER IS USER SYSTEM | USYST |
| `076` | ITOSWP | MESSAGE TO SWAPPER | TOSWP |
| `077` | IRMESS | READ MESSAGE | RPROC |
| `100` | RRFLAG | READ FLAGS FROM ND-500 DATA SEGMENT | RFLAG |
| `101` | WWFLAG | WRITE FLAGS INTO ND-500 DATA SEGMENT | SPFLAG |
| `102` | IFORGET | STOP ND-500 SYSTEM (ABORT ALL ACTIVE PROCS, RELEASE MON60 BUFFERS) | GPSGE |
| `103` | IRSYSP | READ SYSTEM VARIABLES | RSYSP |
| `104` | IWSYSP | WRITE SYSTEM PARAMETERS | WSYSP |
| `105` | 5NOPAR | SET PRIORITY | SPRIO |
| `106` | 5NOPAR | (link to process) | LNKPR |
| `107` | 5NOPAR | (undocumented) | -- |
| `110` | IWPHSG | WRITE INTO A PHYSICAL SEGMENT | WPHSG |
| `111` | ISTAPRLOG | START PROCESS LOG ONE | SLOG1 |
| `112` | ISTOLOG | STOP LOGGING | STOPLOG |
| `113` | IPRILOG | READ LOG DATA (PRINT LOG INFO) | RLOG |
| `114` | IRELLOG | STOP LOGGING AND RELEASE LOGGING FACILITY | RELLOG |
| `115` | ISTLAPR | START PROCESS-LOG-ALL | SLOGA |
| `116` | 5NOPAR | LOG OFF OWN PROCESS | -- |
| `117` | IPRABORT | ABORT PROCESS | ABORT |
| `120` | 5NOPAR | (set output device) | SETOUT |
| `121` | 5NOPAR | READ FROM SWAPPERS DATA MEMORY (LOGICAL ADDRS) | RDSWP |
| `122` | ILOGOFF | LOGOFF PROCESS | LOGOUT |
| `123` | IMRELSPES | RELEASE ND-500 AND MEMORY USED BY THE ND-500 TEST SYSTEM (HAREM) | RELMEM |
| `124` | ISTAMLOG | START MONITOR CALL LOG | SMONLOG |
| `125` | IPRIMLOG | READ MONCALL LOG DATA (PRINT MONCALL LOG) | PMONLOG |
| `126` | ISTOMLOG | STOP AND RELEASE MONCALL LOG | XMONLOG |
| `127` | IDFSYDOM | DEFINE STANDARD DOMAIN | DEFDOM |
| `130` | ISFSYDOM | START STANDARD DOMAIN | PLADOM (see note) |
| `131` | IDLSYDOM | DELETE STANDARD DOMAIN | DELDOM |
| `132` | 5NOPAR | (list standard domains) | LSTDOM |
| `133` | ILI5EXQ | LIST ND-500 EX-QUEUE | LSTEXQ |
| `134` | IPLDEB | PLACE DEBUGGER | PLADBG |
| `135` | IABLOG | LOG OFF PROCESS AND ABORT ND-100 PROGRAM (RT-PROGRAM) | -- |
| `136` | IPRACTIVE | ACTIVATE STOPPED PROCESS | ACTIV |
| `137` | 0 | (table slot unused) | UNUSED |
| `140` | 5NOPAR | (undocumented) | -- |
| `141` | 5NOPAR | (set block size of a file) | SETBLK |
| `142` | 5NOPAR | (redefine default infant file) | DEFINF |
| `143` | IMO5RT | ACTIVATE PROGRAM EITHER IN ND-500 OR IN ND-100 | -- |
| `144` | ICHACPU | CHANGE CPU | -- |
| `145` | ISSTDOM | START STANDARD DOMAIN FROM S3 OP.COM. | -- |
| `146` | ILLFUNC | (illegal) | -- |
| `147` | ILLFUNC | (dispatch=ILLFUNC; a FUNCTION=147 comment "ESCAPE TYPED WHILE RUNNING STD DOMAIN" exists - reached by another path) | -- |
| `150` | ILI5TQU | LIST ND-500 TIME-QUEUE | -- |
| `151` | ILLFUNC | (illegal) | -- |
| `152` | ILLFUNC | (illegal) | -- |
| `153` | ILLFUNC | (illegal) | -- |
| `154` | 5NOPAR | DEBUG SWAPPER <ON/OFF> | -- |
| `155` | 5NOPAR | (free for patching) | -- |
| `156` | 5NOPAR | READ SYSTEM INFO | -- |
| `157` | IWCNT | WRITE CONTROL STORE (equal for func=024) | -- |
| `160` | IN5SEGLOAD | LOAD (PLACE), ONE SEGMENT (NEW DOMAIN FORMAT) | -- |
| `161` | INDFSYDOM | DEFINE STANDARD DOMAIN (NEW DOMAIN FORMAT) | -- |
| `162` | 5NOPAR | (undocumented) | -- |
| `163` | 5NOPAR | (undocumented) | -- |
| `164` | 5NOPAR | (undocumented) | -- |
| `165` | 5NOPAR | (undocumented) | -- |
| `166` | 5NOPAR | DUMP-TRACE-MEMORY | -- |
| `167` | 5NOPAR | (undocumented) | -- |
| `170` | 5NOPAR | READ ND-500 CPU-TYPE AND MIC.VERSION | -- |
| `171` | 5NOPAR | (undocumented) | -- |
| `172` | 5NOPAR | READ HW SCRATCH REGISTER FILE | -- |
| `173` | ICPUSTAT | SET CPU STATUS | -- |
| `174` | ILLFUNC | (illegal) | -- |
| `175` | ILLFUNC | (illegal) | -- |
| `176` | ILLFUNC | (illegal) | -- |
| `177` | 5NOPAR | (undocumented; FUNCMAX boundary) | -- |

---

## Notes and reconciliations

1. **`5NOPAR` is not "no-op".** It is the common input path: the handler computes the
   process number and forwards the message to the ND-500 without special parameter
   marshalling (`5P-P2-MON60.NPL:1889`). A subfunction can be fully functional and still
   dispatch to `5NOPAR` (e.g. `041` READ INTERFACE STATUS, `060` LIST MEMORY CONFIG).

2. **Subfunction `130` = START STANDARD DOMAIN**, per the NPL, not "place domain". The
   yaml client name `PLADOM` and the NDInsight table sit on this code, but the authoritative
   purpose is START STANDARD DOMAIN (`ISFSYDOM`), one of the standard-domain family
   `127`/`130`/`131` (define/start/delete). The operator command **PLACE-DOMAIN** most
   likely brackets a place with `055` START-PLACE + `006` LOAD-ONE-SEGMENT + `056`
   END-PLACE rather than issuing `130`. Which operator command reaches `130` needs the
   command dispatch table to confirm; do not assume it is PLACE-DOMAIN.

3. **This binary extends past the documented table.** The NDInsight yaml documents `0`-`142B`.
   Codes `143B`-`173B` are real here: `143` activate-program, `144` change-CPU, `145`
   start-standard-domain-from-op.com, `150` list-time-queue, `154` debug-swapper, `156`
   read-system-info, `157` write-control-store (alias of `024`), `160`/`161` new-domain-format
   place/define, `166` dump-trace-memory, `170` read-CPU-type+mic-version, `172`
   read-HW-scratch-register-file, `173` set-CPU-status. The remaining `5NOPAR` slots above
   `142B` have no `FUNCTION=` comment and are marked undocumented.

4. **Duplicate `WRITE CONTROL STORE`**: `024` and `157` share the handler `IWCNT`
   (the NPL notes "equal for func=157"). Likewise `023` READ CONTROL STORE notes
   "equal for func=157" in the read direction.

5. **`147`** dispatches to `ILLFUNC` in the array, yet a `FUNCTION=147` comment
   ("ESCAPE TYPED WHILE RUNNING STANDARD DOMAIN STARTED FROM S3 OP.COM.") exists - the
   escape path is reached by a mechanism other than the `5IFUNC` index. Noted, not resolved.

Source of every purpose string: the `% FUNCTION=nnn:` comments and the `5IFUNC` array in
`SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL`.
