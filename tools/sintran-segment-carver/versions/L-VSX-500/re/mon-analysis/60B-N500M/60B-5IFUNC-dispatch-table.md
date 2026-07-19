# MON 60B / N500M - the 5IFUNC subfunction dispatch table (authoritative)

**Source:** `SINTRAN/NPL-SOURCE/NPL/5P-P2-MON60.NPL` - the NPL source of the
MON 60 worker itself. The `INTEGER ARRAY 5IFUNC` (NPL lines 1319-1335) is the 128-entry
(`000B`-`177B`) jump table; `params[0]` (the subfunction code) indexes it, and the entry is the
**parameter-preparation routine** to run before the common system-monitor call.

**Grade: NPL (logic/names). Cross-verified three ways** (see bottom). Individual handler bodies still
to be byte-confirmed against L07 `050-S3I5PIT` - but the structure is nailed.

**How dispatch works** (NPL 5P-P2-MON60.NPL:1294-1296, matches the L07 bytes at `030416B`):
```
X := 5FUNCTION            % params[0], the subfunction code (range-checked to 177B)
A := 5IFUNC(X)            % entry point for this function's parameter prep  (031310B)
A =: P                    % jump to it
```
Each entry then copies user parameters into the MON60 buffer (`FRUSMOVE`/`XFRUSMOVE`) and does
`GO FAR 5NOPAR` to the common path, or is `5NOPAR` (no prep needed - e.g. pure reads), or `ILLFUNC`
(illegal), or `0` (null slot).

## The table (octal code -> 5IFUNC handler; user-facing name; notes)

| code | 5IFUNC handler | user-facing (doc/caller) | notes |
|------|----------------|--------------------------|-------|
| 000-003 | `5NOPAR` | RRREG/WRREG/RPROG/RDATA | reads: no user->buf prep |
| 004 | `IPMWRITE` | WPROG | logical program-mem write; copies <=4000B bytes |
| 005 | `IDMWRITE` | WDATA | logical data-mem write |
| 006 | `ISEGLOAD` | PLACE | load/place one segment (name + shared info) |
| 007 | `IPLSWAPPER` | SWLOD (LOAD-SWAPPER) | place swapper; copies file name |
| 010 | `5NOPAR` | RRREG_BLOCK | |
| 011 | `IWRGS` | WRREG_BLOCK | write registers block |
| 012 | `5NOPAR` | RUNN (start program) | |
| 013 | `ICONNFI` | CNCFI (Connect File) | copies file name + type |
| 014-017 | `5NOPAR` | CLSFI/... | |
| 020-023 | `5NOPAR` | TIMUS/WHO/ERRFL/REACS | |
| 024 | `IWCNT` | WRICS (Write Control Store) | copies <=2000B CS words |
| 025-032 | `5NOPAR` | MICST/DMEXAM/DMDEP/PMEXAM/PMDEP/ABSMR | |
| 033 | `IDAMW` | ABSMW (physical data-mem write) | |
| 034-036 | `5NOPAR` | MSTOP/MSTCL/... | |
| **037** | **`ICSLOAD`** | **LDCS (LOAD-CONTROL-STORE)** | copies CS file name. **caller call site 006114** |
| 040-042 | `5NOPAR` | DEFM/RSTAT/... | |
| **041** | `5NOPAR` | **RSTAT (STATUS)** | **caller call site 127566** |
| 043 | `ISRES` | SPRES (reserve special use) | |
| 044 | `ISREL` | SPREL (release special use) | |
| 045 | `5NOPAR` | DEFSW | |
| 046 | `IDEFSWAP` | DEFSW (Define Swap File) | copies file name |
| 047 | `IDELSWAP` | DELSW (Delete Swap File) | copies file name |
| 050-053 | `5NOPAR` | TESTF/RIFRG/G500P/T500P | |
| **054** | `5NOPAR` | **STSWP (START-SWAPPER)** | **caller call site 010217** |
| 055 | `ISPLACE` | SPLAC (Start-Place) | clears 55REP bit |
| 056 | `IEPLACE` | EPLAC (End-Place) | |
| 057 | `5NOPAR` | MPVER | |
| 060 | `5NOPAR` | LIMEM | |
| 061 | `IMRESSPES` | RESER (reserve mem for test-monitor) | |
| 062 | `IDEFHIST` | HIDEF (Define Histogram) | |
| 063 | `ISTAHIST` | HISTA (Start Histogram) | |
| 064 | `ISTOHIAT` | HISTP (Stop Histogram) | |
| 065 | `IREAHIST` | HISTN (Read Histogram) | |
| 066 | `IRELHIST` | HIREL (Release Histogram) | |
| 067 | `ISPRTE` | SPRTE (read process entry) | copies name |
| 070 | `5NOPAR` | GPRTE | |
| 071 | `ISSGTE` | SSGTE (read phys segment entry) | copies name |
| 072-073 | `5NOPAR` | GSGTE/RPHSG | |
| 074 | `ISPRNM` | SPRNM (Set Process Name) | |
| 075 | `ITSTUSER` | USYST (User SYSTEM test) | |
| 076 | `ITOSWP` | TOSWP (send message to swapper) | |
| 077 | `IRMESS` | RPROC (read last message) | |
| 100 | `RRFLAG` | RFLAG (Read Process Flag) | |
| 101 | `WWFLAG` | SPFLAG (Set Process Flag) | |
| 102 | `IFORGET` | GPSGE (release ND-500 system) | |
| **103** | **`IRSYSP`** | **RSYSP (LIST-SYSTEM-PARAMETERS)** | **caller call site 073132** |
| 104 | `IWSYSP` | WSYSP (Write System Params) | |
| 105-107 | `5NOPAR` | SPRIO/LNKPR/... | |
| 110 | `IWPHSG` | WPHSG (Write Physical Segment) | |
| 111 | `ISTAPRLOG` | SLOG1 (Start Process Log One) | |
| 112 | `ISTOLOG` | STOPLOG | |
| 113 | `IPRILOG` | RLOG (Read Log Info) | |
| 114 | `IRELLOG` | RELLOG (Release Log Facility) | |
| 115 | `ISTLAPR` | SLOGA (Start Log All) | |
| 116 | `5NOPAR` | | |
| 117 | `IPRABORT` | ABORT (Abort Process) | |
| 120-121 | `5NOPAR` | SETOUT/RDSWP | |
| 122 | `ILOGOFF` | LOGOUT (Logout Process) | |
| 123 | `IMRELSPES` | RELMEM (Release Memory) | |
| 124 | `ISTAMLOG` | SMONLOG (Start Moncall Log) | |
| 125 | `IPRIMLOG` | PMONLOG (Print Moncall Log) | |
| 126 | `ISTOMLOG` | XMONLOG (Stop Moncall Log) | |
| 127 | `IDFSYDOM` | DEFDOM (Define Standard Domain) | |
| **130** | `ISFSYDOM` | **START STANDARD DOMAIN** (family 127 define / 130 start / 131 delete) | caller call site 043171; NOT "PLACE-DOMAIN" - the `PLADOM` label sits here but the keyword is unconfirmed (see cross-analysis) |
| 131 | `IDLSYDOM` | Delete Standard Domain | |
| **132** | `5NOPAR` | **LSTDOM (LIST-STANDARD-DOMAINS)** | **caller call site 007352** |
| **133** | `ILI5EXQ` | **LSTEXQ (LIST-EXECUTION-QUEUE)** | **caller call site 111445** |
| 134 | `IPLDEB` | (place debug) | |
| 135 | `IABLOG` | | |
| 136 | `IPRACTIVE` | (list active processes) | |
| 137 | `0` | (null slot) | |
| 140-142 | `5NOPAR` | | |
| 143 | `IMO5RT` | | |
| 144 | `ICHACPU` | CHANGE-CPU | |
| **145** | `ISSTDOM` | | **matches the L07 dispatcher `SAT 145B` boundary compare** |
| 146-147 | `ILLFUNC` | illegal | |
| 150 | `ILI5TQU` | LIST-TIME-QUEUE | |
| 151-153 | `ILLFUNC` | illegal | |
| 154-156 | `5NOPAR` | | |
| 157 | `IWCNT` | WRICS (same handler as 024) | |
| 160 | `IN5SEGLOAD` | ND-500 segment load | |
| 161 | `INDFSYDOM` | | |
| 162-172 | `5NOPAR` | | |
| 173 | `ICPUSTAT` | CPU status | |
| 174-176 | `ILLFUNC` | illegal | |
| 177 | `5NOPAR` | | |

## Cross-verification (why this table is trustworthy)

1. **L07 bytes (VERIFIED):** the `050-S3I5PIT` dispatcher at `030416B` range-checks to `177B` (=128
   entries) and has a boundary compare at `145B` - exactly where `5IFUNC` switches from valid
   (`ISSTDOM` @145) to `ILLFUNC` (@146-147).
2. **Caller side (byte-proven by the nd-500-mon:prog decode):** `37B->ICSLOAD/LDCS`,
   `103B->IRSYSP/RSYSP`, `133B->ILI5EXQ/LSTEXQ`, `7B->IPLSWAPPER/SWLOD` all match this table.
3. **Manual (documented):** the 7-category user-facing names (`Developer/MON/calls/60B_N500M_Functions.md`)
   line up slot-for-slot.

## L07 byte-location of the table (finding 2026-07-15)

`5IFUNC` is declared **`*2BANK`** in the NPL (line 1295: `A:=5IFUNC(X); *2BANK`) - it lives in the
**data bank**, not the `050-S3I5PIT` code image. A 128-word window in `050-S3I5PIT` at `034535B`
(dominant value `035704B` x90) is a dispatch-like table but only matches the NPL `5IFUNC` 5NOPAR
pattern 75/128 - so it is a *different* table (or bank-1 shadow), NOT `5IFUNC`. **To byte-confirm
individual handler bodies, locate the bank-2 5IFUNC table first** (the dispatcher's table-base
operand resolves into a bank-2 address ~`115542B`). Until then, per-handler bodies are documented
from the authoritative NPL source with L07 byte-location marked pending.

## Version caveat + next step

`5P-P2-MON60.NPL` is NPL source (possibly a slightly different revision than L07). Names and structure
are authoritative; each **handler body** (e.g. `ICSLOAD` @NPL `031703B`, `ISEGLOAD` @NPL `031625B`)
should be byte-confirmed in `segments/050-S3I5PIT.bin` before its per-subfunction folder
(`60B-NN-Name/`) is finalized with pseudo-C. Priority order (from the CPU team): `037B ICSLOAD`,
`007B IPLSWAPPER`, `054B` (START-SWAPPER), `041B` (STATUS), then the rest.
