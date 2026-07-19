# TASK-05 results — undocumented/unclear MON calls

Consolidated, source-verified findings for the MON calls in
[../../../ghidra-tasks/TASK-05-undocumented-mon-calls.md](../../../ghidra-tasks/TASK-05-undocumented-mon-calls.md).
Method and wave ordering: [../../../ghidra-tasks/TASK-05-PLAN.md](../../../ghidra-tasks/TASK-05-PLAN.md).
**Mismatches between the request's premises and the findings:**
[TASK-05-mismatches.md](TASK-05-mismatches.md).

Confidence tags: **VERIFIED** (read from labeled handler body), **PARTIAL**
(entry/dispatch verified, some detail outside the excerpt), **UNCERTAIN**
(cannot be determined from source alone). All MON numbers are **OCTAL**.

Source files (under `SINTRAN/NPL-SOURCE/NPL/`): `MP-P2-N500.NPL` (ND-500
level-12 service handlers), `RP-P2-MONCALLS.NPL` (ND-100 handlers),
`MP-P2-2.NPL` (level-14 dispatch), `RP-P2-SEGADM.NPL` (segment/MEXIT).

---

## Wave 1 — ND-500 monitor-call service handlers (source-verified)

Dispatch frame (VERIFIED): `SYMBOL L12MIN=500` (`MP-P2-N500.NPL:1269`),
`L12MAX=523` (`:1271`); GOSW index = `5CMNO-L12MIN` (`:1385`), so slot 0 =
MON 500 … slot 19 = MON 523. Every level-12 handler is entered with
`X` = current ND-500 message (in `5MBBANK`, addressed `N5MESSAGE`) and
`B` = ND-500 CPU datafield.

### MCHANDEL — common level-12 entry / dispatch — VERIFIED
- Entry: `T` = stop-reason (`T=:CSTOPREASON`, `:1286`); message in `X`/`N5MESSAGE`.
- Saves stop reason + MON number into `SMCNO` (`:1302`); special-cases
  `2TUSED`/`2CLOCK`/swapper `377`/CERN `376`/fast-UDMA `333` before dispatch.
- Marks process in-mon-call (`5INMCALL`, `:1372`); if `L12MIN<=A<=L12MAX` →
  `5CMNO-L12MIN GOSW` into the 20-entry table (`:1382`–`:1390`), else `GO NORMMC`.
- Return path `NORMMC` (`:1277`) forwards to the system monitor: sets
  `5FRTBAK=:PROCAD.MFUNC`, `CALL 5RRTWT; GO NXTMSG`. Handlers end via
  `OKMONICO`/`EMONICO` + `XACTRDY` + `GO NXTMSG`.

### DVIO (MON 511) — output prompt then read input — VERIFIED  *(showstopper cleared)*
`SUBR DVIO,NOUTSTR,OSTRS,PT5RST` (`:1688`). DVIO shares the output body with
NOUTSTR (504); DVIO = combined outstring + instring.
- **Entry:** `X=N5MESSAGE`, `B=cpu df`. Message offsets: `TODF` output datafield
  (`:1692`), `DNOBY` output byte count, max 4000B (`:1693`); input phase reads
  `11DMA` = max input bytes (`:1772`), `11MXBRK` = max chars before break (`:1888`).
- **Body:** `CALL 5GTDF; GO NORMMC` if not a terminal (`:1691`); validates
  `DNOBY` (`:1694`); sets up the micro-program read (`3RMED`/`NRBYT`/`OSTRA→N500A`/
  `ABUFA→N100A`, `:1701`–`:1712`); restart `OSTRS` reloads `SMCNO`, and
  `IF A=511` loads `11DMA` and `CALL XNINSTR` to continue into instring
  (`:1770`–`:1774`).
- **Writes back / returns:** `AD=:X.11NOCHRET` = number of returned bytes, and
  `100000=:X.NUMPAR` = write-back mask (`:1900`–`:1901`). (The NINSTR/DVINST 503
  path instead writes `X.NOCHRET` with `NUMPAR=4`, `:1903`–`:1904`.)

```
1770  IF A=511 THEN                               % DVIO
1772     *AAX 11DMA; LDDTX                         % max input bytes, continue
1774     X=:N5MESSAGE; CALL XNINSTR
1900  A:=0=:D; AD=:X.11NOCHRET      % MON DVIO; number of returned bytes
1901  100000=:X.NUMPAR              % monitor-call write-back mask
```

### A5XMSG (MON 512) / B5XMSG (MON 513) — ND-500 XMSG interface — VERIFIED  *(showstoppers cleared)*
`SUBR A5XMSG,B5XMSG` (`:2062`); both labels fall into **one identical body**
(`A5XMSG: B5XMSG:` @`:2076`–`:2077`, **no branch on the MON number**).
- **Entry:** subfunction code read from message field `N5XFU` and masked
  `A/\X5MASK=:D` with `X5MASK=77` (`:2078`–`:2079`, `:2071`); illegal if
  `A-X5MAXF>0` (`:2080`). Parameters per subfunction come from message offsets
  (`5ADP2/5DP2/5ADP3/5DP3/5DP4/5AP2…`, `LBUFA`).
- **Dispatch** `A/\X5MASK GOSW` (`:2095`), subfunctions (octal):

  | Fn | Name | Fn | Name | Fn | Name |
  |----|------|----|------|----|------|
  | 0 | LFDUM dummy | 1 | LFDCT disconnect | 2 | LFGET get buffer |
  | 3 | LFREL release buf | 4 | LFRHD read header | 5 | LFWHD write header |
  | 6 | **LFREA** read msg buf | 7 | **LFWRI** write msg buf | 10 | LFSCM set current msg |
  | 11 | LFMST msg status | 12 | LFOPN open port | 13 | LFCLS close port |
  | 14 | LFSND send | 15 | LFRCV receive next | 16 | LFPST port status |
  | 17 | LFGST general status | 26 | LFM2P magic→port | 27 | LFP2M port→magic |
  | 36 | LFPRV request privs | 37 | LFRTN return msg | 40 | LFRRH recv+read hdr |
  | 45 | LFDMM max task space | 46 | LFALM alloc msgs | 47 | LFFRM free / LFGSM gen-status-mult |
  | 50 | LFLMP list msgs/ports | 51 | LFRRE recv+read msg | 52 | LFCPV check privs |
  | 53 | LFWRT write+return | | | | |

- Per-subfunction input regs are inline, e.g. `LFPRV/LFGET…: A=2nd, D=3rd, X=4th`
  (`:2130`); `LFREA/LFWRI/LFWRT: D=2nd, X=4th, A=LBUFA` (`:2156`);
  `LFSND/LFM2P: AD=2nd, X=3rd` (`:2187`). All converge on `MONXM:` → `*MON 2XMSG`
  (`:2227`–`:2229`). Buffer functions 6/7/53 validate `IF A><0 OR 4000<<D`
  (max 4000B, `:2164`) and DMA via `X5BUF→N500A`/`ABUFA→N100A`/`CNVWADR`.
- **Writes back / returns:** result path `X5RET` (`:2261`) → second GOSW
  `RFDUM…RFGSM` (`:2280`–`:2286`) stores results into message params, applies the
  per-function write-back mask `XMRETMASK(X)` (`:2066`–`:2069`, `:2393`) into
  `NUMPA`, then `MCCO`/`XACT500`/`GO CALLID12`.
- **A-vs-B:** body is identical for 512 and 513 (no `IF MON=512/513`). Only
  in-source hint: comment `use B5XMSG` on buffer functions 6/7/53
  (`:2160`–`:2162`), i.e. **B carries a data buffer (`LBUFA`) in the A slot**;
  A is the non-buffer convention. The concrete A≠B distinction is a **caller-side
  parameter convention — UNCERTAIN from this body**.

### 5MTRANS (MON 515) — ND-500 disk transfer / event / start — VERIFIED (mapping PARTIAL)
`SUBR 5MTRANS` (`:2440`), body `:2441`. Header (`:2427`–`:2437`): 8 functions —
disk transfer (nowait/wait), check-event (nowait/wait), start-process (nowait/wait).
- **Entry:** `X=CMSGA` current message (`:2443`), `B` cpu df → `XC5CPUDF` (`:2444`).
  Message offsets: `5MNWA`→`NWFUNC` (`:2445`); `5MFNC` flag word tested
  `NBIT 5DTRANS` (`:2447`) / `NBIT 5CHEVENT` (`:2534`), `5MFNC/\6` selects nowait
  (`:2503`,`:2526`); `5MLGN`→`LOGPH` logical device (`:2449`); `5MDIS`→`DISID`
  disk function (low 6 bits: read=60/write=61/read-no-clear=66, `:2482`–`:2497`);
  `5MEMA`→`MEMAD` (`:2498`); `5DSEC`→`ABPA2` sector (`:2499`); `5MNOS`→`ABP31`
  sector count (`:2500`); `5MREQ`→`REQID` request id (`:2501`).
- **Body:** requires a hard disk with disk-optimization (`9BBHD…9EEHD`, `:2452`);
  allocates a disk-queue element (`:2479`), else links message into `5MWQU`
  wait-queue and `GO NXTMSG` (`:2462`–`:2476`); builds the request via
  `CHDISCADDR` (`:2497`–`:2517`), verifies memory fixed (`CALL CHFIX`, `:2519`),
  `CALL M5TRANS` to queue (`:2523`). CHEVENT searches `READYQ` by `ADMESS`/`REQID`
  (`:2540`–`:2549`).
- **Writes back / returns:** status via bare literals before `GO FIN`/`GO OUT`:
  `1` received (`:2527`), `4` (`:2556`), `2` no-event-nowait (`:2560`); errors
  `A:=6` no-disk-opti (`:2457`), `A:=7` illegal r/w func (`:2493`) via `XRXX`.
  Wait variants `CALL WN5STATUS` with `5MWAIT` (`:2528`,`:2562`). Exact
  status-code→caller mapping is **PARTIAL** (final MONICO write-back outside excerpt).

### GOSW handlers (short)
- **STAPROC (500) / SWITPROC (502)** — VERIFIED. `SUBR NSTOPROC,STAPROC,SWITPROC`
  (`:1527`), `PRSWITCH=502` (`:1528`). Reads `NPROC` (proc#, magno, `:1537`),
  `MAGNO` (`:1540`); validates `A<=5SWPROC OR A>>MX5PROCS` (`:1538`) and
  magic/reservation `A><D OR X.RTRES=0` (`:1542`); starts target
  (`FR5TMQU`/`XTER500`/`SPITMQ`/`ITO500XQ`, `:1559`–`:1564`); at `:1580`
  `IF A><PRSWITCH` plain start `OKMONICO`+`XACTRDY`, switch(502) falls into
  NSTOPROC to stop caller. Error `EMONICO EILPROC`.
- **NSTOPROC (501)** — VERIFIED. `NSTOPROC:` (`:1587`). `SLOCK`; reads `5MSFL`;
  if `55REP` bit set clear it + `OKMONICO` restart (`:1590`–`:1594`), else
  `STOPPED; CALL WN5STATUS` (`:1597`); `GO NXTMSG`.
- **GERRC (505)** — VERIFIED. Name = **"GET ERROR CODE"** (`:1933`–`:1934`,
  "used after a programmed trap"). Reads trap error register at
  `"N500DF".CNTXPAGE+X.ADRZERO` (`:1943`–`:1944`), reads and clears it (`:1945`),
  stores into message param `M505E` (`:1946`), `NUMPA=1` (`:1947`), then
  `3MONCO`/`MCCO`/`XACTRDY`/`GO NXTMSG`.
- **SWMC (510)** — VERIFIED. Name = **"MONITOR CALL TO THE SWAPPER"**
  (`:2042`–`:2043`), not "switch context". Whole body 3 lines (`:2048`–`:2050`):
  reads `TRAPN` from message, masks low 8 bits and ORs `MSM510<<8`, writes back
  to `TRAPN`, `CALL 5ACTSWAPPER; GO NXTMSG`. (`MSM510`/`5ACTSWAPPER` external —
  values UNCERTAIN, swapper semantics explicit.)

---

## Wave 2 — MON 410-427 (ND-500 segment/process management)

**Verdict: carved-only.** The 410–427 *packaging* handler bodies live in the
ND-500 System Monitor image `030-S3SM5.bin`, reached from the ND-100 via the
`NORMMC` forward path. **They are NOT in the NPL source** — so their message-offset
contracts require ND-500 disassembly of S3SM5 (Phase 2), not a source read.

### Routing (VERIFIED, all in `MP-P2-N500.NPL`)
`MCHANDEL` (`:1286`) reads the ND-500 MON number from message offset `MCNO`
(`:1300`–`:1302`). 410–427 are neither `347` (nucleus) nor in the `500B–523B`
level-12 band (`:1382`), so they fall to `GO NORMMC` (`:1393`). `NORMMC`
(`:1277`–`:1283`) sets `5FRTBAK=:PROCAD.MFUNC` and `CALL 5RRTWT` (`:24`), which
restarts the ND-100 shadow-RT program that runs the ND-500 System Monitor domain.
The reverse leg (S3SM5 → ND-100 file system) is `FSYSINTERFACE`
(`CC-P2-N500.NPL:394`, entry `X` = ND-500 message address, `CALL FSYSENTRY :409`).

### S3SM5 vector offsets (from `030-S3SM5-routine-map.md`, table slot = octal MON)
| MON | Friend name | S3SM5 offset | Native ND-100 back-end (physical work) | Confidence |
|-----|-------------|--------------|-----------------------------------------|------------|
| 410 | fixseg | `0xBAE1` | `MOFIX` (`RP-P2-SEGADM.NPL:248,251`) | location VERIFIED |
| 411 | unfix | `0xBB38` | `MUNFIX` (`RP-P2-SEGADM.NPL:248,297`) | location VERIFIED |
| 416 | wsegn | `0xBD70` | `WSEG` (`RP-P2-SEGADM.NPL:985,990`) | location VERIFIED |
| 417 | mxpisg (MaxPagesInMemory) | `0xBDF6` | — | PARTIAL |
| 420 | (GetUserRegisters) | `0xBE0F` | — | PARTIAL |
| 421 | (GetActiveSegment) | `0xBFCF` | — | PARTIAL |
| 425 | sprname | `0x0000` (empty) | — | **NOT LOCATED** |
| 426 | gprnum | `0x0000` (empty) | — | **NOT LOCATED** |
| 427 | gprname | `0x0000` (empty) | — | **NOT LOCATED** |

Native back-end contracts recoverable from NPL (the physical step the S3SM5
handlers drive; distinct ND-100 MON numbers — FIX/UNFIX = MON 116, WSEG = MON 164):
- **`MOFIX`** (`RP-P2-SEGADM.NPL:248`): entry `D0` = segment number; enforces
  `FIXPAGES>FIXMAX` ceiling (`:260`); sets `5FIX` protect bit on each resident page
  (`:276`,`:282`–`:287`); unlinks segment (`:289`).
- **`MUNFIX`** (`:297`): `D0` = seg no; clears `5FIX`/`5FIXC` (`:303`–`:304`);
  `UREMSG` removes from PITs (`:312`); relinks (`:330`).
- **`WSEG`** (`:985`): `D0=:WSSG` seg no (`:992`); reserves swap resources, writes
  modified pages back.

**Open item:** 425/426/427 (sprname/gprnum/gprname) servicing point is unconfirmed
(empty S3SM5 slots, absent from NPL). Deep S3SM5 disassembly (Phase 2) is required
to extract the 410–421 message-offset contracts and to search for the 425–427 code.

---

## ND-100 dispatch + handlers (source-verified)

### GOTAB / ENT14 — level-14 MON dispatch — VERIFIED (byte-confirmed 2026-07-10)
`ENT14` (`MP-P2-2.NPL:366`) is the level-14 internal-interrupt entry; reads IIC
(`:369`); MON number = low byte of `T` (`X:=377; T/\X; T=:14MONNO`, `:376`);
dispatch `X:=GOTAB(T); *2BANK; JMP ,X` (`:387`) — a **direct-indexed jump table**
keyed by the MON number itself (0..377 octal), each entry a handler word-address.

**BYTE-VERIFIED (live L + static):** the real `GOTAB` is in
`resident/SINTRAN-DATA_commoncode.bin` at virtual **`071233B`**, indexed directly as
`071233B + MON#`. Read offline; 6/6 entries match a live-DAP read of a booted L
system. Structure: odd MON `1B..161B` -> a uniform entry-stub block `120303B..122506B`
(each slot `025B` words) backed by the **S3RPIT** overlay (`025-S3IRPIT.bin`, load
`32000B`); higher ranges `163B-173B / 230B-270B / 304B-333B / 364B-377B` -> other
worker areas. **A `GOTAB[n]=000000` slot is the fall-through path (`MFELL ->
CALLPROC`), NOT "illegal MON"** — proof: `GOTAB[14B]=000000` yet MON 14B (OUTBT) is a
real, heavily-used call. (Correction: the earlier "unassigned -> MFELL = illegal"
reading, and the NPL "GOTAB[15/45/51]=MFELL" claims, are wrong — 15B/45B/51B have
DIRECT entries `120501B/121075B/121147B`. See `TASK-05-mismatches.md` §G.)
NPL samples (different revision): 1=`M1`, 2=`M2` (`:184`), 21–24=`M21..M24` (`:186`),
200=`XMSGY` (`:200`), 310=`M310` (`:209`).

### MEXIT (MON 132) — VERIFIED (switch mechanics PARTIAL)
`SUBR SGMTY,MCALL,MEXIT,…` (`RP-P2-SEGADM.NPL:31`); also SGMTY fn 1 (`:78`).
`CALL GET0`; return PC = caller L (`ZLREG=:ZPREG`), target segment `ZTREG=:MSEGM`,
current active segments `RTREF.DACTSEG` (`:58`). Old-mode (`A<=376 AND D<=376`)
returns current segment-number pair packed in caller T (`D SH 10; A+D; A=:ZTREG`,
`:59`–`:61`); `MMC:` rejects segment 1 and `377` (`ERRIL`, `:63`–`:64`), then
`GO FELLS`. No register change to caller (`:87`).

### IBRSIZ (MON 313) — input-buffer size / chars-before-break — VERIFIED
`SUBR IBRSIZ,T2P06` (`RP-P2-MONCALLS.NPL:2938`). `CALL GET0`; `CALL GZTREG` →
logical device in `A` (`:2942`). For a terminal (`TYPRING BIT 5TERM`) walks the
input buffer counting chars to the next break honoring `BRKMAX`
(`:2944`–`:2971`); TAD → `BISIZ` (`:2979`); internal-buffer devices return `BHOLD`
(`:2983`). Return `RETU` (`:2989`): `A=BHOLD=:ZAREG` (chars held),
`BCOUNT=:ZXREG` (chars before break), skip-return. Error `ZAREG=240` (`:2988`).

### BRPNT / DEBUGGER (MON 45 / 51) — VERIFIED (numeric binding UNCERTAIN)
`SUBR BRPNT,DEBUGGER` (`RP-P2-MONCALLS.NPL:1871`).
- **BRPNT** (`:1965`): `IF BACKGROUND=0 GO RTBPT`; `CALL FINDINDEX` (`:1971`);
  copies caller register block to the debugger register area (`:1985`–`:2033`).
- **DEBUGGER** (`:2043`): `CALL GET0`; `IF ZTREG>>17 GO RETU` (`:2045`);
  subfunction in `A`; `A GOSW FAR GETDSEG, PLAC1, PSTART, READLOC, WRITLOC,
  RELDSEG, RTGDBSEG, CHLGSEG, RRGBLOCK, WRGBLOCK, STRRT, RETU, RETU, DWPERMIT,
  DWPROTECT, PLREENTRANT` (`:2047`–`:2062`) — 16 subfunctions (0 get-data-seg,
  1 place, 2 start, 3 read-loc, 4 write-loc, 5 release, 6 get-data-seg,
  7 check-legal-seg, 8 read-reg-block, 9 write-reg-block, 10 start-RT,
  13 write-permit, 14 write-protect, 15 place-reentrant).
- The 45/51 numeric MON binding is not in these labeled bodies (lives in the
  RPIT/GOTAB dispatch) — **UNCERTAIN from this file**.

### GDEVTY — get device type — VERIFIED
`SUBR GDEVTY` (`RP-P2-MONCALLS.NPL:2603`). `CALL GET0`; logical unit from `ZTREG`
(or caller terminal when `BACKGROUND><0 AND ZTREG=1`, `:2610`–`:2618`); `CALL
LOGPH` (`:2620`). Classifies into a **type code** (`9BTERM`=1 terminal, `9BBAD`
TAD, `9BIBDV`, `9BFLOP`, `9BMT`, `9BRFILE`, else 0; `:2623`–`:2629`) and builds an
**attribute bitmask** (`AIOBT,ACONCT,ATISET,AM144,ANORES,ACLDV,ACOSOP,ANNOP,
ANOTS,AMTAD,AREMC`; `:2631`–`:2649`). Return `UT` (`:2651`): `T=:ZDREG` attribute
mask, type code in `ZTREG` (`:2630`), `ZAREG=0` ok, skip-return; error
`ZAREG=33` no such logical unit (`:2652`).

---

## Name reconciliations (friend's labels vs source)

| MON | Friend's label | Source name | Verdict |
|-----|----------------|-------------|---------|
| 505 | GetTrapReason | **GERRC** "get error code" (after a programmed trap) | Name wrong; returns trap error code (`:1933`) — VERIFIED |
| 510 | switch context | **SWMC** "monitor call to the swapper" | "switch context" wrong; activates swapper (`:2042`,`:2050`) — VERIFIED |
| 512/513 | XMSG / convert-domain | **A5XMSG/B5XMSG**, one shared XMSG mechanism | Both = XMSG; A vs B = caller buffer-passing convention (`:2062`,`:2160`) — VERIFIED (A≠B detail UNCERTAIN) |

---

## Wave 3 — ND-100 file-system MON calls (binary-verified)

The file-system module has no NPL source; all these dispatch `GOTAB[n] → MFELL →
CALLPROC` (`MP-P2-2.NPL:387/342`) and the bodies live in the carved
`segments/006-S3FS.bin` (segment 6, load `026000B`, big-endian) or the resident
common code. Verified by byte-swapping the carve to little-endian and running
`nd100-dis -b 026000` at each symbol address — all decode as clean ND-100 code
with a uniform file-system MON prologue.

| Call | MON | Symbol / addr (octal) | Location | Confidence |
|------|-----|-----------------------|----------|------------|
| RDISK | 5 | `RDISK=102021` | `006-S3FS.bin` | VERIFIED |
| WDISK | 6 | `WDISK=102023` | `006-S3FS.bin` (write-flag entry into RDISK body) | VERIFIED |
| OSIZE | 67 | `OSIZE=044231` | resident `SINTRAN-DATA_commoncode` | VERIFIED |
| SETBT | 74 | `SETBY=103720` | `006-S3FS.bin` | VERIFIED (loc) |
| REABT | 75 | `REABT=104005` | `006-S3FS.bin` | VERIFIED (loc) |
| WFILE | 120 | `WFILE=102132` | `006-S3FS.bin` (write-flag entry into RFILE body) | VERIFIED |
| MAGTP | 144 | `MAGTP=026354` | `006-S3FS.bin` | VERIFIED (loc) |
| FSMTY | 327 | no symbol; func-4 worker `GTYPR=113312` | `006-S3FS.bin` via GOTAB[327]=MFELL | PARTIAL |

- **RDISK/WDISK** share one body split by a read/write skip flag (`BSET ZRO/ONE
  SSK`), 6-word param frame. Scratch file = file `100B`, one block/call, standard
  block size 512 bytes, block 0 first; byte offset = `block# × block-size`.
- **RFILE(117)/WFILE(120)** likewise share one body (15-word param frame, 5 params:
  FileNo, ReturnFlag, Buff, BlockNo, NoOfBytes).
- **144 MAGTP** and **327 FSMTY** full function-code tables extracted (verbatim
  from `Reference-Manuals/ND-860228-2-EN` and cross-checked against the decoded
  range-checks in the handler). MAGTP: device-dependent, bits 0-5, tables for STC
  magtape / floppy / Versatec / SCSI streamer. FSMTY: func 1 write index block
  (only func available on ND-100), func 2 block size, func 3 full file name, func 4
  file/device info (= GTYPR).

## Wave 4 — TSS carryovers + misc (dispatch byte-verified; bodies REAL L where direct)

**UPDATED 2026-07-10 with the byte-verified `GOTAB` (commoncode.bin `071233B+MON#`):**
- **15B** — `GOTAB[15B]=120501B`, a **DIRECT** entry stub in the S3RPIT dispatch block.
  **real SINTRAN L bytes carved** from `025-S3IRPIT.bin` (see `mon-analysis/015B-Undocumented/`).
  High-level purpose still undocumented, but the handler code is recovered.
- **51B** (DMAC breakpoint) — `GOTAB[51B]=121147B`, **DIRECT** entry stub. **REAL L
  bytes carved** from `025-S3IRPIT.bin` (`mon-analysis/051B-DMACBreakpoint/`).
- **42B** — `GOTAB[42B]=000000` = **fall-through** (`MFELL -> CALLPROC`), the even-MON
  path; body in the uncarved resident `CALLPROC` overlay. Documented negative
  (`mon-analysis/042B-Undocumented/`). NOT illegal (`GOTAB[14B]=OUTBT` is also 0).
- 13=CIBUF ClearInBuffer, 14=COBUF ClearOutBuffer, 304=MAPS1B SendSIBASMessage
  (already real L). Correction: the bodies are **not** in `116-S3SERWD.bin` (that
  segment is data); the direct-dispatched ones are in the S3RPIT overlay.

## §2.14 additional internal calls (batches B/C/D — beyond the friend's list)

Verified handler bodies found in NPL source:
- **343 Configuration** `MNCFG` (`RP-P2-CONFG.NPL:220`) — indexed config
  read/write, 6 sub-functions. VERIFIED.
- **331 DiskMirroring** `MSYSU` (`RP-P2-MSYSU.NPL:16`) — 16 sub-functions
  (DIMIR tags/headers/layout/locks). VERIFIED.
- **345 MTAFunction** `MTSTART` (`MP-P2-TERM-DRIV.NPL:2151`) — connect/disconnect a
  terminal line to a datafield. VERIFIED.
- **304 SendSIBASMessage** `MAPSIB` (`RP-P2-MONCALLS.NPL:1745`) — SIBAS IPC via
  RT-common; **corrects a manual OCR error**: Performance is 344 (`MOPERFORMANCE`,
  `MP-P2-PERF-CODE.NPL:685`), NOT 304. VERIFIED.
- **342 ADP/UELAMUFunction** `MNADP` (`RP-P2-MON-ADP.NPL:59`) — 15 LAMU-segment
  functions. VERIFIED.
- **305 GetSIBASMessage** `MAPSIB`/`MSIBB` (`RP-P2-MONCALLS.NPL:1745`/`1822`) —
  SIBAS client + server entries. VERIFIED.
- **45/46/47/51 debug** via `DEBUGGER` 16-subfunction GOSW
  (`RP-P2-MONCALLS.NPL:2043-2062`) + `BRPNT` (`:1965`) — bodies VERIFIED, but the
  exact MON#→handler binding is in the absent monitor-level module (UNCERTAIN).

Not located in this source tree (symbol-only / carved-only): 407 GetStopInfo,
347 NucleusFunction (`5SERVER`), 264/265/266 ND500 file/magtape, 432 SIBFU,
163 AwaitRequest (GOTAB[163]=MONERR = illegal at level 14), 261 SyncTable,
20 WCI, 166 DOLW, 321 UEADM, 320 UELOG, 260 USCNT.

## Answers to the friend's specific questions (TASK-05 §A)

- **MON 5/6 (RDISK/WDISK):** one shared body, R/W skip flag; scratch = file 100B,
  block×block-size offset math. **Answered.**
- **MON 67 (OSIZE):** returns bytes **FREE** in the output buffer (space before the
  program must wait), not bytes used. **Answered.**
- **MON 75 (REABT) for file 0:** no static file-0 special-case found; manual
  restricts to sequential mass-storage files. **UNCERTAIN** (needs live check).
- **MON 120 (WFILE) seek trick:** yes, `NoOfBytes=0` seeks — but to **block
  boundaries only**, and to **any block** (offset = block×size), not only block 0.
  Arbitrary byte offsets need MON 74 (SETBT). **Answered.**
- **MON 144 (MAGTP):** full per-device function-code table dumped. **Answered.**
- **MON 313 (IBRSIZ):** terminal/TAD/internal-buffer devices only (walks the input
  ring); not a file call. **Answered** (Wave 1).
- **MON 327 (FSMTY) func 2:** returns **block size in words**, NOT bytes in the
  command buffer — **contradicts the guess.** func 4 **= GTYPR** (confirmed).
  **Answered.**
- **MON 45 = GTYPR ≡ MON327 func 4:** the ND-500 GTYPR job (return TYPRING) is
  exactly FSMTY func 4 → GTYPR; consistent with the friend's conclusion.
  **Answered.**
- **MON 74 (SETBT) for random-access files:** SETBT (`SETBY=103720` in `006-S3FS.bin`)
  is the byte-pointer positioning primitive — it sets the next byte to read/written
  in an opened mass-storage file, bytes numbered from 0. No source-level restriction
  to sequential-only was found, so it is the mechanism used for random access; the
  exact per-file-type branching was not fully traced statically. **Answered
  (behaviour PARTIAL).**
- **MON 42 (friend's TSS guess):** BYTE-VERIFIED `GOTAB[42B]=000000` — a **fall-through**
  (even-MON path, `MFELL -> CALLPROC`), NOT `MONERR`/illegal (proof: `GOTAB[14B]=0`,
  OUTBT). Body is in the uncarved resident `CALLPROC` overlay (not `116-S3SERWD`, which
  is data). 42B is undocumented in the manual; "TSS carry-over" cannot be confirmed.
  **Answered (documented negative — fall-through byte-proven; body outside carved set).**
- **MON 51 (DMAC) vs MON 45 (DBRK):** 45=DefineBreakpoint, 51=DMACBreakpoint. BOTH now
  **REAL L**: byte-verified DIRECT entries `GOTAB[45B]=121075B` / `GOTAB[51B]=121147B`
  (NOT MFELL) — entry stubs in the S3RPIT `120xxx` dispatch block, carved from
  `025-S3IRPIT.bin` (`mon-analysis/051B-DMACBreakpoint/`). The stubs link to the shared
  S3RPIT debugger workers (`DEBUGGER` 16-subfunction GOSW + `BRPNT`) via `JPL I`.
  **Answered (REAL L — 45B/51B entry stubs carved; the exact worker each stub selects
  is its own JPL-I target within S3RPIT).**
- **MON 511 / 512 / 513 / 515 (the showstoppers) — parameter passing:** VERIFIED
  from **real SINTRAN L bytes** in the `S3MPIT` overlay (not just source): 511 DVIO uses
  message offsets TODF/DNOBY(max 4000B)/11DMA and returns 11NOCHRET with write-back
  mask 100000B; 512/513 share ONE XMSG body dispatching on `N5XFU & X5MASK(77)` with
  the `XMRETMASK` write-back array (byte-exact vs NPL); 515 5MTRANS reads
  5MNWA/5MFNC/5MLGN/5MDIS/5MEMA/5DSEC/5MNOS/5MREQ. Disassembly in
  `mon-analysis/511B-DVIO/`, `512B-XMSGCallA/`, `513B-XMSGCallB/`,
  `515B-MultipleDataTransfer/`. **Answered (VERIFIED, real L).**

## Status

- **Waves 1–4 — DONE.** Full coverage of the friend's TASK-05 list (ND-100 +
  ND-500), plus the §2.14 internal-use expansion (batches A–D).
- **Carved-only bodies (need disassembly, not source):** 410–427 (S3SM5),
  13/14/15/42/304 (`116-S3SERWD.bin` via CALLPROC), 347/`5SERVER` (S3SM5).
- **Remaining consolidation:** finish the missing YAMLs (506/510/511/512/513/515
  written; add 45/46/47/51/343/331/345/342 as capacity allows), and — per user
  request — build the per-call comprehensive docs (assembly + analysis + Mermaid
  flow), template established at `mon-analysis/511B-DVIO.md`.
