# HDD Image Findings — Working Research Notes (TEMPORARY)

> **Status: TEMPORARY WORKING NOTES — not final documentation.** Aggregated from automated
> analysis of a local collection of ND HDD disk images using `ndtool` (the NDFS disk-image
> tool, read-only `-i`/`-t`/`-x`).
> Generated 2026-05-25. Purpose: a queryable knowledge base on MODE files, disk layout,
> BPUN/PROG homes, and RT/reentrant init, to feed the real install guides.
>
> **Provenance:** "[obs]" = directly observed in extracted file contents. "[inf]/UNVERIFIED"
> = inference by the analyzing agent, not yet confirmed against a primary manual or source.
> Cross-check anything load-bearing before promoting it into a published guide.

---

## 1. Image inventory & validity

| Image | Volume | Pages (tot/used) | Class | Notes |
|-------|--------|------------------|-------|-------|
| BIGDISK0-H.IMG | PACK-ONE | 38400 / 5233 | H, bare | minimal pack; only SYSTEM has files; **no MODE files** |
| BDH.IMG | PACK-ONE | 38400 / 6741 | H, bare | SINTRAN:DATA populated; mailbox welcome msg; **no MODE files** |
| BIGDISK0-K.IMG | PACK-ONE | 38400 / 10334 | K, skeleton | subsystem files are 0-byte stubs; no MODE |
| BIGDISK0-K2.IMG | PACK-UNO | 38400 / 10334 | K, skeleton | same as K, different volume name |
| BIGDISK0-SCSI.IMG | PACK-ONE | 38400 / 10793 | K, base | only `XMSG-STARTEX:MODE` (example); XMSG + FILE-MANAGER installed |
| scsi-k.img | PACK-ONE | 64656 / 37477 | K, **production** | full NOTIS/COSMOS office system; SCSI boot; rich MODE files |
| c3-k-bd.img | PACK-ONE | 38400 / 16936 | K, **Compact C3** | dev + process-control; TCP/IP; compilers; `-C3` MODE files |
| c3-k-bd-files.img | PACK-ONE | 38400 / 16974 | K, Compact C3 | MODE files byte-identical to c3-k-bd |
| c3-k-bd-clean.img | PACK-ONE | 38400 / 10691 | K, base | symbol/patch base, **no MODE** (pre-config starting point) |
| c3-k.img | — | — | **INVALID** | not a valid NDFS image |
| BIGDISK0-L.IMG | PACK-ONE | 38400 / 14861 | L, populated | XMSG L03, COSMOS E, NOTIS-WP, compilers; **no LOAD/HENT-MODE** but many COSMOS load modes |
| BIGDISK0-L-TEST.IMG | — | — | **INVALID** | header all zeros |
| WD-L.IMG | — | — | **INVALID** | not parseable as NDFS |
| WD0-L.img | — | — | **INVALID** | shares boot header w/ BIGDISK0-L but body not NDFS-parseable |
| WD0-M.IMG | PACK-ONE | 38400 / 12862 | M, **live pack** | XMSG, ND500-MONITOR, DMAC, config; init via `PATCHES:OUT` |
| BIGDISK0-M.IMG | PACK-TWO | 38400 / 12273 | M, **tool/library pack** | BPUN-FILES languages, PED editor; SYSTEM progs are stubs |
| WD0.img | — | — | **INVALID** | not parseable as NDFS |
| BIGDISK0.IMG / a.IMG / RAND.IMG | — | — | **INVALID** | not valid NDFS images |
| BIGDISK1.IMG | PACK-TWO | 38400 / 9 | not a system disk | only NEW-SYSTEM:PROG + TEST1:SYMB |
| WD.IMG | PACK-ONE | 38400 / 8209 | tooling | MAC/FMAC/NPL assembler disk; no MODE |
| HD0.IMG | PACK-ONE | 38400 / 12862 | ND-100/500 service | only stock XMSG-STARTEX; ND500-MONITOR present |
| 1325.img | PACK-ONE | 36864 / 21709 | OTS/COSMOS app | related to c3 site; no top-level LOAD/HENT-MODE-C3 |
| c3_2024_1/2/5/6/7.img | PACK-ONE/UNO/TWO | 36864 / ~30020 | **Compact C3 production** | one filesystem ("Compact C3, Sys 16505"); full OTS/SCOPS + ND-500 |

> Foreign/non-ND images (Sun/SunOS/BSD/Mac/IDE) were excluded by design.

---

## 2. The cold-start spine — HENT-MODE structure [obs]

The richest examples are `scsi-k.img` (SUPERVISOR), `c3-k-bd.img` and `c3_2024_*`
(`HENT-MODE-C3:MODE`, SYSTEM). The canonical cold-start sequence observed:

```
@INITIAL-COMMAND ENTER-DIRECTORY,,<disk-device> 0      ; e.g. DISC-SCSI-1, DISC-74MB-1
@NEXT-INITIAL-COMMAND CONNECT SYS-OUT-1 105 R
@NEXT-INITIAL-COMMAND CLOSE 105
@NEXT-INITIAL-COMMAND BATCH
@NEXT-INITIAL-COMMAND APPEND-BATCH 1 LOAD-MODE:MODE SYS-OUT-1   ; queue warm-start
@RTENTER
@INITIALIZE-BACKGROUND-PROGRAMS                          ; = the "INIT-BACKGROUND" step
@RT-LOADER
YES
READ-BINARY DMAC 7                                       ; DMAC driver → segment 7
YES
READ-BINARY COS-TADADM 36                                ; COSMOS TAD admin → segment 36 (when COSMOS)
YES
EXIT
@BATCH
@MODE (RT)DEFINE-SEGMENTS:MODE,,,                         ; define RT segment names
@SINTRAN-SERVICE / @DEFINE-RTCOMMON-SIZE 2,,Y Y / @EXIT
@MAIL / @INIT 10 / @RUN-MAIL / @EXIT
@BATCH
... subsystem load-mode calls (reentrant dumps, COSMOS/XMSG/TCP loaders) ...
@APPEND-BATCH 1 LOAD-MODE:MODE SYS-OUT-1                  ; hand off to warm-start
```

**Key facts learned:**
- `INITIAL-COMMAND` / `NEXT-INITIAL-COMMAND` build the boot command chain; the last one
  typically does `APPEND-BATCH 1 LOAD-MODE:MODE` to run LOAD-MODE as a batch job. This
  **confirms the automatic-boot mechanism** for [08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md](../08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md). [obs]
- `ENTER-DIRECTORY,,<device> 0` is where the **boot disk device** is named — varies by
  hardware: `DISC-SCSI-1` (SCSI), `DISC-74MB-1` (74 MB Winchester). [obs]
- RT/background bring-up order is fixed: `RTENTER` → `INITIALIZE-BACKGROUND-PROGRAMS` →
  `RT-LOADER READ-BINARY DMAC 7` → MAIL init. [obs]
- The M-version live pack (`WD0-M`) uses a **different mechanism**: a `(SYSTEM)PATCHES:OUT`
  script does `DEFINE-SEGMENT-FILE` + `RT-LOADER READ-BINARY DMAC 7` + DMAC S3PATCH +
  `READ-BINARY ND500-MONITOR 62`, rather than a HENT-MODE file. [obs]

---

## 3. The warm-start — LOAD-MODE structure [obs]

From `scsi-k.img` and `c3*` (`LOAD-MODE:MODE` / `LOAD-MODE-C3:MODE`):

```
@ENTER SYSTEM,<pwd>,,99
@SINTRAN-SERVICE / @CHANGE-DATAFIELD ... (terminal/printer types) / @EXIT
@SET-TERMINAL-TYPE 36..51 -5067
@DEFINE-MASS-STORAGE-UNIT FLOPPY-DISC-1 0   (and STREAMER-1 / FL-DISC-1)
@BATCH
@MODE (UTILITY)XMSG-START...:MODE,,         ; start XMSG
@START-TADADM                               ; COSMOS TAD admin
@MODE (COSMOS-BASIC)COS-START...:MODE,,      ; start COSMOS
@START-SPOOLING COSMOS-SPOOLING
@MODE (COSMOS-BASIC)COS-DEF-PRI...:MODE,,    ; define spool printers
@MODE (TCP-IP)TCP-START:MODE,,,              ; start TCP/IP (later systems)
@GIVE-SPOOL-PAGES <n>
@SINTRAN-SERVICE / @DEF-HDLC 1362B..1365B Y 4 / @EXIT   ; HDLC links
@SET-AVAILABLE
@OPERATOR === SYSTEM IS AVAILABLE ===
```

**Learned:** warm-start = start subsystems already loaded by cold-start, configure
terminals/printers/HDLC, give spool pages, then `SET-AVAILABLE`. The first command in a
SYSTEM batch job must be `ENTER SYSTEM (<password>)...`. [obs]

---

## 4. How MODE files reference other MODE files [obs]

The chaining idiom is `@MODE (<user>)<file>:MODE,,,` (trailing commas = default params),
and `@CC <file>:MODE` to comment-out. Top-level files orchestrate a tree:

```
HENT-MODE (cold)                          LOAD-MODE (warm)
├── (RT)DEFINE-SEGMENTS:MODE              ├── (UTILITY)XMSG-START*:MODE
├── XON-XOFF-ENAB:MODE / SPEED:MODE       ├── (COSMOS-BASIC)COS-START*:MODE
├── (SYSTEM)BACKUP-LOAD*:MODE             │     └── COS-DEF-PRIN*:MODE (spool printers)
├── (COSMOS-BASIC)COS-HENT*:MODE          └── (TCP-IP)TCP-START:MODE
│     ├── COS-XFTRA*:MODE  (XFTRAD daemon)
│     ├── COS-COSP-VS*:MODE (COSPO spooling)
│     ├── COS-FAU-VS*:MODE  (remote file access)
│     ├── COS-FA-SERV*:MODE (FS server / TADADM)
│     └── COS-FSART*:MODE   (FSART admin)
├── FM-DUMP:MODE             (file manager reentrant)
├── (TCP-IP)TCP-IP-LO*:MODE  (load TCP/IP stack)
└── OTS-HENT-C3:MODE → LOAD-IDB-SEG:MODE  (process-control IDB segment)
```

On the big production disk (`scsi-k.img`), a master dispatcher `LOAD-SUBSYS:MODE` calls ~19
sub-loaders (BACKUP-LOAD, XMSG-LOAD, COSMOS-HENT, ENCOS-LOAD, UE-LOAD, OE-LOAD, SPRINT-LOAD,
DS-LOAD, ID-LOAD, etc.), and `DUMP-REENTRANT:MODE` dumps editor/backup/SNA reentrant code. [obs]

---

## 5. Software subsystems seen, and what each needs [obs]

| Subsystem | Started/loaded by | Binaries (typical home) |
|-----------|-------------------|--------------------------|
| **XMSG** (messaging) | `XMSG-LOAD*:MODE` + `START-XMSG` (SINTRAN-SERVICE) | XMSG-KERNEL/XROUT `:BPUN`, XMSG-COMMAND `:PROG` (UTILITY); versions K and L03 seen |
| **COSMOS** (network/file-transfer/spool) | `COS-HENT*` (cold) + `COS-START*` (warm); needs XMSG+TADADM first | COS-* `:PROG/:BPUN` (UTILITY / COSMOS-BASIC); COS-TADADM `:BPUN` (SYSTEM) |
| **TCP/IP** (COSMOS TCP gateway, ~1992) | `TCP-IP-LO*` (load) + `TCP-START` | TCPP/FTPRT, TELNET/FTP/RSH clients (TCP-IP user) |
| **MAIL** | `@MAIL / @INIT 10 / @RUN-MAIL` in HENT-MODE | — |
| **RT-LOADER** | core; used by every load mode | — |
| **SINTRAN-SERVICE-PROG** | config (RTCOMMON, HDLC, datafields, segment write-protect) | — |
| **Spooling** | `START-SPOOLING`, `GIVE-SPOOL-PAGES` | SSY-SPOOL (ND-SPOOL-AREA on big systems) |
| **BACKUP-SYSTEM** | `BACKUP-LOAD*:MODE` → DMA server to DMASEG | BACKUP-SERV/SYS `:BPUN/:PROG` (UTILITY) |
| **NOTIS** (WP/DS/ID/NCALC/DRAW/IR) | DS-LOAD/ID-LOAD/WP-DUMP modes | NOTIS user |
| **User/Operator Environment** (UE/OE) | UE-LOAD/OE-LOAD modes | USER-ENVIRONMENT / ND-OPERATIONS users |
| **ENCOS** | `ENCOS-LOAD*:MODE` | ENCOS servers (UTILITY) |
| **ND-500** | `RT-LOADER READ-BINARY ND500-MONITOR 62` | ND500-MONITOR `:BPUN` (SYSTEM), N500-SYMBOLS |
| **Compilers/editors** (FORTRAN, PLANC, PED, MAC, FMAC, NPL, QED, BRF, GPM, CC) | `DUMP-REENTRANT:MODE` / `DUMP-PROGRAM-REENTRANT` | `:BPUN` in BPUN-FILES; `:PROG` in SYSTEM |
| **Process control / OTS-SCOPS** (Compact C3) | `OTS-HENT*`/`LOAD-OTS`/`LOAD-IDB-SEG` | PICDISP/TREND/MELD/STATUP/PROBAS (OTS-* and RT users) |

---

## 6. BPUN vs PROG file "home" convention [obs]

Strong, consistent pattern across disks:

| File type | Home user/directory | Examples |
|-----------|---------------------|----------|
| Bootstrap `:BPUN` | **(SYSTEM)** | DMAC-191x, COS-TADADM, ND500-MONITOR |
| Subsystem `:BPUN` (loaded reentrant) | **(UTILITY)** | XMSG-KERNEL/XROUT, COSMOS, ENCOS, TCP servers, BACKUP |
| Language/tool `:BPUN` library | **(BPUN-FILES)** | FMAC, MAC, QED, NPL, DITAP, PED, FTN, ASSEMBLER-500, DEBUGGER |
| Floppy/util `:BPUN` | **(FLOPPY-USER)** | FLOPPY-MON, COP-VERIFY, FILSYS-INV, MCOPY |
| Ready-to-run `:PROG` (system tools/compilers) | **(SYSTEM)** | FMAC-1920C, S3-CONFIGURATION, compilers, PED |
| Feature `:PROG` | feature-specific user | XMSG (UTILITY), COSMOS (COSMOS-BASIC), TCP (TCP-IP), NOTIS (NOTIS), UE (USER-ENVIRONMENT) |

> The standard empty user set on a fresh install (SYSTEM, FLOPPY-USER, UTILITY, BPUN-FILES,
> SCRATCH, RT) is created in `System initialisation.txt` — see
> [03-FILESYSTEM-INIT.md](../03-FILESYSTEM-INIT.md). The "home" convention shows what each
> user directory is *for*. [obs]

---

## 7. Load-reentrant & RT subsystem init patterns [obs]

**A. Driver/monitor load into a fixed segment** (in HENT-MODE / PATCHES:OUT):
```
@RT-LOADER
YES
READ-BINARY DMAC 7              ; binary → segment number
YES
READ-BINARY ND500-MONITOR 62
YES
EXIT
```

**B. Dump a reentrant program** (editors, compilers, backup):
```
@DUMP-REENTRANT      <name> <lower> <upper> (<user>)<bpun-file>
@DUMP-PROGRAM-REENTRANT <name> (<user>)<prog-file> [<segname>]
@LOAD-REENTRANT-SEGMENT (<user>)<2nd-part-file> <segname>   ; for 2-segment progs (e.g. PLANC)
@DEFINE-REENTRANT-PROGRAM <name> <lower> <upper> <segname>  ; alias another reentrant prog
```

**C. Full RT-server load pattern** (COSMOS/TCP/NOTIS daemons):
```
@ABORT <prog> / @SCHEDULE 503
@RT-LOADER
CLEAR-SEGMENT <seg> / NEW-(BACKGROUND-)SEGMENT <seg>,<type>,... / SET-PAGE-TABLE <n>
READ-PROGFILE | READ-BINARY (<user>)<file>,<seg>,...
DECLARE-PROGRAM <prog>
CHANGE-RT-DESCRIPTION <prog> <prio> <seg> ...
WRITE-SEGMENT <seg>
END-LOAD | EXIT
```

**D. RT segment-name definitions** — `(RT)DEFINE-SEGMENTS:MODE` uses RT-LOADER
`DEF-SEG-NAME <name> <octal> [<owner>]` to name system + application segments
(S3IMAGE 2, S3COM 3, S3RTL 4, S3DMAC 7, S3XMSGP/D/R 33–35, S3TAD 36, IDB_0 250, etc.). [obs]

**E. IDB in-memory database segment** (process control):
```
@UNFIX IDB_0
@RT-LOADER / CLEAR-SEGMENT IDB_0 / SET-PAGE-TABLE 2
NEW-SEG IDB_0,2,... / ALLOCATE-AREA IDB_0,177777,0 / END / EXIT
@FIXC IDB_0 600          ; fix segment in memory at page 600
```

> **Not observed** as literal commands in any MODE file: `CHA-BACK-SEG-SI` and a standalone
> `DEFINE-SEGMENT-FILE` *inside HENT/LOAD-MODE* (the latter appears in M's `PATCHES:OUT` and
> in `System initialisation.txt`). Background-segment sizing (`CHA-BACK-SEG-SI`) was only seen
> in the earlier SINTRAN-M archive `Admin/HENT-MODE.txt`, not in these disk images. UNVERIFIED
> whether it is version-specific.

---

## 8. Open questions / to validate before publishing

- [ ] `CHA-BACK-SEG-SI` usage and when it's needed (seen in SINTRAN-M archive HENT-MODE.txt, not in images).
- [ ] Exact semantics of `READ-BINARY <name> <seg>` segment numbering vs `DEF-SEG-NAME`.
- [ ] Difference between `DUMP-REENTRANT`, `DUMP-PROGRAM-REENTRANT`, `LOAD-REENTRANT-SEGMENT`,
      `DEFINE-REENTRANT-PROGRAM` — validate against System Supervisor / RT-Loader manuals.
- [ ] Why bare/skeleton packs (H, K) have 0-byte stub files — fresh-install placeholder? (UNVERIFIED)
- [ ] SINTRAN version of WD.IMG / HD0.IMG / 1325.img (no version banner observed).
- [ ] Confirm the boot device tokens (`DISC-SCSI-1`, `DISC-74MB-1`) against the
      CREATE-DIRECTORY device list in [01-DISK-DEVICES.md](../01-DISK-DEVICES.md).

---

## 9. Source agents
Five parallel agents analyzed image groups H / K / L / M / generic. All used read-only
ndtool ops and removed their temp extraction dirs. Raw per-image detail is summarized above;
re-run a targeted `ndtool -x` on a specific image to recover any file's full contents.
