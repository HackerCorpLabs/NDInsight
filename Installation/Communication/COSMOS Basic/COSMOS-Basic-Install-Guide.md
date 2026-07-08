# COSMOS Basic Module — Installation & Start Guide (rev E04, SINTRAN III/VSX)

**Product:** COSMOS Basic Module, ND number **210374E**, revision **E04**
**Target OS:** SINTRAN III / VSX (verified on VSX/500 version L, system number 341)
**Media:** floppy image `210374E04-XX-01D.img` (directory name on disk: `210374E04-XX-01D`)
**Reference doc:** `ND-895036-2-EN` (that PDF/markdown describes the later rev **G** — the procedure matches, but all `-G` file names become `-E` on this media)

> This guide records an **actual, successful** installation onto user `PACK-ONE:COSMOS-BASIC`
> and the correct start-up sequence. Where something is inferred rather than observed, it is
> marked `NOTE`.

---

## 1. What COSMOS Basic gives you

Four independent network services (you may install all or a subset):

1. **CONNECT-TO** — log in to other machines in the COSMOS network.
2. **FILE-TRANSFER** — transfer files to/from other computers (incl. remote batch).
3. **COSMOS SPOOLING** — remote spooling (print to printers on other systems).
4. **FILE ACCESS** — remote file access, split into two parts that run independently:
   - **File User** part (this system reads/writes files on remote servers),
   - **File Server** part (this system serves its files to remote users).

---

## 2. Prerequisites

From the installer's own prerequisite screen:

| Service | Requires |
|---|---|
| FILE-TRANSFER | XMSG (X-MESSAGE) for SINTRAN III, version K or later |
| CONNECT-TO | SINTRAN version K or later; XMSG version K or later |
| COSMOS SPOOLING | SINTRAN version K or later; XMSG version K or later; **SINTRAN configured with COSMOS SPOOLING** |
| FILE ACCESS — File User | SINTRAN version K or later; XMSG configured; **REMOTE FILE ACCESS OPTION** configured |
| FILE ACCESS — File Server | SINTRAN version K or later; XMSG configured; **TERMINAL ACCESS DEVICES (TADs)** configured |

Resource requirements (from `ND-895036-2-EN`, rev G figures — treat as guidance):

- Permanent mass storage on **SYSTEM**: 120 pages / 1 file (for the installer).
- On the target user area (default `COSMOS-BASIC`): **500 pages / 17 files**.
- Segments (ND-100): min 2, max 7. RT-descriptions: 3. Segment-file space: 300 pages.

**Dependencies:**

- The **M-version** of SINTRAN III/VSX is required for the improved file-transfer performance.
- The **D-version** of User Environment is required for CONNECT-TO with local-domain login.

---

## 3. The VSE vs VSX split (why it matters, and why you can ignore it)

Only **two** of the shipped components exist in both a VSE and a VSX flavour; everything else is
version-neutral:

| Component | VSX file (used on VSX) | VSE file (ignored on VSX) | Difference |
|---|---|---|---|
| COSMOS Spooling loader | `COS-COSP-VSX-E02` | `COS-COSP-VSE-E02` | VSX uses `SET-PAGE-TABLE 1`; VSE uses `SET-PAGE-TABLE 0` |
| File Access — User | `COS-FAU-VSX-E03` (1 segment, seg 22) | `COS-FAU-VSE1/VSE2-E03` (2 segments, seg 22 + 26) | VSX fits in one segment; VSE splits across two |

**The installer detects your SINTRAN version automatically** and copies only the matching flavour.
On a VSX/500 system it copies the `VSX` files and never touches the `VSE` ones — verified in the
install log.

`NOTE` The *why* of the page-table / segment-count difference (VSX extended addressing lets the
File-User code live in a single larger segment) is a reading of the two mode files, not a statement
from ND documentation.

---

## 4. How SINTRAN resolves the "generic" file names

The two orchestrator mode files (`COS-HENT-E04:MODE`, `COS-START-E04:MODE`) refer to the other
files by **abbreviated, version-neutral names** — e.g. `COS-COSP-VS-E:MODE`, not
`COS-COSP-VSX-E02:MODE`. This is deliberate and relies on SINTRAN's file-name matching:

- SINTRAN matches an abbreviated file name **component-by-component across the hyphens**; each
  hyphen-separated group only needs to be a **unique prefix** of the real group.
- So `COS-COSP-VS-E` → `COS`,`COSP`,`VS`,`E` matches `COS`,`COSP`,`VSX`,`E02`
  (`VS` is a prefix of `VSX`, `E` a prefix of `E02`) → resolves to `COS-COSP-VSX-E02`.
- This is unambiguous **only because the installer copied the VSX variant alone.** If both
  `COS-COSP-VSX-*` and `COS-COSP-VSE-*` were present, `-VS-` would be ambiguous and SINTRAN would
  raise *"Ambiguous file name"* (error 057 octal).

Every generic reference resolves uniquely on a correctly installed VSX system:

| Reference in mode file | Resolves to |
|---|---|
| `COS-CONN-TO-E:PROG` | `COS-CONN-TO-E02:PROG` |
| `COS-FILE-TRA-E:PROG` | `COS-FILE-TRA-E02:PROG` |
| `COS-XFTRA-E:MODE` | `COS-XFTRA-E02:MODE` |
| `COS-COSP-VS-E:MODE` | `COS-COSP-VSX-E02:MODE` |
| `COS-FAU-VS-E:MODE` | `COS-FAU-VSX-E03:MODE` |
| `COS-DEF-PRIN-E:MODE` | `COS-DEF-PRIN-E02:MODE` |
| `COS-FA-SERV-E:PROG` / `:MODE` | `COS-FA-SERV-E04:PROG` / `:MODE` |
| `COS-FS-ADMIN-E:PROG` | `COS-FS-ADMIN-E02:PROG` |
| `COS-FSART-E:MODE` | `COS-FSART-E02:MODE` |

Therefore **no hand-editing of file names is required.**

---

## 5. Installation procedure

Perform as user **SYSTEM**.

### 5.1 Run the installer

You can run the installer directly from the floppy (as done here). Insert the diskette, then:

```
@(21:F)COS-B-IN
```

(`(21:F)` = the floppy directory/unit; adjust to your unit. The generic form from the doc is
`@ENTER-DIRECTORY 210374E04 FLOPPY-DISC-1 <unit>` then copy `IN-COS-BAS-E:PROG` out and run it —
either approach works.)

The installer:

1. Prints a banner and does initial checks (enables user-break and fatal-error termination handling).
2. Shows the prerequisite list (see section 2). Hit CR.
3. Asks **which services** to install:
   `1=FILE-TRANSFER  2=CONNECT-TO  3=COSMOS SPOOLING  4=COSMOS FILE-ACCESS  5=ALL`.
   Enter a comma-separated list (e.g. `1,2,3,4`) or `5`. Default is `5`.
4. If FILE-ACCESS is chosen, asks **which parts**: `1=File user  2=File server`. Default `1,2`.
5. Asks the **target user** (`DIRECTORY:USERNAME`). Default `COSMOS-BASIC`.
   `NOTE` If you pick a user other than COSMOS-BASIC, the doc states the installer will
   automatically patch the user name inside the mode files. (Not exercised in this run — default
   was accepted.)
6. Copies all files, then releases the floppy directory and resets termination handling.

### 5.2 Verify the copy

The 17 files landed on `PACK-ONE:COSMOS-BASIC` (verified with `@LIST-FILES` / `li-fi`):

```
COS-HENT-E04:MODE          orchestrator — cold-start load
COS-START-E04:MODE         orchestrator — warm-start activate
COS-FILE-TRA-E02:PROG      File-Transfer program
COS-XFTRA-E02:PROG         File-Transfer RT daemon (XFTRAD)
COS-XFTRA-E02:MODE         File-Transfer loader
COS-CONN-TO-E02:PROG       Connect-To program
COS-COSP-VSX-E02:BPUN      Spooling COSPO segment (VSX)
COS-SPOO-SER-E02:PROG      Spooling service program
COS-COSP-VSX-E02:MODE      Spooling loader (VSX)
COS-DEF-PRIN-E02:MODE      Printer definitions (EDIT THIS)
COS-FAU-VSX-E03:BPUN       File-User segment (VSX)
COS-FAU-VSX-E03:MODE       File-User loader (VSX)
COS-FSART-E02:BPUN         File-Server FSART segment
COS-FSART-E02:MODE         File-Server FSART loader
COS-FA-SERV-E04:PROG       File-Server (FA-SERVER-TAD) program
COS-FA-SERV-E04:MODE       File-Server start mode
COS-FS-ADMIN-E02:PROG      File-Server administrator program
```

### 5.3 Post-copy edits

- If you installed **only some** of the four services, edit `COS-HENT-E04:MODE` and
  `COS-START-E04:MODE` and delete the sections for the parts you skipped.
- **COSMOS Spooling:** edit `COS-DEF-PRIN-E02:MODE` to match your network/printer layout.
  If an older `COS-DEF-PRIN:MODE` already exists you can reuse it, changing only the user name.
- **File-Transfer (optional):** `COS-XFTRA-E02:MODE` sets the remote-batch device
  (`61465/1241` = batch 1) and the timeout (`61641/74`, octal seconds). Defaults are fine.
- Delete the installer program from SYSTEM when done (if you copied it out).

---

## 6. Starting COSMOS Basic

Two phases, one per orchestrator file. Run as **SYSTEM**.

### 6.1 Bring-up prerequisites (must already be running/configured)

- **XMSG (X-MESSAGE)** running — every service depends on it. Normally started by
  `@MODE (UTILITY)XMSG-START:MODE`.
- **COSMOS SPOOLING** configured in SINTRAN (peripheral file `COSMOS-SPOOLING` at `1731B`).
- **REMOTE FILE ACCESS option** configured (File-User part).
- **TADs** configured (File-Server part — the mode files run `START-TADADM` themselves).

### 6.2 Phase 1 — load (cold-start work)

Ask users to log out, then:

```
@SET-UNAVAILABLE LOADING COSMOS BASIC MODULE $PLEASE WAIT...
@MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,
```

`COS-HENT-E04:MODE` performs:

- **CONNECT-TO:** `DELETE-REENTRANT` + `DUMP-PROGRAM-REENTRANT CONNECT-TO` (segment CCT);
  `DEFINE-REENTRANT-PROGRAM LIST-SYSTEMS`.
- **FILE-TRANSFER:** dump `TRANSFER-FILE` (segment CFT); define `TRANSFER`, `REMOTE-BATCH`,
  `COMPRESS`, `COMPRESS-FILE`; then chain `COS-XFTRA-E02:MODE` (loads the XFTRAD RT segment).
- **COSMOS SPOOLING:** chain `COS-COSP-VSX-E02:MODE` (loads COSPO into segment COSPOOL,
  `SET-PAGE-TABLE 1`).
- **FILE ACCESS — User:** chain `COS-FAU-VSX-E03:MODE` (loads File-User BPUN into segment 22,
  write-protects it).
- **FILE ACCESS — Server:** dump `FA-SERVER-TAD` and `FS-ADMINISTRATOR`; chain
  `COS-FSART-E02:MODE` (loads the FSART RT segment).

### 6.3 Phase 2 — activate (warm-start work)

Only after XMSG and TADADM are up:

```
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
```

`COS-START-E04:MODE` performs:

- **FILE-TRANSFER:** `ABORT XFTRAD` then `RT XFTRAD` (start the daemon).
- **COSMOS SPOOLING:** `RTON COSPO`; `START-SPOOLING COSMOS-SPOOLING`; chain
  `COS-DEF-PRIN-E02:MODE` (apply printer defs).
- **FILE ACCESS — Server:** chain `COS-FA-SERV-E04:MODE`, which does
  `SET-AVAILABLE` -> `START-TADADM` -> `ABORT/RT FSART` -> `FS-ADMINISTRATOR / SELECT-FSA /
  START-SERVER` -> `SET-UNAVAILABLE`.

Then make the system available and broadcast:

```
@SET-AVAILABLE
```

> **M-version note.** `COS-FA-SERV-E04:MODE` toggles `SET-AVAILABLE` … `SET-UNAVAILABLE`. That
> toggle is only needed on pre-M SINTRAN. On VSX version L leave it as-is (harmless). On an
> M-version you may delete the `@SET-AVAILABLE` / `@SET-UNAVAILABLE` lines at the start/end of that
> file.

---

## 7. Making it permanent across restarts

- In **HENT-MODE:MODE** (cold start), anywhere after `@INITIALIZE-BACKGROUND-PROGRAMS`:

  ```
  @MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,
  ```

- In **LOAD-MODE:MODE** (warm start), after `@MODE (UTILITY)XMSG-START:MODE` and TADADM:

  ```
  @MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
  ```

Also delete any lines in those files that refer to **previous versions** of COSMOS Basic Module.

---

## 8. Changes in this revision (from `ND-895036-2-EN`)

- **Performance:** FILE TRANSFER is significantly faster on files containing "holes".
- **Security:** repeated wrong-password attempts on a remote file are now reported to the error
  device (SINTRAN Watchdog) every 20th attempt; the user is then denied further access
  ("Remote File Server is not Available").
- **New functions:** File Server supports MON IOPEN (MON 351) and MON FSMTY (MON 327) function 14
  (octal); with User Environment version D, connecting within the same UE local domain also logs
  you into that system as the same UE user ("Automatic UE local domain login").
- **Fixed:** remote file access while XMSG not running (previously corrupted SINTRAN);
  closing a remote spooling file when the remote queue is full (now reports "Remote Spooling Queue
  is Full" and cleans up fully).
- **Known, not fixed:** "No Answer from Remote System" is sometimes shown when the correct message
  is "Attempt to Open Too Many Connections" or "No Access to Remote System".

---

## 9. Quick reference — the two commands to run

```
@MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,      (once, to load — cold-start work)
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,     (to activate — needs XMSG + TADADM up)
@SET-AVAILABLE
```
