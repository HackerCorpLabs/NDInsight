# Installing an ND-500 Program Product on the SINTRAN III VSX/500 L Image

**Worked example**: ND-500 LINKAGE-LOADER, product number 210319, distribution media label
`210319H02-XX-01D`.

**Target image**: `BIGDISK0-L.IMG` (78,643,200 bytes), source of record
`F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG`, SINTRAN III VSX/500 L.

**Scope**: this is a procedure document. Nothing here was executed while writing it - the image
was only READ (ndtool listing modes). Every command is traced to a manual, a repo document, or a
listing shown below. Anything not traceable is marked ASSUMPTION or UNVERIFIED.

**Companion documents in this repo**:

- [INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md](INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md)
  - the live-session walkthrough of the same product, with observed gotchas.
- [ND-500-MON Setup and Operations Guide](../Reference-Manuals/500/ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md)
  - domains, PLACE-DOMAIN, standard domains, swap files.

---

## 1. Where the 210319 media actually is on this machine

**FOUND.** The distribution floppy image is:

    C:\Users\ronny\Downloads\ND-disk-00042.img      (1,310,720 bytes)

Verified by reading its volume label and file list with ndtool (read-only `-t` mode, run
2026-07-28):

```
Volume: 210319H02-XX-01D

USER: FLOPPY-USER
  [0000]  1988-03-03 17:49:34  (FLOPPY-USER)IN-NLL-XX-H02:PROG;1    194484 bytes   66 pages
  [0001]  1988-03-03 17:50:13  (FLOPPY-USER)IN-NLL-XX-H02:XCOM;1     26030 bytes   13 pages
  [0002]  1988-03-03 17:50:24  (FLOPPY-USER)IN-NLL-XX-H02:INIT;1     15514 bytes    8 pages
  [0003]  1988-03-03 17:50:36  (FLOPPY-USER)DESCRIPTION-FILE:DESC;1  22528 bytes    8 pages
  [0004]  1988-03-03 17:50:44  (FLOPPY-USER)LINKAGE-LOAD-H02:PSEG;1 123989 bytes   61 pages
  [0005]  1988-03-03 17:51:30  (FLOPPY-USER)LINKAGE-LOAD-H02:DSEG;1 2184977 bytes  44 pages
  [0006]  1988-03-03 17:52:13  (FLOPPY-USER)LINKAGE-LOAD-H02:LINK;1      0 bytes    0 pages
  [0007]  1988-03-03 17:52:15  (FLOPPY-USER)LINKAGE-LOAD-H02:UTIL;1   2440 bytes    2 pages
```

The volume label IS the product identifier `210319H02-XX-01D`, so this file is unambiguously the
product 210319 revision H02 media.

**NOT FOUND**: the prerequisite Backup System floppy `210337I04-XX-01D`. The live-session document
records it as `ND-disk-00081.img`, but no file of that name exists in
`C:\Users\ronny\Downloads` or anywhere under `F:\ND` as of 2026-07-28. Its absence does not block
the installation on THIS image, because the Backup System is already installed on the pack
(see section 3).

**Not in `F:\ND`**: a recursive search of `F:\ND` for `*210319*` returned nothing. The floppy
images shipped alongside the L image (`F:\ND\SINTRAN-L - 2026\FLOPPY\`) are
`210580B01-XX-01D.img`, `210721C01-XX-01D.img`, `211024E02-XX-01D.IMG`,
`211305B02-XX-01D.img`, `VSXL1.IMG`, `VSXL2.IMG`, `VSXL3.IMG` - none of them is 210319.

**Repo references to product 210319** (documentation, not media):

- `E:\Dev\Ronny\NDInsight\Installation\INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md`
- `E:\Dev\Ronny\NDInsight\Installation\Installation-Description\ND-211049-1-EN.md`
  (lists 210319 LINKAGE-LOADER for ND-500 as a required product)
- `E:\Dev\Ronny\NDInsight\Installation\Installation-Description\ND-210895-2-EN.md`
  (lists `210319H` LINKAGE-LOADER for ND-500)
- `E:\Dev\Ronny\NDInsight\Installation\Installation-Description\ND-211114-1-EN.md`
  (requires Linkage Loader version H or later)
- `E:\Dev\Ronny\NDInsight\Installation\Installation-Description\ND-895067-2-EN.md`
- `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md`

---

## 2. What an ND-500 program product consists of

From `ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md` section 3.0, which cites manual ND-60.136.04A
chapter 2:

- The executable unit is a **DOMAIN**, not a file. "For practical purposes a domain may be
  considered equivalent to a program."
- A domain is made of 1 to 32 **segments**. Each segment is a set of SINTRAN files sharing one
  name:
  - `:PSEG` - the instruction (program) segment. Read-only at run time, read directly from the
    file, never written back, needs no swap-file space.
  - `:DSEG` - the data segment.
  - `:LINK` - used only during loading and by the symbolic debugger, not at run time.
- **A domain is NOT a file.** The segment tables for all domains of one user live in that user's
  `DESCRIPTION-FILE:DESC`. There is no "program file" to run - you name the domain and the
  Monitor resolves it.
- Each user has their own description file, up to 256 domains. Domain names are 1 to 16
  characters, alphanumeric or hyphen.
- Compiler output before loading is an `:NRF` relocatable file. NLL turns `:NRF` into
  `:PSEG` / `:DSEG` / `:LINK` plus a description-file entry.

So for product 210319 the actual product is the four files
`LINKAGE-LOAD-H02:PSEG` / `:DSEG` / `:LINK` / `:UTIL` plus the `DESCRIPTION-FILE:DESC` entry that
names the domain. `:UTIL` is not a domain segment type from the manual list.

UNVERIFIED: what `:UTIL` contains. The live session observed the installer copying
`LINKAGE-LOAD-H02:UTIL` to user UTILITY, and that file IS present on the pack today
(`(UTILITY)LINKAGE-LOAD-H02:UTIL`, 2440 bytes), but I have not determined its purpose.

`IN-NLL-XX-H02:PROG` / `:XCOM` / `:INIT` are the ND-100 installer program and its data, not part
of the delivered ND-500 product.

**Where each file must end up** (from the live-session document, section 4):

| File | Destination |
|------|-------------|
| `LINKAGE-LOAD-H02:PSEG` / `:DSEG` / `:LINK` | user `DOMAIN-USER` (installer default) |
| `DESCRIPTION-FILE:DESC` entry for domain `LINKAGE-LOAD-H02` | user `DOMAIN-USER` |
| `LINKAGE-LOAD-H02:UTIL` | user `UTILITY` |
| Standard-domain name `LINKAGE-LOADER` | ND-500 Monitor standard-domain table (SYSTEM only) |

---

## 3. Current state of BIGDISK0-L.IMG (read 2026-07-28)

Read with `ndtool -u` and `ndtool -t` against
`F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG`:

```
Users: 7
  [000]  SYSTEM             42 files  Reserved: 15000  Used: 11182  Free:  3818
  [001]  FLOPPY-USER         0 files  Reserved:     0  Used:     0  Free:     0
  [002]  UTILITY             3 files  Reserved:   502  Used:   184  Free:   318
  [003]  BPUN-FILES          0 files  Reserved:  1000  Used:     0  Free:  1000
  [004]  SCRATCH            25 files  Reserved:  2500  Used:    30  Free:  2470
  [005]  RT                  0 files  Reserved:     0  Used:     0  Free:     0
  [006]  DOMAIN-USER         0 files  Reserved:  1000  Used:     0  Free:  1000
```

Facts that follow directly from this listing:

1. **The Backup System IS already installed**: `(UTILITY)BACKUP-SERV-I02:PROG`,
   `(UTILITY)BACKUP-SYS-I02:PROG`, `(SYSTEM)BACKUP-LOAD-I:MODE`, `(SYSTEM)BACKUP-LOAD-I:OUT`,
   `(SYSTEM)RESERVE-SYSTEM:MODE` / `:BATC`, `(SYSTEM)UE-ERMSG-EN-C05:ERR`. The hard prerequisite
   is therefore satisfied and the missing 210337 floppy image is not needed for this pack.
2. **Users `DOMAIN-USER` (1000 pages) and `UTILITY` (502 pages) already exist with space.**
   Prerequisites 1 and 2 of the live-session document are already done.
3. **The NLL install is HALF DONE and currently broken.** `(UTILITY)LINKAGE-LOAD-H02:UTIL` is
   present, and the installer left `(SYSTEM)IN-NLL-XX-H02:INST` and
   `(SYSTEM)IN-NLL-XX-H02:LOGG` behind, but `DOMAIN-USER` has **0 files**. This is exactly gotcha
   G12 in the live-session document: module 4's domain copy reported success and copied nothing.
   Section 5 below is therefore the procedure that actually matters for this image.
4. **The ND-500 Monitor is present**: `(SYSTEM)ND-500-MON-J:PROG`, `(SYSTEM)SWAPPER-K:PSEG`,
   `(SYSTEM)SWAPPER-K:DSEG`, `(SYSTEM)CONTROL-STORE:DATA`.
5. **There is NO `HENT-MODE:MODE`, NO `LOAD-MODE:BATC` and NO `ND500-HENT` file on SYSTEM.**
   The whole persistence chain described in section 7 does not exist on this image yet. Do not
   assume an append target exists - create it.

---

## 4. Prerequisites

| Prerequisite | Status on BIGDISK0-L.IMG | Command if missing |
|---|---|---|
| ND-500 Monitor available | present (`ND-500-MON-J:PROG`) | out of scope here |
| ND-500 swapper + control store | present | out of scope here |
| Backup System installed | present | install `210337I04-XX-01D` FIRST |
| User `DOMAIN-USER` with space | present, 1000 pages | `CREATE-USER DOMAIN-USER` then `GIVE-USER-SPACE DOMAIN-USER, 1000` |
| User `UTILITY` with >= 177 free pages | present, 318 free | `GIVE-USER-SPACE UTILITY, 500` |
| Free pages on SYSTEM | 3818 free | `GIVE-USER-SPACE SYSTEM, <n>` |
| Logged in as SYSTEM | required for `DEFINE-STANDARD-DOMAIN` (SYSTEM only, guide section 4.1) | - |
| A swap file defined | UNVERIFIED - I did not determine whether a swap file is defined on this image | `CREATE-FILE` contiguous, then `DEFINE-SWAP-FILE <name>` (guide section 2.5) |

Note on the ND-500 subsystem: the guide states the swapper is normally loaded automatically on
the first ND-500 process. UNVERIFIED whether that works on this image without an explicit
`LOAD-SWAPPER` / `START-SWAPPER`.

Note on the ND-500 monitor entry point (MON 60B): the repo documents MON 60B / N500M as the
ND-100 side of the ND-500 interface. I did not verify that any special initialisation beyond
running `@ND-500` is required, so nothing is claimed here.

**Syntax reminders that apply to every command below** (from the `sintran-install` skill):

- `@` is the SINTRAN PROMPT. Do not type it at the terminal. Inside a `:MODE` or `:BATC` file the
  `@` IS part of each line.
- Double quotes mean "create this". They wrap the ENTIRE specification INCLUDING the `(USER)`
  prefix: `"(DOMAIN-USER)LINKAGE-LOAD-H02"`, never `(DOMAIN-USER)"LINKAGE-LOAD-H02"`.
- Command-name abbreviation is character-PREFIX per name part.

---

## 5. Install procedure

### 5.0 Mount the distribution floppy

In the emulator, attach `C:\Users\ronny\Downloads\ND-disk-00042.img` to a floppy unit, then in
SINTRAN, logged in as SYSTEM:

```
ENTER-DIRECTORY
DIRECTORY NAME:                       (empty CR - taken from the floppy label)
DEVICE NAME: FLOPPY-DISC-1
DEVICE UNIT: 0
```

Verify:

```
LIST-DIRECTORIES-ENTERED
LIST-FILES (210319H02-XX-01D:FLOPPY-USER)LINKAGE
```

Expect `DIR INDEX nn : FLOPPY-DISC-1 UNIT 0 : 210319H02-XX-01D`.

### 5.1 Route A - run the product installer (the ND-intended path)

```
(210319H02-XX-01D:FLOPPY-USER)IN-NLL-XX-H02:PROG
```

The installer presents a 5-module menu that **must be run in order 1 to 5**:

```
1  Get start information       (which users get the domain / utility files:
                               defaults DOMAIN-USER and UTILITY)
2  Delete product files        (strict YES/NO answers)
3  Check environment and resources
4  Copy product files          (only allowed after module 3 says environment OK)
5  Exit program
```

Answering rules observed in the live session: a prompt reading `(default: X)` wants a VALUE, and
plain CR accepts the default - answering `y` there is taken as a literal user name and terminates
the installer. Module 2's questions insist on real `YES` / `NO`.

**Verify module 4 before believing `INSTALLATION FINISHED`:**

```
LIST-FILES (DOMAIN-USER)
```

If that listing is empty, module 4 failed silently. Go to route B.

### 5.2 Route B - install from the floppy by hand (the recovery path, and the one this image needs)

The floppy is itself a complete, runnable NLL installation under FLOPPY-USER: it carries a
`DESCRIPTION-FILE:DESC` plus the `:PSEG` / `:DSEG` / `:LINK` files. So NLL can be started from
the floppy and told to copy itself onto the disk.

Log in as the user who should OWN the loader. Domains on SYSTEM are found automatically for every
user, so SYSTEM avoids needing a standard domain at all; the installer's own convention is
DOMAIN-USER.

```
ND-500
N500: RECOVER-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02
NLL: COPY-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02,"LINKAGE-LOAD-H02"
NLL: LIST-DOMAIN
NLL: EXIT
```

`COPY-DOMAIN` with a quoted destination creates a new domain, and the destination always lands on
the CURRENT user - no user prefix is allowed on it (guide section 3.3, manual section 6.1.8).
Entering NLL creates the current user's `DESCRIPTION-FILE:DESC` if it is missing.

Test the local copy with the floppy no longer involved:

```
N500: LINKAGE-LOAD-H02
NLL: EXIT
```

Then, as SYSTEM, give it the short name:

```
N500: DEFINE-STANDARD-DOMAIN LINKAGE-LOADER (DOMAIN-USER)LINKAGE-LOAD-H02
```

(Drop the `(DOMAIN-USER)` prefix if the owner is SYSTEM.) `DEFINE-STANDARD-DOMAIN` is user SYSTEM
only.

Release the floppy:

```
RELEASE-DIRECTORY 210319H02-XX-01D
```

Status of route B: derived from manual ND-60.136.04A sections 6.1.8, 8.1.1, 8.1.2 as recorded in
`INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md`. **UNVERIFIED end to end on a live machine.**

### 5.3 Route C - place the files with ndtool, offline (viable, with caveats)

`ndtool` can write files straight into the `.img`, which sidesteps the floppy device entirely.

    E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build-win\ndtool.exe

Extract from the floppy, then put into the pack. **The emulator must be STOPPED** while writing.

```
ndtool.exe -x -o C:\<somewhere>\nll C:\Users\ronny\Downloads\ND-disk-00042.img

ndtool.exe --put C:\<somewhere>\nll\LINKAGE-LOAD-H02.PSEG DOMAIN-USER/LINKAGE-LOAD-H02:PSEG "F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG"
ndtool.exe --put C:\<somewhere>\nll\LINKAGE-LOAD-H02.DSEG DOMAIN-USER/LINKAGE-LOAD-H02:DSEG "F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG"
ndtool.exe --put C:\<somewhere>\nll\LINKAGE-LOAD-H02.LINK DOMAIN-USER/LINKAGE-LOAD-H02:LINK "F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG"
ndtool.exe --put C:\<somewhere>\nll\DESCRIPTION-FILE.DESC DOMAIN-USER/DESCRIPTION-FILE:DESC "F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG"

ndtool.exe --chmod "PUBLIC+R" DOMAIN-USER/LINKAGE-LOAD-H02:PSEG "F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG"
```

Repeat `--chmod` for every file copied in.

**Verdict: viable as a file-placement mechanism, but it is NOT a complete install.** Reasons,
each traceable:

- `--put` leaves the new file `PUBLIC=NONE`, which produces `NOT READ ACCESS` for any non-owner.
  Always follow with `--chmod 'PUBLIC+R'` (verified previously on this project).
- ndtool writes sparse files FULL. `LINKAGE-LOAD-H02:DSEG` is 2,184,977 bytes logical but only 44
  pages on the floppy - a previous run of this same copy expanded a 44-page sparse DSEG to 1067
  pages. DOMAIN-USER has 1000 pages reserved, so **the copy can overflow the quota.** Raise it
  first with `ndtool --quotaadd DOMAIN-USER <pages>` or plan for the expansion.
- The `NDFS_PATH` form is `USER/NAME:TYPE` with a slash. The `(USER)NAME:TYPE` paren form MANGLES
  the name - do not use it.
- `ndtool --fsck` on this SMD image reports 2 PRE-EXISTING errors (a cross-link and a
  free-but-referenced block) which SINTRAN tolerates. Do not treat them as damage you caused.
- Copying `DESCRIPTION-FILE:DESC` wholesale brings the FLOPPY user's whole domain table with it.
  For a user with no other domains that is fine. For a user who already has domains it would
  overwrite them.

ASSUMPTION (not verified): that a description file copied byte-for-byte from the floppy is valid
for a different owning user on a different pack. The domain table references segment files by
name, and the names are unchanged, so it is plausible - but I did not verify it, and if it is
wrong the symptom would be a domain that lists correctly and fails to place.

`DEFINE-STANDARD-DOMAIN` cannot be done by ndtool at all - the standard-domain table is a Monitor
run-time table, so that step always happens inside the running system.

---

## 6. Persistence - what does not survive a restart

The standard-domain table "survives a warm start but not a cold start" (guide section 4.1). The
domain files themselves are ordinary SINTRAN files and do survive.

So after every COLD start the standard-domain definition must be re-issued. The ND convention,
per the installer's own closing message:

```
Append to the ND500-HENT file on user SYSTEM:

@ND-500-MONITOR
DEFINE-STANDARD-DOMAIN LINKAGE-LOADER (DOMAIN-USER)LINKAGE-LOAD-H02
```

On BIGDISK0-L.IMG there is no `ND500-HENT` file and no `HENT-MODE:MODE` at all, so this must be
created and then wired into the boot chain. `HENT-MODE:MODE` is the cold-start mode file;
`LOAD-MODE:BATC` is the warm-start batch file
(`Operations\SINTRAN\ND-30.003.007 EN SINTRAN III System Supervisor.md`, appendix H).

UNVERIFIED: whether `ND500-HENT` is meant to be a `:MODE` file invoked from `HENT-MODE:MODE`, or
a separate file with its own invocation. The installer message quotes the name without a type.

Reminder from the skill: inside a `:MODE` file every line carries its own `@`, and a `:BATC` file
starts logged out so it must `@ENTER SYSTEM,...` and end with `@EXIT`.

---

## 7. How ND-500 install differs from ND-100 install

| | ND-100 product | ND-500 product |
|---|---|---|
| Delivered artefact | `:PROG` (runnable file) or `:BPUN` (binary punch) | a DOMAIN: `:PSEG` + `:DSEG` + `:LINK` plus a `DESCRIPTION-FILE:DESC` entry |
| How it is made runnable by name | `DUMP-PROGRAM-REENTRANT` (or prompted `DUMP-REENTRANT` for `:BPUN`, which needs start/restart addresses) into the segment file | `DEFINE-STANDARD-DOMAIN` in the ND-500 Monitor, SYSTEM only |
| Where "installed" state lives | the segment file `SEGFIL0:DATA`, plus RT-LOADER segments | the user's description file (on disk) plus the Monitor's standard-domain table (in memory) |
| Loader used to build it | `RT-LOADER` / `BRF-LINKER` from `:BRF` | `ND-500-LINKAGE-LOADER` from `:NRF` |
| Cold-start rebuild | `HENT-MODE:MODE` re-runs the RT-LOADER loads and reentrant dumps | re-issue `DEFINE-STANDARD-DOMAIN` (the segment FILES persist; only the name table is lost) |
| Namespace | one system-wide reentrant-subsystem name space | per-user domain namespace, up to 256 domains per user |
| Invocation | `@NAME` at the SINTRAN prompt | `@ND-500 NAME`, or `NAME` at the `N500:` prompt |
| Verification | `LIST-REENTRANT` | `LIST-DOMAIN`, `LIST-STANDARD-DOMAINS`, `WRITE-DOMAIN-STATUS` |

The practical consequence: **segment-file space can never explain a missing ND-500 domain.**
Segment files are an ND-100 reentrant mechanism; the NLL install never touches them. It copies
plain files and defines a standard domain.

Note that a real product install mixes both worlds - product 210319 ships an ND-100 installer
program (`IN-NLL-XX-H02:PROG`) that installs an ND-500 domain, and its prerequisite (the Backup
System) is a pure ND-100 product installed with `DUMP-PROGRAM-REENTRANT`.

---

## 8. What could go wrong

All of these are real errors recorded in
`INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md` from a live session, unless marked otherwise.

1. **`TOO LONG PARAMETER` from `@ND-500-LINKAGE-LOADER`.** Misleading. With no standard domain
   defined the name is treated as a domain name, and domain names are limited to 16 characters -
   `ND-500-LINKAGE-LOADER` is 21. It says nothing about the product being missing. The honest
   probe is `N500: LINKAGE-LOADER`, which answers
   `DESCRIPTION FILE ERROR: DESCRIPTION-FILE / NO SUCH FILE NAME` when NLL has never been
   installed for that user.

2. **Module 4 reports success and copies nothing.** CONFIRMED on this pack: the installer printed
   `Copying the domain ...`, `COPYING FINISHED` and
   `Defining the domain: LINKAGE-LOADER as a standard-domain` with no error, yet DOMAIN-USER has
   0 files today. The message is printed BEFORE the work and the result is not checked. Always
   verify with `LIST-FILES (DOMAIN-USER)`. Root cause not determined.

3. **`User DOMAIN-USER does not exist!` aborts the whole installer.** It re-prompts once then
   quits with `YOU HAVE ABORTED THE PROGRAM`. Create the user with pages first.

4. **`The subsystem BACKUP-SYSTEM is not found.` / `The environment is NOT OK.`** Module 3 fails
   hard if the Backup System is not installed. Install `210337I04-XX-01D` first. That media is
   NOT present on this machine, so if the pack ever loses its Backup System this route is blocked
   until the floppy image is found.

5. **`Illegal user name` / `User 'y' don't exist` then termination.** A `(default: X)` prompt was
   answered `y`. Those prompts want a value; CR accepts the default.

6. **`NOT REQUIRED ACCESS TO SEGMENT` on RECOVER-DOMAIN.** File access on the segment files. Fix
   with `SET-FILE-ACCESS` on the `:PSEG` and `:DSEG` (RW needed on `:DSEG` if the data segment
   swaps away from its original file). Documented in ND-60.136.04A chapter 14. The ndtool
   equivalent of this failure is `--put` leaving `PUBLIC=NONE`.

7. **`NO SUCH (AMBIGUOUS) DOMAIN ON THE SPECIFIED USER`.** The domain name inside the floppy's
   description file differs from the file name. Run
   `N500: LIST-DOMAIN (210319H02-XX-01D:FLOPPY-USER)` to see the real name.

8. **Quota overflow when using ndtool.** The 44-page sparse `:DSEG` expands to roughly 1067 pages
   when written back full - more than DOMAIN-USER's 1000 reserved pages.

9. **Directory-prefix abbreviation quirks.** `(21:fl)` worked with one floppy entered; `(21-fl)`
   gave `NO SUCH USER NAME IN MAIN DIRECTORY` (hyphen instead of colon). With two product
   floppies entered, the second needed a longer prefix. Also `LIST-DIRECTORIES-ENTERED` takes a
   directory NAME, so a parenthesized spec gives `ILLEGAL CHARACTER IN PARAMETER`.

10. **Cold start silently loses the loader.** Everything works until the next cold start, then
    `LINKAGE-LOADER` stops resolving because the standard-domain table is empty. The files are
    still there. See section 6.

11. **Leftover installer files.** `(SYSTEM)IN-NLL-XX-H02:LOGG` and `:INST` must be deleted by
    hand. Both are present on the pack right now.

---

## 9. What I could NOT verify

- The Backup System media `210337I04-XX-01D` / `ND-disk-00081.img` is not present on this
  machine.
- Whether a swap file is defined on BIGDISK0-L.IMG, and whether the swapper auto-loads.
- What `LINKAGE-LOAD-H02:UTIL` is for.
- Whether a `DESCRIPTION-FILE:DESC` copied byte-for-byte from the floppy is valid under a
  different user on a different pack (route C).
- Whether `ND500-HENT` is a `:MODE` file and how it is chained from `HENT-MODE:MODE`.
- The root cause of the silent module 4 failure.
- Route B (the floppy recovery path) has never been confirmed on a running machine.
- Nothing in this document was executed. The disk image was opened read-only.

---

**Document created**: 2026-07-28
**Method**: `sintran-install` skill, repo documents cited inline, and read-only `ndtool` listings
of `C:\Users\ronny\Downloads\ND-disk-00042.img` and
`F:\ND\SINTRAN-L - 2026\HDD\BIGDISK0-L.IMG`.
