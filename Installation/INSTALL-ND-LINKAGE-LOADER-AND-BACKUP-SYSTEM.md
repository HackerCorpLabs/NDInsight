# Installing the ND-500 Linkage-Loader (NLL) and the Backup System

**Products**:
- ND-500 Linkage-Loader H02, floppy `210319H02-XX-01D`
- Backup System I04 (program version I02), floppy `210337I04-XX-01D` - HARD PREREQUISITE for the NLL install

**Source**: verified live installation session on SINTRAN with ND-500/5000 MONITOR Version J04 (88.6.16 / 88.8.17), run in the RetroCore ND-100 emulator, session date 2026-07-19 (emulated system clock 19-JUL-98). Everything below is observed behavior from that session log, not manual text.

**Related**: [ND-500-MON Setup and Operations Guide](../Reference-Manuals/500/ND-500-MON-SETUP-AND-OPERATIONS-GUIDE.md)

---

## 1. Symptoms Before Installation (Why You Are Here)

On a system without NLL installed:

```
N500: ND-500-LINKAGE-LOADER
TOO LONG PARAMETER

@ND-500-LINKAGE-LOADER
"ND-500-LINKAGE-LOADER"
TOO LONG PARAMETER

N500: LINKAGE-LOADER
DESCRIPTION FILE ERROR: DESCRIPTION-FILE
NO SUCH FILE NAME
```

Interpretation of the observed errors:

- `TOO LONG PARAMETER`: with no LINKAGE-LOADER command/standard-domain present, the name is treated as a domain name, and domain names are limited to 16 characters - "ND-500-LINKAGE-LOADER" is 21. The error is therefore about the NAME LENGTH, not about the product missing, which makes it a misleading first symptom.
- `LINKAGE-LOADER` is the correct short form inside the monitor, and its error is the honest one: there is no DESCRIPTION-FILE:DESC yet, i.e. NLL has never been installed/used for this user.

## 2. Prerequisites (Do These FIRST)

The NLL installer will fail or abort without these. All were discovered the hard way in the session:

1. **User `DOMAIN-USER` must exist with disk space.** The installer's module 1 defaults to placing the NLL domain on user DOMAIN-USER, but does NOT create it:

   ```
   @CREATE-USER DOMAIN-USER
   @GIVE-USER-SPACE DOMAIN-USER, 1000    (1000 pages used in the session)
   ```

2. **User `UTILITY` must exist with at least 177 free pages** (the Backup System installer states the 177-page requirement explicitly; it found only 2 and terminated):

   ```
   @GIVE-USER-SPACE UTILITY, 500
   ```

3. **The Backup System must be installed BEFORE the Linkage-Loader.** NLL's module 3 environment check fails hard otherwise:

   ```
   :::  ERROR  :::
   The subsystem BACKUP-SYSTEM is not found.
   :::  WARNING  :::
   The environment is NOT OK. You cannot COPY before the environment is ok!
   ```

4. **Mount the floppies.** In the session (RetroCore emulator: Ctrl-E to the debugger, `attach fd0/fd1 <image>`, `cont`), then in SINTRAN:

   ```
   @ENTER-DIRECTORY
   DIRECTORY NAME: (empty - taken from the floppy label)
   DEVICE NAME: FLOPPY-DISC-1
   DEVICE UNIT: 0            (or 1 for the second drive)
   ```

   Verify with `@LIST-DIRECTORIES-ENTERED` (the session used the DIR abbreviation) - the floppy shows up as e.g. `DIR INDEX 40 : FLOPPY-DISC-1 UNIT 0 : 210319H02-XX-01D`.

## 3. Installing the Backup System (210337I04-XX-01D)

Floppy contents:

```
(210337I04-XX-01D:FLOPPY-USER)INST-BASY-I04:PROG      <- the installer
(210337I04-XX-01D:FLOPPY-USER)RESERVE-SYSTEM:MODE
(210337I04-XX-01D:FLOPPY-USER)RESERVE-SYSTEM:BATC
(210337I04-XX-01D:FLOPPY-USER)BACKUP-SERV-I02:PROG
(210337I04-XX-01D:FLOPPY-USER)BACKUP-SYS-I02:PROG
(210337I04-XX-01D:FLOPPY-USER)UE-ERMSG-EN-C05:ERR
```

Run:

```
@(210337I04-XX-01D:FLOPPY-USER)INST-BASY-I04:PROG
```

Question-and-answer flow (observed):

1. `Delete previous version ... (Y/N)` - Y/N answer.
2. `Please specify which user you want the files ... copied to (default: UTILITY)` - **this prompt wants a USER NAME. Press plain CR to accept UTILITY.** See gotcha G6 below - answering `y` here creates a demand for a user literally named `y` and the installer terminates.
3. `Should the BACKUP-SYSTEM be dumped reentrant (Y/N) ?` - answered Y. The installer itself warns: **"The message: NO SUCH FILE NAME is ok!"** - the subsequent `@DELETE-REENTRANT BACKUP` prints `NO SUCH NAME` on a first-time install and that is EXPECTED, not a failure.

The installer then copies the files (RESERVE-SYSTEM to SYSTEM, BACKUP-SERV/BACKUP-SYS to UTILITY, error file to SYSTEM), does `@DUMP-PROGRAM-REENTRANT BACKUP-SYSTEM-I (UTILITY)BACKUP-SYS-I`, and runs a MODE job (`BACKUP-LOAD-I:MODE`) that loads the DMA-Server onto a segment called DMASEG (output logged to `BACKUP-LOAD-I:OUT`).

**Post-install steps the installer tells you NOT to forget** (they do not survive a cold start otherwise):

```
Append to the DUMP-REENTRANT file on user SYSTEM:
  @DUMP-PROGRAM-REENTRANT BACKUP-SYSTEM-I, (UTILITY)BACKUP-SYS-I02:PROG

Append (SYSTEM)BACKUP-LOAD-I:MODE to the HENT-MODE file on user SYSTEM
```

It also resets termination handling to blanks and tells you how to restore it (`@DEFINE-TERMINATION-HANDLING`, B, NO TERMINATION DEFINED!).

**Verify**:

```
@LIST-REENTRANT
   START RESTART SEGMENT   NAME
      0B      1B    130B   BACKUP-SYSTEM-I

@BACKUP-SYSTEM
BACKUP-SYSTEM / I02  870827
Ba-sy:
```

## 4. Installing the Linkage-Loader (210319H02-XX-01D)

Floppy contents:

```
(210319H02-XX-01D:FLOPPY-USER)IN-NLL-XX-H02:PROG      <- the installer
(210319H02-XX-01D:FLOPPY-USER)IN-NLL-XX-H02:XCOM      <- installer data
(210319H02-XX-01D:FLOPPY-USER)IN-NLL-XX-H02:INIT
(210319H02-XX-01D:FLOPPY-USER)DESCRIPTION-FILE:DESC
(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02:PSEG   <- the NLL domain itself
(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02:DSEG
(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02:LINK
(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02:UTIL
```

Run (abbreviated spec `@(21:fl)IN-NLL:PROG` worked while only one floppy directory was entered; use the full spec to be safe):

```
@(210319H02-XX-01D:FLOPPY-USER)IN-NLL-XX-H02:PROG
```

The installer copies itself and its :XCOM data to SYSTEM (deleted again on finish/abort), creates `IN-NLL-XX-H02:INST` on SYSTEM (your answers, reused as defaults next run), creates a log `IN-NLL-XX-H02:LOGG` on SYSTEM (delete manually after success), disables Escape, and enables USER-BREAK / FATAL-ERROR termination handling ("messages which are not error messages").

It presents a **5-module main menu that MUST be executed in order 1 to 5**:

```
1  Get start information       (which users get the domain / utility files:
                                defaults DOMAIN-USER and UTILITY)
2  Delete product files        (previous/current versions; strict YES/NO answers)
3  Check environment and resources
                               (SINTRAN environment, BACKUP-SYSTEM present,
                                free pages + object entries on SYSTEM,
                                DOMAIN-USER, UTILITY)
4  Copy product files          (only allowed after module 3 says environment OK)
5  Exit program
```

Module 4 (observed):

- Copies `LINKAGE-LOAD-H02:UTIL` to user UTILITY
- Copies the domain `LINKAGE-LOAD-H02` to user DOMAIN-USER
- Defines the domain as standard domain `LINKAGE-LOADER`
- Then prompts to remove the floppy

**Post-install step the installer tells you to do** (standard domains do NOT survive a cold start):

```
Append to the ND500-HENT file on user SYSTEM:

@ND-500-MONITOR
DEFINE-STANDARD-DOMAIN LINKAGE-LOADER (DOMAIN-USER)LINKAGE-LOAD-H02
```

On module 5 exit it asks whether to copy `IN-NLL-XX-H02:INST` back to the floppy (default N).

After this, `N500: LINKAGE-LOADER` (and per the manual `@ND-500-LINKAGE-LOADER`) resolves to the standard domain.

## 4a. Manual Installation Straight From the Floppy (Recovery Path)

Use this when the installer's module 4 claimed success but `@LIST-FILES (DOMAIN-USER)` is empty (gotcha G12). The trick: the floppy itself is a complete, runnable NLL installation under FLOPPY-USER (description file + :PSEG/:DSEG/:LINK), so NLL can be started directly from the floppy and then told to copy ITSELF to the local disk with its own COPY-DOMAIN command.

Status: manual-derived (commands verified against ND-60.136.04A sections 6.1.8, 8.1.1, 8.1.2); pending live confirmation.

Step by step, logged in as the user who should OWN the loader (SYSTEM recommended - domains on SYSTEM are found automatically for every user, no standard domain needed):

```
1. Mount the floppy (RetroCore: Ctrl-E, then)
     attach fd0 "C:\Users\ronny\Downloads\ND-disk-00042.img"
     cont

2. Enter the floppy directory (skip if already entered - check with the DIR listing):
   @ENTER-DIRECTORY
   DIRECTORY NAME:               (empty CR - taken from the floppy label)
   DEVICE NAME: FLOPPY-DISC-1
   DEVICE UNIT: 0

3. Sanity check - the domain files must be visible:
   @LIST-FILES (210319H02-XX-01D:FLOPPY-USER)LINKAGE

4. Start NLL directly from the floppy:
   @ND-500
   N500: RECOVER-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02
   (expect the NLL: prompt - NLL is now running from the floppy)

5. Copy the whole domain to the current user
   (double quotes on the destination = create new domain; no user prefix
    allowed on the destination - it always lands on the CURRENT user):
   NLL: COPY-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02,"LINKAGE-LOAD-H02"

6. Verify and leave:
   NLL: LIST-DOMAIN
   NLL: EXIT

7. Test the local copy (no floppy involvement anymore):
   N500: LINKAGE-LOAD-H02
   (expect the NLL: prompt again; NLL: EXIT to leave)

8. Optional speed-up, as user SYSTEM (and append the same lines to the
   ND500-HENT file on SYSTEM so it survives a cold start):
   N500: DEFINE-STANDARD-DOMAIN LINKAGE-LOADER LINKAGE-LOAD-H02
   (prefix the domain name with (<owner>) if the owner is not SYSTEM)

9. Release the floppy:
   @RELEASE-DIRECTORY 210319H02-XX-01D
```

If a step fails:

- `NOT REQUIRED ACCESS TO SEGMENT` at step 4 -> file access on the floppy's segment files; fix with `@SET-FILE-ACCESS` on `(210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02:PSEG` and `:DSEG` (RW for :DSEG), then retry. Documented in ND-60.136.04A chapter 14.
- `NO SUCH (AMBIGUOUS) DOMAIN ON THE SPECIFIED USER` at step 4 -> the domain name in the floppy's description file differs; run `N500: LIST-DOMAIN (210319H02-XX-01D:FLOPPY-USER)` to see the real name and use that.
- `DOMAIN ALREADY EXISTS`-type error at step 5 -> a half-created local domain exists; drop the double quotes to overwrite it, or `DELETE-DOMAIN` it first (from NLL, no domain set).

## 5. Pain Points and Gotchas (All Observed)

**G1 - The pre-install error is misleading.** `@ND-500-LINKAGE-LOADER` fails with `TOO LONG PARAMETER` (name > 16 chars treated as a domain name), which says nothing about the product being missing. The informative probe is `N500: LINKAGE-LOADER` -> `DESCRIPTION FILE ERROR ... NO SUCH FILE NAME`.

**G2 - Missing default user aborts the whole installer.** Module 1's default DOMAIN-USER does not exist on a fresh system; the installer prints `User DOMAIN-USER does not exist!`, re-prompts once, and then **aborts the entire program** (`YOU HAVE ABORTED THE PROGRAM`) rather than letting you retry. Create DOMAIN-USER (with pages) before starting.

**G3 - The abort/exit path itself loops.** After an abort, the `Do you want to copy IN-NLL-XX-H02:INST to floppy number 1?` question came back three times (Escape at the prompt re-aborts into the same question). Answer it with `N` and CR to actually get out.

**G4 - Installer state is not persisted between runs.** After the Backup System detour and a fresh start of the NLL installer, selecting module 3 directly gives `You have to run module 1 before starting this module!` - module 1 must be re-run every session, even if only to CR through the defaults.

**G5 - Backup System page requirement.** UTILITY needs >= 177 free pages; with only 2 the installer terminates immediately with an explicit message. `@GIVE-USER-SPACE UTILITY, 500` fixed it.

**G6 - "default:" prompts are NOT yes/no prompts.** The Backup System question `Please specify which user ... (default: UTILITY):` was answered `y` - the installer took `y` as a literal user name (`Illegal user name` ... `User 'y' don't exist`) and terminated. Rule of thumb from this session: **CR accepts the default; only type text if you mean it as the value.** Conversely, module 2's questions insist on real YES/NO (`This is a YES/NO question!`).

**G7 - Expected "errors" that are not errors.** The Backup System install legitimately prints `NO SUCH FILE NAME` (during DELETE-REENTRANT of a version that was never installed) and warns beforehand that this is OK. The termination-handling ENABLED/DISABLED banners are also informational.

**G8 - Directory-prefix abbreviation quirks.** `@LIST-FILES (21:fl)` worked with one floppy entered; later `(21-fl)` gave `NO SUCH USER NAME IN MAIN DIRECTORY` (hyphen instead of colon) and `(21-:fl)` worked. With BOTH product floppies entered, the second one needed a longer prefix (`(21033:fl)`). Also `@LIST-DIRECTORIES-ENTERED` takes a directory NAME parameter - giving it `(21:fl)` produces `ILLEGAL CHARACTER IN PARAMETER`; parenthesized specs belong to FILE parameters only.

**G9 - Three manual persistence steps, or a cold start loses the install.** Collected from both installers:
1. `@DUMP-PROGRAM-REENTRANT BACKUP-SYSTEM-I, (UTILITY)BACKUP-SYS-I02:PROG` -> append to the DUMP-REENTRANT file on SYSTEM.
2. `(SYSTEM)BACKUP-LOAD-I:MODE` -> append to the HENT-MODE file on SYSTEM.
3. `@ND-500-MONITOR` + `DEFINE-STANDARD-DOMAIN LINKAGE-LOADER (DOMAIN-USER)LINKAGE-LOAD-H02` -> append to the ND500-HENT file on SYSTEM.

**G12 - CONFIRMED: module 4's domain copy can fail SILENTLY.** In this session the installer printed `Copying the domain: LINKAGE-LOAD-H02 to user DOMAIN-USER`, continued to `COPYING FINISHED` and `Defining the domain: LINKAGE-LOADER as a standard-domain` with no error - yet afterwards `@LIST-FILES (DOMAIN-USER)` showed NO files at all (verified 2026-07-19). The "Copying the domain" message is printed BEFORE the work and the installer does not check the result. **Always verify module 4 with `@LIST-FILES (DOMAIN-USER)` before trusting INSTALLATION FINISHED.**

Root cause is NOT determined from the log (no error was emitted). Plausible-but-unverified candidates: (a) a domain copy needs the DESTINATION user's DESCRIPTION-FILE:DESC, which cannot exist before NLL has ever run for that user (chicken-and-egg), and NLL's own COPY-DOMAIN can only target the CURRENT user (manual ND-60.136.04A sections 6.1.1, 6.1.8) while the installer ran as SYSTEM; (b) an access/rights problem on the freshly created DOMAIN-USER. Marked ASSUMPTION until the installer internals are analyzed.

**G12 recovery - run NLL directly from the distribution floppy and let it install itself.** The floppy is a complete NLL installation under FLOPPY-USER (description file + all three segment files). With the floppy entered as a directory, log in as the user who should OWN the loader (DOMAIN-USER per installer convention, or SYSTEM - domains on SYSTEM are found automatically for all users per the RECOVER-DOMAIN search order, section 8.1.2):

```
@ND-500
N500: RECOVER-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02
NLL: COPY-DOMAIN (210319H02-XX-01D:FLOPPY-USER)LINKAGE-LOAD-H02, "LINKAGE-LOAD-H02"
NLL: EXIT
```

Entering NLL creates the current user's description file if missing; COPY-DOMAIN with a quoted destination creates the domain and copies every segment (section 6.1.8). Then, as SYSTEM:

**Access rights (what the manual actually requires)**: NO friend relationship is needed - cross-user domain use is governed by ordinary SINTRAN file access on the segment files. Per the Monitor error `NOT REQUIRED ACCESS TO SEGMENT` (ND-60.136.04A chapter 14): the default access set by NLL's EXIT lets other users EXECUTE a program segment (not modify it), and if the data segment swaps from its original file, other users need RW on the :DSEG - fixed with SINTRAN `@SET-FILE-ACCESS`. If the floppy recovery fails with that error, set access on the floppy's LINKAGE-LOAD-H02:PSEG/:DSEG and retry. Cross-user DELETION of domains/segments is flat-out illegal regardless of access (`NOT DELETE ACCESS`, chapter 13), and the manual documents no way for NLL to CREATE a domain under another user (SET-DOMAIN/COPY-DOMAIN always target the current user) - which is why running the copy AS the owning user is the clean path, and how the installer's cross-user "copy the domain" step remains undocumented magic.

```
N500: DEFINE-STANDARD-DOMAIN LINKAGE-LOADER (<owner>)LINKAGE-LOAD-H02
```

and append that (plus the `@ND-500-MONITOR` line) to ND500-HENT for cold-start survival. The user-prefixed RECOVER-DOMAIN form is per manual section 8.1.1 ("syntax is equal to the file system syntax"); running NLL off the floppy this way is the manual-derived recovery, status: pending live verification.

**G10 - Cleanup is manual.** `(SYSTEM)IN-NLL-XX-H02:LOGG` (the installation log) must be deleted by hand after a successful install; the :PROG/:XCOM copies clean themselves up.

**G11a - The "NO FREE SPACE ON THE SEGMENT FILES" text is a WARNING template, not an error.** Before the reentrant dump, the Backup System installer prints what that message WOULD mean if it appeared. In the session it never fired: DUMP-PROGRAM-REENTRANT completed silently, and LIST-REENTRANT + a working `@BACKUP-SYSTEM` (Ba-sy: prompt) prove the dump succeeded. Do not mistake the explanatory text for a failure. Note also that the DMA-Server MODE job writes its output to `(SYSTEM)BACKUP-LOAD-I:OUT` - any error INSIDE that job is invisible on the terminal, so inspect that file when in doubt. Segment files are an ND-100 reentrant-subsystem mechanism; the NLL install itself never uses them (it copies plain files and defines a standard domain), so segment-file space can never explain a missing LINKAGE-LOADER.

**G11 - Termination handling is left reset.** The Backup System installer resets termination handling to blanks and only prints instructions for restoring it - if the machine had custom `@DEFINE-TERMINATION-HANDLING` settings, they are gone until redone.

## 6. Emulator-Specific Notes (RetroCore Session)

- Floppy images were attached from the debugger: Ctrl-E (stops the CPU, status shows CRASHED - that is the Ctrl-E stop reason, not a real crash), then `attach fd0 "...\ND-disk-00042.img"` (NLL floppy) / `attach fd1 "...\ND-disk-00081.img"` (Backup System floppy), then `cont`.
- The two floppies map to `FLOPPY-DISC-1` unit 0 and unit 1 for `@ENTER-DIRECTORY`.
- The Backup System install ran a MODE job and multiple reentrant dumps; total wall time for the whole session was on the order of minutes (353601 ms execution time shown at one Ctrl-E stop).

---

**Document created**: 2026-07-19
**Source**: live installation session log (RetroCore ND-100, SINTRAN with ND-500/5000 MONITOR J04), 2026-07-19.
