# ND-500-MON Setup and Operations Guide

**How to set up, run and supervise an ND-500 / ND-5000 using the ND-500 Monitor (ND-500-MON): creating domains, listing domains, listing processes, checking CPU status and priorities.**

**Primary source**: [ND-60.136.04A ND-500 Loader Monitor](../ND-60.136.04A%20ND-500%20Loader%20Monitor.md) (all section numbers below refer to that manual unless stated otherwise).

**Scope note**: Everything in sections 1-9 is taken directly from the official manual. ND-5000-specific notes are collected in section 10 and are based on this repository's own reverse-engineering analysis - they are marked as such.

---

## 1. What the ND-500 Monitor Is

The ND-500 Monitor (the program file is `ND-500-MONITOR`, prompt `N500:`) is the SINTRAN III subsystem that controls the ND-500 CPU. It:

- Places domains (programs) into ND-500 memory and starts them
- Manages processes, segments, swapping and memory shared between ND-100 and ND-500
- Provides debugging, performance measurement and System Supervisor commands

Two related tools share many commands (manual chapter 7):

| Tool | Prompt | Purpose |
|------|--------|---------|
| ND Linkage-Loader (NLL) | `NLL:` | Creates domains and segments (loads NRF code) |
| ND-500 Monitor | `N500:` | Runs domains, supervises the ND-500 |

### 1.1. Entering and leaving the Monitor

```
@ND-500                      start the monitor, get the N500: prompt
ND-500 MONITOR 81.11.14/81.11.04
N500: DOMAIN-NAME            run a domain, return to N500:
N500: EXIT                   leave the monitor, back to @
```

Or run a domain in one line - control returns to SINTRAN when it finishes:

```
@ND-500 DOMAIN-NAME
```

(Manual section 1.1.3.) Pressing Escape during execution stops the program and returns to the `N500:` command processor; no files are closed, and execution can be resumed with `CONTINUE` (sections 8.1.4 and 8.9.3).

---

## 2. System Supervisor Setup (Bring-Up) Sequence

These commands are allowed for user SYSTEM only (manual section 8.10). Most require that no other users are logged in on the ND-500.

A typical bring-up / maintenance sequence:

```
@ND-500
N500: SET-ND-500-UNAVAILABLE                 block new logins while configuring
N500: DEFINE-MEMORY-CONFIGURATION <page#>    define shared/local memory layout
N500: GIVE-ND-500-PAGES <no. of pages>       hand ND-100 pages to the ND-500
N500: LOAD-CONTROL-STORE                     load microcode (CONTROL-STORE:DATA)
N500: COMPARE-CONTROL-STORE                  verify microcode (optional)
N500: DEFINE-SWAP-FILE <file name>           define contiguous swap file(s)
N500: LOAD-SWAPPER                           (SYSTEM)SWAPPER -> process 0
N500: START-SWAPPER
N500: SET-SYSTEM-PARAMETERS ...              tune (or accept defaults)
N500: SET-ND-500-AVAILABLE                   open the machine to users
```

NOTE: Much of this is automatic. When a user starts an ND-500 process after a `STOP-ND-500`, "the microcode will automatically be reloaded, the swapper process placed in memory and started (warm start of ND-500)" (section 8.10.3). The explicit commands are for first-time configuration, tuning, or recovery.

### 2.1. Availability control

| Command | Effect | Section |
|---------|--------|---------|
| `SET-ND-500-UNAVAILABLE` | No new users may log on to the ND-500. Does NOT force out users already logged in. Implicitly attempted by any command needing exclusive access. | 8.10.1 |
| `SET-ND-500-AVAILABLE` | Reverses the above. Implicit when the reserving user leaves the monitor. | 8.10.2 |

### 2.2. Memory configuration

- `DEFINE-MEMORY-CONFIGURATION <ND-100 page# for ND-500 phys.addr 0>` - tells the OS the physical memory layout. Subcommands ask, per memory part: size in pages, ND-100 access?, ND-500 access as program?, ND-500 access as data?, last part? Survives a warm start but NOT a cold start (MACM `)HENT` / `22!`) - the manual recommends keeping the definition in a permanent macro (section 8.10.4.1).
- `MEMORY-CONFIGURATION` - prints the current configuration (section 8.10.4.2).
- Local ND-500 memory is not legal in the ND-500 multiuser Monitor (section 8.10.4).

### 2.3. Memory administration

When the ND-500 is started the first time, every page of shared memory belongs to the ND-100 (section 8.10.5):

- `GIVE-ND-500-PAGES <no. of pages>` - moves pages from ND-100 to ND-500. All system tables live in ND-100 memory, so every page given is available for user processes.
- `TAKE-ND-500-PAGES <no. of pages>` - gives pages back to the ND-100.

### 2.4. Microprogram (control store)

Sections 8.10.6.x. Requires detailed microprogram knowledge (see also ND-30.013 Test Micro Program Descriptions):

- `LOAD-CONTROL-STORE (<file>), (<start addr>), (<no. of words>)` - loads microcode. Defaults: file `CONTROL-STORE:DATA`, address 0, 20000B words (entire control store). Each microword is 144 bits (18 bytes) on the classic ND-500. Verifies after load; failure prints `CONTROL STORE UNSUCCESSFULLY LOADED`.
- `COMPARE-CONTROL-STORE` - compares live control store with the file (4 words are modified after load and will always differ).
- `LOOK-AT-CONTROL-STORE <address>` - inspect/patch microcode (subcommands EDIT, ORIN, OCTAL, SYMBOLIC, GROUP, WORD).
- `MICRO-STOP` / `MICRO-START <address>` - stop/resume the microprogram with all registers retained; running jobs need not be restarted (section 8.10.6.1). Contrast with `STOP-ND-500`, which requires a warm start.

### 2.5. Swap files

Sections 8.10.10.x:

- Create the file first with SINTRAN `@CREATE-FILE` (must be contiguous; user SYSTEM needs at least RW access).
- `DEFINE-SWAP-FILE <file name>` - register it with the Monitor. Several swap files may exist; definitions survive a warm start but not a cold start.
- `DELETE-SWAP-FILE <file name>` - deregister (file itself is not deleted).
- `LIST-SWAP-FILE-INFO <swap file no.|ALL>` - file system statistics + current usage.
- `LOAD-SWAPPER <file name>` - loads the swapper (default `(SYSTEM)SWAPPER`) into ND-500 memory; the swapper always runs as process number 0. Normally automatic on first ND-500 process.
- `START-SWAPPER` - starts the loaded swapper.

### 2.6. System parameters

`SET-SYSTEM-PARAMETERS` (section 8.10.11) sets, among others: max number of physical segments (max 2000B, needs restart), clean/swapout thresholds, default ND-100 priority, default ND-500 priority, max ND-100 CPU percentage, disk cache buffer size/count, low priority factor, max pages fixed. `LIST-SYSTEM-PARAMETERS` prints them.

---

## 3. Domains: What They Are and How to Create Them

### 3.0. What a domain is (manual chapter 2)

**A DOMAIN is an ND-500 addressing space.** It contains an executable program that can be started through the ND-500 Monitor - "for practical purposes a domain may be considered equivalent to a program" (section 2.1). Key properties, all from manual sections 2.1-2.2:

- **Size**: the address range of a domain may vary from 2K bytes up to 4 gigabytes (a full 32-bit address space).
- **Separate instruction and data areas**: a domain contains one area for instructions and another for data. They cover the SAME address range, but instructions may never be read as data, nor data executed as instructions, so no conflict arises.
- **Divided into SEGMENTS**: a domain comprises 1 to 32 segments; the uppermost five address bits select the segment. The instruction part and data part of a segment are called the instruction (program) segment and data segment.
- **A segment is a set of SINTRAN files**: the program segment and data segment share a name but have types `:PSEG` and `:DSEG`. A third file, `:LINK`, is used only during loading and by the symbolic debugger, not at run time. Files are indexed by default (may be contiguous).
- **A domain is NOT a file**: it is a table of segments. The segment tables for all domains of one user live in the file `DESCRIPTION-FILE:DESC`. That is why you never see a "program file" for a domain - you always refer to it by its domain name, and NLL resolves names to internal numbers via the description file (LIST-DOMAIN / LIST-SEGMENT show the numbers when needed).
- **Per-user namespace**: each user has their own description file and up to 256 domains; domain names are 1 to 16 alphanumeric characters or hyphen and follow file-name syntax.

**Why split a domain into several segments** (section 2.1 lists these motivations):

- Time-critical parts can be fixed permanently in memory while the rest is demand-paged.
- A segment can be part of several domains (e.g. the Fortran library) - one copy on disk instead of one per domain.
- At run time the Monitor keeps ONE in-memory copy of a program segment used by several users concurrently, reducing swapping.
- Different segments can have different protection.
- Two concurrent programs can communicate through a shared data segment.
- Modularization: modifying one segment does not force a reload of the whole domain.
- Program segments need no swap-file space - they are read directly from the `:PSEG` file and never written back.

**Linking**: a segment is always declared in exactly one domain. Other domains (even of other users, e.g. a SYSTEM library segment) use it via the NLL command `LINK-SEGMENT`. Linking is only possible if the segment has no external references to unlinked segments of its home domain.

**Indirect segments and monitor calls** (sections 2.1-2.2): ND-500 hardware lets a segment be "indirect" - calling into it transfers control to ANOTHER domain. SINTRAN uses this for monitor calls: system routines sit on an indirect segment (by convention segment number 37B / decimal 31), so a monitor call looks exactly like an ordinary routine call; the microprogram recognizes the indirect capability and transfers control - the ND-100 itself is treated as "another domain in another machine".

**Capabilities** (section 2.2): at run time the ND-500 keeps a 16-bit descriptor (capability) per logical segment in use: access rights (write/parameter protection for data segments), direct/indirect flag for program segments, sharing information, and the physical segment number the logical segment maps onto. Two logical segments mapping the same physical segment is how sharing works; shared-segment accesses bypass the cache to keep data consistent, and a data segment mapped directly onto its file makes modifications permanent (a very low-overhead way to address a whole file as memory).

### 3.0.1. The domain lifecycle in practice

1. **Compile** with an ND-500 compiler - output is an `:NRF` relocatable file (section 1.1.1).
2. **Load** with NLL: name the domain (`SET-DOMAIN "NAME"`), load code (`LOAD-SEGMENT`), `EXIT`. NLL builds the `:PSEG`/`:DSEG`/`:LINK` files and records the domain in `DESCRIPTION-FILE:DESC` (section 1.1.2).
3. **Execute** via the Monitor: `@ND-500 NAME`, or `NAME` at the `N500:` prompt (section 1.1.3). PLACE-DOMAIN maps each logical segment onto a physical segment and sets up the start address and trap registers (section 8.1.1).

### 3.0.2. Installing the Linkage-Loader

The ND Linkage-Loader is not part of the base SINTRAN III system - it is delivered and installed from its own distribution media:

- **ND-500 Linkage-Loader**: installed from product **210319H02-XX-01D**
- **Prerequisite**: the Linkage-Loader requires the **Backup System**, which is installed from product **210337I04-XX-01D** - install the Backup System first.

(Source: installation media product numbers provided from the site's ND distribution set, 2026-07-19; not from ND-60.136.04A.)

**Full walkthrough with the observed pitfalls** (misleading `TOO LONG PARAMETER` before install, required users DOMAIN-USER/UTILITY, the 5-module installer, the persistence steps for cold-start survival): [Installing the ND-500 Linkage-Loader and Backup System](../../Installation/INSTALL-ND-LINKAGE-LOADER-AND-BACKUP-SYSTEM.md).

Enter the loader with `@ND-500-LINKAGE-LOADER` (or `N500: LINKAGE-LOADER`).

### 3.1. Simple single-segment domain

```
NLL: SET-DOMAIN "MY-DOMAIN"        double quotes = create NEW domain
NLL: LOAD-SEGMENT TESTPROG         load compiler output (:NRF file)
NLL: EXIT                          link libraries, update files, return
```

Without quotes, an EXISTING domain is set (and overwritten by subsequent loads). Domain names: 1 to 16 alphanumeric characters or hyphen. Default is SCRATCH-DOMAIN, which is overwritten on every unnamed load (section 6.1.1).

### 3.2. Multi-segment domain (shared subroutine segment)

```
NLL: SET-DOMAIN "TWO-SEGMENTS"
NLL: OPEN-SEGMENT "SUBROUTINES" P     P attribute = usable by other domains
NLL: LOAD-SEGMENT SUBR-FILE
NLL: CLOSE-SEGMENT
NLL: SET-SEGMENT-NUMBER 2             avoid clashing with segment 1
NLL: LOAD-SEGMENT MAINPROG
NLL: LINK-SEGMENT SUBROUTINES
NLL: EXIT
```

A second domain reusing the same segment loads only its main program and repeats `LINK-SEGMENT SUBROUTINES` (section 1.1.4).

### 3.3. Domain management commands (NLL, chapter 6.1)

| Command | Effect |
|---------|--------|
| `SET-DOMAIN <name>` | Set current domain (quotes = create new). Implicit END-DOMAIN of the previous one. |
| `END-DOMAIN` | Finish work on current domain (auto CLOSE-SEGMENT). Implicit in SET-DOMAIN and EXIT. |
| `CLEAR-DOMAIN <name>` | Remove all segments from the domain; domain remains, :PSEG/:DSEG/:LINK files retained. Not while a domain is set. |
| `DELETE-DOMAIN <name>` | Delete segments AND the domain itself (files retained). Cannot delete SCRATCH-DOMAIN. |
| `LIST-DOMAIN (<name>)` | List matching domains + start addresses. Default: all domains of current user. |
| `WRITE-DOMAIN-STATUS [<name>]...` | Print ALL available information about the domain(s). Default: current domain. |
| `RENAME-DOMAIN <old>, <new>` | Rename. |
| `COPY-DOMAIN <source>, <dest>` | Copy a whole domain; source may be prefixed `(directory:user)`. New destination must be in double quotes. |

---

## 4. Listing Domains

- **In NLL or the Monitor**: `LIST-DOMAIN <domain name>` - writes all domains with matching names and their start addresses (default: all domains of the current user). In the **Monitor** a user name in parentheses may prefix the name to list another user's domains; in **NLL** use `LIST-SEGMENT` with a `(user)` prefix for that (sections 6.1.5 and 7.1.2).
- `LIST-STANDARD-DOMAINS` - names of all standard domains and their segments; allowed for all users (section 8.2.3).
- `WRITE-DOMAIN-STATUS` (NLL) - full detail for one or more domains (section 6.1.6).

### 4.1. Standard domains (section 8.2)

Standard domains are a fast-lookup name table (like ND-100 reentrant subsystems) searched before any description file is opened - used for compilers, NLL, etc. The table survives a warm start but not a cold start.

- `DEFINE-STANDARD-DOMAIN <standard name> <domain name>` - user SYSTEM only.
- `DELETE-STANDARD-DOMAIN <name>` - user SYSTEM only; not while in use. Removes the table entry, not the domain.

---

## 5. Running Domains

Manual sections 8.1.x. Command lookup order when you type a name at `N500:` (section 8.1.2): basic commands, then standard domains, then your own domains, then user SYSTEM's domains, then temporary macros, then permanent `:MACR` files - otherwise `NO SUCH COMMAND OR DOMAIN`.

| Command | Effect |
|---------|--------|
| `PLACE-DOMAIN <name>` | Make a domain ready: searched in your description file, then SYSTEM's. Sets program counter to the start address, initializes trap enable registers, maps each logical segment onto a physical segment. Program segments are read-only (use DEBUG-PLACE to patch). |
| `RUN` | Start the placed domain at its start address. |
| `GO <address>` | Start execution at a specific address. |
| `RECOVER-DOMAIN <name>` | PLACE-DOMAIN + RUN in one. The words RECOVER-DOMAIN may be omitted - just type the domain name. |
| `CONTINUE` | Restart at the current program counter (after Escape, a breakpoint, or normal stop). After a normal stop (MON 0 / stack underflow) restarts at the original start address. |
| `RESIDENT-PLACE <name>` | Place a domain PERMANENTLY in memory (no swapping) - user SYSTEM only, one at a time (section 8.8.16). |
| `DEBUG-PLACE <name>` | As PLACE-DOMAIN but writable program segment, for breakpoints/patches (section 8.5.3). |

---

## 6. Seeing What Is Active and Running (Status Commands)

This is the core "is the CPU alive, who is on it, what is it doing" toolbox.

### 6.1. Any user

| Command | What it shows | Section |
|---------|---------------|---------|
| `WHO-IS-ON` | List of users currently logged on the ND-500. | 8.9.5 |
| `LIST-ACTIVE-PROCESSES` | Active processes and their process names - including processes NOT owned by a terminal background program. | 8.9.6 |
| `VERSION` | Version numbers of subsystem (background monitor), system part (SINTRAN part), swapper, and microprogram. Also a cheap "is the monitor sane" check. | 8.9.7 |
| `TIME-USED` | ND-500 CPU time, ND-100 CPU time and clock time since the Monitor was entered. | 8.9.4 |
| `LIST-EXECUTION-QUEUE <interval>` | The currently executing program, its priority, the queue of jobs for the ND-500 and their priorities - repeated every `<interval>` seconds. THE command for "what is the ND-500 doing right now and in what order". | 8.6.5 |
| `LIST-OPEN-FILES` | Files currently opened from the Monitor. | 8.3.3 |
| `STATUS` | Debugging status (registers etc.) of the current process. | 8.5.18 |

### 6.2. User SYSTEM (Supervisor)

| Command | What it shows | Section |
|---------|---------------|---------|
| `PROCESS-STATUS` | Summary of ALL active processes: terminal number, user name, process status (idle or active), ND-500 CPU time used and login time since Monitor entry. | 8.10.8.4 |
| `LIST-ACTIVE-SEGMENTS <process no.>` | All segments in use by a process, logical-to-physical segment mapping, process name. `<process no.>` = OWN/-1 (own), ALL/-2 (all). | 8.10.9.2 |
| `LIST-PROCESS-TABLE-ENTRY <process no.>` | Full process description: process segment, program and data capabilities. OWN/-1, ALL/-2 accepted. | 8.10.9.4 |
| `LIST-SEGMENT-TABLE-ENTRY <segm. no.|ALL>` | Physical segment table: name, type, owner process, size, attributes, swap file allocation, current users. | 8.10.9.3 |
| `LIST-TABLE <table name>` | Raw system tables. Subcommands include SW-SEGM-TAB, MEMORY-MAP, LAST-N500-MSG (ring buffer of last 64 messages to ND-500), N500-MSG, FOLLOW-LINK, FOLLOW-TABLE. | 8.10.9.1 |
| `LIST-SWAP-FILE-INFO <no.|ALL>` | Swap file statistics and usage. | 8.10.10.3 |
| `MEMORY-CONFIGURATION` | Current memory configuration. | 8.10.4.2 |
| `LOOK-AT-HARDWARE <register name>` | Internal ND-500 CPU registers and ND-100/ND-500 interface registers (INTERFACE, MMS, single registers, or all approx. 80). NOTE: microprogram must be restarted afterwards with MICRO-START. | 8.10.7.3 |

### 6.3. Process identity (section 8.7.1)

A process number is: process index (0-31) in the upper half + a 16-bit cycle number in the lower half of a word. Process 0 is always the swapper. Numbers come from `WHO-IS-ON` or `PROCESS-STATUS`. Processes may also be named: `SET-PROCESS-NAME <name>` (up to 16 chars, optional `(user)` prefix), and monitor calls SPRNAME (MON 425B) / GPRNUM (MON 426B) / GPRNAME (MON 427B) translate between the two.

---

## 7. Priorities and Scheduling

### 7.1. SET-PRIORITY (section 8.9.8, user SYSTEM only)

```
SET-PRIORITY <ND-100 mon call priority>, <max % of ND-100 time>,
             <ND-500 priority>, [<process no>]
```

| Parameter | Meaning | Default |
|-----------|---------|---------|
| ND-100 mon call priority | Priority (0:70B) of the ND-100 twin process that executes monitor calls on behalf of the ND-500 process. | 70B |
| max % of ND-100 time | Max percentage of ND-100 CPU time the twin process may use over a 2-second period. If exceeded, the mon call priority is reduced to 20B. | 50% |
| ND-500 priority | 0:377B. **0 = time-sliced**, priority varying dynamically between 20B and 61B. **Non-zero = fixed priority.** A priority given in the source program is ignored. | dynamic (time-sliced) |
| process no | Which process to affect. | own process |

How it works (section 8.9.8): every SINTRAN monitor call from an ND-500 process is executed by a twin RT process on the ND-100. Caution from the manual: the measured ND-100 CPU time covers only interrupt levels 4 and 1 (not 14, 12, 3, 11, 10), so the ND-100 can saturate even when the sum of all max-percentages is well below 100%.

### 7.2. Time-slicing and swap-rate demotion (section 8.8.x)

For time-sliced processes the swapper measures each process's swap rate (time between 20 page faults). If the rate exceeds an expression involving the global swap rate and the LOW-PRIORITY factor (a system parameter), the process priority is set to 16. The higher the LOW-PRIORITY factor, the more likely demotion is.

Defaults for ND-100/ND-500 priority and max ND-100 CPU percentage are set system-wide with `SET-SYSTEM-PARAMETERS` (section 8.10.11).

### 7.3. Seeing priorities

`LIST-EXECUTION-QUEUE <interval>` shows the running program and every queued job WITH their priorities (section 8.6.5).

---

## 8. Performance Measurement (section 8.6)

All histogram and log commands share ONE system-wide buffer - only one user at a time; release it explicitly or it is released when you leave the Monitor.

### 8.1. Program histogram (where does my program spend time)

```
N500: SET-HISTOGRAM <start addr>, <max addr>, (<no. of intervals>)   reserve+clear, up to 64 intervals
N500: START-HISTOGRAM        sample PC every 20 ms (cumulative)
N500: STOP-HISTOGRAM
N500: PRINT-HISTOGRAM        print (buffer kept)
N500: RELEASE-HISTOGRAM      free the buffer for other users
```

### 8.2. Monitor call logging (ND-100 load caused by the ND-500)

- `START-MONCALL-LOG [OWN/ALL]` - count every monitor call (default OWN).
- `PRINT-MONCALL-LOG` - per-call-number counts up to 777B (does not clear).
- `STOP-MONCALL-LOG` - release the buffer.

The manual's rule of thumb: ND-100 CPU load imposed by the ND-500 is roughly proportional to the number of monitor calls executed from the ND-500 (section 8.6.2.1).

### 8.3. Process logging (who is using the CPU)

- `START-PROCESS-LOG-ALL` - CPU usage of ALL active processes, sampled every 20 ms, as percent of total CPU capacity.
- `START-PROCESS-LOG-ONE <process no>` - one process, split into six states: 1) Idle, 2) Waiting for swapper, 3) Using swapper, 4) In monitor call, 5) Active, 6) Waiting for CPU.
- `PRINT-PROCESS-LOG <first process>` - print accumulated measurements (logging continues, cumulative).
- `PROCESS-LOG-ALL <interval> <first process>` / `PROCESS-LOG-ONE <process no> <interval>` - periodic self-printing versions (buffer cleared between reports).
- `RELEASE-LOG-BUFFER` - free the shared buffer.

### 8.4. Swapping and queue

- `SWAPPING-LOG <interval>` - page faults, transfers, free space etc. per interval / average / total. User SYSTEM only. Stop with Escape.
- `LIST-EXECUTION-QUEUE <interval>` - see section 6.1 above.

---

## 9. Controlling and Stopping Processes / the CPU

### 9.1. Process control (sections 8.7.x, 8.10.8.x)

| Command | Effect | Who |
|---------|--------|-----|
| `RESTART-PROCESS <process name>` | Restart a process halted by STOPPR (MON 501B), or set its repeat bit if active. | any |
| `GET-FLAG <process no.>` / `SET-FLAG <process no.> <value>` | Read a process's 32-bit output flag / write its input flag (simple ND-100 to ND-500 signalling; no queueing - a second write overwrites). ND-100 side: RFLAG (100B) / SFLAG (101B) functions of N500M (MON 60B). | any (other terminal) |
| `ATTACH-PROCESS <process no>` | Route subsequent LOOK-AT / RUN etc. to another process (used e.g. to debug the swapper). | SYSTEM |
| `LOGOUT-PROCESS <process no>` | Abort a process cleanly, release resources, force the user out of the Monitor. The NORMAL removal command (analogous to @STOP-TERMINAL). | SYSTEM |
| `ABORT-PROCESS <process no>` | Abort WITHOUT table/queue cleanup. Last resort for a system hangup only. | SYSTEM |

### 9.2. CPU control (sections 8.10.3, 8.10.6)

| Command | Effect |
|---------|--------|
| `STOP-ND-500` | Stop the ND-500 CPU. Next process start triggers automatic warm start: microcode reload, swapper placed and started. |
| `MICRO-STOP` | Stop the microprogram, ALL registers retained; running jobs continue after MICRO-START - use this when a stop-and-resume without restarting jobs is needed. |
| `MICRO-START <address>` | Resume microprogram at a control store address. |
| `MASTER-CLEAR` | Listed in the monitor command list (section 15.7). |

Process creation happens implicitly: a process is allocated when a user starts the monitor and terminates when they leave it; all domains run in one monitor session share that process. An ND-100 RT program can also allocate one via the RESRV function of N500M (MON 60B) and release it with RELIS (section 8.7.3).

---

## 10. ND-5000 Notes

**Marked: repository reverse-engineering analysis, NOT from ND-60.136.04A.** Sources: [SINTRAN/ND5000](../../SINTRAN/ND5000/README.md) analysis documents and [SINTRAN/ND500](../../SINTRAN/ND500/ND500-STATUS-AND-INDEX.md).

From the user's terminal the ND-500 Monitor is the same tool on ND-5000 systems - domains, listing and status commands are the operating-system level and unchanged. What differs is underneath:

- The ND-5000 (SAMSON) has NO 3022 bus interface; the ND-100 talks to it over the **Octobus** fabric (frame protocol + ACCP command layer for presence/selftest/CS-load), with shared memory through the 5MPM window. See `SINTRAN/ND5000/OCTOBUS-DEVICE-CONTROLLERS-ANALYSIS-AND-EMULATION-PLAN-2026-07-19.md`.
- Activation uses a mailbox work flag (X5ACT) rather than the classic MAR-write kick; microcode load goes through the Octobus CSLOAD path.
- Control store format differs: the repo's `ND500UC CONTROL-STORE.DATA` file was identified as ND-5800 microcode, 128 bits x 16384 words, versus the classic ND-500's 144-bit words (the 144-bit binary has not been found in the repo). So the `LOAD-CONTROL-STORE` defaults quoted in section 2.4 above (144-bit, 20000B words) describe the classic ND-500.
- For ND-5000 hardware-level detail see [ND-05.020.01 EN ND-5000 Hardware Description](ND-05.020.01%20EN%20ND-5000%20Hardware%20Description.md) and [ND-830102.1B EN ND-5000 ES Model C Hardware Maint. Manual-Sintran](ND-830102.1B%20EN%20ND-5000%20ES%20Model%20C%20Hardware%20Maint.%20Manual-Sintran.md) in this folder.

---

## 11. Quick Reference Card

```
Enter/leave:        @ND-500 [domain]        N500: EXIT
Create domain:      NLL: SET-DOMAIN "X" / LOAD-SEGMENT f / EXIT
List domains:       LIST-DOMAIN             (NLL + Monitor)
Domain detail:      WRITE-DOMAIN-STATUS     (NLL)
Std domains:        LIST-STANDARD-DOMAINS / DEFINE- / DELETE-STANDARD-DOMAIN
Run:                <domain-name>  |  PLACE-DOMAIN + RUN  |  GO <addr>  |  CONTINUE
Who/what is on:     WHO-IS-ON / LIST-ACTIVE-PROCESSES / PROCESS-STATUS (SYS)
CPU alive/version:  VERSION / TIME-USED
Queue+priorities:   LIST-EXECUTION-QUEUE <interval>
Set priority:       SET-PRIORITY <n100prty>,<max%>,<n500prty>,[proc]   (SYS)
Per-process detail: LIST-ACTIVE-SEGMENTS / LIST-PROCESS-TABLE-ENTRY (SYS; OWN/-1, ALL/-2)
Segments:           LIST-SEGMENT-TABLE-ENTRY <n|ALL>  (SYS)
CPU load:           START-PROCESS-LOG-ALL/-ONE, PRINT-PROCESS-LOG, RELEASE-LOG-BUFFER
Swapping:           SWAPPING-LOG <interval> (SYS) / LIST-SWAP-FILE-INFO
Availability:       SET-ND-500-UNAVAILABLE / SET-ND-500-AVAILABLE      (SYS)
Stop CPU:           STOP-ND-500 (warm-start next use) / MICRO-STOP + MICRO-START
Kick a user:        LOGOUT-PROCESS <n> (clean) / ABORT-PROCESS <n> (last resort)
Memory:             MEMORY-CONFIGURATION / GIVE- / TAKE-ND-500-PAGES   (SYS)
Microcode:          LOAD-CONTROL-STORE / COMPARE-CONTROL-STORE          (SYS)
```

The complete alphabetical monitor command list is in the manual, section 15.7.

---

**Document created**: 2026-07-19
**Sources**: ND-60.136.04A ND-500 Loader/Monitor (primary); SINTRAN/ND5000 repository analysis (section 10 only, marked).
