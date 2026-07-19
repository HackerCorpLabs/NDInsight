# 06 - Remote and Network Printing

This document answers "is printing done over the network (TCP or COSMOS)?" The
short answer for the SINTRAN era: there is no TCP/IP printing; remote printing
happens either over **NORDNET / COSMOS** (NORD-to-NORD) or through a **Remote
Job Entry (RJE)** emulator to a non-NORD mainframe. Both are documented below,
with a clear line between what is verified and what is not.

Sources: `../../Reference-Manuals/ND-60.134.2 EN SINTRAN III Communication
Guide.md` chapter 3 (RJE) and its remote-batch material;
`../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md` (`APPEND-REMOTE`).

---

## 1. There is no TCP/IP printing here

SINTRAN III predates TCP/IP as the standard. Networking is done with Norsk
Data's own stacks: **NORDNET** (NORD-to-NORD networking) and **COSMOS** (the
wider ND network/communications product family, including X.25). "Network
printing" on these machines means one of the two mechanisms below - not an LPR/
IPP/socket print service.

**UNVERIFIED:** the exact COSMOS product boundaries and version details are not
re-derived here; this document describes the SINTRAN-visible mechanisms. For the
network stack itself see the XMSG/COSMOS material elsewhere in this repository
(`../XMSG/`) and the Communication Guide.

---

## 2. NORD-to-NORD: COSMOS Spooling (the real network-printing product)

The proper "print to a printer on another NORD computer" product is **COSMOS
Spooling**, also called **C-S-S** (COSMOS Spooling Service). It is documented in
the System Supervisor manual (`../../Operations/SINTRAN/ND-30.003.007 EN SINTRAN
III System Supervisor.md`, section 5.6) and is built on the COSMOS
**File-Transfer** subsystem, which rides on **XMSG**.

### 2.1 How it works

The operator defines a list of **local printer names**. Each local name is an
alias that maps to a **printer on a local or remote system**. Users only ever
learn the local names - they never need to know the real printer names on the
remote machines (System Supervisor 5.6).

The flow (manual, Figure 39) uses spooling queues **twice**:

```
   User prints in NOTIS / application
        |  -> copied to a local COSMOS spooling file
        v
   COSMOS Spooling Queue on MY-COMPUTER   (RT-program COSPO)
        |  -> sent over XMSG port to the remote system
        v
   Remote system's spooling queue         (RT-program XFTRAD on the remote host)
        |
        v
   Real printer on the remote system
```

Doing it in two stages (local queue, then remote queue) keeps the user's wait
time minimal - their print returns as soon as the local copy is made.

### 2.2 Operator setup (verified commands)

- Create the transport peripheral file once:
  `@SET-PERIPHERAL-FILE "COSMOS-SPOOLING"` with `DEVICE NUMBER (OCT): 1731`.
- Startup mode files load and start the spooler RT-program **COSPO**
  (`@RTON COSPO`), start spooling on `COSMOS-SPOOLING`, and define the network
  printers (`COS-DEF-PRIN`). (System Supervisor Table 16.)
- The operator manages printer definitions with the **COSMOS Spooling Service
  program** (`@COS-SPOOL-SERVIC`, run as user SYSTEM). Its commands include:

| C-S-S command | Purpose |
|---------------|---------|
| `DEFINE-PRINTER <local name> <remote system> <remote printer> <header Y/N> <no. of local spooling files>` | Map a local printer name to a real printer on a remote system. |
| `DELETE-PRINTER <local name>` | Remove a local printer definition. |
| `LIST-PRINTERS` | List the defined local printers. |
| `LIST-NAMES <system>` | List printer names on a system. |
| `CHANGE-PASSWORD <remote system> <password>` | Credentials for the remote FLOPPY-USER. |
| `GET-FILE-STATISTICS`, `LIST-SERVER-ERRORS`, `DEBUGPRINT-ON/OFF` | Diagnostics. |

A local printer definition with **no** remote printer name is a **default
printer** - used when a user prints without naming one (System Supervisor 5.6).

So the NORD-to-NORD network-printing answer is: **COSMOS Spooling over XMSG**,
with an operator-maintained name map, not a raw remote-file hack.

### 2.3 The simpler remote-file view

Underneath, the transport is COSMOS File-Transfer, and the Communication Guide's
remote-file examples show output going to a **remote line-printer** directly
(e.g. a compiler listing "output to the remote line-printer"). COSMOS Spooling is
the managed, operator-friendly layer on top of that capability.

---

## 3. To a non-NORD mainframe: Remote Job Entry (RJE)

To submit work (including print output) to a **large non-NORD host**, SINTRAN
uses an **RJE emulator** - a foreground program that makes the ND machine look
like that vendor's remote batch terminal. Verified facts (Communication Guide
chapter 3):

- RJE is delivered as **software packages emulating RJE terminals** for **CDC,
  Honeywell, IBM, Siemens and UNIVAC** equipment. (These, plus interactive
  terminal emulators for IBM 3270, Honeywell VIP 7750 and Univac UTS-400, form
  the "NORD Intelligent Data Terminals" / IDT packages.)
- **User SYSTEM** loads and starts the emulator as a foreground program. On
  start it allocates a terminal (normally terminal 2) as the **remote batch
  console**.
- Once running, **any timesharing user** may append jobs to a batch queue in the
  local computer; the jobs contain the **host's** job control language, and are
  sent to the host in due course.

### `@APPEND-REMOTE`

```
@APPEND-REMOTE <remote computer>,<input file>
```

- `<remote computer>` - a peripheral file name denoting the host. Standard
  names are `IBM`, `CDC`, `UNIVAC`, `HONEYWELL-BULL` (default type REM).
- `<input file>` - the file containing one or more batch jobs; it must have read
  access for user RT.

Example:
```
@APPEND-REMOTE UNIVAC,JOB-1
```
appends `JOB-1` to the batch queue for the remote computer UNIVAC.

Related commands (Communication Guide 3.x): `@LIST-REMOTE-QUEUE` lists a remote
batch queue; `@DELETE-REMOTE-QUEUE-ENTRY` removes an entry.

RJE is the path by which print output destined for a mainframe leaves the ND
machine: you submit a job, and the host does the printing on its own printers
(or returns output). It is batch/job-oriented, not a live device queue like
local spooling.

---

## 4. Three flavours of "print/submit elsewhere"

The Communication Guide summarises three ways to run batch (and thus route
output), which maps directly onto the printing options:

1. **Local batch** - runs on this ND machine; output can be spooled locally
   (docs 04/05).
2. **NORDNET remote batch** - runs on **another NORD** computer; can direct
   output to that machine's (remote) printer (section 2).
3. **RJE** - submits to a **non-NORD host**; the host runs and prints it
   (section 3).

---

## 5. Interaction with XMSG memory (operational note)

The Communication Guide warns that XMSG must be started (its PAGING-OFF and
buffer-area segments fixed in physical memory) **before** starting NORDNET or
SPOOLING, "since these can 'steal' the POF space reserved for XMSG when they fix
their segments." So on a system that uses XMSG, spooling and NORDNET start
ordering matters. This is an operational configuration detail, cited here
because it directly couples the spooler to the networking start-up sequence.

---

## 6. What is verified vs. not (honesty note)

- **Verified from the manuals:** COSMOS Spooling (C-S-S) architecture, the COSPO
  / XFTRAD RT-programs, the two-stage queue, the `COSMOS-SPOOLING` device
  (1731 octal), the C-S-S service-program commands (`DEFINE-PRINTER` etc.); RJE
  packages and vendors, `@APPEND-REMOTE` syntax and standard host names; remote
  line-printer output over COSMOS File-Transfer; and the XMSG start-order caveat.
- **Not fully verified here:** the byte-level COSMOS/X.25 transport of print
  traffic and the exact remote-file naming syntax for a one-off remote printer
  (outside C-S-S). Good candidates to confirm from the Communication Guide in
  full or from the SINTRAN source (see doc 07).
