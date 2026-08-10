# Programming Against a SIBAS-II Database

**SIBAS-II is a CODASYL-DBTG database system — call-oriented, not SQL. This chapter covers the
core concepts, the real DDL (schema definition) syntax, and the real DML (data manipulation) call
names found in this repo's actual PD sheets. It does not invent syntax that hasn't been sourced.**

Install docs: [ND-10166](../../Installation/Software/ND-10166/README.md) (older article) /
[ND-210166](../../Installation/Software/ND-210166/README.md) (later article, fully verified from
a real PD sheet).

---

## 1. The mental model — CODASYL, not relational

Source: the SIBAS-II PI sheets ([`ND-10166-A1-EN.md`](../../Installation/Product-Info/ND-10166-A1-EN.md),
[`ND-210166-A1-EN.md`](../../Installation/Product-Info/ND-210166-A1-EN.md)). [PI]

There is no `CREATE TABLE`/`SELECT` — SIBAS predates SQL and follows the CODASYL DBTG model:

- A database is divided into **realms** — named storage areas you open with a declared usage
  mode (`Retrieval`/`Load`/`Up-date`) and protection mode (`Non exclusive up-date`/`Exclusive
  up-date`).
- Each realm holds **records** of one or more **record types**, analogous to a table's row type,
  but records are not implicitly related by shared column values — relationships are explicit
  **set types**, not joins.
- Access is **call-oriented**: your program (in FORTRAN, COBOL, BASIC, NORD-PL, PLANC, or
  Assembly) calls named DML routines directly, not a query parser.

## 2. Defining structure — location modes, keys, sets

### Location modes (where/how a record is physically stored)
- **CALC** — the primary key is hashed/randomized to distribute records across the realm. A CALC
  key may permit or prohibit duplicates, and may span multiple non-contiguous items in the record.
- **SERIAL** — records are stored in arrival order; location depends only on when they were
  written, not on any key value.

### Keys and indices
- **Primary key** — not mandatory, but usually desirable; drives CALC placement if used.
- **Secondary (search) keys** — any number, each one or more items; automatically maintained,
  always stored in ascending order, internally implemented as **B-trees**.
- "Out of the blue" access — retrieve by primary or secondary key value plus record type; if
  multiple records share a key, the first is returned, others via **relative access**.

### Set types (CODASYL relationships)
- **Single-member set** — one owner record type, one member record type (e.g. `BRANCH-OFFICE`
  owns `CUSTOMER` via a set named `HANDLES`).
- **Multi-member set** — one owner, two or more member record types (SIBAS-II PI sheet only;
  not mentioned on the older ND-10166 sheet).
- **Involuted set** — a record type related to itself, i.e. a hierarchy (e.g. `PARTS USES PARTS`
  for a bill-of-materials).
- Each set occurrence is stored as a **chain** — you choose unidirectional (link-to-next only) or
  bidirectional (link-to-next-and-prior) per set type.
- SIBAS does **not** support CODASYL sorted sets — secondary indices are the substitute (always
  sorted by key already), or a manually-maintained set under program control.

### The real DDL syntax — defining/redefining realms

Source: the `ND-210166F` PD sheet §1.1.4, verbatim — this is DRL (Database Redefinition
Language), run via the `SIB2-DRL` program: [PD]

```
NEW xxxx-REALM <realm-name> OS-FILE(<file-name>)
    REALMSIZE <no-of-pages>
    ( ADDITIONAL OS-FILE <file-name> SIZE <no-of-pages>
      ADDITIONAL OS-FILE <file-name> SIZE <no-of-pages> )...

CHANGE xxx-REALM <realm-name>
    ( REALMSIZE <no-of-pages> )
    ( ADDITIONAL OS-FILE <file-name> SIZE <no-of-pages>
      ADDITIONAL OS-FILE <file-name> SIZE <no-of-pages> )...
```
Rules from the same source: one OS-file belongs to exactly one realm, but a realm may have
several additional OS-files; max 3 additional OS-files per `SIB2-DRL` run (run it again for more);
`REALMSIZE` in a `CHANGE` cannot be combined with adding additional OS-files — resize the *last*
additional OS-file instead; delete an additional OS-file by setting its `SIZE` to zero (it must be
empty first). A worked start:
```
START REDEFINITION DATABASE
DATABASE <databasename> (DPA-PASSWORD) <password>
(SUPPRESS .... )
SCRATCH-FILE <filename> (DIRECTORY <abbr-dir-name:user>)
```
(`SCRATCH-FILE`'s `DIRECTORY` clause is an addition in this same F revision.) **Warning from the
PD sheet:** you cannot define a new index on a set owner and a new set in the same redefinition
run.

**Beyond realm/set definition** (record types, item definitions, individual index declarations)
— **not covered by any source read for this document.** The full DDL grammar lives in the SIBAS-II
ND User Manual (`ND-60.127.5`, already in this repo — see §4 below for how to read it) — this
chapter does not invent syntax beyond what the PD sheet actually shows.

## 3. The real DML call names — manipulating data

Source: the `ND-210166F` PD sheet §4.2.1 "New Commands/Features", verbatim — these are actual
SIBAS DML subroutine names, called from your host-language program: [PD]

| Call | What it does |
|---|---|
| `SOPDB` | open database (usage/protection mode declared here) |
| `SCLDB` | close database — F-revision adds accounting info to the log: user ID, open time, read/update mode, call counts, `STORE`/`SMDFY`/`SRASE` counts |
| `SFTCH` | find (by key) |
| `SGET` | get (fetch the current record) |
| `STORE` | store a new record |
| `SMODFY` | modify the current record |
| `SRASE` | erase (delete) the current record |
| `SRRLM` | ready a realm (declare usage/protection mode for it) |
| `SFRNO` | find using a physical record number (relative to realm start) instead of a key |
| `SFRGT` | combined `SFRNO` + `SGET` |
| `SWHAT` | return the realm-name/record-number of a "temporary db key" |
| `SSGET` | combined `SFORG` (find-owner?) + `SREMB` (find-member?) + `SGET` — exact expansion of `SFORG`/`SREMB` not stated in the source read |
| `SYNCPT` | synchronized checkpoint — taken once all active critical sequences complete |
| `SEMSG` | multifunction: read log info, db info, user info, error messages, SSI/SEC codes |
| `SINFO` | database/realm structural info (free space, page counts) |
| `SUBEG` / `SUENDO` | begin/end a **transaction unit** (backs out cleanly on failure; requires before-image logging) |

**Standard Error Codes (SEC)** are implemented for every SIBAS error — read the actual text with
`SEMSG`, or load `UE-ERMSG-804` to decode a returned `DBEC` or negative status. This repo has not
independently traced the exact parameter lists for any of these calls — see §4.

## 4. Where the actual call syntax/parameters live

**This document intentionally stops short of inventing per-call parameter lists.** Two real
manuals are already in this repo and are the authoritative source for that level of detail:

- **`ND-60.127.5 EN THE DATABASE SYSTEM SIBAS II ND User Manual`** —
  [Reference-Manuals/ND-60.127.5 EN THE DATABASE SYSTEM SIBAS II ND User Manual.md](../../Reference-Manuals/ND-60.127.5%20EN%20THE%20DATABASE%20SYSTEM%20SIBAS%20II%20ND%20User%20Manual.md)
  — the DML/DDL programming reference (this is a large document; search it for a specific call
  name like `SFTCH` or `STORE` rather than reading start to end).
- **`ND-30.009.3 EN SIBAS II Operator Manual`** —
  [Reference-Manuals/ND-30.009.3 EN SIBAS II Operator Manual.md](../../Reference-Manuals/ND-30.009.3%20EN%20SIBAS%20II%20Operator%20Manual.md)
  — operating a running SIBAS system (`SIBAS-SERVICE` commands like `STATUS`,
  `DATABASE-STATUS`, `SET-PASSIVE`, `TURN-ON/OFF-TERMINAL-LOG`).

## 5. Interactive access without writing a program — SIBINTER

`SIBINTER` (the "ISAM interactive"-equivalent for SIBAS — see the module table in the PI sheets)
lets you store/retrieve/modify data and run simple reports **without writing a host-language
program**, using the same DML calls interactively. It's installed alongside the DML libraries in
every version documented in this catalog (e.g.
[ND-10166E's `SIBINTER-MH-E00:PROG`/`SIBINTER-MX-E00:PROG`](../../Installation/Software/ND-10166/ND-10166E/README.md)).
Exact command syntax inside `SIBINTER` itself is not covered by any source read for this document
— see the User Manual.

## 6. Concurrency, integrity, recovery — what SIBAS gives you for free

- **Run-units** — the same program executed by multiple simultaneous users, each an independent
  "run-unit."
- **Extended monitor mode** — every record a run-unit is actively processing is watched; actions
  by other run-units that could affect it are surfaced back to it. Individual records can be
  explicitly **locked**.
- **Privacy locks** — an item can be marked as a privacy lock; retrieving the record then requires
  the correct value for that item (occurrence-level access control, not a database-wide
  password) — though a DBA password bypasses the need to supply it (F-revision addition).
- **Logging** — routine log (input/output packets, for reprocessing after a crash), before-image
  log (roll back to a consistent checkpoint), after-image log (roll a backup forward). Checkpoints
  are automatic on database close, or can be forced by a program call.
- **Restructuring** — add/delete/rename items, record types, set types, indices, texts, without a
  full dump/redefine/reload cycle (via `SIB2-DRL`, §2 above).

---

## See Also

- [ND-10166](../../Installation/Software/ND-10166/README.md) / [ND-210166](../../Installation/Software/ND-210166/README.md) — install docs.
- [ISAM programming](../../Installation/Software/ND-210073/README.md) — the simpler,
  non-CODASYL indexed-file alternative also used from FORTRAN/PLANC/COBOL/Pascal.
