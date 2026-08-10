# ND-210729 — UNIQUE II (SIBAS, for ND-100)

> Status: IN-PROGRESS — no PD sheet or floppy located; documented from a real, detailed PI sheet with a worked example

| Field | Value |
|-------|-------|
| ND article number | `ND-210729` |
| Product name | UNIQUE II — SIBAS for ND-100 (one of 8 target/database variants — see below) |
| Functional category | 4th-generation application tools (DIALOGUE family) |
| ND doc-category tag | 60 General [curated] |
| CPU target | ND-100 |
| OS requirement | SINTRAN III version J or later; SIBAS-II version E or later (version B prerequisites) |
| Related products | `ND-211005` UNIQUE Text System (hard prerequisite for any UNIQUE product, see [../ND-211005/README.md](../ND-211005/README.md)) |

## The UNIQUE / DIALOGUE-1 product family — many article numbers, two independent PI sheets

Two independent PI sheets cover this family:
[`ND-210729-A1-EN.md`](../../Product-Info/ND-210729-A1-EN.md) (UNIQUE II / UNIQUICK feature
description, with a worked example) and
[`ND-211202-A1-EN.md`](../../Product-Info/ND-211202-A1-EN.md) ("DIALOGUE-1" — the umbrella
package, with a full **Product Specifications** table of article numbers and additional modules
neither the ND-210729 sheet nor this catalog had seen: **UNIQUE START** (database definition),
**UNIQUE XTRA** (reports/queries), **UNIQUE DOCUMENTATION** (system/user documentation), and
runtime-only variants).

| Product | ND-100/110 | ND-500/5000 |
|---|---|---|
| DIALOGUE-1 (full package: UNIQUE II SIBAS + UNIQUICK SIBAS + UNIQUE Upgrade) | `ND-211202` | `ND-211203` |
| DIALOGUE-1 Runtime | `ND-211204` | `ND-211205` |
| UNIQUE Upgrade (UNIQUE START + XTRA + DOCUMENTATION) | `ND-211400` | `ND-211401` |
| UNIQUE II, SIBAS | `ND-210729` (this doc) | `ND-210730` |
| UNIQUE II, SIBAS Runtime | `ND-211083` | `ND-211084` |
| UNIQUE II, ISAM | `ND-210731` | `ND-210856` |
| UNIQUE UNIQUICK, SIBAS | `ND-210871` | `ND-210872` |
| UNIQUE UNIQUICK, ISAM | `ND-210869` | `ND-210897` |

> **Discrepancy between the two source sheets, not resolved:** the `ND-210729` sheet lists ISAM
> variants as `ND-210731`/`ND-210895` (ND-100/ND-500) and UNIQUICK-ISAM as `ND-210896`/`ND-210897`.
> The `ND-211202` sheet instead lists UNIQUE II ISAM as `ND-210731`/`ND-210856` and UNIQUICK ISAM
> as `ND-210869`/`ND-210897`. The two sheets **agree** on `ND-210731` (UNIQUE II ISAM, ND-100)
> and `ND-210897` (UNIQUICK ISAM, ND-500), but **disagree** on the other two ISAM article numbers.
> Both are presented above without picking a winner — treat `ND-210895`/`ND-210896` (from the
> earlier sheet) and `ND-210856`/`ND-210869` (from this later, more detailed sheet) as unresolved
> alternates until a PD sheet or floppy settles it.

**`ND-211250`** (a floppy you have, containing `DOC-MAIN`/`DOC-SIBAS`/`DOC-XTRA`/`DOC-LIB`/
`INSTALL-UNIQ:PROG` and real `:MODE` scripts) is very likely **UNIQUE DOCUMENTATION** — its file
prefix `DOC-` matches this module's real description above almost exactly ("produces system
documentation and user documentation... database documentation, cross-reference lists") — but
this module has no explicit standalone article number in either sheet read, so this is a strong
inference, not a confirmed identity. See [../ND-211250/README.md](../ND-211250/README.md).

**No PD sheet or floppy image has been located for any of the `ND-210729`-family article numbers
themselves** (only `ND-211250`, likely a different, related module, has one) — this entry is
PI-sheet-only.

## What UNIQUE II is

A 4th-generation application-development tool — not a program generator, but a single PROGRAM
that interprets an application description you write: a screen-form layout, plus a mapping from
form fields to database items and the relations/functions between them. Runs on ND-100 or ND-500,
against either SIBAS-II or ND ISAM (see [../ND-210073/README.md](../ND-210073/README.md)). [PI]

## Building a UI — the real, worked example

Source: the PI sheet's own worked example, verbatim. [PI] An application is a text file with two
parts: a **screen layout** (`start-form`...`end-form`) and a **database mapping**
(`start-fields`...`end-fields`):

```
+-----------------+
| Start-form      |
|                 |
| CUSTOMER        |
|-----------------|
| Number | Name   | Shortname   |
| A..... | A..... | A...........|
| A..... |        |             |
...
| end-form        |
|-----------------|
| start-fields    |
| data-base-name = ORDRBASE
| register-name = CUSTOMER
| field = "CUSTNO" key
| field = "NAME" alternative-key letter-type (UPPER)
| field = "SHORTNM" alternative-key letter-type (UPPER)
| field = "ADDRESS1"
| field = "ADDRESS2"
| field = "ADDRESS3"
| field = "TELNO"
| field = "CONTPERS" alternative-key letter-type(UPPER)
| field = "PAYCOND"
| end-fields
+-------------------+
```

Reading it: each `A....` run in the form is a field placeholder — its actual type, length, and
display format come from the data dictionary, not from this file. In `start-fields`,
`data-base-name`/`register-name` name the SIBAS realm this form is bound to (a "register" here
= a SIBAS realm — see [SIBAS-DATABASE-PROGRAMMING.md](../../../Developer/Workflow/SIBAS-DATABASE-PROGRAMMING.md)),
and each `field = "<item>"` line maps one database item to one form position, optionally marked
`key` (the realm's primary/CALC key) or `alternative-key` (a secondary index — `letter-type
(UPPER)` forces uppercase comparison). Uppercase words in the fields section are database/realm/
item names; `UPPER` itself is the one exception (a type keyword, not a name).

**Once running**, the form supports navigation (`F`/`N`/`P`/`L` — first/next/previous/last
record), direct search by key or alternative key, direct screen printout, and sorted printout of
record lists. Control flow inside an application uses `IF`/`ELSEIF`/`ELSE`/`ENDIF` and
`BEGIN`/`END` sequences — special-purpose FORTRAN/COBOL routines are only needed beyond that.

## UNIQUE UNIQUICK — the menu-driven generator for the same output

A companion product (the four right-hand-column article numbers above) that produces the *same*
kind of application file, but built by picking database → register → keys → fields from
successive on-screen menus with function keys, instead of writing the text file by hand — "With
only 6 keystrokes your application is ready to run." [PI] The generated file can then be run
directly by UNIQUE II, or hand-edited with standard ND editors (PED, NOTIS-WP).

## A second worked example, from the ND-211202 "DIALOGUE-1" sheet — confirms the syntax, minor variations

Source: [../../Product-Info/ND-211202-A1-EN.md](../../Product-Info/ND-211202-A1-EN.md), verbatim. [PI]

```
start-fields
database-name = ORDRBASE
table-name = CUSTOMER
field-name = "CUSTOM" key
field-name = "NAME" letter-type(UPPER) alternative-key
field-name = "SHORTNM" letter-type(UPPER) alternative-key
field-name = "ADDRESS1"
field-name = "ADDRESS2"
field-name = "ADDRESS3"
field-name = "TELNO"
field-name = "CONTPERS" letter-type(UPPER) alternative-key
field-name = "PAYCONID"
end-fields
```
Same shape as the §"Building a UI" example above, with two vocabulary differences worth noting
(both sheets describe the same real product, so treat these as **synonyms**, not a version
difference confirmed either way): `table-name`/`field-name` here vs. `register-name`/`field` in
the other sheet's example. This second sheet also documents the interactive form-layout
convention: each field position is marked with a `°` character when designing the form on-screen
(vs. this catalog's other example's `A` placeholder character) — again, likely just two different
scans/eras of the same underlying convention, not confirmed which is authoritative.

## Documentation
- Program Description (PD-sheet): not located for any of the 8 article numbers
- Product Information (PI-sheet): [../../Product-Info/ND-210729-A1-EN.md](../../Product-Info/ND-210729-A1-EN.md), [../../Product-Info/ND-211202-A1-EN.md](../../Product-Info/ND-211202-A1-EN.md)
- Manual(s): `ND-60.206.2` UNIQUE II User Manual · `ND-60.210.2` UNIQUE II Programmer Manual ·
  `ND-60.240.1` UNIQUE UNIQUICK User Manual (none located in this repo)

## Provenance
PI sheet only — no PD sheet or floppy image located for this product in this repo's archive or
the floppy reference catalog consulted.

---
**Parent:** [../README.md](../README.md) (Software catalog)
