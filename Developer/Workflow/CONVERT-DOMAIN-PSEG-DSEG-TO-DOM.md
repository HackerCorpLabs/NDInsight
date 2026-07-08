# Converting an Old-Format ND-500 Domain (PSEG/DSEG/LINK) to a Domain File (:DOM)

**Purpose:** Step-by-step procedure for converting a legacy ND-500 domain, built by
the Linkage-Loader and stored as a triple of files (`:PSEG`, `:DSEG`, `:LINK`) plus
an entry in `DESCRIPTION-FILE:DESC`, into a single self-contained domain file
(`:DOM`) using the **CONVERT-DOMAIN** program (ND-211229).

**Sources (all claims verified against these documents):**

- [ND Linker User Guide and Reference Manual (ND-860289-2-EN)](../../Reference-Manuals/ND-860289-2-EN%20ND%20Linker%20User%20Guide%20and%20Reference%20Manual.md) - Appendix F "The CONVERT-DOMAIN Program" (pages 258-261)
- [SINTRAN III System Supervisor (ND-30.003.007)](../../Operations/SINTRAN/ND-30.003.007%20EN%20SINTRAN%20III%20System%20Supervisor.md) - pages 120-122 (domain file organization)
- [SINTRAN III L-Version Release Information (ND-860230-6-EN)](../../SINTRAN/Release-Documentation/ND-860230-6-EN%20Sintran%20III%20-%20Release%20Information%20-%20L-Version.md) - product/version requirements
- [LINKING-GUIDE-500-DEEP-DIVE.md](LINKING-GUIDE-500-DEEP-DIVE.md) - sections 3.4 and 9.1

---

## 1. Background: The Two Domain Formats

### 1.1 Old format (Linkage-Loader, pre-K/L SINTRAN)

A domain is stored as a *triple* of files per segment set, plus a shared
description file:

```
User directory:
  MYPROG:PSEG            - program segment(s): pure ND-500 machine code
  MYPROG:DSEG            - data segment(s): initialized data and variables
  MYPROG:LINK            - link information: how the segments are linked
                           together (symbol table, entry points)
  DESCRIPTION-FILE:DESC  - one per user; describes ALL domains owned by
                           that user (file locations, domain metadata)
```

(Source: System Supervisor manual, page 120-121, Figure 27.)

A domain can consist of up to 32 program segments and 32 data segments.
Standard (system) domains keep the description-file information on a
system-included segment on SEGFILE0 (segment number 20B) instead.

### 1.2 New format (ND Linker, SINTRAN K and later)

The whole domain lives in ONE file of type `:DOM` (shared library segments in
`:SEG` files). The `:DOM` file is self-contained: header, slave segments, link
information area, and debug information area. No `DESCRIPTION-FILE:DESC`
dependency.

(Source: K-version release information ND-60230-5-EN, "A domain is stored on a
single file (of type :DOM) instead of a triple of files for each segment".)

---

## 2. Prerequisites

| Requirement | Detail |
|-------------|--------|
| CONVERT-DOMAIN program | Product **ND-211229** (delivered with SINTRAN III L; "available during the transition period") |
| ND Linker | Product **ND-211224**, version A or later (to verify/use the result) |
| Complete old-format domain | ALL FOUR parts must exist: `:PSEG`, `:DSEG`, `:LINK`, and the domain's entry in `DESCRIPTION-FILE:DESC` |
| Access | Read access to all source domain files; write/directory access on the destination user area |

**IMPORTANT - the :LINK file is mandatory.** There is no documented parameter
or mode of CONVERT-DOMAIN that skips it. The System Supervisor manual (page
121) states: "If one of these files is deleted, it is no longer possible to
run the domain, and it has to be reloaded." If you are missing the `:LINK`
file (or the DESC entry), CONVERT-DOMAIN cannot be used - see section 7.

---

## 3. Command Syntax

CONVERT-DOMAIN is started as an ND-500 domain from the SINTRAN prompt:

```
@ND CONVERT-DOMAIN <Destination domain>
                   <Source domain>
                   <Include linked segment(s) (Y,N)>
                   <Display progress information (Yes,No)>
                   <Force free segment number(s)>
```

Parameters (from ND Linker manual, Appendix F, page 258):

| # | Parameter | Mandatory | Default | Meaning |
|---|-----------|-----------|---------|---------|
| 1 | Destination domain | yes | same name as source | Name of the new `:DOM` file. Can be on another user: `(directory:user)file-name`. New file: wrap the whole spec in double quotes. |
| 2 | Source domain | yes | (none) | Old-format domain name. Do **NOT** specify a file type here (no `:PSEG` etc.). |
| 3 | Include linked segment(s) (Y,N) | no | - | YES = also convert segments this domain links to, copying them onto the destination user (useful when converting onto a floppy). NO = the new domain links to the existing free segment. |
| 4 | Display progress information | no | YES | Progress output to the terminal. |
| 5+ | Force free segment number(s) | no | - | List of segment numbers to be written to SEPARATE `:SEG` files instead of slave segments inside the `:DOM`. Ranges allowed: `3-6`, `3..6`, or `3:6`. Use when another domain must link to these segments. |

Default shortcut for parameter 1: press space, type two commas (`,,`), or type
`$` to accept the source name. To derive a modified name, `$` expands to the
source domain name, e.g. entering `new-$` for source `ACCOUNTS-DOMAIN` gives
destination `NEW-ACCOUNTS-DOMAIN`.

---

## 4. Step-by-Step Procedure

### Step 1: Verify the source files exist

```
@LIST-FILES MYPROG,,
```

You should see `MYPROG:PSEG`, `MYPROG:DSEG`, `MYPROG:LINK`, and the user must
have a `DESCRIPTION-FILE:DESC`. Optionally confirm the domain is registered:

```
@ND-500-MONITOR
N500: LINKAGE-LOADER
N11: LIST-DOMAIN
      (MYPROG should appear in the list)
N11: EXIT
N500: EXIT
```

### Step 2: Run the conversion

Simplest form - convert in place, same name, all defaults:

```
@ND CONVERT-DOMAIN "MYPROG" MYPROG
```

Notes:

- Double quotes around the destination create a NEW file (standard SINTRAN
  new-file convention; the quotes must wrap the ENTIRE file spec, including
  any `(directory:user)` prefix).
- No file type on the source parameter.
- With progress display on (the default) you will see output like:

```
>> Converting free segment number 3 <<
>> Converting free segment number 1 <<
>> Finished <<
```

Real example from the ND Linker manual (page 260) - converting the NOTIS-WP
domain from user DOMAINS onto the current user, including linked segments,
and forcing segments 1 through 3 out as free `:SEG` files so that other
applications can still link to them:

```
@ND CONVERT-DOMAIN "(\)WP-500-NO8" (DOMAINS)WP-500-NO8 Y Y 1:3
```

(The manual's OCR shows the domain name as `WP-5Q0-NQ8`; UNVERIFIED exact
spelling, shown here as `WP-500-NO8`.)

### Step 3: Interactive mode (alternative)

Starting the program with no parameters enters a command processor with the
prompt `CONV:`:

```
@ND CONVERT-DOMAIN
CONV: CONVERT-DOMAIN
Destination domain: "MYPROG"
Source domain: MYPROG
...
```

Help facilities: SHIFT+HELP lists available commands; type a command name and
press HELP for its description; HELP on an empty line describes the program.
Additional help TOPICS exist (e.g. `OLD-DOMAIN-FORMAT`). To avoid the command
processor, give the command and all parameters on one `@ND CONVERT-DOMAIN`
line.

### Step 4: Verify the result

```
@LINKER
NDL: LIST-STATUS MYPROG
      (shows the domain's segments, entry point, trap block)
NDL: EXIT
```

Then test-run it:

```
@ND MYPROG
```

### Step 5: Clean up (only after successful verification)

The old files are left untouched by the conversion. Once the `:DOM` runs
correctly:

```
@DELETE-FILE MYPROG:PSEG
@DELETE-FILE MYPROG:DSEG
@DELETE-FILE MYPROG:LINK
```

The DESC entry can be removed with the Linkage-Loader (`DELETE-DOMAIN`
updates the description file) - but note DELETE-DOMAIN deletes the domain's
files too, so do NOT use it after you have already deleted the files above;
UNVERIFIED whether DELETE-DOMAIN tolerates already-deleted segment files.

---

## 5. What the Conversion Does

1. Reads `MYPROG:PSEG` - extracts all program segments.
2. Reads `MYPROG:DSEG` - extracts all data segments.
3. Reads `MYPROG:LINK` - extracts symbol table and entry points.
4. Reads the DESC entry - extracts domain metadata (trap block, entry point,
   segment info).
5. Creates the `:DOM` file header and writes the segments as slave segments.
6. Copies debug and link information into the `:DOM` file's reserved areas.

Segment handling rules (ND Linker manual, pages 259-260):

- Segments *belonging to* the source domain become **slave segments** inside
  the destination `:DOM`.
- Segments *linked to* become **free segments** (`:SEG` files).
- If a free segment file of the right name already exists on the destination
  user, it is reused (matched by FILE NAME ONLY - beware of a same-named but
  different segment being silently linked instead).
- If the linked segment lives on another user (say LIB) while you convert
  from user CURR to user DST, the program tries to create `LINKSEG:SEG` on
  LIB first, then on DST, then on CURR; if all fail it stops with an error.

Properties of the result:

- One-way: a `:DOM` cannot be converted back to PSEG/DSEG/LINK.
- No recompilation or reloading needed; execution is identical.
- The new domain no longer depends on `DESCRIPTION-FILE:DESC`.

---

## 6. Required Product Versions (SINTRAN III L)

From the L-version release information:

| Product | Version | Role |
|---------|---------|------|
| CONVERT-DOMAIN (ND-211229) | (L release) | The conversion program itself |
| ND-LINKER (ND-211224) | A | Handles the new `:DOM` files |
| SYMBOLIC-DEBUGGER (ND-210336) | H | Needed to debug domains stored on `:DOM` files |
| LED-DEBUGGER (ND-211157) | B02 | Needed to handle `:DOM`-based domains |

---

## 7. If You Are Missing the :LINK File (or the DESC Entry)

There is **no documented way** to run CONVERT-DOMAIN without the complete
file set. The documented alternatives are:

1. **Reload the domain from the original NRF object files** with the
   Linkage-Loader (old format) or, better, load it directly with the ND
   Linker to produce a `:DOM` natively. See
   [LINKING-GUIDE-500-DEEP-DIVE.md](LINKING-GUIDE-500-DEEP-DIVE.md).
2. **Fetch an intact copy.** If the domain still exists complete on the
   original machine or user area, run CONVERT-DOMAIN there and copy the
   resulting `:DOM` file (a single, self-contained file - easy to move).
3. **Standard/system domains:** their description-file information lives on
   SEGFILE0 segment 20B rather than in a user DESC file (System Supervisor
   manual, page 121). No conversion tool for these is documented in this
   repository.

UNVERIFIED / not documented: reconstructing a `:LINK` file by hand, or
hand-building a `:DOM` from raw PSEG/DSEG contents. The `:DOM` layout is
partially described in the ND Linker manual (domain file header, link
information area sizes, Appendix G "Format of a link information entry"),
so this may be technically possible, but no procedure exists in any manual
in this repository.

---

## 8. Quick Reference Card

```
Verify sources:   @LIST-FILES MYPROG,,
Convert:          @ND CONVERT-DOMAIN "MYPROG" MYPROG
  (new name)      @ND CONVERT-DOMAIN "NEW-MYPROG" MYPROG
  (other user)    @ND CONVERT-DOMAIN "(PACK:DST)MYPROG" (SRC)MYPROG Y Y
  (free segs)     @ND CONVERT-DOMAIN "MYPROG" MYPROG Y Y 1:3
Verify result:    @LINKER  ->  NDL: LIST-STATUS MYPROG
Test run:         @ND MYPROG
Clean up:         @DELETE-FILE MYPROG:PSEG / :DSEG / :LINK
```
