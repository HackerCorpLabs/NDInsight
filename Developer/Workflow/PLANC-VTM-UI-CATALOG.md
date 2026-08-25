# Screen-Oriented UI on ND Hardware — the Full Catalog

**Every documented way to build a screen UI (forms, boxes, editable fields) on SINTRAN III, and
how they relate to each other and to VTM underneath.**

This is the survey; each entry links to the real, sourced detail. Everything below is either a
byte-verified decode of a real floppy/file already in this repo, a direct manual/PI-sheet quote,
or is explicitly marked as unconfirmed. Nothing here is invented to fill a gap.

---

## 1. The common substrate: VTM

Every system in this catalog sits on **VTM**, SINTRAN's terminal-independence layer — a
system-wide `DDBTABLES:VTM` (or per-version `DDBTABLES-n:VTM`) file maps a numeric "terminal type"
to the real escape-sequence dialect of the physical terminal. VTM itself has no published
call-level manual (listed "Internal" in `ND-20034-1-EN`'s library list). Full detail:
[VTM-TERMINAL-INTERFACES.md](VTM-TERMINAL-INTERFACES.md).

**Confirmed dependency, a new citation not previously in that doc:**

> "If the file (SYS)DDTABLES-Exx:VTM is not available (does not exist, no access, ambiguous
> etc.), UNIQUE gets suspended without giving any error message."
> — `Installation/Installation-Description/ND-210731-2-EN.md:194`

This confirms UNIQUE (§4 below) depends on the exact same `DDBTABLES-n:VTM` mechanism as
PLANC-SCREEN-H and COBOL's screen handling — every entry in this catalog is a layer over the same
one file, not five independent terminal-handling schemes.

## 2. PLANC-SCREEN-H — the PLANC-native option

**Full guide:** [../Languages/Application/PLANC-UI-VTM-GUIDE.md](../Languages/Application/PLANC-UI-VTM-GUIDE.md)
**Floppy decode:** [Installation/Software/ND-PLANC-SCREEN-H/README.md](../../Installation/Software/ND-PLANC-SCREEN-H/README.md)

A real, byte-verified PLANC callable library (`SCREEN:SYMB`): `frame`/`fullbar`/`sparsebar` draw
boxes and bars; `bytdis`/`bytacc`, `intdis`/`intacc`, `realdis`/`realacc` display and edit typed
fields; `blankscreen`/`blankarea`/`resetscreen` clear the screen. Screens can be hand-coded call by
call, or declared in a `.PICT` file (`%HEADING`/`%CONTROL`/`%DEFINITIONS`/`%ATTRIBUTES`) and
compiled by `PLANC-GEN-A00:PROG` into a generated PLANC source file (`:PGEN`) that the program
`$INCLUDE`s and calls directly — this build pipeline is now fully decoded, see the linked guide
§6.

**No ND article number exists for this product anywhere searched** — confirmed against both this
repo and the source floppy archive's own metadata (`products/PLANC-SCREEN.yaml` has no article
number, no PI/PD doc IDs).

## 3. NSHS — NORD Screen Handling System (`ND-10013`)

**Source:** [Installation/Software/ND-10013/README.md](../../Installation/Software/ND-10013/README.md), PI sheet [ND-10013-A2-EN](../../Installation/Product-Info/ND-10013-A2-EN.md).

Two modules: an interactive **Screen Picture Maintenance Program** (create/edit/test screen
"pictures" — leading text plus input-field descriptions) and a **Screen Picture Handling
Library** (routines a program calls to read/write records through a saved picture). Field-level
features go well beyond PLANC-SCREEN-H's plain byte/integer/real triad: numeric/decimal/
alphabetic/alphanumeric/bank-account/social-security-number field types, check-digit verification
(mod 10/11), date controls, field accumulation, and user-supplied control procedures.

**Callable from FORTRAN, BASIC, COBOL, RPG II — PLANC is not in this list.** This is real evidence
against (though not a disproof of) the "NSHS and PLANC-SCREEN-H are the same product" theory
raised in the PLANC-SCREEN-H README.

**Floppy contents** (parts 2-3 of a 3-part set; part 1, likely the installer, was never
imaged/found): `SCREEN-1BANK-K:BRF`, `SCREEN-2BANK-K:BRF`, `SCREEN-1REEN-K:BRF` (three runtime
variants — 1-bank, 2-bank, and reentrant), `SCREEN-COPY-K:BPUN` (a utility), `SCREEN-DEMO-K:SYMB`
and `SCREEN-UCONT-K:SYMB` (source), `SCREEN-SYMB-K:SYMB` (symbol table). **No picture-file source
was decoded from this floppy** — there is no NSHS equivalent of PLANC-SCREEN-H's `SUM:PICT` to
compare byte-for-byte. The "NSHS's picture concept looks like PLANC-SCREEN-H's `.PICT`" claim in
earlier docs is inference from the PI sheet's prose only, not a file-format match.

**Open, and likely to stay open**: identity vs. PLANC-SCREEN-H, and identity vs. FOCUS (§5) —
both are "maintenance program + callable library, same handful of caller languages" products, and
nothing found so far distinguishes NSHS from FOCUS beyond their names and product numbers.

## 4. UNIQUE / UNIQUICK — the 4GL form interpreter

**Source:** worked example in [Installation/Software/ND-10730/README.md:39-86](../../Installation/Software/ND-10730/README.md), product family in [Installation/Software/ND-210729/README.md](../../Installation/Software/ND-210729/README.md).

**UNIQUE II is not a callable library — it's a 4th-generation-language *interpreter*.** You write
a plain text file and UNIQUE runs it directly; there is no PLANC/FORTRAN/COBOL source at all in
the normal path. Real, verbatim shipped example, `CUSTOMER-REG-A00:UNIQ`:

```
start-form
  Customer: ^NNNNNN
  Name:     ^AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA
  ...
end-form
start-fields
data-base-name=CUSTBASE
register-name=CUSTOMER
          field-name=CUSTOMER        key
          field-name=NAME            alternative-key     update
          ...
end-fields
```

`^N` and `^A` are the real field-placeholder codes — numeric and alphanumeric respectively. This
resolves an ambiguity a PI-sheet-only description had left open. `key`/`alternative-key`/`update`
in `start-fields` mark each field's role against the named database/register
(`data-base-name`/`register-name`).

A companion product, **UNIQUICK**, builds the same `.UNIQ` text file interactively via menus and
function keys instead of hand-editing it — the generator/interpreter split mirrors
PLANC-GEN/PLANC-SCREEN-H and NSHS's maintenance-program/library split, but for a form-interpreter
model instead of a compiled/linked-library model. Full product-family article-number table (8
variants across SIBAS/ISAM x ND-100/ND-500, plus a Runtime-only tier) is in the
[ND-210729](../../Installation/Software/ND-210729/README.md) README.

**Boxes**: `Installation/Product-Info/ND-211202-A1-EN.md:40` notes UNIQUE forms can include "ND-NOTIS
graphics (S-format)... to include boxes or special symbols" — a **fifth, distinct** box-drawing
mechanism in this catalog, mentioned exactly once and not documented further anywhere found.

## 5. FOCUS — the ND-100 forms system (`ND-10188`, later `ND-210188`; ND-500 variant is `ND-10341`)

**Primary source, now decoded:** [Installation/Software/ND-10188/README.md](../../Installation/Software/ND-10188/README.md)
— a real PI sheet already in this repo (`Installation/Product-Info/ND-10188-A2-EN.md`), plus a
real 4-disk floppy set. (An ND-500 variant, product `ND-10341`, is separately described only
secondhand in `Reference-Manuals/500/ND-80.001.2 TO NEC CCIS Data Processing Subsystem.md:7758-7845`;
that manual, `ND-60.137.04`, is not in this repo.)

FOCUS = an "ND FORMS maintenance program" (interactive form editor) plus an "ND FORMS/Runtime
handling library" (FORTRAN/BASIC/COBOL-callable, **both reentrant and non-reentrant variants
ship**). Near-identical shape to NSHS — same two-module split, same caller-language set minus RPG
II.

**Three things the real floppy adds beyond the PI sheet:**
- **FOCUS is itself built in PLANC** — its floppies bundle `PLANC-1BANK-E:BRF`/`PLANC-2BANK-E:BRF`
  as a build dependency.
- **It carries its own compiled VTM bridge** (`FC-MVTM-1CODE:BRF`/`-2CODE:BRF`) — the same
  "splice a bridge module into the runtime" pattern as COBOL's `VTM-BRIDGE-*` scripts, but shipped
  pre-compiled rather than as a patch script.
- **It ships its own private VTM terminal-table set** (`VTM-1B-ARRAY-D-C:BRF`,
  `DDBTABLES-D-C:VTM`) instead of relying purely on the system-wide `DDBTABLES:VTM`.

**Open**: relationship to NSHS. Nothing found distinguishes the two beyond product number and
name — both are now confirmed as real, independently-decoded products with near-identical PI-sheet
wording, not one being a stub description of the other.

## 6. COBOL's built-in screen handling

**Source:** [../Languages/Application/COBOL-DEVELOPER-GUIDE.md](../Languages/Application/COBOL-DEVELOPER-GUIDE.md), detailed in [VTM-TERMINAL-INTERFACES.md §4](VTM-TERMINAL-INTERFACES.md#4-how-an-application-actually-presents-ui-through-vtm--the-cobol-example).

Not a separate product — COBOL's own `DISPLAY`/`ACCEPT`/`BLANK` screen-section statements are
built directly on VTM (the ND-10176H00 floppy's `VTM-BRIDGE-*:MODE` scripts splice a VTM bridge
module into the COBOL runtime at install time — a binary-level integration, not a documented call
convention). The COBOL PI sheet itself names FOCUS as the explicit alternative: "The programmer
may choose to use either the screen handling system (incorporated in COBOL) or ... FOCUS."

## 7. Comparison table

| System | Model | Callable from | Box drawing | Picture/form format | ND article # |
|---|---|---|---|---|---|
| VTM | terminal-config layer, no manual | (everything below, indirectly) | n/a (substrate only) | n/a | — |
| **PLANC-SCREEN-H** | callable library + `.PICT`->PLANC generator | PLANC only | `frame`/`fullbar`/`sparsebar` | `.PICT` (4 sections) | none found |
| **NSHS** | callable library + interactive maintenance program | FORTRAN, BASIC, COBOL, RPG II | not confirmed | "picture" (format not decoded) | `ND-10013` |
| **UNIQUE/UNIQUICK** | 4GL interpreter (no source language at all) | n/a — its own `.UNIQ` text format | NOTIS S-format graphics (1 mention only) | `start-form`/`start-fields` | `ND-210729` family |
| **FOCUS** | callable library + interactive maintenance program | FORTRAN, BASIC, COBOL | not confirmed | not confirmed (no manual in repo) | `ND-10188` (ND-500 variant `ND-10341`) |
| **COBOL built-in** | language statements, VTM-bridged at install time | COBOL only | not applicable (form drawing is via DISPLAY literals) | inline `DISPLAY`/`ACCEPT`/`BLANK` statements | n/a (part of COBOL) |

## 8. What is still open across this whole catalog

- PLANC-SCREEN-H vs. NSHS: unresolved, evidence leans slightly toward "different products"
  (caller-language lists don't overlap on PLANC).
- NSHS vs. FOCUS: never previously asked; both have the exact same two-module shape and caller
  language set minus RPG II. Genuinely unclear whether these are sibling products, one being an
  ND-500 port of the other, or unrelated designs that converged on the same shape.
- No full manual (`ND-60.137`), only a PI sheet, exists for FOCUS in this repo; no manual at all
  for PLANC-GEN or PLANC-SCREEN-H.
- NSHS's actual picture-file format was never decoded (part 1 of its floppy set, which likely had
  the installer and description file, was never imaged).
- The NOTIS S-format box-drawing mechanism used inside UNIQUE forms is a single-line mention with
  nothing else to expand on.

---

## See Also

- **[VTM-TERMINAL-INTERFACES.md](VTM-TERMINAL-INTERFACES.md)** — the VTM substrate underneath every system above.
- **[../Languages/Application/PLANC-UI-VTM-GUIDE.md](../Languages/Application/PLANC-UI-VTM-GUIDE.md)** — the PLANC-specific how-to.
- **[Installation/Software/ND-10013/README.md](../../Installation/Software/ND-10013/README.md)** — NSHS floppy decode.
- **[Installation/Software/ND-210729/README.md](../../Installation/Software/ND-210729/README.md)** — UNIQUE/UNIQUICK product family.
- **[Installation/Software/ND-10730/README.md](../../Installation/Software/ND-10730/README.md)** — the real `CUSTOMER-REG-A00:UNIQ` example.
