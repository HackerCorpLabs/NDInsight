# Norsk Data Numbering — Reference

> **Source:** *ND-40.004.7 EN Documentation Catalogue* (Norsk Data), §1.1.1.1–1.1.1.4 and
> §1.2. All statements here are **transcribed verbatim** from that manual. Where this repo's
> tooling (ndfloppy) or NDWIKI use codes that the manual does **not** define, that is called
> out explicitly as **non-official**. No assumptions.

---

## 1. There are (at least) TWO different numbers — do not conflate them

§1.2 (item 1) states a manual identifies "the product(s) documented in the manual, **their ND
or PD number and version number**." So a product has:

- an **ND number** (the product/article number, e.g. `ND-10022`, `ND-210309`), and
- a **PD number** (Program Description), and
- a **version number**.

These are **distinct from the documentation (manual) number** described below. **ND-40.004.7
formally defines only the documentation-number format.** It does **not** define the format of
the product ND-number or the floppy volume-name scheme — so any decomposition of those (e.g.
the `-XX-` field or trailing density letter on floppy volume names) is **our inference, not an
ND-40.004.7 fact.**

---

## 2. Documentation (manual) numbering — OFFICIAL (§1.1.1.1)

> "The numbering system for Norsk Data documentation consists of three numbers, as follows:"

```
ND-XX.YYY.Z LA
   │   │   │  └─ Language version suffix
   │   │   └──── Version number
   │   └──────── Sequential identification number
   └──────────── (XX) Subject-matter code
```

### Subject-matter codes (verbatim table)

**Hardware**

| Code | Description |
|------|-------------|
| 02 | PIOC hardware |
| 05 | NORD-50, ND-500 CPUs |
| 06 | NORD-10, NORD-10/S, ND-100 CPUs |
| 11 | Data storage equipment |
| 12 | I/O interfaces |
| 13 | Miscellaneous hardware |

**Mixed hardware/software/other**

| Code | Description |
|------|-------------|
| 30 | Operating, diagnostic, maintenance |
| 40 | Miscellaneous |

**Software**

| Code | Description |
|------|-------------|
| 60 | General software |
| 61 | NORTEXT software |
| 63 | NOTIS software |
| 64 | SINTRAN IV |
| 65 | Technovision |

**Quick reference**

| Code | Description |
|------|-------------|
| 99 | Reference cards, posters, etc. |

> The §1.1.1.1 summary table lists software codes **60, 61, 63, 64, 65** — but this table is
> **NOT complete**. Code **62** is proven official because the catalogue itself lists manual
> **ND-62.008 (NORD-50 Test System)** (§ manual list), even though 62 is absent from the table
> above. So treat the table as a summary, not the full code set. See §5.

## 2b. The September 1988 change — `ND-8xxyyy.zz` (NOT in ND-40.004.7)

> **Source:** the NDDOC archive's `document-description.md`, **corroborated by actual document
> filenames** across the archive. This change postdates our ~1985 ND-40.004.7 copy, which is
> why that catalogue cannot mention it.

Per `document-description.md`:
> "The numbering system was changed in September 1988 by adding an **8** in front of the first
> digit and **dropping the first dot**. Numbers of the form `ND-xx.yyy.zz` turned into
> `ND-8xxyyy.zz`. The new numbering system was used on all new manuals and on older ones when
> they were updated or reprinted. After the number there is an optional two-letter language
> version and an optional **p** for preliminary versions."

So: `ND-30.003.07` → `ND-830003-07`; the subject code `xx` is preserved as the digits after the
`8`. **Confirmed empirically** by filenames in the archive [obs]:
`30.003`→`ND-830005`/`ND-830008` · `20.x`→`ND-820023` · `05.x`→`ND-805013` · `14.x`→`ND-814009`
· `99.x`→`ND-899159` · plus `ND-860230`, `ND-868208`, `ND-880001`, `ND-891092`.

**Practical rule for parsing a document number:** if it matches `ND-8XXNNN[-.]rr`, the subject
code is the two digits **XX** (after the leading 8); if it matches `ND-XX.YYY.ZZ`, the subject
code is **XX**. Both forms denote the same code space.

## 3. Revisions and Versions (§1.1.1.3)
- **Revisions** = inserted replacement pages for small corrections; indicated by **letters**
  (`REV.A`, `REV.B`).
- **New versions** = a complete replacement document incorporating all prior revisions;
  indicated by a **number** (e.g. "version 7"). The version number is the `Z` in `ND-XX.YYY.Z`.

## 4. Language version suffix (§1.1.1.4)
Two-letter uppercase abbreviation, always the **last** part of the document number:

| Abbr. | Language | | Abbr. | Language |
|-------|----------|-|-------|----------|
| EN | English | | FR | French |
| NO | Norwegian | | DA | Danish |
| SW | Swedish | | | |
| GE | German | | | |

## 4b. Color codes (§1.1.1.2)
| Color | Subject |
|-------|---------|
| Red | Hardware |
| Yellow | Operator/supervisor documentation |
| Blue | General software |
| Green | NOTIS office automation software |

---

## 5. This catalogue edition is OLD — codes beyond it are "unknown", not "invalid"

**Edition/date:** this is *ND-40.004 version **7***; the newest manual dates inside it are
~**July 1985**, and it references "SINTRAN III version J or later." The product list we catalog
extends well past that (TCP/IP ~1992, NOTIS-MAIL X.400, OWS, DSS, NUCLEUS/DOMINO kits, ND-5000),
so **this catalogue cannot describe the later products or any subject codes added after ~1985.**

**Status of the codes** — updated with evidence from the NDDOC archive (real documents sorted by
subject code; see [Software/research/NDDOC-INVENTORY.md](Software/research/NDDOC-INVENTORY.md) §2):

| Code(s) | Status | Basis |
|---------|--------|-------|
| 60, 61, 63, 64, 65 | **Official** | listed in §1.1.1.1 table |
| 62 | **Confirmed real** | §1.1.1.1 table omits it, but catalogue lists `ND-62.008`; NDDOC has 19 docs in /62 (`ND-62.009` etc.) |
| 68, 80, 65 | **Confirmed by real documents** | NDDOC folders populated: /68 (`ND-868208`, `ND-868210`), /80 (`ND-880001-01/02`), /65 (`ND-865-A1`) |
| 67, 61, 23 | **Defined but unused** | category exists in the taxonomy but **0 documents** found in NDDOC |
| 67 | **Status of officialness still open** | no document found to confirm; may be a later/tooling code |

So codes **62, 68, 80** that were earlier flagged "unconfirmed" are now backed by **actual ND
documents bearing those numbers** — they are real ND subject codes (the ~1985 §1.1.1.1 table was
simply a summary and predated some). Only **67** remains unconfirmed (defined, no document).

**Consequence for the catalog:** in [Software/README.md](Software/README.md), tags
**60/61/62/63/64/65/68/80** are now backed by real documents; only **67** is unconfirmed. Do not
present any of these as "non-official". (Note these are *document* subject codes; applying them
per *product* is still an approximation.)

---

## 6. What this reference does NOT establish
- The format/meaning of the **product (ND-) article number** (`ND-10022`, `ND-210309`).
- The meaning of floppy **volume-name** fields (`-XX-`, trailing `D`). Those remain inferences
  documented (and marked) in [OS/floppy-contents/README.md](OS/floppy-contents/README.md).
- The **PD-number** format.
These are not defined in ND-40.004.7 §1.1.1.x; treat them as open until a primary source is found.

---
**Parent:** [README.md](README.md)
