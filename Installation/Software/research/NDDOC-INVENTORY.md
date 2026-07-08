# NDDOC Document Archive — Inventory & Findings (TEMPORARY)

> **Status: TEMPORARY WORKING NOTES.** Aggregated from automated analysis of a local Norsk
> Data document archive (categorized PDFs + metadata). Classification is mostly from
> **filenames** (descriptive) plus the archive's own `document-category.json` /
> `categorization-summary.md`; a handful of PDFs were opened to characterize document
> structure. `[obs]` = observed; `UNVERIFIED` = inferred, not confirmed by opening the file.
> Read-only analysis; nothing was modified.

---

## 1. Archive shape

- `ND/` — PDFs pre-sorted into folders by **ND subject code** (01–99 + `uncategorized`), with
  `document-category.json`, `categorization-summary.md`, `document-description.md`.
  Summary reports **722 files processed**.
- `Other/` — ~430 mixed PDFs (ND + third-party), unsorted.
- `html/`, `txt/` — OCR/converted renditions.

> Many files are **duplicate OCR variants** of one document (`-Gandalf-OCR`, `-Tingo`,
> `-Tingo-OCR`, `-Adobe-OCR`, `_ocr`, `-Bitsavers`). Distinct document count ≪ file count.

## 2. Subject-code population (from `categorization-summary.md` + folder listings)

| Group | Code | Subject | Docs | Note |
|-------|------|---------|------|------|
| HW | 01 | NORD-1 hardware | 6 | |
| HW | 02 | PIOC hardware | 4 | |
| HW | 05 | NORD-50/ND-500/ND-5000 CPUs | 23 | |
| HW | 06 | NORD-10/ND-100/110/120 CPUs | 61 | incl. ND-120, BUTTERFLY-110 |
| HW | 07 | NORD-12 | 1 | |
| HW | 10 | MPM hardware *(per metadata)* | 44 | **actually** NORD Software Library + test programs + subsystem packages (mislabeled) [obs] |
| HW | 11 | Data storage equipment | 26 | disk/floppy controllers |
| HW | 12 | I/O interfaces | 46 | incl. HDLC, SCSI, Ethernet, ND-120 internals |
| HW | 13 | Misc hardware | 11 | incl. `ND-13.028.1 Site preparation` |
| HW | 14 | Domino interfaces | 2 | |
| Mixed | 20 | Internal System Documentation | 10 | SINTRAN III-VSX System Docs, DOMINO/NUCLEUS |
| Mixed | 23 | OpenLAN | 0 | empty |
| Mixed | 30 | Operating, diagnostic, maintenance | 70 | **richest OS/ops source** (see §4) |
| Mixed | 40 | Documentation catalogues | 6 | incl. ND-40.004 catalogue revisions |
| Mixed | 50 | HW/SW system description | 2 | |
| Mixed | 70 | Documentation packages | 1 | |
| SW | 60 | General software | ~291 | the big one (see §3) |
| SW | 61 | NORTEXT | 0 | empty |
| SW | 62 | Test & Verification | 19 | e.g. `ND-62.009 Test Program Descriptions`, `ND-62.008 NORD-50 Test System` |
| SW | 63 | NOTIS | 40 | NOTIS-WP/IR/TF/ID/RG/DS/CALC/PM/DRAW |
| SW | 65 | TECH/Technovision | 1 | `ND-865-A1-EN` |
| SW | 67 | Misc software | 0 | empty |
| SW | 68 | NORCCIS | 2 | `ND-868208 Service Handbook Addendum 1993`, `ND-868210` |
| SW | 80 | Utility software | 2 | `ND-880001-01/-02` |
| Ref | 99 | Reference cards | 15 | operator-panel, instant-codes, NOTIS ref cards |
| — | uncategorized | no parseable category ID | 38 | mostly real ND docs w/ non-coded IDs (§6) |

## 3. Folder 60 (General software) — products covered [obs from filenames]
Languages (FORTRAN 60.011/074/145, NPL 60.047, BASIC 60.040/071, PASCAL 60.086/124, COBOL
60.089/144, SIMULA 60.092, PLANC 60.117/860117, C 60.214/860251, MAC 60.009/096, ND-500 asm
60.075/113); loaders/linkers/debuggers (BRL 60.030, RT-Loader 60.051/072, ND Rel. Loader
60.066, BRF-LINKER 60.196, Sym. Debugger 60.158, Trace 60.046, BRF-Editor 60.085); SINTRAN III
(Users Guide 60.050, System Doc 60.062/112, Intro 60.125, Ref 60.128/860228, TSS-Batch 60.132,
RT Guide 60.133, Comm Guide 60.134, Utilities 60.151, Monitor Calls 860228); file/data (NORD FS
60.052/122, SIBAS 60.057/078/097/127, ISAM 60.108, ACCESS 60.153, SORT-MERGE 60.123/146/236,
File Manager 60.215, TPS 60.105/111); comms (NORDNET 60.081, SCANNET 60.087, COSMOS
60.163/164/227/860284, Ethernet 60197, OpenLAN 860353); UI (Screen Handling 60.088, FOCUS
60.137, PED 60.121/148, QED 60.031, User Environment 60.194/261/860320); other (NORD-OPS 60.026,
TSS 60.039, BACKUP 60.250, ABM 60.203, NDIX 860328).

## 4. Installation / OS-relevant document sources (priority)
- **SINTRAN III Release Information series** (folder 60): `ND-60.230.01` J · `ND-60.230.2`/`ND-60230-5` K · `ND-860230-6` L · `ND-860230-7A` M · `ND-860230-8` N. *(Contain version load/upgrade procedures.)*
- **Folder 30** (operating/diagnostic/maintenance): `ND-30.003.x` SINTRAN III System Supervisor (rev 04/05/06/06A/007), `ND-30.053.01 How to order it`, `ND-30.049.1 Tuning Guide`, Satellite/Butterfly **Installation** guides, `ND-30.005.x`/`ND-830005-x` Test Program Descriptions, hardware maintenance manuals, `ND-30.040.x TELEFIX`.
- **Folder 20**: `ND-820023-1 SINTRAN III-VSX System Documentation`, `ND-820026.1 DOMINO/NUCLEUS Software Guide`.
- **Folder 13**: `ND-13.028.1 Site preparation`.
- **`Other/`**: `211024C SINTRAN III Configuration program`, `ND-891092 How to install WinLink`, the Pascal release package (see §5).

## 5. The ND "Software Library" 4-part release package  ← KEY for the catalog
Found complete for **ND-10076J PASCAL for ND-100** in `Other/` (date-prefixed
`YYYYMMDD_ND-…`). A standard ND software release comes as **four documents** [obs, PDFs opened]:

1. **Program Description** — a *one-page form* "NORSK DATA A/S — NORD SOFTWARE LIBRARY /
   PROGRAM DESCRIPTION". Fields: PRODUCT (name, ND-number, category, source ND-number), ISSUED
   (date/by), checkbox grids COMPUTERS (10/12/50/100/500) · INSTR.SET (48/32-bit/commercial) ·
   OP.SYSTEM (SIN III VS/RT/ALONE), DOCUMENTATION (manual ref), PURPOSE, and a PROGRAMS(FILES)
   table (prog-number, name, type BRF/SYMB, containing). **Metadata/index sheet — NOT step-by-step.**
   Footer points to the Installation Guide. *(This is the "PD-sheet".)*
2. **Installation** — titled e.g. "INSTALLATION OF ND-100 PASCAL. VERSION J." Sections: **1 FILES**
   (required files), **2 INSTALLATION PROCEDURE** with numbered steps using **real SINTRAN
   commands**, **branched per OS version**: SINTRAN **H** uses `§NRL` / `*IMAGE-FILE` / `*SIZE` /
   `*LOAD` / `*DUMP` / `§DUMP-REENTRANT`; SINTRAN **I+** uses `§DUMP-PROGRAM-REENTRANT`. Ends
   "The installation is now complete." **← the actual install instructions live here.**
3. **Diskette** — "NORD SOFTWARE LIBRARY DISKETTE": floppy contents manifest (Directory Name =
   `ND-10076J`, SINTRAN file table: num, name, type, access RWACD, pages, bytes).
4. **Revision Log** — change history.

> This 4-part structure is the model our [VERSION-TEMPLATE](../_templates/VERSION-TEMPLATE.md)
> should map onto: PD-sheet → identity/description; Installation → procedure; Diskette → media.
> The version-branched commands (`DUMP-REENTRANT` H vs `DUMP-PROGRAM-REENTRANT` I+) **match** the
> reentrant-load patterns in [../../OS/research/HDD-IMAGE-FINDINGS.md](../../OS/research/HDD-IMAGE-FINDINGS.md) §7.

## 6. Numbering-format evidence (feeds the numbering reference)
Three families seen across the archive [obs]:
- **Old dotted** `ND-xx.yyy.zz` (xx=subject code) — dominant pre-1988.
- **Post-Sept-1988** `ND-8xxyyy.zz` — the "8" + collapsed dot. Confirmed examples:
  `30.003`→`ND-830005/830008`, `20.x`→`ND-820023/820026/820059`, `05.x`→`ND-805013`,
  `14.x`→`ND-814009`, `99.x`→`ND-899159`, plus `ND-860230`, `ND-868208`, `ND-880001`,
  `ND-891092`. **This confirms `document-description.md`'s rule** (see numbering reference).
- **Product/article numbers** `ND-10076J`, `210079N`, `211024C` (6-digit + version letter) — a
  *separate* scheme from the documentation number.

Empirical confirmation that codes **62, 65, 68, 80** are real (folders populated with real docs);
**61, 67, 23** defined but empty in this archive.

## 7. `Other/` classification (≈430 PDFs)
- ND software product docs (~35): `210079N NOTIS-WP`, `211024C SINTRAN III Config`, Pascal set, etc.
- ND hardware/CPU docs (~70): NORD-1/2B/10/12/20/50, ND-100/110/500, microcode, `ND-B1*`/`ND-B2*` board sheets (~33, UNVERIFIED).
- ND service/news (~75): Service Handbooks `ND-NDSHxx`, `ECO-ND-100-xxx` (9), `ND-SIDxxx` (~25), CSI, ND-News/ND-Nytt, MODUS, NOCUS, price lists.
- SINTRAN/TSS/source (~25): VSX program listings, NORD TSS manuals, NORD-OPS, NORD-5 assembler.
- **Third-party (~150):** AMD 2900 family, TTL/UART datasheets, CDC/Seagate/Micropolis/Tandberg drives & terminals, ANSI/ECMA/X.21 standards, textbooks. *(Not ND — exclude from ND catalog.)*
- Ambiguous (~25): generic names (`out.pdf`, `binder-1.pdf`, `DELILAH*`, `Part-1..8`).

**Merge candidates** (ND docs in `Other/` with clear IDs): all `210xxx`/`10xxx` software manuals,
`ND-SIDxxx`, `ND-NDSHxx`, `ECO-ND-100-xxx`, the Pascal release package, VSX listings.

## 8. Open items
- [ ] De-duplicate OCR variants when building any definitive index.
- [ ] Open representative `ND-B1*`/`ND-B2*` to confirm they are ND board drawings.
- [ ] Body chapters (not front matter) hold install steps in manuals — extract per product as needed.
- [ ] Reconcile folder-10 mislabel ("MPM hardware" vs actual Software-Library content).

---
**Parent:** [../README.md](../README.md) · related: [../../ND-NUMBERING-REFERENCE.md](../../ND-NUMBERING-REFERENCE.md)
