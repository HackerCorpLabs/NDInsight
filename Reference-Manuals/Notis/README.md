# NOTIS - Norsk Data Office Automation Suite

## What NOTIS Is

**NOTIS** is Norsk Data's integrated family of office-automation software for the
**ND-100** and **ND-500** minicomputers, running under the **SINTRAN III** operating
system. Introduced in the early 1980s, NOTIS was one of the first complete
"electronic office" packages in Europe: a suite of programs covering word
processing, text formatting, document archiving, information retrieval, electronic
mail, spreadsheets, business graphics, drawing, report generation and a personal
calendar - all sharing a common document format and a common screen-oriented user
interface built around the dedicated **ND-NOTIS terminals** (TDV-2200/9 and
similar) with their special function keys.

The name is a Norwegian/Scandinavian play on *notis* ("note" / "notice"); each
component carries a two-letter suffix (WP, TF, DS, IR, ...) identifying its role.

## What You Would Use It For

NOTIS was the day-to-day productivity environment for office staff, secretaries,
engineers and managers on a Norsk Data installation. With it a user could:

- Write, edit and revise letters, reports and manuals on screen (**NOTIS-WP**).
- Produce professionally typeset, paginated output with headers, footers, indexes
  and multi-column layout (**NOTIS-TF**).
- Store, index, retrieve and keep order in thousands of documents (**NOTIS-DS**).
- Search large free-text archives for information (**NOTIS-IR**).
- Send electronic mail across a COSMOS network (**NOTIS-ID**).
- Build spreadsheets and do calculations (**NOTIS-CALC**).
- Generate charts and business graphics (**NOTIS-BG**, **NOTIS-DRAW**).
- Produce formatted reports from SIBAS databases, ISAM and flat files
  (**NOTIS-RG**).
- Keep a personal appointment calendar (**NOTIS-PM**).

## The Value / Why You Would Use It

- **Integration** - all NOTIS programs share one document format, so a document
  written in NOTIS-WP can be formatted in NOTIS-TF, archived in NOTIS-DS, mailed
  through NOTIS-ID, searched in NOTIS-IR or pulled into a report by NOTIS-RG,
  without conversion.
- **Consistency** - a single screen-oriented, menu-and-function-key interface
  across the whole suite meant users learned one system, not ten.
- **Terminal-native** - designed to exploit the ND-NOTIS terminals' special keys,
  graphic/Greek/mathematical character sets and screen handling.
- **Multi-machine** - the same products run on both ND-100 and (for several
  components) ND-500 hardware.
- **Multilingual** - documentation and program text existed in English (EN),
  Norwegian (NO) and Swedish (SW) editions.

For this repository specifically, these manuals are the authoritative source for
understanding NOTIS behavior when emulating or reverse-engineering ND-100/ND-500
systems, terminal protocols and SINTRAN application software.

## The NOTIS Product Variants

| Suffix | Product | Purpose |
|--------|---------|---------|
| **WP** | NOTIS-WP | Word processing - screen editor for creating and editing documents. |
| **TF** | NOTIS-TF | Text Formatter - typesetting/pagination of documents, with a macro language. |
| **DS** | NOTIS-DS | Document Storage - archiving, indexing and retrieval of large document collections. |
| **IR** | NOTIS-IR | Information Retrieval - storage and free-text search of large information sets (also as NTX-IR for NORTEXT typographic systems). |
| **ID** | NOTIS-ID | Information Distribution - electronic mail over a COSMOS network, integrated with WP and DS. |
| **RG** | NOTIS-RG | Report Generator - high-level report generation from SIBAS/ISAM/flat files (NOTIS-RG editor + NOTIS-RP producer). |
| **CALC** | NOTIS-CALC | Spreadsheet / calculation sheets with fields, formulas and sub-sheets. |
| **BG** | NOTIS-BG | Business Graphics - charts and graphs (ND-100, in 32-bit and 48-bit floating-point builds). |
| **DRAW** | NOTIS-DRAW | Drawing program for diagrams and overhead slides. |
| **PM** | NOTIS-PM | Personal calendar / appointment manager for the individual user. |

Suffix **-M** on a product name (e.g. NOTIS-WP-M, NOTIS-TF-M) denotes the later
**M-release** generation with expanded functionality.

Language codes in the document list: **EN** English, **NO** Norwegian, **SW** Swedish.

---

## Analysis documents (NDInsight)

| Document | Description |
|----------|-------------|
| [NOTIS-PRINTING-AND-A4-PAGES.md](./NOTIS-PRINTING-AND-A4-PAGES.md) | How a user edits and prints in NOTIS-WP, how NOTIS-TF produces typeset A4 pages, and the print path (printer-definition file, SINTRAN/SPRINT spooling, terminal-attached printers, supported models, no laser). Cross-linked to `../../SINTRAN/Print/`. |
| [NOTIS-ID-EMAIL-AND-COSMOS.md](./NOTIS-ID-EMAIL-AND-COSMOS.md) | What email is in NOTIS (NOTIS-ID), how you write/send/read mail (Intray/Outtray, command menu), and how delivery works - mailbox in NOTIS-DS, address/mailing-list resolution, the mail server, registered mail, and store-and-forward over the COSMOS network. Notes the missing Supervisor Guide. |

---

## Documents

### NOTIS-WP - Word Processing

| Document | Description |
|----------|-------------|
| [ND-63.001.02 Introduction to NOTIS-WP](./ND-63.001.02%20Introduction%20to%20NOTIS-WP.md) | (EN) Introductory guide to the NOTIS-WP word processor, 1983. |
| [ND-63.002.02 NOTIS-WP Reference Manual - Editor](./ND-63.002.02%20NOTIS-WP%20Reference%20Manual%20-%20Editor.md) | (EN) Full reference manual for the NOTIS-WP screen editor. |
| [ND-63.003.02 NOTIS-WP Introduksjon (July 1983)](./ND-63.003.02_NOTIS-WP_Introduksjon_July_1983.md) | (NO) Norwegian introduction to NOTIS-WP. |
| [ND-63.004.02 NOTIS-WP Haandbok - Editor (June 1983)](./ND-63.004.02_NOTIS-WP_Haandbok_-_Editor_June_1983.md) | (NO) Norwegian handbook/reference for the NOTIS-WP editor. |
| [ND-63.036.1 NO NOTIS-WP-M Nye funksjoner (May 1985)](./ND-63.036.1_NO_NOTIS-WP-M_Nye_funksjoner_Referanseinformasjon_og_opplaering_May_1985.md) | (NO) NOTIS-WP-M new functions - reference information and training. |
| [ND-63.042.1 EN NOTIS-WP M Release Information for new users](./ND-63.042.1%20EN%20NOTIS-WP%20M%20Release%20Information%20for%20new%20users.md) | (EN) Getting-started release information for NOTIS-WP version M (ND-10079 M). |
| [ND-99.035.1 NO NOTIS-WP-M Paa 5 Minutter (January 1986)](./ND-99.035.1_NO_NOTIS-WP-M_Paa_5_Minutter_January_1986.md) | (NO) "In 5 minutes" quick overview of NOTIS-WP-M for experienced WP users. |
| [ND-99.006.3 EN NOTIS-WP REFERENCE CARD for ND-NOTIS terminals](./ND-99.006.3%20EN%20NOTIS-WP%20REFERENCE%20CARD%20for%20ND-NOTIS%20terminals.md) | (EN) Quick-reference card of NOTIS-WP keys for ND-NOTIS terminals. |
| [ND-99.007.2 NO NOTIS-WP OPPSLAGSKORT for TDV-2200/9 (August 1986)](./ND-99.007.2-NO_NOTIS-WP_OPPSLAGSKORT_for_TDV-2200-9_ND-NOTIS_terminaler__August_1986.md) | (NO) Norwegian quick-reference card for NOTIS-WP on TDV-2200/9 ND-NOTIS terminals. |

### NOTIS-TF - Text Formatter

| Document | Description |
|----------|-------------|
| [ND-63.007.01 NOTIS-TF Text Formatter Reference Manual](./ND-63.007.01%20NOTIS-TF%20Text%20Formatter%20Reference%20Manual.md) | (EN) Complete reference for the NOTIS-TF text formatter. |
| [ND-63.008.01 NOTIS-TF Haandbok (June 1983)](./ND-63.008.01_NOTIS-TF_Haandbok_June_1983.md) | (NO) Norwegian handbook for NOTIS-TF. |
| [ND-63.009.01 NOTIS-TF Macro Guide](./ND-63.009.01%20NOTIS-TF%20Macro%20Guide.md) | (EN) Guide to the NOTIS-TF macro language. |
| [ND-63.010.01 NOTIS-TF Makro Veiledning (August 1983)](./ND-63.010.01_NOTIS-TF_Makro_Veiledning_August_1983.md) | (NO) Norwegian guide to NOTIS-TF macros. |
| [ND-63.041.1 NO NOTIS-TF-M Nye funksjoner (October 1985)](./ND-63.041.1_NO_NOTIS-TF-M_Nye_funksjoner_October_1985.md) | (NO) New functions in the NOTIS-TF-M release. |

### NOTIS-DS - Document Storage

| Document | Description |
|----------|-------------|
| [ND-63.017.2 SW NOTIS-DS Introduktion](./ND-63.017.2%20SW%20NOTIS-DS%20Introduktion.md) | (SW) Swedish introduction to the NOTIS-DS document-archiving program. |

### NOTIS-IR - Information Retrieval

| Document | Description |
|----------|-------------|
| [ND-63.005.3 NO NOTIS-IR Brukerveiledning (January 1987)](./ND-63.005.3_NO_NOTIS-IR_Brukerveiledning_January_1987.md) | (NO) User guide for NOTIS-IR (storage and free-text retrieval), ND-100 and ND-500. |
| [ND-63.043.1 SW Nya funktioner i Notis-IR F](./ND-63.043.1%20SW%20Nya%20funktioner%20i%20Notis-IR%20F.md) | (SW) New functions in the NOTIS-IR F release. |

### NOTIS-ID - Information Distribution (Electronic Mail)

| Document | Description |
|----------|-------------|
| [ND-63.011.2 EN NOTIS-ID User Guide](./ND-63.011.2%20EN%20NOTIS-ID%20User%20Guide.md) | (EN) User guide for NOTIS-ID electronic mail (version B), integrated with WP and DS over COSMOS. |

### NOTIS-RG - Report Generator

| Document | Description |
|----------|-------------|
| [ND-63.013.5 SW NOTIS-RG Referenshandbok](./ND-63.013.5%20SW%20NOTIS-RG%20Referenshandbok.md) | (SW) Reference manual for the NOTIS-RG report generator (NOTIS-RG editor + NOTIS-RP producer), ND-100 and ND-500. |

### NOTIS-CALC - Spreadsheet

| Document | Description |
|----------|-------------|
| [ND-63.026.01 NOTIS-CALC Brukerhaandbok (November 1983)](./ND-63.026.01_NOTIS-CALC_Brukerhaandbok_November_1983.md) | (NO) User handbook for the NOTIS-CALC calculation-sheet program. |

### NOTIS-DRAW - Drawing

| Document | Description |
|----------|-------------|
| [ND-63.045.1 SW NOTIS-DRAW Handbok](./ND-63.045.1%20SW%20NOTIS-DRAW%20Handbok.md) | (SW) Handbook for NOTIS-DRAW (diagrams and overhead slides), 1987. |

### NOTIS-PM - Personal Calendar

| Document | Description |
|----------|-------------|
| [ND-63.030.1 SW NOTIS-PM Handbok](./ND-63.030.1%20SW%20NOTIS-PM%20Handbok.md) | (SW) Handbook for NOTIS-PM, the individual user's private calendar (A-version), 1987. |

### Software Library Diskettes (Distribution Media Listings)

These are the file directories of the original ND Software Library floppy diskettes -
useful for identifying the actual program files, installation commands and version
letters shipped for each product.

| Document | Description |
|----------|-------------|
| [10079K_NOTIS-WP](./10079K_NOTIS-WP.md) | Diskette directory for NOTIS-WP (ND-10079K-1), including INSTALL-WP and printer definitions. |
| [210079N NOTIS-WP for ND-100](./210079N_NOTIS-WP_for_ND-100.md) | Diskette directory for NOTIS-WP N-release for ND-100 (editor, command, resident, spooler, print modules). |
| [ND-10079G NOTIS-1](./ND-10079G%20NOTIS-1.md) | Diskette directory for NOTIS-1 (English part, ND-10079G) - early TED/TEX-based NOTIS. |
| [210193C NOTIS-RG for ND-100](./210193C_NOTIS-RG_for_ND-100.md) | Diskette directory for NOTIS-RG C-release for ND-100 (English), incl. NOTIS-RG/RP programs and error messages. |
| [210691C NOTIS-DS for ND-100](./210691C_NOTIS-DS_for_ND-100.md) | Diskette directory for NOTIS-DS C-release for ND-100 (Norwegian), incl. install and utility programs. |
| [10724B NOTIS-BG for ND-100 (Business Graphics, 48-bit float)](./10724B_NOTIS-BG_for_ND-100_Business_Graphics_48_bits_floating_format.md) | Diskette directory for NOTIS-BG business graphics, 48-bit floating-point build, with plot examples. |
| [10758B NOTIS-BG for ND-100 (Business Graphics, 32-bit float)](./10758B_NOTIS-BG_for_ND-100_Business_Graphics_32_bits_floating_format.md) | Diskette directory for NOTIS-BG business graphics, 32-bit floating-point build, with plot examples. |

---

*Part of the [NDInsight](../../README.md) Norsk Data / SINTRAN III documentation and
preservation project. All manuals are Norsk Data A.S publications; copyrights belong
to their original holders and are reproduced here for historical and technical
reference.*
