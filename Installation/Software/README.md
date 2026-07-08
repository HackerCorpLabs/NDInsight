# Software Catalog & Installation

**Catalog of Norsk Data software for SINTRAN III, organized by function, with per-product /
per-version install documentation.**

This is the *catalog* (categories + product index). Detailed install docs live in per-product
subfolders (`<ND-XXXXX>/`) and per-version subfolders (`<ND-XXXXX>/<ND-XXXXXv>/`). Building the
full per-floppy / per-version archive is a **separate project** — here we catalog and document
*selected* products as we go.

> **Sources & honesty:** the product **Id → Name** list is from the `ndfloppy` `products.json`
> (200 ND article-numbered products), cross-checked against the NDWIKI
> [Software list](https://www.ndwiki.org/wiki/Software_list). The **functional categories**
> and the **ND doc-category tags** below are *curated by us* — Norsk Data did not publish a
> per-product functional taxonomy, so treat the grouping as an organizing aid, not an ND fact.
> Items with no ND article number (NORD-OPS, QED, etc.) are added from NDWIKI and marked *(no art. no.)*.

---

## How products map to floppies (recognition rule)

A distribution floppy belongs to a product when its **volume-name article number matches the
product Id** (strip `ND-`). E.g. floppy `211024E02-XX-01D` → product `ND-211024`; floppy
`ND-10325C` → product `ND-10325`. (This is exactly the matching the `ndfloppy` web app does in
`floppyMatchesProduct()`.) Use this to auto-populate each version's *Distribution media* section.
The OS-floppy recognition signature is documented separately in
[../OS/floppy-contents/README.md](../OS/floppy-contents/README.md).

## ND doc-category tags — confirmed vs unconfirmed
Based on *ND-40.004 version 7* (~1985) — see
[../ND-NUMBERING-REFERENCE.md](../ND-NUMBERING-REFERENCE.md). **That edition is old** (newest
manuals ~Jul 1985) and predates many products below, so its code list is not the last word.

- **Official (in §1.1.1.1 table):** `60` General · `61` NORTEXT · `63` NOTIS · `64` SINTRAN IV · `65` Technovision.
- **Confirmed by real documents** (found in the NDDOC archive, see
  [research/NDDOC-INVENTORY.md](research/NDDOC-INVENTORY.md) §2): `62` Test (`ND-62.009`, `ND-62.008`) ·
  `68` NORCCIS (`ND-868208`) · `80` Utility (`ND-880001`). These are real ND subject codes.
- **Unconfirmed:** `67` Misc — defined in the taxonomy but **no document found**; officialness open.

The post-Sept-1988 form of these is `ND-8XXyyy` (e.g. code 80 → `ND-880001`); see the numbering
reference. These are *document* subject codes, applied here per product as an approximate tag.

## Methodology & templates
- [`INSTALL-METHODOLOGY.md`](INSTALL-METHODOLOGY.md) — the **generic reusable install process**
  (copy typed files → NRL load+dump → version-branched re-entrant dump → boot-mode hooks), with a
  verbatim worked example (ND-100 Pascal J).
- [`_templates/PRODUCT-TEMPLATE.md`](_templates/PRODUCT-TEMPLATE.md) — per-product overview
- [`_templates/VERSION-TEMPLATE.md`](_templates/VERSION-TEMPLATE.md) — per-version install doc
  (maps onto the ND 4-part release package; handles missing PD/PI sheets — falls back to floppy
  MODE files / loading notes / observation)

> **Note:** PD-sheets (Program Description) and PI-sheets (Product Information) are **missing
> for many products**. The version template does not depend on them; it tags install steps by
> source (`[PD]/[PI]/[MODE]/[WIKI]/[OBS]/[INF]`) and never fabricates a procedure.

Documented products link from the **Doc** column below.

---

## 1. Operating Systems
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10047 | SINTRAN III/RT operating system | ND-100 | — |
| ND-10048 | SINTRAN III/VS operating system | ND-100 | — |
| ND-210575 | SINTRAN III/VSX | ND-100 | — |
| ND-210576 | SINTRAN III/VSX-500 | ND-100/500 | — |
| ND-250304 | SINTRAN III/VSX-500 Standard system A | ND-500 | — |
| ND-250306 | SINTRAN III for ND-500 | ND-500 | — |
| ND-250360 | SINTRAN III/VSX Patch File 106000 | ND-100 | — |
| ND-250379 | System files | — | — |
| ND-211305 | ND-500(0) System Package for SINTRAN III/VSX L | ND-500/5000 | — |
| ND-380799 | Basic System Package for ND-5000 Systems | ND-5000 | — |
| — | NORD-OPS (mass-storage OS) *(no art. no.)* | NORD-10 | — |
| — | NORD-TSS (Time Share System) *(no art. no.)* | NORD-10 | — |

> OS install is covered in depth under [../OS/](../OS/README.md), not duplicated here.

## 2. Programming Languages & Compilers
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10018 | RPG-II (RPG Compiler) | ND-100 | 60 |
| ND-10020 | COBOL (phased out) | ND-100 | 60 |
| ND-10023 | FORTRAN (48-bit) | NORD-10 | 60 |
| ND-10024 | BASIC (48-bit) | NORD-10 | 60 |
| ND-10033 | FORTRAN (32-bit) | NORD-10 | 60 |
| ND-10034 | BASIC (32-bit) | NORD-10 | 60 |
| ND-10058 | NORD Simula (48-bit) | NORD-10 | 60 |
| ND-10067 | FORTRAN Runtime system (48-bit) | NORD-10 | 60 |
| ND-10076 | Pascal (48-bit) | NORD-10 | 60 |
| ND-10133 | Pascal (32-bit) | NORD-10 | 60 |
| ND-10136 | FORTRAN Runtime system (32-bit) | NORD-10 | 60 |
| ND-10176 | COBOL for ND-100/NORD-10 | ND-100 | 60 |
| ND-10177 | ND-500 COBOL | ND-500 | 60 |
| ND-10187 | Pascal for ND-500 | ND-500 | 60 |
| ND-10189 | COBOL runtime System | ND-100 | 60 |
| ND-10190 | FORTRAN for ND-500 | ND-500 | 60 |
| ND-10191 | FORTRAN for ND-100/NORD-10 | ND-100 | 60 |
| ND-10196 | ND-100 APL (48-bit) | ND-100 | 60 |
| ND-10309 | PLANC for ND-100 | ND-100 | 60 |
| ND-10310 | PLANC for ND-500 | ND-500 | 60 |
| ND-10510 | FORTRAN Runtime System for ND-500 | ND-500 | 60 |
| ND-10536 | COB-GEN for ND-100/ND-10 | ND-100 | 60 |
| ND-210024 | ND-100 BASIC | ND-100 | 60 |
| ND-210034 | ND-100 BASIC | ND-100 | 60 |
| ND-210177 | COBOL-85 for ND-500/5000 | ND-500/5000 | 60 |
| ND-210191 | Fortran 77 for ND-100/NORD-10 | ND-100 | 60 |
| ND-210863 | FORTRAN Crosscompiler for ND-100 running on ND-500 | ND-500 | 60 |
| ND-211003 | ND Pascal for ND-500 | ND-500 | 60 |
| ND-211196 | Fortran 77 for ND-500 | ND-500 | 60 |
| — | NODAL (interpreted language) *(no art. no.)* | — | 60 |
| — | CAT (Common Abstract Tree language) *(no art. no.)* | — | 60 |
| — | Forth *(third-party)* | — | 67 |

## 3. Language Tools — Linkers / Loaders / Debuggers / Assemblers / Monitors
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10311 | ND-500 Assembler | ND-500 | 60 |
| ND-10319 | Linkage-Loader for ND-500 | ND-500 | 60 |
| ND-10320 | ND-500 Monitor (Single-user) | ND-500 | 60 |
| ND-10333 | ND-500 Monitor (Multi-user) | ND-500 | 60 |
| ND-10335 | ND-500 Symbolic Debugger | ND-500 | 60 |
| ND-10336 | ND-100 Symbolic Debugger (48-bit) | ND-100 | 60 |
| ND-10457 | Linkage Loader for PIOC, ND-500 and ND-100 | ND-100/500 | 60 |
| ND-210333 | ND-500 Monitor (Background Monitor) | ND-500 | 60 |
| ND-210534 | JEC (Job Execution Control) | ND-100 | 80 |
| ND-210721 | BRF-Linker for ND-100 | ND-100 | 60 |
| ND-210913 | SINTRAN III Monitor Call Package | ND-100 | 60 |
| ND-211034 | ND-500 Swapper | ND-500 | 60 |

## 4. Editors & Word Processing
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10080 | PED (NORD Program Editor) | ND-100 | 60 |
| ND-250007 | Mini-Line editor (MLE) for ND-100/500 | ND-100/500 | 60 |
| ND-211465 | LED language program editor (Xenix, ND-5100/xi) | ND-5000 | 60 |
| — | QED (text editor) *(no art. no.)* | ND-100 | 60 |

## 5. Office — NOTIS & NORTEXT suite
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10079 | NOTIS-WP | ND-100 | 63 |
| ND-10152 | NOTIS-IR | ND-100 | 63 |
| ND-10526 | NOTIS-WP for ND-500 | ND-500 | 63 |
| ND-10527 | NOTIS-IR for ND-500 | ND-500 | 63 |
| ND-10724 | NOTIS-BG (Business Graphics) 48-bit | ND-100 | 63 |
| ND-10758 | NOTIS-BG (Business Graphics) 32-bit | ND-100 | 63 |
| ND-210079 | NOTIS-WP for ND-100 | ND-100 | 63 |
| ND-210193 | NOTIS-RG for ND-100 | ND-100 | 63 |
| ND-210691 | NOTIS-DS for ND-100 | ND-100 | 63 |
| ND-211065 | NOTIS-RP for ND-100 | ND-100 | 63 |
| ND-211286 | NOTIS-MAIL | ND-100 | 63 |
| ND-211289 | NOTIS-MAIL Remote User Interface for ND-500/5000 | ND-500/5000 | 63 |
| ND-211290 | NOTIS-MAIL X.400 Gateway | ND-100 | 63 |
| ND-230012 | NORTEXT Page Designer | OWS | 61 |
| — | NOTIS (umbrella product) *(no art. no.)* | ND-100 | 63 |

## 6. Databases & File Access
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10008 | SIBAS Database Multi-User System | NORD-10 | 60 |
| ND-10073 | NORD ISAM (Indexed Sequential Access Method) | NORD-10 | 60 |
| ND-10166 | SIBAS II | ND-100 | 60 |
| ND-10185 | ACCESS-1 (48-bit) | ND-100 | 60 |
| ND-10197 | SIBAS Backend Communication Module | ND-100 | 60 |
| ND-10340 | SIBAS-II for ND-500 | ND-500 | 60 |
| ND-10343 | ISAM for ND-500 | ND-500 | 60 |
| ND-10371 | Data Dictionary System | ND-100 | 60 |
| ND-10379 | Unique | ND-100 | 60 |
| ND-10516 | FILE-HANDLER | ND-100 | 80 |
| ND-210166 | SIBAS II for ND-100 | ND-100 | 60 |
| ND-210185 | ACCESS for ND-100 | ND-100 | 60 |
| ND-210729 | UNIQUE-II SIBAS for ND-100 | ND-100 | 60 |
| ND-211005 | UNIQUE Text System | ND-100 | 60 |
| ND-380419 | SIBAS database system (limited to NOTIS-MAIL use) | ND-100 | 60 |
| ND-380493 | SIBAS libraries | ND-100 | 60 |

## 7. Networking & Communications
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10014 | FLOCON System (SIII VS) | NORD-10 | 60 |
| ND-10021 | FLOCON system (SIII RT) | NORD-10 | 60 |
| ND-10072 | Datacon | NORD-10 | 60 |
| ND-10130 | Xmessage for SINTRAN-III/VS | ND-100 | 60 |
| ND-10199 | X.25 Packet and Link Level for S-III/VS | ND-100 | 60 |
| ND-10373 | IS XMSG | ND-100 | 60 |
| ND-10374 | COSMOS Basic Module | ND-100 | 60 |
| ND-10403 | COSMOS X.21 Option | ND-100 | 60 |
| ND-10409 | X.21 Dialing Driver | ND-100 | 60 |
| ND-10573 | COSMOS X.25 Option | ND-100 | 60 |
| ND-210373 | X-Message (XMSG) | ND-100 | 60 |
| ND-210405 | COSMOS X.29 PAD | ND-100 | 60 |
| ND-210771 | ND Coloured Books Job and File transfer | ND-100 | 60 |
| ND-210866 | ND Coloured Books File Transfer | ND-100 | 60 |
| ND-211154 | COSMOS TELNET/FTP Clients | ND-100 | 60 |
| ND-211185 | COSMOS TCP/IP Gateway | ND-100 | 60 |
| ND-211327 | TCP/IP Basic Module/III | ND-100 | 60 |
| ND-380349 | ND OSI transport service | ND-100 | 60 |
| ND-380718 | (X.400) gateway | ND-100 | 60 |

## 8. Terminal Emulators / IDT / RJE
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10016 | IBM 3270 Emulator | NORD-10 | 60 |
| ND-10026 | NORD IDT CDC 200 User Emulator | NORD-10 | 60 |
| ND-10027 | NORD IDT Honeywell Gerts 115 | NORD-10 | 60 |
| ND-10028 | NORD IDT IBM HASP Work Station | NORD-10 | 60 |
| ND-10029 | NORD IDT Univac NTR | NORD-10 | 60 |
| ND-10030 | NORD IDT IBM 2780/3780 | NORD-10 | 60 |
| ND-10031 | Univac DCT 200 emulator (VS) | NORD-10 | 60 |
| ND-10056 | NORD IDT Univac NTR | NORD-10 | 60 |
| ND-10057 | NORD IDT Univac DCT 2000 | NORD-10 | 60 |
| ND-10059 | 7750 VIP Emulator (48-bit) | NORD-10 | 60 |
| ND-10061 | UTS-400 Emulator | NORD-10 | 60 |
| ND-10063 | NORD IDT IBM HASP Work Station (DMA) (48-bit) | NORD-10 | 60 |
| ND-10069 | NORD IDT CDC 200 User Multidrop Emulator (48-bit) | NORD-10 | 60 |
| ND-10183 | HASP-II | ND-100 | 60 |
| ND-10184 | HASP-II DMA | ND-100 | 60 |
| ND-10312 | VIP 7750 II | ND-100 | 60 |
| ND-10339 | Honeywell GRTS-II Remote Job Entry Emulator | ND-100 | 60 |
| ND-10741 | SNA Gateway Single Line | ND-100 | 60 |
| ND-10742 | SNA Terminal Emulator | ND-100 | 60 |
| ND-210741 | SNA Gateway Single Line | ND-100 | 60 |
| ND-210742 | SNA Terminal Emulator | ND-100 | 60 |

## 9. Transaction Processing & Data Entry
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10053 | NORD Data Entry System | NORD-10 | 60 |
| ND-10054 | NORD TPS (48-bit) | NORD-10 | 60 |
| ND-10188 | FOCUS Level 1 Screen Handling for Transaction Processing | ND-100 | 60 |
| ND-10195 | ND Multiuser Data Entry System | ND-100 | 60 |
| ND-10341 | FOCUS Level 1 for ND-500 | ND-500 | 60 |
| ND-10342 | ND Transaction Processing System TPS-II | ND-100 | 60 |
| ND-210713 | ABM — Application Building and Maintenance for ND-100 | ND-100 | 60 |
| ND-210718 | ABM — Application Building and Maintenance for ND-500 | ND-500 | 60 |

## 10. Graphics, Plotting & Screen Handling
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10010 | Calcomp Plot Package (48-bit) | NORD-10 | 60 |
| ND-10011 | Versaplot Plot Package (48-bit) | NORD-10 | 60 |
| ND-10012 | Plot-10 for NORD-10 (48-bit) | NORD-10 | 60 |
| ND-10013 | NORD Screen Handling System | NORD-10 | 60 |
| ND-10015 | ND-500 Versaplot 07 | ND-500 | 60 |
| ND-10032 | Plot-10 for NORD-10 (32-bit) | NORD-10 | 60 |
| ND-10046 | NORD Colour Terminal Software | NORD-10 | 60 |
| ND-10068 | Versaplot Package (32-bit) | NORD-10 | 60 |
| ND-10135 | NSHS Runtime System | ND-100 | 60 |
| ND-10137 | Versaplot-07 (48-bit) | NORD-10 | 60 |

## 11. Scientific / Math Libraries
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10004 | NORD-50 Program Package | NORD-50 | 60 |
| ND-10007 | Scientific Subroutine Package (48-bit) | NORD-10 | 60 |
| ND-10009 | Scientific Subroutine Package (32-bit) | NORD-10 | 60 |
| ND-10070 | Scientific Subroutine Package for N-50 | NORD-50 | 60 |

## 12. Spooling / Print / Job Control
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-211056 | SPRINT Spooling system | ND-100 | 80 |
| ND-211068 | Operator Environment | ND-100 | 80 |

## 13. Backup / Storage / Disk Utilities
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10141 | LOAD/UNLOAD | ND-100 | 80 |
| ND-10337 | Backup-System | ND-100 | 80 |
| ND-10634 | Memory To floppy dump (MEMTOF-100) | ND-100 | 80 |
| ND-210337 | Backup-System | ND-100 | 80 |
| ND-210855 | ND Disk Mirror and Error Logger | ND-100 | 80 |
| ND-211067 | Mass Storage Utilities | ND-100 | 80 |
| ND-211187 | Disk Restore | ND-100 | 80 |

## 14. System Utilities & Subsystem Packages
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10005 | Subsystem Package (32-bit) | NORD-10 | 80 |
| ND-10022 | SINTRAN Utility Programs | ND-100 | 80 |
| ND-10025 | Sort system (phased out) | NORD-10 | 80 |
| ND-10044 | Subsystem Package (48-bit) | NORD-10 | 80 |
| ND-10142 | DDPP (48-bit) | NORD-10 | 80 |
| ND-10179 | Sort/Merge System | ND-100 | 80 |
| ND-10315 | SINTRAN III Accounting System | ND-100 | 80 |
| ND-10344 | Sort/Merge for ND-500 | ND-500 | 80 |
| ND-10400 | Subsystem Package II | ND-100 | 80 |
| ND-210375 | Telefix for User Sites | ND-100 | 80 |
| ND-210400 | Subsystem Package II | ND-100 | 80 |
| ND-210455 | VTM terminal tables (Standard) | ND-100 | 80 |
| ND-210507 | Software Keys | ND-100 | 80 |
| ND-210518 | User-Environment | ND-100 | 80 |
| ND-210601 | UE-Library | ND-100 | 80 |
| ND-210628 | SINTRAN III Utility programs | ND-100 | 80 |
| ND-210873 | ND I/O Line Switch | ND-100 | 80 |
| ND-211024 | SINTRAN III Configuration Program | ND-100 | 80 |
| ND-211464 | VTM terminal tables (Type 128/129 DEC VT200) | ND-100 | 80 |
| ND-211846 | SOFTWARE-KEYS | ND-100 | 80 |
| ND-250006 | Perform for ND-500 | ND-500 | 80 |
| — | TELEFIX (remote analyze) *(no art. no.)* | ND-100 | 80 |

## 15. Diagnostics & Test Programs
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10112 | Test-programs for ND-50 | NORD-50 | 62 |
| ND-10300 | Test Sys. for X.25 Packet and Link Level (48-bit) | ND-100 | 62 |
| ND-10321 | ND-500 Micro Test Programs | ND-500 | 62 |
| ND-10324 / ND-10324E | Test programs No. 1 for ND-10/12/100 | ND-100 | 62 |
| ND-10325 / ND-10325B | Test programs No. 2 for ND-10/12/100 | ND-100 | 62 |
| ND-10326 / ND-10326B | Test programs No. 3 for ND-10/12/100 | ND-100 | 62 |
| ND-210523 | Test programs for ND-100/110/120 | ND-100 | 62 |
| ND-211479 | DSS Basic System | ND-100 | 62 |
| ND-211480 | DSS Libraries | ND-100 | 62 |
| ND-211481 | DSS Test programs | ND-100 | 62 |

## 16. Maintenance Kits
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-211321 | NUCLEUS Maintenance Kit | ND-100 | 62 |
| ND-211322 | DOMINO Maintenance Kit | ND-100 | 62 |
| ND-230007 | Maintenance Programs for OWS 10/11/12 | OWS | 62 |

## 17. OWS / Desktop / PC Integration
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-10561A | PC-LINK | PC | 67 |
| ND-10563A | ND-LINK.EXE | PC | 67 |
| ND-211297 | OWS Access Server | ND-100 | 67 |
| ND-211325 | OWS Access Server for ND 500/5000 | ND-500/5000 | 67 |
| ND-230006 | ND Desktop Manager for OWS 11/12 | OWS | 67 |
| ND-230008 | OWS 12 Windows Set-up and Build Disks | OWS | 67 |
| ND-230025 | Desk Top Manager for OWS | OWS | 67 |
| — | GETNORD (NORD diskette reading utility) *(no art. no.)* | PC | 67 |
| — | GETCPM (CP/M companion) *(no art. no.)* | PC | 67 |

## 18. Uncategorized / Unknown
| ID | Name | Target | Tag |
|----|------|--------|-----|
| ND-380451 | Unknown product | — | 67 |

---

## Documented products
*(subfolders created so far; the rest of the catalog above is index-only)*

| Product | Versions documented |
|---------|---------------------|
| [ND-10022 SINTRAN Utility Programs](ND-10022/README.md) | [ND-10022U](ND-10022/ND-10022U/README.md) |

---

## Coverage note (vs NDWIKI)
`products.json` (200 entries) covers the full NDWIKI software list **and adds** items the wiki
summary omits (NOTIS-MAIL family, NOTIS-RG/RP, DSS, NORTEXT Page Designer, OWS Desktop/Access,
NUCLEUS/DOMINO kits, OSI transport, SIBAS variants, ND-5000 package). Items present on NDWIKI
but **without an ND article number** — NORD-OPS, NORD-TSS, QED, NODAL, CAT, GETNORD, GETCPM,
Forth — are folded into the categories above and marked *(no art. no.)*.

**Parent:** [../README.md](../README.md)
