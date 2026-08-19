# SINTRAN III VSE/VSX Version J - SI-GEN Output (1985)

Two files recovered from a backup volume, plus the split-out NPL source files extracted from them.

---

## The Two Original Files

### BOUT-6.SYMB (3,007 lines)

The **batch console log** of a real SINTRAN III generation (SI-GEN) run:

- Header: `SINTRAN III - VSX J`, product `ND-100-1016 SI-GEN`
- Comment banner: `**** SIII-VSE VERSION J ****`
- Run by user `AUX-SINTRAN` at **15:59, 8 October 1985**
- Target machine serial: `**** N102.2921 L****` (an ND-102 CPU)
- Shows the whole build: the MAC/FMAC sessions, the date being patched into the image (GENDA), and the full list of library marks chosen for this configuration (8XMSG, 8STRN, 8CXHD common HDLC, 8PIOC, 8COSP, COS-TAD access devices, SCSI marks, ND-500 marks 9TR27/9TR28, etc.)

The key lines that explain the other file:

```
@CONNECT-FILE (SINTRAN)BPUN3:SYMB,103,RX
@CONNECT-FILE (AUX)S3VS-6:SYMB,105,WA
@FMAC
```

`BPUN3:SYMB` is opened read-only as file number 103, `S3VS-6:SYMB` is opened **write-append as file 105**, and FMAC then dumps, mixes and assembles the *variable* (configuration-dependent) parts of SINTRAN into it. The log says so itself: *"THIS LISTING CONTAINS THE CONFIGURATION-DEPENDENT PART OF YOUR SINTRAN-III LISTING."*

**So: BOUT-6 is the build log, and S3VS-6 is the source listing that very run produced.**

The tail of BOUT-6 (from line 2,079) is a **"SYMBOL LIST WITH ADDRESSES IN ASCENDING ORDER" with 4,315 symbol entries** - a complete per-build symbol table for exactly this N102.2921 image. Use it to place every routine in the split NPL files at its absolute address, and to cross-check `../NPL-SOURCE/SYMBOLS/J/` (which comes from the 86-08-04 floppy set, a later build of the same version).

### S3VS-6.SYMB (23,974 lines, 991 KB)

The generated listing: an octal load-address column followed by NPL/MAC source. It is the version-J counterpart of the existing `../NPL-SOURCE/s3vs-4.symb` (3.9 MB), from which the 45 NPL files in `../NPL-SOURCE/NPL/` were extracted. The file is **complete** - it ends with `@EOF` at address 133651.

**History note (2026-08-18):** the first export of these two files carried extra garbage past the real end (read-past-EOF slack from the extraction: stray fragments of earlier disk blocks, and in BOUT-6 also raw binary). That junk tail made S3VS-6 look truncated mid-statement - it was not. Both files were replaced with clean re-exports the same day; the split below is from the clean copies.

---

## The Split (NPL/ and NPL-CLEAN/)

Section boundaries come from three kinds of markers **inside the listing itself**:

1. `@DEV (S-S-J)<name>` file-switch lines - these name the **original ND source files** on directory `S-S-J` that were being read (COS-TAD-RES-CODE, SINB-X, CDR1..3, XMSG-SYSTABS, MRES-*, COS-TAD-POF-CODE)
2. `%%%%` banner headers for the head sections (MACROES - SIN1-GEN .. SIN4-GEN, SSCOM, BUFFERS)
3. Embedded vendor file headers in the tail - e.g. `FILE NAME : WINCHESTER:NPL, PROGRAM NO. : HUT-2475 A.6, ISSUED 82.02.18, LAST CHANGED 85.09.04` (appears twice: once for the resident copy, once for the paging-off copy)

Two forms of every file:

- **`NPL/`** - listing form, octal address column **kept**. Use this to match code against memory dumps, symbol tables and carved segments.
- **`NPL-CLEAN/`** - address column stripped (a leading MAC conditional quote `"` is preserved), for reading and for diffing against other SINTRAN versions.

File names keep the original ND source-file names from the `@DEV` lines; the numeric prefix preserves the order within S3VS-6.SYMB.

## Section Table

"Overlap" = fraction of a section's distinct normalized code lines that also appear anywhere in `../NPL-SOURCE/s3vs-4.symb` (address column stripped, whitespace collapsed, banner/empty lines ignored). **This is an upper bound on how much is duplicate**: a low number means genuinely new version-J material, but a middling number can also just mean different configuration values in otherwise-known code. Computed 2026-08-18 by set comparison, not reviewed line by line.

| File | Lines in S3VS-6 | Size | Overlap vs s3vs-4 |
|------|----------------|------|-------------------|
| 01-SIN1-GEN.NPL | 2-1229 | 1228 | 45% |
| 02-SIN2-GEN.NPL | 1230-2043 | 814 | 88% |
| 03-SIN3-GEN.NPL | 2044-2587 | 544 | 44% |
| 04-SIN4-GEN.NPL | 2588-4116 | 1529 | 63% |
| 05-SSCOM.NPL | 4117-4162 | 46 | 7% |
| 06-COS-TAD-RES-CODE.NPL | 4163-4928 | 766 | 31% |
| 07-SINB-X.NPL | 4929-6189 | 1261 | 22% |
| 08-CDR1.NPL | 6190-6593 | 404 | 69% |
| 09-CDR2.NPL | 6594-7570 | 977 | 41% |
| 10-XMSG-SYSTABS.NPL | 7571-7626 | 56 | 8% |
| 11-CDR3.NPL | 7627-8660 | 1034 | 58% |
| 12-EX-MRES-SINA.NPL | 8661-10395 | 1735 | 34% |
| 13-MRES-SINI.NPL | 10396-11941 | 1546 | 45% |
| 14-MRES-SEGADM.NPL | 11942-13144 | 1203 | 32% |
| 15-ND500-DRIVER-1.NPL | 13145-13154 | 10 | 43% |
| 16-MRES-CDR1.NPL | 13155-13796 | 642 | 78% |
| 17-MRES-CDR2.NPL | 13797-14670 | 874 | 55% |
| 18-MRES-CDR3.NPL | 14671-15692 | 1022 | 58% |
| 19-MRES-SSCOM.NPL | 15693-15703 | 11 | 20% |
| 20-COS-TAD-POF-CODE.NPL | 15704-17919 | 2216 | 68% |
| 21-BUFFERS.NPL | 17920-18489 | 570 | 92% |
| 22-WINCHESTER-RES.NPL | 18490-19205 | 716 | 76% |
| 23-WINCHESTER-POF.NPL | 19206-20739 | 1534 | 39% |
| 24-SYMBOLIC-DEBUGGER.NPL | 20740-23185 | 2446 | 54% |
| 25-PIT3-PIT0-CODE.NPL | 23186-23974 | 789 | 61% |

Overall, roughly **half of the code lines are not in s3vs-4** - the biggest genuinely-new-looking blocks are the COS-TAD terminal-access-device code, SINB-X, SSCOM, XMSG-SYSTABS and MRES-SEGADM.

Notes on individual files:

- **15-ND500-DRIVER-1** and **19-MRES-SSCOM** are only the `@DEV` switch plus a few lines - in this configuration the content was conditionally excluded (this machine's SI-GEN answers did not include those options), so only the file switch itself made it into the listing.
- **22/23-WINCHESTER-RES/POF** are two copies of the same vendor driver source `WINCHESTER:NPL` (HUT-2475 A.6): once assembled into the resident part, once into the paging-off part. File 23 also carries the code that follows the driver up to the symbolic-debugger section.
- **24-SYMBOLIC-DEBUGGER** contains the BRPNT/3DEBUG code (MON 204/205) plus surrounding paging-off code.
- **25-PIT3-PIT0-CODE** (MONPI, EXEL, PITIM, PIORE) is the last section and ends with the listing's real `@EOF`.

## Version J Symbol Tables (recovered separately)

The matching **version J symbol tables** were extracted 2026-08-18 from the four original distribution floppies `N-900-188-I..IV` ("SINTRAN III VERSION J", MACM-1718, gen. date 86.08.04, images on backup disk `D:\ND\Frode\Sintran III Version J 86-08-04\`) and installed as `../NPL-SOURCE/SYMBOLS/J/` (FILSYS-SYMBOLS, RTLO-SYMBOLS, SYMBOL-1-LIST, SYMBOL-2-LIST, LIBRARY-MARKS - converted from ND 8-bit parity text to ASCII). The floppies also carry the version J system itself: `SINTRAN-I:DATA` + `SINTRAN-II:DATA` (the OS image), `MACM-1718L:BPUN`, `NEW-SYSTEM:PROG`, F32/F48 FMAC, `DMAC-1915F:BPUN` and `COS-TADADM:BPUN` - not copied into the repo, read them from the images with ndtool when needed.

Cross-check: the J `LIBRARY-MARKS.SYMB` first line is word-for-word identical to the library-marks list in `BOUT-6.SYMB` - same product generation family. Note the dates differ: the SI-GEN run logged here is 8 October 1985, the floppy set is generated 4 August 1986.

## How This Relates to ../NPL-SOURCE

- `../NPL-SOURCE/s3vs-4.symb` is the same kind of artifact (SI-GEN output listing) from a **different, later generation** and covers more ground (it also contains RTDES-GEN, DISK-GEN, MASS-GEN, BDIO-GEN, CX-SIN0-GEN sections).
- The value of this version-J set: it is dated **8 October 1985**, older than the K03/L07/M06 symbol-table versions in `../NPL-SOURCE/SYMBOLS/`, making it the earliest SINTRAN source generation in the repository, and it is a **VSE/VSX** (extended) configuration with COS-TAD and XMSG selected.

---

**Split performed**: 2026-08-18. Source of truth remains the unmodified `S3VS-6.SYMB` / `BOUT-6.SYMB`.
