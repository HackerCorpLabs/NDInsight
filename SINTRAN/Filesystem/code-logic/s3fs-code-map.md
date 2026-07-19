# `006-S3FS` filesystem code map (Phases 5-6)

A routine map of the `006-S3FS` filesystem segment, grouped by area. For each
FILSYS symbol: its octal address and a one-line role. Segment **load base
26000B**, 54272 words. Every address here is a real label in the FILSYS symbol
table and resolves to a real entry point in the carved SINTRAN L bytes.

**Evidence tags:** **VERIFIED** = read from the `006-S3FS` bytes (this document
traces the entry bytes of the routines it discusses); **INFERRED** = role deduced
from the symbol name plus a carved cross-check but the full body was not traced
here; **OPEN** = not settled by the bytes examined.

**Sources.** Disassembly: [`../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm)
(and `.hex`, `.symbols.txt`). Already-carved MON workers under
[`../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/)
are used as anchors and linked per row. Instruction meanings are grounded in
[`ND100-INSTRUCTION-SEMANTICS.md`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

See also the companion analyses: [`allocation.md`](allocation.md) (page bitmap +
directory/user/file allocation) and [`file-io.md`](file-io.md) (read/write/append/delete).

---

## The two structural idioms you must know first (both VERIFIED)

Almost every I/O and create/delete primitive in `006-S3FS` is built from one of
two idioms. Recognising them collapses ~30 symbols into a handful of shared
bodies.

### Idiom 1 - the SSK/SSM flag fork

Several public symbols are *not* separate routines - they are one-instruction
prologues that set the ND-100 status-register one-bit flags **SSK** (the "K"
bit) and/or **SSM** (the "M" bit), then jump into a **shared body**. The shared
body later reads the flag back with `BSKP ONE SSK` / `BSKP ONE SSM` to fork
read-vs-write (or which of four operations). This is verified from the byte
patterns `BSET ONE/ZRO SSK` / `BSET ONE/ZRO SSM` at each entry.

| Entry pair/group | Shared body | Flag meaning (VERIFIED from the `BSET`s) |
|------------------|-------------|------------------------------------------|
| `RPAGE` 101707B / `WPAGE` 101711B | 101712B | SSK: 0 = read bit-file page, 1 = write |
| `RDISK` 102021B / `WDISK` 102023B | 102024B | SSK: 0 = read, 1 = write |
| `RFILE` 102130B / `WFILE` 102132B | 102133B | SSK: 0 = read file, 1 = write file |
| `RDPAG` 107447B / `WDPAG` 107451B | 107452B | SSK: 0 = read disk page, 1 = write |
| `GP5IX` 51451B / `RINDX` 51453B | 51454B | SSK: 0 = read index block, 1 = get/5-index |
| `ALPAG` 50627B / `XRLPA` 50632B / `RLPAG` 50635B | 50637B | SSK/SSM together = mark used / release |
| `SFACC` 105552B / `EXPFI` 105555B / `CRALN` 105560B / `CRALF` 105562B | 105564B | (SSM,SSK) selects set-access / expand / create-indexed / create-contiguous |
| `STEFI` 106052B / `SPEFI` 106055B / `MRNFI` 106060B / `MDLFI` 106063B | 106065B | (SSM,SSK) selects set-temp / set-perm / rename / delete |

### Idiom 2 - PLANC frame + indirect call table

Every routine opens with `STD I nn` (save the L/return double), `RADD CLD SL DA`
/ `RADD CLD SB DD`, `SAB k` (set up a **B-relative local frame** of `k` words),
then `JPL I m` into a per-routine **pointer table** that lives just past the code
body (the words the disassembler renders as data at the routine's tail, e.g. the
`ORA`/`LDF`/`FAD` "instructions" that are actually addresses). Calls to helpers
appear as `JPL I nn` where `nn` indexes that table. This is standard PLANC
compiler output. Consequence: a helper call's *target* is a data word in the
frame table, so `JPL I` targets are only as reliable as reading that pool word -
several are traced explicitly in [`allocation.md`](allocation.md) and
[`file-io.md`](file-io.md); others are marked INFERRED.

---

## Open / close

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 67432B | `FOPEN` | File open - core open path | [`50B-OpenFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/50B-OpenFile/README.md) |
| 67612B | `FCLOS` | File close - core close path | [`43B-CloseFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/43B-CloseFile/README.md) |
| 103026B | `DOPEN` | Direct-open (MON 220 DirectOpen) | [`220B-DirectOpen`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/220B-DirectOpen/README.md) |
| 103034B / 103037B | `OPFIL` / `OLDOP` | Open-file / open-old-version | INFERRED |
| 103350B / 103355B | `BCLOS` / `CLOFI` | Backup-close (MON 252) / close file | [`252B-BackupClose`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/252B-BackupClose/README.md) |
| 66072B / 66123B | `FOFT` / `SOFT` | Find / set open-file table slot | INFERRED |
| 67002B | `FCON` | File-connect (bind open entry to object) | INFERRED |

## Read / write (file I/O)

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 102130B / 102132B | `RFILE` / `WFILE` | Read / write file (shared body 102133B, SSK-forked) | [`117B-ReadFromFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/117B-ReadFromFile/README.md), [`120B-WriteToFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/120B-WriteToFile/README.md) |
| 102021B / 102023B | `RDISK` / `WDISK` | Read / write disk (page-level, shared body 102024B) | VERIFIED |
| 77542B / 100130B | `FREA` / `FWRT` | File read / write core (buffered) | VERIFIED (pool of RFILE) |
| 100566B / 100570B | `FDREA` / `FDWRT` | File direct read / write (unbuffered) | VERIFIED (pool of RFILE) |
| 77000B / 77230B | `REBUF` / `WRBUF` | Read / write the file byte buffer | INFERRED |
| 102517B / 102625B | `FGET` / `FPUT` | File get / put (byte/record level) | INFERRED |
| 71771B / 72014B | `RBUF` / `RBYTE` | Read buffer / read byte | INFERRED |
| 103767B | `RMAX` | Read max-bytes | INFERRED |

## Disk-page primitives

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 101707B / 101711B | `RPAGE` / `WPAGE` | Read / write one **bit-file** page (shared body 101712B) | VERIFIED |
| 107447B / 107451B | `RDPAG` / `WDPAG` | Read / write one raw **disk** page (2048 B; shared body 107452B) | [`270B-ReadDiskPage`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/270B-ReadDiskPage/README.md), [`271B-WriteDiskPage`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/271B-WriteDiskPage/README.md) |
| 110050B | `COPAG` | Copy file pages (MON 251 CopyPage) | [`251B-CopyPage`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/251B-CopyPage/README.md) |
| 110472B | `DELPG` | Delete file pages between two page numbers (MON 272) | [`272B-DeletePage`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/272B-DeletePage/README.md) |

## Create / delete / rename / expand

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 64410B / 64670B | `CRNEW` / `GCFIL` | Create new file / get-or-create file | [`221B-CreateFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/221B-CreateFile/README.md) |
| 63726B / 64146B | `CROBJ` / `DLOBJ` | Create / delete the object entry (file record) | VERIFIED (entry traced in [`allocation.md`](allocation.md)) |
| 65144B | `FFILE` | Find file (name -> object) | INFERRED |
| 105555B | `EXPFI` | Expand file (MON 231; grows contiguous/index space) | [`231B-ExpandFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/231B-ExpandFile/README.md) |
| 105560B / 105562B | `CRALN` / `CRALF` | Create-allocated **indexed** / **contiguous** (via dispatcher 105564B) | VERIFIED |
| 105552B | `SFACC` | Set file access (shares dispatcher 105564B) | VERIFIED |
| 106060B / 106063B | `MRNFI` / `MDLFI` | Rename / delete file (via dispatcher 106065B) | [`232B-RenameFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/232B-RenameFile/README.md), [`54B-DeleteFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/54B-DeleteFile/README.md) |
| 106052B / 106055B | `STEFI` / `SPEFI` | Set temporary / permanent file (shares 106065B) | [`233B-SetTemporaryFile`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/233B-SetTemporaryFile/README.md), [`236B-SetPermanentOpen`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/236B-SetPermanentOpen/README.md) |
| 63313B / 63315B | `CNEWV` / `CHIGV` | Create-new-version / change-highest-version | [`253B-NewFileVersion`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/253B-NewFileVersion/README.md) |

## Object-entry access

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 55563B / 55566B / 55750B | `FOBJB` / `ROBJE` / `WOBJE` | Find / read (MON 41) / write object block | [`41B-ReadObjectEntry`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/41B-ReadObjectEntry/README.md) |
| 56307B / 56326B | `ROBJB` / `GOBJI` | Read object block / get object index | INFERRED |
| 61502B | `COBJE` | Change object entry | INFERRED |
| 104035B / 104037B / 104410B | `MROBJ` / `DROBJ` / `DWOBJ` | MON-read / direct-read / direct-write object entry | INFERRED |
| 57173B / 57627B | `GFILI` / `GVERS` | Get file index / get version | INFERRED |
| 57527B / 57567B | `GPREV` / `GNEXV` | Get previous / next version (version chain walk) | INFERRED |

## Directory / user-entry access

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 107106B / 107111B | `WDIEN` / `GDIEN` | Write / get directory entry (GDIEN = MON 244) | [`244B-GetDirEntry`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/244B-GetDirEntry/README.md) |
| 30225B / 30235B | `GDIRA` / `GNAMA` | Get directory address / name address | INFERRED |
| 47653B / 47716B | `GMAIN` / `WDIRE` | Get main directory / write directory label | INFERRED |
| 107401B / 107403B | `RESDI` / `RELDI` | Reserve / release directory | INFERRED |
| 53174B / 53243B | `TUSEN` / `FUSEB` | Test / find user block | INFERRED |
| 53246B / 53410B | `RUSER` / `WUSER` | Read / write user entry | INFERRED |
| 55111B | `GUSEN` | Get user entry | [`214B-GetUserName`](../../../tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/214B-GetUserName/README.md) |
| 55206B | `CUSED` | Change pages-used (user quota accounting) | VERIFIED (called by `GPAGE`; see [`allocation.md`](allocation.md)) |
| 62206B / 62314B | `CHNUS` / `INSUS` | Change / insert user entry | INFERRED |

## Page bitmap (allocation) primitives

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 76205B | `GPAGE` | Get (allocate) one file page - the core allocator | VERIFIED ([`allocation.md`](allocation.md)) |
| 50627B / 50635B | `ALPAG` / `RLPAG` | Mark one page used / free in the bit file | VERIFIED |
| 50632B | `XRLPA` | Release page variant (SSK=0, SSM=1) | VERIFIED |
| 51120B | `RSPAG` | Reserve/set a page **range** (contiguous allocation) | VERIFIED |
| 51025B | `TPAGF` | Test whether a given page is free | VERIFIED |
| 51353B / 51355B | `TESTB` / `TESTP` | Test bit / **scan for a free page** (downward) | VERIFIED |
| 60147B / 60151B | `DLSPA` / `DLPAG` | Release / delete page(s) from a file | INFERRED |
| 74510B | `DFPAG` | Deallocate/free page | INFERRED |
| 74443B / 74513B / 74516B / 74521B | `GPUPI` / `GPCOP` / `GPREA` / `GPADR` | Get-page-unit-pointer / copy / read / address helpers | INFERRED |
| 76013B / 76037B | `WBACK` / `MASKE` | Write bitmap word back / build bit mask | INFERRED |

## Index blocks (indexed files)

| Addr | Sym | Role | Anchor |
|------|-----|------|--------|
| 51451B / 51453B | `GP5IX` / `RINDX` | Get index / read an index block (shared body 51454B) | VERIFIED |
| 52066B | `FINDX` | Find index - walk an index block for a file page's disk block | INFERRED |
| 52163B | `PRKEY` | Process key (index-block key handling) | INFERRED |
| 52501B | `WINDX` | Write an index block | INFERRED |

## Queue / lock plumbing (context, not FS structure)

`FCSTA` 26000B, `INIQ` 26003B, `RELQ` 26044B, `WLOCQ` 26106B, `LOCQ` 26116B,
`REAQ` 26125B, `WRIQ` 26130B, `APPQ` 26134B, `TAKQ` 26146B, `UNLQ` 26277B - the
file-system request-queue and per-file lock primitives the I/O paths run on.
Roles INFERRED from the symbol names; the queue-entry layout is OPEN.

---

**Last updated:** Phases 5-6. Evidence base: carved `006-S3FS` SINTRAN L bytes,
already-carved MON workers, NDFS C library cross-check, ND-60.128.5 docs.
