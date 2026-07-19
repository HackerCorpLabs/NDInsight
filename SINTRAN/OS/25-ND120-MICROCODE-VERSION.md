# ND-120 Microcode Version: Identification and the 013/K Question

**Scope**: How to identify the ND-120 CPU microcode ("microprogram") version, what
version the carved SINTRAN L-VSX-500 (L07) system actually carries, and how to
obtain the older **013/K** microcode.

**Status**: Investigation complete. VERIFIED / UNCERTAIN tags used throughout.

---

## 1. The version byte (VERIFIED)

The ND-120 microword is 64 bits wide. The microcode **version is stored in the low
8 bits of the 64-bit microword at microword address octal 020** (decimal 16, the
17th microword, zero-based).

Calibrated against the known-good ND-120 EPROM dump (version **L**) in the ND-120
FPGA recreation repo (`Code/Microcode/` -> `wcs_image.hex`, line number = CSA/WCS
word address):

```
microword o020 = 7b80 0080 8006 800c   (64-bit)
low 8 bits     = 0x0c = octal 014 = decimal 12
```

**The low-8 value is the alphabet position of the version letter:**

| low-8 (hex) | octal | decimal | letter | "revision/letter" |
|:-----------:|:-----:|:-------:|:------:|:-----------------:|
| 0x0b        | 013   | 11      | K      | **013/K**         |
| 0x0c        | 014   | 12      | L      | 014/L             |

So "013/K" literally means *microword-020 low-8 = octal 013 = K*, and the L EPROM
reads octal 014 = L. Mapping VERIFIED against the L EPROM.

### Recipe to read the version from a raw ND-120 microword array

For a raw microword image stored as **big-endian 16-bit ND-100 words** (the on-disk
ND word order), where a 64-bit microword = 4 consecutive 16-bit words:

1. Word packing (per the EPROM->WCS mapping `word = {P(RF=3),P(RF=2),P(RF=1),P(RF=0)}`,
   RF=0 -> bits[15:0]): the **first** 16-bit word of the four is the low group
   (bits 15:0), the fourth is the high group (bits 63:48).
2. Microword o020 starts at word offset `16 * 4 = 64` (byte offset `128`) from the
   start of the array.
3. Version = **low byte of the first of those four 16-bit words** (bits 7:0 of the
   RF=0 group). In a big-endian `.bin` that is the *second* physical byte of that
   16-bit word (e.g. word `0x800c` is stored `80 0c`; the version is `0c`).
4. Decode: octal 013 = K, octal 014 = L (low-8 = alphabet position).

---

## 2. What the carved L07 system actually carries (VERIFIED)

The carved segments (see
[../../tools/sintran-segment-carver/versions/L-VSX-500/segments/](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/)):

| Segment | File | Description | Contents |
|:--------|:-----|:------------|:---------|
| 0113 | `113-S3IU120.bin` | "Image of ND-120 Microprogram" | ND-100 **loader program** code |
| 0112 | `112-S3SU120.bin` | "Save of ND-120 Microprogram"  | loader data / save area |

**IMPORTANT (VERIFIED): these segments are NOT a raw 64-bit microword array.**
The premise that microword o020 can be indexed directly out of segment 112/113 does
not hold for this system:

- Segment `113-S3IU120.bin` (32768 bytes) is ND-100 instruction code throughout.
  All four 16-bit stride-4 phases have identical entropy (~9.8 bits) and share the
  same top values (`0xa803`, `0xcc1d`, `0x0902` = ND-100 opcodes). There is no
  low-entropy "tag column" anywhere, i.e. no periodic 64-bit microword structure.
- It contains the ASCII reference `(SYSTEM)MACM-AREA:DATA` -- the loader's runtime
  microcode area.
- On the running L07 system disk (`SMD0.IMG`), `(SYSTEM)MACM-AREA:DATA` is **0 bytes**
  (a 64-page scratch area, empty on disk -- populated only at load time).
- Neither carve matches the L EPROM in any byte arrangement (LE/BE, hi-first/lo-first,
  byte-plane), so the microcode is embedded in the loader in a serialized/packed
  form, not a plain array.

**Conclusion (VERIFIED by distribution manifest):** segment 113 (`S3IU120`) is the
loaded image of the ND-120 microprogram loader **MACM-1718L**. The L07 distribution
floppy `250305L07-XX-01D` ships `(SYSTEM)MACM-1718L:BPUN` -- so this system's ND-120
microcode is **version L (014/L)**, the same version as the ND-120 EPROM dump. It is
**NOT 013/K.**

> UNCERTAIN: reading the exact microword-020 byte out of the *carved* image would
> require reversing MACM's embedded/serialized microcode payload (or running the
> loader and dumping the WCS). The version is established here from the distribution
> file name (`MACM-1718L`) rather than from a raw microword read, because no raw
> microword array is present in the carve.

---

## 3. Where 013/K lives, and how to get it (VERIFIED locations)

The ND-120 microprogram ships as `(SYSTEM)MACM-1718<letter>:BPUN` (a ~24-page
ND-100 BPUN loadable -- the loader with the microcode payload). Indexed from the ND
floppy archive (`~/repos/ndfloppy`, `floppies.json` / `floppifiles.json`):

| Microcode | File | Floppy image label(s) |
|:----------|:-----|:----------------------|
| **013/K** | `(SYSTEM)MACM-1718K:BPUN` | **`N-10-102-I`** (only), paired with `SINTRAN-I:DATA` |
| 014/L     | `(SYSTEM)MACM-1718L:BPUN` | `250305L07-XX-01D` (our L07), `250306M06-XX-01D` (M06), `N-250306K05--01D` (K05), `N-220046K03--01D` (K03), `ND-BFLY-SIN-J01` (J01), `N-100-1001-I`, `N-100-1002-I`, `N-102-2921-I` |

**Key insight (VERIFIED):** the microcode letter is *independent* of the SINTRAN
release letter. SINTRAN **J01, K03, K05, L07 and M06** distribution floppies all
ship **MACM-1718L**. So "go to an older SINTRAN version" (K03/K05) does **not** yield
the K microcode -- those already carry L. The **013/K** microprogram is found on
exactly one archived floppy: **`N-10-102-I`** (an early SINTRAN-I release).

### Plan to obtain 013/K

1. **Get the floppy image `N-10-102-I`.** The `ndfloppy` repo holds only JSON
   listings, not the images; the images are hosted at the storage backend referenced
   by its deploy script (`ndlib` / `https://ndlib.hackercorp.no`). Fetch the
   `N-10-102-I` image from there (or any physical/archived copy of that floppy).
2. **Extract the file:**
   `ndtool -x "(SYSTEM)MACM-1718K:BPUN" N-10-102-I.<img>`
3. `MACM-1718K:BPUN` is the loader-with-payload (same shape as segment 113), **not**
   a raw microword array. To recover raw microwords for a direct o020 read, either:
   - **(a)** diff `MACM-1718K:BPUN` against `MACM-1718L:BPUN` (extractable from the
     L07 floppy `250305L07-XX-01D`); the differing region is the microcode payload,
     and the version byte sits where microword o020 maps; or
   - **(b)** load/interpret the MACM loader to populate the WCS, then dump the WCS
     and index microword o020 with the recipe in section 1; or
   - **(c)** if a K-version EPROM dump (AM27256 pair) surfaces, apply the section-1
     recipe directly (fastest, most reliable).
4. Confirm success: microword o020 low-8 = **0x0b (octal 013) = K**.

---

## 3B. MACM-1718K (013/K) — verified

A copy of `MACM-1718K.BPUN` (31916 bytes) was analysed directly. Findings below use
VERIFIED / UNCERTAIN tags.

### BPUN container (VERIFIED)

The file is a standard ND **BPUN** (bootable-punch loader). Layout:

| Region | File offset (bytes) | Contents |
|:-------|:--------------------|:---------|
| Null leader | `0x000`..`0x07e` (127 bytes) | zero fill |
| Preamble / bootstrap | `0x07f`..`0x1ad` | octal loader words as 7-bit-ASCII-with-parity (mask `0x7F`), one per `CR/LF` group |
| `!` start-of-data marker | `0x1ae` | `0xA1` (`'!' | parity`) |
| Load block header + data | `0x1af`.. | address, count, data, checksum, action (see below) |
| Trailer | `0x7c2f`..`0x7cab` | leftover leader/message bytes |

Preamble decode (mask `0x7F`) is a sequence of octal bootstrap words
(`160616 146101 134021 146157 134017 ...`) — the toggle-in loader, **not** a version
string. Parsed per the BPUN rules: **Start (exec) = octal 137777**, **Boot = octal
160616**. There is no literal "K"/"013"/date banner; the file's version identity is its
name `MACM-1718K` (see section 3) plus the driver text below.

### Load block map (VERIFIED)

Exactly **one** load block:

| Field | Value |
|:------|:------|
| Block header | file offset `0x1af` |
| Load address | **octal 101304** (decimal 33476) |
| Word count | **15676** words |
| Data | file offset `0x1b3`..`0x7c2e` (31352 bytes, big-endian 16-bit ND words) |
| Checksum (file) | octal 143623 |
| Checksum (recomputed) | octal 143623 — **valid** |
| Action | 0 (execute at Start) |
| Loaded span | octal **101304 .. 137777** (decimal 33476..49151) |

The reconstructed loaded image is extracted to
[../../tools/sintran-segment-carver/versions/L-VSX-500/re/MACM-1718K-loaded-image.bin](../../tools/sintran-segment-carver/versions/L-VSX-500/re/MACM-1718K-loaded-image.bin)
(31352 bytes, big-endian 16-bit ND words, load address octal 101304, sha256
`8f0406969b6d3188b8b781713f00d8070bb6e82cae92a321321adaf95f79ad95`).

### What the block actually contains (VERIFIED)

The block is the **MACM WCS-loader program**, not a flat microword array. Masked-ASCII
strings in the data are an ND-100 **disk driver** and its error handler:
`DEVNO, 0 % DEVICE NUMBER (01540 OR 01550)`, `CY, 0 % CYLINDER`, `SRFSC`, `BANKN`,
`ADRES`, `WORDC`, `RSCON, 0 % SEEK CONDITION`, `REGI, 0 % BITS 017-011 ARE ERROR
INDICATORS`, and the message `BELOW FOLLOW 11 WORDS THAT CONTAIN ERROR INFORMATION
ABOUT THE LAST ERROR`. So MACM reads/loads the microcode into the WCS at runtime.

### Microword o020 could NOT be read directly (UNCERTAIN)

The premise that microword octal 020 can be indexed out of the BPUN payload does **not**
hold here — same structural result as the L carve in section 2:

- The data payload is **31352 bytes**, far smaller than a full 8192-microword × 8-byte
  array (64 KB), so the microwords are **not** stored as an uncompressed 64-bit array.
- A byte-signature search for microword o020 — both the K pattern
  (`...800b`) and the L pattern (`7b8000808006800c`) — finds **nothing** in ND-word
  big-endian, ND-word little-endian, or reversed group order.
- A longest-common-substring correlation of the known-good **L** microcode against this
  payload (P-stream `hi<<8|lo` BE and LE, `lo`-plane, `hi`-plane, plane concatenations)
  yields **no significant common run** (best 23 bytes, coincidental). The microcode is
  therefore embedded in the loader in a serialized/shuffled (or compressed) form, not the
  EPROM/WCS byte layout.
- The carved L loaded image `113-S3IU120.bin` shares **zero** common run with this K
  payload and lacks these driver strings, so it is not a usable byte-level L reference
  for a diff either.

**Conclusion:** the BPUN container, single load block, checksum, and loader identity are
**VERIFIED**; the file is the **013/K** microprogram by name/provenance (section 3).
Directly reading microword o020 low-8 = `0x0b` to confirm K from this file is **not
possible by inspection** — it requires reversing MACM's embedded/serialized microcode
payload, running the loader to populate and dump the WCS, or obtaining a K-version EPROM
dump (then apply the section-1 recipe). A byte-level **K vs L microword delta** likewise
awaits a comparable L artifact (a `MACM-1718L:BPUN` or a K/L EPROM pair); the carved
`113-S3IU120.bin` is not byte-compatible with this loader and cannot serve as the L side.

---

## 4. Not the microcode (disambiguation)

These similarly named SYSTEM files are **development tools, not ND-120 CPU microcode**:

- `MAC-1628C`, `FMAC-1408D`, `F32-MAC-1628C`, `F32-FMAC-1920C` -- the MAC macro/micro
  **assembler** toolchain (there is a whole `(BPUN-FILES)` directory of these).
- `DMAC-1915G:BPUN` -- a segment debugger/dump tool (strings: "PREVIOUS B",
  "RETURN ADDRESS", "segment is not modifyable").
- `MICRO-5x00-*`, `MIC-5x00-90-500`, `MIC-REGISTER`, `MIC-SEQUENCE` -- **ND-500**
  microcode/microtest, not ND-120.

Only `MACM-1718<letter>` is the ND-120 CPU microprogram.

---

## References

- ND-120 EPROM / WCS calibration source: ND-120 FPGA recreation repo,
  `Code/Microcode/wcs_image.hex` (version L), microword field layout in
  `Verilog/CPU-BOARD-3202/circuit/CPU_PROC_CGA_33.v`.
- ND-06.031.1 ND-110 and ND-120 Microprogrammer's Guide (microword field semantics).
- Carved segments: [../../tools/sintran-segment-carver/versions/L-VSX-500/segments/](../../tools/sintran-segment-carver/versions/L-VSX-500/segments/)
  (`113-S3IU120.bin`, `112-S3SU120.bin`) and
  `versions/L-VSX-500/segment-facts.json`.
- ND floppy archive index (`floppies.json` / `floppifiles.json`) for MACM-1718K/L
  floppy locations.
