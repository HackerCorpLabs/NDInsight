# L-VSX-500-07 — cross-check against the carve

What the distribution floppy says versus
[`../../../sintran-segment-carver/versions/L-VSX-500/`](../../../sintran-segment-carver/versions/L-VSX-500/).

Facts are marked **[VERIFIED]** when read directly out of a file, and
**[INFERRED]** when derived.

---

## The mapping rule

**[VERIFIED]** The `:DATA` stream states it itself:

> `NOTE THAT THE ABOVE SYMBOLS STATES BLOCK ADDRESSES FOR THE RESPECTIVE
> AREAS AND MUST BE CALCULATED FROM PAGE NUMBER`

So the layout parameters are **SEGFIL page numbers** — the same quantity as
`madr` in `segment-facts.json`.

**[INFERRED]** but exact on 28 of 32 parameters: take the leading additive
terms in octal and discard the trailing `-N` (a length / in-page offset, not
part of the address), then subtract the image base `0o200`:

```
madr = (sum of leading additive terms, octal) - 0o200
```

## Agreement

**[VERIFIED]** 28 exact matches against carved `madr`:

| parameter | raw | page | madr | carved segment |
|---|---|---|---|---|
| `PCCS` common code/start | `200` | 0o200 | 0 | `S3IMAGE` |
| `PSY` system segment | `355-62` | 0o355 | 0o155 | `S3SSYS` |
| `PSPD` spooling datafields | `360-72` | 0o360 | 0o160 | `S3SSPD` |
| `PXP` ext common | `361-13` | 0o361 | 0o161 | `S3SECOM` |
| `PRP` / `PMP` / `PIP` | `363` / `446` / `531-15` | | 0o163 / 0o246 / 0o331 | `S3SRPIT` / `S3SMPIT` / `S3SIPIT` |
| `P5P` / `P5M` | `200+414-13` / `200+421-20` | | 0o414 / 0o421 | `S3S5PIT` / `S3SSM5` |
| `PSTB` segment table | `200+501` | | 0o501 | `S3SSGT` |
| `PU1` / `PU2` | `200+521` / `200+561` | | 0o521 / 0o561 | `S3SU110` / `S3SU120` |
| `PFS` / `PCS` / `PSM` | `200+621` / `706` / `773` | | 0o621 / 0o706 / 0o773 | `S3SFS` / `S3SCP` / `S3SSM` |
| `PXK` / `PXR` / `PXF` | `200+1037` / `1067` / `1137` | | 0o1037 / 0o1067 / 0o1137 | `S3SXMK` / `S3SXROU` / `S3SXMFI` |
| `PNM` / `PDM` / `PNS` / `PNN` | `200+1225` / `1233` / `1237` / `1323` | | | `S3SDNAM` / `S3SDMWD` / `S3SNKSE` / `S3SNKNA` |
| `PEC` / `PED` / `PPM` / `PEX` / `PBO` / `PMT` | `200+1423`…`1767` | | | `S3SERWC` / `S3SERWD` / `S3SPRMA` / `S3SEVMS` / `S3SBOPC` / `S3SMTSE` |

## Actionable findings

### 1. Segment 61 has a copy-pasted description — **fix**

**[VERIFIED]** `segment-facts.json` gives segments **60 and 61 the same**
`description: "Save of XMSG kernel"`. The floppy separates them:

| parameter | page | segment | correct meaning |
|---|---|---|---|
| `PXK=200+1037` | 0o1037 | `S3SXMK` | XMSG **kernel** |
| `PXR=200+1067` | 0o1067 | `S3SXROU` | XMSG **XROUT** |

Segment 61's description should be *"Save of XMSG XROUT"*, per the macro
legend (`PXRO` → XROUT) in `inputs/distribution-layout-params.txt`.

### 2. Thirty medium-confidence segments can be promoted — **update**

**[VERIFIED]** 30 records carry `confidence: "medium"` *solely* because the
OCR'd manual §8.3 name disagreed with the live `LIST-SEGMENT` name — e.g.
manual `#74 name=S3IDMWD` against live `S3SXMK`, because the manual's rows
are shifted by one in that region (`EXTRACTING-SEGMENTS.md` §8 documents the
OCR damage).

The floppy is a **third, independent, non-OCR witness** and it backs the
live names. Those 30 can go **medium → high**, citing this file.

### 3. A parity/OCR corruption in the repo's symbol table — **fix**

**[VERIFIED]** `SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT`
line 6140 reads `ENT0-011302` (hyphen). The floppy has `ENT0=011302`.
This is the **only** differing line across all five comparable tables — the
rest are byte-identical, which is itself a useful confirmation that the
repo's symbol tables are sound.

### 4. An open conflict — **flag, do not "fix"**

**[VERIFIED]** `PRD` is documented in the floppy legend as *"RESIDENT DATA
PART ONE"* and resolves to page 0o300 → madr 0o100. The carve names madr
0o100 `S3SDPIT` — *"Save of DPIT"*.

These disagree. **Do not rename anything on this basis.** It may be that
the resident-data area and the DPIT save area overlap, or that the `-2`
suffix in `PRD=300-2` changes the interpretation. Record as `[OPEN]`.

### 5. Parameters below the image base — **unresolved**

**[VERIFIED]** `PPOP=1`, `PEP=100-14`, `PRL=137-14` sit *below* the `0o200`
base, so the `-0o200` rule does not apply. Their nearest carved neighbours
`S3SERRP` (0o77) and `S3SRTC` (0o136) are off by exactly one page.
**[INFERRED]** these address a bootstrap / error-program / RT-loader
pre-image area with a different convention. Not resolved.

---

## Not worth doing

**Re-importing the symbol tables.** Already in the repo byte-identical and
already applied — 98,120 placements across 70 `segments-ref` bundles.
Source breakdown: SYMBOL-1-LIST 80,284 · N500-SYMBOLS 7,988 ·
SYMBOL-2-LIST 3,947 · RTLO 2,809 · FILSYS 1,788 · XMSG 1,304.

## The bigger prize, not yet attempted

The remaining ~1.08 MB of `SINTRAN-L-1:DATA` after the 7,457-byte header is
the **as-shipped, un-patched system** in BPUN loadable form (first control
byte at offset 7897; 273,595 of 1,095,538 bytes have the high bit set).

The carve is of an **installed and site-patched** system — it contains a
segment `S3PATCH` at madr 0o4506. Decoding the 22 `)9READ` blocks and
diffing them against the carved `.bin` files would separate *what ND
shipped* from *what this installation became*, byte by byte.

Nothing in the current carve can do that. It needs a BPUN block decoder for
the `)9READ` stream; the block format inside the stream is **not yet
verified**, so a clean diff is not guaranteed.
