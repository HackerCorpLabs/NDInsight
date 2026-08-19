# ND-500(0) File-Format Viewer

A single-page, vanilla-JS, no-build-step browser tool that loads an NRF, DOM, SEG, or
DESC file (drag-and-drop or file picker) and shows a parsed, human-readable view next to
a hex dump, with click-to-highlight between the two.

It is **not** a standalone artifact - it fetches the three JSON format specs
(`../nrf-format.json`, `../dom-format.json`, `../desc-format.json`) live from the parent
`File-Formats/` folder at load time, so those JSON files stay the single source of truth.
Field names, descriptions and bit-table labels shown in the UI come from the JSON, not
from a copy baked into this page.

## How to run it

Browsers block `fetch()` of a sibling folder from a `file://` page (CORS), so this must be
served over `http://`, not double-clicked open directly.

**Windows: double-click `run.bat`** in this folder - it starts the server from the right
directory and opens the page in your default browser.

Manual equivalent, from the **`File-Formats/`** folder (one level up from this one - so the
viewer's `../*.json` fetches resolve correctly):

```
cd SINTRAN\File-Formats
python -m http.server 8888
```

then open `http://localhost:8888/viewer/` in a browser.

## What it does

- **Format detection**: filename extension (`.nrf`, `.dom`, `.seg`, `.desc`, or
  `DESCRIPTION-FILE` in the name) combined with a structural check - DOM/SEG via the
  FLAGS byte at offset 0x06 (IS_ND500 bit), DESC via the same heuristic name-field scan
  `nd500-dump.c` uses, NRF via a first-group sanity decode. A **Format** dropdown lets you
  override auto-detect if it guesses wrong.
- **NRF**: full control-group stream listing (control name, NL, raw/decoded numeric
  value, symbol) plus a module/symbol summary (BEG..END spans with their LIB/DEF/REF/
  LRF/DDF/DRF/MSG symbols) - matches `nd500-dump -N`'s level of detail. Ports
  `nrf_utils.c`'s `nrf_read_group()`/`nrf_scan_modules()` group-by-group (not the
  linear-buffer reconstruction layer - out of scope for an inspector).
- **DOM/SEG**: file header, mother/child domains, segment table (DOM) or program/data
  parts + ND-100 RT segments + linked segments (SEG), indirect segments, common part -
  with names resolved from the name pool and flag/attribute bytes decoded to their
  documented bit labels. Byte offsets ported from `nd500/dom.h`'s struct layout.
- **DESC**: domain and segment entries found by the exact heuristic block-scan algorithm
  from `nd500-dump.c` (`looks_like_desc_name` / `desc_scan_block`, ported line-for-line).
  Only the two fields the C code itself trusts per entry - `SEGLINK` and the
  apostrophe-terminated name (`DNAME`/`SNAME`) - are decoded. Every other DESC field
  (`CHILDDOMAINS`, `PLB`/`PSIZE`/`DLB`/`DSIZE`, the cross-domain-ref array, etc.) is
  explicitly marked unverified in `desc-format.json`/`DESCRIPTION-FILE-FORMAT.md` and is
  **not shown** here rather than guessed at.
- **Hex dump**: paginated (1024 bytes/page - some real `.DOM` files are several MB, so the
  whole file is never rendered as one giant `<pre>`), octal or hex addresses, a jump-to-
  offset box, and click-to-highlight from any parsed field.
- Malformed/unrecognized files show a clear error panel (parsing is wrapped in
  try/catch) instead of crashing or rendering garbage.

## Validated against

All three parsers were exercised against real files by extracting the exact same
group-reader / heuristic-scan logic into a throwaway Node.js script and running it
(no browser screenshot was taken - see "Known limitations" below):

- **NRF**: `nd-500-apf-lib-e.nrf` (18173 bytes, from the ND-500 microcode set, outside this repo) - decodes to
  **5716 control groups, 53 modules**, with symbol names `VADDXXX`, `VSUBXXX`, `VMULXXX`,
  `VDIVXXX`, `VMAXXXX`, `VMINXXX`, `VMAXMGX`, `VMINMGX`, ... matching the expected
  APF/vector-math library pattern exactly.
- **DESC**: `description-file.desc` from the vendor floppy `210319H02-XX-01D` (outside this repo)
  (22528 bytes) - heuristic scan finds domain block at offset 0x100 (2 entries:
  `SCRATCH-DOMAIN`, `LINKAGE-LOAD-H02`) and segment block at offset 0x4000 (2 entries:
  `(210319H02:FLOPPY-USER)SCRATCH-SEG-01`, `(210319H02:FLOPPY-USER)LINKAGE-LOAD-H02`) -
  exactly matching the task's expected domains/segments.
- **DOM**: `SINTRAN/ND500-APPS/CONVERT-DOM-A03/files/CONVERT-DOM-A03.DOM`
  (339968 bytes) and `SINTRAN/ND500-APPS/LINKER-B01/files/LINKER-B01.DOM`
  (724992 bytes) - both decode FLAGS=0xF0/0xF8 (IS_DOMAIN_FILE | IS_ROOT_DOMAIN |
  IS_SINTRAN_III | IS_ND500, +TRAPBLOCK_VALID on LINKER-B01), one used segment slot each
  with plausible program/data `lb`/`sz`/`att` (program ATT decodes to `PROGRAM_SEGMENT |
  SEGMENT_USED`; data ATT decodes to `WRITE_PERMIT | PARAMETER_ACCESS |
  SWAP_ON_SWAPFILE | CACHE | SEGMENT_USED` - both structurally sane). LINKER-B01's used
  segment index is **22**, which independently matches a live cross-check note already in
  `desc-format.json` ("CONVERT-DOM-A03 reported LINKAGE-LOAD-H02's logical segment as 22
  during a real conversion run").
  `linkage-load-h02.pseg` (a raw PSEG, not a DOM file) was deliberately **not** used to
  test DOM parsing, per the task's own warning.

The page's inline `<script>` was also checked with `node --check` for syntax errors, and
the local `python -m http.server` was confirmed to serve both `viewer/index.html` and
the sibling JSON specs correctly (HTTP 200).

## Known limitations / gaps

- **No browser screenshot was taken.** Validation above was done by extracting the
  parser logic into a standalone Node.js script and running it against the real files
  (asserting the exact numbers this task asked for), plus a `node --check` syntax pass
  and an HTTP reachability check of the served page - not by visually inspecting the
  rendered UI in a browser.
- DESC fields beyond `SEGLINK`/`DNAME`/`SNAME` are not decoded at all (by design - they
  are unverified in the spec itself). Domain Entry's `CHILDDOMAINS`/`MOTHER`/etc. and
  Segment Entry's `PLB`/`PSIZE`/`DLB`/`DSIZE` onward are simply absent from the UI.
- NRF's Layer-3 linear PSEG/DSEG reconstruction (`nrf_reconstruct()` in the C reference)
  is not ported - this viewer shows the group stream and module/symbol summary only, not
  a reconstructed byte buffer.
- DOM/SEG format detection's "magic" is a heuristic (FLAGS byte IS_ND500 bit + file
  length >= 4096), since the format has no literal magic number field in the spec.
- The hex dump highlights only the exact byte range a parsed field claims; it does not
  attempt bit-level highlighting within a byte (unlike ND500UC's microcode-field bit
  boxes, which operate on a 144-bit word where sub-byte fields are the norm - none of
  these three formats pack multiple named fields into one byte outside NRF's control
  byte, which the group listing already labels as controlNumber+NL together).
