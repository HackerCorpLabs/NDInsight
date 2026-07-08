# Place Library (PLACE-BIG-2B-C01) - Q5 / Contradiction C9 findings

**Date:** 2026-07-08
**Binary:** F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\500\PLACE-BIG-2B-C01.BRF
(ND-100 BRF object, loaded in Ghidra; language ND-100:BE:16)
**Task:** handoff Q5 (segment-capability word layout) and dossier contradiction
C9 (11-bit vs 12-bit physical-segment field, W/P/S bit positions).
**Method:** static string + hexdump analysis in Ghidra. Byte offsets are Ghidra
ram: addresses. Numeric bit positions that are NOT yet proven from a mask/bit-test
are marked UNVERIFIED.

> STATUS: PARTIAL. This advances C9 with real binary evidence (the capability
> word's bit-NAME inventory and the logical-address field widths, verified in the
> shipped Place Library) but does NOT yet assign numeric bit positions to W/P/S.
> That still needs either the capability-decoder loop in this binary or the
> ND-05.009.4 MMS chapter. Do not upgrade C9 to RESOLVED on this note alone.

---

## 1. What PLACE-BIG-2B actually is

Only 11 functions, but a very large string section: this variant is dominated by
the **physical-segment / Memory-Management-System (MMS) inspection and
hardware-fault reporting** code (LOOK-AT-PHYSICAL-SEGMENT, domain information
table dump, SAMSON/500 hardware-fault decode). That is precisely the code that
decodes and prints capability words, so it is the right place for C9.

Evidence (strings): "$Domain information table" (ram:1e31), "Contents of Physical
segment Table" (ram:1e67), "$ *** SAMSON HARDWARE FAULT ***" (ram:2711),
"$ *** 500 HARDWARE FAULT ***" (ram:2795).

---

## 2. Capability / status bit-name inventory (verified strings)

The bit-name table lives at ram:20e3-2199. NUL (0x00) bytes separate DISTINCT
status words; within a word, the `$`-delimited substrings are consecutive bit
descriptions. Parsed from the hexdump:

```
Word A (ram:20e3, NUL-terminated 0x20e8+... at 0x20eb):
    "$ Lock request"
    "$ Waiting dirty request"

Word B (ram:20f6):
    "$ Current ALT bit"

Word C (ram:20fe):
    "$ Capability - written in page updated"
    "$ Write permitted"

Word D (ram:2135 onward):
    "$ Parameter access permitted"
    "$ Shared segment"
    "$ USED bit"
    "$ Physical segment used"        (string truncated in listing as "use...")
    "$ Inhibit cache write"          (ram:2166)
    "$ TSB miss"                     (ram:2171)

Word E (debug register matches, ram:2178+):
    "$ (A) DOM register match DDOM"
    "$ PS register match DPS"
    "$ LA register match DLA"
```

The **segment-capability word** is Words C+D: the flags dossier C9 cares about
are all present and named:

| Dossier name | Place-Library bit name | ram offset |
|---|---|---|
| W (write) | "Write permitted" | 0x2226 |
| P (parameter) | "Parameter access permitted" | 0x2135 |
| S (shared) | "Shared segment" | 0x2152 |
| (used) | "USED bit" | 0x2161 |
| (phys-seg-used) | "Physical segment used" | 0x2168 |
| (cache) | "Inhibit cache write" | 0x2166 area |
| (written/updated) | "Capability - written in page updated" | 0x2106 |

This CONFIRMS the existence and names of W/P/S as distinct capability bits, and
adds several the dossier did not list (USED, Physical-segment-used, Inhibit cache
write, Capability-written/updated, TSB miss). It does NOT yet fix their numeric
positions.

UNVERIFIED: exact bit NUMBER of each. The '$'-scan order gives RELATIVE order
within each word, but whether the decoder counts bit 0->15 or 15->0, and how
Words C/D pack into the 16-bit capability word, needs the decoder's shift/mask
(not yet located - the table is reached by a computed base, no direct xref).

---

## 3. Logical-address field widths (bears on the 11 vs 12-bit question)

Two MMS index-error strings pin the ND-500 logical-address structure:

- ram:1ea2: "Logical address bits 26-11 >< 0 and index level 0"
- ram:1ebb: "Logical address bits 26-21 >< 0 and index level 1"

Reading: the logical address is **27 bits (bits 0-26)**. The MMS index is
two-level; at index level 0 the significant split is at **bit 11**, at index
level 1 at **bit 21**. The recurring "bit 11" boundary is consistent with the
dossier C9 Claim A (physical-segment field = bits 0-10, 11 bits, with W/P/S
starting at bit 11), and is evidence AGAINST a 12-bit field boundary at bit 12.
UNVERIFIED that this address-index boundary is the SAME field as the
capability-word segment number; it is suggestive, not conclusive.

---

## 4. Related MMS / capability structures named in this binary

Useful anchors for a follow-up pass (all verified strings):

- "Zero in capability table" / "(DMM and ALT PV)" / "(DMM and write PV)"
  (ram:1fe7-2008) - there is a CAPABILITY TABLE; "PV" = protect violation, and
  the ALT-PV / write-PV wording ties protection bits to the capability entry.
- "Zero in index page for process segment", "Zero in PST" (ram:2045) - Physical
  Segment Table + per-process index page.
- "Indirect capability to other machine", "Indirect capability..." (ram:1fb6) -
  matches the PROG string "other-machine"/"indirect-seg" capability names seen in
  ND-500-MON-J04:PROG (BANK2::4188-41ba), cross-confirming the capability-flag
  vocabulary across two binaries.

---

## 5. What remains to close C9

1. Locate the capability-word decoder loop (the routine that walks Words C/D and
   prints via the ram:20e3 table) and read its bit mask/shift -> assigns numeric
   bit positions to W/P/S. It is reached by computed base; find it by scanning
   for a load of an address in 0x2100-0x2140 or by tracing the LOOK-AT-PHYSICAL-
   SEGMENT / domain-info-dump handler.
2. Cross-check against ND-05.009.4 (ND-500 Reference Manual) MMS chapters, which
   are the authoritative source the dossier C9 says it lacked.
3. Only then upgrade dossier C9 from UNRESOLVED.

Deliverable status: capability bit-NAME set and address-field widths are new,
shipped-binary evidence; numeric W/P/S positions are still open.
