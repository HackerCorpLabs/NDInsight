# ND-210412 — ND-500/1 CXA Microprogram

> Status: IN-PROGRESS — real floppy decoded; notable cross-reference to the ND-500 microcode preservation project

| Field | Value |
|-------|-------|
| ND article number | `ND-210412` |
| Product name | ND-500/1 CXA Microprogram |
| Functional category | Hardware / Microcode (not a language/dev tool — logged here as it turned up in the same floppy sweep) |
| CPU target | ND-500/1 (CXA control unit) |
| Related products | `ND-805013` Single Precision Array Processing Functions (see [../../../Reference-Manuals/500/ND-805013-3 EN ND-500 Single Prec. Array Proc. Func..md](../../../Reference-Manuals/500/ND-805013-3%20EN%20ND-500%20Single%20Prec.%20Array%20Proc.%20Func..md)) — `ND-500-APF-LIB` must be loaded together with this microprogram. |

## What is known — real floppy, decoded

Floppy `210412F01-XX-01S` (downloaded via NDwiki, imaged by Torfinn "Tingo" Ingolfsen) mounts
cleanly, files owned by user `SYSTEM`:

```
CONT-STORE-10611:DATA      the microcode control-store image itself
ND-500-APF-LIB-E:NRF       the Array Processing Function library (see ND-805013 above)
```

**`CONT-STORE-10611:DATA` matches the exact filename this repo's ND-500 microcode preservation
work already treats as "the only surviving real ND-500 (144-bit microword) control store"** — see
the `nd500-microcode` skill and `E:\Dev\Ronny\ND500UC`. This floppy is very likely a — or *the* —
original source for that file, found independently via a different route (NDwiki, not the
`floppies.json` reference library). **Not cross-checked byte-for-byte against the copy already in
use by that project** — if the two differ at all, that would be significant and worth flagging to
that project directly; this catalog entry only records that the file exists here too.

## Documentation
- No PD sheet, no PI sheet located in this catalog's archive for this specific article number.

## Provenance & open items
- Source: one real floppy image, downloaded via NDwiki and decoded in this session (`ndfs -t`).
- **TODO:** byte-for-byte comparison against the `CONT-STORE-10611.DATA` already used by the
  ND-500 microcode toolchain has not been done — see the `nd500-microcode` skill for that
  project's own verification method (round-trip disassemble/reassemble/byte-compare).

---
**Parent:** [../README.md](../README.md) (Software catalog)
