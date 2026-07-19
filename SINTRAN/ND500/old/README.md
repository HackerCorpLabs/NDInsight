# Retired ND-500 Documents

**Status: RETIRED - DO NOT USE AS SOURCES**

The documents in this folder were retired on 2026-07-07 during the ND-500 bus interface
documentation overhaul. Each contains claims that are fabricated, contradicted by the
SINTRAN III NPL source code, or fully superseded by other documents.

Do NOT copy content from these files back into active documentation without re-verifying
every claim against the NPL sources (../../NPL-SOURCE/NPL/) and the symbol tables
(../../NPL-SOURCE/SYMBOLS/).

Authoritative replacement: [../ND500-BUS-INTERFACE-REFERENCE.md](../ND500-BUS-INTERFACE-REFERENCE.md)
(created by this overhaul; see also [../ND500-EVIDENCE-AND-CONTRADICTIONS.md](../ND500-EVIDENCE-AND-CONTRADICTIONS.md)
for the evidence trail behind each retirement decision).

---

## ND500-BOOT-DETECTION-MECHANISM.md

**Why retired: FABRICATED CONTENT with claims contradicted by NPL source.**

The document presents a "DETECTND500" routine as pseudo-code "based on MP-P2-N500.NPL and
hardware documentation". No such routine exists in the NPL sources. The real detection routine
is CH5CPUPRESENT in PH-P2-OPPSTART.NPL (around line 3903).

Specific known-wrong claims:

1. **Detection polarity is REVERSED.** The doc claims "A=0 means trap occurred ... IOX failed -
   no 3022 interface card". The real code (PH-P2-OPPSTART.NPL:3913-3917) arms the IOX-error
   trap (A:=200, TRR IIE), reads RSTA5, reads IIC - and A=0 means NO IOX error occurred,
   i.e. the card IS present (CPU flagged OLD500 + 5ALIVE).
2. **5MPM allocation is wrong.** Its "INIT5MPM" section claims SINTRAN allocates 5MPM pages
   from ND-100 physical RAM. 5MPM is a separate multiport memory module accessed through
   port modules with BASE-register translation - see ../WHERE-IS-5MPM-LOCATED.md and
   ../../OS/MPM5-KEY-FINDINGS.md.
3. **Interrupt-enable value is wrong.** Its "CONFIG3022" claims writing octal 10 to LCON5
   "enables interrupts on level 12". In the real driver, LCON5:=10 is written in the
   activate sequence (MP-P2-N500.NPL:3089, comment "Enable for interrupt") but the doc's
   surrounding description of the control bits does not match the NPL usage
   (values actually written: 40, 10, 1, 5, 400 - see the master reference).
4. **Internal bit-number errors.** Calls 5PFAIL "bit 5" and 5CLOST "bit 7"; the symbol table
   and XC-P2-N500.NPL:41-45 give 5PFAIL=bit 7, 5POWOF=bit 8, 5CLOST=bit 9, 5ILOC=bit 5.

Superseded by: ../ND500-BUS-INTERFACE-REFERENCE.md (boot/detection section) and
../ND500-ND5000-INTERFACE-COMPREHENSIVE-GUIDE.md (quotes the real CH5CPUPRESENT code).

Note: the RSTA5 status-bit TABLE inside this doc happens to be correct; everything narrative
around it is not. That table exists in better-sourced form elsewhere.

---

## MP-P2-N500_API_Documentation.md

**Why retired: DUPLICATE - strict subset of ../MP-P2-N500.md.**

A subroutine index with no independent content: zero register claims, zero bus-interface
information not already present (with more context) in ../MP-P2-N500.md. Retired to avoid
maintaining two indexes of the same NPL file that could drift apart.

Superseded by: ../MP-P2-N500.md (the full analysis of MP-P2-N500.NPL).

---

## ND-500-INTERFACE.md

**Why retired: EARLY GUESSWORK, superseded.**

An early one-page IOX register table with hedged, unverified annotations: offset +14 labeled
"ReadLockedMaybe", +15 "Unclear - may depend on context", +16 "ReadLocked - plausible" and
marked as a read register. The symbol table (SYMBOLS/M06/N500-SYMBOLS.SYMB.TXT) gives these
definitively: SLOC5=14 (start-lock write), CLKD5=15, UNLC5=16 (unlock). The same guesswork
table was copied verbatim into the four NPL-dump docs (CC/MP/RP/XC-P2-N500.md), where it is
being corrected in place.

Also states "IOX Base: Device number 1560 (octal)" as if fixed; the base device number HDEV
is thumbwheel/configuration dependent, held in the device datafield (X.SWHDEV=:HDEV,
PH-P2-OPPSTART.NPL:274).

Superseded by: ../ND500-BUS-INTERFACE-REFERENCE.md (IOX register reference section) and
../ND500-IF-USAGE-DEEP-ANALYSIS.md.

---

## Superseded ND-500 swapper analysis (moved 2026-07-15)

Unlike the documents above, these three are **not fabricated** - they were valid earlier
work (2026-07-08) that has been fully superseded by a more complete, byte-verified pass.
They are kept for provenance. The current canonical swapper analysis lives in `../swapper/`.

### SWAPPER-K01-ANALYSIS.md

The first ND-500-side swapper reverse-engineering write-up. Superseded by
[../swapper/swapper-k01-deep-analysis.md](../swapper/swapper-k01-deep-analysis.md) (role
determination, MON 377B descriptor decode, request/response loop) and
[../swapper/swapper-k01.pseg.md](../swapper/swapper-k01.pseg.md) (routine-level pseudo-C).
Corrections the newer pass makes: **WPHS is absent** (only RPHS present); the swapper **does**
reach ND-100 memory, via sanctioned **RIOM DMA**; the pre-trap "internal call with identical
args" is a **trace/log routine**, not a try-local-else-forward fast path.

### SWAPPER-MON-DISPATCH.md

Investigated whether the swapper is the trap target for ND-500 monitor calls. The question is
answered definitively in [../swapper/swapper-k01-deep-analysis.md](../swapper/swapper-k01-deep-analysis.md):
the swapper is a CLIENT of SINTRAN, has no receive-side MON dispatcher, and every `MON 377B`
is an OUTWARD trap (segment-31 monitor call 255 = N5SWAP).

### SWAPPER-K01.PSEG.asm

The earlier plain ND-500 disassembly of the swapper PSEG. Superseded by the richer
[../swapper/swapper-k01-pseg.asm](../swapper/swapper-k01-pseg.asm) (produced with
`nd500-dis -a -o` at base `0x08000000`, with address/byte prefixes).
