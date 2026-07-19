# CARVE REQUEST (V3) - LOAD "52" pinned dynamically to 0x9016 @ B0035291 (a RANGE check)

**For:** the sintran/linker byte-carver.
**From:** nd500x linker bring-up (2026-07-18).
**This request:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-REQUEST-LINKER-LOAD-ERROR52-V3.md`
**Builds on:** CARVE-ANSWER-LINKER-LOAD-ERROR52-REFINED.md (segment-used-bit answer).
**Disassembly:** `/mnt/d/ND/500/nd-linker/linker-b01.dom.asm`.

## Your semantic direction looks right; the exact site is different - here is the byte-proof

Your refined answer: error 42 = current segment descriptor's "used" bit (desc+0xA &
0x20) clear; raise sites B001735E / B00185D2 (H1:=0x46D -> hub B0015B3F). You flagged
the LOAD hop as not statically walked and told me to break B0015B3F + read H1. I did,
plus a write-watch on the linker's error cell. Results:

- **B0015B3F (hub) fires ONCE during LOAD**, instr 182797, with **H1 = 0x9016**, NOT
  0x46D. So neither B001735E nor B00185D2 (which pass 0x46D) is the LOAD raise; the
  segment-used-bit check at B0017302 is **never reached** during LOAD.
- **The error is set at B0035291**: `w move $0x9016, [0xB0048CFC]` (0xB0048CFC is the
  linker's current-error cell). Write-watch on 0xB0048CFC during LOAD:
  `0 -> 0x9011 @B0034777 -> 0 @B0034C2F -> 0x9016 @B0035291 (instr 182462) -> 0
  @B0035CC1 (after display)`. So the displayed "52" is code 0x9016, raised at
  B0035291.
- **B0035291 is guarded by a RANGE check** (bytes at B003527F..B003528F):
  `w2 = b.0x4C - b.0x48 + 2 ; w3 = b.0x38 - b.0x34 + 1 ; if w2 <= w3 skip else error`.
- **Operand values at the LOAD error** (break @B0035291, B=0xB0002274, instr 182461):
  b.0x34=0, b.0x38=0x40(64) -> w3 = 65 ; b.0x48=0, b.0x4C=0x46(70) -> w2 = 72.
  **72 > 65 -> error 0x9016.** (Nearby: b.0x3C=5, b.0x44 & b.0x50 = ptr 0xB0048FEC.)

Note the display encoding still resolves to "(-677:52)" (word 0x906A) so this IS the
"52" the user sees - your DIV64/MOD64 decode holds; only the source code/site is 0x9016
at B0035291, not 0x46D at B001735E.

## Questions (byte-level)

1. **What is the routine at/around B0035213 (entry above it) and error 0x9016?**
   It calls B003F659, B003A1DA, B004D4F4, bmoves from 0xB0049924, and does the
   72-vs-65 range compare. What are the two ranges b.0x34..b.0x38 (=[0,64]) and
   b.0x48..b.0x4C (=[0,70])? Segment address bounds? entry counts? name lengths?
   Cite the ND Linker manual field if it maps to a descriptor.

2. **Does this confirm "segment 1 not set up by OPEN-DOMAIN"?** i.e. is b.0x38-b.0x34
   (the "available" 64) derived from the open domain's current-segment size/bounds,
   which real OPEN-DOMAIN would have set larger (or which makes the check pass)? If
   yes, WHICH field of the in-memory domain header must OPEN-DOMAIN populate, and
   from what (a MON reply? the domain file header? a fixed default)? If no, what is
   the 64 and why does 70 exceed it for a trivial one-routine NRF (B.NRF, 513 bytes)?

3. **What is the 0x9011 that is set first (B0034777) and cleared (B0034C2F) each
   round** - a benign "in progress" marker, or a related pre-check? Same for the
   0x9014/0x9015 siblings on that cell.

4. **Confirm the LOAD->B0035213 call path** so we know this routine runs as part of
   loading B.NRF into the open domain (per-file loader B0019914 you already pinned).

## Ground truth verified this session

- OPEN-DOMAIN "A-TEST" succeeds, writes A-TEST.DOM (4096 bytes) - header is almost all
  zero (dumped): word0=0x0003, word1=0x4201D400, +0x10=0x40, rest ~0. No populated
  segment descriptor with a "used" bit anywhere obvious.
- MON 144B MAGTP read-record writes at buffer offset 0 (mirroring regressed startup).
- The "52" raise is 0x9016 @B0035291, a range check 72>65 (proven above).

## nd500x refs
- LOAD driver `/home/ronny/repos/nd500x/test/diag_linkdrive.c` (ND500X_WATCH_VA,
  KWATCH, BREAK_PC/BREAK_DUMP). Run pinned from
  `/home/ronny/repos/nd500x/build/link_sandbox/`.
