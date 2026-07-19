# CARVE REQUEST (V4) - LOAD 0x9016: the 70-vs-64 range check is INPUT-INDEPENDENT

**For:** the sintran/linker byte-carver.
**From:** nd500x linker bring-up (2026-07-18).
**This request:** `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/CARVE-REQUEST-LINKER-LOAD-ERROR52-V4.md`
**Builds on:** CARVE-ANSWER-LINKER-LOAD-ERROR52-V3.md ("parameter too long").
**Disassembly:** `/mnt/d/ND/500/nd-linker/linker-b01.dom.asm`.

## V3 was right about WHERE, not WHY - proof

V3: 0x9016 @B0035291 in B003472C ("read one parameter", frame 0x1F0) = "parameter too
long": token[0,70] vs 65-byte filename slot[0,64]. I instrumented it two ways and the
"token length" claim is disproven:

The check (B003527F..B003528F):
```
w2 := b.0x4C ; w2 -= b.0x48 ; w2 += 1 ; w2 += 1     ; w2 = (b.0x4C - b.0x48) + 2
w3 := b.0x38 ; w3 -= b.0x34 ; w3 += 1               ; w3 = (b.0x38 - b.0x34) + 1
w2 comp r3 ; if <= go (skip) ; else B0035291 park 0x9016
```

Dumped b.* at the error (break @B0035291, frame B=0xB0002274) in TWO runs:

- **Run 1 (feed "LOAD B:NRF" one line):** scan buffer @0xB0048FEC = "LOAD B:NRF" + ~60
  SPACES (71 non-delim chars). Operands: b.0x34=0, b.0x38=0x40(64), b.0x48=0,
  b.0x4C=0x46(70). -> 72 > 65 -> error.
- **Run 2 (feed "LOAD" ; "B:NRF" ; "" as SEPARATE lines):** scan buffer = "B:NRF'" +
  NULs (0x27 apostrophe then NUL - a PROPER 6-char terminated token). Operands
  **IDENTICAL**: b.0x34=0, b.0x38=0x40(64), b.0x48=0, b.0x4C=0x46(70). -> 72 > 65 ->
  error.

**So b.0x4C=70 and b.0x38=64 do NOT depend on the token (71 vs 6 chars).** They are the
same every time. Whatever [0,70] and [0,64] are, they are fixed here and 70 always
exceeds 64. b.0x50 = 0xB0048FEC (scan buffer ptr) in both; b.0x44 = a ptr
(0xB000230C in run 2). The chars-remaining word @0xB00491D4 = 0x0A(10) in run 1
(correct typed length), so the length tracking is fine - the range check is about
something else.

Also: Run 2's empty-line answer ENDS the file-collection loop and the linker reaches a
CLEAN MON 0B LEAVE (no page fault). So the empty-terminator behaviour is right; only the
0x9016 on the actual file name blocks us.

## The questions (byte-level, this is the whole blocker now)

1. **In B003472C, what are b.0x34/b.0x38 (=[0,64]) and b.0x48/b.0x4C (=[0,70])?**
   They are not stored by simple `=:` inside B00347xx-B00352xx (I grepped) - they look
   passed in from the caller or built by a struct/bmove. Trace where each is set.
   b.0x38=0x40 looks like the 65-byte filename-slot size (per V3). What is b.0x4C=0x46
   (70)? It is NOT the token length. Is it a SOURCE descriptor length, a screen/field
   column, a default-type-appended length, or a fixed table stride?

2. **Why does 70 > 64 fire for a trivial `B:NRF`?** On real hardware LOAD of one small
   NRF into an open domain must pass this. What state makes b.0x4C <= 0x3F on real HW
   that our emulator leaves at 0x46? Is b.0x4C derived from a MON reply, the domain
   header, the appended default type ":NRF", or an uninitialised slot our OPEN-DOMAIN
   should have set? (b.0x48=0 both times, so it is really "is 70 <= 64".)

3. **Is b.0x4C stale?** Frame B=0xB0002274 is identical across both runs (fixed frame
   VA). If B003472C does not re-init b.0x4C per call, could 70 be left over from an
   earlier parameter read? If so, which prior read writes 70 and what should clear it?

4. **Confirm the caller path** B00163FD (LOAD body) -> B0016458..B0016481 prompt setup
   (you cited B0016471: stz r.0x4 ; move $0x40,r.0x8 building the [0,64] slot) ->
   call B003472C @B001648A. Does the caller also pass the [0,70]? If [0,64] is built at
   the call site as a constant, where does the matching [0,70] come from?

## Ground truth verified

- OPEN-DOMAIN "A-TEST" succeeds (4096-byte A-TEST.DOM). MAGTP writes buffer[0]
  (mirroring regressed startup). Feed as separate lines -> clean exit.
- 0x9016 @B0035291, range (b.0x4C-b.0x48+2) > (b.0x38-b.0x34+1) = 72 > 65, operands
  fixed at 70 & 64 independent of input length.

## nd500x refs
- `/home/ronny/repos/nd500x/test/diag_linkdrive.c` (ND500X_WATCH_VA, KWATCH,
  BREAK_PC/BREAK_DUMP/BREAK_DUMPLEN). Run pinned from
  `/home/ronny/repos/nd500x/build/link_sandbox/`.
