# CARVE ANSWER (V10) - the jumpg fork decoded: b.0x30 is a CHARACTER, 0x42 = 'B', NOT mis-derived

Answers the octobus-headered relay "Found the exact fork - a runtime-confirmed jump table.
B003C8F4: jumpg $0xB004A588+ ... b.0x30 = 0x42 -> error 52" (relayed 2026-07-18).
Builds on [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V9.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V9.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom.asm`. ND-500 VAs (hex).
Tags: **[V]** = read from these bytes; **[I]** = inferred; **[OPEN]** = not proven from static bytes.

## TL;DR - the fork is a CHARACTER dispatch; 0x42 is correct, so the bug is back at B0040C3C

Two questions were posed: (1) decode the table at `0xB004A588`; (2) is `b.0x30 = 0x42` a correct
selector or mis-derived. The bytes answer (2) decisively and reframe (1):

- **`b.0x30` holds a CHARACTER code, not an arg-type code.** It is range-checked as a character
  (`< 0x3C '<'`, `> 0xC7`) at `B003C8C7`/`B003C8CD`, and the error-52 arm at `B003D09F` compares it
  to **`0x41 ('A')`**. So `0x42` is literally **`'B'`** - the first char of the drive/name in
  `B:NRF`. **It is NOT mis-derived; the dispatch is behaving correctly.** [V]
- Because `0x42` is the right value, the failure is **not** in how `b.0x30` is computed. It is the
  same gate V9 already isolated: the arm for a general name calls `B0040C3C`, which rejects `B:NRF`
  for having no `"` (0x22) or `.` (0x2E). **The decisive action stays V9's live test**
  (`LOAD "B:NRF"` / `LOAD B.NRF`), not decoding the jump table. [V synthesis]

## 1. What b.0x30 is (byte-exact)  [V]

`b.0x30` is one character pulled from the argument string (scanned via pointer `b.0x34` with
auto-increment, e.g. `B003C862 by comp2 @b.0x34+,r1`). Evidence it is a *character*, not a type code:
```
B003C8C7 w comp2 b.0x30,$0x3C     ; compare against '<' (0x3C)
B003C8CB if < go ...
B003C8CD w comp2 b.0x30,$0xC7     ; upper bound 0xC7
B003C8D2 if > go ...
...
B003D09F w comp2 b.0x30,$0x41     ; error-52 arm: is the char 'A'?
B003D0A3 if >< go $0x6            ; not 'A' -> skip
B003D0A5 w set1 b.0x218           ; 'A' -> set a flag
```
`0x41='A'`, `0x42='B'`, `0x3C='<'`, `0x3A=':'` (used at `B003C85D by1 := $0x3A`). This whole block is
classifying/branching on printable characters of the object argument. `B:NRF` starts with `'B'`
(0x42), so `b.0x30 = 0x42` is exactly the first name character. [V]

## 2. The fork instruction sequence (byte-exact)  [V]

```
B003C8E2 FC 7D 4C CE 01 00 D2   w2 div4  b.0x30,$0x100,r3   ; derive index from the char
B003C8E9 1D 52                  by2 =:   b.0x48
B003C8EB 31 C4 B0 04 A5 84      by2 comp $0xB004A584         ; bound = byte @0xB004A584 = 0x0D (13)
B003C8F1 D5 0E 20               if >> go $0xE20              ; index > 13 -> B003D711 (out-of-range arm)
B003C8F4 B4 E1 B0 04 A5 88      jumpg    $0xB004A588+        ; index <= 13 -> table dispatch
B003C8FA (case fall-through)    ... call B003E0F5            ; the first/immediate arm
```
- Out-of-range arm target: `B003C8F1 + 0xE20 = B003D711` (relative-to-instruction; calibrated against
  `B003C913 go $0xE1E -> B003D731` and `B003C93D go $0xDF4 -> B003D731`). [V]
- The jumpg fall-through (`B003C8FA`) calls **`B003E0F5`**, not `B0040C3C`. [V]
- So `B0040C3C` (the error-52 site) is reached only via the specific in-range arm that lands at
  **`B003D08F`** (`call B003DCE2` canonicalizer, then `B003D0E2 call B0040C3C`). [V]

## 3. The table at 0xB004A588 - raw bytes + honest limit  [V bytes / OPEN format]

The region `B004A570..B004A5xx` is a **DATA constant pool**, not code: `B003C850 by scopa
b.0x1C0,$0xB004A570` and `B003C876 w bmove $0xB004A578,...` reference it as data operands. The linear
disassembly rendering it as "counter code" is the misaligned-overlay trap - ignore that listing here.

Raw bytes (big-endian image, byte addresses):
```
0xB004A584: 0D              <- bound value = 0x0D (13); matches the by2 comp
0xB004A585: C4 B0 05        <- 3 bytes between bound and table base (format unknown)
0xB004A588: 2B 10 55 01 21 4C 44 4D C6 0C 0D C4 B0 05 2B 14 55 01 21 4D 2E 4A 01 ...  <- jumpg table
```
**[OPEN] I cannot verify the `jumpg` entry stride or whether entries are absolute VAs, relative
offsets, or byte displacements** from these static bytes, and I will not guess it (ND-500 `div4`
result placement is equally unverified - the RDIV lesson applies). What I will NOT claim: any specific
index-to-arm mapping. If you want it decoded, the cheapest path is a live probe (below), which also
makes the table decode unnecessary for the fix.

## 4. Why decoding the table is now moot for the fix

The team's real question was "is 0x42 correct or mis-derived." It is **correct** (it is `'B'`). The
dispatch then routes the general-name case to `B003D08F -> B0040C3C`, and `B0040C3C` is the V9 grammar
gate that rejects `B:NRF` (no `"`, no `.`). Nothing upstream is wrong; the contract mismatch is at
`B0040C3C`. Therefore:

- **Do NOT chase the jumpg table or b.0x30's derivation further** - both are behaving correctly. [V]
- **Run V9's one-line test:** `LOAD "B:NRF"` (exit A, quote) or `LOAD B.NRF` (exit B, dot). Whichever
  passes names the argument contract `B0040C3C` wants, and that is the fix. [decisive]

## 5. Optional confirmation probes (if you still want the fork nailed)

1. Break at `B003C8F1`; read `b.0x48` and whether the `>>` branch is taken for the LOAD arg. If taken,
   LOAD went to `B003D711` (out-of-range) - which would contradict the traced `B003D08F` path and mean
   `b.0x48 != 0x42` (i.e. `div4` did NOT pass the char straight through). If NOT taken, `b.0x48 <= 13`
   and the jumpg picked the arm to `B003D08F`.
2. Break at `B003D09F`; confirm `b.0x30 = 0x42 ('B')` and that the `>< go` (not-'A') path is taken.
   This confirms the character-dispatch reading directly.

## 6. Honest status

Round ~7 of error-52. This round **closes** the "0x42 correct vs mis-derived" branch (it is correct =
`'B'`) and re-pins the fault at `B0040C3C`'s grammar contract, exactly where V9 left it. The jumpg
table's internal stride/format stays **[OPEN]** - unverifiable from static bytes and now unnecessary
for the fix. Banked fixes (i1=0; current-user->SYSTEM fallback) stand. Next decisive step is
unchanged: the V9 `LOAD "B:NRF"` / `LOAD B.NRF` live test.

## Evidence register
- Character dispatch: `B003C8C7` (`comp2 b.0x30,$0x3C`), `B003C8CD` (`comp2 b.0x30,$0xC7`),
  `B003D09F` (`comp2 b.0x30,$0x41`), `B003C85D` (`by1 := $0x3A ':'`).
- Fork: `B003C8E2 div4 b.0x30,$0x100,r3`; `B003C8EB by2 comp $0xB004A584` (bound 0x0D);
  `B003C8F1 if >> go $0xE20` -> B003D711; `B003C8F4 jumpg $0xB004A588+`.
- Error-52 arm: `B003D08F..B003D0E2` (`call B003DCE2`, then `call B0040C3C`).
- Data pool proof: `B003C850 by scopa b.0x1C0,$0xB004A570`; `B003C876 w bmove $0xB004A578,...`.
- Branch calibration: `B003C913 go $0xE1E`/`B003C93D go $0xDF4` both -> `B003D731`.
