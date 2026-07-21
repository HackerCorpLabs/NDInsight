# CARVE: S3SM5 stamps MICFU:=3SWMESS + SWFUN and fills the swapper-message body

Date: 2026-07-21
Track: TRACK A (static ND-100/ND-500 disassembly of SINTRAN III L, L-VSX-500)
Scope: get a RELIABLE instruction-level decode of segment `030-S3SM5` and name the
routine that stamps `MICFU := 3SWMESS (=5)` / `SWFUN := MSWIN (=5)` and fills the
15-word swapper-message body that SWAPPER-K01 reads.

Grades on every claim:
- **[V]** byte/line-verified by direct reading of a cited disassembly line or file
- **[I]** inference from that reading (reasoned, not literally stated by the bytes)
- **[OPEN]** not established; needs a further carve or a live trace

Files read / produced (full absolute paths):
- IN  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.bin`
- IN  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\030-S3SM5.meta.json`
- IN  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\030-S3SM5-routine-map.md`
- IN  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\030-S3SM5.ghidra-symbols.txt`
- IN  `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\006-S3FS.dis` (sibling recipe reference)
- IN  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\N500-SYMBOLS.SYMB.TXT`
- IN  `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\s3vs-4.symb`
- IN  `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\swapper\swapper-k01-handlers.md`
- IN  `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\CARVE-MSWIN-MESSAGE-SENDER-2026-07-21.md` (prior art - PARTLY CORRECTED here)
- OUT `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\030-S3SM5.dis` (full reliable decode, 49152 words)

---

## 0. HEADLINE CORRECTION OF THE TASK PREMISE (read first)

The task states that `030-S3SM5` is "32-bit BYTE-ADDRESSED ND-500 code" and that I
"MUST use `nd500-dis`, NOT `nd100-dis` (nd100-dis will produce garbage)". **That is
empirically FALSE for this segment.** [V]

- `030-S3SM5` is **ND-100** code (16-bit, word-addressed). `nd100-dis` (on the
  byte-swapped image) produces clean, coherent ND-100 code; `nd500-dis` drifts into
  garbage/ASCII within a few instructions.
- Corroboration: the sibling ND-500-subsystem segments already carved in the same
  `re\` directory - `006-S3FS.dis`, `003-S3CP.dis`, `045-S3ISYS.dis` - were ALL
  produced with `nd100-dis` on a byte-swapped `.le.bin`, and they decode cleanly
  (e.g. `006-S3FS.dis` shows `MON 122 ReserveResource`, `MON 164 SaveSegment`).
- Corroboration: the project MEMORY note already records "030-S3SM5 is ND-100 code
  (compiled 5STDRIV)".

The "S3" prefix = SINTRAN-III. `S3SM5` is the ND-100 **System Monitor for the ND-500**
(the code that loads/manages the ND-500 swapper and services non-500B ND-500 MON
calls). It runs on the ND-100, not the ND-500. The ND-500 swapper it feeds
(`SWAPPER-K01`) is the separate ND-500 domain analysed in `swapper-k01-handlers.md`.

The prior `030-S3SM5-routine-map.md` header line ("Disassembler: nd500-dis ... big-endian")
is the source of the wrong premise; that same doc's section 6 admits the nd500-dis
decode was untrustworthy. It was untrustworthy because it was the wrong ISA.

---

## 1. THE RELIABLE-DECODE RECIPE (reproducible)

```
# WSL (Git Bash is NOT WSL). nd100-dis + python3 are WSL-only.
# 1. Copy the .bin to a LOCAL WSL path. (nd100-dis/nd500-dis fail to open files
#    under /mnt/... with "Cannot open ''"; a local copy in /tmp works.)
cp .../segments/030-S3SM5.bin /tmp/s3sm5.bin

# 2. Byte-swap BIG-ENDIAN -> LITTLE-ENDIAN (swap each 16-bit word).
python3 -c 'd=open("/tmp/s3sm5.bin","rb").read(); o=bytearray(len(d))
for i in range(0,len(d)-1,2): o[i]=d[i+1]; o[i+1]=d[i]
open("/tmp/s3sm5.le.bin","wb").write(o)'

# 3. Disassemble with nd100-dis, octal, base = segment load word address 040000.
nd100-dis -a -o -b 40000 /tmp/s3sm5.le.bin  > /tmp/s3sm5.full.dis
```

What makes it reliable (all [V]):
- **Base word 040000 octal = 0x4000** is the segment load address from
  `030-S3SM5.meta.json` (`load_address.oct=40000`). With that base the displayed
  octal address IS the runtime ND-100 word address, and the ghidra data symbols land
  exactly: `VERSI 0x4005` maps to the ASCII version string `88. 8.17`, `REVIS 0x400a`
  to `L00`.
- **Byte order:** the raw `.bin` is BIG-ENDIAN (per meta.json and the readable ASCII
  in the raw file); `nd100-dis` wants LITTLE-ENDIAN, so byte-swap first. This matches
  the sibling `.dis` files (they were built from a `.le.bin`).
- **Sanity signals:** the classic ND-100 PLANC call idiom `JPL I <disp>` immediately
  followed by a pointer word that disassembles as `RAND 0 0` (144400) appears
  throughout; MON calls decode to sensible SINTRAN numbers; the swapper-message
  builders (section 3) are internally consistent (message base, field offsets, and
  the literal 5 all agree with the N500-SYMBOLS field layout).

Address arithmetic:
```
runtime_word (octal, as displayed) = file_word + 0x4000
file_byte = 2 * file_word = 2 * (runtime_word - 0x4000)
```

Caveat [V]: this is a **linear** disassembly. `030-S3SM5` is PLANC with inline
`'`-terminated ASCII strings and pointer data interspersed in the code, so stretches
of the listing that fall inside a string/data pool decode as nonsense instructions.
The decode is reliable **as ND-100 code when read from a true instruction boundary**;
do not trust a run that starts mid-string. The builders in section 3 were read from
verified instruction boundaries.

---

## 2. BOTTOM LINE (graded verdict)

**S3SM5 itself - ND-100 code - stamps `MICFU := 3SWMESS (=5)`, writes `SWFUN`, and
fills the swapper-message body.** The stamp/fill is NOT in the ND-5800 microcode and
NOT (only) on the ND-500 side. [V for the field writes; [I] for the MICFU/SWFUN/body
symbolic identity, which rests on the N500-SYMBOLS offsets, not on an S3SM5 symbol.]

Two swapper-message builders were located and byte-verified in the reliable decode.
Both take the message base into X with `LDX ,B -67` (octal 054711) and write the
control field `MICFU` at offset 6 with the literal 5 (`SAA 5`):

1. **Generic builder - the MSWIN=5 path** - stamp at runtime octal **140771..141001**
   (hex word 0xC1F9..0xC201; file byte 0x103F2..0x10402).
   - `SWFUN` (offset 7) := a caller-supplied variable `[B-77]`, gated at 140765 so
     that if `[B-61] != 0` the code instead uses 24B (=MSWSWAIT). For a swap-in the
     caller passes `[B-77] = MSWIN = 5`. **This is the routine that produces a
     `MICFU=3SWMESS, SWFUN=MSWIN=5` message.**
   - `MICFU` (offset 6) := 5 = 3SWMESS (`SAA 5; STA ,X 6`).
   - Body words: offset 2 := 1, offset 4 := `[B+72]`.

2. **Full-body builder - the MSWSWAIT path** - stamp at runtime octal
   **162155..162207** (hex word 0xE46D..0xE487; file byte 0x1093A..0x1096E).
   - `MICFU` (offset 6) := 5 = 3SWMESS; `SWFUN` (offset 7) := 24B = MSWSWAIT (=20).
   - Fills a ~15-word body: offsets 2,3,4,10,11,12,13,14,15,16,17 plus the double
     words at 110 and 112 - i.e. the multi-word message body the task describes.

`SWFUN` is **never** hard-coded to 5; it is always the per-message swap-function code
(variable in builder 1, literal 24B in builder 2). `MICFU` is **always** the literal 5
(3SWMESS). That is exactly the two-different-5s structure the field table predicts:
`MICFU=3SWMESS=5` (the interface control field) and `SWFUN=MSWIN=5` (the swap
sub-function, supplied by the requester). [V]

**This corrects the prior `CARVE-MSWIN-MESSAGE-SENDER-2026-07-21.md`.** That doc
concluded (its bottom-line item 3, and evidence F) that "no ND-100 code fills the
body / SWFUN is only ever LOADED never STORED / the sender is on the ND-500 side."
That conclusion was an artefact of its grep scope: it searched the NPL source tree
and `s3vs-4.symb` (the resident SINTRAN nucleus). **S3SM5 is a separate loadable
segment whose source is NOT in the repo** (verified in section 4), so its stores were
invisible to that grep. The binary carve shows S3SM5 (still ND-100 code) DOES store
`SWFUN` and DOES write `MICFU:=3SWMESS` and DOES fill the body. [V]

Implication for the D4 blocker (INFERENCE, not proven here): if S3SM5 is the ND-100
routine that builds and fills the MSWIN body, then an empty body at HSWPI on the
ND-5800 image means either (a) S3SM5's builder for MSWIN did not run / took the
`[B-61]!=0` MSWSWAIT branch, or (b) the buffer read at HSWPI is not the one S3SM5
filled. That is the next thing to pin (section 6). [I]/[OPEN]

---

## 3. ADDRESS-LEVEL EVIDENCE CHAIN

### A. Field/const values (N500-SYMBOLS, L07) [V]
```
MICFU = 000006   (message word offset 6  - the interface control field)
SWFUN = 000007   (message word offset 7  - the swap sub-function)
3SWME = 000005   (3SWMESS - the MICFU value that selects the SWMESS handler)
MSWIN = 000005   (swap-function value MSWIN)
SWPST = 000103   HSWPI = 000104   (the ND-100-side SWMSG fields)
```

### B. S3SM5 is the swapper manager (strings in the raw .bin) [V]
Byte offsets in `030-S3SM5.bin` (big-endian, ASCII stored in natural order):
```
0x00C92  "$Swapper in use on another CPU'"
0x071BF  "\"$> Loading Swapper'"          <- the exact D4-stall string
0x0605C  "...(SYSTEM)SWAPPER.PROCESS-SEGM..."
0x160C2  "(SYSTEM)SWAPPER.EXTRA-DATA-SEGMENT-2'"
0x16E29  "$Swapper debug mode set: use \"go\" to restart Swapper'"
```
S3SM5 loads, names, CPU-arbitrates and debug-controls the swapper. It is the ND-100
home of swapper bring-up.

### C. Builder 1 - generic MSWIN/MSWSWAIT stamp (reliable decode) [V]
Runtime octal addresses (from `030-S3SM5.dis`):
```
140765  044717   LDA ,B -61          ; gate variable
140766  131003   JAZ  -> 140771      ; if [B-61]==0 use the passed SWFUN
140767  044024   LDA 24              ; else A := 24B (MSWSWAIT=20)
140770  124020   JMP  -> 141010
140771  044701   LDA ,B -77          ; A := caller SWFUN  (=MSWIN=5 for swap-in)
140772  054711   LDX ,B -67          ; X := MESSAGE-BUFFER BASE
140773  006007   STA ,X 7            ; msg.SWFUN (offset 7) := A
140774  170401   SAA 1
140775  006002   STA ,X 2            ; msg body offset 2 := 1
140776  044472   LDA ,B 72
140777  006004   STA ,X 4            ; msg body offset 4 := [B+72]
141000  170405   SAA 5               ; A := 5 = 3SWMESS
141001  006006   STA ,X 6            ; msg.MICFU (offset 6) := 3SWMESS(5)
```

### D. Builder 2 - full ~15-word body + stamp (reliable decode) [V]
Runtime octal addresses:
```
162155  026352   LDD ,X -26
162156  022110   STD ,X 110          ; body double @110
162157  170401   SAA 1
162160  006016   STA ,X 16           ; body @16
162161  046354   LDA ,X -24
162163  046351   LDA ,X -27
162164  022112   STD ,X 112          ; body double @112
162166  006017   STA ,X 17           ; body @17
162167  002013   STZ ,X 13           ; body @13 := 0
162170  002014   STZ ,X 14
162171  002015   STZ ,X 15
162172  002010   STZ ,X 10
162173  002011   STZ ,X 11
162174  170423   SAA 23
162175  006012   STA ,X 12           ; body @12 := 23B
162176  170405   SAA 5
162177  006006   STA ,X 6            ; msg.MICFU (offset 6) := 3SWMESS(5)
162200  170424   SAA 24
162201  006007   STA ,X 7            ; msg.SWFUN (offset 7) := 24B (MSWSWAIT=20)
162202  170401   SAA 1
162203  006002   STA ,X 2            ; body @2 := 1
162204  044720   LDA ,B -60
162205  006003   STA ,X 3            ; body @3 := [B-60]
162206  044400   LDA ,B 0
162207  006004   STA ,X 4            ; body @4 := [B+0]
```
(X was set with `LDX ,B -67` at the head of this block, same message base as builder 1.)

### E. S3SM5 also drives the ND-100 SWMSG + MON 377 [V]
Independent corroboration that S3SM5 speaks the swapper protocol on the ND-100 side:
```
164103  054711   LDX ,B -67
164104  046103   LDA ,X 103          ; A := SWMSG.SWPST (offset 0o103)
164106  153377   MON 377             ; issue MON 377 (swapper/interface)
```
S3SM5 both reads SWMSG.SWPST/HSWPI (0o103/0o104) and issues MON 377 (3 sites).

### F. Why the prior negative proof missed this (scope, not contradiction) [V]
- `s3vs-4.symb` (the resident-nucleus build the prior doc grepped) does NOT contain
  S3SM5's data symbols: `grep -E '\b(FSCAP|MRSEG|ADRZO|MBDYN|SGCOM)\b' s3vs-4.symb`
  returns nothing. Those symbols ARE S3SM5's (from `030-S3SM5.ghidra-symbols.txt`).
- S3SM5's distinctive strings (`SET-TEMPORARY-FILE`, `WAIT-FOR-OPERATOR`,
  `Open File Table is full`, `power fail has occured`) do NOT appear anywhere in
  `SINTRAN\NPL-SOURCE\NPL\*.NPL`. **S3SM5's source is not in the repo.**
- Therefore the prior full-tree grep (`SWFUN` only ever LOADED, `MSWIN` absent,
  `3SWMESS` only tested) is TRUE for the resident nucleus but says nothing about
  S3SM5. The stamp/fill lives precisely in the segment the grep could not see.

---

## 4. EVIDENCE TABLE

| Claim | Grade | Evidence |
|-------|-------|----------|
| `030-S3SM5` is ND-100 code, not ND-500 | [V] | nd100-dis(LE,base 040000) clean; nd500-dis garbage; sibling 006-S3FS.dis uses nd100-dis; MEMORY note |
| Reliable recipe = byte-swap BE->LE + `nd100-dis -a -o -b 40000` | [V] | header symbols land (VERSI=88. 8.17), clean PLANC JPL-I/RAND pairs |
| S3SM5 is the ND-100 swapper loader/manager | [V] | strings "> Loading Swapper", "Swapper in use on another CPU", debug-mode strings |
| Message base for the builders = `X:=[B-67]` (054711) | [V] | 030-S3SM5.dis @140772, @162150-ish, @164103 |
| MICFU (offset 6) := 3SWMESS = literal 5 | [V] | `SAA 5; STA ,X 6` @141000-141001 and @162176-162177 |
| SWFUN (offset 7) := per-message swap fn (var / 24B), never hardcoded 5 | [V] | @140771-140773 (var [B-77]); @162200-162201 (24B=MSWSWAIT) |
| Builder 1 (oct 140771..141001) is the MSWIN=5 path | [I] | SWFUN taken from caller var [B-77]; =5 for a swap-in; gate at 140765 selects MSWSWAIT otherwise |
| Builder 2 (oct 162155..162207) fills the ~15-word body | [V] | stores to offsets 2,3,4,10-17,110,112 |
| The written offsets 6/7 ARE MICFU/SWFUN of a swapper message | [I] | offsets match N500-SYMBOLS exactly; base holds a message; but no S3SM5 symbol proves the struct identity |
| S3SM5 reads SWMSG.SWPST(0o103)/HSWPI(0o104) and issues MON 377 | [V] | 030-S3SM5.dis @164103-164106 |
| Prior "no ND-100 code fills the body" was a grep-scope artefact | [V] | S3SM5 symbols/strings absent from s3vs-4.symb + NPL tree |
| ND-5800 microcode is NOT required to stamp/fill the message | [I] | the stamp/fill is present in S3SM5 ND-100 code |

---

## 5. WHAT THE STAMP ROUTINE IS (best statement)

- **The stamp/fill is an ND-100 subroutine inside S3SM5 with two entry variants**
  (a compact one and a full-body one). Both write `MICFU:=3SWMESS(5)` into offset 6
  of the message buffer addressed by `[B-67]`, set `SWFUN` at offset 7 to the swap
  function requested, and write the body words. [V]
- **The MSWIN=5 message specifically** is produced by the compact builder at runtime
  octal **140771..141001** (hex word 0xC1F9..0xC201, file byte 0x103F2..0x10402) when
  its caller supplies `[B-77]=MSWIN=5` and `[B-61]==0`. [V for the code; [I] for
  "=5 is the MSWIN case".]
- **The routine has no symbol-table name** (S3SM5's internal routine labels are not in
  `N500-SYMBOLS`, and its source is not in the repo), so it can only be named by
  address, not by a source identifier. [V]

---

## 6. [OPEN] - what still needs a carve or a live trace

1. **[OPEN] The enclosing subroutine ENTRY of each builder.** I read the builders from
   verified instruction boundaries but did not resolve the JPL/pointer that calls them,
   so the routine's entry address and its caller (the PLACE-DOMAIN path) are not pinned.
   Trace backward from oct 140771 / 162155 to the nearest subroutine head, or set a
   breakpoint there under the boot harness.

2. **[OPEN] Confirm `[B-67]` is the buffer whose address becomes HSWPI.** The offset
   match (6=MICFU, 7=SWFUN) is strong [I] but not proven to be the same buffer that
   `5ACTSWAPPER` publishes as `SWMSG.HSWPI` (byte 0x420E30 in the live D4 capture).
   A live single-step of PLACE-DOMAIN should show `X:=[B-67]` equal to the HSWPI value.

3. **[OPEN] Which builder runs in the D4 flow, and does it run at all.** The D4 body is
   empty. Determine at runtime whether (a) the MSWIN builder at 140771 executes and the
   buffer it fills is later overwritten/recycled, (b) the `[B-61]!=0` gate diverts to
   the MSWSWAIT builder, or (c) neither builder runs because the swapper bring-up
   stalls upstream ("> Loading Swapper", emitted by S3SM5 at raw byte 0x071BF).

4. **[OPEN] Is MICFU=3SWMESS set here by S3SM5 or ALSO by microcode elsewhere?** S3SM5
   demonstrably writes it here; whether the interface also stamps MICFU in the
   trap/microcode path for other message classes is a separate question, not needed to
   answer this one.

5. **[OPEN] Symbolic naming.** If an S3SM5 `.SYMB` with internal labels ever surfaces,
   re-resolve oct 140771 / 162155 to their source routine names.

---

## 7. NOTE ON THE TWO 5s (kept from prior art, still true)

`MICFU=3SWMESS=5` and `SWFUN=MSWIN=5` are two different message fields that both hold 5.
This carve confirms the split at the byte level: `MICFU` (offset 6) is ALWAYS the
literal 5 (the interface control field selecting the SWMESS handler), while `SWFUN`
(offset 7) is the requester-chosen swap sub-function (=5 for MSWIN, =24B for MSWSWAIT,
etc.). Do not collapse them.
