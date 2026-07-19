# CARVE ANSWER (REFINED) - Linker "(-677:52)": full display chain decoded, error = linker error 42

> **PARTIALLY RETRACTED 2026-07-18 - superseded by
> [CARVE-ANSWER-LINKER-LOAD-ERROR52-V3.md](CARVE-ANSWER-LINKER-LOAD-ERROR52-V3.md).**
> nd500x round-2 dynamic data (hub H1 = 0x9016, cell write at B0035291) disproved the
> event attribution below: the displayed 0x906A is raw code **0x9016 + 0x54**
> (mem[0xB002C5DC] = 0x497 at runtime, NOT 0xFFFF9040), i.e. **0x9016 "parameter too
> long"** parked in the read-one-parameter routine B003472C - NOT internal 0x46D, and
> NOT the segment-used ATT bit. The display-chain decode (DIV/MOD 64, reporter
> B0035C88, hub B0015B3F, rebase formula) and the 0x46D site carves below remain
> byte-correct; only their linkage to the "(-677:52)" event is retracted.

Answers [CARVE-REQUEST-LINKER-LOAD-ERROR52-REFINED.md](CARVE-REQUEST-LINKER-LOAD-ERROR52-REFINED.md).
Supersedes the raise-site claim in [CARVE-ANSWER-LINKER-LOAD-ERROR52.md](CARVE-ANSWER-LINKER-LOAD-ERROR52.md)
(B004AFBE/0x106A is the startup DDBTABLES check, as your trace proved - retracted as the LOAD site).
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ .asm). Tags: [V] byte-cited, [I] inferred.

## ADDRESSING CORRECTION FIRST (affects every static DSEG read ever made)

**DSEG file offset = VA - 0xB0000000 + 0x57800, NOT + 0x58000.** [V]

Proof: the basic-mode command-table records hold runtime name pointers; record 0
is {0xB00314F4, 0, 4}. The string "CLOSE" sits at file offset 0x88CF4. So
0xB00314F4 -> 0x88CF4 => base = +0x57800. Every static DSEG byte read done with
+0x58000 (including in the superseded answer) was reading 0x800 bytes past the
true cell. PSEG rule (+0x1000) is unaffected. Instruction-operand-derived facts
(addresses, sizes) are unaffected; only "static content of DSEG VA x" claims were
poisoned.

## TL;DR - the whole mechanism, byte-verified

1. **"(-677:52)" is ONE error word, not two numbers.** The reporter prints
   field1 = word DIV 64 and field2 = word MOD 64 (both octal). Your event's word
   was **0x906A** (halfword, sign-extended to 0xFFFF906A = -28566):
   -28566 = 64*(-447) + 42 -> "-677" (=-447) and "52" (=42). [V, see section 1]
   The earlier "(0055:41)" example is the same scheme: word 0xB61 = 64*45+33.
   So "-677" is NOT garbage - it is the linker's SSI, and 52 is **linker error
   42 decimal**.
2. There is NO literal 0x906A (or 0x9040) in the binary. The word is built at
   report time: **display word = internal code - 0x443 + mem[0xB002C5DC]**,
   where mem[0xB002C5DC] is the runtime SSI base (static 0, set during startup;
   for SSI -677 it must hold 0xFFFF9040). Internal code for your event:
   **0x46D = 0x443 + 42**. [V for the arithmetic at B0015E07-B0015E1C and
   B0015F06-B0015F1B; I that mem[0xB002C5DC]==0xFFFF9040 at runtime - one live
   read confirms]
3. **The blank text** is the message-file lookup failing: texts live in an
   external ERMSG file (13-byte name template "UE-ERMSG-EN-C" at DSEG 0xB00542F0,
   language code spliced into chars 9-10 by B004C95B; opened via MON 257B FOPEN
   inside B004C9C1). No file in the sandbox -> blank text. Expected, ignorable. [V]
4. **What error 42 means / the state LOAD needs**: the two immediate raise sites
   of 0x46D both fire when the current segment-descriptor pair in the in-memory
   domain-header image has the **"Segment used" attribute bit clear** in BOTH the
   program-ATT and data-ATT words. Manual ND-860289-2 p.256: ATT bit 13 =
   "Segment used"; descriptor stride 0x1C bytes (= the program part, p.249);
   the tested byte is ATT+2 (bits 15..8), mask 0x20 -> bit 13. [V for the code
   test; the manual mapping assumes LSB=bit0 numbering - I]
   **=> your hypothesis is CONFIRMED: LOAD requires a current segment marked
   "used" in the open domain, and your OPEN-DOMAIN leaves the A-TEST header's
   segment 1 descriptor without that bit.** Basic-mode LOAD loads into slave
   segment 1 (manual examples: "Program:...P01 Data:...D01").

## 1. The display chain (your B0035EAA/B0035EC3 anchored and explained)

**B0035C88** (ents $0x118) is the central error REPORTER - "print
`*** <text> (SSI:NN)`". B0035EAA and B0035EC3 are not two callers; they are the
text-found / text-missing branches INSIDE it:

- B0035E31: `w swap $0xB0049B6C,r1` - fetch pending-text handshake cell.
- B0035E4D-B0035E5D: `call B003ABAF` = look up the error text for the code
  (message file machinery). Result flag -> 0xB0048CE4.
- Lookup OK -> print text via B0040BB1, then **B0035EAA** `call B004C95B`.
- Lookup failed -> **B0035EC3** `call B004C95B` with descriptor 0xB0049BAC =
  the 2-char language string "EN" (DSEG file 0xA13A8 = "EN"). [V]
- B004C95B: copies the 13-byte template at 0xB00542F0 ("UE-ERMSG-EN-C", DSEG
  file 0xABAF0), splices the language into chars 9-10, calls B004C9C1 which
  RSIOs (MON 143B at B004CA35) and FOPENs the message file (MON 257B at
  B004CA76). This is the message-FILE path, not the number formatter. [V]

**The numbers are printed by the reporter itself**, into the output line buffer
(frame b.0x98), from its argument b.0x30 (passed as r.0x30 by its caller):

```
B003604F: w move $0xE,r.0x14          ; output op
B0016052: b.0x104:=$0x48 b.0x100:=$0x45  ; line columns 0x45..0x48 (4 chars)
B003606D: w3 := b.0x30                ; THE ERROR WORD
B003606F: w3 / $0x40                  ; DIV 64  -> "-677" field
B0036079: call B00403B8               ; format number into the line
B0036094: w move $0xE,r.0x14
B0016097: b.0x104:=$0x4B b.0x100:=$0x4A  ; columns 0x4A..0x4B (2 chars)
B00360B2: w3 div4 b.0x30,$0x40,r4    ; DIV/MOD 64 -> "52" field
B00360BF: call B00403B8
```
[V] So one word in, two octal fields out. (The template " (0000:00) " itself is
static at DSEG VA 0xB0049BD0, descriptor 0xB0049BDC, appended at B0036031.)

## 2. Who feeds the reporter: the B0015B3F hub

**B0015B3F** (ents $0xF8; the .asm renders its entry bytes misaligned - real
entry is B0015B3F) = "report error, code in H1":

```
B0015B45: h1 =: b.0x54 / b.0x56      ; save raw code (halfword)
B0015B4C: comp 3 -> silent; comp 0x12 -> silent (ret)
B0015B57: comp 0xB6 -> replace with 0x494
B0015B71: h comp2 b.0x56,$0x3E8      ; >= 1000 -> internal-code path
B0015B7A: comp 0x3E7 (=999)          ; 999 = "already reported" sentinel -> skip
B0015B91: h wconv b.0x56,r1          ; SIGN-EXTEND halfword
B0015B96: w1 =: r.0x30 ; call B0035C88   ; direct path: report raw code
```
Two of the six B0035C88 call sites rebase internal codes [V]:
```
B0015E07: h1 := b.0x56 ; - $0x442 ; - $0x1     ; code - 0x443
B0015E12: h wconv r1,r2
B0015E16: w2 + $0xB002C5DC                     ; + runtime SSI base (memory word)
B0015E1C: w2 =: r.0x30 ; B0015E1E: call B0035C88
```
(same at B0015F06-B0015F1D). So **internal code 0x46D -> 0x46D-0x443=42 ->
+ base 0xFFFF9040 -> 0xFFFF906A -> "(-677:52)"**. After reporting, raisers
re-raise **0x3E7 (999)** - that is why your K-watch never saw any code with
low bits 0x2A: the 42 never travels through K; it is synthesized at print time.
[V]

The 0x90xx codes you logged (0x9016, 0x9011, 0x9021...) are the OTHER family:
pre-based SINTRAN-side codes (SSI -700 block: 0x9000 = 64*(-448)); they display
as (-700:NN) and pass the hub unmapped. Note B003530B-B0035311:
`w1 := mem[0xB0048CFC]; retk` - the input layer parks a deferred SINTRAN error
in global **0xB0048CFC** and raises it later; your logged raise at B0035311 with
I1=0x9016 is that mechanism. [V]

## 3. Q1/Q3: LOAD command body, and the raise for 42

**LOAD command bodies** [V]: twin routines **B00163FD** and **B00165FC**
(ents $0x6A0 each; basic/advanced variants). Both: prompt "File name" with
default type ":NRF" (descriptor pairs 0xB0035CFC/0xB0035D08 used at B0016458,
and 0xB0035D24/0xB0035D30 at B0016657 - strings at DSEG file 0x8D4EC/0x8D514),
collect up to 23 names of 65 chars, then per name call the one-file loader
**B0019914**:

```
B0019914: syntax check B00026A2 (bad -> 0x494)
B001993E: open via B000061B(name, ":NRF" desc 0xB0036058, mode) ; file number
          -> halfword global 0xB00272FA
B00199EC+: B0005358, B0000876 (MON 62B RMAX wrapper), B000074E,
          then NRF processing B0020616 / B001D6CF / B001CE27
B0019960: local entd catch: reports the CAUGHT code via B0015B3F, closes the
          NRF file, and SWALLOWS (ret) - one bad file does not abort the list
```
LOAD's command-level entd (B0016405/B0016604) tolerates 0x9011 per file and
rethrows anything else (your logged rethrow at **B0016446** = exactly this
`w1 := b.0xC; retk`). [V]

**Immediate raise sites for internal 0x46D (error 42)** - there are exactly two
`h1 := $0x46D` in the binary [V]:

- **B001735E** in worker **B0016D4F** (multi-name command family B0017CEE whose
  default file type is ":SEG").
- **B00185D2** in **B0018289** (helper called from wrappers B0018637/B0018681
  and from the big body B00186BC).

Both are reached by the same test [V]:

```
B00172F1: r := b.0x138            ; domain context struct
B00172F5: w2 laddr r.0x10         ; descriptor of the segment-descriptor array
B00172FD: by rladdr @b.0x150+     ; current descriptor record
B0017302: by4 := r.0xA            ; ATT byte +2 (bits 15..8 of ATT word)
B0017305: by4 and $0x20           ; bit 13 of ATT = "Segment used"
B0017309: if = go $0x30           ; CLEAR -> B0017339 = error block
B0017339..B001735E: r.0x14 := 65-char name buffer b.0x61 ; h1 := $0x46D ;
          call B0015B3F ; close fd 0xB0025470 ; w1 := $0x3E7 ; retk
```
The paired test just above (B0017237-B001725F, second record at +0x1C) checks
the program-part descriptor; descriptor stride 0x1C = the 28-byte program part
of a segment descriptor (manual p.249: LB 4, SZ 4, ATT 4, FLA 4, FUA 4, AFA 4,
MINP/MAXP 4). Same shape at B0018540-B00185AD for the B0018289 copy. [V]

**Honest gap [I]**: the hop from B0019914's NRF-processing subtree to the
segment-ATT check was not statically walked (B0015B3F is also called with
VARIABLE codes, e.g. `h1 := b.0x140` at B0016FCC fed from table 0xB0001204, so
the two immediate sites are not provably the only 42-raisers). ONE live probe
closes this completely - see section 5.

## 4. Q2: meaning of 52 / Q4: EXIT page fault

**Error 42 (displayed :52) = "the current segment of the open domain is not
marked Segment-used"** - i.e. there is no segment to load into. [V for the bit
test; the English name comes from the manual's ATT bit table p.256, bit 13
"Segment used", assuming LSB=bit-0 numbering - I]

This confirms your leading hypothesis: it is a domain-header state gap, not
DDBTABLES, not DEABF, not a missing script command. On real hardware,
OPEN-DOMAIN "A-TEST" initializes the fresh domain header with slave segment 1
current and used (basic LOAD prints "P01/D01"). In your run the in-memory
segment descriptor (read back from / written to the A-TEST header the linker
itself wrote via the two WFILEs) ends up with ATT lacking bit 13.

Since the linker writes that header itself, the missing bit most plausibly
enters via a file-layer round trip your MON layer zeroes (a read-back of the
2-page domain header after the WFILEs, or an RMAX/OPEN attribute the header
init depends on). [I - live watch decides, below]

**Q4 EXIT page fault at B001F66D** [I]: consistent as a downstream symptom -
close-time finalization walks the same segment context; with no used segment
the zero-fill loop runs with an uninitialized base. Fix the segment state
first; only chase B001F66D if it survives that fix.

## 5. Live probes (each one is decisive)

1. **Breakpoint B0015B3F** (report hub entry). On hit read H1 (raw internal
   code - expect 0x46D) and the caller link -> the EXACT raiser PC in one shot.
   This closes the [I] gap in section 3 definitively.
2. **Breakpoint B0035C88** and read its arg r.0x30 (= reporter word; expect
   0xFFFF906A) plus mem[0xB002C5DC] (expect 0xFFFF9040) - verifies the mapping
   arithmetic on your build.
3. **Find the missing write**: watchpoint on the in-memory segment-descriptor
   array. Its runtime address = the 12-byte descriptor at struct+0x10 where the
   struct pointer is what the raiser's b.0x138/b.0xB8 holds (for the B0016D4F
   copy the struct constant is 0xB001845C). Watch the ATT bytes during
   OPEN-DOMAIN: on real behaviour something must set bit 13 (byte value 0x20 at
   descriptor offset 0xA) for segment 1. The write that never happens - and the
   MON reply it depended on - is your actual bug.

## Poisoned priors (delete on sight elsewhere)

- "B004AFBE/B004AFC3 raises the LOAD 52" - startup DDBTABLES check only.
- "0x106A is the 52 code" - the LOAD event's word is 0x906A; 0x106A would
  display (101:52).
- "-677 is a garbage/uninitialized SSI" - it is the linker's SSI, printed from
  the same word as the 52.
- "DSEG file offset = VA + 0x58000" - correct rule is +0x57800 (section 0).

## Evidence register

linker-b01.dom.asm (PSEG VAs): reporter B0035C88 (ents $0x118); swap
B0035E31 (52 C4 B0 04 9B 6C D0); text lookup call B0035E5D -> B003ABAF;
fallback branch B0035E79/B0035EBA; B004C95B (ents $0x4C) template copy
B004C961 (FE 24 C4 B0 05 42 F0) + splice cols 9-10 (b.0x44=9,b.0x48=0xA);
RSIO B004CA35, FOPEN B004CA76; number prints B003604F-B0036079 and
B0036094-B00360C7 (FC 7E 4C CD 40 D3 = div4 b.0x30,$0x40,r4); template
" (0000:00) " DSEG VA 0xB0049BD0 (file 0xA13D0), descriptor 0xB0049BDC;
hub B0015B3F: compares at B0015B4C/B0015B51/B0015B57, 0x3E8/0x3E7 at
B0015B71/B0015B7A, wconv+r.0x30 B0015B91-B0015B98; rebase sites
B0015E07-B0015E1E and B0015F06-B0015F1D (55/56 C4 B0 02 C5 DC = + mem word);
raise sites B001735E and B00185D2 (08 CE 04 6D = h1:=$0x46D); ATT tests
B00172FD-B0017309 and B0018540-B001854B / B001857B-B0018586 (FC 92/93 CD 20 =
and $0x20 on byte r.0xA); stride 0x1C at B0016F5B (6C 1C) and B0017251/B001729B;
LOAD bodies B00163FD/B00165FC, prompts B0016458/B0016657 ("File name" file
0x8D4EC/0x8D514, ":NRF" 0x8D4F5/0x8D51D); one-file loader B0019914, catch
B0019960, file-no global 0xB00272FA; deferred-error raise B003530B-B0035311
(0C C4 B0 04 8C FC / 81 = w1:=mem[0xB0048CFC]; retk); DSEG base proof: record
{B00314F4,0,4} at file 0x88C94ff, "CLOSE" at file 0x88CF4.
Manual ND-860289-2 (in repo): SSI:code convention line 519; ATT bit table
p.256; SEGTABDISP p.257; segment descriptor layout p.249; basic LOAD example
"P01/D01" p.157.
