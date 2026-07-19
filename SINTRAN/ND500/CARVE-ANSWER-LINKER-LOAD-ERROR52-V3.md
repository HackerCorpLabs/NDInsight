# CARVE ANSWER (V3) - 0x9016 = "parameter too long": LOAD dies READING a file name, not loading it

> **PARTIALLY RETRACTED 2026-07-18 - superseded by
> [CARVE-ANSWER-LINKER-LOAD-ERROR52-V4.md](CARVE-ANSWER-LINKER-LOAD-ERROR52-V4.md).**
> nd500x proved the [0,70] is INPUT-INDEPENDENT. The raise site (B0035291 in B003472C),
> the 0x90xx family, and the LOAD call-site facts below stand. RETRACTED: the "your
> answer line was 71 delimiter-free characters" content explanation, and the delimiter
> analysis (the filename kind uses bitmap 0xB0048F20, not 0xB0048F40, and in the token
> loop chars <= 0x20 including NUL/space/CR are CONSUMED, not terminators). The real
> mechanism: token end = count limit r.0x94 - 1; r.0x94 = 71 = the line length reported
> by line reader B003F876. See V4.

Answers [CARVE-REQUEST-LINKER-LOAD-ERROR52-V3.md](CARVE-REQUEST-LINKER-LOAD-ERROR52-V3.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ `.dom.asm`).
Addressing: PSEG file = VA - 0xB0000000 + 0x1000; DSEG file = VA - 0xB0000000 + 0x57800.
Tags: [V] = byte-cited from .dom/.asm or your dynamic trace; [I] = inferred, basis stated.

## TL;DR

- **B003472C is the linker's universal "read one parameter" routine** (~100 call
  sites - every prompt in the program goes through it). B0035213/B0035291 are deep
  inside it; there is no separate routine at B0035213. [V]
- **The range check is a LENGTH check: token vs destination buffer.**
  b.0x30/0x34/0x38 = destination descriptor; b.0x44/0x48/0x4C = the token the
  scanner extracted from the current input line. `token_len + 1 > dest_capacity`
  -> park 0x9016. **0x9016 = "parameter too long for its field".** [V]
- **The [0,64] is NOT domain state.** It is LOAD's own 65-byte file-name slot,
  built as a compile-time constant at B0016471-B0016481 (`stz r.0x4; move $0x40,
  r.0x8`). Nothing about segments, headers, or OPEN-DOMAIN is involved. [V]
- **The [0,70] is the token: characters 0..70 of the input line at 0xB0048FEC**
  (the 256-byte line buffer). Your run answered a "File name" prompt with a
  71-character token containing none of the delimiter characters. [V]
- **The segment-used-bit story (error 42 / 0x46D) is NOT this event** - your
  dynamic data kills it, and it also kills my display-base inference (see
  section 6, retractions). The fix hypothesis changes completely: **your input
  feed to the second "File name" prompt is wrong**, not your domain header.
- **One dump answers everything: 256 bytes at VA 0xB0048FEC at the break.**
  That is literally the 71-char "file name" the linker thinks you typed.

## 1. Q1: the routine, and what the two ranges are

### 1.1 The routine is B003472C, "read one parameter"

`ents $0x1F0` at B003472C; the body runs to B0035318. It contains every address
in your trace: the 0x9011 park (B0034777), the clear (B0034C2F), the 0x9014 park
(B0035274), the 0x9016 park (B0035291), and the deferred rethrow (B003530B). [V]

It is called from ~100 sites across the binary (first: B00041BD; LOAD's two
bodies call it at **B001648A** and **B0016689**) - one call per prompted
parameter, program-wide. [V]

Argument block (caller writes callee slots r.0x14.., observed as b.0x14.. inside):

| Slot | Content | LOAD's value (bytes at B0016458-B0016481) |
|---|---|---|
| b.0x14..0x1C | prompt text descriptor | 0xB0035CFC -> "File name" [V] |
| b.0x20..0x28 | default text descriptor | 0xB0035D08 -> ":NRF" [V] |
| b.0x2C | parameter-kind code | 0x42 (`w move $0x42,r.0x2C` B001646A) [V]; meaning of the code values not carved [I] |
| b.0x30..0x38 | **destination descriptor** | {addr of LOAD local b.0x18, lo=0, hi=0x40} = **65-byte name slot** [V] |
| b.0x3C | out: result info (LOAD reads r.0x3C after the call, B00164A3) | your observed 5 = stale from the previous round [I] |

Destination construction at the LOAD call site [V]:

```
B001646E: FD 3C 46             w1 laddr b.0x18          ; address of LOAD's 65-byte name slot
B0016471: 18 CF B0 03 5C 8C    r := $0xB0035C8C
B0016477: 20 80                w1 =: r.0x0              ; desc.ptr
B0016479: 4A 81                w stz r.0x4              ; desc.lo = 0
B001647B: 1A CD 40 82          w move $0x40,r.0x8       ; desc.hi = 0x40  -> capacity 65
B0016481: FE 79 ... 8C 03      w bmove $0xB0035C8C,r.0x30,$0x3
B001648A: C3 B0 03 47 2C 00    call B003472C
```

So w3 = b.0x38 - b.0x34 + 1 = 65 = the name slot's capacity. **Fixed constant.
Not derived from the domain.** [V]

### 1.2 The [0,70] side: the scanned token

Inside B003472C, the token is produced by scanner **B0036620** and lands in
b.0x44/0x48/0x4C [V]:

```
B00349A7: w bmove $0xB0048F40,r.0x20,$0x8   ; delimiter bitmap, 8 words
B00349B0: w set1  r.0x40
B00349B2: call    B0036620                  ; scan token from input stream
B00349C4: call    B00402A1                  ; adjust/trim
B00349CD: w bmove r.0x14,b.0x44,$0x3        ; token descriptor -> b.0x44/0x48/0x4C
B00349D2: w bmove b.0x44,b.0x50,$0x3        ; copy kept in b.0x50 (your ptr 0xB0048FEC)
```

The token descriptor points into the **line buffer 0xB0048FEC** (256 bytes; its
only writer is `by bmove r1,$0xB0048FEC,$0x100` at B0039236, and the static
stream descriptors at DSEG 0xB0049918/0xB004A034/0xB004A040 are all
{0xB0048FEC, 0, 0xFF}). [V]

**Delimiter set** [V]: DSEG 0xB0048F40 (file 0xA0740) is a 256-bit character
bitmap: `80 00 00 00 06 08 00 ...` = bits for **NUL (0x00), '%' (0x25),
'&' (0x26), ',' (0x2C)** only. A token ends only at one of those or at end of
line. **Space, colon, period are NOT delimiters** - a file name token runs to
end of line if no comma follows. The terminating character of each scan is
stored at byte 0xB0048CE0 (swap at B0034A02). [V]

So in your run: the answer line was 71 characters with no ',', '%', '&' in it
-> the whole line is one token -> 72 > 65 -> 0x9016.

### 1.3 The check and what follows [V]

```
B0035274: w move $0x9014,$0xB0048CFC        ; arm "copy phase" code
B003527F: w2 := b.0x4C ; - b.0x48 ; +1 ; +1 ; = token_len + 1  (your 72)
B0035287: w3 := b.0x38 ; - b.0x34 ; +1     ; = dest capacity   (your 65)
B003528D: w2 comp r3
B003528F: if <= go -> B003529C               ; fits: skip the park
B0035291: w move $0x9016,$0xB0048CFC        ; TOO LONG: park 0x9016
B003529C: r := b.0x8
B003529E: w bmove b.0x44,r.0x14,$0x3        ; both paths still do the (bounded) copy
B00352A3: w bmove b.0x30,r.0x20,$0x3
B00352A8: call B00401FC                     ; copy token -> destination
```

The +1 on the token side is room for the terminator byte appended after the
copy (`by3 := $0xB0048E0C ; by3 =: @b.0x30+` at B00352C2-B00352C8). [V]

At routine exit, the parked cell decides the outcome [V]:

```
B0035303: w test $0xB0048CFC ; if = go +9   ; clean -> plain ret
B003530B: w1 := $0xB0048CFC  ; retk         ; parked code -> K-raise (your logged site)
```

## 2. Q2: does this confirm "segment 1 not set up by OPEN-DOMAIN"? **NO.**

The error has **nothing to do with the domain**. The 64/65 is a compile-time
buffer size; the check fires while LOAD is still **collecting file names from
the prompt**, before any per-file work (per-file loader B0019914 is reached
only after name collection ends). Your A-TEST.DOM header being near-empty is a
separate observation and is not what this error tests. [V]

What actually happened in your run, per your own watch data plus the bytes:

- LOAD loops collecting up to 23 names; each round is one B003472C call.
- Your observed stale b.0x3C = 5 fits "a previous round returned a 5-char
  token" = `B:NRF`. [I - stale-slot interpretation; the 5-char match to B:NRF
  is suggestive, not proven]
- A subsequent round's answer line was 71 chars, delimiter-free -> 0x9016 ->
  LOAD's error path aborts the whole command (your logged rethrow B0016446),
  so B.NRF never loads even if an earlier round accepted it.
- The collection loop ends NORMALLY only when a round yields no parameter:
  the parked 0x9011 survives to the rethrow and LOAD explicitly tolerates it -
  `w comp2 b.0x678,$0x9011` at **B00164B0**, equal -> proceed to loading. [V]

**Fix hypothesis for nd500x** [I, directly actionable]: after answering
`B:NRF` to the first "File name" prompt, the next prompt must be answered with
an **empty line** (just the line terminator). Your harness is instead feeding
something 71 characters long (rest of script? padded record? unstripped
buffer?). Note space is NOT a delimiter, so trailing padding spaces count.
**Dump 256 bytes at VA 0xB0048FEC at the 0x9016 break - that IS the offending
line, verbatim.** Also useful: byte at 0xB0048CE0 (the terminator the scanner
saw) and word at 0xB0049140+0x94 (chars remaining in the stream).

## 3. Q3: the 0x90xx parked-code family on 0xB0048CFC

0xB0048CFC is a **deferred-error cell**: "the code to raise if the current
phase fails / does not complete". Each phase arms its own code; success paths
clear or re-arm it; at routine exit a nonzero cell becomes the K-raise. [V]

| Code | Armed at | Phase | Meaning |
|---|---|---|---|
| 0x9011 | B0034777 (and B00351F0 for the next round) | parameter acquisition | "no parameter obtained". Cleared at B0034C2F once a token is in hand. Survives on empty input -> callers use it as **end-of-parameters**: LOAD B00164B0 tolerates exactly this code. [V sites; meaning I from LOAD's tolerance + arm/clear placement] |
| 0x9014 | B0035274 | token->destination copy | "copy phase failed" (generic). Replaced by 0x9016 when the length check already knows why. [V site; meaning I] |
| 0x9015 | B0033996 | line-input layer (the B00338xx machinery that refills 0xB0048FEC via B0039227) | input-layer failure; several catchers special-case it (B000F399, B000F444, B000FF71, B001001C, B0013BAB). [V sites; meaning I] |
| 0x9016 | B0035291 (this event) and B003541F (same pattern in the neighbour routine B0035319, frame 0xB8) | length check | **parameter too long for destination field** [V] |

Non-parked siblings raised directly with `ret`/`retk` in the same layer:
0x9003 (B003507E), 0x9021 (B003475C, B003508D, B0035096), 0x9024 (B003476B). [V sites only]

## 4. Q4: the LOAD -> B003472C path [V]

```
LOAD body B00163FD (twin B00165FC)
  name-collection loop:
    B0016458: prompt desc  0xB0035CFC "File name"
    B0016461: default desc 0xB0035D08 ":NRF"
    B001646A: kind := 0x42
    B0016471: dest desc {b.0x18, 0, 0x40}    ; 65-byte name slot
    B001648A: call B003472C                  ; <- your instr 182462 lives in here
    B0016490: if -k go +8                    ; K set?
    B0016492:   call B0016405                ; error normalizer (same frame)
                  -> b.0x678 := code; non-0x9011 -> report via B0015F6A,
                     rethrow (B0016446, your logged site)
    B00164B0: w comp2 b.0x678,$0x9011        ; 0x9011 = collection done -> go load
    B00164BC: name accepted: slot index * 0x41 (65-byte stride), continue loop
  after collection: per-file loader B0019914 for each collected name
```

The twin B00165FC does the same at B0016657/B0016689. [V]

## 5. The display arithmetic, re-anchored by your data [V]

Your trace: hub B0015B3F receives H1 = 0x9016; the printed word is 0x906A.
The carved hub formula is display = code - 0x443 + mem[0xB002C5DC]
(B0015E07-B0015E1C). Therefore **mem[0xB002C5DC] = 0x497 at runtime**
(0x906A - 0x9016 + 0x443), i.e. display = code + 0x54 for every code. [V - the
formula is bytes, the 0x497 value is your dynamic data through it]

0x906A as a sign-extended halfword = -28566 = 64 x (-447) + 42 -> printed
octal as "(-677:52)". So the "-677" pseudo-SSI is an artifact of pushing the
pre-based SINTRAN-family code 0x9016 through the internal-code rebase; it does
not identify a subsystem. [V]

## 6. Retractions (poisoned priors from the REFINED answer)

1. **"(-677:52) <=> internal code 0x46D (linker error 42)" - WRONG.** That
   equivalence assumed mem[0xB002C5DC] = 0xFFFF9040. Your data proves the cell
   holds 0x497 and the displayed 0x906A is raw 0x9016 + 0x54. The 0x46D raise
   sites (B001735E, B00185D2) and their segment-used-bit test remain correctly
   carved facts about THOSE routines, but they are unrelated to this event and
   never fired in your run (your breakpoint data).
2. **"Break B0015B3F and expect the segment-ATT hop" - moot** for this bug.
   You did exactly that probe; it returned 0x9016 and closed the case in the
   opposite direction. The honest-gap flag in the refined answer worked as
   intended.
3. The refined answer's claim that "0x9016 raised at B0035311 = input layer
   parks a SINTRAN error from global 0xB0048CFC" was correct as far as it
   went; this answer completes it: the parker for your event is B0035291 and
   the code is the linker's own, not a SINTRAN MON status. (The neighbouring
   path `w1 := $0xB0048F60 -> cell` at B00351E3 CAN forward an external code;
   not your case.)

## Evidence register

All PSEG addresses verified in `D:\ND\500\nd-linker\linker-b01.dom.asm`;
DSEG bytes read from `linker-b01.dom` at VA - 0xB0000000 + 0x57800:

- Routine entry B003472C (`B8 CF 00 00 01 F0`); tail B0035303-B0035318.
- Parks: B0034777 / B00351F0 (0x9011), B0035274 (0x9014), B0035291 / B003541F
  (0x9016, bytes `1A CF 00 00 90 16 C4 B0 04 8C FC`), B0033996 (0x9015).
- Length check B003527F-B003528F (`0D 53 / 61 52 / 55 01 / 55 01 / 0E 4E /
  62 4D / 56 01 / 35 D2 / CE 0D`).
- Scanner feed B00349A7-B00349CD; delimiter bitmap DSEG 0xB0048F40 = file
  0xA0740: `80 00 00 00 06 08 00 00 ...` (NUL, %, &, comma).
- Line buffer: writer B0039236 (`FD 20 D0 C4 B0 04 8F EC CE 01 00`); stream
  descriptors DSEG 0xB0049918 / 0xB004A034 / 0xB004A040 = {B0048FEC,0,FF};
  default dest DSEG 0xB0049924 = {B0048DB0,0,4F}.
- LOAD call site B0016458-B0016490; tolerance compare B00164B0
  (`2E C2 06 78 CF 00 00 90 11`); slot stride B00164C7 (`6D CD 41` = *0x41).
- Hub rebase B0015E07-B0015E1C; base cell save/restore sites B0013DE8/B0013E0D,
  B00157D0/B00157EC, B0016142.
