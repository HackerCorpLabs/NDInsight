# CARVE ANSWER - Linker LOAD error 52: the exact check, and what it means

> **RETRACTED 2026-07-18 - superseded by
> [CARVE-ANSWER-LINKER-LOAD-ERROR52-REFINED.md](CARVE-ANSWER-LINKER-LOAD-ERROR52-REFINED.md).**
> nd500x dynamic tracing disproved the central claim: B004AFBE/B004AFC3 (0x106A) is the
> STARTUP DDBTABLES check (passes; never reached during LOAD). The real "(-677:52)" is one
> error word 0x906A = linker internal error 42 (segment lacks the "Segment used" ATT bit),
> printed as DIV/MOD 64 by reporter B0035C88. ALSO: the DSEG file-offset rule used below
> (+0x58000) is WRONG; correct is +0x57800 - static DSEG content reads below are poisoned.

Answers [CARVE-REQUEST-LINKER-LOAD-ERROR52.md](CARVE-REQUEST-LINKER-LOAD-ERROR52.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom` (+ .asm). Addressing per the established
model: PSEG file offset = VA - 0xB0000000 + 0x1000, DSEG = VA - 0xB0000000 + 0x58000.
Tags: [V] = byte-cited from the .asm/.dom, [I] = inferred with stated basis.

## TL;DR

- **Error "52" is the K-error code 0x106A** (displayed low 6 bits: 0x106A & 0x3F =
  0o52). It is raised at exactly ONE place in the whole binary: **B004AFC3**. [V]
- **The decision instruction is B004AFBE/B004AFC1**: `r := $0xB00530BC;
  w comp2 r.0x0,$0x1; if = skip` - i.e. **the word at DSEG 0xB00530BC must be 1**.
  If it is not, `w1 := $0x106A; setk` and the error unwinds K-style to the command
  loop. [V]
- The check does NOT read anything DEABF produced. The resolved name is not
  consulted at the raise site - so your MON 256B output is NOT the differentiator
  (confirming your own finding). [V]
- 0xB00530BC is the base of a **0x228-byte context block** (descriptor built at
  B004B2AE-B004B2C4) with word[0] = valid/open state, word[+4] = a count, and a
  slot table at +0x78 (up to 0x64=100 entries, empty sentinel 0x3E7=999) scanned
  right after the check passes. [V for the fields used; naming it "the open-domain
  context" is I - see section 4]
- **Your script is NOT missing a command.** The basic-mode command table contains
  exactly: CLOSE, EXIT, LIST-DOMAINS, LIST-ENTRIES, LIST-STATUS, LOAD,
  OPEN-DOMAIN, SET-ADVANCED-MODE (byte-listed at DSEG 0xB0030C94 + name blob).
  There is no SET-DOMAIN / OPEN-SEGMENT in basic mode, so a real user's
  `OPEN-DOMAIN "X"; LOAD f; EXIT` cannot require one. Error 52 means OPEN-DOMAIN
  did not leave the context word at 1 - on real hardware it would have. This is an
  emulator-side state gap, not a missing script step. [V for the table; the
  real-hardware claim is I from the manual's basic workflow]

## 1. Q2/Q3: the exact raise, byte-cited

```
B004AFB3: 44 6F                    w test  b.0xBC          ; earlier result, 0 -> far exit
B004AFB5: C5 02 71                 if = go $0x271
B004AFB8: 18 CF B0 05 30 BC        r:=     $0xB00530BC     ; context base
B004AFBE: 2E 80 01                 w comp2 r.0x0,$0x1      ; word[0] == 1 ?
B004AFC1: C4 0A                    if = go $0xA            ; yes -> continue (slot scan)
B004AFC3: 0C CE 10 6A              w1 :=   $0x106A         ; THE error "52"
B004AFC7: FE 02                    setk
B004AFC9: B4 6E                    jumpg   b.0xB8          ; K-error exit
```

Display decode [V for 52; I for the left field]: the reporter prints the code's
low 6 bits as the right-hand number (0x106A & 0x3F = 0o52; sibling codes seen:
0x106C, 0x1036 at B004B017, 0x102E->0x1012 remap in the buffered reader). The
left-hand field ("-677") is not derived from the code at the raise site; your
reading (uninitialized descriptor, secondary garbage) is consistent - the blank
text comes from the error-message lookup failing. Do not chase -677.

If the check passes (word[0]==1), the code immediately scans the context's slot
table: base r.0x78, up to min(word[+4], 0x64) entries, comparing each with global
0xB0052E88 and with sentinel 0x3E7 (999); scan failure raises the sibling error
**0x1036** at B004B01B (displays as :66B). [V]

## 2. Q1: the LOAD handler / how the error surfaces

Command dispatch chain [V]:

- **B0013B41**: `init $0xB0001ABC,$0xD4,$0x10000` (PLANC stack init) then
  `call B00162FA`, then **`call B00150CD`** at B0013B5D - the command phase.
- **B00150CD** = "execute one command line" wrapper. Its body calls **B0035972**
  (read + match + dispatch one command; B0035972 uses the alternatives matcher
  B003C66E and the mode's command table). Its `entd` handler at B00150D5 catches
  **exactly w1 == 0x106A** (compare at B00150DB) and prints the two-part message
  via B004E01D + the I/O gateway B004D4F4. That is why you see the error at the
  command loop: the code K-propagates from B004AFC3 all the way up. [V]
- The per-command body that reaches B004AFC3 is dispatched table-driven inside
  B0035972; I did not statically walk the index-to-handler binding for LOAD.
  **Live shortcut: breakpoint B004AFC3 (or B004AFBE) and read the call stack -
  that gives you LOAD's body entry in one shot.** [honest gap]

Command tables [V]: basic-mode record table at DSEG **0xB0030C94** (8 records of
12 bytes: {runtime name ptr, len-1}), names blob directly after it: CLOSE, EXIT,
LIST-DOMAINS, LIST-ENTRIES, LIST-STATUS, LOAD, OPEN-DOMAIN, SET-ADVANCED-MODE.
The advanced-mode name blob (LOAD, OPEN-DOMAIN, OPEN-SEGMENT, SET-DOMAIN is NOT
in it either - closest are OPEN-SEGMENT/SET-SEGMENT-NUMBER) sits just above at
DSEG ~0xB00309xx-0xB0030C8x.

## 3. Q4: what state must exist - and the answer for your script

[V] Basic mode has no segment/domain-selection command besides OPEN-DOMAIN, so
the manual's minimal workflow cannot require one. The state LOAD requires is
**word[0xB00530BC] == 1**, and the only party that can have set it in your run is
OPEN-DOMAIN's implementation (or common file-layer code it calls).

[I - working model, byte-anchored but not fully walked]: 0xB00530BC sits directly
after the buffered-stream context cells (0xB0053068/74/78/7C/80/84, carved
earlier) and is handled as one 0x228-byte block:

```
B004B2AE: r:= $0xB00530BC
B004B2B4: w1 laddr r.0x0 ; =: b.0x104     ; block address
B004B2BB: w move $0x228,b.0x100           ; block length 0x228
B004B2C4: w smove b.0xEC,b.0x100          ; hand to generic I/O exit (jumpg b.0xE8)
```

i.e. the block is read/written as a unit through the linker's file layer - the
domain description the linker keeps for the OPEN-DOMAIN'd file. word[0] = 1 =
"context valid/domain open"; +4 = entry count; +0x78 = slot table (100 max,
999 = empty). [I]

## 4. Q5: does LOAD expect DEABF fields you do not populate?

No - not at this gate. The raise site touches only b.0xBC (an earlier boolean)
and the context word. The resolved-name descriptor is not read between the
resolve helper's success and the error. Your DEABF output is off the hook for
error 52. [V for the raise site's operands]

## 5. What to do next (live, on your side)

1. **Data watchpoint on 4 bytes at VA 0xB00530BC** (DSEG) across the whole run.
   During OPEN-DOMAIN you should see the linker store 1 there (directly or via a
   0x228-byte block load). Finding the write that does NOT happen - and which MON
   reply it depended on - is the actual bug. Candidates to watch for: a read-back
   of the just-written domain header (MON 117B RFILE) whose data your handler
   returns as zeros; or an error swallowed by an entd handler during OPEN-DOMAIN.
2. **Breakpoint B004AFBE** during LOAD: confirm word[0] is 0 in your run, and
   stack-walk to pin LOAD's body entry (closes Q1 exactly).
3. After word[0]==1 is achieved, expect the slot scan next: it compares entries
   at 0xB00530BC+0x78 against global 0xB0052E88 with sentinel 999; failure gives
   error :66B (0x1036 at B004B017) - if you hit that next, the segment-slot state
   is the follow-up carve.

## Evidence register

All from `linker-b01.dom.asm` (addresses = PSEG VAs): raise B004AFB8-B004AFC9
(bytes 18 CF B0 05 30 BC / 2E 80 01 / C4 0A / 0C CE 10 6A / FE 02); sibling
raise B004B017 (0C CE 10 36); slot scan B004AFCB-B004B015 (compares with
$0xB0052E88 and $0x3E7, cap $0x64, count r.0x4); block descriptor B004B2AE-B004B2C4
(w move $0x228 - the only 0x228 in the binary); catch B00150D5-B00150DB
(34 CE 10 6A) inside wrapper B00150CD; phase init B0013B41
(DC B0 00 1A BC CF 00 00 00 D4 CF 00 01 00 00) and call B0013B5D; matcher calls
B003C66E from B00340F5/B0034AEB/B00353DA/B0036F06; matched-index global
0xB0048F60 (stored B0034AFC). Basic command table: DSEG file bytes at 0x88C94
(records) + 0x88CF4 (names blob) - dump reproduced in this doc's section 2.
