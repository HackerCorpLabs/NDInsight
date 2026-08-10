# MON 60B subfunction 006B - ISEGLOAD (PLACE / load one segment)

Loads (places) one ND-500 segment. Copies the segment name, and if the segment has ND-100/ND-500
shared parts, also copies the shared-part info.

**Status:** dispatch byte-verified; body from `5P-P2-MON60.NPL` (`60B-006B-ISEGLOAD.npl`); L07 body
byte-location pending (bank-2 5IFUNC).

## Handler (verbatim NPL in `.npl`)
1. `A:=5P1; T:=200; CALL FRUSMOVE` - copy segment name (param1, <=200B bytes) user->MON60 buffer.
2. `IF 5D51><0` - if there are shared parts, `A:=5P5; T:=40; X:=100; CALL XFRUSMOVE` copies the
   shared-info block (param5, 40B bytes) too.
3. `GO FAR 5NOPAR` - common path performs the place.

## Contract
- `params[1]` (5P1) = segment-name string (<=200B bytes).
- `5D51` (a field of param5) != 0 => shared parts present.
- `params[5]` (5P5) = shared-info block (40B bytes) when shared.

## CALLER SIDE CARVED 2026-08-02 `[V]` - and the fetch is NOT here

The ND-500 monitor's caller is bank 1 `042115` (445 words), reached twice per
RECOVER-DOMAIN from the PLACE sequence at `043547`-`044062`. Full carve:
`SINTRAN/ND500/nd-500-mon/RECOVER-DOMAIN-WORKER-AND-SEGMENT-LOAD-CARVED-2026-08-02.md`.

`006` has exactly **two** call sites in the entire monitor image, `042230` and `042535`,
both inside `042115` - matching the two `006` seen in the live MON 60B trace.

**The caller contains no `MON` and no `IOX` instruction anywhere**, so the monitor never
reads segment content itself. Combined with step 3 below, that means **the disc fetch
lives in `5NOPAR`, the common place path** - not in this handler, whose only work is
copying the segment name. Carving `5NOPAR` is the open item; the "L07 body byte-location
pending" note below is now the thing actually blocking progress, not a loose end.

Observed parameter shape at both call sites (identical):

| Param | Passed as | How |
|---|---|---|
| 1 | value of `B-162` | `LDA ,B -162` / `STA ,X 6` - consistent with the segment-name pointer this handler expects |
| 2 | **address** `B-127` | `RADD CLD SB DA` / `AAA -127` / `STA ,X 7` |
| 3 | **address** `B-155` | same idiom |
| 4 | **address** `B-157` | same idiom |
| 5 | double from `B-135` | `SAA 11` / `SWAP CLD SA DD` / `LDT ,B -135` / `STF ,X 12` |

Three of the five are **addresses of caller locals** - output slots for SINTRAN to write
back into. Worth knowing when reading this handler's contract, which only documents
params 1 and 5.

## Byte status
VERIFIED: dispatch + 5IFUNC[006]=ISEGLOAD. From NPL: body. PENDING: L07 body address.
