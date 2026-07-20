# CONKI @ 040765 - KICKENT registration carve (S0-1)

**Date:** 2026-07-20
**Segment:** `026-S3IMPIT` (= `017-S3SMPIT`, byte-identical), load base 032000B
(`nd100-dis -a -o -b 13312` on the byte-swapped image; byte offset in file =
(addr - 032000B) * 2, big-endian).
**Binary:** `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\026-S3IMPIT.bin`
**Symbols:** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
(`CONKI=040765`, `OLINK=000000`, `DLEVE=000001`, `DFADD=000002`; kallsyms cross-check
`0x41F5 T CONKI`).
**NPL logic reference (DIFFERENT revision, never authoritative bytes):**
`PH-P2-OPPSTART.NPL` CENTRY @063267 (creates the 16 KICKENT triplets), OCSTART @063460
area (OCTICONT input controller). CONKI itself has NO NPL body in the repo tree [V by
grep over `SINTRAN\NPL-SOURCE\NPL\`].
**Companion:** `a-conki-040765.txt` (full annotated listing incl. call site and
receive-side consumers). Context docs: `NUCLEUS-PRIMITIVES-CARVE.md` sections 5.3/5.8,
`OCTOBUS-DRIVER-ROUTINES-CARVE.md` section 4 (SKICK).

Tags: **[V]** bytes reproduced below, **[NPL-V]** logic matches NPL, **[I]** inference,
**[OPEN]** unresolved.

---

## HEADLINE ANSWER [V]

**Incoming octobus KICK NUMBER 1 (NUCKI) dispatches to DKICK @ 044747.**

`NKINI` calls `CONKI` with **T = 1 (the kick number = KICKENT index)**, A = 14B,
X = 0 (ring), B = 125144 (datafield). CONKI writes the ring-0 input controller's
KICKENT entry for kick 1:

```
KICKENT[1].DLEVE = 14B      (dispatch code: "activate PIL level 12")
KICKENT[1].DFADD = 125144   (datafield whose word -1 = 125143 holds 044747 = DKICK)
```

On reception of a kick frame the input interrupt does
`entry := KICKENT[frame & 17B]`, computed-jumps on `DLEVE & 17B`; code 14B selects the
level-12 arm, which fires PIL level 12 with `B := DFADD (125144)` and
`P := mem[DFADD - 1] = mem[125143] = 044747 = DKICK`. This closes the loop with the
send side: `NKICK -> SKICK(A=1)` sends kick 1 [V, NUCLEUS carve 5.2] and the receiver
binds kick 1 to the DKICK drain. The [OPEN] item "exact octobus kick-1 -> datafield
wiring" from NUCLEUS-PRIMITIVES-CARVE.md 5.3 is now CLOSED.

Argument meanings (all proven from the bytes, section 3):

| Reg | Value from NKINI | Meaning |
|---|---|---|
| T | 1 | **kick number** = KICKENT index (validated 1..17B) |
| A | 14B | **DLEVE dispatch code** = target PIL level (14B octal = level 12 dec) |
| X | 0 | octobus ring (0..3) |
| B | 125144 | **DFADD** = datafield to activate (level-12 B; P fetched from DFADD-1) |

A=14B is NOT a datafield and NOT an index - it is the dispatch-table selector whose
value happens to be the octal PIL level number (12B -> level 10, 13B -> level 11,
14B -> level 12; see section 4).

---

## 1. dd reproduction of every published anchor [V]

All `dd if=026-S3IMPIT.bin bs=1 skip=OFF count=2 | xxd -p` (big-endian), except the
last block which is `044-S3IDPIT.bin` (base 004000B):

| Addr (oct) | byte off | bytes | word (oct) | what |
|---|---|---|---|---|
| 040765 | 7146 | cc 59 | 146131 | CONKI entry `RADD CLD SB DD` |
| 041002 | 7172 | 5e 12 | 057022 | `LDX I ,X 22` ring -> input ctrl df |
| 041006 | 7180 | 5f f5 | 057765 | `LDX I ,B ,X -13` KICKENT lookup |
| 041007 | 7182 | 51 f4 | 050764 | `LDT ,B -14` table bank |
| 041010 | 7184 | c6 ce | 143316 | `STDTX` write (DLEVE, DFADD) |
| 041025 | 7210 | 83 02 | 101402 | error: bad ring |
| 041026 | 7212 | 83 03 | 101403 | error: ring not configured |
| 041027 | 7214 | 83 07 | 101407 | error: bad kick number |
| 042132 | 8372 | cc 47 | 146107 | NKINI: `X := 0` (ring) |
| 042133 | 8374 | f2 01 | 171001 | NKINI: `SAT 1` (kick number 1) |
| 042134 | 8376 | f1 0c | 170414 | NKINI: `SAA 14` (DLEVE code 14B) |
| 042136 | 8380 | ba 17 | 135027 | NKINI: `JPL I [042165]` -> CONKI |
| 042164 | 8424 | aa 64 | 125144 | pointer cell: datafield arg (B) |
| 042165 | 8426 | 41 f5 | 040765 | pointer cell -> CONKI |
| 035562 | 3812 | fa 35 | 175065 | decoder: `BSKP ZRO 60 DA` (K bit test) |
| 035575 | 3834 | ba 2a | 135052 | decoder: `JPL I [035647]` kick dispatch |
| 035647 | 3918 | 3c 27 | 036047 | pointer cell -> kick dispatch |
| 036047 | 4174 | 70 38 | 070070 | `AND [036137]` frame & 17B |
| 036051 | 4178 | 5f f5 | 057765 | `LDX I ,B ,X -13` SAME KICKENT lookup |
| 036052 | 4180 | 51 f4 | 050764 | `LDT ,B -14` bank |
| 036107 | 4238 | c6 ca | 143312 | `LDDTX` read (DLEVE, DFADD) |
| 036115 | 4250 | 70 12 | 070022 | `AND [036137]` DLEVE & 17B |
| 036116 | 4252 | cc 2a | 146052 | `RADD SA DP` computed jump |
| 036133 | 4278 | a8 40 | 124100 | table[14B] -> 036233 level-12 arm |
| 036137 | 4286 | 00 0f | 000017 | the 17B mask constant |
| 036233 | 4406 | 4a 1b | 045033 | level-12 arm entry |
| 036256 | 4444 | cc 4d | 146115 | `A := DFADD` |
| 036257 | 4446 | d7 63 | 153543 | `IRW 140 DB` level-12 B := DFADD |
| 036261 | 4450 | 4c ff | 046377 | `LDA ,X -1` P from mem[DFADD-1] |
| 036262 | 4452 | d7 62 | 153542 | `IRW 140 DP` level-12 P |
| 036263 | 4454 | 48 0d | 044015 | load bit-12 mask |
| 036264 | 4456 | d0 c6 | 150306 | `MST PID` fire level 12 |
| 036266 | 4460 | 0e e7 | 007347 | pointer cell: level-pending mask [I role] |
| 036300 | 4480 | 10 00 | 010000 | bit-12 mask constant |
| 040467 | 6766 | d1 01 | 150401 | ECONID entry `IOF` (sibling coherence) |
| 040062 | 6244 | d1 01 | 150401 | CONOMD entry `IOF` (sibling coherence) |

`044-S3IDPIT.bin` (base 004000B, byte off = (addr - 004000B) * 2):

| Addr | byte off | bytes | word | what |
|---|---|---|---|---|
| 125142 | 83140 | 49 e7 | 044747 | DKICK pointer (datafield - 2) |
| 125143 | 83142 | 49 e7 | 044747 | DKICK pointer (datafield - 1) <- read by 036261 |
| 125144 | 83144 | 00 00 | 000000 | datafield head (runtime cells, zero on disk) |

Overlay / sibling coherence [V]: CONKI 040765 sits between ECONID 040467 (+ a
disconnect twin at 040676) and GSTAT 041030 in the same carve; CONOMD 040062,
ECONID 040467, CONKI 040765 all decode as parallel routine entries, and CONKI's
internal idioms (`,B -13`/`,B -14` tables, STDTX) are the exact mirror of the
receive dispatch at 036047 in the same segment. Same overlay proof as
NUCLEUS-PRIMITIVES-CARVE.md section 1 and OCTOBUS-DRIVER-ROUTINES-CARVE.md section 2.

## 2. CONKI disassembly (condensed; full listing in a-conki-040765.txt)

```
040765 RADD CLD SB DD      ; D := B (datafield arg)
040766-040772              ; ring check: 0 <= X <= 3, else A=101402, EXIT L+1
040773-041001              ; kick check: 1 <= T <= 17B, else A=101407, EXIT L+1
041002 LDX I ,X 22         ; X := mem[mem[ring + 22B]] = input controller df
041003 JXZ 041016          ; df == 0 -> A=101403, EXIT L+1
041004 B := df
041005 X := T              ; kick number
041006 LDX I ,B ,X -13     ; X := KICKENT pointer table[kick]  (df[-13])
041007 LDT ,B -14          ; T := table bank (IENTBANK)
041010 STDTX               ; mem[T:X] := A (DLEVE), mem[T:X+1] := D (DFADD)
041011 B := D              ; restore caller B
041012 RADD AD1 CLD SL DP  ; SUCCESS return L+2
```

### Pseudo-C

```c
/* CONKI: A=dispatch code (DLEVE), T=kick number, X=ring, B=datafield.
   Error -> return L+1 with A = 1014xx; success -> L+2, A preserved. */
int conki(int dleve /*A*/, int kick /*T*/, int ring /*X*/, u16 dfaddr /*B*/) {
    if (ring > 3)              return 0101402;   /* bad ring            */
    Df *in = ringTable22[ring];                  /* mem[mem[ring+22B]]  */
    if (!in)                   return 0101403;   /* not configured      */
    if (kick < 1 || kick > 017) return 0101407;  /* bad kick number     */
    u16 *e = physptr(in->bank /* [-14] */, in->kicktab /* [-13] */[kick]);
    e[0] = dleve;    /* DLEVE: dispatch code = target level (14B = lvl 12) */
    e[1] = dfaddr;   /* DFADD: datafield; e[-1] = OLINK (busy chain link)  */
    return OK;
}
```

Note the STDTX double order A-then-D matches the LDDTX convention proven in MBSEND
(record[0] -> A, record[1] -> D) [V].

## 3. What each argument is - proof from the bytes

- **T = kick number.** Range-checked 1..17B (041773-041001) and used DIRECTLY as the
  index into the df[-13] table (041005-041006). The receive dispatch indexes the same
  table with `frame & 17B` (036047-036051). So T is the incoming kick number. [V]
- **A = DLEVE dispatch code.** Stored untouched as entry word 0 (no instruction between
  entry and STDTX writes A). The receiver reads entry word 0, masks & 17B and
  computed-jumps `P := 036117 + code` (036107-036116). Code 14B lands on 036133 ->
  level-12 activation arm. So 14B means "fire PIL level 12" - the value is the octal
  level number, not a level-bit, not an address. [V]
- **B = DFADD.** Saved to D at entry, stored as entry word 1. The level-12 arm loads
  it into the level-12 B register and fetches the level-12 P from `mem[DFADD-1]`
  (036256-036264). [V]
- **X = ring**, resolved through the page-zero pointer table at 22B..25B to the
  octobus input controller datafield (identity OCTICONT [NPL-V]; the bytes only prove
  "the df whose [-13]/[-14] the kick receiver also uses"). [V mechanism]

## 4. The KICKENT structure and the receive dispatch [V]

- **KICKENT = 16 entries (kick 0..17B)**, pointer table at input-df[-13], bank at
  input-df[-14]. Created by OCSTART/CENTRY as (OLINK, DLEVE, DFADD) triplets
  [NPL-V; displacements OLINK=0 DLEVE=1 DFADD=2 from SYMBOL-1-LIST]. The stored
  pointers address the DLEVE word (base+1): the STDTX/LDDTX pair reads/writes
  (DLEVE, DFADD) at pointer+0/+1, and the busy-chain code links via `mem[pointer-1]`
  = OLINK (append routine 036022, terminator -1). [V offsets, NPL-V names]
- **Kick frame path:** input interrupt decoder 035555 tests C (bit 15) then K (bit 6);
  kick frames go via pointer cell [035647] to 036047: `entry := KICKENT[frame & 17B]`;
  `DFADD == 0` -> counted as error (df[5]++), else dispatch on `DLEVE & 17B`:

  | DLEVE & 17B | Arm | Action |
  |---|---|---|
  | 0, 1, 2 | 036301 | `B := DFADD`, `JPL I -> 013552` (datafield driver activation, same resident routine as SETEV mode 2) |
  | 5 | 036310 | fire level 5 (`SAA 40; MST PID`) |
  | 12B | 036145 | activate PIL level 10 (`IRW 120`) with B := DFADD, P := mem[DFADD-1] |
  | 13B | 036200 | activate PIL level 11 (`IRW 130`), same shape |
  | **14B** | **036233** | **activate PIL level 12 (`IRW 140`), B := DFADD, P := mem[DFADD-1], `MST PID` 010000** |
  | 3, 4, 6, 7, 10B, 11B, 15B-17B | 036313 | error (`JPL I -> 000215`) |

  Arm roles for 0-2/5 are [V control flow, I on which subsystems use them].
- **Busy handling:** each level arm first tests a bit in resident cell mem[007347]
  ([I]: level-pending mask); if the level is already activated the KICKENT entry is
  CHAINED via OLINK (036236-036243 -> append 036022) instead of re-fired. Drain of
  that chain is done by the level wait loops (WT10/WT12/WT13 family) [I, shared edge
  with the octobus module - not carved here].
- **Only the low 4 bits of the kick number select the entry** (mask 000017 at 036137),
  although SKICK accepts numbers up to 37B. Kick numbers 20B-37B alias 0-17B on this
  receiver. [V]

## 5. The NKINI binding (caller) [V]

NKINI's tail (042112-042146, full listing in a-conki-040765.txt / a-data-nkini.txt):
after checking the master version word it sets resident cells 007307 := 1 and
007310++ ([I]: NUCLEUS-connected flag + connect count), then loads B := [042164]
= 125144, X := 0, T := 1, A := 14B and `JPL I [042165] -> CONKI`. On success A := 0.

The registered datafield 125144 sits in the resident DPIT server-datafield block:
words 125142/125143 (= DFADD-2 / DFADD-1) both hold 044747 = DKICK [V,
044-S3IDPIT.bin]. The receive arm reads DFADD-1. So the binding is:

```
octobus kick frame, number & 17B == 1
  -> KICKENT[1] = (DLEVE=14B, DFADD=125144)
  -> fire PIL level 12 with B = 125144, P = [125143] = DKICK 044747
  -> DKICK drains the own-station NUCLEUS kick queue, SETEVs the kicked ports
```

Send side match: NKICK emits `SKICK(A=1 NUCKI, X=0, T=dest station)` [V, NUCLEUS
carve 5.2] - the numbers agree end to end.

## 6. ECONID @ 040467 (brief) [V structure]

ECONID does NOT share a registration helper with CONKI - it is a separate body
(entry `IOF`), managing the per-source-station IDENT entry lists (ring resolved via a
DIFFERENT page-zero table, `LDX I ,X 32`; per-station list heads at df[-7]; 3-word
ident entries scanned/allocated with the SSK flag; bank from the same df[-14]).
Error pool 101401/101402/101403/101405/101406/101433 at 040665-040672. A twin routine
at 040676 clears an entry pair (`STZTX` twice) - a disconnect counterpart
(errors 101402/101403/101430/101432) [I on its exact identity]. Only the
ring-table + df[-14] bank IDIOM is shared with CONKI, no common subroutine. [V]

## 7. Open items

- [OPEN] Exact identity of resident cell 007347 (level-pending mask) and the chain
  drain point for the busy path (WT10/WT12/WT13 loops) - octobus module edge.
- [OPEN] Roles of dispatch codes 0-2 (datafield driver activation via 013552) and 5
  (level-5 fire): which subsystems register them (grep for other CONKI callers found
  only NKINI in this carve; other callers may live in other segments).
- [I] 007307/007310 counter semantics (NUCLEUS-connected flag / connect count).
