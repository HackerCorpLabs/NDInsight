# SINTRAN L07 Octobus Driver Send/Receive Routines - Byte-Verified Carve

**Date:** 2026-07-19
**Segment:** `026-S3IMPIT` (load base 32000B; byte-identical twin over the same range in `017-S3SMPIT`)
**Binary:** `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\segments\026-S3IMPIT.bin`
(sha256 `0806cd3e...9ab3c4`, big-endian, byte offset of octal address A = (A - 32000B) * 2)
**Committed disassembly used:** `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm`
**NPL logic reference (DIFFERENT revision, never authoritative bytes):**
`E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\MP-P2-N500.NPL` (5MTRANS ~L2439, XKICK500 ~L3278,
XRS5CPU ~L3328, 5OMBREAD ~L3453, MFPREPARE ~L3586, CON5IDENT ~L3614, 5MRDTRANS ~L2759),
`RP-P2-N500.NPL` (XX5CONOMD ~L944).
**Symbol tables:** `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\SYMBOL-1-LIST.SYMB.TXT`
and `SYMBOL-2-LIST.SYMB.TXT` (+ `l07-kallsyms.txt` hex cross-check).

Tags: **[V]** = byte-verified in the carve (dd-reproduced or asm word cross-checked against .bin),
**[NPL-V]** = logic matches the (different-revision) NPL and the bytes are consistent with it,
**[I]** = inference, **[OPEN]** = unresolved.

---

## 0. dd reproduction of every published anchor [V]

All big-endian, `dd if=026-S3IMPIT.bin bs=1 skip=OFF count=2`:

| Addr (oct) | byte off | bytes | word (oct) | what |
|---|---|---|---|---|
| 037254 | 5464 | f8 90 | 174220 | SKICK entry `BSET ONE SSK` |
| 037320 | 5536 | 51 fd | 050775 | SKICK direct-TX IOXT block start |
| 037425 | 5674 | d1 01 | 150401 | MBSEND entry `IOF` |
| 037576 | 5884 | 48 24 | 044044 | MBSEND level-13 fire (loads SOCTW ptr) |
| 037660 | 5984 | d1 01 | 150401 | OMBREAD entry `IOF` |
| 143445 | 75338 | 1a 62 | 015142 | 5MTRANS entry `STX I 142` |
| 144014 | 75800 | c9 e0 | 144740 | pointer cell -> 5MRDTRANS |
| 146526 | 78508 | 08 fc | 004374 | XKICK500 entry `STA -4` |
| 146632 | 78644 | 3e ac | 037254 | pointer cell -> SKICK |
| 146750 | 78800 | 01 00 | 000400 | MPFATAL array word 0 (bytes 1,0) |
| 146756 | 78812 | 48 7b | 044173 | 5OMBREAD entry |
| 147171 | 79090 | 83 08 | 101410 | the OMBREAD "no message" status constant |
| 147300 | 79232 | 58 16 | 054026 | MFPREPARE entry `LDX 26` |
| 147330 | 79280 | 3f 15 | 037425 | pointer cell -> MBSEND |

## 1. Symbol addresses and source tables [V]

| Routine | L07 symbol | Address | Source table |
|---|---|---|---|
| SKICK (and SIDEN at +2) | `SKICK` | 037254 (SIDEN=037256) | SYMBOL-1-LIST |
| MBSEND | `MBSEN` | 037425 | SYMBOL-1-LIST |
| OMBREAD | `OMBRE` | 037660 | SYMBOL-1-LIST |
| CONOMD / ECONID (context) | `CONOM` / `ECONI` | 040062 / 040467 | SYMBOL-1-LIST |
| 5MTRANS | `5MTRA` | 143445 | SYMBOL-2-LIST |
| 5MRDTRANS | `5MRDT` | 144740 | SYMBOL-2-LIST |
| XKICK500 (LV12KICK inside) | `XKICK` | 146526 (LV12KICK=146555) | SYMBOL-2-LIST |
| XRS5CPU / RS5CPU | `XRS5C` / `RS5CP` | 146642 / 146700 | SYMBOL-2-LIST |
| 5OMBREAD / I5OMBR / GN5CPUDF | `5OMBR` / `I5OMB` / `GN5CP` | 146756 / 147240 / 147252 | SYMBOL-2-LIST |
| CON5OMD | `CON5O` | 147271 | SYMBOL-2-LIST |
| MFPREPARE | `MFPRE` | 147300 | SYMBOL-2-LIST |
| CON5IDENT | `CON5I` | 147334 | SYMBOL-2-LIST |

`l07-kallsyms.txt` hex values agree for all of the above (e.g. 0xCD56=146526B, 0xCEC0=147300B). [V]

## 2. Overlay proof (why these bytes ARE these routines) [V]

The 14xxxx ND-500 handler family was already byte-located in `017-S3SMPIT`/`026-S3IMPIT`
(identical bytes, load 32000B) - see `re\ND500-HANDLERS-OVERLAY.md` (STAPROC 140356 etc.).
For THIS routine set the proof is threefold:

1. **Uniform NPL offset.** Every routine in the MP-P2-N500 block sits at
   `L07 = NPL-listing-address + 200B`: XKICK500 146326->146526, LV12KICK 146355->146555,
   XRS5CPU 146442->146642, RS5CPU 146500->146700, 5OMBREAD 146556->146756, I5OMBR 147040->147240,
   GN5CPUDF 147052->147252, CON5OMD 147071->147271, MFPREPARE 147100->147300,
   CON5IDENT 147134->147334, MBSUSPROC 147207->147407, 5MTRANS 143245->143445,
   5MRDTRANS 144540->144740, CHEVENT 143621->144021, FIN 144372->144572, XRXX 144361->144561,
   CHDISKADDR 144426->144626. Fourteen siblings, one offset, zero exceptions. [V]
2. **Literal-pool cross-references resolve to named symbols.** Inside the carved bodies, the
   indirect-call pointer cells contain exactly the L07 symbol values:
   `[146632]=037254` SKICK, `[146677]=037425` MBSEND (XRS5CPU), `[147330]=037425` MBSEND
   (MFPREPARE), `[147404]=037425` MBSEND (CON5IDENT), `[147152]=037660` OMBREAD,
   `[147400]=040467` ECONID, `[147276]=040062` CONOMD, `[147251]=146756` 5OMBREAD loop,
   `[146744]=146642` XRS5CPU, `[146745]=145372` XTER500, `[147161]=146100` XRSTARTALL,
   `[144613]=145466` XACTRDY, `[144014]=144740` 5MRDTRANS, `[144005]=144626` CHDISKADDR,
   `[143630]=023706` SLOCK, `[143633]=024041` SUNLOCK, `[143640]=026656` GETOUT,
   `[144617]=026701` PTFREE, `[143632]=022704` IFM500XQ, `[144612]=030332` 5MONICO,
   `[144615]=030325` 5EMONICO, `[144017]=023670` WN5STATUS, `[144007]=073024` CHFIX,
   `[143616]=010376` LOGPH, `[146635]=012210` 9ERR, `[147245]=012325` 9FLER,
   `[146637]=072172` YWAIT, `[144621 region]=135067` NXTMSG, `[146627]=[146747]=[147331]=033616`
   WT12, `[147250]=033613` ID12. Every one is a named L07 symbol with the right role. [V]
3. **Constants match the L07 constant symbols**: FN5DEST=70, LN5DEST=77, OMDACCP=3, MFOMDNO=4,
   N100IDENT=1, CMSYSPAR=16, CMCPURES=71 all appear as immediates/pool constants exactly where
   the logic needs them. [V]

---

## 3. Recovered data structures (from bytes)

### 3.1 LMFIELD - the multibyte message record @ 011545 (resident DPIT data, bank 1) [V]

`"LMFIELD" = 011545`, `"LMDF" = 011537` (both resident cells; pool constants in XRS5CPU /
5OMBREAD / MFPREPARE / CON5IDENT / I5OMBR). The physical-address constant handed to
MBSEND/OMBREAD is the SAME value 011545, i.e. **DPITPHYS = 0** in this build, and
**DPITBANK = 1** (`SAA 1` before every MBSEND/OMBREAD call). [V]

| Word | Send use | Receive use (written by OMBREAD) |
|---|---|---|
| +0 | MOCTSTATION = destination station | source station (frame bits 13-8, masked 77B) |
| +1 | MOCTOMD = destination OMD | source OMD (from buffer word 3) [V store, I meaning] |
| +2 | MBROADCAST (0 = unicast; nonzero sets frame bit 14) | broadcast flag (frame bit 14, 0/1) |
| +3 | MMSGLENGTH in BYTES (valid 1..377B) | received byte count |
| +4 | MCOMMAND / MSTS (command or status in HIGH byte) = "S4" | ETYPE (hi=status/SEC "CSTS", lo=reporting source "CMICP") |
| +5 | MDP1 / "S5" first parameter | S5 (e.g. shadow-process patch target region) |
| +6, +7 | S6, S7 | payload continues |

Payload copied to/from LMFIELD+4 onward (MBSEND copies `(len+1)/2` words from LMFIELD+4;
OMBREAD copies the received bytes to LMFIELD+4). [V]

`LMDF[0]` = **5OMDNO** - the ND-100's reserved receive OMD, written by CON5OMD from CONOMD's
return value (`STA ,B 0` at 147274). L07 symbol `5OMDN=000000` is the displacement, i.e.
5OMDNO is NOT a constant; it is LMDF word 0, allocated at runtime. [V]

### 3.2 ND-500/5000 CPU datafield offsets used here [V]

| Offset | Field | Evidence |
|---|---|---|
| -14B | CPUNO | 5OMBREAD 147040 `LDA ,B -14` -> LMFIELD[0] |
| 17B | 5STATION | LV12KICK 146560, XRS5CPU 146646, CON5IDENT 147335/147345, GN5CPUDF 147260 |
| 22B | MAILINK | RS5CPU 146710 (`-1` = empty test) |
| 27B | CPUAVAILABLE | RS5CPU 146705, 5OMBREAD 147016 |
| 46B | 5CPUDFSZ (df stride) | `AAB 46` at 146735 and 147264 |

CPUAVAILABLE bits: **5ALIVE = bit 13 (decimal)** (`BSET/BSKP ONE 150 DA`), **5CPUTYPE mask = 7**,
**SAMSON = 3**, MPNACTIVE = 0 (X5CPU compare). CPU df list bounds: `"S5CPUDF"` cell = 052222,
`"E5CPUDF"` cell = 052404. 5MPM mailbox offset X5CPU=4 (`AAX 4` in RS5CPU; matches
N500-SYMBOLS). 5MBBANK lives in resident cell **004654**. [V]

### 3.3 Octobus driver objects (resident, runtime-initialized) [V structure, I on some roles]

- **Ring/interface table:** ring number X (0..3) -> interface datafield via `mem[mem[X+77]]`
  (helper 036765) / `mem[mem[X+125]]` (SKICK). OCTORING = 0 everywhere here (`SAX 0`;
  L07 symbol `OCTOR=000000`).
- **Per-OMD entry table:** entry = `mem[ df[-11] + OMD ]` (idiom `LDX I ,B ,X -11` in both
  MBSEND and OMBREAD, X = OMD number). Entry fields used: `[-7]` current buffer, `[-6]`
  received-message chain head (OMBREAD pops it), `[-4]/[-5]` saved caller L / caller B,
  `[-2]` back-pointer to the output datafield [I], `[-10]` message count, `[-11]/[-12]`
  accumulated status words (cleared when read) [I on exact roles].
- **TX datafield:** `df[-3]` = output register base (= HDEV+4; SKICK does `AAT 3` -> control
  100407, `AAT -2` -> transmit 100405, `AAT 2` + `SAA 1` -> control 100406+1?; see section 4),
  `df[-7]` = transmitter-idle flag (nonzero = idle; cleared when a TX is started),
  `df[-10]` ring in-pointer, `df[-11]` ring out-pointer [I], `df[-12]` ring size,
  `df[-13]` ring buffer base, `df[4]/df[5]` multibyte TX queue head/tail.
- **Buffer pool (CBPOOL):** free-list head = resident cell **007341**, free count = **007342**
  (MBSEND pops: `head=:buf; buf[0]=:head; count-1`; helper 036626 pushes back and `MIN` the
  count). Buffer record: `[0]` link, `[2]` OMD (send), `[3]` source OMD (receive) [I],
  `[4]` frame descriptor word `station<<8 | OMD` (+bit 14 if broadcast; M bit 5 set by MBSEND;
  C and S added on the wire by SOCTW), `[5]` byte length (send), `[6]` received byte count,
  `[7]` data-area address, `[10B]` data-area bank. [V for the offsets exercised here]

### 3.4 Octobus driver status codes [V values]

- **101410** = "no message available" - OMBREAD's benign empty status; 5OMBREAD's ERR path
  ignores exactly this value (compare constant in cell 147171). Any other A on the error
  return becomes a SEC code with station 0.
- MBSEND error exits load T from a pool of **101426 / 101427 / 101430 / 101431**
  (cells 037645-037650): 101427 = bad length (len-1 > 376B), 101430 = bad destination station
  (station-1 > 75B, i.e. legal 1..76B), 101431 = bad destination OMD (> 17B), 101426 = reached
  via 037621 (path not exercised by the flows traced here) [V values, I assignment of 101426].
- SKICK error exits return small codes in A: 13B = TX ring full, 14B = bad station
  (station & 77B != station), 15B (037404, shared exit), 16B = bad kick number (> 37B),
  17B = ring not configured (table entry 0), 20B = bad ring number. [V values, I meanings]

### 3.5 5MPM ND-500 message displacements consumed by 5MTRANS [V]

All confirmed by `AAX` arithmetic against the message pointer (X) with T=5MBBANK:

| Displacement | Symbol | Seen at |
|---|---|---|
| 100B | 5MNWA (nowait/function double) | 143452 |
| 105B | 5MREQ | 143661 (via 115-10), 145005 |
| 106B | 5MEMA (memory address, double) | 143650 (111-3) |
| 110B | 5MLGN (logical device no) | 143461 |
| 111B | 5MDIS (disk function) | 143560 |
| 112B | 5DSEC (start sector, double) | 143653 (106+4) |
| 115B | 5MNOS (number of sectors) | 143656 |
| 147B | PLINK | 143531 |

Process/status constants: 5MWAIT = 22B (SAA 22 before WN5STATUS at 143767; `AAA -22` in
5MRDTRANS 145003), 5PRDSIZE = 10B, "S500S" = 115542, 5SWPROC cell = 011254. [V]

### 3.6 Disk access queue element (built by 5MTRANS) [V]

B = element from the QP100 pool (`"QP100"` cell value 033315; free count 5MQCU at pool df
offset 13B; element taken with GETOUT):

| Offset | Field |
|---|---|
| 1 | RTRES (shadow RT reservation) |
| 5 | NLINK (ready-queue link) |
| 14B | ABFUN (DISID & 300B + 60/61/66 function code) |
| 15B/16B | MEMAD (double) |
| 17B/20B | ABPA2 (ABP21 hi / lo) |
| 21B | ABP31 (sector count) |
| 25B | REQID |
| 26B | ADMESS (message address) |
| 27B | 5MNOWAIT |

Disk function codes written to ABFUN low bits: **60** = read + clear cache, **61** = write
(5MDIS&77 = 1 or 7), **66** = read without cache clear (5MDIS&77 = 6); anything else ->
error 7 (illegal function). Phoenix disks (S10=20B in the HTABL entry): ABP21&7 << 11B folded
into ABFUN, ABP21 zeroed. [V]

### 3.7 Resident work cells for the ND-500 driver level (all [V] from pool refs)

CMSGA=011160, XC5CPUDF=011157, DDFADDR=011161, NWAIT=011162, 5MFNC=011163, DISID=011164,
CUREL=011165, 5MWQU=011167/011170 (double), 5DSKC=011171, N5MESSAGE=011260.

---

## 4. SKICK / SIDEN @ 037254 - send single octobus control frame [V]

Entry (from LV12KICK): T = destination station, X = octobus ring (0), A = kick number.
SIDEN (037256) is the same body with the K flag cleared -> sends an IDENT frame instead.
Error return = EXIT to L+1 with code in A; success = EXIT to L+2 (`RADD AD1 0 DL` first).

```
037254 174220 BSET ONE SSK        ; SKICK: flag := 1 (build a KICK frame)
037255 124002 JMP 037257
037256 174020 BSET ZRO SSK        ; SIDEN: flag := 0 (build an IDENT frame)
037257 146151 RADD CLD SA DD      ; D := A            (save kick/ident number)
037260 133403 JXN 037263          ; ring != 0?
037261 173775 AAX -3              ;   ring 1..3 check: X-3
037262 141070 SKP IF 0 GRE SX     ;   X <= 3 ?
037263 124113 JMP 037376          ;   no -> error A=20B (bad ring)
037264 173403 AAX 3               ;   restore X
037265 057125 LDX I ,X 125        ; X := mem[mem[X+125]] = ring's interface datafield
037266 133112 JXZ 037400          ; not configured -> error A=17B
037267 146165 RADD CLD ST DA      ; A := T (station)
037270 070124 AND 037414(=000077) ; A := station & 77B
037271 140065 SKP IF DA EQL ST    ; masked value must equal station
037272 124114 JMP 037406          ;   no -> error A=14B (bad station)
037273 146115 RADD CLD SD DA      ; A := D (kick number)
037274 146151 RADD CLD SA DD      ; D := A
037275 070120 AND 037415(=000037) ; A := kick & 37B
037276 140015 SKP IF DA EQL SD    ; must be <= 37B
037277 124103 JMP 037402          ;   no -> error A=16B (bad kick no)
037300 175220 BSKP ONE SSK        ; kick entry?
037301 124002 JMP 037303          ;   ident: skip
037302 174265 BSET ONE 60 DA      ;   kick: set bit 6 (K) in A
037303 146151 RADD CLD SA DD      ; D := low frame bits
037304 150001 TRA STS             ; (save caller flags)
037305 176775 BLDA 170 DA         ; pick up C-bit position (bit 15)
037306 146115 RADD CLD SD DA      ; A := D
037307 144037 SWAP SB DX          ; X <-> B  (B := interface df)
037310 150401 IOF
037311 014314 STX 037225          ; save old B in cell 037225
037312 156010 SHT ZIN 10          ; T := station << 8   (bits 13-8)
037313 146056 RADD SA DT          ; T := station<<8 | K | number   (MALTF)
037314 174376 BSET ONE 170 DT     ; set bit 15 (C) -> complete control frame
037315 146161 RADD CLD ST DD      ; D := frame
037316 044771 LDA ,B -7           ; transmitter idle flag
037317 131020 JAZ 037337          ; busy (0) -> queue the frame
; --- direct transmit (interface idle) ---
037320 050775 LDT ,B -3           ; T := output register base (HDEV+4)
037321 173003 AAT 3               ; T := base+3  (output CONTROL, 100407)
037322 170404 SAA 4               ; A := 4  (transmit enable)
037323 150415 IOXT                ; control := 4
037324 173376 AAT -2              ; T := base+1  (TRANSMIT DATA, 100405)
037325 146115 RADD CLD SD DA      ; A := frame
037326 150415 IOXT                ; write the 16-bit frame
037327 173002 AAT 2               ; T := base+3 again
037330 170401 SAA 1               ; A := 1  (interrupt enable)
037331 150415 IOXT                ; control := 1
037332 044774 LDA ,B -4           ; touch df[-4] (read-modify, TX bookkeeping)
037333 004774 STA ,B -4
037334 000771 STZ ,B -7           ; idle flag := 0  (transmitter now busy)
037335 146404 RADD AD1 0 DL       ; L++  (success return)
037336 124032 JMP 037370          ; -> common exit
; --- queue path (transmitter busy) ---
037337 146115 RADD CLD SD DA      ; A := frame
037340 054767 LDX ,B -11          ; ring out-pointer
037341 140007 SKP IF DX EQL 0
037342 124007 JMP 037351
037343 054770 LDX ,B -10          ; in-pointer
037344 173401 AAX 1
037345 050766 LDT ,B -12          ; ring size
037346 140067 SKP IF DX EQL ST
037347 124002 JMP 037351
037350 124040 JMP 037410          ; ring FULL -> error A=13B
037351 054770 LDX ,B -10          ; in-pointer
037352 050767 LDT ,B -11
037353 173377 AAT -1
037354 142067 SKP IF DX UEQ ST
037355 124033 JMP 037410          ; full -> error A=13B
037356 007765 STA I ,B ,X -13     ; ringbuf[in] := frame  (base ptr at df[-13])
037357 050766 LDT ,B -12
037360 173377 AAT -1
037361 140067 SKP IF DX EQL ST    ; wrap?
037362 124003 JMP 037365
037363 146107 RADD CLD 0 DX       ;   X := 0
037364 124002 JMP 037366
037365 173401 AAX 1               ;   else X++
037366 014770 STX ,B -10          ; in-pointer update
037367 146404 RADD AD1 0 DL       ; L++ (success)
; --- common exit ---
037370 050235 LDT 037225          ; T := saved B
037371 146163 RADD CLD ST DB      ; restore B
037372 175220 BSKP ONE SSK
037373 124002 JMP 037375
037374 150402 ION                 ; (kick entry re-enables interrupts)
037375 146142 EXIT
; --- error exits (A = code, EXIT to L+1) ---
037376 044020 LDA 037416? -> A:=20B  037377 EXIT     ; bad ring
037400 044017 A:=17B               037401 EXIT       ; ring not configured
037402 044016 A:=16B               037403 EXIT       ; bad kick number
037404 044015 A:=15B               037405 EXIT       ; (shared exit, other caller)
037406 044014 A:=14B               037407 EXIT       ; bad station
037410 044013 A:=13B  037411 JMP 037370              ; ring full (restores B first)
```

(The `LDA 20/17/16/15/14/13` forms are immediate-style short loads; codes [V].)

**Frame built [V]:** `100000B(C) | K(bit6, kick only) | station<<8 | number(5..0)` - exactly the
section-3 frame model of `OCTOBUS-ND100-ND5000-REFERENCE.md`, now per-instruction verified.

### Pseudo-C

```c
/* SKICK: A=kick number, T=dest station, X=ring; returns error code in A or OK.
   SIDEN: identical, sends IDENT (no K bit). */
int skick(int kick, int station, int ring, bool isKick) {
    if (ring > 3) return E20_BAD_RING;
    IfDf *df = ringTable[ring];                 /* mem[mem[ring+125]] */
    if (!df) return E17_NOT_CONFIGURED;
    if (station & ~077) return E14_BAD_STATION;
    if (kick & ~037)    return E16_BAD_KICK;
    u16 frame = 0100000 | (station << 8) | kick | (isKick ? 0100 : 0);
    disable_interrupts();
    if (df->idleFlag /* -7 */) {                /* transmitter idle: send now */
        iox(df->outBase + 3, 4);                /* 100407 := 4 transmit enable */
        iox(df->outBase + 1, frame);            /* 100405 := frame             */
        iox(df->outBase + 3, 1);                /* 100407 := 1 int enable      */
        df->idleFlag = 0;
    } else {                                    /* queue in single-frame ring  */
        if (ringFull(df)) return E13_RING_FULL;
        df->ringBase[df->in] = frame;           /* base at df[-13]             */
        df->in = (df->in + 1 == df->ringSize) ? 0 : df->in + 1;
    }
    enable_interrupts();
    return OK;
}
```

---

## 5. MBSEND @ 037425 - send multibyte message [V]

Entry: T = source OMD (5OMDNO), X = ring (0), D = physical address of the message record
(LMFIELD), A = bank (DPITBANK=1), B = caller's datafield (LMDF / working field).
Error return = EXIT to L+1 with SEC-style code in A; success = L+2.

**MBSEND contains no IOXT.** It validates, copies the payload into a pool buffer, queues the
buffer on the output datafield, and if the transmitter is idle FIRES LEVEL 13 with
P := SOCTW (036342) to start transmission. The wire work (SOMB/data/EOMB) is done by SOCTW
on the octobus TX interrupt level. [V]

```
037425 150401 IOF
037426 004376 STA 037424          ; save bank param (local cell)
037427 146135 RADD CLD SB DA
037430 004057 STA 037507          ; save caller B
037431 146143 RADD CLD SL DB      ; B := L   (helper uses B for error unwind)
037432 135051 JPL I [037503]=036765 ; helper: validate ring/OMD entry + level check
037433 124156 JMP 037611          ;   error -> restore B, ION, EXIT (code in A)
037434 146134 RADD CLD SB DL      ; L := B  (restore return address)
037435 057047 LDX I ,X 47         ; X := mem[mem[X+47]]  (interface df)
037436 146173 RADD CLD SX DB      ; B := interface/output df
037437 146167 RADD CLD ST DX      ; X := T = OMD number
037440 057767 LDX I ,B ,X -11     ; X := per-OMD entry  (table at df[-11])
037441 146173 RADD CLD SX DB      ; B := OMD entry
037442 045043 LDA I 037505        ; A := free-buffer count  (cell 007342)
037443 131152 JAZ 037615          ;   none -> error path (restore B, EXIT)
037444 055042 LDX I 037506        ; X := free-list head    (cell 007341)
037445 046000 LDA ,X 0
037446 005040 STA I 037506        ; head := buf.link    (POP buffer)
037447 045036 LDA I 037505
037450 172777 AAA -1
037451 005034 STA I 037505        ; count--
037452 044035 LDA 037507          ; caller B
037453 004773 STA ,B -5           ; entry[-5] := caller B
037454 146145 RADD CLD SL DA
037455 004774 STA ,B -4           ; entry[-4] := return L
037456 012002 STT ,X 2            ; buf[2] := source OMD
037457 044345 LDA 037424          ; A := bank param
037460 150402 ION
037461 014771 STX ,B -7           ; entry[-7] := buffer
037462 146174 RADD CLD SX DL      ; L := buffer (temp)
037463 146156 RADD CLD SA DT      ; T := bank
037464 146117 RADD CLD SD DX      ; X := record phys addr
037465 143302 LDDTX               ; A,D := record[0],record[1]  (station, OMD)
037466 172777 AAA -1
037467 130403 JAN 037472          ; station-1 < 0 -> bad
037470 172703 AAA -75
037471 141050 SKP IF 0 GRE SA     ; station-1-75 <= 0 ?
037472 124133 JMP 037625          ;   no -> error T:=101430 (station must be 1..76B)
037473 172476 AAA 76              ; restore
037474 144015 SWAP SD DA          ; A := OMD word, D := station
037475 130403 JAN 037500
037476 172761 AAA -17
037477 141050 SKP IF 0 GRE SA     ; OMD <= 17B ?
037500 124127 JMP 037627          ;   no -> error T:=101431
037501 172417 AAA 17              ; restore OMD
037502 124006 JMP 037510
; (037503-037507: literal/local pool: 036765 helper ptr, 007342, 007341, saved-B)
037510 156210 SHD ZIN 10          ; D := station << 8
037511 146051 RADD SA DD          ; D := station<<8 | OMD    (descriptor)
037512 173402 AAX 2
037513 143300 LDATX               ; A := record[2] = MBROADCAST
037514 131002 JAZ 037516
037515 174361 BSET ONE 160 DD     ;   nonzero -> set bit 14 (B) in descriptor
037516 146115 RADD CLD SD DA
037517 174255 BSET ONE 50 DA      ; set bit 5 (M = multibyte)
037520 144047 SWAP SL DX          ; X := buffer
037521 006004 STA ,X 4            ; buf[4] := descriptor (C/S added by SOCTW on wire)
037522 144047 SWAP SL DX
037523 173401 AAX 1
037524 143300 LDATX               ; A := record[3] = MMSGLENGTH (bytes)
037525 130403 JAN 037530
037526 064113 SUB 037641(=000377)
037527 141050 SKP IF 0 GRE SA     ; 1 <= len <= 377B ?
037530 124073 JMP 037623          ;   no -> error T:=101427 (max 255 bytes!)
037531 060110 ADD 037641          ; restore len
037532 144047 SWAP SL DX
037533 006005 STA ,X 5            ; buf[5] := byte length
037534 176605 BLDA 0 DA           ; (flag pickup for odd-length rounding)
037535 144047 SWAP SL DX
037536 173401 AAX 1               ; X := record+4 (payload start)
037537 146171 RADD CLD SX DD      ; D := source address
037540 144046 SWAP SL DT
037541 146167 RADD CLD ST DX      ; X := buffer
037542 052010 LDT ,X 10           ; T := buffer data bank
037543 056007 LDX ,X 7            ; X := buffer data area
037544 156577 SHA ZIN SHR 1       ; words := bytes >> 1
037545 175220 BSKP ONE SSK
037546 124002 JMP 037550
037547 172401 AAA 1               ;   odd -> words+1  (round up)
037550 144045 SWAP SL DA
037551 143110 MOVEW               ; copy payload record+4.. -> buffer data area
037552 050771 LDT ,B -7           ; T := buffer
037553 000771 STZ ,B -7           ; entry[-7] := 0
037554 146131 RADD CLD SB DD      ; D := entry
037555 044776 LDA ,B -2
037556 146153 RADD CLD SA DB      ; B := entry[-2] = output datafield
037557 150401 IOF
037560 054405 LDX ,B 5            ; TX queue tail
037561 140007 SKP IF DX EQL 0
037562 124004 JMP 037566
037563 010404 STT ,B 4            ;   empty: head := buf
037564 010405 STT ,B 5            ;          tail := buf
037565 124003 JMP 037570
037566 012000 STT ,X 0            ;   else: tail.link := buf
037567 010405 STT ,B 5            ;         tail := buf
037570 146167 RADD CLD ST DX
037571 002000 STZ ,X 0            ; buf.link := 0
037572 044771 LDA ,B -7           ; transmitter idle flag (output df)
037573 131007 JAZ 037602          ;   busy -> interrupt service will drain queue
037574 146135 RADD CLD SB DA
037575 153553 IRW 150 DB          ;   idle: level-13 B := output df
037576 044044 LDA 037642(=036342) ;   A := SOCTW entry address
037577 153552 IRW 150 DP          ;   level-13 P := SOCTW
037600 044043 LDA 037643(=020000) ;   A := bit 13 mask
037601 150306 MST PID             ;   FIRE level 13 -> SOCTW transmits
037602 150402 ION
037603 146113 RADD CLD SD DB      ; B := entry
037604 044774 LDA ,B -4
037605 146154 RADD CLD SA DL      ; L := saved return
037606 044773 LDA ,B -5
037607 146153 RADD CLD SA DB      ; B := saved caller B
037610 146542 RADD AD1 CLD SL DP  ; return L+1  (SUCCESS)
; --- error paths ---
037611 146134 RADD CLD SB DL      ; (helper error) L := B
037612 050275 LDT 037507          ; restore caller B
037613 146163 RADD CLD ST DB
037614 124023 JMP 037637          ; ION; EXIT  (A = helper's code)
037615 044272 LDA 037507          ; (no free buffer) restore B
037616 146153 RADD CLD SA DB
037617 044025 LDA 037644(=101426)?; A := error status
037620 124017 JMP 037637
037621 050024 LDT 037645(=101426)
037622 124006 JMP 037630
037623 050023 LDT 037646(=101427) ; bad length
037624 124004 JMP 037630
037625 050022 LDT 037647(=101430) ; bad station
037626 124002 JMP 037630
037627 050021 LDT 037650(=101431) ; bad OMD
037630 054771 LDX ,B -7           ; un-pop the buffer:
037631 135020 JPL I [037651]=036626 ; release buffer back to free pool
037632 044774 LDA ,B -4
037633 146154 RADD CLD SA DL
037634 044773 LDA ,B -5
037635 146153 RADD CLD SA DB
037636 146165 RADD CLD ST DA      ; A := error status (1014xx)
037637 150402 ION
037640 146142 EXIT                ; return L+1 (ERROR)
; pool: 037641=000377 037642=036342(SOCTW) 037643=020000 037645..037650=101426,101427,101430,101431
; 037651=036626(buffer-release helper)
```

### Pseudo-C

```c
/* MBSEND: T=srcOmd, X=ring, D=recPhys, A=bank, B=callerDf. */
int mbsend(int srcOmd, int ring, u16 recPhys, int bank) {
    Entry *e = validate(ring, srcOmd);          /* helper 036765; error unwinds */
    if (freeCount == 0) return E_NO_BUFFER;     /* cells 007342/007341 */
    Buf *buf = popFreeList();
    buf->omd = srcOmd;                          /* [2] */
    u16 station = mem[bank][recPhys + 0];       /* MOCTSTATION */
    u16 dstOmd  = mem[bank][recPhys + 1];       /* MOCTOMD     */
    if (station < 1 || station > 076) { release(buf); return 0101430; }
    if (dstOmd > 017)                 { release(buf); return 0101431; }
    u16 desc = (station << 8) | dstOmd | 0x0020 /* M bit 5 */;
    if (mem[bank][recPhys + 2] != 0) desc |= 0x4000;   /* B bit 14 */
    buf->desc = desc;                           /* [4]; SOCTW adds C (and S on SOMB) */
    u16 len = mem[bank][recPhys + 3];           /* bytes */
    if (len < 1 || len > 0377)        { release(buf); return 0101427; }
    buf->len = len;                             /* [5] */
    movew(&mem[bank][recPhys + 4], buf->data, (len + 1) / 2);
    appendTxQueue(outputDf, buf);               /* df[4]/df[5] head/tail */
    if (outputDf->idleFlag) {                   /* transmitter idle */
        LEVEL13.B = outputDf; LEVEL13.P = SOCTW; /* 036342 */
        trigger_level(13);                      /* SOCTW emits SOMB/data/EOMB */
    }
    return OK;
}
```

---

## 6. OMBREAD @ 037660 - read received multibyte message [V]

Entry: T = OMD number (5OMDNO), X = ring (0), D = physical address of the receive record
(LMFIELD), A = bank, B = caller df. Error return = L+1 (A = status; **101410 = no message**),
success = L+2 with the record filled in.

```
037660 150401 IOF
037661 004373 STA 037654          ; save bank
037662 146135 RADD CLD SB DA
037663 004367 STA 037652          ; save caller B
037664 146143 RADD CLD SL DB      ; B := L for helper unwind
037665 135126 JPL I [040013]=036765 ; same validate helper as MBSEND
037666 124120 JMP 040006          ;   error -> restore B, ION, EXIT
037667 146134 RADD CLD SB DL      ; L := B
037670 057124 LDX I ,X 124        ; X := interface df (input side)
037671 146173 RADD CLD SX DB
037672 146167 RADD CLD ST DX      ; X := OMD number
037673 057767 LDX I ,B ,X -11     ; X := per-OMD entry
037674 146173 RADD CLD SX DB      ; B := entry
037675 050357 LDT 037654          ; T := bank
037676 146145 RADD CLD SL DA
037677 004774 STA ,B -4           ; entry[-4] := return L
037700 044352 LDA 037652
037701 004773 STA ,B -5           ; entry[-5] := caller B
037702 054772 LDX ,B -6           ; X := received-message chain head
037703 133055 JXZ 037760          ;   empty -> status/empty exit
037704 046000 LDA ,X 0
037705 004772 STA ,B -6           ; pop message buffer from chain
037706 150402 ION
037707 046004 LDA ,X 4            ; A := descriptor (source station<<8 | OMD ...)
037710 146154 RADD CLD SA DL      ; L := descriptor (temp)
037711 156570 SHA ZIN SHR 10      ; A := descriptor >> 8
037712 070103 AND [040015]=000077 ; A := source station
037713 144017 SWAP SD DX          ; X := record phys addr (D param)
037714 143304 STATX               ; record[0] := source station
037715 144017 SWAP SD DX
037716 046003 LDA ,X 3            ; A := buf[3]  (source OMD)
037717 144017 SWAP SD DX
037720 173401 AAX 1
037721 143304 STATX               ; record[1] := buf[3]
037722 044333 LDA 037655(=040000) ; bit-14 mask
037723 144445 RAND SL DA          ; A := descriptor & 040000
037724 156562 SHA ZIN SHR 16      ; A >>= 14 -> 0/1
037725 173401 AAX 1
037726 143304 STATX               ; record[2] := broadcast flag
037727 144017 SWAP SD DX
037730 046006 LDA ,X 6            ; A := received byte count
037731 144017 SWAP SD DX
037732 173401 AAX 1
037733 143304 STATX               ; record[3] := byte count
037734 146575 RADD AD1 CLD SX DA  ; A := record+4  (payload destination)
037735 146154 RADD CLD SA DL      ; L := dest addr
037736 146117 RADD CLD SD DX
037737 014771 STX ,B -7           ; entry[-7] := buffer
037740 150401 IOF
037741 026007 LDD ,X 7            ; A,D := buf[7],buf[10] = data addr + bank
037742 020314 STD 037656          ; save copy-source pair
037743 046005 LDA ,X 5            ; A := buf[5]
037744 172401 AAA 1
037745 156577 SHA ZIN SHR 1       ; words := (len+1) >> 1
037746 146167 RADD CLD ST DX      ; X/T register setup for MOVEW
037747 146146 RADD CLD SL DT
037750 146154 RADD CLD SA DL
037751 024305 LDD 037656
037752 150402 ION
037753 143110 MOVEW               ; copy payload buffer -> record+4..
037754 054771 LDX ,B -7
037755 000771 STZ ,B -7
037756 135040 JPL I [040016]=036626 ; release buffer to free pool
037757 150401 IOF
; --- status / empty join ---
037760 050766 LDT ,B -12          ; T := accumulated status word 1  (clear on read)
037761 000766 STZ ,B -12
037762 044767 LDA ,B -11          ; A := accumulated status word 2
037763 000767 STZ ,B -11
037764 146151 RADD CLD SA DD      ; D := status word 2
037765 054770 LDX ,B -10          ; X := message count
037766 173777 AAX -1              ; X := count-1
037767 143007 SKP IF DX LST 0
037770 124003 JMP 037773
037771 146105 RADD CLD 0 DA       ;   count was 0: A := 0
037772 124002 JMP 037774
037773 146175 RADD CLD SX DA      ;   else A := count-1
037774 004770 STA ,B -10          ; count := max(count-1, 0)
037775 150402 ION
037776 044774 LDA ,B -4
037777 146154 RADD CLD SA DL      ; restore L
040000 044773 LDA ,B -5
040001 146153 RADD CLD SA DB      ; restore B
040002 133402 JXN 040004          ; X (count-1) != 0 ?
040003 146542 RADD AD1 CLD SL DP  ;   == 0: SUCCESS return L+1... (see [OPEN])
040004 044013 LDA [040017]=101410 ; A := 101410 "no message"
040005 146142 EXIT                ; ERROR return
; helper-error path:
040006 146134 RADD CLD SB DL
040007 054243 LDX 037652
040010 146173 RADD CLD SX DB
040011 150402 ION
040012 146142 EXIT
; pool: 040013=036765 040015=000077 040016=036626 040017=101410
```

**Receive record produced [V]:** record[0]=source station (desc>>8 & 77B), record[1]=buf[3]
(source OMD [I]), record[2]=broadcast bit (desc bit 14), record[3]=byte count, record+4..=payload.

**[OPEN] - tail status logic.** The final `JXN` makes the L+1/L+2 choice depend on
`count-1`: count==1 -> success; count==0 -> A=101410 error (correct for "empty"); but
count>=2 would ALSO take the 101410 exit even though a message was just delivered. Either
entry[-10] is not a plain queued-message count, or multi-pending is resolved by the ID12
re-activation loop in 5OMBREAD. The raw control flow above is exact; the semantic of
entry[-10]/[-11]/[-12] beyond "cleared when read" is unresolved. Tried: matched against
MBSEND (which does not touch -10/-11/-12) and the SOCTO receive dispatch (which increments
them from interrupt context - not annotated here).

### Pseudo-C

```c
/* OMBREAD: T=omd, X=ring, D=recPhys, A=bank. Returns 0101410 when nothing pending. */
int ombread(int omd, int ring, u16 recPhys, int bank) {
    Entry *e = validate(ring, omd);             /* helper 036765 */
    Buf *buf = e->chainHead;                    /* entry[-6] */
    if (buf) {
        e->chainHead = buf->link;
        mem[bank][recPhys+0] = (buf->desc >> 8) & 077;  /* source station  */
        mem[bank][recPhys+1] = buf->srcOmd;             /* buf[3]          */
        mem[bank][recPhys+2] = (buf->desc >> 14) & 1;   /* broadcast       */
        mem[bank][recPhys+3] = buf->count;              /* bytes, buf[6]   */
        movew(buf->data, &mem[bank][recPhys+4], (buf->lenWord + 1) / 2);
        release(buf);                            /* helper 036626 */
    }
    status  = e->stat1; e->stat1 = 0;            /* entry[-12], [-11] cleared */
    statusD = e->stat2; e->stat2 = 0;
    n = e->count - 1; e->count = max(n, 0);      /* entry[-10] */
    if (n != 0) return 0101410;                  /* "no message" (see [OPEN]) */
    return OK;
}
```

---

## 7. XKICK500 / LV12KICK @ 146526 / 146555 - kick the ND-500/5000 [V]

Locals (data cells): 146522 CKICKTYPE, 146523 CLVL, 146524 CPIE, 146525 LREG.
Entry: A = kick type (N100KICK=1 / CLRKICK=3 / IDLEKICK=6), B = ND-500 CPU datafield.
Callers must be in IOF unless already on level 12.

```
146526 004374 STA 146522          ; CKICKTYPE := A
146527 150001 TRA STS
146530 154573 SHA SHR 5           ; A := STS >> 5
146531 070075 AND 146626(=000170) ; A := current PIL << 3
146532 004371 STA 146523          ; CLVL := caller level bits
146533 150401 IOF
146534 044367 LDA 146523
146535 171140 SAT 140             ; T := LV12B (level 12 << 3)
146536 142065 SKP IF DA UEQ ST
146537 124016 JMP 146555          ;   already level 12 -> LV12KICK
146540 146135 RADD CLD SB DA
146541 153543 IRW 140 DB          ; level-12 B := caller's B (the CPU df)
146542 044065 LDA 146627(=033616) ; WT12
146543 153544 IRW 140 DL          ; level-12 L := WT12
146544 044064 LDA 146630(=146555) ; LV12KICK
146545 153542 IRW 140 DP          ; level-12 P := LV12KICK
146546 044063 LDA 146631(=010000) ; bit-12 mask
146547 150306 MST PID             ; trigger level 12
146550 150402 ION                 ; (level 12 runs LV12KICK now)
146551 150401 IOF
146552 044352 LDA 146524          ; CPIE
146553 150307 MST PIE
146554 146142 EXIT
; --- entry on level 12 ---
146555 150402 ION                 ; LV12KICK:
146556 146145 RADD CLD SL DA
146557 004346 STA 146525          ; LREG := L
146560 050417 LDT ,B 17           ; T := 5STATION   (CPU df offset 17B)
146561 171400 SAX 0               ; X := OCTORING (=0)
146562 044340 LDA 146522          ; A := CKICKTYPE
146563 135047 JPL I [146632]=037254 ; CALL SKICK
146564 124014 JMP 146600          ;   error -> ERR
146565 044336 LDA 146523          ; OUT: CLVL
146566 171140 SAT 140
146567 142065 SKP IF DA UEQ ST
146570 124007 JMP 146577          ;   caller was level 12 -> plain GO LREG
146571 150401 IOF
146572 044041 LDA [146633]=146606 ; LV14KICK
146573 153562 IRW 160 DP          ; level-14 P := LV14KICK
146574 044040 LDA [146634]=040000 ; bit-14 mask
146575 150306 MST PID             ; trigger level 14 (protected return)
146576 150402 ION
146577 125326 JMP I 146525        ; GO LREG
146600 146156 RADD CLD SA DT      ; ERR: T := SKICK error code
146601 135034 JPL I [146635]=012210 ; CALL 9ERR
146602 034471 (inline param, #99)  ; error id word [I encoding]
146603 124362 JMP 146565          ; GO OUT
; --- level-14 helper: mask all levels below the caller ---
146604 154400 (data: SHA 0)       ; CSHSHR template
146605 154200 (data: SHD 0)       ; CSH template
146606 150007 TRA PIE             ; LV14KICK:
146607 004315 STA 146524          ; CPIE := PIE
146610 044026 LDA [146636]=037777
146611 146151 RADD CLD SA DD      ; D := 37777B
146612 044311 LDA 146523          ; CLVL
146613 156575 SHA ZIN SHR 3       ; level number
146614 172401 AAA 1               ; +1
146615 050367 LDT 146604          ; T := SHA-template
146616 146656 RSUB SA DT          ; T := SHA -(lvl+1)
146617 140660 EXR ST              ; execute -> A := 37777B >> (lvl+1)
146620 050365 LDT 146605          ; T := SHD-template
146621 146056 RADD SA DT
146622 140660 EXR ST              ; execute -> build enable mask
146623 146115 RADD CLD SD DA
146624 150207 MCL PIE             ; clear PIE bits of all lower levels
146625 125012 JMP I [146637]=072172 ; GO YWAIT
; pool: 146626=000170 146627=033616(WT12) 146630=146555 146631=010000 146632=037254(SKICK)
;       146633=146606 146634=040000 146635=012210(9ERR) 146636=037777 146637=072172(YWAIT)
```

### Pseudo-C

```c
/* XKICK500: A=kickType (1/3/6), B=cpu df. */
void xkick500(int kickType, Df *cpuDf) {
    CKICKTYPE = kickType;
    CLVL = current_level_bits();
    IOF();
    if (CLVL != LV12B) {
        LEVEL12 = { B: cpuDf, L: WT12, P: LV12KICK };
        trigger_level(12);                   /* LV12KICK runs, then level 12 drops */
        ION(); IOF();
        restore_PIE(CPIE);                   /* set by LV14KICK on the way back */
        return;
    }
    lv12kick();
}
void lv12kick(void) {                        /* on level 12; B = cpu df */
    LREG = L;
    int rc = skick(CKICKTYPE, cpuDf->station /*B[17]*/, OCTORING /*0*/, KICK);
    if (rc != OK) err9(rc, 99);              /* 9ERR, then continue */
    if (CLVL != LV12B) {                     /* return path via level 14 */
        LEVEL14.P = LV14KICK; trigger_level(14);
        /* LV14KICK: CPIE := PIE; mask (MCL PIE) every level below caller;
           GO YWAIT - a pseudo-IOF so the caller resumes atomically */
    }
    goto *LREG;
}
```

---

## 8. XRS5CPU @ 146642 and RS5CPU @ 146700 [V]

```
146642 146145 RADD CLD SL DA      ; XRS5CPU: (B = CPU df)
146643 004376 STA 146641          ; LREG
146644 146135 RADD CLD SB DA
146645 004373 STA 146640          ; BREG
146646 044417 LDA ,B 17           ; A := 5STATION
146647 054026 LDX [146675]=011545 ; X := LMFIELD
146650 006000 STA ,X 0            ; MOCTSTATION := station
146651 170403 SAA 3               ; OMDACCP = 3
146652 006001 STA ,X 1            ; MOCTOMD := 3
146653 002002 STZ ,X 2            ; MBROADCAST := 0
146654 170401 SAA 1
146655 006003 STA ,X 3            ; MMSGLENGTH := 1
146656 170471 SAA 71              ; CMCPURES = 071B
146657 156410 SHA ZIN 10          ; << 8
146660 006004 STA ,X 4            ; MCOMMAND := 071B << 8
146661 044015 LDA [146676]=011537 ; LMDF
146662 146153 RADD CLD SA DB      ; B := LMDF
146663 050400 LDT ,B 0            ; T := 5OMDNO
146664 171400 SAX 0               ; X := ring 0
146665 044010 LDA [146675]=011545 ; "LMFIELD+DPITPHYS" (DPITPHYS=0)
146666 146151 RADD CLD SA DD      ; D := record phys
146667 170401 SAA 1               ; A := DPITBANK = 1
146670 135007 JPL I [146677]=037425 ; CALL MBSEND
146671 144400 RAND 0 0            ;   error: 0/\0 (ignore)
146672 044346 LDA 146640          ; restore B
146673 146153 RADD CLD SA DB
146674 125345 JMP I 146641        ; GO LREG
; pool: 146675=011545 146676=011537 146677=037425

146700 044040 LDA [146740]=052222 ; RS5CPU: B := "S5CPUDF"
146701 146153 RADD CLD SA DB
146702 050037 LDT [146741]=052404 ; T := "E5CPUDF"
146703 141436 SKP IF DT MGRE SB   ; loop while B <= E5CPUDF
146704 124033 JMP 146737          ;   done -> GO WT12
146705 044427 LDA ,B 27           ; CPUAVAILABLE
146706 175355 BSKP ONE 150 DA     ; bit 13 = 5ALIVE ?
146707 124026 JMP 146735          ;   no -> next df
146710 044422 LDA ,B 22           ; MAILINK
146711 171377 SAT -1
146712 142065 SKP IF DA UEQ ST    ; MAILINK != -1 ?
146713 124022 JMP 146735
146714 051026 LDT I [146742]=004654 ; T := 5MBBANK
146715 054422 LDX ,B 22           ; X := MAILINK
146716 173404 AAX 4               ; + X5CPU (=4)
146717 143300 LDATX               ; A := mailbox X5CPU word
146720 171000 SAT 0
146721 142065 SKP IF DA UEQ ST    ; != MPNACTIVE (0) ?
146722 124013 JMP 146735
146723 044427 LDA ,B 27
146724 070017 AND [146743]=000007 ; CPUAVAILABLE & 5CPUTYPE(7)
146725 171003 SAT 3               ; SAMSON = 3
146726 140065 SKP IF DA EQL ST
146727 124003 JMP 146732
146730 135014 JPL I [146744]=146642 ;   SAMSON: CALL XRS5CPU
146731 124004 JMP 146735
146732 135013 JPL I [146745]=145372 ;   else: CALL XTER500
146733 144400 RAND 0 0
146734 135012 JPL I [146746]=022554 ;   CALL X5MCST [I identity, V value]
146735 172046 AAB 46               ; B += 5CPUDFSZ
146736 124344 JMP 146702
146737 125010 JMP I [146747]=033616 ; GO WT12
```

---

## 9. 5OMBREAD @ 146756 - receive dispatch [V]

Activated on the driver level when a multibyte message arrives on the reserved OMD
(entry B = LMDF). Locals: CSTS = 147147, CMICP = 147150, LMREC = 147205, LMSIZE = 147206
(the last two double as the inline 9FLER parameters). MPFATAL byte array @ 146750..146755 =
words 000400,000001,000000,000000,000001,000400 = bytes `1,0, 0,1, 0,0, 0,0, 0,1, 1,0` [V].

```
146756 044173 LDA [147151]=011545 ; A := LMFIELD
146757 146151 RADD CLD SA DD      ; D := record phys
146760 170401 SAA 1               ; A := DPITBANK
146761 171400 SAX 0               ; X := ring 0
146762 050400 LDT ,B 0            ; T := 5OMDNO  (LMDF[0])
146763 135167 JPL I [147152]=037660 ; CALL OMBREAD
146764 124146 JMP 147132          ;   error -> ERR (octobus error / empty)
146765 054164 LDX [147151]=011545 ; X := LMFIELD
146766 046000 LDA ,X 0            ; source station
146767 171070 SAT 70              ; FN5DEST = 70B
146770 141065 SKP IF DA GRE ST
146771 124116 JMP 147107          ;   < 70 -> MF-controller test
146772 171077 SAT 77              ; LN5DEST = 77B  (L07! M06 had 73B)
146773 141056 SKP IF DT GRE SA
146774 124113 JMP 147107          ;   > 77 -> MF test
; --- source is a SAMSON (70..77B) ---
146775 046004 LDA ,X 4            ; ETYPE
146776 146151 RADD CLD SA DD
146777 156570 SHA ZIN SHR 10
147000 004147 STA 147147          ; CSTS := ETYPE >> 8
147001 146115 RADD CLD SD DA
147002 070151 AND [147153]=000377
147003 004145 STA 147150          ; CMICP := ETYPE & 377B
147004 046000 LDA ,X 0
147005 135147 JPL I [147154]=147252 ; CALL GN5CPUDF (A=station -> B=cpu df)
147006 125147 JMP I [147155]=147240 ;   not found -> GO I5OMBR
147007 044140 LDA 147147          ; CSTS
147010 171000 SAT 0               ; MFACK = 0
147011 142065 SKP IF DA UEQ ST
147012 124004 JMP 147016          ;   == MFACK -> alive
147013 050140 LDT [147153]=000377 ; MFNACK = 377B
147014 140065 SKP IF DA EQL ST
147015 124005 JMP 147022          ;   neither -> error message
147016 044427 LDA ,B 27           ; Ack/Nack on WriteSysPar:
147017 174355 BSET ONE 150 DA     ;   CPUAVAILABLE |= 5ALIVE (bit 13)
147020 004427 STA ,B 27           ;   "I'm present"
147021 125134 JMP I [147155]=147240 ; GO I5OMBR
; --- error message from SAMSON ---
147022 044125 LDA 147147          ; CSTS
147023 070133 AND [147156]=000017
147024 171017 SAT 17
147025 143065 SKP IF DA LST ST    ; (CSTS & 17B) < 17B  -> known error
147026 124012 JMP 147040
147027 146157 RADD CLD SA DX
147030 050127 LDT [147157]=146750 ; T := "MPFATAL"
147031 174000 BSET ZRO SSPTM      ; 1BANK
147032 142200 LBYT                ; A := MPFATAL[CSTS & 17B]
147033 174200 BSET ONE SSPTM      ; 2BANK
147034 131004 JAZ 147040
147035 044112 LDA 147147          ;   fatal: CSTS | N5SECCODE
147036 074122 ORA [147160]=002000
147037 135122 JPL I [147161]=146100 ;   CALL XRSTARTALL
147040 044764 LDA ,B -14          ; CPUNO  (cpu df offset -14B)
147041 054110 LDX [147151]=011545
147042 006000 STA ,X 0            ; LMFIELD[0] := CPUNO (station now = logical cpu)
147043 044105 LDA 147150          ; CMICP
147044 171001 SAT 1
147045 140065 SKP IF DA EQL ST    ; CMICP == 1 (microprogram) ?
147046 124027 JMP 147075
147047 044100 LDA 147147          ; CSTS
147050 050112 LDT [147162]=000200 ; hwfault = 200B
147051 142065 SKP IF DA UEQ ST
147052 124004 JMP 147056
147053 050110 LDT [147163]=000201 ; general trap = 201B
147054 140065 SKP IF DA EQL ST
147055 124020 JMP 147075
; --- mp hw-fault / trap record: patch shadow-process id ---
147056 054073 LDX [147151]=011545
147057 046005 LDA ,X 5            ; S5 (process number field)
147060 065104 SUB I [147164]=011254 ; - 5SWPROC
147061 120104 MPY [147165]=000010 ; * 5PRDSIZE (10B)
147062 060104 ADD [147166]=115542 ; + "S500S"
147063 146157 RADD CLD SA DX
147064 046001 LDA ,X 1            ; A := RTRES (shadow process id)
147065 054064 LDX [147151]=011545
147066 006004 STA ,X 4            ; LMFIELD.S4 := shadow id
147067 044100 LDA [147167]=011547 ; "LMFIELD+2"
147070 004115 STA 147205          ; LMREC := LMFIELD+2
147071 046003 LDA ,X 3            ; MMSGLENGTH
147072 172404 AAA 4
147073 004113 STA 147206          ; LMSIZE := len+4
147074 124007 JMP 147103
147075 044073 LDA [147170]=011550 ; else: LMREC := "LMFIELD+3"
147076 004107 STA 147205
147077 054052 LDX [147151]=011545
147100 046003 LDA ,X 3
147101 172402 AAA 2
147102 004104 STA 147206          ; LMSIZE := len+2
147103 044044 LDA 147147
147104 074054 ORA [147160]=002000
147105 004042 STA 147147          ; CSTS |= N5SECCODE (2000B)
147106 124040 JMP 147146          ; -> BYPASS
; --- MF-controller source (2..6) ---
147107 171002 SAT 2               ; FMFDEST = 2
147110 141065 SKP IF DA GRE ST
147111 124020 JMP 147131          ;   < 2 -> I5OMBR (unknown source ignored)
147112 171006 SAT 6               ; LMFDEST = 6
147113 141056 SKP IF DT GRE SA
147114 124015 JMP 147131          ;   > 6 -> I5OMBR
147115 046004 LDA ,X 4            ; ETYPE
147116 171000 SAT 0
147117 142065 SKP IF DA UEQ ST
147120 125035 JMP I [147155]      ;   == MFACK -> I5OMBR (ack for our message)
147121 004026 STA 147147          ; CSTS := etype
147122 044045 LDA [147167]=011547
147123 004062 STA 147205          ; LMREC := LMFIELD+2
147124 054025 LDX [147151]=011545
147125 046003 LDA ,X 3
147126 172402 AAA 2
147127 004057 STA 147206          ; LMSIZE := len+2
147130 124016 JMP 147146          ; -> BYPASS
147131 125024 JMP I [147155]      ; GO I5OMBR
; --- OMBREAD error return ---
147132 050037 LDT [147171]=101410 ; the "no message" status
147133 142065 SKP IF DA UEQ ST
147134 124011 JMP 147145          ;   == 101410 -> I5OMBR (benign)
147135 004012 STA 147147          ; CSTS := octobus error code
147136 054013 LDX [147151]=011545
147137 002000 STZ ,X 0            ; station := 0
147140 044027 LDA [147167]=011547
147141 004044 STA 147205          ; LMREC := LMFIELD+2
147142 170402 SAA 2
147143 004043 STA 147206          ; LMSIZE := 2
147144 124002 JMP 147146
147145 125010 JMP I [147155]      ; GO I5OMBR
147146 124024 JMP 147172          ; -> BYPASS
; (147147 CSTS, 147150 CMICP, 147151=011545, 147152=037660(OMBREAD), 147153=000377,
;  147154=147252(GN5CPUDF), 147155=147240(I5OMBR), 147156=000017, 147157=146750(MPFATAL),
;  147160=002000(N5SECCODE), 147161=146100(XRSTARTALL), 147162=000200, 147163=000201,
;  147164=011254(5SWPROC), 147165=000010(5PRDSIZE), 147166=115542(S500S),
;  147167=011547(LMFIELD+2), 147170=011550(LMFIELD+3), 147171=101410)
; --- BYPASS: complete the 9FLER record and report ---
147172 054052 LDX [147244]=011545 ; X := LMFIELD
147173 046000 LDA ,X 0            ; station (or CPUNO / 0)
147174 054011 LDX 147205          ; X := LMREC
147175 006003 STA ,X 3            ; LMREC[3] := source station  ("EOCTSOURCE" disp 3)
147176 044351 LDA 147147
147177 006002 STA ,X 2            ; LMREC[2] := CSTS            ("ESECCODE"  disp 2)
147200 044006 LDA 147206
147201 172401 AAA 1
147202 154577 SHA SHR 1
147203 004003 STA 147206          ; LMSIZE := (bytes+1)/2 words
147204 135041 JPL I [147245]=012325 ; CALL 9FLER
147205 (data) LMREC               ;   param 1: record address
147206 (data) LMSIZE              ;   param 2: record size (words)
; --- MF-controller requires an ack ---
147207 054035 LDX [147244]=011545
147210 046003 LDA ,X 3            ; "LMFIELD".EOCTSOURCE  (word 3!)  [see 12.2]
147211 171002 SAT 2
147212 141065 SKP IF DA GRE ST
147213 124025 JMP 147240          ;   < 2 -> I5OMBR
147214 171006 SAT 6
147215 141056 SKP IF DT GRE SA
147216 124022 JMP 147240          ;   > 6 -> I5OMBR
147217 054025 LDX [147244]=011545
147220 006000 STA ,X 0            ; MOCTSTATION := source MF station
147221 170404 SAA 4               ; MFOMDNO = 4
147222 006001 STA ,X 1
147223 002002 STZ ,X 2            ; not broadcast
147224 170401 SAA 1
147225 006003 STA ,X 3            ; MMSGLENGTH := 1
147226 170400 SAA 0               ; MFACK = 0
147227 154410 SHA 10              ; << 8
147230 006004 STA ,X 4            ; MSTS := MFACK << 8
147231 171400 SAX 0               ; ring 0
147232 050400 LDT ,B 0            ; T := 5OMDNO
147233 044011 LDA [147244]=011545
147234 146151 RADD CLD SA DD      ; D := record phys
147235 170401 SAA 1               ; bank 1
147236 135010 JPL I [147246]=037425 ; CALL MBSEND
147237 144400 RAND 0 0            ;   0/\0
147240 044007 LDA [147247]=011537 ; I5OMBR: B := LMDF
147241 146153 RADD CLD SA DB
147242 135006 JPL I [147250]=033613 ; CALL ID12   (wait for next activation)
147243 125006 JMP I [147251]=146756 ; GO 5OMBREAD (loop forever)
; pool: 147244=011545 147245=012325(9FLER) 147246=037425(MBSEND) 147247=011537(LMDF)
;       147250=033613(ID12) 147251=146756
; --- GN5CPUDF @147252: station -> cpu df ---
147252 146151 RADD CLD SA DD      ; D := station
147253 044014 LDA [147267]=052222 ; B := "S5CPUDF"
147254 146153 RADD CLD SA DB
147255 050013 LDT [147270]=052404 ; T := "E5CPUDF"
147256 141436 SKP IF DT MGRE SB
147257 124007 JMP 147266          ;   exhausted -> EXIT (error)
147260 044417 LDA ,B 17           ; 5STATION
147261 140015 SKP IF DA EQL SD
147262 124002 JMP 147264
147263 146542 RADD AD1 CLD SL DP  ;   match -> return L+1 with B = cpu df
147264 172046 AAB 46              ; B += 5CPUDFSZ
147265 124370 JMP 147255
147266 146142 EXIT                ; not found (L+0)
```

### Pseudo-C

```c
void fiveOmbread(void) {           /* driver process; B = LMDF on entry */
  for (;;) {
    int rc = ombread(LMDF->omdno, 0, LMFIELD_PHYS, 1);
    if (rc == ERROR) {
        if (A == 0101410) goto wait;               /* nothing pending */
        CSTS = A; LMFIELD[0] = 0;                  /* octobus error, station 0 */
        LMREC = LMFIELD+2; LMSIZE = 2; goto bypass;
    }
    int src = LMFIELD[0];
    if (src >= 070 && src <= 077) {                /* SAMSON (L07 range!) */
        CSTS  = LMFIELD[4] >> 8; CMICP = LMFIELD[4] & 0377;
        Df *cpu = gn5cpudf(src); if (!cpu) goto wait;
        if (CSTS == MFACK /*0*/ || CSTS == MFNACK /*0377*/) {
            cpu->available |= 5ALIVE /*bit 13*/;   /* WriteSysPar ack = present */
            goto wait;
        }
        if ((CSTS & 017) < 017 && MPFATAL[CSTS & 017])
            xrstartall(CSTS | N5SECCODE /*02000*/);
        LMFIELD[0] = cpu->cpuno;                   /* df[-14] */
        if (CMICP == 1 && (CSTS == 0200 || CSTS == 0201)) {
            shadow = S500S[(LMFIELD[5] - 5SWPROC) * 010].rtres;
            LMFIELD[4] = shadow;                   /* patch S4 */
            LMREC = LMFIELD+2; LMSIZE = LMFIELD[3] + 4;
        } else { LMREC = LMFIELD+3; LMSIZE = LMFIELD[3] + 2; }
        CSTS |= N5SECCODE;
    } else if (src >= 2 && src <= 6) {             /* MF-controller */
        if ((LMFIELD[4]) == MFACK) goto wait;      /* ack for our msg */
        CSTS = LMFIELD[4];                         /* etype as SEC code */
        LMREC = LMFIELD+2; LMSIZE = LMFIELD[3] + 2;
    } else goto wait;
bypass:
    LMREC[3] = LMFIELD[0];                         /* source station   */
    LMREC[2] = CSTS;                               /* SEC code         */
    LMSIZE = (LMSIZE + 1) / 2;                     /* bytes -> words   */
    fler9(LMREC, LMSIZE);                          /* layer manager    */
    if (LMFIELD[3] >= 2 && LMFIELD[3] <= 6) {      /* MF wants an ack  */
        LMFIELD[0] = LMFIELD[3]; LMFIELD[1] = MFOMDNO /*4*/;
        LMFIELD[2] = 0; LMFIELD[3] = 1; LMFIELD[4] = MFACK << 8;
        mbsend(LMDF->omdno, 0, LMFIELD_PHYS, 1);
    }
wait:
    B = LMDF; id12();                              /* sleep until next OMD event */
  }
}
```

---

## 10. CON5OMD @ 147271, MFPREPARE @ 147300, CON5IDENT @ 147334 [V]

```
147271 171400 SAX 0               ; CON5OMD: X := OCTORING (B = working field = LMDF)
147272 135004 JPL I [147276]=040062 ; CALL CONOMD  (allocate an OMD entry)
147273 125004 JMP I [147277]=033616 ;   error -> GO WT12
147274 004400 STA ,B 0            ; 5OMDNO := allocated OMD number
147275 125002 JMP I [147277]=033616 ; GO WT12

147300 054026 LDX [147326]=011545 ; MFPREPARE: (A = dest MF station, B = working field)
147301 006000 STA ,X 0            ; MOCTSTATION := dest station (2..6)
147302 170404 SAA 4               ; MFOMDNO = 4
147303 006001 STA ,X 1            ; MOCTOMD := 4
147304 002002 STZ ,X 2            ; MBROADCAST := 0
147305 170403 SAA 3
147306 006003 STA ,X 3            ; MMSGLENGTH := 3 bytes
147307 170416 SAA 16              ; CMSYSPAR = 016B
147310 156410 SHA ZIN 10          ; << 8
147311 074016 ORA [147327]=000001 ; | N100IDENT (1)
147312 006004 STA ,X 4            ; MCOMMAND := 0x0E01
147313 044400 LDA ,B 0            ; A := 5OMDNO
147314 154410 SHA 10              ; << 8
147315 006005 STA ,X 5            ; MDP1 := 5OMDNO << 8
147316 171400 SAX 0               ; ring 0
147317 050400 LDT ,B 0            ; T := 5OMDNO
147320 044006 LDA [147326]=011545
147321 146151 RADD CLD SA DD      ; D := record phys
147322 170401 SAA 1               ; bank 1
147323 135005 JPL I [147330]=037425 ; CALL MBSEND
147324 125005 JMP I [147331]=033616 ;   error -> GO WT12
147325 125005 JMP I [147332]=147240 ; GO I5OMBR (ack handled by 5OMBREAD)
; pool: 147326=011545 147327=000001 147330=037425 147331=033616 147332=147240

147334 014377 STX 147333          ; CON5IDENT: CWFIELD := X (working df); B = cpu df
147335 050417 LDT ,B 17           ; T := 5STATION
147336 171400 SAX 0               ; X := ring 0
147337 170401 SAA 1               ; N100IDENT = 1
147340 146151 RADD CLD SA DD      ; D := 1
147341 170540 SAA 140             ; LV12B
147342 156575 SHA ZIN SHR 3       ; >> 3 -> level number 12
147343 135035 JPL I [147400]=040467 ; CALL ECONID (bind ident 1 -> level 12)
147344 125035 JMP I [147401]=033616 ;   error -> GO WT12
147345 044417 LDA ,B 17           ; 5STATION
147346 054034 LDX [147402]=011545
147347 006000 STA ,X 0            ; MOCTSTATION
147350 170403 SAA 3
147351 006001 STA ,X 1            ; MOCTOMD := OMDACCP (3)
147352 002002 STZ ,X 2            ; not broadcast
147353 170407 SAA 7
147354 006003 STA ,X 3            ; MMSGLENGTH := 7
147355 170416 SAA 16
147356 156410 SHA ZIN 10
147357 074024 ORA [147403]=000001
147360 006004 STA ,X 4            ; MCOMMAND := CMSYSPAR<<8 | N100IDENT
147361 044352 LDA 147333          ; CWFIELD
147362 146153 RADD CLD SA DB      ; B := working field (= LMDF)
147363 044400 LDA ,B 0            ; 5OMDNO
147364 156410 SHA ZIN 10
147365 006005 STA ,X 5            ; S5 := 5OMDNO << 8
147366 002006 STZ ,X 6            ; S6 := 0
147367 002007 STZ ,X 7            ; S7 := 0
147370 050400 LDT ,B 0            ; T := 5OMDNO
147371 171400 SAX 0
147372 044010 LDA [147402]=011545
147373 146151 RADD CLD SA DD
147374 170401 SAA 1
147375 135007 JPL I [147404]=037425 ; CALL MBSEND
147376 125003 JMP I [147401]=033616 ;   error -> GO WT12
147377 125006 JMP I [147405]=147240 ; GO I5OMBR
; pool: 147400=040467(ECONID) 147401=033616 147402=011545 147403=000001
;       147404=037425 147405=147240
```

**MFPREPARE on-wire body (to station 2..6, OMD 4, 3 bytes):** `0E 01 <5OMDNO>` -
CMSYSPAR, N100IDENT, reply-OMD. **CON5IDENT body (to SAMSON, OMD 3, 7 bytes):**
`0E 01 <5OMDNO> 00 00 00 00`. Both [V] to the byte-build level; matches the live-decoded
CMSYSPAR trace in `CARVE-ANSWER-OCTOBUS-CPU-PRESENCE-2026-07-18.md`.

---

## 11. 5MTRANS @ 143445 and 5MRDTRANS @ 144740 - abstrans transfer engine

### 11.1 5MTRANS entry-to-handoff (fully annotated) [V]

Entry: X = current 5MPM message, B = ND-500 CPU datafield (driver level).

```
143445 015142 STX I [143607]=011160 ; CMSGA := X    (message addr)
143446 001142 STZ I [143610]=011165 ; CUREL := 0
143447 146135 RADD CLD SB DA
143450 005141 STA I [143611]=011157 ; XC5CPUDF := B
143451 051141 LDT I [143612]=004654 ; T := 5MBBANK
143452 173500 AAX 100               ; + 5MNWA
143453 143302 LDDTX                 ; AD := nowait/function double
143454 173700 AAX -100
143455 021136 STD I [143613]=011162 ; NWFUNC (NWAIT @011162, 5MFNC @011163)
143456 045136 LDA I [143614]=011163 ; A := 5MFNC
143457 175205 BSKP ONE 0 DA         ; bit 0 = 5DTRANS ?
143460 125135 JMP I [143615]=144021 ;   no -> GO FAR CHEVENT
143461 173510 AAX 110               ; + 5MLGN
143462 143300 LDATX                 ; A := logical device number
143463 135133 JPL I [143616]=010376 ; CALL LOGPH  (logical -> datafield)
143464 146157 RADD CLD SA DX        ; X := device datafield
143465 050132 LDT [143617]=031441   ; "9BBHD"
143466 141467 SKP IF DX MGRE ST     ; X >= 9BBHD ?
143467 124024 JMP 143513            ;   no -> error 6
143470 050130 LDT [143620]=042312   ; "9EEHD"
143471 143467 SKP IF DX MLST ST     ; X < 9EEHD ?
143472 124021 JMP 143513            ;   no -> error 6
143473 050126 LDT [143621]=041062   ; "9EDFD"
143474 141467 SKP IF DX MGRE ST
143475 124003 JMP 143500
143476 170401 SAA 1                 ;   X >= 9EDFD: disk type 1 (Domino/SCSI-500)
143477 124002 JMP 143501
143500 146105 RADD CLD 0 DA         ;   else type 0 (SMD / SCSI-100)
143501 005121 STA I [143622]=011171 ; 5DSKC := type
143502 050121 LDT [143623]=031441   ; "9FSTR"
143503 141467 SKP IF DX MGRE ST
143504 124006 JMP 143512
143505 050117 LDT [143624]=033315   ; "9ESTR"
143506 143467 SKP IF DX MLST ST
143507 124003 JMP 143512
143510 046060 LDA ,X 60             ;   streamer range: STREN flag (offset 60B)
143511 131002 JAZ 143513            ;   not enabled -> error 6
143512 124003 JMP 143515            ; DIST1
143513 170406 SAA 6                 ; error 6: no disk optimization
143514 125111 JMP I [143625]=144561 ; GO FAR XRXX
143515 015111 STX I [143626]=011161 ; DIST1: DDFADDR := X
143516 054111 LDX [143627]=033315   ; X := "QP100" (disk queue pool df)
143517 046013 LDA ,X 13             ; 5MQCU (free element count)
143520 131427 JAF 143547            ;   nonzero -> take an element
; --- no free element: park the process on 5MWQU ---
143521 055066 LDX I [143607]=011160 ; X := CMSGA
143522 135106 JPL I [143630]=023706 ; CALL SLOCK
143523 144400 RAND 0 0
143524 135105 JPL I [143631]=145372 ; CALL XTER500
143525 144400 RAND 0 0
143526 135104 JPL I [143632]=022704 ; CALL IFM500XQ
143527 135104 JPL I [143633]=024041 ; CALL SUNLOCK
143530 051062 LDT I [143612]=004654 ; T := 5MBBANK
143531 173547 AAX 147               ; + PLINK
143532 143300 LDATX
143533 173631 AAX -147
143534 005100 STA I [143634]=011260 ; N5MESSAGE := PLINK word
143535 025100 LDD I [143635]=011167 ; AD := 5MWQU
143536 051054 LDT I [143612]=004654
143537 143306 STDTX                 ; message.LINK := old queue head
143540 146165 RADD CLD ST DA        ; (CNVWADR: word-addr conversion)
143541 146171 RADD CLD SX DD
143542 154606 SAD 6
143543 065073 SUB I [143636]=037777? ; [I] CNVWADR internals not glossed further
143544 156773 SAD ZIN SHR 5
143545 021070 STD I [143635]=011167 ; 5MWQU := new head (this message)
143546 125071 JMP I [143637]=135067 ; GO NXTMSG   (back to 5STDRIV main loop)
; --- take a disk access queue element ---
143547 146173 RADD CLD SX DB        ; B := QP100
143550 173405 AAX 5                 ; + QPFRH (free head at offset 5)
143551 135067 JPL I [143640]=026656 ; CALL GETOUT  (unlink first free element -> T)
143552 044413 LDA ,B 13
143553 172777 AAA -1
143554 004413 STA ,B 13             ; 5MQCU--
143555 011033 STT I [143610]=011165 ; CUREL := element
143556 146163 RADD CLD ST DB        ; B := element
143557 055030 LDX I [143607]=011160 ; X := CMSGA
143560 051032 LDT I [143612]=004654 ; T := 5MBBANK
143561 173511 AAX 111               ; + 5MDIS
143562 143300 LDATX                 ; A := disk function word
143563 005056 STA I [143641]=011164 ; DISID := A
143564 070056 AND [143642]=000077?  ; A & 77B
143565 131403 JAF 143570
143566 171060 SAT 60                ;   0 -> T := 60 (read, clear cache)
143567 124054 JMP 143643
143570 171001 SAT 1
143571 142065 SKP IF DA UEQ ST
143572 124004 JMP 143576
143573 171007 SAT 7
143574 140065 SKP IF DA EQL ST
143575 124003 JMP 143600
143576 171061 SAT 61                ;   1 or 7 -> T := 61 (write)
143577 124044 JMP 143643
143600 171006 SAT 6
143601 140065 SKP IF DA EQL ST
143602 124003 JMP 143605
143603 171066 SAT 66                ;   6 -> T := 66 (read, keep cache)
143604 124037 JMP 143643
143605 170407 SAA 7                 ;   else error 7 (illegal function)
143606 125017 JMP I [143625]=144561 ; GO XRXX
; --- fill the queue element ---
143643 045127 LDA I [143772]=011164 ; A := DISID
143644 070127 AND [143773]=000300   ; & 300B
143645 146065 RADD ST DA            ; + function code (60/61/66)
143646 004414 STA ,B 14             ; ABFUN
143647 051125 LDT I [143774]=004654 ; T := 5MBBANK
143650 173775 AAX -3                ; 5MDIS -> 5MEMA (106B)
143651 143302 LDDTX
143652 020415 STD ,B 15             ; MEMAD (double)
143653 173404 AAX 4                 ; -> 5DSEC (112B)
143654 143302 LDDTX
143655 020417 STD ,B 17             ; ABPA2 (start sector, double)
143656 173403 AAX 3                 ; -> 5MNOS (115B)
143657 143302 LDDTX
143660 004421 STA ,B 21             ; ABP31 (sector count)
143661 173770 AAX -10               ; -> 5MREQ (105B)
143662 143300 LDATX
143663 004425 STA ,B 25             ; REQID
143664 045111 LDA I [143775]=011162 ; NWAIT
143665 004427 STA ,B 27             ; 5MNOWAIT := NWAIT
143666 045110 LDA I [143776]=011163 ; 5MFNC
143667 070110 AND [143777]=000006   ; & 6
143670 131003 JAZ 143673
143671 170401 SAA 1
143672 004427 STA ,B 27             ;   func 2..6 -> 5MNOWAIT := 1
143673 045105 LDA I [144000]=011160 ; CMSGA
143674 004426 STA ,B 26             ; ADMESS := message
143675 045104 LDA I [144001]=011171 ; 5DSKC
143676 131427 JAF 143725            ;   nonzero -> Domino path
; --- SMD/SCSI-100: geometry via HTABL ---
143677 045073 LDA I [143772]=011164 ; DISID
143700 156572 SHA ZIN SHR 6         ; unit := DISID >> 6
143701 070101 AND [144002]=000007   ; & 7
143702 060101 ADD [144003]=000030   ; + "HTABL"
143703 055101 LDX I [144004]=011161 ; X := DDFADDR
143704 146057 RADD SA DX
143705 056000 LDX ,X 0              ; X := X.S0  (unit geometry entry)
143706 046010 LDA ,X 10             ; S10
143707 171020 SAT 20
143710 140065 SKP IF DA EQL ST      ; == 20B -> Phoenix disk
143711 124007 JMP 143720
143712 044417 LDA ,B 17             ;   ABP21 (start sector hi)
143713 070067 AND [144002]=000007
143714 156411 SHA ZIN 11            ;   << 9
143715 060414 ADD ,B 14
143716 004414 STA ,B 14             ;   fold into ABFUN
143717 000417 STZ ,B 17             ;   ABP21 := 0
143720 135065 JPL I [144005]=144626 ; CALL FAR CHDISKADDR
143721 046000 LDA ,X 0              ; SECWO (words per sector)
143722 120421 MPY ,B 21             ; * ABP31
143723 146151 RADD CLD SA DD        ; D := word count
143724 124005 JMP 143731
143725 135060 JPL I [144005]=144626 ; Domino: CALL FAR CHDISKADDR
143726 044060 LDA [144006]=002000   ; 2000B words/sector
143727 120421 MPY ,B 21
143730 146151 RADD CLD SA DD        ; D := 2000B * ABP31
; --- fixed-area check + queue insertion + controller start ---
143731 051043 LDT I [143774]=004654 ; T := 5MBBANK
143732 055046 LDX I [144000]=011160 ; X := CMSGA
143733 143331 LDXTX                 ; X := message.X5SND
143734 146116 RADD CLD SD DT        ; T := word count
143735 024415 LDD ,B 15             ; AD := MEMAD
143736 135051 JPL I [144007]=073024 ; CALL CHFIX  (is the area fixed?)
143737 125051 JMP I [144010]=144560 ;   no -> GO FAR FERR (error 10)
143740 146175 RADD CLD SX DA
143741 065050 SUB I [144011]=011254 ; - 5SWPROC
143742 120050 MPY [144012]=000010   ; * 5PRDSIZE
143743 060050 ADD [144013]=115542   ; + "S500S"
143744 146157 RADD CLD SA DX
143745 046001 LDA ,X 1              ; shadow RTRES
143746 004401 STA ,B 1              ; element.RTRES
143747 144037 SWAP SB DX            ; X := element, B := ?
143750 045034 LDA I [144004]=011161 ; A := DDFADDR
143751 146153 RADD CLD SA DB        ; B := disk controller datafield
143752 135412 JPL I ,B 12           ; CALL M5TRANS  (controller df slot 12B!)
143753 124003 JMP 143756            ;   busy/queued -> BUSR
143754 144037 SWAP SB DX            ;   started+finished: X <-> B
143755 125037 JMP I [144014]=144740 ;   GO 5MRDTRANS
143756 001037 STZ I [144015]=011165 ; BUSR: CUREL := 0
143757 045017 LDA I [143776]=011163 ; 5MFNC
143760 070017 AND [143777]=000006
143761 131440 JAF 144021            ;   func has check-event bits -> CHEVENT
143762 045013 LDA I [143775]=011162 ; NWAIT
143763 131003 JAZ 143766
143764 170401 SAA 1                 ;   nowait: status 1 "request received"
143765 125031 JMP I [144016]=144572 ;   GO FAR FIN
143766 055012 LDX I [144000]=011160 ; X := CMSGA
143767 170422 SAA 22                ; A := 5MWAIT (22B)
143770 135027 JPL I [144017]=023670 ; CALL WN5STATUS  (mark process waiting)
143771 125027 JMP I [144020]=?      ; GO FAR OUT (144601)
; literal pool 143772-144020 as annotated inline
```

L07 symbol `M5TRA=000012` confirms: **M5TRANS is not an absolute routine here - it is the
per-controller entry stored at disk-controller-datafield offset 12B**, invoked
`JPL I ,B 12`. [V]

### 11.2 5MTRANS remaining blocks (block-level, [NPL-V] with byte anchors)

- **CHEVENT @ 144021**: search READYQ (`X:=X.NLINK`, NLINK=5) for a finished element whose
  ADMESS matches; honor `5MREQ = -1` = any request; on miss test/clear 55REP in 5MSFL under
  SLOCK; status 4 (restarted by other), 2 (no event, nowait), or 5MWAIT+WN5STATUS.
- **FOUND @ ~144130**: unlink (GETOUT/PTFREE), write back SSSTAT/REQID to 5MHIO/5MHRE,
  clear 55REP, clear-cache mask 7400B if ABFUN==60, then `5EMONICO` (disk error, code 11) or
  `5MONICO` (status 3 = completed / 13B = completed+restarted), `XACTRDY`, and restart the
  first 5MWQU waiter (SLOCK + GCPUDF/ERRFATAL + XTER500 + ITO500XQ + ITOFIFOQ + SUNLOCK).
- **SPROCESS @ ~144301** (function 7/8 start process) and **GETMAGNO @ ~144474** (function
  with 5GMAGNO: search open-file table CNSTART/5CNSIZE for the file connect number, return
  magic number, status 1, error 16 = not opened, 12 = illegal function).
- **Error tails [V]**: FERR=144560 (A:=10), XRXX=144561 (X:=CMSGA; D:=0; CALL 5EMONICO
  @030325; CALL XACTRDY @145466; PTFREE if CUREL; GO OUT), FIN=144572 (D:=0,T:=0; CALL
  5MONICO @030332; CALL XACTRDY), OUT=144601 (B := XC5CPUDF; GO NXTMSG @135067).
  Error codes seen: 5 (illegal process), 6 (no disk opti), 7 (illegal r/w function),
  10 (area not fixed), 12 (illegal function code), 15 (illegal magic no), 16 (file not
  opened), 17 (not write access), 20/21/22 (transfer outside file / wrong unit / wrong disk).

### 11.3 5MRDTRANS @ 144740 (annotated head) [V]

Driver-level completion routine; entry B = finished disk access queue element.

```
144740 054167 LDX [145127]=?        ; X := "N500DF"
144741 046356 LDA ,X -22            ; SYSINITFLAG (N500DF offset -22B)
144742 175125 BSKP ZRO 120 DA       ; bit 10 (B5STOP) clear?
144743 125165 JMP I [145130]=033616 ;   stopped -> GO WT12
144744 044401 LDA ,B 1              ; RTRES
144745 131432 JAF 144777            ;   nonzero -> process still there
; --- process terminated while transfer ran ---
144746 146136 RADD CLD SB DT
144747 000425 STZ ,B 25             ; REQID := 0
144750 135161 JPL I [145131]=026701 ; CALL PTFREE (element back to pool)
144751 025161 LDD I [145132]=011167 ; AD := 5MWQU
144752 142001 SKP IF DD UEQ 0
144753 124023 JMP 144776            ;   empty -> GO WT12
144754-144762                       ; CNVBYADR + LINK@3 LDDTX: unlink first waiter,
                                    ; 5MWQU := next   (T = 5MBBANK via [145134]=004654)
144763 135152 JPL I [145135]=023706 ; CALL SLOCK
144765 135151 JPL I [145136]=023624 ; CALL GCPUDF
144766 135151 JPL I [145137]=?      ; CALL ERRFATAL
144767 146153 RADD CLD SA DB        ; B := cpu df
144770 135150 JPL I [145140]=145372 ; CALL XTER500
144772 135147 JPL I [145141]=022570 ; CALL ITO500XQ
144773 135147 JPL I [145142]=030455 ; CALL ITOFIFOQ
144774 135147 JPL I [145143]=024041 ; CALL SUNLOCK
144775 125147 JMP I [145144]=?      ; GO N500 (driver restart)
144776 125132 JMP I [145130]=033616 ; GO WT12
; --- process exists: is it waiting for this transfer? ---
144777 044427 LDA ,B 27             ; 5MNOWAIT
145000 131030 JAZ 145030            ;   0 -> process is waiting: restart it (5RMSTART)
145001 054426 LDX ,B 26             ; X := ADMESS
145002 135143 JPL I [145145]=023662 ; CALL RN5STATUS
145003 172756 AAA -22               ; status == 5MWAIT (22B) ?
145004 131411 JAF 145015            ;   no -> park element on READYQ
145005 173505 AAX 105               ; + 5MREQ
145006 143300 LDATX
145007 171377 SAT -1
145010 142065 SKP IF DA UEQ ST      ; waiting for any request?
145011 124017 JMP 145030            ;   yes -> 5RMSTART
145012 050425 LDT ,B 25             ; REQID
145013 142065 SKP IF DA UEQ ST      ; waiting for THIS request?
145014 124014 JMP 145030            ;   yes -> 5RMSTART
145015 054131 LDX [145146]=?        ; X := "READYQ-NLINK"
145016 146176 RADD CLD SX DT        ; walk to tail:
145017 056005 LDX ,X 5              ;   X := X.NLINK  (NLINK = 5)
145020 133002 JXZ 145022
145021 124375 JMP 145016
145022 146165 RADD CLD ST DA
145023 060124 ADD [145147]=000005   ; A := tail + NLINK
145024 146157 RADD CLD SA DX
145025 146136 RADD CLD SB DT
145026 135122 JPL I [145150]=026670 ; CALL PUTIN  (append element to READYQ)
145027 125101 JMP I [145130]=033616 ; GO WT12
145030 ...                          ; 5RMSTART: PTFREE + write-back + MONICO status 3/13
                                    ; + XACTRDY (same machinery as 5MTRANS FOUND)
```

---

## 12. Divergences and corrections

1. **LN5DEST = 77B in L07** (`SAT 77` at 146772; L07 symbol `LN5DE=000077`). The message
   catalog quotes M06's 73B. Not a contradiction - a generation difference (L07 already
   allows the full hardware station range 70-77B). Both symbol tables agree with their own
   binaries. [V]
2. **ESECCODE/EOCTSOURCE displacements**: the L07 bytes store the SEC code at **LMREC+2**
   and the source station at **LMREC+3**, and the MF-ack test reads `LMFIELD+3` as
   "EOCTSOURCE". The catalog (`SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` section 1) states
   "EOCTSOURCE = offset 1, ESECCODE = offset 0 of the error record" from M06 symbols. The
   two statements are reconcilable only if the M06 offsets are relative to a record base 2
   words above LMREC; **the L07 store displacements are 2 and 3** - use those for the
   emulator. FLAGGED as a correction candidate for the catalog. [V bytes / OPEN reconciliation]
3. **The catalog's MBSEND row ("IOXT sequence at 037320+") is wrong about WHICH routine**:
   037320 is inside **SKICK** (its direct-transmit path). **MBSEND contains no IOXT at
   all** - it queues the buffer and fires level 13 with P := SOCTW (036342), which does the
   IOX work. The row was marked [I]; now resolved. [V]
4. **MFPREPARE NPL comment says "Message length = 2 bytes"; both the NPL code and the L07
   bytes set MMSGLENGTH = 3.** The comment is wrong, the code is consistent. [V]
5. **MBSEND hard length limit: 1..255 bytes** (`len-1 > 376B` -> error 101427). Any emulated
   ACCP/MF record longer than 255 bytes can never be sent by SINTRAN. [V]
6. NPL revision offset: every routine in this block sits at NPL-listing address + 200B
   (consistent with the known "+0o200 ND-500 block" note in `TASK-05-mismatches.md`). [V]

## 13. Open questions

- **[OPEN]** OMBREAD tail: exact semantics of entry[-10]/[-11]/[-12] and the `JXN`-based
  success/empty decision (section 6). The raw flow is exact; the counter meaning is not.
- **[OPEN]** Helper 036765: the per-level jump table at 037021-037040 (dispatch on caller
  PIL) is only partially understood - levels map to three targets (reject 037063, check
  037041, direct 037050). Roles of `mem[X+47]` vs `mem[X+77]` vs `mem[X+124]` vs
  `mem[X+125]` interface-table cells (output vs input datafield selectors [I]).
- **[OPEN]** MBSEND error status 101426 (cell 037645): load site 037621 not reached from any
  path traced here (probably the no-free-buffer code via 037617; the exact code word loaded
  at 037617 is `LDA 037644` whose cell contents were not pinned).
- **[OPEN]** The inline parameter word 034471 after the 9ERR call in XKICK500 (NPL says
  `#99`); 9ERR's parameter encoding not analyzed.
- **[OPEN]** 5MRDTRANS pool cells 145127 ("N500DF"), 145137 (ERRFATAL), 145144 (N500),
  145146 ("READYQ-NLINK") - values not individually dumped (roles from NPL + surrounding
  verified cells).
- **[I]** Receive-side buffer word 3 = source OMD (write-side never sets buf[3]; SOCTO's
  receive path fills it - not annotated in this pass).

## 14. Cross-references

- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\OCTOBUS-ND100-ND5000-REFERENCE.md` (frame model,
  section 6.4 carve summary - this doc supersedes its routine-body level of detail)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\SINTRAN-OCTOBUS-MESSAGE-CATALOG.md` (message
  catalog; see divergence notes 1-3 above)
- `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\ND500-HANDLERS-OVERLAY.md`
  (the 14xxxx overlay proof for the level-12 handler family)
- `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-OCTOBUS-CPU-PRESENCE-2026-07-18.md`
  (the 5ALIVE handshake this doc's section 9 byte-proves)
