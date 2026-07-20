# DOMDF initializer - L07 byte carve (task S0-2, BLOCKING)

**Date:** 2026-07-20
**Question answered:** who initializes the DOMINO datafield `DOMDF=041064` -
the writers of `DSVER=041104`, `DOMDF+21`, the "static header" words
`DSVER+32..67`, and the port cells `DOMDF.DLPRT` / `PDF.DRPRT`.
BDIO carve (`BDIO-DOMINO-DRIVER-CARVE.md` section 11) left all of these [OPEN].

**HEADLINE ANSWER:**
The initializer is the **FILE SYSTEM's DOMINO pool/port module in segment
`006-S3FS` (= `012-S3SFS`, byte-identical), VA `133203B-137000B`**, lazily
run at first pool access:

- **`QUINI` @ `134206B` writes `DOMDF.DLPRT`, `DSVER := 1`, and
  `DOMDF+21 := 30B`** (once, guarded by init flag `DOMDF+15`), creating the
  local NUCLEUS port with `MON 347` and one NUCLEUS message descriptor per
  disk queue element (stored into each element's `DMSID` word +13).
- **`GPOOL` @ `133343B` / `RGPOO` @ `133701B` / `RCPOO` @ `134516B` write
  `PDF.DRPRT`** (plus `PDF.DIPOO/OPAIX/ARESZ`), the remote port coming from a
  NUCLEUS **name-server lookup by pool NAME** (`DOPPR` = MON 347 fn 3).
- **The "static header" `DSVER+32..67` is NOT a config header at all**: it is
  the generated tail of the DOMDF record (zeros) + the `ADOML` lock + the
  start of the `NKMBU` message buffer, swept along because the BDIO message
  window (70B/76B words) is larger than the 32B-word record content. The
  SCSI unit/LUN binding is **NOT carried there** - it is carried by
  `PDF.DRPRT` (which remote port = which pool) + `DXPOO`/`OPAIN` (pool/area
  index doubles inside every message).

Evidence tags: `[V]` byte-verified here (dd reproduction below), `[NPL-V]`
NPL/listing logic (different revision), `[I]` inferred, `[OPEN]` unresolved.

Companion annotated listing: `a-domdf-init-006-s3fs.txt` (region
133340B-137000B of 006-S3FS).

---

## 1. Overlay resolution [V]

Candidate produced by a raw word-scan of ALL carved segments + resident
images for operand words `041064/041103/041104/041136/041146`:

- Real code references to `041064 DOMDF` exist in exactly TWO places:
  the known BDIO driver literals in `017-S3SMPIT`/`026-S3IMPIT` (already
  [V] in the BDIO carve) and a compact cluster in **`006-S3FS`**
  (= `012-S3SFS`, `cmp` whole-file identical [V]) at VA 133570, 134023,
  134271, 134343(region), 134506(region), 134612.
- **Every other hit in every other segment is a false positive**, checked
  byte-by-byte: `041103/041104` = ASCII "BC"/"BD" inside error-message
  strings (WBDIO-style texts in 116-S3SERWD, 124-S3SBOPC, 130-CFT,
  136-FSASG, 140, 141, 022-S3RFAC, 135-XFTRAD, 007-S3DMAC, commoncode) or
  unrelated table words. The `041136 DSVER+32` hit at MPIT 056233 decodes as
  the instruction `MIN I 136` inside a terminal-driver loop - not a literal.
  The `041104` at FS 135131 is ASCII "BD" inside WBDIO's message text.  [V]

Sibling coherence for 006-S3FS (base `026000B`): **35 FILSYS-SYMBOLS (L07)
symbols land on parallel `021xxx STD I` PROC entries** in this one region:
`GNNAD=133203, GNNAM=133205, GPOOL=133343, CHRON=133637, RGPOO=133701,
GDINA=134030, RGDIN=134122, QUINI=134206, CLTOT=134303, DCPOO=134346,
RCPOO=134516, INICO=134620, CLCOM=134657, ECLCO=134670, WNKER=134721,
WBDIO=135010, WERRE=135075, FPOOL=135155, CPOOL=135307, AREST=135414,
CLALL=135557, DPOOL=135636, DSHIN=135711(leaf, LDX entry), WSNRC=135733,
GVACD=136056, CRPRT=136123, OPPRT=136162, CRMSG=136252, DCRPR=136312,
DOPPR=136352, DCRMS=136443, CLPRT=136506, DCLPR=136544, REMSG=136603,
WRMSG=136613, SNMSG=136650, RCMSG=136705, CRDIR=136741`. Zero mismatches.
Literal pools resolve to `DOMDF 041064`, `NKMBU 041152`, `QUINI 134206`,
`DOPPR 136352`, `LOGPH 010376`, device numbers `2260/2277`, `BFQUE 033332`,
`EFQUE 036350` - all L07 symbol values.  [V]

## 2. On-disk state of DOMDF (what is generated vs runtime) [V]

DOMDF is DATA: it lives in the **data-PIT overlay `044-S3IDPIT` =
`053-S3SDPIT` (base 004000B)**, the same overlay the NUCLEUS carve proved
for cells 125142ff. Carved on-disk content (byte offsets = (addr-04000)*2):

| VA | on disk | meaning |
|---|---|---|
| 041064 DOMDF+0..1 | 0,0 | RESLINK,RTRES |
| 041066 +2 | **041064** | BWLINK = `*-2` self pointer (generated) |
| 041067 +3 | **2** | TYPRING = 2 (generated) |
| 041070..71 +4..5 | 0,0 | ISTATE,MLINK |
| 041072 +6 | **074246** | MFUNC/NFUNC = **REBDIO** pre-planted at generation |
| 041073..041145 | all 0 | incl. DLPRT(+17)=0, DSVER(+20)=0, +21=0, +32..67 region=0 |
| 041146 ADOML+0..1 | 0,0 | allocation/message-buffer lock |
| 041150 ADOML+2 | **041146** | `*-2` self pointer |
| 041151 ADOML+3 | **2** | |
| 041152 NKMBU | 0... | 160B-word DOMINO message buffer |

This exactly reproduces the generation listing (`s3vs-4.symb` 4276-4289,
different generation, addresses shifted):
`DOMDF, 0;0;*-2;2;0;0;REBDI;0` + 42 zero words + `ADOML, 0;0;*-2;2` +
`NKMBU=*; *+160/`.  [V carve + NPL-V layout]

**Consequence: every cell this task asked about is ZERO on disk. There is a
runtime writer.**

Also from the same listing [NPL-V]: pool datafields `DOM01..DOM20` are
generated with `RSFLA=0, PLDNO=2257+n, DRPRT=0, DIPOO=0,0`, DIBRE=-1;
device numbers **2257 = DOMDF/ADOML ("DOMINO ALLOCATION LOCK"), 2260-2277 =
BDIO pools 1-16** (logical-device table entries `D2200+137/141..177`).

## 3. QUINI @ 134206B - THE DOMDF initializer [V]

Annotated disassembly (all words dd-reproduced, section 8):

```
134206  021061  QUINI: (PROC entry, STD I 61)
134207-134211   A:=L; D:=B; frame 7 words (SAB 7 + JPL frame-alloc [134270]=003752)
134213  054056  X := DOMDF                 ([134271] = 041064)
134214  046015  A := DOMDF+15              (init-done flag)
134215  131437  IF A<>0 GO 134254          (already initialized -> OK return)
134216  146176  T := X (=DOMDF)            (owner context for the D* wrappers)
134217  170401  A := 1
134220  144151  D := 1, A := 0
134221  135051  CALL DCRPR                 ([134272] = 136312; MON 347 fn 1 T=3)
134222  124035  (error -> 134257)
134223  004406  [B+6] := A                 (created local port number)
134224  006017  DOMDF.DLPRT (041103) := A  *** THE DLPRT WRITER ***
134225  170401  A := 1
134226  006020  DSVER (041104) := 1        *** THE DSVER WRITER: version word = 1 ***
134227  170430  A := 30B
134230  006021  DOMDF+21 (041105) := 30B   ***
134231  044042  A := 033332 (BFQUE)        (constant, gate always true here)
134232  131017  JAZ 134251
134233  054041  X := 033341                ([134274]; = BFQUE+7, first queue element)
134234  050041  T := 036350                ([134275] = EFQUE)
134235  143467  WHILE X < EFQUE:
134237  146171    D := X                   (queue-element address)
134240  050031    T := DOMDF               ([134271])
134241  170500    A := 100B                (message size arg)
134242  054406    X := [B+6]               (local port)
134243  135033    CALL DCRMS               ([134276] = 136443; MON 347 fn 6 T=3)
134244  124017    (error -> 134263)
134245  146117    X := D                   (element address again)
134246  006013    elem+13 := A             *** queue-DF DMSID := created message no ***
134247  173437    X += 37B                 (queue-element stride)
134250  124364  OD
134251  054020  X := DOMDF                 ([134271])
134252  170401  A := 1
134253  006015  DOMDF+15 := 1              (init done)
134254  040404  MIN [B+4]  (skip-return = OK)
```

Pseudo-C:

```c
/* QUINI @134206 (006-S3FS): lazy one-shot DOMDF init.               */
/* Called from GPOOL (first pool connect) and siblings.              */
int quini(void)
{
    if (mem[DOMDF + 015]) return OK;              /* already done      */
    int port = dcrpr(/*A*/0, /*D*/1, /*T*/DOMDF); /* MON 347 fn 1:
                                     create local port, DOMDF owner   */
    if (port < 0) return ERR;
    mem[DOMDF + 017] = port;                      /* DLPRT  = 041103  */
    mem[DOMDF + 020] = 1;                         /* DSVER  = 041104  */
    mem[DOMDF + 021] = 030;                       /* 041105 := 30B    */
    /* one NUCLEUS message descriptor per disk queue element          */
    for (word e = 033341 /*BFQUE+7*/; e < 036350 /*EFQUE*/; e += 037)
        mem[e + 013] = dcrms(/*size*/0100, /*port*/port, /*T*/DOMDF);
        /* elem+13 = DMSID - exactly what STRBDIO passes to NKWRI [V] */
    mem[DOMDF + 015] = 1;                         /* init-done flag   */
    return OK;
}
```

So the values the BDIO driver later relies on are:
**`DSVER = 1`** (the version word content asked for in the BDIO carve),
**`DOMDF+21 = 30B`**, **`DLPRT` = a NUCLEUS-assigned port number** (runtime
value - not knowable from disk, must be produced by the port-create
answer), and **each queue element's `DMSID` = a NUCLEUS-assigned message
descriptor number**.  [V]

## 4. Who writes PDF.DRPRT (and DIPOO/OPAIX/ARESZ) [V]

Three FILSYS functions write the pool datafield connection cells; all three
get the remote port from `DOPPR` (open port **by name**) with `T = DOMDF`:

- **`GPOOL` @ 133343** (first connect, pool selected by NAME):
  allocates a vacant pool DF via `GVACD` (scans device numbers
  **2260B..2277B** through `LOGPH 010376`, claims the first with
  `RSFLA=0` by setting `RSFLA:=1` [V]), copies the caller's 17B-word pool
  name into its frame, calls `QUINI` [V ptr 133567] and `INICO` [V ptr
  133556], then:
  `PDF.DIPOO(+17) := D` @133410, `PDF.ARESZ(+23) := D` @133442,
  `PDF.OPAIX(+21) := D` @133447, and
  `PDF.DRPRT(+16) := DOPPR(T=DOMDF, name)` @133460-133464.  [V]
- **`RGPOO` @ 133701** (= NPL `RGPOOL`, reconnect after transfer error;
  reached from the RPIT reconnect RT program via
  `XBDTU: T:=PLDNO; CALL FILSYS(RGPOOL)` [NPL-V listing 075423]):
  `PDF.DIPOO := D` @133730, `PDF.ARESZ := D` @133752,
  `PDF.OPAIX := D` @133757,
  `PDF.DRPRT := DOPPR(T=DOMDF([134023]), ...)` @133764-133770.  [V]
- **`RCPOO` @ 134516** (= NPL `RCPOOL`, switch to mirror pool; `XBDTV`
  path): `PDF.DRPRT := A` @134553-134554, `PDF.DIPOO := D` @134556, with
  DOMDF literal [134612] + DOPPR [134613].  [V]

The retry plumbing around them (RPIT overlay `016-S3SRPIT` =
`025-S3IRPIT`, base 032000B) is byte-confirmed and matches the listing:
`RECST @075225` = RT program draining two 20B-entry arrays `X1ARR/X2ARR`
(at 075411../075420-ish frame cells) calling `XBDTU`/`XBDTV`;
**`BDTMU @075326` / `BDTMV @075356`** (entries `174000 BSET ZRO SSPTM` [V])
only INSERT the pool DF into X1ARR/X2ARR and start RTREC - the earlier
MPIT decode of these two addresses was the wrong overlay (the MPIT bytes
there are XMSG code).  [V]

The exact provenance of the `D` doubles stored into DIPOO/ARESZ/OPAIX
(returned via INICO/CRDIR/name-server exchanges before the stores) was not
fully decoded: **[OPEN]** - they are answers from the NUCLEUS/DIOC side,
not constants (nothing on disk, no config file read in this module).

## 5. The MON 347 wrapper family (how ports/messages are made) [V]

Every wrapper builds a small request block on the FILSYS stack frame and
issues `MON 347` (NUCLEUS SERVE, worker `SERVE=047072`, MCTAB-verified in
NUCLEUS-PRIMITIVES-CARVE.md section 6). Byte-verified shape:

| Routine | Addr | MON 347 regs | request fn word | means |
|---|---|---|---|---|
| CRPRT | 136123 | X=7, T=1, D=11B, A=&req | fn:=1 | create port (own owner); result req+16 -> A |
| OPPRT | 136162 | X=7, T=1, D=27B | fn:=3 | open port by NAME (37B-word name copied into req) |
| CRMSG | 136252 | X=7, T=1, D=13B | fn:=6 | create message (buffer size in req) |
| DCRPR | 136312 | X=7, **T=3**, D=11B, req+0:=DOMDF, req+1:=14B | fn:=1 | create port on behalf of the DOMDF owner |
| DOPPR | 136352 | X=7, **T=3**, D=27B, req+0:=DOMDF | fn:=3 | open port by name, DOMDF owner -> A = remote port |
| DCRMS | 136443 | X=7, **T=3** | fn:=6 | create message, DOMDF owner -> A = message number |
| CLPRT | 136506 | X=7, T=1 | fn:=7 | close port |
| DCLPR | 136544 | X=7, T=3 | fn:=7 | close port, DOMDF owner |
| SNMSG | 136650 | **X=1**, T=1 | - | send (server-side nkSend family) |
| RCMSG | 136705 | **X=2**, T=1 | - | receive |
| REMSG/WRMSG | 136603/136613 | **X=3**, T=1 | mode 0/1 | message data read/write |

(T=1 vs T=3 = owner-spec selector; X = server function family. Names are
L07 FILSYS symbols; per-field meanings of the request blocks beyond the fn
word are [I] pending the segment-105 server carve, task S0-3.)
`INICO @134620` combines CRPRT + OPPRT (+CRMSG/CLPRT on its error paths)
with T=1 - the "own connection" variant used by GPOOL.  [V]
`DSHIN @135711` initializes the `NKMBU (041152)` header: +0:=0, +1:=30B,
+2..7 := (0,[CURPROG 004010],...), +10/+11 := -1, +16/+17 := 0.  [V]

## 6. DSVER+32..67 (write) / +73 (read) - resolved [V layout]

The BDIO message window is **larger than the record content**:

- NKWRI sends 70B words from `DSVER` (= DOMDF+20): covers DOMDF+20..107.
- NKREA reads up to 76B words back to the same address: DOMDF+20..115.
- The DECLARED record content ends at `DNRPG+1 = DOMDF+51` (msg rel 31);
  the generated record itself ends at DOMDF+61 (all zeros after DNRPG).

So the "uninitialized static header" decomposes as:

| msg rel (from DSVER) | VA | what it really is |
|---|---|---|
| +32..41 | 041136-041147 | generated zero tail of DOMDF + ADOML+0..1 |
| +42..45 | 041150-041153 | `ADOML+2..3` (`041146,2`) + first NKMBU words |
| +46..67 | 041154-041173 | NKMBU message-buffer area (runtime content = leftovers of FILSYS name-server/connect messages) |
| +70..75 (read only) | 041174-041201 | NKMBU area, overwritten by the answer read-back |

**None of these words are BDIO request fields.** They ride along because
70B/76B are round transfer sizes. REBDIO's completion logic reads only
DSSTS/DSQCN/DXPOO [V, BDIO carve]. Therefore the DIOC (and the RetroCore
emulation) must treat msg rel +32.. as DON'T CARE on requests, and is free
to (but need not) write beyond rel 31 in answers.  [V layout + I on DIOC
behavior - confirm at S4-2 live diff]

**The SCSI unit/LUN binding is NOT in DSVER+32..67.** The binding chain is:
pool NAME -> `DOPPR` name-server lookup -> `PDF.DRPRT` (remote port =
DIOC-side pool port) and per-request `DXPOO` (=PDF.DIPOO) + `OPAIN`
(=PDF.OPAIX) doubles copied into the message by MBUILD [V, BDIO carve].
The unit/LUN mapping lives on the DIOC side of those ports/indices.  [V/I]

## 7. What the RetroCore emulator must provide

| Cell / value | Writer (real system) | Emulator duty |
|---|---|---|
| DOMDF+2..3, +6 (=REBDIO), ADOML+2..3 | generation (on disk) | comes free with the image [V] |
| DOMDF+15 init flag | QUINI | leave to guest (runs on first pool access) |
| DOMDF.DLPRT (041103) | QUINI <- MON 347 fn1 answer | NUCLEUS/DIOC side must ANSWER create-port with a valid port number |
| DSVER (041104) = 1, DOMDF+21 = 30B | QUINI | guest writes; DIOC should accept version 1 |
| queue-elem DMSID (+13, elems 033341..036350 step 37B) | QUINI <- MON 347 fn6 answers | answer create-message with distinct descriptor numbers |
| PDF.DRPRT | GPOOL/RGPOO/RCPOO <- DOPPR (name-server fn3) | DIOC must REGISTER one nucleus port per pool NAME and answer the open-by-name lookup |
| PDF.DIPOO/OPAIX/ARESZ | GPOOL/RGPOO <- connect exchange | DIOC must supply pool index / area index / area size in the connect answer ([OPEN]: exact message layout - carve segment 105 or S4-2 live capture) |
| BDIO request rel +32..67 | nobody (junk ride-along) | DIOC decode MUST ignore them |
| BDIO answer | DIOC | write DSSTS (0,0 ok / status), DSQCN (-1,-1 mirror-switch marker), sizes <= 76B words |

## 8. dd reproductions (all published words)

`006-S3FS.bin` byte offset = (addr_oct - 026000B)*2; `044-S3IDPIT.bin` =
(addr - 004000B)*2; `016-S3SRPIT.bin`/`017-S3SMPIT.bin` = (addr - 032000B)*2.

```
cd .../L-VSX-500/segments
dd if=006-S3FS.bin bs=1 skip=71976 count=2 | od -An -tx1   # 0c 0f  134224 STA ,X 17  DLPRT writer
dd if=006-S3FS.bin bs=1 skip=71980 count=2 | od -An -tx1   # 0c 10  134226 STA ,X 20  DSVER := 1
dd if=006-S3FS.bin bs=1 skip=71984 count=2 | od -An -tx1   # 0c 11  134230 STA ,X 21  +21 := 30B
dd if=006-S3FS.bin bs=1 skip=72050 count=2 | od -An -tx1   # 42 34  [134271] literal DOMDF 041064
dd if=006-S3FS.bin bs=1 skip=72012 count=2 | od -An -tx1   # 0c 0b  134246 STA ,X 13  DMSID writer
dd if=006-S3FS.bin bs=1 skip=71664 count=2 | od -An -tx1   # 0c 0e  133770 STA ,X 16  RGPOO DRPRT writer
dd if=006-S3FS.bin bs=1 skip=71272 count=2 | od -An -tx1   # 0c 0e  133464 STA ,X 16  GPOOL DRPRT writer
dd if=006-S3FS.bin bs=1 skip=73930 count=2 | od -An -tx1   # d6 e7  136145 MON 347 in CRPRT
dd if=006-S3FS.bin bs=1 skip=74278 count=2 | od -An -tx1   # d6 e7  136423 MON 347 in DOPPR
dd if=006-S3FS.bin bs=1 skip=73884 count=4 | od -An -tx1   # 04 b0 04 bf  GVACD 2260/2277 pool devnos
dd if=044-S3IDPIT.bin bs=1 skip=29804 count=2 | od -An -tx1 # 42 34  041066 DOMDF BWLINK self-ptr
dd if=044-S3IDPIT.bin bs=1 skip=29812 count=2 | od -An -tx1 # 78 a6  041072 MFUNC = REBDIO 074246
dd if=044-S3IDPIT.bin bs=1 skip=29832 count=2 | od -An -tx1 # 00 00  041104 DSVER zero on disk
dd if=016-S3SRPIT.bin bs=1 skip=36268 count=2 | od -An -tx1 # f8 00  075326 BDTMU entry (RPIT!)
```

Full 90-word verification run (all OK) is reproducible with the checker
embedded in the analysis session; every table row above cites its address.

## 9. Poisoned priors corrected

1. **BDTMU/BDTMV are NOT in the MPIT overlay.** `075326/075356` decode as
   coherent entries only in `016-S3SRPIT`/`025-S3IRPIT` (RPIT); the MPIT
   bytes at those VAs are XMSG-related code. The BDIO carve's routine list
   (SYMBOL-2 `BDTMU=075326 BDTMV=075356`) is correct, but any MPIT-based
   disassembly of their bodies would be the wrong overlay (trap 4).  [V]
2. **"DSVER+32..67 = static config header written at init" is DISPROVEN**;
   see section 6. Nothing initializes them because they are not fields.
3. The `QP100` "100-word queue elements" phrasing in the BDIO carve:
   QUINI walks the element list `033341..036350 (EFQUE)` with stride
   **37B words** [V]; `BFQUE=033332`. Element size 37B words, not 100.

## 10. Open items

- **[OPEN]** Exact layout of the MON 347 request blocks / name string
  format for fn 3 (open-by-name), and of the pool-connect answer that
  yields DIPOO/OPAIX/ARESZ: needs the segment-105 (S3INKSE) server carve
  (S0-3) or an S4-2 live capture. This is now THE remaining gap for real
  request routing.
- **[OPEN]** Where the pool NAME string given to GPOOL comes from
  (DDS-DEVICES:CNFG / directory entry / DP-SERVICE): caller-side of
  FILSYS(GPOOL), not carved here.
- **[OPEN minor]** QUINI's `100B` DCRMS size argument unit (bytes vs
  words); NKWRI later moves 70B words per message.
- **[I]** GPOOL is reached from ENTER-DIRECTORY/mount of a DOMINO device
  (5DSKC=1); the FILSYS dispatch indices for GPOOL/RGPOOL/RCPOOL were not
  extracted.
