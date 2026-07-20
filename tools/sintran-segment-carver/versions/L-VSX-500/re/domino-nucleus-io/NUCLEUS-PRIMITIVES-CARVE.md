# NUCLEUS kernel primitives - L07 byte carve

Byte-verified carve of the SINTRAN III L07 DOMINO/NUCLEUS message-passing kernel
(ND-100 side): the NK* primitives, the NUCLEUS kernel shared-memory structures
(master block, descriptor records, kick table, message buffer), the
NKSEND -> octobus-kick path, and the MON 347B dispatch.

- Date: 2026-07-19. Sources of ground truth: carved bytes only. NPL = logic reference
  (different revision). Manual = field NAMES only (ND-820026 section 7.4).
- Evidence tags: [V] = bytes reproduced in this analysis, [NPL-V] = matches NPL source
  logic, [MANUAL] = manual claim mapped onto proven bytes, [I] = inference, [OPEN] = not
  resolved (with what was tried).
- Companion files (same folder): `a-data-nkini.txt`, `a-nksen-nkins.txt`,
  `a-nkget-nkick.txt`, `a-nkloc-nmovw.txt`, `a-snucl-enkic.txt`, `a-enucl-ncall.txt` -
  full annotated disassembly (nd100-dis output + computed effective addresses + resolved
  indirect pointers + SYMBOL-1-LIST labels) of the whole NUCLEUS region of 026-S3IMPIT.
- Sibling deliverables (do not duplicate): `OCTOBUS-DRIVER-ROUTINES-CARVE.md` (SKICK,
  MBSEND, OMBREAD...), `BDIO-DOMINO-DRIVER-CARVE.md`.

---

## 1. Overlay resolution - where the NUCLEUS kernel lives [V]

The primitives are in the **MPIT resident overlay**: segment pair
`017-S3SMPIT.bin` = `026-S3IMPIT.bin` (byte-identical, `cmp` verified), load base
**032000B** (0x3400, nd100-dis `-b 13312`), from
`tools/sintran-segment-carver/versions/L-VSX-500/segments/`.

Proof (per the carving-skill discriminators):

1. **Call-target density scan** over ALL carved segments + both resident images for the
   16 NK* symbol addresses (+ SKICK/SNUCL/CONKI/LNKSE/NRETM/NURET/NCHBU/NMOVW):
   017/026-S3xMPIT score **21 distinct targets / 64 pointer-word hits**; next best
   (134-SNA3270) 10/19; commoncode 5/14; 104/105-NKSE 0. [V]
2. **Sibling coherence:** every JPL-called primitive lands on a uniform prologue in
   026-S3IMPIT (section 3); the same addresses in 003-S3CP are zero-fill
   (043375-045346) and in 104/105-S3xNKSE are mid-instruction garbage. [V]
3. Internal pointer cells resolve exactly: `[042605]=045134` NKLOC, `[043067]=044354`
   NKICK, `[044614]=037254` SKICK, `[047426]=043672` NKGET, `[047464]=042171` NKSEN,
   `[047534]=043076` NKREC, `[050524]=045643` GWAIT. [V]

**Trap note:** the first attempt tested only 003-S3CP vs 104/105 (the "obvious"
NUCLEUS-server segments) and got a split verdict. The density scan settled it. The NKSE
segments hold the NUCLEUS *server program* (section 10), NOT the kernel primitives.

## 2. Symbol pins (all from L07 `SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT` unless noted)

Code (all resolve in MPIT at coherent entries [V]):

| Symbol | Addr | Role (this doc's finding) |
|---|---|---|
| NKINI | 041703 | nkInitialise - bind kernel to master block, register kick |
| NKSEN | 042171 | nkSend (+ kick-only mode) |
| NKREC | 043076 | nkReceive |
| NKREA | 043375 | nkfRead (stub -> common body 043440, mode 0) |
| NKWRI | 043411 | nkfWrite (stub -> common body, mode 1) |
| NKINS | 043425 | insert/append write (stub -> common body, mode 2) |
| NKGET | 043672 | nkGetInfo (fn 0..4) |
| NKTRA | 044204 | trace-record writer |
| NKICK | 044354 | kick-queue insert + octobus kick decision (internal) |
| DKICK | 044747 | drain own kick queue (receive side, public prologue) |
| SETEV | 044620 | local process activation dispatch (16-way) |
| ACONV | 045101 | descriptor number -> descriptor ID (+ validity check) |
| NKLOC | 045134 | physical TSET lock (opcode 140516 "USER1") with retry |
| NKUNL | 045214 | unlock (write 0 to lock word) |
| NKCHE | 045237 | check message descriptor (type/owner) -> buffer phys |
| NKRDE | 045346 | RT-description helper (RT table base 115542) |
| NMOVW | 045702 | move words helper |
| SNUCL | 047017 | NUCLEUS function-block dispatch fn 0..7 (driver side) |
| SERVE | 047072 | **MON 347B worker** (ND-100 processes) |
| NURET | 047315 | ND-500 answer writer / return to level-12 driver |
| N5FU0/1/2 | 047364/047431/047467 | ND-500 bridge: NKGET / NKSEN / NKREC |
| N5FU5/6 | 050073/050106 | ND-500 bridge, incl SKICK emission at 050100 |
| ENUCL | 050123 | level-12 entry for ND-500 MON 347 (fn dispatch 0..7) |
| 5SERV | 050211 | forward ND-500 request to NUCLEUS server process |
| 1NURE / 5NURE | 050253 / 050303 | server return paths (ND-100 / ND-500) |
| NCALL | 050407 | ND-100 -> NUCLEUS-server mailbox call (NUCST buffer) |
| CTNMA / RNMSG | 045404 / 045432 | server support: contact name server / read mailbox msg |
| GNMSG / GWAIT | 045543 / 045643 | server support (mailbox get / wait) |
| CONKI | 040765 | register/connect kick entry (called by NKINI with A=14B,T=1) |
| LNKSE | 035056 | link-to-NKSE-segment helper (octobus module edge) |

Data / cells:

| Symbol | Value | Meaning |
|---|---|---|
| CLUST | 041574 | cluster base for ND-500 owner IDs (NKINI: A<<8 stored) [V] |
| NSAVA | 041575 | A-register save cell used by every prologue [V] |
| BBASE | 041576 | **kernel data field base**; all `,B nn` in the module [V] |
| 5NFUN..5NMBU | 047541-047555 | ND-500 NUCLEUS parameter cells (zero on disk, runtime) [V] |
| NKBRE | 047540 | BSS cell (zero on disk) [V] |
| NKUSE | 144162 | (not carved here; resident data region) [OPEN] |
| 5NKSE / 5NKNA | 000105 / 000107 | segment numbers of the two server segments [V vs meta] |
| MTNKD/MTNKP | 011013/011022 | MTAD NUCLEUS datafield / own port (SYMBOL-2-LIST) |
| ENKIC | 047526 | N500-SYMBOLS; **does NOT resolve in MPIT** (mid-N5FU2) [OPEN, section 9] |

NUCLEUS error codes (word constants embedded as error-exit operands) [V]:

```
101002 ILLTY  illegal descriptor type      101023 ERLOC  lock failure
101003 NOMSG  no message                   101024 NNOTE
101004 ILLNO  illegal descriptor number    101025 NNOTI  server not initialised
101006 ERDIS                               101032 PORTC  port closed / kick-proc=0
101014 NOACC  owner/access check failed    101033 NILLF  illegal function
101021 NREJE  rejected                     101034 NPROT
101035 ILLHA  illegal handle               101036 NFATA  fatal
101042 ERKLO  kick-table lock failure      101427 (NCALL mailbox-busy family) [I]
```

## 3. The common machinery [V]

### 3.1 Primitive prologue / epilogue

Every JPL-called primitive (NKINI, NKSEN, NKREC, NKREA, NKWRI, NKINS, NKGET, NKCHE,
DKICK) opens with the identical sequence (NKSEN shown, 042171):

```
174000  BSET ZRO SSPTM      ; clear PTM (leave alternate-page-table mode)
005135  STA I ->NSAVA       ; save caller A in NSAVA=041575
044135  LDA  ->BBASE        ; A := 041576 (address constant BBASE)
144053  SWAP SA DB          ; B := BBASE  (module data field)
004402  STA ,B 2            ; [B+2] := caller B
146145  RADD CLD SL DA
004403  STA ,B 3            ; [B+3] := caller L
045127  LDA I ->NSAVA       ; A := caller A again
```

and returns through the shared epilogue (NKSEN's at 043040):

```
005027  STA I ->NSAVA       ; stash return code
044403  LDA ,B 3 ; ->L      ; restore caller L
044402  LDA ,B 2 ; ->B      ; restore caller B
045022  LDA I ->NSAVA       ; A := return code (0 = OK, else 101xxx error)
174200  BSET ONE SSPTM      ; re-enter caller's PTM state
146142  EXIT
```

So: primitives run with PTM cleared, callers' A returns the status, B/L are preserved,
D/X/T carry per-call results.

### 3.2 Physical-memory access idiom (the whole kernel is in PHYSICAL memory)

All NUCLEUS structures live in a physically-allocated area outside any page table
(allocated by NUCST at boot, section 8). Two idioms:

**ID -> physical address** (descriptor IDs are 32-bit, byte-granular, flag in bit 15
of the high word):

```
174175  BSET ZRO 170 DA     ; clear flag bit in A (high half of ID)
156777  SAD ZIN SHR 1       ; AD >>= 1   (byte offset -> word offset)
060404  ADD ,B 4            ; A += [B+4] (bank number of the NUCLEUS area)
                            ; now AD = physical (bank,word) address
```

**MOVEW block copies** (L = word count set via `SAA n / RADD CLD SA DL`):

| Opcode | Observed direction | Register use |
|---|---|---|
| 143106 | physical -> local (module cell) | AD = phys src, T = local dest |
| 143102 | local -> physical | D = local src, TX = phys dest |
| 143110 | physical -> physical | AD = phys src, TX = phys dest |
| 143105/143107 | caller-space (PTM set) <-> physical | used by NKWRI/NKREA data copy, NCALL, RNMSG |
| 143103 | (NKINI one-off, L=5) | [I] |

Direction/registers are [V] from consistent usage; the exact MOVEW sub-field encoding is
not re-derived here.

### 3.3 The BBASE data field (cells at 041576+n, offsets octal) [V]

| Cell | Set by | Content |
|---|---|---|
| B+2 / B+3 | prologue | saved caller B / L |
| B+4 | NKINI | physical **bank** of the NUCLEUS area (= master page >> 6) |
| B+5..6 | callers / N5FUx | current OWNER id (double); ND-500 side = CLUST+process |
| B+7 | NKINI | phys addr of **own kick-table entry** (= kicktab + ownstation*14B) |
| B+11..12 | NKINI | phys addr (bank,word) of the **master block** |
| B+13..14 | NKINI | descriptor-table base **ID** (master+2..3) - symbols BEGD1/BEGD2 |
| B+17 | NKINI | phys addr of master+104 (trace lock) - symbol TRLOC |
| B+21..22 | NKINI | trace bounds cache (master+40..41) |
| B+24 | NKINI | **own octobus station number** (from cell 042025) |
| B+25 | NKINI | number of descriptors (master+7) - symbol DESCM |
| B+26..27 | NKSEN | caller A(reply port#), D(sendref#) |
| B+31 | NKSEN | caller X (message#) |
| B+32..33 | scratch | MOVEW landing cells (symbols MBUF1/MBUF2 collide here) |
| B+40..42, 44/46, 50/52, 54..56, 60..66, 70..73 | per-call | IDs and phys addrs of message / homeport / destination port / buffer / kick entries |
| B+100..104 | NKUNL | lock addr / zero cell / saved L |

## 4. Kernel structure layouts (offsets octal, unit = 16-bit words)

### 4.1 Master block [V unless noted]

| Offset | Field | Evidence |
|---|---|---|
| +2..3 | descriptor table base ID (double) | NKINI 042037-042047 -> B+13,14; ACONV adds it |
| +7 | number of descriptors | NKINI 042050-042060 -> B+25; ACONV/NKCHE bound-check |
| +10 | hash array pointer [MANUAL name, I] | symbol HASHA=10; not touched by carved primitives (server-side) |
| +12 | hash mask [MANUAL name, I] | symbol HMID=12 |
| +20..21 | **kick table base ID** (double) | NKINI 041775-042005 reads it; own entry = base + ownstation*14B (042006 `SAA 14 / MPY ,B 24`) |
| +25 | version/config word | NKINI 041756-041771 compares vs cell 042025-derived value; mismatch -> A=-1 fatal exit |
| +26 | net address table pointer [MANUAL name, I] | symbol NTADD=26 |
| +34..35 | trace buffer base ID (double) | NKTRA 044250-044264 (symbol TRADR=34) |
| +36 | trace min [I] | symbol TRMIN=36 (not read by carved code path) |
| +40..41 | trace max / bound | NKINI 042066-042076 -> B+21,22; NKTRA wrap check (TRMAX=40) |
| +42 | trace current index | NKTRA 044211-044247 read/increment/write-back (TRCUR=42) |
| +74, +76 | crash/health flag doubles | NKLOC busy loop 045161-045200 ANDs them; nonzero -> abort lock (NDISP=74) [I on meaning] |
| +102 | general lock [I] | symbol GENLO=102 |
| +104 | trace lock word | NKINI B+17; NKTRA locks it (TRACL=104) |

Manual figure 23 lists: descriptor-table ptr, hash array ptr + mask, kick table ptr,
net-addr table ptr, buffer start/end. Buffer start/end offsets not exercised by the
primitives (buffer allocation is server-side) - [OPEN], look in segment 105.

### 4.2 Descriptor records - 40B words (100B bytes) each [V]

`ACONV` (045101): `ID = number<<6 (byte units) + master[+2..3]`; bound `number < master[+7]`.
So the descriptor table is an array of 64-byte = 32-word records addressed by number.

Common head (all types):

| Offset | Field | Evidence |
|---|---|---|
| +0 | LOCK (TSET word, value 070000B written by NKLOC) | NKREC/NKSEN lock port at +0 |
| +1 | TYPE (2=message, 3=port, 4=sendref) | compares at 042232 (2), 042465 (3), 042356+ (4), 043153 (3) |
| +2..3 | OWNER (double) | compared vs [B+5..6] everywhere (NOACC) |

Message record (TYPE=2), manual figure 24 mapping:

| Offset | Field | Evidence |
|---|---|---|
| +4..5 | FREELINK [MANUAL order, I] | not touched in carved paths |
| +6..7 | USER [MANUAL order, I] | |
| +10..11 | LINK (message queue link) | NKSEN append 042733 (cleared), NKREC pop 043220-043232 |
| +12..13 | BUFFERPOINTER (ID of buffer) | 042255-042272, NKCHE 045312, NKINS 043511 |
| +14..15 | HOMEPORT (ID) | NKSEN "send home" path 042275-042312 |
| +16..21 | (4 words) sender port identity stamped on send | NKSEN 042510-042522 copies homeport+30..33 here? see 4.3 note [I on exact grouping] |
| +21 | OWNINDEX (descriptor number) | NKREC 043233-043243 returns it in [B+31] |
| +22 | KICK PROC / COMSTAT area read as double at +22 | NKSEN 043002-043013 [I on name] |

NOTE +16..21 vs +21: NKSEN writes a 4-word block via the buffer, NKREC reads one word at
+21; the message record tail (HASHLINK/COMSTAT/OWNINDEX/TRACECOND per manual) is only
partially exercised - offsets +21 (OWNINDEX) and +22 (event/kick info) are [V], the
rest of the tail naming is [MANUAL, I].

Port record (TYPE=3), manual figure 25 mapping - **order matches the manual exactly**:

| Offset | Field | Evidence |
|---|---|---|
| +4..5 | FREELINK [MANUAL, I] | |
| +6..7 | USER [MANUAL, I] | |
| +10..11 | MESS HEAD (ID of first queued message) | NKSEN 042613-042625, NKREC 043177-043212 |
| +12..13 | MESS TAIL | NKSEN 042671-042732 |
| +14..15 | KICKLINK (next kicked port) | NKICK 044503-044533, DKICK pop 045053-045065 |
| +16..17 | KICK HEAD (ID into kick table) | NKICK 044363-044400 (symbol KICKH=16) |
| +20 | KICK DEST = octobus station number | NKSEN 042760-042771 + compare vs [B+24]; NKICK 044566-044576 passes it to SKICK (symbol KICKD=20) |
| +21 | INQUEUE (0 = not in kick queue) | NKICK 044410-044422 test, 044546-044561 set 1; DKICK 045041-045052 clear |
| +22..23 | KICK PROC + EVENTS (activation descriptor double) | NKSEN 043002; DKICK 045022-045036 -> SETEV (symbol KKPRO=22) |
| +30..33 | OWNID (4-word port identity: ID + PRANDOM + NETADDRESS) | NKGET fn2 (type 3) reads +30 4w; NKSEN stamps sender identity from homeport+30 (symbol PORT=30) [V offsets, I naming split] |

Sendref record (TYPE=4):

| Offset | Field | Evidence |
|---|---|---|
| +12..13 | (read as double, dest-port-id area) [I] | NKSEN 042467-042507 owner check region |
| +14..15 | DESTINATIONPORT (ID of receiving port) | NKSEN 042401-042416 -> [B+50] -> phys [B+52] |

### 4.3 Message buffer (in the buffer area; addressed by BUFFERPOINTER ID) [V]

| Word offset | Field | Evidence |
|---|---|---|
| +2..5 | current PORT ID + MESSAGE ID (4 words) | NKSEN 042313-042325 copies +12..15 -> +2..5 on send |
| +12..15 | original/home id group (4 words) | source of the above; NKGET fn2 (type 2) reads +12 4w |
| +16..21 | HOME/LAST sender port identity (4 words) | NKSEN 042510-042522 writes homeport+30..33 -> +16 when reply port given |
| +23 | SIZE (bytes) | NKREA/NKWRI bound check 043547-043554; NKGET fn0 |
| +25 | LENGTH (bytes used) | NKWRI update 043614-043654; NKREC reads; NKGET fn1 |
| +26 | start of data | NKREA/NKWRI copy base `+26 + byteoffset/2` (043563-043612) |

Manual figure 27 names (PROTOCOL, MESSAGE STATUS at the very top, +0..1 [MANUAL, I] -
not touched by the carved fast paths).

### 4.4 Kick table [V]

Array of **14B-word entries indexed by octobus station number**, base ID at master+20.

| Offset | Field | Evidence |
|---|---|---|
| +0..1 | KHEAD - ID of first kicked port | NKICK 044423-044436; DKICK read |
| +2..3 | KTAIL - ID of last kicked port | NKICK 044472-044521 |
| +4 | KLOCK - TSET word | NKICK 044401-044405, DKICK 045001 |
| entry size | 14B words (12 dec) | NKINI 042006 `SAA 14 / MPY ,B 24`; symbol KSIZE=14 |

## 5. Routine semantics (annotated summaries + pseudo-C)

Register convention below: `A/D/X/T` = ND-100 registers at entry; return status in A
(0 = OK, 101xxxB = error).

### 5.1 NKSEN @ 042171 - nkSend / send-kick [V]

Entry: `A` = reply(home) port descriptor number or 0, `D` = sendref descriptor number
or 0 (0 = deliver to the message's own HOMEPORT), `X` = message descriptor number or 0
(**0 = kick-only** - no message queued, just kick the destination port).
Caller from MP-P2-TERM-DRIV MTA06 [NPL-V]: `SOWN; D:=sendref; A:=MTNKPORT; X:=0; CALL NKSEND`.

```c
int nkSend(int homePort /*A*/, int sendref /*D*/, int msgNo /*X*/) {
    if (msgNo == 0 && sendref == 0) return NOMSG;
    if (msgNo != 0) {
        msgId  = ACONV(msgNo); msg = phys(msgId);
        if (msg->type != 2)        return ILLTY;      // 042230
        if (msg->owner != curOwner) /* falls through kick path checks */;
        buf = phys(msg->bufptr);                       // +12..13
        copy4(buf+2, buf+12);                          // stamp current ids
    }
    if (sendref != 0) {
        srId = ACONV(sendref); sr = phys(srId);
        if (sr->type != 4)         return ILLTY;
        if (sr->owner != curOwner) return NOACC;
        portId = sr->destport;                         // +14..15
    } else {
        portId = msg->homeport;                        // +14..15 of message
    }
    port = phys(portId);
    if (homePort != 0 && msgNo != 0) {                 // stamp sender identity
        hpId = ACONV(homePort); hp = phys(hpId);
        if (hp->type != 3)         return ILLTY;
        copy4(buf+16, hp+30);                          // OWNID -> buffer
    }
    if (NKLOC(&port->lock) != 0)   return ERLOC;       // TSET, retry loop
    if (port->type != 3)  { NKUNL(); return PORTC; }   // 042571-042577: dest not an
                                                       // open port -> 101032 PORTC
    if (msgNo != 0) {                                  // append to message queue
        if (port->messhead == 0) port->messhead = port->messtail = msgId;
        else { last = phys(port->messtail); last->link = msgId;
               port->messtail = msgId; }
        msg->link = 0;                                 // 042733
        trace(op=2, ...);                              // NKTRA if tracing on
    }
    kickdest = port->kickdest;                         // +20
    NKUNL(&port->lock);
    if (kickdest == 0)             return 0;           // 042776 JAZ: nobody to kick,
                                                       // message stays queued, OK
    if (kickdest == ownStation) {                      // [B+24]
        mode = port->kickinfo & 017;                   // +22..23
        if (mode == 1) jump(port->kickinfo);           // direct resident routine
        else SETEV(port->kickproc, port->events);      // RT / datafield activation
    } else {
        NKICK(port);                                   // queue + octobus kick
    }
    return 0;
}
```

(The exact interleaving of the owner checks and the +16-stamp branch is richer than
this sketch; see `a-nksen-nkins.txt` lines 042171-043075.)

### 5.2 NKICK @ 044354 (internal) - enqueue port on remote station's kick queue [V]

```c
void NKICK(port) {                    // [B+52]=port phys, [B+50]=port ID
    entry = phys(port->kickhead);     // +16..17 -> kick-table entry of DEST station
    if (NKLOC(&entry->klock)) return ERKLO;         // +4
    if (port->inqueue == 0) {                       // +21
        if (entry->khead == 0) {                    // queue was empty
            entry->khead = entry->ktail = portId;   // +0..3
            first = 1;                              // [B+64]
        } else {
            tail = phys(entry->ktail);
            tail->kicklink = portId;                // +14..15
            entry->ktail = portId;
        }
        port->kicklink = 0;
        port->inqueue = 1;
    }
    NKUNL(&entry->klock);
    if (first)
        SKICK(A=1 /*kick number NUCKI*/, X=0 /*ring*/, T=port->kickdest);
}
```

**This is the NKSEND -> octobus crown-jewel link [V]:** `044577 RADD CLD 0 DX (X:=0)` /
`044600 SAA 1` / `044601 LDT ,B 32 (T := port+20 value)` / `044602 BSET ONE SSPTM` /
`044603 JPL I 11 -> [044614] = 037254 = SKICK`. Kick number 1 = symbol `NUCKI=000001`.
SKICK itself (octobus control-frame TX, T=station, A=kick number, X=ring) is carved in
`OCTOBUS-DRIVER-ROUTINES-CARVE.md` section 4 - consistent register convention.

### 5.3 DKICK @ 044747 - receive side: drain own kick queue [V]

Public prologue. Uses [B+7] = own kick-table entry (base + ownstation*14B, cached by
NKINI).

```c
void DKICK(void) {
    while (1) {
        if (ownEntry->khead == 0) { jump WT12; }       // 033616 wait loop
        for (try = 0; NKLOC(&ownEntry->klock); )
            if (++try == 020) jump WT12;               // give up, wait
        port = phys(ownEntry->khead);
        ev   = port->kickinfo;                         // +22..23
        IOF(); SETEV(ev); ION();                       // activate local process
        port->inqueue = 0;                             // +21
        ownEntry->khead = port->kicklink;              // pop (+14..15)
        NKUNL(&ownEntry->klock);
    }
}
```

Activation trigger [V bytes, I chain]: the resident common data holds the pointer word
`044747` (DKICK) twice at VA 125142/125143 (carved in 044-S3IDPIT/053-S3SDPIT, file
word offset 121142 + base 04000). This sits in the NUCLEUS server datafield block
(125153/125170 referenced by SERVE/5SERV/ENUCL/RNMSG) - i.e. the datafield's driver
function slots point at DKICK, so the octobus kick-1 reception (octobus module, sibling
doc) activates DKICK through the standard datafield driver mechanism. Exact octobus
kick-1 -> datafield wiring: CLOSED 2026-07-20 [V] - see CONKI-KICKENT-CARVE.md:
NKINI calls CONKI(T=1, A=14B, X=0, B=125144); receive path = frame decoder 035555 ->
kick dispatch 036047 KICKENT[frame & 17B] -> DLEVE code 14B arm 036233 fires PIL
level 12 with B := 125144, P := mem[125143] = 044747 = DKICK. Incoming KICK 1
dispatches to DKICK, matching the send side (NKICK -> SKICK kick 1) end to end.

### 5.4 SETEV @ 044620 - local activation dispatch [V]

16-way `JMP` table on `(activation word & 017)`:
- 0: RT program - `X := n*10B + 115542` (RT-description table), validity check, call
  resident 023367 (set event/activate).
- 1: direct jump to resident routine address (also short-circuited inside NKSEN).
- 2: `B := datafield; JPL I -> 013552` - datafield driver activation (this is how the
  MTAD "MTNUCL, activated by NUCLEUS, lev 2" driver is entered [NPL-V]).
- 7/010/011: variants calling resident 026755/027002/027027 with B=datafield.
- others: error via resident 000215.

### 5.5 NKREC @ 043076 - nkReceive [V]

Entry: `T` = port descriptor number. Checks owner (+2..3) then TYPE==3, locks port,
pops MESS HEAD (queue empty -> status path at 043330), transfers message OWNER to
caller, returns in-registers: message descriptor number (from msg+21 OWNINDEX),
buffer info read from buffer header (+25 LENGTH etc.). Errors: NOACC, ILLTY, ERLOC.

### 5.6 NKREA / NKWRI / NKINS @ 043375 / 043411 / 043425 - buffer data movers [V]

Three stubs setting mode 0/1/2 into cell 043534, then the common body 043440:
`T` = message descriptor number, `X` = byte offset (NKREA/NKWRI) , `D` = caller buffer
descriptor (address in caller space).

```c
int nkfReadWrite(int msgNo /*T*/, int byteoff /*X*/, addr user /*D*/, int mode) {
    msg = check_type2_owner(ACONV(msgNo));           // ILLTY / NOACC
    buf = phys(msg->bufptr);
    len = min(request, buf->SIZE /*+23*/ - byteoff);
    L   = (len+1)>>1;                                 // words
    if (mode == READ)  movew_to_caller (user, buf + 026 + byteoff/2, L);  // 143107
    else               movew_from_caller(buf + 026 + byteoff/2, user, L); // 143105
    if (mode == WRITE or INSERT)
        buf->LENGTH /*+25*/ = max(buf->LENGTH, byteoff + len);
    return D = len;                                   // bytes moved
}
```

### 5.7 NKGET @ 043672 - nkGetInfo [V]

Entry: `A` = function 0..4 (else NILLF), `D` = descriptor number, `X` = caller
destination. Owner-checked. fn 0: buffer+23 (SIZE); fn 1: buffer+25 (LENGTH);
fn 2: 4 words - buffer+12 (type 2) or descriptor+30 OWNID (type 3); fn 3: buffer+16
(4w) / type-3 variant descriptor+10; fn 4: buffer+26 (first data words) / +16.
(Exact fn/type matrix in `a-nkget-nkick.txt` 043773-044170.)

### 5.8 NKINI @ 041703 - initialise kernel binding [V]

- Maps the physical area: `TRR PCR` with literal PCR values 052216 (on) / 051616 (off)
  under IOF, cell [004010]+17 = per-level PCR image slot.
- [B+4] := page>>6 (bank) from cell MLGRS 042027; CLUST := A<<8 (ND-500 owner base).
- Converts the master-block ID (cells 042023..) -> phys [B+11..12]; verifies
  master+25 version word (mismatch -> A=-1 fatal).
- Caches: descriptor base ID (master+2), descriptor count (master+7), own kick entry
  phys (master+20 + ownstation*14B), trace cells (master+34/40/42/104).
- Registers the kick connection: bumps resident counters 007307/007310, then
  `T:=1; A:=14B; JPL CONKI` with B := a datafield (value cell 042164 = 125144 - inside
  the same resident server-datafield block as the DKICK pointers 125142/125143 [V]).

### 5.9 NKLOC / NKUNL @ 045134 / 045214 [V]

NKLOC: `TX` = physical address of lock word, lock value 070000B, atomic set via opcode
**140516 (nd100-dis "USER1")** - the physical-memory TSET (manual: "LOCK -> used for
TSET"). Retry loop up to 020 times; between retries checks master+74/+76 flag doubles
(remote-CPU health [I]); returns A=0 acquired, 1 = retry-exhausted, 2 = health-flag
abort. NKUNL: writes a zero word to the lock physical address (MOVEW 143102).

### 5.10 NKTRA @ 044204 - trace writer [V]

Locks master+104, cur := master+42, wraps vs cached max, writes an 8-word record at
`phys(master[+34..35]) + cur*8`: +0 op code ([B+23]: 2 = send observed), +1 message
([B+31]), +2..3 owner ([B+5..6]), +4.. tail from [B+67] context. Unlocks, returns.

## 6. MON 347B - the NUCLEUS monitor call [V]

Two independent doors, both byte-verified:

1. **ND-100 processes:** `MCTAB[347B] = 047072`. Measured directly in
   `044-S3IDPIT.bin` (base 04000): slot 005620B+347B = 006167B holds 047072; validation
   slots 005B->102021 (RDISK), 144B->026354 (MAGTP), 317B->050701 (UECOM) all match.
   047072 = symbol `SERVE` (SYMBOL-1-LIST line 7082; the MON-CALL-INDEX name `MGDAE`
   for this slot is a flat-table collision from another link job - MGDAE also appears
   in N500-SYMBOLS as 121675). SERVE validates the request (function <= 13B via [B+11])
   against the server datafield 125153 (word +1 = alive flag, else NNOTI) and transfers
   into the NUCLEUS server (X := [df+21], `MST PIE`, jump) - i.e. the real work is done
   by the server program on segments 104/105.
   **Discrepancy note:** the carving-skill's MCTAB validation example "MON 200B ->
   007516B" does not reproduce - `MCTAB[200B] = 000000` in this carve [V]. 200B (XMSG)
   is a level-14 GOTAB fast call, so an empty MCTAB slot is coherent.
2. **ND-500 processes:** the level-12 driver dispatch (MP-P2-N500.NPL L1381
   `IF A = 347 GO 5SERVER` [NPL-V]) enters **ENUCL 050123** [V by structure]: function
   code read from message word +102; fn 7 -> level-12 driver code 137167; fn 0..6 ->
   N5FU0 (NKGET), N5FU1 (NKSEN, under IOF), N5FU2 (NKREC), N5FU3, N5FU5, N5FU6, each
   reading arguments out of the 5MPM message (LDATX/LDDTX via cells 004654/011260),
   setting owner = CLUST + process, and answering through NURET (writes result into
   message words +110/+0/+2, chains resident 023044 and driver cells 145466/135067).
   `5SERV 050211` forwards non-primitive functions to the server process, like SERVE.
   NUCLEUS delayed abort: `NKREL: CALL 5NUREL; GO MONEN` at MP-P2-N500 L563 area
   [NPL-V, not carved here].

## 7. NCALL @ 050407 - ND-100 mailbox call to the NUCLEUS server [V]

The "buffer for routine NCALL" allocated by NUCST [NPL-V]. Mechanism:

- Mailbox physical address in resident cells **007300/007301** (bank/addr); a second
  mailbox for the server side in **007276/007277** (used by RNMSG). These are the
  NMAA1/NMAA2 / SMAA1/SMAA2 pair written by NUCST [NPL-V mapping, V cells].
- GWAIT 045643 serialises access; PCR is retargeted (TRR PCR, values 052216/051616)
  to reach the physical mailbox page.
- Request: mailbox+0 = state (4 observed = free, 5 = posted [I]), +1..+4 = caller
  identification (level, B, ids), +10 = parameter byte length (bounded by 377B, error
  cell 101427), parameters copied in with MOVEW 143105.
- Server activation then wait; errors NILLF / 101427 family; answer copied back with
  MOVEW 143107 (050630-050650).

The NCALL layout is documented here only to the byte-verified skeleton; a full
field-by-field mailbox map is a follow-up (see RECON target list).

## 8. NUCST (boot allocation) [NPL-V - PH-P2-OPPSTART.NPL L4098/063570]

Allocates `NXRTP*2` words of abort table + 2 extra pages in physical memory:
`NUPHYSPAGE` = first/last page; bank -> NMAA1=SMAA1=ABTBNK; NMAA2 = addr,
SMAA2 = addr+2000B, ABTSTART = addr+4000B; ABTPUT=ABTGET=0. I.e. one page NUCLEUS
server mailbox, one page second mailbox, then the abort table. Generation parameters
[NPL-V, PH-P2-CONFG-TAB.NPL 036503 NUPAR]: msg-buffer area system=250, descriptors
system=500, public=250/300, per-user=10/10, trace size=2, startup fn=0. RP-P2-CONFG
L164: `NUCON:=(NUPAR,12,1,CXDUM)`.

## 9. Open questions / follow-ups

- **ENKIC=047526 (N500-SYMBOLS) overlay [OPEN].** In MPIT 047526 is mid-N5FU2; tried
  commoncode, MACM rtloader, RPIT/DPIT/IPIT/5PIT and every carved segment covering
  044000-050400 - no coherent entry. Its N500-SYMBOLS neighbours (NSPIT=044000,
  GMESS=044717, ACCPE=045001, VPARP=045531, OCTOS=050361, NMPIT=050000) are the
  octobus/ACCP ND-5000 servicer family, i.e. a different module (sibling agent's
  domain). Caller pointer-words for 047526 exist in 007-S3DMAC, 130-CFT, 135-XFTRAD,
  134-SNA3270. Hand to the octobus agent.
- Master block +10/+12 hash array/mask, +26 net table, buffer start/end: only named
  [MANUAL/symbol]; exercised by the server segments, not the primitives. Carve segment
  105 to prove them.
- Message record tail (+16..17 HASHLINK, COMSTAT, TRACECOND naming), buffer header
  words +0/+1 (PROTOCOL/STATUS): [I]/[MANUAL] - server-side.
- NCALL mailbox full field map; GWAIT/GNMSG bodies; WT10/WT12/WT13
  (033542/033616/033671) server wait loops - shared edge with the octobus module.
  (CONKI full decode: DONE 2026-07-20, CONKI-KICKENT-CARVE.md. Correction: the
  "kick-entry registration list at cells 007341-007344" lead here was WRONG -
  007341/007342 are the CBPOOL free-list head/count; CONKI writes the KICKENT
  table in octobus physical memory, bank at input-df[-14].)
- NKUSE=144162 (resident data region, zero on disk) - live capture needed.
- MON-CALL-INDEX row 347B should be renamed SERVE (NUCLEUS) and its "003-S3CP"
  overlay attribution corrected to MPIT; MCTAB[200B]=0 discrepancy recorded.

## 10. Reproduction commands

```bash
# byte-swap + disassemble the overlay
wsl python3 -c "d=bytearray(open('/mnt/e/.../segments/026-S3IMPIT.bin','rb').read());\
d[0::2],d[1::2]=d[1::2],d[0::2];open('/tmp/026.le','wb').write(d)"
wsl nd100-dis -a -o -b 13312 /tmp/026.le > 026.dis
# verify MCTAB[347B]
wsl python3 -c "d=open('/mnt/e/.../segments/044-S3IDPIT.bin','rb').read();\
i=((0o5620+0o347)-0o4000)*2; print(oct((d[i]<<8)|d[i+1]))"   # -> 0o47072
```
