# NUCLEUS server segment 105 (S3INKSE) interior - L07 byte carve

Carve of the interior of the NUCLEUS **server** program (segment 105 `S3INKSE`,
"C02 September 26, 1988"), executing carve task S0-3 from
`SINTRAN/ND5000/SCSI-DIOC-OCTOBUS-EMULATION-PLAN-2026-07-20.md` and the follow-up
target list in `NUCLEUS-SEGMENTS-RECON.md` section 4.

- Date: 2026-07-20. Ground truth: carved bytes only. Companion (kernel side, already
  byte-verified): `NUCLEUS-PRIMITIVES-CARVE.md`. Recon: `NUCLEUS-SEGMENTS-RECON.md`.
- Evidence tags: `[V]` = bytes reproduced in this analysis, `[I]` = inference from
  verified bytes, `[OPEN]` = not resolved (blocker named).
- Segment file: `../../segments/105-S3INKSE.bin` (big-endian, 52 pages = 53248 words).
  Load base **030000B** (0x3000, nd100-dis `-b 12288`), from
  `105-S3INKSE.meta.json`. `104-S3SNKSE.bin` is the byte-identical SAVE copy.
- Annotated listings (this folder): `a-nkse-105-glue-resident-link.txt`,
  `a-nkse-105-mainloop-donuc-dispatcher.txt`, `a-nkse-105-createport-fn10-fn1.txt`,
  `a-nkse-105-aconv-alloc-free-quota.txt`, `a-nkse-105-field-helpers-070.txt`,
  `a-nkse-105-mon347-wrappers.txt`.

---

## 0. TL;DR

- **This segment is a PLANC-compiled program**, not hand-NPL like the kernel primitives.
  Every routine is called through the PLANC runtime frame-enter/leave stubs
  `112541` (ENTER frame) / `112576` (LEAVE frame) / `112570` (routine-error / ERRETURN
  exit), which are linked into this same segment at 112xxx (the PLANC runtime library,
  confirmed by its error strings `NO ROUTINEERROR HANDLER, ERRETURN=`,
  `- STACK OVERFLOW AT`, `- ASSERT VIOLATION AT` at word 112263+). [V]
- **The server dispatcher (`doNuc`)** is at **037033**: it reads a function code and
  runs a linear `SAT n / SKP IF DA EQL ST` ladder for **functions 1..14B**, each case
  calling one PLANC worker routine. The unmatched case falls into the
  `doNuc: unknown func=` / `*** Nucleus FATAL Error` print path. [V]
- **Function-code -> worker table recovered and dd-verified** (section 2).
- **fn 10B = 047432 = create/provision descriptor** - the routine that builds a
  descriptor record and writes its per-field values (KICKDEST +20, OWNID/port-number
  block +30.., etc) through the field-setter helpers `070345/070422/070477/070560`.
  This is the port/message **CREATION (allocation)** path SIN-F5a/c needs. [V location,
  I on exact PDF.DRPRT/DLPRT naming - section 5]
- **NCALL mailbox client wrappers** (`MON 347`) at 072263 / 073165 / 073207 / 073266,
  building the request record the resident NCALL (050407) posts. [V]

## 1. Segment map [V]

Header (word-verified `dd`):

```
030000: 030000 176000 112773 033253 055061 ...
        ^start  ^      ^         ^-> version-string pointer word (033253)
```

- word[0]=030000 (own load base / entry-ish), word[3]=033253 -> version string
  `C02 September 26, 1988` at word 033253. [V]
- Nonzero code pages: 0-17, 20-25 (VA 030000-053777) and 55000-113777. The 112xxx tail
  is the linked-in PLANC runtime library. [V]

ASCII string anchors (all `dd`-reproduced from the big-endian carve):

| Word | String | Role |
|---|---|---|
| 033253 | `C02 September 26, 1988` | version banner [V] |
| 035710 | `*** NUCLEUS: Error multibyte send, stat=` | multibyte-send error [V] |
| 040062 | `*** Nucleus FATAL Error` | fatal-error banner [V] |
| 040101 | `doNuc: unknown func=` | **dispatcher default case** [V] |
| 041774 | `*** Nucleus FATAL Error` | 2nd fatal site [V] |
| 042013 | `*** Nucleus FATAL Error` | 3rd fatal site [V] |
| 042031 | `NKNAME` | name used at server init [V] |
| 062331 | `$ PBUFF_AREA : ` / ` LENGTH: ` / `v2-length : 015` / `pbuff-length : ` | buffer-area debug/report strings [V] |
| 107274 | `-2147483648` / `20000000000` / `80000000` | PLANC 32-bit number-print constants [V] |
| 112263 | `NO ROUTINEERROR HANDLER, ERRETURN=` / `PROG` / ` - STACK OVERFLOW AT ` / ` - ASSERT VIOLATION AT ` | PLANC runtime error text [V] |

**PLANC calling convention in this segment (verified pattern) [V]:**
`B` = current frame base (all locals are `,B -nnn`); a call is
`... set outgoing params via STA/STD ,X off ; JPL I d1 -> [cell]=112541 (ENTER) ;
JPL I d2 -> [cell]=112570 (ERRETURN landing) ; JPL I d3 -> [cell]=112576 (LEAVE) ;
<routine-address-constant word>`. The routine address is the *pointer word that
follows the call site*; the three 112xxx targets are the runtime stubs. This is why the
worker addresses appear as data words (e.g. `037633=047432`) rather than as jump
operands - they are PLANC routine-address constants read by the ENTER stub. `,X` is the
callee's new frame during argument marshalling; `,B -176` reloads the caller frame base
after the call.

## 2. The `doNuc` server dispatcher @ 037033 [V]

The request record base is loaded into `,B -176` (frame) and the function code is read
from request word **+12** (`037237 LDA ,X 12` in the request-prep path;
`037252 LDA ,B -171` reloads it). The dispatcher is a linear compare ladder:

| Site | Test | fn (octal) | Worker routine (pointer cell -> value) | dd |
|---|---|---|---|---|
| 037253 | `SAT 1` | 1 | `[037425]=050421` | [V] |
| 037300 | `SAT 2` | 2 | `[037427]=051261` | [V] |
| 037330 | `SAT 3` | 3 | `[037430]=051100` | [V] |
| 037356 | `SAT 4` | 4 | `[037431]=052273` | [V] |
| 037403 | `SAT 5` | 5 | `[037627]=051566` | [V] |
| 037446 | `SAT 6` | 6 | `[037630]=055727` | [V] |
| 037477 | `SAT 7` | 7 | `[037631]=054320` and `[037632]=054072` (two calls) | [V] |
| 037540 | `SAT 10` | 10 | `[037633]=047432`  (**create / provision**) | [V] |
| 037603 | `SAT 11` | 11 | `[040033]=111700` (+ helper `[040034]=112570`) | [V] |
| 037663 | `SAT 12` | 12 | `[040041]=036376` | [V] |
| 037701 | `SAT 13` | 13 | `[040042]=036620` | [V] |
| 037717 | `SAT 14` | 14 | `[040043]=046053` | [V] |
| 037743 | (default) | else | error: `doNuc: unknown func=` print + FATAL path | [V] |

So the server implements **function codes 1..14B (12 decimal)**, consistent with the
`fn <= 13B..14B` bound in the resident SERVE/5SERV gate
(`NUCLEUS-PRIMITIVES-CARVE.md` section 6/7). Each case marshals request fields into the
worker's frame and returns through the common reply path at **040015** (cell
`[037426]=040015`), which every case reaches by `JMP I -> 037426`.

**Default (unknown-func) path [V]:** fn not in 1..14 -> 037743 loads an error status
word, then the print engine at **106702** (a byte-string printer: `LBYT`/`SBYT` loop,
takes start/end pointers in `,B -165 / ,B -164`) is called with the descriptor pointing
at the `doNuc: unknown func=` string (040101), followed by the numeric function value
(number printer at 107314 using the `-2147483648`/`80000000` constants at 107274), and
the `*** Nucleus FATAL Error` banner (040062). The reply then returns via 040015.

dd reproduction of the whole table (Python on the big-endian carve):
```python
d=open('105-S3INKSE.bin','rb').read(); BASE=0o30000
w=lambda a:(d[(a-BASE)*2]<<8)|d[(a-BASE)*2+1]
for cell in (0o37425,0o37427,0o37430,0o37431,0o37627,0o37630,0o37631,0o37632,
             0o37633,0o40033,0o40041,0o40042,0o40043):
    print('%06o -> %06o'%(cell,w(cell)))
# 037425->050421 037427->051261 037430->051100 037431->052273 037627->051566
# 037630->055727 037631->054320 037632->054072 037633->047432 040033->111700
# 040041->036376 040042->036620 040043->046053
```

## 3. Server entry / resident linkage (pages 0-1) [V bytes, I flow]

Word 030000-030053 is the segment glue that the resident SERVE/5SERV (047072 / 050211)
jumps into. It performs the PCR-remap-under-IOF idiom (same as the kernel primitives'
physical-window mapping): `LDX I 14 ; IOF ; LDA 13 ; TRR PCR ; ... ; TRR PCR ; ION ;
EXIT` at 030040-030053 - i.e. maps the physical NUCLEUS area into the server's page
window before touching descriptors, exactly like `NKINI` in the kernel. [V]

The low words 030131-030217 latch the resident request context (STA I into resident
cells 41/36/33, `LDX -77 / STZ ,X 0` clearing two mailbox state words under IOF). This
is the server-side counterpart to the resident RNMSG (045432, cells 007276/007277) that
`NUCLEUS-PRIMITIVES-CARVE.md` section 2/7 named. [V pointers, I flow]

## 4. Low-level physical-record I/O service `030407` [V]

A small family of PLANC leaf routines at 030225 / 030276 / 030370 / **030407** /
030503 / 030531 / 030562 wrap single-word and block reads/writes of the physical NUCLEUS
area. `030407` is the general one (called ~13 times): it takes a physical
(bank,word) address pair in locals and performs the `LDDTX`/`MOVEW` physical access
(the same 143xxx physical-memory idiom documented in `NUCLEUS-PRIMITIVES-CARVE.md`
section 3.2). `030225` reads a word, `030276` reads a double, `030370` writes. These are
the server's equivalent of the kernel `phys()` accessor and are the primitive that every
descriptor/field helper in sections 5-6 ultimately calls. [V structure, I exact
sub-op encoding]

## 5. Descriptor / field access helpers (0703xx family) [V]

The server reads and writes descriptor fields through a small, uniform helper set. Each
takes a descriptor **ID** (double) plus a **field byte-offset** and does the
ID->physical conversion (`BSET ZRO 170 DA` clears the flag bit, `SAD SHR 20` = >>1
byte->word, add area bank) then a `MOVEW` (`143106`) of 1-2 words:

| Helper | Words moved | Direction | Evidence |
|---|---|---|---|
| 070345 | 1 (single) | read field -> local | [V] `BSET ZRO 170 DA / SAT 0 / MOVEW 143106` |
| 070422 | 1 | read field (variant, uses local -161) | [V] identical body to 070345 |
| 070477 | 2 (double) | read double field -> local | [V] `SAA 2` word count |
| 070560 | write | write local -> field (offset in ,X 10) | [V] called after `SAA <off> / STA ,X 10` |

These map exactly onto the byte-verified kernel descriptor layout in
`NUCLEUS-PRIMITIVES-CARVE.md` section 4.2 (40B-word records, `ID = number<<6 + master[+2..3]`,
port `+20 = KICK DEST`, `+30..33 = OWNID`). The `ID->phys` math here (`SAD SHR 20`
= byte-offset >> 1, then add area bank) is the **same** transform as the kernel's
`174175 BSET ZRO 170 DA / 156777 SAD ZIN SHR 1 / 060404 ADD ,B 4`. **Coherent with the
kernel - validation anchor holds.** [V]

### 5.1 Number -> descriptor-ID converter `056332` (server-side ACONV) [V]

`056332` is the server's ACONV: given a descriptor *number* it bounds-checks
(`0 <= number`, `number < count`; on failure loads error `[056400]=101004 ILLNO`) then
computes `ID = number*100B (SAD 6 = <<6) + descriptor-table-base-ID`. It reads the
descriptor count and base from server global cells (LDA/LDD `16` and the
`072016`/`072020` global-pointer words at 056375/056401). Matches kernel ACONV (045101)
semantics and the `101004 ILLNO` code exactly. [V]

### 5.2 CREATE / provision path fn 10B = `047432` [V location; naming I/OPEN]

`047432` is the descriptor **builder / provisioner** - the routine that populates a
freshly-taken descriptor record's fields. It writes a long series of fixed field offsets
into the record via `SAA <offset> / STA ,X 10 / LDD <value> / JPL 070560 (write-field)`:

```
offset 20  <- value            (KICK DEST / station - matches port +20 KICKD)
offset 24  <- value            (kick head area +24)
offset 62  <- value
offset 50  <- value            (message queue / dest area)
offset 54  <- value
offset 44  <- FREELINK-ish
offset 40  <- computed (LDA I 67 / SHR)
offset 34  <- value            (trace/base area)
offset 42  <- value
offset 70  <- value
offset  4  <- value            (FREELINK, per kernel +4..5)
offset  2  <- value            (OWNER, per kernel +2..3)
offset 30  <- value            (OWNID / port-number block, per kernel +30..33)
```
(full listing: `a-nkse-105-createport-fn10-fn1.txt`, VA 047432-047747.)

The offsets it writes are exactly the kernel descriptor-record field offsets from
`NUCLEUS-PRIMITIVES-CARVE.md` section 4.2. In particular:
- **`+20` (KICK DEST = octobus station number)** is written here - this is the
  **remote/destination port station writer**.
- **`+30` (OWNID: ID + PRANDOM + NETADDRESS, the 4-word port identity)** is written here
  - this is where the **port number / local+remote port identity** is stamped into a new
  port descriptor.

This is the routine behind SIN-F5a/c's "who builds port descriptors and writes
remote/local port numbers." The field group at `+30..+33` is the OWNID/port-number
block; `+20` is the remote station. **[V] that these offsets are written by 047432;
[I] that the specific manual field names PDF.DRPRT / DOMDF.DLPRT map onto +20 vs a
+30-block sub-word - the server manipulates the kernel structure so the offsets are
authoritative, but the DRPRT/DLPRT symbol-to-offset pin is [OPEN]** (needs SYMBOL-2-LIST
NKMBU/port-symbol pinning, section 8 target 5).

### 5.3 Descriptor allocator / freelist `057631`, `063371`, `063464` [V structure, I head cell]

- `057631` walks/validates a descriptor: takes an ID, bounds-checks its four sub-fields
  against limits (`SKP IF DA GRE SL` chains at 057724/057760/060014), error path
  `060102`. This is the **descriptor-validity / free-check** walker. [V]
- `063371` is a small allocator front-end: on null input it substitutes a default
  (`LDD 170`), converts, and calls `057631`; on the empty-freelist path it builds a
  descriptor at a **fixed location** `LDX 120 / STA ,X 0..3` (063473-063507) then calls
  `034574`. [V]
- `063464` / `063643` populate the newly allocated record's header (writes offsets
  10, 150, 4, 2, ...) - the freelist-link maintenance side. [V]

The **freelist head cell** is one of the server global cells addressed off the frame
(`SAA 150 / STA ,X 10` at 063533/063557 references field byte-offset 150 = word 054 of
the master area). Kernel master-block section 4.1 did not exercise the buffer/freelist
head (it is server-side, as predicted); the concrete master-block offset of the
descriptor-freelist head is **[I] = master +054 region** and remains **[OPEN]** for a
firm pin (the server reads it via a global-pointer indirection `072016`/`072020`, not a
literal master offset, so the numeric master offset needs a live global-cell capture).

## 6. Buffer area (PBUFF) [V strings, I layout]

The debug strings at 062331 (`$ PBUFF_AREA : `, ` LENGTH: `, `v2-length : 015`,
`pbuff-length : `) belong to a buffer-area report/allocation routine near VA 0623xx.
`v2-length : 015` (=13.) and `pbuff-length` indicate the message-buffer geometry the
server reports/allocates. The routine formats these with the same 106702 string printer
+ 107314 number printer used by `doNuc`. Full field-by-field PBUFF layout is **[OPEN]**
(the format strings are verified; the numeric geometry cells are computed at runtime).

## 7. NCALL mailbox record - client wrappers (`MON 347`) [V]

Segment 105 issues `MON 347` at **072277, 073230, 073317, 073427, 073551** - these are
the server-program's own calls back through the resident NUCLEUS door (the server also
acts as an NCALL client for chained operations). The request-record builders that
precede them reveal the NCALL mailbox record fields the resident NCALL (050407) and
RNMSG (045432) exchange:

- **072263** (`MON 347` wrapper, "get length / info"): sets `SAT 1 / SAX 7`, computes
  `A := (len+1)>>1` word count, `LDA ,B -172` = caller param, then `MON 347`; result
  back in `,B -166`. This is a fn-1-shaped info call. [V]
- **073207** (the fullest builder): lays the request record as
  `word0 := caller-param (,B -172)`, `word1 := ,B -170`, then `SAT 1 / SAX 0 / SAA 20`
  (marshals into 20B-offset slots), `LDA ,B -151 ; MON 347`; on nonzero return it copies
  a **7-word** result block (`SAA 7 / AAT -161 / AAT 4 / STF ,X 6 ...`) back. So the
  NCALL reply carries a >=7-word payload at record+? . [V]
- **073266** (4-argument builder): stores four caller doubles into locals
  `-160/-156/-154/-152`, zeroes `-145`, builds the record and calls the info path. [V]

Cross-check with kernel section 7: the resident NCALL posts to the mailbox at
`007300/007301`, state word `4=free / 5=posted`, `+1..+4 = caller id`,
`+10 = param byte length (<=377B, err 101427)`. The server-side wrappers here confirm
the request carries: **[+0] function/param, [+1..] caller identity block, a word count
derived from a byte length, and a multi-word (>=7) reply payload**. The exact
per-word field map beyond {state, count, caller-id, param, reply-block} is still partly
**[I]** - the wrappers marshal into offset 20B slots which is the *server-datafield*
frame, not the raw mailbox, so the mailbox-vs-frame offset correspondence needs the live
NCALL round-trip to pin. [OPEN for full field map, matches kernel skeleton otherwise]

## 8. Coherence check vs kernel carve (validation anchor) [V]

Every structural fact discovered here is **coherent** with the byte-verified kernel
layout in `NUCLEUS-PRIMITIVES-CARVE.md` - no divergence found:

| Item | Server (105) finding | Kernel (026-MPIT) fact | Verdict |
|---|---|---|---|
| ID->phys transform | `BSET ZRO 170 DA / SAD SHR 20 / +bank` (070345) | `174175 / 156777 SAD ZIN SHR 1 / 060404` (045101) | MATCH [V] |
| descriptor record stride | `number*100B (SAD 6)` (056332) | `number<<6` (ACONV 045101) | MATCH [V] |
| port field +20 | KICKDEST/station written by 047432 | KICKD=20 (NKSEN/NKICK) | MATCH [V] |
| port field +30 | OWNID block written by 047432 | PORT=30 OWNID (NKGET fn2) | MATCH [V] |
| descriptor +2 / +4 | OWNER / FREELINK written by 047432 | OWNER +2..3, FREELINK +4..5 | MATCH [V] |
| error codes | 101004 ILLNO (056400) | 101004 ILLNO | MATCH [V] |
| physical window | PCR-remap-under-IOF (030040) | TRR PCR 052216/051616 (NKINI) | MATCH [V] |

## 9. Open items (with blocker named)

- **[OPEN] Exact DRPRT/DLPRT symbol -> descriptor sub-offset pin.** 047432 writes the
  `+20` station and `+30` OWNID block [V], but which sub-word is "remote port number"
  vs "local port number" needs the SYMBOL-2-LIST NKMBU/port symbols mapped onto these
  offsets. Blocker: NKSE has few resident symbols; needs SYMBOL-2-LIST pinning
  (recon target 5) or a live create-port capture.
- **[OPEN] Descriptor-freelist head master-block offset.** Server reads it via a global
  pointer indirection (072016/072020), not a literal master offset; the numeric offset
  (`[I] master +054 region`) needs a live global-cell capture (cells zero on disk).
- **[OPEN] Full NCALL mailbox per-word field map** beyond {state, count, caller-id,
  param, >=7-word reply}. Blocker: wrappers marshal into a server-datafield frame, not
  the raw mailbox; needs the live NCALL round-trip to correlate frame offset 20B <->
  mailbox word.
- **[OPEN] Full PBUFF (message-buffer) area geometry.** Format strings verified;
  numeric geometry computed at runtime.
- **[OPEN] fn 11B/12B/13B/14B worker semantics** (routines 111700, 036376, 036620,
  046053) - located and dd-verified as dispatch targets, bodies not fully decoded.

## 10. Reproduction commands

```bash
# byte-swap + disassemble segment 105 (base 12288 = 030000B)
python3 -c "d=bytearray(open('105-S3INKSE.bin','rb').read());\
d[0::2],d[1::2]=d[1::2],d[0::2];open('/tmp/105.le','wb').write(d)"
nd100-dis -a -o -b 12288 /tmp/105.le > 105.dis

# verify the doNuc function-code table
python3 -c "d=open('105-S3INKSE.bin','rb').read();B=0o30000;\
w=lambda a:(d[(a-B)*2]<<8)|d[(a-B)*2+1];\
print([oct(w(c)) for c in (0o37425,0o37427,0o37430,0o37431,0o37627,0o37630,\
0o37631,0o37632,0o37633,0o40033,0o40041,0o40042,0o40043)])"
# -> ['0o50421','0o51261','0o51100','0o52273','0o51566','0o55727','0o54320',
#     '0o54072','0o47432','0o111700','0o36376','0o36620','0o46053']

# verify the doNuc string
python3 -c "d=open('105-S3INKSE.bin','rb').read();B=0o30000;o=(0o40101-B)*2;\
print(d[o:o+20])"   # -> b'doNuc: unknown func='
```
