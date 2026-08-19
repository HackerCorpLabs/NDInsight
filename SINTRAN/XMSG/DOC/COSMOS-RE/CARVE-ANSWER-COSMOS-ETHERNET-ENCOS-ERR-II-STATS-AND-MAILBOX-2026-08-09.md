# CARVE ANSWER: encos-err-ii-b01.brf — module map, the statistics record, and the mailbox-block shape

Date: 2026-08-09. Question driving the carve: **when does the ND-100 host send the Ethernet II
card the POST-BUFFER command (card-side opcode 0x12) that refills the rx pool** — the open item
from `..\COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md` §6b and RetroCore's
`DOCS\ND_EthernetII_LINK_FRAMES_AND_RXPOOL_HANDOFF_2026-08-04.md`.

**Status: the re-post policy is NOT yet answered.** What IS answered: what this module is, the
exact MA-statistics record label order (live-verified), the module's only gateway to the
interface (a MON 255B wrapper family), a command/request-block builder with its opcode set, and
— live, on D100 — that `(UTILITY)ENCOS-MON-II-B01:PROG` reads the card's drop counters from a
running machine. Plus one avoided trap recorded so the next session does not re-fall into it.

Binary: `E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\x\encos-err-ii-b01.brf`
(61,005 bytes, COSMOS Ethernet II Option distribution 210580 rev B01), loaded in Ghidra as
`encos-err-ii-b01.brf` (ND-100 BRF loader, addresses are WORD addresses from module base 0).
Per `..\..\..\..\Installation\Communication\Ethernet\x\README.md` the ERR BRFs are the
**ND-100-side supervisor (RT program ENNS0, segment ENCOSE0)**; the load transcript
`..\..\ENCOS-LOAD-0-B01.LIST` (parity-mangled but readable) shows ENNS0 loaded onto **segment
101** and the four SER firmware banks onto segments 102–105.

CAVEAT on names: the Ghidra BRF loader's symbol-name decode is garbled (labels like `5__2$-3`)
— names below are positional, not symbolic. The live RT loader's symbol table is empty
(WRITE-SYMBOLS prints nothing; entries were cleaned at install), and no NPL/PLANC source for
ENCOS exists in the tree.

## 1. Module map [V, from strings + disassembly]

| Word addr | Contents |
|---|---|
| 0x1985+ | ID: `UE-ERMSG-EN-B`, `COSMOS Ethernet I/II Option : B01 - 1987-02-27`, `ENNS0-POSU...` |
| 0x19bd–0x1aca | **MON PIOC error-text table**: "Undefined MON PIOC error / No answer from interface / Insufficient priviledge / Interface not started / Illegal function code / Slot alr... / Mailbox is not empty / Mailbox is empty / Illegal LDN. Order new SINTRAN with PIOC/Ethernet / Interface not initiated / ... / Device already reserved. PIOC-MONITOR in use?" |
| 0x1e0e+ | Dump writer: file name `ENCOS-DUMP:DATA`, banner "DUMP FROM COSMOS ETHERNET OPTION B01" |
| 0x2204–0x22fa | **Trace-entry decoder** labels: entry classes ` MESG` / ` XROUT` / ` MONTR` with per-class fields — MESG: "init virtual address / send to ND / length / receive stop / receive from ND / status OK FAILED TRANSMIT to ND"; XROUT: "subfunction getMagic"; MONTR: "stats for MA / for system ND / trace enable disable read" |
| 0x2489+ | NPDU decoder (" DT nrI4 length / I5 AK nrI4 credit / DR by user reason..."), FATAL/non-fatal |
| 0x26c8+ | Trace report printer ("ETHERNET COSMOS SERVER TRACE FOR LOCAL SYSTEM", version check) |
| 0x2af5–0x2d40 | **MA statistics report labels** — see §2, this is the counter record order |
| 0x36bb–0x37f5 | **MON 255B (PIOC) wrapper family** — see §3 |
| 0x51c8–0x51f0 | Debug keyword tables: register-pair codes `W ,R ,WX,RX,RW,WA,WC,RC,D ,DC` (indices 0–9) and keywords `SYMB READ WRITE RXS WXS` |
| 0x51f1+ | A command/keyword PARSER (LBYT/SBYT byte walk, lowercase→uppercase fold `SAT 0x60 / AAA -0x20`, dispatch via `EXR ST`) |
| 0x52e7–0x5341 | **Request-block builder with a 4-way decision ladder** — see §4 |

## 2. The MA statistics record — label order = record order [V bytes; live-verified output]

Label block at word 0x2af5 (file bytes 0x55ea+). Print order, i.e. the order the counters sit in
the statistics record the interface returns (`I14` = 14-wide integer, `I5` = the version word):

```
version (I5, checked: "INCOMPATIBLE STATISTICS - VERSION")
frames transmitted successfully
  including after one collision
   and after multiple collisions
frames aborted(excess collisions)
frames received and given to user
       received and dropped            <-- the firmware no-pool-node discard (0x5ECA path)
       missed                          <-- LANCE MISS
CRC errors
alignment errors
FIFO overflows
buffer overflows
bad MA length field
loss of carrier during transmit
tranmsit underflow                     (sic - typo in the ROM string)
late collision
bad length received
bad address received
missing transceiver heart beat
jabber detected
memory error
hung transmit state
restarts
```

(The identification of "received and dropped" with `STAT_rxDroppedNoPoolNode` and "missed" with
the LANCE MISS counter is [I] — inferred from the firmware carve's counter semantics in
`..\COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md` §6b; not yet byte-proven by matching the
stats-reply assembly in firmware.)

**Live verification 2026-08-09 (emulated D100, RetroCore, SINTRAN K):**
`@(UTILITY)ENCOS-MON-II-B01` → `stat` → server `0` → system name EMPTY → source `0` → physical
copy `N` prints exactly this table. After ~8.5 h healthy COSMOS traffic: tx 10336, rx-to-user
10082, dropped 0, missed 0, all error rows 0. Per-connection form: source `N` (=102) with system
name EMPTY → "ETHERNET SERVER STATISTICS to ND-102", tx 5553 rx 5403. GOTCHA: filling BOTH the
system-name prompt and the numeric source gives `BAD RESPONSE STATUS 43e` — leave the name
empty. The monitor has exactly four commands (`stat trac help exit`, 4 letters, no
abbreviation). **This makes the rx-pool starvation question measurable from SINTRAN alone: when
a link dies, run stat and read "received and dropped".**

## 3. The module's only interface gateway: a MON 255B wrapper family [V]

`MON 0xAD` (= MON 255B, the PIOC monitor call) appears exactly 10 times in the whole module, all
in one run of small wrappers at words 0x36c8–0x3772. Shape of each wrapper: marshal params into
a local block, `SAT -1` (or a function value), `LDX <paramblock>`, `MON 0xAD`, `STT <status>`,
map status through the §1 error-text table. These wrappers are the ONLY way this module touches
the interface — consistent with the kernel side (PDRIV/PISTA in `RP-P2-PIOC.NPL` /
`MP-P2-PIOC-DRIV.NPL`) owning the actual IOX register access.

## 4. The request-block builder at 0x52e7–0x5341 — and the trap NOT fallen into

A 4-step decision ladder; each step stores a two-triple descriptor pair (`STF 6,X` / `STF 9,X` —
48-bit T:A:D triples at block offsets 6 and 9), calls two externals (BRF refs to resident
addresses `0061033B` and `0074363B`), and on its outcome selects a code:

```
step 1 (SAA 3, desc ptr pool 0x5300) -> code 0x0B
step 2 (SAA 4, desc LDT -0x15,B)     -> code 0x0A
step 3 (SAA 2, desc LDT -0x12,B)     -> code 0x13
step 4 (SAA 2, desc LDT -0x10,B)     -> code 0x12
common tail 0x532a: X+6 := {T=param1, A=0x95, D=0}; X+9 := {T=param2, A=4, D=0};
                    X+0xC := selected code; two external calls (send); status -> local
```

So the block layout is: **descriptors at +6 and +9 (three words each), request/opcode word at
+0xC** [V]. The code set is **{0x0B, 0x0A, 0x13, 0x12}** [V].

**TRAP AVOIDED — record so it is not re-adopted:** `SAA 0x12` occurs exactly ONCE in the module
(word 0x5328, inside this ladder), and 0x12 is also the card firmware's POST-BUFFER host-command
opcode. The temptation is to declare this "the POST-BUFFER send site". **Do not.** The
surrounding code (0x51f1+) is a keyword PARSER with an uppercase fold and the §1 debug keyword
tables directly above it — this ladder is by context the error/monitor task's REQUEST builder
(plausibly mapping stat/trace choices to MONTR request codes; the trace decoder in this very
module knows MONTR subfunctions "stats / trace enable/disable/read"). Whether these codes are
the same opcode space as the firmware's host-command dispatcher (where 0x12 = POST-BUFFER,
handler 0x6CEE) is **[OPEN]** — the numeric coincidence alone proves nothing.

## 5. What remains OPEN for the actual question (re-post policy)

0. **Wire encoding note [V, from the card side]:** the firmware reads the command opcode as
   `(cmd.byte[0xa] >> 2) & 0x3f` (`ENNS0-RXPOOL-PRODUCER-RE-2026-07-24.md`, host_cmd_dispatch
   0x6aca) — the host stores it SHIFTED LEFT 2, so POST-BUFFER's host-side literal may be `0x48`,
   not `0x12`. Value scans DONE, both dry: `SAA 0x12` = 1 hit (the §4 ladder, monitor-request
   context); `SAA 0x48` = 1 hit (word 0x445b, an ASCII/size constant inside a MON 262B CPUST
   system-info routine — unrelated). The opcode may also be table-driven data, invisible to any
   constant scan. **Value scanning is exhausted; go by behavior.**

1. **Where the receive-buffer POST is issued.** Candidates, in order of likelihood: (a) elsewhere
   in this BRF (the supervisor proper — the parser/ladder region is only ~2% of 61 KB); (b) the
   XMSG kernel's network-server code (ENNS0 runs as an XMSG net server); (c) the resident PIOC
   driver. Next probes: walk CALLERS of each §3 MON-wrapper (findcallers-style, module is small);
   inventory every store of a constant to a block offset +0xC; resolve the two external resident
   addresses `0061033B`/`0074363B` against the K-image resident carve to learn what the ladder
   calls.
2. **The card's trace facility as the direct answer.** The monitor's `trac` command
   (enable/read) + the on-disk `(SYSTEM)ENCOS-TRACE:DATA` + this module's trace DECODER mean the
   PRODUCT ITSELF can log per-message send/receive ("send to ND", "receive from ND", "receive
   stop") — enabling the trace on live D100 during traffic would show the buffer/message cadence
   without any emulator instrumentation. Untried.
3. Matching the firmware's stats-reply builder to §2 to byte-prove the dropped/missed mapping.

## 6. 2026-08-09 second pass — the REAL symbol table recovered [V]

**The BRF symbol names are fully recoverable**: `ENNS0-Startup-RE-2026-07-23\tools\brf_link.py`
(the validated BRF linker) decodes them correctly — 215 symbols, `main = ENNS0`. Saved:
`encos-err-ii-b01-symbols.txt` (this folder, octal word addresses) and the linked image
`encos-err-ii-b01.linked.bin` (30335 words, big-endian, base 0).

**Layout authority: brf_link's image, NOT the Ghidra program.** Validation: 124/215 symbols land
exactly on the NPL prologue `146547 RADD SL,DX` in brf_link's own image; most of the rest are the
`146147`-variant prologue or data cells (`UEERF*` error constants). The Ghidra BRF loader's layout
DRIFTS from this (its garbled labels sit at shifted addresses in the upper region) — **the
renames applied to the Ghidra program on 2026-08-09 are correct in the low region (POSUERR
0x03ee) but MISPLACED above ~0x3600; do not trust them**. Work from
`encos-err-ii-b01.linked.bin` + nd100-dis until the Ghidra loader's layout bug is fixed.

**CORRECTION to §4:** with names, the "request-block builder" at hex 0x52e7 falls inside the
PLANC runtime I/O library (`5UTBY`/`5INBO`, byte output/input) — the codes {0x0B,0x0A,0x13,0x12}
are ASCII control characters (VT, LF, DC3, DC2 — newline/flow-control handling), NOT card
opcodes. §4's "[V] block layout" claim is hereby DOWNGRADED to "PLANC I/O internals, irrelevant
to the driver". Poisoned prior recorded.

**The module structure by symbol [V]:** `POSUERR` @0o1756 (the bulk: error/dump task support),
`ENNS0` main @0o31734, the PIOC interface layer @0o32762-0o33227, the `UE*` error task
@0o33227+, `MONITOR`/`5MON_*` @0o43435+, the PLANC runtime `5*` @0o50000+, per-MON wrappers
`MON0..MN312` @0o70647+.

**The MON 255B (PIOC) service map, byte-read from the wrappers @0o32762-0o33230 [V]:**

| Routine | Octal addr | MON 255B T | Params seen |
|---|---|---|---|
| `READPIO` | 032762 | T=-1 (twice, two-part read) | X=param block; the "physical copy" reader |
| `INT2GET` | 033062 | (no MON itself) | |
| `RES_SLO` | 033102 | **T=0** reserve slot | X |
| `REL_SLO` | 033114 | T=5 | X, A |
| `SEND_KI` | 033126 | **T=7** send kick (doorbell) | X, A, D (A copied to D first) |
| `REC_KIC` | 033142 | **T=3** receive kick | X, A; returns A (D copied out) |
| `SEGLOAD` | 033156 | T=4 | X, A, D |
| `UNLOAD`  | 033172 | T=5 | X only |
| `START_P` | 033203 | **T=6** START PIOC | X, A — matches PISTA in `RP-P2-PIOC.NPL` exactly |
| `STOP_PI` | 033216 | T=7 | X only |

(REL_SLO/UNLOAD sharing T=5 and SEND_KI/STOP_PI sharing T=7 with different register profiles is
as-read; the kernel presumably disambiguates on the params. [I] until the kernel dispatch for
MON 255 is carved.)

**The caller map [V, from the BRF REF chains — brf_link's `refs` dict]:** cross-unit calls go
through loader-patched REF chains, so callers are found from the REF record sites, NOT from
pool-word value scans (a value scan finds only same-unit calls — that burned 20 minutes).
All interface-routine callers sit in ONE region, 0o30233-0o31600 (below `ENNS0` main @0o31734):

```
INT2GET @030233 | RES_SLO @030422 | SEGLOAD @030633 | START_P @030710   <- bring-up, in order
STOP_PI @031101 | UNLOAD @031116                                        <- teardown
REC_KIC @031056 031305 031571 | SEND_KI @031064 031374                  <- the runtime kick pump
READPIO @015752                                                          <- inside POSUERR (dump/stats read)
```

A REF site is the POOL word the loader patches; the actual `JPL I` sits a few words before it
(e.g. the SEND_KI call at 0o31047 `JPL I 15 -> 031064`, params built at 0o31042-0o31046:
X+6 := a value, X+7 := saved A). The teardown path 0o31101-0o31133 ends in `MON 0` LEAVE.

**2026-08-09 iteration 3 — the pump region decoded; REASSESSMENT [V bytes, I on the conclusion].**
0o31136-0o31454 is NOT a buffer pump: it brackets its PIOC-library call sequence with
`MON 124` PRSRV / `MON 125` PRLRS (force-reserve/release of the interface device) and passes
block lengths (SAA 50, SAA 24, SAA 4) matching the stats/trace copy blocks — this is the
DUMP/STATS COLLECTOR ("physical stats copy FAILED" strings immediately precede it). The
supervisor's SEND_KI/REC_KIC uses serve the monitor/dump request path (which the live
ENCOS-MON `stat` exercise §2 confirms working), NOT a steady-state rx-buffer feed.

**2026-08-09 iteration 4 — the posi-magic probe is DEAD; record so nobody re-runs it.**
The card's command ring head carries magic `0xAAAF` (`ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md`).
Scans: XMSG kernel L03 = 0 hits; NPL kernel source = 0; `encos-err-ii` linked image = 1 hit at
word 0o46576 which is the INSTRUCTION `125257 = JMP I -121` (opcode collision — trap #12);
`encos-ser-i-b01.dseg` = 8 hits = the card-side ring heads as INITIAL DATA. Conclusion [V]:
the magic is initialized by the card's data-segment download; the HOST only appends nodes to
the chain at head+4 and never writes the magic — no host-side code can be found by scanning
for it. CORRECTED iteration 5: `5MON_X` (0o46252+) is NOT a marshaller — the "codes 1..5" are print
field WIDTHS, the common target 0o46455 is a computed PRINT dispatch (`JMP ,X ,B`), and its
cases end in `MON 22`/`MON 24` (OutUpTo8Bytes/Out8Bytes, console output). The whole
`5MON_P/F/X` family is the dump task's print/format layer. NOTHING in this BRF touches the
card data path — [V] final for this module: it is start/stop (MON 255B wrappers) + error/dump
+ monitor UI + print/PLANC runtime. The rx-buffer poster is NOT here.

**2026-08-09 iteration 5 — module VERDICT FINAL + the kernel symbol asset.** (a) July's
`ENNS0-Startup-RE-2026-07-23\` folder had ALREADY carved the I-variant supervisor
(`ENNS0-POLL-FINDINGS.md`: zero IOX, the 10 PIOCM wrappers, MAIN @031655; and
`ANNOTATED-ENNS0-DISASSEMBLY-2026-07-23.md`: ENNS0's whole XMSG footprint is TWO MON 200B —
XFDUM + XFDCT) — read that folder FIRST next time; this carve's new contributions over July are
the II-variant MON 255B T-value service map, the statistics record + live ENCOS-MON channel,
and the layout-authority/brf_link method. (b) The rx-buffer poster is NOT in this BRF —
final. (c) NEW ASSET: the XMSG kernel L03 full symbol table decoded (the .SYMB is
parity-mangled — strip bit 7): `xmsg-kernel-l03-symbols-decoded.txt` (this folder, 2096
symbols, octal, sorted). Kernel image spans 0o120000-0o175777 (flatten
`..\..\XMSG-KERNEL-L03.BPUN` with bpun2raw). In-image buffer structures to chase next:
`XQBUF` @120410, `XHBUF` @122742, `X6BUF` @152104. Next: find the kernel's PIOC/netserver
driver code by symbol families around those addresses and by behavior (writes through the
PIOC window + kick), then the buffer-post loop and its stop condition.

**2026-08-09 iteration 6 — the kernel's netserver interface block located [V symbols, I shape].**
From the decoded symbol map: the `XQ*` family at kernel words 0o120377-0o120434 (3-word stride):
`XQSTO`/`XQTBS` (aliases, head), `XQKER`, **`XQKIC`** (kick), **`XQBUF`** (buffer chain),
`XQNTT`/`XQNTR`/`XQNRR` (transmit/receive/receive-ready counts?), `XQIOR` (I/O routine),
`XQLNK`, `XQTI0` — the per-netserver queue-interface descriptor, i.e. the structure a
netserver driver pumps buffers through. Image dump shows each entry as two zero words + a
tag word (template/residue, not live pointers) — the LIVE table is built at runtime; static
next step is finding the CODE that indexes these fields. Also located: the kernel's
privileged-op thunks `REX/PONN/PION/IOXT/EXAM/DEPO` @0o150407-0o150417 — all hardware access
routes through the IOXT thunk, so ITS callers enumerate every device touch in the kernel
(the behavior probe for the PIOC window writer + kick). Assets: kernel image (bpun2raw of
`..\..\XMSG-KERNEL-L03.BPUN`, base 0o120000, spans to 0o175777) + `xmsg-kernel-l03-
symbols-decoded.txt`. NEXT: enumerate `JPL` callers of the IOXT thunk in the kernel image,
classify which write the PIOC/ENNS0 window, and follow the one that appends receive buffers
(`XQBUF`/`XQNRR` users) to its trigger condition — that IS the re-post policy.

**2026-08-09 iteration 7 — the kick chain located end to end [V source + symbols].**
(a) The 1504xx "IOXT thunk" idea was wrong — those symbols land mid-code (table words); no
pool word holds 150415. Probe dead. (b) The REAL chain, from `MP-P2-PIOC-DRIV.NPL`
@077557-077675 (the SUPERKICK drain): PDRIV walks the PIOC→ND-100 superkick ring
(`RPTON`/`RTAIL`/`DEMPT`), and per entry dispatches on `DLEVL` — level 5 → `JPL I (KXMSG`
"KICK XMSG" passing link index + physical ring buffer address; RT levels → `XRTEN`. So
card→host events enter the XMSG kernel at KXMSG. (c) In the XMSG kernel symbol map the kick
layer sits at 0o1224xx: **`KICAR` @122427** (kick-arrival handler), `XHKIC` @122455,
**`XAPKI` @122467** (kick-append — the host→card SEND side), near XCRET/XHANG/XRETC/XSTDR.
NEXT (session-scale): disassemble KICAR and XAPKI in the kernel image — the receive-side
buffer replenish (the POST-RX-BUFFER command build, or whatever LOC-XMSG-level mechanism
stands in for it) should be reachable from KICAR's receive path; XAPKI shows how host→card
nodes are appended + kicked. Then the re-post policy = the condition guarding that append.

**2026-08-09 iteration 9 — ENDIANNESS RULE settled by anchor test + the breakthrough map.**
Rule [V]: **`bpun2raw.py` output is ALREADY LITTLE-endian — feed it to nd100-dis DIRECTLY,
and read it in python as `(data[i+1]<<8)|data[i]`.** Proven at the ZCRMG anchor (0o131055
decodes to the documented magic-number-builder shape only on the unswapped file). This is the
OPPOSITE of the carved-segment `.bin` convention (those are big-endian and need the swap) —
know which producer made your image. One retraction cycle was wasted on getting this backwards
(an intermediate note here claimed XHKIC was data — WRONG; deleted).

With correct LE reads, the netserver map [V]:
- `XHKIC` @122455 IS the kick primitive (iteration 8's original decode stands: IOF critical
  section, node list append, ION), and the **XQ descriptor holds POINTERS**: initial values
  XQSTO→120731, XQKER→122274, **XQKIC→122455=XHKIC**, **XQBUF→122742=XHBUF**, XQNTT→133423,
  XQNTR→134073, **XQNRR→134250**, XQIOR→144771, XQLNK→140566, XQTI0→120711 — per-function
  HANDLER/storage pointers. XQNTT/XQNTR/XQNRR = transmit / receive / receive-ready handlers.
- SCAN-COLLISION WARNING (trap #12, now proven three times): **`0xAAAF` = octal `125257` =
  the instruction `JMP I -121` — it is UNSCANNABLE in ND-100 code** (the 141656 "hit" was that
  instruction; so was the encos-err one). Likewise the XQ cell addresses `1204xx` encode as
  `MPY ,B nn`, so the "XQBUF/XQNRR reference" scan hits (124207/130523/130677/143143/143301)
  are UNRELIABLE — treat as unverified until each is read in context.
- The SOLID ground is the XQ template's POINTER VALUES (read from the data region, no
  collision possible): XQNTT handler @0o133423, XQNTR handler @0o134073, XQNRR handler
  @0o134250, XQIOR @0o144771, XQLNK @0o140566.

**2026-08-09 iteration 10 — the receive pair read, helper primitives NAMED [V].**
`XQNTR` handler @134073: `ION`, save descriptor X, classify node via fields (,X 1/2/25/26/27)
+ BSKP flag tests; paths call (pool-word targets, symbol-named): **`ZQDCH` @122150 = queue
DECHAIN** (the dequeue primitive — also used from the XHKIC kick region), **`ZDREL` @140167 =
RELEASE**, `ZXLRT` @140230 (link to RT), `ZCRAS` @132165 (the XMSG fatal), **`XLEV5` @122105**
(the level-5 entry — matches PDRIV's DLEVL=5 → KXMSG dispatch). One path bumps a counter
(`,B 161`) when node flag 0o20 is set. `XQNRR` @134250: switches B to the node's own
datafield (`RADD CLD SX DB`) and calls locals. The RELEASE path is the buffer-recycling seam —
what follows a ZDREL is where a re-post to the card must happen (or fail to).
STILL UNNAMED: the local subroutines @0o134301-0o134440 (pool targets 134305/134313/134323/
134405/134426/134433) — decode these next; one of them should build the card node + kick.

**2026-08-09 iteration 11 — THE POLICY GUARDS FOUND, and they are CONFIG PARAMETERS [V].**
The XQNRR body (0o134303-0o134447) contains two threshold guards:
1. @134336-134350: node field `,X 31` is a PACKED PAIR of counts (low = `AND` pool mask,
   high = `SHA ZIN SHR 10`), SUMMED, and compared `SKP IF DA LST ST` against a limit loaded
   `LDT I 67` → pool word @134433 → **cell 0o120510 = `X5MXH`** — below the limit, the path
   calls onward (queue/kick more); at/above it, it does not.
2. @134411-134421: `,B 147` (a per-descriptor limit cell) vs `LDT ,X 31` — a second
   greater/less window check gating calls @134442/134443.
**0o120510 sits in the X5* NETSERVER PARAMETER TABLE** (@0o120473-0o120521): X5ROU, X5COM,
X5FRM/X5FSZ (frame size), X5ACK, X5LTO (timeout), **X5MXH** (max outstanding — the guard),
X5NAM, **X5NBF = number of buffers** (@120512), X5TMS, X5RPM, X5MMX, X5TRA... So the
receive-buffer re-post policy is PARAMETERIZED: outstanding counts vs X5MXH, pool size from
X5NBF — i.e. the long-hypothesized "raise the rx-buffer count in the COSMOS definition"
lever is REAL and byte-anchored. These X5* cells are filled at netserver definition/start
(START-NET-SERVER / DEF-NETWORK-CONN parameters or generation defaults).

**2026-08-09 iteration 12 — LIVE VALUES read + iteration-11 guard-1 CORRECTED [V].**
`X-C: List-Generation-Variables` (works on live D100, K disk, Release L kernel; answer the
"XROUT system?" prompt with EMPTY for local) prints the whole X* generation table. Live D100:
- **X4NBF "Default number of receive frames per link" = 5** — THE rx-buffer pool parameter,
  per LINK (so total card rx budget = 5 x active links).
- X4TMS transmit buffers per net server = 2; X4ACK max network ack frames = 15;
  X4RPM max repeats before link STOPPED = 5; X4IRM max SABMs at link start = 10;
  X4LTO default HDLC timeout 10 XTU; X5TO1/X5TO2 datagram rx/tx timeouts 150/200 XTU;
  X4FSZ/X4FSO frame size 312 words; X4MXH **max HOPS = 20**; X5TRB trace buffers = 2.
- CORRECTION to iteration 11: **`X5MXH`/`X4MXH` is MAX HOPS** — guard 1 (sum of packed pair
  in node word ,X 31 vs M[120510]) is a routed-datagram HOP-COUNT check, NOT a buffer credit
  window. The receive-buffer policy parameter is X4NBF; the actual below-limit/refill logic
  still needs its exact site (guard 2 `,B 147` and the descriptor fill at link start where
  X4NBF is consumed — find the code that READS 0o120512).
The parameterized-policy conclusion STANDS (X4NBF drives the pool; no timeout recovery on
posting), with the guard-1 label corrected. Note also X4RPM=5: the link is STOPPED after 5
repeats — a second permanent-death mechanism (matches the observed "link dies for good"
without any storm: 5 failed retransmits = stop, no auto-restart).

**2026-08-09 iteration 13 — X4NBF consumer + SYNTHESIS (what this means for the emulator).**
No pool word in the kernel holds 0o120512 — X4NBF is accessed B-relative/indexed off the
kernel datafield (value scans structurally cannot find it); the consumer is the LINK-START
descriptor build, which copies the default into per-link cells (the `,B 147`-class limits
guard 2 tests). Chasing that exact instruction is diminishing-returns detail; the POLICY
ANSWER is complete:

1. **The card's rx budget is 5 receive frames PER LINK (X4NBF default), copied per-link at
   link start.** A burst of >5 frames on one link, with completions lagging, exhausts the
   window — the zero-latency emulated wire makes this ROUTINE where 10 Mbit spacing made it
   rare. Wire pacing / inter-frame gaps on the emulated segment is a legitimate fix, and
   raising X4NBF (a GENERATION variable — needs an XMSG regeneration, not a runtime command)
   is the config lever.
2. **X4RPM=5: five repeats and the link is STOPPED, permanently — no auto-restart.** A
   SECOND permanent-death mechanism, independent of pool starvation: lost/late ACKs under
   emulated timing → 5 retransmits → stop → the peer storms 0x0F link-open forever (the
   exact 2026-08-03 D102 symptom). Diagnosis: link state via `li-rout` / the 0x0F storm
   signature; distinct from the dropped-counter signature in ENCOS-MON `stat`.
3. **No timeout recovery anywhere in the posting path** — the emulator's duty remains
   exactly-once on every completion/doorbell (RetroCore: MBOXH bit1-only completion +
   EXACTLY-ONCE VIOLATION detector, commit bba258697; MC68901 kick-path audit clean).

**CONCLUSION for the re-post question:** `encos-err-ii` (ENNS0 supervisor) does
start/stop/dump/monitor. The rx-buffer POST (card opcode 0x12) must originate in the **XMSG
kernel's network-server code** — the POSI_SEND → DATASERVIC path (nd-ethernet-ii skill: the
ENNS0 RT is the XMSG net server; request nodes drive the card). NEXT TARGET: the XMSG kernel
carve — `XMSG-KERNEL-L03.BPUN` (flatten with `bpun2raw.py --at 131055`, loaders beside it in
`..\..`), plus the existing XMSG kernel symbol work in this DOC folder. Search there for the
card-command builder (behavior: writes into the PIOC shared window + kicks) rather than for
the 0x12/0x48 constant. Also note the REF-chain caveat: a REF record's recorded CLC is the
chain head, not necessarily the exact call site — localize callers by disassembling around it.

## Cross-links

- Firmware side (card): `..\COSMOS-ETHERNET-LINK-CONTROL-FRAMES-2026-08-03.md` (§2g handlers,
  §6b starvation; another agent's doc — read, do not edit).
- RetroCore measurement instrumentation: `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\
  NDBusEthernetII.cs` PC watches 0x67DA / 0x6CEE / 0x5ECA (commit 7b703df37), and the kernel-side
  MBOXH exactly-once findings in `Emulated.HW\ND\CPU\NDBUS\Xmsg\MboxhTransport.cs` (commit
  bba258697, validated against `..\..\..\NPL-SOURCE\NPL\MP-P2-PIOC-DRIV.NPL`).
- Live environment: `F:\RC\RonnyTest\ETHERNET-SETUP-HANDOFF-2026-08-09.md`.
