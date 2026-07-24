# XROUT XSNET / XRUNN=2 condition decode - the real precondition (2026-07-23)

Static byte-level reverse-engineering of the SINTRAN **XROUT** kernel
(`XMSG-XROUT-L03.BPUN`) to find the EXACT condition under which
`START-NETWORK-SERVER ENNS0` yields XROUT error **XRUNN=2 "Unknown name
(of server or system)"**. This closes the `[OPEN]` left by the prior
`XMSG-COMMAND-START-NETWORK-SERVER-DECODE-2026-07-23.md` (which could not
disassemble the XROUT BPUN for lack of a load base).

Tags: `[V]` = VERIFIED (decoded bytes / read source / manual quote);
`[I]` = INFERRED; `[OPEN]` = not decoded here, exact anchor given.
STRICT no-guessing on XMSG per repo policy.

Version caveat: the running guest banner is **XMSG version M00 (88.03.25)**;
these binaries are **L03**. The prior decode verified the START-NETWORK-SERVER
flow is version-stable L03<->M. The XROUT service structure decoded here is L03;
where an address is L03-specific it is marked.

---

## 0. Headline (the answer)

- `[V]` **XSNET (service 85) NEVER returns XRUNN=2.** Its handler `RSNET`
  @ **0o13753** has a fully-decoded error jump-table (0o14364-0o14407) whose
  only status codes are **6, 16, 17, 18, 27, 31, 33, 36, 39** - never 2.
  So the "Unknown name" the operator sees is **not produced by the XSNET
  start call itself.**
- `[V]` **XRUNN=2 is a name-table MISS.** It is returned by the XROUT
  services that RESOLVE a name through the kernel lookup routine **CHNAM
  @ 0o20161**: `getMagic` (**RSGMG** @0o6200, service XSGMG=71),
  `get-info-about-name` (**RSGIN** @0o6231, XSGIN=82), and the other
  name-consuming services. Each does `JPL CHNAM; JXZ notfound; ... SAA 2;
  JPL XRPLY`. **When the name is not in the XROUT name table, XRUNN=2.**
- `[V]` The name table is populated **only** by the kernel inserter **YNNAM
  @ 0o17710**, which is called by `XSCRS` (create service, 80 / `RSCRS`),
  `XSNAM` (give name to port, 66 / `RSNAM`) and `XSDRN` (define remote name,
  73 / `RSDRN`). All three first require a **local system number** (they
  return **XRNLS=27** "No local system number defined" if it is unset).
- `[V]` **Why the tested DEFINE-REMOTE-NAME fix did nothing:** `DEF-REMOTE,,D100 100`
  DID insert a name via YNNAM - but the literal string it inserted was
  **`D100`**, not **`*XM-ENNS0`**. `getMagic(*XM-ENNS0)` still misses CHNAM ->
  still XRUNN=2. DEFINE-REMOTE-NAME was not the wrong *command family*, it was
  the wrong *name argument* (and semantically it is for remote SYSTEM names, not
  the local `*XM-` server name).
- `[V]/[I]` **The true missing precondition:** the literal name **`*XM-ENNS0`**
  must be created in the XROUT name table via **XSCRS/XSNAM** (which itself
  needs a **local system number defined first**, XSDLO=83 / DEFINE-LOCAL-SYSTEM).
  Neither ENNS0 (proven: only XFDUM+XFDCT) nor the XSNET start handler creates
  it - XSNET *assumes it already exists* (it does a getMagic/lookup, not a
  create). So the creator is an upstream COSMOS network-generation / config step
  that the harness skipped. This is a **config/procedure gap, not an emulator
  or SINTRAN defect.**

---

## 1. BPUN loader (Task 1) - VERIFIED

Added **`tools/bpun_load.py`** implementing the ND-100 absolute binary format
per **ND-06.014.2A section 4.2.5.1** ("Binary Format Load"):

```
A B C ! E F G H I
  A = arbitrary text (here a paper-tape octal bootstrap, high-bit CR = 0x8D)
  C = octal number just before '!'  = program start address
  ! = 0x21 start-of-binary marker
  E = block load address (2 bytes, MSB first)
  F = word count           (2 bytes, MSB first; E,F,H not counted)
  G = F data words         (2 bytes each, MSB first)
  H = checksum = 16-bit arithmetic sum of the G words
  I = action byte (0 => start at C ; nonzero => return to operator)
```

Applied to the two XMSG segment images (both checksums **VERIFIED OK**):

| file | load base | words | span | checksum |
|---|---|---|---|---|
| `XMSG-XROUT-L03.BPUN`  | **0o000000** | 39943 | 0o000000-0o116006 | calc=file=0o000540 OK |
| `XMSG-KERNEL-L03.BPUN` | **0o120000** | 23551 | 0o120000-0o175776 | calc=file=0o005520 OK |

`[V]` **Key structural discovery:** XROUT and the XMSG-KERNEL are linked into
**one shared logical address space** - XROUT at 0, the kernel at 0o120000. That
is why XROUT's indirect-call pointer cells contain kernel addresses
(0o120605 CFMSK, 0o131104 YPFND, 0o132617 ZRALL, ...). The findings below load
BOTH images into one flat `mem[]`. Anchoring is by `XMSG-SYMBOL-L03.SYMB`
(NAME=octal, high-bit ASCII); the `RS*` symbols are the service-handler entry
addresses, `RSNET=0o13753`, `RSGMG=0o6200`, `RSGIN=0o6231`, `CHNAM=0o20161`,
`YNNAM=0o17710`, `XRUNN=2`.

`tools/disbpun.py` (loader + symbol annotator over `nd100dis`) was used for the
disassembly; the ND-100 opcode reads were hand-verified against the `nd100-asm`
reference.

---

## 2. The XROUT service dispatcher (Task 2) - VERIFIED

The service-code -> handler table is at **0o2323 (run 1) + 0o2334 (run 2)**,
indexed as **`RSTAB[0o2223 + service_code]`** (verified against three known
slots: XSCRS=80@0o2343, XSGIN=82@0o2345, XSDLO=83@0o2346, XSNET=85@0o2350).
Relevant entries:

```
0o2343 -> 0o5463  RSCRS  (XSCRS=80  create service)
0o2345 -> 0o6231  RSGIN  (XSGIN=82  get info about name)
0o2346 -> 0o11357 RSDLO  (XSDLO=83  define local system)
0o2350 -> 0o13753 RSNET  (XSNET=85  start/stop gateway)   <== the "start" call
0o2334 -> 0o5620  RSDRN  (XSDRN=73  define remote name)
     RSGMG=0o6200 (XSGMG=71 getMagic), RSNAM=0o5433 (XSNAM=66 give name)
```

---

## 3. XSNET (RSNET) does NOT emit XRUNN=2 - VERIFIED

`RSNET` @0o13753 processing spine (frame register B; the request/letter is
reached via `-115,B`/`-118,B`), resolved call targets in brackets:

```
0o14050  LDD  =0o14012          ; A,D = server-name descriptor from the letter
0o14051  JPL  [CHMAG 0o20204]   ; check the caller's MAGIC number in the request
0o14054  JMP  [-> 0o14370]      ; magic bad  -> SAA 39
0o14055  JPL  [RCHEK 0o20036]   ; request check (calls kernel YPFND 0o131104)
0o14056  JMP  [-> 0o14370]      ; check fail -> SAA 39
0o14060  JPL  [QNFND 0o15141]   ; search the net-server/friend table (50-word entries)
0o14062  JMP  [-> 0o14374]      ; not found -> SAA 16
 ...
0o14126  JPL  [QNSIN 0o14430]   ; success path: Name-Server INSERT (net-server table)
```

Its complete error jump-table (each `SAA n; JPL XRPLY`), decoded byte-for-byte:

```
0o14364 SAA 6  -> XRPLY   XRMMP  Missing mandatory parameter
0o14366 SAA 18 -> XRPLY   XRNXD  Not enough resources (XD/XF/XM) for start-link/netserver
0o14370 SAA 39 -> XRPLY   XRSNR  This server is not running
0o14372 SAA 27 -> XRPLY   XRNLS  No local system number defined
0o14374 SAA 16 -> XRPLY   XRILN  Illegal/Reserved LUN for link
0o14376 SAA 17 -> XRPLY   XRNXL  No more Link Descriptors (XL-blocks)
0o14400 SAA 36 -> XRPLY   XRRFU  Routing table full
0o14402 SAA 10 -> XRPLY   XRPRV  Caller not privileged
0o14404 SAA 31 -> QCRAS   XRNSE  Not a service port
0o14406 SAA 33 -> QCRAS   XRUKS  Unknown remote system name or number
```

`[V]` **There is no `SAA 2` anywhere on the RSNET path.** XSNET cannot return
XRUNN. The two `SAT 2` instructions in RSNET (0o14171, 0o14752) are index/arg
loads (`T:=2`), not status codes. RCHEK (0o20036) and CHMAG (0o20204) were also
fully disassembled - neither emits `SAA 2` (RCHEK exits via the dispatcher
fall-through `FDPLX`; CHMAG is a compare loop ending in `EXIT`).

Note the co-requisite already visible here: XSNET itself returns **XRNLS=27**
(0o14372) if no local system number is defined, and **XRSNR=39** (0o14370) if
the server/magic check fails.

---

## 4. Where XRUNN=2 actually comes from (Task 3) - VERIFIED

### 4a. getMagic (RSGMG, XSGMG=71) - the smoking gun
`RSGMG` @0o6200, byte-verified:

```
0o6200 RSGMG  JPL  [RCPRV 0o20074]   ; check privileged
0o6201        JPL  [CHNAM 0o20161]   ; look up the NAME in the XROUT name table
0o6202        STX  -95,B             ; X = name-entry ptr (0 if not found)
0o6203        JXZ  =>0o6220          ; name NOT FOUND -> error path
0o6204        JPL  [RCHEK 0o20036]
0o6205        JMP  =>0o6220
0o6206        JPL  [QBINC 0o17451]   ; found: build reply carrying the magic number
 ...
0o6217        JPL  [XLPLY 0o2454]    ; success (letter reply)
0o6220 SAA 2                         ; <== XRUNN = 2  (Unknown name)
0o6221 JPL  [XRPLY 0o2452]           ; error reply
```

`getMagic(name)` = XSGMG=71 "Get magic number from name". If `name` is not in
the CHNAM name table, `X==0` -> `SAA 2` -> XRUNN=2. This IS the "Unknown name"
condition, and it is exactly the getMagic handshake flagged in the older
2026-07-07 `ENNS0-XROUT-GETMAGIC` finding.

### 4b. get-info-about-name (RSGIN, XSGIN=82) - same shape
`RSGIN` @0o6231:

```
0o6231 RSGIN  JPL  [CHNAM 0o20161]   ; look up name
0o6233        JXZ  =>0o6257          ; not found ->
 ...
0o6257 SAA 2                         ; XRUNN=2
0o6260 JPL  [XRPLY 0o2452]
```

### 4c. The lookup routine CHNAM (0o20161)
`CHNAM` is the single kernel primitive that searches the XROUT **name table**
and returns `X` = entry pointer or `X=0` on miss. Cells that call it:
`0o5737` (RSNAM area), `0o6223` (RSGMG), `0o6261` (RSGIN), `0o112575` (kernel).
Every XRUNN=2 in XROUT is a CHNAM miss immediately followed by `SAA 2; JPL XRPLY`.

---

## 5. What populates the name table (Task 3 cont.) - VERIFIED

Names enter the CHNAM table **only** through the kernel inserter **YNNAM
@0o17710**. Callers (all byte-verified to `JPL [YNNAM]`):

| service | code | handler | inserts name? | local-sys gate |
|---|---|---|---|---|
| XSNAM "give name to this port" | 66 | RSNAM @0o5433 | YES (0o5445) | XRNLS=27 @0o5452 |
| XSCRS "create service"          | 80 | RSCRS @0o5463 | YES (0o5507) | XRNLS=27 @0o5534 |
| XSDRN "define remote name"      | 73 | RSDRN @0o5620 | YES (0o5673) | XRNLS=27 @0o5706 |

`[V]` So **before any name can be created, a local system number must be defined**
(XSDLO=83 = DEFINE-LOCAL-SYSTEM); otherwise the create returns XRNLS=27.

`[V]` **DEFINE-REMOTE-NAME really does insert** (RSDRN calls YNNAM). The live
test `DEF-REMOTE,,D100 100` therefore added the literal name **`D100`** to the
table - not `*XM-ENNS0`. That is precisely why it did not fix the failure:
`getMagic(*XM-ENNS0)` still misses. (RSDRN also has its own `SAA 2` @0o5716 via a
second CHNAM at 0o5712, and requires privilege (XRPRV=10) + a valid system
number (XRISY=11) + local system (XRNLS=27).)

---

## 6. Reconciliation with ENNS0's "Error in communicating with XROUT" (Task 4)

`[V]` The prior static ENNS0 disassembly proved ENNS0's ENTIRE XMSG footprint is
`XFDUM` (liveness "is XMSG up?") + `XFDCT` (disconnect) - **no XSNAM/XSCRS**.
So ENNS0 never registers `*XM-ENNS0`, by design.

`[I]` ENNS0's own "Error in communicating with XROUT" during startup and the
operator's later "Unknown name" are the **same root fact seen from two sides**:
XROUT is not carrying a `*XM-ENNS0` name entry (and, upstream, likely no local
system number). ENNS0's startup handshake to XROUT (a getMagic/liveness step)
and the command's `getMagic(*XM-ENNS0)` both resolve a name through CHNAM and
both fail because the COSMOS network configuration that creates the local system
number + the `*XM-ENNS0` service was never run. There is **no two-way handshake
defect** in the emulator here - the guest XROUT is answering correctly.

`[OPEN]` The exact XMSG-COMMAND MON 200B that surfaces the 2 (the initial
`getMagic`/`get-info` query the handler issues before/around the XSNET start) is
in the **command program**, not XROUT; decoding it requires the XMSG-COMMAND
`Define-Network-*` / query handlers (out of scope of the XROUT carve). The XROUT
side is now fully pinned: any CHNAM-resolving service on `*XM-ENNS0` returns 2.

---

## 7. Conclusion - the precise missing precondition (Task 5)

`[V]` **XRUNN=2 = a CHNAM name-table miss on the literal name `*XM-ENNS0`.**
It is emitted by name-resolving services (getMagic XSGMG=71, get-info XSGIN=82,
et al.), NOT by the XSNET start (which has no `SAA 2` and would instead fail with
XRNLS=27 / XRSNR=39 if reached).

`[V]` **To make `*XM-ENNS0` resolvable, an entry for that exact string must be
inserted into the XROUT name table via YNNAM** - i.e. a call to **XSCRS**
(create service) or **XSNAM** (give name to port) for `*XM-ENNS0` - and that in
turn requires a **local system number defined first** (XSDLO=83 /
DEFINE-LOCAL-SYSTEM), else the create returns XRNLS=27.

`[I]` **Concrete gap:** neither ENNS0 nor the START-NETWORK-SERVER (XSNET)
handler creates `*XM-ENNS0`. On a real COSMOS system the local-system definition
and the `*XM-<server>` service creation are done by the site network-generation /
XMSG-STARTUP configuration BEFORE `START-NETWORK-SERVER`. The harness ran only
`start-x` + `@RT ENNS0` + `START-NETWORK-SERVER`, so the name was never created.

`[V]` **Not an emulator/SINTRAN defect.** The guest XROUT behaves correctly:
resolve an unregistered name -> XRUNN=2. The corrective action is a
configuration/procedure step:
1. Ensure a **local system number** is defined (DEFINE-LOCAL-SYSTEM / generation).
2. Ensure the network server name **`*XM-ENNS0`** is **created** (XSCRS/XSNAM),
   by running the site COSMOS network-generation / XMSG-STARTUP config that the
   ND-210580 warm-start procedure assumes has already run.
   NB: DEFINE-REMOTE-NAME with the wrong argument (`D100`) does not satisfy this;
   only an entry for the literal `*XM-ENNS0` (or the proper Define-Network-*/
   XSCRS step for it) does.

---

## 8. Evidence index (combined XROUT+KERNEL image; L03 addresses, octal)

| item | addr / value |
|---|---|
| XROUT load base / span | 0o0 / 0o0-0o116006 (checksum OK) |
| KERNEL load base / span | 0o120000 / 0o120000-0o175776 (checksum OK) |
| dispatch table index | `RSTAB[0o2223 + service_code]` (runs at 0o2323, 0o2334) |
| RSNET (XSNET=85) | **0o13753** - error set {6,16,17,18,27,31,33,36,39}, NO `SAA 2` |
| RSNET error jump table | 0o14364-0o14407 |
| RSNET calls | CHMAG 0o20204, RCHEK 0o20036, QNFND 0o15141, QNSIN 0o14430 |
| RSGMG (getMagic XSGMG=71) | **0o6200**; `JPL CHNAM;JXZ;...SAA 2`@**0o6220**->XRPLY |
| RSGIN (get-info XSGIN=82) | 0o6231; `SAA 2`@0o6257->XRPLY |
| CHNAM (name-table lookup) | **0o20161** (X=entry or 0) |
| YNNAM (name-table insert) | **0o17710** |
| RSNAM/RSCRS/RSDRN (insert via YNNAM) | 0o5433 / 0o5463 / 0o5620 |
| local-system gate XRNLS=27 | RSNAM 0o5452, RSCRS 0o5534, RSDRN 0o5706, RSNET 0o14372 |
| XRPLY / XLPLY (reply routines) | 0o2452 / 0o2454 |
| XRUNN | 2 |

## 9. Tools added / used
- `tools/bpun_load.py` - ND-100 BPUN absolute-load loader (format ND-06.014.2A
  4.2.5.1); returns flat `mem[]` + load base + start addr; validates checksums.
  `load_bpun(path, verbose=True)`.
- `tools/disbpun.py` - loads one or more BPUNs + `*.SYMB` symbol table (high-bit
  NAME=octal) and disassembles a range via `nd100dis`, annotating indirect-call
  targets with symbol names. Both binaries left UNMODIFIED (read-only).

**Findings file:** `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\XROUT-XSNET-XRUNN-CONDITION-DECODE-2026-07-23.md`
