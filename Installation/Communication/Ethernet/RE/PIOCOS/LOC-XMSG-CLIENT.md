# LOC-XMSG - the on-card XMSG client library

`LOC-XMSG` is the PLANC module inside the ENCOS firmware that lets the Ethernet II card's PIOCOS tasks
run **XMSG program-to-program conversations** with the ND-100 SINTRAN kernel. The card is a MC68000 and
cannot issue `MON 200` itself, so LOC-XMSG asks the kernel to execute each XMSG call on its behalf by
posting an **activation element** on the MBOXH queue in shared card DRAM, then reading the kernel's
reply back out of that same element's parameter block.

This is the exact mechanism the RetroCore HLE reproduces (`...\NDBUS\Xmsg\XmsgClient` +
`MboxhTransport`). This doc records the protocol from the reverse-engineering corpus so the C# client
mirrors real behavior rather than replaying captured bytes.

Tag convention: **[V]** verified (NPL source / disassembly / symbol file, cited); **[U]** unverified /
inferred; **[E]** empirical (observed in a live HLE/oracle run, mechanism may be unconfirmed).

Provenance of the name: PLANC module headers embedded in the image include `LOC-XMSG` (Apr-Aug 1986),
alongside `NCOM`, `HDLC-DR`, `ASYN-DR`, `MAIN`, `M-MANAG`, `PHLS-GEN`, `RT-CLOCK`, `SHORTLIB`
([V] `../../x/stripped/docs/ND_EthernetII_68000_Firmware_COMPLETE.md` line 52). Its routines carry the
`XMP*` / `PORT*` / `PONA*` symbol names.

---

## 1. The MBOXH activation-queue transport

The authoritative field layout comes from the ND-100 kernel driver that consumes the queue,
`../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-PIOC-DRIV.NPL` (PDRIV/PICXM/DOIT). [V]

### 1.1 Element layout (NPL `MP-P2-PIOC-DRIV.NPL` lines 10-24)

| Field | Type | Byte off | Meaning (verbatim NPL comment) |
|-------|------|----------|--------------------------------|
| `NXMSG` | DOUBLE | +0 | normal XMSG activation queue (link to next element, 0 = end) |
| `NXFNC` | INTEGER | +4 | STATUS: **bit1 set by ND-100 = XMSG FUNC DONE; bit3 set by PIOC = XMSG FUNC REQUESTED** |
| `NXPAR` | DOUBLE | +6 | parameter pointer (byte-ptr to the 6-word param block) |
| `NXXTB` | INTEGER | +10 | XT block - **given by N100 XMSG** (0 = "virgin", see 1.3) |
| `NXLB` | INTEGER | +12 | last local bank for this task (updated by the kernel driver) |
| `NXPNU` | INTEGER | +14 | process/slot number |

A second overlay at the same base is the **RT activation queue**: `NXRTW` (DOUBLE link) + `NXRTF`
(bit0 set by PIOC = RTWAK requested; bit2 set by ND-100 = RTWAK completed). [V]

### 1.2 Parameter block - the "PIOC 6-word format" (NPL lines 268-273, 336-339)

`NXPAR` points at 6 words the kernel loads as three DOUBLEs: [V]

| Words | Kernel name | Request meaning | On reply (see 4) |
|-------|-------------|-----------------|------------------|
| P0 (+0, +2) | `PIPAT`/`PIPAX` | **w0 = func (T)**, w1 = A | **w0 = ISTAT (T)**, w1 = A |
| P2 (+4, +6) | `PIXTA` | w2 = D, w3 = X | w2 = D, w3 = X |
| P4 (+8, +10) | user addr | w4/w5 = user/msg-buffer byte address | (unchanged) |

The kernel's multicall converter names this explicitly: "CONVERT PARAMETER BLOCKS FROM PIOC 6 WORD
FORMAT TO NORD 4 WORD FORMAT" (`XMMC`, NPL lines 336-339). [V]

### 1.3 The "virgin" element (NXXTB = 0) -> kernel issues XFDBK + XFWDF

`DOIT` (NPL lines 426-439): if the slot has no XT block (`L = 0`, virgin), the kernel: [V]
1. marks `PXT(PICPN) = -1` (getting a new XT block),
2. issues `XFDBK` (35, "define bank no", drivers only) with `A := PIOCA` (the PIOC bank base),
3. issues `XFWDF` (34, "define wake-up context", drivers only) registering `PIWKF` as the wake routine,
4. writes the **kernel-returned XT block back into `NXXTB`**.

Consequence for the C# client: the first call is virgin (NXXTB = 0); the "established" NXXTB value on
later calls is the **kernel-assigned XT handle** - the client should **read it back from `NXXTB` after
the virgin call**, not hard-code it. (Our capture happened to show `0x1003`.) [V]

### 1.4 Doorbells

- **68K -> ND-100 (SCIP, INT12), two cells:** [V]
  - `0xEF0080` = monitor/OPCOM doorbell (`post_and_signal_nd100_scip` @ `0x1A48`).
  - `0xEF0180` = XMSG/superkick mirror doorbell (`clr.w (0xEF0180)` @ `0xECF8`).
- **ND-100 -> 68K (kick):** kernel rings `PWCR` bit `BNDC` via `IOXT HDEV+3` (NPL `SPARK` line 476,
  `PIWKF` line 241). On the card this arrives as **MC68901 MFP GPIP-I6 -> vector 0x4E ->
  `nd_host_interrupt_handler` @ `0x250E`**, a scanner over 8 flag words at `nd_channel_flags 0x0B56`.
  The separate OPCOM/START doorbell (`PWCR = 11B`) arrives as 68K IPL-6 autovector `0x1E` @ `0x78 ->
  0x1B00`. [V] (`ND_EthernetII_68000_Firmware_COMPLETE.md`, `...\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\FIRST-SUPERKICK-BRIDGE-DECODE-2026-07-23.md`)

---

## 2. Call cycle (one XMSG function)

1. LOC-XMSG writes the 6-word param block at `NXPAR` (func in P0.w0, A/D/X, uaddr in P4).
2. Fills the element (`NXPAR`, `NXPNU` slot, `NXXTB` = 0 on the first call), links it onto the MBOXH
   head, sets `NXFNC` bit3 (requested), and rings SCIP. [V transport / I exact 68K box-write bytes]
3. Kernel `PDRIV -> PICXM (MON 2XMSG)` executes the call, then `PISAC` writes the reply back (section 4),
   sets `NXFNC` bit1 (done), and rings `PWCR.BNDC` to wake the card. [V]
4. LOC-XMSG (woken) reaps the element and reads ISTAT + A/D/X from the param block. [V]

Only one element/slot is in flight at a time in the classic bring-up (single reused element). [V]

---

## 3. Function codes, XROUT services + errors (XMSG-VALUES-M.SYMB) [V]

Functions (decimal): `XFDUM=0`, `XFDCT=1` (disconnect), `XFGET=2` (get msg space), `XFREL=3` (release),
`XFREA=6` (read msg->user), `XFWRI=7` (write user->msg), `XFMST=9` (**get message status**), `XFOPN=10`
(open port), `XFSND=12` (send to port; port 0 = XROUT), `XFRCV=13` (receive), `XFPRV=30` (request
privilege), `XFWDF=34` (define wake-up, drivers only), `XFDBK=35` (define bank, drivers only), `XFSMC=36`
(**start multi-function call / multicall**), `XFRRE=41` (receive-and-read), `XFWRT=43` (write-and-return).

The func word (T) = function code in the low 6 bits ORed with option flags in the high bits (e.g.
`0x800D` = XFRCV + wake-on-status; `0x040C` = XFSND + route). [V]

XROUT services: `XSGMG=71` getMagic, `XSNET=85` start/stop gateway, `XSCRS=80` create service,
`XSNAM=66` give name to port, `XSDRN=73` define remote name, `XSGIN=82` get info, `XSDLO=83` define local
system; trace: `XSTIN=77` init tracing, `XSTCL=78` close, `XSTDC=79` define conditions. [V]

XROUT errors: `XRUNN=2` "Unknown name", `XRNTR=19` "No trace generated", **`XRTRA=20` "Trace already
active"**, `XRTRP=21` "Trace passive", `XRNLS=27` "No local system number defined", `XRSNR=39` "server
not running". [V]

---

## 4. Where the reply lands (the critical fact for an adaptive client) [V]

The kernel writes the returned registers **back into the PIOC parameter block in place** - NOT a separate
return area. `PISAC` ("SAVE CURRENT XMSG CONTEXT TO PIOC ... PARAMETER BLOCK", NPL lines 280-290):

```
T:=PIPAT; X:=PIPAX            % param address (from NXPAR)
AD:=PIXTA; *P0@3 STDTX        % SAVE T-REG & A-REG  -> P0   (T = ISTAT, then A)
AD:=PIXDX; *P2@3 STDTX        % SAVE D-REG & X-REG  -> P2
A:=PIXLB;  *NXLB@3 STATX      % save last local bank
```

Then `SPARK` (NPL lines 463-478): `*NXFNC@3 ... A BONE 1 ... STATX` (set NXFNC bit1 = done) and
`A:=PWCR BONE BNDC; T:=HDEV+3; *IOXT` (ring the card). On XMSG error (`T<0`) the kernel also invalidates
the bank (`-1=:PIXLB`) and, for a crashed XMSG (`XEIXT`/`XENRU`), zeroes `NXXTB`/`PXT`. [V]

**So the reply the client reads is: ISTAT = param word P0.w0 (byte `NXPAR+0`), A = `NXPAR+2`,
D = `NXPAR+4`, X = `NXPAR+6`.** ISTAT: positive/zero = success, negative = XE* error; for calls sent to
XROUT the status byte is overwritten with the `XR*` return (0 = OK, else e.g. `XRUNN`/`XRTRA`)
([V] `../../../../../SINTRAN/XMSG/DOC/XMSG-API.md` sec 1 + sec 4.1). This confirms the RetroCore
`MboxhTransport` reply-read (offsets P0/P2) is correct.

---

## 5. The PIOCOS coroutine model (concurrency shape) [V]

Decoded in `...\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\FIRST-SUPERKICK-BRIDGE-DECODE-2026-07-23.md`:

- **Scheduler main loop @ `0x2CB6`.** List-head table `0x0B06` = 16 longword priority heads. Per node,
  status byte at slot+23: a node RUNS when **bit1 is CLEAR** (`btst #1,(23,A1)` @ `0x2CD4`; blocked =
  bit set). Next-link at slot+40. Nothing runnable -> `STOP #2000` @ `0x2CEA` (wait for interrupt).
- **Dispatch @ `0x2CF0`:** stores current node ptr to `0x650`, loads SP from `(108,A1)`,
  `movem.l (48,A1)` restores registers, resumes.
- **Yield idiom:** `jmp (A5)` (e.g. `0x1C36`, `0x3B40`); A6 = coroutine activation frame. Execution
  resumes at the instruction after the `jmp (A5)`.
- **Make-runnable (unblock):** only two `bclr #1,(23,An)` sites in the whole image - `0x2292`
  (timer/deferred sweep over heads `0x4C2`/`0x4C6`) and `0x259A` (inbound message-type dispatcher
  `0x2562`, which indexes control-block table `0x0A8A[code]`, ORs arrived bits, and if enabled clears
  the block bit -> `jsr 0x2192` reschedule). **There is no `bset #1,(23,An)` anywhere** (block polarity
  is: set = blocked, cleared to run).

This is why the C# runtime, IF built, maps cleanly onto `async`/`await`: `jmp (A5)` yield == `await`;
the block bit == an incomplete `Task`; the unblock sites == completing it. (See the deferred runtime
plan in `RetroCore ...\EthernetII\ETHII-68K-RTOS-RE-PLAN.md` section 2b.)

---

## 6. Card -> host RT wake: the "superkick" ring (for completeness) [V]

Separate from MBOXH: when the message layer has a completion to deliver to an ND-100 RT program (e.g.
ENNS0), LOC-XMSG's producer `0xEAA6` writes a ring entry and rings `0xEF0180` (INT12). Ring header at
`SUKOF = 1012B` (68K byte `0x414`), magic `0x5555AAAA` (written once @ `0x7C60..0x7C74`), entry array
+22, bit31 = occupied, head index +18. The ND-100 consumer is `PISUPER` (NPL lines 32-141): it dequeues
the PIOC->ND100 ring (`RPTON`), and per entry routes by level - level != 5 -> RT-wake `RTPR` (schedules
the RT program via `XRTEN`), level == 5 -> XMSG kick `KXMS`. PDRIV calls `PISUPER` on each level-12
interrupt. [V] The firmware header geometry matches PISUPER byte-for-byte.

---

## 7. `*XM-ENNS0` registration - RESOLVED [V]

> **RESOLVED 2026-07-26 (Ghidra decode).** The card does NOT create the global XROUT name. It builds
> "*XM-ENNS0" in a RAM buffer (name string @ `0x2D282`, builder coroutine @ `0xCCE4`, RAM buffer
> @ `0x1E210`, appends its node id with `add.b`) and registers it **locally on the card** via
> `XMSGIOCGAT @0xBD32` (`move.w #0x19,D0; TRAP #2` = PIOCOS supervisor fn 25) - **not** an XROUT wire
> request. A whole-image sweep found ZERO XROUT create service bytes (XSNAM 0x42 / XSCRS 0x50 /
> XSDRN 0x49) and ZERO resolve bytes (XSGMG 0x47 / XSGIN 0x52) in any code send path, and no
> `FF 09`-prefixed wire descriptor. The card's XMSG sends (`XMPFSND @0x10AE6`, `XMPROUT @0x10666`) carry
> no service byte - it is application data a caller supplies, and no caller supplies a create byte for
> this name. `PORTNAME @0xE8F4` / `PONAREGIST @0xED10` touch only the LOCAL PIOCOS port directory. [V]
>
> **So the corpus is right:** the global `*XM-ENNS0` is created **host-side** by SINTRAN XMSG in
> response to the card's registration, not by the card. Our HLE result reconciles: replaying the card's
> registration conversation drove the *host* to create the name, which is why "Unknown name" cleared.
> The correlation was real but indirect - the create service byte is emitted host-side (outside this 68K
> image, so not byte-verifiable here). [V card side / I host trigger]
>
> Key addresses: name `0x2D282`; builder `0xCCE4` (trampoline `0xCCC4`); RAM buf `0x1E210`; copy helper
> `0x13286`; local registration `XMSGIOCGAT 0xBD32` (`D0=0x19, TRAP #2`); relay `0xBD94`.

### Original tension (kept for context) [V corpus vs E replay]

The corpus is clear that the **name is created host-side, not by the card**: [V]
- ENNS0's entire verified XMSG footprint is `XFDUM` + `XFDCT` + driver-port setup
  `XFOPN`/`XFWDF`/`XFDBK` - it issues **no** `XSNAM`/`XSCRS`
  (`...\ENNS0-Startup-RE-2026-07-23\ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md` sec 1c).
- `START-NETWORK-SERVER` (handler `0o50722`) builds an XROUT letter for `*XM-ENNS0` and loads
  `XSNET=85` (start gateway) - it **resolves/starts an already-defined name; it does not create it**
  (`...\XMSG-COMMAND-START-NETWORK-SERVER-DECODE-2026-07-23.md` sec 3).
- The XROUT name table is populated only by kernel inserter `YNNAM @0o17710` via `XSNAM/XSCRS/XSDRN`,
  each gated by a defined local system number (`XRNLS=27` otherwise, needs `XSDLO=83` first). Per the
  ND-210580 recipe, the site **XMSG-STARTUP** mode file does the `*XM-<server>` creation +
  local-system definition BEFORE `START-NETWORK-SERVER`. A `getMagic(*XM-ENNS0)` miss returns
  `XRUNN=2` "Unknown name". [V]

**Tension with our HLE result [E]:** in the RetroCore HLE, "Unknown name" disappeared only after the
card's replayed burst-1 conversation (which included `XFWRI "*XM-ENNS0"` + `XFSND` to port 0). The
corpus says the card does not create the name. These are not yet reconciled. Possible resolutions to
test (do not assert until pinned): (a) the disk image's XMSG-STARTUP already defines `*XM-ENNS0` and the
real gate was card presence/`XSNET`, not name creation; (b) the burst-1 `XFSND`-to-port-0 does something
other than name creation and the correlation is incidental. RESOLVE by tracing the XROUT service byte on
the burst-1 `XFSND` and checking the image's XMSG-STARTUP file. [OPEN]

**Concrete Ghidra targets (from the symbol table, section 8a):** the card's local postbox names are
registered by `PONAREGIST @0xED10` (PIOCOS port directory) - this is NOT the XROUT global name. The
global-name traffic goes through `PROCESSXRO @0xCD4A` (process XROUT) and the gateway `XGATEVIAPO
@0x1E16C` / `XMSGIOCGAT @0xBD32`. Decode those to see whether the card ever asks XROUT to CREATE
`*XM-ENNS0` (XSNAM/XSCRS) or only to resolve/route it - that settles the tension directly.

---

## 8. The trace / XRTRA failure - RESOLVED at the RE level [V]

**Observed [E]:** the HLE fixed-replay run produced, on the console,
`XMSG Routing/Naming error: Trace already active` (XROUT `XRTRA=20`); the working oracle reached
"ENNS0 started, sysid 9800" with no such error.

**Decoded [V] (Ghidra, 2026-07-26):** `PROCESSXRO @0xCD4A` is a coroutine trampoline (sets up an RT
context, `jsr 0xbe84`, `jmp (A5)` back to the scheduler) - it builds/sends no XROUT request itself. The
decisive facts from an exhaustive decode + constant search:

- **The card firmware is the XROUT trace SERVER - it PRODUCES XRTRA, it never consumes it.** The only
  occurrences of XRTRA/XRTRP/XRNTR in the whole image are *stores into a reply status field*, e.g.
  `move.w #0x14,(0x14,A0)` @ `0x721C` (a coroutine that emits "XRTRA / trace already active"), sibling
  `move.w #0x15,(0x14,A0)` @ `0x727C` (XRTRP), and a second pair @ `0x748D`/`0x74E5`.
- The card owns the **active-trace list** (head global `0x1A2BC`, mutated at `0x9058`/`0xA224`/`0xA92A`
  via `jsr 0x134E6`) and the capture buffer **`POCSTRACEB @0x2AB5E`** (read at `0xB344`, guarded by
  `tst.l (0x1A2BC); beq skip` = "no active trace -> skip capture").
- **Exhaustive search: ZERO `cmp`/`cmpi` against 0x4D/0x4F (XSTIN/XSTDC as a *sent* service) and ZERO
  against 0x14/0x13/0x15 (XRTRA/XRNTR/XRTRP as a *returned* status).** So the card never sends a
  trace-arm and never reads an XRTRA reply. **There is NO card-side "tolerate XRTRA" branch to mirror.**

**Correction to this doc's earlier framing:** the notion (implied in section 5) that the real card reads
each XROUT reply and branches on the trace state is WRONG *for the trace specifically*. The card reads
replies for ordinary results (XFOPN port, XFGET buffer), but trace is something done TO the card by an
XROUT client, not something the card requests.

**Mechanism [V/I]:** the trace-init is issued exactly ONCE by the ND-100 XROUT client (XMSG-COMMAND /
XROUT), on an empty card trace-list -> clean arm, no error. Byte-for-byte replay re-drives the arm ->
the list `0x1A2BC` already holds the trace -> the card's server coroutine returns `XRTRA=20`.

**Fix (client/replay-side, unambiguous):** arm the trace **once**, or **treat an `XRTRA` reply as benign
(already armed) and continue** rather than aborting. There is nothing to reproduce from firmware - it is
the error's *source*, not its handler.

**Residual (live-trace, not firmware RE):** pin exactly which party+call in our HLE flow re-drives the
arm - the ND-100 XMSG-COMMAND `START-NETWORK-SERVER` path (does it send `XSTIN`/`XSTDC`?) vs a burst-2
step our replay re-issues. Capture the MON 200B service byte on each XROUT `XFSND` in a live run. This is
an implementation/live-trace question, so it is deferred with the rest of the wiring.

Firmware anchors: XRTRA emitter coroutine `0x720C` (`move.w #0x14,(0x14,A0)` @ `0x721C`); XRTRP `0x7266`
(@ `0x727C`); sibling pair `0x748D`/`0x74E5`; active-trace list head `0x1A2BC`
(insert/remove @ `0x9058`/`0xA224`/`0xA92A` via `jsr 0x134E6`); capture path @ `0xB344` reading
`POCSTRACEB 0x2AB5E`; service dispatcher @ `0xA434`; `PROCESSXRO` trampoline @ `0xCD4A`.

---

## 8a. Vendor routine map (from the embedded symbol table) [V]

The firmware carries its own linker symbol table (241 names, file offset 0x663E0-0x689FF, extracted in
[../../x/stripped/docs/ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md](../../x/stripped/docs/ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md)).
These are Norsk Data's own names - authoritative. The LOC-XMSG client is the `XMP*` family.

**Per-function XMSG wrappers (`XMPF*`) - the client's public API, one routine per MON-200 function:**

| Func | Routine | Addr | Func | Routine | Addr |
|------|---------|------|------|---------|------|
| XFDUM(0) | XMPFDUM | 0x1106E | XFSND(12) | XMPFSND | 0x10AE6 |
| XFDCT(1) | XMPFDCT | 0x110BC | XFRCV(13) | XMPFRCV | 0x10BA6 |
| XFGET(2) | XMPFGET | 0x10820 | XFRRE(41) | XMPFRRE | 0x10C4C |
| XFREL(3) | XMPFREL | 0x10880 | XFMST(9)  | XMPFMST | 0x10E32 |
| XFREA(6) | XMPFREA | 0x10936 | multicall | XMPFSCM | 0x10EAA |
| XFWRI(7) | XMPFWRI | 0x109AA | XFPRV(30) | XMPFPRV | 0x10F64 |
| XFOPN(10)| XMPFOPN | 0x10772 | XFCLS     | XMPFCLS | 0x107CA |

Also XMPFGST 0x108D6, XMPFRHD 0x10A1E, XMPFWHD 0x10A86, XMPFRTN 0x10B42, XMPFRRH 0x10D06,
XMPFPST 0x10DAC, XMPFP2M 0x10F08, XMPFDMM 0x10FBA, XMPFALM 0x11010. Higher-level wrappers: XMPOPNM
(open-by-name) 0xFCC4, XMPCLNM 0xFF64, XMPOPCN 0xFF94, XMPROUT (route) 0x10666, XMPSEND 0x106F0,
XMPREAD 0x10478, XMPWRTE 0x1050C, XMPRDHD/XMPWRHD, XMPINFC 0x10302.

**Confirmation:** there is **no `XMPFDBK`/`XMPFWDF`** in the table - independently corroborating that
XFDBK/XFWDF are kernel-issued on the virgin element (section 1.3), never called by the card. [V]

**Reply/message processors (where XROUT-reply branches live):** `PROCESSXRO` (process XROUT) 0xCD4A,
`PROCESSXGA` (process XGATE) 0xD1FC, `PROCESSXMS` (process XMSG) 0xD4C0; gateway `XGATE` 0x1E224,
`XGATEVIAPO` 0x1E16C, `XMSGIOCGAT` 0xBD32; COSMOS-server `POCSPROCES` 0xE380.

**PIOCOS local port system (distinct from XROUT global names):** `PORTCREATE` 0xE73C, `PORTNAME` 0xE8F4,
`PORTCONNEC` 0xE940, `PORTRECEIV` 0xE994, `PORTSEND` 0xEAA6 (= the superkick producer, section 6),
`PONAREGIST` (postbox name register) 0xED10, `PONALOOKUP` 0xEE48, `POMSGETMES` 0xEF68.

**PIOCOS kernel (for the deferred runtime plan):** `PIOCOS` 0x1222E, scheduler `POSP*`/`POSI*`
(start/stop/append/remove/getnext), monitor `POMNPROCES` 0x7BA2 / `POMNREPORT` 0x11F78, locks
`POLKLOCK` 0x12168 / `POLKUNLOCK` 0x12212, `POWAITFORL` 0xE6B0, `PIUSERMAIN`/`AUTO_START` 0x7E0E.

## 8b. ENNS0 server-response ALGORITHM (for dynamic reimplementation) [V]

Decoded from `POCSPROCES @0xE380` (server coroutine) + the LOC-XMSG helpers (2026-07-26). This is the
logic to reimplement in C# - **no captured bytes, no hardcoded handles**.

- **Server loop `POCSPROCES @0xE380`**: PLANC coroutine (frame `A6=0x1D290`, dispatch `A5=0x135A8`).
  Inits subsystems, then a main event-dispatch loop @ `0xE52E`: reads pending flags `0x1E1CA`, else
  waits for an event (`jsr 0x1222E`, state `0xA`), bit-decodes it, and routes to sub-handlers. The XMSG
  request arrives as an event; the conversation is run by the wrappers below.
- **Handle provenance (the crux):** the message handle is **BORN as the return of `XFRRE`**
  (`XMPFRRE @0x10C4C`): after `trap #2` the kernel writes back status + **handle** (card-internal result
  words `(0x4c)/(0x4e)` -> stored `(0x2c,A6)`). `XMPFMST @0x10E32` and `XMPFSCM @0x10EAA` take that
  handle as an **input** argument. **So call XFRRE and use its returned handle - never replay a value.**
- **Identity / "sysid":** the constant `0x2648` (9800) does **not** exist in the firmware. The reply
  carries the runtime value in global `0x1E21A` (written @ `0xBDD2` from an earlier XMSG identity call
  `XMSGIOCGAT`; read @ `0xC1A6` into the reply's type-0xc descriptor). Reimplement as: echo the identity
  the XMSG layer hands us (low16 of the connection identity), do not hardcode 9800. [V write/read; I which
  printed field]
- **Reply build `maybe_build_xrout_message @0xBFF8`:** assembles a descriptor array (each entry 12 bytes)
  then sends via multicall header `(0x8a)=0x24 fn`, `(0x90)=count`, `(0x92)=&array`, `d0=0x19`,
  `trap #2`.

  **CORRECTED 2026-08-10 from a full disassembly of the builder** - the earlier list below was
  incomplete and mis-named one type. It read "type 6 `{len, subcode, ptr}`, type 7 `{handle, 0,
  ptr}`, type 7 `{r6, 0xffff, ptr}`, type 0xc `{big, identity}`", which **missed descriptor type 8
  altogether** and put the handle in a type 7. The actual set, in build order, each emitted only
  when its guard passes:

  | # | word 0 | emitted when | +2 | +4 | +6 | +8 |
  |---|--------|--------------|----|----|----|----|
  | 1 | `8` = `XFSCM` | `$16(a6) != 0xFFFFFFFF` | low16 `$16(a6)` = handle/MESAD | `low16(*0x1E21A)` | - | - |
  | 2 | `6` = `XFREA` | `$20(a6) > 0` | - | `$20(a6)` len | `$1a(a6)` sub-code | long `$1c(a6)` ptr |
  | 3 | `7` = `XFWRI` | `$26(a6) > 0` | - | `$26(a6)` len | `0` | long `$22(a6)` ptr |
  | 4 | `7` = `XFWRI` | `$2c(a6) > 0` | - | `$2c(a6)` len | `0xFFFF` | long `$28(a6)` ptr |
  | 5 | `0x020C` = `XFSND\|XFSEC` | `$2e(a6) > 0` | long `$2e(a6)` = destination magic | - | `low16(*0x1E21A)` | - |

  Type **8 is `XFSCM`** ("set current message", `XMSG-VALUES-M.SYMB` line 24), NOT `XFMST` (=9).
  Descriptor 5 is built as `move.w #$c,(a4)` then `bset #9`; bit 9 is `XFSEC` (line 91), which is
  why the observed send is `0x020C`. The identity is stamped TWICE - `0xC0AA/0xC0B0` into the
  XFSCM descriptor and `0xC1A6/0xC1AC` into the XFSND descriptor. [V]

- **A SECOND identity global exists:** `0x1E21E`, written at `0xBE1E`
  (`move.l $22(a0), $1e21e.l`) in the continuation of `PORTCREATE` (`$e73c`), alongside the
  `0x1E21A` written at `0xBDD2` from the `XMSGIOCGAT` continuation. [V]

- **No record-type table, and no record-type constants.** `POCSPROCES @0xE52E` routes on an event
  BITMASK read from `0x1E1CA` (`and.l #$7f` to one handler, `and.l #$ff0000` to the next), not on
  a message's record type. The COSMOS TLV record types and tags do not exist as immediates in the
  image at all (`0x0149`, `0x1102`, `0x2753`: zero hits; `0x054A`: two hits, both at odd addresses
  so neither is an aligned instruction word). `0xBFF8` is called by `bsr.w` from ten distinct
  sites - `0xCDC2, 0xD0A8, 0xD1EE, 0xD290, 0xD46C, 0xD4B2, 0xDD2C, 0xDD82, 0xDFB0, 0xE006` - across
  `PROCESSXRO`/`PROCESSXGA`/`PROCESSXMS`. So a reply is the RECEIVED message with a branch's
  XFWRI edits applied, not a record composed from a template. [V]

Pseudocode (handle sources explicit; descriptor list CORRECTED 2026-08-10 to match the
disassembly - the old line put the handle in a type 7 and omitted type 8):
```
on_enns0_request():
    port = identity(0x1E21A)                         # runtime, from XMSGIOCGAT return
    (st, HANDLE, r6, r7) = XFRRE(flags=0x4000, port, recvbuf, hdrlen=4)   # HANDLE is BORN here
    desc = [ (8,    HANDLE, low16(identity)),        # XFSCM - omitted when HANDLE == 0xFFFFFFFF
             (6,    readlen,  subcode, readptr),     # XFREA
             (7,    writelen, 0,       writeptr),    # XFWRI - first edit
             (7,    writelen2, 0xffff, writeptr2),   # XFWRI - second edit
             (0x20c, destination_magic, low16(identity)) ]   # XFSND | XFSEC
    multicall(fn=0x24, count=len(desc), array=desc)  # d0=0x19, trap #2
```

MBOXH mapping note [manual-corrected 2026-07-26, verify live]: via the MBOXH 6-word reply the kernel
returns ISTAT(P0.w0), A(P0.w1), D(P2.w0), X(P2.w1). The COSMOS Programmer Guide (ND-60.164.3 sec
3.2.15 XFRRE / 3.2.13 XFRCV) says the RETURN registers are **T=METYP, A=RPORT, D=MESAD, X=NBYTES**.
=> the message **HANDLE = MESAD = D**, NOT A. (Our earlier "handle in A" hypothesis was wrong; the
capture's XFRRE A=0x22FE is RPORT, the hashed remote magic, and we never logged D.) XFMST then takes
**A=MESAD as INPUT** (sec 3.2.17). So the dynamic chain is: **XFRRE.D (MESAD) -> XFMST.A / XFSMC handle**.
The reply-build multicall "descriptor types" 6/7/0xC are XmsgFunction SUB-CALLS (6=XFREA, 7=XFWRI,
12=XFSND) - i.e. write the reply fields then send. Confirm the D=handle mapping by logging the full
XFRRE reply (A/D/X) on a live run before relying on it.

## 9. What this settles for the C# client

- Reply-read offsets (P0/P2) are **verified correct** - the `MboxhTransport` scaffold matches PISAC. [V]
- The virgin/XFDBK+XFWDF behavior is **verified**; the client must read the kernel-assigned `NXXTB` back
  rather than hard-code it. [V]
- The bring-up can be modeled as a linear async sequence over `XmsgClient`; the coroutine/queue
  concurrency (section 5) is only needed if inbound delivery must interleave with an outstanding call
  (the promotion trigger for the deferred PIOCOS-lite runtime). [V basis]
- The XRTRA cause is **RESOLVED at the RE level** (section 8): the card is the trace *server*/producer,
  there is no card-side tolerance branch, and the fix is client-side (arm once / treat XRTRA as benign).
  A residual live-trace step remains to pin which call re-drives the arm, but no more firmware RE is
  needed for it.
- The `*XM-ENNS0` registration is **RESOLVED** (section 7): the card registers the name only LOCALLY
  (`XMSGIOCGAT`, TRAP #2 fn 25); the global XROUT name is created host-side by SINTRAN XMSG in response.
- **Both bring-up questions are now RE-settled.** The remaining path to "ENNS0 started" is implementation:
  the adaptive `XmsgClient` bring-up with **arm-the-trace-once / tolerate `XRTRA`** (section 8), driven
  over the verified `MboxhTransport` reply contract (section 4).

## Sources
- `../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-PIOC-DRIV.NPL` (PDRIV/PICXM/DOIT/PISAC/SPARK/PISUPER)
- `../../../../../SINTRAN/XMSG/XMSG-VALUES-M.SYMB`, `../../../../../SINTRAN/XMSG/DOC/XMSG-API.md`
- `../../x/stripped/docs/ND_EthernetII_68000_Firmware_COMPLETE.md`, `..._QuickMap.md`, `..._ReverseEngineering.md`
- `../../../../../SINTRAN/XMSG/DOC/COSMOS-RE/ENNS0-Startup-RE-2026-07-23/` (FIRST-SUPERKICK-BRIDGE-DECODE,
  XMSG-COMMAND-START-NETWORK-SERVER-DECODE, ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE, XROUT-XSNET-XRUNN-CONDITION-DECODE)
- `../../../../../SINTRAN/XMSG/DOC/COSMOS-RE/ENNS0-RX-FORWARD-ROOTCAUSE-2026-07-24.md`,
  `ETHII-HOST-PROTOCOL-SPEC-2026-07-25.md`
