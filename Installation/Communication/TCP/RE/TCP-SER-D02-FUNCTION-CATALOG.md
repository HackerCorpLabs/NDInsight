# TCP-SER-B0-D02 — function catalog and renaming plan

> **SUPERSEDED SNAPSHOT — read `TCP-SER-D02-CALL-TREE.md` instead.**
>
> This catalog was written on 2026-08-19 **before the naming work**, as the plan for
> it. Its companion `.csv` still classifies **724 of 1002** entry points as `unnamed`
> and 58 as `named`; in the finished database every routine carries a name. A few of
> the names it does quote were later corrected (`AIP_ArpOutOfFragments` is now
> `ARP_SendPacket`).
>
> It is kept because the per-entry structural columns (size, layer, caller/callee
> counts, vector and trap flags, strings) are still accurate and are not reproduced
> anywhere else. Treat the NAMES here as historical.

**Target:** `TCP-SER-B0-D02.BIN`, 512 KB MC68000.
**Written:** 2026-08-19

> The Ghidra database that carries the applied names, types and plate comments is **not
> in this repo** — it lives on the workstation the analysis was done on. Everything in
> these documents was measured from the `.BIN` and can be re-derived from it.

Companion files:
- `TCP-SER-D02-FUNCTION-CATALOG.csv` — every entry point, machine-readable
- `TELNET-XMSG-SIN.md` — the telnet → XMSG → SINTRAN write-up

---

## 1. The count

| | |
|---:|---|
| 1003 | candidate entry points found in the image |
| 906 | functions currently carved in Ghidra |
| 797 | still called `FUN_nnnnnnnn` |
| **221** | of those are **2-byte `trap #1` fault stubs** — not routines at all |
| **782** | real routines |
| 58 | already named |
| **~724** | genuinely need a name |

The 221 stubs matter: they are a fifth of everything unnamed and they need **no analysis at
all**. Each is literally `4E 41` (`trap #1`), one per exception vector, existing only so the
stub ADDRESS tells the fault handler which vector fired. They should be bulk-renamed and then
never looked at again.

## 2. Distribution by layer

| Layer | total | stubs | real | with strings | leaf |
|---|---:|---:|---:|---:|---:|
| PIOC-OS kernel `0x000500-0x004600` | 303 | 221 | 82 | 4 | 254 |
| ENMA driver `0x004600-0x006A00` | 52 | 0 | 52 | 2 | 13 |
| POMN monitor `0x006A00-0x008000` | 17 | 0 | 17 | 6 | 1 |
| TCPD client `0x008000-0x0089C0` | 7 | 0 | 7 | 4 | 2 |
| AIP arp/ip/icmp `0x0089C0-0x00A700` | 28 | 0 | 28 | 7 | 9 |
| PIOC glue / EII `0x00A700-0x00BD00` | 29 | 0 | 29 | 2 | 14 |
| TCP `0x00BD00-0x00E900` | 8 | 0 | 8 | 1 | 3 |
| RAW `0x00E900-0x00F500` | 4 | 0 | 4 | 2 | 1 |
| UDP `0x00F500-0x010300` | 4 | 0 | 4 | 1 | 1 |
| MAIN + socket layer `0x010300-0x014000` | 50 | 0 | 50 | 2 | 11 |
| FSMR TCP state machine `0x014000-0x019E00` | 80 | 0 | 80 | 2 | 16 |
| TELNET `0x019E00-0x01C300` | 42 | 0 | 42 | 8 | 7 |
| SLib sockets `0x01C300-0x01E000` | 34 | 0 | 34 | 14 | 8 |
| SLib trace `0x01E000-0x021400` | 65 | 0 | 65 | 12 | 12 |
| XMSG session `0x021400-0x028100` | 131 | 0 | 131 | 6 | 53 |
| XMSG library `0x028100-0x028900` | 19 | 0 | 19 | 0 | 0 |
| XMSG support `0x028900-0x02B900` | 48 | 0 | 48 | 1 | 14 |
| PLANC runtime + data `0x02B900-` | 82 | 0 | 82 | 0 | 37 |

Two things jump out. **XMSG session is the biggest unnamed block (131)** and it is exactly the
region the open questions live in. **PLANC runtime (82) is compiler-generated library code** —
it should never be hand-analysed.

---

## 3. THE PLAN

Six passes, cheapest and most certain first. Each pass is independently useful, and each one
shrinks the set the next pass has to think about.

### Pass 1 — Bulk-name the 221 fault stubs · effort: minutes · certainty: total

Every 2-byte `4E 41` entry becomes `FaultStub_<addr>`, or `FaultStub_vec<N>` where a vector
points at it — the vector number is already in the catalog CSV. Give them all one repeatable
comment saying what the class is.

**Result: 797 unnamed → 576.** No analysis, no risk.

### Pass 2 — Import ND's own names by BYTE SIGNATURE · effort: hours · certainty: high

This is the single highest-value pass and it is already proven to work.

The ENCOS firmware carries ND's linker symbol table (241 names). D02 does not. But the two
products share compiled library code — the same PLANC runtime, the same XMSG client, the same
subprocess layer — **just linked at different addresses.** So match by CONTENT, not address:
take each ENCOS named routine's first 48 bytes and look for that exact sequence in D02.

Tried already: **51 unique matches out of 144 ENCOS code symbols**, 41 more ambiguous.

The result independently confirms work already done by a completely different route:

| ENCOS name | D02 address | already named as |
|---|---|---|
| `XMPFOPN` | `0x028196` | `XMSG_XFOPN_OpenPort` |
| `XMPFCLS` | `0x0281EE` | `XMSG_XFCLS_ClosePort` |
| `XMPFGET` | `0x028244` | `XMSG_XFGET_GetMessageSpace` |
| `XMPFREL` | `0x0282A4` | `XMSG_XFREL_ReleaseMessageSpace` |
| `XMPFREA` | `0x0282FA` | `XMSG_XFREA_ReadFromMessage` |
| `XMPFWRI` | `0x02836E` | `XMSG_XFWRI_WriteToMessage` |
| `XMPFSND` | `0x0283E2` | `XMSG_XFSND_SendMessage` |
| `XMPFRCV` | `0x02843E` | `XMSG_XFRCV_ReceiveMessage` |
| `XMPFPST` | `0x02859A` | `XMSG_XFPST_PortStatus` |
| `XMPFMST` | `0x028620` | `XMSG_XFMST_MessageStatus` |
| `XMPFSCM` | `0x028698` | `XMSG_XFSCM_SetCurrentMessage` |
| `XMPFPRV` | `0x0286F6` | `XMSG_XFPRV_RequestPrivilege` |
| `XMPFDMM` | `0x02874C` | `XMSG_XFDMM_DefineMaxMemory` |
| `XMPFALM` | `0x0287A2` | `XMSG_XFALM_AllocateMessages` |

Fourteen for fourteen, derived two completely independent ways — trap function code on one
side, byte signature on the other. That agreement is what makes the method trustworthy for the
rest. **Adopt ND's names**: `XMPFOPN` beats `XMSG_XFOPN_OpenPort`, because a vendor name always
wins.

Also recovered this way: the whole PLANC leaf runtime (`#IMU` `0x02B98C`, `#IDV` `0x02B9D2`,
`#REMV` `0x02BBBC`, `#ERET` `0x02BC52`, `#XRET` `0x02BC64`, `#ERROR` `0x02AF32`, `#OUTBYT`
`0x02B02A`, `#INBY` `0x01C5FC`, `#UTBY` `0x02A01E`, `#SPASI` `0x02AE68`, `#BCPC` `0x02003A`),
the MON family (`MON0` `MON1` `MON2` `MON64` `MON65` `#QUIT` at `0x02BDAA-0x02BE04`), and
`PIOCOS` `0x02901A`, `POMNREPORT` `0x028D54`, `POLKUNLOCK` `0x028FFE`, `XMPBLET` `0x028958`,
`XMPXRTS` `0x028CF2`, `XMPBLENGTH` `0x028D32`.

**Two rules, because this pass can lie:**

1. **48-byte signatures only.** Every 16-byte match must be verified by hand. Proof it matters:
   `CSEVENTS` matched `0x02BDD0` on 16 bytes — but `MON1` matched the SAME address on 48 bytes.
   `CSEVENTS` is a false positive. Others to distrust for the same reason:
   `XMTCOMPLET`→`0x015946`, `DATASERVIC`→`0x0243E8`, `CONNECTERR`→`0x02097A`.
2. **Check family consistency.** Where several symbols sit adjacent in ENCOS, their D02 matches
   must keep the same relative offsets. The MON family does: ENCOS `MON64→MON65` is +0x1E and
   D02 is +0x1E; `MON1→MON2` is +0x34 in both. That agreement is worth more than any single
   match on its own.

**Result: ~576 → ~530**, and the names are ND's own.

### Pass 3 — Harvest what D02 says about itself · effort: hours · certainty: high

Already done for the obvious cases (16 names). Push it further:

- **Every** `lea <ascii>.l` in the image, not only the ones near a prologue — some routines load
  their name several instructions in.
- Format strings that name an operation (`"SleepOnEvent, eventmask="`, `"WakeUp from
  SleepOnSocket"`, `"SL :(t=I) (c=I,s=I)"`) name the routine that prints them.
- The catalog marks **68 routines that reference a string and are still unnamed**. Those are the
  cheapest remaining wins; §5 lists them per layer.

### Pass 4 — Name by ROLE from the call graph · effort: days · certainty: medium

For the rest the name comes from position, not from a string. Work outward from every named
anchor:

- a routine whose only caller is `SLsend` and which calls `XMPFWRI` is a send-path marshaller;
- a routine called only from `TELNET_AcceptLoop` is a per-connection handler;
- a routine called from 40 places with no callees is a utility (`Copy`, `Compare`, `Checksum`).

Name these `<LAYER>_<Verb><Object>` — `SL_MarshalSendBuffer`, `TELNET_AllocConnection`. Use the
layer prefixes already established: `PIOCOS_ ENMA_ POMN_ TCPD_ AIP_ TCP_ UDP_ RAW_ MAIN_ FSMR_
TELNET_ SL_ XMSG_ PLANC_`.

**Do this layer by layer, smallest first** — TCP (8), RAW (4), UDP (4), TCPD (7), POMN (17)
before FSMR (80) or XMSG session (131). The small layers give you the vocabulary the big ones
use.

### Pass 5 — The two layers that pay for themselves · effort: days · certainty: medium

Only two of the eighteen layers actually block an outstanding question:

- **XMSG session (131 routines, `0x021400-0x028100`).** This is where the open question lives:
  *which XMSG call carries terminal data, and is the payload TAD?* Start at `0x021850` —
  telnet's entry into the layer — and follow the path that reaches `XMPFWRI`/`XMPFSND` rather
  than the teardown path that reaches `XMPFCLS`/`XMPFREL`. 53 of the 131 are leaves, so the
  real structure is nearer 78 routines.
- **FSMR (80 routines, `0x014000-0x019E00`).** The TCP state machine. Name it against the BSD
  `tcp_input` / `tcp_output` / `tcp_timers` structure the rest of this stack already follows.

### Pass 6 — Leave these alone, deliberately

- **PLANC runtime + data (82).** Compiler-generated. Pass 2 names the ones ND named; the rest
  are `#`-prefixed helpers whose behaviour is in the PLANC manuals. Hand-naming them is wasted
  effort.
- **PIOC-OS kernel leaves (254 of 303).** Mostly the fault stubs plus small kernel helpers, and
  the kernel is byte-identical to ENCOS — anything worth naming there is already named, or is
  in the ENCOS write-up.

---

## 4. Effort and expected outcome

| Pass | Routines named | Effort | Certainty |
|---|---:|---|---|
| 1 · fault stubs | 221 | minutes | total |
| 2 · ENCOS byte signatures | ~46 | hours | high, under the 48-byte rule |
| 3 · self-naming strings | ~68 | hours | high |
| 4 · call-graph role | ~250 | days | medium |
| 5 · XMSG session + FSMR | ~211 | days | medium |
| 6 · deliberately skipped | ~130 | — | — |

**After passes 1–3 — about a day's work — roughly 335 of the 724 are named**, and every one of
those is evidence-backed rather than invented. Passes 4 and 5 are the real reverse engineering.

### Rules to hold to

1. **A vendor name always beats an invented one.** Where pass 2 gives ND's name, use it even if
   an invented name reads better.
2. **Never overwrite a name someone chose.** Only `FUN_`/`SUB_` defaults are fair game.
3. **Record the evidence in the comment, not just the name.** A name asserts; a comment lets the
   next person check.
4. **Mark inference.** A name from a string or a byte signature is evidence. A name from
   call-graph position is a reading — say so in the comment.

---

## 5. The catalog

Every unnamed non-stub routine, grouped by layer. `in` = number of callers, `out` = number of
callees. Full machine-readable form, including stubs and already-named routines:
`TCP-SER-D02-FUNCTION-CATALOG.csv`

### PIOCOS-KERNEL   80 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x0005C8` | 5194 | 0 | 2 | **vector 0** |
| `0x001A12` | 132 | 1 | 0 | leaf |
| `0x001A96` | 52 | 7 | 0 | leaf |
| `0x001ACA` | 54 | 1 | 0 | leaf |
| `0x001B00` | 510 | 0 | 2 | **vector 30** · str: `CX5 APRIL 21, 1986` |
| `0x001DD8` | 40 | 0 | 0 | **vector 31** |
| `0x001E00` | 56 | 0 | 0 | **vector 42** |
| `0x001E38` | 98 | 0 | 0 | **vector 9** |
| `0x001E9A` | 138 | 2 | 3 |  |
| `0x00211C` | 26 | 0 | 0 | **vector 2** |
| `0x002136` | 12 | 0 | 1 | **vector 3** |
| `0x002142` | 30 | 1 | 0 | **vector 79** |
| `0x002160` | 118 | 0 | 1 | **vector 33** · str: `NsT` |
| `0x0021D6` | 288 | 2 | 2 | trap#2 D0=0x09 |
| `0x0022F6` | 74 | 2 | 2 |  |
| `0x002340` | 138 | 1 | 2 |  |
| `0x0023CA` | 18 | 1 | 2 |  |
| `0x0023DC` | 72 | 0 | 2 |  |
| `0x002424` | 134 | 1 | 2 |  |
| `0x0024AA` | 100 | 1 | 3 |  |
| `0x00250E` | 84 | 0 | 1 | **vector 78** · trap#2 D0=0x09 |
| `0x002562` | 164 | 1 | 0 | leaf |
| `0x002606` | 48 | 0 | 0 | **vector 35** |
| `0x002636` | 122 | 1 | 1 |  |
| `0x0026B0` | 566 | 0 | 8 | str: `FREEPRO1` / ` O,\|` |
| `0x0028E6` | 114 | 2 | 4 |  |
| `0x002958` | 88 | 1 | 3 |  |
| `0x0029B0` | 86 | 2 | 2 |  |
| `0x002A06` | 368 | 2 | 6 |  |
| `0x002B76` | 98 | 2 | 3 |  |
| `0x002BD8` | 92 | 1 | 3 |  |
| `0x002C34` | 612 | 1 | 10 |  |
| `0x002E98` | 18 | 2 | 2 |  |
| `0x002EAA` | 412 | 0 | 5 |  |
| `0x003046` | 1106 | 2 | 13 |  |
| `0x003512` | 172 | 0 | 1 |  |
| `0x0035BE` | 230 | 1 | 2 |  |
| `0x0036A4` | 412 | 2 | 3 |  |
| `0x003840` | 298 | 1 | 4 |  |
| `0x00396A` | 120 | 1 | 1 |  |
| `0x0039E2` | 118 | 1 | 1 |  |
| `0x003A58` | 16 | 3 | 1 |  |
| `0x003A68` | 106 | 0 | 1 | **vector 69** · trap#2 D0=0x09 |
| `0x003AD2` | 400 | 1 | 8 | trap#2 D0=0x0A,0x18 · str: `RTC ` |
| `0x003C62` | 316 | 2 | 4 |  |
| `0x003D9E` | 98 | 2 | 4 |  |
| `0x003E00` | 98 | 1 | 5 |  |
| `0x003E62` | 530 | 1 | 8 |  |
| `0x004074` | 2 | 0 | 1 | **vector 91** |
| `0x004076` | 2 | 0 | 0 | **vector 82** |
| `0x004078` | 2 | 0 | 0 | **vector 84** |
| `0x00407A` | 2 | 0 | 0 | **vector 86** |
| `0x00407C` | 2 | 0 | 0 | **vector 90** |
| `0x00407E` | 2 | 0 | 0 | **vector 92** |
| `0x004080` | 2 | 0 | 0 | **vector 94** |
| `0x004082` | 2 | 0 | 0 | **vector 98** |
| `0x004084` | 2 | 0 | 0 | **vector 100** |
| `0x004086` | 2 | 0 | 0 | **vector 102** |
| `0x004088` | 2 | 0 | 0 | **vector 106** |
| `0x00408A` | 2 | 0 | 0 | **vector 108** |
| `0x00408C` | 2 | 0 | 0 | **vector 110** |
| `0x00408E` | 2 | 0 | 0 | **vector 89** |
| `0x004090` | 2 | 0 | 0 | **vector 83** |
| `0x004092` | 2 | 0 | 0 | **vector 81** |
| `0x004094` | 2 | 0 | 0 | **vector 107** |
| `0x004096` | 2 | 0 | 0 | **vector 105** |
| `0x004098` | 2 | 0 | 0 | **vector 99** |
| `0x00409A` | 4 | 0 | 0 | **vector 97** |
| `0x00409E` | 32 | 1 | 1 |  |
| `0x0040BE` | 288 | 1 | 2 |  |
| `0x0041DE` | 304 | 4 | 2 |  |
| `0x00430E` | 94 | 1 | 2 |  |
| `0x00436C` | 158 | 1 | 2 |  |
| `0x00440A` | 14 | 2 | 1 |  |
| `0x004418` | 26 | 1 | 0 | leaf |
| `0x004432` | 36 | 3 | 0 | leaf |
| `0x004456` | 60 | 3 | 0 | leaf |
| `0x004492` | 34 | 32 | 0 | leaf |
| `0x0044B4` | 16 | 32 | 0 | leaf |
| `0x0044C4` | 408 | 9 | 2 | trap#2 D0=0x02,0x02,0x02,0x02,0x02,0x02,0x02,0x02 |

### ENMA-driver   51 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00465C` | 158 | 3 | 0 | trap#2 D0=0x00 |
| `0x0046FA` | 240 | 1 | 1 |  |
| `0x0047EA` | 84 | 1 | 1 |  |
| `0x00483E` | 40 | 1 | 1 |  |
| `0x004866` | 72 | 1 | 1 | trap#2 D0=0x09 |
| `0x0048AE` | 56 | 1 | 2 |  |
| `0x0048E6` | 56 | 1 | 2 |  |
| `0x00491E` | 690 | 1 | 6 |  |
| `0x004BD0` | 66 | 1 | 3 |  |
| `0x004C12` | 54 | 1 | 2 |  |
| `0x004C48` | 80 | 3 | 1 | str: `ENMA` |
| `0x004C98` | 154 | 3 | 2 |  |
| `0x004D32` | 82 | 4 | 2 |  |
| `0x004D84` | 138 | 1 | 2 |  |
| `0x004E0E` | 22 | 2 | 0 | leaf |
| `0x004E24` | 150 | 5 | 2 |  |
| `0x004EBA` | 104 | 1 | 2 |  |
| `0x004F22` | 82 | 1 | 2 |  |
| `0x004F74` | 104 | 1 | 1 |  |
| `0x004FDC` | 92 | 1 | 2 |  |
| `0x005038` | 64 | 0 | 2 |  |
| `0x005078` | 98 | 1 | 0 | leaf |
| `0x0050DA` | 50 | 0 | 0 | leaf |
| `0x00510C` | 270 | 1 | 0 | leaf |
| `0x00521A` | 106 | 1 | 0 | leaf |
| `0x005284` | 126 | 2 | 0 | leaf |
| `0x005302` | 34 | 1 | 2 |  |
| `0x005324` | 398 | 1 | 1 |  |
| `0x0054B2` | 172 | 0 | 1 |  |
| `0x00555E` | 152 | 0 | 1 |  |
| `0x0055F6` | 40 | 4 | 1 |  |
| `0x00561E` | 72 | 2 | 4 |  |
| `0x005666` | 404 | 4 | 4 |  |
| `0x0057FA` | 154 | 1 | 1 |  |
| `0x005894` | 116 | 3 | 0 | leaf |
| `0x005908` | 118 | 1 | 2 |  |
| `0x00597E` | 118 | 0 | 2 |  |
| `0x0059F4` | 1064 | 2 | 4 |  |
| `0x005E1C` | 194 | 2 | 0 | leaf |
| `0x005EDE` | 1034 | 1 | 6 |  |
| `0x0062E8` | 96 | 1 | 0 | leaf |
| `0x006348` | 16 | 1 | 0 | leaf |
| `0x006358` | 352 | 0 | 10 |  |
| `0x0064B8` | 154 | 1 | 2 |  |
| `0x006552` | 168 | 1 | 4 |  |
| `0x00660A` | 318 | 0 | 7 | str: `ENMA` |
| `0x006748` | 132 | 3 | 2 |  |
| `0x0067CC` | 88 | 1 | 1 |  |
| `0x006824` | 88 | 1 | 1 |  |
| `0x00687C` | 202 | 3 | 1 |  |
| `0x006946` | 238 | 2 | 0 | leaf |

### POMN-monitor   16 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x006A34` | 128 | 7 | 2 | str: `POMN` |
| `0x006AB4` | 130 | 1 | 3 |  |
| `0x007022` | 116 | 1 | 4 | calls `POMN_ReportError` |
| `0x007096` | 88 | 2 | 2 |  |
| `0x0070EE` | 268 | 1 | 3 |  |
| `0x0071FA` | 366 | 1 | 3 |  |
| `0x007368` | 44 | 1 | 0 | leaf |
| `0x007394` | 178 | 1 | 1 | trap#2 D0=0x09 |
| `0x007446` | 1186 | 1 | 4 |  |
| `0x0078E8` | 94 | 1 | 3 | calls `POMN_ReportError` |
| `0x007946` | 246 | 1 | 4 | str: `a$POMN FATAL: skpinit statuso7a$PO` / `$POMN FATAL: skpinit statuso7a$POM` |
| `0x007A3C` | 232 | 1 | 4 | str: `a$POMN FATAL: SKP schedule multipl` / `$POMN FATAL: SKP schedule multiple` |
| `0x007B24` | 712 | 0 | 14 | str: `,_$_.n` / `PO100portsPO100messagesPOMN` |
| `0x007DEC` | 46 | 1 | 2 |  |
| `0x007E1A` | 462 | 0 | 3 | str: `,_$_.n` |
| `0x007FE8` | 70 | 4 | 2 |  |

### TCPD-client   6 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00802E` | 100 | 0 | 2 | str: `~s : ~d~n` |
| `0x0080A2` | 28 | 1 | 0 | leaf |
| `0x0080BE` | 1910 | 0 | 15 | str: `*TCPDSERVER` / `TCPD Client, Status (from TCPD) Ba` |
| `0x008834` | 154 | 1 | 2 | str: `UNSET~n * TCP/IP Ethernet II givin` |
| `0x0088CE` | 138 | 1 | 4 | calls `TCPD_ClientSession` · str: `~n * TCP/IP Ethernet II giving up.` |
| `0x008958` | 116 | 5 | 1 |  |

### AIP-arp-ip-icmp   25 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x008A34` | 54 | 3 | 1 |  |
| `0x008A6A` | 114 | 1 | 2 |  |
| `0x008ADC` | 188 | 1 | 0 | leaf |
| `0x008B98` | 408 | 0 | 4 | calls `AIP_ReportPiocError` · str: `AIP ` |
| `0x008D30` | 70 | 2 | 1 |  |
| `0x008D76` | 210 | 1 | 1 |  |
| `0x008E5C` | 330 | 0 | 3 | str: `$AIP: no more fragments for ARP XA` / `AIP ` |
| `0x008FA6` | 162 | 2 | 1 |  |
| `0x009048` | 376 | 1 | 3 | calls `AIP_ArpOutOfFragments` |
| `0x0091C0` | 510 | 1 | 6 | calls `AIP_ArpOutOfFragments` · str: `AIP ` |
| `0x0093BE` | 200 | 1 | 3 |  |
| `0x009486` | 226 | 1 | 3 |  |
| `0x009568` | 648 | 2 | 6 | str: `XI $AIP: ICMP message ` |
| `0x0097F0` | 202 | 6 | 2 |  |
| `0x0098BA` | 148 | 1 | 1 |  |
| `0x00994E` | 720 | 1 | 4 |  |
| `0x009C34` | 1572 | 0 | 10 | calls `TCP_Input`, `RAWinput`, `UDP_Input` · str: `AIP ` / `$AIP: ICMP message ` |
| `0x00A258` | 298 | 1 | 5 | calls `TCP_Input`, `RAWinput`, `UDP_Input` |
| `0x00A382` | 762 | 2 | 7 | calls `IP_Input` · str: `RI RA RN        ND/EII-TCP-  XN ` / `RA RN        ND/EII-TCP-  XN ` |
| `0x00A67C` | 22 | 0 | 0 | leaf |
| `0x00A692` | 22 | 0 | 0 | leaf |
| `0x00A6A8` | 26 | 0 | 0 | leaf |
| `0x00A6C2` | 26 | 0 | 0 | leaf |
| `0x00A6DC` | 26 | 0 | 0 | leaf |
| `0x00A6F6` | 22 | 0 | 0 | leaf |

### PIOC-glue-EII   29 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00A70C` | 26 | 0 | 0 | leaf |
| `0x00A726` | 26 | 0 | 0 | leaf |
| `0x00A740` | 26 | 0 | 0 | leaf |
| `0x00A75A` | 26 | 0 | 0 | leaf |
| `0x00A774` | 26 | 0 | 0 | leaf |
| `0x00A78E` | 26 | 0 | 0 | leaf |
| `0x00A7A8` | 26 | 0 | 0 | leaf |
| `0x00A7C2` | 26 | 0 | 0 | leaf |
| `0x00A7DC` | 22 | 2 | 0 | leaf |
| `0x00A7F2` | 74 | 1 | 1 | str: `TCP PIOC` |
| `0x00A83C` | 16 | 35 | 0 | leaf |
| `0x00A84C` | 74 | 0 | 1 |  |
| `0x00A896` | 24 | 5 | 0 | leaf |
| `0x00A8AE` | 90 | 1 | 3 | str: `PIOC` |
| `0x00A908` | 72 | 5 | 1 |  |
| `0x00A950` | 294 | 1 | 2 |  |
| `0x00AA76` | 192 | 2 | 0 | leaf |
| `0x00AB36` | 118 | 1 | 2 |  |
| `0x00ABAC` | 98 | 1 | 2 |  |
| `0x00AC0E` | 70 | 1 | 2 |  |
| `0x00AC54` | 100 | 1 | 2 |  |
| `0x00ACB8` | 142 | 1 | 0 | leaf |
| `0x00AD46` | 124 | 4 | 0 | leaf |
| `0x00ADC2` | 798 | 15 | 8 | calls `FSMR_TcpExtractOob` |
| `0x00B0E0` | 252 | 2 | 2 |  |
| `0x00B1DC` | 58 | 4 | 1 |  |
| `0x00B216` | 532 | 6 | 4 |  |
| `0x00B42A` | 1894 | 1 | 7 |  |
| `0x00BB90` | 410 | 1 | 3 |  |

### TCP   6 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00C326` | 170 | 1 | 0 | leaf |
| `0x00C3D0` | 158 | 1 | 0 | leaf |
| `0x00C46E` | 152 | 3 | 4 |  |
| `0x00C506` | 122 | 1 | 4 |  |
| `0x00C580` | 3984 | 2 | 5 | calls `TCP_UsrReq` · str: `Ethernet IIJanuary 20, 1992` |
| `0x00D520` | 5096 | 0 | 29 | calls `TCP_UsrReq` |

### RAW   2 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00E908` | 422 | 6 | 6 |  |
| `0x00ED50` | 2130 | 0 | 20 | str: `RAWusrequest` |

### UDP   1 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x00FB7E` | 2152 | 0 | 17 | calls `MAIN_UdpOutput` |

### MAIN+socket-layer   49 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x0105D2` | 486 | 1 | 10 |  |
| `0x0107B8` | 212 | 1 | 2 | str: `0123456789ABCDEF` |
| `0x01088C` | 1032 | 1 | 2 |  |
| `0x010C94` | 178 | 0 | 2 |  |
| `0x010D46` | 328 | 1 | 0 | leaf |
| `0x010E8E` | 158 | 6 | 0 | leaf |
| `0x010F2C` | 210 | 7 | 2 |  |
| `0x010FFE` | 128 | 13 | 2 |  |
| `0x01107E` | 22 | 0 | 0 | leaf |
| `0x011094` | 192 | 1 | 3 |  |
| `0x011154` | 20 | 1 | 0 | leaf |
| `0x011168` | 336 | 0 | 4 |  |
| `0x0112B8` | 216 | 1 | 4 | calls `ENMA_Service` |
| `0x011390` | 426 | 1 | 6 | calls `XMSG_XFPRV`, `XMSG_XFDMM`, `XMSG_XFALM` |
| `0x01153A` | 20 | 3 | 0 | leaf |
| `0x01154E` | 306 | 0 | 5 |  |
| `0x011680` | 22 | 1 | 0 | leaf |
| `0x011696` | 664 | 0 | 7 |  |
| `0x01192E` | 96 | 3 | 1 |  |
| `0x01198E` | 16 | 4 | 0 | leaf |
| `0x01199E` | 180 | 0 | 2 |  |
| `0x011A52` | 442 | 11 | 3 |  |
| `0x011C0C` | 926 | 7 | 5 |  |
| `0x011FAA` | 86 | 3 | 1 |  |
| `0x012000` | 156 | 3 | 2 |  |
| `0x01209C` | 190 | 1 | 2 |  |
| `0x01215A` | 212 | 3 | 1 |  |
| `0x01222E` | 376 | 5 | 2 |  |
| `0x0123A6` | 142 | 2 | 1 |  |
| `0x012434` | 38 | 4 | 0 | leaf |
| `0x01245A` | 82 | 6 | 2 |  |
| `0x0124AC` | 216 | 3 | 2 |  |
| `0x012584` | 80 | 1 | 1 |  |
| `0x0125D4` | 230 | 6 | 3 |  |
| `0x0126BA` | 156 | 1 | 1 |  |
| `0x012756` | 336 | 2 | 1 |  |
| `0x0128A6` | 136 | 1 | 2 |  |
| `0x01292E` | 162 | 2 | 4 |  |
| `0x0129D0` | 122 | 2 | 1 |  |
| `0x012A4A` | 204 | 5 | 0 | leaf |
| `0x012B16` | 150 | 2 | 1 | calls `TCP_UsrReq` |
| `0x012BAC` | 148 | 1 | 4 |  |
| `0x012C40` | 218 | 2 | 6 |  |
| `0x012D1A` | 236 | 1 | 6 | calls `TCP_UsrReq` |
| `0x012E06` | 18 | 1 | 0 | leaf |
| `0x012E18` | 2374 | 0 | 8 | calls `TCP_UsrReq` |
| `0x01375E` | 2048 | 1 | 11 | calls `TCP_UsrReq` |
| `0x013F5E` | 16 | 0 | 0 | leaf |
| `0x013F6E` | 1026 | 0 | 6 |  |

### FSMR-tcp-fsm   78 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x014370` | 16 | 1 | 0 | leaf |
| `0x014380` | 670 | 0 | 5 |  |
| `0x01461E` | 16 | 1 | 0 | leaf |
| `0x01462E` | 720 | 0 | 6 |  |
| `0x0148FE` | 16 | 1 | 0 | leaf |
| `0x01490E` | 216 | 0 | 1 |  |
| `0x0149E6` | 16 | 1 | 0 | leaf |
| `0x0149F6` | 236 | 0 | 1 |  |
| `0x014AE2` | 16 | 2 | 0 | leaf |
| `0x014AF2` | 272 | 0 | 1 |  |
| `0x014C02` | 26 | 0 | 0 | leaf |
| `0x014C1C` | 296 | 1 | 1 |  |
| `0x014D44` | 320 | 22 | 3 |  |
| `0x014E84` | 38 | 1 | 1 |  |
| `0x014EAA` | 52 | 21 | 1 |  |
| `0x014EDE` | 336 | 9 | 4 |  |
| `0x01502E` | 128 | 0 | 1 |  |
| `0x0150AE` | 424 | 1 | 2 |  |
| `0x015256` | 298 | 1 | 2 |  |
| `0x015380` | 358 | 1 | 0 | leaf |
| `0x0154E6` | 66 | 1 | 0 | leaf |
| `0x015528` | 312 | 6 | 3 |  |
| `0x015660` | 112 | 4 | 2 |  |
| `0x0156D0` | 46 | 1 | 1 |  |
| `0x0156FE` | 458 | 2 | 2 |  |
| `0x0158C8` | 126 | 2 | 3 |  |
| `0x015946` | 466 | 2 | 3 |  |
| `0x015B18` | 66 | 1 | 0 | leaf |
| `0x015B5A` | 160 | 1 | 1 |  |
| `0x015BFA` | 112 | 1 | 1 |  |
| `0x015C6A` | 46 | 1 | 1 |  |
| `0x015C98` | 84 | 1 | 1 |  |
| `0x015CEC` | 390 | 3 | 2 |  |
| `0x015E72` | 258 | 3 | 2 |  |
| `0x015F74` | 92 | 2 | 3 |  |
| `0x015FD0` | 66 | 3 | 0 | leaf |
| `0x016012` | 66 | 3 | 0 | leaf |
| `0x016054` | 22 | 0 | 0 | leaf |
| `0x01606A` | 1040 | 1 | 5 |  |
| `0x01647A` | 90 | 3 | 0 | leaf |
| `0x0164E4` | 1056 | 0 | 6 | str: `FSMR.SendPacket` |
| `0x016904` | 436 | 1 | 3 |  |
| `0x016AB8` | 236 | 2 | 0 | leaf |
| `0x016BA4` | 278 | 3 | 2 |  |
| `0x016CBA` | 404 | 1 | 2 |  |
| `0x016E4E` | 114 | 2 | 0 | leaf |
| `0x016EC0` | 240 | 1 | 4 |  |
| `0x016FB0` | 58 | 11 | 2 |  |
| `0x016FEA` | 1470 | 14 | 3 | calls `FSMR_SendPacket` |
| `0x0175A8` | 688 | 1 | 2 |  |
| `0x017858` | 2196 | 3 | 7 |  |
| `0x0180EC` | 1018 | 12 | 6 |  |
| `0x0184E6` | 306 | 1 | 3 | calls `FSMR_SendPacket` |
| `0x018618` | 306 | 2 | 3 |  |
| `0x018A4A` | 48 | 0 | 1 |  |
| `0x018A7A` | 78 | 0 | 2 |  |
| `0x018AC8` | 44 | 0 | 1 |  |
| `0x018AF4` | 100 | 0 | 1 |  |
| `0x018B58` | 110 | 0 | 2 |  |
| `0x018BC6` | 100 | 0 | 1 |  |
| `0x018C2A` | 72 | 0 | 2 |  |
| `0x018C72` | 324 | 0 | 3 |  |
| `0x018DB6` | 84 | 0 | 2 |  |
| `0x018E0A` | 44 | 0 | 1 |  |
| `0x018E36` | 134 | 0 | 3 | calls `FSMR_SendPacket` |
| `0x018EBC` | 44 | 0 | 1 |  |
| `0x018EE8` | 56 | 0 | 1 |  |
| `0x018F20` | 438 | 0 | 5 | calls `TCP_UsrReq` |
| `0x0190D6` | 260 | 0 | 3 |  |
| `0x0191DA` | 382 | 0 | 3 |  |
| `0x019358` | 248 | 0 | 4 |  |
| `0x019450` | 220 | 0 | 3 |  |
| `0x01952C` | 354 | 0 | 5 | calls `TCP_UsrReq` |
| `0x01968E` | 108 | 0 | 3 |  |
| `0x0196FA` | 114 | 0 | 2 |  |
| `0x01976C` | 220 | 0 | 3 |  |
| `0x019848` | 160 | 0 | 1 |  |
| `0x0198E8` | 1406 | 0 | 11 | calls `TCP_UsrReq` |

### TELNET   33 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x019E66` | 26 | 0 | 0 | leaf |
| `0x019E80` | 94 | 9 | 1 | str: `TN  ` |
| `0x019EDE` | 66 | 1 | 1 |  |
| `0x019F20` | 50 | 1 | 0 | leaf |
| `0x019F52` | 34 | 3 | 0 | leaf |
| `0x019F74` | 72 | 0 | 2 |  |
| `0x019FBC` | 188 | 4 | 2 | calls `SLsend` |
| `0x01A114` | 140 | 1 | 2 | calls `TELNET_SendToSocket` |
| `0x01A1A0` | 266 | 2 | 2 |  |
| `0x01A312` | 76 | 2 | 2 | calls `TELNET_CloseConnection` |
| `0x01A35E` | 100 | 8 | 2 |  |
| `0x01A3C2` | 36 | 1 | 0 | leaf |
| `0x01A3E6` | 16 | 3 | 0 | leaf |
| `0x01A3F6` | 74 | 0 | 3 |  |
| `0x01A440` | 94 | 3 | 3 | calls `TELNET_CloseConnection` |
| `0x01A49E` | 52 | 6 | 2 |  |
| `0x01A4D2` | 252 | 1 | 2 | str: `0123456789ABCDEFJanuary 20, 1992C0` / `0123456789ABCDEFJanuary 20, 1992C0` |
| `0x01A5CE` | 608 | 1 | 4 |  |
| `0x01A82E` | 272 | 1 | 1 |  |
| `0x01A9CE` | 68 | 2 | 0 | leaf |
| `0x01AA12` | 146 | 0 | 3 |  |
| `0x01AAA4` | 94 | 1 | 0 | leaf |
| `0x01AB02` | 198 | 0 | 3 |  |
| `0x01ABC8` | 986 | 1 | 4 |  |
| `0x01AFA2` | 212 | 1 | 4 |  |
| `0x01B076` | 84 | 1 | 1 |  |
| `0x01B2A2` | 132 | 1 | 5 | calls `TELNET_PollSocket` |
| `0x01B326` | 302 | 2 | 1 |  |
| `0x01B7F0` | 290 | 1 | 3 | calls `TELNET_TearDownConnection`, `SLioctl` |
| `0x01B912` | 152 | 1 | 4 |  |
| `0x01B9AA` | 230 | 1 | 3 | calls `TELNET_ReportServerAvailable` |
| `0x01C058` | 182 | 1 | 2 |  |
| `0x01C250` | 176 | 0 | 6 | calls `TELNET_ServerInit`, `TELNET_Main` · str: `,_$_.n` |

### SLib-sockets   23 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x01C300` | 20 | 1 | 1 |  |
| `0x01C314` | 212 | 0 | 2 | str: `PIOCPIOCSLsleep` |
| `0x01C3E8` | 148 | 1 | 1 |  |
| `0x01C47C` | 82 | 5 | 1 |  |
| `0x01C4CE` | 156 | 4 | 2 |  |
| `0x01C56A` | 146 | 1 | 1 |  |
| `0x01C5FC` | 230 | 1 | 0 | leaf |
| `0x01C6E2` | 16 | 1 | 0 | leaf |
| `0x01C6F2` | 344 | 0 | 4 | str: `PIOCSLsleep` |
| `0x01C84A` | 50 | 1 | 1 |  |
| `0x01C87C` | 94 | 1 | 2 |  |
| `0x01C8DA` | 72 | 1 | 0 | leaf |
| `0x01C922` | 64 | 1 | 2 |  |
| `0x01C962` | 82 | 1 | 0 | leaf |
| `0x01C9B4` | 112 | 1 | 1 |  |
| `0x01CA24` | 566 | 1 | 3 |  |
| `0x01CC5A` | 180 | 1 | 0 | leaf |
| `0x01CD0E` | 676 | 1 | 3 |  |
| `0x01CFB2` | 142 | 0 | 0 | leaf |
| `0x01D040` | 34 | 2 | 1 |  |
| `0x01D0B4` | 94 | 1 | 0 | leaf |
| `0x01D112` | 94 | 1 | 0 | leaf |
| `0x01DFB6` | 76 | 5 | 1 | str: `AA1A$ATTACH` |

### SLib-trace   65 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x01E002` | 78 | 0 | 1 | str: `A1A$ATTACH` |
| `0x01E050` | 80 | 3 | 1 | str: `A$ATTACH` / `$ATTACH` |
| `0x01E0A0` | 108 | 1 | 1 | str: `PRU_ cflag=CTED CTINGDISCTEDNEWCTE` / `ATTACH` |
| `0x01E10C` | 418 | 6 | 0 | leaf |
| `0x01E2AE` | 210 | 5 | 0 | leaf |
| `0x01E380` | 196 | 1 | 1 | str: ` cflag=CTED CTINGDISCTEDNEWCTEDUnk` / `CTED CTINGDISCTEDNEWCTEDUnknown` |
| `0x01E444` | 1076 | 1 | 6 | str: `SL :(t=I)  (c=I,s=IANIL)  Newid=I3` / `I)  (c=I,s=IANIL)  Newid=I3` |
| `0x01E878` | 388 | 12 | 3 | str: `SL :(t=I)  (c=I,s=I) SL :(t=I) Sle` / `I)  (c=I,s=I) SL :(t=I) SleepOnEve` |
| `0x01E9FC` | 524 | 3 | 3 | str: `SL :(t=I) SleepOnEvent, eventmask=` / `I) SleepOnEvent, eventmask=O) Wake` |
| `0x01EC08` | 150 | 5 | 1 | str: `ASL : Arequest for too big msg$Sle` / `SL : Arequest for too big msg$Slee` |
| `0x01EC9E` | 104 | 2 | 2 |  |
| `0x01ED06` | 374 | 3 | 4 |  |
| `0x01EE7C` | 32 | 3 | 0 | leaf |
| `0x01EE9C` | 1002 | 0 | 9 | str: `request for too big msg$Sleeping f` / `Sleeping for free messages$` |
| `0x01F286` | 44 | 3 | 1 |  |
| `0x01F2B2` | 62 | 0 | 1 |  |
| `0x01F2F0` | 180 | 2 | 3 | calls `MAIN_MainLoop` |
| `0x01F3A4` | 40 | 1 | 0 | leaf |
| `0x01F3CC` | 138 | 1 | 2 |  |
| `0x01F456` | 82 | 6 | 2 |  |
| `0x01F4A8` | 88 | 3 | 3 |  |
| `0x01F500` | 90 | 3 | 2 |  |
| `0x01F55A` | 154 | 4 | 1 |  |
| `0x01F5F4` | 190 | 13 | 1 |  |
| `0x01F6B2` | 74 | 1 | 1 |  |
| `0x01F6FC` | 64 | 2 | 1 |  |
| `0x01F73C` | 78 | 0 | 1 |  |
| `0x01F78A` | 42 | 2 | 1 |  |
| `0x01F7B4` | 398 | 1 | 8 | calls `MAIN_MainLoop` |
| `0x01F942` | 314 | 3 | 7 |  |
| `0x01FA7C` | 264 | 2 | 5 |  |
| `0x01FB84` | 94 | 3 | 2 |  |
| `0x01FBE2` | 140 | 1 | 3 |  |
| `0x01FC6E` | 134 | 4 | 3 |  |
| `0x01FCF4` | 124 | 1 | 3 |  |
| `0x01FD70` | 98 | 0 | 0 | leaf |
| `0x01FDD2` | 48 | 2 | 0 | leaf |
| `0x01FE02` | 94 | 3 | 1 |  |
| `0x01FE60` | 320 | 6 | 3 |  |
| `0x01FFA0` | 154 | 1 | 2 |  |
| `0x02003A` | 262 | 0 | 3 |  |
| `0x020140` | 268 | 3 | 4 |  |
| `0x02024C` | 132 | 3 | 3 |  |
| `0x0202D0` | 36 | 0 | 0 | leaf |
| `0x0202F4` | 112 | 2 | 0 | leaf |
| `0x020364` | 104 | 4 | 1 |  |
| `0x0203CC` | 104 | 9 | 0 | leaf |
| `0x020434` | 64 | 1 | 1 |  |
| `0x020474` | 132 | 3 | 0 | leaf |
| `0x0204F8` | 72 | 1 | 1 |  |
| `0x020540` | 72 | 1 | 1 |  |
| `0x020588` | 96 | 0 | 1 |  |
| `0x0205E8` | 160 | 1 | 4 |  |
| `0x020688` | 210 | 1 | 4 |  |
| `0x02075A` | 48 | 0 | 1 |  |
| `0x02078A` | 48 | 1 | 1 |  |
| `0x0207BA` | 34 | 1 | 1 |  |
| `0x0207DC` | 228 | 1 | 5 |  |
| `0x0208C0` | 186 | 1 | 2 |  |
| `0x02097A` | 118 | 1 | 3 |  |
| `0x0209F0` | 16 | 1 | 0 | leaf |
| `0x020A00` | 400 | 0 | 5 | str: `Lost SK message$Lost SK message$Lo` / `Lost SK message$Lost PL$ProcessSK:` |
| `0x020B90` | 134 | 1 | 3 | str: `Lost PL$ProcessSK: error from SKPr` |
| `0x020C16` | 16 | 2 | 0 | leaf |
| `0x020C26` | 2034 | 0 | 14 | str: `ProcessSK: error from SKPreceive$i` / `invalid message type (SKMTviaXROUT` |

### XMSG-session   130 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x021418` | 60 | 0 | 1 |  |
| `0x021454` | 42 | 1 | 1 |  |
| `0x02147E` | 122 | 0 | 3 | str: `Fatal internal SLib error` |
| `0x0214F8` | 50 | 1 | 0 | leaf |
| `0x021850` | 16 | 1 | 0 | leaf |
| `0x021860` | 2528 | 0 | 16 | str: `SLinit ` |
| `0x022240` | 34 | 0 | 1 |  |
| `0x022262` | 124 | 1 | 1 |  |
| `0x0222DE` | 82 | 2 | 0 | leaf |
| `0x022330` | 76 | 1 | 0 | leaf |
| `0x02237C` | 78 | 1 | 0 | leaf |
| `0x0223CA` | 16 | 1 | 0 | leaf |
| `0x0223DA` | 82 | 0 | 1 | str: `B1B1` |
| `0x02242C` | 16 | 1 | 0 | leaf |
| `0x02243C` | 96 | 0 | 1 |  |
| `0x02249C` | 24 | 1 | 0 | leaf |
| `0x0224B4` | 54 | 1 | 0 | leaf |
| `0x0224EA` | 48 | 1 | 0 | leaf |
| `0x02251A` | 24 | 1 | 0 | leaf |
| `0x022532` | 48 | 1 | 0 | leaf |
| `0x022562` | 32 | 0 | 0 | leaf |
| `0x022582` | 30 | 0 | 0 | leaf |
| `0x0225A0` | 30 | 0 | 0 | leaf |
| `0x0225BE` | 20 | 0 | 0 | leaf |
| `0x0225D2` | 492 | 0 | 2 |  |
| `0x0227BE` | 16 | 0 | 0 | leaf |
| `0x0227CE` | 72 | 0 | 1 |  |
| `0x022816` | 28 | 0 | 0 | leaf |
| `0x022832` | 150 | 1 | 0 | leaf |
| `0x0228C8` | 36 | 1 | 0 | leaf |
| `0x0228EC` | 846 | 1 | 6 |  |
| `0x022C3A` | 504 | 2 | 6 |  |
| `0x022E32` | 114 | 3 | 1 |  |
| `0x022EA4` | 100 | 2 | 1 |  |
| `0x022F08` | 84 | 4 | 1 |  |
| `0x022F5C` | 104 | 4 | 1 |  |
| `0x022FC4` | 264 | 0 | 3 |  |
| `0x0230CC` | 56 | 2 | 1 |  |
| `0x023104` | 36 | 3 | 1 |  |
| `0x023128` | 122 | 1 | 1 |  |
| `0x0231A2` | 226 | 0 | 1 |  |
| `0x023284` | 168 | 0 | 2 |  |
| `0x02332C` | 192 | 2 | 2 | str: `0123456789ABCDEF%d` |
| `0x0233EC` | 898 | 3 | 5 |  |
| `0x02376E` | 84 | 1 | 1 |  |
| `0x0237C2` | 70 | 1 | 1 |  |
| `0x023808` | 72 | 0 | 1 |  |
| `0x023850` | 1034 | 2 | 2 |  |
| `0x023C5A` | 138 | 3 | 2 |  |
| `0x023CE4` | 84 | 1 | 1 |  |
| `0x023D38` | 52 | 0 | 1 |  |
| `0x023D6C` | 44 | 1 | 1 |  |
| `0x023D98` | 38 | 1 | 1 |  |
| `0x023DBE` | 72 | 0 | 2 |  |
| `0x023E06` | 104 | 0 | 1 |  |
| `0x023E6E` | 46 | 1 | 0 | leaf |
| `0x023E9C` | 46 | 0 | 0 | leaf |
| `0x023ECA` | 24 | 0 | 0 | leaf |
| `0x023EE2` | 26 | 0 | 0 | leaf |
| `0x023EFC` | 250 | 0 | 0 | leaf |
| `0x023FF6` | 150 | 0 | 0 | leaf |
| `0x02408C` | 208 | 1 | 0 | leaf |
| `0x02415C` | 184 | 0 | 0 | leaf |
| `0x024214` | 228 | 0 | 0 | leaf |
| `0x0242F8` | 118 | 0 | 0 | leaf |
| `0x02436E` | 122 | 0 | 0 | leaf |
| `0x0243E8` | 86 | 3 | 0 | leaf |
| `0x02443E` | 94 | 2 | 0 | leaf |
| `0x02449C` | 186 | 0 | 1 |  |
| `0x024556` | 20 | 3 | 0 | leaf |
| `0x02456A` | 28 | 1 | 0 | leaf |
| `0x024586` | 40 | 1 | 1 |  |
| `0x0245AE` | 882 | 0 | 9 | calls `XMSG_XFPRV`, `XMSG_XFDMM`, `XMSG_XFDUM` |
| `0x024920` | 20 | 2 | 0 | leaf |
| `0x024934` | 140 | 0 | 3 | calls `XMSG_XFRRE` |
| `0x0249C0` | 238 | 1 | 4 | calls `XMSG_XFREL`, `XMSG_XFPST` |
| `0x024AAE` | 292 | 0 | 3 | calls `XMSG_XFGSM` |
| `0x024BD2` | 20 | 1 | 0 | leaf |
| `0x024BE6` | 458 | 0 | 3 |  |
| `0x024DB0` | 320 | 0 | 4 |  |
| `0x024EF0` | 20 | 4 | 0 | leaf |
| `0x024F04` | 154 | 0 | 2 |  |
| `0x024F9E` | 20 | 2 | 0 | leaf |
| `0x024FB2` | 126 | 0 | 2 |  |
| `0x025030` | 100 | 1 | 2 | calls `XMSG_XFOPN` |
| `0x025094` | 20 | 4 | 0 | leaf |
| `0x0250A8` | 934 | 0 | 6 | calls `XMSG_XFCLS` |
| `0x02544E` | 20 | 1 | 0 | leaf |
| `0x025462` | 828 | 0 | 9 | calls `XMSG_XFOPN`, `XMSG_XFCLS`, `XMSG_XFREL`, `XMSG_XFREA` |
| `0x02579E` | 958 | 2 | 5 |  |
| `0x025B5C` | 20 | 1 | 0 | leaf |
| `0x025B70` | 82 | 0 | 2 | calls `XMSG_XFMST` |
| `0x025BC2` | 20 | 1 | 0 | leaf |
| `0x025BD6` | 40 | 1 | 1 |  |
| `0x025BFE` | 388 | 0 | 5 |  |
| `0x025D82` | 20 | 1 | 0 | leaf |
| `0x025D96` | 70 | 0 | 2 | calls `XMSG_XFGET` |
| `0x025DDC` | 20 | 4 | 0 | leaf |
| `0x025DF0` | 44 | 1 | 1 |  |
| `0x025E1C` | 604 | 0 | 5 |  |
| `0x026078` | 20 | 10 | 0 | leaf |
| `0x02608C` | 464 | 0 | 6 | calls `XMSG_XFCLS`, `XMSG_XFREL` |
| `0x02625C` | 20 | 6 | 0 | leaf |
| `0x026270` | 40 | 1 | 1 |  |
| `0x026298` | 1008 | 0 | 8 |  |
| `0x026688` | 20 | 3 | 0 | leaf |
| `0x02669C` | 446 | 0 | 4 |  |
| `0x02685A` | 20 | 4 | 0 | leaf |
| `0x02686E` | 40 | 1 | 1 |  |
| `0x026896` | 396 | 0 | 4 |  |
| `0x026A22` | 20 | 5 | 0 | leaf |
| `0x026A36` | 40 | 1 | 1 |  |
| `0x026A5E` | 402 | 0 | 4 |  |
| `0x026BF0` | 186 | 1 | 1 |  |
| `0x026CAA` | 256 | 1 | 2 | calls `XMSG_XFM2P` |
| `0x026DAA` | 462 | 3 | 2 | trap#2 D0=0x19 · calls `XMSG_XFREL`, `XMSG_MapStatusToError` |
| `0x026F78` | 132 | 2 | 3 | calls `XMSG_XFWRI`, `XMSG_XFSCM` |
| `0x026FFC` | 168 | 2 | 3 | calls `XMSG_XFREA` |
| `0x0270A4` | 20 | 1 | 0 | leaf |
| `0x0270B8` | 100 | 0 | 2 |  |
| `0x02711C` | 170 | 1 | 3 | calls `XMSG_XFWRI`, `XMSG_XFSND`, `XMSG_XFMST` |
| `0x0271C6` | 280 | 2 | 3 | calls `XMSG_XFREL` |
| `0x0272DE` | 114 | 1 | 3 | calls `XMSG_XFREA` |
| `0x027350` | 20 | 1 | 0 | leaf |
| `0x027364` | 342 | 0 | 5 | calls `XMSG_XFREL`, `XMSG_XFRCV`, `XMSG_XFMST` |
| `0x0274BA` | 20 | 2 | 0 | leaf |
| `0x0274CE` | 22 | 1 | 1 |  |
| `0x0274E4` | 428 | 0 | 4 |  |
| `0x027690` | 1620 | 18 | 1 | str: `XMXMPONKSKUX` / `XMPONKSKUX` |
| `0x027CE4` | 1066 | 1 | 12 | calls `XMSG_XFOPN`, `XMSG_XFCLS`, `XMSG_XFGET`, `XMSG_XFREL` |

### XMSG-library   1 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x02810E` | 136 | 1 | 2 | calls `XMSG_XFSND`, `XMSG_XFSCM` |

### XMSG-support   47 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x028958` | 300 | 1 | 4 |  |
| `0x028A84` | 128 | 2 | 1 |  |
| `0x028B04` | 96 | 2 | 1 |  |
| `0x028B64` | 348 | 2 | 4 |  |
| `0x028CF2` | 64 | 1 | 0 | leaf |
| `0x028D32` | 34 | 2 | 0 | leaf |
| `0x028D54` | 480 | 11 | 5 |  |
| `0x028F34` | 202 | 6 | 1 |  |
| `0x028FFE` | 28 | 8 | 0 | leaf |
| `0x02901A` | 48 | 31 | 0 | trap#2 D0= |
| `0x02904A` | 178 | 5 | 2 |  |
| `0x0290FC` | 492 | 1 | 5 |  |
| `0x0292E8` | 94 | 0 | 1 |  |
| `0x029346` | 24 | 1 | 0 | leaf |
| `0x02935E` | 146 | 1 | 2 |  |
| `0x0293F0` | 86 | 1 | 1 |  |
| `0x029446` | 16 | 1 | 0 | leaf |
| `0x029456` | 332 | 0 | 2 |  |
| `0x0295A2` | 16 | 1 | 0 | leaf |
| `0x0295B2` | 770 | 0 | 4 |  |
| `0x0298B4` | 316 | 3 | 5 |  |
| `0x0299F0` | 290 | 1 | 3 |  |
| `0x029B12` | 226 | 1 | 2 |  |
| `0x029BF4` | 248 | 1 | 2 |  |
| `0x029CEC` | 86 | 1 | 0 | leaf |
| `0x029D42` | 90 | 0 | 0 | leaf |
| `0x029D9C` | 132 | 1 | 1 |  |
| `0x029E20` | 152 | 1 | 1 |  |
| `0x029EB8` | 266 | 16 | 0 | leaf |
| `0x029FC2` | 32 | 6 | 0 | leaf |
| `0x029FE2` | 60 | 1 | 1 |  |
| `0x02A01E` | 604 | 9 | 3 |  |
| `0x02A27A` | 1042 | 1 | 2 |  |
| `0x02A68C` | 2012 | 8 | 6 | str: `2000000000080000000` / `80000000` |
| `0x02AE68` | 202 | 2 | 2 |  |
| `0x02AF32` | 132 | 1 | 1 |  |
| `0x02AFB6` | 116 | 3 | 0 | leaf |
| `0x02B02A` | 46 | 0 | 1 |  |
| `0x02B058` | 62 | 1 | 1 |  |
| `0x02B096` | 200 | 1 | 2 |  |
| `0x02B15E` | 246 | 1 | 2 |  |
| `0x02B254` | 62 | 13 | 2 |  |
| `0x02B292` | 430 | 2 | 2 |  |
| `0x02B440` | 112 | 1 | 1 |  |
| `0x02B4B0` | 348 | 1 | 0 | leaf |
| `0x02B60C` | 346 | 22 | 1 |  |
| `0x02B766` | 550 | 3 | 1 |  |

### PLANC-runtime+data   82 unnamed routines

| addr | size | in | out | evidence |
|---|---:|---:|---:|---|
| `0x02B98C` | 70 | 25 | 0 | leaf |
| `0x02B9D2` | 172 | 18 | 0 | leaf |
| `0x02BA7E` | 278 | 1 | 3 |  |
| `0x02BB94` | 14 | 21 | 0 | leaf |
| `0x02BBA2` | 26 | 23 | 0 | leaf |
| `0x02BBBC` | 168 | 27 | 0 | leaf |
| `0x02BC64` | 232 | 23 | 0 | trap#2 D0=0x02,0x02,0x02,0x02,0x02,0x02,0x02 |
| `0x02BD4C` | 94 | 0 | 0 | trap#2 D0=0x02,0x02 |
| `0x02BDAA` | 34 | 0 | 0 | leaf |
| `0x02BDCC` | 4 | 4 | 0 | trap#2 D0=0x00 |
| `0x02BDD0` | 52 | 1 | 0 | trap#2 D0=0x01 |
| `0x02BE04` | 3148 | 2 | 1 | trap#2 D0=0x02 |
| `0x02CA50` | 2886 | 1 | 2 |  |
| `0x02D596` | 7 | 1 | 0 | leaf |
| `0x02D59D` | 45 | 1 | 3 |  |
| `0x02D5CA` | 30 | 1 | 0 | leaf |
| `0x02D5E8` | 74 | 1 | 0 | leaf |
| `0x02D632` | 358 | 1 | 1 |  |
| `0x02D798` | 975 | 1 | 2 |  |
| `0x02DB67` | 31 | 1 | 1 |  |
| `0x02DB86` | 105 | 1 | 1 |  |
| `0x02DBEF` | 27 | 1 | 0 | leaf |
| `0x02DC0A` | 37903 | 1 | 1 |  |
| `0x037019` | 209790 | 1 | 2 |  |
| `0x06A397` | 61 | 1 | 0 | leaf |
| `0x06A3D4` | 45199 | 1 | 1 |  |
| `0x075463` | 2962 | 1 | 4 |  |
| `0x075FF5` | 16 | 1 | 1 |  |
| `0x076005` | 4 | 1 | 0 | leaf |
| `0x076009` | 7 | 1 | 1 |  |
| `0x076010` | 12 | 1 | 1 |  |
| `0x07601C` | 6 | 1 | 0 | leaf |
| `0x076022` | 14 | 1 | 1 |  |
| `0x076030` | 4 | 1 | 1 |  |
| `0x076034` | 17 | 1 | 1 |  |
| `0x076045` | 3 | 1 | 0 | leaf |
| `0x076048` | 15 | 1 | 2 |  |
| `0x076057` | 4 | 1 | 0 | leaf |
| `0x07605B` | 15 | 1 | 1 |  |
| `0x07606A` | 5 | 1 | 1 |  |
| `0x07606F` | 14 | 1 | 1 |  |
| `0x07607D` | 8 | 1 | 1 |  |
| `0x076085` | 11 | 1 | 0 | leaf |
| `0x076090` | 9 | 1 | 1 |  |
| `0x076099` | 9 | 1 | 1 |  |
| `0x0760A2` | 11 | 1 | 1 |  |
| `0x0760AD` | 16 | 1 | 1 |  |
| `0x0760BD` | 2 | 1 | 0 | leaf |
| `0x0760BF` | 19 | 1 | 2 |  |
| `0x0760D2` | 8 | 1 | 1 |  |
| `0x0760DA` | 11 | 1 | 1 |  |
| `0x0760E5` | 12 | 1 | 1 |  |
| `0x0760F1` | 6 | 1 | 0 | leaf |
| `0x0760F7` | 16 | 1 | 1 |  |
| `0x076107` | 148 | 0 | 5 |  |
| `0x07619B` | 20 | 1 | 0 | leaf |
| `0x0761AF` | 14 | 1 | 0 | leaf |
| `0x0761BD` | 1 | 1 | 0 | leaf |
| `0x0761BE` | 138 | 1 | 1 |  |
| `0x076248` | 22596 | 1 | 1 |  |
| `0x07BA8C` | 350 | 1 | 3 |  |
| `0x07BBEA` | 14 | 1 | 2 |  |
| `0x07BBF8` | 28 | 1 | 0 | leaf |
| `0x07BC14` | 4 | 1 | 0 | leaf |
| `0x07BC18` | 49 | 1 | 1 |  |
| `0x07BC49` | 86 | 1 | 0 | leaf |
| `0x07BC9F` | 622 | 1 | 1 |  |
| `0x07BF0D` | 280 | 1 | 3 |  |
| `0x07C025` | 13 | 1 | 0 | leaf |
| `0x07C032` | 72 | 1 | 0 | leaf |
| `0x07C07A` | 227 | 1 | 3 |  |
| `0x07C15D` | 16 | 1 | 0 | leaf |
| `0x07C16D` | 53 | 1 | 1 |  |
| `0x07C1A2` | 57 | 1 | 3 |  |
| `0x07C1DB` | 75 | 1 | 3 |  |
| `0x07C226` | 18 | 1 | 0 | leaf |
| `0x07C238` | 2 | 1 | 0 | leaf |
| `0x07C23A` | 18 | 1 | 0 | leaf |
| `0x07C24C` | 2 | 1 | 0 | leaf |
| `0x07C24E` | 18 | 1 | 0 | leaf |
| `0x07C260` | 438 | 1 | 1 |  |
| `0x07C416` | 15338 | 1 | 0 | leaf |
