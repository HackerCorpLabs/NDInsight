# XMSG-COMMAND START-NETWORK-SERVER decode - why XRUNN=2 "Unknown name" (2026-07-23)

Static reverse-engineering of the SINTRAN **XMSG-COMMAND** program's
`START-NETWORK-SERVER` handler, to prove statically what it does and pin why
`START-NETWORK-SERVER ENNS0` returns XROUT error **XRUNN=2 "Unknown name (of
server or system)"**. This closes the last unknown in the ND Ethernet II
(PCB 3094 / ND-110063) bring-up.

Tags: `[V]` = VERIFIED (bytes read / disassembled / manual quote / prior wire
trace); `[I]` = INFERRED; `[OPEN]` = not decoded here, exact anchor given.

Primary target: `xmsg-L-binaries/XMSG-COMMAND.PROG` = the **Release M** build
(guest banner "XMSG command program (210373M)"). Cross-checked against
`xmsg-L-binaries/XMSG-COMMAND-L03.PROG` (older L03). Both binaries left UNMODIFIED.

---

## 0. Headline

- `[V]` The `START-NETWORK-SERVER` handler lives at **mem 0o50722** (M build).
  It builds an XROUT letter for the server name **`*XM-ENNS0`**, loads XROUT
  service code **XSNET = 85** (`SAA 85` at **0o72546**), and issues **MON 200B**
  through the shared XMSG-library wrapper at **0o72314** to **XROUT port 0**.
- `[V]` The handler does **NOT** itself create the name (no `XSCRS`/`XSNAM` on
  its path). It resolves/starts an *already-defined* gateway. It first queries
  the name, prints **"Server not yet started - will try to start him now (wait
  10 sec!)"** (@ mem 0o50617), then issues the `XSNET` start - which is the call
  that returns XRUNN.
- `[I]` XRUNN=2 is returned because the name/system that `XSNET` must resolve
  (the network-server `*XM-ENNS0` and/or the local system it is bound to) is not
  present in the running XROUT's tables at that moment. Per the official
  ND-210580 recipe those tables are populated by the **XMSG-STARTUP** mode file
  (the `DEFINE-REMOTE-NAME` name<->system mappings) which must run *before*
  `START-NETWORK-SERVER`. The emulator harness ran only `start-x` + `@RT ENNS0`
  + `START-NETWORK-SERVER`, skipping XMSG-STARTUP.
- `[V]` `SET-PRIVILEGED` is **not** the missing step: the live trace shows
  `XFPRV` succeeded ("...you can now bypass system protection mechanisms...");
  a privilege failure would print the *different* string
  "*- ERROR: XMSG not started. (XFPRV status not 1) -*".

---

## 1. The :PROG loader (Task 1) - VERIFIED

Added `tools/prog_load.py` (+ `tools/progtool.py`, `tools/disprog.py`).
Header format is the VERIFIED layout from the repo doc
`SINTRAN/File-Formats/prog-fileformat.md` (7 big-endian words at file offset 0,
bank-1 image at file offset **0x200**):

| word | field | M value | L03 value |
|---|---|---|---|
| 0 | start addr | 0 | 0 |
| 1 | restart addr | 1 | 1 |
| 2 | bank1 first | 0 | 0 |
| 3 | bank1 last | 0o125223 | 0o125275 |
| 4 | bank2 first | 0o177777 (none) | 0o177777 (none) |
| 5 | bank2 last | 0 | 0 |
| 6 | data-bank-copy | 0 | 0 |

Both are **one-bank** images loaded at address 0. The mapping is therefore

```
memory word address A  <->  file byte offset  0x200 + 2*A
```

**`[V]` M-file caveat (do not trip on it):** `XMSG-COMMAND.PROG` is 111103 bytes
(odd). The header's bank-1 image ends at file 0x15728; everything past that
(including a *second* complete copy of the XROUT/XMSG error-string block, e.g.
"Unknown name..." at file 0x18906) is **beyond bank1_last and is NOT loaded** by
`@RECOVER`. All addresses in this document use the loaded bank-1 copy (the
"Unknown name" string that the running program uses is at mem **0o113352**, file
0x12fd4). The L03 file is a clean 44-page one-bank image with no such tail.

`tools/prog_load.py` verified: `load_prog()` reproduces the header table above
and yields a flat `mem[]` indexed by ND-100 word address.

---

## 2. Locating the handler (Task 2) - VERIFIED

### 2a. Command dispatch table @ mem 0o22334
XMSG-COMMAND is a command interpreter. The command names are one big
apostrophe(0x27)-separated blob starting at **file 0x4f74 / mem 0o23272**
("List-Ports'List-Tasks'...'Start-Network-Server'...'). `0x00` bytes are word
alignment padding.

The dispatch table is at **mem 0o22334** (referenced by the four command-scanner
sites 0o21756 / 0o22334 / 0o22736 / 0o22776). It is an array of
**(name_ptr, handler_ptr) word pairs**, one per command:

```
0o22334: 023272 033224   ; List-Ports
0o22336: 023300 033253   ; List-Tasks
...
name_ptr 024066 -> handler 050722   ; Start-Network-Server   <===
name_ptr 024101 -> handler 050672   ; Stop-Network-Server
name_ptr 024113 -> handler 051054   ; Define-Network-Connection
name_ptr 023501 -> handler 046343   ; Define-Remote-Name
name_ptr 023467 -> handler 046201   ; Define-Local-System
```

`[V]` name_ptr **0o24066** decodes to the ASCII "Start-Network-Server" (file
0x0526c), so its handler is **mem 0o50722**.

### 2b. Operator strings (all VERIFIED, loaded bank-1 copy)
| string | mem addr |
|---|---|
| "Start-Network-Server" (command name) | 0o24066 |
| "Server not yet started - will try to start him now (wait 10 sec!)" | 0o50617 |
| "Error in communicating with XROUT." | 0o27510 |
| "Unknown name (of server or system)" | 0o113352 |

(The task's paraphrase "Server not started - will try to start it now" is the
"...him now..." string above.)

---

## 3. What the handler does (Task 3) - VERIFIED (flow) + trace (letter body)

### 3a. Handler @0o50722
The handler is PLANC-compiled code that calls the shared XMSG-library helper
routines through a nearby pointer/literal pool at **0o51024-0o51052**. (PLANC
data-in-code note: `nd100dis` mis-renders that pool as instructions; the words
0o51024.. are indirect-call targets and string pointers, not opcodes.) Resolved
call targets include helpers at 0o54026, 0o70752, 0o70760, 0o62534, and the two
`MON 104` string-print calls at 0o50744 / 0o50746 that emit the "Server not yet
started..." message.

### 3b. The XROUT service = XSNET (85) - VERIFIED statically
All XMSG monitor calls in XMSG-COMMAND funnel through **three** `MON 200B`
sites; the shared library wrapper is at **0o72314** (M) / 0o72366 (L03):

```
0o72312  LDF *-55   ; XMSG function word
0o72313  LDX *-53   ; parameter-block pointer
0o72314  MON 200    ; <== the one XMSG call site used by all XF* operations
```

The network-server helper that feeds this wrapper loads the XROUT service code
**85 = XSNET**:

```
0o72544  LDA -86,B          ; branch on "start vs stop" selector
0o72545  JAZ =072565
0o72546  SAA 85             ; <== XSNET  "Start/stop gateway (network server)"
```

There is also a per-service constant table (each cell referenced by its helper):
`XSGMG=0o107` @0o77323, `XSCRS=0o120` @0o77433, `XSGIN=0o122` @0o77453,
`XSDLO=0o123` @0o77463 (Define-Local-System), `XSLEK=0o124` @0o77473,
`XSNET=0o125` @0o77503. `[V]` **No `XSCRS`/`XSNAM` cell is on the
START-NETWORK-SERVER path** - it does not create a name.

Cross-version `[V]`: identical structure in L03 (MON 200 @0o72366, `SAA 85`
@0o72620). The flow is version-stable, so L03 is a valid oracle for the M guest.

### 3c. The letter it builds - VERIFIED from the live MON 200B trace
(From `ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md`, device log ~6210,
XFWRI NBYTES=28, big-endian words):

```
0154 0018 FF09 2A58 4D2D 454E 4E53 3000 FD05 454E 4E53 3000 0A02 0001
                FF 09  *  X  M  -  E  N  N  S  0        <- server name *XM-ENNS0 (9 chars)
                          FD 05  E  N  N  S  0          <- ENNS0 (5 chars, RT/friendly)
                                              0A02 0001 <- trailing integer params
```
`FF <len> 2A <name>` is the XROUT server-name descriptor; the name is
**`*XM-ENNS0`**. Sent via `XFGET(2) -> XFWRI(7) -> XFSND(12)` to **port 0**
(receiving port 0x00000000, sending port 4) = the XROUT well-known routing port.

### 3d. Answer to Task 3 (a/b/c)
`[V]` It is **(a) + (b)**: the handler first **resolves/queries** the name
(server-not-running is the *normal* result, handled gracefully with the "will
try to start him now" message), then **builds an XSNET(85) letter and XFSNDs it
to port 0** and interprets the reply. It is **not (c)** - it never issues
`XSCRS`(80)/`XSNAM`(66) to create the name itself.

---

## 4. Pinning the failure (Task 4)

`[V]` The second port-0 exchange (the `XSNET` start of `*XM-ENNS0`) returns
**XRUNN=2**, and the handler's reply-error path prints "Unknown name (of server
or system)". (Confirmed by the live trace ordering: "Server not yet started..."
then the error; and statically the XSNET literal + MON 200B are the only XROUT
start path in the handler.)

`[V]` The error originates in the **genuine SINTRAN XROUT kernel** running in the
guest, not an emulator stub: the C# `MON()` dispatch is `#if false`, so every
MON 200B is serviced by real SINTRAN (see the prior root-cause doc, section 1a).

`[OPEN]` The exact XROUT-internal test that yields XRUNN for *this* XSNET was not
disassembled here. `XMSG-XROUT-L03.BPUN` is an absolute BPUN (bootstrap area
zero-filled at its head) and needs its load base + `XMSG-SYMBOL-L03.SYMB` anchor
before its XSNET handler can be read; that is the one remaining carve if a
byte-level XROUT proof is wanted. What is established is that XSNET's
**name/system resolution** fails - "of server or system" - meaning the referenced
entry is absent from XROUT's name/routing tables.

---

## 5. Conclusion - the missing precondition (Task 5)

`[V]` **Official ND-210580 page-6 bring-up sequence** (the WARM-START file):
```
@ABORT ENNS0
@RT ENNS0
@HOLD 0,0 / @HOLD 5,2
@(UTILITY)XMSG-COMMAND
SET-PRIVILEGED
'  All mapping between COSMOS names and System numbers defined in
'  the XMSG-STARTUP file. (DEFINE-REMOTE-NAME COMMANDS)
START-NETWORK-SERVER ENNS0,,,N
DEF-NETWORK-CONN <remote> ENNS0,,0,0,0,0    (x N)
LIST-NETWORK-SERVERS,,
```

The document is explicit: the **name<->system-number mapping is defined by
`DEFINE-REMOTE-NAME` commands in the separate XMSG-STARTUP mode file, which runs
BEFORE this sequence.** `DEF-NETWORK-CONN` comes *after* START-NETWORK-SERVER and
only attaches already-defined remotes, so it is not the creator either.

`[I]` **Concrete missing step in the emulator harness:** the **XMSG-STARTUP mode
file was never run**. The harness performed only `start-x` (which brings up XMSG
and self-registers `*XM-FIDO`), `@RT ENNS0`, and `START-NETWORK-SERVER`. Without
XMSG-STARTUP's `DEFINE-REMOTE-NAME` (and the local-system definition it implies),
XROUT has no name/system entry for `XSNET` to bind `*XM-ENNS0` to, so it answers
XRUNN.

`[V]` This is consistent with the two independent prior static findings that
neither the controller nor the ENNS0 RT supervisor create the name: ENNS0's
entire XMSG footprint is `XFDUM`+`XFDCT` plus driver-port setup
(`XFOPN`/`XFWDF`/`XFDBK`) - it opens the IOC driver ports (the "Xnser-port /
Xgate" columns in LIST-NETWORK-SERVERS) but issues **no** `XSNAM`/`XSCRS`. The
name/system binding is XROUT's job, seeded by XMSG-STARTUP + the `XSNET` start.

### Fix / correct operator sequence
Run the site's **XMSG-STARTUP** mode file (the `DEFINE-REMOTE-NAME` / local-system
definitions) *before* `START-NETWORK-SERVER`, i.e. reproduce the full page-6
WARM-START sequence rather than jumping straight to `START-NETWORK-SERVER` after
`@RT ENNS0`. `SET-PRIVILEGED` is already satisfied (XFPRV succeeded). No emulator
MON-call change is warranted - the guest XROUT is behaving correctly.

---

## 6. Evidence index (all mem addresses, M build unless noted)

| item | addr / value |
|---|---|
| command dispatch table (name_ptr,handler_ptr pairs) | 0o22334 |
| Start-Network-Server name_ptr -> handler | 0o24066 -> **0o50722** |
| "Server not yet started - will try to start him now" | 0o50617 |
| shared XMSG-library MON 200B wrapper | **0o72314** (L03: 0o72366) |
| `SAA 85` (XSNET service load) | **0o72546** (L03: 0o72620) |
| XROUT service constant cells | XSGMG@0o77323 XSCRS@0o77433 XSGIN@0o77453 XSDLO@0o77463 XSLEK@0o77473 XSNET@0o77503 |
| "Unknown name (of server or system)" (loaded copy) | 0o113352 |
| service codes (decimal, from XMSG-VALUES-L.SYMB) | XSNAM=66 XSGMG=71 XSCRS=80 XSGIN=82 XSDLO=83 XSLEK=84 **XSNET=85** ; XRUNN=2 |

## 7. Tools added (reusable)
- `tools/prog_load.py` - SINTRAN one/two-bank :PROG loader (header VERIFIED),
  returns flat `mem[]` + header info. `load_prog(path)`.
- `tools/progtool.py` - string->addr, xref, word-dump over a loaded :PROG.
- `tools/disprog.py` - disassemble a range of a loaded :PROG via `nd100dis`.
  (`nd100dis` reg/bit-op decode is partial and it does not separate PLANC
  data-in-code pools; use raw words for the pool regions.)

---

## 8. The command parameters and the "Wide Area Network (Y/N)?" flag

The full command is **START-NETWORK-SERVER** (`start-net-server` is the
abbreviation). It takes **four** parameters, documented in the COSMOS Ethernet
Option installation description **ND-210580B p.4**
(`E:\Dev\Ronny\NDInsight\Installation\Installation-Description\ND-210580-02-EN.md`):

```
X-C:START-NETWORK-SERVER
Server name?               ENNS0
Server system name?        => default: local system
Window size?               => number of receive buffers (default 5, max 6 on the
                              old 108630 board)
Wide Area Network(Y/N)? N  => Local Area Network
```

So the trailing `Y`/`N` is the **4th parameter** answering "Wide Area Network
(Y/N)?" - it declares whether the server serves a **wide-area network** (public
data network, X.21/X.25) or a **local area network**. `[V]` It has **nothing to
do with firmware download, clearing, or restart** - it is a routing/network-type
classification only.

`[V]` The COSMOS X.25 Operator's Guide **ND-30.034.01 p.84** confirms the same
four-parameter list, and notes that for the X.25/X.21 servers the default is
**Y** - those servers run over the public data network, e.g.
`START-NETWORK-SERVER,X21NS,,,Y`.

### 8a. What the flag does downstream (manual / symbol-verified)
`[V]` It marks the server's link as a **wide-area gateway**:
- The XMSG symbol table has **`XL5WA = 14` (% Wide area gateway, i.e. costs money)**.
- The **XSNSI** service reports "Network type: 0 = local area, 1 = wide area".
- XROUT treats WAN destinations differently: e.g. the Send-Letter variant
  restricted to the local network fails for WAN-only systems, and **LIST-ROUTES**
  shows "WAN - using Wide Area Network (PDN)".

`[I]` That the **`Y` answer is what sets `XL5WA`** is the one inferred step; the
rest of 8a is quoted from the manuals/symbols.

### 8b. For ENNS0 the answer is N (important)
`[V]` **Every ND source gives `N` for ENNS0.** The official warm-start sequence
is **`START-NETWORK-SERVER ENNS0,,,N`** - see ND-210580-02 p.6 (the page-6
WARM-START file quoted in section 5 above), the **SINTRAN III System Supervisor
ND-30.003.007**, and the Norwegian driftsansvarlig handbook.

So **`start-net-server,enns0,,,Y` declares the Ethernet server a
wide-area / public-network gateway - contrary to the documented recipe for the
Ethernet option.** If that `Y` came from a script or log, it is worth checking
whether it was intentional: it changes the **routing / cost classification** of
everything reached via that server (marks the link WAN / "costs money" via
`XL5WA`), though it does **not** affect the server's basic operation (ENNS0 still
starts either way - the value is a network-type attribute, not a start gate).

Cross-reference: the working bring-up captured in the harness used `,,,Y`
(section 5's recipe uses `,,,N`); either starts the server, but `N` is the
correct classification for the LAN Ethernet option.

---

**Findings file:** `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\XMSG-COMMAND-START-NETWORK-SERVER-DECODE-2026-07-23.md`
