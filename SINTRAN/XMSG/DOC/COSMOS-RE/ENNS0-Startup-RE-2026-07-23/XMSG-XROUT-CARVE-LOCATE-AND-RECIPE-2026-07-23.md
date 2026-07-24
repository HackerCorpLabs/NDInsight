# XMSG + XROUT Reverse-Engineering: LOCATE + PLAN (2026-07-23)

Read-only LOCATE+PLAN. Goal: be able to carve/trace the SINTRAN XMSG+XROUT internals
and the XMSG-COMMAND program to prove WHICH call creates the `*XM-ENNS0` name and WHEN,
and whether DEFINE-NETWORK-CONNECTION is what creates it.

Every item tagged **VERIFIED** (file opened / bytes or strings seen) or **INFERRED**
(reasoned). Nothing on the wire is guessed.

---

## TL;DR verdict

**The shortest path is NOT to carve anything first.** Three artifacts already answer
most of the question and were opened during this task:

1. **`XMSG-VALUES-L.SYMB`** gives the exact XROUT service codes WITH descriptions
   (VERIFIED, see section 3): `XSNAM=66` "Give name to this port", `XSCRS=80`
   "Create service (name, init no of SP's)", `XSNET=85` "Start/stop gateway
   (network server)", `XSGMG=71` "Get magic number from name", `XRUNN`/`XRUNM`
   unknown-name/magic.
2. **`xmsg-command-l03.prog`** (the XMSG-COMMAND program image) literally contains the
   command strings `Define-Network-Connection`, `Define-Network-Local-Endpoint`,
   `Define-Network-Remote-Endpoint`, `Define-Network-Direct-Connection`,
   `Define-Network-Remote-Groupnumber`, `Start-Network-Server`, plus 20x `XROUT` and
   `magic` (VERIFIED via string scan). This is the DEFINE-NETWORK-* handler binary.
3. A prior-session root-cause doc already proved (from the live MON 200B trace) that
   `*XM-ENNS0` is **never** registered: `@rt enns0` issues only driver-port calls
   (XFOPN/XFWDF/XFDBK), **no XSNAM/XSCRS to port 0**, and only `*XM-FIDO` self-registers.
   See `ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md` (scratchpad).

So the disassembly/carve is confirmatory: prove that `Define-Network-Connection`
(or another Define-Network-*) is the handler that issues `XSCRS`/`XSNAM` for
`*XM-ENNS0`, which the ENNS0 bring-up harness never ran.

---

## 1. Inventory of XMSG/XROUT sources + binaries (Task 1) - VERIFIED (paths opened)

### 1a. Repo: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\`
- `DOC\` - large doc set (wire protocol, MON 200B API, operator cmds). Directly relevant:
  - `DOC\XMSG-COMMAND-REFERENCE.md` - operator command family incl. the Define-Network-* list.
  - `DOC\XMSG-API.md` - MON 200B function/service map.
  - `DOC\XMSG-PROTOCOL.md` (132 KB) - envelope/XROUT dispatch.
  - `DOC\ENNS0-XROUT-DISASSEMBLY-HANDOFF.md` - prior handoff naming the exact RE targets
    (ENCOSE0-DUMP.BPUN, encos-mon-*.prog) and Ghidra state.
  - `DOC\ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md`.
- `SRC\` - C# XMSG protocol library + tests (Xmsg.Protocol/Node/Live/Servers/...). This is
  the emulator-side reimplementation, NOT the ND binary; the "magic stub" lives in
  `SRC\Xmsg.Live\XmsgNode.cs` and is the SEPARATE HDLC multi-node path, not the local ENNS0 path.
- `XMSG-PL-VALUES-M.INCL`, `XMSG-VALUES-M.SYMB`, `xmsg-constants.json` - **version M** symbol
  material (matches the running guest, see version note). VERIFIED present.

### 1b. NPL source for the XMSG/XROUT kernel - **NOT PRESENT** (VERIFIED)
- `SINTRAN\NPL-SOURCE\NPL\` is the resident SINTRAN III kernel only (SCSI, HDLC, TAD, N500,
  terminal, perf...). There is **no XMSG/XROUT kernel NPL file**. The two grep hits
  (`IP-P2-1.NPL`, `PH-P2-OPPSTART.NPL`) are incidental references, not the XROUT source.
- XMSG is a **separately-loaded product** (loaded onto segments at install, see 1d), so its
  source is not in the OS NPL tree. What we DO have is the linker symbol lists:
  - `SINTRAN\NPL-SOURCE\SYMBOLS\L07\XMSG-SYMBOL-LIST.SYMB.TXT` (VERIFIED: has
    `XSNAM=000102`(=66d), `XSCRS=000120`(=80d), `XSNET=000125`(=85d), `XRUNN=000002`).
  - `SINTRAN\NPL-SOURCE\SYMBOLS\M06\XMSG-SYMBOL-LIST.SYMB.TXT` (version M).

### 1c. The XMSG distribution binaries - **PRESENT on F:** (VERIFIED, directory listed)
Full XMSG L03 kit at `F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\xmsg\`:
| File | Size | What it is |
|------|------|-----------|
| `xmsg-command-l03.prog` | 90112 | **XMSG-COMMAND program** (DEFINE-NETWORK-* / START-NETWORK-SERVER handler). Primary carve target. |
| `xmsg-xrou-cx-l03.bpun` | 80450 | **XROUT kernel** (CX build) - the routing/naming server BPUN. |
| `xmsg-xrou-nx-l03.bpun` | 80076 | XROUT kernel (NX build). |
| `xmsg-kern-cx-l03.bpun` | 47666 | XMSG kernel (CX). |
| `xmsg-kern-nx-l03.bpun` | 25136 | XMSG kernel (NX). |
| `xmsg-fido-l03.prog` | 290816 | XMFIDO (the thing that self-registers `*XM-FIDO`). |
| `xmsg-in-l03.prog` | 180372 | XMSG install program. |
| `xmsg-library-l03.brf` | 31710 | XMSG relocatable library (BRF) - has ENTR symbols. |
| `xmsg-01-l03.xcom` | 66798 | XMSG :XCOM. |
| `xmsg-load-cx/nx-l03.mode`, `xmsg-startex-l03.{mode,batc}` | - | load/startup MODE scripts. |

Symbol tables (VERIFIED present, opened one):
- `...\FLOPPY\xmsg\xmsg-values-l.symb` (== repo `XMSG-VALUES-L`) - **service codes with comments**.
- `xmsg-symb-cx-l03.symb`, `xmsg-symb-nx-l03.symb`, `xmsg-poftabs-l03.symb`,
  `xmsg-systabs-l03.symb`, `xmsg-pl-values-l.incl`.
- Copies also at `F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\`.

### 1d. How XROUT is loaded (VERIFIED from `xmsg-load-nx-l03.mode`)
The MODE script does `LOAD-BINARY (USER)XMSG-KERN..:BPUN` / `XMSG-XROUT` / `XMSG-FIDO`
each onto its own SEGMENT (segment 33x range), then defines the RT programs. So in a running
image the XROUT code lives in a loaded segment, not resident SINTRAN. => carvable from the
running image, but the standalone BPUN above is cleaner.

### 1e. Version note (IMPORTANT caveat) - VERIFIED/INFERRED
- Running guest banners: `XROUT: XMSG version M00 (88.03.25)`, `XMSG command program
  (210373M) of 88.08.18` => **version M**.
- The BINARIES on F: are **L03** (`210373L03`). M-version **symbols** exist
  (`SYMBOLS\M06\`, repo `XMSG-VALUES-M.SYMB`) but **no M-version .prog/.bpun** was found on F:.
- INFERRED: L03 vs M service-code layout is stable for the codes we care about (XSNAM/XSCRS/
  XSNET are architectural), so L03 binaries are a valid oracle for the FLOW. But for
  byte-exact addresses in the running guest, carve the running M image (option C).

---

## 2. XROUT service codes (the answer key) - VERIFIED from `xmsg-values-l.symb`
```
XSNAM=66   % Give name to this port
XSGMG=71   % Get magic number from name (privileged)
XSCRS=80   % Create service (name, init no of SP's)
XSNET=85   % Start/stop gateway (network server) (privileged)
XSCMG=2    % Check magic number
XFM2P=22   % Magic number to system number and port id
XFP2M=23   % Port to magic number
XEIMA=-19  % Invalid magic number
XRUNM=7    % Unknown magic number
XRUNN=2    % Unknown name   (this is the error we get)
```
INFERRED mapping to the failure: creating the named service `*XM-ENNS0` is `XSCRS`(80)
and/or `XSNAM`(66) sent to XROUT port 0. START-NETWORK-SERVER is `XSNET`(85), which
first resolves the name (`XSGMG`/lookup) and returns `XRUNN`(2) because no `XSCRS` ever ran.

---

## 3. Carvability assessment (Task 2 + 3)

### 3a. XMSG-COMMAND `.prog` - CARVABLE, .prog loader is the gap - INFERRED
- `xmsg-command-l03.prog` = 90112 bytes. `scanprog.py` (BE) already finds device/structure
  words, i.e. it is a normal ND-100 :PROG: ~256-word BE header then raw 16-bit BE words
  (same shape as `encos-in-b01.prog` per the task note; a dedicated .prog loader is still TODO).
- The command strings are in cleartext in the image (VERIFIED string scan). Each
  operator-command dispatch entry in a :PROG points its handler at the routine that
  builds the MON 200B message. So the recipe (below) is: locate `Define-Network-Connection`
  string offset -> find the command-table entry referencing it -> disassemble the handler
  -> look for the `XSCRS`(80o=`0120`)/`XSNAM`(66o=`0102`) literal loaded before the
  MON 200B (`153 200` = `0153200`) `XFSND`-to-port-0 sequence.

### 3b. XROUT kernel `.bpun` - CARVABLE - INFERRED
- `xmsg-xrou-cx/nx-l03.bpun` are BPUN (bootable/absolute). The XROUT SERVER side (the code
  that RECEIVES `XSCRS`/`XSNAM` and returns `XRUNN`) lives here. Disassemble to see the
  service dispatch on the first message byte and the name-table insert/lookup.
- Symbols to anchor from: `xmsg-symb-cx-l03.symb` / `xmsg-poftabs-l03.symb`.

### 3c. Running SINTRAN image carve - POSSIBLE but not needed first - VERIFIED tooling exists
- `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\` (carve.py, EXTRACTING-SEGMENTS.md,
  EXTRACTING-RESIDENT-CODE.md, per-version `versions\*\inputs\list-segment.txt`) + the
  `sintran-carving` skill can extract loaded segments from a `LIST-SEGMENT`/dump.
- XROUT is a LOADED SEGMENT (33x), NOT resident SINTRAN (VERIFIED from the load MODE). So
  it is carvable from the running M image, and this is the ONLY way to get the exact
  version-M bytes/addresses. But since we already have the L03 standalone BPUN and the flow
  is what matters, carve the running image only if L03-vs-M divergence is suspected.

### 3d. Existing scratchpad tooling (Task 2)
`C:\Users\ronny\AppData\Local\Temp\claude\...\scratchpad\`:
- `brf_link.py` - BRF linker (for `xmsg-library-l03.brf` -> symbols/addresses).
- `nd100dis.py` - ND-100 disassembler (feed it the .prog/.bpun word image).
- `m68kdis.py` - 68K (controller side, not needed here).
- `scanprog.py` - endian/format probe (already run on the .prog).
- **.prog loader is TODO**: strip the ~256-word BE header, then emit raw BE words; the
  `encos-in-b01.prog` format note applies. For a first pass you can skip precise header
  parsing and just disassemble from candidate offsets near the command-string references.

---

## 4. The RECIPE (concrete steps)

### Step 0 - already done (read existing artifacts) - fastest, do first
- Read `xmsg-values-l.symb` (service codes, section 2 - DONE).
- Read `SINTRAN\XMSG\DOC\XMSG-COMMAND-REFERENCE.md` for the Define-Network-* semantics.
- Read `ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md` (proves `*XM-ENNS0` never
  registered; only driver-port calls during `@rt enns0`).

### Step 1 - carve XMSG-COMMAND to pin the DEFINE-NETWORK-CONNECTION handler (confirmatory)
1. `python scanprog.py xmsg-command-l03.prog` (done: BE, normal :PROG).
2. Find byte offsets of the strings `Define-Network-Connection` and `Start-Network-Server`
   (grep -abo). ND :PROG strings are typically packed 2 chars/word.
3. Search the word image for the command TABLE entry that points at each string offset
   (the offset/2 as a word address, or a pointer near it).
4. Disassemble the referenced handler with `nd100dis.py`. In the handler look for:
   - a literal `0120` (XSCRS=80) or `0102` (XSNAM=66) or `0125` (XSNET=85) being loaded,
   - the message build (XFWRI) and the `MON 200` (`0153200`) `XFSND` with receiving port 0.
5. RESULT: proves which Define-Network-* command issues `XSCRS`/`XSNAM *XM-ENNS0*` -> answers
   "what call creates `*XM-ENNS0` and when".

### Step 2 - (optional) carve XROUT kernel to see the server side
- Disassemble `xmsg-xrou-cx-l03.bpun` with `nd100dis.py`, anchor with `xmsg-symb-cx-l03.symb`,
  find the service dispatch that returns `XRUNN`(2) on name-not-found and the insert path
  for `XSCRS`/`XSNAM`.

### Step 3 - (only if L03 != guest M matters) carve the running image
- Use `sintran-segment-carver` + `sintran-carving` skill on the running M image's
  `LIST-SEGMENT` to pull the XROUT segment (33x) and compare.

### Step 4 - definitive live confirmation (grounded oracle)
- DAP/cpu-trace the MON 200B calls during `Define-Network-Connection` (if run) and
  `START-NETWORK-SERVER`; at each `XFSND` to port 0 decode the first message byte
  (service code) + name string + reply error byte. Confirms `XSCRS *XM-ENNS0*` presence/absence
  with certainty. (The MON_200_XMSG_TRACE decoder already prints the buffers.)

---

## 5. Ranked recommendation for "what call creates *XM-ENNS0 and when"

1. **(a) Read existing NPL/DOC + symbols - HIGHEST, near-zero cost.** `xmsg-values-l.symb`
   + `XMSG-COMMAND-REFERENCE.md` already name the mechanism (`XSCRS`/`XSNAM` create the
   named service). Combined with the root-cause doc it is 90% answered: the harness ran
   `@rt enns0` + `START-NETWORK-SERVER` but **none of the Define-Network-* commands**, and
   ENNS0 self-registered nothing.
2. **(b) Carve `xmsg-command-l03.prog` (Step 1) - RECOMMENDED confirmatory.** Cheapest carve
   (standalone :PROG, strings in clear, tooling ready) that positively identifies the
   handler emitting `XSCRS *XM-ENNS0*`. Do this to prove DEFINE-NETWORK-CONNECTION is the creator.
3. **(d) Live MON 200B service-byte trace (Step 4) - best ground truth,** run whenever the
   harness is up; disambiguates "handler exists but wasn't invoked" vs "invoked but failed".
4. **(c) Carve the resident XROUT segment - LAST,** only if version-M byte-exactness is needed;
   more work than the standalone BPUN for the same information.

**Bottom line:** you do not need to fix any emulator MON stub. The name `*XM-ENNS0` is created
by an `XSCRS`(80)/`XSNAM`(66) message to XROUT port 0, issued by the XMSG-COMMAND
Define-Network-* handler (INFERRED: `Define-Network-Connection`), which the ENNS0 bring-up
sequence never executed. Step 1 (carve `xmsg-command-l03.prog`) proves the exact command;
Step 4 (live trace) proves it at runtime.

---

## Full paths referenced
- Repo XMSG: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\` (DOC\, SRC\, XMSG-VALUES-M.SYMB, xmsg-constants.json)
- Symbol lists: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\{L07,M06}\XMSG-SYMBOL-LIST.SYMB.TXT`
- XMSG L03 kit: `F:\ND\SINTRAN-K05-XMSG-2026\FLOPPY\xmsg\` (xmsg-command-l03.prog,
  xmsg-xrou-cx/nx-l03.bpun, xmsg-kern-cx/nx-l03.bpun, xmsg-fido-l03.prog, xmsg-library-l03.brf,
  xmsg-values-l.symb, xmsg-symb-cx/nx-l03.symb, xmsg-poftabs/systabs, xmsg-load-nx-l03.mode)
- Symbols dir: `F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\XMSG-VALUES-L.SYMB`
- Carver: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\` (carve.py, EXTRACTING-SEGMENTS.md)
- Scratchpad tooling: `C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Ronny-NDInsight\b17a7474-33c0-4d7f-b9cb-f921c3ad419b\scratchpad\` (brf_link.py, nd100dis.py, scanprog.py, m68kdis.py)
- Prior root cause: `...\scratchpad\ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md`
- Disassembly handoff: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\ENNS0-XROUT-DISASSEMBLY-HANDOFF.md`
