# Handoff: Disassemble the ENNS0 / XROUT registration path

**For:** another LLM/engineer to reverse-engineer the ND-100-side ENCOS Ethernet
server so we can fix why `START-NETWORK-SERVER ENNS0` fails with
"Error in communicating with XROUT / Unknown name (of server or system)".

**Date:** 2026-07-07. Written after a long session that fixed the emulated Ethernet
controller end-to-end; the remaining failure is in the XMSG/XROUT layer.

---

## 1. Objective (what to find)

`START-NETWORK-SERVER ENNS0` must make the ENNS0 network server register a name
with **XROUT** (the XMSG routing/naming server) so it appears in `list-net-ser` /
`list-ver`. It currently fails. We need the exact ND-100 code path:

1. What ENNS0 (and/or the ENCOS monitor program) sends to XROUT during startup -
   specifically the **`getMagic` subfunction** and the magic-number handshake.
2. What response it expects, and which XMSG/XROUT error it actually hits.
3. Enough detail to fix the **emulator's XMSG/XROUT side** so `getMagic` returns a
   valid magic number instead of being stubbed/rejected.

**Strong hypothesis (confirmed by strings, not yet by disassembled call sites):**
the failure is the known "our node stubs the magic number" issue. The XROUT
protocol here uses a `getMagic` subfunction; the emulated node rejects it
(XEIMA -19 invalid magic / "Unknown magic number"), so the name never registers
and lookups return "Unknown name". Confirm by disassembly and pin down the exact
message format XROUT `getMagic` expects.

---

## 2. Current state / what is already fixed (do NOT re-investigate the card)

The MC68000 Ethernet-II CONTROLLER emulation is now correct end-to-end. Session
fixes (all in `E:\Dev\Repos\Ronny\RetroCore`):
- Ethernet-II status register mapping (read offset 0/2 = status, not a data reg).
- SCIP doorbell latch (68K->ND-100 interrupt survives until interrupts enabled).
- Removed a bogus MFP-init-on-every-reset workaround.
- **MC68000 STOP instruction never woke on interrupt** (WAIT-clear inverted in
  `Cpu68K.Interrupts.cs`) - core bug; the ENCOS firmware STOPs waiting for the
  ND-100 and never resumed.
- LANCE (Am7990) register byte-lane swap for the big-endian 68000 host.
- LANCE `DmaIn`/`DmaOut` callbacks wired (init block / ring descriptors).

Result: the 68K firmware boots, initializes the LANCE (IDON set, MAC read), runs
its main loop, and exchanges SCIP messages with the ND-100. The card is done.
The remaining wall is purely the ND-100 XMSG/XROUT registration.

---

## 3. Files (all in `Installation/Communication/Ethernet/x/`)

| File | What it is |
|------|-----------|
| `encos-err-i-b01.brf`, `encos-err-ii-b01.brf` | BRF object of the ND-100 supervisor (ENNS0 = ENCOSE0). 174 units, PLANC. Symbols: MAIN `ENNS0`, ENTR `POSUERR/READPIO/SEGLOAD/START_P/STOP_PI/RES_SLO/REL_SLO/SEND_KI/REC_KIC/INT2GET`. |
| `encos-in-b01.prog` (183K) | ENCOS install program (loads server+supervisor onto segments). |
| `encos-mon-i-b01.prog`, `encos-mon-ii-b01.prog` (40K) | **ETHERNET COSMOS SERVER monitor/trace** program. Contains the `XROUT ID / subfunction getMagic / MONTR ID` protocol strings and "FROM XROUT NO SUCH PORT / UNKNOWN SYSTEM". `-i` = Ethernet I (old ND-108630), `-ii` = Ethernet II (new ND-110063). **Primary RE target for getMagic.** |
| `encos-ser-i-b01.dseg` (130K) | Server data segment ("COSMOS Ethernet I - IOC Server" / "XMSG Server"). |
| `po-pwrfail-a00.prog` (30K) | Contains the **full XMSG/XROUT error-message catalog** (see section 5). |
| `ue-ermsg-en-b03.err` (106K) | UE error-message catalog. "PO error not found in UE system:NNN" means code NNN is absent here. |
| `encos-ser-b0..b3-b01.bpun` | The 4-bank MC68000 firmware (the CONTROLLER side - already REd, working). |
| `stripped/encos-ser-all-banks-68k.bin` | Concatenated 68K firmware, loaded in Ghidra as 68000:BE:32 (already annotated). |
| `stripped/README.md`, `stripped/Ghidra-Analysis.md` | Prior 68K firmware analysis notes. |

`.prog` = ND-100 :PROG executable, `.brf` = relocatable object, `.dseg` = data
segment, `.bpun` = bootable punched tape (BPUN). Format specs:
- `SINTRAN\File-Formats\PROG-FILE-FORMAT.md`
- `SINTRAN\File-Formats\BRF-FILE-FORMAT.md`
- `SINTRAN\File-Formats\BRF-GHIDRA-LOADER-HANDOFF.md` (how to load BRF into Ghidra)

---

## 4. Ghidra state (as of this handoff)

Three programs are OPEN in the connected Ghidra (via the `ghidra` MCP tools):
1. `encos-ser-all-banks-68k.bin` - 68000:BE:32 - the CONTROLLER firmware (annotated).
2. `RAM_00003BAA.BIN` - 68000:BE:32 - a TPE test-firmware RAM dump (annotated;
   dispatch loop `pioc_cmd_dispatch_loop`, `stc_timer_test_cmd12`, etc.).
3. `ENCOSE0-DUMP.BPUN` - **ND-100:BE:16** - the linked absolute image of the ENNS0
   supervisor (`BINARY-DUMP "(UTILITY)ENCOSE0-DUMP:BPUN",ENCOSE0,0,47777`). Path
   `F:\RC\RonnyTest\HDLC1\ENCOSE0-DUMP.BPUN`. **This is the ND-100 code to RE.**

**IMPORTANT:** ENCOSE0-DUMP is NOT auto-analyzed - only the `START @ ram:0000` stub
is defined, so `xrefs` return nothing until you disassemble. The ND-100 SLEIGH
processor module IS present (language `ND-100/big/16/default`). You must
disassemble code regions manually (or run auto-analysis) before xrefs/decompile
work. Known strings already located in ENCOSE0-DUMP:
- ram:227a `" XROUT   ID  "`, then `" subfunction getMagic"`, `defDTE`, `clrDT`,
  `"**SUBFUNCTION UNKNOWN**"`, ram:22be `" MONTR   ID  "` - a message/format table.
- ram:2ecf `" PO error not found in UE system:"`
- ram:2f90 `"XMSG error not found in UE system :"`

(The same getMagic/XROUT strings live in `encos-mon-*.prog` too - either target
works; the monitor prog may be easier since it is a standalone :PROG.)

---

## 5. XMSG / XROUT error taxonomy (decoder - from po-pwrfail-a00.prog)

The exact runtime error is **"Unknown name (of server or system)"**. Magic-number
errors in the same catalog (these are what a getMagic stub triggers):
- `Invalid magic number or destination port closed`
- `Unknown magic number`
- `XMSG crash : Illegal port address in the creation of magic number`

Other relevant entries: `This server is not running`, `Unknown RT-program name`,
`Netserver: remote system is not defined`, `Netserver: network not available`,
`Netserver: internal server error`, `XMSG generated without gateway. No interface
to Network Server`, `Remote system not defined in routing tables`,
`No inter-system XMSG`. Use these to classify whatever error code the disassembled
path produces.

---

## 6. What to disassemble and find (concrete tasks)

1. In `ENCOSE0-DUMP.BPUN` (or `encos-mon-ii-b01.prog`): disassemble from the entry
   and from the code around the `getMagic` / `XROUT ID` strings. Find the routine
   that builds and sends the XROUT `getMagic` request.
2. Identify the **XMSG monitor call** used (ND-100 XMSG is `MON 200B` = XMSGFunction;
   see `Reference-Manuals\ND-860228-2-EN SINTRAN III Monitor Calls.md` and
   `Developer\MON\Monitor Calls.md`). Determine the XMSG function code / message
   layout for the getMagic request and the expected response (the magic number).
3. Trace what ENNS0 does with the magic number: it should then register its server
   name with XROUT (a second XROUT subfunction). Find that name-registration call
   and the name it registers ("ENNS0"? a server number 0-3?).
4. Determine the failure branch: which XMSG/XROUT status code leads to the
   "Unknown name" / getMagic failure. Map it to section 5.
5. Deliver: the message format(s) the emulated XROUT must implement for `getMagic`
   and name-registration so ENNS0 can register successfully.

The FIX will then be on the emulator's XMSG/XROUT side (Ronny's
`Xmsg.Live.Runner` / retrocore node), not in the Ethernet card.

---

## 7. Tooling and reference material

- **Ghidra MCP tools** (`ghidra` server): `list_programs`, `disassemble`,
  `get_disassembly`, `get_code` (decompiler), `list_functions`, `list_strings`,
  `xrefs`, `rename_symbol`, `set_comment`, etc. Target
  `program_name="ENCOSE0-DUMP.BPUN"`.
- **ND-100 disassembly skill:** `nd100-ghidra` (RE pitfalls: PLANC calling
  conventions, data-before-code, self-modifying code, TPE-MON structure) and
  `nd100-asm` (instruction set: MON=`153 0nn`, IOX, MOVEW, addressing modes,
  word-addressed memory).
- **BRF tooling:** `SINTRAN\File-Formats\BRF-GHIDRA-LOADER-HANDOFF.md` includes a
  full Python BRF linker (emulates the ND Relocating Loader) that produces an
  absolute image with symbols from `encos-err-*.brf`. Use it to get ENNS0's symbol
  names/addresses to annotate ENCOSE0-DUMP (correlate by the ENTR/MAIN symbols:
  ENNS0, POSUERR, READPIO, SEGLOAD, START_P, STOP_PI, RES_SLO, REL_SLO, SEND_KI,
  REC_KIC, INT2GET, and the UEIE* library routines).
- **XMSG docs (repo):** `SINTRAN\XMSG\DOC\` (wire protocol, MON 200B API, operator
  commands), `SINTRAN\XMSG\` constants + `xmsg-constants.json`. The `xmsg-decode`
  skill covers the XMSG-over-HDLC envelope, XROUT server/service dispatch,
  secure-ACK, reachability/resync.
- **Known prior context (Ronny's notes):** the emulated node "stubs the magic
  number"; `list-systems` hangs for the same reason; subtype 0x07 = network
  error/reject, flags2 = negative XE* code (XEIMA -19 invalid magic, XENSE -34
  seq error). This getMagic finding is almost certainly the same root cause.

---

## 8. The one-paragraph summary for the next LLM

The emulated ND Ethernet-II controller now works (68K firmware boots, LANCE inits,
main loop runs, SCIP to ND-100 OK). `START-NETWORK-SERVER ENNS0` still fails with
XMSG error "Unknown name (of server or system)". The ND-100 ENCOS server talks to
XROUT using a `getMagic` subfunction (strings confirm it in `ENCOSE0-DUMP.BPUN` and
`encos-mon-*.prog`), and the emulated node stubs the magic number, so registration
fails. Disassemble the ND-100 getMagic/XROUT path (ENCOSE0-DUMP, ND-100:BE:16, in
Ghidra - needs manual disassembly, not yet auto-analyzed) to extract the exact
getMagic request/response and name-registration message formats, so the emulator's
XMSG/XROUT side can answer getMagic with a valid magic number and let ENNS0 register.
