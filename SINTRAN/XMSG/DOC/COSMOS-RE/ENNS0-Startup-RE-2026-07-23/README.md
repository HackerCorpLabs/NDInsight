# ENNS0 startup / XROUT "Unknown name" investigation - 2026-07-23

Reverse-engineering of why `START-NETWORK-SERVER ENNS0` fails with
`XMSG Routing/Naming error: Unknown name (of server or system)` on the emulated
ND Ethernet II (PCB 3094 / ND-110063) controller, and what the ND-100 ENNS0 supervisor
does when it talks to the controller.

This folder holds the ND-100-side (ENNS0 supervisor) analysis + the reconstructed
disassembly tooling. The 68K firmware side lives with the firmware docs at
`Installation\Communication\Ethernet\x\stripped\docs\` (see ND_EthernetII_68000_Firmware_*
and ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md).

Convention throughout: `[V]` = VERIFIED (decoded bytes / read source / manual quote);
`[I]` = INFERRED.

---

## Headline conclusion (state at 2026-07-23)

- The ND Ethernet II CONTROLLER emulation is correct: in a live harness trace the 68K
  boots, posts PRKEY @0x0404, reaches monitor postbox MON_CODE=3 (READY) + STARTED_FLAG=1,
  and the LANCE fully initializes (IDON, MAC 08:00:26:64:00:00, 128 RX buffers) and starts
  (RX=ON TX=ON). The ND-100<->controller path works.
- ALL controller I/O happens during `@RT ENNS0`. `START-NETWORK-SERVER` does ZERO controller
  I/O and returns "Unknown name" immediately.
- Root cause is one level above the controller: ENNS0 never issues the XROUT name-creation
  (`XSNAM`=66 / `XSCRS`=80) for the service `*XM-ENNS0`, so `START-NETWORK-SERVER`'s
  name-resolve (`XSNET`=85) returns `XRUNN`=2 "Unknown name". MON 200B is handled by REAL
  SINTRAN (the emulator C# handler is dead `#if false`), so this is genuine guest behavior,
  NOT an emulator stub.
- OPEN: the exact reason ENNS0 stops before `XSNAM`. Two candidates under active decode:
  (1) an undecoded ENNS0<->firmware command exchange - OPCOM `SUBFUNCTION=5`, firmware
  replies MON_CODE=1 / PARAM=0x1E(30) - that may be a status query ENNS0 mis-reads;
  (2) an XMSG version gate (ENNS0 is COSMOS Ethernet B01/1987, running XMSG M00/1988).
  A missing COSMOS config step is NOT the cause (official ND-210580 p6 sequence is
  `@RT ENNS0` then `START-NETWORK-SERVER ENNS0`; DEFINE-NETWORK-CONNECTION comes AFTER and
  only binds already-defined remotes).

---

## Findings docs (reading order)

1. `ENNS0-POLL-FINDINGS.md` - first pass: ENNS0 has ZERO IOX/IOXT; all controller I/O is via
   the kernel PIOCM (MON 255B) driver; MAIN=ENNS0 @octal 031655; the 10 PIOCM wrappers.
2. `ENNS0-PIOCM-START-FINDINGS-2026-07-23.md` - the kernel PISTA (T=6) start path: reset+
   initiate, then busy-poll PIOC word 1002B (68K DRAM 0x0404) for PRKEY=052163B, 3-sec
   timeout, then write MPIOC=5 + ring PWCR=11B. START_P @033124.
3. `ENNS0-XROUT-UNKNOWN-NAME-ROOTCAUSE-2026-07-23.md` - the name `*XM-ENNS0` is written via
   XFWRI + XFSND to XROUT port 0; XROUT replies XRUNN because no XSCRS ran; MON 200B is real
   SINTRAN, not a stub.
4. `XMSG-XROUT-CARVE-LOCATE-AND-RECIPE-2026-07-23.md` - XROUT service codes from
   xmsg-values-l.symb (XSNAM 66 / XSCRS 80 / XSNET 85 / XSGMG 71 / XRUNN 2); location of the
   XMSG-COMMAND / XROUT binaries on F: (xmsg-command-l03.prog etc.); carve recipe.
5. `ENNS0-POSU-XROUT-ERROR-GATE-2026-07-23.md` - the POSU error catalog @014642-015250
   ("No answer from interface / Interface not started / Check if RTCOMMON…"); "Error
   communicating with XROUT" is a paraphrase; the controller gate is PRKEY, not the IDENT
   interrupt latch.

Sibling firmware-side doc (in the Ethernet docs dir):
`Installation\Communication\Ethernet\x\stripped\docs\ENNS0-PRKEY-FIRMWARE-FIXPOINT-2026-07-23.md`
- the 68K firmware writes PRKEY from exactly one instruction (0x1CF4) inside reset_entry.

DONE (2026-07-23 static-decode agents):
- FIRMWARE-SUBFUNCTION-5-DECODE-2026-07-23.md (filed to `Installation\Communication\Ethernet\
  x\stripped\docs\`) - VERDICT: the OPCOM SUBFUNCTION=5 exchange is a NORMAL SUCCESS/ACK, not
  an error. Handler @0x1B00 dispatches SUBFUNCTION via a jump table @0x512; SUB=5 -> 0x1C48
  writes MON_CODE=+1 (ACK) + SCIP. PARAM=0x1E(30) is a hardcoded constant (= the OPCOM vector
  number), NOT a version/size/error. REQUEST=0 -> D0=-5 = benign "no request pending" (stale
  doorbell). MON_CODE is SIGNED: +1=ACK, -2/-4/-5=errors. => the controller-comm exchange does
  NOT make ENNS0 abort. This RULES OUT the last controller-side suspect.

- ANNOTATED-ENNS0-DISASSEMBLY-2026-07-23.md - DECISIVE, statically-provable: ENNS0's ENTIRE
  XMSG footprint is just TWO MON 200B: `030230 SAT 0;MON 200`=XFDUM (liveness "is XMSG up?")
  and `030233 SAT 1;MON 200`=XFDCT (disconnect). The name-registration functions
  (XFGET/XFWRI/XFSND to XROUT port 0 carrying XSNAM 66 / XSCRS 80) are NOT PRESENT in ENNS0 at
  all - opcode-exact (only 2 MON 200B total). So ENNS0 does NOT and CANNOT register *XM-ENNS0.
  The runtime XFPRV/XFOPN/XFWDF/XFDBK are SINTRAN's RT-driver-port setup, NOT ENNS0. Rules
  out controller-SUBFN5 (succeeded) and version/getMagic (no port-0 send possible).

CONCLUSION (both static agents): the ND Ethernet II CONTROLLER emulation AND the ENNS0
supervisor are both cleared. `*XM-ENNS0` is created by the XMSG-COMMAND `START-NETWORK-SERVER`
/ `Define-Network-*` handler, NOT by ENNS0. START-NETWORK-SERVER returning XRUNN(2) is that
handler failing to resolve/create the server - a config/procedure issue in the XMSG-COMMAND
flow. NEXT (only remaining unknown): disassemble the XMSG-COMMAND program.

## RECONCILED ROOT CAUSE (2026-07-23, live RT-descriptor + XROUT decode)

Two agents + live evidence now converge. The AUTHORITATIVE root cause is the LIVE
`LIST-RT-DESCRIPTION ENNS0`: **ENNS0 hangs in a SINTRAN device INPUT wait on logical unit
2240B** (RTWT, resume-P 030440, datafield 103356B), after `MON 124 PRSRV` force-reserving a
device. It never completes startup, so it never registers `*XM-ENNS0`. See
`ROOT-CAUSE-ENNS0-HANGS-INPUT-WAIT-LU2240B-2026-07-23.md`.

Everything else is DOWNSTREAM of that hang:
- `XROUT-XSNET-XRUNN-CONDITION-DECODE-2026-07-23.md` (VERIFIED via new bpun_load.py; checksums
  exact): XSNET(85) does NOT emit XRUNN. XRUNN=2 is a **name-table MISS** from the kernel
  lookup CHNAM @0o20161, surfaced by getMagic (RSGMG/XSGMG=71) `SAA 2 @0o6220`. Names enter
  the table only via YNNAM @0o17710 (XSCRS/XSNAM). So "Unknown name" simply means `*XM-ENNS0`
  is not in the table - because ENNS0 hung before creating/registering it.
- The config theories are RULED OUT empirically: `list-ser` shows local **system 100** IS
  defined and **`*XM-FIDO` registered fine** on it - so registration works and a local system
  number exists. `DEFINE-REMOTE-NAME`/`DEFINE-LOCAL-SYSTEM` are NOT the blocker. The earlier
  `DEF-REMOTE,,D100 100` failed because it inserted the string `D100`, not `*XM-ENNS0` (right
  command family, wrong name) - but even the right name would not help while ENNS0 hangs.

So: FIDO (a normal server) self-registers and works; ENNS0 (network server) hangs in its
POSU INPUT wait on LU 2240B and never registers. The ONLY open question is what LU 2240B is
and why its input never arrives (agent running) - that is THE fix point.

## xmsg-L-binaries/ - actual binaries from the running L disk image

Extracted with `ndtool` from `D:\BIGDISK0-L.IMG` (the running system, NOT the K floppy).
23 XMSG/XROUT files. The key ones for the START-NETWORK-SERVER decode:
- `XMSG-COMMAND.PROG` (111103 B) = the (SYSTEM) build; guest banner is Release **M** (210373M),
  so this is the RUNNING command interpreter - PRIMARY target.
- `XMSG-COMMAND-L03.PROG` (90112 B) = older L03 build (comparison).
- `XMSG-XROUT-L03.BPUN` (80450 B) = XROUT kernel (where XRUNN=2 is actually returned).
- `XMSG-SYMBOL-L03.SYMB`, `XMSG-VALUES-L.SYMB`, `XMSG-SYS-DEF-L.SYMB`, `XMSG-SYSTABS-L03.SYMB`,
  `XMSG-POFTABS-L03.SYMB`, `XMSG-PL-VALUES-L.INCL` = symbol/constant tables.

- XMSG-COMMAND-START-NETWORK-SERVER-DECODE-2026-07-23.md - DONE. Built+verified a SINTRAN
  :PROG loader (tools/prog_load.py: 7 BE header words, bank-1 image at file 0x200, load addr 0,
  mem word A = file byte 0x200+2A). Command dispatch table @mem 0o22334; `Start-Network-Server`
  handler @0o50722 (string "Server not yet started..." @0o50617). The handler loads XROUT
  service **XSNET=85** (SAA 85 @0o72546) and issues MON 200B (wrapper @0o72314) to XROUT port 0
  carrying `FF09 *XM-ENNS0 / FD05 ENNS0`. It RESOLVES/STARTS an already-defined gateway - it
  does NOT create the name (no XSCRS/XSNAM on its path). XSNET returns XRUNN=2 because XROUT has
  no name/system entry to bind *XM-ENNS0 to. (Exact XROUT-internal test left [OPEN]: XMSG-XROUT-
  L03.BPUN needs a load base to disassemble.)

## FINAL ROOT CAUSE (investigation closed 2026-07-23)

`START-NETWORK-SERVER ENNS0` "Unknown name" is **a config/procedure gap, NOT an emulator or
firmware or ENNS0 bug**. Cleared, in order: controller wiring/68K/LANCE/PRKEY (works); firmware
SUBFUNCTION=5 (normal ACK); ENNS0 supervisor (only XFDUM+XFDCT, never registers a name - by
design); XMSG-COMMAND START-NETWORK-SERVER handler (correctly issues XSNET=85 to start the
gateway). The break: **the DEFINE-REMOTE-NAME name<->system mappings (from the XMSG-STARTUP mode
file) were never run**, so XROUT has no entry for XSNET to bind *XM-ENNS0 to -> XRUNN=2. The
harness/manual run did only start-x + @RT ENNS0 + START-NETWORK-SERVER, skipping XMSG-STARTUP.
Corroborating evidence: on the L image `(SYSTEM)XMSG-START:MODE` is **0 bytes** (empty) - no
DEFINE-REMOTE-NAME mappings exist. FIX = run the official ND-210580 page-6 WARM-START sequence
(XMSG-STARTUP / DEFINE-REMOTE-NAME BEFORE START-NETWORK-SERVER). Not an emulator change.

---

## tools/ - reconstructed disassembly tooling (session-local until now)

Built this session from `SINTRAN\File-Formats\BRF-FILE-FORMAT.md`. NOT the committed
sintran-segment-carver; these are lightweight ND-100/BRF/68K helpers.

- `brf_link.py` - BRF loader/linker. Resolves a `.brf` object into a flat ND-100 image +
  symbol table. Validated: 174 units of encos-err-i-b01.brf, all checksums OK.
  **SPEC FIX baked in:** packed-symbol sixbit decode is `ascii = c|0o100 if c<0o40 else c`
  (0=space), NOT `c + 0o40` (the BRF-FILE-FORMAT.md value mis-decodes every symbol).
- `nd100dis.py` - ND-100 disassembler (memory-reference / MON / IOX / skip / jump decode
  reliable; PLANC inline-data blocks are not auto-separated).
- `m68kdis.py` - minimal MC68000 disassembler (for the encos-ser 68K firmware).
- helpers: `find_callers.py`/`callers.py` (jsr/JPL caller scan), `csval.py` (checksum),
  `scan.py`/`scanprog.py` (.prog probing - note the `.prog` loader for encos-in-b01.prog is
  still TODO: ~256-word BE header then raw words), `census*.py`, `dumpstrings.py`,
  `dumpwords.py`, `resolve.py`, `symdump.py`.

Typical use: `python brf_link.py <path-to>.brf` then `python nd100dis.py <image> <base>`.
Targets:
- ENNS0 supervisor: `Installation\Communication\Ethernet\x\encos-err-i-b01.brf` (MAIN @031655)
- 68K firmware: `Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`

---

## Related NDInsight docs

- `SINTRAN\XMSG\DOC\ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md` + `ENNS0-XROUT-DISASSEMBLY-HANDOFF.md`
  - the earlier getMagic/XSGMG registration analysis.
- `Installation\Communication\Ethernet\ND-210580-02-EN.md` - official COSMOS Ethernet Option
  install + operator doc (ENCOS-INJ install, the page-6 start recipe, LIST-NETWORK-SERVERS).
- `SINTRAN\NPL-SOURCE\NPL\RP-P2-PIOC.NPL` - the kernel PIOCM (MON 255B) driver (PISTA T=6).
- RetroCore emulator + logging: `Emulated.HW\ND\CPU\NDBUS\NDBusEthernetII.cs` +
  `NDBusEthernetIIDecode.cs` ([MBX]/[BIT2]/[PRKEY]/[ETH-*] traces); master reference at
  RetroCore `DOCS\ND_EthernetII_MASTER_REFERENCE_2026-07-23.md`.
</content>
