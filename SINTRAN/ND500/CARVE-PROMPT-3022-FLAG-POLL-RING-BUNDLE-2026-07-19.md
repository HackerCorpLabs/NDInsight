# CARVE PROMPT: 3022 bundle — RFLAG/SFLAG, background poll block, LAST-N500-MSG ring

**For**: the SINTRAN carving/RE assistant. **From**: architect. **Date**: 2026-07-19.
**Discipline**: read `CARVING-HANDOFF.md` + `ND500-STATUS-AND-INDEX.md` first; byte-verified only,
cite segment + octal address (SINTRAN side) / HEX (Ghidra); mark inference ASSUMPTION; update status
files. Do NOT confirm the emulator's fabricated TAG protocol.

Three bundled questions, all seeded with LIVE 3022 harness captures (below). The bus-interface team
will validate each answer against the harness.

## Seed evidence (raw, ungraded — from the 3022 command-ladder + F6 harness)
Source docs: `ND500-F6-MESSAGE-RING-RAW-CARVE-SEED-2026-07-19.md`,
`ND500-BUS-INTERFACE-COMMAND-LADDER-ANALYSIS-2026-07-19.md`,
trace `scratchpad\sintran-3022-trace-f6ring.txt`.

- **Two 5MPM sites**, both start `FF FF FF FF` (link=0xFFFFFFFF):
  - **Site A** = window offset `0xE30` (ND-100 byte `0x420E30`). Byte `0x420E35` advanced `03→04`
    across `LIST-TABLE LAST-N500-MSG`. A repeating **~16-byte entry array** runs `0x420E40..0x420EE0`
    with an incrementing 2-byte value (`11 E6, 11 F4, 11 F5, … 11 FB`). **Ring candidate.**
  - **Site B** = window offset `0x4130` (byte `0x424130`, the PROCMSG base). Went all-zero →
    `FF FF FF FF 00 03 FF FF … 00 01 2E 9A`. **Architect note (verify): `0x2E9A = 11930 = MicroVersion
    027232B`; `00 03` at word offset 2 = N5STA=ANSWER(3). So site B is the ReadMicroVersion ANSWER
    mailbox (3RMICV), NOT the ring.** The tail `00 01 2E 9A` also appears at site A `0x420E3C`.
- Trace: `MSGHDR base=0x424130 N5STA=0x0001 MICFU=0x0001`, `PROCMSG lastMICFU=ReadMicroVersion +
  ResidentRead`.
- `LIST-TABLE LAST-N500-MSG` rendered NO visible ring to the terminal (only "> Loading Swapper").

## (a) RFLAG=100B / SFLAG=101B — interface transaction or ND-100-resident?
The command-ladder trace showed GET-FLAG/SET-FLAG produce NO command-specific 3022 traffic (the
"17-event round-trip" was a background ReadMicroVersion poll, byte-identical on LIST-ACTIVE-PROCESSES).
Carve: locate the RFLAG(100B)/SFLAG(101B) subfunction handlers in the MON 60B/N500M path
(5IFUNC/FUNCS). Determine byte-verified: **do they cross the 3022 at all**, or read/write ND-100
resident process-table state? If they cross, give the parameter block (offsets, MAR build, which
FUNCS entry). If resident, cite the table + field they touch. (Sections 8.7.6-7 GET/SET-FLAG.)

## (b) The background poll — ReadMicroVersion + ResidentRead BLOCK format + cadence
The only confirmed periodic 3022 traffic is a poll round-trip: lock → LoadMarX2 MS+LS →
`MAR=0x212098`=byte `0x424130` in-window → `LCON5:=5` ACTIVATE → MSGHDR@0x424130 → PROCMSG →
ND500Finished. Carve: **what SINTRAN routine issues this poll, what MESSAGE BLOCK it builds** (field
offsets in the 5MPM buffer at site B), and **what triggers it / at what cadence** (timer level? per
monitor command? RT loop?). Cross-check the block against the message catalog (N5STA/MICFU/version
answer = 3RMICV, MICFU=1). Confirm/deny the site-B decode above.

## (c) LAST-N500-MSG ring format — seeded by site A
Carve the `LIST-TABLE LAST-N500-MSG` ring (section 8.10.9.1, "last 64 messages to ND-500"). Site A
(`0x420E30`) is the ring candidate: header + incrementing 16-byte records `0x420E40..`. Determine
byte-verified: the ring base + header layout, the **record size and field offsets** (is it 16 bytes?
what is the incrementing 2-byte value — sequence #, msg #, timestamp?), the capacity (64?), and the
head/tail pointers (the `0x420E35 03→04` advance is a candidate index). Also carve **why the terminal
render was empty** — is the ring populated only when the ND-500 actually runs (no microengine here),
or does `LIST-TABLE` need a different arg/output target?

## Deliverables
Per question: byte-verified answer with segment+octal (and Ghidra HEX where relevant), the emulator
test-vector (concrete bytes/offsets), and an explicit divergence note vs the current emulator. Mark
anything unresolved ASSUMPTION rather than guessing. Update `ND500-STATUS-AND-INDEX.md`.
