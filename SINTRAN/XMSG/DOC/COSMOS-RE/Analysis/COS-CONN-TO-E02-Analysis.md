# cos-conn-to-e02.prog — XMSG/TAD Structures, Startup & Disconnect

**Program:** `E:\Dev\Ronny\NDInsight\Installation\Communication\COSMOS Basic\x\cos-conn-to-e02.prog`
**Identity:** COSMOS CONNECT-TO client, *"VERSION - E02, September 25, 1987"*. TAD **asker / RP** role.
**CPU / format:** ND-100 big-endian 16-bit, SINTRAN-III `:PROG`, **PLANC**, 210 functions.

**Evidence tags:** `[BIN]` = decoded from this binary in Ghidra · `[SYM]` = ND symbol file
`F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\` · `[DOC]` = capture-verified spec
(`…\SINTRAN\XMSG\DOC\XMSG-PROTOCOL.md`, `…\SINTRAN\TAD\TAD-Message-Formats.md`) ·
`[INF]` = inferred, not yet byte-proven.

> Instruction semantics checked against `E:\Dev\Ronny\nd100-markdown\docs\cpu_documentation.md`.
> Companion note: this program is one endpoint; the on-wire bytes it produces are specified
> in the `[DOC]` files, which this analysis anchors to the actual code.

---

## 1. Transport: how a message is sent and received `[BIN][SYM]`

Everything goes through **`MON 200` (octal) = `MON 0x80`** with a function code in `T`
(option bits OR'd into the high bits), params in `A`/`D`, port/pointer in `X`; status
returned in `T`. The XMSG library is `ram:a0b1`–`ram:a1e3`, one wrapper per function —
all renamed in Ghidra:

| Ghidra name | Addr | Fn | XMSG | Role in a session |
|---|---|---:|---|---|
| `xmsg_XFGET` | a0b1→a138 | 2 | get msg buffer | start of every send |
| `xmsg_XFREL`* | — | 3 | release buffer | end of every receive |
| `xmsg_XFRHD`* | — | 4 | read header | receive |
| `xmsg_XFWHD`* | — | 5 | write header | send (sets dst/src sys+port) |
| `xmsg_XFREA` | a16e | 6 | read user data | receive |
| `xmsg_XFWRI` | a0b1 | 7 | write user data | send (writes TAD bytes) |
| `xmsg_XFSCM` | a0db | 8 | set current message | |
| `xmsg_XFMST` | a1d4 | 9 | message status | |
| `xmsg_XFOPN` | a1ad | 10 | open port | **startup** |
| `xmsg_XFCLS` | a1bb | 11 | close port | **teardown** |
| `xmsg_XFSND` | a11a | 12 | **send message** | send |
| `xmsg_XFRCV` | a197 | 13 | **receive message** | receive |
| `xmsg_XFPST` | a14b | 14 | port status | |
| `xmsg_XFM2P` | a1e3 | 22 | magic→sys+port | name resolution |
| `xmsg_XFALM` | a105 | 38 | allocate msgs to task | init |
| `xmsg_XFLMP` | a0ee | 40 | list msgs & ports | |

\* `XFREL/XFRHD/XFWHD` are used but their wrapper sites are still being located
(header write appears partly inline in the builders). `[BIN partial]`

**Send sequence** `[SYM][BIN]`: `XFGET(size) → XFWHD(dst sys/port, src sys/port) →
XFWRI(bytes) → XFSND(options)`.
**Receive sequence:** `XFRCV(port) → XFRHD → XFREA(bytes) → XFREL`.

**XFSND option bits** (bit numbers OR'd into `T`) `[SYM]`: XFTCM=8, XFSEC=9 (secure/
return-if-undelivered), XFROU=10 (route via XROUT), XFFWD=11, XFBNC=12, XFHIP=13
(high-priority), XFWAK=14, XFWTF=15 (wait until terminated).

Globals: `g_xmsg_msg_counter` (`ram:a0b0`, bumped by XFGET/XFRCV), `g_xfsnd_options`
(`ram:0c7d`), `g_state_0af8` (`ram:0af8`, session state), `g_tx_seq_pair` (`ram:4507`,
2-word TX datagram sequence, +1 per message).

---

## 2. The XMSG wire envelope `[DOC]` (what MON 200 puts on HDLC)

XFWHD fills the SINTRAN header; XFWRI fills the trailer (TAD chain). On the wire:

**SINTRAN header (13 bytes):**
```
0  Marker1 = 0x21
1  Marker2 = 0x13 normal / 0x12 relayed
2  PacketType = 0x00
3  Subtype: 0x0E Data · 0x03 Ack · 0x13 ReachReply · 0x19 ReachRequest · 0x07 NetErr
4-5  Dest node (BE)      6-7  Src node (BE)
8-9  Flags1 = datagram sequence (per direction per link, +1 per Data frame)  <- g_tx_seq_pair
10-11 Flags2 = frame class (XMCSM>>16)
12   Protocol/Channel byte (0xD8..0xDE)
```
**XMSG sub-header (Data frames, from off 13):** Counter, Marker `21 00`, frameFlags,
Role(=XF* option high byte), XMDSY/XMDPT (dest sys/port), XMSSY/XMSPT (src sys/port),
XMCSM (32-bit service/class word), XMLEN (16-bit), then the TAD trailer.
Envelope math (seed/Counter/Channel/epoch) and secure-ACK closed form: `XMSG-PROTOCOL.md`.

---

## 3. TAD message structures — every opcode `[DOC]`, builders `[BIN]`

Generic frame: `[pad 0x00 if odd][type:1][count:1][data:count]`. **15 builder sites**
in the binary (`ram:72xx–7fxx` + scattered) each do `XFWRI` then `XFSND`. Full opcode
catalog (validated against the binary where a builder is decoded):

| Op | Sym | Cnt | Dir | Meaning |
|---:|-----|----:|:---:|---------|
| 01 | 7BDAT | 0–255 | C↔S | terminal data block |
| 02 | 7RFI | 0 | host→ | ready-for-input credit |
| 03 | 7ECKM | 1/21 | host→ | echo strategy (+20-byte table) |
| 04 | 7BMMX | 3/23 | host→ | break strategy + maxbreak (+table) |
| 07 | 7CORS | 5 | host→ | assigned terminal port `00 00 node port16` |
| 08 | 7ESCA | 0 | →C | escape received |
| 09 | 7DCON | 0 | →C | **disconnect** |
| 0B | 7LUN | 2 | host→ | TAD logical unit = 768+value |
| 0C | 7TMOD | 1 | C→ | terminal mode flags |
| 0D | 7TTYP | 2 | C→ | terminal type (16-bit) |
| 0E | 7CESC | 1 | C→ | enable/disable escape |
| 0F | 7DESC | 1 | C→ | define escape char |
| 13 | 7SYCN | 2 | C↔S | system control (login ladder) |
| 14 | 7USCN | 2 | C↔S | user control |
| 16 | 7RESE | 0 | C→ | reset request |
| 17 | 7RECO | 0 | →C | reset confirm |
| 18 | 7DUMM | 0 | any | filler/pad |
| 1F | 7OPSV | 3 | C→ | **OS+protocol version (handshake)** |
| 20 | 7ESRS | 0 | host→ | escape response |
| 21 | 7CERS | 0 | →C | escape-control response |
| 2A | 7TREP | 2 | →C | terminal status report |
| 2B | 7UMOD | 2 | C→ | UMOD strategy (proto ≥4) |
| 2C | 78MOD | 2 | C→ | 8-bit mode |
| FA | 7CPCO | 4 | →C | completion code |
| FB | 7ERRS | 2 | →C | error response |
| FD | 7POLL | 0 | host→ | server poll |
| FE | 7REJE | 1 | →C | reject (bad opcode echoed) |
| FF | 7EOP | 0 | both | chain terminator |

Named handler routines in the binary `[BIN]`: `REJECTRUT`@0x321d (7REJE),
`disconnect_cmd_thunk`/`disconnect_teardown` (7DCON path), plus name-strings for
ECHORUT (7ECKM), BRMORUT (7BMMX), DEF_ECRUT/EC_RESRUT, NOWTRUT, TREPRUT, RESCREM
(7RECO), WHO_ARE_YOU (7WHO 0xFC).

---

## 4. STARTUP sequence `[BIN]` = `connect_to_session_setup` (ram:710d)

Binary-confirmed order (Ghidra-commented):
1. Clear session flags (`DAT_ram_717f/7180 = 0`).
2. **`XFOPN`** the local asker port (indirect via ptr 0x72a1); store the returned port
   into `session[+0x13]` (session base = `-0x7a,B`).
3. Build & send a chain of XMSG fields through the `XFGET/XFWRI/XFSND` builders, using
   the PLANC argument record at `-0x7e,B` (offsets +6..+0xB carry count/descriptor/opts).
4. Octal-format a 16-bit value (`session[+9]`) into 6 ASCII bytes via
   `SBYT >>15,>>12,>>9,>>6,>>3,>>0 & mask` — i.e. the node/version rendered as octal
   digits into the outgoing buffer.
5. Send **`7OPSV`** (opcode `0x1F`, count 3: OS version + subversion + TAD protocol) —
   the handshake message. `[BIN: opcode 0x1F + count seen]`
6. Enter the receive/ACK loop: build+send (ptrs 0x7326/0x7327), read status (ptr 0x72a0).

Matches the `[DOC]` connect-to flow: asker `XFOPN` → XSLET connect letter to the
`*TADADM` server → host returns assigned terminal port (`7CORS`) + `7LUN` → OPSV
handshake → login ladder (`7SYCN`).

---

## 5. DISCONNECT / teardown `[BIN]` = `disconnect_teardown` (ram:44a0)

Body of `disconnect_cmd_thunk`@0x4497 (7DCON `09 00`). Binary-confirmed:
- Branches on `g_state_0af8`.
- Loops building & sending the final TAD messages; after each build calls the
  commit/get-status helper `FUN_ram_449b`.
- Maintains the 2-word TX sequence `g_tx_seq_pair` (+1 per message) and inspects
  received status codes `5` and `0x0D` (`XMKIK`? / `XFRCV` msg-type returns).
- Invokes a session-specific handler via a function pointer at `session[+0x89]`.
- Final message-system teardown uses **`XFDCT`** (called from 0x7510 / 0x7580) and
  **`XFCLS`** to close the port.

Matches `[DOC]` teardown: `7DCON` (09 00) from the terminal port for instant close,
then port close + task disconnect from XMSG.

---

## 5b. RECEIVE PATH DECODED — the two hard gates `[BIN, 2026-07-08]`

Decoded `tad_receive_and_dispatch` (ram:46db) and `tad_rx_BDAT_01` (ram:2b62) to answer the
multi-chunk display bug (only the FINAL chunk of a >255-byte reply renders on 100's screen).

**GATE 1 — msg-type filter (ram:46ee) `[BIN VERIFIED]`:**
```
46e1 JPL [0x4751=0xa197]   ; XFRCV
46e3 STA -0x6e,B           ; A = returned msg-type
46ed SAT 0x3               ; T := 3
46ee SKP DA,UEQ,ST         ; skip next if A != 3
46f0 JPL *46d6             ; (A!=3) DISCARD the message, loop for next XFRCV
```
A received XMSG message is **dropped unless its XFRCV msg-type == 3 == XMTHI (high-priority)**.
So terminal traffic MUST arrive high-priority (sender uses XFHIP). This is kernel-local and
invisible in pcap. NOTE: this does NOT by itself explain the chunk drop — the FINAL chunk renders,
so our normal terminal frames already classify as XMTHI (our frameFlags/role produce it). But it
is a hard, previously-unconfirmed acceptance rule worth honoring.

**GATE 2 — the BDAT render gate (ram:2b76) `[BIN VERIFIED]`:**
```
2b76 LDX -0x73,B     ; X = per-LU terminal control block (set at 47c2 from element arg[0xa])
2b77 LDA 0x11,X      ; A = ctrl[0x11]
2b78 JAF *2ba7       ; if ctrl[0x11] != 0  -> RETURN without drawing
2b79 LDA I *0c81     ; A = global DAT_0c81
2b7a JAF *2b7e       ; if 0c81 != 0 -> render
2b7c LDA I *0c7f     ; A = global DAT_0c7f  (session-active flag; =1 after login)
2b7d JAZ *2ba7       ; if 0c7f == 0 -> RETURN without drawing
2b7e..               ; render count bytes (loop 2b8b-2b9a; count from LBYT @2b72)
```
A BDAT is drawn **only if `ctrl[0x11]==0` AND (`0c81!=0` OR `0c7f!=0`)**. `ctrl[0x10]`/`ctrl[0x11]`
are a paired per-LU flag set (0x10 = RFI/input-ready, set by tad_rx_RFI @32c4; 0x11 = an
output-suppress/mode flag, set =1 at ram:2379 and ram:49c6, which also touch 0x10). `0c7f` is the
connection-active flag (set =1 across the SYCN login handlers). The BDAT handler treats
`count==0xFF` no differently from any other count — there is NO 255-specific branch in the handler;
it simply renders `count` bytes.

**HONEST LIMIT — what static analysis CANNOT settle here:** both our continuation chunks and the
final chunk pass through this SAME handler and gate, in the SAME logged-in state, and the final
renders. So the differentiator is a RUNTIME flag VALUE (most likely `ctrl[0x11]` being nonzero when
a bare continuation arrives but zero for the final, or a coroutine state in the FUN_ram_2b66
re-entrant loop keyed on local -0x7b), which cannot be proven by reading the code — it needs the
actual per-chunk register/flag values. The dispatch also has a multi-element WALK
(46db loop, opcode special-casing for 0xFD/0xFC/0x0C at 4781-478a) and a PLANC coroutine
(FUN_ram_2b66, state in -0x7b) that resists clean static tracing.

**DECISIVE NEXT STEP = DYNAMIC:** breakpoint the live ND-100 (running as node 100) at ram:46ee and
ram:2b76 while reproducing the `3` echo command, and read for chunk1 vs chunk2 vs the final chunk:
the XFRCV msg-type (-0x6e,B), `ctrl[0x11]` (LU block, via -0x73,B), and globals `0c7f`/`0c81`.
Whichever value differs between a dropped continuation and the rendered final IS the gate. Tooling:
the ND-100 runs the OLD-style runner, so this is the **dap-debugger MCP** (debug_attach /
debug_set_breakpoints / debug_read_memory), NOT the retrocore MCP (retrocore = the SDL2CLI
NuGet runner, which ND-100 has not been migrated to). Minutes of work vs hours of unreliable
static tracing.

---

## 6. Progress / still open

- Done `[BIN]`: full XMSG wrapper map (16 fns named+commented), transport convention,
  startup routine, disconnect routine, sequence counter, key globals.
- Open: byte-exact decode of each of the 15 builders → its specific opcode; the receive
  dispatcher (opcode compare-chain → `…RUT` handlers); locate the `XFWHD/XFRHD/XFREL`
  wrapper sites; finish renaming the remaining `FUN_ram_*`. Then repeat for
  `cos-file-tra-e02.prog` and `cos-fa-serv-e04.prog`.
```
```

---

## Function-naming completeness (deliberately left)

All **protocol-significant** functions are named + commented: the XMSG API wrappers, the SINTRAN
OS-call library, `connect_to_session_setup` (startup), `tad_receive_and_dispatch` + the opcode
jump table `tad_rx_dispatch_table` (0x330e) + all 23 `tad_rx_<op>` handlers, and the
`disconnect_teardown` path (~55 functions).

The remaining ~155 `FUN_ram_*` are the **application layer** — the command parser, the script/`*SCRIPT:`
engine, the interactive command handlers (HELP, LIST-SYSTEMS, SET-COMMAND-PROTECTION, RECONNECT-TAD,
INITIALIZE-SCRIPT, DUMP-PROGRAM, …), and Ghidra fragments. They are **intentionally left**: they are
not on the TAD/XMSG wire path, so they add names but not protocol understanding. They can be named
later from the command-name strings (ram:62xx–64xx) if a full-coverage pass is wanted.
