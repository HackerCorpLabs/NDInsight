# Carve log — ACCP E2-P2 dispatcher handlers (running, started 2026-08-09)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ACCP-E2-P2-HANDLER-LOG-2026-08-09.md`
**Purpose:** the running record of carving the 31 unnamed ACCP command handlers (octo.bin, 68000).
One row per handler; NAMED handlers also get a detailed `CARVE-ACCP-<NAME>-<code>-HANDLER-*.md`.
Does NOT touch `ACCP-COMPLETE-REFERENCE.md`. Arm map: `CARVE-ACCP-E2-P1-COMMAND-BYTE-MAP-2026-08-08.md`.

## Method (verified)

Ghidra `get_disassembly` at the arm address, **`program_name: "octo.bin"`** (the active program is an
ND-100 BRF — a bare call returns garbage). Handler body = the code after the arm's `cmpi.b/bne`, up to
the next arm; every handler ends `bra.w 0x6878` (the dispatcher return). Bind the code (as OCTAL) to a
`CM*` symbol in `N500-SYMBOLS.SYMB`; if none exists, carve behavior-only and **HOLD the name** (do not
invent one). Behavior overrides a misleading alias (authority rule). Lock CM names in
`AccpCommandChannelTests.namedByTheNd100Carve` and validate `-Filter CarvedCommandNumbers…`.

**LOCK BAR (adopted 0o41, refined 0o44 and 0o67 — final form):** lock a name iff the candidate
`CM*` symbol is **uniquely picked** AND the carved behavior **does not contradict** it. "Uniquely
picked" = either (a) the **sole** `CM*` symbol at the command's octal value, OR (b) the value
collides but behavior matches **exactly one** candidate (a *behavior-resolved collision*, e.g. 0x24
CMR16). The behavior factor is a **veto, not a requirement**: a sole symbol whose meaning is merely
*unexpandable* (I can't decode the abbreviation, but nothing about the handler conflicts with it)
is still locked — with the expansion marked `[INFERRED]` (precedent: CMLMA "MA", CMLOO, CMSPE).
**Hold** when: the value collides with no behavioral separation (0x21/0x23), or a sole symbol's
meaning actively *contradicts* the behavior (0x33 CMBUF="buffer" vs a register transaction), or
there is no `CM*` symbol at all (0x0D/0x15/0x17/0x18/0x34/0x35). Held names are NOT added to the
test array. Authoritative disambiguator for held collisions: the ND-100 SINTRAN driver's
command-send sites (which `CM*` symbol it emits per command byte), or ND-05.020.01 §5.3 by description.

Worked example of (b): 0x24 = **CMR16** — value 0o44 collides {CMR16, CMATE}, but the handler
returns a 16-bit word and "read 16" matches CMR16 alone; it is also the read-16 twin of the
sole-confirmed 0x25 = CMR32 (read-32). That twin structure + behavior removes the ambiguity.

## Shared helpers / registers (pinned by usage)

| Sym | Role |
|---|---|
| `0x6FFA` / `0x6F9C` | request-word / request-byte reader (pull from the ND-100 message) |
| `0x6986` | reply-CODE sender (`0xFF` nak-marker then a code; `0` = ack) |
| `0x69D0` | reply-WORD sender (data word back to the ND-100) |
| `0x73B2` / `0x741E` | control-store word WRITE / READ (the CS word ↔ the `0x1144F0` staging buffer) |
| `0x70AA` | `MfBusMemoryTransaction_VariantA` — write data OUT to MFbus (ref §1885) |
| `0x79E4` | OR `0xC` (bits 2,3) into the `0x330000` OBCON latch shadow (bit 2 = CS gate) |
| `0x1144F0` | 8-halfword (128-bit) CS staging buffer |
| `0x1143AC` | shared mode flag — when ≠0, the transfer commands take a `0xFF`-nak abort path |
| `0x1143B6` | second gate flag — when ≠0, the read commands (0x24/0x25/0x35…) NAK with code `0xFE` (not-available); distinct from the `0x1143AC` mode flag |
| `0x72EC` / `0x7374` | read-16-bit / read-32-bit result helpers (0x24 CMR16 uses `0x72EC`, 0x25 CMR32 uses `0x7374`) |
| `0x72A0` | write-16-bit helper (0x26 CML16 — the write twin of `0x72EC`) |
| `0x7320` | write-32-bit helper (0x27 CML32 + 0x34 MFbus→reg — the write twin of `0x7374`) |
| `0x7138` | read-long-from-MFbus-mem helper (0x34, at addr `0x1143AE`; the read counterpart of `0x70AA`/`0x795A` used by 0x35) |
| `0x1143B2` | third gate flag — when 0 the 0x35 read-forward NAKs with code 1 (distinct from `0x1143B6`) |
| `0x795A` | alloc/context helper (0x35 — returns A0, result stored at `A0+0x14` before the MFbus forward) |
| `0x440000` | a hardware data/output port (16-bit word writes, gated by `0x660001` bit 1 busy) |
| `0x330000` | OBCON control latch (bit 2 CS-gate, bit 6 write-strobe; shadow at `0x1144EE`) |
| `0x76E6` | load-one-word-to-`0x550000` helper (used by 0x20 CMLMA; strobes `0x220000` + OBCON `0x330001` from `0x1144EF` shadow) |
| `0x773E` | commit 8-word staging (`0x1144F0`) to `0x220000`-family HW — `jsr 0x7776` then `move.w #0x2018 → 0x220000` (used by 0x21 write) |
| `0x775A` | read 8-word block from HW into `0x1144F0` staging (used by 0x22 read; the `0x773E` twin) |
| `0x7036` / `0x6A04` | request-LONG reader (32-bit param from ND-100) / reply-LONG sender (32-bit result back) |
| `0x71F8` / `0x71B2` | long→long transaction helpers (0x23 uses `0x71F8`, 0x33 uses `0x71B2`; different HW targets) |
| `0x550000` | a second HW data-register port (16-bit, written by the `0x76E6` load helper) |
| `0x220000` | HW sequencer/strobe port (driven by `0x76E6` with a 0x10/0xF/0x3010/0x15 sequence) |
| `0x113138` | CS/HW busy-ready flag (spin until 0; shared by 0x14/0x16/0x20) |
| `0x114550` | fixed 16-word (256-bit) result buffer returned by 0x10 CMREA |

## Handlers

| Cmd | 0o | Symbol | Status | Behavior (carved) |
|---|---|---|---|---|
| 0x14 | 024 | CMDWW | **NAMED** (doc, locked) | checksummed 8-halfword CS-staging write (`0x73B2` → `0x1144F0`); NAK4 bad checksum, ACK0 |
| 0x15 | 025 | CMRWC/CMADR (alias mismatch) | behavior, name HELD | checksummed multi-word **MFbus memory write-block** (`0x70AA`); count+addr from `0x1143AE`. Aliases say read/address; behavior is WRITE |
| 0x16 | 026 | CMDRW | **NAMED** (doc, locked) | CS word **read-back**: addr≤0x3FFF, `0x741E` reads → return 8 halfwords+checksum (`0x69D0`); NAK3 addr, NAK5 read-err |
| 0x17 | 027 | *(none)* | behavior, name HELD | mode-ENTER: `0x79E4` gates OBCON latch bits 2,3 + sets `0x1143AC=1` (makes 0x14/0x15/0x16 refuse) + ACK0 |
| 0x18 | 030 | *(none)* | behavior, name HELD | bulk **word-stream write** to HW port `0x440000` (count-1 bytes from `0x1144E8`, big-endian pairs, busy-wait `0x660001` bit1, `0x330000=0xD0` strobe/word; `0x1143B4` selects a trace path) |
| 0x0D | 015 | *(none)* | behavior, name HELD | **read-3-result-words**, gated: `tst 0x1143A6` ready flag → ACK0 + reply words `0x1143A0`/`0x1143A2`/`0x1143A4` (`0x69D0`); flag clear → NAK marker + code `0x0D` (=13, "not ready"). No `CM*=000015` symbol exists. |
| 0x0F | 017 | CMTEC | **NAMED** (locked) | **communication echo/loopback test**: read count byte (`& 0x1F`, 1–31) via `0x6F9C`, read N bytes into a local buffer, ACK0 (`0x6986`), then echo all N bytes back byte-by-byte (`0x6986`). Behavior = "Test Communication" → clean name-match. Sole `CM*=000017`. |
| 0x10 | 020 | CMREA | **NAMED** (locked) | **read 16-word result block**: ACK0, then send 16 consecutive 16-bit words from the fixed buffer at `0x114550` via reply-WORD (`0x69D0`); loop `i=0..15`. Behavior = "read" → clean name-match. Sole `CM*=000020`. |
| 0x20 | 040 | CMLMA | **NAMED** (locked) | **load one word into HW port `0x550000`**: read a word param (`0x6FFA`), if mode flag `0x1143AC` clear call helper `0x76E6` (stores param → `0x550000`, strobes `0x220000`/OBCON `0x330001` from `0x1144EF` shadow), busy-wait CS-ready `0x113138`, ACK0; mode flag set → NAK abort. Sole `CM*=000040`. "MA" not expanded in any source I have (name from symbol). |
| 0x21 | 041 | **CMLMI** / LMIR | **NAMED** (locked 2026-08-09) | **8-word block WRITE** to the `0x220000`-family HW: read 8 words into `0x1144F0` staging (`0x6FFA`×8), mode-flag guarded, commit via helper `0x773E` (`jsr 0x7776` then `move.w #0x2018 → 0x220000`), busy-wait `0x113138`, ACK0. Collision `CM*=041`={CMMAC,**CMLMI**,CMTMO} resolved by the ND-05.020.01 §5.3.27–.42 section-order run (only CMLMI abbreviates a command in the run) = **Load MIR (5.3.28)**. |
| 0x22 | 042 | **CMRMI** / RMIR | **NAMED** (locked 2026-08-09) | **8-word block READ** (0x21's twin): mode-flag guarded, helper `0x775A` fills `0x1144F0` (8-word HW read), busy-wait `0x113138`, ACK0, then return the 8 words via reply-WORD (`0x69D0`×8). Collision `CM*=042`={**CMRMI**,CMACO} resolved by section-order run + CMLMI/CMRMI L/R pair = **Read MIR (5.3.29)**. |
| 0x23 | 043 | **CMBUS** / TBUS | **NAMED** (locked 2026-08-09) | **long→long transaction**: read 32-bit param (`0x7036` request-long) → `@0x38`, mode-flag guarded, call helper `0x71F8(param)` → 32-bit result `@0x3C`, busy-wait `0x113138`, ACK0, return result via `0x6A04` (reply-long). Collision `CM*=043`={**CMBUS**,CMAST} resolved: TBUF(§5.3.30) is CMBUF at 0x33, leaving **Test Bus (5.3.31)** for 0o43. |
| 0x33 | 063 | **CMBUF** / TBUF | **NAMED** (locked 2026-08-09) | **long→long transaction** (same shape as 0x23, helper `0x71B2` instead of `0x71F8`): read long param → `0x71B2` → return long via `0x6A04`. Sole `CM*=063`=CMBUF = **Test Buffer (§5.3.30)**. Earlier "buffer contradicts" was wrong: TBUF is a *diagnostic that tests a buffer* (write-read-verify), which a long→long transaction supports. Promoted — also anchors the 0x23 disambiguation (TBUF sits here, not at 0o43). |
| 0x24 | 044 | CMR16 | **NAMED** (locked, behavior-resolved collision) | **read 16-bit result**: doubly-gated (mode flag `0x1143AC` → NAK abort; second flag `0x1143B6` set → NAK `0xFE`), else helper `0x72EC` → word `@0x56`, busy-wait `0x113138`, ACK0, return word via `0x69D0`. `CM*=044`={CMR16,CMATE}; "read 16" matches the 16-bit result uniquely + read-16 twin of CMR32 → locked (bar clause b). |
| 0x25 | 045 | CMR32 | **NAMED** (locked) | **read 32-bit result**: gated by `0x1143B6` (set → NAK `0xFE`), else helper `0x7374` → long `@0x3C`, busy-wait `0x113138`, ACK0, return long via `0x6A04`. Sole `CM*=045`, "read 32" matches the 32-bit result → clean two-factor lock. |
| 0x26 | 046 | CML16 | **NAMED** (locked) | **load/write 16-bit**: read word param (`0x6FFA`) `@0x54`, gated by `0x1143B6` (set → NAK `0xFE`), else helper `0x72A0(param)`, busy-wait `0x113138`, ACK0 (no data back). Sole `CM*=046`, "load 16" matches the word write + is the write twin of CMR16 → clean lock. |
| 0x35 | 065 | **RAIB32M** [D] (no CM*) | behavior; manual mnemonic DERIVED | **read-32-and-forward-to-MFbus**: triple-gated (`0x1143B6` → NAK `0xFE`; `0x1143B2`==0 → NAK code 1), else read long via CMR32's helper `0x7374`, alloc/context via `0x795A`, store result into struct `+0x14`, write it OUT to MFbus mem via `0x70AA` at address `0x1143AE`, ACK0. No `CM*=065` symbol (confirmed absent). Direction (register→memory read) = **Read AIB32 Via Memory (§5.3.34)** — the via-memory twin of 0x25 CMR32=RAIB32D. Manual mnemonic derived from direction; NOT a CM*-symbol lock, so not in the test. |
| 0x27 | 047 | CML32 | **NAMED** (locked) | **load/write 32-bit**: read long param (`0x7036`) `@0x38`, gated by `0x1143B6` (set → NAK `0xFE`), else write-32 helper `0x7320(long)`, busy-wait `0x113138`, ACK0. Sole `CM*=047`, "load 32" matches + write twin of CMR32 → clean lock. |
| 0x28 | 050 | CMRAS / **RASTS** | **NAMED** (locked, expansion CONFIRMED) | **read one word from HW**: call helper `0x7852` → word `@0x56`, busy-wait `0x113138`, ACK0, return the word via `0x69D0`. Arm `0x5FD6`. Sole `CM*=050` = **Read ASTS (§5.3.38)**; "RAS" = Read A-STatus; read-status behavior agrees. |
| 0x29 | 051 | CMLDM / **LMODE** | **NAMED** (locked, expansion CONFIRMED) | **write word split into two byte ports**: read word param (`0x6FFA`) `@0x54`, mode-flag `0x1143AC` guarded (set → `0xFF`/`0xFF`/`0x6A64` abort), else send low byte via `0x781C` and high byte (`>>8`) via `0x77FE`, busy-wait `0x113138`, ACK0. Arm `0x6016`. Sole `CM*=051` = **Load MODE (§5.3.39)**; "LDM" = LoaD Mode. |
| 0x2B | 053 | CMWMP / **WMPM** | **NAMED** (locked, expansion CONFIRMED) | **write two longs → MFbus**: read two longs (`0x7036`×2) `@0x38`,`@0x40`, mode-flag guarded, store `@0x40` into struct field `+0x14` (via `(A6)`), then MFbus-write `@0x38` via `0x70AA`, busy-wait `0x113138`, ACK0. Arm `0x60F6`. Sole `CM*=053` = **Write Multiport (§5.3.41)**; MFbus-write behavior agrees; write twin of CMRMP. |
| 0x2C | 054 | CMRMP / **RMPM** | **NAMED** (locked, expansion CONFIRMED) | **read long at addr → reply long**: read long addr (`0x7036`) `@0x38`, mode-flag guarded, read via `0x7138` → long `@0x3C`, busy-wait `0x113138`, ACK0, return long via `0x6A04`. Arm `0x6178`. Sole `CM*=054` = **Read Multiport (§5.3.42)**; read twin of CMWMP. |
| 0x2D | 055 | CMSET / **SETTRAC** | **NAMED** (locked, expansion CONFIRMED) | **set a 3-word parameter block**: read 3 words (`0x6FFA`×3) `@0x5E`/`@0x60`/`@0x62`, build a descriptor on `(A6)` (ptr=`&@0x5E` at `+0x14`, `+0x18`=0, `+0x1C`=2), apply via helper `0x7ACE`, busy-wait `0x113138`, ACK0. Arm `0x6326`. Sole `CM*=055` = **Set Trace Selector (§5.3.44)**; setting a small parameter block agrees. |
| 0x34 | 064 | **LAOB32M** [D] (no CM*) | behavior; manual mnemonic DERIVED | **MFbus→register-write-32** (0x35's inverse): triple-gated (`0x1143B2`==0 → NAK code 1; `0x1143B6` → NAK `0xFE`), else `0x795A` alloc, read long from MFbus mem via `0x7138` at `0x1143AE`, write it to the 32-bit register via CML32's helper `0x7320`, busy-wait, ACK0. No `CM*=064` symbol (confirmed absent). Direction (memory→register load) = **Load AOB32 Via Memory (§5.3.37)** — the via-memory twin of 0x27 CML32=LAOB32D. Manual mnemonic derived from direction; NOT a CM*-symbol lock, so not in the test. |
| 0x37 | 067 | CMLOO / **LOOP** | **NAMED** (locked, expansion CONFIRMED) | **set busy flag + ACK**: `move.w #1 → 0x113138`, clear staging, ACK0. Trivial (no param, no data). Sole `CM*=067` = **LOOP (§5.3.45)**; "LOO" = LOOP. |
| 0x38 | 070 | CMSPE | **NAMED** (locked, ⚠️ mnemonic UNMATCHED) | **byte-param status-gated action**: read byte param (`0x6F9C`) `@0x15`, call helper `0x7B20` → status byte; status≠1 → ACK0, status==1 → NAK code `0x0A` (=10). Sole `CM*=070`. **CAVEAT:** no §5.3.12–.49 command begins "SPE"; every other run-member is a clean initial-letter contraction, so either this is not an ACON command or its name breaks the family convention. Name is SINTRAN's; expansion UNKNOWN. |
| 0x3A | 072 | CMTES / **TESTMPM** | **NAMED** (locked, expansion CONFIRMED) | **memory write-verify sweep** (RAM test): read start+end long params (`0x7036`×2), mode-flag guarded, walk `[start,end)` step 4 — write `~addr` pattern to MFbus (`0x70AA`), read back (`0x7138`), verify `==~addr`; on mismatch NAK code `0x08` + return failing address & bad value via `0x6A04`. Sole `CM*=072` = **TESTMPM (§5.3.43)**; the memory test agrees. |
| 0x3B | 073 | CMCCD / **DCCD** | **NAMED** (locked, expansion CONFIRMED) | **guarded 8-word block read+checksum**: reject NAK 5 if sentinel `0x11455C`==`0x7F55` (locked) OR mode flag `0x1143AC` set; else helper `0x764E` fills `0x1144F0` (8 words) + returns status (≠0 → NAK 5), ACK0, return 8 halfwords + checksum via `0x69D0`. Sole `CM*=073` = **DCCD (§5.3.21)**; "CCD" = DCCD. |
| 0x3C | 074 | (HELD) | behavior, name HELD | **staged-block → MFbus write** (0x3B's write twin): triple-gated (`0x7F55` lock sentinel → NAK5; mode flag → NAK abort; `0x1143B2`==0 → NAK1); fills 8-word staging via `0x764E`, reassembles 4 longs (halfword pairs via routine `0x11438`), writes each to MFbus (`0x70AA`) at `0x1143AE` step 4 + a checksum long; status ≠0 → NAK5, else ACK0. No `CM*=000074` symbol → held. |
| 0x3D | 075 | CMRPR | **NAMED** (locked, ⚠️ mnemonic UNMATCHED) | **identify/version report**: ACK0, then send 8 bytes from a fixed ROM table at `0x13BF4` (a card ID), then the literal ASCII bytes `0x20 0x49 0x30 0x31` = `" I01"` (a version tag). Sole `CM*=075`. **CAVEAT:** no §5.3.12–.49 command begins "RPR"; like CMSPE this name breaks the family's initial-letter convention or the command is outside the ACON set. Name is SINTRAN's; expansion UNKNOWN. |
| 0x3E | 076 | (HELD) | behavior, name HELD | **status/queue report**: check count `0x1131F6` (==0 → NAK code `0x0C`=12 "empty") and ready flag `0x1131FA` (==0 → NAK code `0x0B`=11 "not ready"), else ACK0 + return a packed status byte `(0x1131F6<<4) \| ((0x1131F8>>8)&0xF)`. No `CM*=000076` symbol → held. |

## Observation

The named commands (0x0E, 0x11–0x16, 0x1B–0x1F, 0x2A, 0x30–0x32, 0x36, 0x39) all carry `CM*` symbols.
The unnamed ones increasingly do **not** (0x17, 0x18 …) — they are firmware-internal HW-port commands
SINTRAN never symbolized. Naming those needs a manual §5.3 behavioral match or a real-traffic capture,
not the symbol table. **First dispatcher run 0x0D–0x18 fully carved (12/12).** Named+locked this session: 0x14 CMDWW,
0x16 CMDRW, 0x0F CMTEC, 0x10 CMREA, 0x20 CMLMA. Behavior-held (no `CM*` symbol): 0x0D, 0x15, 0x17,
0x18. **Every carve now greps `N500-SYMBOLS.SYMB` for `CM*=<octal cmd>` first** — CMTEC/CMREA/CMLMA
proved the table is not exhausted.

**⚠️ VALUE-COLLISION starts at 0o41 (do not repeat the sole-symbol assumption past 0o40).** The
"exactly one `CM*` symbol at the command's octal value → lock it" heuristic held cleanly for
0o16–0o40 (each had one CM-prefixed symbol). From **0o41 it breaks**: `CM*=000041` = {CMMAC, CMLMI,
CMTMO}, `CM*=000042` = {CMRMI, CMACO}. SINTRAN reuses the same numeric EQU value across unrelated
symbol namespaces, so a shared value no longer pins the command name. Behavior still disambiguates a
*hypothesis* — 0x21 (8-word write) = **CMLMI** "load M…", 0x22 (8-word read) = **CMRMI** "read M…",
matching the CMLMA(0x20)/CMLMI(0x21)/CMRMI(0x22) L/R family — but these are **held, not locked**.
**Authoritative disambiguator to run later:** the ND-100 SINTRAN driver's command-send sites (which
CM* symbol it actually emits for 0o41/0o42), or ND-05.020.01 §5.3 matched by description. Until then,
collision-valued arms are behavior-carved with the name held.

**⚠️ COUNT CORRECTION (0x3E) — NOW RESOLVED (2026-08-09):** the chain-order carve had jumped from
0x27 into the 0x33+ block and **skipped the 0x28–0x2D block**. Those 5 arms are now carved (below).
0x3E is the LAST arm in the dispatcher (0x6746 = the default "undefined command" reject, NAK code 6).

**✅ ALL 46 DISPATCHER ARMS CARVED.** The final block locked cleanly — each is the SOLE `CM*` symbol
at its octal value and its carved behavior does not contradict the name:
- **0x28 CMRAS** (050, arm `0x5FD6`) — read one word from HW (`0x7852`) → return word.
- **0x29 CMLDM** (051, arm `0x6016`) — write word split into two byte ports (`0x781C` lo / `0x77FE` hi).
- **0x2B CMWMP** (053, arm `0x60F6`) — write two longs, MFbus-write via `0x70AA`. Write twin of CMRMP.
- **0x2C CMRMP** (054, arm `0x6178`) — read long at addr (`0x7138`) → reply long (`0x6A04`). Read twin.
- **0x2D CMSET** (055, arm `0x6326`) — read 3-word block into a descriptor, apply via `0x7ACE`.

**⚡ §5.3 SECTION-ORDER DISAMBIGUATION (2026-08-09) — 4 collision names promoted.** Cross-checking
against the prior ND-500 carving thread (`REPLY-OCTOBUS-0x22-0x23-RESOLVED-2026-08-03.md`) plus a
direct read of the real manual `ND-05.020.01` §5.3 table (verified: §5.3.28 LMIR, .29 RMIR, .30 TBUF,
.31 TBUS, .38 RASTS, .41 WMPM, .42 RMPM, .44 SETTRAC all present as claimed). The command codes
`0o040`–`0o054` form a 13-code run that rises in step with §5.3.27–.42, the only skipped sections
being the two via-memory commands (LAOB32M/RAIB32M) which independently are the symbol-less 0x34/0x35.
That positional argument (which the LOCK BAR names as an authoritative disambiguator) resolves the
four held collisions, and my independent behavior carves corroborate every checkable neighbour
(0x28 RASTS=read-status, 0x2B WMPM=MFbus-write, 0x2C RMPM=MFbus-read, 0x2D SETTRAC=set-block):
- **0x21 → CMLMI = Load MIR (§5.3.28)**   **0x22 → CMRMI = Read MIR (§5.3.29)**
- **0x23 → CMBUS = Test Bus (§5.3.31)**   **0x33 → CMBUF = Test Buffer (§5.3.30)**

Also upgraded eight `[INFERRED]` expansions to manual-confirmed mnemonics (0x28 RASTS, 0x29 LMODE,
0x2B WMPM, 0x2C RMPM, 0x2D SETTRAC, 0x37 LOOP, 0x3A TESTMPM, 0x3B DCCD). Two names flagged: **CMSPE
(0x38) and CMRPR (0x3D)** — no §5.3 command begins "SPE"/"RPR", so their locks rest on the sole CM*
symbol alone; the manual mnemonic is unmatched (recorded as a caveat, not unlocked).

**Named/locked total: 23** — CMTEC(0x0F), CMREA(0x10), CMDWW(0x14), CMDRW(0x16), CMLMA(0x20),
**CMLMI(0x21), CMRMI(0x22), CMBUS(0x23),** CMR16(0x24), CMR32(0x25), CML16(0x26), CML32(0x27),
CMRAS(0x28), CMLDM(0x29), CMWMP(0x2B), CMRMP(0x2C), CMSET(0x2D), **CMBUF(0x33),** CMLOO(0x37),
CMSPE(0x38), CMTES(0x3A), CMCCD(0x3B), CMRPR(0x3D).
**Held (8, NO `CM*` symbol): 0x0D, 0x15, 0x17, 0x18, 0x34, 0x35, 0x3C, 0x3E.** Of these, the
**0x34/0x35 Group-A via-memory pair is now direction-resolved (2026-08-09):** the carved data
direction fixes the manual mnemonic — **0x34 = LAOB32M** (memory→register load, §5.3.37, via-memory
twin of 0x27 CML32=LAOB32D) and **0x35 = RAIB32M** (register→memory read, §5.3.34, via-memory twin of
0x25 CMR32=RAIB32D). §5.3.34/.37 verified against the real manual. These are `[D]` derived from
direction, NOT CM*-symbol locks (both symbols confirmed absent), so they stay out of the test.
The remaining **6 truly nameless** arms — 0x0D, 0x15, 0x17, 0x18, 0x3C, 0x3E — have neither a `CM*`
symbol nor a §5.3 match.

**Only work left in this carve:** the 6 nameless arms have no symbol to bind. 0x0D/0x3E look like
status/result-block readers, 0x15 a MFbus write-block, 0x3C the 0x3B write twin, 0x17/0x18 undocumented
everywhere — closing them needs a hardware-code source or a real ND-100 traffic capture, not the
symbol table.

Also note: `0x1143A0`–`0x1143A6` is now identified as a small **result/status block** —
`0x1143A0/A2/A4` = three result words, `0x1143A6` = their ready flag. Sits alongside the
already-pinned `0x1143AC` mode flag, `0x1143AE` (0x15 count+addr) and `0x1143B4` (trace select).
The `0x114xxxx` region is the ACCP firmware's scratch/parameter RAM.
