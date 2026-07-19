# ADDENDUM - MON 503B (InputString / DVINST / NINST): the message-buffer parameter layout

**Full path of this file:**
`tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/503B-InputString/ADDENDUM.md`

Date: 2026-07-15. Companion to this folder's `README.md`, `503B-InputString.ASM`,
`503B-InputString.pseudo.c`, `503B-InputString.bin`.

## Why this addendum exists

The carve `README.md` (section "Parameter / register contract" + "Honest caveats") is honest that
the EXACT parameter fields of MON 503B are **inferred, not byte-isolated**: the max-byte-count, the
destination-array pointer, and the returned-byte-count write-back are all read/written as numeric
displacements into the ND-500 **message buffer**, and the carve captured the handler CODE but not
the data-structure DECLARATIONS that give those displacements meaning. This addendum collects the
pieces that DO pin the layout down - from three other artifacts - plus a live consumer trace, so a
future reader does not have to re-derive it.

## 1. WHY the carve alone cannot name the fields (root cause, not a defect)

The carve is a stripped-binary recovery. The 503B handler reads its parameters like this (real
bytes, setup block `141342B..141417B`, see `503B-InputString.ASM`):

```
141360  054066  LDX 66            ; X = a base pointer (message-buffer / datafield base)
141361  046111  LDA ,X 111        ; read the field at displacement 111 from that base
...
141414  047030  LDA I ,X 30       ; read the (indirect) field at displacement 30
141415  004770  STA ,B -10        ; stash into a local slot
...             STA ,B -11        ; (part-2 block) returned byte count staged here
```

The disassembly is COMPLETE (218 words, closed control flow - not cut short) and the pseudo-C is a
FAITHFUL model of that control flow (not over-simplified). The limitation is structural: the code
says "field at displacement 111 / 30 / -11", but *what those displacements MEAN*
(`MAXBYT` / destination array / `NOCHRET`) is defined by struct/symbol declarations that live in the
SINTRAN **NPL source**, which is a different artifact than the carved binary. Same as decompiling a
stripped executable: you see `[base+0x49]` but not `struct.max_bytes` without symbols.

## 2. The NPL source DOES name the fields (from the NC oracle)

The tier-2 oracle doc built from NPL - full path
`SINTRAN/ND500/mon-oracle-for-NC/tier2-123B-RELES_54B-MDLFI_503B-DVINST.md`
(section "Q3 - 503B DVINST (InputString): break strategy + returned byte count") - names the exact
message-buffer fields from the resident char-move/break loop shared by DVINST(503) and DVIO(511):

| NPL field | Meaning | Evidence (NPL, RP-P2-N500.NPL / MP-P2-N500.NPL) |
|-----------|---------|-------------------------------------------------|
| `X.MAXBYT`   | max bytes to read (DVINST). `X.11MXBRK` for DVIO(511) | `130326  IF X.SMCNO=511 THEN X.11MXBRK ELSE X.MAXBYT` |
| `X.NOCHRET`  | RETURNED byte count (DVINST). `X.11NOCRET` for DVIO | `130416  ELSE A:=0; AD=:X.NOCHRET` (count written = 5FYLLE) |
| `X.SMCNO`    | the monitor-call number (503 vs 511) - selects the DVINST vs DVIO variant | `130326` (branch on SMCNO) |

VERDICT from that oracle (real-SINTRAN behaviour, cross-checked against the two emulators):
- The terminating **break character IS read, stored, and COUNTED**. `MaxNo` is an **inclusive**
  ceiling (the char that reaches MaxNo is kept and counted). So `NOCHRET` INCLUDES the break byte.
  The emulator that returns **14** (break byte counted) matches real SINTRAN; **12** under-counts.
- DVINST(503) and DVIO(511) share the resident char-move+break loop; they differ only in which
  message field holds max/return (`MAXBYT`/`NOCHRET` vs `11MXBRK`/`11NOCRET`), keyed by `X.SMCNO`.

So `MAXBYT` and `NOCHRET` are the authoritative field NAMES; their exact numeric displacements into
the message buffer are what the carve marks "inferred" (displacements `111` / `30` / `-11` in the
handler above are the candidates, not yet mapped one-to-one to the NPL names).

## 3. The documented ND-500 argument order (YAML / manual)

Authoritative arg contract - full path
`Developer/MON/calls/503B_InputString.yaml` (from
"Monitor Calls.md", ND-860228.2 EN). 14 arguments, in order:

| # | Name | Dir | Meaning |
|---|------|-----|---------|
| 0 | DevNo        | I | logical device number (1 = own terminal) |
| 1 | MaxNo        | I | max bytes to read before break (== NPL `MAXBYT`) |
| 2 | NoOfBytesRet | O | number of bytes read (== NPL `NOCHRET`) |
| 3 | Buff         | O | buffer that receives the input line |
| 4 | BreakStrat   | I | break setting (8 = user table) |
| 5 | EchoStrat    | I | echo setting (8 = user table) |
| 6..9  | BreakT1..T4 | I | 128-bit break table (bits set = break) |
| 10..13| EchoT1..T4  | I | 128-bit echo table (bits 0 = echo) |

The caller lays these 14 out; SINTRAN's level-12 driver moves them into the message buffer that the
handler in this folder reads. NB: MON 503B is an ND-500 **level-12** call - not GOTAB-dispatched;
the arg->message-buffer transfer is done by the (uncarved) level-12 GOSW path.

## 4. LIVE CONSUMER TRACE - the ND LINKER passes an arg-1 that is NOT a byte count

Consumer: `/mnt/d/ND/500/nd-linker/linker-b01.dom` (ND LINKER B01), run under the nd500x emulator
(`/home/ronny/repos/nd500x`). The linker's 503B call site (disassembly of
`/mnt/d/ND/500/nd-linker/linker-b01.dom.asm` around `0xB004AC90`):

```
B004AC90: call $0xF8000143, $14, b.20,b.24,b.28,IND(b.32),b.36,b.40,
                                  b.44,b.48,b.52,b.56,b.60,b.64,b.68,b.72
          ; $0xF8000143 = segment 31 | 0x143 = MON 0o503 (503B), 14 args
```

Observed argument VALUES at that call (nd500x arg-extraction; command "LIST-DOMAINS"):

| # | field (YAML) | LINKER value | NC value (for comparison) |
|---|--------------|--------------|---------------------------|
| 0 | DevNo        | `0x00000000` | `0x00000000` |
| 1 | MaxNo        | **`0xF80000CB`** | `0x00000001` |
| 2 | NoOfBytesRet | `0x00000000` | `0x10000140` (addr) |
| 3 | Buff (IND b.32) | buffer @ `0xB0001C2E` | works |
| 4 | BreakStrat   | `0x00000007` | `0x00000001` |
| 5 | EchoStrat    | `0xFFFFFFFF` | `0x00000001` |
| 6 | BreakT1      | `0x20202020` | `0` |
| 7 | BreakT2      | `0x20202020` | `0` |
| 8 | BreakT3      | `0xB0001D48` | `0` |
| 9 | BreakT4      | `0xB004D8DE` | `0` |
| 10| EchoT1       | `0xFFFFFFFF` | `0` |
| 11| EchoT2       | `0x00000000` | `0` |
| 12| EchoT3       | `0x00000000` | `0` |
| 13| EchoT4       | `0x00000003` | `0` |

Key anomalies (all OBSERVED on nd500x, so partly emulator-dependent - see caveat 6):
- **arg1 (MaxNo) = `0xF80000CB`**. That is `0xF8000000 | 0xCB` = a segment-31 address (the MON-call
  area), NOT a plausible byte count. NC by contrast passes `MaxNo=1` (reads 1 char per call).
- **args 6..7 = `0x20202020`** (ASCII spaces) and **args 8..9 = pointers** (`0xB0001D48`,
  `0xB004D8DE`) - not a coherent 128-bit break table. Read positionally as BreakT1..T4 they make
  the letter `T` (0x54) a "break" char, which truncated every linker command at its first `T`
  (that specific symptom was worked around in nd500x, see section 5).

The COMMAND BYTES themselves are read correctly into Buff. The failure is downstream, in the
linker's command PARSER:

```
; linker command-parse routine, disasm of linker-b01.dom.asm @0xB00391A0:
B00391A1: r := b.8            ; R = [B+8] = 0xB0001D48  (== the linker's own 503B arg[8] value)
B00391A3: w move r.48, b.36   ; b.36 = [R+48]  = the "length" the parser will use
B00391AF: w2 := b.36; w2-1; w2 =: b.36    ; decrement count
B00391B3: by3 := b.50(r2)     ; read byte at (b.50 base) indexed by r2 = count-1   <-- faults
B00391B6: by3 =: b.306
```

Observed at the fault (nd500x, LIST-STATUS): `b.36 (count) = 0xF80000CA`, `b.50 (base) = 0x4C495354`
= "LIST", `R = 0xB0001D48`. The count `0xF80000CA` = `arg1 (0xF80000CB) - 1`. So the parser is
using (MaxNo - 1) as the input length -> ~4.29 billion -> the indexed read walks off into unmapped
memory `0xA8001CF8` -> protection-violation trap (trapBit `0x1000000000` = bit 36) -> the linker's
own trap handler dumps registers ("--- ND-500 ... BACKUP ON MAP ---") and exits. This happens for
EVERY command (OPEN-DOMAIN, LIST-STATUS, ...), so it is the common parse path, not command-specific.

## 5. The open question (what still needs proof)

The parser uses `[R+48]` as the input length and gets `MaxNo-1`, NOT the returned `NOCHRET` count.
Two hypotheses, UNRESOLVED:

- (H1) The linker's 503B truly expects the returned count (`NOCHRET`) to be written to a
  MESSAGE-BUFFER field that the parser later reads as `[R+48]` - and the nd500x handler writes the
  count to the wrong place. nd500x currently writes `NoOfBytesRet` to the arg-2 slot address
  (`/home/ronny/repos/nd500x/src/libmon/handlers/mon_503B_InputString.c`, the
  `mon_write_param_word(ctx, 2, bytes_read)` call), i.e. the documented arg[2], NOT into a
  message-buffer field. Per this carve, real SINTRAN writes `NOCHRET` into the message buffer, so
  the nd500x count may simply be landing where the linker does not look.
- (H2) `0xF80000CB` in arg1 is not the linker's intent at all but an nd500x upstream mis-computation
  that stuffed a code address into the MaxNo local (`b.24`) before the call. Not yet traced back.

Deciding between H1 and H2 needs: (a) the exact numeric displacement of `NOCHRET` in the message
buffer (map NPL `X.NOCHRET` to the carve's `STA ,B -11` / `LDA ,X` displacement), and (b) a trace of
where the linker sets `b.24` (MaxNo) before `0xB004AC90`. The NPL source
(`RP-P2-N500.NPL`, `MP-P2-N500.NPL`) is the artifact that carries the `5MB...` message-buffer field
displacements; cross-referencing it to this carve's `111/30/-11` accesses would close the gap.

## 6. Caveats

- Section 4/5 VALUES are observed on the nd500x emulator, whose 503B handler, level-12 arg->message
  transfer, and CALLG arg-extraction are themselves approximations. Treat them as "what the current
  consumer+emulator do", not as ground truth for SINTRAN. The linker's intent for arg1 is exactly
  what H1-vs-H2 is about.
- This carve's own dispatch (level-12 GOSW slot 3 -> NINSTR) is UNVERIFIED (uncarved overlay), and
  the message-field displacements are inferred - see this folder's `README.md` "Honest caveats".
- nd500x currently ships two PRAGMATIC 503B workarounds (not fixes) so the linker can read whole
  command lines: clamp an out-of-range `MaxNo` to 2048, and, when a user break table (strat 7/8)
  does not break on CR, fall back to MAC-style CR/LF/ESC break. These mask the arg-layout mismatch;
  they are documented in `/home/ronny/repos/nd500x/docs/MON_TO_BINARY_PLAN.md`.

## 7. Source index (all full paths)

- This carve: `tools/sintran-segment-carver/versions/L-VSX-500/re/mon-analysis/503B-InputString/`
  (`README.md`, `503B-InputString.ASM`, `503B-InputString.pseudo.c`, `503B-InputString.bin`)
- NPL oracle (field names MAXBYT/NOCHRET, break/count semantics):
  `SINTRAN/ND500/mon-oracle-for-NC/tier2-123B-RELES_54B-MDLFI_503B-DVINST.md`
- Documented arg order: `Developer/MON/calls/503B_InputString.yaml`
- Consumer binary + disassembly: `/mnt/d/ND/500/nd-linker/linker-b01.dom`,
  `/mnt/d/ND/500/nd-linker/linker-b01.dom.asm`
- nd500x emulator handler: `/home/ronny/repos/nd500x/src/libmon/handlers/mon_503B_InputString.c`
- nd500x plan / running notes: `/home/ronny/repos/nd500x/docs/MON_TO_BINARY_PLAN.md`
- NPL source to mine for the message-buffer displacements (5MB fields): `RP-P2-N500.NPL`,
  `MP-P2-N500.NPL` (referenced by the oracle doc above).

---

## RESOLVED (2026-07-15) - it was NOT 503B; it was 313B IBRISZ not returning W1

Follow-up tracing (nd500x write-watch + disassembly of `/mnt/d/ND/500/nd-linker/linker-b01.dom`)
overturned the 503B-arg-convention hypothesis in sections 4-6 above. The real chain:

- The linker gets its command-line LENGTH from **MON 313B IBRISZ (InBufferState)**, not from the
  503B `NoOfBytesRet`. Call site `0xB004DA8F`: `callg b.64, $2, IND(b.24), IND(b.32)` where
  `b.64 = 0xF80000CB` = the seg-31 gate address of MON 313B. Immediately after: `0xB004DA96
  w1 =: b.68` - the linker reads the result from **W1**, and (via `0xB004DBEB w4 =: r.0`)
  propagates it: parser b.24 -> r.40 -> worker `0xB00476B8` fills `[struct+48]` -> parser
  (`0xB0039160`, `r := b.8; w move r.48,b.36`) uses it as the scan length.
- Our nd500x MON-call CALLG left **W1 = the call-target address (0xF80000CB)** because the 313B
  handler wrote its count only into the output ARG, never into W1. So the linker's "length" = the
  gate address = ~4.29 billion -> the parse scan walked off the end -> PV `0xB00391B6`
  (data `0xA8001CF8`) -> the linker's trap handler dumped registers and exited. This happened for
  EVERY command (the common command-length step), which is why it looked command-independent.
- The `0xF80000CB` that also appeared in the 503B `arg1` slot (section 4) is the SAME MON-313B gate
  address the linker keeps in a local; its presence in the 503B args was a red herring, not a
  MaxNo. The 503B standard layout in section 3 is correct and our handler matches it.

FIX (nd500x): `/home/ronny/repos/nd500x/src/libmon/handlers/mon_313B_InBufferState.c` now also
returns the count in W1 (`ctx->set_error_code(ctx->cpu, remaining)`). RESULT: the command-parser
PV at `0xB00391B6` is GONE; the linker reads `LIST-STATUS`, processes it, and enters its normal
interactive command loop (now polling MON 1B INBT for the next command - a separate, expected
input-delivery issue, not a crash).

LESSON for emulator authors: after a MON CALLG, do not leave the gate/target address in W1. A
handler whose contract returns a value the consumer reads from W1/A MUST set it explicitly;
otherwise the leftover CALLG target leaks in as a (huge) result. Worth auditing other handlers
that only write output args and never set W1.
