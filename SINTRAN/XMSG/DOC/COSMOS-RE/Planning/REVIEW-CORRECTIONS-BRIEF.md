# Corrections Brief — RE-verify the cos-file-tra / cos-xftra analyses against the binary

**To:** the LLM that produced `COS-FILE-TRA-E02-XMSG-Analysis.md`, `COS-XFTRA-E02-Analysis.md`,
`CosFileTraE02.cs`, `CosXftraE02.cs`.
**From:** the cos-conn-to / cos-fa-serv analysis session.
**Why:** an independent line-by-line review of the *sibling* reconstruction (`CosConnToE02.cs`)
found a class of errors that are **methodological, not typos** — so your four deliverables are
very likely to contain the *same* error class. Please re-verify them against the Ghidra DB and the
measured specs. This brief gives the root-cause rules, the specific checks to run, and the new
`[BIN]`-verified facts to reconcile.

Authoritative sources (do not "improve" from memory — trace/measure):
- Wire truth: `…\SINTRAN\XMSG\DOC\XMSG-PROTOCOL.md`, `…\SINTRAN\TAD\TAD-Message-Formats.md`, the pcap decode report.
- Symbols: `F:\ND\SINTRAN-K05-XMSG-2026\XMSG-Symb\*` (XF codes, XM* offsets, msg-type codes).
- Binaries in the Ghidra project: `cos-file-tra-e02.prog`, `cos-xftra-e02.prog`.

---

## 1. Root-cause rules (the review found all errors reduce to these)

1. **Decode the byte, don't infer it.** Every wire field must come from a specific `SBYT`/`LBYT`
   instruction with an explicit byte index (`SAX n` / `AAX` chain) and the constant it writes/reads.
   If you cannot point at the instruction, the field is **`UNKNOWN`** — never a plausible constant.
   (In the sibling file this produced a fabricated `TTYP = 01 08` — actually the FBSI class pasted
   into the wrong builder — and an off-by-one field offset that contradicted its own analysis doc.)
2. **Decode tag/opcode *values*, not just names.** Naming an op from a command string is not a wire
   spec. You need the numeric **opcode constant** and the **param tag bytes**.
3. **Keep the "type" fields separate** with one glossary: SINTRAN-header **Subtype** (0x0E=Data…),
   XMSG **msg-type return code** (XMTNO1/XMROU2/**XMTHI3**/XMTRE4/XMKIK5/XMTPS6), TAD opcode,
   FA request-op, FA param-tag, FA entry-type. A `==N` compare must be attributed to the right field.
   (Sibling error: labelled a `msg-type==3` compare "Data" — it is **XMTHI = high-priority**.)
4. **Preserve confidence tags** (`VERIFIED`/`INFERRED`/`CANDIDATE`) from the measured docs into the
   `.cs`/`.md`. The binary corroborates; it does not upgrade a CANDIDATE to fact.
5. **State the layer boundary loudly.** These binaries are **application-level, above `MON 200B`**.
   The envelope (seed/Counter/channel), secure-ACK closed form, odd-length LAPB address, and the
   ≤2-datagram flow-control window are **kernel-invisible** in these binaries. Your `.cs` explains the
   app's *intent*; it can never be a transport build-spec, and a node built from it alone will crash
   the real machine the way the early probes did. Put this caveat at the top of both `.md` files.
6. **Apply your own findings.** If the disasm shows an *append-into-one-buffer-then-flush* builder,
   the `.cs` must not send each field as a separate datagram.

---

## 2. Specific checks to run on YOUR files

### 2a. The typed-param tags are PROGRAM-SPECIFIC — do NOT assume a shared table
> **CORRECTED 2026-07-07** after the file-tra/xftra session verified this against its binaries.

The `cos-fa-serv` emit tag bytes are **`0x92`=INT16 / `0x94`=INT32 / `0xA2`=classA / `0xF2`=classF**
`[BIN-VERIFIED, fa-serv only]`. **They do NOT appear in cos-file-tra or cos-xftra** (`SAA 0x92`/`0x94`
= 0 hits in both — verified by the file-tra/xftra session). My earlier suggestion that the family
shares this tag set was itself an *inference* and is **wrong** — the exact "don't infer" error.
- **cos-xftra / cos-file-tra** use a different, **letter-indexed** scheme: `(typeByte & 0x7F) − 0x41`
  → 6-way jump on codes `0x41..0x46` (A–F) → a common XFWRI/XFREA accessor (`decode_param_value`
  file-tra @~0x640a). So each program encodes typed params its own way; publish each program's tag
  scheme separately, both `[BIN-VERIFIED]`.

### 2b. Field offsets
For every message field your `.cs`/`.md` names (opcode position, length position, param positions,
XD-block fields `XDHAC/XDDST/XDSNA/XDREF`, etc.), verify the offset against the actual `SBYT`/`LBYT`
index in the binary. Any field you cannot anchor to an instruction → mark `UNKNOWN`.

### 2c. Opcodes
- file-tra's **only confirmed on-wire send** is the XROUT query with baked opcode `0x0845`
  (byte1 `0x45` = XSGNI). That is solid `[BIN+SYM]`. **But** confirm the **reply payload** is marked
  `UNKNOWN` (needs a live capture) and not invented.
- The file-transfer *data* commands (Transfer/Append-batch/Compress) had **no traced send edge** —
  make sure the `.md`/`.cs` say so explicitly rather than implying a wire format for them.
- Re-check `op = -0x64,B & msg_opcode_mask` (file-tra) against the actual mask constant.

### 2d. The msg-type / high-priority knob
Check every `== N` compare on an XFRCV/receive return. If either binary compares the returned
msg-type to 3, that means **XMTHI (high-priority)** traffic (kernel-local, invisible in pcap) — a
real `[BIN]`-only fact worth recording, but label it XMTHI, **not** "Data".

### 2e. Confidence + layer tags
Sweep both `.md` and both `.cs`: restore `VERIFIED/INFERRED/CANDIDATE` on every opcode/field/name,
and add the app-layer caveat (rule 5) at the top.

---

## 3. New `[BIN]`-verified facts to reconcile into your files

1. **FA/QFORM param tags:** `0x92`=INT16, `0x94`=INT32, `0xA2`=classA, `0xF2`=classF (per §2a).
2. **msg-type `3` = XMTHI** (high-priority); a receive filter of `==3` means the program only accepts
   high-priority messages → its traffic is sent with `XFHIP`.
3. **Ghidra fragment inflation:** in dense/garbled regions Ghidra mis-splits one routine into many
   phantom `FUN_ram_*`. Don't count or name fragments as distinct functions; verify a function's
   decompile isn't a byte-identical tail of its neighbour.
4. **PLANC name-strings mark the END of a routine, not the start** — re-check any "the string before
   the RADD names this routine" assumption.

---

## 4. Deliverable

For each of your four files: a corrected version where **every wire fact is anchored to a decoded
byte** (cite the `SBYT`/`LBYT` address in a comment), unknowns are tagged `UNKNOWN`, the type-field
glossary is applied, and confidence + layer tags are restored. Same discipline being applied to
`CosConnToE02.cs` and `CosFaServerE04.cs` on this side.
