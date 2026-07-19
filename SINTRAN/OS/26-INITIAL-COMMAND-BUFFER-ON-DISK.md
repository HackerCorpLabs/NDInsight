# Reading and Writing the SINTRAN Initial-Command Buffer From a Disk Image

**Status:** VERIFIED end-to-end on live SINTRAN III VSX/500 L + three real SMD images (2026-07-10)
**Scope:** SINTRAN III VSX / VSX-500, versions K03 / L07 / M06
**Purpose:** A complete, implementable specification for locating, decoding, and encoding the
SINTRAN **initial-command buffer** (`@LIST-INITIAL-COMMANDS` / `@INITIAL-COMMAND` /
`@NEXT-INITIAL-COMMAND`) inside an NDFS disk image — **offline, without booting the system**.

Written for implementers extending:
- `/mnt/e/Dev/Ronny/norskdata-ndfs` — `ndfs-c`, `ndfs-ts`, `ndfs-py`
- `/mnt/e/Dev/Ronny/RetroFS/src/RetroFS.NDFS` — C#

Cross-references:
- [24-INITIAL-COMMANDS-AND-STARTUP.md](24-INITIAL-COMMANDS-AND-STARTUP.md) — what initial commands
  are, and the disk mode files (`HENT-MODE` / `LOAD-MODE`) they chain into
- [22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md](22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md)
  — segment-table field layout (§3.3) and the `LOGAD`/`SEGLE`/`MADR` semantics used below
- `tools/sintran-segment-carver/EXTRACTING-SEGMENTS.md` — the 2048-byte page model used here

Notation: **words are 16-bit, big-endian.** 1 page = **1024 words = 2048 bytes**. Octal is written
`NNNNN₈`. Every claim is tagged **VERIFIED** or **UNCERTAIN**.

---

## 0. Executive summary

| Question | Answer | Confidence |
|---|---|---|
| Is the initial-command buffer a file? | **No.** It has no file form. It lives at kernel symbol `INIBU` inside the SINTRAN **command segment**, whose disk image is inside `(SYSTEM)SEGFIL0:DATA`. | VERIFIED |
| Can it be located without byte-searching? | **Yes.** Symbol value + the segment table read off the same image ⇒ an exact byte offset. | VERIFIED |
| Can the segment table be found offline? | **Yes** — it self-validates: the page contains an entry whose `MADR` equals that page number. | VERIFIED |
| Is the location version-portable? | **Yes** — one symbol (`INIBU`) per version; everything else is derived from the image. | VERIFIED (K03/L07/M06 symbols; runtime-checked on L07) |
| Is the encoding known? | **Yes** — 7-bit ASCII, `'` (0x27) terminates each command, word-aligned with NUL, a lone `'` ends the buffer. | VERIFIED |
| Is there a maximum length? | **Yes — 254 bytes of text** (`textLen`), enforced by a literal `252` in the `NEXIN` handler. No ND manual documents it; it was read from the code (§2.2). | VERIFIED (L07, M06) / UNCERTAIN (K03) |
| Can it be written? | **Yes** — in-place; the buffer is a fixed 131-word region, so rewriting never moves anything else. **You must also update the length cell (§4.3)** — text alone is ignored. | VERIFIED end-to-end: offline write → boot → `@LIST-INITIAL-COMMANDS` (§6.1) |

---

## 1. What the buffer is

The initial commands are the list SINTRAN executes **first** at every restart, before the disk mode
files. On a live system:

```
@LIST-INITIAL-COMMANDS,,

ENTER-DIR,,DI-75-1,0
ENTER-DIR,,DI-74-1,0
SET-AVAIL
```

They are set with `@INITIAL-COMMAND <cmd>` (first entry, must be `@ENTER-DIRECTORY`) and appended
with `@NEXT-INITIAL-COMMAND <cmd>`. Typically they enter the directories and then chain into
`LOAD-MODE:MODE` via `APPEND-BATCH` (doc 24 §4.1).

The buffer survives restarts because it lives inside the **SINTRAN memory image** that is
checkpointed to `SEGFIL0` — not because it is stored as a file. **VERIFIED.**

---

## 2. The anchor symbol: `INIBU`

The buffer begins **exactly at** the kernel symbol `INIBU` ("initial buffer"), from each version's
`SYMBOL-1-LIST.SYMB.TXT` (`SINTRAN/NPL-SOURCE/SYMBOLS/{K03,L07,M06}/`):

| Version | `INIBU` (octal) | decimal (word address) |
|---|---|---|
| K03 | `067172₈` | 28282 |
| L07 | `074123₈` | 30803 |
| M06 | `102327₈` | 34007 |

**VERIFIED** (present in all three symbol lists; L07 value confirmed byte-exact against a live
system, §6).

> Note: `INICO` (`134620₈` L07 / `137255₈` M06) in `FILSYS-SYMBOLS` is a *different*, file-system
> symbol and is **not** the command-text buffer. Doc 24 §2 named it as the buffer; that was an
> inference. The text is at `INIBU`. **VERIFIED (corrects doc 24).**

### 2.1 Buffer region — version-stable

The `@INITIAL-COMMAND` handler `INCOM` sits at **`INIBU + 131` words in K03, L07 and M06 alike**
(`INCOM` = `067375₈` / `074326₈` / `102532₈`; `NEXIN`, the `@NEXT-INITIAL-COMMAND` handler, is the
next word). The region therefore lays out as:

| Words | Content |
|---|---|
| `INIBU + 0 … +129` | command text (260 bytes) |
| `INIBU + 130` | **length cell** — byte count of the text (§4.3) |
| `INIBU + 131` | `INCOM` — **executable code**. Never write here. |

**VERIFIED** (symbol arithmetic on all three versions; the three uses of `INIBU+130` in the
disassembled handler, §2.2).

> **Correction (2026-07-10).** An earlier revision of this document proposed a smaller "safe bound"
> of `INIBU+44` words (88 bytes) for L07, on the grounds that the symbol `ACLEA` sits there.
> **That was wrong.** `ACLEA` is `ACLEAR`, a *bitmask constant* (`CURPROG.ACTPRI/\ACLEAR\/T`,
> `NPL-SOURCE/NPL/CC-P2-COMMON.NPL:862`), not an address — its value is `074177₈` in **all three**
> versions, which is what gives it away. SINTRAN symbol lists mix constants and addresses freely;
> a symbol "inside" an address range means nothing unless the symbol is known to be an address.
> Disproved empirically too: a live L07 system accepts a text length of 114 bytes without complaint.

### 2.2 The real limit — read from the `NEXIN` code (L07)

There is **no documented maximum** in any ND manual (`@INITIAL-COMMAND`,
`@NEXT-INITIAL-COMMAND`, `@LIST-INITIAL-COMMANDS` in ND-60.128.5 are silent on size), and the
handler is not in the NPL source subset. The limit is a literal in the handler itself, recovered
by breaking on `NEXIN` on a running L07 system (so the command segment is correctly mapped) and
disassembling.

```
INCOM  074326:  STZ  -1              ; length cell (INIBU+130) := 0   <- @INITIAL-COMMAND resets
NEXIN  074327:  RADD CLD SL DD
       074330:  JPL  I ,B -37        ; read the command line
       074332:  LDX  -6              ; X := [INIBU+130]  -> append at current length
loop:  074334:  LDT  115             ; T := [074451] = 000374 octal = 252   <-- THE LIMIT
       074335:  SKP  IF DT GRE SX    ; continue only while T >= X
       074336:  JMP  13              ;   else -> buffer-full error path
       074337:  JPL  I 113           ; fetch next character into A
       074340:  LDT  113             ; T := [074453] = 074123 = INIBU  (byte-pointer base)
       074341:  BSET ZRO SSPTM
       074342:  SBYT                 ; store the byte at byte offset X from INIBU
       074343:  BSET ONE SSPTM
       074344:  SAT  15              ; T := 15 octal = 13 = CR
       074345:  SKP  IF DA UEQ ST    ; was the character a CR?
       074346:  JMP  5               ;   yes -> finish
       074347:  AAX  1               ; X := X + 1
       074350:  JMP  -14             ; loop
fin:   074353:  SAA  47              ; A := 47 octal = 0x27 = "'"
       074354:  LDT  77              ; T := INIBU
       074355-7:  SBYT               ; store "'" AT THE SAME X -> it OVERWRITES the CR
       074360:  AAX  2               ; X := X + 2
       074361:  BSET ZRO 0 DX        ; clear bit 0 -> round to an even (word) boundary
       074362:  STX  -35             ; [INIBU+130] := X   -> the new length cell
```

Three independent instructions (`STZ -1` at `074326`, `LDX -6` at `074332`, `STX -35` at `074362`)
all resolve to `074125₈` = `0x78D5` = **`INIBU + 130`**, which is how the length cell was confirmed.
Live reads at a `NEXIN` breakpoint: `[074451] = 0x00FC` (252), `[074453] = 0x7853` (= `INIBU`), and
the length cell held `0x005E` (94) after two 20-byte commands had been appended to the 54-byte
default — exactly `54 + 20 + 20`. **VERIFIED.**

**The limit, stated precisely:**

```
a character may be stored only while  X <= 252        (X = byte offset from INIBU)
the "'" that ends a command is stored at that same X
the length cell then becomes  (X + 2) rounded down to even   ->  at most 254
```

| Quantity | Value |
|---|---|
| Max byte offset at which a character may be stored | **252** (`0o374`) |
| **Max value of the length cell (= max total text)** | **254 bytes** |
| Text region physically available before the length cell | 260 bytes |
| First word that is executable code (`INCOM`) | `INIBU + 131` |

So a writer must enforce **`textLen <= 254`**, where
`textLen = Σ roundUpToEven(len(command) + 1)`. Exceeding it on a live system takes the
buffer-full error branch at `074351`; exceeding it in an *offline* write would overwrite the
length cell and then `INCOM`'s code — corruption.

**Confirmed independently on M06** (2026-07-10, `BIGDISK0-M.IMG`, SINTRAN III VSX/500 M).
Breaking on M06's `NEXIN` (`102533₈` = `0x855B`) gives a **byte-for-byte identical instruction
sequence**, and the same three anchors:

| | L07 | M06 |
|---|---|---|
| `INIBU` | `074123₈` (`0x7853`) | `102327₈` (`0x84D7`) |
| length cell (`INIBU+130`) | `0x78D5` | `0x8559` |
| `INCOM` = `STZ -1` | `074326₈` | `102532₈` |
| `NEXIN` | `074327₈` | `102533₈` |
| byte-pointer base literal | `0x7853` = `INIBU` | `0x85AD` → `0x84D7` = `INIBU` |
| **limit literal** | `[0x7929]` = `0x00FC` = **252** | `[0x85AB]` = `0x00FC` = **252** |

The M06 length cell read `0x0030` (48) mid-append — its 40-byte default plus `CC TEST'` (8) —
exactly as the model predicts. **VERIFIED (L07 and M06).**

> **Scope:** `252` has now been read from the **L07 and M06** handlers. K03's literal has **not**
> been read, but its region is laid out identically (`INCOM` at `INIBU+131` in all three), so 254
> is expected there too. **VERIFIED (L07, M06) / UNCERTAIN (K03).**
> To confirm on K03: break on its `NEXIN` (`067376₈`), disassemble, and read the word the `LDT`
> immediately before `SKP IF DT GRE SX` points at.

> **Resolved — `CPTSL` on M06.** An earlier revision flagged this as an open question: `CPTSL`
> (`102455₈`) is a genuine address and lands at `INIBU + 86` words on M06, seemingly inside the
> buffer, suggesting a 172-byte bound. **It is not a bound.** M06's own `NEXIN` allows byte offsets
> up to 252 — SINTRAN writes straight through `CPTSL`'s address — so it cannot be a live variable
> there. The full 254-byte limit applies to M06 exactly as to L07. (Whatever `CPTSL` names, it is
> not referenced in the NPL source subset and does not participate in this structure.)

### 2.3 Why no CR or LF ever appears in the buffer

`074353: SAA 47` loads `0o47` = `0x27` = `'` and stores it **at the same byte offset `X`** where the
CR that terminated the typed command line was just written. The apostrophe literally overwrites the
carriage return. That is the mechanical reason the stored format uses `'` as the command separator
and contains no `0x0D`/`0x0A` at all (§4). **VERIFIED from code.**

---

## 3. Locating the buffer in the image (the algorithm)

No byte-searching. Everything derives from `INIBU` plus the image's own segment table.

### 3.1 Step 1 — read `(SYSTEM)SEGFIL0:DATA`

The SINTRAN memory image (all swapped segments) is an ordinary NDFS file owned by `SYSTEM`, named
`SEGFIL0:DATA` (some packs: `SEGFILE:DATA`). Read it with the existing `read_file` API. All page
numbers below are **relative to the start of this file**, in 2048-byte pages. **VERIFIED.**

### 3.2 Step 2 — find the segment table page (self-validating)

The segment table is itself a segment (`S3ISGT` "image of segment table", `S3SSGT` "save of segment
table"). Locate its page by scanning `SEGFIL0` for a page that satisfies **all** of:

1. **Entry 0 is all-zero** (segment 0 is never used).
2. It has **≥ 40 plausible entries**: for entry `n` at page offset `n*16`,
   `0 < (SEGLE & 0x3FF) < 1024`, `MADR > 0`, `FLAG & 1` (the `5OK` bit), `SGSTA & 0xE000`
   (some protection bit set).
3. **It is self-describing:** some entry has `MADR == thisPageNumber` and `SEGLE <= 32` — i.e. the
   table page is itself covered by the segment that describes it.

Criterion 3 is what makes this reliable; without it, unrelated pages produce false positives. If
several pages qualify, take the one with the **most** plausible entries.

**VERIFIED:** on both L07 packs this yields exactly pages **1275** (`S3ISGT`, 83 entries) and
**321** (`S3SSGT`, 66 entries); page 1275 wins and gives correct results.

Segment-table entry layout (8 words, doc 22 §3.3):

| Word | Byte off | Field |
|---|---|---|
| 0 | 0 | `SEGLI` |
| 1 | 2 | `PRESE` |
| 2 | 4 | `LOGAD` — virtual base page |
| 3 | 6 | `SEGLE` — length in pages (low 10 bits) |
| 4 | 8 | `MADR` — **page** offset of the segment in `SEGFIL0` |
| 5 | 10 | `FLAG` — SEGFIL# = `(FLAG>>13)&7`; bit0 = `5OK` |
| 6 | 12 | `SGSTA` |
| 7 | 14 | `BPAGL` |

### 3.3 Step 3 — find the segment containing `INIBU`

Each segment's virtual base **within its 64 KW bank** is the low 6 bits of `LOGAD`:

```
segBaseWord = (LOGAD & 0x3F) * 1024
```

Scan entries `n = 1..127` for the one where

```
segBaseWord <= INIBU < segBaseWord + (SEGLE & 0x3FF) * 1024
```

**Caution — several segments match.** Save/image copies share the same `LOGAD` low bits, so on a
real L07 pack ~40 entries satisfy the containment test. Do **not** take the first blindly; iterate
in segment order and accept the first candidate whose **length cell is plausible**
(`0 < textLen <= 260`, §4.3) **and** whose text parses to ≥ 1 command. The save copy `S3SCP`
(seg `13₈`) contains an *empty* buffer (`''`) and is correctly skipped by this rule.

**VERIFIED (L07):** this selects **segment 3 = `S3CP`, the running command segment** — `LOGAD = 0x24C`
⇒ `LOGAD & 0x3F = 12` ⇒ `segBaseWord = 12288 = 030000₈`, exactly the documented command-segment
virtual base; `SEGLE = 52` pages; `MADR = 1408`.

### 3.4 Step 4 — compute the byte offset

```
segRelWord = INIBU - segBaseWord
byteOffset = MADR * 2048 + segRelWord * 2        # offset within SEGFIL0:DATA
```

**Worked example (L07, both packs):**

```
INIBU        = 074123₈ = 30803
segBaseWord  = 030000₈ = 12288        (LOGAD 0x24C & 0x3F = 12, ×1024)
segRelWord   = 044123₈ = 18515        (= rel page 18, word 0123₈ into it)
MADR         = 1408
byteOffset   = 1408*2048 + 18515*2 = 2 920 614 = 0x2C90A6
```

The first command byte sits at exactly `0x2C90A6`. **VERIFIED byte-exact.**

---

## 4. Buffer encoding — the wire format

Verified against a live L07 system, including a command appended at runtime (§6).

```
command  := <7-bit ASCII text>  0x27  [0x00]      ; "'" terminates; NUL pads to a word boundary
buffer   := command*  0x27  0x00...               ; a LONE 0x27 (empty command) ends the buffer
```

Rules, precisely:

1. **Text is plain 7-bit ASCII, high bit CLEAR.** (Unlike SINTRAN *text files*, which set the
   parity bit. Mask with `& 0x7F` on read anyway; write with the high bit clear.) **VERIFIED.**
2. Each command is terminated by **`'` = 0x27** (the ND carriage-return character in command text).
3. After the `'`, a single **`0x00` is inserted only if needed to reach an even byte offset**
   (word alignment). It is *padding, not a separator.* **VERIFIED** — on the real pack
   `ENTER-DIR,,DI-75-1,0'` (21 bytes, odd) is followed by `0x00`, while `SET-AVAIL'` (10 bytes,
   even) is followed immediately by the next command with **no** NUL.
4. The buffer ends with **an extra lone `0x27`** — i.e. a zero-length command. Everything after is
   `0x00` fill.

Real bytes from `SMD0-L.IMG` at `0x2C90A6` (`<xx>` = non-printable):

```
ENTER-DIR,,DI-75-1,0'<00>ENTER-DIR,,DI-74-1,0'<00>SET-AVAIL''<00><00>...
                    ^pad                     ^pad          ^^ terminator
```

And after issuing `@NEXT-INITIAL-COMMAND CC HELLO` on the running system, the *same offset* read:

```
ENTER-DIR,,DI-75-1,0'<00>ENTER-DIR,,DI-74-1,0'<00>SET-AVAIL'CC HELLO'<00>'<00>...
                                                            ^no pad (even)  ^terminator
```

**VERIFIED** — SINTRAN itself wrote through to this exact byte range, independently confirming both
the location and the format.

### 4.1 Reference decoder (pseudocode)

**Bound the parse by the length cell, not by the terminator.** That is what SINTRAN itself does
(`NEXIN` appends starting at `X := [INIBU+130]`, §2.2). The trailing lone `'` is a convenient
marker but is not authoritative — stale bytes can follow it.

```
function readInitialCommands(seg, byteOffset):
    textLen = readWordBE(seg, byteOffset + 260)      # the length cell (§4.3)
    if textLen == 0 or textLen > 260: return null    # implausible -> wrong segment/version
    return parseInitialCommands(seg, byteOffset, textLen)

function parseInitialCommands(seg, off, textLen):
    cmds = []; i = off; end = off + textLen
    while i < end:
        if seg[i] == 0x00: i += 1; continue          # alignment padding
        if seg[i] == 0x27: break                     # empty command -> end of buffer
        s = ""
        while i < end and seg[i] != 0x27:
            s += chr(seg[i] & 0x7F); i += 1
        i += 1                                       # consume the "'"
        if i < end and seg[i] == 0x00 and ((i - off) is odd): i += 1
        if s is not all-printable: return null       # sanity: not a real buffer
        cmds.append(s)
    return cmds
```

A plausible length cell is also the cleanest way to pick the right segment in §3.3.

### 4.2 Reference encoder (pseudocode)

```
function encodeInitialCommands(cmds):
    out = []
    for c in cmds:
        assert c is 7-bit printable ASCII and "'" not in c
        out += bytes(c.upper())        # SINTRAN commands are upper case
        out += [0x27]
        if len(out) is odd: out += [0x00]      # word-align
    textLen = len(out)                         # EXCLUDES terminator -> the length cell (§4.3)
    assert textLen <= 254                      # the limit NEXIN enforces (§2.2)
    out += [0x27]                              # terminator (empty command)
    if len(out) is odd: out += [0x00]
    return out, textLen
```

Writing = zero the 260-byte text region, blit `out` at `byteOffset`, then store `textLen` in the
length cell. The region is fixed-size, so **nothing else in the image moves**.

### 4.3 The length cell at `INIBU + 130` words — MANDATORY for writers

The text alone is **not** authoritative. A single word at **`INIBU + 130` words**
(= `byteOffset + 260`) holds the **byte length of the command text, excluding the terminator**.
SINTRAN reads exactly that many bytes; anything after is invisible to it.

```
lengthCell = sum over commands of  roundUpToEven( len(command) + 1 )
             #                                      ^ the "'" terminator byte
```

**VERIFIED by A/B diff and by a booted round-trip:**

| Buffer contents | text bytes | length cell |
|---|---|---|
| `ENTER-DIR,,DI-75-1,0` · `ENTER-DIR,,DI-74-1,0` · `SET-AVAIL` | 22+22+10 = 54 | `0x0036` = 54 |
| … + `CC HELLO` (appended by the OS at runtime) | 54 + 10 = 64 | `0x0040` = 64 |
| … + `CC OFFLINE WRITE OK` (written offline by us) | 54 + 20 = 74 | `0x004A` = 74 |
| … + three 19-char `CC` commands (typed on the live system) | 54 + 3×20 = 114 | `0x0072` = 114 |

When SINTRAN appended `CC HELLO` itself, the **only** bytes it changed outside the text were this
one word — which is how the cell was found. It is the last word of the 131-word
`INIBU`…`INCOM` region: **130 words of text + 1 length word**, a tidy and self-consistent layout.

> **This is the trap.** Our first offline write patched the text correctly and left the length cell
> at 54. The image booted fine, but `@LIST-INITIAL-COMMANDS` showed only the original three
> commands — the fourth was on disk, byte-perfect, and simply never read. Updating the cell to 74
> made the same command appear. **A writer that forgets the length cell silently does nothing.**

### 4.4 Complete write procedure

```
1. locate byteOffset      (§3)
2. text, textLen = encodeInitialCommands(cmds)      # textLen EXCLUDES the terminator
3. assert textLen <= 254                            # code-derived limit (§2.2)
4. zero  SEGFIL0[byteOffset : byteOffset+260]       # the text region only
5. write text            at byteOffset
6. write textLen as a big-endian word at byteOffset + 260      # <-- INIBU+130 words
```

Step 6 is not optional. Steps 4-6 touch a fixed region; nothing else in the image moves.

---

## 5. Implementation notes per codebase

All four implementations share: page = 2048 bytes, LBA == NDFS page number, byte offset =
`blockId * 2048`. The buffer work happens **inside the `SEGFIL0:DATA` file contents**, so it uses
the ordinary `read_file` / `write_file` APIs, **not** raw-page primitives — no new addressing
scheme is needed.

Suggested API shape (mirror the existing `boot_loader` feature in each repo):

```
read_initial_commands(fs)            -> list[str]           (+ location info)
write_initial_commands(fs, cmds)     -> void                (in-place, bounded)
```

| Repo / lang | Where it belongs | Follow the pattern of |
|---|---|---|
| `ndfs-c` | new `include/ndfs/sintran.h` + `src/sintran.c`; expose `ndfs_read_initial_commands()` / `ndfs_write_initial_commands()` | `boot_loader.{h,c}` (`ndfs_load_boot_code`) |
| `ndfs-ts` | new `src/sintran.ts`, re-exported from `src/index.ts`; methods on `NdfsFileSystem` | `src/boot-loader.ts` (`loadBootCode`) |
| `ndfs-py` | new `src/ndfs/sintran.py` | `src/ndfs/boot_loader.py` |
| `RetroFS.NDFS` (C#) | new `Boot/NdfsInitialCommands.cs`; methods on `INdfsFileSystem` / `NdfsFileSystem` | `Boot/NdfsBootLoader.cs` (`LoadBootCode()`), and `GetPasswordClearInstructions()` for the "locate + patch a raw region" idiom |

Tests: follow the one-test-file-per-module convention in each repo (Unity `tests/test_sintran.c`;
vitest `tests/sintran.test.ts`; pytest `tests/test_sintran.py`; xUnit under `RetroFS/tests/`).
Golden expectations from real packs are in §6.

Docs: `norskdata-ndfs/docs/` — add a spec page following the `XAT-FIELD-EXTENSION-SPEC.md`
precedent, and link this document.

### 5.1 Working reference implementation

A complete, tested JavaScript implementation of §3 + §4 (locator, decoder, version table) lives in
the nd100x glass frontend and can be ported directly:

- `/home/ronny/repos/nd100x/template-glass/js/sintran-initial-commands.js`
  (`findSegmentTablePage`, `parseBuffer`, `readInitialCommands`)
- Tests: `/home/ronny/repos/nd100x/template-glass/js/tests/test_initial_commands.js`
  (19 checks, run: `cd template-glass && node js/tests/test_initial_commands.js`)

---

## 6. Golden test vectors (real packs, verified)

Both packs are SINTRAN III VSX/500 **L**; both resolve to segment-table page **1275**, command
segment **3**, `MADR` **1408**, byte offset **0x2C90A6** in `SEGFIL0:DATA`.

| Image | Version | Seg-table page | seg | `MADR` | `textLen` | Decoded initial commands |
|---|---|---|---|---|---|---|
| `~/repos/nd100x/SMD0-L.IMG` | L | 1275 | 3 | 1408 | 54 | `ENTER-DIR,,DI-75-1,0` · `ENTER-DIR,,DI-74-1,0` · `SET-AVAIL` |
| `~/repos/nd100x/SMD0-org.IMG` | L | 1275 | 3 | 1408 | 40 | `ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0` |
| `F:\ND\SINTRAN-M - 2026\HDD\BIGDISK0-M.IMG` | **M** | **1311** | 3 | **1444** | 40 | `ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0` |

Two L packs yielding **different** command lists from the same derived offset proves the algorithm
reads real per-pack data rather than matching a fixed pattern. The **M** pack — a different
SINTRAN version, a different segment-table page and a different `MADR` — is decoded correctly by
the *same code with no changes*, with the version auto-detected from which `INIBU` produces a sane
length cell. That is the portability claim, demonstrated. **VERIFIED.**

Live cross-check on M06: `@LIST-INITIAL-COMMANDS` printed exactly the offline-decoded command;
two `@NEXT-INITIAL-COMMAND CC ...` appends then produced `textLen` 54 (= 40 + 8 + 6), which the
offline reader read back correctly. **VERIFIED.**

Live cross-check (L07, booted under nd100x with the DAP debugger):

1. `@LIST-INITIAL-COMMANDS` printed the three `SMD0-L` commands above — identical to the offline
   decode.
2. `@NEXT-INITIAL-COMMAND CC HELLO` was issued; `@LIST-INITIAL-COMMANDS` then showed four commands.
3. Re-reading the **same derived byte offset** in the image showed `...SET-AVAIL'CC HELLO'<00>'`.

⇒ location, encoding, alignment rule and terminator all confirmed by the OS itself. **VERIFIED.**

### 6.1 Offline write → boot round-trip (the acceptance test) — PASSED

Run 2026-07-10 on a fresh copy of `SMD0-L.IMG` (nothing booted from it beforehand):

1. **Attempt 1 — text only.** Wrote the 4-command text at the derived offset, left the length cell
   at 54. Image booted cleanly to `SINTRAN III RUNNING`; `@LIST-INITIAL-COMMANDS` showed only the
   **original three**. The fourth command was on disk byte-perfect but never read.
   *This is how the length cell (§4.3) was discovered.*
2. **Attempt 2 — text + length cell.** Same write, plus `lengthCell = 74` at `byteOffset + 260`.
   Total change: **22 bytes** (21 text + 1 word), image size unchanged.
   Booted cleanly, and:

```
@LIST-INITIAL-COMMANDS,,

ENTER-DIR,,DI-75-1,0
ENTER-DIR,,DI-74-1,0
SET-AVAIL
CC OFFLINE WRITE OK          <-- injected offline, never typed on the system
```

⇒ **the write path is fully verified end-to-end.** Using a `CC` (comment) command as the payload
makes this test safe: it is listed but executes nothing, so a mistake cannot break the boot.

---

## 7. Caveats for writers

- **Back up the image first.** Booting a SINTRAN pack under an emulator already mutates it
  (segment swap + checkpoint writes); an offline edit on top of a mutated pack is hard to reason
  about. Keep a pristine copy.
- **The disk copy reflects the last checkpoint.** The live buffer lives in RAM in the command
  segment; SINTRAN writes it through to `SEGFIL0` (observed immediately in §6 step 3), but a
  running system may later overwrite an offline edit when it checkpoints. **Edit offline images
  only while nothing is booted from them.**
- **Never exceed the bound.** `textLen <= 254` bytes, the limit the `NEXIN` code itself enforces
  (§2.2). Writing past 260 bytes clobbers the length cell; past 262 it clobbers `INCOM`'s
  executable code. (Do **not** use the retracted `ACLEA`/88-byte bound from an earlier revision.)
- **First command must be `@ENTER-DIRECTORY`** per the SINTRAN Reference Manual (ND-60.128.5).
  A writer should validate this rather than silently produce an unbootable configuration.
- **Update the length cell (§4.3) or your write is a no-op.** This is the single most likely bug.
- **Test with a `CC ...` comment command.** It is listed by `@LIST-INITIAL-COMMANDS` but executes
  nothing, so a bad write cannot break the boot. Only once that round-trips should a writer be
  trusted with real commands.
- The offline write → boot → `@LIST-INITIAL-COMMANDS` round-trip has now been **run and passed**
  (§6.1); it remains the recommended acceptance test for any new implementation.

---

## 8. Sources

- Live SINTRAN III VSX/500 L under `nd100x --debugger --boot=smd`, console via DAP:
  `@LIST-INITIAL-COMMANDS`, `@NEXT-INITIAL-COMMAND CC HELLO` (2026-07-10)
- `SINTRAN/NPL-SOURCE/SYMBOLS/{K03,L07,M06}/SYMBOL-1-LIST.SYMB.TXT` — `INIBU`, `INCOM`, `NEXIN`,
  `ACLEA`, `CPTSL`
- Segment-table field layout: [22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md](22-READING-RT-AND-SEGMENT-TABLES-FROM-MEMORY.md) §3.3
- Page model (2048 bytes): `tools/sintran-segment-carver/EXTRACTING-SEGMENTS.md`
- Images: `~/repos/nd100x/SMD0-L.IMG`, `~/repos/nd100x/SMD0-org.IMG`
- Reference implementation + tests (31 checks incl. encoder round-trip):
  `nd100x/template-glass/js/sintran-initial-commands.js`,
  `nd100x/template-glass/js/tests/test_initial_commands.js`
- Offline write → boot round-trip on a copy of `SMD0-L.IMG` (2026-07-10)
- `NEXIN` / `INCOM` disassembly + literal reads at a live breakpoint on **L07 and M06** (§2.2), 2026-07-10
- M06 image: `F:\ND\SINTRAN-M - 2026\HDD\BIGDISK0-M.IMG` (SINTRAN III VSX/500 M, generated 1994-09-16)
- `SINTRAN/NPL-SOURCE/NPL/CC-P2-COMMON.NPL:862` — proves `ACLEAR` is a bitmask constant, not an
  address (the basis for retracting the old 88-byte bound, §2.1)
- `Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md` — §5340, §5356, §5797, §6840:
  the commands are described, **no maximum size is stated anywhere**

---

**Last updated:** 2026-07-10
**Status:** Read path VERIFIED (live + two packs). Write path VERIFIED end-to-end: offline write →
cold boot → `@LIST-INITIAL-COMMANDS` shows the injected command (§6.1).
