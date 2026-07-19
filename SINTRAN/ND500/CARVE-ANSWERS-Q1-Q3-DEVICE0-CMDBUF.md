# Carved-L07 Answers: Device-0 / Command Buffer / MON 1B (Q1-Q3)

**Scope**: MON 1B (INBT), logical device 0 (the command buffer), and the command-buffer
structure, answered FROM THE CARVED L-VSX-500 (L07) BINARY BYTES - the disassembled
segment images under
`tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/`
and the segment `.bin` files under
`tools/sintran-segment-carver/versions/L-VSX-500/segments/`.

**Method note**: octal is written `nnnB` as the carve source writes it. Byte offset into a
`.bin` = `(addr - loadbase) * 2` decimal, loadbase taken from each segment's first address.
Every conclusion is tagged VERIFIED (read directly from the carved bytes) / INFERRED /
UNKNOWN. "Unknown" is used wherever the deciding bytes are not present or not
distinguishable in this carve. All paths are repo-root-relative from `E:\Dev\Ronny\NDInsight`.

**One critical structural fact that governs all three answers (VERIFIED)**: the MON 1B
level-14 handler `M1` is a two-word activation stub that fires **program level 4** with the
worker routine `INBT=032471B`. `INBT=032471B` is a REAL worker CODE-ENTRY ADDRESS (byte-proven,
see the "032471B code-vs-data resolution" note at the end of Q1): the word at `071666B` in
`026-S3IMPIT` literally holds `032471B`, `M1` loads it, and `IOB14` writes it into level 4's
saved P register. The problem is only that the *executable bytes* at virtual `032471B` are not
cleanly recoverable from the static carve: the three resident images that each span that virtual
address disagree and none decodes as a coherent INBT byte-reader (003-S3CP = ASCII message text,
026-S3IMPIT = a pointer/data word, resident common code = an unrelated IOX poll loop). So the
`INBT` device-0 read worker body is NOT statically extractable in this carve, which is why Q1
and the device-classing half of Q3 come back "needs a live trace" rather than fully byte-proven.
The precise break address for that trace is given at the end of Q1.

---

## Q1 - Device-0 exhaustion semantics

**Answer: UNKNOWN from these bytes. The exhaustion-decision code was NOT found in cleanly
carved code.** I will not pick (a)/(b)/(c)/(d)/(e).

### What is VERIFIED from the bytes

1. **The command-string terminator is `47B` (ASCII apostrophe `'`), and the system stops a
   command-string scan ON that byte.** Carved resident loop, VERIFIED:
   `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/025-S3IRPIT/025-S3IRPIT.asm`
   lines 17555-17569 (`072210B`-`072226B`):
   ```
   072210  146151  RADD CLD SA DD      ; D := A   (D = byte pointer)
   072211  146107  RADD CLD 0 DX       ; X := 0   (byte index)
   072212  146116  RADD CLD SD DT      ; T := D
   072213  174000  BSET ZRO SSPTM      ; select alternate page-table for the byte fetch
   072214  142200  LBYT                ; A := byte at [pointer + X]
   072215  174200  BSET ONE SSPTM
   072216  171047  SAT 47              ; T := 47B  (apostrophe terminator)
   072217  142065  SKP IF DA UEQ ST    ; skip next if byte != 47B
   072220  124006  JMP 6    ; -> 072226   (byte == 47B  ->  EXIT: end of string)
   072221  051023  LDT I 23            ; else T := (an output device number)
   072222  153002  MON 2               ; OUTBT - emit the byte
   072223  124001  JMP 1    ; -> 072224
   072224  173401  AAX 1               ; X++  (advance)
   072225  124365  JMP -13  ; -> 072212  (loop)
   072226  146142  EXIT
   ```
   This is a resident routine that walks a byte pointer through the command string, emits
   each byte with MON 2 (OUTBT), and **terminates the moment it reads `47B`**. It proves the
   system's own end-of-command-string convention is "read up to, and stop at, `47B`". It does
   NOT prove what a *user* MON 1B read on device 0 returns once the pointer reaches that byte.

2. **The same LBYT / `SAT 47` (`171047B`) stop-on-apostrophe pattern recurs**, VERIFIED, in
   the same segment:
   - `025-S3IRPIT.asm` lines 4391-4395 (`041512B` LBYT, `041514B` `SAT 47`, `041515B`
     `SKP IF DA UEQ ST`) - a command-string byte scan.
   - `025-S3IRPIT.asm` lines 17627-17629 (`072320B` LBYT, `072321B` `SAT 47`) - a
     copy-until-apostrophe loop that SBYTs into a second buffer.
   All three loops stop on `47B` and none of them consult a byte-count word: **the string is
   delimited positionally by the `47B` terminator, not by a length field** (VERIFIED).

3. **The carved MCTAB[1B] worker `YFGET=026576B` is the OPENED-FILE byte primitive, not the
   device-0 command-buffer path** (VERIFIED). In
   `tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/006-S3FS.asm`
   lines 418-455 (`026576B`-`026641B`): `YFGET` loads a get selector (`050046B` = `LDT 46`),
   stores it into an open-file control block (`012030B` = `STT ,X 30`), calls a
   byte-transfer helper (`JPL I 44` -> `026646B`), clears a byte counter (`STZ ,B 27`), and
   on the refill/error fork sets file error codes `132B`/`133B` (`170532B` `SAA 132`,
   `170533B` `SAA 133`). Those are file errors, not EOF `3B` and not a command-buffer path.
   So YFGET does not answer the device-0 question.

### Why the decision cannot be pinned (VERIFIED negative)

The routine that actually decides device-0 read-after-terminator behaviour is `INBT`
(`032471B`), the level-4 worker that `M1` activates (see Q3). Its worker ADDRESS is
byte-proven (`071666B` = `032471B`; NPL `M1: "INBT"; GO IOB14`), but its executable BYTES at
virtual `032471B` are not statically recoverable. I checked EVERY resident image whose virtual
window spans `032471B` (not just one overlay), and they hold three mutually different,
non-INBT contents - the classic segment-address overlap:

| Image (repo-root path under `tools/sintran-segment-carver/versions/L-VSX-500/`) | Load base | Bytes at `032471B` | Decodes as |
|---|---|---|---|
| `re/segments-ref/003-S3CP/003-S3CP.asm` (line 1508) | `30000B` | `020104B` | ASCII text - inside the string `"$$NO TERMINATION DEFINED!$'"` (the `$'` at `032476B` is the `47B` terminator). DATA. |
| `re/segments-ref/006-S3FS/006-S3FS.asm` | `026000B` | mis-decoded | prompt/error-message string block (`"...name:  '..."`). DATA. |
| `re/segments-ref/026-S3IMPIT/026-S3IMPIT.asm` (line 395) | `32000B` | `010563B` | a POINTER word: `032424B JPL I 45` indirects THROUGH `032471B` to jump to `010563B`. DATA (jump-table slot). |
| `resident/SINTRAN-DATA_commoncode.dis` (line 13628) | `0` | `150415B` | `IOXT` - real code, but the body is a device-poll loop (`032403B..032473B`, `MIN ,B 10 / JMP -70`), the neighbouring byte-I/O symbols `5INB=032340B` / `OUTBT=032355B` land MID-loop, and there is no MON-return: it is an UNRELATED resident routine at the same octal address, not INBT. |

So the INBT worker code at virtual `032471B` is served by a level-4 PIT overlay that is not
one of the statically carved images (or is one of them under a page mapping this carve does
not reconstruct). `032471B` is BELOW the swappable-overlay window (`0o104000-0o170000`), so the
`EXTRACTING-RESIDENT-CODE.md` sec 7.6 density-disambiguation does NOT apply - this is a
fixed-resident address whose backing image differs by level/PIT, which static carving alone
cannot resolve here.

Neither did I find, anywhere in the carved worker overlays, a single-byte command-buffer
reader that on hitting `47B` returns EOF `3B` (`170403B`) or enters a wait, provably reached
from a CPNT/command-buffer terminator test. The partial NPL source
(`SINTRAN/NPL-SOURCE/NPL/`) also does NOT contain the INBT worker body - it holds the level-4
driver frame `BIOTR` ("INBT-OUTBT LEVEL:", `RP-P2-MONCALLS.NPL:3436`, `112444B`), the string
reader `3INSTR` (`112521B`, which calls `IOTR` per byte and branches `IF =D3 GO TERM` on the
terminator), and the `M1:"INBT";GO IOB14 / IOB14:*IRW BLEVB DP` activation
(`MP-P2-2.NPL:231-244`, `071455B`), but not the INBT body itself. I will not assert a
read-after-terminator branch I cannot read in the bytes.

**Q1 verdict label: NEEDS LIVE TRACE (option among a-e not determinable from static bytes).**
What IS VERIFIED statically is only: the terminator value `47B`, that the system stops scanning
ON it, that there is no byte-count field (positional exhaustion), and that INBT runs on level 4
at `032471B`.

### The decisive live trace (exact break address)

Do NOT guess (a)/(b)/(c)/(d)/(e). Run the native-emulator trace, break at the INBT level-4
entry with `CPNT` already parked on the `47B` terminator, and single-step:

- **Break address: `032471B` = `0x3539`, program level 4** (INBT worker entry; VERIFIED as the
  level-4 restart P value that `M1`/`IOB14` install).
- Cross-check anchor (live-verified by the team): `ENT14 = 072167B = 0x7477` (level-14 MON
  entry) - stepping from a MON 1B on device 0 should pass through `ENT14 -> GOTAB[1B]=M1
  (071633B) -> IOB14 (071660B) -> level 4 @ 032471B`.
- Recipe (native breakpoint, NOT DAP - nd100x DAP breakpoints are known to stall the CPU):
  `cd ~/repos/nd100x && ./build/bin/nd100x --breakpoint=0x3539 --ring-dump=200 --max-instr=N --boot=smd`
- Observe: whether INBT (i) returns EOF error `3B` (`SAA 3` = `170403B`), (ii) suspends/WAITs,
  (iii) re-sources from the terminal, (iv) returns the `47B` terminator (and then what the NEXT
  read returns), or (v) other; and whether it TESTS `CPNT` (`144033B`) against a max/terminator
  before the read (Q1c: exhausted-vs-empty).

### Q1a/b/c - explicit status

- **Q1a (what a post-terminator device-0 read does: EOF `3B` / suspend / terminal / return-
  terminator-then-X / other):** NEEDS LIVE TRACE. Not determinable from static bytes (INBT body
  not statically extractable). Break `0x3539` level 4, above.
- **Q1b (device 0: special-cased `SKP/JMP` compare, or ordinary datafield/table entry):**
  INFERRED = datafield/table, NOT a device-0 compare, but UNCONFIRMED for INBT. The carved and
  NPL device-input paths resolve a device through a DATAFIELD (`IP-P2-1.NPL:185`
  `A:=XUNIT(CDRG)=:XNOWUNIT % GET INBT/OUTBT DATAFIELD`; the driver frame `BIOTR` calls
  `IOTRANS`/`STDEV` on a datafield, never a literal `= 0` test). This suggests device 0 is
  handled as an ordinary table/datafield entry, not a hard-coded compare - which, if true,
  means the emulator should NOT special-case devno 0 in INBT. But I found no INBT compare
  against `0` in any carved image, so this stays INFERRED; the live trace must confirm whether
  INBT reaches the command buffer via a datafield or via an explicit devno-0 branch.
- **Q1c (does INBT distinguish "buffer EXHAUSTED / pointer at terminator" from "buffer EMPTY at
  entry"):** UNKNOWN. This is exactly the load-bearing distinction and it lives inside the
  missing INBT body (the `CPNT`=`144033B` test). The live trace with `CPNT` parked on `47B` is
  the only way to settle it; capture the `CPNT`/max-address compare that precedes the byte read.

### 032471B code-vs-data resolution (was the "CORRECTED" note over-corrected?)

**Yes - the `1B-InByte/README.md` "CORRECTED 2026-07-13" note OVER-corrected.** From the bytes:

- **PROVEN, and the "CORRECTED" note got this right:** `GOTAB[1B] = M1 = 071633B` (a resident
  level-14 fast handler, one of the 32 non-`MFELL` slots), and the old
  `GOTAB[1B]=120303B -> F1607` reading was a wrong-table artefact (read out of the fake GOTAB in
  `SINTRAN-DATA_commoncode`).
- **OVER-CORRECTED:** the note also withdrew "`INBT = 032471B`" as a pure wrong-overlay data
  artefact. That is wrong. `INBT = 032471B` is a GENUINE, byte-proven worker CODE-ENTRY
  ADDRESS: `026-S3IMPIT.asm` line 17338 shows the literal pool word `071666 032471`, and `M1`
  (`071633B`) loads exactly that word (`LDA 33`, P-relative to `071666B`) and hands it to
  `IOB14` (`071660B`), which installs it as the level-4 restart P (`IRW 40 DP`). The NPL source
  confirms the mechanism verbatim: `M1: "INBT"; GO IOB14` under the comment `% ACTIVATE LEVEL 4
  FOR MONITOR CALLS` (`MP-P2-2.NPL:229-244`). So `032471B` is a real level-4 code entry for the
  M1 fast path; it is a DIFFERENT worker from `MCTAB[1B]=YFGET=026576B` (the opened-file
  primitive). Both exist: M1/INBT is the level-4 fast path (terminal + command buffer), YFGET is
  the file path.
- **What the note observed that is still true:** at the linear offset for `032471B` in the
  `006-S3FS` / `003-S3CP` segment images the bytes ARE ASCII string data - because those SEGFIL0
  images carry error/prompt strings in that virtual range, NOT because `032471B` is "only data".
  It is data IN THOSE overlays and code in the level-4 map. Segment addresses overlap; the same
  octal address is different content per segment/level (see the four-image table above).

**Net:** `INBT=032471B` should be RESTORED as the real MON 1B fast-path (level-4) worker
address in the MON-call docs; only the executable-bytes extraction remains open (needs the
live trace / the correct level-4 PIT overlay), not the identity of the worker.

---

## Q2 - Command-buffer structure

### The real command-string machinery (VERIFIED addresses, from the L07 data map)

`tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/025-S3IRPIT/025-S3IRPIT.symbols.txt`
lines 1288-1298, consecutive one-word cells on the background field (base
`BGSYS=144000B`, from `003-S3CP.symbols.txt` line 1370 `144000B V0/FV0/BGSYS`):

| Addr (L07) | Symbol | Role |
|---|---|---|
| 144030B | INDEX | (background index) |
| 144031B | TTNO  | terminal number |
| 144032B | TTIFI | terminal input-datafield address |
| **144033B** | **CPNT** | **command-buffer byte pointer** |
| 144034B | OPNT  | output byte pointer |
| **144035B** | **CSTRI** | **command string (pointer)** |
| 144036B | OSTRI | output string (pointer) |
| 144037B | PASST | (password state) |

**These are the real command-buffer fields** (VERIFIED addresses). `CPNT` is the read/byte
pointer; `CSTRI` is the command string.

- **`CSTRIN` (144035B) is a one-word POINTER, not an inline multi-word text buffer**
  (VERIFIED by layout): it sits exactly one word before `OSTRI` (144036B), so it cannot
  itself hold a 16-word / 32-character inline string. It holds the address (byte pointer) of
  the command text, which is then walked with `LBYT` through a pointer in a register - exactly
  as the carved loop at `072210B`-`072226B` (Q1 item 1) does.
- **`CPNT` (144033B) is a one-word byte pointer / read cursor.** CPNT = 0 meaning
  "start of buffer" is stated by the prior NPL answer (`RP-P2-MONCALLS.NPL` `0=:PCPNT`); I
  could not re-derive the "0 = start" convention from the carved bytes, so that specific
  value is INFERRED (carried from NPL), not VERIFIED here.
- **Terminator = `47B`** (apostrophe): VERIFIED (Q1 items 1-2).
- **No byte-count field**: VERIFIED negative - every carved command-string loop is bounded by
  the `47B` terminator, none by a length word. "Exhausted" is represented positionally (the
  byte pointer reaching the `47B` terminator).

### What `170207B` actually is (RESOLVING the disputed label)

`170207B` in `025-S3IRPIT` is a single DATA word, `000000B` in this L image
(`re/mon-analysis/12B-SetCommandBuffer/README.md` lines 62, 73-75, byte offset 96526 =
`00 00`). **It is NOT the command-string storage** - that is `CPNT`/`CSTRI` at
144033B/144035B, on a different segment (the BGSYS field), as shown above.

- VERIFIED: `170207B` is a lone zero word; the real command string is reached through
  `CSTRI`/`CPNT`, not through `170207B`.
- UNKNOWN/INFERRED: whether `170207B` is specifically the NPL `INTEGER CBUF % ADDRESS OF
  CURRENT I/O BUFFER` scratch pointer cannot be distinguished from the bytes alone (a single
  `000000B` word is consistent with any zero-initialised scalar). The prior NPL answer's
  reading (a startup I/O-buffer address, not a command buffer) is the more likely one, but
  from the carved bytes it stays INFERRED. Either way, **the "command-buffer DATA area" label
  on `170207B` is not supported by the L07 bytes and should be retired.**

### Still UNKNOWN from the bytes

- Total command-buffer size in words/bytes (the manual's "32 characters" is not confirmable).
- Whether the program NAME is stored together with the parameters in `CSTRIN` (the NPL PLREE
  path copies a name into CSTRIN, but that source is not in this carve; from carved bytes:
  UNKNOWN).

---

## Q3 - MON 1B device dispatch (the M1 fast handler)

### M1 IS a two-word activation stub, with NO device-number branch (VERIFIED)

`GOTAB[1B] = 071633B = M1` (VERIFIED: `re/mon-analysis/1B-InByte/README.md` dd proof, byte
offset 32056 in `026-S3IMPIT.bin` = `73 9b` = `071633B`).

Carved M1 body,
`tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/026-S3IMPIT/026-S3IMPIT.asm`
lines 17311-17312 and the shared activation at 17332-17337:
```
071633  044033  LDA 33          ; A := word at 071633+33 = 071666
071634  124024  JMP 24          ; -> 071660  (IOB14 shared activation)
...
071660  153442  IRW 40 DP       ; park A (routine addr) as the activated level's routine
071661  170401  SAA 1
071662  153440  IRW 40 DS       ; set status bit 0 on that level
071663  170420  SAA 20
071664  150306  MST PID         ; fire the level
071665  125014  JMP I 14        ; -> 071701  (return to level-14 exit)
```
The constant M1 loads is at `071666B`, and it is `032471B` = `INBT` (VERIFIED:
`026-S3IMPIT.asm` line 17338 shows the word `071666 032471`; `032471B` = `INBT` per
SYMBOL-1-LIST). So:

```
M1:  A := INBT (=032471B)  ;  GO IOB14   ->  activate a lower level with routine = INBT
```

**This CONFIRMS the prior NPL note exactly**: `M1: "INBT"; GO IOB14`, a two-word stub, and
**M1 does NO device-number branching whatsoever** (VERIFIED). The `LDA 33` + `JMP 24`
pattern is one entry of a shared jump table (`071633B`-`071657B`) where each handler loads a
different worker-address constant from the pool at `071666B`+ and jumps to the common IOB14
activation at `071660B`. (The activated level is **level 4**: VERIFIED
against the NPL source, `MP-P2-2.NPL:227-244`, whose comment over this exact stub table reads
`% ACTIVATE LEVEL 4 FOR MONITOR CALLS THAT WILL BE HANDLED ON LEVEL 4.` and whose `IOB14`
installs the worker address into the level-4 save register via `*IRW BLEVB DP`. The carved
`IRW 40 DP` / `MST PID` bytes are the compiled form of that source.)

### How the device number is classed: UNKNOWN from carved bytes

All device-number classification happens inside `INBT` (`032471B`), the routine M1 hands to.
**That body is not statically extractable** (Q1: all three resident images spanning `032471B`
hold different non-INBT content - string data in 003-S3CP/006-S3FS, a pointer word in
026-S3IMPIT, an unrelated poll loop in the resident common code; the real level-4 INBT overlay
is not among the carved images). Therefore:

- Whether **device 0 is SPECIAL-CASED by a compare** vs. resolved as an ordinary
  device/datafield table entry: **UNKNOWN from the carve.**
- The prior note's classes (device 1 = own terminal special-cased; `100B`-`177B` = file
  numbers; else resolved via `LOGPH` into datafields) come from OTHER resident routines
  (`5GTDF`, `GDEVTY`) in the NPL tree, not from the INBT bytes. From this carve they remain
  **INFERRED**, not confirmed. I found no carved INBT compare against `0` to confirm or refute
  a device-0 special case.

What IS additionally VERIFIED from the carve: the MON-call dispatch head model - `MON 1B ->
ENT14 (072167B) -> GOTAB[1B]=M1 (071633B)`, and separately `MCTAB[1B]=YFGET (026576B)` (the
opened-file worker, `re/mon-analysis/1B-InByte/README.md`; MCTAB slot dd-proven at
`044-S3IDPIT.bin` offset 1826 = `2d 7e` = `026576B`). The runtime hand-off between the M1/INBT
fast path and the YFGET file worker is NOT byte-provable here (INBT missing) - INFERRED.

---

## Verdicts

### (a) IS Q1 CARVEABLE FROM THESE BYTES? **PARTLY.**

- VERIFIED from the carve: the command-string terminator is `47B` (apostrophe); the system
  stops scanning ON that byte; command strings are delimited positionally with no byte-count
  word; `CPNT`/`CSTRI` (144033B/144035B) are the real read-pointer/string fields; `CSTRIN` is
  a one-word pointer; `170207B` is a zero data word and NOT the command string.
- NOT carveable here: the exact read-after-terminator behaviour for a user MON 1B on device 0
  (EOF `3B` vs suspend vs terminal-passthrough vs return-terminator). The deciding routine is
  **`INBT = 032471B`** (a byte-proven level-4 worker ADDRESS), whose executable BODY is not
  statically extractable: all three resident images spanning virtual `032471B` disagree and
  none decodes as a coherent INBT byte-reader (003-S3CP/006-S3FS = string data, 026-S3IMPIT = a
  pointer word, resident common code = an unrelated IOX poll loop - see the four-image table in
  Q1). The real level-4 INBT overlay is not among the carved images and `032471B` is below the
  swappable-window so sec-7.6 density disambiguation does not apply. **To answer Q1 definitively:
  live trace, break `032471B` = `0x3539` on level 4, with CPNT parked on the `47B` terminator;
  single-step INBT and record whether it returns `3B`, blocks, re-sources from the terminal, or
  returns the terminator - and whether it tests CPNT (`144033B`) for exhausted-vs-empty.**

### (b) Contradictions with the prior NPL answer / the carve's own notes

1. **CONFIRMED against NPL**: `M1 = "INBT"; GO IOB14`, a two-word activation stub with no
   device branch - now VERIFIED from the L07 bytes at `071633B`/`071666B`/`071660B`
   (previously only INFERRED from the s3vs-4 generation). Prior NPL "M1 is a 2-word stub" is
   UPGRADED to VERIFIED for L07.
2. **CONFIRMED against NPL**: terminator `47B`, no byte-count field, positional exhaustion -
   VERIFIED here directly from carved `LBYT`/`SAT 47` loops (previously NPL-only).
3. **CONFIRMED against NPL, CONTRADICTS the carve's own 12B/1B folder label**: `170207B` is
   NOT the command-buffer data area. The `re/mon-analysis/12B-SetCommandBuffer/README.md`
   "CBUF command-buffer data area" label is unsupported by the bytes; the real command string
   is `CSTRI`/`CPNT` at 144035B/144033B. This matches the prior NPL finding.
4. **Q3 device classing stays INFERRED, matching NPL's own "UNKNOWN for device 0"**: the
   carve does not contain the INBT body, so it can neither confirm nor refute the
   dev1-special / 100B-177B-files / else-LOGPH model. No contradiction, but no upgrade either.
5. No contradiction found with any manual quote, except that the manual's "32 characters"
   command-buffer size remains unconfirmable from the bytes.
