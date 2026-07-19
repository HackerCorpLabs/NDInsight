# MON 050B OPEN (OpenFile) -- reverse-engineering analysis

Opens a mass-storage file (or peripheral) by name string plus a default file
type, validates the caller's access code, allocates an open-file-table slot, and
returns a file number in the A register (or a file-system error code on failure).
Manual name: **OpenFile (50B)**; code entry symbol: **OPENF**.

All addresses and values in this document are **octal** unless a decimal
equivalent is spelled out.

---

## Dispatch (byte-verified)

`prove-mon.py 050` output, verbatim:

```
GOTAB base = 071233 octal in tools/sintran-segment-carver/versions/L-VSX-500/resident/SINTRAN-DATA_commoncode.bin
Overlay    = tools/sintran-segment-carver/versions/L-VSX-500/segments/025-S3IRPIT.bin (load 32000 octal)
Symbols    = SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT

MON 50B:
  GOTAB[50] : file byte 0xe586 of commoncode.bin, raw = 00 00  -> 000000 octal
  = 000000 -> FALL-THROUGH (no direct handler; dispatched via MFELL/CALLPROC)
```

**Interpretation.** `GOTAB[50B] = 000000`. This is **not** a direct-dispatch
monitor call: the level-14 monitor entry finds a zero in the go-table slot and
therefore **falls through** to the second-level dispatch (`MFELL` ->
`CALLPROC`), which routes the call into the file-system monitor process. The
real work is done by the file-system routine **OPENF**, which lives in a
*different* carved segment from the resident go-table.

**Handler location (file + offset window).**
- File: `006-S3FS.bin` (the FILSYS segment, big-endian, load base `26000B`).
- Entry symbol: `OPENF = 123525B` -- confirmed at line 3229 of
  `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT`.
- Next symbol: `CONNF = 123640B` (line 2501) -> handler length =
  `123640 - 123525 = 113B = 75` words = 150 bytes.
- Byte offset inside `006-S3FS.bin`: `123525 - 26000 = 75525B = 63146` decimal
  bytes.
- The 75-word slice was copied verbatim into `050B-OPEN.bin`. First word on disk
  (big-endian) is `0x22 0x3b = 021073B`, which is exactly the first disassembled
  instruction `STD I 73` -- so the slice is aligned on the real entry, not on
  data or padding.

Because the dispatch is a fall-through, `prove-mon.py` prints no entry symbol or
entry bytes for the resident go-table slot (there is nothing there but the zero
word). The entry bytes quoted above (`00 00`) are the **go-table slot**; the
handler's own entry bytes come from `050B-OPEN.bin` / `050B-OPEN.ASM`.

---

## Instruction walkthrough

Disassembly of the 75-word window (see [`050B-OPEN.ASM`](050B-OPEN.ASM)). The
last 16 words (`123620B..123637B`) are the routine's **literal pool** -- address
constants used by the `JPL I` / `JMP I` indirect calls -- and are not executable
code. The pool words are:

| pool word | value  | meaning |
|-----------|--------|---------|
| 123621    | 003752 | resident param-entry (monitor-call frame setup) |
| 123625    | 044777 | `CLPAR` (parse one string parameter) |
| 123631    | 010500 | resident helper |
| 123633    | 067432 | `FOPEN` (open-file-table allocation worker) |
| 123634    | 010506 | resident helper |
| 123635    | 040730 | resident helper |
| 123636    | 040336 | resident helper |
| 123637    | 003776 | resident return / MEXIT |

### Entry prologue (`123525B..123531B`)
```
123525  STD I 73            ; save/spill via indirect slot
123526  RADD ... (reg ops)  ; register housekeeping
123530  SAB 110             ; set up B-relative base
123531  JPL I 70   -> 123621 (003752)   ; call resident param-entry: build MON frame
```

### Parameter parsing (`123532B..123542B`)
```
123535  LDT 67
123536  JPL I 67   -> 123625 (044777)   ; CLPAR: parse string parameter #1 (file name)
123537  JMP 53     -> 123612            ; parse failure -> error exit
123540  LDA 66
123541  JPL I 64   -> 123625 (044777)   ; CLPAR: parse string parameter #2 (default type)
123542  JMP 50     -> 123612            ; parse failure -> error exit
```
`CLPAR` is called twice, once for the file name and once for the default file
type. Either failing branches straight to the error exit at `123612B`.

### Access-code validation loop (`123543B..123556B`)
```
123545  SAT 11                          ; T = 11 (9-entry table, octal count)
123546  SKP IF DT GRE SX                ; loop guard: index vs table size
123547  JMP 6      -> 123555            ; exhausted -> "no such access code"
123550  LDA I ,X 57                     ; load table entry [X]
123551  SKP IF DA UEQ SD                ; compare against caller's access code
123552  JMP 5      -> 123557            ; match -> continue to open
123553  AAX 1                           ; next table slot
123554  JMP -7     -> 123545            ; loop
123555  SAA 104                         ; error code 104B "No such access code"
123556  JMP 34     -> 123612            ; -> error exit
```
The caller-supplied access code is checked against a 9-entry table. No match ->
`SAA 104` (error **104B**, "No such access code") -> error exit.

### Open the file (`123557B..123573B`)
```
123557  LDA I 51
123560  JPL I 51   -> 123631 (010500)   ; resident helper
123562  LDA 50
123564  RADD SB DX
123565  JPL I 46   -> 123633 (067432)   ; FOPEN: directory search + slot allocation
123566  JMP 26     -> 123614            ; FOPEN error -> error path
123570  LDA I 40
123571  JPL I 43   -> 123634 (010506)   ; resident helper
123573  JPL I 42   -> 123635 (040730)   ; resident helper
```
`FOPEN` (`067432B`) is the shared worker that locates the named file in the
directory and allocates an open-file-table slot. On error it returns via the
`JMP 26 -> 123614B` path. `FOPEN` is the routine that yields file-system errors
such as 056B "No such file name", 105B "File already opened", 107B/122B "too
many open files".

### Success finalisation + normal exit (`123574B..123611B`)
```
123574..123607  ... book-keeping (store file number, update tables) ...
123610  SAA -110                        ; set up return status
123611  JMP I 26   -> 123637 (003776)   ; indirect return via resident MEXIT
```
The tail stores the allocated file number and returns through the resident
return/exit vector `003776B`.

### Error exits (`123612B..123617B`)
```
123612  STA ,B 2                        ; store error code into caller return slot
123613  JMP -3     -> 123610            ; join the exit path
123614  STA ,B 2                        ; FOPEN-error variant: store error code
123615  LDA I 13
123616  JPL I 16   -> 123634 (010506)   ; resident helper (cleanup)
123617  JMP -7     -> 123610            ; join the exit path
```
Both error exits deposit the error code in the caller's return slot (`STA ,B 2`)
and rejoin the common exit at `123610B`, so failures and successes leave through
the same resident return vector -- with a non-zero error code in the return slot
on failure.

**Control-flow closure.** Every direct `JMP`/`JPL` target (`123612`, `123555`,
`123557`, `123545`, `123610`, `123614`, and the pool words reached by `JPL I`)
lands inside the `123525B..123637B` window; the only escapes are the indirect
`JPL I` / `JMP I` calls to the resident/worker literal-pool addresses, which is
expected for a monitor handler. The validator confirms this (see Integrity).

---

## Parameter / register contract

Manual calling sequence (ND-860228 SINTRAN III Monitor Calls, OpenFile 50B,
section 1.7): `SAT <access>; LDX (<name-ptr>; LDA (<type>; MON 50`.

| Item | Contract | Status |
|------|----------|--------|
| Param 1 -- File number | INT, returned in A on success | UNVERIFIED from bytes (manual); the tail stores a file number, consistent |
| Param 2 -- Access code | INT, in T at call (`SAT`) | VERIFIED: validated against the 9-entry table at `123543B..123556B` |
| Param 3 -- File name | STR pointer (in X) | VERIFIED: parsed by first `CLPAR` at `123536B` |
| Param 4 -- Default file type | STR | VERIFIED: parsed by second `CLPAR` at `123541B` |
| Output (success) | file number in A | UNVERIFIED which register carries it out; manual says A |
| Output (failure) | file-system error code in return slot | VERIFIED: `STA ,B 2` at both error exits |
| Error 104B | "No such access code" | VERIFIED: `SAA 104` at `123555B` |
| Error 056B / 105B / 107B / 122B | from `FOPEN` (not in this window) | UNVERIFIED here (FOPEN body is elsewhere in `006-S3FS.bin`) |
| Skip return | success via resident MEXIT `003776B` | UNVERIFIED from these bytes (return goes through indirect resident vector) |

**Empty / all-zero name.** OPENF has **no** empty-name fallback branch. A NUL
name terminates the directory search immediately and matches nothing, so it
surfaces as an `FOPEN` error (most consistently 056B "No such file name") or a
`CLPAR` parameter error. Scratch/default opens are *separate* sibling calls
(ScratchOpen 235B -> `OPENS`, DirectOpen 220B -> `DOPEN`), each with its own
routine that also calls `FOPEN`. VERIFIED: `OPENF` contains no default-name
substitution.

---

## Cross-reference

- Manual: **ND-860228 SINTRAN III Monitor Calls**, OpenFile (50B).
- Symbols: `SINTRAN/NPL-SOURCE/SYMBOLS/L07/FILSYS-SYMBOLS.SYMB.TXT`
  (`OPENF=123525`, `CONNF=123640`, `FOPEN=067432`, `CLPAR=044777`,
  `DOPEN=103026`, `OPENS=126176`) -- these FILSYS addresses **match** the carved
  `006-S3FS.bin`.
- The NPL **source** in this repo is a **different revision** of SINTRAN III; its
  resident addresses carry a uniform offset and do **not** equal the L binary.
  Resident targets reached from the literal pool (`003752B`, `010500B`,
  `010506B`, `040730B`, `040336B`, `003776B`) are therefore given as **raw
  addresses**, not named from NPL. Do not treat any NPL address as L byte truth.

---

## Integrity

Validator result line, verbatim:

```
ok    050B-OPEN.ASM  [123525..123637, 75w]

1/1 checked passed; 0 FAILED; 0 skipped (ND-500)
```

The window is control-flow closed: 0 direct branches escape the file.

---

## Confidence and open questions

- **HIGH**: the carve is byte-aligned on the real `OPENF` entry (first disk word
  `021073B` == first instruction), length is exactly `123525B..123640B`, and the
  file-system symbol table matches the binary.
- **HIGH**: dispatch is a GOTAB fall-through (`GOTAB[50B]=000000`) routed via
  `MFELL`/`CALLPROC`; the handler body is in `006-S3FS.bin`, not the resident
  go-table.
- **HIGH**: access-code validation and the 104B error path are byte-verified;
  `CLPAR` is called twice; `FOPEN` is the open worker.
- **MEDIUM**: the exact register that carries the returned file number out, and
  the precise `FOPEN` error code for an empty name (056B is the best-supported
  guess -- the `FOPEN` body is outside this window and was not disassembled here).
- **Open question**: the several resident helper calls (`010500B`, `010506B`,
  `040730B`, `040336B`) are named only by raw address; their bodies are in the
  resident image, which for this L build is not carved with matching symbols.
  Their exact roles are inferred, not byte-proven.
- **Anomaly**: none. The folder's claim (MON 050B == OPEN == `OPENF`) is
  consistent with every ground-truth artifact checked (bytes, symbols,
  validator, prove-mon).
