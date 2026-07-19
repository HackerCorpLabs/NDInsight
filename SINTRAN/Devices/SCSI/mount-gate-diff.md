# SCSI mount-gate diff: why SINTRAN III K mounts an SMD disk but not a SCSI disk

**Question answered:** find the exact device-kind gate that lets `@ENTER-DIRECTORY` mount an
SMD disk (`DISC-75-1`) but reject a SCSI disk (`DISC-SCSI-1`), by diffing two live execution
traces of SINTRAN III K.

**Traces**
- FAIL (SCSI): `/mnt/c/Users/ronny/AppData/Local/trace/file-trace-3.txt` (mount ends `@` ~line 326768)
- OK (SMD): `/mnt/c/Users/ronny/AppData/Local/trace/file-trace-smd-OK.txt` (mount returns `@` at line 1241832)

---

## DEVICE TYPE/FLAGS WORD — the steering value (K03, verified)

- SCSI device datafield word-0 `mem[164414] = 103325` (`0x86D5`); working SMD = `012314` (`0x14CC`).
  Differing bits (SCSI/SMD): 15(1/0), 12(0/1), 9(1/0), 4(1/0), 3(0/1), 0(1/0).
- `103325` is READ from memory at `126276` (never in a register before) => a pre-existing value
  written at **boot/sysgen**, not computed at mount and not from any SCSI response. The gate (line
  266388) runs ~13k instructions BEFORE the first SCSI CDB (INQUIRY, 279508), confirming it is
  **static device config, not disk content**.
- Path: `ENDIR -> COLDE -> load datafield[0]=103325 -> route SCSI to general-connect -> decode LDN
  2210 -> device found (031600) -> async connect -> read geometry (last block) -> return WITHOUT
  queueing block-0 (directory) read`.
- Fix lever: SINTRAN builds this type/flags word at boot from the controller's **hardware ident**
  (cf. SMD `BIGDI,1100,17` = ident 17B). If the emulated SCSI controller presents an ident/geometry
  that makes SINTRAN write `103325` (flags that send COLDE down the "connect, don't read directory"
  branch) rather than a mountable-directory value, that is the emulator-side cause — consistent with
  "fails on K/L/M" (same config-build logic, off input). The single bit COLDE tests to gate the
  block-0 read is NOT yet isolated (async connect-completion path).

---

## CORRECTED STATE (K03, verified — READ THIS; earlier gate/"never-dispatched" claims are void)

Verified facts on the correct version (K-VSX-500 binary, K03 symbols):
- SCSI `@ENTER-DIRECTORY` **does** run `ENDIR (127774)` and `COLDE (125270, cold-enter-directory)`.
- `COLDE` **correctly decodes** the SCSI device: subroutine at `011577` decodes LDN `002210`
  (= SCSI HD-1) into device-group `22` + unit, then `011634 LDX ,X I *11` device-table lookup ->
  SCSI device datafield `031600` (stored `126320 STA 31,B`). Device is recognized, not rejected.
- `COLDE` sets up datafield `031600` (`126327` stores `103325` = device flags/params), hands off
  (`130015`), the process `EXIT`s to the scheduler (`026000 MST PID`) and **waits** — the connect
  is ASYNC.
- On completion SINTRAN reads the disk's **last block (129311 = geometry/area-map, checksum
  valid)** but issues **NO block-0 (directory-master) read** — only 1 SCSI data CDB total. Block 0
  (PACK-ONE master) is valid but never read.
- `126305` is NOT the reject: it routes SMD (reference/default device `1100`) to a fast path and
  SCSI (new device `2210`) to the general device-connect path. Both legitimate.

**Failure, as far as verified:** SINTRAN completes the SCSI connect + geometry-identify but never
transitions to reading the directory master (block 0). NOT a rejected device, NOT bad disk content,
NOT the `126305` branch. The exact instruction that should queue the block-0 read (and doesn't) is
in the async connect-completion path and is NOT yet pinned. Concrete lead: the SCSI device datafield
at `031600` and the flags word `103325` set during connect (where a "connect-ready / is-mountable-
directory" state would live).

---

## VERSION CORRECTION (READ FIRST — supersedes all routine names in this file)

The running traces are **SINTRAN VSX/500 K** = carve `K-VSX-500` (byte-exact at the gate). The
correct symbol set is **K03** (proven: K03 `ENDIR=127774`/`RCBLO=032433` land on `STD I` routine
entries in the K binary; L07 addresses do not). **All L07 names previously used here — `CHIND`,
`COPFI`, `SG110`, `ENDIR=140176`, `CHDSI`, `RXDIR` — are WRONG VERSION and void.**

Correct K03 facts:
- The gate at `126305` is inside routine **`COLDE` (K03 `125270`)** — cold-enter-directory.
- `ENDIR = 127774` (K03). It **runs once in BOTH** traces (SCSI line 184732, SMD 166467), and
  `RCBLO=032433` (read disc block) runs 13-15x in both — so the earlier "mount worker never runs /
  identify-only" conclusion was an L07-address artifact and is **RETRACTED**.
- Gate `126305`: `mem[B+25]` (device LDN: SMD `001100` / SCSI `002210`) vs `T=001100`. SMD matches
  -> mount arm; SCSI mismatches -> other arm. `T=001100` is `1100` in BOTH runs => not read from the
  directory being entered; it is a system/reference value (the SMD/main-directory device). Its exact
  identity is still open.

---

## VERIFICATION (re-checked directly in both live traces — this supersedes routine-name claims below)

- Gate = running PC **`126305 SKP IF DA EQL ST`**, executes exactly once per mount.
- **SMD:** `A=001100`, `T=001100` → equal → skip → `126307` → **MOUNT** (reads directory).
- **SCSI:** `A=002210`, `T=001100` → not equal → `126306 JMP` → `126312` → **REJECT** (no directory read).
- Gate at trace line **266388**, BEFORE the first SCSI CDB (INQUIRY at 279508) → the value is
  **device geometry/config-derived, not disk content**.
- `T=001100` = commoncode symbol `SG110` (`1100B`, VERIFIED).
- `A` computed from device-object fields (`mem[B+20]/+24/+25`, B=132473) + a device-parameter
  table lookup; differing input = SCSI descriptor word `103325` vs SMD `012314`.

**CAVEAT (overlay mismatch):** carved `025-S3IRPIT@126303` = `LDA ,B 22`, but the RUNNING code at
`126303` is `SAX 0` — so the carved label **`CHIND` does NOT match the running code**. Gate facts
above are from the running traces (ground truth); the routine name and exact field meaning from the
carve are **INFERRED/uncertain**. Still-open: origin of the SCSI descriptor word `103325` (the exact
emulated geometry parameter), which pins the one-line fix.

---

## THE GATE (VERIFIED)

**PC = `126305` (octal), inside routine `CHIND` (= `126303`, L07 `SYMBOL-2-LIST`); gate is `CHIND+2`.**

The branch executes **exactly once per mount** in each trace. It is a one-shot decision, not a loop:

| trace | gate arm taken | count of skip-arm (`->126307`) | count of no-skip arm (`->126306`) |
|-------|----------------|--------------------------------|-----------------------------------|
| OK (SMD)  | **skip -> 126307 = MOUNT**  | 1 | 0 |
| FAIL (SCSI) | **no-skip -> 126306 = REJECT** | 0 | 1 |

Both run at the same context (Program level 1, PID 06). All SCSI bus I/O in the FAIL trace
(INQUIRY / READ CAPACITY / READ_6) happens *after* this gate (line 278719+ vs gate at 266388),
so the tested value is **not** disk content — it comes from the device descriptor built from
static device configuration.

### Carved / traced disassembly (identical opcodes in BOTH traces — not overlay-confounded)

```
126277  174200   BSET ONE SSPTM
126300  131047   JAZ 47
126301  004430   STA 30,B          ; mem[B+30] := A   (directory address just computed)
126302  044425   LDA 25,B          ; A := mem[B+25]   <-- the tested device-descriptor word
126303  171400   SAX 0             ; X := 0
126304  053067   LDT ,X I *67      ; T := 001100  (constant, = equate SG110; same in both traces)
126305  140065   SKP IF DA EQL ST  ; skip next instr IF A == T          <==== THE GATE
126306  124004   JMP *4            ; -> 126312 = COPFI (FILSYS)   [taken when A != T : REJECT]
126307  044402   LDA 2,B           ; ...continue -> IOF / IRW directory I/O [taken when A==T : MOUNT]
```

- `SKP IF DA EQL ST` = *skip the next instruction if A equals T* (opcode 140065). VERIFIED
  against the trace bytes; opcode is byte-identical at PC 126305 in both traces.
- Skip taken (A==T) -> `126307` -> the OK path proceeds into `IOF`/`IRW` directory-I/O setup and
  reads/enters the directory.
- Skip not taken (A!=T) -> `126306 JMP *4` -> `126312 = COPFI` (FILSYS symbol) -> a value-format
  conversion routine (`011577`) -> alternate/reject path; the directory is never read.

### The field and the two values (VERIFIED)

| symbol / expression | OK (SMD)  | FAIL (SCSI) |
|---------------------|-----------|-------------|
| `A = mem[B+25]` (device-descriptor word, `LDA 25,B`, B=132473) | **001100** | **002210** |
| `T = 001100` (constant `SG110`, `LDT ,X I *67`) | 001100 | 001100 |
| `SKP IF DA EQL ST` result | equal -> **skip -> MOUNT** | unequal -> **no-skip -> REJECT** |

The same value is loaded earlier in the routine at `126016 LDA 34,B` (mem[B+34] = 001100 / 002210),
i.e. it originates in the descriptor's configuration/geometry area and is carried into field +25.

Octal/decimal: `001100 = 576`, `002210 = 1160`. The test is **exact equality** against a fixed
system constant (`SG110`), not a range/size check — so 001100 behaves as a required
directory/geometry **format value** that every mountable SINTRAN directory device must present.

---

## Plain-English result

> **SINTRAN mounts the disk only if the directory-descriptor word `mem[B+25]` equals `001100`
> (octal) — the constant `SG110`, tested at PC `126305` (`CHIND+2`) by `SKP IF DA EQL ST`.
> The SMD disk presents `001100` and mounts. The emulated SCSI disk presents `002210`, so the
> equality fails, `CHIND` branches to `126306 -> 126312 (COPFI)` instead of `126307`, and the
> directory is never read.**
>
> **Fix:** make the emulator's SCSI device present geometry/descriptor data such that this
> descriptor word computes to `001100` (SG110) instead of `002210`.

---

## Cross-check: what kind of value is this? (step 4)

- It is **not** a ready/connected state (the SCSI device connects and answers INQUIRY/READ
  CAPACITY fine) and **not** a unit-select word.
- It is tested by **exact equality against a fixed constant** (`SG110 = 001100`), which is the
  signature of a **device-format / directory-geometry** word — a "this device is formatted the
  way SINTRAN directories must be" attribute. VERIFIED that the test is exact-equality against
  SG110; INFERRED that the semantic role of descriptor field +25 (mirrored at +34) is a
  directory/geometry format word.

## VERIFIED vs INFERRED

**VERIFIED**
- Gate PC 126305, opcode 140065 `SKP IF DA EQL ST`, identical bytes in both traces.
- Routine name `CHIND` (=126303) and reject-target `COPFI` (=126312) from L07 `SYMBOL-2-LIST` /
  `FILSYS-SYMBOLS`.
- Comparand constant = `001100` = symbol `SG110` (L07 `SYMBOL-2-LIST`, `SG110=001100`).
- Tested word `mem[B+25]` (B=132473): OK=001100, FAIL=002210 (register dumps at the gate).
- One-shot gate: OK takes mount arm once and never the reject arm; FAIL the opposite.
- Value is device-config-derived: all SCSI bus I/O occurs after the gate.

**INFERRED**
- The precise physical meaning of descriptor field +25 / +34 (called here a
  directory/geometry format word). It is compared for exact equality with a fixed system
  constant, which is why it reads as a format/type signature rather than a size.
- That correcting the SCSI device's presented geometry so this word becomes `001100` is
  sufficient to make the mount proceed (the gate is necessary; downstream directory-content
  checks were not re-verified past the gate).

## Method note (why earlier candidates were rejected)

- Cross-trace comparison by virtual PC is confounded by SINTRAN **PIT/S3SRPIT overlays**
  (same virtual address holds different code at different times) and by the fact SCSI and SMD
  use **entirely different drivers**. Gates were therefore filtered to PCs whose **opcode is
  byte-identical in both traces**, excluding overlay artifacts (e.g. 032704, 044767 disassemble
  differently in the two traces and are overlay/self-modifying dispatch, not gates).
- The name-lookup skip at `027074` (12x) and the accumulator loop `126033-126152`
  (both step 112000->114042 identically and both exit) are red herrings, not the gate.
- `126305` is the earliest clean, same-opcode, once-per-mount value-divergence, and its two arms
  demonstrably lead to directory-read (OK) vs reject (FAIL).
