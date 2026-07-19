# FUNCTION-42-RETURN - what SCSI READ FORMAT returns, the 5SCIN state, and whether it skips block 0

Byte-verified reverse-engineering of **SCSI disk-layer function 42 (READ FORMAT)**
in SINTRAN III VSX/500 **L07**: every return value it can hand its caller, the
condition for each, the **5SCIN** initialization-flag state machine, and whether
any of that causes the `@ENTER-DIRECTORY ,,DISC-SCSI-1,0` mount to skip the
block-0 (page-0 directory) read.

This closes the specific hypothesis that the two companion carves
([`../SCSI-DRIVER/`](../SCSI-DRIVER/), [`../RCBLO/`](../RCBLO/),
[`../ENTER-DIRECTORY/`](../ENTER-DIRECTORY/)) left as "function 42 returns
geometry, block-0 is a separate upstream request." Here we prove **exactly what
function 42 returns and what state it leaves**, from the bytes.

- **Segment:** `065-S3SIPIT` (load base `32000B`; byte-identical twin `066-S3IIPIT`).
- **Source bin:** `../../segments/065-S3SIPIT.bin` (52224 words).
- **Disasm:** `nd100-dis -a -o -b 13312` over the byte-swapped image.
- Evidence: **VERIFIED** = read from L07 bytes (dd offset = `(addr-32000B)*2` decimal).
  **INFERRED** = from NPL logic / architecture. **OPEN** = uncarved / runtime.
  All addresses/values **octal**.

---

## 0. The revision-offset fact that unblocked this (byte-verified)

The SCSI **disk layer** (`IP-P2-SCSI-DISK`: SCSDI / SCSID / INQUI / function 42)
lives in the L07 image at **NPL-source-label + 376B**:

| Symbol | NPL label | L07 (SYMBOL-2-LIST) | delta |
|--------|-----------|---------------------|-------|
| SCSID  | 061621    | **062217**          | +376B |
| SCSDI  | -         | 057215              | -     |

Proof: `062217 - 061621 = 376B`. The SCSI **driver** (`IP-P2-SCSI-DRIV`:
`SCLLD=067160`, `SCINT=067247`, `SELEC=070165`, `INITO=070261`) is a *separate*
compilation unit and DOES align with its NPL labels - which is why the companion
`SCSI-DRIVER` carve verified the driver at the NPL addresses but its function-42
disassembly (taken at the **NPL** labels `057074 / 062214 / 062503`) decoded to
`IOF / ION / FAD` **garbage**. Those were the wrong addresses. Adding `376B`
makes every function-42 instruction below decode coherently and match the NPL.

Byte spot-checks (dd, big-endian words):

| L07 addr | byte off | word | mnemonic | meaning |
|----------|----------|------|----------|---------|
| `057472` | 22132 | `171042` | `SAT 42` | `IF A=42` (function-42 gate) |
| `057506` | 22156 | `174075` | `BSET ZRO 70 DA` | **CLEAR 5SCIN** on entry |
| `062217` | 24862 | `006010` | `STA ,X 10` | `SCSID:` entry (confirms +376B) |
| `063101` | 25730 | `044423` | `LDA ,B 23` | pre-`BSET ONE 5SCIN` (INQUI done) |

---

## 1. Function-42 return-value table (VERIFIED)

Function 42 hands its caller a **return code in `T`** (0 = OK) and, on success,
writes geometry into the caller's DMA buffer (`SLINK.MEMA1/MEMA2`). Internal
disk-layer code values are proven at the ERREX->user-error map (`062124`:
`IF D=TYPER(1)`, `IF D=ILAOP(4)`, `IF D=BADPA(5)`).

| `T` | Symbol | Condition (byte site) | Caller-visible effect |
|-----|--------|-----------------------|-----------------------|
| **0** | OK | control record read, HSTAT ok, XOR checksum = 0, `2 < NPART <= NCOPA` (`057716->057747`) | buffer := {data-area size, UHLIM, status word `36`}; op terminates OK |
| 1 | TYPER | after inquiry `SUTYP>>8 != 0` = illegal device type (`057552-057555`) | error; mapped to user code `61` |
| 4 | ILAOP | illegal operation (`057233`, e.g. non-direct-access) | error; mapped to user code `55` |
| 5 | BADPA | bad partition / address out of `UHLIM` bounds (`057401`; partition path `056707+`) | error; mapped to user code `174B` (illegal parameter) |
| 11 | NOCRC | control-record XOR `!= 0`, or `NPART<=2`, or `NPART>NCOPA` (`057707-057720`) | error "no control record" |
| (6 / 13) | UNIT-ATTN / ABORTED | SCSID status `6` or `13` | **not** returned - retried in-place (`TACOU--`, `GO RETRY`) |
| PFAIL/SBRST/LIRST | bus reset / power fail | at ERREX (`057240` region) | `MIN TACOU; GO RETRY` |

**Which one OUR run produces:** `blockSize=1024`, control record valid
(`NPART=8`, XOR `=0`), `UHLIM=121560`. Trace: `057472` (A=42) -> preamble ->
`057514` RETRY -> automatic INQUIRY + READ CAPACITY (5SCIN was cleared) ->
`057557` control-record `READ(6)` at the last LBA -> `057655` FINEX -> checksum
loop passes (`057716`) -> geometry published (`057737-057744`) -> **`057747
T := 0` (SUCCESS)**. This is the single `READ(6)` seen on the wire, and it
returns **success**, not an error.

---

## 2. The 5SCIN state machine (VERIFIED)

`5SCIN` = bit 7 of `SUTYP` (offset `23`), "inquiry / geometry initialisation done".

| Step | L07 addr | Action on 5SCIN |
|------|----------|-----------------|
| func-42 entry | `057506` `BSET ZRO 70 DA` | **CLEAR** (force re-inquiry) |
| RETRY gate | `057515` `BSKP ZRO 70 DA` | if clear -> run INQUIRY+READ CAPACITY |
| INQUI success | `063102` `BSET ONE 70 DA` | **SET** ("INITIALIZATION FINISHED") |
| **after func 42 returns** | - | **5SCIN = SET** |
| next fn-0 (block-0) read reads it | `063752-063754` `LDA SUTYP; BSKP ONE 70 DA; JMP I 163 -> INQUI` | 5SCIN SET -> **does NOT** divert to INQUI; proceeds to transfer |

**So: after function 42, 5SCIN is SET.** The only consumer of 5SCIN on the next
operation is the queue/entry gate at `063752`. With 5SCIN SET it takes the
fall-through (normal transfer -> `CACOB -> EXCOM -> SCLLD -> INITO -> SCWAQ`),
i.e. it **enqueues** block 0. It would only divert to INQUI if 5SCIN were CLEAR.
**5SCIN cannot cause a block-0 skip** - it is left in the state that *permits*
the block-0 read.

---

## 3. SCSI vs SMD function 42 - the real divergence

| | SCSI (`IP-P2-SCSI-DISK`, VERIFIED L07 bytes) | SMD/Winchester (`IP-P2-DISK-START`, INFERRED NPL, uncarved) |
|---|---|---|
| Disk I/O | INQUIRY + READ CAPACITY + one control-record `READ(6)` | **none** |
| Geometry source | computed from the platter (control record -> `UHLIM`, partitions) | static in-core table: format number `DISPN` from `HTABL` (`054632 *DEPO`) |
| Return on our run | `T := 0` (OK) + buffer {UHLIM, partitions, status `36`} | `HSTAT = 0` (OK) + buffer[0] = format number |
| Non-`BDISK` driver | n/a | `054066`: silent no-op, no error |
| Failure modes | TYPER/ILAOP/BADPA/NOCRC/PFAIL + retries | essentially none (table lookup) |

The divergence is **NOT the return code** (both report success) - it is
(a) SCSI does real, failure-prone I/O to learn geometry, and (b) SCSI's output
is a disk-derived `UHLIM` + partition table, whereas SMD's is a static format
index. Both then let the mount proceed to the separate block-0 read.

Because SMD's function 42 is a table lookup that cannot fail and needs no
follow-up state, the SMD mount proceeds to `CHDSI/RXDIR/RCBLO` -> block 0
unconditionally. The SCSI path reaches the **same** block-0 request point;
nothing in the SCSI disk layer diverts away from it.

---

## 4. The caller branch that decides "issue block-0 vs not"

**There is no such branch inside the SCSI disk layer or the driver.** Verified:

- Function 42 (`057472-057747`) terminates the operation via `RETEX -> RETOP`
  with `T=0`; it neither reads block 0 nor chains a block-0 request.
- The block-0 (page-0) directory read is a **separate fn-0 request** issued by
  the device-agnostic path: `ENDIR -> CHDSI -> RXDIR` (`037651` sets block 0)
  `-> RCBLO -> JPL I ,B 10` via datafield `,X 14 -> SCSDISK fn 0` (fully carved
  in [`../ENTER-DIRECTORY/`](../ENTER-DIRECTORY/) and [`../RCBLO/`](../RCBLO/)).
- When that fn-0 request re-enters the disk layer it hits the `063752` 5SCIN
  gate, which (5SCIN SET) lets it through to `SCLLD -> SCWAQ`.

So the decision "issue the block-0 read or not" is taken **entirely in the
uncarved device-agnostic connect/mount overlay** (the caller of function 42 and
of `CHDSI`), not in any byte carved here. The exact deciding instruction is
therefore **OPEN** from the SCSI disk-layer bytes - the same boundary the
`RCBLO`/`ENTER-DIRECTORY` carves reached, now confirmed from the *disk-layer*
side that the layer does not make that decision.

---

## 5. THE ANSWER

**The hypothesis - "SCSI function 42 returns a DIFFERENT value (or leaves a
different device state) and the caller BRANCHES on it and skips the block-0
read" - is DISPROVED on both the return-value axis and the 5SCIN-state axis, for
our run, from the bytes:**

1. **Return value:** function 42 returns `T = 0` (SUCCESS) - byte-verified at
   `057747`, reached because the control record is valid (XOR `=0`, `NPART=8`,
   `UHLIM` set). This is the **same success sense** SMD reports (`HSTAT=0`).
   There is no distinct "SCSI error return" for a caller to branch on, because
   function 42 **succeeded**.
2. **5SCIN state:** function 42 leaves 5SCIN **SET** (`063102`). The sole
   consumer, the fn-0 re-entry gate at `063752`, reads 5SCIN and - because it is
   SET - **does not** divert or block; it lets the block-0 read through to
   `SCLLD/SCWAQ`. A SET 5SCIN *enables* block 0, it cannot skip it. (A CLEAR
   5SCIN would only cause an extra INQUIRY, still not a skip.)

Therefore the block-0 skip is **not caused by function 42's return value or its
5SCIN state.** The fault is upstream, in the **uncarved device-agnostic
connect/mount overlay** that consumes function 42's *output* (the disk-derived
`UHLIM` / partition table / status `36`) and is responsible for then issuing the
block-0 request. Consistent with the ground truth (`SCWAQ` empty; no block-0
enqueue; silence after the control-record read), that caller never advances to,
or is rejected before, the block-0 request. This matches the `RCBLO` /
`ENTER-DIRECTORY` conclusion and now rules out the disk layer itself as the
brancher.

### The ONE settling runtime check
After the function-42 control-record read completes (`T=0`), with DAP breakpoints:

- **Caller advance:** break `CHDSI 037763B`. If **never hit** after func-42
  completion -> the connect/mount overlay never handed off to the page-0 read
  (it consumed `UHLIM`/status-36 and stopped or errored). Carve that overlay
  next - **not** the SCSI disk layer.
- **Disk-layer re-entry:** break `SCSID 062217B` and the 5SCIN gate `063752B`.
  If a **second** SCSID entry with `(ABFUN & 077) == 0` (a READ) and block-0
  address arrives -> read `SUTYP@23` (expect 5SCIN SET) and watch whether it
  reaches `SCLLD 067160B`. If it errors first (e.g. `BADPA`, `T=5`, from the
  `UHLIM` bounds check at `056760`), the block-0 **address** is mis-scaled
  against the SCSI `UHLIM` (the caller built a bad LBA). If SCSID is never
  re-entered, the caller never issued block 0.

The distinguishing read: at each break, dump the func-42 output buffer
(`SLINK.MEMA1/MEMA2` = `UHLIM` + status `36`) to see whether the caller received
sane geometry it could turn into a valid block-0 LBA.

---

## 6. VERIFIED / INFERRED / OPEN

| # | Claim | Verdict |
|---|-------|---------|
| 1 | Disk layer is at L07 = NPL + 376B (`SCSID 062217` = NPL `061621`+376) | VERIFIED (symbol + dd) |
| 2 | func-42 gate `IF A=42` at `057472` (`SAT 42`/`SKP IF DA EQL ST`) | VERIFIED (dd `171042`) |
| 3 | func-42 entry CLEARS 5SCIN: `057506 BSET ZRO 70 DA` -> `STA ,X 23` | VERIFIED (dd `174075`) |
| 4 | RETRY gate `057515 BSKP ZRO 70 DA` runs INQUIRY when 5SCIN clear | VERIFIED |
| 5 | INQUI SETS 5SCIN: `063102 BSET ONE 70 DA` ("INITIALIZATION FINISHED") | VERIFIED (dd `044423` @063101) |
| 6 | Single control-record `READ(6)` path `057557-057612` (`X.ABP32=1`, one block) | VERIFIED |
| 7 | Success return `T:=0` at `057747`; publishes UHLIM + status `36` (`057737-057744`) | VERIFIED |
| 8 | Return-code values OK=0/TYPER=1/ILAOP=4/BADPA=5/NOCRC=11 (ERREX map `062124`) | VERIFIED |
| 9 | Our run -> `T=0` (ctrl record valid, XOR=0, NPART=8, UHLIM set) | VERIFIED (bytes) + ground-truth trace |
| 10 | After func 42 -> 5SCIN SET; next fn-0 gate `063752` lets block 0 through | VERIFIED |
| 11 | SMD func 42 = static format number, no I/O, HSTAT=0 | INFERRED (NPL `IP-P2-DISK-START` 054066/054620; different uncarved segment) |
| 12 | No branch in the SCSI disk layer decides issue-block-0 vs not | VERIFIED (no such branch in the carved body) |
| 13 | Hypothesis (func-42 return/state causes block-0 skip) | DISPROVED for our run (return=0, 5SCIN=SET) |
| 14 | Exact deciding instruction (upstream never-issue vs mis-addressed LBA) | OPEN - settled by the section-5 DAP check |

**Provenance.** Carved `065-S3SIPIT` L07 bytes (load base `32000B`); symbols
`SINTRAN/NPL-SOURCE/SYMBOLS/L07/SYMBOL-2-LIST.SYMB.TXT` (SCSID/SCSDI/SCLLD...)
and `FILSYS-SYMBOLS`/`SYMBOL-1-LIST` (SUTYP=23, 5SCIN=7, SCDFA, SCWAQ); logic
(different revision, INFERRED) `SINTRAN/NPL-SOURCE/NPL/IP-P2-SCSI-DISK.NPL`
(function 42 / INQUI / FINEX / RETEX) and `IP-P2-DISK-START.NPL` (SMD function
42). Ground-truth trace: INQUIRY + READ CAPACITY + one control-record READ(6),
then silence; `SCWAQ` empty.

## See also
- [`../SCSI-DRIVER/README.md`](../SCSI-DRIVER/README.md) - driver core + the enqueue path (`SCLLD -> INITO -> SCWAQ`).
- [`../ENTER-DIRECTORY/README.md`](../ENTER-DIRECTORY/README.md) - device-agnostic mount path + the `,X 14` hand-off (the caller side).
- [`../RCBLO/README.md`](../RCBLO/README.md) - cache-block dispatcher; "no page-0 read" analysis + the same DAP settling check.
