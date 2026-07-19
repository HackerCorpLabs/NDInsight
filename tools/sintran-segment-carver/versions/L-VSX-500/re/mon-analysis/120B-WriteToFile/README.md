# MON 120B (octal) - WriteToFile (WFILE)

> **CORRECTED 2026-07-15 (byte-verified).** The worker + dispatch described below are on the
> DEBUNKED model and are WRONG. Byte truth from the carved L07 image:
> `MCTAB[120B] = 005740B = XWFIL=026407B` in segment 006-S3FS, reached by the real dispatch
> `MON 120B -> ENT14(072167B) -> GOTAB[120B]=MFELL(072114B) -> CALLP(032201B) -> MCTAB[120B]=XWFIL`.
> Any "GOTAB from commoncode" / "uncarved CALLPROC bridge" / "F16xx stub" / old worker name below
> is an artefact of the wrong table. Verified: `dd if=044-S3IDPIT.bin bs=1 skip=1984 count=2`
> -> `2d 07`. Cross-ref ../317B-ExecuteCommand/README.md and SINTRAN/CARVING-HANDOFF.md sec 3a.

Writes a run of bytes to an opened file starting at a block boundary
(byte offset = block-number x block-size). It shares one code body with MON 117B
ReadFile (RFILE); the two enter two words apart and the body forks on a
read/write skip flag (`SSK`).

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[120B] = 000000`,
no per-call stub); the WFILE worker body is real SINTRAN L bytes; the exact
`MON 120 -> worker` link crosses an uncarved kernel bridge (see
[Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`120B-WriteToFile.ASM`](120B-WriteToFile.ASM) - the actual code (the WFILE worker body; there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 120B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[120B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> E["WFILE write worker<br/>006-S3FS :102132B"]
    E --> F["fs write/read block<br/>via JPL I workers"]
    class A blue
    class B,C blue
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> E`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch -
it is **not present in any carved segment**, so it is the one link that cannot be
followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[120] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071353B` (1 word) | 58838 | `GOTAB+120` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| WFILE write worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `102132B-102516B` (245w) | 45236 | `WFILE` | real bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^102132 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `45236`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=45236 count=8 | od -An -tx1` -> `f8 90 22 6e cc 65 cc 59`
(= octal `174220 021156 146145 146131` = `BSET ONE SSK` / `STD I 156` ..., the WFILE write entry).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58838 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`120B-WriteToFile.ASM`](120B-WriteToFile.ASM). The functional body
is the WFILE worker (region B); there is no F16xx stub because `GOTAB[120] = 0`.
All calls to shared file-system workers are **indirect** (`JPL I` / `JMP I`)
through a small table of pointer words at the tail of the window
(`102311-102320` and `102501-102516`). nd100-dis renders those pointer words as
bogus instructions (`FAD`, `ROP NOOP`, `STZ I ...`) - they are **data (link
cells)**, not code; their contents are the real worker addresses (resolved below).

**Entry prologue (`102132-102151`)** - `102132 BSET ONE SSK` sets the read/write
selector to 1 (write); WFILE enters here, RFILE (`102130`, two words earlier)
reaches the same joined code with the flag clear. `102136 SAB 17` builds the
local frame `B`; `102137 JPL I 153` -> `003752` is a prologue worker. `102140 LDX
,B 5` loads `X` = pointer to the caller's parameter RECORD, and `102141-102150`
copy record fields (`X+23`,`X+22`,`X+24`,`X+20`) into frame slots; `102151 JPL I
143` -> `010376` is a second setup worker.

**Read/write split + validation (`102152-102207`)** - `102152 BSKP ONE SSK`
branches on the flag and `102157 STX ,B 10` records the direction/mode word.
Argument validation follows: each failure loads an error number with `SAA`
(`132`, `126`, `125`, `133`) and exits via `JMP I` -> link cell `102315` (=
`102476`, the common exit).

**Block / byte arithmetic (`102210-102305`)** - `102211-102215` handles block
`-1` ("next") via `JPL I 101` -> `ATMUL` (`033740`); the else path validates the
block with `RDIV` by the block-size field and returns error `57` on a bad
remainder. `102254-102255 MPY ,X 14` multiplies the block number by the
block-size (`X+14`); `102256-102304` build the 32-bit (INTEGER4) byte count and
double-word offset. `102305 JPL I 13` -> `CLRDB` (`035250`) clears the buffer.

**Core transfer (`102321-102432`)** - `102321 BSKP ONE SSK` selects the half.
Write half (`102323-102362`): `102342 JPL I 141` -> `CHSGM` (`101373`, segment
change/check), then `102351 JPL I 133` -> `FDWRT` (`100570`, file-data write) or
`102361 JPL I 124` -> `FWRT` (`100130`, file write). Read half (`102364-102432`,
mirror bounds checks): `102423 JPL I 63` -> `FDREA` (`100566`) or `102431 JPL I
56` -> `FREA` (`077542`). Each half falls to `JMP ... -> 102476` on completion.

**Exit (`102433-102500`)** - `102437-102460` recompute the residual byte count
into `B+11` (SAA `31` on underflow), guarded by `SVCAL`/`RSCAL` (`102462`/`102471`
link cells `027542`/`027576`). `102473 LDA ,B 11` fetches the status, `102476 STA
,B 2` stores it into the caller's status slot, and `102500 JMP I 16` -> `003776`
returns through the resident return cell. `102476` is the single unified "store
status and return" point that every error path funnels into via link cell `102315`.

Presence of **both** the write workers (`FWRT`, `FDWRT`) **and** the read workers
(`FREA`, `FDREA`) in one body is the byte-level proof that WFILE and RFILE share
this code, selected by the `SSK` skip flag.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `102132B` = write (WFILE); `102130B` = read (RFILE), shared body, `SSK` split | VERIFIED (bytes) |
| `SSK` | internal | read (0) / write (1) selector; set at WFILE entry `102132`, tested at `102152/102202/102243/102321` | VERIFIED (bytes) |
| `B+5` | in | pointer to caller parameter RECORD (`LDX ,B 5`) | VERIFIED (bytes) |
| record `X+20`,`X+22`,`X+23`,`X+24` | in | parameter fields copied to frame (file / buffer / block / bytes group) | VERIFIED (copies); field meaning inferred |
| record `X+14` | in | block-size (used as `MPY` multiplier) | VERIFIED (bytes); label inferred |
| `B+10` | int | direction/mode word | VERIFIED (bytes) |
| `B+11` | int | accumulated error/status code | VERIFIED (bytes) |
| `B+2` | out | returned status word (`STA ,B 2` at `102476`) | VERIFIED (bytes) |

Error numbers observed as literals (`SAA 3/31/57/125/126/132/133`, octal) are
VERIFIED in the code; their mapping to the SINTRAN error-code table is
**UNVERIFIED**. The user-visible A/T/X register convention lives in the
caller-side `MON 120` wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the
precise user-register-to-field assignment is **inferred**, not byte-proven here.
The manual's documented order (file number, return flag, buffer, block number,
NoOfBytes INTEGER4) is consistent with the field copies seen but is not
byte-proven.

---

## Pseudo-code (for an emulator)

See **[`120B-WriteToFile.pseudo.c`](120B-WriteToFile.pseudo.c)** - a pseudo-C
model of the handler for emulator authors. Control flow + the read/write (`SSK`)
fork are byte-verified; the file-system worker semantics and error-number
meanings are inferred from the call structure. Every instruction is translated
per the canonical
[`../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

---

## Honest caveats

**What is byte-proven:** `GOTAB[120B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector - matches a live read of the running system); the `WFILE`
worker body at `102132B` in `006-S3FS` is real code (entry bytes match the
disassembly); and it belongs to the read/write file family (its `SSK` split calls
both `FWRT`/`FDWRT` and `FREA`/`FDREA`).

**What is NOT proven:** the link from the zero GOTAB slot to the `WFILE` worker.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level
path, which lives in an **uncarved overlay**. So the `MON 120 -> WFILE`
attribution rests on the `WFILE` symbol name + the matching write behaviour, not
a followed pointer - hence **MISATTRIBUTED** in the strict sense. (An earlier
note citing `GOTAB[120B] = MFELL ... MP-P2-2.NPL:342` was an NPL-source claim from
a *different* revision, not a byte proof; the real L bytes only show
`GOTAB[120] = 000000` and do not name `MFELL`.) Confirming the link needs a live
trace: issue a real `MON 120`, single-step the level-14 fall-through into the
resident `CALLPROC` dispatch, and confirm P lands on `WFILE = 102132`.

Several link-cell contents (`003752`, `010376`, `004141`, `004177`, `004116`,
`003776`) match no `FILSYS-SYMBOLS` entry; their low addresses suggest
resident-monitor / save-restore routines outside the file-system segment and are
not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
