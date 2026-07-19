# MON 235B (octal) - ScratchOpen (SCROP)

Opens a file as a scratch file, returning an open file number. A maximum of 64
pages of the file is kept when you close it (use SET-CLOSED-FILE-SIZE in the
SINTRAN-SERVICE-PROGRAM to change this). The scratch file is closed like any
other opened file with [MON 43B CloseFile](../43B-CloseFile/README.md).

**Status:** GOTAB dispatch head byte-proven as **fall-through** (`GOTAB[235B] = 000000`,
no per-call stub); `SCROP` is a 3-word flag-fork prelude into a shared `OPEN`
worker body (real SINTRAN L bytes) - the same body used by
[MON 220B DirectOpen](../220B-DirectOpen/README.md) - that calls the `FOPEN`
file-open primitive; the exact `MON 235 -> worker` link crosses an uncarved
kernel bridge (see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`235B-ScratchOpen.ASM`](235B-ScratchOpen.ASM) - the SCROP prelude + the shared OPEN body (there is no entry stub because the GOTAB slot is zero).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 235B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[235B] = 000000<br/>(byte-proven: fall-through)"]
    C -.uncarved MFELL / CALLPROC.-> D["SCROP prelude<br/>006-S3FS :103031B<br/>set M=1 K=0"]
    D --> E["shared OPEN body<br/>006-S3FS :103041B"]
    E --> F["FOPEN file-open primitive<br/>006-S3FS :67432B"]
    class A blue
    class B,C blue
    class D teal
    class E,F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

The GOTAB slot is zero, so there is **no per-call entry stub**. The dashed hop
(`C -> D`) is the resident `MFELL`/`CALLPROC` fall-through second-level dispatch -
it is **not present in any carved segment**, so it is the one link that cannot be
followed statically. `SCROP` sets two STS scratch flags (M=1, K=0) and jumps into
the common `OPEN` body, which reads the flags back to select the scratch-open mode.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[235] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071470B` (1 word) | 58992 | `GOTAB+235` = `000000` | **VERIFIED** (fall-through) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| SCROP flag-fork prelude | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `103031B-103033B` (3w) | 46130 | `SCROP` | real bytes; link **MISATTRIBUTED** |
| shared OPEN worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `103041B-103347B` (207w) | 46146 | (unnamed; between `OLDOP` and `103350B`) | real bytes |
| FOPEN file-open primitive | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `67432B` (call target) | 32340 | `FOPEN` | called by OPEN body (link cell `103341`) - **VERIFIED** |

**Verify by hand:** `grep '^103031 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `46130`;
then `dd if=../../../segments/006-S3FS.bin bs=1 skip=46130 count=6 | od -An -tx1` -> `f8 b8 f8 10 a8 06`
(= octal `174270 174020 124006` = `BSET ONE SSM` / `BSET ZRO SSK` / `JMP 6`, the SCROP prelude).

The GOTAB slot itself:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58992 count=2 | od -An -tx1` -> `00 00` (= `000000`, fall-through).

---

## Instruction walkthrough

Full listing: [`235B-ScratchOpen.ASM`](235B-ScratchOpen.ASM).

**SCROP prelude (`103031-103033`)** - `103031 BSET ONE SSM` / `103032 BSET ZRO SSK`
set the STS scratch flag bits to M=1, K=0; `103033 JMP 6 -> 103041` enters the
shared body. The four open entries differ only in these two bits: `DOPEN`
(MON 220) M=1,K=1; `SCROP` M=1,K=0; `OPFIL` M=0,K=0; `OLDOP` M=0,K=1.

**Shared OPEN entry (`103041-103045`)** - `103041 STD I 173` stashes the caller's
double-word parameter; `103044 SAB 150` builds the 150-word local frame;
`103045 JPL I 170` -> `003752` is the shared resident prologue worker.

**Flag readback / mode select (`103046-103063`)** - `103046 BSKP ONE SSM` and
`103050/103056 BSKP ONE SSK` read the fork flags back; SCROP arrives with M=1,K=0
so `103054 SAA 2` selects the scratch-open mode, stored at frame `B+147`
(`103063 STA ,B 147`).

**Slot scan / open (`103064-103312`)** - the body copies the caller's name/type
descriptor words into the frame, scans the open-file table for the mode's slot
(indirect `JPL I` calls through the pointer table at `103235-103266`), and
finally opens the file: `103310 JPL I 31` -> `103341 = 067432` (**FOPEN**, the
file-open primitive). This call is the byte-level proof that this body is the
open worker.

**Store status / return (`103313-103347`)** - `103313 STA ,B 2` writes the result
word into the caller's status slot `B+2`; `103314-103321` restore the saved
descriptor words; `103323 JMP I 22` -> `103345 = 003776` is the resident return.
Words `103234-103266` and `103335-103347` are pointer tables (data), rendered by
nd100-dis as bogus instructions.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| entry point | in | `103031B` = SCROP prelude (fall-through, no stub) | VERIFIED (bytes) |
| STS M (SSM) | internal | `BSET ONE SSM` = fork flag 1 | VERIFIED (bytes) |
| STS K (SSK) | internal | `BSET ZRO SSK` = fork flag 2 (M=1,K=0 = ScratchOpen) | VERIFIED (bytes) |
| `X` (manual) | in | address of file-name string | inferred (manual MAC example) |
| `A` (manual) | in/out | in: address of default file-type string; out: open file number | inferred (manual MAC example) |
| `T` (manual) | in | access code (0..9) | inferred (manual) |
| local frame `B` | internal | `SAB 150` = 150-word working frame | VERIFIED (bytes) |
| mode word `B+147` | internal | selected access mode from the M/K flags | VERIFIED (bytes) |
| `B+2` | out | returned status word (`STA ,B 2` at `103313`) | VERIFIED (bytes) |

The user-visible `X`/`A`/`T` convention lives in the caller-side `MON 235`
wrapper and the uncarved `MFELL`/`CALLPROC` frame, so the precise
user-register-to-field assignment is **inferred** from the manual, not byte-proven
here. The access-code meanings are from the
[`235B_SCRATCHOPEN.yaml`](../../../../../../../Developer/MON/calls/235B_SCRATCHOPEN.yaml) parameter contract.

---

## Pseudo-code (for an emulator)

See **[`235B-ScratchOpen.pseudo.c`](235B-ScratchOpen.pseudo.c)** - a pseudo-C model of
the handler for emulator authors. The flag-fork mechanism, control flow, and the
call to the FOPEN primitive are byte-verified; the parameter-field semantics and
access-code meanings are inferred from the call structure and the manual.

Every instruction in the pseudo-code is translated against the canonical
[ND-100 instruction semantics reference](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(BSET/BSKP bit ops with `dr=0` = STS bit, RADD/COPY register ops, addressing-mode
effective addresses, and skip/branch senses).

---

## Honest caveats

**What is byte-proven:** `GOTAB[235B] = 000000` (level-14 dispatch, a fall-through
with no per-call vector); the `SCROP` prelude at `103031B` in `006-S3FS` is real
code (`174270 174020 124006` = `BSET ONE SSM` / `BSET ZRO SSK` / `JMP -> 103041`);
the shared `OPEN` body at `103041B` is real code and calls `FOPEN` (`067432B`, link
cell `103341`), the file-open primitive.

**Shared-body fork (documented honestly):** `DOPEN` (103026), `SCROP` (103031),
`OPFIL` (103034) and `OLDOP` (103037) are each **three-word preludes** that set the
STS `M` and `K` flag bits to a distinct 2-bit code, then all `JMP` into the **same**
body at `103041`. The body reads the two bits back (`103046-103062`) to choose the
access mode. So ScratchOpen and DirectOpen share one worker body; they differ only
in the M/K flag pair set by their prelude (`SCROP` = M=1,K=0). The scratch-specific
64-page retention behaviour is a property of the file-open primitive / the mode
value, not of a separate code body - it is not isolated in these bytes.

**What is NOT proven:** the link from the zero GOTAB slot to the `SCROP` prelude.
Because the vector is zero there is no stub to disassemble and no pointer to
dereference; dispatch drops into the resident `MFELL`/`CALLPROC` second-level path,
which lives in an **uncarved overlay** - hence **MISATTRIBUTED** in the strict
sense. Confirming the link needs a live trace: issue a real `MON 235`, single-step
the level-14 fall-through into the resident `CALLPROC` dispatch, and confirm P
lands on `SCROP = 103031`.

Several pointer-table cell contents (`003752`, `010500`, `010506`, `003776`, and
others) match no `FILSYS-SYMBOLS` entry; their low addresses suggest resident-monitor
/ save-restore routines outside the file-system segment and are not resolved here.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
