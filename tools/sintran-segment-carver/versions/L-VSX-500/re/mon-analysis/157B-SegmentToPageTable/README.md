# MON 157B (octal) - SegmentToPageTable (ENTSG)

Enters a routine as a direct task or a device driver and "remembers" which segments have been
entered (up to 24), so they are re-entered at restart after a power failure. The routine is
connected to the interrupt system; the segment where it resides must be fixed in memory.
Documented short-name **ENTSG** (`@ENTSEG`); the worker is **ENTSG = 067764B** in segment
`025-S3IRPIT`.

**Status:** **byte-verified** (dispatch + worker body). All addresses/values are **octal**.

> **CORRECTED 2026-07-13.** The previous version of this folder said the worker was
> `misattributed`: it read a "GOTAB" out of `SINTRAN-DATA_commoncode`, got `GOTAB[157B]=122506B`
> vectoring to an `F1676` entry stub, and could only attach `ENTSG` by name across an "uncarved
> CALLPROC bridge". **That whole GOTAB-in-commoncode table is fiction** - its slot 0 is `000000`,
> not the real GOTAB slot-0 `MFELL=072114B`; it is the wrong overlay. MON calls are dispatched
> through **`MCTAB @ 005620B`** (segment `044-S3IDPIT`), indexed directly by N.
> **`MCTAB[157B] = 067764B = ENTSG`** - the SAME worker the old folder found by name, now proven
> directly from the table slot, no stub and no uncarved bridge. See
> [`../317B-ExecuteCommand/README.md`](../317B-ExecuteCommand/README.md) and
> `SINTRAN/CARVING-HANDOFF.md` section 3a.

- **Full disassembly:** [`157B-SegmentToPageTable.ASM`](157B-SegmentToPageTable.ASM).
- **Emulator model:** [`157B-SegmentToPageTable.pseudo.c`](157B-SegmentToPageTable.pseudo.c).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 157B"] --> B["ENT14 level-14<br/>026-S3IMPIT :072167B"]
    B --> C["GOTAB[157B] = MFELL<br/>MGOTA=071233B"]
    C --> D["MFELL level switch<br/>:072114B -> CALLP 032201B"]
    D --> E["MCTAB[157B] = ENTSG<br/>044-S3IDPIT :005777B = 067764B"]
    E --> F["ENTSG worker<br/>025-S3IRPIT :067764B"]
    class A blue
    class B,C,D,E teal
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

There is **no dashed hop**. `GOTAB[157B]` is `MFELL` (as it is for 224 of the 256 calls); `MFELL`
is a carved program-**level** switch (writes `CALLP=032201B` into the monitor level's `P`), and
the monitor level then dispatches through `MCTAB`.

---

## Code location (dispatch path)

Byte offset = `(addr - loadbase)` in octal words x 2 (decimal). Offsets reproduced with `dd`.

| Role | Segment | Addr (octal) | Byte offset | Symbol | Verdict |
|------|---------|--------------|-------------|--------|---------|
| MCTAB[157B] slot | [044-S3IDPIT.asm](../../segments-ref/044-S3IDPIT/044-S3IDPIT.asm) - [.hex](../../segments-ref/044-S3IDPIT/044-S3IDPIT.hex) | `005777B` = `067764B` | 2046 | -> `ENTSG` | **VERIFIED** |
| ENTSG worker body (excerpt) | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) - [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `067764B-070107B+` | 30696 | `ENTSG` (SYMBOL-2-LIST) | **VERIFIED** |

**Verify by hand** (from `tools/sintran-segment-carver/versions/L-VSX-500/segments/`):
```
dd if=044-S3IDPIT.bin bs=1 skip=2046  count=2 | od -An -tx1   ->  6f f4   (= 067764B = ENTSG)
dd if=025-S3IRPIT.bin bs=1 skip=30696 count=2 | od -An -tx1   ->  ba 54   (= 135124B, ENTSG entry JPL I 124)
```
The identical symbol `ENTSG=067764B` in resident commoncode (load base 0, byte offset 57320) is
zero: `dd if=../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=57320 count=2 | od -An -tx1` -> `00 00`.
That zero overlay is what the old analysis misread; the real code lives in `025-S3IRPIT`.

---

## Instruction walkthrough

Full listing: [`157B-SegmentToPageTable.ASM`](157B-SegmentToPageTable.ASM). Region B is an
**excerpt** of a longer routine (it continues past `070107B` before returning).

**Parameter read + range checks (`067764B-070025B`).** After an in-overlay call (`JPL I 124`),
it reads the parameter block (`LDA ,B 20/21/22/23` = SegmentNo / PageTable / InterruptLevel /
StartAddress) and range-checks each: SegmentNo `< 30`, PageTable in the `6..17B` VSX range,
InterruptLevel in `6,7,10B,11B`. Each failure takes an error/exit vector (`JMP I`).

**Segment-table index + presence test (`067775B-070016B`).** `MPY 120 / ADD I 120` forms a
segment-table pointer (entry stride `120B`), then `LDATX` does a **physical** read (24-bit,
MMU-bypassed) and `BSKP ONE 10` tests a presence bit.

**PIT-slot write (`070026B-070037B`).** Computes a slot base (`LDX 75 / RADD SB DX`) and stores
`(SegmentNo, PageTable<<8 | level, StartAddress)` into `,X 0/1/2` - the restart record.

**Segment-table scan + page-table walk (`070040B-070106B`).** Scans the segment table for an
already-entered `(segment, page-table)` match with `LDDTX` physical pair reads; then walks the
target page table with `LDXTX`/`LDATX` physical reads, masking the page-frame field (`AND 36B`)
and ORing in the page-table-select bits (`RORA SL DA`) until the chain terminates.

The physical transfers are the mechanism by which a level-14 handler edits page tables directly.
The routine tail past `070107B` (page-table entry commit + `EXIT`) is beyond this excerpt.

---

## Parameter / register contract

Manual-side names/types are from [`157B_SegmentToPageTable.yaml`](../../../../../../../Developer/MON/calls/157B_SegmentToPageTable.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A` (param-list addr) | in | address of the parameter list (`MAC` example `LDA (PAR / MON 157`) | inferred (manual) |
| `SegmentNo` (INTEGER2) | in | segment where the routine resides | VERIFIED (read `mem[B+020]`, range-checked `< 30`) |
| `PageTable` (INTEGER2) | in | page table for the segment (in practice 3; VSX K: 0-17B) | VERIFIED (read `mem[B+021]`, range-checked `6..17B`) |
| `InterruptLevel` (INTEGER2) | in | free interrupt level 6, 7, 10B or 11B | VERIFIED (read `mem[B+022]`, range-checked) |
| `StartAddress` (INTEGER2) | in | entry point of the direct task | VERIFIED (read `mem[B+023]`, stored to PIT slot) |
| PIT slot | out | `(SegmentNo, PageTable<<8\|level, StartAddress)` written for restart | VERIFIED (`STA ,X 0/1/2`) |
| error return | out | standard error code (appendix A) | inferred (manual; error vectors in the routine tail) |

The parameter offsets, the range checks and the PIT-slot write are byte-proven; the final
page-table commit and the error-code values live in the routine tail beyond the excerpt and
remain **inferred**.

---

## Pseudo-code (for an emulator)

See **[`157B-SegmentToPageTable.pseudo.c`](157B-SegmentToPageTable.pseudo.c)** - the `ENTSG`
parameter processing, range checks, PIT-slot write and physical page-table walk are byte-verified
from the excerpt; the routine tail is marked UNVERIFIED. Physical transfers (`LDDTX`/`LDXTX`/
`LDATX`) are 24-bit MMU-bypassing accesses per
[`../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
section 5.

---

## Honest caveats

**What is byte-proven:** `MCTAB[157B] @005777B = 067764B = ENTSG`; the `ENTSG` entry bytes at
`067764B` in `025-S3IRPIT` (`135124B = JPL I 124`) match the disassembly; the body reads the
documented parameter block, range-checks it, writes the PIT restart slot and edits page tables
with physical `LDDTX`/`LDATX` transfers - exactly SegmentToPageTable. The `MCTAB` identification
is not a one-slot coincidence: 216 of 256 slots land on named L07 symbols (e.g. `MON 005B ->
RDISK`, `MON 200B -> XMSG 007516B`).

**What is NOT proven:** the routine **tail** past the carved excerpt (the final page-table entry
commit + `EXIT`) and the exact error-code contract. Those live beyond `070107B` and are not
recovered here.

**Correction to earlier work.** The old folder's `GOTAB[157B]=122506B -> F1676` chain was the
wrong-overlay artefact described above; `MCTAB[157B]` is the real link, byte-proven, so the
verdict is upgraded from `misattributed` to **VERIFIED**. The `F1676` bytes are real code but
are **not** on the MON 157B path.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) -
master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
