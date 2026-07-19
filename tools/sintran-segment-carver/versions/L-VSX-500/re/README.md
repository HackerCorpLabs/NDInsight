# SINTRAN III L-VSX-500 (L07) - reverse-engineering tree (`re/`)

Single entry index for all static reverse-engineering carried out on the carved L07 segments
(the `.bin` files under `../segments/`). Everything here is byte-verified against those segments
unless a document explicitly flags itself as inferred / different-revision / stub.

Method + fact sources: [`../../../EXTRACTING-SEGMENTS.md`](../../../EXTRACTING-SEGMENTS.md) and
[`../../../EXTRACTING-RESIDENT-CODE.md`](../../../EXTRACTING-RESIDENT-CODE.md).

---

## The two big efforts

### 1. Monitor-call analysis - [`mon-analysis/`](mon-analysis/README.md)

Per-call reverse-engineering of SINTRAN III monitor calls (ND-100 + ND-500). The master table is
[`MON-CALL-INDEX.md`](MON-CALL-INDEX.md), regenerated from the carved `MCTAB` (the real monitor-call
table at `005620B`, which is ground truth for what this build implements and where each worker lives).
Each analysed call has a `NNB-Name/` folder (dispatch flow, `.ASM`, carved `.bin` where available).
The TASK-05 deliverable covers 31 undocumented/unclear calls; consolidated contracts are in
[`TASK-05-results.md`](TASK-05-results.md).

The largest single call is **MON 60B / N500M**, the ND-100 -> ND-500 control gateway, carved in
[`mon-analysis/60B-N500M/`](mon-analysis/60B-N500M/README.md): 47 subfunction folders (README +
`.pseudo.c` + verbatim `.npl` from `5P-P2-MON60.NPL`), the `5IFUNC` dispatch table, the documented-
subfunction list, and the caller-vs-worker cross-analysis. The MON 60B worker `N500M` runs in the
5PIT context (segment `050-S3I5PIT`).

### 2. ND-500 system-monitor carve - [`ND500-SYSTEM-MONITOR/`](ND500-SYSTEM-MONITOR/README.md)

The server side of MON 60B: the ND-100-resident ND-500 system monitor (segment `030-S3SM5`, base
`040000B`) that the `5NOPAR` common path hands off to via `FPT2ENTRY` -> `5FP2E`. Fully carved and
byte-verified: the `FUNCS` dispatch table (twin of `5IFUNC`), the 3022 IOX bus driver + register map,
the control-store gate (`RSTA5` bit 9 `5CLOST`), the 5MPM message + `ACT50` activation, the level-12
return path (`5STDR`/`CHN5S`/`DECOM`/`MCHAN`, which live in the RESIDENT interrupt segment
`026-S3IMPIT`, not `030-S3SM5`), and ALL ~60 FUNCS operation routine bodies
([`ND500-SYSTEM-MONITOR/FUNCS-BODIES/`](ND500-SYSTEM-MONITOR/FUNCS-BODIES/README.md)).

**End to end, the MON 60B path is now:** command -> caller thunk (SAA) -> gateway -> MON 60 ->
N500M `5IFUNC[code]` (param prep) -> `5NOPAR` -> `FPT2ENTRY` -> `5FP2E` -> `FUNCS[code]` ->
ND-500 operation, with a named routine at every hop. Overlay facts: `N500M` worker = `050-S3I5PIT`;
system monitor = `030-S3SM5`; level-12 ISR = `026-S3IMPIT`.

---

## Other material in this tree

| Path | What it is |
|------|-----------|
| [`mon-emulation/`](mon-emulation/README.md) | Emulator-oriented models of selected monitor calls (`050B-OPEN`, `144B-MAGTP`). |
| [`kernel-carving/`](kernel-carving/) | Filesystem / SCSI / disk-driver carve work (ENTER-DIRECTORY, SCSI driver, NAMEWALK, RESERVE, boot block, ...). |
| [`instruction-semantics/`](instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md) | ND-100 and ND-500 instruction-semantics references (from Ghidra + emulator). |
| [`segments-ref/`](segments-ref/) | Per-segment reference notes and symbol data. |
| [`ND500-HANDLERS-OVERLAY.md`](ND500-HANDLERS-OVERLAY.md) | Which overlay the ND-500 5xx level-12 handlers execute in. |
| [`030-S3SM5-routine-map.md`](030-S3SM5-routine-map.md) | Routine map of the ND-500 system-monitor segment. |
| [`README-password-login.md`](README-password-login.md), [`PASSWORD-ALGORITHM.md`](PASSWORD-ALGORITHM.md) | Login / password-fold analysis (live-capture handoff in [`HANDOFF-fold-live-capture.md`](HANDOFF-fold-live-capture.md)). |
| [`HANDOFF-MON-SWEEP.md`](HANDOFF-MON-SWEEP.md) | Open handoff: sweep remaining MON calls. |

ND-500 project status/index: `../../../../../SINTRAN/ND500/ND500-STATUS-AND-INDEX.md`.
