# SINTRAN III monitor-call analysis - TASK-05 deliverable

Per-call reverse-engineering bundle for the undocumented / unclear SINTRAN III
monitor calls (ND-100 + ND-500), version **L-VSX-500**. Each call has its own
folder with a `README.md` (analysis + Mermaid dispatch flow + "how this was
carved"), a `.ASM`, and the carved handler `.bin` where the handler is a carved
binary. **Generated from the folders that actually exist on disk.**

Friend's TASK-05 list coverage: **31/31 folders**.

Status meanings (honest, per what is actually in each folder):
- **real SINTRAN L bytes ... + bin** = the actual L machine code, carved from a segment
  (file-system handlers from `006-S3FS.bin`; ND-500 level-12 handlers from the
  `S3MPIT` overlay - see `../../EXTRACTING-RESIDENT-CODE.md` 7.6/7.7).
- **NPL source - DIFFERENT revision** = `.ASM` is original NPL source of a
  *different* SINTRAN revision; behaviour is right, exact L bytes are not (real L
  recovery pending via the overlay method).
- **carved-only (stub)** = routing verified, handler body still inside a carved
  segment not yet disassembled - no fabricated `.ASM`/`.bin`.

Consolidated verified contracts: [../TASK-05-results.md](../TASK-05-results.md).

## MON 60B / N500M (ND-500 monitor) - separate large sub-effort

Beyond the TASK-05 list, the folder [`60B-N500M/`](60B-N500M/README.md) holds the full MON 60B
carve: 47 subfunction folders (README + `.pseudo.c` + verbatim `.npl`), the `5IFUNC` dispatch table,
the documented-subfunction list, and the caller-vs-worker cross-analysis. Its server-side twin (the
ND-500 system monitor that `5NOPAR` hands off to) is carved under
[`../ND500-SYSTEM-MONITOR/`](../ND500-SYSTEM-MONITOR/README.md). The `re/` tree index is
[`../README.md`](../README.md).

## ND-100 calls (friend TASK-05 list)

| MON | Folder | Status |
|-----|--------|--------|
| 5B | [005B-ReadScratchFile](005B-ReadScratchFile/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 6B | [006B-WriteScratchFile](006B-WriteScratchFile/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 13B | [013B-ClearInBuffer](013B-ClearInBuffer/README.md) | carved-only (stub, no fabricated code) |
| 14B | [014B-ClearOutBuffer](014B-ClearOutBuffer/README.md) | carved-only (stub, no fabricated code) |
| 15B | [015B-Undocumented](015B-Undocumented/README.md) | real SINTRAN L bytes (S3RPIT overlay) + bin |
| 42B | [042B-Undocumented](042B-Undocumented/README.md) | documented (GOTAB[42B]=0 fall-through; body in uncarved CALLPROC) |
| 45B | [045B-DefineBreakpoint](045B-DefineBreakpoint/README.md) | real SINTRAN L bytes (S3RPIT overlay) + bin |
| 51B | [051B-DMACBreakpoint](051B-DMACBreakpoint/README.md) | real SINTRAN L bytes (S3RPIT overlay) + bin |
| 67B | [067B-OutBufferSpace](067B-OutBufferSpace/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 74B | [074B-SetStartByte](074B-SetStartByte/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 75B | [075B-GetStartByte](075B-GetStartByte/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 120B | [120B-WriteToFile](120B-WriteToFile/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 144B | [144B-DeviceFunction](144B-DeviceFunction/README.md) | real SINTRAN L bytes (carved segment) + bin |
| 304B | [304B-SendSIBASMessage](304B-SendSIBASMessage/README.md) | NPL source - DIFFERENT revision (version-suspect) |
| 313B | [313B-InBufferState](313B-InBufferState/README.md) | NPL source - DIFFERENT revision (version-suspect) |
| 327B | [327B-FileSystemFunction](327B-FileSystemFunction/README.md) | real SINTRAN L bytes (carved segment) + bin |

## ND-500 calls (friend TASK-05 list)

| MON | Folder | Status |
|-----|--------|--------|
| 410B | [410B-FixInMemory](410B-FixInMemory/README.md) | carved-only (stub, no fabricated code) |
| 411B | [411B-MemoryUnfix](411B-MemoryUnfix/README.md) | carved-only (stub, no fabricated code) |
| 416B | [416B-SaveND500Segment](416B-SaveND500Segment/README.md) | carved-only (stub, no fabricated code) |
| 417B | [417B-MaxPagesInMemory](417B-MaxPagesInMemory/README.md) | carved-only (stub, no fabricated code) |
| 425B | [425B-SetProcessName](425B-SetProcessName/README.md) | documented-absent (S3SM5 dispatch slot = 0x0000, byte-verified) |
| 426B | [426B-GetProcessNo](426B-GetProcessNo/README.md) | documented-absent (S3SM5 dispatch slot = 0x0000, byte-verified) |
| 427B | [427B-GetOwnProcessInfo](427B-GetOwnProcessInfo/README.md) | documented-absent (S3SM5 dispatch slot = 0x0000, byte-verified) |
| 500B | [500B-StartProcess](500B-StartProcess/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 501B | [501B-StopProcess](501B-StopProcess/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 505B | [505B-GetTrapReason](505B-GetTrapReason/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 510B | [510B-CallSwapper](510B-CallSwapper/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 511B | [511B-DVIO](511B-DVIO/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 512B | [512B-XMSGCallA](512B-XMSGCallA/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 513B | [513B-XMSGCallB](513B-XMSGCallB/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |
| 515B | [515B-MultipleDataTransfer](515B-MultipleDataTransfer/README.md) | real SINTRAN L bytes (S3MPIT overlay) + bin |

## Live status

For the authoritative, always-current per-call status (this hand-maintained table can
drift), run the reporter:

```
python3 scripts/mon-status-report.py            # markdown; --json for machine-readable
```

Current: of the friend's 31 calls, **27 real SINTRAN L bytes** + **4 documented negatives**
(42B fall-through; 425/426/427 absent from the S3SM5 dispatch), **0 not-started**.

The `GOTAB` dispatch table is byte-verified in
`../../resident/SINTRAN-DATA_commoncode.bin` at `071233B` (indexed `071233B+MON#`);
the odd-MON direct handlers (incl. 15B/45B/51B) live in the **S3RPIT** overlay
(`025-S3IRPIT.bin`). See `../TASK-05-mismatches.md` §G and
`../../EXTRACTING-RESIDENT-CODE.md` §8.
