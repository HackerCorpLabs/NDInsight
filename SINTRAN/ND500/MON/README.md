# MON — SINTRAN III Monitor Calls (hub)

The one place that ties together **all** monitor-call documentation: the ND-100
call system, the ND-500 cross-processor call system, how they map to each other,
and how to find the implementation code for any MON call.

This folder is the **index/hub**. Some authoritative documents remain in their
established locations (to preserve cross-repo links) and are linked from here.

---

## Start here

| Document | What it gives you |
|---|---|
| [ND500-MON-ACTIVATION-AND-MAPPING.md](ND500-MON-ACTIVATION-AND-MAPPING.md) | **The end-to-end process**: how an ND-500 MON call is activated, routed 500↔100, dispatched, handled, and answered — with Mermaid flow and source refs. **Read this first.** |
| ND500-MON-CALL-ROUTING-MAP.md *(pending — being generated)* | The per-call routing table: for every MON number, which actor handles it (ND-500 system monitor / ND-100 driver / ND-500-local) and whether it forwards |

## The two MON-call systems

### A. ND-100 monitor calls (`MON 0..377`, issued by ND-100 programs)
Dispatched by the CPU on **program level 14** through the 256-entry `GOTAB` jump
table.
- [../../OS/23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md](../../OS/23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md) — how dispatch works, reading `GOTAB` in memory, DAP breakpoints, disassembly (level 14 → `GOTAB(n)` → handler).
- Call **definitions** (name, parameters, examples): [../../../Developer/MON/](../../../Developer/MON/) — `Monitor Calls.md` and the per-call YAMLs in `Developer/MON/calls/`.
- Undocumented / unclear ND-100 calls to reverse-engineer: [../../../tools/sintran-segment-carver/ghidra-tasks/TASK-05-undocumented-mon-calls.md](../../../tools/sintran-segment-carver/ghidra-tasks/TASK-05-undocumented-mon-calls.md).

### B. ND-500 monitor calls (issued by ND-500 programs)
Handled first by the **ND-500 System Monitor** (segment `S3SM5`, ND-500 code),
which handles some locally and forwards others to the ND-100 (level 12).
- [ND500-MON-ACTIVATION-AND-MAPPING.md](ND500-MON-ACTIVATION-AND-MAPPING.md) — the full activation/mapping/response process (this folder).
- [../ND500-MONITOR-CALL-MECHANISM.md](../ND500-MONITOR-CALL-MECHANISM.md) — inter-processor call mechanism, message buffer layout.
- [../ND500-MONITOR-CALL-PARAMETER-PASSING.md](../ND500-MONITOR-CALL-PARAMETER-PASSING.md) — message/parameter offsets, `NUMPA` write-back mask, the 500–523 dispatch table.
- [../ND500-MON-RE-FINDINGS.md](../ND500-MON-RE-FINDINGS.md) — `ND-500-MON-J:PROG` background monitor, `MON 60B` (N500M) subfunction array.
- [../swapper/swapper-k01-deep-analysis.md](../swapper/swapper-k01-deep-analysis.md) — proof the **swapper is NOT the MON dispatcher** (it forwards swap work via `MON 377B`).
- ND-500 System Monitor disassembly + how to align symbols: [../../../tools/sintran-segment-carver/versions/L-VSX-500/segments/030-S3SM5-DISASSEMBLY-PROMPT.md](../../../tools/sintran-segment-carver/versions/L-VSX-500/segments/030-S3SM5-DISASSEMBLY-PROMPT.md) and the disassembly `030-S3SM5.asm` in that folder.

## The bridge (ND-100 ↔ ND-500 transport)
- [../ND500-BUS-INTERFACE-REFERENCE.md](../ND500-BUS-INTERFACE-REFERENCE.md) — **the spec**: message mailbox, 3022/5015 registers, level-12 ISR, the `3MONCO`/`3WMONCO` message codes that carry MON calls.

## Kernel NPL source (annotated)
- [../MP-P2-N500.md](../MP-P2-N500.md) — ND-100 N500 driver: level-12 ISR, `MCHANDLE`/`5MONICO`, the `GOSW` for MON 500–523, `NORMMC`.
- [../CC-P2-N500.md](../CC-P2-N500.md), [../RP-P2-N500.md](../RP-P2-N500.md), [../XC-P2-N500.md](../XC-P2-N500.md) — the rest of the N500 driver family.
- ND-100 MON-60 interface source: `../../NPL-SOURCE/NPL/5P-P2-MON60.NPL` (`SEGMONC`, function-code tables).

---

## How to find the code for any MON call (quick guide)

1. **Which system?** ND-100 program → system A (level 14 / `GOTAB`). ND-500
   program → system B (ND-500 System Monitor first).
2. **ND-100 call:** handler = `GOTAB(n)`; follow the method in the [dispatch
   guide](../../OS/23-MON-CALL-DISPATCH-DEVELOPER-GUIDE.md).
3. **ND-500 call:** look it up in the routing map; then either read the named
   ND-100 handler in `MP-P2-N500.NPL` (calls 500–523), or `SEGMONC` on the ND-100
   (calls < 500 standard), or disassemble `030-S3SM5.bin` with `nd500-dis` (calls
   handled ND-500-side). Details: activation doc §5.

## Status of the reverse-engineering effort
- **ND-500 MON calls (410–427, 500–523):** routing established; ND-100-side
  handlers (500–523) are named and readable in `MP-P2-N500.NPL`; the ND-500-side
  handlers are in `030-S3SM5.asm` (RE in progress).
- **ND-100 MON calls (0–377):** dispatch mechanism fully documented (doc 23);
  per-handler RE tracked in TASK-05 (handlers live in the paged level-4 view — via
  live DAP recovery or composite-view reconstruction).
