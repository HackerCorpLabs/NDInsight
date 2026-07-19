# SINTRAN III reverse-engineering — master plan

Full path: `tools/sintran-segment-carver/RE-MASTER-PLAN.md`

Single view of everything for this RE effort — what is done, what is left, in
phases with per-item todos. Detailed MON-call method lives in
[ghidra-tasks/TASK-05-PLAN.md](ghidra-tasks/TASK-05-PLAN.md); this file is the
umbrella.

Status: `[x]` done · `[~]` in progress / partial · `[ ]` not started · `[blocked]`.

---

## Phase 0 — Foundations (DONE)

- [x] Segment carver corrected: full-length `SEGLE*1024`-word carves from extracted
      `SEGFIL0` at `MADR*2048`; K05 / L07 / M06 (L was also mis-based before).
- [x] Per-segment metadata sidecars (`NNN-<NAME>.meta.json`) + manifest, with the
      Ghidra base address in **hex**.
- [x] Resident code/data extracted per version (`SINTRAN-DATA_commoncode.bin`,
      `MACM-AREA-DATA_rtloader.bin`).
- [x] Docs consolidated to two: `EXTRACTING-SEGMENTS.md`,
      `EXTRACTING-RESIDENT-CODE.md`; per-version catalogs regenerated.
- [x] Password fold solved **statically**: `acc = ROL16(acc,3) + toupper(char)`
      (see `versions/L-VSX-500/re/PASSWORD-ALGORITHM.md`).
- [x] `nd500-dis` SEG false-positive fixed (descriptor sanity check) so SINTRAN
      ND-500 segments disassemble as RAW.
- [x] **Wave 5** S3SM5 routine map: numeric MON dispatch (0x60 vector), the
      `@HELP` interactive command table, and YAML alignment
      (`versions/L-VSX-500/re/030-S3SM5-routine-map.md`).

---

## Phase 1 — MON-call semantics (TASK-05) — core deliverable

Method + addresses: [ghidra-tasks/TASK-05-PLAN.md](ghidra-tasks/TASK-05-PLAN.md).

### 1.1 Wave 1 — read handler bodies from NPL source (DONE)
Results: [versions/L-VSX-500/re/TASK-05-results.md](versions/L-VSX-500/re/TASK-05-results.md).
- [x] `DVIO` (511), `A5XMSG`/`B5XMSG` (512/513), `5MTRANS` (515) — bodies in
      `MP-P2-N500.NPL` (1689 / 2076 / 2441): parameter contracts extracted
      (ND-500 message offsets + write-back mask). **3 showstoppers cleared.**
- [x] 500-series GOSW handlers `STAPROC`/`NSTOPROC`/`GERRC`/`SWMC` (500/501/505/510).
- [x] In-source ND-100 handlers: `IBRSIZ` (313), `BRPNT`/`DEBUGGER` (45/51),
      `MEXIT` (132), `GDEVTY`; plus `GOTAB`/`ENT14` level-14 dispatch verified.
- [x] Reconcile friend's names: 505=`GERRC` "get error code" (not GetTrapReason),
      510=`SWMC` "swapper monitor call" (not switch-context), 512/513=
      `A5XMSG`/`B5XMSG` = one shared XMSG mechanism (A/B = caller buffer convention).

### 1.2 Wave 2 — the 410–427 handlers (DONE for source scope; contracts → Phase 2)
Results: [versions/L-VSX-500/re/TASK-05-results.md](versions/L-VSX-500/re/TASK-05-results.md) Wave-2 section.
- [x] Routing VERIFIED: `MCHANDEL` (`MP-P2-N500.NPL:1286`) → `NORMMC` (`:1277`) →
      `5RRTWT` (`:24`); reverse leg `FSYSINTERFACE` (`CC-P2-N500.NPL:394`).
- [x] Located: **410–427 packaging bodies are carved-only in S3SM5** (NOT in NPL);
      native back-ends `MOFIX`/`MUNFIX`/`WSEG` in `RP-P2-SEGADM.NPL:248/297/985`.
- [x] Open item flagged: **425/426/427** (sprname/gprnum/gprname) NOT LOCATED —
      empty S3SM5 vector slots, absent from NPL.
- [blocked→Phase 2] Message-offset parameter contracts (fixseg/unfix/wsegn/mxpisg…)
      need S3SM5 disassembly. **NOTE:** the routine-map offsets `0xBAE1`/`0xBB38`/…
      do **not** decode as code (`nd500-dis -s BAE1` → ASCII text); the `0x60`
      vector-table seeds were `0x8bae`-range, so the offsets must be re-derived from
      the vector table first. See Phase 2.2.

### 1.3 Wave 3 — ND-100 file-system calls not in source
- [ ] Re-test the corrected carves first (old "not in any .bin" verdict was on the
      mis-based L bytes): 5 RDISK, 6 WDISK, 67 OSIZE, 74 SETBT, 75 REABT, 120 WFILE,
      144 MAGTP, 327 FSMTY.
- [ ] If still absent → live DAP at the handler symbol addresses; disassemble.
- [ ] Dump the function-code tables for MAGTP (144) and FSMTY (327).

### 1.4 Wave 4 — TSS carryovers + odds
- [ ] 13/14/15, 42, 51, 304 (SIBAS, low) — read `GOTAB[n]` (in source,
      `MP-P2-2.NPL:184`) → handler → body-from-source or DAP.

### 1.5 Consolidate
- [ ] Write `versions/L-VSX-500/re/TASK-05-results.md` (VERIFIED vs UNCERTAIN per call).
- [ ] Draft the **missing YAMLs**: 510, 511 (DVIO), 512, 513, 515 (5MTRANS); update
      existing YAMLs where the body disagrees.
- [ ] Answer the friend's specific yes/no questions (TASK-05 §A).

---

## Phase 2 — ND-500 monitor deep disassembly (extends Wave 5)

- [ ] 2.1 Full routine-body disassembly of S3SM5 seeded from the 0x60 vector table
      (better framing than the linear pass; resolve the string-as-opcode noise).
- [ ] 2.2 Verify the MON parameter contracts against the S3SM5 handler bodies
      (resolve the UNCERTAIN flags in the routine map). **First reconcile the
      offsets:** `nd500-dis -s BAE1` returns ASCII text, so the routine-map hex
      offsets (410=`0xBAE1`…) are wrong/mislabelled; re-derive true file offsets
      from the `0x60` vector table (seeds `0x8bae 0x8bb5 0x8bf1 …`) before reading
      contracts. Then extract fixseg/unfix/wsegn/mxpisg/420/421 message offsets and
      search for the 425/426/427 servicing code.
- [ ] 2.3 Cross-check the `S3SSM5` save copy; disassemble the M06 ND-5000 monitor
      equivalent.

---

## Phase 3 — ND-120 microcode version (task #7)

- [blocked] 3.1 Determine the on-disk format of the `S3IU120`/`S3SU120` segments
      (raw WCS 7b80 signature was absent; format unknown).
- [ ] 3.2 Decode the version = low 8 bits of the 64-bit microword at octal 020
      (K=013, L=014) once the format is known.

---

## Phase 4 — Housekeeping & coordination

- [ ] 4.1 Update the now-stale `versions/L-VSX-500/segments/030-S3SM5-DISASSEMBLY-PROMPT.md`
      (it says the disassembly is infeasible — it is now cracked).
- [ ] 4.2 Commit the new tool work (routine map, plans, resident READMEs, metadata
      sidecars regen) — the last commit predates them.
- [ ] 4.3 `nd500-dis` fix: commit in the `pcc-nd500` repo on a branch (pending your OK).
- [~] 4.4 Password RE write-up — owned by the Ghidra side; coordinate so
      `README-password-login.md` supersede banner and `PASSWORD-ALGORITHM.md` stay
      consistent.

---

## Phase 5 — Hardware / protocol interface RE (user priority: AFTER the MON calls)

Ordered by the user's stated priority. These are physical-interface / controller
reverse-engineering targets, source-first from the NPL drivers.

### 5.1 ND-500 bus / Octobus interface (task #15) — first of the hardware set
- [ ] MON 60 `ND500Function` subfunctions (`5P-P2-MON60.NPL`); IOX device registers
      + bit fields; message/mailbox mechanism (`5MBBANK`/`N5MESSAGE`, `ITO500XQ`/
      `IFM500XQ`); DMA + address conversion (`N500A`/`N100A`/`CNVWADR`); interrupt/
      activation path; Octobus physical notes. Drivers: `MP-P2-N500`, `RP-P2-N500`,
      `CC-P2-N500`, `XC-P2-N500`, `MP-P2-PIOC-DRIV`, `RP-P2-PIOC`.
- Output doc under `SINTRAN/ND500/` or `SINTRAN/Devices/Octobus/`.

### 5.2 SCSI / floppy / CD-ROM drivers (task #16)
- [ ] `IP-P2-SCSI-DISK/DRIV/MAGTP/OPDI.NPL` (OPDI = optical/CD-ROM) + floppy path:
      SCSI command interface, device registers, DMA. Cross-ref `SINTRAN/Devices/SCSI`.

### 5.3 XMSG / HDLC / Ethernet protocols + controllers (task #17) — LOWEST priority
- [ ] XMSG wire protocol already has a hub in `SINTRAN/XMSG/`; extend with the
      ND-500 XMSG service side (512/513 A5XMSG/B5XMSG) and the controller-level RE.
- [ ] HDLC controller + framing (`MP-P2-HDLC-DRIV.NPL`, `SINTRAN/Devices/HDLC`).
- [ ] Ethernet controller + protocol stack. Goal: reverse-engineer the protocols
      and the physical controllers.

---

## Phase 6 — Comprehensive per-MON-call documentation (initiative)

Goal: document **every** MON call — not just a YAML — with a uniform bundle:
- [ ] **Assembly code** — the extracted handler body (NPL source excerpt and/or the
      carved/disassembled bytes) per call.
- [ ] **Metadata** — the YAML (octal, name, short names, params, compatibility,
      source provenance) — already the `Developer/MON/calls/*.yaml` format.
- [ ] **Analysis data** — parameter contract, register/message-offset map, subfunction
      tables, return/write-back, VERIFIED/UNCERTAIN tags.
- [ ] **Markdown write-up per call** with **Mermaid** diagrams: dispatch-path flow
      (level-14 GOTAB / level-12 GOSW → handler), call/return sequence, and any
      subfunction decision tree. WCAG 2.1 AA palette per `MERMAID_COLOR_STANDARDS.md`.
- [ ] Decide the layout (e.g. `Developer/MON/analysis/<octal>-<Name>/` bundling
      `handler.asm`, the YAML link, `analysis.md`, `flow.md`) and a generator/index.

This is a large, ongoing effort layered on top of the wave results; it runs behind
Phases 1 and 5 and consumes their verified contracts.

---

## Priority stack (user-stated, 2026-07-10)

1. **The friend's MON calls** (Phase 1 + the whole §2.14 YAML set) — IN PROGRESS.
2. ND-500 bus / Octobus interface (Phase 5.1).
3. SCSI / floppy / CD-ROM drivers (Phase 5.2).
4. XMSG / HDLC / Ethernet protocols + controllers (Phase 5.3) — after all others.
- Cross-cutting: Phase 6 comprehensive documentation, applied as calls are verified.

## Critical path / recommended order

1. **Phase 1.1 (Wave 1)** — DONE; cleared the 3 showstoppers.
2. **Phase 1.2 (Wave 2)** — DONE (routing/location; contracts → Phase 2).
3. **§2.14 full YAML RE** (batches A–D) + Waves 3/4 — IN PROGRESS; the friend's
   priority-#1 deliverable.
4. Phase 1.5 — fold results into the YAMLs + `TASK-05-results.md`.
5. Phase 5 hardware RE in the stated order; Phase 6 documentation layered on top;
   Phase 2 deep disasm and Phase 3 (blocked) as capacity allows.
