# MON 65B (octal) - ErrorMessage (QERMS)

Displays a **file-system error message** for the input error number (appendix A lists the message
for each number) and then **terminates the program**. The message is written to the terminal; RT
programs write it to the error device (normally the console). Error number `0` is illegal. This is
an ND-100 monitor call and the **sibling** of [MON 64B WarningMessage](../64B-WarningMessage/README.md):
they share ONE worker body with two entry points - `ERMSG=16714B` (T:=0, warning, program continues)
and `QERMS=16716B` (T:=1, error, program terminates).

**Status:** `partial`. GOTAB dispatch head byte-proven (`GOTAB[65B] = 121345B`, the `F1641` level-14
stub in `025-S3IRPIT`); the `QERMS`/`ERMSG` worker body is real SINTRAN L bytes (executable code,
body `16714B-17020B` closing at `EXIT`) with the two entry points above, and the exact
`MON 65 -> worker` link crosses an uncarved kernel bridge (see [Honest caveats](#honest-caveats)).
All addresses/values are **octal**.

- **Full disassembly:** [`65B-ErrorMessage.ASM`](65B-ErrorMessage.ASM) - both regions (the `F1641`
  entry stub + the shared `QERMS`/`ERMSG` worker body, entered at `QERMS` for MON 65B).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 65B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[65B] = 121345B<br/>(byte-proven)"]
    C --> D["F1641 entry stub<br/>025-S3IRPIT :121345B"]
    D -.uncarved CALLPROC.-> E["QERMS worker (T:=1)<br/>commoncode :16716B (real code)"]
    E --> F["format message for ErrNumber<br/>-> terminal / error device, terminate"]
    class A blue
    class B,C,D teal
    class E green
    class F green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

`GOTAB[65]` = `121345B` = `F1641`, an F16xx level-14 stub. Its own edges stay inside `025-S3IRPIT`
(`121345 JPL I 47 -> 121414`, `121346 JMP 34 -> 121402`, shared F16xx family code). The dashed hop
(`D ⇢ E`) is the resident `CALLPROC`/segment-switch to the shared commoncode worker - it is **not
present in any carved segment**, so it is the one link that cannot be followed statically. `QERMS`
(E) is the named commoncode entry (T:=1) for this call and **is** real executable code, shared with
MON 64B (`ERMSG`, T:=0).

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2; for
commoncode (load base `0`) the byte offset is simply `octal-addr x 2` (decimal).

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[65] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071320B` (1 word) | 58784 | `GOTAB+65` = `121345B` | **VERIFIED** |
| F1641 entry stub | [025-S3IRPIT.asm](../../segments-ref/025-S3IRPIT/025-S3IRPIT.asm) · [.hex](../../segments-ref/025-S3IRPIT/025-S3IRPIT.hex) | `121345B-121366B` (18w, to P36R2=121367) | 56778 | `F1641` | **VERIFIED** (bytes) |
| resident CALLPROC bridge | - (uncarved) | - | - | `CALLPROC` | **UNVERIFIED** |
| QERMS/ERMSG worker body | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) · [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `16714B-17037B` (body 16714-17020 + link cells) | 15256 | `QERMS` / `ERMSG` | real bytes = **CODE**; body link **MISATTRIBUTED** |

**Verify by hand:** the GOTAB slot:
`grep '^71320 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71320  121345  ...  58784`; then
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58784 count=2 2>/dev/null | od -An -tx1`
-> `a2 e5` (the stored word = octal `121345`, `F1641`). For the `F1641` stub head:
`dd if=../../../segments/025-S3IRPIT.bin bs=1 skip=56778 count=4 2>/dev/null | od -An -tx1`
-> `ba 27 a8 1c` (= octal `135047 124034` = `JPL I 47` / `JMP 34`, the stub entry). For the `QERMS`
entry word:
`dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=15260 count=2 2>/dev/null | od -An -tx1`
-> `f2 01` (= octal `171001`, `SAT 1` - the QERMS entry sets T:=1, confirming the region is code).
`prove-mon.py 65` reports the same `GOTAB[65]=121345` -> `F1641`.

---

## Instruction walkthrough

Full listing: [`65B-ErrorMessage.ASM`](65B-ErrorMessage.ASM). Two regions.

**Region A - F1641 stub (`121345-121366`)** is the level-14 entry pointed at by `GOTAB[65]`. The
entry proper is two words: `121345 JPL I 47 -> 121414` (call a shared F16xx routine) and
`121346 JMP 34 -> 121402` (jump into shared F16xx family tail code). Both targets stay inside
`025-S3IRPIT`. Words `121347-121366` (`STATX` physical stores, `,B 17`/`,B 46` counter updates) are
shared continuation reached through those family routines, not from the F1641 entry directly. The
stub does **not** itself reach the `QERMS` worker in commoncode - that transfer is the uncarved
`CALLPROC` hop.

**Region B - QERMS/ERMSG worker (`16714-17037`)** is the shared error/warning message body.

- **Dual entry (`16714-16717`)** - `ERMSG=16714B` sets `T:=0` (`RADD CLD 0 DT`, MON 64B warning
  flavour) then `JMP 2 -> 16717`; the secondary entry `QERMS=16716B` sets `T:=1` (`SAT 1`, MON 65B
  error flavour) and falls into the same merge point `16717`. MON 65B enters at `QERMS` (T=1).
- **Store parameters (`16717-16746`)** - the flavour flag and the caller error number are stored
  through pointer cells (`16717 STT I 102`, `16722 STT I 101`, `16723 STA I 101`). `JAF`/`JAZ` tests
  then choose the message source/format path, calling the format/lookup workers via indirect `JPL I`
  through the trailing link cells (`16731 JPL I 76 -> 17027`, `16737 JPL I 72 -> 17031`, ...).
- **Format + output + return (`16747-17020`)** - `16750 JPL I 63` / `16753 JPL I 62` format the
  message text; the `16766-17011` block (`JAZ` tests, `STATX`) routes output to the correct device;
  the routine restores context and returns at `17020 EXIT`. Words `17021-17037` are pointer/constant
  cells (link cells), not code.

---

## Parameter / register contract

Manual-side names/types are from [`65B_ErrorMessage.yaml`](../../../../../../../Developer/MON/calls/65B_ErrorMessage.yaml).

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| `A` (ErrNumber) | in | error number of the message to display; **0 is illegal** (`MAC` example `LDA ERRNO`) | inferred (manual); stored by `16723 STA I 101` (VERIFIED as a copy) |
| `T` (flavour) | internal | `1` at `QERMS` (MON 65B, error/terminate); `0` at `ERMSG` (MON 64B, warning/continue) | **VERIFIED** (bytes: `16716 SAT 1` / `16714 RADD CLD 0 DT`) |
| output device | out | terminal (interactive) or error device (RT) | inferred (manual) |
| return | out | program **terminates** (ErrorMessage) - vs MON 64B which continues | inferred (manual); the carved tail returns via `17020 EXIT` for both entries |

The worker's register staging (the dual-entry flag, the pointer-cell parameter stores, the `EXIT`
frame return) is VERIFIED from bytes; the mapping onto the user-visible error-number-in contract, the
terminate-vs-continue policy, and the device routing live in the caller-side `MON 65` wrapper and the
uncarved `CALLPROC` frame, so those rows are **inferred**, not byte-proven here.

---

## Pseudo-code (for an emulator)

See **[`65B-ErrorMessage.pseudo.c`](65B-ErrorMessage.pseudo.c)** - a pseudo-C model of both the
`F1641` stub and the shared `QERMS`/`ERMSG` worker (entered at `QERMS`, T:=1). Control flow (dual
entry, parameter stores, format/output, `EXIT`) is byte-verified; the output routing, the appendix-A
message table, and the error-terminate policy are inferred from the manual. The stub-to-worker
`CALLPROC` bridge is modelled but not proven. Every ND-100 instruction in the model is translated per
the canonical [`ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md)
(`RADD CLD 0 DT` = `T := 0`; `SAT 1` = `T := 1`; `JPL I disp` = call through link cell at `P+disp`;
`STATX` = physical store `phys[EL] = A`).

---

## Honest caveats

**What is byte-proven:** `GOTAB[65B] = 121345B` = `F1641` (level-14 dispatch). The `F1641` stub at
`121345B` in `025-S3IRPIT` is real code (entry `135047 124034` = `JPL I 47` / `JMP 34`). The
`QERMS`/`ERMSG` worker body at `16714B` is real code - the `QERMS` entry word `171001` is a genuine
`SAT 1`, the body (`16714-17020`, bounded by `SEGTX=17040B`) has clean, in-region control flow and
returns via `EXIT`, and it carries two entry points `ERMSG` (T=0) / `QERMS=16716B` (T=1).

**What is NOT proven:** the link from the `F1641` stub (in `025-S3IRPIT`) to the `QERMS` worker (in
commoncode). The stub's own direct branches stay in `025-S3IRPIT` (to shared F16xx tail code at
`121402`/`121414`); the stub->worker transfer is the resident `CALLPROC`/segment switch in an
**uncarved overlay**. So the `MON 65 -> QERMS` attribution rests on the `QERMS` symbol name (= the
`65B` short name in the manual) + the dual-entry `ERMSG`/`QERMS` shape + the message behaviour, not a
followed pointer - hence **MISATTRIBUTED** in the strict sense.

**The 64B/65B relationship (byte-honest):** MON 64B WarningMessage and MON 65B ErrorMessage are one
shared worker with two entries - `ERMSG=16714B` (T=0) and `QERMS=16716B` (T=1) - that merge at
`16717B`. The flavour is stored (`16717 STT I 102`) and later steers the message/return behaviour;
the manual says WarningMessage continues while ErrorMessage terminates, so T=1 (`QERMS`) is the
terminate flavour. That continue-vs-terminate split is **inferred** from the manual, not isolated in
this window (the visible tail returns via `EXIT` for both entries). One asymmetry is byte-real:
MON 64B is a `GOTAB[64]=0` fall-through while MON 65B has a real `GOTAB[65]=F1641` stub - the two
sibling calls enter the shared body by different dispatch routes.

Confirming the link needs a live trace: issue a real `MON 65`, break at `121345`, single-step the
segment switch, and confirm P lands on `QERMS=16716`.

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) · dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) · master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
