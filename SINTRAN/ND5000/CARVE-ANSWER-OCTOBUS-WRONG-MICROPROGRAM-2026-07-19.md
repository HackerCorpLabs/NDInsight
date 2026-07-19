# Carve answer: "Microprogram error: Wrong microprogram" over the octobus (2026-07-19)

**Full path:** `E:\Dev\Ronny\NDInsight\SINTRAN\ND5000\CARVE-ANSWER-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md`
**Answers:** `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-OCTOBUS-WRONG-MICROPROGRAM-2026-07-19.md`
**Builds on (do not re-derive):** CARVE-ANSWER-OCTOBUS-CPU-PRESENCE / -ACCP-SELFTEST-CSLOAD /
-CSLOAD-MICROSTART (all 2026-07-18, this folder).
**Evidence grade:** **[V]** byte/manual/symbol-cited · **[I]** inferred · **[OPEN]** not carved.

---

## VERDICT

**Ronny's hypothesis is REFUTED in its mechanism, CONFIRMED in its data.** SINTRAN/MON does **not**
read the CS-file header and compare it to a CPU-reported value. "Wrong microprogram" is **not** a
SINTRAN-side decision at all.

**"Microprogram error: Wrong microprogram" is an OCB error message that the ND-5000 side SENDS to the
ND-100.** It is produced by the **ND-5000 microcode's `CPU_READ` routine** (`MICRO-5800-B30.md`
`@017130`), which runs during microprogram INIT. `CPU_READ` compares, entirely inside the ND-5000:

1. **CPU MODEL** — the packed type/model byte the ACCP/MF-backplane reports (`TYP,HW`, "Current
   CPUModel") against the model baked into the **loaded control-store word 7** (`CPUMODEL`, "My
   CPUModel"). Mismatch → **OCB fault 203B (Fatal)**. [V]
2. **VERSION** — the control-store **word-1 version constant** against a **per-model allowed
   [min..max] range**. Out of range → OCB fault 203B / 202B. [V]

On failure the microcode builds an octobus multibyte error message (`CPU_MESSAGE @017301 → TRAP_OCBM`)
with FaultType 202B/203B, ErrorReporter = 1 (MicroProgram), body `{Current CPUModel, My CPUModel,
MicroprogramVersion}`, addressed to the ErrorStation/HostStation. SINTRAN's **`5OMBREAD`**
(`MP-P2-N500.NPL:3459`) receives it, splits `ETYPE` into FaultType (`CSTS`) + ErrorReporter (`CMICP`),
and hands the record to `9FLER`, which prints "Microprogram error:" (reporter = MicroProgram) +
"Wrong microprogram" (fault 202/203). **SINTRAN performs no version compare; it only decodes and
prints what the ND-5000 sent.** [V]

**Why it is "pre-load" and invisible in the ACCP-command frame trace:** this OCB message travels
**ND-5000 → ND-100** (from station 70B-73B `FN5DEST..LN5DEST`, to the error/host OMD), i.e. it is an
**inbound** octobus multibyte, not one of the OMD-3 ACCP commands SINTRAN *sends*. A trace that logs
only outbound ACCP commands shows nothing after VPARP even though a frame did arrive. **So the emulator
side is emitting this OCB fault** (its servicer / emulated ND-5000 is the only thing that can produce
the string). Fix = stop emitting it: make the reported model and the loaded-image model/version agree,
or suppress the fault. [V mechanism; [OPEN] exact emitter in current build — see §6]

---

## Q1 — Where "Wrong microprogram" is decided, and the two compared values

**Decided in the ND-5000 microcode, not in J04 and not in SINTRAN.** The string is **not** in the J04
message pool (`nd-500-mon-j04.prog.md:1462-1477` lists the CS-load siblings
`$CONTROL STORE NOT SUCCESSFULLY LOADED` etc. — "Wrong microprogram" is absent) and **not** in the
MON60 NPL. It exists only as an **OCB FaultType** (manual + microcode). [V]

**Manual definition** (`ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md:10933-10934, 10987-10992`):
```
202B (82h) = Wrong microprogram : NotFatal 6 Bytes
203B (83h) = Wrong microprogram : Fatal    6 Bytes
MessageBody  Current CPUModel      : 1 Byte
             My CPUModel           : 1 Byte
             MicroprogramVersion   : 2 Bytes
<ErrorReporter> 1 = MicroProgram
```
Cross-checked in `MP-P2-N500.md:1932` (`5OMBREAD` handles "5000-MP: Microprogram errors").

**Microcode site** (`E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md`), the `CPU_READ` model/version
validator called from INIT (`@014574`):
- `CPU_READ @017130` reads word-7 `CPUMODEL`, extracts the model index, jumps through
  `CPU_MODEL00-17` (`017143-017162`) to a per-model handler `CPUMODxx`.
- **Model check** (example, model 8 = 5800, `CPUMOD08 @017240`):
  `017241 ALU,XOR TYP,HW A,SARG SARG=000070 B,SC4` then
  `017242 … SEQ … INVSEQ → CPU_UNAVA1`. I.e. `if (TYP,HW != 0o70) goto CPU_UNAVA1`. `0o70 = 0x38` =
  the packed 5800 byte. Same pattern for every model (model 7 → `SARG=000047 @017231`, model 5 →
  `SARG=000045 @017221`, model 4 → `SARG=000044 @017205`, …). [V]
- **Version-range check** `CPUVERS28 @017260-017265`: reads the word-1 version, does
  `version - min` / `min? - max` comparisons; out-of-range → `CPU_UNAVA1`. The per-model ranges are the
  `VERSIONxx` tables `017314-017325` (model 8 = `VERSION3 @017324`: `SARG=027174..027337` =
  `0x2E7C..0x2EDF`). [V]
- **Fault emission:** `CPU_UNAVA1 @017276 ALU,XOR A,SARG SARG=000203 …` (= fault **203B**);
  `CPU_AVAIL @017266 … SARG=000202 …` (= fault **202B**) → `ADR_CPUAVA` → `CPU_MESSAGE @017301` builds
  the OCB body from `CPUPAR`/`CPUMODEL`/`VERSION` → `TRAP_OCBM` sends it. [V]

**The two compared values:** (a) `TYP,HW` = model reported by the **ACCP/MF-backplane** ("Current
CPUModel") vs word-7 `CPUMODEL` of the **loaded CS** ("My CPUModel"); and (b) word-1 **version** vs the
loaded image's per-model **range**.

---

## Q2 — The "CPU-reported" side of the comparison

**It is NOT the `3RMICV` mailbox reply, NOT an octobus ACCP command SINTRAN issues, and NOT a value
SINTRAN cached.** The comparison happens **inside the ND-5000 microcode**; the "CPU-reported model" is
read there by the microcode via `TYP,HW` / the ACCP path (`ACCP_READ`, `CPU_READ @017130-017142`), i.e.
the **MF-backplane EEPROM STATION/`SET-CPU-MODEL`** value the ACCP presents. [V]

`3RMICV` (mailbox MICFU 1, `MSG_VERSRD @015330`) is a *separate*, later, read-only query: it returns
`version = 0o27232` (word-1 constant, `015331→VERSION`) + `cpupar = srf[0o2015]` (the CPUPAR that
`CPU_READ` already composed) into the message. It is what MON60 `057B RMVER` / `170B READ-CPU-TYPE`
display; **it does not gate "Wrong microprogram"** and it is answered over shared memory, not the ACCP
command channel. The gating comparison already happened in `CPU_READ` before `3RMICV` is ever serviced.
[V]

What SINTRAN actually sees on the wire for the failure is the **inbound OCB error frame** decoded by
`5OMBREAD` (`MP-P2-N500.NPL:3459-3511`): `X.ETYPE SHZ -10 → CSTS` (FaultType 202/203),
`ETYPE /\ 377 → CMICP` (ErrorReporter; `CMICP=1` = microprogram), record → `9FLER` (printer). The
station range test `MOCTSTATION >= FN5DEST AND <= LN5DEST` (70B-73B) confirms it is an ND-5000-sourced
message. [V]

---

## Q3 — The "CS-file" side

**"My CPUModel" = word 7 (`CPUMODEL`, ustore addr `000007`) of the control-store image the ND-5000
loaded; "MicroprogramVersion" = word 1 (`LARG`, ustore addr `000001`).** [V]

Byte layout in the 128-bit-word `.DATA` image (`docs\MC\README.md:54-98` [V, 4-image diff]):
- **word 1 → version** = the `LARG` operand, low 16 bits of the 128-bit word (byte offsets 14-15).
  5800-B30 = `0x2E9A` = `027232B`.
- **word 7 → CPUMODEL** = packed `(CPU_type<<4)|model_digit` in the last byte: 5200=`0x12`,
  5500=`0x25`, 5700=`0x27`, **5800=`0x38`**.

**Important:** MON/SINTRAN does **not** parse this file header itself. The bytes reach the comparison
because the **microcode reads its own loaded store** (`CPU_READ` reads `CPUMODEL`; `VERSION` reads the
word-1 constant). Ronny's "MON reads the CS file and compares" is the right *pair of fields* (word 7
model, word 1 version) but the wrong *actor* — the ND-5000 microcode does it, and only reports the
result. There is no ND-100-side CS-header parse in this path. [V]

---

## Q4 / Q5 — Is it model, version, or both — and what the emulator must return

**Both.** `CPU_READ` requires **model match** (word-7 vs ACCP `TYP,HW`) **AND** **version in the
per-model range** (word-1). Either failure → fault 202B/203B → "Wrong microprogram". So a per-model
CPUPAR alone is *not* sufficient; the version must also sit in that model's window. [V]

### Concrete numbers for the modelled 5800-B30 (the corpus export image)

| Field | Value | Where |
|---|---|---|
| word 7 `CPUMODEL` (My CPUModel) | `0x38` (type 3, model 8) | CS image word 7 |
| ACCP/EEPROM model (`TYP,HW`, Current CPUModel) | **must also be `0x38` (`0o70`)** | microcode check `017241` `SARG=000070` |
| word 1 version | `0x2E9A` = `027232B` | CS image word 1 |
| model-8 version range | `0x2E7C..0x2EDF` (`027174..027337`) | `VERSION3 @017324`; `027232B` is IN range ✓ |
| CPUPAR (`srf 0o2015`, 3RMICV HW `0o10`) | `001741B` for model 8 | `CPUMOD08 @017243` `SARG=001741` → `CPUSAVE` |
| 3RMICV version (HW `0o7`) | `027232B` | `MSG_VERSRD @015331` |

### Bottom line for the emulator

The failure is **the emulated ND-5000 sending OCB fault 202B/203B to the ND-100.** To pass:

1. **Make "Current CPUModel" == "My CPUModel".** Whatever model byte the emulated ACCP/servicer
   reports as the hardware CPU type MUST equal word 7 of the CS image it models as loaded. For a 5800:
   both = **`0x38`**. If the servicer reports a different `CpuParameter`/model than the image implies,
   that is the mismatch. [V]
2. **Keep the version in the model's range.** MicroVersion `027232B` is already inside the model-8
   window `[027174..027337]`, so the default is fine **for a 5800 image**. If you switch the reported
   model, switch the version to that model's range too (5200 `026354-026517`-class, etc. — the
   `VERSIONxx` tables `017314-017325`). [V]
3. **Simplest robust fix:** the emulator's servicer should **never emit the 202B/203B "Wrong
   microprogram" OCB** (i.e. model its `CPU_READ` as always-available for the configured model). Since
   the emulator owns both the reported model and the "loaded" image, it can simply treat CPU_READ as
   passing and answer `3RMICV` with `{version 027232B, CPUPAR 001741B}`. [I — safe because the check is
   entirely ND-5000-internal; SINTRAN only prints what it receives]

Per Ronny's "echo it from the image" idea: yes — drive both the reported model and the `3RMICV`
version/CPUPAR from the **csStore word 7 / word 1** the emulator holds, so they are self-consistent by
construction and no OCB fault is ever generated.

---

## 6. Honest [OPEN] items

- **The exact emitter in the current build.** The carve proves the *only* code path that prints
  "Microprogram error: Wrong microprogram" is `5OMBREAD` decoding an inbound OCB FaultType 202/203, and
  the *only* real-hardware source of that OCB is microcode `CPU_READ`. It follows that the emulated
  ND-5000/servicer is sending (or leaving in the octobus ring) a frame `5OMBREAD` reads as
  FaultType 202/203, ErrorReporter 1. **Which servicer path emits it (an explicit CPU_READ model, a
  default "unavailable", or stale ring bytes misread as a fault) is not confirmed from the emulator
  source in this carve** — verify against `Nd500MicrocodeServicer` / the octobus station send path, and
  ideally capture the inbound multibyte from station 70B. [OPEN]
- **`9FLER` string table.** The literal binding FaultType 203 → the printed words "Wrong microprogram"
  is [I] (manual + `5OMBREAD` decode); `9FLER`'s message pool was not dumped to bytes. The FaultType
  and ErrorReporter *decode* in `5OMBREAD` is [V] (`MP-P2-N500.NPL:3464, 3483`).
- **CPUPAR = `001741B` for model 8** is read from `CPUMOD08 @017243 SARG=001741`; that it is the exact
  byte SINTRAN caches/needs is [I strong], not independently confirmed against a live 3RMICV capture.
- **Per-`VERSIONxx`→model mapping** beyond model 8 (`VERSION3`) is inferred from the ascending ranges
  and the `CPUMODxx` call sites; only the model-8 path was traced end-to-end. [I]
- **`202B` (NotFatal) vs `203B` (Fatal) selection** — `CPU_AVAIL @017266` uses 202B, `CPU_UNAVA1
  @017276` uses 203B; which branch a given mismatch takes was not fully traced (model-mismatch → 203B
  is the firm one). The user's symptom text does not distinguish them. [I]

---
*Sources: `MICRO-5800-B30.md` (`CPU_READ @017130`, `CPU_MODEL00-17`, `CPUMOD08 @017240`, `CPUVERS28
@017260`, `VERSION3 @017324`, `CPU_UNAVA1 @017276` SARG=000203, `CPU_AVAIL @017266` SARG=000202,
`CPU_MESSAGE @017301`); `docs\MC\README.md:54-98`; `ND-05.017.01 EN ND-5000 HARDWARE
MAINTENANCE.md:10930-11007`; `MP-P2-N500.NPL:3459-3511` (`5OMBREAD`), `:1204-1216` (3RMICV watchdog,
nd5000-skipped), `:1932`; `MAILBOX-MICROCODE-PSEUDOCODE.md` (`MSG_VERSRD @015330`); `5P-P2-MON60.NPL`
(`FRMICV`/`FGCPUTYPE`); prior 2026-07-18 carves.*
