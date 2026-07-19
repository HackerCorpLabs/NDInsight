# ND-5000 Family / Models — Reference & Emulation Cross-Check

**Date compiled:** 2026-07-18
**Primary source:** community wiki summary of the ND-5000 family (grade **[DOC-wiki]** —
secondary/community, NOT byte-verified), itself citing:
- ND-05.009.3 NORD-500 REFERENCE MANUAL
- ND-05.020.1 ND-5000 Hardware Description
- ND-05.017.01 ND-5000 Hardware Maintenance
- PRODUCT NEWS November 1987

**Purpose:** capture the family/model structure and — importantly — flag where it
CROSS-CONFIRMS or reconciles with our independently CARVED findings ([V]/[NPL-V]) in the
octobus/microcode/servicer work. Grades: **[V]** carve-verified by us; **[DOC-wiki]** from
this secondary source; **[D]** our inference; **[?]** open/unreconciled.

---

## 1. What matters for the emulation (the high-value cross-confirmations)

### 1.1 "Samson" = the ND-5000 CPU; "Delilah" = the ND-120 I/O side  [DOC-wiki, matches our usage]
The ND-5000 uses an ND-100/110/120 CPU as its I/O processor. The ND-5000 CPU line is named
**Samson** (sold as ND-5200/5400/5500/5700/5800); the ND-120 I/O-side line is named
**Delilah**. A separate later processor, **Rallar** (ND-5830/5850), is two VLSI gate arrays
**KUSK** ("jockey") + **GAMP** ("horse").
- CONNECTION: we have been calling the 5000 CPU path "SAMSON" throughout the servicer work
  (OnStartProcessSamson, Samson5800 generation, `MICRO-5800-B30`). This pins that name to
  the official hardware name. Rallar (5830/5850) is a DIFFERENT processor from Samson — our
  microcode/servicer work targets Samson (ND-5800/B30), NOT Rallar. [D: Rallar likely needs
  its own microcode; out of current scope.]

### 1.2 The four ND-5000 CPUs are octobus stations 70B–73B  [DOC-wiki CONFIRMS carve [NPL-V]]
The ND-5900 (multi-CPU) assigns:
- ND-5000 CPU 1 -> octobus station "708"  = octal **70B**
- ND-5000 CPU 2 -> "718" = **71B**
- ND-5000 CPU 3 -> "728" = **72B**
- ND-5000 CPU 4 -> "738" = **73B**

The trailing "8" is a rendering artifact of the octal subscript (70₈ = 70B) [D — but the
increment-by-10-octal pattern and the range make this unambiguous]. This EXACTLY matches
our carved **`FN5DEST=070B` / `LN5DEST=073B`** (stations 070–073, `CARVE-ANSWER-ND5000-
ACTIVATION-WORKFLAG.md` [NPL-V]) and the "up to 4 CPUs" multi-CPU threading requirement.
- CONNECTION: independent official-doc confirmation that (a) the max is 4 CPUs and (b) the
  station range is exactly 70B–73B. Our GIVEINT-needs-5OMDNO>=8-to-reach-station-1 finding
  and the fabric station addressing are consistent with this range.

### 1.3 CPU MODEL (2/4/5/7/8) is DISTINCT from the SINTRAN 5CPUTYPE  [DOC-wiki + our carve]
Two different identifiers — do not conflate:
- **`5CPUTYPE`** (SINTRAN, 3-bit field, our skill/carve): OLD500=1, **SAMSON=3**. This is the
  GENERATION the driver detects (CH5CPUPRESENT / READ-CPU-TYPE 170B GETCP / CPUAVAILABLE).
- **CPU MODEL** (MF-backplane EEPROM, this doc): **2/4/5/7/8** = ND-5200/5400/5500/5700/5800.
  A FINER designation WITHIN Samson, set by the MF-bus updating tool (`SET-CPU-MODEL`), read
  via the MF-bus maintenance `LIST-CONFIGURATION` (NOT a SINTRAN 5CPUTYPE read). All five
  models report `5CPUTYPE=3` (Samson) to SINTRAN. [D: the model number is an MF-bus/ACCP
  concern, invisible to the 5MPM mailbox path — so it does NOT affect the servicer.]

### 1.4 Model / CPU-type / microprogram / clock table  [DOC-wiki]
| Model | CPU MODEL # | CPU type | Master clock | Microprogram ver | Caches enabled |
|---|---|---|---|---|---|
| ND-5200 | 2 | 1 | 70 ns | (base) | none |
| ND-5400 | 4 | 2 | 156 ns | 144xx | I-cache, Smart-IfGo |
| ND-5500 | 5 | 2 | 156 ns | 145xx | + D-cache |
| ND-5700 | 7 | 2 | 70 ns  | 147xx | + Address-cache |
| ND-5800 | 8 | 3 | 70 ns  | 148xx | + WICO (write-in-cache-only) |

- **CPU types 1/2/3** = 2/3/4 card layers. Type 1 lacks the instruction/data-cache layer
  AND the **AAP** (Additional Arithmetic Processor) -> on type 1 **floating point is done by
  the microprogram** [DOC-wiki]. RELEVANCE: if FP is ever emulated on a type-1 (ND-5200)
  Samson, it runs through microcode, not an AAP.
- Master clock jumpered Slow (156 ns) / Normal (70 ns); performance = CPU type × clock ×
  cache-feature enables. No mailbox/servicer relevance — timing is not modeled (protocol is
  flag-driven, not delay-driven), but noted for the live-oracle timing discussions.

### 1.5 '87 extensions — 25 new instructions  [DOC-wiki]
The ND-5000 CPU runs the same instruction set as the ND-500 CPU PLUS **25 new instructions**
("'87 extensions"), also present in the ND-500/1 and ND-500/2 CPUs.
- RELEVANCE to `CpuND500`: a full fetch/execute Samson bring-up (octobus Phase-3 next tier)
  may encounter these 25 opcodes; they are NOT in the base ND-500 instruction set. [?] which
  25 — needs the ND-05.009.3 / ND-05.020.1 opcode appendix. Flag for the CPU-implementation
  session.

---

## 1.6 HOW the CPU type/model is encoded and read — [V] microcode-image evidence (2026-07-18)

Source corpus: `E:\Dev\Ronny\ND5000UC\docs\MC\` (per-model microcode images + `.LABE`
symbol files + PROGRAM DESCRIPTION `.TEXT` files — the FULL set; the `microcode\*.md`
markdown are only exports of the 5800 image). manifest: 4 models (5200/5500/5700/5800) x
work modes (A=WM406, B=WM500, M) x revisions (27/29/30).

**FINDING [V by direct 4-image comparison]:** the CPU **type+model is baked into the
microcode image at control-store word `000007` (symbol `CPUMODEL`)**. Comparing word 7
(bytes 0x70-0x7F) across `MIC-52/55/57/58 00-90-500.DATA`, all bytes are IDENTICAL except
the last, which decodes as a packed byte **`(CPU_type << 4) | model_digit`**:

| Image | word7 last byte | CPU type (hi nibble) | model digit (lo nibble) |
|---|---|---|---|
| 5200 | 0x12 | 1 | 2 |
| 5500 | 0x25 | 2 | 5 |
| 5700 | 0x27 | 2 | 7 |
| 5800 | 0x38 | 3 | 8 |

Exact match to the wiki type column (1/2/2/3) AND model numbers (2/5/7/8). The
`MICRO-5800-B30.DATA` we exported carries 0x38 (type 3, model 8) = confirmed genuine 5800.

**The read chain [V symbol addresses, `MICRO-5800-B30.LABE`]:**
- `CPUMODEL` = word `000007` (the packed type/model byte in the LARG field).
- `CPU_READ` @`017130` reads `CPUMODEL` and indexes the `CPU_MODEL00-17` table
  (`017143-017162`, 16 entries; values `000101/001240/001041/001001/001000/000001...` per
  the INIT-SAMSON decode) to compose **CPUPAR** -> SRF `0o2015` (exact mask/index [D]).
- CPUPAR is returned to the ND-100 in the `3RMICV` mailbox reply (msg HW `0o10`) and
  surfaced through the cached CPU-DF that MON60 `170B READ-CPU-TYPE` reports.
- Version is a SEPARATE baked field: word 1 LARG = `0x2E9A` = `027232B` (same across
  models — tracks REVISION, not model).

**CORRECTION to §1.3's earlier phrasing:** the model number the MICROCODE reports to
SINTRAN comes from **word 7 of the loaded image**, NOT the MF-backplane EEPROM. The EEPROM
(`SET-CPU-MODEL`) is the MF-bus hardware's separate copy; an upgrade sets BOTH (new
microprogram floppy carries word 7 + the updating tool writes the EEPROM) — which is why
every wiki upgrade step pairs "exchange microprogram" with "set CPU model". EMULATION
consequence: the reported model follows whichever microcode image is loaded into the
control store (the emulator's `csStore`); to emulate a 5700 vs 5800, load that image (or
patch word 7). [?] whether the real loader ALSO patches word 7 from the EEPROM — not proven.

## 1.7 Directly-useful content in the PROGRAM DESCRIPTION texts  [DOC — 211276D]

`5800-30.TEXT` = the real ND-5800 Microprogram PROGRAM DESCRIPTION (Reg.no `211276D`,
rev 30, source `250291D`; A=WM406 / B=WM500, both SINGLE-CPU configs). Its changelog
documents fields we had flagged as unmodeled in the context-block work:
- **CED register:** changing it (when included in the register-block mask) could cause a
  PV-trap OR load wrong data to the rest of the register block — fixed rev 30. Relevant to
  `CED@0x5C` (R2-4 byte-mask) semantics.
- **PS register:** changing it "was not done in hardware" — fixed rev 30. Relevant to the
  `PS/PHS@0x48` context field we listed as NOT modeled.
- Built-in **RTC** (`ND-500-RTC-LIB`): `CLINT` (gen external interrupt), `CLRCLK` (reset
  RTC, must be privileged else Illegal Instruction Code), `RDCLK` (read 32-bit). Relevant
  to the octobus RTC-determinism work; "INIT-5000 routine to reset the clock counter"
  changelog.
- ALT-prefix string instructions, `Lregbl` fix, `Nksend`/`Nkmove` nucleus changes, kicklock
  timeout error code — microcode-behavior notes for the CPU/nucleus layers.

## 2. Open reconciliations / [?]

1. **Microprogram version numbering vs our extracted constant.** This doc gives ND-5800
   microprogram = **"148xx" (decimal)**. We extracted **`027232B` (octal)** = 0x2E9A =
   11930 decimal from microword 1 LARG of `MICRO-5800-B30.DATA`, and separately noted work
   modes A=406 / B=500. `027232B` does NOT obviously equal 148xx. UNRECONCILED [?] but now better bounded:
   the image-internal version (`027232B` @word1 LARG) tracks the REVISION (same across the
   5200/5500/5700/5800 word-7-differing images) and is DISTINCT from the model (word 7). The
   ND-5800 microprogram product/floppy is Reg.no `211276D` rev 30 (source `250291D`) per
   `5800-30.TEXT` — none of these equal "148xx" either. So "148xx" is likely a THIRD
   identifier (an earlier/marketing microprogram version or LOAD version), NOT the word-1
   constant and NOT the product number. Do NOT assume equality. (Classic-500 microprograms
   were 15211/15311/15111 per model — a different scheme again.)
2. **`LIST-CONFIGURATION` fields** (MF-bus maintenance): `STATION NO=0000708`, `POWER FAIL
   DESTINATION=1B`, `BROADCAST TYPE=0`, `SPEED=1B`, `CPU MODEL=7B`, `MASTER CONTROL
   REG=201B`. These are MF-bus/EEPROM slot-config fields, NOT 5MPM mailbox fields — no direct
   servicer mapping. `MASTER CONTROL REG=201B` and `SPEED=1B` recorded verbatim; meaning not
   independently verified [?].
3. **Updating tool destroys CPU model** if `INITIATE-EEPROM`/`CONFIGURATE-SLOT` used on the
   MF-controller/CPU slot — pure hardware-maintenance hazard, no emulation relevance beyond
   noting the model lives in nonvolatile MF-backplane memory (an emulator would seed it, not
   compute it).

---

## 3. What this does NOT change

- **The 5MPM mailbox / servicer model is unaffected.** CPU model (5200–5800), clock speed,
  cache enables, and the MF-backplane EEPROM are all BELOW / BESIDE the mailbox protocol.
  The servicer sees `5CPUTYPE=3` (Samson) and the message fields, not the model number.
- **Rallar (5830/5850) is out of scope** — different processor (KUSK/GAMP), likely different
  microcode; our Samson (5800/B30) work does not cover it.
- **NDIX (UNIX) coexistence** confirmed (5000 runs SINTRAN III AND NDIX) — consistent with
  the microcode's NDIX MON-call case that answers WITHOUT stopping (async MON). No new action.

---

## 4. Compact vs Large cabinet (background)  [DOC-wiki]
- **Compact series** (ND-5200/5700 Compact): ND-110/CX (ND-5200 Compact uses ND-110) or
  ND-120 I/O processor; internal disks; updating tool = **350156 "Double Bus Contr."**.
- **Large cabinet**: updating tool = **350157 "MF Bus Controller"**.
- Both hold the CPU model in MF-backplane nonvolatile memory. Emulation: seed the model per
  the configured machine; there is no runtime computation of it.
