# ND-100 Instruction Semantics — Canonical Merged Reference

**Purpose / how to use this for pseudo-C.** This is the single authoritative reference
for translating carved ND-100 disassembly into correct pseudo-C. For any instruction,
find its row in the cheat-sheet (§1) or its class section (§§2-10), copy the "exact C
meaning", and substitute the actual register letters / displacement from the carved listing.
Confidence is marked on every entry so you never ground a MON-call body in an
unverified semantic. All numeric literals are **octal** unless suffixed otherwise
(ND-100 convention). Registers are 16-bit, memory is word-addressed, arithmetic is
two's-complement.

**Provenance.** This document merges two independently produced authorities:

- **EMU** = `ND100-SEMANTICS-FROM-EMULATOR.md` — derived from the executable behavior of
  the nd100x CPU core, cited `cpu.c` / `cpu_instr.c` / `cpu_disasm.c` / `cpu_types.h` by line.
  **This is the executable ground truth.**
- **SLEIGH** = `ND100-SEMANTICS-FROM-GHIDRA.md` — derived from the formal Ghidra SLEIGH /
  P-code definition, cited `nd100.slaspec` / `nd100_memory.sinc` / `nd100_shift.sinc` by line.

**Reconciliation rule (applied to every entry):**

- Both sources agree → **VERIFIED** (cited to both).
- Sources disagree → **EMULATOR-AUTHORITATIVE**: the emulator semantics is stated as correct,
  with a one-line note recording what SLEIGH modelled differently.
- Both unresolved / missing → **UNRESOLVED**, with the reason. Semantics are never invented.

Register-index encoding (used by ROP, SKP, IRW/IRR, bit ops), agreed by both sources
(EMU `cpu_types.h:163-170`; SLEIGH `nd100.slaspec:100-102`):

| idx | reg | idx | reg |
|-----|-----|-----|-----|
| 0 | STS | 4 | L |
| 1 | D   | 5 | A |
| 2 | P   | 6 | T |
| 3 | B   | 7 | X |

Register-letter map for ROP/SKP operand fields: **D=1, A=5, T=6, X=7, B=3, L=4, P=2, STS=0.**
Disassembler suffixes: source field prints `S<r>` (`SD SA ST SX SB SL SP`), destination
field prints `D<r>` (`DD DA DT DX DB DL DP`); field value 0 = STS (EMU `cpu_disasm.c:40,49,50`;
SLEIGH `nd100.slaspec:85-89`).

Status bits inside STS (both agree): bit2 `K` (one-bit accumulator), bit3 `Z` (error),
bit4 `Q` (dynamic/sticky overflow), bit5 `O` (overflow), bit6 `C` (carry), bit7 `M`
(multi-shift link) (EMU `cpu_types.h:233-245`; SLEIGH `nd100_registers.sinc:5-7`).

---

## 1. Translation cheat-sheet (most common MON-handler forms)

`P` = address of the instruction word itself. `disp` = 8-bit displacement, sign-extended.
`mem[]` = word memory. `ind()` = one indirect word fetch. `phys[]` = physical memory
(page tables bypassed). `EA` per §2.1. Confidence: **V**=VERIFIED, **E**=EMULATOR-AUTHORITATIVE,
**U**=UNRESOLVED.

| Disassembly form | Exact C meaning | Conf. | Sources |
|------------------|-----------------|-------|---------|
| `LDA disp`            | `A = mem[P + disp]` | V | EMU `cpu.c:186`,`cpu_instr.c:591`; SL `slaspec:966` |
| `LDA ,B disp`         | `A = mem[B + disp]` | V | EMU `cpu.c:186`; SL `memory.sinc:27` |
| `LDA ,X disp`         | `A = mem[X + disp]` | V | EMU `cpu.c:186`; SL `memory.sinc:47` |
| `LDA I disp`          | `A = mem[ mem[P + disp] ]` | V | EMU `cpu.c:205`; SL `memory.sinc:33` |
| `LDA I ,B disp`       | `A = mem[ mem[B + disp] ]` | V | EMU `cpu.c:210`; SL `memory.sinc:40` |
| `LDA ,X ,B disp`      | `A = mem[B + X + disp]` | V | EMU `cpu.c:186`; SL `memory.sinc:53` |
| `LDA I ,X disp`       | `A = mem[ X + mem[P + disp] ]` | V | EMU `cpu.c:223`; SL `memory.sinc:59` |
| `LDA I ,B ,X disp`    | `A = mem[ X + mem[B + disp] ]` | V | EMU `cpu.c:228`; SL `memory.sinc:67` |
| `STA/STT/STX ,B N`    | `mem[EA] = A / T / X` | V | EMU `cpu_instr.c:547,556,564`; SL `slaspec:1675,1718,1724` |
| `STZ ,B N`            | `mem[EA] = 0` | V | EMU `cpu_instr.c:539`; SL `slaspec:1730` |
| `STD I ,B ,X N`       | `EA = X + mem[B+disp]; mem[EA]=A; mem[EA+1]=D` | V | EMU `cpu_instr.c:572`; SL `slaspec:1688` |
| `LDD ,B N`            | `A = mem[B+disp]; D = mem[B+disp+1]` | V | EMU `cpu_instr.c:615`; SL `slaspec:988` |
| `ADD ,B N`            | `A = do_add(A, mem[EA], 0)` (C/O/Q) | V | EMU `cpu_instr.c:799`; SL `slaspec:260` |
| `SUB ,B N`            | `A = do_add(A, ~mem[EA], 1)` (= A − mem) | V | EMU `cpu_instr.c:809`; SL `slaspec:1743` |
| `AND ,B N`            | `A &= mem[EA]` (no flags) | V | EMU `cpu_instr.c:818`; SL `slaspec:280` |
| `ORA ,B N`            | `A \|= mem[EA]` (no flags) | V | EMU `cpu_instr.c:826`; SL `slaspec:1171` |
| `MIN ,B N`            | `t=mem[EA]+1; mem[EA]=t; if(t==0) PC++` | V | EMU `cpu_instr.c:785`; SL `slaspec:1088` |
| `MPY ,B N`            | `A = (int16)A * (int16)mem[EA]` (O/Q) | V | EMU `cpu_instr.c:3101`; SL `slaspec:1136` |
| `JMP ,B N` / `JMP I N`| `PC = EA` | V | EMU `cpu_instr.c:917`; SL `slaspec:765` |
| `JPL I N`             | `L = returnaddr(P+1); PC = mem[P+disp]` | V | EMU `cpu_instr.c:453`; SL `slaspec:844` |
| `JAF N`               | `if (A != 0) PC = P + disp` | V | EMU `cpu_instr.c:393`; SL `slaspec:724` |
| `JAZ N`               | `if (A == 0) PC = P+disp; C = (A==0)` | V | EMU `cpu_instr.c:378`; SL `slaspec:750` |
| `JAN/JAP N`           | `if (A<0)/(A>=0) PC = P+disp` | V | EMU `cpu_instr.c:365,352`; SL `slaspec:734,742` |
| `JXZ/JXN N`           | `if (X==0)/(X<0) PC = P+disp` | V | EMU `cpu_instr.c:446,434`; SL `slaspec:913,905` |
| `JPC N` / `JNC N`     | `X++; if (X>=0)/(X<0) PC = P+disp` | V | EMU `cpu_instr.c:406,421`; SL `slaspec:828,818` |
| `SAA N`               | `A = signext8(N)` | V | EMU `cpu_instr.c:230`; SL `slaspec:1390` |
| `SAT/SAX/SAB N`       | `T/X/B = signext8(N)` | V | EMU `cpu_instr.c:243,251,237`; SL `slaspec:1456,1462,1397` |
| `AAA N`               | `A = do_add(A, signext8(N), 0)` (C/O/Q) | V | EMU `cpu_instr.c:161`; SL `slaspec:208` |
| `AAX/AAT/AAB N`       | `reg = do_add(reg, signext8(N), 0)` (C/O/Q) | V | EMU `cpu_instr.c:191,181,171`; SL `slaspec:247,234,221` |
| `RADD SD DA`          | `A = do_add(A, D, 0)` (A = A + D) | V | EMU `cpu_instr.c:1727`; SL `slaspec:1215` |
| `RADD CLD SD DA`      | **`A = D`** (COPY: dest cleared, then +source) | V | EMU `cpu_instr.c:1694,1727`; SL `slaspec:1215,485` |
| `COPY SA DX` (=`RADD CLD SA DX`) | `X = A` | V | EMU `cpu_disasm.c`; SL `slaspec:485` |
| `RSUB SX DA`          | `A = do_add(A, ~X, 1)` (A = A − X) | V | EMU `cpu_instr.c:1727`; SL `slaspec:1376` |
| `RADD ADC SD DA`      | `A = do_add(A, D, C)` (add-with-carry) | V | EMU `cpu_instr.c:1727`; SL `slaspec:1244` |
| `RCLR DA` (=`RADD CLD 0 DA`) | `A = 0` | V | EMU `cpu_instr.c:259`; SL `slaspec:1267` |
| `RINC DA` (=`RADD AD1 0 DA`) | `A = do_add(A,0,1)` (A+1) | V | EMU `cpu_instr.c:260`; SL `slaspec:1348` |
| `RDCR DA` (=`RADD CM1 0 DA`) | `A = do_add(A,~0,0)` (A−1) | V | EMU `cpu_instr.c:261`; SL `slaspec:1275` |
| `EXIT` (=`RADD CLD SL DP`, 0146142) | `PC = L` (return) | V | EMU `cpu_instr.c:262`; SL `slaspec:569` |
| `SWAP SA DT`          | `t=T; T=A; A=t` (full exchange) | E | EMU `cpu_instr.c:1704-1726`; SL differs (see §3.3) |
| `STATX N`             | `phys[EL] = A`, `EL=((T&0xFF)<<16)\|((X+(N>>3&7))&0xFFFF)` | E | EMU `cpu_instr.c:652`; SL modelled X-only |
| `STZTX N`             | `phys[EL] = 0` | E | EMU `cpu_instr.c:639`; SL modelled X-only |
| `STDTX N`             | `phys[EL] = A; phys[EL+1] = D` | E | EMU `cpu_instr.c:665`; SL modelled X-only |
| `LDATX N`             | `A = phys[EL]` | E | EMU `cpu_instr.c:689`; SL modelled X-only |
| `LDXTX N`             | `X = phys[EL]` | E | EMU `cpu_instr.c:711`; SL modelled X-only |
| `LDDTX N`             | `A = phys[EL]; D = phys[EL+1]` | E | EMU `cpu_instr.c:733`; SL modelled X-only |
| `LDBTX N`             | `B = 0177000 \| ((2*phys[EL]) & 0xFFFF)` | E | EMU `cpu_instr.c:759`; SL modelled X-only |
| `STBTX` (143307)      | *(no dispatch / no constructor)* | U | EMU `cpu_instr.c:3424-3430`; SL `slaspec:529` |
| `SKP IF DA EQL ST`    | `if (A == T) PC++` | V | EMU `cpu_instr.c:2210`; SL `slaspec:1634` |
| `SKP IF DA GRE SX`    | `if ((int16)A >= (int16)X) PC++` | V | EMU `cpu_instr.c:2213`; SL `slaspec:1642` |
| `BSET ONE 5 DA`       | `A \|= (1<<5)` | V | EMU `cpu_instr.c:2256`; SL `slaspec:346` |
| `BSET ZRO 5 DA`       | `A &= ~(1<<5)` | V | EMU `cpu_instr.c:2256`; SL `slaspec:340` |
| `BSKP ONE 0 DA`       | `if (A & 1) PC++` | V | EMU `cpu_instr.c:2256`; SL `slaspec:382` |
| `BSKP ZRO 0 DA`       | `if (!(A & 1)) PC++` | V | EMU `cpu_instr.c:2256`; SL `slaspec:374` |
| `SHA N`               | `A <<= N` (zero fill) | V | EMU `cpu_instr.c:2323`; SL `slaspec:1508` |
| `SHA SHR N`           | `A >>= N` (arithmetic, sign kept) | V | EMU `cpu_instr.c:2323`; SL `slaspec:1508` |
| `SHA ROT SHR 1`       | rotate A right 1 | V | EMU `cpu_instr.c:2323`; SL `slaspec:1508` |
| `IOX dev`             | `A = io_op(dev & 0x7FF, A)` | V | EMU `cpu_instr.c:1025`; SL `slaspec:688` |
| `MON N`               | trap to level-14 monitor, code N in T | V | EMU `cpu_instr.c:202`; SL `slaspec:1108` |
| `MST PID` / `MCL PIE` | masked set/clear of named system reg by A | V | EMU `cpu_instr.c:1821,1784`; SL `slaspec:1149,1078` |
| `IRW 120 DB`          | `reg[level 12][B] = A` | V | EMU `cpu_instr.c:1096`; SL `slaspec:717` |
| `IRR 120 DB`          | `A = reg[level 12][B]` | V | EMU `cpu_instr.c:1129`; SL `slaspec:710` |
| `WAIT`                | yield / dismiss current interrupt level | V | EMU `cpu_instr.c:1979`; SL `slaspec:1973` |
| `147440`              | ROP NOOP (`/* nop */`) | V | EMU `cpu_instr.c:1686`; SL (RADD AD=6) |

---

## 2. Memory-reference instructions + addressing modes

### 2.1 Effective address (authoritative formula)

`disp = signExtend(instr & 0xFF)`; `P = address of the instruction word`; selector = bits 8-10.
Both sources agree on all eight modes (**VERIFIED**). EMU `New_GetEffectiveAddr`
(`cpu.c:186-233`); SLEIGH `nd100_memory.sinc:16-72`.

| bits 8-10 | disasm | Effective address `EA` |
|-----------|--------|------------------------|
| 0 | *(none)*   | `EA = P + disp` |
| 1 | `,B `      | `EA = B + disp` |
| 2 | `I `       | `EA = ind(P + disp)` |
| 3 | `I ,B `    | `EA = ind(B + disp)` |
| 4 | `,X `      | `EA = X + disp` |
| 5 | `,X ,B `   | `EA = B + X + disp` |
| 6 | `I ,X `    | `EA = X + ind(P + disp)` |
| 7 | `I ,B ,X ` | `EA = X + ind(B + disp)` |

**Order of operations (both sources confirm):** in modes 6 and 7 the indirect word is
fetched FIRST, then X is added to the fetched pointer — **post-indexed indirect**. There is
exactly one level of indirection. Mode 5 is pure pre-indexing (`B + X + disp`, no fetch)
(EMU `cpu.c:205,210,223,228`; SLEIGH `nd100_memory.sinc:59-64,67-72`).

### 2.2 The instructions (all VERIFIED unless noted)

| Mnemonic | Opcode base | Operation | EMU / SLEIGH |
|----------|-------------|-----------|--------------|
| `STZ` | 000000 | `mem[EA] = 0` | `cpu_instr.c:539` / `slaspec:1730` |
| `STA` | 004000 | `mem[EA] = A` | `:547` / `:1675` |
| `STT` | 010000 | `mem[EA] = T` | `:556` / `:1718` |
| `STX` | 014000 | `mem[EA] = X` | `:564` / `:1724` |
| `STD` | 020000 | `mem[EA] = A; mem[EA+1] = D` | `:572` / `:1688` |
| `LDD` | 024000 | `A = mem[EA]; D = mem[EA+1]` | `:615` / `:988` |
| `STF` | 030000 | `mem[EA]=T; mem[EA+1]=A; mem[EA+2]=D` | `:581` / `:1706` |
| `LDF` | 034000 | `T=mem[EA]; A=mem[EA+1]; D=mem[EA+2]` | `:625` / `:1006` |
| `MIN` | 040000 | `t=mem[EA]+1; mem[EA]=t; if(t==0) PC++` | `:785` / `:1088` |
| `LDA` | 044000 | `A = mem[EA]` | `:591` / `:966` |
| `LDT` | 050000 | `T = mem[EA]` | `:599` / `:1018` |
| `LDX` | 054000 | `X = mem[EA]` | `:607` / `:1024` |
| `ADD` | 060000 | `A = do_add(A, mem[EA], 0)` (C/O/Q) | `:799` / `:260` |
| `SUB` | 064000 | `A = do_add(A, ~mem[EA], 1)` (= A−mem; C=1 means no-borrow) | `:809` / `:1743` |
| `AND` | 070000 | `A &= mem[EA]` (no flags) | `:818` / `:280` |
| `ORA` | 074000 | `A \|= mem[EA]` (no flags) | `:826` / `:1171` |
| `MPY` | 120000 | `A = (int16)A * (int16)mem[EA]`; sets O/Q on \|prod\|>32767 | `:3101` / `:1136` |
| `JMP` | 124000 | `PC = EA` | `:917` / `:765` |
| `JPL` | 134000 | `L = P+1 (return); PC = EA` | `:453` / `:844` |
| `FAD/FSB/FMU/FDV` | 100000/104000/110000/114000 | 48-bit float T:A:D op mem[EA..EA+2] | `:834-913` / (opaque pcodeop) |

Notes (both agree): **STD/LDD put A in the low word (EA), D in EA+1** (A first, then D).
`AND`/`ORA` touch no status bit. `SUB` is one's-complement add + 1 (proper two's-complement
subtract; C=1 = no borrow). `JPL` stores the return address (word after JPL) into L, then jumps.
`FDV` sets `Z` on divide-by-zero (EMU `cpu_instr.c:906`). Float format is 48-bit (T:A:D);
SLEIGH models the float ops as opaque pcodeops (§10 caveat 5), so float bodies are
**EMULATOR-AUTHORITATIVE** — consult EMU `float.c` if float translation is required.

---

## 3. Register-operation (ROP) class — base 144000-147777

**This is the #1 source of mistranslation.** Dispatched by `regop` (EMU `cpu_instr.c:1686-1769`);
SLEIGH constructor `nd100.slaspec:1215-1258`.

### 3.1 Field layout of the 16-bit ROP word (both sources agree, VERIFIED)

```
bit:  10        9     8     7     6     5 4 3    2 1 0
      RAD      ADC   AD1   CM1   CLD   [  sr ]  [  dr ]
```
EMU extraction (`regop`): `RAD=(op>>10)&1`, `CM1=(op>>7)&1`, `CLD=(op>>6)&1`,
`sr=(op>>3)&7`, `dr=op&7` (`cpu_instr.c:1692-1697`). SLEIGH fields: `rop_ad=(8,9)`,
`rop_cm1=(7,7)`, `rop_cld=(6,6)`, `rop_src=(3,5)`, `rop_dst=(0,2)` (`nd100.slaspec:62-64`).

### 3.2 Operand fetch — both classes (VERIFIED)

* `source = (sr == 0) ? 0 : reg[CurrLEVEL][sr]` — **when the source field is 0 (STS), the
  literal 0 is used, the STS register is NEVER read** (EMU `cpu_instr.c:1699`; SLEIGH via the
  `rop_src` value/attach).
* `dest = CLD ? 0 : reg[CurrLEVEL][dr]` — **CLD clears the destination operand to 0 BEFORE the
  operation** (EMU `cpu_instr.c:1700`; SLEIGH `nd100.slaspec:1232-1233`).
* EMU: if `dr == 0` (destination = STS) the logical/arithmetic result write is suppressed;
  `147440` (dr=0, arithmetic sub-op 6 = NOOP) changes nothing (`cpu_instr.c:1758-1761`).

### 3.3 Logical operations (RAD = 0), sub-op = bits 8,9

Effective only when `dr != 0`. Let `s = CM1 ? ~source : source` (EMU `cpu_instr.c:1704-1726`).

| bits 8,9 | mnem | operation | Conf. |
|----------|------|-----------|-------|
| 0 | `SWAP` | `t = reg[dr]; reg[dr] = s; reg[sr] = CLD ? 0 : t` — **full exchange** (dr↔sr) | **E** |
| 1 | `RAND` | `reg[dr] &= s; if (CLD) reg[dr] = 0` | V |
| 2 | `REXO` | `reg[dr] = CLD ? s : (reg[dr] ^ s)` (XOR) | V |
| 3 | `RORA` | `reg[dr] = CLD ? s : (reg[dr] \| s)` (OR) | V |

Logical ops do **not** change C/O/Q/Z. RAND/REXO/RORA are VERIFIED
(EMU `cpu_instr.c:1704-1726`; SLEIGH `nd100.slaspec:1261,1336,1370`).

**SWAP is EMULATOR-AUTHORITATIVE.** The emulator's plain `SWAP` (bits 8,9 = 0) performs a
full register exchange: dr receives the source, sr receives the old dr value (complemented if
CM1, zeroed on the sr side if CLD). SLEIGH modelled the plain/CLD/CM1+CLD SWAP forms as a bare
`dst = src` copy and only its `CM1` form as a true exchange (`nd100.slaspec:1780-1802`,
self-flagged as contradicting its own header comment). Use the emulator exchange semantics.

### 3.4 Arithmetic operations (RAD = 1), sub-op = bits 7,8,9 (VERIFIED)

Effective only when `dr != 0`. `dest = CLD ? 0 : reg[dr]`, `source` per §3.2. All go through
`do_add`, so **C/O/Q are affected** (C = carry-out bit16; O set + Q set on signed overflow,
O sticky; EMU `cpu_instr.c:82-105,1727-1762`; SLEIGH `nd100.slaspec:1248-1257`).

| bits 7,8,9 | disasm | operation | note |
|------------|--------|-----------|------|
| 0 | `RADD`         | `reg[dr] = do_add(dest, source, 0)` | dest + source |
| 1 | `RADD CM1`     | `do_add(dest, ~source, 0)` | dest − source − 1 |
| 2 | `RADD AD1`     | `do_add(dest, source, 1)` | dest + source + 1 |
| 3 | `RADD AD1 CM1` | `do_add(dest, ~source, 1)` | = **RSUB**: dest − source |
| 4 | `RADD ADC`     | `do_add(dest, source, C)` | add-with-carry |
| 5 | `RADD ADC CM1` | `do_add(dest, ~source, C)` | subtract-with-borrow |
| 6 | NOOP | nothing | |
| 7 | NOOP | nothing | |

The disassembler prints sub-op 3 as `RSUB` and the word 0146142 as `EXIT` (EMU `cpu_disasm.c:457-477`).

### 3.5 The critical example: `RADD CLD SD DA` — VERIFIED (both agree)

`sr = SD = 1 (D)`, `dr = DA = 5 (A)`, `CLD = 1`, arithmetic sub-op 0.
`dest = CLD ? 0 = 0`; `source = reg[D] = D`; `reg[A] = do_add(0, D, 0) = D`.

**Result: `A = D` (a plain register copy).** This is the `COPY` idiom: `COPY <sr> <dr>`
assembles to `RADD CLD <sr> <dr>` = `dst = src` (destination cleared, source added to 0).
`do_add(0,src,0)` updates C/O/Q as a side effect (C=0; O/Q cleared, since copying a value has
no signed overflow). EMU `cpu_instr.c:1694,1727-1762`; SLEIGH `nd100.slaspec:1215-1258` (and
`COPY` alias at `:485-490`, which explicitly forces `C=0; O=0`).

### 3.6 Distinctions to remember (VERIFIED)

* `RADD SD DA` (no CLD) = `A = A + D`. `RADD CLD SD DA` = `A = D`. The **only** difference is
  CLD, which decides whether the destination register participates or is zeroed.
* `COPY` = `RADD CLD` (dst := src). `SWAP` = full exchange (dst↔src), a logical opcode.
* `RCLR dr` = `RADD CLD` with `sr=0` → `dst = do_add(0,0,0) = 0`.
* `RINC dr` = `RADD AD1` with `sr=0` → `dst = dst + 1`.
* `RDCR dr` = `RADD CM1` with `sr=0` → `dst = dst + ~0 = dst − 1`.
* `EXIT` = `RADD CLD SL DP` (0146142) → `P = L` (procedure return).

### 3.7 RMPY / RDIV (same register fields, separate opcodes)

* `RMPY sr dr` (base 141200; EMU `cpu_instr.c:3061-3096`, SLEIGH `nd100.slaspec:1357`):
  signed 16×16→32, **always** writes `A = high16, D = low16` regardless of the operand
  registers; `source=(sr==0)?0:reg[sr]`, `dest=(dr==0)?0:reg[dr]`; sets `C` if the product
  exceeds 16 bits. VERIFIED on the A:D result and C. (EMU note: the active `rmpy` does not set
  O/Q; the older `rmpy_org` did.)
* `RDIV sr` (base 141600; EMU `cpu_instr.c:2990-3018`, SLEIGH `nd100.slaspec:1284`): 32-bit
  dividend `(A<<16)|D` divided by `reg[sr]` (0 if sr=0). Quotient→A, remainder→D.
  Divide-by-zero or \|quotient\|≥32768 sets `Z` and skips the A/D write. Sets `C` if quotient
  exceeds 16 bits. VERIFIED.

---

## 4. Argument instructions — set / add signed 8-bit argument (all VERIFIED)

`arg = signExtend(operand & 0xFF)`. EMU `cpu_instr.c:159-254`; SLEIGH `nd100.slaspec:208-257,
1390-1465`.

| Mnemonic | Opcode base | Operation | EMU / SLEIGH |
|----------|-------------|-----------|--------------|
| `SAA n` | 170400 | `A = arg` (no flags) | `:230` / `:1390` |
| `SAB n` | 170000 | `B = arg` | `:237` / `:1397` |
| `SAT n` | 171000 | `T = arg` | `:243` / `:1456` |
| `SAX n` | 171400 | `X = arg` | `:251` / `:1462` |
| `AAA n` | 172400 | `A = do_add(A, arg, 0)` (C/O/Q) | `:161` / `:208` |
| `AAB n` | 172000 | `B = do_add(B, arg, 0)` | `:171` / `:221` |
| `AAT n` | 173000 | `T = do_add(T, arg, 0)` | `:181` / `:234` |
| `AAX n` | 173400 | `X = do_add(X, arg, 0)` | `:191` / `:247` |

`SAx` loads the sign-extended argument (no status change). `AAx` uses `do_add`, so **C/O/Q are
affected**. The 8-bit field gives an argument range of −200..+177 (octal).

---

## 5. T/X-indexed physical transfers — base 143300-143306

**EMULATOR-AUTHORITATIVE (sources disagree on the address).** These are privileged (EMU calls
`CheckPriv`). Dispatched with mask 0xFFC7; the variable field is bits 3-5. EMU
`cpu_instr.c:3424-3430`.

### 5.1 The effective address `EL` — EMULATOR is correct

EMU `calcEL(displacement)` (`cpu_instr.c:109-118`), with `displacement = (operand >> 3) & 0x07`:

```
EL = ( (T & 0xFF) << 16 ) | ( (X + displacement) & 0xFFFF )
EL = EL & 0xFFFFFF          // 24-bit PHYSICAL address
```

**`EL` is a 24-bit PHYSICAL address**: high 8 bits = `T & 0xFF` (the memory bank), low 16 bits
= `(X + disp) & 0xFFFF`. `disp = (operand >> 3) & 7` (the 3-bit field, bits 3-5). Access is via
`ReadPhysicalMemory` / `WritePhysicalMemory` (EMU `ReadEL`/`WriteEL`, `cpu_instr.c:121-130`) —
**it BYPASSES the MMU / page tables.**

> SLEIGH modelled the EA as `X + disp3` only (X register plus the 3-bit displacement) and
> **missed the T bank byte entirely** (`nd100.slaspec:972-1034,1681-1740`; self-flagged at
> caveat 4). The SLEIGH form is incomplete; use the emulator's 24-bit physical `EL`.

### 5.2 The instructions (semantics EMULATOR-AUTHORITATIVE)

| Mnemonic | Opcode | Operation | EMU |
|----------|--------|-----------|-----|
| `LDATX n` | 143300 | `A = phys[EL]` | `:689-698` |
| `LDXTX n` | 143301 | `X = phys[EL]` | `:711-720` |
| `LDDTX n` | 143302 | `A = phys[EL]; D = phys[EL+1]` | `:733-745` |
| `LDBTX n` | 143303 | `B = 0177000 \| ((2 * phys[EL]) & 0xFFFF)` | `:759-773` |
| `STATX n` | 143304 | `phys[EL] = A` | `:652-661` |
| `STZTX n` | 143305 | `phys[EL] = 0` | `:639-648` |
| `STDTX n` | 143306 | `phys[EL] = A; phys[EL+1] = D` | `:665-675` |

Notes: `LDDTX`/`STDTX` operate on the pair `(EL, EL+1)`, low word first; `STDTX` stores A into
`EL` and D into `EL+1` (matches STD's A-then-D convention). `LDBTX` doubles the fetched word
(`2*phys[EL]`, wrapped to 16 bits) and ORs in `0177000` (`0xFE00`), forming a page-table-entry
pointer with the top bits forced set (documented ND hardware quirk, used by CLEPT/SETPT;
EMU `cpu_instr.c:1272,1300`).

**`STBTX` (143307): UNRESOLVED.** Neither source defines it — the emulator has no dispatch
entry (falls through to illegal instruction, EMU `cpu_instr.c:3424-3430`) and SLEIGH defines
`fixed3` 0-6 only, with no `STBTX` constructor (`nd100.slaspec:529`). Do not translate 143307.

---

## 6. Conditional jumps & the SKP class

### 6.1 Register-conditional jumps (CJP family), base 130000-133777 — VERIFIED

Target is **P-relative**: `PC = P + signExtend(disp8)`, range −200..+177 (octal). EMU
`cpu_instr.c:331-343`; SLEIGH `rel_target` (`nd100_memory.sinc:197-199`).

| Mnemonic | Opcode base | Condition to jump | EMU / SLEIGH |
|----------|-------------|-------------------|--------------|
| `JAP d` | 130000 | `A >= 0` (bit15 == 0) | `:352` / `:742` |
| `JAN d` | 130400 | `A < 0`  (bit15 == 1) | `:365` / `:734` |
| `JAZ d` | 131000 | `A == 0` — **also sets `C = (A==0)`** | `:378` / `:750` |
| `JAF d` | 131400 | `A != 0` | `:393` / `:724` |
| `JPC d` | 132000 | `X++` first, then jump if `X >= 0` | `:406` / `:828` |
| `JNC d` | 132400 | `X++` first, then jump if `X < 0` | `:421` / `:818` |
| `JXZ d` | 133000 | `X == 0` | `:446` / `:913` |
| `JXN d` | 133400 | `X < 0` (bit15 == 1) | `:434` / `:905` |

Both sources confirm: `JPC`/`JNC` **always** increment X (even when not taken); `JAZ` writes
the carry bit; the others touch no status.

### 6.2 SKP — skip on register comparison, base 140000 — VERIFIED

EMU `IsSkip` (`cpu_instr.c:2197-2254`); SLEIGH `nd100.slaspec:1624-1666`. Disassembly
`SKP IF <dst> <cond> <src>`. Fields: `sr=(instr>>3)&7`, `dr=instr&7`; `source=(sr==0)?0:reg[sr]`,
`desti=(dr==0)?0:reg[dr]` (STS never read). If the condition holds, **skip the next
instruction** (`PC++`).

Flags computed on `desti` vs `source`: `z=(desti==source)`; `s = bit15 of (int16)(desti−source)`;
`o = signed-overflow`; `c = unsigned (desti >= source)`.

| bits 8-10 | disasm | skip if |
|-----------|--------|---------|
| 0 | `EQL`  | `desti == source` |
| 1 | `GEQ`  | `!s`  (signed ≥, sign-only test) |
| 2 | `GRE`  | `!(s ^ o)`  (signed desti ≥ source, overflow-correct) |
| 3 | `MGRE` | `c`   (unsigned desti ≥ source) |
| 4 | `UEQ`  | `desti != source` |
| 5 | `LSS`  | `s`   (signed desti < source) |
| 6 | `LST`  | `s ^ o`  (signed less, overflow-correct) |
| 7 | `MLST` | `!c`  (unsigned desti < source) |

Example: `SKP IF DA EQL ST` → `if (A == T) PC++`. `SKP IF DA GRE SX` →
`if ((int16)A >= (int16)X) PC++`. The disassembler order is **dst then src**; in C the
comparison reads `desti <cond> source`. (Note: both docs list the bit-6/7 mnemonics as
`LST`/`MLST`; agree.)

---

## 7. Bit instructions — base 174000-177777 — VERIFIED

EMU `do_bops` (`cpu_instr.c:2256-2321`); SLEIGH `nd100.slaspec:287-437`. Fields:
`bn=(op>>3)&0x0F` (bit number 0-15), `dr=op&7` (register), sub-op = bits 7-10. `K` = STS bit2.
When `dr==0` the target is the STS register bit.

| sub-op | disasm | operation |
|--------|--------|-----------|
| 0 | `BSET ZRO b Dr` | `reg[dr] &= ~(1<<b)` |
| 1 | `BSET ONE b Dr` | `reg[dr] \|= (1<<b)` |
| 2 | `BSET BCM b Dr` | `reg[dr] ^= (1<<b)` |
| 3 | `BSET BAC b Dr` | `reg[dr] bit b = K` (EMU) / `= carry` (SLEIGH) — see note |
| 4 | `BSKP ZRO b Dr` | `if (reg[dr] bit b == 0) PC++` |
| 5 | `BSKP ONE b Dr` | `if (reg[dr] bit b == 1) PC++` |
| 6 | `BSKP BCM b Dr` | `if ((bit ^ 1) == K) PC++` |
| 7 | `BSKP BAC b Dr` | `if (bit == K) PC++` |
| 8 | `BSTC` | `reg bit = K^1; K = 1` |
| 9 | `BSTA` | `reg bit = K;   K = 0` |
| 10 | `BLDC` | `K = bit ^ 1` |
| 11 | `BLDA` | `K = bit` |
| 12 | `BANC` | `K = (bit^1) & K` |
| 13 | `BAND` | `K = bit & K` |
| 14 | `BORC` | `K = (bit^1) \| K` |
| 15 | `BORA` | `K = bit \| K` |

So `BSKP ZRO 0 DA` = `if ((A & 1)==0) PC++`; `BSET ONE 5 DA` = `A |= (1<<5)`. The bit number
is bits 3-6 (0-15); the register is bits 0-2. (Minor note: for sub-op 3 `BSET BAC` EMU sets the
bit to the K accumulator (`cpu_instr.c` bit path) while SLEIGH sources the carry flag STS[6]
(`nd100.slaspec:358-366`); K and C occupy adjacent STS bits — where a handler uses BSET BAC,
prefer the emulator's K semantics.)

---

## 8. Shift instructions — base 154000-155777

EMU `ndfunc_shifts` / `ShiftReg` / `ShiftDoubleReg` (`cpu_instr.c:258-281,2323-2387`);
SLEIGH `nd100.slaspec:1414-1613`, `nd100_shift.sinc`. Target by bits 7-8: 0=`SHT`(T),
1=`SHD`(D), 2=`SHA`(A), 3=`SAD`(A:D 32-bit). Direction: bit5 set = **RIGHT** (disasm `SHR`);
clear = LEFT. Shift type = bits 9-10.

| type (bits 9,10) | disasm | left (fill bit0) | right (fill bit15) | Conf. |
|------------------|--------|------------------|--------------------|-------|
| 0 | *(plain)* | 0 (logical left) | **sign** (arithmetic right) | V |
| 1 | `ROT` | rotated-out bit → bit0 | rotated-out bit → bit15 | V |
| 2 | `ZIN` | 0 | 0 (logical right) | V |
| 3 | `LIN` | `M` bit → bit0 | `M` bit → bit15 | **E** |

Plain (type 0), ROT (type 1) and ZIN (type 2) are VERIFIED (both sources agree). The last bit
shifted out is written to the `M` status bit (EMU `cpu_instr.c:2352`). `SAD` applies the same
logic over the 32-bit `(A<<16)|D` value, writing A/D back.

**LIN (type 3) is EMULATOR-AUTHORITATIVE for the right-shift fill.** The emulator feeds the
`M` (link) bit into the vacated position for both directions (EMU `cpu_instr.c:2323-2354`).
SLEIGH modelled LIN-right as a plain arithmetic right shift and explicitly did **not** model
the M-flag fill (`nd100.slaspec:1532`). Use the emulator's M-bit link fill.

Examples: `SHA 3` = `A <<= 3` (zero fill). `SHA SHR 3` = arithmetic `A >>= 3` (sign kept).
`SHA ROT SHR 1` = rotate right 1. `SHD ZIN 4` = `D <<= 4` zero-fill.

---

## 9. System / misc instructions

### 9.1 IOX / IOXT (privileged) — VERIFIED

* `IOX <dev>` (164000, mask F800): `A = io_op(operand & 0x07FF, A)`; the 11-bit device address
  is embedded in the instruction. EMU `cpu_instr.c:1025-1033`; SLEIGH `nd100.slaspec:688-700`
  (bit0 = direction: even → in, odd → out).
* `IOXT` (150415 / SLEIGH 0xD10D): `A = io_op(T, A)` — device address from the T register.
  EMU `cpu_instr.c:1037-1046`; SLEIGH `nd100.slaspec:703-707`.

### 9.2 MON — monitor call, base 153000 — VERIFIED

`monitor_number = operand & 0x1FF` (9-bit, sign-extended into T if bit8 set). Loads
`reg[14][T] = monitor_number` and triggers interrupt level 14. `MON n` = transfer control to
the level-14 monitor with the sign-extended 9-bit call number in T. The number printed is the
9-bit field. EMU `cpu_instr.c:202-226`; SLEIGH `nd100.slaspec:1108-1114` (opaque pcodeop
`nd100_mon`; the operand binding is exact, internal effect per emulator).

### 9.3 WAIT / give-up-priority, base 151000 — VERIFIED

Privileged. If the interrupt system is off (`!IONI`) the CPU stops (EMU: `gA` = exit code).
Otherwise it clears the current level's PID bit — relinquishing the CPU to a lower level — and
requests a priority recalc. Model as "yield / dismiss current interrupt level". EMU `DoWAIT`
(`cpu_instr.c:1979-2007`); SLEIGH `nd100.slaspec:1973-1980`.

### 9.4 MST / MCL — masked set/clear of an internal register — VERIFIED

Bases 150300 (`MST`) / 150200 (`MCL`), mask FFF0; low nibble selects the register. Privileged.
Only STS, PID, PIE are implemented. EMU `cpu_instr.c:1784-1847`; SLEIGH `nd100.slaspec:1078,1149`.

| low nibble | reg | `MST` (set) | `MCL` (clear) |
|------------|-----|-------------|---------------|
| 01 | STS | `STS_lo \|= (A & 0xFF)` | `STS_lo &= ~(A & 0xFF)` |
| 06 | PID | `PID \|= A` | `PID &= ~A` |
| 07 | PIE | `PIE \|= A` | `PIE &= ~A` |

Other nibbles: no operation. STS masking touches only the low 8 bits.

### 9.5 ION / IOF / PON / POF / PION / PIOF / SEX / REX — VERIFIED

EMU `cpu_instr.c:1171-1229`; SLEIGH `nd100.slaspec:662-671,1185-1208,1330-1333,1502-1505`.

| Mnemonic | Opcode | Effect |
|----------|--------|--------|
| `ION`  | 150402 | `IONI = 1` (interrupts on); request PK recalc |
| `IOF`  | 150401 | `IONI = 0` (privileged) |
| `PON`  | 150410 | `PONI = 1` (paging on) |
| `POF`  | 150404 | `PONI = 0` (paging off, privileged) |
| `PION` | 150412 | `IONI = 1; PONI = 1` |
| `PIOF` | 150405 | `IONI = 0; PONI = 0` (privileged) |
| `SEX`  | 150406 | `SEXI = 1` (extended 24-bit addressing) |
| `REX`  | 150407 | `SEXI = 0` |

### 9.6 IRW / IRR — inter-register write / read — VERIFIED

Bases 153400 (`IRW`) / 153600 (`IRR`), mask FF80. Fields: `level=(op>>3)&0x0F`, `reg=op&7`.
Both privileged. The printed number is `level<<3` (raw field, octal), e.g. `IRW 120 DB` →
level = 0120>>3 = 12 (octal), register B.

* `IRW`: `reg[level][dr] = A`. EMU special cases (`cpu_instr.c:1096-1119`): A→A same level = NOP;
  P→P same level = NOP; writing STS updates only the low 8 bits.
* `IRR`: `A = reg[level][sr]`; if `sr==0` (STS) then `A = reg[level][STS] & 0xFF`.
  EMU `cpu_instr.c:1129-1145`.

SLEIGH models both as opaque pcodeops with exact operand bindings (`nd100.slaspec:710-721`);
the internal effect (special-case NOPs, STS byte masking) is per the emulator.

### 9.7 TRA / TRR — transfer to/from internal (system) register — VERIFIED (bindings)

Bases 150000 (`TRA`, read → A) / 150100 (`TRR`, write ← A), mask FFF0; low nibble picks the
register. Privileged. EMU `DoTRA`/`DoTRR` (`cpu_instr.c:1857-1946,2049-2113`); SLEIGH
`nd100.slaspec:1820-1942`.

`TRA <reg>`: `A = <internal reg>`. `TRR <reg>`: `<internal reg> = A`. Treat as reads/writes of
named CPU system registers (PID, PIE, PCR, STS, PES, PEA, IIC, IIE, ALD, …). Notable side
effects (EMU): `TRA IIC` reads then clears IIC/IID; `TRR STS` changes only the low 8 bits;
`TRR PID`/`TRR PIE`/`TRR IIE` request a priority recalc; `TRA PGC`/`TRR PCR` use the level in
A bits 3-6. SLEIGH register selectors: TRA `PANS STS OPR PSR PVL IIC PID PIE CSR ACTL ALD PES
PGC PEA … CS` (`nd100.slaspec:95`); TRR `PANC STS LMP PCR … IIE PID PIE CCL LCIL UCIL CILP …
ECCR CS` (`nd100.slaspec:98`). SLEIGH models each as a per-register pcodeop; per-register
arithmetic per the emulator.

### 9.8 The `147440` idiom = ROP NOOP — VERIFIED

`147440` decodes as a ROP word: `RAD=1`, arithmetic sub-op 6 (NOOP), `sr=SL(4)`, `dr=0`.
No register or status change. Emitted as a one-word pad. Treat as `/* nop */`.

### 9.9 Other single-purpose instructions seen in handlers

Emulator bodies (SLEIGH models most as opaque pcodeops with exact bindings; where SLEIGH is
opaque, the semantics below are **EMULATOR-AUTHORITATIVE**):

| Mnemonic | Opcode | Operation | EMU |
|----------|--------|-----------|-----|
| `TSET` | 140123 | `A = mem[T]` (alt PT); `mem[T] = 0xFFFF` (atomic test&set) | `cpu_instr.c:2442` |
| `RDUS` | 140127 | `A = mem[T]` (alt PT read, no cache) | `:2422` |
| `LBYT` | 142200 | load byte: `A = byte(T + X/2)`, X odd = low byte | `:1619` |
| `SBYT` | 142600 | store byte: A's low byte → `T + X/2` | `:1646` |
| `MIX3` | 143200 | `X = (A − 1) * 3` | `:1678` |
| `EXR sr` | 140600 | execute the instruction word in `reg[sr]` without moving P (EXR-of-EXR sets Z) | `:1951` (SL `:576`) |
| `INIT/ENTR/LEAVE/ELEAV` | 140134-140137 | stack-frame create/enter/leave (PLANC convention) | `:1529` (SL models in P-code, `:510-560,1037`) |
| `MOVEW` | 143100 | block word move, count in L (≤2048), A:D source / X:T dest | `:2474` |
| `SRB/LRB level` | 152402/152602 | store/load 8-word register block (P,X,T,A,D,L,STS,B) at X, alt PT | `:2123` (SL `:1048,1669`) |
| `NLZ/DNZ` | 151400/152000 | normalize / denormalize float | `float.c:306,349` |
| `IDENT lvl` | (op10=0x31E) | `A = ident(level6)` | (SL `:644`; EMU per IO logic) |
| `OPCOM` | 150400/0xD100 | halt to operator | (SL `:1165`) |

The PLANC stack-frame ops are fully modeled by SLEIGH in P-code (VERIFIED where both present):
`ENTR demand,err,norm` checks `B+demand-122 > SMAX[B-125]`, builds `B = STP+128`, saves
`LINK=L+1`/`PREVB`/`SMAX`/`STP` (`nd100.slaspec:542-560`); `LEAVE`: `P=mem[B-128]; B=mem[B-127]`
(`:1037-1045`); `ELEAV`: decrement LINK, store A as ERRCODE, then LEAVE (`:510-525`).

---

## 10. UNRESOLVED / discrepancies (on the record)

1. **T/X transfers `EL` address — RESOLVED to emulator (EMULATOR-AUTHORITATIVE).** The
   emulator computes a 24-bit PHYSICAL address `EL = ((T & 0xFF) << 16) | ((X + disp) & 0xFFFF)`,
   accessed via ReadPhysical/WritePhysical, bypassing the page tables (EMU `cpu_instr.c:109-130`).
   SLEIGH modelled the EA as `X + disp3` only and missed the T bank byte entirely
   (`nd100.slaspec:972-1034`, self-flagged caveat 4). The emulator form governs. See §5.

2. **`STBTX` (143307) — UNRESOLVED (both).** No dispatch entry in the emulator (would execute
   as an illegal instruction, EMU `cpu_instr.c:3424-3430`); no SLEIGH constructor (`fixed3` 0-6
   only, `nd100.slaspec:529`). Do not translate.

3. **LIN right-shift fill — RESOLVED to emulator (EMULATOR-AUTHORITATIVE).** The emulator feeds
   the `M` (link) bit into the vacated bit position (EMU `cpu_instr.c:2323-2354`). SLEIGH did not
   model the M-flag fill and treated LIN-right as plain arithmetic right (`nd100.slaspec:1532`).
   Use the emulator's M-bit link fill. See §8.

4. **SWAP modifier semantics — RESOLVED to emulator (EMULATOR-AUTHORITATIVE).** The emulator's
   plain `SWAP` performs a full register exchange (dr↔sr, with CM1 complement / CLD zero on the
   sr side; EMU `cpu_instr.c:1704-1726`). SLEIGH modelled plain/CLD/CM1+CLD SWAP as a bare
   `dst = src` copy and only its `CM1` form as an exchange (`nd100.slaspec:1780-1802`, contradicting
   its own header comment). Use the emulator exchange. See §3.3.

5. **`BSET BAC` bit source — minor discrepancy (prefer emulator).** EMU sets the target bit to
   the K accumulator (STS bit2); SLEIGH sources the carry flag STS bit6 (`nd100.slaspec:358-366`).
   Where a handler uses BSET BAC, prefer the emulator's K semantics. See §7.

6. **Opaque SLEIGH pcodeops — bindings exact, arithmetic per emulator.** SLEIGH models
   byte/BCD/float/page/MOVEW/IRR/IRW/MON/WAIT/TRA/TRR/IDENT/OPCOM as `define pcodeop` black
   boxes (`nd100.slaspec:109-189`): operand bindings are exact, but the internal arithmetic is
   not expressed as P-code and is taken from the emulator (§9). Not a disagreement — a coverage
   gap on the SLEIGH side, resolved by the emulator authority.

7. **Emulator-only / unimplemented ops — UNRESOLVED for data effect (both).**
   `CLNREENT` (140302), `CHREENTPAGES` (140303), `CLEPU` (140304) — EMU handlers are TODO
   (privilege check only, `cpu_instr.c:1416-1508`); SLEIGH reduces the page-table ops to opaque
   `nd100_page_op` pcodeops (`nd100.slaspec:440-475`). `GECO` (142700) is a no-op in the emulator.
   `IOT` (160000) executes as an illegal instruction in the emulator. ND-110 segment/byte-pointer
   ops 140500-140517 / 140700-140707 map to `unimplemented_instr`. Do not translate blindly.

---

### Source files cited

**EMU** (`~/repos/nd100x/src/cpu/`): `cpu.c` (effective address, dispatch), `cpu_instr.c` (all
instruction bodies + opcode table), `cpu_disasm.c` (mnemonic/field decode, register-letter maps),
`cpu_types.h` (register indices, status bits), `float.c` (NLZ/DNZ), `bcd.c` (ADDD/SHDE).
**SLEIGH** (`ghidra-nd100/ND-100/data/languages/`): `nd100.slaspec` (constructors, attach tables),
`nd100_memory.sinc` (addressing sub-tables), `nd100_shift.sinc` (shift decode),
`nd100_registers.sinc` (flag legend).
