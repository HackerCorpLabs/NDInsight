# ND-100 Instruction Semantics — Derived from the nd100x CPU Implementation

**Purpose.** Ground-truth reference for translating carved ND-100 disassembly into
correct pseudo-C. Every statement below is taken from the executable behavior of the
nd100x emulator CPU core, cited to `~/repos/nd100x/src/cpu/<file>:<line>`. Where the
emulator behavior is ambiguous or unimplemented it is flagged **UNRESOLVED**.

All numeric literals are **octal** unless suffixed differently (ND-100 convention).
Register width is 16 bits. Word-addressed memory. Two's-complement arithmetic.

---

## 0. Register / status model (facts)

Register index encoding (used by ROP, SKP, IRW/IRR, bit ops). Source: `cpu_types.h:163-170`.

| idx | reg | idx | reg |
|-----|-----|-----|-----|
| 0 | STS | 4 | L |
| 1 | D | 5 | A |
| 2 | P | 6 | T |
| 3 | B | 7 | X |

Disassembler letter suffixes map to these indices (`cpu_disasm.c:40,49,50`):
`DA/SA = A (5)`, `DT/ST = T (6)`, `DX/SX = X (7)`, `DD/SD = D (1)`,
`DB/SB = B (3)`, `DL/SL = L (4)`, `DP/SP = P (2)`, index 0 = STS (printed `DS`/`0`).

Status register bits (`cpu_types.h:233-245`): bit0 `PTM`, bit1 `TG`, bit2 `K` (the "one‑bit
accumulator" used by bit instructions), bit3 `Z` (error), bit4 `Q` (dynamic overflow),
bit5 `O` (static overflow, sticky), bit6 `C` (carry), bit7 `M` (multi‑shift link).
Bits 8-11 = program level, bit14 `PONI`, bit15 `IONI` (these live in the shared high
byte `reg_STS`).

**Carry/overflow core** — `do_add(a,b,k)` (`cpu_instr.c:82-105`): computes `tmp = a+b+k`
as 32-bit; sets `C` if bit16+ nonzero; sets `O` **and** `Q` on signed overflow (operands
same sign, result different sign); clears `Q` otherwise; **`O` is sticky — never cleared
here**. Returns low 16 bits. This is the shared adder used by ADD/SUB/AAx/RADD/RSUB/MIN.

**Sign extension of 8-bit fields** — `signExtend(x)` (`cpu_instr.c:70-79`): treats bit7 as
sign, fills bits 8-15. Used for every 8-bit displacement/argument.

---

## 1. Translation cheat-sheet (most common forms in MON handlers)

`P` below = address of the instruction word itself. `disp` = 8-bit displacement,
sign-extended. `mem[]` = word memory. `ind()` = one indirect word fetch.

| Disassembly | C meaning |
|-------------|-----------|
| `LDA disp`        | `A = mem[P + disp]` |
| `LDA ,B disp`     | `A = mem[B + disp]` |
| `LDA ,X disp`     | `A = mem[X + disp]` |
| `LDA I disp`      | `A = mem[ mem[P + disp] ]` |
| `LDA I ,B disp`   | `A = mem[ mem[B + disp] ]` |
| `LDA ,X ,B disp`  | `A = mem[B + X + disp]` |
| `LDA I ,X disp`   | `A = mem[ X + mem[P + disp] ]` |
| `LDA I ,B ,X disp`| `A = mem[ X + mem[B + disp] ]` |
| `STA …` / `STT` / `STX` | `mem[EA] = A / T / X` |
| `STZ …`           | `mem[EA] = 0` |
| `STD I ,B ,X disp`| `EA = X + mem[B+disp]; mem[EA]=A; mem[EA+1]=D` |
| `LDD ,B disp`     | `A = mem[B+disp]; D = mem[B+disp+1]` |
| `ADD ,B disp`     | `A = do_add(A, mem[B+disp], 0)` |
| `SUB ,B disp`     | `A = do_add(A, ~mem[B+disp], 1)`  (= A − mem) |
| `AND ,B disp`     | `A &= mem[B+disp]` |
| `ORA ,B disp`     | `A \|= mem[B+disp]` |
| `MIN ,B disp`     | `t=mem[EA]+1; mem[EA]=t; if(t==0) PC++` (skip) |
| `MPY ,B disp`     | `A = (int16)A * (int16)mem[EA]` (sets O,Q) |
| `JMP …`           | `PC = EA` |
| `JPL I disp`      | `L = returnaddr; PC = mem[P+disp]` |
| `JAF disp`        | `if (A != 0) PC += disp` |
| `JAZ disp`        | `if (A == 0) PC += disp` (also sets C = (A==0)) |
| `JAN/JAP disp`    | `if (A<0)/(A>=0) PC += disp` |
| `JXZ/JXN disp`    | `if (X==0)/(X<0) PC += disp` |
| `JPC disp`        | `X++; if (X>=0) PC += disp` |
| `JNC disp`        | `X++; if (X<0)  PC += disp` |
| `SAA n`           | `A = signext8(n)` |
| `SAT/SAX/SAB n`   | `T/X/B = signext8(n)` |
| `AAA n`           | `A = do_add(A, signext8(n), 0)` |
| `AAX/AAT/AAB n`   | `X/T/B = do_add(reg, signext8(n), 0)` |
| `RADD SD DA`      | `A = do_add(A, D, 0)`  (A = A + D) |
| `RADD CLD SD DA`  | `A = D`  (COPY: dest cleared, then +source) |
| `COPY SA DX` (=`RADD CLD SA DX`) | `X = A` |
| `RSUB SX DA`      | `A = do_add(A, ~X, 1)`  (A = A − X) |
| `RADD ADC SD DA`  | `A = do_add(A, D, C)`  (add with carry) |
| `SWAP SA DT`      | `t=T; T=A; A=t`  (exchange) |
| `RCLR DA` (=`RADD CLD 0 DA` w/ sr=0) | `A = 0` |
| `RINC DA` (=`RADD AD1 0 DA`) | `A = do_add(A,0,1)` (A+1) |
| `RDCR DA` (=`RADD CM1 0 DA`) | `A = do_add(A,~0,0)` (A−1) |
| `EXIT` (=`RADD CLD SL DP`) | `PC = L`  (return) |
| `STATX n` | `phys[(T<<16)|(X + (n>>3))] = A` |
| `STZTX n` | `phys[EL] = 0` |
| `STDTX n` | `phys[EL] = A; phys[EL+1] = D` |
| `LDATX n` | `A = phys[EL]` |
| `LDXTX n` | `X = phys[EL]` |
| `LDDTX n` | `A = phys[EL]; D = phys[EL+1]` |
| `LDBTX n` | `B = 0177000 \| (2 * phys[EL])` |
| `SKP IF DA EQL ST` | `if (A == T) PC++` |
| `SKP IF DA GRE SX` | `if ((int16)A >= (int16)X) PC++` |
| `BSET ONE 5 DA`  | `A bit5 = 1` |
| `BSET ZRO 5 DA`  | `A bit5 = 0` |
| `BSKP ONE 0 DA`  | `if (A bit0 == 1) PC++` |
| `BSKP ZRO 0 DA`  | `if (A bit0 == 0) PC++` |
| `SHA n` / `SHA SHR n` | shift A left/right n places |
| `SHA ROT n` / `ZIN` / `LIN` | rotate / zero-in / link-in shift |
| `IRW 120 DB`  | `reg[level=120>>3][B] = A` (inter-level write) |
| `IRR 120 DB`  | `A = reg[level][B]` (inter-level read) |
| `147440` (`RADD` sr=SL dr=STS) | NOOP |

---

## 2. Memory-reference instructions + addressing modes

### 2.1 Effective address (THE authoritative formula)

Source: `New_GetEffectiveAddr` (`cpu.c:186-233`). `disp = signExtend(instr & 0xFF)`;
`P = (gPC - 1)` = address of the instruction word itself (PC was pre-incremented in
`do_op`, `cpu.c:162`). Selector = bits 8-10 (`(instr>>8)&7`). Disassembler string map is
`cpu_disasm.c:45`.

| bits 8-10 | disasm | Effective address `EA` |
|-----------|--------|------------------------|
| 0 | *(none)*    | `EA = P + disp` |
| 1 | `,B `       | `EA = B + disp` |
| 2 | `I `        | `EA = ind(P + disp)` |
| 3 | `I ,B `     | `EA = ind(B + disp)` |
| 4 | `,X `       | `EA = X + disp` |
| 5 | `,X ,B `    | `EA = B + X + disp` |
| 6 | `I ,X `     | `EA = X + ind(P + disp)` |
| 7 | `I ,B ,X `  | `EA = X + ind(B + disp)` |

`ind(w)` = a single indirect word fetch (`ReadIndirectVirtualMemory`, `cpu.c:205,210,223,228`).

**Order of operations (critical):** In the indexed-indirect modes 6 and 7 the indirection
happens **first**, then the X register is added to the fetched pointer (post-indexing).
There is only **one** level of indirection (no chained indirection in the emulator).
Mode 5 (`,X ,B`) is pure pre-indexing with no indirection: `B + X + disp`.

`P`-relative (mode 0) accesses the primary page table; all other modes set `use_apt=true`,
i.e. the final data access uses the alternative page table. This distinction rarely
matters for pseudo-C data-flow but is noted for completeness.

### 2.2 The instructions

All resolve `EA = New_GetEffectiveAddr(...)` then:

| Mnemonic | Opcode base | Operation | Source |
|----------|-------------|-----------|--------|
| `STZ` | 000000 | `mem[EA] = 0` | `cpu_instr.c:539-543` |
| `STA` | 004000 | `mem[EA] = A` | `:547-552` |
| `STT` | 010000 | `mem[EA] = T` | `:556-560` |
| `STX` | 014000 | `mem[EA] = X` | `:564-568` |
| `STD` | 020000 | `mem[EA] = A; mem[EA+1] = D` | `:572-577` |
| `LDD` | 024000 | `A = mem[EA]; D = mem[EA+1]` | `:615-621` |
| `STF` | 030000 | `mem[EA]=T; mem[EA+1]=A; mem[EA+2]=D` | `:581-587` |
| `LDF` | 034000 | `T=mem[EA]; A=mem[EA+1]; D=mem[EA+2]` | `:625-632` |
| `MIN` | 040000 | `t=mem[EA]+1; mem[EA]=t; if(t==0) PC++` | `:785-795` |
| `LDA` | 044000 | `A = mem[EA]` | `:591-595` |
| `LDT` | 050000 | `T = mem[EA]` | `:599-603` |
| `LDX` | 054000 | `X = mem[EA]` | `:607-611` |
| `ADD` | 060000 | `A = do_add(A, mem[EA], 0)` | `:799-805` |
| `SUB` | 064000 | `A = do_add(A, ~mem[EA], 1)` (= A−mem) | `:809-814` |
| `AND` | 070000 | `A &= mem[EA]` (no status change) | `:818-822` |
| `ORA` | 074000 | `A \|= mem[EA]` (no status change) | `:826-830` |
| `MPY` | 120000 | `A = (int16)A * (int16)mem[EA]`; sets Q, O on \|prod\|>32767 | `:3101-3120` |
| `JMP` | 124000 | `PC = EA` | `:917-926` |
| `JPL` | 134000 | `L = PC(return); PC = EA` | `:453-464` |
| `FAD/FSB/FMU/FDV` | 100000/104000/110000/114000 | 48-bit float T:A:D op mem[EA..EA+2] | `:834-913` |

Notes: **STD/LDD store A into the low word and D into the high word+1** (A first, then D).
`STD` writes `mem[EA]=A`, `mem[EA+1]=D`. `LDD` mirrors it. `AND`/`ORA` do **not** touch any
status bit. `SUB` is implemented as add of one's-complement + 1 (proper two's-complement
subtract, sets C/O/Q per `do_add`). `JPL` stores the return address (the word after JPL —
`gL = gPC` after PC pre-increment) into L, then jumps.

`FDV` sets `Z` on divide-by-zero (`cpu_instr.c:906-909`). Float format is 48-bit
(T=exponent/sign+mantissa hi, A, D) — see `float.c` if float translation is required
(out of scope here).

---

## 3. Register-operation (ROP) class — base 144000-147777

Dispatched by `regop` (`cpu_instr.c:1686-1769`); table entry `cpu_instr.c:3439`.
**This is the #1 source of mistranslation. Read carefully.**

### 3.1 Field layout of the 16-bit ROP word

```
bit:  10        9     8     7     6     5 4 3    2 1 0
      RAD      ADC   AD1   CM1   CLD   [  sr ]  [  dr ]
```
Extraction (`regop`): `RAD=(op>>10)&1` (`:1692`), `CM1=(op>>7)&1` (`:1693`),
`CLD=(op>>6)&1` (`:1694`), `sr=(op>>3)&7` (`:1696`), `dr=op&7` (`:1697`).
For the arithmetic sub-op the code switches on `(op & 0x0380)>>7` (bits 7,8,9), i.e. the
triple **{ADC, AD1, CM1}**; for the logical sub-op it switches on `(op & 0x0300)>>8`
(bits 8,9).

### 3.2 Operand fetch (both classes)

* `source = (sr == 0) ? 0 : reg[CurrLEVEL][sr]` — **when the source field is 0 (STS), the
  literal value 0 is used, NOT the STS register.** (`:1699`)
* `destination = CLD ? 0 : reg[CurrLEVEL][dr]` — **CLD clears the destination operand to 0
  BEFORE the operation** (does not read the dr register). (`:1700`)
* If `dr == 0` (destination = STS): logical/arithmetic ops are **suppressed** (guarded by
  `if (dr != 0)`), except the arithmetic path with `dr==0` clears carry: `C = 0`
  (`:1758-1761`). This is why `147440` (dr=0) is a NOOP that also clears C only when it
  reaches the arithmetic path — for `147440`, sub-op field = 6 = NOOP, so nothing changes.

### 3.3 Logical operations (RAD = 0), sub-op = bits 8,9

Effective only when `dr != 0`. Let `s = CM1 ? ~source : source`. (`:1704-1726`)

| bits 8,9 | mnem | operation |
|----------|------|-----------|
| 0 | `SWAP` | `t = reg[dr]; reg[dr] = s; reg[sr] = CLD ? 0 : t` — exchange (dr gets source, sr gets old dr) |
| 1 | `RAND` | `reg[dr] &= s; if (CLD) reg[dr] = 0` |
| 2 | `REXO` | `reg[dr] = CLD ? s : (reg[dr] ^ s)` (XOR) |
| 3 | `RORA` | `reg[dr] = CLD ? s : (reg[dr] \| s)` (OR) |

`SWAP` writes `s` into dr and the old dr value into sr; with CM1 the value put into dr is
complemented; with CLD the value put into sr is 0. Logical ops do **not** change C/O/Q/Z.

### 3.4 Arithmetic operations (RAD = 1), sub-op = bits 7,8,9

Effective only when `dr != 0`. `dest = CLD ? 0 : reg[dr]`, `source` per §3.2.
All go through `do_add` so **C/O/Q are affected** per §0. (`:1727-1762`)

| bits 7,8,9 | disasm | operation | note |
|------------|--------|-----------|------|
| 0 | `RADD`            | `reg[dr] = do_add(dest, source, 0)` | dest + source |
| 1 | `RADD CM1`        | `do_add(dest, ~source, 0)` | dest − source − 1 |
| 2 | `RADD AD1`        | `do_add(dest, source, 1)` | dest + source + 1 |
| 3 | `RADD AD1 CM1`    | `do_add(dest, ~source, 1)` | = **RSUB**: dest − source |
| 4 | `RADD ADC`        | `do_add(dest, source, C)` | add-with-carry |
| 5 | `RADD ADC CM1`    | `do_add(dest, ~source, C)` | subtract-with-borrow |
| 6 | NOOP | nothing | |
| 7 | NOOP | nothing | |

The disassembler prints sub-op 3 as **`RSUB`** (`cpu_disasm.c:475-477`) and sub-op 0 with
CLD as **`EXIT`** when the whole word equals 0146142 (`cpu_disasm.c:457-461`).

### 3.5 The critical example: `RADD CLD SD DA`

`sr = SD = 1 (D)`, `dr = DA = 5 (A)`, `CLD = 1`, arithmetic sub-op 0.
`dest = CLD ? 0 = 0`; `source = reg[D] = D`; `reg[A] = do_add(0, D, 0) = D`.

**Result: `A = D` (a plain copy).** This is the `COPY` idiom: `COPY <sr> <dr>` assembles to
`RADD CLD <sr> <dr>`, meaning `dst = src` (destination cleared, then source added to 0).
`do_add(0,src,0)` still updates C/O/Q as a side effect (C stays 0, O/Q cleared unless src
overflow — for a copy of a value there is no signed overflow, so O/Q are cleared, C=0).

### 3.6 Distinctions to remember

* `RADD SD DA` (no CLD) = `A = A + D`. `RADD CLD SD DA` = `A = D`. The **only** difference is
  CLD, which decides whether the destination register participates or is zeroed.
* `COPY` = `RADD CLD` (dst := src). `SWAP` = full exchange (dst↔src), a *logical* opcode,
  not arithmetic. `RADD` (no CLD) = arithmetic accumulate.
* `RCLR dr` = `RADD CLD` with `sr=0` → `dst = do_add(0,0,0) = 0`.
* `RINC dr` = `RADD AD1` with `sr=0` → `dst = dst + 1`.
* `RDCR dr` = `RADD CM1` with `sr=0` → `dst = dst + ~0 = dst − 1`.
* `EXIT` = `RADD CLD SL DP` (0146142) → `P = L` (procedure return).

### 3.7 RMPY / RDIV (same register fields, separate opcodes)

* `RMPY sr dr` (base 141200, mask FFC0, `cpu_instr.c:3061-3096`): signed 16×16→32,
  `A = high16, D = low16`; `source=(sr==0)?0:reg[sr]`, `dest=(dr==0)?0:reg[dr]`;
  sets `C` if product exceeds 16 bits. (Note: the emulator's `rmpy` sets C but the sign is
  applied after the C test; O/Q are **not** set in this active version — the older
  `rmpy_org` set O/Q. Table entry uses `rmpy`.)
* `RDIV sr` (base 141600, mask FFC0, `cpu_instr.c:2990-3018`): 32-bit dividend `(A<<16)|D`
  divided by `reg[sr]` (0 if sr=0). Quotient→A, remainder→D. Divide-by-zero or
  \|quotient\|≥32768 sets `Z` and returns without writing A/D. Sets `C` if quotient > 16 bits.

---

## 4. Argument instructions — set / add signed 8-bit argument

`arg = signExtend(operand & 0xFF)`. Sources: `cpu_instr.c:159-254`; table `:3487-3494`.

| Mnemonic | Opcode base | Operation | Source |
|----------|-------------|-----------|--------|
| `SAA n` | 170400 | `A = arg` (via setreg) | `:230-233` |
| `SAB n` | 170000 | `B = arg` | `:237-239` |
| `SAT n` | 171000 | `T = arg` | `:243-246` |
| `SAX n` | 171400 | `X = arg` | `:251-253` |
| `AAA n` | 172400 | `A = do_add(A, arg, 0)` | `:161-167` |
| `AAB n` | 172000 | `B = do_add(B, arg, 0)` | `:171-177` |
| `AAT n` | 173000 | `T = do_add(T, arg, 0)` | `:181-187` |
| `AAX n` | 173400 | `X = do_add(X, arg, 0)` | `:191-198` |

`SAx` simply loads the sign-extended argument (no status change). `AAx` uses `do_add`, so
**C/O/Q are affected**. The 8-bit field means the argument range is −200..+177 (octal).

---

## 5. T/X-indexed physical transfers — base 143300-143306

**Privileged** (each calls `CheckPriv`). Dispatched by opcode with mask 0xFFC7, so the
variable field is bits 3-5. Table `cpu_instr.c:3424-3430`.

### 5.1 The effective address `EL` (authoritative)

`calcEL(displacement)` (`cpu_instr.c:109-118`), with
`displacement = (operand >> 3) & 0x07` (`:644,657,670,694,...`):

```
EL = ( (T & 0xFF) << 16 ) | ( (X + displacement) & 0xFFFF )
EL = EL & 0xFFFFFF          // 24-bit physical address
```

So **EL is a 24-bit PHYSICAL address**: high 8 bits = `T & 0xFF`, low 16 bits =
`(X + displacement) & 0xFFFF`. The `displacement` is the 3-bit field (0-7), taken from bits
3-5 of the instruction. In listings the suffix is the raw octal of that field region, e.g.
`LDDTX 20` → `020 octal >> 3 = 2` → `calcEL(2)` (see the SETPT microcode transcription,
`cpu_instr.c:1287,1296,1308`). Access is via `ReadPhysicalMemory`/`WritePhysicalMemory`
(`ReadEL`/`WriteEL`, `cpu_instr.c:121-130`) — **it bypasses the page tables**.

### 5.2 The instructions

| Mnemonic | Opcode | Operation | Source |
|----------|--------|-----------|--------|
| `LDATX n` | 143300 | `A = phys[EL]` | `:689-698` |
| `LDXTX n` | 143301 | `X = phys[EL]` | `:711-720` |
| `LDDTX n` | 143302 | `A = phys[EL]; D = phys[EL+1]` | `:733-745` |
| `LDBTX n` | 143303 | `B = 0177000 \| ((2 * phys[EL]) & 0xFFFF)` | `:759-773` |
| `STATX n` | 143304 | `phys[EL] = A` | `:652-661` |
| `STZTX n` | 143305 | `phys[EL] = 0` | `:639-648` |
| `STDTX n` | 143306 | `phys[EL] = A; phys[EL+1] = D` | `:665-675` |

Notes:
* `LDDTX`/`STDTX` operate on the pair `(EL, EL+1)`, low word first. `STDTX` stores A into
  `EL` and D into `EL+1` (matches STD's A-then-D convention).
* `LDBTX` doubles the fetched word (`2*phys[EL]`, wrapped to 16 bits) and ORs in `0177000`
  (`0xFE00`). This forms a page-table-entry pointer with the top bits forced set. The ND
  hardware had a documented bug here (see `cpu_instr.c:1272,1300`) — the value is used as an
  in-page-table address by CLEPT/SETPT.
* Opcodes 143307 (`STBTX`) and the `n`-field pattern: the emulator implements 143300-143306;
  there is **no `STBTX` (143307) handler** in the table (`cpu_instr.c:3424-3430`). If a carve
  contains 143307, treat as **UNRESOLVED (emulator: no dispatch entry; would fall through to
  illegal_instr)**.

---

## 6. Conditional jumps & the SKP class

### 6.1 Register-conditional jumps (CJP family), base 130000-133777

Target is **P-relative**: `PC = do_add(P, signExtend(disp8), 0)` where `P` = address of the
jump instruction (`CJP`, `cpu_instr.c:331-343`). Range −200..+177 (octal). Table
`:3285-3306`.

| Mnemonic | Opcode base | Condition to jump | Source |
|----------|-------------|-------------------|--------|
| `JAP d` | 130000 | `A >= 0` (bit15 == 0) | `:352-356` |
| `JAN d` | 130400 | `A < 0`  (bit15 == 1) | `:365-369` |
| `JAZ d` | 131000 | `A == 0` — **also sets `C = (A==0)`** | `:378-384` |
| `JAF d` | 131400 | `A != 0` (filled) | `:393-396` |
| `JPC d` | 132000 | `X++` first, then jump if `X >= 0` | `:406-411` |
| `JNC d` | 132400 | `X++` first, then jump if `X < 0` | `:421-425` |
| `JXZ d` | 133000 | `X == 0` | `:446-449` |
| `JXN d` | 133400 | `X < 0` (bit15 == 1) | `:434-437` |

`JPC`/`JNC` **always** increment X (even when the branch is not taken). `JAZ` has the side
effect of writing the carry bit. The others touch no status.

### 6.2 SKP — skip on register comparison, base 140000 (mask F8C0)

`ndfunc_skp` → `IsSkip` (`cpu_instr.c:2197-2254`), dispatch `:3309`. Disassembly
`SKP IF <dst> <cond> <src>` (`cpu_disasm.c:181`). Fields: `sr=(instr>>3)&7`, `dr=instr&7`;
`source=(sr==0)?0:reg[sr]`, `desti=(dr==0)?0:reg[dr]` (STS never read, 0 used instead,
`:2204-2205`). If the condition holds, **skip the next instruction** (`gPC++`, `:471-472`).

Flags computed on `desti` vs `source` (`:2210-2215`): `z = (desti==source)`;
signed `sgr = (int16)desti − (int16)source`; `o = signed-overflow(sgr)`;
`c = ((desti−source) < 0) ? 0 : 1` (unsigned "≥"); `s = bit15 of (int16)(desti−source)`.

Condition = bits 8-10 (`(instr>>8)&7`), mnemonics `cpu_disasm.c:48`:

| bits 8-10 | disasm | skip if |
|-----------|--------|---------|
| 0 | `EQL`  | `desti == source` |
| 1 | `GEQ`  | `!s`  (signed desti ≥ source, sign-only test) |
| 2 | `GRE`  | `!(s ^ o)`  (signed desti ≥ source, overflow-correct) |
| 3 | `MGRE` | `c`   (unsigned desti ≥ source) |
| 4 | `UEQ`  | `desti != source` |
| 5 | `LSS`  | `s`   (signed desti < source) |
| 6 | `LST`  | `s ^ o`  (signed less, overflow-correct) |
| 7 | `MLST` | `!c`  (unsigned desti < source) |

Example: `SKP IF DA EQL ST` → dr=A, sr=T → `if (A == T) PC++`.
`SKP IF DA GRE SX` → `if ((int16)A >= (int16)X) PC++`.
The disassembler order is **dst then src**; in C the comparison reads `desti <cond> source`.

---

## 7. Bit instructions — base 174000-177777

`do_bops` (`cpu_instr.c:2256-2321`), dispatch `:3496`. Fields: `bn=(op>>3)&0x0F`
(bit number 0-15), `dr=op&7` (register). Sub-op = bits 7-10 (`(op & 0x0780)>>7`). `K` = STS
bit2, the one-bit accumulator. Disassembly `<bop> <bitnum> D<reg>` (`cpu_disasm.c:653-656`);
when `dr==0` (STS) the bit is named symbolically (`SSK`, `SSZ`, …, `cpu_disasm.c:52`).

| sub-op | disasm | operation |
|--------|--------|-----------|
| 0 | `BSET ZRO b Dr` | `reg[dr] bit b = 0` |
| 1 | `BSET ONE b Dr` | `reg[dr] bit b = 1` |
| 2 | `BSET BCM b Dr` | `reg[dr] bit b ^= 1` (complement) |
| 3 | `BSET BAC b Dr` | `reg[dr] bit b = K` |
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

So `BSKP ZRO 0 DA` = `if ((A & 1)==0) PC++`; `BSET ONE 5 DA` = `A |= (1<<5)`.
The bit number is bits 3-6 (4 bits, 0-15); the register is bits 0-2. When `dr==0` the target
is the STS register bit (e.g. `SSK` sets the K bit).

---

## 8. Shift instructions — base 154000-155777

`ndfunc_shifts` (`cpu_instr.c:258-281`) selects the target by bits 7-8
(`(op>>7)&3`): 0=`SHT`(T), 1=`SHD`(D), 2=`SHA`(A), 3=`SAD`(A:D 32-bit pair). Dispatch
`:3477-3480`. The work is `ShiftReg` (`:2323-2354`) / `ShiftDoubleReg` (`:2356-2387`).

Field decode (`ShiftReg`): `isneg=(op>>5)&1` (bit5 = direction sign); when `isneg` the count
is `(~((op&0x3F)|0xFFC0)+1)` i.e. the magnitude of a 6-bit two's-complement count; otherwise
count = `op & 0x3F`. **`isneg` set = shift RIGHT** (disassembler prints `SHR <n>`,
`cpu_disasm.c:577`); clear = shift LEFT. Shift type = bits 9-10 (`(op>>9)&3`).

| type (bits 9,10) | disasm | left (isneg=0) fill of bit0 | right (isneg=1) fill of bit15 |
|------------------|--------|-----------------------------|-------------------------------|
| 0 | *(plain)* | 0 (arithmetic/logical left) | **sign** (msb preserved → arithmetic right) |
| 1 | `ROT` | rotated-out bit → bit0 | rotated-out bit → bit15 |
| 2 | `ZIN` | 0 | 0 |
| 3 | `LIN` | `M` bit → bit0 | `M` bit → bit15 |

The shift runs one bit position per iteration for `count` iterations. The last bit shifted
out is written to the `M` status bit (`setbit(_STS,_M,tmp)`, `:2352`). For **plain right
shift** the sign bit (msb) is replicated, so type-0 right shift is **arithmetic**; type-0
left shift feeds 0 into bit0. `SAD` does the same over the 32-bit `(A<<16)|D` value
(`ShiftDoubleReg`), writing A/D back (`:274-277`).

Example: `SHA 3` = `A <<= 3` (zero-filled). `SHA SHR 3` = arithmetic `A >>= 3` (sign kept).
`SHA ROT SHR 1` = rotate right 1. `SHD ZIN 4` = `D <<= 4` zero-fill (same as plain here).

---

## 9. System / misc instructions

### 9.1 IOX / IOXT (privileged), base 164000 / opcode 150415

* `IOX <dev>` (164000, mask F800): `A = io_op(operand & 0x07FF, A)`. The 11-bit device
  address is embedded in the instruction. (`cpu_instr.c:1025-1033`)
* `IOXT` (150415): `A = io_op(T, A)` — device address taken from the **T register**.
  (`cpu_instr.c:1037-1046`)
Both first test the "in-memory IO" window 100000-100777 via `UpdateMemoryIO`
(`cpu_instr.c:982-1001`). Both privileged.

### 9.2 MON — monitor call, base 153000 (mask FF00)

`ndfunc_mon` (`cpu_instr.c:202-226`): `monitor_number = operand & 0x1FF` (9-bit, **sign-
extended** into T if bit8 set). Loads `reg[14][T] = monitor_number` and triggers interrupt
level 14 (`interrupt(14, 1<<1)`). In C terms: `MON n` transfers control to the level-14
monitor with the call number (sign-extended 9-bit) in T. The number printed in disassembly
is the 9-bit field.

### 9.3 WAIT / give-up-priority, base 151000 (mask FF00)

`DoWAIT` (`cpu_instr.c:1979-2007`), privileged. If the interrupt system is off (`!IONI`)
the emulator stops the CPU (`gA` = exit code). Otherwise it clears the current level's PID
bit (`gPID &= ~(1<<CurrLEVEL)`) — relinquishing the CPU to a lower level — and requests a
priority recalculation. Level 0 cannot go lower (returns). For pseudo-C, model `WAIT` as
"yield / dismiss current interrupt level".

### 9.4 MST / MCL — masked set/clear of an internal register

Opcode bases 150300 (`MST`) / 150200 (`MCL`), mask FFF0; low nibble selects the register.
`DoMST` (`cpu_instr.c:1821-1847`) / `DoMCL` (`:1784-1810`), privileged. Only STS, PID, PIE
are implemented.

| low nibble | reg | `MST` (set) | `MCL` (clear) |
|------------|-----|-------------|---------------|
| 01 | STS | `STS_lo \|= (A & 0xFF)` | `STS_lo &= ~(A & 0xFF)` |
| 06 | PID | `PID \|= A` | `PID &= ~A` |
| 07 | PIE | `PIE \|= A` | `PIE &= ~A` |

Other nibbles: no operation. So `MST PID` = "set the PID bits named by A"; `MCL PIE` = "clear
the PIE bits named by A". STS masking touches only the low 8 bits.

### 9.5 ION / IOF / PON / POF / PION / PIOF — interrupt & paging control

Single opcodes (`cpu_instr.c:1171-1229`; table `:3445-3461`).

| Mnemonic | Opcode | Effect |
|----------|--------|--------|
| `ION`  | 150402 | `IONI = 1` (interrupt system on); request PK recalc |
| `IOF`  | 150401 | `IONI = 0` (privileged) |
| `PON`  | 150410 | `PONI = 1` (paging on) |
| `POF`  | 150404 | `PONI = 0` (paging off, privileged) |
| `PION` | 150412 | `IONI = 1; PONI = 1` |
| `PIOF` | 150405 | `IONI = 0; PONI = 0` (privileged) |
| `SEX`  | 150406 | `SEXI = 1` (extended 24-bit addressing) |
| `REX`  | 150407 | `SEXI = 0` |

### 9.6 IRW / IRR — inter-register write / read

Bases 153400 (`IRW`) / 153600 (`IRR`), mask FF80. Fields: `level=(op>>3)&0x0F`, `reg=op&7`.
Both **privileged**. Disassembly `IRW <op&0x78 octal> D<reg>` (`cpu_disasm.c:566-570`) — note
the printed number is `level<<3` (the raw field, octal), e.g. `IRW 120 DB` → level = 0120>>3
= 12 (octal), register B.

* `IRW` (`cpu_instr.c:1096-1119`): `reg[level][dr] = A`. Special cases: writing A→A on the
  **same** level is a NOP; writing P→P on the same level is a NOP; writing STS updates only
  the low 8 bits (`reg[level][STS] = A & 0xFF`).
* `IRR` (`cpu_instr.c:1129-1145`): `A = reg[level][sr]`; if `sr==0` (STS) then
  `A = reg[level][STS] & 0xFF` (high byte cleared).

So `IRW 120 DB` = `reg[level 12][B] = A`; `IRR 120 DB` = `A = reg[level 12][B]`.

### 9.7 TRA / TRR — transfer to/from internal (system) register

Bases 150000 (`TRA`, read → A) / 150100 (`TRR`, write ← A), mask FFF0; low nibble picks the
register. **Privileged.** `DoTRA` (`cpu_instr.c:1857-1946`), `DoTRR` (`:2049-2113`). Register
names `cpu_disasm.c:42-43`.

`TRA <reg>`: `A = <internal reg>`. Notable: `TRA STS` (01) merges the level STS low byte with
the shared system high byte; `TRA PGS`/`TRA PEA` have unlock side effects; `TRA IIC` (05)
reads then clears IIC/IID; `TRA PGC`/`TRA PCR` (014) reads the paging control register for a
level encoded in A bits 3-6.

`TRR <reg>`: `<internal reg> = A`. Notable: `TRR STS` (01) changes only the low 8 bits;
`TRR PID`/`TRR PIE`/`TRR IIE` request a priority recalc; `TRR PCR` (03) writes the paging
control register for the level in A bits 3-6.

For pseudo-C, treat `TRA`/`TRR` as reads/writes of named CPU system registers (PID, PIE,
PCR, STS, PES, PEA, IIC, IIE, ALD, …). Full per-register semantics are in the cited functions.

### 9.8 The `147440` idiom = ROP NOOP

`147440` decodes as a ROP word: `RAD=1`, arithmetic sub-op = 6 (NOOP), `sr=SL(4)`, `dr=0`.
It performs no register or status change (§3.2/§3.4). Emitted by assemblers/compilers as a
one-word pad. Treat as `/* nop */`.

### 9.9 Other single-purpose instructions seen in handlers

| Mnemonic | Opcode | Operation | Source |
|----------|--------|-----------|--------|
| `TSET` | 140123 | `A = mem[T]` (alt PT); `mem[T] = 0xFFFF` (atomic test&set) | `cpu_instr.c:2442-2449` |
| `RDUS` | 140127 | `A = mem[T]` (alt PT read, no cache) | `:2422-2425` |
| `LBYT` | 142200 | load byte: `A = byte(T + X/2)`, X odd = low byte | `:1619-1634` |
| `SBYT` | 142600 | store byte: writes A's low byte into `T + X/2` | `:1646-1662` |
| `MIX3` | 143200 | `X = (A − 1) * 3` | `:1678-1681` |
| `EXR sr` | 140600 | execute the instruction word held in `reg[sr]` without moving P (EXR-of-EXR sets Z) | `:1951-1971` |
| `INIT/ENTR/LEAVE/ELEAV` | 140134-140137 | stack-frame create/enter/leave (PLANC calling convention) | `:1529-1603` |
| `MOVEW` | 143100 | block word move, count in L (≤2048), A:D source / X:T dest | `:2474-2586` |
| `SRB/LRB level` | 152402/152602 | store/load 8-word register block (P,X,T,A,D,L,STS,B) at X, alt PT | `:2123-2195` |
| `NLZ/DNZ` | 151400/152000 | normalize / denormalize float (scaling in low 8 bits) | `float.c:306,349` |

---

## 10. UNRESOLVED / partial in the emulator (do not translate blindly)

* `STBTX` (143307): **no dispatch entry**; would execute as illegal instruction. UNRESOLVED
  (emulator: not in table, `cpu_instr.c:3424-3430`).
* `CLNREENT` (140302), `CHREENTPAGES` (140303), `CLEPU` (140304): handlers present but
  bodies are **TODO / not implemented** (`cpu_instr.c:1416-1508`). Behavior UNRESOLVED
  (emulator: privilege check only, no data effect).
* `GECO` (142700): customer instruction, **no-op** in the emulator (`cpu_instr.c:930-944`).
* ND-110-specific segment/byte-pointer instructions 140500-140517, 140700-140707 map to
  `unimplemented_instr` (print + continue) — UNRESOLVED for data effect.
* `IOT` (160000): implemented as illegal instruction (`cpu_instr.c:1011-1021`).

All other classes in §§1-9 are **fully resolved** against the emulator source.

---

### Source files cited
`~/repos/nd100x/src/cpu/cpu.c` (effective address, dispatch),
`~/repos/nd100x/src/cpu/cpu_instr.c` (all instruction bodies + opcode table),
`~/repos/nd100x/src/cpu/cpu_disasm.c` (mnemonic/field decode, register-letter maps),
`~/repos/nd100x/src/cpu/cpu_types.h` (register indices, status-bit macros, WriteMode),
`~/repos/nd100x/src/cpu/float.c` (NLZ/DNZ), `~/repos/nd100x/src/cpu/bcd.c` (ADDD/SHDE).
