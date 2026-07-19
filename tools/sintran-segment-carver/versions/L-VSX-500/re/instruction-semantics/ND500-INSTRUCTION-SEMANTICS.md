# ND-500 Instruction Semantics Reference

Authoritative reference for translating carved ND-500 (32-bit) disassembly
into correct pseudo-C. Every semantic statement below is grounded in the
`nd500x` emulator source (the executable ground truth for what each
instruction *does*) or the ND-500 Reference Manual bundled with it. The
disassembly *notation* (the `r.`, `b.`, `$`, `:=`, `if << go` forms our
`.ASM` files use) is defined by the `ragge` `nd500-dis` instruction table.

**Scope**: the ND-500 (32-bit) instruction set, prioritizing the mnemonics
that actually appear in the carved S3SM5 MON handlers
(`410B`, `411B`, `412B`, `413B`, `416B`, `417B` under
`../mon-analysis/`). Every mnemonic occurring in those six exemplar `.ASM`
files is covered (see cheat sheet).

## Sources cited

| Tag | File | What it establishes |
|-----|------|---------------------|
| **REGS** | `~/repos/nd500x/src/cpu/cpu_protos.h:43-99` | Register model (`Nd500Cpu` struct) |
| **FLAGS** | `~/repos/nd500x/src/cpu/instruction_helpers.h:305-314` | Status-flag bit positions in `ST1` |
| **DEC** | `~/repos/nd500x/src/cpu/cpu_instr.c:302-324` | address_code -> addressing mode |
| **EA** | `~/repos/nd500x/src/cpu/cpu_instr.c:664-772` | effective-address computation |
| **FMT** | `~/repos/nd500x/src/disasm/nd500_disasm.c:140-324` | operand notation / register banks |
| **TRAP** | `~/repos/nd500x/src/cpu/cpu_protos.h:101-145` | trap bit definitions |
| **DOMAIN** | `~/repos/nd500x/src/cpu/nd500_domain.h` | domain / stack-limit registers |
| per-instruction | `~/repos/nd500x/src/cpu/instructions/<CLASS>/<Name>.c` | exact operation of that instruction |
| **TAB** | `~/repos/ragge/pcc-nd500/src/nd500-dis/nd500_instructions.h` | opcode -> disassembler mnemonic |
| **MANUAL** | `~/repos/nd500x/docs/ND-05.009.4 EN ND-500 Reference Manual.md` | official manual (section numbers) |

NOTE on tooling: our carves are disassembled with `ragge` `nd500-dis`; its
notation differs from `nd500x`'s built-in disassembler (e.g. `ragge` prints
`@b.N` and `DESC(rN)` where `nd500x` prints `IND(b.N)` and `DESCn(...)`).
The two share the same opcode->mnemonic table origin, and the *semantics*
are `nd500x`'s. Where the two disagree on wording this document follows the
`ragge` output because that is what the `.ASM` files contain.

---

## 1. Cheat sheet — mnemonics appearing in the carved S3SM5 handlers

`t` = data-type prefix (`bi`,`by`,`h`,`w`,`f`,`d`); `n` = register index 1-4
shown as e.g. `w3`, `f1`, `d4`. `src`/`dst` = an operand resolved per the
addressing-mode table (Section 3).

| nd500-dis form | One-line C meaning | Section |
|----------------|--------------------|---------|
| `tn := src`            | `Rn = src;` (load, zero-extend sub-word) | 5.1 |
| `tn =: dst`            | `dst = Rn;` (store) | 5.2 |
| `t stz dst`            | `dst = 0;` | 5.3 |
| `tn clr`               | `Rn = 0;` | 5.4 |
| `t move src,dst`       | `dst = src;` | 5.5 |
| `tn neg`               | `Rn = -Rn;` | 5.6 |
| `t set1 dst`           | `dst = 1;` | 5.7 |
| `tn + src`             | `Rn = Rn + src;` | 6.1 |
| `tn - src`             | `Rn = Rn - src;` | 6.1 |
| `tn * src`             | `Rn = Rn * src;` | 6.1 |
| `tn mulad x,y`         | `Rn = Rn * x + y;` | 6.2 |
| `tn comp src`          | set flags from `Rn - src` (Rn unchanged) | 6.3 |
| `t comp2 a,b`          | set flags from `a - b` (nothing stored) | 6.3 |
| `t test src`           | set Z/S from `src` vs 0; C=1 (int) | 6.4 |
| `tn lind idx,lo,hi`    | `Rn = idx;` then bounds-check -> K/IX | 6.5 |
| `tn cind idx,lo,hi`    | bounds-check `idx` in `[lo,hi]` -> K/IX (no load) | 6.5 |
| `go d`                 | `goto pc+d;` (unconditional) | 7.1 |
| `if = go d`            | `if (Z) goto pc+d;` (equal) | 7.2 |
| `if >< go d`           | `if (!Z) goto pc+d;` (not equal) | 7.2 |
| `if > go d`            | `if (!S && !Z) goto pc+d;` (signed >) | 7.2 |
| `if < go d`            | `if (S) goto pc+d;` (signed <) | 7.2 |
| `if >= go d`           | `if (!S) goto pc+d;` (signed >=) | 7.2 |
| `if <= go d`           | `if (S \|\| Z) goto pc+d;` (signed <=) | 7.2 |
| `if >> go d`           | `if (C && !Z) goto pc+d;` (unsigned >) | 7.2 |
| `if >>= go d`          | `if (C) goto pc+d;` (unsigned >=) | 7.2 |
| `if << go d`           | `if (!C) goto pc+d;` (unsigned <) | 7.2 |
| `if <<= go d`          | `if (!C \|\| Z) goto pc+d;` (unsigned <=) | 7.2 |
| `if -k go d`           | `if (!K) goto pc+d;` | 7.2 |
| `entsn dem,maxargs`    | build stack frame; enter subroutine (bounded argc) | 8.2 |
| `entf area`            | enter subroutine using fixed (static) data area | 8.2 |
| `init bos,dmain,dtot`  | initialize stack: set B, TOS, frame header | 8.1 |
| `retd`                 | `PC = L;` (return, no frame unwind) | 8.3 |
| `noop`                 | do nothing | 5.8 |
| `bp`                   | breakpoint trap (BPT) / illegal-instruction | 5.9 |
| `??? ; opcode 0xNN`    | NOT an instruction — mis-aligned byte (see 9) | 9 |

The `DESC(rN)` wrapper on an operand (e.g. `by1 := DESC(r3) $0xF`) is an
operand *prefix*, not a separate instruction; see Section 3.4.

---

## 2. Register model  (REGS, DOMAIN)

All ND-500 programmable registers are **32-bit**; the machine is
**big-endian** (FMT lines 145-149; carve headers).

### General / accumulator registers (REGS `cpu_protos.h:47-50`)

| Name | Struct field | Role |
|------|-------------|------|
| `I1..I4` | `I[0..3]` | integer registers; targets of BY/H/W ops. In disasm shown as `W1..W4` for BYTE/HALFWORD/WORD dtypes (FMT 202-217). |
| `A1..A4` | `A[0..3]` | float accumulators; shown as `F1..F4` for FLOAT dtype. |
| `E1..E4` | `E[0..3]` | float extension; `A`+`E` pair forms the 64-bit `D1..D4` double register shown for DOUBLEWORD dtype. |

So a disasm token like `w3` = integer register `I3`; `f2` = float `A2`;
`d4` = double `A4:E4`. The **register index and data type are encoded in
the opcode**, not in an operand — see the per-instruction opcode ranges.

### Addressing / linkage registers (REGS `cpu_protos.h:52-54`)

| Name | Field | Role |
|------|-------|------|
| `L` | `L` | Link register — holds return address (set by CALL; restored to PC by RETD). |
| `B` | `B` | Local base — base for `b.` (LOCAL) operands and current stack frame. |
| `R` | `R` | Record base — base for `r.` (RECORD) operands. |
| `PC` | `PC` | Program counter (called `P` in the manual). |

### Stack-limit / domain registers (REGS `cpu_protos.h:56-68`, DOMAIN)

`TOS` (top-of-stack limit), `LL` (lower limit), `HL` (higher limit),
`THA` (trap handler address), `CED` (current executing domain),
`CAD` (current alternative domain), `PS`, `PSTP`, `DITBASE`, plus control
registers `OTE1/2 CTE1/2 MTE1/2 TEMM1/2`. Stack frame layout used by the
ENT*/INIT/RET* family (per DOMAIN `nd500_domain.h:38-51` and
`CALL/Entsn.c`, `CONTROL/Init.c`):

```
B+0   PREVB   previous B          B+12  AUX   auxiliary
B+4   RETA    return address      B+16  N     argument count
B+8   SP      stack pointer       B+20+ ARGn  argument addresses
```

### Status register `ST1` — flag bit positions (FLAGS)

| Flag | Bit | Meaning |
|------|-----|---------|
| `Z` | 5 | zero |
| `C` | 6 | carry — **ND-500 convention: C=1 means NO borrow** (i.e. `a >= b` after a subtract/compare). See `COMPARE/Comp.c`, `ARITHMETIC/Sub2.c`. |
| `S` | 7 | sign (result negative) |
| `K` | 8 | "destination full" / index-out-of-bounds condition flag |
| `O` | 9 | overflow |

Higher `ST1` bits hold ignorable-trap status bits, e.g. `IX` (Illegal
Index) = bit 26, `STO` (Stack Overflow) = bit 27 (TRAP; used by LIND/CIND
and the ENT*/INIT family).

---

## 3. Addressing modes / operand notation  (DEC, EA, FMT)

The first byte after the opcode is the operand **address_code**; DEC maps
it to a mode, EA computes the effective address. Displacements in
LOCAL/RECORD/PREINDEXED modes are **UNSIGNED** (FMT 7-30, EA 638-654,
MANUAL §8.4). Branch displacements are **SIGNED** (Section 7).

### 3.1 nd500-dis notation seen in the carves

| Disasm token | Mode (DEC) | Effective address / value (EA) |
|--------------|-----------|--------------------------------|
| `$K` (small) | CONSTANT_SHORT | immediate 6-bit **signed** constant (FMT 178-184) |
| `$K` (with data) | CONSTANT / ABSOLUTE | immediate literal, or absolute address `$addr` (FMT 276-299) |
| `$<double>` | CONSTANT (8-byte) | 64-bit immediate literal |
| `b.D` | LOCAL (0xC1-0xC3) | `B + D` (D unsigned) — EA 691-697 |
| `b.D` (short) | LOCAL_SHORT (top=0x01) | `B + low6*4` — EA 699-702 |
| `r.D` | RECORD (0xC9-0xCB) | `R + D` (D unsigned) — EA 704-707 |
| `r.D` (short) | RECORD_SHORT (top=0x02) | `R + low6*4` — EA 709-712 |
| `rN.(D)` | PREINDEXED (0xF4-0xFF) | `I[N] + D` (D unsigned) — EA 714-722 |
| `@b.D` | LOCAL_IND (0xC5-0xC7) | `mem32[B + D]` (indirect) — EA 736-741 |
| `@b.D(rN)` | LOCAL_IND_PI (0xE4-0xEF) | `mem32[B+D] + I[N]*scale` — EA 750-772 |
| `b.D(rN)` | LOCAL_PI (0xD4-0xDF) | `B + D + I[N]*scale` |
| `$addr(rN)` | ABSOLUTE_PI (0xE0-0xE3) | `addr + I[N]*scale` |
| `Wn/Fn/Dn` (as operand) | REGISTER (0xD0-0xD3) | the register itself (no memory) — DEC 316 |

`scale` in post-indexed modes = data-element size (BYTE=1, HALF=2, WORD=4,
DOUBLE=8) — EA 759-772. For BIT dtype the index is a *bit* offset and sets
`bit_position` (EA 762-772).

### 3.2 Register bank shown for REGISTER-mode operands (FMT 202-217)

FLOAT dtype -> `F1..F4`; DOUBLEWORD -> `D1..D4`; everything else -> `W1..W4`.

### 3.3 Data-type prefix letters

`bi`=BIT, `by`=BYTE(8), `h`=HALFWORD(16), `w`=WORD(32), `f`=FLOAT(32),
`d`=DOUBLEWORD(64). Loads of sub-word types zero-extend into the 32-bit
register (`MOVE/AssignTo.c` description).

### 3.4 `DESC(rN)` and `ALT(...)` operand prefixes (FMT 151-162, DEC 320)

address_code `0xF0-0xF3` is a **descriptor prefix** (`DESC(rN)`), `0xC8` an
**alternative-domain prefix** (`ALT(...)`). These wrap the following
operand; they change how the operand is fetched (descriptor / alternate
domain) but are not standalone instructions. In carved output such as
`by1 := DESC(r3) $0xF`, the `DESC(r3)` decorates the source operand of the
`:=` load.

---

## 4. The MON / subroutine-call mechanism

**There is no ND-500 "MON" opcode.** The SINTRAN MON numbers (e.g. 410B
FixInMemory) are ND-100-side monitor-call numbers. Their ND-500 *handler
bodies* live in the S3SM5 in-memory segment and are reached through
SINTRAN's own software dispatch: the `0x60` MON vector table in
`030-S3SM5.bin` (vector slot = `0x60 + 2*<octal MON no.>`), as documented
in each carve's header and folder README. The ND-500 CPU reaches them by an
ordinary subroutine transfer, not a dedicated trap opcode.

ND-500 **subroutine linkage** proper is CALL/CALLG + one of the ENT*
entry-point instructions + a RET* return (`CALL/Call.c` lines 95-140):

- `CALL`/`CALLG` push nothing yet; they record the return address in `L` and
  a pending arg list, then transfer to the callee entry point. The callee's
  first instruction **must** be an ENT* (ISE trap otherwise — `CALL/Call.c`
  130-140).
- The `nd500x` CALL path also intercepts *indirect segment calls* that
  implement SINTRAN MON dispatch and completes them in-line
  (`CALL/Call.c:105-123`, `INDIRECT_HANDLED`).
- ENT* builds the stack frame (Section 8); RET* tears it down / restores PC.

`JUMPS`/`JUMPG` are unconditional absolute jumps (`BRANCH/Jumps.c`).
NOTE: the manual-mapping table (`INSTRUCTION_DOCUMENTATION_STATUS.md`) maps
`jumps` to MANUAL §16.34 "jump to supervisor", but `nd500x`'s `Jumps.c`
implements it as an ordinary absolute jump. UNRESOLVED (documentation vs
implementation conflict); neither appears in the six exemplars.

---

## 5. Load / store / move / clear class  (MOVE, CONTROL)

### 5.1 `tn := src`  — AssignTo (load register)
`MOVE/AssignTo.c`; MANUAL §10.1. Opcodes 0xFC04-07 (BI), 0x0004-07 (BY),
0x0008-0B (H), 0x000C-0F (W), 0x0010-13 (F), 0x0014-17 (D).
Operation: `Rn = src`, right-justified; sub-word (BI/BY/H) types
**zero-fill** the upper register. Flags: Z, S set from the loaded value.
Register/dtype come from the opcode. C pseudo: `I3 = mem[R+0xE8];`

### 5.2 `tn =: dst`  — AssignFrom (store register)
`MOVE/AssignFrom.c`; MANUAL §10.4. Opcodes 0x001C-1F (BY), 0x0020-23 (W),
0x0024-27 (F), 0x0028-2B (D), 0xFC0C-0F (BI), 0xFC10-13 (H).
Operation: `dst = Rn` (datatype-sized). Source register unaffected.
Flags: Z, S from the stored value. C pseudo: `mem[R+0x34] = I2;`

Related (not in exemplars but same family): `b:=`/`r:=` load B/R from a
source (AssignToBaseReg semantics reversed), `b=:`/`r=:` store B/R
(`MOVE/AssignToBaseReg.c` §10.5, `AssignToRecordReg.c` §10.6).

### 5.3 `t stz dst`  — Stz (store zero)
`MOVE/Stz.c`. Opcodes 0x0048 (BY), 0x0049 (H), 0x004A (W), 0x004B (F),
0x004C (D), 0xFC85 (BI). Operation: `dst = 0`. Flags: Z=1, S=0.
C pseudo: `mem[...] = 0;`

### 5.4 `tn clr`  — Clr (clear register)
`MOVE/Clr.c`; MANUAL §10.16. Opcodes 0x0084 (int), 0x0088 (F), 0x008C (D);
register in low 2 bits. Operation: `Rn = 0` (whole register; for D also
clears E). Flags: Z=1, S=0. Zero operands. C pseudo: `I1 = 0;`

### 5.5 `t move src,dst`  — Move
`MOVE/Move.c`; MANUAL §10.7. Opcodes 0x0019 (BY), 0x001A (W), 0x001B (F),
0x002C (D), 0xFC0B (BI), 0xFC14 (H). Operation: `dst = src`; source
unaffected. Flags: Z, S from the value. C pseudo: `mem[R+0x21]=mem[...]` —
2 operands, source first.

### 5.6 `tn neg`  — Neg (negate register)
`ARITHMETIC/Neg.c`; MANUAL §10.12. Opcodes 0x0090-93 (W), 0x0094-97 (F/D),
0xFE08-0B (BY), 0xFE0C-0F (H). Integer: two's complement `Rn = -Rn`
(sub-word clears upper bits); float/double: flip sign bit. Zero operands.
Flags: Z, S, C (C=1 iff negating 0), O (O=1 iff negating the greatest
negative integer -> overflow trap).

### 5.7 `t set1 dst`  — Set1 (set to one)
`CONTROL/Set1.c`; MANUAL §10.18. Opcodes 0x004D (W), 0x0047 (F),
0xFC86/87/88/89 (BI/BY/H/D). Operation: `dst = 1`. Flags: Z=S=C=O=0.

### 5.8 `noop`  — Noop
`CONTROL/Noop.c`; MANUAL §15.13. Does nothing; PC advances. No flags.

### 5.9 `bp`  — Breakpoint
`CONTROL/Bp.c`; MANUAL §16.1. Raises the Breakpoint Trap (BPT) when
enabled, else an Illegal-Instruction (IIC) trap. In `nd500x` it currently
logs and lets the debugger intercept. Zero operands, no flags. Opcode 0x02.

---

## 6. Arithmetic / compare / index class  (ARITHMETIC, COMPARE, SYSTEM)

### 6.1 `tn + src` / `tn - src` / `tn * src`  — Add / Subtract / Multiply
`ARITHMETIC/Add.c`, `Subtract.c`, `Multiply.c`; MANUAL §11.
One explicit operand; the target register `Rn` is both a source and the
destination.
- `+` opcodes 0x0054 (W)/0x0058 (F)/0x005C (D)/0xFC34 (BY)/0xFC38 (H):
  `Rn = Rn + src`.
- `-` opcodes 0x0060 (W)/0x0064 (F)/0x0068 (D)/0xFC3C (BY)/0xFC40 (H):
  `Rn = Rn - src`.
- `*` opcodes 0x006C (W)/0x0070 (F)/0x0074 (D)/0xFC44 (BY)/0xFC48 (H):
  `Rn = Rn * src`.
Integer arithmetic is signed with datatype-sized wrap; flags Z, S, C, O set
(carry per ND-500 no-borrow convention; O -> overflow trap). Example
`w3 - r.0xCC` => `I3 = I3 - mem[R+0xCC];`

NOTE: two-explicit-operand forms `add2/sub2/mul2` (`ARITHMETIC/Add2.c`
etc., `dst = dst OP src`) exist and share the `+`/`-`/`*` mnemonics for
higher opcodes; the exemplars use the 1-operand register forms above.

### 6.2 `tn mulad x,y`  — Mulad (multiply and add / sum of products)
`ARITHMETIC/Mulad.c`; MANUAL §11.20. Two operands. Operation:
`Rn = Rn * x + y` (signed, datatype-sized). Flags Z, S, C, O.
C pseudo: `I3 = I3 * x + y;`

### 6.3 `tn comp src` / `t comp2 a,b`  — Comp / Comp2 (compare)
`COMPARE/Comp.c` (MANUAL §10.9) and `COMPARE/Comp2.c` (§10.10).
- `comp`: one operand; computes `Rn - src`, **stores nothing**, sets flags.
  Z=(Rn==src), C=(Rn>=src) [no-borrow], S=sign, O=signed overflow.
- `comp2`: two operands; computes `a - b`, stores nothing, same flags.
Used before the `if <cc> go` branches to set the condition.

### 6.4 `t test src`  — Test (test against zero)
`COMPARE/Test.c`; MANUAL §10.11. One operand. Sets Z and S from `src`
compared to 0; for integer types also forces C=1. Nothing stored.
C pseudo: `Z = (src==0); S = (src<0);`

### 6.5 `tn lind idx,lo,hi` / `tn cind idx,lo,hi`  — Lind / Cind (index bounds check)
`SYSTEM/Lind.c` (MANUAL §15.8) and `SYSTEM/Cind.c` (§15.9). Three operands
(index, lower, upper), signed comparison after sign-extension.
- `lind`: `Rn = idx;` then if `idx < lo || idx > hi` set K=1 and IX=1
  (bit 26 of ST1) else clear both. (LIND loads the index into `Rn`.)
- `cind`: identical bounds check but **does not** load `Rn` (calculate
  index only). Used for array-bounds validation.
Opcodes: `lind` W=0xAC-AF, `cind` W=0xB0-B3 (plus BY/H/F/D variants).

---

## 7. Branches  (BRANCH)  — displacements are SIGNED

Branch operand is a **signed** PC-relative displacement; target =
`instruction_start_address + displacement` (`BRANCH/Go.c`,
`nd500_get_operand_displacement` in FMT 350-363, and the `is_pc_relative`
path FMT 519-535). The disassembler prints the operand value; treat it as a
displacement from the instruction's own address, not an absolute address.
(In mis-aligned mid-block carves the printed target is meaningless — see 9.)

### 7.1 `go d`  — Go (unconditional)
`BRANCH/Go.c`; MANUAL §13.2. Opcode 0x00C0. `PC = addr + d`. No flags.

### 7.2 Conditional `if <cc> go d`
Each tests `ST1` flags set by a preceding compare/test/arithmetic; if true,
`PC = addr + d`, else fall through. Flags are not modified. Opcode/condition
map (TAB `0x00C4..0x00DA`, semantics from the named `BRANCH/*.c` files):

| Disasm | Opcode | Branch taken when | nd500x file |
|--------|--------|-------------------|-------------|
| `if = go`  | 0x00C4 | `Z==1` | `IfEqualGo.c` |
| `if >< go` | 0x00C6 | `Z==0` | `IfNotEqualGo.c` |
| `if > go`  | 0x00C8 | `S==0 && Z==0` | `IfGreaterThanGo.c` |
| `if < go`  | 0x00CA | `S==1` | `IfLessThanGo.c` |
| `if >= go` | 0x00CC | `S==0` | `IfGreaterEqualGo.c` |
| `if <= go` | 0x00CE | `S==1 \|\| Z==1` | `IfLessEqualGo.c` |
| `if k go`  | 0x00D0 | `K==1` | `IfKeyGo.c`* |
| `if -k go` | 0x00D2 | `K==0` | `IfKeyGo.c`* |
| `if >> go` | 0x00D4 | `C==1 && Z==0` (unsigned >) | `IfUnsignedGreaterGo.c` |
| `if >>= go`| 0x00D6 | `C==1` (unsigned >=) | `IfUnsignedGreaterEqualGo.c` |
| `if << go` | 0x00D8 | `C==0` (unsigned <) | `IfUnsignedLessGo.c` |
| `if <<= go`| 0x00DA | `C==0 \|\| Z==1` (unsigned <=) | `IfUnsignedLessEqualGo.c` |

\* `IfKeyGo.c` implements the branch-when-K-clear variant (`if -k go`). The
`if k go` (0x00D0) branch-when-K-set variant is the complementary opcode;
the exemplars (411B) use `if -k go`. The signed `>`/`>=`/`<`/`<=` forms use
S and Z; the unsigned `>>`/`>>=`/`<<`/`<<=` forms use C (ND-500 no-borrow
carry) and Z.

Also in the family (not in exemplars): `if st go` (IfStackGo,
`BRANCH/IfStackGo.c`) — 2 operands, tests a numbered ST1 bit and branches
if clear, setting BT; `loop`/`loopd`/`loopi` (MANUAL §13.4).

---

## 8. Entry / stack / return class  (CALL, CONTROL)

Stack frame field offsets are fixed by the architecture (Section 2).

### 8.1 `init bos,dmain,dtot`  — Init (initialize stack)
`CONTROL/Init.c`; MANUAL §13.12. Three WORD operands: bottom-of-stack,
main stack demand, total stack demand. Operation: `B = bos`;
`TOS = bos + dtot`; write frame header `PREVB=0, RETA=0` at B; `L = 0`;
`SP(B+8) = bos + dmain`; clears STO. Traps STO if `dmain >= dtot`.
Executed once at program start before any CALL.

### 8.2 `entsn dem,maxargs` / `entf area`  — entry points
Both must be reached via a preceding CALL (else ISE trap).
- `entsn` (`CALL/Entsn.c`; opcode 0x00BA; MANUAL §13.10): "enter stack
  subroutine, bounded args". `new_B = mem[old_B + SP]`; STO-checks
  `new_B + dem >= TOS`; writes PREVB=old_B, RETA=return addr, new SP,
  N = min(actual_args, maxargs), copies up to `maxargs` arg addresses;
  sets `B = new_B`, `L = return addr`.
- `entf area` (`CALL/Entf.c`; opcode 0x00DD): "enter with fixed/static data
  area". Same header, but `B` is set to the operand address (a static area,
  **not** the stack), so locals persist between calls; inherits caller's SP.

Family (not in exemplars): `ents` 0xB8, `entd` 0x9C, `entt` 0xBC,
`entb` 0xBD, `entfn` 0xDE, `entm` 0xDF (all MANUAL §13.10, `CALL/*.c`).

### 8.3 `retd`  — Retd (return direct)
`CALL/Retd.c`; MANUAL §13.11. Opcode 0x0082. `PC = L`. No frame unwind, B
unchanged. Pairs with ENTD. Family: `ret` 0x83, `retb`, `retk`, `retbk`,
`rett`, `ifkret` (§13.11, `CALL/*.c`).

---

## 9. `??? ; opcode 0x00NN` lines — NOT instructions (mis-alignment)

Bytes `0xF1`, `0xF2`, `0xFB`, `0x01` printed as `??? ; opcode 0x00NN` are
**not** standalone opcodes: they are absent from the instruction table
(verified against TAB) because `0xF0-0xFF` and the `top==0x01/0x02` ranges
are **operand address-code / prefix bytes** (DEC 316-324), and `0x01` is a
short-form region. They appear only where `nd500-dis` starts decoding
*inside* an instruction — i.e. the packed 400B-421B fix-family handlers and
the deliberately mid-block entries (417B at 0xBDF6). The carve headers flag
these regions as low-confidence. In pseudo-C, do **not** invent semantics
for a `???` line; treat the raw bytes as data whose true meaning depends on
the real (caller-fixed) instruction alignment.

Likewise, any decoded instruction on a mid-block boundary (e.g. the leading
`by stz`/`entsn` in 417B, or lines that "spill past" the region end noted in
410B/411B/413B) is decoded from a wrong start byte and its mnemonic is not
trustworthy even though individual bytes are ground truth.

---

## 10. Coverage confirmation (six exemplars)

Every mnemonic that occurs in
`410B-FixInMemory.ASM`, `411B-MemoryUnfix.ASM`, `412B-FileAsSegment.ASM`,
`413B-FileNotAsSegment.ASM`, `416B-SaveND500Segment.ASM`,
`417B-MaxPagesInMemory.ASM` is covered above:

`:=`, `=:`, `stz`, `clr`, `move`, `neg`, `set1`, `noop`, `bp` (§5);
`+`, `-`, `*`, `mulad`, `comp`, `comp2`, `test`, `lind`, `cind` (§6);
`go`, `if << go`, `if < go`, `if > go`, `if >< go`, `if >= go`,
`if <<= go`, `if >>= go`, `if -k go` (§7);
`init`, `entsn`, `entf`, `retd` (§8);
`??? ; opcode 0x00F1/0x00F2/0x00FB/0x0001` (§9);
plus the operand notations `$`, `$<double>`, `r.`, `b.`, `rN.()`, `@b.`,
`DESC(rN)` and the register-prefix letters `bi/by/h/w/f/d` and indices
`1..4` (§2-3).

---

## 11. Resolved vs UNRESOLVED summary

**RESOLVED** (semantics grounded in `nd500x` source + manual section):
- Register model, addressing modes, effective-address computation, flags.
- Load/store/move/clear: `:=`, `=:`, `stz`, `clr`, `move`, `set1`, `neg`, `noop`.
- Arithmetic: `+`, `-`, `*`, `mulad`.
- Compare/index: `comp`, `comp2`, `test`, `lind`, `cind`.
- All branch conditions `go` / `if <cc> go` including the K and unsigned forms.
- Stack/entry/return: `init`, `entsn`, `entf`, `retd` (and the wider ENT*/RET* family).
- MON dispatch mechanism (SINTRAN software vector, not a CPU opcode).
- `bp` (breakpoint) — resolved at the manual level; `nd500x` implementation
  is a debugger stub pending full trap-enable support.

**UNRESOLVED**:
- `??? ; opcode 0x00F1/0x00F2/0x00FB/0x0001` — not instructions; mis-aligned
  operand/prefix bytes (documented in §9, verified absent from the opcode table).
- `jumps` — documentation (MANUAL §16.34 "jump to supervisor") conflicts
  with `nd500x`'s `Jumps.c` (absolute jump). Not used by the exemplars; the
  conflict is left flagged, not guessed.
