# ND-100 Instruction Semantics — Derived from the Formal Ghidra SLEIGH / P-code Definition

**Authority**: This document is an INDEPENDENT second authority on ND-100 instruction
semantics. Every statement below is transcribed or paraphrased directly from the Ghidra
SLEIGH P-code definitions and is cited to the exact source line. It is intended to be
cross-checked against the emulator-derived authority.

**Primary source files** (all under `ghidra-nd100/ND-100/data/languages/`):

| File | Role |
|------|------|
| `nd100.slaspec` | Token layout, register bindings, attach tables, all instruction constructors |
| `nd100_memory.sinc` | Addressing-mode sub-tables (`addr`, `ea_addr`, `jmp_addr`, `rel_target`, `io_ref`) |
| `nd100_shift.sinc` | Shift-operand decode sub-table (`shift_operand`) |
| `nd100_registers.sinc` | Flag-bit documentation only (comment) |
| `nd100.sinc` | Empty stub (comment only) |

Citations are written `nd100.slaspec:<line>`, `nd100_memory.sinc:<line>`,
`nd100_shift.sinc:<line>`.

---

## 0. Machine model, registers, flags (foundation)

**Address/data model** (`nd100.slaspec:4-10`): big-endian; `ram` space is **word-addressed**
(`wordsize=2`), so a P-code address is a WORD index, and `ea + 1` advances one 16-bit word.
There is a separate `io_space` (also wordsize=2).

**General registers** — code-order encoding 0..7 (`nd100.slaspec:12-13`, `100`):

| Code | 0 | 1 | 2 | 3 | 4 | 5 | 6 | 7 |
|------|---|---|---|---|---|---|---|---|
| Reg  | STS | D | P | B | L | A | T | X |

Semantic binding for register operand fields (`nd100.slaspec:102`):
`attach variables [ rop_dst rop_src reg3 skp_dr skp_sr ] [ STS D P B L A T X ]`.

**Display-name attaches** — source fields get an `S` prefix, destination fields a `D` prefix
(`nd100.slaspec:85-89`):

- `rop_src_d`: `0 SD SP SB SL SA ST SX`  (so `SD`=reg D, `SA`=reg A, `ST`=reg T, `SX`=reg X, `SB`=reg B, `SL`=reg L, `SP`=reg P)
- `rop_dst_d` / `reg3_d`: `0 DD DP DB DL DA DT DX`  (so `DA`=reg A, `DD`=reg D, `DT`=reg T, `DX`=reg X, …)

> Reading rule: in a mnemonic like `RADD CLD SD DA`, `SD` is **source = register D** and
> `DA` is **destination = register A**. The letters after S/D name the physical register.

**Status/flag bits inside STS** (used throughout; bit indices are `STS[bit,width]`):

| P-code | Flag | Meaning |
|--------|------|---------|
| `STS[2,2]` | K | one-bit accumulator / carry-of-bit-ops |
| `STS[3,3]` | Z | error / zero flag (see RDIV, and `nd100_registers.sinc:6`) |
| `STS[4,4]` | Q | sticky (dynamic) overflow |
| `STS[5,5]` | O | overflow |
| `STS[6,6]` | C | carry |

(Flag-bit legend: `nd100_registers.sinc:5-7`; usage e.g. `nd100.slaspec:215-217`, `1251-1257`.)

**Token / operand fields** (`nd100.slaspec:22-77`):
`op5=(11,15)`, `op8=(8,15)`, `op9=(7,15)`, `op10=(6,15)`, `op12=(4,15)`, `op13=(3,15)`,
`op16=(0,15)`; addressing `X_bit=(10,10)`, `I_bit=(9,9)`, `B_bit=(8,8)`; `disp=(0,7) signed`;
`imm8=(0,7)`; `disp3=(3,5)`, `fixed3=(0,2)`; `bit4=(3,6)`; ROP `rop_src=(3,5)`,
`rop_dst=(0,2)`, `rop_cld=(6,6)`, `rop_cm1=(7,7)`, `rop_ad=(8,9)`; SKP `skp_cond=(8,10)`,
`skp_sr=(3,5)`, `skp_dr=(0,2)`.

---

## Cheat-sheet — ~40 most common MON-handler forms

Disassembler mnemonic → one-line C meaning (EA = effective address; `mem[]` is word memory).
Every row is expanded and cited in the sections below.

| Mnemonic (form) | C-ish meaning | Cite |
|-----------------|---------------|------|
| `STZ addr` | `mem[EA] = 0` | slaspec:1730 |
| `STA addr` | `mem[EA] = A` | slaspec:1675 |
| `LDA addr` | `A = mem[EA]` | slaspec:966 |
| `STT addr` | `mem[EA] = T` | slaspec:1718 |
| `LDT addr` | `T = mem[EA]` | slaspec:1018 |
| `STX addr` | `mem[EA] = X` | slaspec:1724 |
| `LDX addr` | `X = mem[EA]` | slaspec:1024 |
| `STD ea` | `mem[EA]=A; mem[EA+1]=D` | slaspec:1688 |
| `LDD ea` | `A=mem[EA]; D=mem[EA+1]` | slaspec:988 |
| `MIN addr` | `mem[EA]++; if(mem[EA]==0) skip` | slaspec:1088 |
| `ADD addr` | `A = A + mem[EA]` (C/O/Q) | slaspec:260 |
| `SUB addr` | `A = A - mem[EA]` (C=no-borrow) | slaspec:1743 |
| `AND addr` | `A = A & mem[EA]` | slaspec:280 |
| `ORA addr` | `A = A \| mem[EA]` | slaspec:1171 |
| `MPY addr` | `A = A * mem[EA]` (O/Q) | slaspec:1136 |
| `JMP addr` | `P = EA` | slaspec:765 |
| `JPL addr` | `L = P+1; P = EA` (call) | slaspec:844 |
| `JAP/JAN/JAZ/JAF *t` | jump if A `>=0 / <0 / ==0 / !=0` | slaspec:742/734/750/724 |
| `JXN/JXZ *t` | jump if X `<0 / ==0` | slaspec:905/913 |
| `JPC/JNC *t` | `X++; jump if X >=0 / <0` | slaspec:828/818 |
| `SAA/SAT/SAX/SAB d` | `A/T/X/B = sign_ext(disp)` | slaspec:1390/1456/1462/1397 |
| `AAA/AAT/AAX/AAB d` | `A/T/X/B += disp` (C/O/Q) | slaspec:208/234/247/221 |
| `COPY SsRc DdSt` | `dst = src; C=0;O=0` | slaspec:485 |
| `RADD CLD SsRc DdSt` | `dst = src` (clear-dst add) | slaspec:1215 |
| `RADD SsRc DdSt` | `dst = dst + src` (C/O/Q) | slaspec:1215 |
| `RSUB SsRc DdSt` | `dst = dst - src` (C/O/Q) | slaspec:1376 |
| `RAND/RORA/REXO SsRc DdSt` | `dst &= / \|= / ^= src` | slaspec:1261/1370/1336 |
| `RCLR DdSt` | `dst = 0; C=0;O=0` | slaspec:1267 |
| `RINC/RDCR DdSt` | `dst++ / dst--` (C/O/Q) | slaspec:1348/1275 |
| `SWAP SsRc DdSt` | `dst = src` | slaspec:1780 |
| `SWAP CM1 SsRc DdSt` | exchange `src <-> dst` | slaspec:1791 |
| `EXIT` | `P = L` (return) | slaspec:569 |
| `SKP IF DdSt cond SsRc` | skip next if `(dst cond src)` | slaspec:1624 |
| `BSET/BSKP c bit reg` | set/skip on register bit | slaspec:340/374 |
| `SHA/SHT/SHD op` | shift A / T / D register | slaspec:1508/1584/1545 |
| `LDATX d` | `A = mem[X+d]` | slaspec:972 |
| `STATX d` | `mem[X+d] = A` | slaspec:1681 |
| `LDDTX d` | `A=mem[X+d]; D=mem[X+d+1]` | slaspec:998 |
| `STDTX d` | `mem[X+d]=A; mem[X+d+1]=D` | slaspec:1698 |
| `STZTX d` | `mem[X+d] = 0` | slaspec:1736 |
| `IOX io` | `A = io[a]` (in) / `io[a] = A` (out) | slaspec:688 |
| `MON n` | monitor trap, code `n` | slaspec:1108 |
| `WAIT` | relinquish level / halt | slaspec:1973 |
| `IRW/IRR lvl reg` | inter-level reg write/read | slaspec:717/710 |
| `TRA reg` / `TRR reg` | `A = sysreg` / `sysreg = A` | slaspec:1820/1887 |

---

## 1. Memory-reference instructions + addressing modes

### 1.1 Addressing-mode sub-tables

Memory-reference instructions with a 5-bit opcode carry the 3-bit mode field
`X_bit(10) I_bit(9) B_bit(8)`. There are three parallel sub-tables; each has 8 constructors,
one per mode. All are in `nd100_memory.sinc`.

**`addr`** — exports the *value* `*:2 ea` at the effective address (loads/stores/ALU)
(`nd100_memory.sinc:16-72`):

| Mode | X I B | Form | Effective address / value | Cite |
|------|-------|------|---------------------------|------|
| 0 | 0 0 0 | `* reloc` (P-rel) | `EA = inst_start + disp`; value `mem[EA]` | :22 |
| 1 | 0 0 1 | `disp,B` | `EA = B + disp` | :27 |
| 2 | 0 1 0 | `I *reloc` | `ptr = inst_start+disp; EA = mem[ptr]` | :33 |
| 3 | 0 1 1 | `I disp,B` | `ptr = B+disp; EA = mem[ptr]` | :40 |
| 4 | 1 0 0 | `disp,X` | `EA = X + disp` | :47 |
| 5 | 1 0 1 | `disp,B,X` | `EA = B + X + disp` | :53 |
| 6 | 1 1 0 | `,X I *reloc` | `ptr=inst_start+disp; EA = mem[ptr] + X` | :59 |
| 7 | 1 1 1 | `,X I disp,B` | `ptr=B+disp; EA = mem[ptr] + X` | :67 |

**Order of indirection vs indexing (CRITICAL, from the P-code):** in modes 6 and 7 the memory
word is fetched FIRST (`ea = *ptr`), and X is added AFTER the fetch
(`result = ea + X`) — i.e. **post-indexed indirect** (`nd100_memory.sinc:59-64`, `67-72`).
There is no pre-indexed-indirect mode: with I=1, X is always applied to the loaded pointer,
never to `ptr` before the fetch. In modes 1/3/5/7 the B relative base is added to `disp`
*before* any indirect fetch (`ptr = B + disp`).

**`ea_addr`** — exports the *raw address* (not dereferenced), used by the multi-word
LDD/STD/LDF/STF and the float ops (`nd100_memory.sinc:74-131`). Same 8 modes, but exports
`ea`/`result` instead of `*:2 ea`. Modes 6/7 again post-index: `result = ea + X`
(`nd100_memory.sinc:118-123`, `126-131`).

**`jmp_addr`** — like `addr` (exports `*:2 ea`), used by JMP/JPL for arrow generation
(`nd100_memory.sinc:133-189`). Same 8 modes, same post-indexed-indirect ordering.

**`rel_target`** — single P-relative constructor `EA = inst_start + disp`, used by the 8-bit
conditional jumps (`nd100_memory.sinc:197-199`).

**`io_ref`** — exports `*[io_space]:2 io_addr` (`nd100_memory.sinc:208-210`).

### 1.2 Single-word load/store (opcode `op5`)

| Mnemonic | op5 | P-code | Cite |
|----------|-----|--------|------|
| `STZ addr` | 0x00 | `addr = 0` (store 0 to EA) | slaspec:1730 |
| `STA addr` | 0x01 | `addr = A` | slaspec:1675 |
| `STT addr` | 0x02 | `addr = T` | slaspec:1718 |
| `STX addr` | 0x03 | `addr = X` | slaspec:1724 |
| `LDA addr` | 0x09 | `A = addr` | slaspec:966 |
| `LDT addr` | 0x0A | `T = addr` | slaspec:1018 |
| `LDX addr` | 0x0B | `X = addr` | slaspec:1024 |

Here `addr` is the sub-table export (the value at EA for loads; the l-value at EA for stores).
No flags touched.

### 1.3 Double / float load/store (opcode `op5`, uses `ea_addr`)

| Mnemonic | op5 | P-code | Cite |
|----------|-----|--------|------|
| `STD ea` | 0x04 | `mem[EA]=A; mem[EA+1]=D` | slaspec:1688 |
| `LDD ea` | 0x05 | `A=mem[EA]; D=mem[EA+1]` | slaspec:988 |
| `STF ea` | 0x06 | `mem[EA]=T; mem[EA+1]=A; mem[EA+2]=D` | slaspec:1706 |
| `LDF ea` | 0x07 | `T=mem[EA]; A=mem[EA+1]; D=mem[EA+2]` | slaspec:1006 |

Word order for the double/float pair: A occupies EA (D at EA+1) for LDD/STD; for LDF/STF the
triple is T,A,D at EA,EA+1,EA+2 (`nd100.slaspec:988-1015`, `1688-1715`).

### 1.4 MIN — memory increment and skip

`MIN addr` (`op5=0x08`, `nd100.slaspec:1088-1095`):
`result = mem[EA] + 1; mem[EA] = result; if (result == 0) skip next instruction`
(skip target via `skip_next` = `inst_start + 2`, `nd100.slaspec:194`).

### 1.5 ALU memory-reference (opcode `op5`, uses `addr`) with flags

- `ADD addr` (0x0C, `nd100.slaspec:260-270`):
  `tmp = zext(A)+zext(mem[EA]); A = tmp:2; C = (tmp>>16)!=0; O = scarry(A_old,mem); Q |= O`.
- `SUB addr` (0x0D, `nd100.slaspec:1743-1755`): implemented as ones'-complement add:
  `tmp = zext(A) + zext(~mem) + 1; A = tmp:2; C = (tmp>>16)!=0` (**C=1 means NO borrow**);
  `O = sborrow(A_old,mem); Q |= O`.
- `AND addr` (0x0E, `nd100.slaspec:280-284`): `A = A & mem[EA]`. No flags.
- `ORA addr` (0x0F, `nd100.slaspec:1171-1175`): `A = A | mem[EA]`. No flags.
- `MPY addr` (0x14, `nd100.slaspec:1136-1146`): `tmp = sext(A)*sext(mem); A = tmp:2;`
  `O = (tmp>32767)|(tmp<-32767); Q |= O`. No carry.

### 1.6 JMP / JPL (8 modes each)

`JMP` (`op5=0x15`) — mode 0 `goto jmp_addr`; other modes compute EA then `goto [ea]`
(`nd100.slaspec:765-815`). The EA computation exactly mirrors the addressing table, including
post-indexed indirect in modes 6/7 (`target = ea + X`, `nd100.slaspec:805`, `813`).

`JPL` (`op5=0x17`) — **Jump and Link**: every constructor sets `L = inst_start + 1` first,
then `call <target>` (`nd100.slaspec:844-902`). Same 8-mode EA computation as JMP, same
post-indexed indirect in modes 6/7 (`nd100.slaspec:891`, `900`).

---

## 2. Register-operation (ROP) class — the #1 mistranslation risk

### 2.1 RADD — the general adder with modifiers (authoritative expansion)

Constructor `nd100.slaspec:1215-1258`, opcode `op5=0x19 & X_bit=1` (= 0146000₈ / 0xCC00),
modifier fields `rop_ad(8-9)`, `rop_cm1(7)`, `rop_cld(6)`, `rop_src(3-5)`, `rop_dst(0-2)`.

Exact P-code (paraphrased line-for-line):

```
src_op = rop_src                     # source register value
if CM1: src_op = ~rop_src            # CM1 => ones'-complement of source   (slaspec:1226-1227)
dst_op = rop_dst                     # destination register value
if CLD: dst_op = 0                   # CLD => clear destination first      (slaspec:1232-1233)
addend = 0
if AD==01 (AD1): addend = 1                                                (slaspec:1240-1241)
if AD==10 (ADC): addend = zext(C)    # ADC => add carry flag               (slaspec:1244-1245)
tmp = zext(dst_op) + zext(src_op) + zext(addend)                          (slaspec:1248)
rop_dst = tmp:2                      # result written to DESTINATION reg    (slaspec:1250)
C = (tmp>>16)!=0                                                           (slaspec:1251)
O = (sign(dst_op)==sign(src_op)) & (sign(result)!=sign(dst_op))           (slaspec:1253-1256)
Q |= O                                                                    (slaspec:1257)
```

**Order of application: complement (CM1) and clear (CLD) are applied to the operands BEFORE
the add; then the AD1/ADC addend is added.** Destination is the sink.

**Worked answer to `RADD CLD SD DA`:** `SD` = source register **D**; `DA` = destination
register **A**; `CLD` set, no CM1, AD=00.
→ `src_op = D`, `dst_op = 0` (cleared), `addend = 0`.
→ `tmp = 0 + D + 0`; **`A = D`.** (A copy of D into A, with C=carry-out(=0), O by the
sign rule, Q sticky.) This is exactly the "clear-destination add" idiom used to move a
register while going through the adder.

Other canonical RADD idioms that follow from the same P-code:
- `RADD SsRc DdSt` (no modifiers): `dst = dst + src`.
- `RADD ADC SsRc DdSt`: `dst = dst + src + C`.
- `RADD AD1 SsRc DdSt`: `dst = dst + src + 1`.
- `RADD CM1 SsRc DdSt`: `dst = dst + ~src` (= dst − src − 1).

### 2.2 RSUB, COPY, and the other named ROP forms

| Mnemonic | opcode | P-code (exact) | Cite |
|----------|--------|----------------|------|
| `RSUB SsRc DdSt` | op10=0x336 | `tmp = zext(dst) + zext(~src) + 1; dst = tmp:2; C=(tmp>>16)!=0 (no-borrow); O=sborrow(dst,src); Q\|=O` — i.e. `dst = dst - src` | slaspec:1376-1387 |
| `COPY SsRc DdSt` | op8=0xCC & rop_cld=1 | `dst = src; C=0; O=0` | slaspec:485-490 |
| `RAND SsRc DdSt` | op10=0x324 | `dst = dst & src` (no flags) | slaspec:1261-1264 |
| `RORA SsRc DdSt` | op10=0x32C | `dst = dst \| src` (no flags) | slaspec:1370-1373 |
| `REXO SsRc DdSt` | op10=0x328 | `dst = dst ^ src` (no flags) | slaspec:1336-1339 |
| `RCLR DdSt` | op13=0x1988 | `dst = 0; C=0; O=0` | slaspec:1267-1272 |
| `RINC DdSt` | op13=0x19A0 | `O=scarry(dst,1); Q\|=O; C=carry(dst,1); dst = dst+1` | slaspec:1348-1354 |
| `RDCR DdSt` | op13=0x1990 | `O=sborrow(dst,1); Q\|=O; C=(dst!=0); dst = dst-1` | slaspec:1275-1281 |
| `RMPY SsRc DdSt` | op10=0x30A | `res=sext(src)*sext(dst); A=res_hi; D=res_lo; C=|res|>16bit` — **always writes A:D** regardless of operand regs | slaspec:1357-1367 |
| `RDIV SsRc` | op10=0x30E | `dividend = (A<<16)|D; A = dividend / src; D = dividend % src; C=0; on div0/overflow set Z(bit3)` | slaspec:1284-1302 |
| `EXR SsRc` | op10=0x306 | execute register `rop_src` as an instruction (`nd100_exr`) | slaspec:576-579 |

> Note: `COPY` (`nd100.slaspec:485`) is encoded as `op8=0xCC & rop_cld=1`, i.e. it is the
> `RADD CLD` alias with AD=00/CM1=0, so `dst = 0 + src = src`, and it explicitly forces
> `C=0; O=0`. This matches the RADD expansion above.

### 2.3 SWAP (4 modifier forms)

Constructors `nd100.slaspec:1780-1802`, opcode `op8=0xC8`. Fields `rop_cm1(7)`, `rop_cld(6)`:

| Form | Condition | P-code | Cite |
|------|-----------|--------|------|
| `SWAP SsRc DdSt` | cm1=0 cld=0 | `dst = src` | slaspec:1780-1783 |
| `SWAP CLD SsRc DdSt` | cm1=0 cld=1 | `dst = src; src = 0` | slaspec:1785-1789 |
| `SWAP CM1 SsRc DdSt` | cm1=1 cld=0 | exchange: `t=src; src=dst; dst=t` | slaspec:1791-1796 |
| `SWAP CM1 CLD SsRc DdSt` | cm1=1 cld=1 | `dst = src; src = 0` | slaspec:1798-1802 |

> Caveat: the SLEIGH SWAP variants (except `CM1`) do NOT match the header comment's stated
> "CM1 => complement" semantics — the P-code for the plain/CLD/CM1+CLD forms performs a plain
> copy, and only the `CM1` (cm1=1, cld=0) form performs a true exchange. Documented exactly as
> the P-code is written (`nd100.slaspec:1780-1802`); the surrounding comment `1765-1778` is a
> YAML note, not the emitted semantics.

### 2.4 EXIT

`EXIT` (`op16=0xCC62`, `nd100.slaspec:569-573`): `return [L]` — transfer L to P (subroutine
return). It is the `COPY SL DP` alias.

---

## 3. Argument instructions SAA/SAT/SAX/SAB and AAA/AAT/AAX/AAB

`disp` is declared **signed** (`nd100.slaspec:37`), so it is sign-extended to 16 bits on
assignment.

**Set-argument (load sign-extended immediate)** — no flags:

| Mnemonic | op8 | P-code | Cite |
|----------|-----|--------|------|
| `SAB disp` | 0xF0 | `B = sign_ext(disp)` | slaspec:1397-1402 |
| `SAA disp` | 0xF1 | `A = sign_ext(disp)` | slaspec:1390-1394 |
| `SAT disp` | 0xF2 | `T = sign_ext(disp)` | slaspec:1456-1459 |
| `SAX disp` | 0xF3 | `X = sign_ext(disp)` | slaspec:1462-1465 |

**Add-argument (add immediate)** — set C/O/Q, all identical pattern:

| Mnemonic | op8 | P-code | Cite |
|----------|-----|--------|------|
| `AAB disp` | 0xF4 | `B += disp; C=carry; O=scarry(B_old,disp); Q\|=O` | slaspec:221-231 |
| `AAA disp` | 0xF5 | `A += disp; C=carry; O=scarry(A_old,disp); Q\|=O` | slaspec:208-218 |
| `AAT disp` | 0xF6 | `T += disp; C=carry; O=scarry(T_old,disp); Q\|=O` | slaspec:234-244 |
| `AAX disp` | 0xF7 | `X += disp; C=carry; O=scarry(X_old,disp); Q\|=O` | slaspec:247-257 |

For all AA*: `tmp = zext(reg)+zext(disp); reg = tmp:2; C = (tmp>>16)!=0`.

---

## 4. T/X-indexed field-transfers (LDATX/STATX/LDDTX/STDTX/STZTX/LDBTX/LDXTX)

All share `op10=0x31B` and select the operation with `fixed3` (bits 0-2); the displacement is
`disp3` (bits 3-5). **Effective address is `EA = X + disp3`** in every case
(`nd100.slaspec:972-1034`, `1681-1740`). Note: despite the "TX" name, the SLEIGH EA uses the
**X register only** (plus the 3-bit displacement); T does not enter the address computation in
these constructors.

| Mnemonic | fixed3 | opcode | P-code | Cite |
|----------|--------|--------|--------|------|
| `LDATX disp3` | 0 | 0xC6C0 | `A = mem[X+disp3]` | slaspec:972-976 |
| `LDXTX disp3` | 1 | 0xC6C1 | `X = mem[X+disp3]` | slaspec:1030-1034 |
| `LDDTX disp3` | 2 | 0xC6C2 | `A = mem[X+disp3]; D = mem[X+disp3+1]` | slaspec:998-1003 |
| `LDBTX disp3` | 3 | 0xC6C3 | `B = (mem[X+disp3] << 1) \| 0xFE00` | slaspec:979-985 |
| `STATX disp3` | 4 | 0xC6C4 | `mem[X+disp3] = A` | slaspec:1681-1685 |
| `STZTX disp3` | 5 | 0xC6C5 | `mem[X+disp3] = 0` | slaspec:1736-1740 |
| `STDTX disp3` | 6 | 0xC6C6 | `mem[X+disp3] = A; mem[X+disp3+1] = D` | slaspec:1698-1703 |

**Operand size:** single word (A or X or B) for LDATX/LDXTX/LDBTX/STATX; word pair (A then D,
at EA and EA+1) for LDDTX/STDTX; zero-store for STZTX. `LDBTX` additionally transforms the
loaded word into stack-pointer format `(val<<1)|0xFE00` (`nd100.slaspec:984`).

> No `STBTX` constructor exists in the SLEIGH — the store-byte-TX form named in the scope is
> UNRESOLVED (sleigh: only fixed3 values 0-6 are defined at `op10=0x31B`; fixed3=7 has no
> constructor, and no `STBTX` mnemonic appears).

---

## 5. Skip / conditional-jump instructions

### 5.1 8-bit conditional jumps (target via `rel_target` = `inst_start + disp`)

| Mnemonic | op8 | Condition to jump | Side effect | Cite |
|----------|-----|-------------------|-------------|------|
| `JAP *t` | 0xB0 | `A >= 0` (signed) | — | slaspec:742-747 |
| `JAN *t` | 0xB1 | `A < 0` (signed) | — | slaspec:734-739 |
| `JAZ *t` | 0xB2 | `A == 0` | sets `C = (A==0)` | slaspec:750-757 |
| `JAF *t` | 0xB3 | `A != 0` (filled) | — | slaspec:724-731 |
| `JPC *t` | 0xB4 | `X >= 0` after `X++` | `X = X+1` first | slaspec:828-835 |
| `JNC *t` | 0xB5 | `X < 0` after `X++` | `X = X+1` first | slaspec:818-825 |
| `JXZ *t` | 0xB6 | `X == 0` | — | slaspec:913-918 |
| `JXN *t` | 0xB7 | `X < 0` (signed) | — | slaspec:905-910 |

(JAZ additionally writes the carry flag; JPC/JNC pre-increment X before the test.)

### 5.2 SKP — register-compare skip

`SKP IF DdSt cond SsRc` (`op5=0x18 & skp_fixed=0`, `nd100.slaspec:1624-1666`). It computes
`diff = skp_dr - skp_sr` and derives flags, then skips the next instruction (skip target
`inst_start + 2`) if the condition holds:

| `skp_cond` | Mnemonic | Skip when | Cite |
|-----------|----------|-----------|------|
| 0 | EQL | `Z` (dr == sr) | slaspec:1634 |
| 1 | GEQ | `S == 0` (sign of diff clear) | slaspec:1638 |
| 2 | GRE | `S == O` (signed dr >= sr) | slaspec:1642 |
| 3 | MGRE | `C` = `dr >= sr` unsigned | slaspec:1646 |
| 4 | UEQ | `Z == 0` (dr != sr) | slaspec:1650 |
| 5 | LSS | `S == 1` | slaspec:1654 |
| 6 | LST | `S != O` (signed dr < sr) | slaspec:1658 |
| 7 | MLST | `C == 0` (dr < sr unsigned) | slaspec:1661 |

Flag derivations inside SKP: `S=(diff>>15)!=0`, `Z=(diff==0)`, `O=sborrow(dr,sr)`,
`C=(dr>=sr)` unsigned (`nd100.slaspec:1629-1632`). The `skp_cond` attach names are
`EQL GEQ GRE MGRE UEQ LSS LST MLST` (`nd100.slaspec:92`).

> Scope note: the task lists condition mnemonics `EQL/UEQ/GRE/LSS/MGRE/MLSS`; the SLEIGH's
> full set is the eight above (its bit-6/7 forms are `LST`/`MLST`, not `MLSS`). Mapping is by
> the `skp_cond` value in the table.

---

## 6. Bit instructions (BSET / BSKP and the K-accumulator bit ops)

Fields: `bit4=(3,6)` = bit number; `reg3=(0,2)` = register; K flag = `STS[2,2]` for the
K-family, and `STS[6,1]`/`STS[6,6]` (carry) for BSET BAC / BSKP.

### 6.1 BSET (op9=0x1F0..0x1F3) — modify a register bit

| Form | op9 | P-code | Cite |
|------|-----|--------|------|
| `BSET ZRO bit reg` | 0x1F0 | `reg &= ~(1<<bit)` (clear) | slaspec:340-343 |
| `BSET ONE bit reg` | 0x1F1 | `reg |= (1<<bit)` (set) | slaspec:346-349 |
| `BSET BCM bit reg` | 0x1F2 | `reg ^= (1<<bit)` (complement) | slaspec:352-355 |
| `BSET BAC bit reg` | 0x1F3 | `bit = carry` (set bit to C=STS[6,1]) | slaspec:358-366 |

### 6.2 BSKP (op9=0x1F4..0x1F7) — skip on a register bit (skip target `inst_start+2`)

| Form | op9 | Skip when | Cite |
|------|-----|-----------|------|
| `BSKP ZRO bit reg` | 0x1F4 | bit == 0 | slaspec:374-379 |
| `BSKP ONE bit reg` | 0x1F5 | bit == 1 | slaspec:382-387 |
| `BSKP BCM bit reg` | 0x1F6 | bit != carry | slaspec:390-397 |
| `BSKP BAC bit reg` | 0x1F7 | bit == carry | slaspec:400-407 |

### 6.3 K-accumulator bit ops (op9=0x1F8..0x1FF, 0x1AE/0x1AF for IRR/IRW)

| Mnemonic | op9 | P-code | Cite |
|----------|-----|--------|------|
| `BSTC bit reg` | 0x1F8 | store ~K to bit, then `K=1` | slaspec:425-437 |
| `BSTA bit reg` | 0x1F9 | store K to bit, then `K=0` | slaspec:410-422 |
| `BLDC bit reg` | 0x1FA | `K = (bit == 0)` | slaspec:314-318 |
| `BLDA bit reg` | 0x1FB | `K = (bit != 0)` | slaspec:307-311 |
| `BANC bit reg` | 0x1FC | `K = ((bit==0) & K)` | slaspec:287-291 |
| `BAND bit reg` | 0x1FD | `K = ((bit!=0) & K)` | slaspec:294-298 |
| `BORC bit reg` | 0x1FE | `K = ((bit==0) | K)` | slaspec:328-331 |
| `BORA bit reg` | 0x1FF | `K = ((bit!=0) | K)` | slaspec:321-324 |

(`K = STS[2,2]` throughout this sub-family.)

---

## 7. Shift instructions (SHA / SHT / SHD / SAD)

All share `op5=0x1B` and select the register via `shift_reg(8-7)`
(0=T, 1=D, 2=A, 3=A:D). The `shift_operand` sub-table (`nd100_shift.sinc`) decodes a
2-byte value carrying: **type in bits 8-7** (0=Arithmetic, 1=ROT, 2=ZIN, 3=LIN), **direction
in bit 5** (0=left, 1=right), **count in bits 4-0**. For a right shift the assembler count is
`32 - raw` (`nd100_shift.sinc:15-17`, `31`, `43`, etc.).

| Mnemonic | shift_reg | Register shifted | Cite |
|----------|-----------|------------------|------|
| `SHT op` | 0 | T | slaspec:1584-1613 |
| `SHD op` | 1 | D | slaspec:1545-1574 |
| `SHA op` | 2 | A | slaspec:1508-1542 |
| `SAD op` | 3 | A:D (32-bit) | slaspec:1414-1444 |

**Shift semantics** (identical structure for SHA/SHD/SHT — quoting SHA `nd100.slaspec:1508-1542`):
- `stype = (op>>7)&3; sdir = (op>>5)&1; cnt = op & 0x1F`.
- **Left** (`sdir==0`): if `stype==1` (ROT) rotate left by `cnt&0xF`
  (`A = (A<<lcnt)|(A>>(16-lcnt))`); else (Arith/ZIN/LIN) `A = A << cnt` (zero fill).
- **Right** (`sdir==1`, `rcount = 32 - cnt`): `stype==1` ROT → rotate right by `rcount&0xF`;
  `stype==2` ZIN → logical right (`A >> rcount`, zero fill); else (Arith type 0, and LIN
  type 3) arithmetic right `A s>> rcount` (sign fill).

> Explicit sleigh note: LIN right is approximated as arithmetic right — "LIN right (type 3,
> approx — M flag not modeled)" (`nd100.slaspec:1532`). So **LIN-right link-bit fill is
> UNRESOLVED (sleigh: M flag not modeled; treated as arithmetic right).**

**SAD (32-bit A:D)** (`nd100.slaspec:1414-1444`): builds `ad = (A<<16)|D`, applies the same
type/dir/count logic on 32 bits, then `A = ad_hi; D = ad_lo`. Left ROT uses full 32-bit
rotate; right ZIN is logical, Arith is `s>>`.

---

## 8. System / misc instructions in handlers

| Mnemonic | opcode | P-code semantics | Cite |
|----------|--------|------------------|------|
| `IOX io` | op5=0x1D | `io_addr=(0,10)`; bit0=dir: even→`A = io[a]`, odd→`io[a] = A` | slaspec:688-700 |
| `IOT io` | op5=0x1C | same in/out logic as IOX | slaspec:674-685 |
| `IOXT` | 0xD10D | `A = ioxt(T, A)` (device address from T) | slaspec:703-707 |
| `MON n` | op8=0xD6 | monitor trap: `mon_code = imm8; nd100_mon(mon_code)` (traps to level 14) | slaspec:1108-1114 |
| `WAIT d` | op8=0xD2 | `nd100_wait(disp & 0xFF)` — relinquish level / halt if IONI off | slaspec:1973-1980 |
| `IOF` | 0xD101 | `IONI = 0` (interrupts off) | slaspec:662-665 |
| `ION` | 0xD102 | `IONI = 1` (interrupts on) | slaspec:668-671 |
| `POF` | 0xD104 | `PONI = 0` (paging off) | slaspec:1199-1202 |
| `PON` | 0xD108 | `PONI = 1` (paging on) | slaspec:1205-1208 |
| `PIOF` | 0xD105 | `PONI=0; IONI=0` | slaspec:1185-1189 |
| `PION` | 0xD10A | `PONI=1; IONI=1` | slaspec:1192-1196 |
| `SEX` | 0xD106 | `SEXI = 1` (24-bit extended addressing) | slaspec:1502-1505 |
| `REX` | 0xD107 | `SEXI = 0` (back to 19-bit) | slaspec:1330-1333 |
| `MST reg4` | op12=0xD0C | `nd100_mst(reg4, A)` — masked SET of system reg bits | slaspec:1149-1153 |
| `MCL reg4` | op12=0xD08 | `nd100_mcl(reg4, A)` — masked CLEAR of system reg bits | slaspec:1078-1082 |
| `IRW lvl reg` | op9=0x1AE | `nd100_irw(bit4, reg3, A)` — write A to reg at another level | slaspec:717-721 |
| `IRR lvl reg` | op9=0x1AF | `A = nd100_irr(bit4, reg3)` — read reg from another level | slaspec:710-714 |
| `TRA reg` | op12=0xD00 | `A = read_<reg>()` (per-reg pcodeop; unknown→`nd100_tra(reg4)`) | slaspec:1820-1883 |
| `TRR reg` | op12=0xD04 | `write_<reg>(A)` (per-reg pcodeop; unknown→`nd100_trr(reg4,A)`) | slaspec:1887-1942 |
| `MIN addr` | op5=0x08 | (see §1.4) increment memory, skip if zero | slaspec:1088-1095 |
| `OPCOM` | 0xD100 | `nd100_opcom()` — halt to operator | slaspec:1165-1167 |
| `IDENT lvl` | op10=0x31E | `A = nd100_ident(level6)` | slaspec:644-647 |
| `EXR SsRc` | op10=0x306 | execute `rop_src` as instruction | slaspec:576-579 |
| `LRB / SRB` | op10=0x356 / 0x354 | load/store register bank at X | slaspec:1048 / 1669 |

**TRA register selectors** (`tra_reg`, `nd100.slaspec:95`, constructors `1820-1883`):
`0=PANS 1=STS 2=OPR 3=PSR 4=PVL 5=IIC 6=PID 7=PIE 8=CSR 9=ACTL A=ALD B=PES C=PGC D=PEA F=CS`.
**TRR register selectors** (`trr_reg`, `nd100.slaspec:98`, constructors `1887-1942`):
`0=PANC 1=STS 2=LMP 3=PCR 5=IIE 6=PID 7=PIE 8=CCL 9=LCIL A=UCIL B=CILP D=ECCR F=CS`.

Privileged page-table ops (SETPT/CLEPT/CLNREENT/ENPT/REPT/CLPT/etc.) all reduce to
`nd100_page_op(...)` pcodeops (`nd100.slaspec:440-475`, `528-531`, `1318-1327`, `1495-1499`) —
their internal effect is not modeled in SLEIGH beyond the opaque pcodeop.

**Stack-frame ops** (fully modeled in P-code, relevant to PLANC handlers):
- `ENTR demand,err,norm` (0xC05D): checks `B+demand-122 > SMAX[B-125]`; on overflow → error
  return (`inst_start+2`); else builds frame `B = STP+128`, saves `LINK=L+1`, `PREVB`, `SMAX`,
  `STP`, and jumps to normal return (`inst_start+3`) (`nd100.slaspec:542-560`).
- `LEAVE` (0xC05E): `P = mem[B-128]; B = mem[B-127]` (`nd100.slaspec:1037-1045`).
- `ELEAV` (0xC05F): decrement LINK at `[B-128]`, store `A` as ERRCODE at `[B-123]`, then LEAVE
  (`nd100.slaspec:510-525`).
- `INIT` (0xC05C): `nd100_init(B,L,A,T,X)` (`nd100.slaspec:650-653`).

---

## Unresolved / caveats (explicit)

1. **STBTX** — no constructor in SLEIGH. `op10=0x31B` defines `fixed3` 0-6 only; there is no
   store-byte-TX form. UNRESOLVED (sleigh: fixed3=7 undefined at 0x31B; no `STBTX` mnemonic).
2. **SWAP modifier semantics** — the emitted P-code for plain / CLD / CM1+CLD SWAP performs a
   plain `dst = src` (with `src=0` when CLD); only `SWAP CM1` performs a true exchange. This
   contradicts the constructor's own header comment. Documented as-emitted (`nd100.slaspec:1780-1802`).
3. **LIN-right shift** — link-bit (M-flag) fill not modeled; treated as arithmetic right
   (`nd100.slaspec:1532`).
4. **T/X-indexed transfers** — the SLEIGH EA is `X + disp3` (X only); the "T" in the mnemonic
   does not enter the address arithmetic in these constructors (`nd100.slaspec:972-1034`).
5. **Opaque pcodeops** — byte/BCD/float/page/movew/irr/irw/mon/wait etc. are `define pcodeop`
   black boxes (`nd100.slaspec:109-189`); their operand *bindings* are exact but their internal
   arithmetic is not expressed as P-code (must be cross-checked against the emulator authority).
6. **Memory-reference flag caveat** — ADD/SUB/AAx overflow uses `scarry`/`sborrow`/dynamic
   sign logic; these are transcribed exactly but their equivalence to the real ND-100 hardware
   flag latch is a cross-check item.
