# 051B-RIFRG

MON 60 subfunction **RIFRG = 51B = 0x29 = 41 decimal**.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ INTERFACE (COMMUNICATION), IODATUT REGISTER**, server handler `5NOPAR`.

All addresses OCTAL, ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

60B yaml client name = `RIFRG`.

## Thunk (PROVEN, bytes read)

| Addr | Word | Meaning |
|---|---|---|
| `146514` | `170451` | `SAA 51` (subfunction 51B) |
| `146515` | `125001` | `JMP I 1` (-> gateway pointer) |
| `146516` | `146244` | pointer to gateway `146244` |

## Call site(s)

| Item | Value | Status |
|---|---|---|
| MON 60 call site | **130136** `JPL I 35` -> ptr `130173`, `bank1[130173]=146514` = thunk `SAA 51` | PROVEN |
| Error path (callsite+1) | 130137 `JMP I ,B -141` = frame-relative (B-relative) error EXIT. The disassembler's P-relative `; -> 127776` annotation is INVALID for a `,B` operand; the true target is runtime `B-141`, a saved abort vector. | PROVEN (opcode); target dynamic |
| Success path (callsite+2) | 130140 (fall through) `SAA 40` = next operation in the routine | PROVEN |
| Enclosing ENTER routine | 127551 (framesize 000010) - a standalone ENTER routine | PROVEN (prologue `RADD AD1 CLD SL DX`+ENTER) |

## Parameter block (PROVEN stores)

| Slot | Store | Value |
|---|---|---|
| `,X 6` | `130135 STA ,X 6` | parameter 1 = `&(B-167)` (buffer to receive the interface/IODATUT register) |

Gateway convention (prog.md sec 4.4): `LDX ,B -176` sets `X` = stack top; `STx ,X 6/7/10`
are MON 60 parameters 1/2/3.

## What it does

1. Builds `&(B-167)` (address of a local buffer), stores it as param 1. (`130132-130135`)
2. Issues `MON 60` RIFRG to read the ND-500 interface / IODATUT communication register. (`130136`)
3. On error takes the routine's `,B`-relative abort exit `130137`; on success falls through to the next step `130140`.

## Unknown / inferred

- **PROVEN**: param 1 store `130135 STA ,X 6 = &(B-167)`.
- **INFERRED**: the buffer `B-167` receives the IODATUT/interface register value (per NPL purpose); the value layout was not carved.
- **NOTE**: `130137 JMP I ,B -141` is a `,B`-relative indirect jump. Every such line in this routine carries a bogus P-relative `; ->` comment; the real target is `B-141` at run time (an abort/error return address saved in the frame). Marked as dynamic.

octal=hex=decimal: 51B = 0x29 = 41 decimal
