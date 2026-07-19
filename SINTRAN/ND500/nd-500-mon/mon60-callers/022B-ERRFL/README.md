# 022B-ERRFL

MON 60 subfunction **ERRFL = 22B** (octal) = **0x12** = **18** decimal.
NPL purpose (authoritative, `SUBFUNCTION-TABLE.md`): **(set error flag)**.
Server handler: **5NOPAR** (generic forward path).

All addresses OCTAL, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Thunk (PROVEN, from bytes)

| Addr | Word | Meaning |
|---|---|---|
| 146376 | 170422 | `SAA 22` -> subfunction code 022 (18 dec) |
| 146377 | 125001 | `JMP I 1` |
| 146400 | 146244 | -> gateway |

## Call sites

| Call site | Enclosing routine | JPL | Pointer word | Thunk | Status |
|---|---|---|---|---|---|
| 005173 | **CASE 005170-005175** inside command interpreter **002662** (spans 002662-010634) | `JPL I 52` -> 005245 | `bank1[005245]=146376` | `SAA 22` | PROVEN |
| 005201 | **CASE 005176-005203** inside command interpreter **002662** | `JPL I 44` -> 005245 | `bank1[005245]=146376` | `SAA 22` | PROVEN |

Both cases resolve through the *same* pointer word 005245 (`=146376`).

## Parameter block (X := b.-176; one slot)

| Site | Set at | Value | Source |
|---|---|---|---|
| 005173 | 005172 `STA ,X 6` | **010636** | `LDA 54` @005170 loads pool word [005244]=010636 |
| 005201 | 005200 `STA ,X 6` | **010634** | `LDA 50` @005176 loads pool word [005246]=010634 |

The two cases are identical except for the single constant placed in `,X 6`: 010636 vs
010634. (Both `LDA 54`/`LDA 50` are P-relative direct loads of pool words; verified:
005170+54 = 005244 = 010636, and 005176+50 = 005246 = 010634.)

## Skip / error handling

- 005173: err 005174 (`JPL I -155`->005017 = routine **002673**); ok 005175 (`JMP I 35`->005232 = **010613**).
- 005201: err 005202 (`JPL I -163`->005017 = routine **002673**); ok 005203 (`JMP I 27`->005232 = **010613**).

## Unknown / inferred

- **INFERRED**: the two constants 010636 and 010634 are the two error-flag values set by
  the two cases (e.g. an on/off or two distinct flag codes). The values and their load
  paths are PROVEN; their exact meaning is inferred. They happen to lie at/just past the
  end of the interpreter region (010634 = the interpreter's last address per the mechanism
  spec), so they may be pointers/addresses rather than plain codes - UNRESOLVED.
- **PROVEN**: thunk bytes, pointer 005245->146376, both call sites, the `,X 6` stores and
  their loaded constants, and the callsite+1/+2 targets (002673 / 010613).
