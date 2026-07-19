# RPROC

MON 60 subfunction **RPROC = 077B** (octal) = **0x3F** = **63** decimal.
NPL purpose (authoritative, `5P-P2-MON60.NPL`): **READ MESSAGE**, server handler
`IRMESS`.

Note: `RPROC` is the yaml **client** name for this subfunction; the authoritative
server-side purpose is READ MESSAGE (`IRMESS`). Named here by the yaml client name
`RPROC`; no operator command name is invented.

All addresses are OCTAL ND-100 word addresses, BANK 1, base 0.
Source: `SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm`

## Handler location - six call sites

Every site marshals **two** parameters: one word is a small constant loaded
P-relative (a selector/code, role INFERRED) and one is a pointer to a buffer in the
caller's frame; their slot order varies.

| Call site | JPL | ptr -> thunk | Enclosing routine | `,X 6` | `,X 7` | Error (callsite+1) | Success (callsite+2) | Status |
|---|---|---|---|---|---|---|---|---|
| **001332** | `JPL I 135` | 001467 -> 146613 | 001264 (fs 000153) | const (`LDA 142`) | &(B-170) | 001333 `JPL -42` | 001334 `LDA ,B -152` | PROVEN |
| **001510** | `JPL I -21` | 001467 -> 146613 | 001264 | const (`LDA 37`) | &(B-170) | 001511 `JPL I 136` -> 001271 | 001512 `SAA 14` | PROVEN |
| **001545** | `JPL I -56` | 001467 -> 146613 | 001264 | const (`LDA -30`) | &(B-170) | 001546 `JPL I 101` -> 001271 | 001547 `LDA ,B -152` | PROVEN |
| **001625** | `JPL I -136`| 001467 -> 146613 | 001264 | const (`LDA -131`) | &(B-170) | 001626 `JPL I 21` -> 001271 | 001627 `LDA ,B -152` | PROVEN |
| **007531** | `JPL I 102` | 007633 -> 146613 | 002662 (fs 000331) | const (`LDA 107`) | &(B-52) | 007532 `JPL -32` | 007533 `RADD ...` | PROVEN |
| **104456** | `JPL I 105` | 104563 -> 146613 | 103722 (fs 000605) | &(B-125) | const (`LDA 106`) | 104457 `JPL I -77` -> 103735 | 104460 `JMP I 104` -> 105011 | PROVEN |

All six resolve to the single RPROC thunk **146613**. The four sites in routine
`001264` share pointer word `001467`; their error paths converge on routine `001271`.

**Note on 001510**: its parameters are stored at `001450-001455`, then `001456 JMP 32`
jumps **over** an intervening pointer/constant pool (`001457-001507`, mis-decoded as
instructions per prog.md sec 9.1) to land exactly on the `001510` call. The call site
is genuine; the store->call adjacency is broken only by that pool.

Thunk bytes (verified): `146613`=`170477` (`SAA 77`), `146614`=`125001`, `146615`=`146244`.

## MON 60 subfunction used

| Subfn | Octal / Hex / Dec | Thunk | Parameter-block layout | Skip/Error |
|---|---|---|---|---|
| RPROC | 077B / 0x3F / 63 | 146613 | 2 words: a P-relative constant (selector) + a frame buffer pointer (order per site) | see per-site table |

## What it does

At each site the caller loads a small constant into one param slot and a pointer to a
frame buffer into the other, then issues `MON 60` RPROC to read a message. The four
`001264` sites are variants (different constants) reading messages within one routine;
`007531` is an interpreter case; `104456` reverses the slot order (buffer in slot 1).

## Unknown / inferred

- **PROVEN**: two parameter words at every site.
- **INFERRED**: the P-relative constant is a message selector / mailbox id / length,
  and the frame pointer is the receive buffer. The exact constant values were not read
  back and the input/output split is not traced; the store operations are PROVEN.
- **INFERRED (roles)**: routine `001264` is a message-reading routine (issues RPROC
  four times); `001271` is its local error target; `103722` a larger routine. ENTER
  addresses and framesizes are PROVEN; internal behaviour not carved.
