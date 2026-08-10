# Two-Bank Programs on ND-100

**How to split a program's code and data into separate address banks — which languages support
it, how to turn it on at compile time, and how to link and run the result.**

Everything in this document is sourced from real Norsk Data manuals already in this repo (mainly
[`Developer/MON/Monitor Calls.md`](../MON/Monitor%20Calls.md), the SINTRAN III Monitor Calls
manual) — not inferred. Where a claim could not be confirmed against a real source, it is marked
`[unverified]`.

---

## 1. What a "bank" is, and why a program would need two

The ND-100 is a 16-bit machine: a 16-bit register addresses at most 64K **words** directly. That
64KW unit is a **bank** — this is the same "bank" the rest of this repo's memory-architecture
notes use for the CPU's 24-bit physical address space (256 banks × 64KW). A program that fits
code and data together in one bank is a **one-bank program**. A program too big for that, or one
that simply wants code and data kept in separate address spaces, becomes a **two-bank program** —
one bank for instructions, a second bank for data — which is why the SINTRAN Monitor Calls manual
states the concrete numbers plainly:

> "The address area on ND-100 is 128 Kbytes for one-bank programs and 256 Kbytes for two-bank
> programs." — *SINTRAN III Monitor Calls* manual (this repo:
> [`Developer/MON/Monitor Calls.md`](../MON/Monitor%20Calls.md))

(128 Kbytes = 64K **words** = one bank; 256 Kbytes = 128K words = two banks — words, not bytes,
is this repo's primary unit for ND-100 memory, per the project's own convention.)

## 2. How you turn it on, per language — real, cited evidence

There is **no single universal switch**. Each language compiler has its own way of asking for
two-bank code, and the choice always shows up twice: once as a compile-time setting, and again as
which pair of **runtime library files** you feed to the linker afterward (`BRF-Linker` on ND-100
— see [LINKING-GUIDE.md](LINKING-GUIDE.md)). The two must agree — a program compiled for one bank
linked against 2-bank libraries (or vice versa) is exactly the failure mode the SINTRAN error
table warns about (see §4 below).

| Language | Compile-time switch | 1-bank link libraries | 2-bank link libraries | Source |
|---|---|---|---|---|
| **FORTRAN** (100) | `SEPARATE-DATA ON` before `COMPILE`, when the program needs more than 128 Kbyte | `MON-CALL-1BANK:BRF`, `FORTRAN-1BANK:BRF` | `MON-CALL-2BANK:BRF`, `FORTRAN-2BANK:BRF` | `Monitor Calls.md`, §1.4, verbatim |
| **PLANC** (100) | `SEPARATE-DATA ON` before `COMPILE`, same trigger as FORTRAN | `MON-CALL-1BANK:BRF`, `PLANC-1BANK:BRF` | `MON-CALL-2BANK:BRF`, `PLANC-2BANK:BRF` | `Monitor Calls.md`, §1.5, verbatim |
| **Pascal** (CAT-PASCAL, ND-100) | `$OPTION B2` compiler command, before `$COMPILE` | (not shown — the example only demonstrates the 2-bank form, `CAT-2BANK`/`CAT-FREE`) | `CAT-2BANK:BRF`, `CAT-FREE:BRF` | `Monitor Calls.md`, §1.2, verbatim — note this example is for the newer **CAT-PASCAL** compiler ("version A"), explicitly NOT the older `PASCAL-100`/`PASCAL-500` compilers the manual tells you not to use |
| **COBOL** (100) | not stated in the source read — the example links `MON-CALL-2BANK:BRF`/`COBOL-2BANK:BRF` directly, with no accompanying "before COMPILE" command shown the way FORTRAN/PLANC have. `[unverified]` whether COBOL needs an explicit compile-time flag at all, or whether 2-bank is purely a link-time choice for this language | (not shown in the excerpt read) | `MON-CALL-2BANK:BRF`, `COBOL-2BANK:BRF` | `Monitor Calls.md`, §1.3, verbatim |
| **C** (CC-100) | none found in any source read — every example (including the real `CSESSION:MODE` install-time smoke test decoded from the [ND-10760A CC-100 floppy](../../Installation/Software/ND-10760/ND-10760A/README.md)) only shows the link-time choice | (not attested — CC-100's shipped example only uses the 2-bank set) | `CC-2HEADER`, `CC-2BANK`, `CC-2TRAILER` | `CSESSION:MODE` (decoded from the real floppy) + this repo's `C-DEVELOPER-GUIDE.md` |
| **NPL** | Existing `NPL-DEVELOPER-GUIDE.md` text (not sourced to a manual — `[unverified]`) shows `*1BANK`/`*2BANK` **assembler-level directives** used inline around a memory-bank-switching code pattern. **This looks like a different mechanism** from the other languages' single global compile-time switch — it reads as a per-section placement directive (and the surrounding example is about runtime register-level bank switching via `*TRR 10`, not necessarily the same "program compiled as one unit spanning two banks" concept above). Do not assume it is interchangeable with `SEPARATE-DATA ON`/`$OPTION B2` without checking the actual NPL/MAC manual. | unknown | unknown | `NPL-DEVELOPER-GUIDE.md` (unverified in-repo text, not a manual citation) |
| **MAC/FMAC** (System package) | n/a — MAC assembles what you write; bank placement is the programmer's own responsibility via addressing, not a compiler switch | — | — | inferred from MAC's nature as an assembler, not compile-flag driven |

**Do not extrapolate this table to languages/products not listed** — SIBAS, BASIC, and the
ND-500-hosted compilers (which don't have a PROG/BPUN or bank concept at all — see
[LINKING-GUIDE.md §8](LINKING-GUIDE.md#8-what-each-language-actually-produces-evidence-from-real-install-sheets))
have not been checked for this document.

## 3. The full build sequence, worked example (FORTRAN, verbatim from the manual)

```
@FORTRAN-100
FTN: STANDARD-CHECK OFF
FTN: COMPILE EX-PROG:SYMB, EX-PROG:LIST, EX-PROG:BRF
FTN: EXIT

@BRF-LINKER
Br1: PROGRAM-FILE EX-PROG:PROG
Br1: LOAD EX-PROG:BRF, MON-CALL-1BANK:BRF, FORTRAN-1BANK:BRF   % 1-bank, default
Br1: EXIT

@EX-PROG
```

If the program is too big for one bank (128 Kbyte), the *only* things that change are: give
`SEPARATE-DATA ON` before `COMPILE`, and swap in the 2-bank libraries at link time:

```
@FORTRAN-100
FTN: STANDARD-CHECK OFF
FTN: SEPARATE-DATA ON
FTN: COMPILE EX-PROG:SYMB, EX-PROG:LIST, EX-PROG:BRF
FTN: EXIT

@BRF-LINKER
Br1: PROGRAM-FILE EX-PROG:PROG
Br1: LOAD EX-PROG:BRF, MON-CALL-2BANK:BRF, FORTRAN-2BANK:BRF   % 2-bank
Br1: EXIT

@EX-PROG
```

PLANC follows the identical shape (`SEPARATE-DATA ON` before `COMPILE`, then
`MON-CALL-2BANK:BRF`+`PLANC-2BANK:BRF` at link time) — see
[`Monitor Calls.md`](../MON/Monitor%20Calls.md) §1.5 for the verbatim example this is drawn from.

Pascal instead sets the option at the very top of the compile session:
```
@PASCAL
$OPTION B2
$COMPILE EX-PROG:SYMB, "EX-PROG:LIST", "EX-PROG:BRF"
*EXIT
@BRF-LINKER
Br1: PROGRAM-FILE "EX-PROG:PROG"
Br1: LOAD EX-PROG:BRF, CAT-2BANK:BRF, CAT-FREE:BRF
Br1: EXIT
@EX-PROG
```

## 4. What happens if you get it wrong — real runtime warning codes

The SINTRAN error/message table (`Monitor Calls.md`) documents exactly this failure mode:

| Code (octal) | Code (decimal) | Meaning |
|---|---|---|
| 307 | 199 | Warning: 2-bank prog. file, but segment is only 1-bank |
| 310 | 200 | Warning: no such page in data bank, program starts as 1-bank |

Both are about the **background segment size**, not the compile/link step — a two-bank program
needs a big enough terminal/RT background segment to actually hold both banks at runtime. This
repo has two independently-verified examples of the exact command and size needed:
[ND-10076J Pascal](../../Installation/Software/ND-10076/ND-10076J/README.md) and
[ND-210721C BRF-Linker](../../Installation/Software/ND-210721/ND-210721C/README.md) both require
```
@CHANGE-BACKGROUND-SEGMENT-SIZE <terminal number>,128
```
(`128` here is **K-words**, i.e. 256 Kbytes — matching the two-bank address-area figure in §1.)
Run this (once per terminal, or scripted at login) before running a two-bank program; skipping it
is what produces warning 307/199 above.

## 5. Open items

- Confirm whether COBOL genuinely has no compile-time flag for 2-bank, or whether the manual page
  showing it simply wasn't captured in the excerpt read for this document.
- Confirm whether C (CC-100) has any compile-time flag at all, or whether bank selection is purely
  a link-time library choice for that language — no source read so far shows a C-side switch.
- Resolve what NPL's `*1BANK`/`*2BANK` directives actually are and whether they relate to this
  document's topic at all — the existing `NPL-DEVELOPER-GUIDE.md` text is not sourced to a manual.
- ND-500 has no bank concept (flat, much larger address space per segment — see
  [LINKING-GUIDE.md §2.5](LINKING-GUIDE.md#25-psegdseg---nord-500-segments)); this document is
  ND-100-only.

---

## See Also

- **[LINKING-GUIDE.md](LINKING-GUIDE.md)** — BRF/PROG/BPUN mechanics, NRL, DITAP, and the
  PROG-vs-BPUN decision this document builds on.
- **[Monitor Calls.md](../MON/Monitor%20Calls.md)** — the source manual for every verbatim example
  in this document.
- **[C-DEVELOPER-GUIDE.md](../Languages/Application/C-DEVELOPER-GUIDE.md)**,
  **[FORTRAN-DEVELOPER-GUIDE.md](../Languages/Application/FORTRAN-DEVELOPER-GUIDE.md)**,
  **[PASCAL-DEVELOPER-GUIDE.md](../Languages/Application/PASCAL-DEVELOPER-GUIDE.md)**,
  **[PLANC-DEVELOPER-GUIDE.md](../Languages/Application/PLANC-DEVELOPER-GUIDE.md)** — per-language
  guides.
