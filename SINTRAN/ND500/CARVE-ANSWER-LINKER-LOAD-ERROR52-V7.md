# CARVE ANSWER (V7) - V6 RETRACTED; the object-OPEN subtree is DISJOINT from the error-52 parser

Answers the "DEABF dot-form disproven" follow-up (relayed 2026-07-18). Builds on and **retracts part
of** [`CARVE-ANSWER-LINKER-LOAD-ERROR52-V6.md`](CARVE-ANSWER-LINKER-LOAD-ERROR52-V6.md).
Binary: `D:\ND\500\nd-linker\linker-b01.dom.asm`. All addresses ND-500 VAs (hex).
Tags: **[V]** = read from these disasm bytes; **[I]** = inferred; **[OPEN]** = not proven.

---

## 0. RETRACTION (poisoned prior)

**V6's NET claim - "the fix is in what the resolution returns; DEABF's dot-form `B.NRF` becomes
B0040C3C's input" - is WRONG.** You disproved it live (DEABF OUT='B.NRF', error 52 unchanged, byte
identical), and the bytes agree: B0040C3C parses its window at `B0040C4F` **before** its internal
DEABF at `B0040D5C`, so DEABF's output cannot feed that parse; and the `B003DDCD` resolve subroutine
(selector 0x28) is never reached on the no-dot branch (`B003DD82 -> B003DE13` skips
`B003DDB0`-`B003DDFF`). I accept both. The dot-form idea is dead. [confirmed by your test + parse order]

Your redirected hypothesis 2 is the right one, and the call graph supports it: **the object-OPEN
subtree and the error-52 B0040C3C subtree are disjoint.**

---

## 1. Where the object 50B OPEN actually is - and which is which  [V]

There are two `MON 50B OPEN` (`call $0xF8000028`) sites; they are NOT equivalent:

- **`B004E874` = the USER-OBJECT opener.** Its name arg `b.0xEC` is **built dynamically** just before
  the call, wrapping the name in `'` quote chars (`B004E85E by move $0x27,...`), args
  `(b.0xEC, b.0x2C, b.0x34, b.0x3C)`. Dynamic quoted name = the file you typed. Routine `B004E80F`.
  [V, lines 94036-94048]
- **`B004CABC` = a FIXED-name work/scratch file**, NOT your object. Name arg is the constant
  `$0xB0054314`, access mode `b.0x2C = 3` set literally at `B004CAB9`. Routine `B004C9C1`. [V, lines
  91355-91363] Treat this as the linker's own temp/output file, not the LOAD target.

**Intended object-open chain (backward from B004E874):**
```
B004E874 (MON 50B OPEN, object)  <-  B004E80F  <-  B004AEBC     (command-loop-adjacent, entd @B004AEBC)
                                              <-  B004DC33
```
[V, caller trace]. `B004AEBC` sits immediately after the command-read loop and its keyword matcher
(section 2); it reaches the object OPEN **without** calling `B0040C3C` or `B003CFDA` (checked
`B004AEBC` body `B004AEBC`-`B004B0xx`: no call to either). [V for the window checked; full-routine
[I].]

## 2. The command dispatch (where LOAD is routed)  [V structure]

```
B004ACBF  call $0xF8000149   ; MON 511B - read a command line
B004ACD9  ents $0x30         ; keyword MATCHER: walks a command-name table at 0xB00530BC
          (r:= $0xB00530BC ; laddr r.0xC ; index by char ; AND $0x7F ; compare ; ...)
```
[V, lines 89274-89363]. So the typed command is matched against the table at **`0xB00530BC`**, which
yields the handler. **I did not decode that table**, so I cannot yet name the exact static handler
`LOAD` maps to - that is the missing link and the highest-value next step (section 4). [OPEN]

## 3. The error-52 subtree is a SEPARATE name subsystem  [V]

`B0040C3C` is reached during your run via `B003D0E2`, which lives in `B003CFDA`. `B003CFDA` is part of
a **mutually-recursive name-processing cluster** (`B003CFDA`, `B003D19E`, `B003D337`, `B003D442`) -
every call among them is internal recursion. This cluster + `B0040C3C` form a distinct subsystem from
the object-open subtree in section 1. [V - caller maps all internal]

Note also `B0040C3C`'s stage grammar (`;`->2, `)`->3, `.`->4; `:` is only a flag) is **not a plain
file-name grammar** - `;`/`)`/`.` are linker symbol/address/spec delimiters. That is consistent with
`B0040C3C` being a **symbol/spec resolver**, not the file opener. So handing it the file name `B:NRF`
and demanding stage 4 is what produces error 52. [I - grammar shape + the disjoint subtree.]

## 4. Net (honest) and the decisive next step

- **What I can prove:** the object is opened at `B004E874` (`B004E80F` <- `B004AEBC`/`B004DC33`),
  a subtree that does **not** pass through `B0040C3C`. Your LOAD dies earlier, inside the separate
  `B003CFDA`/`B0040C3C` name subsystem. So your hypothesis 2 (mis-route / wrong resolver) is the live
  lead, not any dot-form fix. [V for the subtrees; [I] that yours is mis-routed vs. a legit pre-open
  validation that should have passed.]
- **What I cannot yet prove:** the exact intended `LOAD -> handler -> B004E80F` path, because the
  command keyword table at `0xB00530BC` is not decoded. Also unproven: whether real HW's LOAD calls
  `B003CFDA`/`B0040C3C` at all, or only your run does.

**Decisive probes (yours to run, or point me and I'll keep carving the static side):**
1. **On a KNOWN-GOOD real-HW `LOAD B:NRF`, break at `B004E874` (and `B004CABC`)** and capture the
   full call stack. That is the intended chain, ground-truth. Diff it against your run's stack at the
   error - the first frame that differs is the fork.
2. **Trace your run from `B004ACBF`** (you offered) and log the FIRST entry into the `B003CFxx`
   cluster (`B003CFDA`/`B003D19E`/`B003D337`/`B003D442`) or into `B0040C3C`. Whatever calls into that
   cluster is your fork point.
3. If you want the static handler: I can decode the command-name table at **`0xB00530BC`** (matcher
   `B004ACD9`) to find which handler `LOAD` dispatches to, and whether it reaches `B004E80F` or the
   `B003CFxx` cluster. Say the word and I will carve it.

## Evidence register (linker-b01.dom.asm, PSEG VAs)

- Object OPEN `B004E874` dynamic quoted name build `B004E85C`-`B004E872`; routine `B004E80F`.
- Scratch OPEN `B004CABC` fixed name `$0xB0054314`, mode 3 `B004CAB9`; routine `B004C9C1`.
- Object-open callers: `B004E80F` <- `B004AEBC` (89448), `B004DC33`; `B004C9C1` <- `B004C95B` <-
  `B0035C96` <- `B0035ED3` (the 0x35xxx param subsystem).
- Command read `B004ACBF` (MON 511B); matcher `B004ACD9` over table `0xB00530BC`.
- Error subtree `B003CFDA`/`B003D19E`/`B003D337`/`B003D442` (mutually recursive) -> `B0040C3C`.
- (Retracted) V6 dot-form chain: disproven live + by parse-order (`B0040C4F` parse precedes
  `B0040D5C` DEABF; `B003DDCD` resolve unreached).
