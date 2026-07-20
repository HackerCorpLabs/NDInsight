# CARVE REQUEST — Q-OCT-22: the ND-100 code that patches control-store page 0

**Ready to dispatch when the account weekly limit resets (Jul 22, 1pm Europe/Oslo).**
Hand the "PROMPT TO DISPATCH" block below to a general-purpose carve agent verbatim.

## Why this matters (do not skip)

Remediation item **T0-1** (derive the ND-5000 mailbox base from control-store word `000026`
`START_MESS`) rests on one **[INFER]**: that `START_MESS` and `SAMSON_CPU` are among the "system
parameters" SINTRAN patches into the first page of `CONTROL-STORE:DATA` before the ACCP burns it.
The *patching* is `[READ]` from `ND-05.017.01 HARDWARE MAINTENANCE:3961`. That `START_MESS`
**specifically** is patched is inferred. Per the project rule "no assumptions — if we can find out,
we do the work," this must be carved before T0-1 is implemented, not after.

The patcher is **not** in `NPL-SOURCE/NPL/*.NPL` and **not** in `MON-DEBUG:PROG` — it is in an
uncarved segment (or the resident).

## PROMPT TO DISPATCH

> Static carve, SINTRAN III VSX/500 L. EVIDENCE ONLY: cite file:line or octal addresses for every
> claim; mark inference as [INFER]; "not found" is a valid answer; never guess.
>
> GOAL: find the ND-100 code that **patches system parameters into the first page of
> `CONTROL-STORE:DATA`** before that page is sent to the ACCP for the control-store load. Confirm or
> refute that **`START_MESS` (control-store word `000026`) and `SAMSON_CPU` (word `000025`)** are
> among the patched words, and report EXACTLY which control-store words get patched and with what
> values.
>
> ESTABLISHED CONTEXT (given):
> - `ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE:3961` [READ]: "load the first page of
>   CONTROL-STORE:DATA into the transmission buffer. Some system parameters are patched into this
>   first page. Then this page is read by the ACCP and loaded into the ND-5000 control store memory."
> - The CS-load worker chain is: `LDSWA`(143551) and the LOAD-CONTROL-STORE path via the `500IN`
>   init state machine (`075150`); `CHECS`(074722) prints "> Loading Control Store"; `CCSLO`(075016)
>   → `CSDFI`(075074) hold the file-name templates `(SYSTEM)CONTROL-STORE:DATA` and
>   `(SYSTEM)CONTROL-1-STORE:DATA`. The loader that DMAs file→memory is `PLSWA`(144212)/`144117`
>   (`MON 131` ABSTR). All in segment `030-S3SM5` (load base 040000B).
> - Control-store page 0 constants (from the A30/B30 disassembly, `ND5000UC\microcode\
>   MICRO-5800-A30.md:30-37`): `000020 OFFSET`, `000021 PSTBASE`, `000025 SAMSON_CPU`,
>   `000026 START_MESS` (LARG=0o20000=0x2000), `000027 ZERO_P`. These are the two constants that
>   cannot be static (per-boot / per-CPU) and are the prime patch candidates.
>
> ANSWER:
> 1. Locate the code between reading `CONTROL-STORE:DATA` into a buffer and handing that buffer to
>    the ACCP (the transmission-buffer stage). Which routine writes into the just-loaded first page
>    before the ACCP command? Quote it.
> 2. Which control-store WORD OFFSETS does it write, and with what source values? Map each patched
>    offset to its symbol (`START_MESS`=026, `SAMSON_CPU`=025, `OFFSET`=020, etc.). If `START_MESS`
>    is NOT patched, say so — that would refute T0-1's premise and is a critical finding.
> 3. Where do the patched VALUES come from (which SINTRAN variable / datafield cell)? Specifically:
>    what is the source of the value written into the `START_MESS` slot, and does it equal
>    `5FPMAILBOX`-derived / `5MBBANK` / an ADRZERO-relative address?
> 4. Is the patch CPU-generation-gated (the discriminator is
>    `(mem[mem[B-56]+27] /*CPUAVAILABLE*/ & 000007) == 3` for SAMSON)? Quote any such test on this
>    path.
> 5. If not found in segment `030-S3SM5`, search the resident
>    (`...\L-VSX-500\resident\SINTRAN-DATA_commoncode.dis`, base 0, disassembled) and other segments.
>    Report where you looked and what you found or ruled out.
>
> SOURCES:
> - Disassembly: `E:\Dev\Ronny\NDInsight\tools\sintran-segment-carver\versions\L-VSX-500\re\
>   segments-ref\030-S3SM5\030-S3SM5.asm` (+ `.symbols.txt`); resident `.dis` under
>   `...\L-VSX-500\resident\`.
> - Symbols: `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07\*.SYMB.TXT` (L07, NOT M06).
> - NPL (DIFFERENT REVISION — logic only, never authority):
>   `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\NPL\*.NPL` (esp. `5P-P2-MON60.NPL`, `RP-P2-N500.NPL`).
> - Prior carves: `E:\Dev\Ronny\NDInsight\SINTRAN\ND500\OPEN-QUESTIONS-REGISTER-2026-07-20.md` §2.6,
>   `XMSINIT-BUFFER-GEOMETRY-CARVE-2026-07-19.md`, `WHERE-IS-5MPM-LOCATED.md`.
> - Manual: `E:\Dev\Ronny\ND5000UC\manual\` and
>   `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01*`.
>
> Return a structured report separating READ from INFER. If you cannot find the patcher, report the
> negative result and where it must live, rather than inferring its behaviour.

## What to do with the result

- If `START_MESS` is confirmed patched with an `ADRZERO`/`5MBBANK`-derived value → **T0-1 is
  evidence-backed; implement it** (derive base from loaded CS word `000026`).
- If `START_MESS` is NOT patched → **T0-1's premise is refuted**; fall back to the X5ACT
  self-discovery route (register §2.6 candidate (b)) and reopen how the microcode's fixed `0x2000`
  reconciles with runtime allocation.
- Either way, update `OPEN-QUESTIONS-REGISTER-2026-07-20.md` §2.6 / Q-OCT-22 and
  `REMEDIATION-PLAN-OCTOBUS-TRACK-2026-07-20.md` T0-1 with the verdict.
