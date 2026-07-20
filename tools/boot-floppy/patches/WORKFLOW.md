# Workflow: identifying and replaying SINTRAN III patches

Full path: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\patches\WORKFLOW.md`

This is a **proposal**, not a validated procedure. Read `README.md` first —
it establishes the format facts this workflow rests on. Every step below is
labelled:

* **PROVEN** — demonstrated on real media in this repo, output in `samples/`.
* **PLAUSIBLE** — follows directly from proven facts but not yet executed.
* **GUESSWORK** — I do not have evidence; must be tested before trusting.

---

## Part A — Identify what was patched on an installed system

### A0. Read `REVLE`. Always do this first. — **PROVEN**

```
python3 tools/read_revle.py <carver-version-dir>
```

One word tells you the ND patch level the system was brought to, if any.

Measured today:

```
K-VSX-500  REVLE=010200   -> patch set K-10200   (we hold the floppy)
L-VSX-500  REVLE=000000   -> no ND patch level recorded
M-VSX-500  REVLE=003200   -> patch set M-3200    (floppy not held)
```

**Trap:** `REVLE = 0` is *not* proof of a virgin system. It only proves no ND
patch floppy wrote a level. Site patches leave no trace here.

**Trap 2:** `REVLE = X` is *not* proof that every report ≤ X is present.
H-series patch sets *withdrew* reports (README §7). It tells you which
**patch-set release** was applied, nothing finer.

### A1. Check the `S3PATCH` residue. — **PROVEN (weakly informative)**

`S3PATCH` all-zero (as on L-VSX-500) is corroborating evidence that no patch run
ever happened. Non-zero with `004057` at the segment's base word is the residue
of `PATCH-FILE:MODE`'s `)CLOAD S3PATCH` / `<base>/REVLE`. It carries **no list
of applied reports** — I checked; the remainder is DMAC working data I have not
decoded.

### A2. Diff against an as-shipped baseline. — **PLAUSIBLE, blocked**

```
python3 tools/diff_system.py --dirs shipped/segments installed/segments \
        --symbols .../SYMBOL-1-LIST.SYMB.TXT
```

Emits every differing word run as a candidate patch record, annotated with the
nearest preceding symbol so the output reads like a `.PATC` address expression
(`XRTAC+6`, `FIXCL`, …). Demonstrated end-to-end on a real pair in
`samples/L-VSX-500-IDPIT-vs-SDPIT.json`.

**Blocker:** we do not currently hold a *binary* as-shipped baseline for any
carved system. `D:\ND\extract\VSXL1\SINTRAN-L-1.DATA` is a MACM **source /
generation stream**, not segment images. Two different SINTRAN versions are not
a substitute — `--dirs K-VSX-500 L-VSX-500` produces 12 100 diff runs /
491 328 words, i.e. noise.

To unblock, one of:
1. run the L (or M) generation stream on an emulated ND-100 to produce virgin
   segments, then carve them the same way; or
2. find/carve a never-patched disk image of the same version *and the same
   generation options* — note that generation options (`8N500`, `8XMSG`, …)
   change the image, so the baseline must match the site's generation, not just
   its version letter.

**Trap:** even with a correct baseline, a diff picks up *runtime state*, not
just patches. The `samples/L-VSX-500-IDPIT-vs-SDPIT.json` run shows this
clearly: `LOADI`, `FIXCL`, `XTMRT` differ between the initial and current copies
of the same PIT purely because the system ran. Any diff-based identification
must exclude live data areas.

### A3. Fingerprint by `% OLD:` value — the promising route. — **PLAUSIBLE**

This is the one that works **without** a baseline image, and I think it is the
right thing to build next.

60–70 % of the deposits in every patch file record the pre-patch word:

```
OPSYD+27/ JMP I *+1                % OLD: 142065
```

So for a candidate patch you have both the "before" word and (after
assembling) the "after" word, plus a symbolic address. Procedure:

1. `python3 tools/parse_patch.py PATCHES.PATC --deposits-only > deposits.json`
2. Resolve `address_expr` against the target system's `SYMBOL-1-LIST` /
   `SYMBOL-2-LIST` (this repo has K03, L07, M06 under
   `SINTRAN\NPL-SOURCE\SYMBOLS\`).
3. Assemble `new_expr` with `nd100-as` to get the expected word.
4. Read the actual word out of the carved segment named by the deposit's
   `context.coreload`.
5. Classify per deposit: **APPLIED** (actual == new), **NOT APPLIED**
   (actual == `% OLD:`), **UNKNOWN** (neither).
6. Roll up per report number → a per-report applied/not-applied verdict.

That produces the patch inventory the system itself does not store.

#### A3 pilot — actually run, and it is NOT yet conclusive

Implemented as `tools/check_applied.py`; output saved as
`samples/K-10200-vs-K-VSX-500-pilot.txt`.

I ran a cut-down version of steps 1–5 today against the one pair where we have
both halves: patch set **K-10200** (`D:\ND\S\N-250306K05-patch.img`) versus the
carved **K-VSX-500** system that reports `REVLE = 010200`.

Method: take every `open`-kind deposit that (a) carries `% OLD:`, (b) has a
simple `SYMBOL` or `SYMBOL+octal` address, (c) names a coreload that exists as a
carved segment, and (d) whose symbol is in `SYMBOLS\K03\SYMBOL-1-LIST` +
`SYMBOL-2-LIST`. Resolve the address, read the word from the carved segment at
`meta.json`'s `load_address`, compare with `% OLD:`.

Result:

```
open-location deposits                       1875
  ... with a recorded % OLD:                 1251
      ... coreload not a carved segment       271
      ... address expression too complex      522
      ... symbol not in the symbol list       244
      ... resolvable                          214
            APPLIED                             1
            NOT-APPLIED                         8
            UNRESOLVED-NEW                    200   (new value is assembly)
            MISMATCH                            5
```

Reading this honestly:

* 8 exact 16-bit hits on the `% OLD:` value out of 214 is far above chance
  (chance would be ~0.003 hits), so **symbol resolution is working at least
  partly** — this is not pure noise.
* But 200 of 214 are undecidable without assembling `new_expr`, so the run
  cannot say "these patches are applied". It can only say "8 specific deposits
  appear NOT to be applied".
* The 5 outright mismatches are the warning sign. Most likely cause
  (**GUESSWORK**): the `K03` symbol list is not the symbol list of *this*
  machine's generation. Symbol values are generation-specific, and a
  patch run uses `(SYSTEM)SYMBOL-1-LIST` **from the target machine**, which we
  do not have for K-VSX-500.

**Conclusion of the pilot:** the approach is not disproven, but it is not yet
usable. The blocking dependency is not the patch format — that is solved — it is
(i) an ND-100 assembler pass over `new_expr`, and (ii) the *machine's own*
symbol lists rather than a generic one for the version letter.

**What must be validated first**
* That `context.coreload` (`S3SSM5`, `S3SMPIT`, …) maps 1:1 onto the carver's
  segment files. It looks like it does (names match exactly) — **GUESSWORK**
  until checked.
* That the K-era segment save files on disk are what the carver extracted, i.e.
  that `)CLOAD S3SSM5` addresses the same bytes the carver calls `062-S3SSM5.bin`.
* Address base per segment: the carver's `.meta.json` `load_address` must be the
  same origin MACM uses. For `S3SDPIT`/`S3IDPIT` this is corroborated
  (`REVLE = 4057`, base `4000`, and the word we read there matches a patch level
  we hold on floppy). For other segments it is **unverified**.
* Symbolic addresses in H/J patch files often target `SINTRAN:DATA` /
  `MACM-AREA:DATA` (image files), not segments — those deposits cannot be
  checked against carved segments at all without knowing the image layout.

### A4. Site patches — **GUESSWORK**

Nothing in the format helps. A site patch applied by an engineer at a console
leaves no record anywhere. The only route is A2 (baseline diff), with everything
that is not attributable to A3 treated as a candidate site patch.

---

## Part B — Replay a patch set onto another system

### B1. The honest option: use the media as intended. — **PLAUSIBLE**

The floppy already contains the complete, self-describing procedure:
`START-PATCH-FILE:MODE` → `PATCH-FILE:MODE` → `PATCHES:PATC`. If we can run
SINTRAN under emulation with DMAC loaded (`READ-BINARY DMAC-1915F 7`) and mount
the patch floppy, the correct move is to **let ND's own tooling do it**, then
verify by reading `REVLE`.

Prerequisites, all **PLAUSIBLE, none proven**:
* the target's `SYMBOL-1-LIST` / `SYMBOL-2-LIST` must be present under
  `(SYSTEM)` — `PATCH-FILE:MODE` `)9ASSM`s them explicitly;
* `SEGFIL0` must be definable (`DEF-SEG-FILE YES YES 0 SEGFIL0`);
* DMAC version matters — the floppies warn in a comment:
  `@@  If this job is aborted after calling DMAC / @@  your DMAC is probably
  older than version-F`;
* the target must be the same SINTRAN family (a SIN-K patch set on a J system is
  meaningless — the symbol values and segment layouts differ).

This route is **far safer** than reimplementing, because the patch scripts are
programs, not data: they read the target's segment table (`SG12S`, `SG13L`, …)
at run time and assemble new code into spare space with `)FILL`. A static
re-implementation would have to reproduce MACM/DMAC semantics faithfully.

### B2. The offline option: apply parsed records to segment files. — **GUESSWORK**

Technically possible for the *simple* deposits (single word, literal or
short-expression value, known symbol, `% OLD:` present so it can be verified
before writing). It is **not** possible for:

* any patch that assembles a block of new code into free space and then
  re-points the original code at it (the majority of the interesting ones —
  see SIN-K 449 in `samples/K-011411-report-449.json`, twelve sequential
  instruction words plus symbol redefinition `)KILL SG62F; SG62F=*; SG62F:`);
* anything using `^` (current contents) in the value expression;
* anything inside a conditional guard whose generation flag state we do not know.

If it is ever attempted, the mandatory safety rule is: **refuse to write unless
the target word currently equals the `% OLD:` value.** That single check is what
makes the format tractable at all.

### B3. Cross-version replay — **do not**

A SIN-J patch on a SIN-K system is not merely risky, it is category-wrong: the
report numbering, the symbol values, the segment names and even the target
mechanism (image file vs segment save file) all changed between families.

---

## What I would validate before trusting any of this

1. Confirm the coreload-name → carved-segment-file mapping for K-era patches
   (A3). Cheap, and everything else depends on it.
2. Confirm `)KILL` / `)FILL` / `)RESSM` / `^` semantics from
   `Reference-Manuals\ND-60.096.01 MAC Interactive Assembly and Debugging
   System User's Guide.md` and `ND-60.009.02 MACM…`. Currently inferred.
3. Produce **one** binary as-shipped baseline by generation, and run A2 against
   it. Until that exists, "identify what was patched by diffing" is a design,
   not a capability.
4. **Find K-VSX-500's own `SYMBOL-1-LIST` / `SYMBOL-2-LIST`** (on the disk the
   segments were carved from, under `(SYSTEM)`), then re-run the A3 pilot. The
   pilot above used the generic `SYMBOLS\K03` lists and produced 5 outright
   mismatches, which is exactly what a wrong symbol list looks like.
5. Add an `nd100-as` assembly pass over `new_expr` so APPLIED can be tested, not
   just NOT-APPLIED. Without it the pilot can only ever say "not unpatched".

Steps 4 and 5 together are the highest-value next action; they are what turn
this from a format study into a working identification tool.
