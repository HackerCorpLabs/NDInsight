# TASK-05 execution plan — undocumented/unclear MON calls (ND-100 + ND-500)

Plan for reverse-engineering/verifying the MON calls in
[TASK-05-undocumented-mon-calls.md](TASK-05-undocumented-mon-calls.md). Ordered by
wave: highest-confidence / lowest-cost first.

## Reframe: source-first (this changes TASK-05's method)

TASK-05 §D assumed *live DAP for everything*. Recon of the NPL source
(`../../../SINTRAN/NPL-SOURCE/`) shows that **~half the calls — including all three
showstoppers (511/512/513) — have full labeled handler bodies in the source**, so
they are recoverable by reading code, no running system needed. The ND-500 System
Monitor `S3SM5` only *packages* calls; the ND-100 driver `MP-P2-N500.NPL` *services*
them on level 12. So the plan front-loads source reads.

---

## Wave 1 — Read handlers straight from NPL source (no running system)

Extract each labeled body + its parameter contract. **Clears the 3 showstoppers.**

| Calls | Handler (source) | File / line |
|-------|------------------|-------------|
| 511 DVIO | `DVIO` | `MP-P2-N500.NPL:1688` |
| 512 / 513 | `A5XMSG` / `B5XMSG` (XMSG A/B split) | `MP-P2-N500.NPL:2062` |
| 515 5MTRANS | `5MTRANS` (real name; not "SMTRANS") | `MP-P2-N500.NPL:2440` |
| 500 / 501 / 505 / 510 | `STAPROC` / `NSTOPROC` / `GERRC` / `SWMC` (GOSW, index = MON−500) | `MP-P2-N500.NPL` (MCHANDEL 1286, GOSW 1385) |
| 313 IBRSIZ | `IBRSIZ` | `RP-P2-MONCALLS.NPL:2938` |
| 45 / 51 DBRK | `BRPNT` / `DEBUGGER` (16-subfunction GOSW) | `RP-P2-MONCALLS.NPL:1871/1965/2043` |
| 132 MEXIT, GDEVTY | `MEXIT`, `GDEVTY` | `RP-P2-SEGADM.NPL:31`, `RP-P2-MONCALLS.NPL:2603` |
| (dispatch) | `ENT14`, `GOTAB` | `MP-P2-2.NPL:181/184`, dispatch `:387` |

Also reconcile the names against the friend's labels: **505 = `GERRC`** ("get error
code", not "GetTrapReason"); **510 = `SWMC`** (labeled both "swapper MON call" and
"switch context" — disambiguate from the body); **512/513 = `A5XMSG`/`B5XMSG`** (an
A/B split — reconcile with "512 XMSG / 513 convert-domain-a03").

Deliverable: VERIFIED entries in `../versions/L-VSX-500/re/TASK-05-results.md` with
handler address, parameter registers / ND-500 message offsets, and return values.

## Wave 2 — The 410–427 handlers (the real gap: 7 calls)

Routing is VERIFIED (`MCHANDEL` → `NORMMC` → `5RRTWT` → ND-100 **level-1 shadow RT**
"system monitor"), but the handler bodies are not located. Steps: (1) grep source
for the level-1 RT monitor dispatch; (2) if absent, check the carved `S3SM` (seg 71,
service/mail) and the resident common-code image (now correct — see
`../EXTRACTING-RESIDENT-CODE.md`); (3) else live DAP: breakpoint the forward path,
issue a `MON 410`, trace where it lands, disassemble. Covers fixseg/unfix/wsegn/
mxpisg/sprname/gprnum/gprname.

## Wave 3 — ND-100 file-system calls not in source

5 RDISK, 6 WDISK, 67 OSIZE, 74 SETBT, 75 REABT, 120 WFILE, 144 MAGTP, 327 FSMTY —
symbols-only in the NPL tree (file-system module absent). **First re-test the
corrected carves**: TASK-05 §D's "not in any carved .bin" verdict was made on the
*mis-based* old L carves, so re-check the handler symbol addresses against the
corrected `006-S3FS.bin` and the resident common-code image. If still absent → live
DAP at the handler addresses (`RDISK` 102021 = `0x8411`, `WFILE` 102132, `MAGTP`
114707, `OSIZE` 111254, `SETBT` 112200, `REABT` 104005, `FSMTY` via GOTAB), invoke,
disassemble the mapped view. `MAGTP` (144) and `FSMTY` (327) additionally need their
**function-code dispatch tables** dumped.

## Wave 4 — TSS carryovers + odds

13/14/15, 42, 51, 304 (SIBAS, low). **`GOTAB` is in source** (`MP-P2-2.NPL:184`), so
read `GOTAB[n]` directly to get each handler label, then body-from-source or DAP.
Mostly source-based.

---

## Wave 5 — Disassemble the ND-500 System Monitor `S3SM5` (NOW VIABLE)

Gives the ND-500 *side* of the calls (how S3SM5 packages 511/512/513 before trapping
to the ND-100), complementing Wave 1. The earlier "53% undecodable" verdict was on
the **mis-based old carve**; on the corrected `030-S3SM5.bin` the code decodes at
~14% `???` and the file is a real ND-500 object image.

**Key facts (verified):**
- The carve is a **SINTRAN in-memory ND-500 segment image**, not a linker `:SEG`/
  `:DOM` file. `nd500-dis`'s SEG parser false-positives on byte 6 (`0xAA`, bit 7 =
  "is ND-500") and reads Entry/Program/Data from linker-header offsets that here are
  just code — hence the garbage `Entry 0x92AC92AC`, `Program 0x88009800`.
- Layout: `0x00-0x03` SINTRAN `[start,end]` prefix (`026000 177777`); `0x04-0x5F`
  image header + version banner (`"88. 8.17'"`, `"L00'"`); **`0x60+` an entry-point
  vector table** of 16-bit big-endian routine offsets; message/table pools; routines
  from ~`0x8BAE` with inline `$`-terminated strings.

**Method (nd500-dis tooling fix already applied — see below, so no workaround):**
1. Run `nd500-dis -a <segment>` directly — it now auto-detects the SINTRAN image as
   RAW and disassembles. `nd500-dis` reads big-endian as-is (no byte-swap, unlike
   `nd100-dis`).
2. Parse the **vector table at 0x60** for routine entry offsets (e.g. `8bae 8bb5
   8bf1 8c23 8c52 8c88 8c8c 8cb5 8cc1 8ccd 8cdd 8cf0 …`). These are the seeds.
3. Control-flow disassemble each routine from its entry (`nd500-dis -a -s <off>`),
   stopping at the next vector / inline string. Confirmed: `-s 8bae` yields ND-500
   code then the inline message `"…llegal access code"`.
4. Correlate the routines with the Wave-1 ND-100 handlers (the MON packaging for
   511/512/513/515) and with the message/string pool for labels.

Optional deeper seed: run S3SM5 under the ND-500 emulator `~/repos/nd500x/build/bin/
nd500x` with tracing to capture live entry points.

### Tooling fix (DONE 2026-07-10)
`nd500-dis`'s over-eager DOM/SEG detection is fixed in
`/home/ronny/repos/ragge/pcc-nd500/src/lib/dom/dom_utils.c` `dom_detect_file_type()`:
it now sanity-checks the SEG program descriptor (LB @0x14, SZ @0x18, big-endian) and
rejects a SEG match when `LB > size`, `SZ > size`, or `LB + SZ > size`, falling
through to RAW. This unblocks every SINTRAN-carved ND-500 segment (S3SM5, S3SSM5, the
M06 ND-5000 pair) with no per-file hack. DOM and a.out detection are unaffected. The
change is local to the `pcc-nd500` repo (not committed).

---

## Cross-cutting

- Every VERIFIED result feeds back into `../../../Developer/MON/calls/*.yaml` and
  `Developer/MON/Monitor Calls.md`, marked VERIFIED vs UNCERTAIN.
- Consolidated write-up: `../versions/L-VSX-500/re/TASK-05-results.md`.
- Answer the friend's specific yes/no questions (TASK-05 §A) per call (e.g. MON 75
  for file 0, MON 120 seek-to-nonzero, MON 313 devices-only).
