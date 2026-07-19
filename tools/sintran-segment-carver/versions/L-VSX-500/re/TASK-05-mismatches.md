# TASK-05 - mismatches between the request and the findings

Everything in the friend's TASK-05 request
([../../../ghidra-tasks/TASK-05-undocumented-mon-calls.md](../../../ghidra-tasks/TASK-05-undocumented-mon-calls.md))
that turned out **different** from what he assumed, with the evidence. Companion to
[TASK-05-results.md](TASK-05-results.md). Grouped: wrong premises about where the
code lives, wrong call identities/names, wrong behaviour assumptions, confirmed
hypotheses, and the calls that could not be resolved.

## A. Wrong premises about extraction / where the code lives

| # | Friend's premise (§B/§C/§D) | Finding | Evidence |
|---|----------------------------|---------|----------|
| A1 | "MON dispatch + `GOTAB` are in `116-S3SERWD.bin`, base 0x600, `GOTAB` at octal 071233" | **Wrong.** `116-S3SERWD.bin` is watchdog / error-message **data**, not code. Octal 071233 there is the ASCII string `"...ctobus Message Device input queue"`, not a jump table. The real MON dispatch is in `resident/SINTRAN-DATA_commoncode.bin` (load 0); `GOTAB` is in an RPIT **overlay** that is zeroed/absent in every carve. | 63% of `116-S3SERWD` words are printable ASCII; control-flow density <2% per 1KW (real code = 15-30%). |
| A2 | §D: "these handlers do not sit cleanly in any carved `.bin` -> use **live DAP** (boot SINTRAN, breakpoint, dump)" | **Mostly avoidable.** 25 of 31 were recovered **from disk** via the PIT-overlay model (find which carved segment overlays the address), no running system needed. Live DAP is only a fallback now. | ND-500 handlers resolved to `S3MPIT` (`026-S3IMPIT.bin`); ND-100 45/304/313 to `S3RPIT` (`025-S3IRPIT.bin`); documented in `EXTRACTING-RESIDENT-CODE.md` 7.6/7.7. |
| A3 | Implicit: the NPL source in the repo matches this L image | **Wrong revision.** The NPL source is a *different* SINTRAN revision; symbol addresses differ from the NPL address column by a uniform offset (ND-500 block +0o200, DEBUGGER/BRPNT block +0o101, S3SM5 410B +5). Behaviour matches, exact bytes do not. | e.g. `DVIO`=0o141027 (L07) vs 0o140627 (NPL); `GERRC`=0o141633 vs 0o141433. |
| A4 | §D handler addresses `OSIZE=111254`, `MAGTP=114707`, `GTYPR=107550`, `REABT=112250` (SYMBOL-1-LIST) | **Two-context split.** Several symbols exist twice (a resident copy and an overlay copy). The real bodies used: `OSIZE=044231` (resident common), `MAGTP=026354` and `GTYPR=113312` (in `006-S3FS.bin`). The 45/304/313 addresses are in **SYMBOL-2-LIST**, not SYMBOL-1-LIST. | Wave 3 + the S3RPIT resolution. |

## B. Wrong call identities / names

| # | Friend's label | Correct | Evidence |
|---|----------------|---------|----------|
| B1 | 304 = **Performance** (from the manual's alphabetical table) | 304B = **SendSIBASMessage** (`MAPS1B`/`MAPSIB`). Performance is **344B** (`PERF0`/`MOPERFORMANCE`). The manual's "Performance 304B" row is an OCR/table error. | numeric internal-use table + handler `MAPSIB` @`RP-P2-MONCALLS.NPL:1745`. |
| B2 | 505 = **GetTrapReason** | Source name is **GERRC = "get error code"** (used after a programmed trap). The behaviour matches (it returns the trap error code), so the manual name is not wrong in effect - just not the internal name. | `MP-P2-N500.NPL:1933` header "GET ERROR CODE". |
| B3 | 510 = **"?"** (unknown) / earlier guess "switch context" | 510B = **CallSwapper** (`SWMC` = "monitor call to the swapper"), NOT switch-context. | header `MP-P2-N500.NPL:2042`; body `CALL 5ACTSWAPPER`. |
| B4 | 512 = "XMSG for ND-500", 513 = "used by convert-domain-a03" (two different things) | 512B/513B are **A5XMSG/B5XMSG - ONE shared handler body**, no branch on the MON number. A-vs-B is a caller-side buffer-passing convention (B carries a data buffer). | single `SUBR A5XMSG,B5XMSG`; both labels on one instruction; byte-verified in `S3MPIT` (shared entry 0o142253). |
| B5 | 511 = "DVIO, combines DVINST 503 / DVOUTS 504" | **Confirmed** (this one was right): the L handler shares its output body with NOUTSTR(504) and its input body with NINSTR(503). | `S3MPIT` disasm of DVIO @0o141027. |

## C. Wrong behavioural assumptions

| # | Friend's assumption (§A) | Finding | Evidence |
|---|--------------------------|---------|----------|
| C1 | MON 327 (FSMTY) **function 2 = number of bytes in the command buffer** | **Contradicted.** Func 2 returns the **block size of an open file, in WORDS**. | manual FSMTY table + decoded `006-S3FS.bin` handler. |
| C2 | MON 75 (REABT) might work for **file 0** (command buffer) | **Uncertain / likely no.** No static file-0 special-case found; manual restricts to sequential mass-storage files. | Wave 3 (needs a live check to be definitive). |

## D. Friend's hypotheses that were CONFIRMED

| # | Friend's hypothesis | Result |
|---|---------------------|--------|
| D1 | MON 45 (ND-500 GTYPR) works like **MON 327 function 4** | **Confirmed** - FSMTY func 4 is serviced by `GTYPR`. |
| D2 | MON 120 (WFILE) block=0/bytes=0 is a **seek** | **Confirmed** - seeks, but to **block boundaries only**, and to **any** block (not just 0). Arbitrary byte offsets need MON 74. |
| D3 | 511/512/513 are the "showstoppers" and are "500-calls, probably not directly coded in the ND-100 listing" | **Confirmed** - they are ND-500 level-12 handlers; recovered as real SINTRAN L code from the `S3MPIT` overlay (not the flat ND-100 image). |

## E. Could not be resolved (honest negatives)

| # | Call | Friend's expectation | Finding |
|---|------|----------------------|---------|
| E1 | 15B | (unclear, no YAML) | **Not resolvable.** Undocumented in the manual; no handler symbol; routes `GOTAB->MFELL->CALLPROC`; the `GOTAB` slot is in an uncarved RPIT overlay. |
| E2 | 42B | guessed "TSS carry-over" | **Unprovable.** Same as 15B - undocumented, no symbol, `GOTAB` unreadable. TSS provenance is not encoded anywhere in source. |
| E3 | 51B | "SINTRAN version of DBRK" | **Body not located.** No distinct handler symbol (`VDMAC` is a false match - a HW-interrupt routine). |
| E4 | 425/426/427B | sprname/gprnum/gprname, expected in the ND-500 monitor | **Empty S3SM5 vector slots (`0x0000`)** - not serviced by S3SM5. The ND-100-side back-end was not located. (Documented YAMLs exist, from the manual names.) |
| E5 | 410B | routine-map offset 0xBAE1 | **Defective slot** - the raw vector value 0xBAE1 lands 4 bytes inside an ASCII error string; true code entry is +5 at 0xBAE6 (recovered, confidence MEDIUM-HIGH). |

## F. Missing artifacts the friend flagged, now created

- 510/511/512/513 had **no YAML** (§C table) - created (`510B_CallSwapper.yaml`,
  `511B_DVIO.yaml`, `512B_XMSGCallA.yaml`, `513B_XMSGCallB.yaml`), plus 20 other
  missing YAMLs found in a full audit (235 -> 261).

---

## G. GOTAB is fully in `commoncode.bin`, and 15/45/51 dispatch DIRECT (not MFELL) - CORRECTION (2026-07-10)

Two NPL-based claims used across earlier notes are now **falsified by the
byte-verified GOTAB**:

1. **"GOTAB overlay is not carved / GOTAB is unreadable."** FALSE. The real `GOTAB`
   is byte-present in `resident/SINTRAN-DATA_commoncode.bin` at virtual `071233B`,
   indexed **directly** as `071233B + MON#` (one word per call). All six known values
   match a live-DAP read of a booted L system 6/6 (`GOTAB[1]=120303, [13]=120454,
   [14]=0, [15]=120501, [42]=0, [51]=121147`). (The `116-S3SERWD` ASCII-text finding
   (A1) was a *different file*; it does not contradict this.)
2. **"GOTAB[15/45/51] = MFELL -> CALLPROC (fall-through)."** FALSE for the odd-numbered
   calls. `GOTAB[15B]=120501B`, `GOTAB[45B]=121075B`, `GOTAB[51B]=121147B` are **direct
   entry stubs** in a uniform dispatch block `120303B..122506B` (odd MON 1B..161B, each
   slot `025B` words) backed at runtime by the **S3RPIT** overlay (`025-S3IRPIT.bin`,
   load `32000B`), resolved by density (S3IRPIT 350 ctrl-xfer / 356 in-block targets /
   96 `JPL I` vs S3IMPIT 221/216/25). A `GOTAB[n]=000000` slot (the **even** low MONs,
   e.g. 14B OUTBT, 42B) is the fall-through path - **not "illegal MON."** So "unassigned
   -> MFELL = illegal" is wrong; zero = fall-through, and OUTBT proves it.

## Net

Of the friend's 31 calls: **27 recovered as real SINTRAN-L machine code** (from disk,
no emulator) - now including **15B and 51B**, carved from the S3RPIT overlay
(`025-S3IRPIT.bin`) once the byte-verified `GOTAB` gave their true entry addresses.
The remaining **4 are honest documented negatives**: **42B** (`GOTAB[42B]=000000`
fall-through; body in the uncarved `CALLPROC` path) and **425/426/427** (byte-verified
`0x0000` slots in the S3SM5 numeric dispatch - genuinely absent in L). Per the
deliverable rule, a byte-proven documented negative is a completed call. The single
biggest earlier correction remains **A1** (`GOTAB` is NOT in `116-S3SERWD`; that
segment is data) - now superseded by **G** (the real `GOTAB` is in `commoncode.bin`).
