# Unqualified MON 50 OPEN: does SINTRAN search current-user only, or fall back to (SYSTEM)? - CARVE (L07, segment 006-S3FS)

**Answer for the nd500x / mon_path.c author.** Question relayed:
> On an unqualified 50B OPEN, does the resolver (GFILI/GCFIL) search only the current user, or fall
> back current-user -> (SYSTEM), or use a search-list / default-dir-index / friend-gate - and is the
> fallback SINTRAN's own or the linker's (LOAD emitting an explicit (SYSTEM) prefix)?

**Source of truth:** `006-S3FS.asm` (this folder), whole-segment `nd100-dis`. Addresses octal; every
citation is `ADDR (line N)` into that file. Version L-VSX-500 (L07). Symbols:
`006-S3FS.symbols.txt` (FILSYS-SYMBOLS). Binary: `../../../segments/006-S3FS.bin`, load base `26000B`,
big-endian; byte offset of address `A` = `(A - 26000B) * 2`.

Evidence tags: **[V]** = I read the bytes at the cited addr/line and they say this. **[I]** = inferred
from control-flow shape + symbol names, NOT fully byte-decoded. **[OPEN]** = not determined.

---

## 0. VERDICT (one line)

**Current-user directory FIRST, then a SINTRAN-INTERNAL fallback to user (SYSTEM) - and the fallback
is SUPPRESSED when the spec names a user explicitly.** It is not "current user only"; it is not the
linker's job. The whole thing lives inside `GFILI` in segment `006-S3FS`. A caller does **not** have
to build a `"(SYSTEM)name"` string to get the SYSTEM directory searched - SINTRAN does it.

Mapping to the request's options: it is **(C)-then-(B)** - a default-directory + directory-walk that
**terminates in an automatic (SYSTEM) scan**, all internal to the file system. It is **NOT (A)**.

This also matches the SINTRAN III Users Guide verbatim (independent confirmation, see section 5).

---

## 1. What runs on an unqualified OPEN

`MON 50` OPEN worker `OPENF @123525` calls the resolver **once** (`FOPEN @067432 -> GFILI`) and
branches on the returned status; **OPENF itself does no (SYSTEM) prefixing and no retry loop**
- the fallback is deeper, in `GFILI`. [V] `OPENF` entry `123525 (32097)`; single `FOPEN` call
`123565 JPL I 46 -> ptr 123633=067432 (32129)`; no `(SYSTEM)`/retry in its body.

**`GFILI @057173` is the resolver** (spec -> object entry). Structure [V]:
- `057173 (13233)` entry (SPUSH frame).
- `057203 JPL I 152 -> ptr 057355=056645 = SEPOB (13241)` - split the spec into subparts.
- `057242 JPL I 121 -> ptr 057363=042622 = SEPFS (13272)` - field split.
- `057263 JPL I 102 -> ptr 057365=056326 = GOBJI (13289)` - **first** object/file-name scan.

---

## 2. The default directory for an EMPTY directory/user subpart  [V that GDEFD is called; I for gating]

When the spec has no directory prefix, `SEPOB` fills the directory from the caller's **default
directory** via **`GDEFD @055263`** ("get default directory"):
- **[V]** `SEPOB` calls GDEFD: `057146 JPL I 20 -> ptr 057166=055263 (13211 / ptr 13227)`.
- **[V]** `GDEFD @055263 (12255)` walks a per-user directory table: entry stride **`+030B`**
  (`055364 AAA 30 (12320)`), loop `055371 JMP -62 -> 055307 (12325)`, per-entry visibility-flag
  tests `055311 BSKP ONE 140 DA (12277)` / `055313 BSKP ZRO 130 DA (12279)`, and returns a single
  directory index in `,B1/,B2` (`055345-055351 (12305-12309)`).
- **[I]** GDEFD returns the user's ONE default directory; it does not itself auto-select SYSTEM.
  The exact "which flag = default" semantics are not decoded (**[OPEN]**).

So an unqualified name starts life pointed at the caller's own default directory - not a hardcoded
index, and not SYSTEM.

---

## 3. The fallback: directory walk then automatic (SYSTEM)  [V - decisive]

`GOBJI` returns via the SINTRAN skip-count convention (0-way = fail). Its **fail (+0) return** lands
on `057264`, which jumps into `GFILI`'s tail block at `057405`:
- **[V]** `057264 JMP I 102 -> ptr 057366 = 057405 (13290 / ptr 13356)`.

Tail block `057405-057504` - the fallback engine [V]:
```
057405  STA ,B2                         ; save GOBJI status                       (13371)
057406  SAT 56 ; 057407 SKP IF DA UEQ ST; status == 56 "No such file name"?       (13372-13373)
057410  JMP 4 -> 057414                 ;  yes -> try fallback                     (13374)
057411  SAT 57 ; 057412 SKP IF DA EQL ST; status == 57 "Ambiguous"?               (13375-13376)
057413  JMP 43 -> 057456                ;  neither 56 nor 57 -> return error       (13377)
057414  LDA ,B40 ; 057415 SAT -1 ; 057416 SKP IF DA EQL ST   ; ,B40 == -1 ?        (13378-13380)
057417  JMP 37 -> 057456                ;  ,B40 != -1 (user was named) -> NO fallback, return (13381)
        (,B40 == -1, i.e. UNQUALIFIED:)
057420  LDT ,B43 ; 057421 LDX 36
057422  JPL I 36 -> ptr 057460=050124 = GDIRT   ; advance to next directory        (13384)
057423  JMP -25 -> 057376               ;  more dirs -> loop back, re-scan GOBJI    (13385)
057424  JPL I 35 -> ptr 057461=010506   ;  GDIRT +1 return (list-end helper)        (13386)
057425  JPL I 35 -> ptr 057462=055540 = GSYSI   ; GDIRT +2: get SYSTEM user index   (13387)
        ... (057427-057442 set up SYSTEM dir) ...
057443  JPL I 23 -> ptr 057466=056326 = GOBJI   ; SECOND scan, under SYSTEM         (13401)
```

**Byte-verified facts:**
- **[V]** GFILI calls `GOBJI` **twice**: first `057263 (13289)`, again `057443 -> 057466=056326
  (13401 / ptr 13420)`.
- **[V]** The fallback is entered ONLY when the first scan returns `56`/`57` (`057406-057413`,
  13372-13377) AND `,B40 == -1` (`057414-057417`, 13378-13381). If a user was named, `,B40 != -1`
  and control returns the error at `057456` with no fallback.
- **[V]** Between the two scans it advances a directory table via **`GDIRT @050124`** (`057422`,
  `057433`) and, when that walk is exhausted, obtains the SYSTEM index via **`GSYSI @055540`**
  (`057425`, 13387).
- **[V]** Symbols confirmed present: `GDIRT=050124`, `GSYSI=055540`, `GMUSI=054527`, `GMAIN=047653`.

**Inferred (structure certain, exact semantics not fully decoded):**
- **[I]** The `GDIRT` loop walks the caller's directory search set; `GSYSI` is the terminal SYSTEM
  fallback. `GSYSI` = "Get SYStem Index"; it resolves SYSTEM **dynamically by name**
  (`GSYSI -> GMUSI @054527`, a name->index resolver), NOT from a hardcoded numeric user index.
- **[I]** `,B40 == -1` == "no user/directory was named" (fallback enabled). Set from the parse in
  `SEPOB`/GFILI entry (`057200 STT ,B40 (13238)`, `057217 STZ ,B40 (13253)`); the exact predicate
  that produces `-1` vs a real value is **[OPEN]**. The Users Guide (section 5) independently
  confirms the resulting behaviour.

---

## 4. The four sub-questions, answered

| # | Question | Answer |
|---|----------|--------|
| a | Current-user only, or fall back to (SYSTEM)? | **Falls back.** Own default directory first, then user SYSTEM. [V] two GOBJI scans + GSYSI in GFILI. |
| b | current-user -> (SYSTEM), or a search-list / default-dir-index / friend-gate? | **Both flavours: a default-directory-index (GDEFD) + a directory walk (GDIRT) that TERMINATES in a (SYSTEM) scan (GSYSI).** Not a bare two-shot. [V] structure. |
| c | Is the fallback SINTRAN's own, or the linker's (LOAD prepends "(SYSTEM)")? | **SINTRAN's own** - it lives in `GFILI @057173` inside `006-S3FS`. The linker does NOT need to prepend `(SYSTEM)`; OPEN does it. [V] GSYSI reached from GFILI's own tail block. |
| d | Does the friend/public access check steer name resolution? | **No.** `GFIAC @057771` (ring/friend/public access) is **not referenced anywhere inside GFILI's body** - it is a POST-resolution permission gate run by the OPEN worker AFTER the entry is found. It does not choose which user/dir is searched. [I] (subagent grep of ptr-value `057771`: hits only in the open/rename/set-access workers, none in `057173-057504`). |

---

## 5. Independent confirmation - SINTRAN III Users Guide

`Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md` (lines 1720-1724) documents exactly this,
for the same GFILI resolver used by `@`-command / program lookup:

> "The search in the file-directories will be performed as follows:
> - The file-directory, where the user giving the command is given space, will be searched first.
>   If not found, the user SYSTEM's file-directory is searched.
> - If a user name is specified in the file name, only that user's file-directory will be searched."

- Bullet 1 = the `GDIRT` walk + `GSYSI` fallback (section 3).
- Bullet 2 = the `,B40 != -1` suppression at `057417` (a named user disables the fallback).

The manual states the rule; the carve proves **where** it is (GFILI, `006-S3FS`) and that it is
internal to the file system, not the loader.

---

## 6. For mon_path.c (actionable)

1. On an **unqualified** name (no `(user)`, no directory prefix): search the caller's **own default
   directory first**, and on "no such file" **fall back to the (SYSTEM) directory** - implement this
   inside the OPEN resolver. Do **not** assume the caller/linker prepends `(SYSTEM)`; it does not.
2. **Suppress the SYSTEM fallback** when the spec names a user explicitly (`(USER)name`) - then only
   that user's directory is searched.
3. Trigger is the file-name-not-found code **`56` (octal, "No such file name")**; ambiguous is `57`.
   The fallback runs on `56`/`57`, not on other errors.
4. SYSTEM is resolved **by name** (GSYSI -> GMUSI), not a fixed user number - but for an emulator a
   fixed "(SYSTEM)" directory is a faithful enough model of the observable behaviour.

---

## 7. Still OPEN (honest)

- Exact predicate that sets `,B40` to `-1` vs a real value in `SEPOB` (the "was a user/dir named"
  test) - behaviour is confirmed by the manual, the byte-level setter is not fully decoded.
- Whether the `GDIRT` walk reaches **friend/public** directories in addition to own + SYSTEM (the
  `GDEFD` visibility-flag tests at `055311/055313` hint at per-entry visibility, not decoded).
- The literal string `GSYSI` feeds `GMUSI` (word `055560=010017`) - not decoded, so "SYSTEM" rests
  on the symbol name + the manual, not on a decoded ASCII constant.
- The `010506`/`010500` exit-dispatch helpers (`057461`/`057463`) - commoncode, below this segment.

*Companion (name-MATCHING inside a directory): `FLPAR-MDEAB-FILENAME-RESOLVER-CARVE.md` (this folder).
Status-of-record: [`SINTRAN/CARVING-HANDOFF.md`](../../../../../../../SINTRAN/CARVING-HANDOFF.md).*
