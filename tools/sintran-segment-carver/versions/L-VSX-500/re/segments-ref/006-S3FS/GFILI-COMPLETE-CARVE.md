# GFILI @057173B - complete instruction-level carve (L07, segment 006-S3FS)

**What GFILI is:** the SINTRAN III file-system routine that resolves a file specification (already
tokenised) to an on-disk **object entry** - directory index + object descriptor + version. It is the
lookup core behind `MON 50` OPEN (via `OPFIL -> FCON -> FFILE -> GCFIL -> GFILI`) and the whole FILSYS
name family. It does NOT create files and does NOT check access rights - those are separate routines
(`CROBJ`, `GFIAC`) in the callers.

**Source of truth:** `006-S3FS.asm` (this folder), whole-segment `nd100-dis`. Addresses octal; every
citation is `ADDR (line N)`. Version L-VSX-500 (L07). Symbols: `006-S3FS.symbols.txt`.
Binary: `../../../segments/006-S3FS.bin`, load base `26000B`; byte offset of `A` = `(A-26000B)*2`.
Body span: **`057173B` (entry) .. `057504B`** (next symbol `DIRUN@057505B`). The constant/pointer pool
sits mid-routine at `057353B-057373B` (normal NPL literal-pool placement).

Evidence tags: **[V]** = read from the bytes at the cited addr/line. **[I]** = inferred from
control-flow + symbol names, not fully value-decoded. **[OPEN]** = not determined.

Runtime convention: reentrant stack. `JPL I <p>` calls via a pointer cell = `SPUSH@003752`; result
returned by callee doing `MIN ,B4` k times, so `SPOP@003776` returns to **caller+k** (skip-count
multi-return). `,Bn` = frame-relative local. `SAT c` loads an ASCII code (octal): `50`=`(`, `51`=`)`,
`47`=`'`, `56`=`.`, `72`=`:`.

---

## 1. Routines GFILI calls (all resolved from the pointer pool)  [V]

Constant/pointer pool `057353B-057373B` [V, lines 13345-13361]:

| Cell | Value | Symbol | Role |
|------|-------|--------|------|
| 057353 | 003752 | **SPUSH** | push reentrant frame |
| 057354 | 000006 | (const 6) | frame size arg |
| 057355 | 056645 | **SEPOB** | split spec into subparts; fill empty dir from `GDEFD` |
| 057356 | 057374 | (landing) | SEPOB-fail landing |
| 057357 | 030062 | **GETCH** | fetch one spec char |
| 057360-062 | 023/035/033 | (consts) | entry field offsets |
| 057363 | 042622 | **SEPFS** | split file/type/version fields |
| 057364 | 057376 | (landing) | SEPFS-fail landing / re-scan loop entry |
| 057365 | 056326 | **GOBJI** | object/file-name scan in one directory |
| 057366 | 057405 | (landing) | GOBJI-fail landing = fallback engine |
| 057367 | 000044 | (const) | |
| 057370 | 057627 | **GVERS** | parse requested version field -> number |
| 057371 | 057567 | **GNEXV** | walk the version chain (get next version entry) |
| 057372 | 000046 | (const) | |
| 057373 | 003776 | **SPOP** | pop frame / return |

Fallback-block pool `057457B-057467B` [V, lines 13413-13421]:

| Cell | Value | Symbol | Role |
|------|-------|--------|------|
| 057460 | 050124 | **GDIRT** | directory-table accessor (advance to next dir) |
| 057461 | 010506 | (commoncode) | list-end / exit-dispatch helper (below load base) |
| 057462 | 055540 | **GSYSI** | get user (SYSTEM) index (by name via `GMUSI`) |
| 057463 | 010500 | (commoncode) | exit-dispatch helper |
| 057466 | 056326 | **GOBJI** | SECOND object scan (under SYSTEM) |
| 057467 | 057267 | (landing) | SYSTEM-hit -> re-enter version chain |

**Not in either pool: `GFIAC@057771` is never referenced anywhere in `057173B-057504B`.** [V - I read
every pointer cell]. GFILI does pure name->entry resolution; the friend/public/ring access check is
run by the CALLER after GFILI returns. (This confirms the earlier [I] as [V].)

---

## 2. Stage-by-stage walk  [V bytes; per-line]

### A. Entry + frame  (057173-057200)
```
057173 STD I 157                 ; save link (SPUSH linkage)                     (13233)
057176 SAB 51                    ; B := 51  (frame base)                         (13236)
057177 JPL I 154 -> SPUSH        ; push reentrant frame                          (13237)
057200 STT ,B40                  ; ,B40 := caller's T  (fallback-allow token)    (13238)
```
`,B40` is seeded from the caller's `T`. Callers that permit the SYSTEM fallback pass `T = -1`
(see the gate in stage F). [V store; [I] that the caller's convention value is -1, but the gate at
057415 compares against -1.]

### B. Split spec + detect explicit (USER)  (057201-057217)
```
057203 JPL I 152 -> SEPOB        ; split spec; SEPOB fills empty dir via GDEFD   (13241)
057204 JMP I 152 -> 057374       ; SEPOB fail -> store status, return            (13242)
057205 STT ,B43 ; 057206 STA ,B41; ,B43 = directory index, ,B41 = name descriptor(13243-44)
057210 STA ,B42                  ; ,B42 = object descriptor copy                 (13246)
057211 LDX ,B0 ; 057212 (T:=0)
057213 JPL I 144 -> GETCH        ; read spec char[0]                             (13249)
057214 SAT 50 ('(')              ; is first char '(' = a (USER) prefix ?         (13250)
057215 SKP IF DA EQL ST
057216 JMP 2 -> 057220           ;  char != '(' : leave ,B40 (= -1, fallback ON) (13252)
057217 STZ ,B40                  ;  char == '(' : ,B40 := 0  (fallback OFF)       (13253)
```
**Decisive gate mechanism [V]:** if the spec begins with `'('` (an explicit `(USER)` prefix), `,B40`
is zeroed; otherwise it stays at the caller's `-1`. Stage F only runs the SYSTEM fallback when
`,B40 == -1`. This is the byte-level implementation of the Users Guide rule "if a user name is
specified, only that user's directory is searched."

### C. Field split + FIRST object scan  (057220-057265)
```
057220-057230 init entry fields (const 20 -> +23, 4 -> +33, 5 -> +35)            (13254-62)
057242 JPL I 121 -> SEPFS        ; split file:type;version                       (13272)
057243 JMP I 121 -> 057376       ; SEPFS fail -> tail                            (13273)
057257-057262 load ,B43(dir) + ,B41(name) as scan args
057263 JPL I 102 -> GOBJI        ; FIRST file-name scan in directory ,B43        (13289)
057264 JMP I 102 -> 057405       ; GOBJI +0 (fail) -> FALLBACK ENGINE (stage F)  (13290)
057265 STD ,B44 ; 057266 STD ,B46; GOBJI success: found entry -> version best    (13291-92)
```
GOBJI returns via skip-count; the `+0` (no unique file) return lands on `057264`, which jumps to the
fallback engine at `057405`. A unique hit falls through to the version chain.

### D. Version chain  (057266-057337)
```
057274 JPL I 74 -> GVERS         ; parse requested version field to a number     (13298)
057276 JAF -> 057314             ; no version requested -> skip explicit-match    (13300)
057277-057313 loop: GNEXV (057301) walk chain, keep entry whose version == req   (13303)
057314-057336 loop: GNEXV (057326), MIN ,B50 counts, pick highest/!default       (13326)
057337 STD ,B44 ; STD ,B46       ; final selected version entry                  (13332-33)
```
GNEXV (`057567`) reads entry version keys from cells `+41B`/`+44B` and shifts them
(`057600 LDA ,X 41 ; 057602 LDA ,X 44 ; SAD/SHA ZIN SHR` 13497-13503) [V]. GVERS (`057627`) parses
the version subpart chars via GETCH, terminator `'`(47) (13528) [V]. Full numeric semantics = [I].

### E. Success return  (057337-057351)
```
057343 LDA ,B42 -> ,B0           ; return object descriptor                      (13337-38)
057345 LDA ,B43 -> ,B1           ; return directory index                        (13339-40)
057347 MIN ,B4                   ; skip-count = 1  (success)                      (13341)
057351 JMP I 22 -> SPOP          ; pop frame, return to caller+1                  (13343)
```

### F. Fallback engine  (057376-057504)  [V - the current-user -> SYSTEM path]
Entered two ways: from GOBJI-fail (`057405`), or looped back from a re-scan (`057376`).
```
057376 STA ,B2                   ; (loop-back) save status                        (13364)
057401 JPL I 57 -> GDIRT         ; (loop-back) advance directory                  (13367)
    -- direct GOBJI-fail entry: --
057405 STA ,B2                   ; save GOBJI status                              (13371)
057406 SAT 56 ; 057407 SKP IF DA UEQ ST   ; status == 56 "No such file name"?     (13372-73)
057410 JMP 4 -> 057414           ;  yes -> try fallback                           (13374)
057411 SAT 57 ; 057412 SKP IF DA EQL ST   ; status == 57 "Ambiguous"?             (13375-76)
057413 JMP 43 -> 057456          ;  neither 56 nor 57 -> return the error         (13377)
057414 LDA ,B40 ; 057415 SAT -1 ; 057416 SKP IF DA EQL ST   ; ,B40 == -1 ?        (13378-80)
057417 JMP 37 -> 057456          ;  ,B40 != -1 (user named) -> NO fallback         (13381)
    -- ,B40 == -1 (unqualified): walk dirs then SYSTEM --
057422 JPL I 36 -> GDIRT         ; advance to next directory                       (13384)
057423 JMP -25 -> 057376         ;  GDIRT+0: more dirs -> loop back, re-scan        (13385)
057424 JPL I 35 -> 010506        ;  GDIRT+1: list-end helper                        (13386)
057425 JPL I 35 -> GSYSI         ;  GDIRT+2: get (SYSTEM) user index                (13387)
057433 JPL I 25 -> GDIRT         ; get SYSTEM's directory                           (13393)
057443 JPL I 23 -> GOBJI         ; SECOND scan, under SYSTEM                         (13401)
057444 JMP 34 -> 057500          ;  SYSTEM GOBJI fail -> final 56 -> return          (13402)
057445 STD ,B44 ; STD ,B46       ; SYSTEM hit: save entry                            (13403-04)
057455 JMP I 12 -> 057267        ; -> re-enter version chain (stage D) with SYSTEM hit(13411)
    -- error exits --
057456 JMP -57 -> 057377         ; (error path)                                      (13412)
057470-057504 map GSYSI/GMUSI user errors: 46 no-such-user, 45 ambiguous-user       (13422-33)
057500 SAT 56 ... -> 057504      ; SYSTEM also missing -> final "No such file" (56)   (13430)
```

**So the observable resolution order for an unqualified spec [V]:**
1. caller's default directory (`GDEFD` inside SEPOB) -> `GOBJI` (057263).
2. on `56`/`57`, walk further directories via `GDIRT`, re-scanning `GOBJI` (057422 -> loop 057376).
3. when the walk is exhausted, `GSYSI` -> SYSTEM directory -> `GOBJI` again (057443).
4. SYSTEM miss -> final `56` "No such file name" (057500-057504).
An explicit `(USER)` prefix zeroes `,B40` (stage B) and short-circuits all of 2-4.

---

## 3. Frame local map (as used in the body)  [V stores unless marked]

| Local | Meaning | Evidence |
|-------|---------|----------|
| `,B0` / `,B1` | in: spec ptr; out: object descriptor / dir index | 057211, 057343-057346 |
| `,B2` | working status word (from SEPOB/GOBJI) | 057405 `STA ,B2` |
| `,B3` | out: version/result word | 057342 `STA ,B3` |
| `,B40` | **fallback-allow flag**: `-1` = allow SYSTEM fallback, `0` = user named -> no fallback | 057200 seed, 057217 zero, 057414-057417 gate |
| `,B41` | object-name descriptor (GOBJI arg) | 057206, 057262 |
| `,B42` | object descriptor copy (returned in ,B0) | 057210, 057343 |
| `,B43` | current directory index (GOBJI/GDIRT arg) | 057205, 057261, 057420 |
| `,B44` / `,B46` | version-chain "best entry so far" (double word) | 057265, 057337, 057445 |
| `,B47` | version key under compare | 057303-057307 |
| `,B50` | version-loop counter | 057316, 057330-057334 |

---

## 4. Answers this carve nails down

- **GFILI is lookup-only.** No create path in its pool (create is `CROBJ@063726B`, reached from
  `GCFIL@064670B` on a quoted name). [V - full pool listed in section 1.]
- **GFIAC (access check) is NOT part of GFILI.** Name resolution and permission are separate stages;
  a file is found by name first, access-checked by the caller second. [V - GFIAC absent from body.]
- **The current-user -> (SYSTEM) fallback lives here, inside GFILI, and is gated on the spec having
  no `(USER)` prefix** (`,B40`). It is SINTRAN-internal, not the linker's. [V] - see companion
  `CARVE-ANSWER-UNQUALIFIED-OPEN-USER-SYSTEM-FALLBACK.md`.

---

## 5. Still OPEN (honest)

- Exact numeric semantics of the version chain (GVERS parse + GNEXV compare) - structure verified,
  the "which version wins" arithmetic (default vs highest) not fully decoded.
- The `010506`/`010500` commoncode exit-dispatch helpers (below this segment's load base).
- The literal string `GSYSI` feeds `GMUSI` to name user SYSTEM (word `055560=010017`, not decoded) -
  so "SYSTEM" rests on the symbol name + the Users Guide, not a decoded ASCII constant.
- Which `T` value each GFILI caller passes at entry (seeding `,B40`); the gate compares `-1`, and the
  `(USER)`-prefix zeroing is byte-proven, but the caller-side seed is not traced here.

*Companions in this folder: `CARVE-ANSWER-UNQUALIFIED-OPEN-USER-SYSTEM-FALLBACK.md` (the fallback
question), `FLPAR-MDEAB-FILENAME-RESOLVER-CARVE.md` (name MATCHING inside a directory).
Status-of-record: [`SINTRAN/CARVING-HANDOFF.md`](../../../../../../../SINTRAN/CARVING-HANDOFF.md) section 1.8.*
