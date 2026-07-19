# SINTRAN III file-name abbreviation matcher - COMPLETE carve (L07, segment 006-S3FS)

**Answer for the microcode/other LLM.** How SINTRAN III parses a file spec into subparts and
matches each (possibly abbreviated) subpart against stored names, including the exact prefix rule,
the empty-subpart rule, and the unique / no-such / ambiguous decision.

**Source of truth:** `006-S3FS.asm` (this folder). Addresses octal; every citation is `ADDR (line N)`
into that file. Version L-VSX-500 (L07). Symbols: `006-S3FS.symbols.txt` (FILSYS-SYMBOLS).
Binary: `../../../segments/006-S3FS.bin`, load base `26000B`, big-endian;
byte offset of address `A` = `(A - 26000B) * 2`.

Every line tagged **[V]** verified from bytes (addr/line) or **[OPEN]**. Error-code meanings are
cross-referenced to `Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md` (error table p.284-285).

> **Correction to the original request's premises (both were wrong):**
> 1. The ambiguous-file code is **057** ("Ambiguous file name"), NOT `0111`. (Dir=027, user=045,
>    file=057, per the manual and the bytes.) The `0111/0113` codes DO exist but come from a
>    *separate* access/type classifier in MDEAB (section 6), not the ambiguity decision.
> 2. Matching **is** character-prefix (via `COMPS`, section 3). An interim note in an earlier draft
>    of this file called it "numeric, not prefix" - that was a misread of GOBJI's hash pre-filter;
>    the definitive compare is `COMPS`, and it is a true prefix match. This section supersedes it.

---

## 1. The algorithm in one paragraph  [V]

The raw file spec is tokenised into subpart character strings (`FLPAR`), delimited by `-` (`55`),
with `:`(`72`)/`.`(`56`)/`(`(`50`)/`)`(`51`) framing directory/user/type and `'`(`47`) as the string
terminator. For each name class (directory, user, object/file, version), a **scanner** walks the
corresponding on-disk table and calls one shared comparator **`COMPS @041552`** on every entry.
`COMPS` compares the supplied string A against the stored string B **as a prefix**: A matches B if
every character of A equals B up to A's terminator (`'`) or A's given length; an **exact** match
(A and B end together) is distinguished from a **prefix** match (A ends first). `*`(`52`) is a
wildcard char and `-`(`55`) in A is a positional/empty subpart that skips B forward to its next `-`
(matching any value in that slot). Each scanner **counts** the matches: **exact match wins outright
(unique)**; otherwise **0 prefix matches -> "No such X"**, **1 -> unique (return its index)**,
**>1 -> "Ambiguous X"**. For files the counter is GOBJI `,B41`, producing `056`/unique/`057`.

---

## 2. FLPAR - tokenizer  [V mechanism]

- **[V]** `FLPAR @046231 (line 8585)` / `DLPAR @046233` share one body, differing only in the `SSK`
  flag (`174220 BSET ONE SSK` vs `174020 BSET ZRO SSK`, lines 8585/8588).
- **[V]** Byte-level tokenizer driving `GETCH @030062` / `GTTCH @030070` (`GTTCH` reads the alternate
  page-table space, `SSPTM`), classifying each char with `SAT <c> ; SKP IF DA {EQL|UEQ} ST`.
- **[V] Delimiters:** `55`=`-` (subpart separator), `56`=`.`, `72`=`:`, `50`=`(`, `51`=`)`,
  `47`=`'` (string terminator), `52`=`*` (wildcard).
- **[OPEN]** Exact subpart-descriptor array layout FLPAR emits (pointer block 046341-046416).

---

## 3. COMPS @041552 - the prefix/abbreviation comparator  [V, decisive]

`COMPS (line 6155)` compares supplied string **A** (`,B0`) vs stored string **B** (`,B2`), with
`,B1` = supplied length. Working vars: `,B6`=index into A, `,B7`=index into B, `,B10`/`,B11`=cur
char A/B, `,B4`=**return-skip count** (the result, see 3.1). Chars fetched via `[041710]=GETCH`.

**[V] Main loop (041564-041611):**
```
041564  LDX ,B0 ; LDT ,B6 ; JPL GETCH -> charA (,B10)      (line 6165)
041570  LDX ,B2 ; LDT ,B7 ; JPL GETCH -> charB (,B11)
041574  LDA ,B10 ; LDT ,B11 ; SKP IF DA UEQ ST             ; charA == charB ?
041577  JMP -> 041603                                       ; equal -> 041603
        (fall to 041600 when NOT equal)
041600  SAT 52 ; SKP IF DA EQL ST ; JMP -> 041616           ; charA=='*' ? no-> 041616 (mismatch)
041603  SAT 47 ; SKP IF DA UEQ ST ; JMP -> 041612           ; charA=='\'' (terminator)? yes-> EXACT
041606  MIN ,B6 ; MIN ,B7 ; MIN ,B12 ; JMP -> 041564        ; advance both, loop
```

**[V] Match outcomes:**
- **Exact match (041612-041613, MIN ,B4 x2):** reached from 041603 when `charA==charB=='\''` - both
  strings ended together, fully equal.
- **Prefix match (041613 / one MIN ,B4):** reached from 041616-041620 -
  ```
  041616  SAT 47 ; SKP IF DA UEQ ST ; 041620 JMP -> 041613  ; MISMATCH but charA=='\'' -> A ended = PREFIX match
  ```
  Also from 041671-041675: `LDA ,B6 ; LDT ,B1 ; SKP IF DA UEQ ST ; JMP -> 041613` - A reached its
  **length `,B1`** -> prefix match. So **supplied A matching as a prefix of stored B counts as a
  match**; it is the abbreviation rule ("FI" matches "FILE-1").
- **No match (041614, ,B4 unchanged):** a real character mismatch before A ends (charA is neither
  `'`, `-`, nor `*`) falls through 041621->041623 `JMP -> 041614`.

**[V] Empty / positional subpart `-` (041621-041706):**
```
041621  SAT 55 ; SKP IF DA EQL ST ; JMP -> 041614           ; charA=='-' ?  (else true mismatch)
        (charA=='-':) skip B forward to its next '-' :
041624  LDA ,B11 ; SAT 47 ; ... ; 041630 MIN ,B7 ; re-GETCH charB ;
041635  SAT 55 ; SKP IF DA UEQ ST ; JMP -> 041643            ; found '-' in B -> resync both, continue
041640  MIN ,B12 ; JMP -> 041624                             ; keep skipping B's subpart
```
So a `-` in the supplied name (including the empty subpart of `--`, i.e. two consecutive `-`)
**matches any value in that subpart slot** by advancing B to its next subpart boundary. This is the
`-N` / `--` positional-abbreviation behaviour.

**[V] Edge cases of the `-` machinery (byte-decoded 2026-07-17, closing the earlier OPEN):**
- **B ends before its next `-`:** the terminator test runs EVERY skip iteration
  (`041624 LDA ,B11 ; SAT 47 ; SKP IF DA UEQ ST ; 041627 JMP -> 041614`) - supplied having
  MORE subparts than stored = **NO MATCH**.
- **After a `-` resync (041643), EXACT is impossible:** the secondary compare loop's
  both-ended-together exit is `041664 SAT 47 ; SKP IF DA UEQ ST ; 041666 JMP -> 041613` =
  ONE `MIN ,B4` = **PREFIX**. Only the main loop (041605 -> 041612, or `,B12` exhaustion at
  041610 skipping into 041612) can produce EXACT. A positional match therefore always counts
  as a prefix match in the scanners' ambiguity arithmetic.
- **`,B12` is a compare budget = -(length ,B1)** (negated at 041560: `RADD AD1 CM1 CLD SA DA`),
  advanced by `MIN` (skip-on-zero): exhaustion in the MAIN loop falls through 041611 into
  041612 = EXACT (all `length` chars equal = full-field match); exhaustion inside the B-skip
  loop falls through 041641 into `041642 JMP -> 041614` = NO MATCH.

**[V] Wildcard `*` (`52`):** handled at 041600-041602 and 041661-041663 - a `*` char is treated as
matching, the compare continues.

### 3.1 The multi-return (skip-count) convention  [V]
Routines here use the SINTRAN reentrant-stack runtime **`SPUSH @003752` / `SPOP @003776`**
(commoncode; `EMCOM`/`SPUSH`/`SPOP` symbols). A callee signals a result by doing `MIN ,B4` N times;
`SPOP` returns to **caller+N**, i.e. the caller places N+1 landing instructions after the `JPL`.
`COMPS` therefore returns **3-way**: `+0` no-match, `+1` prefix-match, `+2` exact-match.
This is also why the request's guessed "`A=0` unique / `A=-2`" contract is not literal - results
travel via the skip-count and status words (`,B1`/`,B2`), not a single register.

---

## 4. Per-class scanners: count -> unique / no-such / ambiguous  [V]

Each scanner walks a table, calls `COMPS`, and dispatches on its 3-way return. All use the same
shape; two are byte-verified end to end:

### GDIRI @047402 (directory)  [V]
`COMPS` at `[047533]=041552`. 3-way landing after the call (047446-047451):
`+0 -> 047452` (next entry), `+1 -> 047511 MIN ,B7` (count a prefix match, remember index),
`+2 -> 047515` (exact -> definitive). Terminal decision (047462-047472):
```
047462 LDA ,B7 ; JAF -> 047466        ; ,B7 = #prefix matches
047464 SAA 26                          ; count==0 -> 026 "Directory not entered"
047466 SAT 1 ; SKP IF DT LST SA        ; 1 < count ?
047471 SAA 27                          ; count>1  -> 027 "Ambiguous directory name"
                                       ; count==1 -> unique (047473+)
```

### GOBJI @056326 (object / file) - the FILE case the request asked about  [V]
Per entry it first applies a numeric **hash pre-filter** (`056456 (,B2<<8)|off == entry[42]`,
`056464 entry[44]>>12 == ,B35`) to skip non-candidates cheaply, then calls the definitive
**`COMPS`** at `[056570]=041552` (invoked 056475), incrementing the match counter `,B41`
(`056535 MIN ,B41`). Terminal decision (056576-056607):
```
056576 LDA ,B41 ; JAF -> 056602        ; ,B41 = #file-name matches
056600 SAA 56                          ; count==0 -> 056 "No such file name"
056602 SAT 1 ; SKP IF DT LST SA        ; 1 < count ?
056605 SAA 57                          ; count>1  -> 057 "Ambiguous file name"
                                       ; count==1 -> unique (056607+)
```

### GNAMI @047536 (name/device table)  [V, corrected 2026-07-17]
`COMPS` at `[047650]=041552`; `[047646]=GNAMT 050223` reads table entries. Per entry it
**unpacks the STORED name** (entry cells `70B+0..6`, 7 words, via GNAMT) into a local buffer
(047550-047567), then calls `COMPS(X=supplied ,B0, A=buffer, T=20B)` - `SAT 20` at 047573 =
**16-char field width** feeding COMPS's `,B1`/`,B12` compare budget. `,B22 += 16B` at
047601-047603 is the **entry stride of a LINEAR scan** (until `,B22 == [44]` table end), NOT a
hash bucket - an earlier draft of this paragraph said "packs 7 chars / hash bucket +16 per
probe"; both halves of that were misreadings. Emits `030` (count==0) / `031` (count>1) =
"No such device name" / "Ambiguous device name" (047612/047617).

**[V] Other COMPS callers (24 total):** GDIRI, GNAMI, GOBJI, NMPIT, NX5DP, LEAVE, TSTPR, FFMCH,
GNNAM, VERSN, ICHRE, DIRST, USEST, LIFRI, FILST, LEASP, LIALL, FILOG, APPES, SENDS, FFILI, MSPQE,
DSCON - i.e. the whole file-system name/version/list family shares this one comparator.

**[V] Error-code meanings** (ND-60.050.06 p.284-285): 026 Directory not entered · 027 Ambiguous
directory name · 030 No such device name · 031 Ambiguous device name · 044 Too long parameter ·
045 Ambiguous user name · 046 No such user name · 056 No such file name · 057 Ambiguous file name.

---

## 5. GFILI / SEPOB / SEPFS / GVERS - orchestration  [V structure]

- **[V] `GFILI @057173`** resolves a file spec to an object entry: `SEPOB @056645` (split the spec)
  -> `GOBJI @056326` (name scan, above) -> two **version-chain** loops (057300-312, 057323-335)
  calling **`GNEXV @057567`** and comparing numeric version keys; **`GVERS @057627`** parses the
  version field to a number.
- **[V] `SEPFS @042622`** splits a name array at `72`:/`42`"/`73`;/`47`' into up to 4 sub-fields via
  GETCH/PUTCH. **`SEPOB @056645`** is the top-level spec parser feeding the scanners.
- **[V] `GFIAC @057771`** = access/permission (ring) check on the resolved entry.
- **[V] Shared table accessors** `GDIRT 050124`/`GNAMT 050223` (2-word GET/PUT, SSM=default/user,
  SSK=get/put) and leaf address calculators `GDIRA 030225`/`GNAMA 030235` - see the companion
  `../../../re/kernel-carving/NAMEWALK-COMPLETE/` (the GDIRE exact-hash path).

---

## 6. MDEAB / DEAB family - what 0111/0113 really are  [V]

- **[V] Entries & modes:** `MDEAB @061044`(`,B7`=2), `IDEAB @061057`(0), `SDEAB @061071`(4),
  `DEABB @061101`(3). Shared body `061112`. Mode<2 pre-measures via `REMCH @061451`
  (subpart length to `'`/`.`).
- **[V] Body flow:** `GFILI` (resolve, 061125) -> `GFIAC` (access, 061141) -> if access result
  `>= ,B27` build the canonical name (formatter 061214-371: inserts `(`/`:`/`)`, copies via
  GETCH/PUTCH, `GFILN 060600`), else classify `,B27` via the bit-tree.
- **[V] The `,B27` bit-tree (061146-061202)** - BSKP bit# = printed field / 8 - maps access/type
  status bits to codes: `bit0&bit1&bit3`->0112, `bit0&bit1&!bit3`->**0113**, `bit0&!bit1&bit3`->0114,
  `bit0&!bit1&!bit3`->**0111**, `!bit0&bit1&bit2`->0110, `!bit0&bit1&!bit2`->0106,
  `!bit0&!bit1&bit4`->070. **These are an access/type classification fed by GFIAC - NOT the
  filename-ambiguity decision** (that is section 4, code 057). Do not conflate them.
- **[V] The 0106-0114 codes DECODED (2026-07-17, closing the OPEN):** they are the SINTRAN
  **ACCESS-DENIED error family** (ND-60.050.06 error table p.286): 070 "Not directory access",
  0106 "Not write access", 0110 "Not write and append access", 0111 "**Not read access**",
  0112 "Not read, write and common access", 0113 "Not read and write access", 0114 "Not read
  and common access". The bit correspondence is exact: **`,B27` = the REQUESTED-access mask
  with bit0=Read, bit1=Write, bit2=Append, bit3=Common, bit4=Directory** (SINTRAN's standard
  R/W/A/C/D access types), and the tree emits the error naming the denied combination.
  `GFIAC @057771` supplies the granted side: it loads the file entry's access word from
  `entry[43B]` (060007 `LDA ,X 43 -> ,B23`), selects the owner/friend/public field (calls into
  GUSEN/GUSEI/RUSER/RUSEB, pool 060141-060145; friend-relation loop 060065-060112 masks
  `,B23 >> 5`), and returns `(access >> 12B) & 17B` (060114-060117) via skip-return.
  So the request's original "0111 -> 0113 mapping" = "Not read" vs "Not read and write" -
  access denials, unrelated to name matching.
- **[OPEN]** The `050124`/`010506` exit-dispatch helpers (061345/061347) that set the final
  caller-visible status.

---

## 7. Answers to the five specific questions asked

| # | Question | Answer (byte-verified) |
|---|----------|------------------------|
| a | Each subpart matched as a **prefix** of the stored subpart? | **YES.** `COMPS` (sec 3): A matches if it equals B up to A's `'` terminator or its length `,B1`; exact (both end together) is distinguished from prefix (A ends first) via the `,B4` skip-count. |
| b | Empty subpart (`--`) - does zero length match any value in that slot? | **YES.** `-`(`55`) in A (incl. the empty subpart of `--`) makes COMPS skip B forward to its next `-` (041621-041706) = matches any value in that positional slot. |
| c | How is ambiguity decided vs unique vs not-found? | Each scanner **counts COMPS matches**; **exact match = immediate unique**; else **0 -> "No such"**, **1 -> unique**, **>1 -> "Ambiguous"**. For files: GOBJI `,B41` -> `056`/unique/`057` (056576-056607). |
| d | Exact return codes | Filename: **056** no-such, **057** ambiguous (VERIFIED, GOBJI). Directory: 026/027 (GDIRI). User: 046/045. The request's `0111->0113` is a *different* MDEAB access/type classifier (sec 6), and the literal "`A=0`/`A=-2`" is really the `SPUSH/SPOP` skip-count + status-word convention (sec 3.1). |

---

## 8. Still OPEN (honest)

- Exact bit-field layout of the packed name word / hash pre-filter (`GNAMI` pack loop; GOBJI
  `entry[42]`/`entry[44]` fields) - the *packing* is not needed to state the match rule (COMPS
  works on characters), but the hash pre-filter's field layout is not decoded.
- The 0106-0114 access/type code meanings and the `050124`/`010506` exit dispatch (sec 6).
- FLPAR's exact subpart-descriptor output layout (sec 2).

*Companion: `../../../re/kernel-carving/NAMEWALK-COMPLETE/` (GDIRE exact-hash path, GDIRT/GNAMT
accessors). Status-of-record: [`SINTRAN/CARVING-HANDOFF.md`](../../../../../../../SINTRAN/CARVING-HANDOFF.md).*
