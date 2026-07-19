# CARVE ANSWER: SINTRAN file-name abbreviation matcher - implementation-ready spec

**To: the LLM implementing the resolver (so the linker can open files).**
**From: NDInsight carving session, 2026-07-17.**
**Status: carve COMPLETE and byte-verified. This is the answer to your FLPAR + MDEAB request.**

Full byte-cited carve (same folder): [`FLPAR-MDEAB-FILENAME-RESOLVER-CARVE.md`](FLPAR-MDEAB-FILENAME-RESOLVER-CARVE.md)
WSL path: `/mnt/e/Dev/Ronny/NDInsight/tools/sintran-segment-carver/versions/L-VSX-500/re/segments-ref/006-S3FS/FLPAR-MDEAB-FILENAME-RESOLVER-CARVE.md`
Disassembly source of truth: `006-S3FS.asm` (this folder), symbols `006-S3FS.symbols.txt`. All addresses octal, L-VSX-500 (L07).

---

## 1. TWO OF YOUR PREMISES WERE WRONG - fix them before implementing

1. **`0111` is NOT the ambiguous code.** The real ambiguous-file code is **057** ("Ambiguous file
   name", ND-60.050.06 error table). Byte proof: GOBJI terminal decision 056576-056607 emits
   `SAA 56` (count==0) / `SAA 57` (count>1). The `0111/0113` codes exist but belong to a SEPARATE
   access/type classifier in MDEAB's `,B27` BSKP bit-tree (061146-061202), fed by GFIAC - it fires
   AFTER a unique resolve, and is not part of name matching. Do not conflate them.

2. **There is no literal "A=0 unique / A=-2" register contract.** Results travel via the SINTRAN
   SPUSH/SPOP **multi-return skip-count** (`MIN ,B4` N times = return to caller+N) plus status
   words. COMPS returns 3-way: +0 no-match, +1 prefix-match, +2 exact-match.

Also note: MDEAB is NOT the per-subpart comparator you assumed. The comparator is **COMPS @041552**,
shared by 24 callers across the whole file-system name family. MDEAB/IDEAB/SDEAB/DEABB are top-level
"resolve + access-check + canonical-name formatting" wrappers around GFILI/GFIAC.

---

## 2. The verified algorithm (what your resolver must do)

### 2.1 Tokenization (FLPAR @046231)

Character constants (octal): `55`=`-` subpart separator, `47`=`'` string terminator,
`52`=`*` wildcard, `72`=`:`, `56`=`.`, `50`=`(`, `51`=`)`.
`(DIR:USER)NAME:TYPE;VERSION` framing is split off by SEPOB @056645 / SEPFS @042622 before
name matching; the name field then consists of `-`-separated subparts.

### 2.2 The comparator (COMPS @041552) - per-STRING, not per-subpart

COMPS compares the whole supplied name string A against a whole stored name string B in one pass;
the `-` logic below gives the per-subpart behaviour implicitly. Pseudo-C, byte-verified:

```c
// Returns: NO_MATCH (+0), PREFIX_MATCH (+1), EXACT_MATCH (+2)
// a = supplied (possibly abbreviated) name, alen = its length
// b = stored candidate name; both use '\'' (047) as terminator
int comps(const char *a, int alen, const char *b)
{
    int i = 0, j = 0;                      // ,B6 / ,B7
    for (;;) {
        char ca = a[i], cb = b[j];         // GETCH x2 (041564-041573)
        if (ca == cb) {                    // 041574
            if (ca == '\'')                // 041603: both ended together
                return EXACT_MATCH;        // 041612: MIN ,B4 x2
            i++; j++; continue;            // 041606: advance both
        }
        // chars differ (041600):
        if (ca == '*') { i++; j++; continue; }   // 041600/041661: wildcard char matches
        if (ca == '\'')                    // 041616-041620: A ended first
            return PREFIX_MATCH;           //   -> abbreviation hit ("FI" matches "FILE-1")
        if (ca == '-') {                   // 041621: positional / empty subpart
            // VERIFIED (041624-041641): skip B forward to ITS next '-'; the terminator
            // check runs EVERY iteration: if B ends ('\'' at 041624-041627) before a '-'
            // is found - i.e. supplied has MORE subparts than stored - return NO_MATCH.
            while (b[j] != '-') {
                if (b[j] == '\'') return NO_MATCH;   // 041627 JMP -> 041614
                j++;
            }
            i++; j++;                      // 041643: resync both past their '-'
            // NOTE (VERIFIED): after a '-' resync the compare continues in a secondary
            // loop (041643-041670) whose both-ended-together exit (041664->041666) lands
            // on 041613 = PREFIX - a match that used a positional '-' can NEVER be EXACT.
            continue;
        }
        return NO_MATCH;                   // 041621->041623->041614: true mismatch
        // NOTE: 041671-041675 also grants PREFIX_MATCH when i reaches alen (,B1) -
        // i.e. running out of supplied length counts the same as hitting the terminator.
    }
}
```

Load-bearing rules:
- **(a) Prefix:** supplied string matching as a prefix of the stored string IS a match. Exact
  match (both end together) is a distinct, stronger outcome.
- **(b) Empty subpart:** a `-` in the supplied name (including the empty middle of `--`) matches
  ANY value in that subpart slot by skipping the stored name to its next `-` boundary.
- `*` is a per-character wildcard.

### 2.3 The decision (per-class scanner, e.g. GOBJI @056326 for files)

Each scanner walks the object/name table, calls COMPS per entry, and:

```c
int matches = 0, found = -1;
for (each entry e) {
    // GOBJI only: cheap numeric hash PRE-FILTER first (056456-056470) - an optimization,
    // NOT the decision; a pre-filter miss just skips the entry. You may omit it.
    switch (comps(supplied, alen, e.name)) {
    case EXACT_MATCH:  return e;           // exact wins outright, scan stops (unique)
    case PREFIX_MATCH: matches++; found = e; break;   // 056535 MIN ,B41
    case NO_MATCH:     break;
    }
}
// Terminal decision (056576-056607):
if (matches == 0) return error(056);       // 056 "No such file name"
if (matches  > 1) return error(057);       // 057 "Ambiguous file name"
return found;                              // matches == 1 -> unique
```

Same shape for other classes, different codes: directory GDIRI -> 026 no-such / 027 ambiguous;
device/name GNAMI -> 030 / 031; user -> 046 / 045.

### 2.4 After the name resolves

GFILI @057173 then walks the version chain (GNEXV/GVERS, numeric version compare), and GFIAC
@057771 does the access check whose status feeds MDEAB's 0106-0114 classifier. Those are separate
stages - your resolver only needs 2.1-2.3 to open a file by (possibly abbreviated) name.

---

## 3. Answers to your five questions, one line each

| # | Question | Answer |
|---|----------|--------|
| a | Prefix match per subpart? | YES - COMPS: supplied-is-prefix-of-stored = match; exact distinguished via skip-count. |
| b | Empty subpart (`--`) matches anything? | YES - `-` in supplied skips stored to its next `-` (041621-041706). |
| c | Ambiguity decision? | Scanner COUNTS prefix matches; exact wins immediately; 0/1/>1 -> no-such/unique/ambiguous. |
| d | Return codes? | Files: **056** no-such, **057** ambiguous (NOT 0111). 0111/0113 = MDEAB access/type classifier, unrelated. "A=0/A=-2" = really the SPUSH/SPOP skip-count convention. |

OPEN (not needed for the resolver, documented in the carve doc section 8): hash pre-filter bit
layout, 0106-0114 access/type code meanings, FLPAR's exact descriptor output layout.
