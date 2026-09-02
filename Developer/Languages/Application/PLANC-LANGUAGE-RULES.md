# PLANC Language Rules - a checkable reference for a linter author

**Full path:** `E:\Dev\Ronny\NDInsight\Developer\Languages\Application\PLANC-LANGUAGE-RULES.md`

**What this is.** A rule-by-rule reference for anyone writing or extending a PLANC linter. Each
rule says what is WRONG, what is RIGHT, how a machine can spot it, and where in the manuals it
comes from. It is not a tutorial and it is not a grammar; the grammar is Appendix F of
ND-60.117.5.

---

## What PLANC is, and where it came from

Context rather than rules. **Everything in this section is sourced from the Wikipedia article
"PLANC" and is an ENCYCLOPEDIA CLAIM, not something measured on the machine or taken from a
manual.** It is kept separate from the numbered rules for that reason. Where it touches anything
this project has measured, the measurement wins and the disagreement is written down below rather
than smoothed over.

- **PLANC** stands for **Programming LAnguage for Nd Computers**, and is pronounced "plank".
- Norsk Data built it as a **cross-platform systems-programming language**, aimed at operating
  system components and compilers. It was proprietary.
- It is procedural, imperative and structured, in the **Pascal family**, with static strong typing
  and lexical scoping - though it has no generic `BEGIN`/`END`: blocks are `ROUTINE`/`ENDROUTINE`,
  `DO`/`ENDDO` and so on.
- **The store operator `=:` is inherited, not an oddity.** Left-to-right value flow - `5 =: a =: b`
  - is shared with Plankalkuel, ALGOL 60, **Mary** and C. Mary is the RUNIT systems language, so
  this is a direct Norwegian-computing lineage. Worth knowing because R19 and R42 stop looking
  arbitrary once you see where the operator comes from.

### Two claims in that article that this project's own measurements contradict

**1. "Array pointers were three-word structures containing base address, lower bound and higher
bound, enabling reliable runtime boundary checking."**

The three-field SHAPE is corroborated independently - the `ghidra-planc` skill records an 8-byte
descriptor `{long virtualOrigo, word lowerLimit, word upperLimit}` read out of real MC68000
firmware. The ND-100 word layout has NOT been measured here, so do not assume it is the same.

The "reliable runtime boundary checking" half is what conflicts. **R116 and R117 are MEASURED on
D100: PLANC checks no array bounds, and that reaches the tests too** - a test that wrote past the
end of an array still reported success. The likeliest reconciliation is that the descriptor makes
checking POSSIBLE and `$OPTION ARRAY-INDEX-CHECK` turns it on, whose initial value is documented
as OFF; `MAXINDEX`/`MININDEX` read the same descriptor (R53, R115). That reading leaves both
statements true. **Do not soften R116 or R117 on the strength of the article** - the default is
off, and the default is what every source in this project compiles under.

**2. `FOR ... DO ... ENDDO` "with optional step specification".**

Two problems. R58 gives the FOR list forms from RM5 6.5 - explicit values, implied ranges `a:b`,
one-dimensional arrays, pointer implied ranges, and `REVERSE` - and there is **no step** in any of
them, nor in `planc-lint.py`. The article also writes the terminator as `ENDDO`; ours is
`ENDFOR`, which is measured. At least the terminator is wrong, so treat the step as an unverified
claim. Appendix F's `for statement` production in ND-60.117.5 would settle it without touching the
machine.

**A third, smaller one:** the article names MC88000 and Intel x86 compilers, and NORD-10. Our
measured compiler-command table has `$TARGET-MACHINE` evaluating to **100, 500 or 68000** only.
Any 88000 or x86 compiler is a different product generation or an article error; it is not a
change to that table.

---

## Sources, and which one wins

| Tag | Manual | Covers |
|---|---|---|
| **RM5** | ND-60.117.5 EN PLANC Reference Manual, 5th ed. March 1986 | Compiler version **G**. The authority for the ND-100 compiler this project uses. |
| **UG6** | ND-860117.6 EN PLANC User Guide and Reference Manual, June 1989 | Compiler versions **H/I/J**. Newer, has features version G does not. |
| **ADV** | ND-20034.1 EN ND-Specific Programming and Advanced PLANC | Lists exactly which features are NEW, i.e. absent from version G. |
| **CODE** | `SINTRAN\XMSG\SINTRAN-CHAT\CHAT.PLNC`, `CHATSV.PLNC`, `SINTRAN\XMSG\tools\planc-lint.py` | Behaviour measured on a real ND-100 running PLANC version F. |

**The version question decides several rules.** The ND-100 in this project runs PLANC **F**
(CODE, build headers). RM5 documents G. UG6 documents H/I/J and describes a LARGER language.
Any rule below marked **[NEWER ONLY]** describes something UG6 allows that the ND-100 compiler
here does not have. A linter aimed at this project should treat NEWER ONLY constructs as errors
by default, with a switch to allow them.

**OCR warning for anyone re-checking a citation.** Both manual transcriptions garble the store
operator: `=:` is frequently rendered `::`, `:=`, `=::` or `:=:` inside example code. Where the
running text and the index disagree with an example, the running text and the index win. The
authoritative statements are RM5 index (`"=: operator ... 91"`, `":=: operator value of ... 93"`),
RM5 Appendix F (`<assignment op> ::= =: | :=:`) and UG6 pages 141/144.

**Notation.** Regexes are Python, case-insensitive unless stated. "code line" means: a source line
with any trailing `%` comment removed (respecting quotes) and blank/comment-only lines skipped.

---

# Section 1 - Lexical rules

## R1 - Identifier first character

**Wrong:** `5abc`, `_under`, `$name`
**Right:** `abc5`, `under_score`
An identifier is a sequence of letters, digits and underscores whose **first character must be a
letter**.

**Detect:** flag a declared name matching `^[^A-Za-z]`. Practically: capture the name after `:` in
a declaration and test `^[A-Za-z]`.
**False positives:** routine identifiers built from special characters (R9) are legal and start
with a non-letter. Exclude any name that consists ONLY of characters from the special set. Also
exclude `@` (the in-value token) and compiler commands starting `$`.
**Source:** RM5 2.11 (p.30); Appendix F `<identifier>`; UG6 p.232 (`_`).

## R2 - Identifier body characters

**Wrong:** `in-valid` (hyphen), `a.b`
**Right:** `in_valid`
Only letters, digits and `_`.

**Detect:** name matching `[^A-Za-z0-9_]`.
**False positives:** same as R1 - special-character routine names. Also SINTRAN file names inside
string literals contain `-` and `:` legally; only test identifiers, never string contents.
**Source:** RM5 2.11; RM5 error message ILLEGAL CHARACTER.

## R3 - Trailing and doubled underscore

**Wrong (RM5/version G):** `abc_`, `a__b`
**Right:** `a_b_c`
RM5 2.11 states plainly: "An underscore must not be the last character of an identifier and only
single underscore characters may be used (i.e. two consecutive underscore characters are
invalid)."

**CONTRADICTION, and you must decide which to enforce.** UG6 p.232 says `Under_score` **and**
`Under_` are both valid names, and ADV p.62 lists "Multiple Underscores in Identifiers" as a NEW
feature. So the restriction is real for version G and was lifted later.

**Detect:** `_$` on a name, or `__` inside a name.
**False positives:** none in version G. On a newer compiler this rule is pure noise - make it
optional and default it ON only when targeting the ND-100.
**Source:** RM5 2.11 (p.30) vs UG6 p.232 and ADV 4.1 (p.62). Marked as a genuine version split,
not an assumption.

## R4 - Only the first ten characters are significant

**Wrong:**
```
BYTES : memberName(1:16)
INTEGER : memberNameLen
```
Both truncate to `MEMBERNAME`. The second declaration is a **redeclaration of the first**.
**Right:** `memberName` and `memberLen` - distinct within ten characters.

An identifier may be any length, but only the first ten characters identify it.

**What it actually looks like when it goes wrong** (CODE, measured): the compiler answers
IDENTIFIER ALREADY SPECIFIED/DECLARED for the second declaration, and then every later
`memberName(slot, i)` draws MORE SUBSCRIPTS THAN IN THE ARRAY DECLARATION, because the name now
means the one-dimensional array. Four errors from one cause, and none of them mentions length.
The compiler **listing** is the tell - it prints the TRUNCATED names.

**Detect:** collect every declared name (variables, routines, parameters, TYPE names, CONSTANT
names). Group by `name[:10].upper()`. Report any group with more than one distinct name.
**False positives:** a name declared twice in genuinely separate scopes (two routines each with a
local `counterValue1` / `counterValue2`) is legal in principle - but note R113: PLANC forbids an
inner routine redeclaring a name whose scope already includes it, so most collisions are real.
Being scope-blind here is the correct trade.
**Source:** RM5 2.11 (p.30); RM5 Appendix G restriction 27A (p.347); CODE `planc-lint.py`.

## R5 - Significance limits other than ten

These are separate limits and a linter can check each:

| Limit | Applies to | Source |
|---|---|---|
| 10 chars | identifier uniqueness inside the compiler | RM5 2.11, Appendix G 27A |
| 8 chars | `$CROSS-REFERENCE` output when the compiler runs on an ND-100 | RM5 Appendix G 27B; UG6 p.163 |
| **7 chars** | EXPORTed/IMPORTed identifiers in **BRF** relocatable code (ND-100) | RM5 Appendix G 27C |
| no practical limit | EXPORT/IMPORT in NRF (ND-500/MC68000) | RM5 Appendix G 27C |
| 29 chars | ALIAS names passed to the loaders | RM5 Appendix G 27D |
| 16 chars | `$LONG-NAMES ON` uniqueness **[NEWER ONLY]**, versions I/J, not available on ND-100 | UG6 p.190; ADV 4.1 |
| 16 bytes | `module.routine` composite for `$SELECT` | UG6 p.220; ADV 4.1 |

**Detect (the important one):** for a file that EXPORTs or IMPORTs and targets the ND-100/BRF,
group exported/imported names by `name[:7].upper()` and report collisions.
**False positives:** only if the target is NRF. Gate on target machine.

## R6 - Case is not significant, except inside literals

`ident1` and `IDENT1` are the same data-element. The compiler folds everything to uppercase
**except** single-character literals, string literals, and format descriptors in INPUT/OUTPUT (i.e.
anything between apostrophes).

**Detect:** two declarations differing only in case are a redeclaration - fold to uppercase before
every name comparison in every other rule. Conversely: never uppercase the inside of `'...'` when
analysing.
**False positives:** none.
**Source:** RM5 2.2 (p.66/p.19), RM5 2.4, RM5 2.11.

## R7 - Comment syntax

**Right:** `% everything after this on the line is comment`
`%` may appear in any column. A comment runs to end of line.

**`%%` is not a comment start.** RM5 2.6: `%% this is not a comment line` - the first `%%` inside a
string is an escaped percent (R11), and outside a string a doubled percent does not begin a
comment. `% %% but this is` does.

**[NEWER ONLY]** `(% ... %)` bracketed comments, which may be embedded anywhere and may nest:
`(% Comment (% inside %) here %)`.

**Detect:** a comment stripper must track quote state. Flag `(%` if targeting version F/G.
**False positives:** `(%` can occur legitimately inside a string literal. Strip strings first.
**Source:** RM5 2.6 (p.70), 2.10 (p.76); UG6 p.132; ADV 4.1 (p.62).

## R8 - Line continuation with `&`

**Wrong:**
```
INTEGER : int1,int2,int3,
int4,int5
```
**Right:**
```
INTEGER : int1,int2,int3, & % this line will be continued
int4,int5
```
An `&` after the statement text joins the next line to this one. The `&` may be followed by a
comment on the same line.

**[NEWER ONLY] implicit continuation:** UG6 p.143-144 says end-of-line is NOT end of statement if
it follows a comma in a list of items, or a comma or semicolon in a routine parameter list. Also
`'a ' // &` splitting of a long BYTES literal.

**Detect:** for version F/G, flag a code line ending in `,` that is not followed by `&`. Report as
a warning, not an error.
**False positives:** high on a newer compiler. Also, a trailing comma inside a string literal is
harmless. Strip strings and comments first, and check the LAST non-space character of the
remaining text.
**Source:** RM5 2.5 (p.69); UG6 p.143-144.

## R9 - Statement separator `;`

More than one statement per line requires `;` between them. A trailing `;` on a single statement is
allowed but not required.

**Restriction:** a statement containing a MACRO call, an INLINE routine call, or a `$INCLUDE`
command may be terminated by a semicolon, but **no other statement may follow the semicolon**.

**Detect:** for a line containing a known INLINE routine call or `$INCLUDE`, flag any non-blank
text after the first `;`.
**False positives:** requires knowing which routines are INLINE - collect names declared
`ROUTINE INLINE ...`. Semicolons inside string literals must be ignored.
**Source:** RM5 2.4 (p.69); RM5 Appendix G restriction 1 (p.344); UG6 p.154.

## R10 - Special-character routine names

A routine identifier may alternatively be made **only** of these characters:

```
! " $ * + - . / : < = > ? \ [ ]
```
(RM5 p.176 also prints a dagger glyph; that is an OCR artifact and should not be relied on -
**UNVERIFIED** which character it really is.)

Rules: a `$` cannot **begin** such a name; a `.` can **only** begin such a name; and a space must
precede the routine identifier if it begins with one of these characters.

**Detect:** for a declared routine name that is not a normal identifier, check the character set,
the `$`-first and `.`-first rules, and that the token before it (usually `:`) is followed by a
space.
**False positives:** low. Note R4 still applies - such names are also unique in ten characters.
**Source:** RM5 7.1 (p.176).

## R11 - Escaping inside a string literal

Inside `'...'`, the three characters `%`, `&` and `'` must each be **doubled**.

| Written | Value |
|---|---|
| `'his && hers'` | `his & hers` |
| `'two %%% characters'` | `two %% characters` |
| `'Tom''s 5 %% share'` | `Tom's 5 % share` |
| `''''` | `'` (one apostrophe) |

**Why it bites:** UG6 p.132 - a single `%` inside a string makes the compiler assume the string
continues on the next line, and it will read the rest of the program as a string looking for the
closing `'`. "A quite destructive compile-time error."

**Detect:** inside a scanned string literal, an odd-count run of `%` or `&` is an error.
Apostrophes are already handled by the doubling convention of the scanner itself.
**False positives:** none if the scanner is correct.
**Source:** RM5 2.10 (p.76); UG6 p.132, p.145-146.

## R12 - Single-character literal `#`

**Right:** `#a`, `#Z`, `#(`
`#` followed by exactly one ASCII character yields that character's value, in a `BYTE`.

To write `%`, `&` or `'` as a single-character literal, use **one** occurrence after the `#` -
the doubling rule of R11 does **not** apply here.

`'a'` is a **BYTES** and is NOT equivalent to `#a`, which is a **BYTE**. They have different
internal representations. This is the single most productive source of ILLEGAL DATA TYPE in real
code - see R76.

**[NEWER ONLY]** `#` also forms Ada radix notation: `16#7F#`, `2#1101_1001#`, radix 2 to 32.

**Detect:** flag `#` followed by two or more characters before a delimiter, unless it matches the
Ada form `\b\d{1,2}#[0-9A-Fa-f_]+#` and the newer dialect is enabled.
**False positives:** `#` inside strings. RM5's OCR renders this character as a pound-sign glyph in
several places - do not take that literally; the real character is `#`.
**Source:** RM5 2.9 (p.76), Appendix F `<character literal>`; UG6 p.133, p.146, p.158-159.

## R13 - String subscripting is 0-based when the literal sets the bounds

**Wrong:** assuming `string(1)` is the first character of `BYTES : string := 'i am the greatest'`.
**Right:** `string(0)` is the first character; the last is `string(len-1)`.

RM5 4.1.7.1 (p.109), verbatim: "the first character can be referenced by `string[0]`, the second by
`string[1]` and so on." UG6 p.166: "If the range is specified by the initialization literal, the
bytes in the string are addressed with indexes ranging from zero up to the number of bytes in the
literal minus one."

But when you write the range yourself - `BYTES : magic(1:100)` - the bounds are exactly what you
wrote. **Both forms exist in the same file in real code**, which is why this must be tracked per
variable, not assumed globally.

**Detect:** build a table `name -> (lo, hi)`. For `BYTES : n := 'literal'` set `(0, len-1)`. For
`BYTES : n(a:b)` set `(a, b)`. Flag a constant subscript outside `[lo, hi]`.
**False positives:** none for constant subscripts. Do not attempt this for computed subscripts.
**Source:** RM5 4.1.7.1; UG6 p.165-166; CODE (a live defect: a ten-byte literal indexed to 16).

## R14 - Octal literals

**Right:** `0B`, `777B`, `-765B`
Digits 0-7 only, then the letter `B`. Optional leading minus.

**Detect:** `\b-?[0-9]+B\b` where the digit run contains an `8` or `9` is an error.
**False positives:** an identifier ending in `B` preceded by digits cannot occur (R1), so this is
safe. Strings must be excluded.
**Source:** RM5 2.7.1 (p.71).

## R15 - Real literals

| Wrong | Why |
|---|---|
| `12` | valid integer, but no decimal point - it is not a REAL |
| `.0` | no digit before the decimal point |
| `+1.2` | must not be preceded by `+` |
| `1.5E2.5` | exponent must be a whole number |
| `1.6E+2` | exponent must not carry `+` |

The whole-number part must be present. The exponent must not be preceded by a space.

**Detect:** `(?<![\w.])\.\d` for the missing-leading-digit case; `\+\d+\.` for the leading plus;
`[0-9]E[+]` and `E-?\d+\.\d` for bad exponents.
**False positives:** `.` is also the record-access operator, so `rec.5` cannot occur but
`ADDR(x).0` would look similar - in practice a `.` followed by a digit is always a malformed real.
Strings and comments must be excluded.
**Source:** RM5 2.7.2 (p.72).

## R16 - `$` in column-anything means compiler command

A line whose first non-blank character is `$` is a compiler command. Blanks may precede the `$`.
Such commands **can only be written between statements** - never in the middle of a statement, and
never between the continuation lines of a statement.

**Detect:** flag a `$`-command line whose previous code line ends with `&`.
**False positives:** `$` inside an OUTPUT string literal is the newline character (R83) and is not
a command; only the FIRST non-blank character of a line matters. `$*` is inline assembly, which is
a compiler command too.
**Source:** RM5 Appendix A 0.2 (p.248).

---

# Section 2 - Declarations

## R17 - The declaration shape

```
data-type : ident [:= lit-exp] [, ident [:= lit-exp]] ...
```
The colon separates a **type expression** from a **list of names**. There is no `VAR`, no `DIM`, no
type-after-name.

**Detect:** a line that starts with a known type keyword but has no `:` before the first name is
malformed.
**False positives:** `INTEGER` also appears inside `IMPORT (...)`, `TYPE x = ...`, `ROUTINE ...`
headers and `CONSTANT` expressions. Anchor to line start and exclude those leading keywords.
**Source:** RM5 3.1 (p.83).

## R18 - Every type keyword the linter must know

**Simple types:** `INTEGER`, `REAL`, `BOOLEAN`, `LABEL`, `VOID`, `ENUMERATION`, `POINTER`
**Composite type constructors:** `ARRAY`, `RECORD`, `SET`, `ROUTINE`
**Predefined:** `BYTE` (= `INTEGER RANGE (0:255)`), `BYTES` (= `BYTE ARRAY PACKED`),
`BITS` (= `BOOLEAN ARRAY PACKED`)
**Sized integer subtypes:** `INTEGER1` (8 bit, -128:127), `INTEGER2` (16 bit, -32768:32767),
`INTEGER4` (32 bit, -2147483648:2147483647)
**Real subtype:** `REAL8` (64 bit, 10**-76 to 10**76, 15 significant digits)
**Modifiers:** `RANGE` (INTEGER only), `PRECISION` (REAL only), `READ`, `WRITE`, `PACKED`
**[NEWER ONLY]** `BOOLEAN1`, `BOOLEAN2`, `UNSIGNED`, `PUBLIC`

**Detect:** anything else appearing where a type is expected is either a user TYPE name (collect
those from `TYPE x = ...` and `$INCLUDE`d files) or an error.
**False positives:** heavy, if include files are not read. A linter that cannot read the includes
must NOT report unknown type names as errors.
**Source:** RM5 2.3 keyword list (p.67), 3.2, 3.3, 3.12, 4.1.7; UG6 p.152-153, p.186-187, p.227.

## R19 - `:=` initialises, `=` equivalences, `=:` stores

Three different things share a colon and an equals sign, and mixing them is the commonest error in
this language.

| Token | Meaning | Where legal |
|---|---|---|
| `:=` | give a declared variable an initial value | **declarations only** |
| `=` | (a) CONSTANT value (b) address equivalence in a declaration (c) the equality operator | declarations and expressions |
| `=:` | **store** - the run-time assignment | executable statements |
| `:=:` | swap/change | executable statements |

**Wrong:** `count := count + 1`
**Right:** `count + 1 =: count`

**What the compiler says** (CODE, measured): ILLEGAL SYNTAX `":="`, preceded by a WARNING that the
expression does not store a value - which reads like a separate complaint about the line.

**Detect:** on a code line that is NOT a declaration, `^\s*[A-Za-z]\w*\s*(\([^)]*\))?\s*:=`.
**False positives:** a declaration line - exclude any line whose first token is a type keyword or a
known user TYPE name. `:=:` must not be mistaken for `:=` - test for `:=:` first.
**Source:** RM5 5.1 (p.138), 2.3 (p.68), Appendix F `<assignment op>`; UG6 p.141, p.144;
CODE `planc-lint.py`.

## R20 - Which declarations may carry an initialiser

RM5 3.1 (p.83): "Initial value is valid only for INTEGER, REAL, BOOLEAN types."

In practice arrays, records, sets, enumerations, pointers and BYTES all take initialisers too
(RM5 4.1.1, 4.2.1, 4.3.1, 3.6, 3.7), each with its own form. The restriction that actually matters
to a linter is the scope one, below.

**Rule that bites:** "An initial value should normally be used in the outer level of a module. If
an identifier is to have an initial value inside a routine, then its access must be declared as
READ."

**Wrong (inside a routine):**
```
INTEGER : counter := 0
```
**Right (inside a routine):** either `INTEGER READ : counter := 0` (and then it can never change),
or declare it without an initialiser and store into it as the first statement.

**Detect:** a declaration with `:=` that lies between a `ROUTINE`/`PROGRAM` header and its
`ENDROUTINE`, and does not contain `READ`.
**False positives:** none known. The mechanism is explained at RM5 8.6: a local without READ
access is allocated dynamically on the stack, so it has no place for a compile-time initial value.
**Source:** RM5 3.1 (p.83), 8.6 (p.221-222).

**MEASURED 2026-08-24 on D100 - and it is a HARD ERROR, not the "normally" the manual's wording
suggests.** This entry used to say "report as a warning". The compiler disagrees:

```
    166   (141)/MAINUI  *** ERROR   - INITIAL VALUE ILLEGAL HERE "SMALLOPEN"
    167   (142)/MAINUI  *** ERROR   - INITIAL VALUE ILLEGAL HERE "BIGOPEN"
    228 LINES COMPILED.       2 DIAGNOSTICS.
```

from `BOOLEAN : smallOpen := FALSE` inside a `PROGRAM`. Report it as an error.

**AND IT IS WORSE THAN A FAILED COMPILE.** The link then SUCCEEDED over the failed compile and the
program RAN, with both flags never set - so two menu keys silently did nothing while a third
worked, which reads as a logic bug and sent the diagnosis to the `NOT` operator, which was
innocent. The compile errors were on screen throughout and were read as clean, because the
`0 DIAGNOSTICS` that stays visible belongs to the SECOND pass and sits directly under a `COMPILE`
that had two.

**Now enforced** by `SINTRAN/XMSG/tools/planc-lint.py`, `READ` exempted, with the message saying
why it is expensive: the build still links and runs with the name unset.

## R21 - CONSTANT uses `=`, never `:=`

**Wrong:** `CONSTANT rows := 10`
**Right:** `CONSTANT rows = 10`

`CONSTANT ident [= lit-exp] [, ident = lit-exp] ...`

**Detect:** `^\s*CONSTANT\b.*:=`.
**False positives:** none.
**Source:** RM5 3.5 (p.86). Note RM5's own example on p.169 prints `CONSTANT rows:=10` - that is
an OCR error, contradicted by the syntax box and every other example.

## R22 - CONSTANT default-value rule

If `=` and the expression are omitted, the identifier is INTEGER and takes **the next integer value
higher than the previous integer value in this CONSTANT statement**; if there is no previous
integer value, it is 0.

```
CONSTANT zero, rl2=1.1, one, bl2=FALSE, two   % zero=0, one=1, two=2
CONSTANT four=4, five, nine=four+five         % five=5, nine=9
```

**Detect:** this is a semantic aid, not an error check - a linter that computes constant values
should implement it, because it is easy to assume "just increments from the last item" and get it
wrong when a REAL or BOOLEAN sits in between.
**Source:** RM5 3.5 rules 1 and 2 (p.87); UG6 p.172-173.

## R23 - ARRAY needs one `ARRAY` keyword per dimension

**Wrong:** `REAL ARRAY : rl3(1:2,1:3,1:4)` - three index sets, one ARRAY keyword.
**Right:** `REAL ARRAY ARRAY ARRAY : rl3(1:2,1:3,1:4)`

The compiler answers INCONSISTENT DIMENSIONS.

**Detect:** count `ARRAY` keywords in the type expression; count comma-separated index sets in the
declarator's parentheses; they must be equal. Add one implicit ARRAY for `BYTES` and `BITS`
(R25).
**False positives:** commas inside a nested expression in a bound - e.g. `(1:MAXINDEX(a,1))` -
must not be counted. Count commas at parenthesis depth 1 only.
**Source:** RM5 4.1.1 (p.101), 4.1.2 (p.104); RM5 error INCONSISTENT DIMENSIONS.

## R24 - ARRAY bounds are mandatory unless an initialiser supplies them

**Wrong:** `INTEGER ARRAY : ar1` - error ARRAY BOUNDS MISSING.
**Right:** `INTEGER ARRAY : ar1(1:5)` or `INTEGER ARRAY : ar3 := (-2,6,21,-108)`

With an initialiser and no explicit bounds, the compiler supplies `0 : n-1`.

**Detect:** an ARRAY declarator with neither `(` bounds nor `:=`.
**False positives:** a declarator continued onto the next line with `&`. Join continuations first.
**Source:** RM5 4.1.1 (p.102); RM5 error ARRAY BOUNDS MISSING.

## R25 - `BYTES` and `BITS` already contain one ARRAY keyword

`BYTES` is `BYTE ARRAY PACKED`; `BITS` is `BOOLEAN ARRAY PACKED`. So:

```
BYTES ARRAY : safe_els(0:9,0:9)          % two dimensions
BYTE ARRAY ARRAY PACKED : safe_els(0:9,0:9)   % exactly equivalent
```

**Consequence a linter should flag:** you cannot put a READ/WRITE modifier on the ELEMENTS of a
`BYTES`, because the modifier must precede all ARRAY keywords and `BYTES` hides the first one. If
element-level access modification is wanted, the declaration must be written out as
`BYTE READ ARRAY ARRAY PACKED`.

**Detect:** `BYTES\s+(READ|WRITE)` or `BITS\s+(READ|WRITE)` before an `ARRAY` keyword - report that
this modifies the whole array, not its elements, which is probably not what was meant.
**False positives:** it is legal, just usually not what the author intended. Warning, not error.
**Source:** RM5 4.1.7.1 (p.110), 4.1.7.2 (p.111).

## R26 - READ/WRITE may not sit between ARRAY keywords

**Wrong:** `INTEGER ARRAY READ ARRAY : a(1:2,1:3)`
**Right:** `INTEGER READ ARRAY ARRAY : a(...)` (elements read-only) or
`INTEGER ARRAY ARRAY READ : a(...)` (whole array read-only)

RM5 4.1.4: "the access mode keywords, READ/WRITE, may not be placed between the ARRAY keywords.
READ/WRITE must precede or follow all the ARRAY keywords."

**Detect:** in a type expression, `ARRAY\s+(READ|WRITE)\s+ARRAY`.
**False positives:** none.
**Source:** RM5 4.1.4 (p.107).

## R27 - The meaning of READ/WRITE before vs after ARRAY

| Written | Effect |
|---|---|
| `INTEGER READ ARRAY : a` | cannot store into an individual element; can store the whole array |
| `INTEGER ARRAY READ : a` | cannot store the whole array; can store individual elements |
| `INTEGER READ ARRAY READ : a` | cannot store either way |

This is genuinely counter-intuitive and worth a linter **note** whenever a READ or WRITE appears in
an array type, quoting which of the two it selected.
**Source:** RM5 4.1.4 (p.107); UG6 p.223-224 (READ), p.242 (WRITE).

## R28 - A RECORD type must be introduced by TYPE

**Wrong:** `RECORD : myrec` used as a bare declaration.
**Right:**
```
TYPE partrec = RECORD
    INTEGER : partnumber
    BYTES   : partname(1:20)
ENDRECORD
partrec : mypart, yourpart
```
RM5 4.2: "The RECORD data type must be declared in a TYPE specification statement; declaration
statements for RECORD data-elements must use a record data type specified previously."

**Detect:** a line matching `^\s*RECORD\b` that is not preceded on the same logical statement by
`TYPE\s+\w+\s*=`.
**False positives:** UG6 p.239 shows an anonymous `RECORD ... ENDRECORD : DoublInt` form
**[NEWER ONLY]**. Gate on dialect.
**Source:** RM5 4.2, 4.2.1 (p.112).

## R29 - Record initialisation is all-or-nothing and order-exact

If ANY component of a record data-element is initialised, **all** components must be. The values
must match the declared components in type and in order; a nested record component needs its own
parenthesised group.

**Wrong:** `partrec : psupply := (123)` for a three-component record.
**Right:** `partrec : psupply := (123,'power supply',100.2)`

**Detect:** count top-level commas in the initialiser list and compare against the component count
of the named TYPE. Requires resolving the TYPE, so only possible when the TYPE is in the same file
or a readable include.
**False positives:** variant records inherit the base record's components first, so the expected
count is base-count + variant-count. Get this wrong and every variant initialiser is reported.
**Source:** RM5 4.2.1 (p.114), 4.2.2 (p.116).

## R30 - `MOD` in a record is alignment, not modulo

```
TYPE r = RECORD PACKED
    BITS    : b(11:37)
    INTEGER : i MOD 4
ENDRECORD
```
Inside a record component declaration, `MOD literal-expr` forces the component to start at a
displacement from the record's start that is a multiple of that value.

**Detect:** nothing to flag; a linter must simply not mistake this `MOD` for the arithmetic
operator when it parses a record body.
**Source:** RM5 4.2.1 (p.114); ADV 4.1 (p.64).

## R31 - SET base type restrictions

Valid base types are **ENUMERATION** and **INTEGER RANGE** only.

For an integer set the range must be `0 : x` with `x <= 255` - the **lower bound must be zero** and
there is a hard 256-value ceiling.

**Wrong:** `INTEGER RANGE (1:300) SET : s`
**Right:** `INTEGER RANGE (0:255) SET : s`

**Detect:** `INTEGER\s+RANGE\s*\(\s*(-?\d+)\s*:\s*(\d+)\s*\)\s*SET` with lower != 0 or upper > 255.
**False positives:** bounds written as constant expressions rather than literals cannot be checked
without constant folding - skip those rather than guess.
**Source:** RM5 4.3.1 (p.122); UG6 p.234, p.192 (`IN` note: integer sets have INTEGER1 UNSIGNED
base type).

## R32 - An unnamed set literal needs its type name in front

**Wrong:** `{1,3,5,7,9} =: numbers` - RM5 says this "will give a compile error".
**Right:** `tnumbers (1,3,5,7,9) =: numbers` where `TYPE tnumbers = INTEGER RANGE (0:100) SET`

**Restriction:** such an unnamed constant set "must not be the first statement of a routine, unless
the entire statement is contained within parentheses."

**Detect:** a `{`/`(` set-literal at the start of an expression with no preceding type identifier;
and the same construct as the first executable statement of a routine.
**False positives:** the manual's own examples print these with both `{}` and `()`, which is an OCR
inconsistency - PLANC uses **parentheses** (see R41 and RM5 p.127-128 running text). A brace-based
rule will misfire; match on "a parenthesised comma list where an expression was expected".
**Source:** RM5 4.3.3 (p.125, p.127).

## R33 - POINTER qualification and NIL

`d-type POINTER : ident [:= p-ident]`

Note the ordering trap RM5 1.4 makes explicit:

| Written | Means |
|---|---|
| `REAL ARRAY POINTER` | a pointer **to an array** of reals |
| `REAL POINTER ARRAY` | an **array of pointers**, each to one real |

`NIL` is the only value every pointer can hold regardless of its qualification.

**Detect:** worth a linter **note** when both `ARRAY` and `POINTER` occur in one type expression,
printing which reading applies.
**Source:** RM5 1.4 (p.53), 3.7 (p.89); UG6 p.206.

## R34 - A pointer to an undefined type silently allocates one word

```
norec POINTER : bp    % TYPE norec has not yet been defined
```
allocates one word only, as if it pointed to a simple type. No error is given.

**Detect:** a pointer declaration whose qualification is a name not yet defined by a `TYPE`
statement earlier in the file (PLANC is single-pass, R57).
**False positives:** types coming from `$INCLUDE` files the linter cannot read. Suppress the rule
entirely when includes are unresolved.
**Source:** RM5 Appendix G restriction 18 (p.346).

## R35 - LABEL must be declared, and only locally

```
LABEL : lab1, loop, next
...
lab1 : 1 =: int1
GO lab1
```
Labels must be declared if a `GO` refers to them. UG6 p.200: "Labels can only be declared locally
(i.e. inside routines)."

**Detect:** collect `GO <name>` targets and `LABEL : ...` declarations; report a `GO` to an
undeclared label. Report a `LABEL` declared at module level.
**False positives:** a label declared in an outer routine and used in a nested one is in scope.
Scope-blind checking is fine for the "declared nowhere" case only.
**Source:** RM5 3.9 (p.91), 6.1 (p.154); UG6 p.199-200.

## R36 - Predeclaration `?`

```
INTEGER : int1?
ROUTINE VOID,VOID : rt2?
```
A predeclaration is written exactly like the real declaration with `?` after the name. The real
declaration must follow later in the same module. Used for mutually recursive routines and for
initialising a static linked list of records.

**Restrictions:**
- A predeclared identifier must not appear again in another predeclaration (ILLEGAL PREDECLARATION).
- A routine name declared in a predeclaration **must not later appear as the identifier in a
  PROGRAM statement**. The compiler does not detect this.
- Array bounds in the real declaration must match the predeclaration
  (ARRAY BOUNDS CONFLICT WITH A PREDECLARATION).
- An inner-level routine that is predeclared "will not be executed correctly".

**Detect:** collect `name?` predeclarations; report one with no matching later declaration; report
one that later appears as `PROGRAM : name`; report a predeclared routine declared inside another
routine.
**False positives:** the real declaration may be in an `$INCLUDE`d file.
**Source:** RM5 3.16 (p.98); RM5 Appendix G restrictions 12 and 23ii (p.345, p.347).

## R37 - Equivalence `=` in a declaration

```
INTEGER : int1, int2
REAL    : rl1, rl2 = int1
```
forces `rl2` to begin at the same storage location as `int1`. Different lengths are NOT checked -
the compiler may emit the warning EQUIVALENCE MAY CAUSE STORAGE CONFLICT (ND-100 only).

Where equivalence is used among record components and initial values are wanted, **only the first
declaration of the data-element may have an initial value**.

**Detect:** a declaration of the form `type : newname = oldname` where `oldname` was declared with a
type of a different `SIZE` - report a note, not an error. Report an initialiser on a
non-first member of an equivalence group inside a record.
**False positives:** sizes are machine-dependent (Appendix C), so a size comparison is only
meaningful once a target is fixed.
**Source:** RM5 3.15 (p.97), 4.2.1 (p.114); UG6 p.144, p.139.

## R38 - `TYPE b = a` where `a` is a RECORD is illegal and silent

```
TYPE A = RECORD ... ENDRECORD
TYPE B = A                       % illegal, NO error message
```

**Detect:** `TYPE\s+(\w+)\s*=\s*(\w+)\s*$` where the right-hand name is a known RECORD type.
**False positives:** requires knowing the right-hand name is a record. Only fire when the TYPE is
declared in the same file.
**Source:** RM5 Appendix G restriction 8 (p.344).

## R39 - Do not redeclare a name a library include already defines

**Measured, CODE:** `CHATSV.PLNC` declared its own `XFWTF`, which `XMP-B02:DEFS` also defines. The
compiler reported IDENTIFIER ALREADY SPECIFIED/DECLARED and then **fell over** with
`ASSERT VIOLATION AT 136747B`, taking the batch job with it. This is not a warning you can walk
past.

Related: `XMP-B02:DEFS` and `XMSG-PL-VALUES-L:INCL` share 184 constant names. Including both is 184
redefinitions.

**Detect:** parse the include files named in `$INCLUDE`, collect their `CONSTANT` and declared
names, and report any redeclaration in the main source. `planc-lint.py` already does exactly this
for `XMP-B02-DEFS.readable.txt`.
**False positives:** none, if the include is genuinely being included.
**Source:** CODE `planc-lint.py`; RM5 error IDENTIFIER ALREADY SPECIFIED/DECLARED.

## R40 - A name that is never declared can still compile clean

**Measured, CODE:** a BOOLEAN stored to and tested in two routines, declared in neither and not at
module level, gave **0 DIAGNOSTICS**, and the program ran with it permanently set - printing it
immediately after `FALSE =: x` showed `1`.

This contradicts RM5's NOT PREVIOUSLY DECLARED message, which suggests the compiler ought to catch
it. **The measured behaviour is what a linter must protect against.** Mark this in any report as
project-measured on PLANC F, not as manual-documented.

**Detect:** collect every name on the receiving side of `=:` (regex
`=:\s*([A-Za-z_]\w*)\s*(?![\w(])`) and check each against the set of names declared anywhere in the
file - deliberately scope-blind. `planc-lint.py` implements this.
**False positives:** names coming from an unreadable `$INCLUDE`; record components reached via a
`USING` block (R66), which are declared inside the record TYPE, not at statement level. Make sure
the declaration harvester reads record bodies and `INTEGER RANGE (...) : name` forms - an earlier
version of this rule missed the RANGE clause and cried wolf.
**Source:** CODE `planc-lint.py`, PLANC-DEVELOPER-GUIDE.md.

---

# Section 3 - Operators and expressions

## R41 - Subscripts use ROUND brackets

**Wrong:** `ADDR(buf[0])`
**Right:** `ADDR(buf(0))`

PLANC F answers `EXPECTS ")" ILLEGAL SYNTAX "["` and then INVALID PARAMETER LIST for the enclosing
call. The COSMOS Programmer's Guide prints its samples with square brackets; the typesetting is
wrong and the compiler is right.

**Detect:** `\w\s*\[`.
**False positives:** `[` inside a string literal or a comment. Strip both first.
**Source:** CODE `CHAT.PLNC` header, `planc-lint.py`; RM5 4.1.3 (p.105) syntax box.

## R42 - Operator precedence table (the whole thing)

Higher number binds tighter. Binary operators are **left associative**. Parentheses override.

| Pr | Operator | Kind | Operand types |
|---:|---|---|---|
| 14 | `ADDR` | standard routine | any |
| 14 | `IND` | standard routine | pointer |
| 13 | `.` | binary, record component access | record / record pointer |
| 11 | `**` | binary, exponentiation | left integer or real, **right must be integer** |
| 11 | `ABS` | unary | integer, real; on a SET gives cardinal number |
| 11 | `MOD` | binary | integer only |
| 11 | user routines (default) | | any |
| 11 | most standard routines: `BIT`, `BIT_POSITION`, `BIT_SIZE`, `BLOCKSIZE`, `CLOSE`, `CONVERT`, `DISPOSE`, `FILESIZE`, `FORCE`, `INPUT`, `MAXINDEX`, `MININDEX`, `MONITOR_CALL`, `NEW`, `OPEN`, `OUTPUT`, `PRED`, `SIZE`, `SUCC`, `TYPEOF` | | |
| 10 | unary `-` (change sign) | unary | integer, real |
| 10 | `++`, `--` | unary | integer, pointer |
| 9 | `*`, `/` | binary | integer, real |
| 8 | `+`, binary `-` | binary | integer, real |
| 8 | `SHIFT` | binary | integer |
| 8 | `//` **[NEWER ONLY]** | binary, concatenation | BYTES |
| 7 | `:` (range designator) | binary | integer, enumeration, pointer |
| 6 | `=`, `><`, `>`, `<`, `>=`, `<=` | binary, relational | see R45 |
| 6 | `,` (list item separator) | binary | any listable |
| 5 | `IN` | binary | integer/enumeration/pointer against set or range |
| 5 | `INSERT`, `APPEND`, `REMOVE` | standard routines | |
| 4 | `NOT` | unary | integer, Boolean, set |
| 3 | `AND` | binary | integer, Boolean, set |
| 2 | `OR`, `XOR` | binary | integer, Boolean, set |
| **1 / 12** | `=:` store, `:=:` swap | binary | see R43 |
| 1 | `RETURN`, `ERRETURN` | | |

**Note the two-sided priority of the assignment operators.** RM5 5.1: the left side has priority 1
(the lowest possible, so everything to the left is evaluated first) and the right side has priority
12 (so the target expression on the right binds tightly). This is why `1+2 =: int1+4 =: int2`
stores 3 into `int1` and 7 into `int2`.

**Detect:** a linter that reformats or evaluates expressions must implement this table exactly.
More usefully: emit a **note** on any expression mixing three or more different priority levels
without parentheses.
**Source:** RM5 5.1 (p.138), 5.2 (p.141), 5.3 (p.145), 5.4 (p.148), 7.10 (p.204-207);
UG6 p.129-135 summary table and the per-operator entries p.143-232.

## R43 - `=:` and `:=:` semantics differ in what they leave behind

`a =: b` stores a into b, and **the value of the expression is unchanged** - still a.
`a :=: b` stores a into b, and **the value of the expression becomes b's previous value**.

```
3 =: int
4 :=: int          % int is now 4, the expression's value is 3
```
```
3 =: i ; 4 =: j
i :=: j =: i       % exchanges i and j
```

**Detect:** nothing to flag directly. But a linter reading `x :=: y` where the result is discarded
should note that the swap's whole point is the returned value.
**Source:** RM5 5.1 (p.138-140); UG6 p.141.

## R44 - `:=:` works only on simple types

RM5 5.1 operator table: store `=:` accepts "all simple, composite and predefined"; change `:=:`
accepts "all simple" only. UG6 p.141 repeats: "only simple types can be used, while the `=:` works
with both simple and composite types."

**Detect:** `:=:` where either operand is a known ARRAY, RECORD, SET or BYTES variable.
**Source:** RM5 5.1 (p.138); UG6 p.141.

## R45 - Not-equal is `><`

**Wrong:** `!=`, `<>`, `/=`, `=<`, `=>`
**Right:** `><`, and `<=`, `>=`

`=<` is especially nasty: it gets ILLEGAL SYNTAX, but the compiler blames the line **above** first
with ILLEGAL DATA TYPE "AND", so the operator is the last thing anyone looks at.

**UNVERIFIED:** UG6's SET entry (p.234) lists `<>` among the set test operators alongside `><`.
Every other place in both manuals, and all working code, uses `><`. Treat `<>` as an error and note
the discrepancy rather than accepting it.

**Detect:** `!=|<>|/=|=<|=>` outside strings and comments.
**False positives:** `=>` cannot appear legitimately. `<>` could in theory be `<` followed by `>` in
a bizarre expression - vanishingly unlikely. `/=` cannot appear because `/` and `=` would need an
operand between them.
**Source:** RM5 2.3 (p.68), 5.4 (p.148); CODE `planc-lint.py`.

## R46 - There is no automatic conversion between base types

An expression must contain operands of one data type only. Integer and real do not mix; pointer and
integer do not mix.

**Right:** `int CONVERT REAL =: rl`

`CONVERT` changes value representation between integer and real types/subtypes.
`FORCE` reinterprets the bits and **requires exactly the same size** - a size mismatch gives
ILLEGAL DATA-ELEMENT TO BE CONVERTED. Subtypes of the same base type (INTEGER1 vs INTEGER4, REAL
PRECISION 7 vs 15) DO mix, and the result takes the larger of the two.

**AND IT IS A WARNING, NOT AN ERROR - the program links, runs, and answers something plausible.**
MEASURED 2026-08-27 while establishing whether a routine may take `ADDR` of a buffer it was handed:

```
    668/XRADDROF  *** WARNING - ILLEGAL DATA-ELEMENT TO BE CONVERTED
```

`ADDR(buf(0)) FORCE INTEGER4` on an **ND-100 address, which is SIXTEEN BITS**, is exactly this size
mismatch. The build did not stop. The routine returned `3473663` where the caller's own `ADDR` said
`65791`, and it did so **reproducibly across two separate builds** - which reads as a measurement
and is not one. Two attempts were published and withdrawn before the warning in the listing was
read.

Written as `FORCE` to a 16-bit type it draws **no diagnostic at all** and the two addresses agree.
So: the diagnostic that mattered was in the listing the whole time, it was only a warning, and
**a warning here invalidates a result as thoroughly as an error would** - see R110.



**Detect:** a binary arithmetic or relational operator with one operand of known integer type and
the other of known real type. Requires a type table; only attempt it for variables declared in the
same file.
**False positives:** overloaded operators. If the file declares `ROUTINE REAL, REAL (INTEGER) : +`
then mixed `+` is legal. Collect user routine declarations whose name is an operator and suppress
the rule for that operator.
**Source:** RM5 5 intro (p.136), 5.5 (p.150-151); UG6 p.161-162, p.174, p.126-128.

## R47 - Integer division keeps no remainder; use MOD

`27 MOD 5` = 2, `-27 MOD 5` = -2, `27 MOD -5` = 2, `-27 MOD -5` = -2.
The sign follows the **left** operand.

**Detect:** nothing to flag. Worth knowing when constant-folding.
**Source:** RM5 5.2 (p.141-142).

## R48 - SHIFT direction, and a version disagreement about fill

Positive second operand = shift **left**; negative = shift **right**.

- **RM5 5.2 (p.142):** for a signed integer the sign bit is not affected by left shifts and is
  **extended** for right shifts; for an unsigned type (a non-negative integer range) zeroes come in.
- **UG6 p.235:** "filling vacant bits with zeros", with no signed/unsigned distinction.

These are not the same statement. Do not assert either as fact for an arbitrary compiler version.
A linter may emit a **note** on `SHIFT` with a negative right operand and a signed left operand,
saying the fill behaviour is version-dependent.
**Source:** RM5 5.2 (p.142) vs UG6 p.235. Genuine conflict, marked as such.

## R49 - `ABS` of a SET is the MAXIMUM possible members, not the count

```
colour SET : bright := (red, green, yellow, pink)   % 4 members
ABS bright =: int1                                  % int1 = 8, the base type's size
```
RM5 5.3: "cardinal number, i.e. result is an integer value of the **maximum possible** number of
members of the operand set."

**Detect:** `ABS` applied to a known SET variable - emit a note.
**Source:** RM5 4.3.3 (p.126), 5.3 (p.145).

## R50 - Logical operators on integers are bitwise, on Booleans are logical

`AND`, `OR`, `XOR`, `NOT` accept integer, Boolean **or** set operands. With integer operands the
operator applies to every bit. With sets: `AND` = intersection, `OR` = union, `XOR` = difference,
`NOT` = complement.

**Both operands must be the same kind.** Mixing a Boolean and an integer is an error.

**Detect:** `AND`/`OR`/`XOR` with one operand a known BOOLEAN and the other a known INTEGER.
**False positives:** overloading (see R46).
**Source:** RM5 5.3 (p.145-147); UG6 p.147, p.202, p.232.

## R51 - Expression evaluation order is not guaranteed, and parts may not be evaluated at all

RM5 5.3: "the actual order of interpretation is not fixed so long as the result is mathematically
and logically equivalent. Indeed it can happen that **part of an expression is not evaluated at
all**."

```
IF ( i = 1 OR 1.5 + i =: r > 10.1 ) THEN ...
```
If `i` is 1, nothing is stored into `r`.

**Detect:** a `=:` store appearing inside the condition of an `IF`, `WHILE`, `ASSERT` or a `CASE`
selector, on the right of `OR`/`AND`. Report as a warning: the store may not happen.
**False positives:** the store on the FIRST operand of the expression always happens. Only flag
stores that appear after an `AND`/`OR`.
**Source:** RM5 5.3 (p.145).

## R52 - `.` reaches through pointers implicitly

```
r POINTER : rp := ADDR(rec)
rp.i =: k        % same as ind rp.i =: k
```
Dot access on a record pointer dereferences automatically. `rec.element`, `recp.element` and
`ADDR rec.element` all reach the same data-element.

A pointer declared to a **base** record type may point to a **variant** and its variant components
can be reached through it - the compiler accepts it, and if the pointed-to object is really the base
type, memory outside the record gets written. UG6 p.227 calls this out as a source of hard-to-find
errors.

**Detect:** flag a component name reached through a pointer where the component belongs to a
variant of the pointer's declared type, not to the declared type itself. Needs a record-type table;
only feasible in-file.
**Source:** RM5 4.2.3 (p.117), 4.2.2 (p.116); UG6 p.182, p.219, p.227.

## R53 - `MAXINDEX`/`MININDEX` dimension must be a literal or constant

**Wrong:** `MAXINDEX(a, dimVar)`
**Right:** `MAXINDEX(a, 1)`
RM5 7.9: "the dimension number must be an integer literal, it cannot be an identifier or an
expression."

**[NEWER ONLY]** the dimension may be omitted for a one-dimensional array: `MaxIndex(A)`.

**Detect:** `(MAXINDEX|MININDEX)\s*\(\s*\w+\s*,\s*([A-Za-z_]\w*)` where the second argument is not
a declared CONSTANT. Also flag the one-argument form when targeting version G.
**False positives:** a CONSTANT identifier is acceptable per the 7.10 table ("integer liter. or
constant"). Resolve CONSTANT names first.
**Source:** RM5 7.9 (p.200), 7.10 (p.206); ADV 4.1 (p.61).

## R54 - `MININDEX`/`MAXINDEX`/`IN` cannot take a formal parameter inside a STANDARD routine

Inside a routine declared with the `STANDARD` modifier, these three standard routines **cannot have
one of the routine's own formal parameters** as an actual parameter. "Note that the compiler does
not detect this condition or give any error message."

**Detect:** inside a `ROUTINE STANDARD ...` body, a `MININDEX`/`MAXINDEX`/`IN` whose operand is one
of that routine's parameter names.
**False positives:** none.
**Source:** RM5 Appendix G restriction 5 (p.344); RM5 7.1 (p.178).

---

# Section 4 - Control flow

## R55 - IF

```
IF expr THEN
   stmts
[ELSIF expr THEN
   stmts]...
[ELSE
   stmts]
ENDIF
```
The keyword is `ELSIF`, not `ELSEIF` or `ELIF`. The condition must have a **Boolean** resulting
value. `ENDIF` is mandatory.

**Detect:** `\bELSEIF\b|\bELIF\b` (note: `$ELSIF` is the compiler-command form and is correct with
the dollar). Unbalanced `IF`/`ENDIF` counting per routine.
**False positives:** `ENDIF` also terminates `$IF` conditional compilation - but that form is
`$ENDIF`. Count the two families separately by whether the token starts with `$`.
**Source:** RM5 6.2 (p.155); UG6 p.190.

## R56 - There is no ENDWHILE, and `WHILE cond DO` is not a loop header

**Wrong:**
```
WHILE x > 0 DO
   ...
ENDWHILE
```
**Right:**
```
DO
   ...
   WHILE x > 0
   ...
ENDDO
```

`WHILE` is a **continue-test placed inside** a `DO ... ENDDO` or `FOR ... ENDFOR` loop. When the
condition is TRUE control passes to the statement after the WHILE; when FALSE the loop is left. It
may appear anywhere in the loop body, and there may be **any number** of WHILE statements in one
loop.

**Detect:** `\bENDWHILE\b`; and `\bWHILE\b.*\bDO\s*$`.
**False positives:** none.
**Source:** RM5 6.6 (p.167); UG6 p.177, p.242; CODE `planc-lint.py`.

## R57 - DO ... ENDDO with no exit is an infinite loop

RM5 6.4: "At least one GO statement must be in the group of statements to leave the loop under some
condition. If not the program will contain an infinite loop." UG6 adds the other legitimate exits:
`RETURN`, `ERRETURN`, `ASSERT`, `GO`, a `WHILE` condition, or a call to an exit routine.

**Real cost, CODE:** "a bare loop with no sleep is not an option. When XMSG died under the server,
its receive stopped blocking and the loop burned the machine until somebody pressed ESC."

**Detect:** a `DO ... ENDDO` block containing none of `WHILE`, `GO`, `RETURN`, `ERRETURN` and no
routine call. Report as a warning.
**False positives:** a deliberate event loop that exits by monitor call. Warning only.
**Source:** RM5 6.4 (p.160); UG6 p.177; CODE `CHAT.PLNC`.

## R58 - FOR

```
FOR control-ident IN [REVERSE] list DO
   stmts
   [WHILE expr
    stmts]...
   [EXITWHILE
    stmts]
   [EXITFOR
    stmts]
ENDFOR
```

The **list** may be: explicit integer/enumeration/pointer values; implied ranges `a:b`; one or more
**one-dimensional** arrays; or one or more pointer implied ranges `head:linkfield`.

Rules a linter can check:
- The control identifier's data type must match the list values (ILLEGAL CONTROL IDENTIFIER).
- If the control identifier is a **pointer** and the list contains arrays, only **one** array is
  permitted in the list.
- A pointer control identifier must not be used with a `PACKED` array whose elements are smaller
  than the smallest addressable unit (e.g. `INTEGER1 PACKED` on the ND-100) - "unpredictable
  results".
- Only **one** `EXITFOR` and **one** `EXITWHILE` per loop
  (EXITFOR/EXITWHILE ALREADY PRESENT WITHIN THE LOOP).
- `REVERSE` applies to implied ranges and to arrays. It **may not be used with a pointer implied
  range**. Implied ranges must still be written in ascending order.
- Multi-dimensional arrays are not allowed in the list
  (MULTIDIMENSIONAL ARRAY NOT ALLOWED HERE).

**Detect:** count EXITFOR/EXITWHILE per FOR block; check `REVERSE` against a list containing `:`
between two pointer names; check array dimensionality of names in the list.
**False positives:** telling `a:b` (integer range) from `head:next` (pointer implied range) needs
the declared types. When unknown, skip.
**Source:** RM5 6.5 (p.161-163), 6.6 (p.167); RM5 error messages; UG6 p.184-186.

## R59 - After a FOR loop the control variable is unpredictable

"Upon exit from a FOR - ENDFOR loop, the control identifier will have an unpredictable value. This
applies as soon as the loop exit action begins" - including on entry to the `EXITFOR` block.

The one documented exception: a **pointer implied range** loop leaves the control pointer as `NIL`
if the loop ended by exhausting the list.

**Detect:** a read of the control identifier after `ENDFOR`, or inside an `EXITFOR` block, when the
list was not a pointer implied range. Warning.
**False positives:** the variable may have been re-stored between the ENDFOR and the read.
Only flag the first read.
**Source:** RM5 6.5 (p.161, p.163).

## R60 - FOR list expressions are evaluated once, at loop initialisation

"Expressions are evaluated at runtime within the loop initialization so that modifying identifiers
used in such an expression during execution of the loop will have no effect on the control of the
loop."

But: with an **implied range**, altering the control identifier inside the loop DOES affect loop
control - setting it >= the range's final value terminates that range.

**Detect:** a store into the control identifier inside a FOR body - warning, quoting whichever of
the two behaviours applies.
**Source:** RM5 6.5 (p.162-163).

## R61 - CASE

```
CASE expr
  INCASE value-list
    stmts
  [INCASE value-list
    stmts]...
  [ELSE
    stmts]
ENDCASE
```

Hard constraints:
- `expr` must be an **enumeration of at most 256 values**, or an **integer in 0..255**.
- If the values belong to an `INTEGER RANGE`, **the lower bound of that range must be 0**.
- A value must not appear in more than one INCASE list in the whole CASE
  (INCASE CONTAINS INVALID VALUE).
- If the INCASE lists do not cover every possible value, an `ELSE` **must** be present
  (REQUIRE ELSE OR ALL POSSIBLE VALUES USED IN INCASE PARTS).
- The values in each INCASE must be the same data type as `expr`.
- An INCASE list may be an implied range: `INCASE monday : thursday`.

**Measured, CODE:** a plain `INTEGER` as the CASE selector draws OUT OF RANGE - which is a
**WARNING**, so the build "succeeds" and the CASE misbehaves at run time.

**Detect:** `CASE\s+(\w+)\s*$` where the named variable is not declared
`INTEGER RANGE (0:...)` and is not an enumeration. `planc-lint.py` implements this. Also: collect
INCASE values per CASE block and report duplicates; report a missing ELSE when the selector is a
plain integer.
**False positives:** the selector may be an expression rather than a bare name - the regex only
handles the bare-name case, which is the common one. Enumeration-typed selectors need the type
table.
**Source:** RM5 6.3 (p.158-159); UG6 p.167-168; CODE `planc-lint.py`.

## R62 - GO into a structured block is unpredictable

RM5 6.1: "control transfers **into** structures such as FOR - ENDFOR or DO - WHILE - ENDDO loops
may have unpredictable results."

Loops and IF blocks must nest completely: "statements such as DO - ENDDO and IF - ENDIF must be
entirely contained within the FOR - ENDFOR loop."

**Detect:** a `GO label` where the label is defined inside a loop or IF block that does not contain
the GO. Requires block tracking - straightforward with a bracket-matching pass.
**False positives:** none.
**Source:** RM5 6.1 (p.154), 6.5 (p.163).

## R63 - ASSERT

`ASSERT boolean-expr`. If FALSE, the `ASSERTFALSE` exception is raised. With no
`ON ASSERTFALSE` handler, the program terminates with `ASSERT VIOLATION AT <address>` - the runtime
routine invoked is `5FATA` on ND-100 / `#FATA` on ND-500.

**Detect:** an `ASSERT` in a file with no `ON ASSERTFALSE` handler anywhere - note, not error. A
production server should not carry an ASSERT that can kill it.
**Source:** RM5 6.7 (p.170), 6.8 (p.172); UG6 p.150.

---

# Section 5 - Routines

## R64 - The routine header

```
ROUTINE [modifier...] in-type, out-type [(param-types)] : name [(param-names)] [ALIAS 'x']
    ...body...
ENDROUTINE [name]
```
or the newer combined form:
```
ROUTINE [modifier...] in-type, out-type (type : names; type : names) : name
```

**Both the in-value type and the out-value type are mandatory.** `VOID` denotes absence. There is
no shorthand for "no values" - it is written `ROUTINE VOID,VOID`.

`ENDROUTINE name` (naming the routine at its end) is **[NEWER ONLY]**.

**Detect:** `^\s*ROUTINE\b` without a comma in the header is malformed. `ENDROUTINE\s+\w+` when
targeting version G.
**False positives:** the header may be split across `&` continuation lines - join first. The
`IMPORT ( ROUTINE ... )` form is a declaration, not a definition, and has no body.
**Source:** RM5 7.1 (p.175-176); UG6 p.229-231; ADV 4.1 (p.60).

## R65 - The two parameter-list styles, and the semicolon in the newer one

**Classic (works everywhere):** types in one list, names in another.
```
ROUTINE VOID,VOID (INTEGER,INTEGER,INTEGER WRITE) : simple(in1,in2,outval)
```
The two lists must have the same length, or INVALID PARAMETER LIST.

**[NEWER ONLY]** types and names together, **semicolon-separated groups**:
```
ROUTINE VOID, VOID (INTEGER : a, b, c; BOOLEAN : d, e) : x
```
Note: **commas** separate names within a group, **semicolons** separate groups.

**Detect:** count comma-separated items in the type list against the name list. For the newer
form, flag a comma where a semicolon should separate two `type : names` groups.
**False positives:** a type expression can itself contain a comma inside `RANGE (a:b)`? No - RANGE
uses a colon. But `ARRAY` bounds do not appear in a parameter type. Nesting depth counting is still
the safe way.
**Source:** RM5 7.1 (p.175); UG6 p.154, p.230-231; ADV 4.1 (p.60), 4.2 (p.67).

## R66 - Parameters default to READ

The default access mode for a formal parameter is **READ only**. To return a value through a
parameter you must write `WRITE` or `READ WRITE`.

`READ` on a parameter is therefore redundant (UG6 p.224).

**Detect:** a parameter that is stored into inside the routine body but declared without
`WRITE`. Requires matching parameter names to their type-list position.
**False positives:** a parameter of a composite type (ARRAY, RECORD) is passed **by reference**
(R70), so storing into its components works regardless of the access modifier. Only flag simple
types.
**Source:** RM5 7.1 (p.176), 7.4 (p.190); UG6 p.224, p.243, p.138.

## R67 - A WRITE-only parameter needs an explicit variable, never an expression

**Wrong:** `twice(3+2*5)` where the formal is `INTEGER READ WRITE`
**Right:** `twice(int)` or `twice(3 =: int)`

RM5 7.4: "any invocation of a routine with any WRITE only parameters must have explicit actual
parameter data-elements for such parameters. Expressions are invalid as actual parameters."
The compiler answers INVALID ACTUAL PARAMETER, FORMAL PARAMETER DECLARED AS WRITE.

Also: a WRITE-only (not READ WRITE) formal has an **undefined value** on entry to the routine.

**Detect:** a call whose argument at a WRITE position is not a bare name or a name with subscripts.
**False positives:** `f(3 =: int)` is legal and contains an operator - accept an argument whose
last operation is a store into a variable.
**Source:** RM5 7.4 (p.190-192); RM5 error INVALID ACTUAL PARAMETER.

## R68 - `@` is the in-value, and it cannot be stored to

Inside a routine with a non-VOID in-value, `@` names it. For a composite in-value the component
follows normally: `@.realpart`. In nested routines `@` refers to the **innermost** routine's
in-value.

UG6 p.146: "It can be used in expressions just like any other variable, with the exception that it
cannot be stored to."

**Detect:** `=:\s*@` or `@\s*:=`. Also: `@` used in a routine declared `ROUTINE VOID, ...`.
**False positives:** `@` inside a string literal (SINTRAN command lines contain it constantly -
`@RT CHATSER`). Strip strings.
**Source:** RM5 7.2 (p.180); UG6 p.146.

## R69 - The out-value goes BEFORE `RETURN`

**Wrong:** `RETURN answer`
**Right:** `answer RETURN`

The compiler answers ILLEGAL DATA TYPE "RETURN", which reads as though RETURN itself is the
problem. Same shape for `ERRETURN`: `errorNumber ERRETURN`.

A routine declared with a non-VOID out-value **must contain at least one RETURN**
(ROUTINE WITH AN OUT-VALUE REQUIRES A RETURN), and exit through a bare `ENDROUTINE` is then
illegal. A routine with VOID out-value may fall off the end into `ENDROUTINE`.

`ERRETURN`'s expression must be **INTEGER**, and its value lands in `ERRCODE`.

**Detect:** `^\s*RETURN\s+\S` (after comment stripping) - the value is on the wrong side. Also:
a routine whose out-type is not `VOID` and whose body contains no `RETURN`.
**False positives:** `RETURN  % comment` looks like `RETURN <something>` unless comments are
stripped first, respecting quotes. `planc-lint.py` gets this right.
**Source:** RM5 7.5 (p.193); UG6 p.216, p.182; CODE `planc-lint.py`.

## R70 - Simple parameters go by value, composite by reference

Simple types (INTEGER, REAL, BOOLEAN, ENUMERATION, POINTER) are copied into a temporary local
data-element. Composite types (ARRAY, RECORD; and by extension BYTES, BITS, SET) are passed as an
**address** - so changes made by the callee are visible to the caller immediately, regardless of
access modifiers.

In-values and out-values of composite type are also passed by reference.

**Consequence to flag:** a WRITE modifier on a composite parameter is misleading, and the write-back
timing described in R71 does not apply to it.
**Source:** RM5 7.4 (p.190-192), 7.2 (p.181); UG6 p.138.

## R71 - A WRITE parameter's value is transferred back only on a NORMAL exit

The temporary local is copied back to the actual parameter **after** the routine returns normally.
"Such transfers will not take place if an abnormal routine exit occurs" - i.e. after `ERRETURN`, or
after a `ROUTINEERROR` handler's `ENDON`.

**Consequence:** after an `ON ROUTINEERROR` handler runs, "an out-value data-element or actual
parameter data-elements with WRITE access would contain unpredictable values."

**Measured, CODE:** this is exactly why `CHAT.PLNC` keeps `firstErr` at module level - "a PLANC
local cannot be read after its ON block has run, and the number is the whole point of the attempt."
(Project-measured phrasing; the manual states the parameter/out-value case, not the local case, so
treat the local variant as **project-observed, UNVERIFIED against the manual**.)

**Detect:** a read of a WRITE parameter or of a routine's out-value on a path that can only be
reached after a ROUTINEERROR handler - hard to do statically. More practical: emit a **note**
whenever an `ON ROUTINEERROR` handler stores into a routine-**local** variable that is read after
`ENDON`, recommending module level instead.
**Source:** RM5 7.4 (p.190), 6.8 (p.171), 7.5 (p.194); CODE `CHAT.PLNC`.

## R72 - Parentheses may be omitted for a single parameter

`twice 5` and `twice(5)` are the same. Worth knowing so a linter does not treat a bare
`name expr` as an error.
**Source:** RM5 7.3 (p.183).

## R73 - A bare name on a line of its own must actually BE a routine

**This one shipped a broken client for days** (CODE). `CHAT.PLNC` contained:
```
setMyName(at, argLen)
showMyName                <- no such routine, anywhere in the file
sendJoin
```
and PLANC answered on every build:
```
2494  (1834)/HANDLECOMM  *** ERROR   - ILLEGAL SYNTAX "SHOWMYNAME"
```
Nobody saw it: the compiler prints diagnostics as it goes, on a long source they scroll off a
24-line screen, and the `0 DIAGNOSTICS` left on screen belongs to the **linker**, sitting happily
under a compile that failed. **The listing file is the only place a PLANC error survives.**

A call with no parameters is just the name, so a typo is indistinguishable by eye. This is exactly
what a machine should check.

**Detect:** a code line matching `^\s*([A-Za-z_]\w*)\s*$` whose name is not a statement keyword and
is not declared anywhere in the file.
**False positives:** a line that is an ARGUMENT of a call continued from the previous line with
`&` - skip a line whose predecessor ended in `&`. Statement keywords must be excluded:
`RETURN ENDIF ENDDO ENDFOR ENDON ENDROUTINE ENDMODULE ENDRECORD ENDCASE ENDUSING ELSE DO THEN GO
EXITFOR EXITWHILE`. Labels (`name :`) are not bare names. Names from `$INCLUDE`d files.
**Source:** CODE `planc-lint.py`, `CHAT.PLNC`.

## R74 - PLANC is single-pass: declare or predeclare before use

A routine called above its declaration gets ILLEGAL SYNTAX **on the call**, naming a routine that
plainly exists - which reads as nonsense.

**Right:** either move the declaration up, or predeclare it: `ROUTINE VOID,VOID : rt2?`

**Detect:** for each `ROUTINE`/`PROGRAM` declaration at line N, any use of that name before line N
that is not itself a declaration or a predeclaration.
**False positives:** a name that is both a routine and (say) a record component. `planc-lint.py`
accepts this looseness deliberately.
**Source:** RM5 3.16 (p.98); CODE `planc-lint.py`.

## R75 - Routine modifiers, and what each one breaks

| Modifier | Effect | The trap |
|---|---|---|
| `INLINE` | body expanded at each call site | cannot be declared or invoked inside another INLINE routine (ILLEGAL INLINE INVOCATION); **`ON ROUTINEERROR` does not work correctly inside an INLINE routine**; cannot recurse |
| `SPECIAL` | no entry/exit sequence at all | cannot recurse; a SPECIAL routine that uses stack space draws LOCAL/TEMPORARY VARIABLES REQUIRE STACK SPACE IN |
| `STANDARD` | FORTRAN/COBOL calling sequence | **in-values are not allowed**; `MININDEX`, `MAXINDEX` and `ERRETURN` are not available; array parameters should have lower bound 0 |
| `REFERENCE` | all parameters by address | inner (nested) REFERENCE routines are not executed correctly - outermost level only; array parameters should have lower bound 0 |
| **[NEWER ONLY]** `C`, `NATIVE`, `COBOL`, `FORTRAN`, `PASCAL`, `MAINSTART`, `XARGS`, `PARALLEL`, `DOMAIN` | | see UG6 |

Inner routines declared `STANDARD`, `REFERENCE` or `SPECIAL` "will not be executed correctly".

**Detect:** `ROUTINE\s+(\w+\s+)*STANDARD\b` with a non-VOID in-type; an `ON ROUTINEERROR` inside an
INLINE body; an INLINE call inside an INLINE body; a STANDARD/REFERENCE/SPECIAL routine declared
between another routine's header and its ENDROUTINE.
**False positives:** none.
**Source:** RM5 7.1 (p.178-179), 7.7 (p.195); RM5 Appendix G restrictions 11 and 23 (p.345, p.347);
UG6 p.183, p.215, p.223-224.

## R76 - Recursion is only allowed at the outer level of a module

Direct recursion: a routine may invoke itself **only if it is declared at the outermost level of a
module**. A routine nested inside another routine must not invoke itself.

Indirect recursion is allowed at any nesting level, provided the chain goes via an outer-level
routine.

`SPECIAL` and `INLINE` routines cannot recurse at all.

**Stack overflow from recursive calls never invokes an exception block** (RM5 6.8) - it terminates
the program with `STACK OVERFLOW AT <address>`.

**Detect:** a routine declared inside another routine whose body contains its own name.
**False positives:** a name shadowed by an outer routine of the same name - illegal anyway (R113).
**Source:** RM5 7.7 (p.195-196), 6.8 (p.171); RM5 Appendix G restriction 23i (p.347).

## R113 - An identifier may not be redeclared inside a nested routine

An identifier declared in a routine has the scope of the **entire** routine, and "such identifiers
may not have an identifier name which is identical to an identifier whose scope includes this
routine, i.e. **an identifier may not be declared twice within nested routines**."

**Wrong:**
```
ROUTINE VOID,VOID : outer
    INTEGER : counter
    ROUTINE VOID,VOID : inner
        INTEGER : counter        % illegal - outer's counter is in scope here
    ENDROUTINE
ENDROUTINE
```

**The one documented exception:** an `INLINE` routine MAY use local names that are the same as names
whose scope includes the call site, even though the body is textually inserted there.

Two routines at the **same** level may of course each declare `i` and `j` - those are different
data-elements (RM5 1.10).

**Detect:** track routine nesting; report a declaration in an inner routine whose name (folded to
ten characters, R4) is already declared in an enclosing routine or at module level. Skip routines
declared `INLINE`.
**False positives:** names arriving from `$INCLUDE`. Also record component names, which live in the
record's own scope and are reachable by `USING` - do not treat those as module-level declarations.
**Source:** RM5 7.8 (p.197), 1.10 (p.63).

## R77 - Routines cannot be in-values, out-values or parameters

INVALID TYPE FOR IN-VALUE/OUT-VALUE/PARAMETER: "The data type of a routine in-value, out-value or
parameter must not be a routine. Note that a **pointer** to a routine data-element may be used."

**[NEWER ONLY] exception:** `co_Call`, `co_Detach` and `co_Resume` are "the only ones in PLANC that
have routines as parameters".

**Detect:** `ROUTINE` appearing inside a routine header's in-type, out-type or parameter type list.
**False positives:** `TYPE x = ROUTINE ...` is a type specification, not a header.
**Source:** RM5 error INVALID TYPE FOR ...; UG6 p.218.

## R78 - `ADDR` of a routine takes no parentheses

**Wrong:** `ADDR(myfirst)` where `myfirst` is a routine
**Right:** `ADDR myfirst`

Also: if several routines in a module share a name, `ADDR` returns the address of the **first**
one declared. And `ADDR` of a routine that has an out-value yields the routine's address, not its
out-value.

`ADDR(ADDR(an ARRAY data-element))` does not work; use two statements with an explicit
`ARRAY POINTER`.

**Detect:** `ADDR\s*\(\s*(\w+)\s*\)` where the name is a declared routine. `ADDR\s*\(\s*ADDR\b`.
**False positives:** none.
**Source:** RM5 7.9 ADDR (p.197); RM5 Appendix G restrictions 3, 4, 24 (p.344, p.347).

## R79 - `IND` restrictions

`IND` cannot take a pointer that qualifies a routine **with an in-value**.
Nested routine invocations must not be carried out with `IND`; `IND` may only invoke routines at
the outer level of a module.
An inner-level routine invoked via `IND` "will not be executed correctly".

**Detect:** `IND\s+(\w+)` / `IND\s*\(\s*(\w+)` where the pointer's qualification is a routine type
declared with a non-VOID in-value, or a routine declared inside another routine.
**Source:** RM5 7.1 (p.179), 7.3 (p.183); RM5 Appendix G restrictions 2, 23ii (p.344, p.347).

## R80 - An overload family must differ in TYPES, not just access modifiers

**Wrong:**
```
ROUTINE VOID, VOID (INTEGER)       : RUT?
ROUTINE VOID, VOID (INTEGER WRITE) : RUT?
```
"The compiler cannot distinguish between the two declarations and will give a compile error
message."

Also: the number of parameters must be the same across a family (UG6 p.126: "A new routine that
overloads existing ones is only accepted into the set if the number of variables in the parameter
is the same as in the existing routines").

And: it is illegal to EXPORT a family whose name is the same as a predefined standard routine or
operator.

**Detect:** two routine declarations with the same name whose parameter type lists differ only in
`READ`/`WRITE` tokens; two with the same name and different parameter counts; an `EXPORT` of a name
that is a standard-routine or operator name.
**Source:** RM5 Appendix G restrictions 7 and 9 (p.344-345); RM5 8.3 (p.211); UG6 p.126.

## R81 - Ignoring a routine's out-value is silent

"It is legitimate in an invocation of a user written routine, which is declared with an out-value,
not to store the out-value. The compiler will not give any warning or error messages."

**Detect:** a call to a routine with a non-VOID out-type appearing as a whole statement with no
`=:` and no enclosing expression. Warning at most - it is legal, and sometimes intended.
**Source:** RM5 Appendix G restriction 17 (p.345).

## R82 - `i =: rtn =: j` does not store the routine's out-value

If a routine has both an in-value and an out-value and is invoked with an assignment operator
immediately before AND after it, "then the value of `i` will be the value stored in `j`, **not** the
out-value of the routine invocation."

**Detect:** `=:\s*(\w+)\s*=:` where the middle name is a declared routine. Warning.
**Source:** RM5 7.3 (p.183).

---

# Section 6 - Module and program structure

## R83 - MODULE

```
MODULE name
   [EXPORT ...]
   [IMPORT ...]
   [TYPE ...] [CONSTANT ...]
   declarations, routines, PROGRAM
ENDMODULE
```

- **`EXPORT` statements must be placed immediately following the MODULE statement**, before all
  other declarations. TYPE specifications and IMPORT statements may precede an EXPORT.
- Only identifiers **global in the module** may be EXPORTed
  (IDENTIFIER IN EXPORT, BUT NO DECLARATION).
- A module must be terminated by `ENDMODULE` (ILLEGAL MODULE TERMINATION).
- Module nesting is limited (TOO MANY LEVELS OF MODULE NESTING; UG6 says 16).
- If modules are nested, **routines and executable code may only be in the innermost module** -
  except that two separate nests inside one outer module may each have code in their innermost.
- `TYPE` and `CONSTANT` statements may appear **outside/before any module**, in which case their
  identifiers are global to all modules in the compilation and need no IMPORT.

**Detect:** an `EXPORT` line appearing after a data declaration inside the same module. An
`ENDMODULE` count that does not match `MODULE`. A `ROUTINE`/`PROGRAM` in a module that contains a
nested `MODULE`.
**False positives:** `MODULE` inside a string or comment.
**Source:** RM5 8.1 (p.208-209), 8.3 (p.211), 8.5 (p.220-221), 8.7 (p.223); UG6 p.206-208.

## R84 - EXPORT / IMPORT

`EXPORT [(SYSTEM)] ident[,ident]...`
`IMPORT [(SYSTEM)|(COMMON)] declaration[,declaration]...`

- If `(SYSTEM)` is on the EXPORT it **must** also be on the matching IMPORT, and vice versa.
- `(COMMON)` links to a named FORTRAN COMMON block.
- If declarations of **different data types** go in one IMPORT, **each must be in parentheses**:
  `IMPORT (INTEGER : i1,i2),(REAL : r1,r2),(BOOLEAN : b1)`
- **A routine's IMPORT declaration must be in parentheses**, and **must not include the formal
  parameter names**:
  `IMPORT ( ROUTINE VOID,VOID (INTEGER) : doit )`
- The IMPORT declaration must match the exporting module's declaration
  (CONFLICTING DATA TYPES IN CORRESPONDING IMPORT/EXPORT). This is only checked at compile time
  when the two modules are nested inside a common outer module; **separately compiled modules are
  not checked at all**.
- An identifier must be IMPORTed at **every level** between the declaring module and the using one.
- An identifier created in an outer module by an IMPORT needs an identical IMPORT in the nested
  module.
- TYPE and CONSTANT identifiers are IMPORTed into a nested module **without** any EXPORT in the
  outer module.

**Detect:** `IMPORT\s*\(\s*ROUTINE[^)]*\)\s*\(` - parameter names left in. An IMPORT of a routine
without surrounding parentheses. An IMPORT list mixing types without per-item parentheses.
**False positives:** parentheses inside the parameter type list confuse a naive matcher - count
depth.
**Source:** RM5 8.3 (p.211-213), 8.5 (p.220), 8.7 (p.223); UG6 p.178-180, p.183-184.

## R85 - PROGRAM

```
PROGRAM : name
   declarations
   INISTACK stackarray
   statements
ENDROUTINE
```

- The header is exactly `PROGRAM : name` - **no in-value, out-value or parameters**.
- It is terminated by `ENDROUTINE`, not by `ENDPROGRAM`.
- **Exactly one** PROGRAM (or `ROUTINE MAINSTART` **[NEWER ONLY]**) per executable program.
- A name used in a routine predeclaration must not later appear as a PROGRAM identifier (R36).

**Detect:** `\bENDPROGRAM\b` is not a PLANC keyword. `PROGRAM\s*:` followed by parentheses or a
comma. More than one `PROGRAM :` in a compilation unit.
**Source:** RM5 8.2 (p.210); UG6 p.209, p.207.

## R86 - INISTACK is mandatory and its array is constrained

`INISTACK int-array`

The array must be:
- of type **INTEGER** - not a subtype, not INTEGER2/INTEGER4 (UG6 p.195)
- **one-dimensional**
- with **lower index bound zero**
- **global or imported** - not a routine local
- not a subrange (UG6 p.217)

"However simple a program may be, the INISTACK standard routine must appear in the main program
before any other routines are called."

The compiler messages are INISTACK INVOCATION MISSING and
INVALID ARRAY FOR INISTACK INVOCATION.

**A second INISTACK inside a routine** switches to a new stack for that routine and everything it
calls, and reverts on RETURN. That is a real feature, not an error.

**Beware the OCR:** RM5 and UG6 both print this word as `INISTACK`, `INITSTACK`, `INSTACK` and
`INVSTACK` in different examples. **`INISTACK` is the correct spelling** - it is the one in the
keyword/standard-routine tables (RM5 2.3 p.68, 7.9 p.199) and the one that compiles (CODE).

**Detect:** a `PROGRAM :` block with no `INISTACK`. An `INISTACK` whose array is declared with a
non-zero lower bound, more than one dimension, a non-INTEGER type, or inside a routine. Any of the
misspellings `INITSTACK|INSTACK|INVSTACK`.
**False positives:** none.
**Source:** RM5 1.2 (p.51), 8.6 (p.222), 7.9 (p.199); RM5 error messages; UG6 p.194-195, p.217;
CODE.

## R87 - Static vs dynamic allocation decides what can be initialised

**Statically allocated:** global data-elements in a basic MODULE; locals declared **READ only**;
data made by `NEW` inside a global data-element.
**Dynamically allocated (on the stack):** locals whose access is not READ only; data made by `NEW`
on the stack or inside a local.

"A static data-element may be initialized with a specific value, in its declaration, **provided that
it is not within a nested routine**. Static data-elements may be initialized within a nested routine
if it is declared as READ only."

**All data-elements in a routine's local data area, including dynamically created ones, are lost
when the routine exits.**

**Detect:** this is the mechanism behind R20 - same check.
**Source:** RM5 8.6 (p.221-222), 4.5 (p.130).

## R88 - `$INCLUDE` placement

**Wrong:** `$INCLUDE` above the `MODULE` line, when the included file declares things the module
body needs.

**Measured, CODE:** placed above MODULE the declarations are compiled into the outer scope, the
module body cannot see them, and every call fails with `NOT PREVIOUSLY DECLARED "XMPFOPN"` while
**the include's own lines draw no diagnostic at all** - so the declarations look accepted and are
simply not there. 33 diagnostics, none of them on the include.

**Right:** `$INCLUDE` after the `MODULE` statement.

(TYPE and CONSTANT declarations are the exception - RM5 8.5 explicitly recommends `$INCLUDE`ing a
TYPE-only file outside all modules so its types are global. So the rule is about *declarations*,
not about *include position* in general.)

**Detect:** a `$INCLUDE` line before the first `MODULE` line. `planc-lint.py` implements this.
**False positives:** a genuine TYPE/CONSTANT-only include. Downgrade to a warning naming the file.
**Source:** CODE `CHAT.PLNC`, `planc-lint.py`; RM5 8.5 (p.220), Appendix A 0.8 (p.252).

## R89 - `$INCLUDE` nesting limit and `$EOF`

No more than **16 incomplete `$INCLUDE`s** at any one time.
`$EOF` ends the current file and returns to the enclosing include level.
`$IF ... $ENDIF` groups inside included text must be complete before the include ends.
`$IF` may nest to **11** levels.

**`$EOF` in a MAIN source ends the compiler SESSION** - the commands after COMPILE then go to
SINTRAN instead of to the compiler.

**Detect:** a `$EOF` in a file that contains a `MODULE` statement (i.e. is a main source rather
than an include). `planc-lint.py` implements this.
**False positives:** a file that is both - unusual. Warning.
**Source:** RM5 Appendix A 0.6, 0.8, 0.10 (p.250, p.252, p.254); UG6 p.181, p.189; CODE
`planc-lint.py`.

## R90 - Compiler commands that must live outside the outermost module

`$SEPARATE-DATA` and `$DEBUG-MODE` must be used **outside the outermost module level**.
The compiler message is COMMAND NOT PERMITTED WITHIN A MODULE.

`$COMPILE`'s source-file parameter must be separated from the command by **at least one space**.

**Detect:** `\$(SEPARATE-DATA|DEBUG-MODE)` between a `MODULE` and its matching `ENDMODULE`.
**Source:** RM5 Appendix G restrictions 10, 13 (p.345); RM5 error COMMAND NOT PERMITTED WITHIN A
MODULE.

## R91 - Macro rules

```
$MACRO macname [(param[,param]...)]
    body, referring to parameters as "param"
$ENDMACRO
```
- A formal parameter is referenced inside the body by enclosing its name in **double quotes**.
- **The double quote may not be used for any other purpose inside a macro body**
  (ILLEGAL PARAMETER REFERENCE IN MACRO BODY).
- Macro definitions **must not nest** (ILLEGAL NESTED MACRO DEFINITION). Macro *invocations* may
  nest, and may recurse.
- An actual parameter may be any text **except** comma, right parenthesis and double quote; to
  include a comma or right parenthesis the whole actual parameter must be wrapped in double quotes.
- A macro formal parameter name must not conflict with a previous declaration
  (ILLEGAL FORMAL PARAMETER IN MACRO).
- If `$ENDIF` is used as a parameter in a macro call it must be followed by at least one space.

**Detect:** `$MACRO` inside an unterminated `$MACRO`; a `"` in a macro body that is not a matched
parameter reference; a macro actual parameter containing a bare comma or `)`.
**Source:** RM5 Appendix A 0.11 (p.255-256), Appendix G restriction 19 (p.346); UG6 p.190, p.202-203.

---

# Section 7 - Input and output

## R92 - One data-element per call

"only one data-element may be input/output by a single input/output standard routine invocation."

`INPUT (file-number, 'descriptor', identifier)`
`OUTPUT (file-number, 'descriptor', identifier)`

Random unformatted forms take `(file-number, record-number, bytes-array)`; the first record is
number **0**.

**Detect:** four or more arguments to `INPUT`/`OUTPUT` where the second is a quoted descriptor.
**Source:** RM5 9 (p.224), 9.2 (p.226), 9.3 (p.233), 9.2.7, 9.3.8.

## R93 - The format descriptors, and exactly what the width counts

| Descriptor | Meaning | Direction |
|---|---|---|
| `Iw` | decimal integer, field of w characters | in, out |
| `Ow` | octal integer, field of w | in, out |
| `Zw` | octal integer with leading zeroes | **out only** |
| `Fw.d` | fixed point real; w **includes** the decimal point and any minus sign; d decimals | in, out |
| `Ew.d` | normalised with exponent; w includes point, and the exponent is `E`, a sign and **two** digits | in, out |
| `Dw.d` | as E, for double-precision reals | **out only** |
| `Aw` | alphanumeric, **right**-justified, leading blanks | in, out |
| `ALw` | alphanumeric, **left**-justified, trailing blanks | **out only** |
| `Lw` | Boolean, `T`/`F` in the rightmost position | in, out |

`w` is an unsigned integer **greater than zero**. `d` is an unsigned integer **>= 0**.

Omitting `w` or `w.d` is legal: on output the compiler uses the minimum needed (UG6: default for
the target CPU) and you read the out-value to learn how many bytes went. On input it reads a
type-dependent default maximum.

**Width counting - the rule that is actually violated:**
- `w` is a **FIELD WIDTH, not a maximum.** Too small silently cuts the line off; too large pads it.
- **On output of a numeric value that does not fit, the whole field is filled with asterisks (`*`).**
- **The count INCLUDES the trailing `$`** (the newline character), because `$` is a character in the
  string like any other. `'CHAT: bye$'` is **TEN** characters.
- Nothing in the compiler checks the width against the literal. A wrong width builds clean and shows
  up only on a terminal nobody may be watching.

**Detect:** for `OUTPUT(dev, 'AL<n>', '<literal>')` compute `len(literal)` with `''` counted as one
character, and compare with `n`. Report both directions:
- `n < len` - the line is truncated (and for numeric formats, asterisks);
- `n > len` - the padding lands **after** the newline the `$` emitted, so it indents the NEXT line.
  A `/help` built from over-wide fields came out as a staircase (CODE).

`planc-lint.py` implements both. Regex:
`"'AL(\d+)'\s*,\s*'((?:[^']|'')*)'"`.
**False positives:** a variable rather than a literal in the third argument cannot be checked.
Descriptors built at runtime cannot be checked. Leaving the width off entirely (`'AL'`) is the
safer habit and must not be flagged.
**Source:** RM5 9.2 (p.227), 9.3 (p.233-239); UG6 p.196-197, p.214-215; CODE `planc-lint.py`,
PLANC-DEVELOPER-GUIDE.md.

## R94 - `$` inside an OUTPUT string is CR+LF

A single `$` in a string being output becomes carriage return + line feed. To print a literal `$`,
write `$$`.

**Detect:** an odd-length run of `$` in a literal handed to `OUTPUT` is intentional (a newline); a
`$` in a literal handed to something that is **not** OUTPUT is probably a mistake - the character
is only special to `Output`.
**False positives:** SINTRAN command strings passed to `MON70` contain no `$` normally, but a file
name could. Warning only.
**Source:** RM5 9.3.6 (p.238); UG6 p.133, p.145.

## R95 - Input field termination and parity

A field being read by a formatted INPUT terminates when the maximum `w` characters have been read,
**or** at a comma, **or** at a carriage return. Leading blanks count toward the field width but do
not affect the value.

For `Lw`, the field is scanned for the first `T` or `F`; anything else yields FALSE; and the
out-value is the character **position (relative to 1)** where the letter was found - not a byte
count.

**THE PARITY TRAP (CODE, and the Monitor Calls manual warns of it):** terminal input sets bit 7 as
an even-parity bit, so a carriage return arrives as **141**, not 13. Compare against 13 and the
program never sees the user press return, and looks hung rather than wrong. **Mask every byte read
from a terminal with 127 before looking at it.**

**Detect:** a comparison of a byte read from a device against a bare ASCII control value (13, 10,
27, 3) with no `AND 127` on the same or a preceding line. Warning.
**False positives:** the mask may be applied further up the call chain. Warning only.
**Source:** RM5 9.2 (p.226), 9.2.6 (p.231); CODE `CHAT.PLNC`.

## R96 - OPEN, CLOSE, BLOCKSIZE, FILESIZE

`OPEN (file-number, file-access, file-name, file-type)` - `file-number` is
`INTEGER READ WRITE` and is **returned**, not supplied. Default file type is `SYMB`.

Legal access codes: `R`, `W`, `RW`, `WA`, `RX`, `WX`, `RC`, `WC`, `D`, `DC`.

`CLOSE (file-number)`.
`int BLOCKSIZE (file-number)` - blocksize in **bytes**, must be >= 1, file must already be open.
`int FILESIZE (file-number)` sets the size in bytes (`int` must be `INTEGER4`);
`FILESIZE(file-number) =: int` reads it.

**Detect:** an access code string handed to `OPEN` that is not in the list.
**False positives:** a variable access code.
**Source:** RM5 9.4-9.7 (p.240-242); UG6 p.213, p.164, p.184.

## R97 - I/O errors arrive as ROUTINEERROR

"The ROUTINEERROR exception will be activated by errors in **any** of the input/output or
open/close standard routines. If a ROUTINEERROR condition occurs, the system variable `ERRCODE`
will contain a value from the file system."

**Detect:** an `OPEN`/`INPUT`/`OUTPUT`/`CLOSE` in a routine with no textually preceding
`ON ROUTINEERROR` handler (R99). Warning.
**Source:** RM5 9 (p.224).

---

# Section 8 - Arrays, bounds and dynamic data

## R98 - There is no array bounds checking

Off by default. `$OPTION ARRAY-INDEX-CHECK ON` turns on a compile-time or run-time check; the
initial value is **OFF**.

Two restrictions on the check itself:
- If a subarray is used with bounds **outside** those declared for the original array, the compiler
  gives no warning, and after such a reference the run-time checking is carried out **incorrectly**.
- RANGEERROR - the exception the check would raise - **is not implemented** (RM5 6.8; UG6 p.223).

**Real cost, CODE:** "PLANC does not check array bounds: without the test, an 81st character is
written past the end of this array and into whatever the compiler laid out next - which here is
roomName, typedLen, serverMagic and joined. A user holding a key down would quietly corrupt the
program's own state." And separately, a `BYTES : roomName := 'CHAT-LOBBY'` (ten bytes) accepting a
sixteen-character `/join` argument wrote past the end - a live defect, not tidying.

**Detect:**
1. Constant subscripts outside the declared bounds (R13) - always report.
2. A copy loop `FOR i IN 1:n DO ... =: arr(i) ...` where `n` comes from a parameter or an external
   source and no clamp against the array's declared size appears in the routine. Warning.
3. A `BYTES` declared by an initialiser literal (so its length is the literal's length) that is
   later written by a loop or a subarray store. Warning: give it an explicit range instead.
**False positives:** rule 2 is heuristic and will fire on safe code. Make it opt-in.
**Source:** RM5 4.1.5, 6.8 (p.171), Appendix A 0.14 (p.259), Appendix G restriction 22 (p.346);
UG6 p.162, p.223; CODE `CHAT.PLNC`.

## R99 - Subarrays

`array(lo:hi)` and, for multi-dimensional arrays, omitting trailing dimensions
(`twod(10)` == `twod(10:10, 1:100)`).

- Each subarray index set must be a **subset** of the corresponding original index set.
- **Subarray bounds may be variables** - `name(0:len-1)` is legal and is how you pass part of a
  buffer to a routine. Without one, a routine taking `BYTES` receives the array's whole declared
  length, leftovers included (CODE).
- `twod(10)(2)` works but `twod(10,2)` "gives much faster access at runtime".
- `ADDR(subarray)` builds a real array descriptor, storable in an `ARRAY POINTER`.
- Subarrays of fewer dimensions than the whole array cannot be READ/WRITE modified (UG6 p.223).

**Detect:** `\w\)\s*\(` - the slow two-step subscript form. Note, not error.
**Source:** RM5 4.1.6 (p.108); UG6 p.161; CODE.

## R100 - The BYTES-ARRAY last-subscript exception

For a `BYTES` array of more than one dimension, the **last subscript may be omitted** and the
reference is then to the entire string:
```
BYTES ARRAY : b1(1:2,0:3) := ('abc','xyz')
BYTES : b2(1:3)
b1(1) =: b2                % stores 'abc'
```
"Note that there are certain restrictions on the ND-100 concerning the last dimension. It is imposed
by hardware" (RM5 p.105, pointing at p.235).

**Detect:** nothing to flag; a linter counting subscripts (R23) must allow one fewer for a BYTES
array or it will report every legitimate use.
**Source:** RM5 4.1.3 (p.105), 4.2.3 (p.117).

## R101 - PACKED arrays on the ND-100

- A PACKED array of two-byte integer subtypes **must not have a negative lower bound** in any index
  range (also: NEGATIVE BOUND ILLEGAL, ND-100 only).
- The address computation "demands that the declared lower index bounds must result in the first
  element of a packed integer array being placed on an odd byte". For more than one dimension:
  make the lower bound of the last dimension and the number of values in that index set a multiple
  of the number of elements per word.
- A pointer must not be used as a FOR control identifier over a PACKED array whose elements are
  smaller than the smallest addressable unit (R58).
- `ADDR` used to **write** into a component of a `RECORD PACKED`/`ARRAY PACKED` whose size or
  alignment differs from an addressable element "may overwrite adjacent memory areas" - and since
  `STANDARD`/`REFERENCE` parameters implicitly use `ADDR`, the same trouble arises there.

**Detect:** a `PACKED` array declaration with a negative lower bound. A `STANDARD`/`REFERENCE`
routine taking a component of a packed structure.
**Source:** RM5 4.2.5 (p.120), Appendix G restrictions 21, 22 (p.346); UG6 p.161, p.216;
ADV 4.3 (p.68).

## R102 - `NEW` and `DISPOSE`

```
NEW data-type [IN int-array] =: pointer
NEW (array-type (index-set[,index-set]...)) [IN int-array] =: pointer
DISPOSE pointer
```

- `NEW data-type` may not be an array type - arrays use the second form with explicit index sets.
- Without `IN`, the object goes on the **stack** and is **lost when the routine exits**.
- **Before the first `NEW` into a given array, the storage-management area must be initialised:**
  `0 =: arr(MININDEX(arr,1))`. This is easy to forget and the manual states it as a Note.
- Per-array overhead: free area pointer 1 word, maximum area pointer 1 word, storage management
  area 15 words; plus **2 extra words per NEW**.
- `DISPOSE`d space is reused only by a request of **exactly the same size**. There is no garbage
  collection or compaction.
- `NEW` raises `POINTERERROR` if the target area is too small - but see R106 for the caveats.

**Detect:** a `NEW ... IN arr` where no `0 =: arr(MININDEX(arr,1))` (or equivalent) appears earlier
in the file. A `NEW` with no `IN` whose pointer is stored into a module-level variable - the object
dies at routine exit.
**False positives:** the initialisation may be written differently (`0 =: arr(0)`). Accept any store
of 0 into element MININDEX of that array.
**Source:** RM5 4.5 (p.129-131), 7.9 NEW (p.201); UG6 p.197, p.209-210.

## R103 - `INSERT`, `APPEND`, `REMOVE` on linked lists report nothing when they fail

```
rec-pointer INSERT listhead:linkfield
rec-pointer APPEND listhead:linkfield
rec-pointer REMOVE listhead:linkfield
```

RM5 4.6: these "will not give any error indication if the record pointer in the routine invocation
is empty (i.e. has value NIL). This also applies to REMOVE if the linked list is empty. Take care to
remember that if INSERT or APPEND is used on a record that is already in a linked list, there is no
error indication, but **the address link field will be overwritten**."

`APPEND` requires the list's last link pointer to be `NIL`.
UG6 p.216 says `Remove` raises `POINTERERROR` when the element is not in the list - which
**contradicts** RM5's "no error indication". Version difference or a manual error; treat as
**UNVERIFIED** and do not rely on either.

**Detect:** an `INSERT`/`APPEND` whose in-value pointer is not tested against `NIL` on any path
reaching it. Warning at most.
**Source:** RM5 4.6 (p.132-134); UG6 p.148, p.185, p.216.

---

# Section 9 - Exception and error handling

## R104 - The handler shape and where it must sit

```
ON exception[,exception]... DO
   stmts
ENDON
```

- Exception conditions: `ASSERTFALSE`, `OVERFLOW`, `POINTERERROR`, `RANGEERROR`, `ROUTINEERROR`,
  `STACKERROR`. Nothing else is a legal name here.
- **A handler is only in effect for source code that FOLLOWS it**, within a routine.
- If several handlers for one condition appear in a routine, **the one immediately preceding the
  occurrence in the source** is the one that runs.
- `ASSERTFALSE`, `OVERFLOW` and `POINTERERROR` are handled **only in the routine where the condition
  is raised**.
- `ROUTINEERROR` and `STACKERROR` are handled in the raising routine **or anywhere up its call
  hierarchy**.
- `ENDON` behaves like a `RETURN` of the failed call: control resumes at the statement after the one
  that raised the condition (or after the failed routine call).

**Detect:** an `ON` naming a condition not in the list. An `ON ... DO` with no matching `ENDON`
(MISSING KEYWORD ... ENDON). A call that can raise (any monitor call, any I/O call, any routine
using ERRETURN) with no handler textually above it in the same routine.
**False positives:** a handler declared in a caller covers ROUTINEERROR - so only flag the
per-routine conditions strictly.
**Source:** RM5 6.8 (p.170-171); UG6 p.199, p.211-212.

## R105 - Which exceptions actually work

| Condition | Status per RM5 6.8 (version G) |
|---|---|
| `ASSERTFALSE` | works |
| `OVERFLOW` | works, but **only hardware raises it**; and `ON OVERFLOW` "does not detect overflow conditions for unsigned integer data-elements" |
| `ROUTINEERROR` | works |
| `POINTERERROR` | **not implemented** for the documented case (a NIL dereference). It IS raised by `NEW ... IN` when the space is inadequate, and "only on overflow from NEW in a special area" |
| `RANGEERROR` | **not implemented** |
| `STACKERROR` | **not implemented**. And: "Stack overflow from recursive routine calls will **never** invoke exception blocks." |

RM5 6.8 also records: "ROUTINEERROR blocks will be invoked from NEW, but will receive error code
**-1** if the allocation was in the stack, and **0** if the allocation was in another area."

The system routine that handles stack overflow can be replaced by a user routine with its own
stack, whose ALIAS name must be `5STCO`.

**Detect:** an `ON RANGEERROR` or `ON STACKERROR` - report that it will never fire (UG6 says the
compiler warns for RANGEERROR). An `ON POINTERERROR` used to guard a NIL dereference - report that
it does not catch that.
**Source:** RM5 6.8 (p.171); RM5 Appendix G restriction 6 (p.344); UG6 p.211-212, p.223, p.224.

## R106 - `ERRETURN`, `ERRCODE` and the fall-through

`expression ERRETURN` where the expression is INTEGER. The value lands in the system variable
`ERRCODE`.

If the caller has an `ON ROUTINEERROR` textually before the call, control goes there. Otherwise
control goes to the **next higher level**, and so on, until a handler is found or the outer level is
reached, at which point the program terminates with the runtime message
`NO ON ROUTINEERROR HANDLER, ERRETURN`.

"If a ROUTINEERROR exception occurs and no exception handler has been provided, an ERRETURN exit
from the routine will be **simulated**" - i.e. the current routine also fails.

After a handler runs, control goes on in one of three ways: `ENDON` (acts as if the failed call had
RETURNed), a `GO` to a label, or a `RETURN`/`ERRETURN` from the routine containing the handler.

**Detect:** `ERRETURN` inside a `ROUTINE STANDARD ...` - not available (R75). `ERRETURN` with a
non-integer expression. Reading `ERRCODE` without an `ON ROUTINEERROR` above it.
**Source:** RM5 7.5 (p.193-194), 6.8 (p.171), Appendix B runtime messages (p.272); UG6 p.170,
p.182, p.220.

## R107 - Do not print from inside a handler that wraps a loop

**Measured, CODE (`CHATSV.PLNC`):** "ONE FLAG FOR THE WHOLE LINE, not a message per byte. CHAT.PLNC
learned that the hard way: a handler round a write loop that prints inside it says the same thing
once per character." Because `ENDON` resumes at the statement after the failing one, a handler
wrapping a loop runs on **every** iteration.

**Right shape:**
```
ON ROUTINEERROR DO
    TRUE =: writeFailed
ENDON
FALSE =: writeFailed
FOR i IN 1:n DO
    ... the call that can fail ...
ENDFOR
IF writeFailed THEN ... ENDIF
```
Note the ordering: the handler is installed **before** the flag is cleared, because it must
textually precede the failing statement (R104).

**Detect:** an `ON ... DO` block containing an `OUTPUT` call, where the guarded region (the code
between `ENDON` and the end of the routine) contains a loop. Warning.
**Source:** CODE `CHATSV.PLNC`, `CHAT.PLNC`.

---

# Section 10 - Rules that came from this machine, not from a manual

These have all cost a real build cycle on D100. They are **measured**, and several contradict or
extend what the manuals say. `planc-lint.py` already implements most of them.

## R108 - A string literal cannot be stored into an element of a BYTES array

**Wrong:** `' ' =: buf(i)`
`'x'` is a **BYTES**; `buf(i)` is a **BYTE**.

The compiler answers:
```
1082   (422)/BUILDWHO  *** ERROR   - ILLEGAL DATA TYPE "OUTBUF"
```
naming the **ARRAY**, not the literal - which sends you to a declaration where nothing is wrong.

**Right:** hold the character in a one-element `BYTES` and copy element to element. The same idiom
is needed in the other direction to PRINT one byte, since `'ALn'` formats a **string** and handing
it a byte prints the byte's NUMBER. (In `CHAT.PLNC` that turned `RONNY` into `8279787889` on
screen.)

**Detect:** `'(?:[^']|'')*'\s*=:\s*([A-Za-z_]\w*)\s*\(`.
**False positives:** storing a string into a **subarray** - `'abc' =: magic(10:12)` - is legal and
correct. Distinguish by whether the subscript contains a `:`.
**Source:** CODE `planc-lint.py`, `CHAT.PLNC`.

## R109 - `#c` (a BYTE) versus `'c'` (a BYTES) - the same rule from the other side

`#a` is a BYTE. `'a'` is a one-element BYTES. RM5 2.10 states it flatly: "Note that `'a'` is not
equivalent to `#a` and has a different internal representation."

**Detect:** a `#c` literal stored into a declared `BYTES` variable, or a `'c'` literal stored into a
declared `BYTE` variable.
**Source:** RM5 2.9, 2.10 (p.76); CODE.

## R110 - Read the LISTING, because errors do not survive on screen

Not a source rule, but the reason the linter exists, and worth emitting as advice in any report:

The compiler prints diagnostics as it goes. On a long source they scroll off a 24-line terminal.
The `0 DIAGNOSTICS` left on the screen at the end belongs to the **BRF-LINKER**, and sits happily
under a compile that failed. **BRF-LINKER writes a runnable `:PROG` with entries still undefined**;
`@CHAT` then starts it and it behaves oddly in a way that looks like evidence about something else.

Two consequences for a build script:
- capture and read `<name>:LIST`;
- run `LIST-ENTRIES-UNDEFINED` after linking and read its output - nothing else fails.

**Source:** CODE `CHAT.PLNC`, `planc-lint.py`.

## R111 - A local filespec in a SINTRAN command string must NOT be quoted

The two routes to "make me a file" take **opposite** quoting:

| Route | Quoting |
|---|---|
| `MON50(file, ...)` | quoting the name means CREATE IT IF ABSENT |
| `'CREATE-FILE <name>'` via `MON70` | quoting the name is ILLEGAL CHARACTER IN PARAMETER |

so a quoted name copied from the first into the second fails every time. Measured on D100:
`'CREATE-FILE "CHAT:CNFG",1'` answered ILLEGAL CHARACTER IN PARAMETER, the file was never created,
and every nickname save had been failing **silently**.

Quoting IS right for a **remote** filespec, which goes to another machine's command processor.

**Detect:** inside a string literal, `(CREATE-FILE|DELETE-FILE|RENAME-FILE)\s+"` where the name has
no `(system)` prefix.
**False positives:** remote filespecs - hence the prefix test.
**Source:** CODE `planc-lint.py`.

## R112 - Source files must have CRLF line endings and EVEN PARITY

A SINTRAN text file needs bit 7 set as an even-parity bit. Without it `LIST-FILE` shows **nothing**,
silently, and QED reports PARITY ERROR.

**But do not "fix" a log file for this reason.** `CHATSV.PLNC` records a full control run: the same
two commands against a file that is unquestionably good answer **exactly the same way**. Both tools
behave that way toward every file the project's FA push puts on the machine. The parity theory was
tested and found to be a wrong conclusion from a missing control.

**Detect:** file-level check on line endings. Report parity as informational only.
**Source:** PLANC-DEVELOPER-GUIDE.md; CODE `CHATSV.PLNC` (which retracts the parity conclusion).

---

## R114 - Two EXPORTs that agree in their first SEVEN characters are ONE name

Names are unique in **ten** characters to the compiler but only **seven** across an
`EXPORT`/`IMPORT`, because seven is what a BRF entry carries (ND-60.117.5 appendix G item 27).

**What makes this dangerous is what the linker does NOT do.** It does not report a duplicate. It
resolves every matching import to whichever entry it met first, so calls meant for one routine
land in the other - and if the two have different signatures, the callee reads arguments that
were never passed. The compile is clean, the link is clean, and **`LIST-ENTRIES-UNDEFINED` is
empty, because nothing is undefined.** There is no message anywhere.

```planc
EXPORT cmTxLen                  % CMTXLEN
EXPORT cmTxLenN                 % CMTXLEN  <- the same name to the linker
```

The compiler only says `IDENTIFIER ALREADY SPECIFIED/DECLARED` when the collision is inside ONE
module at ten characters. Across modules at seven, it says nothing at all.

**Detect:** compare the first seven characters, upper-cased, of every `EXPORT` in a module and
refuse any pair that matches. Do the same across modules that are linked together.
**Source:** ND-60.117.5 appendix G item 27; MEASURED 2026-08-27 - caught by hand while adding a
routine beside one it collided with, which is why it is a linter check now.

---

## R115 - `MAXINDEX` works on an array PARAMETER, and on a SUBARRAY

A routine can ask an array it was PASSED how big it is. MEASURED on D100 2026-08-27, all three:

```planc
ROUTINE VOID, INTEGER (BYTES) : askSize(a)
    MAXINDEX(a, 1) RETURN
ENDROUTINE

askSize(buf)        -> 255      % BYTES : buf(0:255)  - a whole array parameter
askSize(nm)         -> 15       % BYTES : nm(0:15)    - it sees the SMALLER one
askSize(tx(0:9))    -> 9        % a SUBARRAY, bounds and all
```

**Why it matters:** it removes a whole class of caller-trust bug. A routine told a size by its
caller - `writeInto(buf, ..., bufMax)` - has no way to check that number, and PLANC checks no
array bound, so a caller that passes the wrong one gets a silent overflow. `MAXINDEX(buf, 1) + 1`
is the real size and cannot be lied about.

A `BYTES` parameter subscripts from **zero**, so the usable length is `MAXINDEX(a, 1) + 1`.

**The one restriction** (ND-60.117.5 p.249): `MININDEX`/`MAXINDEX` on array parameters are NOT
available in a `STANDARD` routine - the FORTRAN/COBOL calling sequence. An ordinary PLANC routine
is fine.

**Detect:** partly a lint rule now - see R121, which refuses a hand-counted length beside a
literal. The rest is a design rule: prefer asking the array over believing a size.
**Source:** ND-60.117.5 3.17 p.52, p.153, p.249; MEASURED on D100 2026-08-27.

---

## R116 - A clamp that bounds only ONE field is not a clamp

When several fields are written into one buffer, every one of them has to be bounded, not just
the last. The usual shape of the bug is a clamp that reduces the FINAL field to make things fit
while an earlier field has already been allowed to fill the buffer on its own:

```planc
IF headerLen + bodyLen > bufMax THEN
    bufMax - headerLen =: bodyLen      % only the BODY is reduced
    IF bodyLen < 0 THEN 0 =: bodyLen ENDIF
ENDIF
...
FOR i IN 1:headerLen DO ... ENDFOR     % still writes headerLen bytes
```

With `headerLen` larger than `bufMax`, the body clamps to nothing and the header writes past the
end regardless. Nothing reports it - see R24, PLANC checks no array bound - and the overflow is
usually a few bytes, which corrupts whatever the compiler happened to place next.

**Clamp each field against what is left after the ones before it**, and treat a comment that
claims a routine is bounded as a claim to be checked, not a fact.

**Detect:** for each field written into a buffer, is there a bound derived from the buffer size?
**Source:** MEASURED 2026-08-27 - a name field written into a 256-byte buffer put three bytes
past the end while the text field was correctly clamped to nothing.

---

## R117 - A test that overflows an array can PASS

This is the testing consequence of R24, and it is worth its own rule because it makes a test
suite actively misleading.

```planc
BYTES : buf(0:255)
BYTES : src(0:63)

writeInto(buf, src, 300, 512)          % 512 is a LIE - buf holds 256
check('length high byte', buf(7), 1)   % PASSES - byte 7 is in bounds
check('length low byte',  buf(8), 44)  % PASSES
```

Fifty-three bytes went past the end of `buf` and 236 past the end of `src`, and every assertion
passed, because the assertions all landed on bytes that were in bounds. The suite printed its
success line.

**So assert the TOTAL, not only the fields.** A message can never be longer than the buffer it
was written into, and that single check is the one that catches it:

```planc
check('total fits the buffer', len, expectedTotal)
```

And size the fixtures for the case being tested rather than telling a routine the buffer is
bigger than it is.

**Detect:** any call whose declared maximum exceeds the declared size of the array passed with
it. Comparable literals make this checkable.
**Source:** MEASURED 2026-08-27 in a live test suite that reported success while corrupting
memory on every run.

---

## R118 - A BOOLEAN will not pass where an INTEGER is declared

PLANC does not convert between them at a call. A routine declared
`ROUTINE VOID, VOID (BYTES, INTEGER, INTEGER) : check(...)` cannot be handed the result of a
`BOOLEAN` function, and an expression like `len <= 256` is a BOOLEAN, not a 0 or a 1.

Write a second routine for the BOOLEAN case rather than trying to convert. It also produces
better output - `got TRUE want FALSE` says more than `got 1 want 0`.

Related: R108 and R109 are the same idea for `BYTES` and `BYTE`, and both report the type error
against the wrong identifier, so check the ARGUMENT TYPES before the argument the message names.

**Detect:** a BOOLEAN-valued expression or routine passed where the declaration says INTEGER.
**Source:** MEASURED 2026-08-27.

---

## R119 - Every `IMPORT` and `EXPORT` belongs in one block, before any ordinary declaration

An `IMPORT` **or an `EXPORT`** that appears after an ordinary declaration at module level draws

```
    991   (331)  *** ERROR   - MISPLACED STATEMENT "IMPORT"
    131          *** ERROR   - MISPLACED STATEMENT "EXPORT"
```

one per offending line. The message is clear for once, but the shape of the mistake is easy to
make: replacing a constant declaration with an `IMPORT` **where it stood** is the obvious edit
when a constant moves into a shared library, and it reads perfectly well - each `IMPORT` sitting
under the comment that explains that constant.

MEASURED 2026-08-27: ten constants moved to a library, ten `IMPORT`s written in their old
places, ten errors in one compile - four minutes to be told something a linter answers instantly.

**The comment can stay where it is.** Only the `IMPORT` has to move to the block at the top; a
one-line note where the constant used to be keeps the explanation next to the thing it explains.

### The same rule caught `EXPORT` the same day, and that one is worse

Ten trunk constants were exported from `CHATLIB` beside their own declarations - below the main
`EXPORT` block, under the comment explaining them - and every one drew
`MISPLACED STATEMENT "EXPORT"`.

**What the machine did with those ten errors is the part worth remembering:**

| step | what it said |
|---|---|
| `LIST-ENTRIES-UNDEFINED` | nothing - no undefined entries |
| the loader | linked and produced a program |
| the program | ran |
| its own test suite | **139 checks, 0 failures** |
| the last line on screen | `codec is good` |

A compile with ten errors produced a program that passed its own tests, including checks that read
those very constants back with the right values. **Only the listing said anything was wrong.** That
is the whole reason this project gates on the pulled listing rather than on the screen - and the
reason a check costing a second belongs in front of a compile costing minutes.

**Detect:** any module-level `IMPORT` or `EXPORT` whose line number is greater than that of the
first module-level `INTEGER`/`BYTES`/`BOOLEAN`/`REAL` declaration.
**Source:** MEASURED on D100 2026-08-27, both halves; `tools/planc-lint.py` checks it, and
`tools/fixtures/LINT-BADCASE.PLNC` pins it.

---

# Appendix A - Compiler messages worth mapping to rules

When a linter reports something, quoting the message the compiler will actually print saves a build
cycle. The mapping:

## R120 - A name may be `IMPORT`ed ONCE. The second one is an ERROR.

Importing a name that is already imported answers

```
    *** ERROR   - ILLEGAL PREDECLARATION
```

and that is an **ERROR, not the warning a repeated ordinary declaration usually gives** - so the
compile fails rather than shrugging.

**This is the one a MERGE produces and nobody notices.** Two halves of a source both legitimately
needed `MON1`, and each said so in its own `IMPORT` block. Bringing the halves together is a clean
edit that reads correctly in the diff, and the duplicate only exists in the combined file.

It is the same family as the `?` predeclaration rule - predeclaring twice is also
`ILLEGAL PREDECLARATION` - because an `IMPORT` *is* a declaration of a name defined elsewhere, and
the compiler will accept exactly one of them.

MEASURED 2026-08-28: `kHist` was imported into `CHATTST` beside six new routine imports, while it
had already been imported forty lines further down. `tools/planc-lint.py` refused it on Windows in
under a second; the compile it saved is four minutes on the machine.

**Detect:** collect every name in an `IMPORT` at module level and report any that appears twice.
Compare case-insensitively - PLANC does not distinguish case in identifiers.
**False positives:** none seen. Two imports of one name have no legitimate use.
**Source:** RM5 3.16 p.98 (predeclaration); CODE `planc-lint.py`, MEASURED on D100 2026-08-28.

---

## R121 - A helper must NEVER take `(text, textLen)`. Derive the length.

The moment a routine accepts a length beside the string, every call site has to type a number that
**nothing can check** - not the compiler, not the linker, not a test. It is correct on the day it
is written and wrong the first time somebody edits the wording, and it still builds clean.

```planc
% WRONG - the caller counts, and the count rots
ROUTINE VOID, INTEGER (BYTES, INTEGER, INTEGER) : putWord(text, textLen, at)
...
putWord('trunk added', 11, at) =: at

% RIGHT - the string knows how long it is, so ask it
ROUTINE VOID, INTEGER (BYTES, INTEGER) : putWord(text, at)
    INTEGER : i, p, textLen
    MAXINDEX(text, 1) + 1 =: textLen
...
putWord('trunk added', at) =: at
```

**MEASURED 2026-08-31 on the chat product: 93 hand-typed numbers deleted in one sitting** - 29
`putWord`, 19 `logLine`, 18 `buildAdmText`, 15 `tryCmd`, 14 `cmdIs`, 12 `showIfMatch`. All 93 were
CORRECT at the time, so this removed a whole class rather than fixing a live fault. **Audit before
refactoring** - a dozen lines of Python comparing each number with its literal - because knowing
whether you are fixing a hazard or a bug changes how it is reported.

**Two preconditions, both cheap to check:**

1. **Every call site passes a LITERAL.** `MAXINDEX` gives the DECLARED bound, so a caller handing a
   64-byte buffer that holds a 10-byte name would get 64. Where the text is a buffer, the length is
   real information and must still be passed. Grep the call sites for a first argument that does
   not begin with a quote - and beware that the ROUTINE definition line matches that grep too.
2. **The routine is not `STANDARD`** - `MAXINDEX` on a parameter is unavailable in the
   FORTRAN/COBOL calling sequence (R115).

**A `BYTES` DECLARED FROM A LITERAL BEHAVES THE SAME WAY - MEASURED, not assumed.** R115 measured
`MAXINDEX` on array parameters and on a subarray, but not on `BYTES : v := 'DROP-MEMBER'` passed as
a parameter, which is the shape `tryCmd` and `showIfMatch` in CHAT-MON now rely on. Three checks
were added to the on-machine suite rather than trusting that it "should" work, because the symptom
of being wrong is a menu where a command quietly stops matching - which reads as a logic bug, not
an array question. MEASURED on D100 2026-08-31, all three PASS:

```planc
BYTES : litShort := 'HELP'             % MAXINDEX + 1 -> 4
BYTES : litVerb  := 'DROP-MEMBER'      % MAXINDEX + 1 -> 11
BYTES : litLong  := 'RESTART-TRUNK'    % MAXINDEX + 1 -> 13
```

So the bounds of a literal-initialised `BYTES` are `0:len-1`, exactly like a literal at a call
site.

**Why not a length constant declared next to the text?** It puts the two facts closer together but
leaves them as two facts, and the failure mode is unchanged: edit the text, forget the number,
builds clean. **Why not a macro?** PLANC's compile-time macros (RM5 0.11 p.208) are text
substitution with parameters; there is no compile-time string-length operator to substitute, so a
macro can wrap the call but has nothing to put in the length slot. **Name the text, never the
length** - a named `BYTES : vDropMbr := 'DROP-MEMBER'` is worth having for reuse and readability,
and the helper still derives its length from it.

**One trap, three costumes.** An `'ALn'` field width (R28), a hand-counted length beside a literal
(this rule), and a start column written as `65 - length` are the same defect: a number a human
counted, that nothing verifies, that builds clean when wrong. Meeting one is a reason to look for
the other two.

**Detect:** two NAMED lists in `planc-lint.py`. `LITERAL_LENGTH_HELPERS` holds helpers that still
take a length - the number must equal the literal - and is deliberately EMPTY, because that is the
goal state rather than an oversight. `DERIVES_ITS_OWN_LENGTH` names every helper whose parameter
has been removed, so the old shape is refused by name if it comes back; the compiler's own
complaint about the parameter list never mentions `MAXINDEX` or why the parameter went away.
**False positives:** none, provided a helper that legitimately takes a buffer length stays out of
both lists.
**Source:** RM5 3.17 p.52, p.153, p.249 (`MAXINDEX`), 0.11 p.208 (macros); CODE `planc-lint.py`,
`SINTRAN/XMSG/SINTRAN-CHAT/`, MEASURED 2026-08-31.

---

| Compiler message | Rule |
|---|---|
| ARRAY BOUNDS MISSING | R24 |
| ARRAY BOUNDS CONFLICT WITH A PREDECLARATION | R36 |
| COMMAND NOT PERMITTED WITHIN A MODULE | R90 |
| CONFLICTING DATA TYPES IN CORRESPONDING IMPORT/EXPORT | R84 |
| DATA TYPE NOT PREVIOUSLY SPECIFIED | R74 |
| EQUIVALENCE MAY CAUSE STORAGE CONFLICT | R37 |
| EXITFOR / EXITWHILE ALREADY PRESENT WITHIN THE LOOP | R58 |
| EXPRESSION DOES NOT STORE A VALUE | R19 |
| IDENTIFIER ALREADY SPECIFIED/DECLARED | R4, R39, R113 |
| *(no message at all)* - two EXPORTs equal in 7 chars | R114 |
| *(no message at all)* - a write past the end of an array | R24, R116, R117 |
| *(no message at all)* - a length typed beside a literal | R28, R121 |
| ILLEGAL PREDECLARATION | R36, R120 |
| MISPLACED STATEMENT "IMPORT" | R119 |
| MISPLACED STATEMENT "EXPORT" | R119 |
| IDENTIFIER IN EXPORT, BUT NO DECLARATION | R83 |
| ILLEGAL CHARACTER | R1, R2, R15 |
| ILLEGAL CONTROL IDENTIFIER | R58 |
| ILLEGAL DATA-ELEMENT TO BE CONVERTED *(a WARNING - the build survives)* | R46 |
| ILLEGAL DATA TYPE | R108, R109 |
| ILLEGAL FORMAL PARAMETER IN MACRO | R91 |
| ILLEGAL INLINE INVOCATION | R75 |
| ILLEGAL MODULE TERMINATION | R83 |
| ILLEGAL PARAMETER REFERENCE IN MACRO BODY | R91 |
| ILLEGAL PREDECLARATION | R36 |
| ILLEGAL SYNTAX | R19, R41, R45, R73, R74 |
| INCASE CONTAINS INVALID VALUE | R61 |
| INCOMPATIBLE DATA TYPES | R33 |
| INCONSISTENT DIMENSIONS | R23 |
| INISTACK INVOCATION MISSING | R86 |
| INITIALIZATION VALUES OVERFLOW DECLARED SIZE | R29 |
| INVALID ACTUAL PARAMETER, FORMAL PARAMETER DECLARED AS WRITE | R67 |
| INVALID ARRAY FOR INISTACK INVOCATION | R86 |
| INVALID PARAMETER LIST | R41, R65 |
| INVALID TYPE FOR IN-VALUE/OUT-VALUE/PARAMETER | R77 |
| MISSING KEYWORD, ENDIF/ENDCASE/ENDFOR/ENDDO OR ENDON | R55, R58, R61, R104 |
| MORE SUBSCRIPTS THAN IN THE ARRAY DECLARATION | R4, R23 |
| MULTIDIMENSIONAL ARRAY NOT ALLOWED HERE | R58, R86 |
| NEGATIVE BOUND ILLEGAL (ND-100) | R101 |
| NOT PREVIOUSLY DECLARED | R40, R74, R88 |
| QUALIFIER REQUIRED FOR THIS RECORD COMPONENT | R52 |
| REQUIRE ELSE OR ALL POSSIBLE VALUES USED IN INCASE PARTS | R61 |
| ROUTINE WITH AN OUT-VALUE REQUIRES A RETURN | R69 |
| SET MEMBER OVERLAP | R31 |
| SQUEEZE OPTION GENERATES INCORRECT CODE FOR THIS ROUTINE (ND-100) | note only |
| WRITE DECLARATION ILLEGAL IN READ ONLY RECORD | R27 |
| ASSERT VIOLATION AT address (runtime) | R63, R39 |
| STACK OVERFLOW AT address (runtime) | R76, R86 |
| NO ON ROUTINEERROR HANDLER, ERRETURN (runtime) | R106 |

**Source:** RM5 Appendix B (p.266-272).

---

# Appendix B - Things the manuals leave genuinely unclear

Marked so nobody builds a rule on them.

1. **UNVERIFIED - the special-character set for routine names.** RM5 p.176 prints a dagger-like
   glyph among `! " $ * + - . / : < = > ? \ [ ]`. The OCR cannot be trusted here and no other source
   repeats the list.
2. **CONFLICT - SHIFT fill on right shifts.** RM5 says sign-extend for signed types; UG6 says zeros,
   full stop. See R48.
3. **CONFLICT - trailing/double underscore in identifiers.** RM5 forbids; UG6 and ADV allow.
   See R3.
4. **CONFLICT - `Remove` on a missing list element.** RM5 says no error indication at all; UG6 says
   `POINTERERROR` is raised. See R103.
5. **UNVERIFIED - `<>` as a not-equal operator.** Appears once in UG6's SET entry; contradicted
   everywhere else. See R45.
6. **UNVERIFIED - reading a routine LOCAL after its ON block has run.** The manual documents the
   unpredictability of out-values and WRITE parameters after a handler; the project observed the
   same for locals and works around it with module-level variables. See R71.
7. **CONFLICT - whether an undeclared name is diagnosed.** RM5 lists NOT PREVIOUSLY DECLARED; the
   project measured `0 DIAGNOSTICS` for a store into a name declared nowhere, on PLANC F. See R40.
8. **UNVERIFIED - which spelling of INISTACK the OCR intends in any given example.** Four spellings
   appear across the two manuals. `INISTACK` is the one in the keyword tables and the one that
   compiles. See R86.
9. **NOT COVERED HERE - PLANC-MC (MC68000) code generation, ND-500 packing layout, storage sizes.**
   All of that lives in RM5 Appendix C and is machine-dependent; a linter should not encode sizes.

---

# Appendix C - Suggested severity for a linter

| Severity | Rules |
|---|---|
| **Error** (will not build, or builds and is certainly wrong) | R1, R2, R14, R15, R19, R21, R23, R24, R26, R28, R32, R41, R45, R55, R56, R61, R65, R69, R73, R77, R80, R84, R85, R86, R108 |
| **Error, silent at compile time** (this is where the days go) | R4, R11, R13, R31, R36, R39, R40, R54, R74, R88, R89, R93, R98, R111, R113 |
| **Warning** | R3, R5, R8, R9, R20, R29, R34, R35, R37, R38, R51, R53, R57, R58, R59, R60, R62, R66, R67, R68, R71, R75, R76, R78, R79, R81, R82, R90, R91, R95, R96, R97, R102, R103, R104, R105, R106, R107 |
| **Note / informational** | R6, R7, R10, R12, R16, R17, R18, R22, R25, R27, R30, R33, R42, R43, R44, R46, R47, R48, R49, R50, R52, R63, R64, R70, R72, R92, R94, R99, R100, R101, R109, R110, R112 |

**Rule count: 114.**

---

**Last updated:** 2026-08-31
**Written for:** a linter author extending `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\tools\planc-lint.py`
