# The COSMOS XMP library - XMSG from PLANC (and its FORTRAN twin XMF)

This is the page behind the one-liner in the PLANC guide: "XMSG IPC: via COSMOS XMP library
routines (MON 200B not directly available)". It says what the XMP library is, why it exists,
what is actually installed on a real machine, every routine it offers, how to declare and
call the core ones from PLANC, how to build, and the traps that were found the hard way on
D100. Every claim states its source.

**Primary sources:**
- `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` - the manual FOR this
  library (chapter 1 intro, chapter 2 routine reference, appendix D errors, appendix G
  sample client/server, the MON 200 appendix)
- Live listings and compile attempts on D100 (SINTRAN III VSX/500 K, XMSG Release L)
- The product media inventory: `Installation/Software/ND-10609/README.md`

---

## 1. What it is

The COSMOS Programmers Library (product **ND-10609**) is the official high-level-language
interface to COSMOS communication. Its own preface lists five libraries:

| Prefix | Meaning |
|---|---|
| **XMP** | **X**MSG from **P**LANC - this page |
| XMF | XMSG from FORTRAN - identical routines, `XMF*` names, functions instead of out-values |
| RRP | RR-LIB from PLANC - request/response layer built ON XMSG |
| TLP / TLF | TLIB (Transport LIBrary) from PLANC / FORTRAN - data transfer between RT programs |

XMP is a set of `XMPxxxx` routines that a PLANC program `IMPORT`s and the linker resolves
from a BRF (ND-100) or NRF (ND-500) library file. Each routine wraps one XMSG function.

## 2. Why a library - the "MON 200B not directly available" claim, unpacked

XMSG itself is entered through **MON 200B (MON XMSG)**. The COSMOS guide's own appendix
says how: "XMSG functions are normally executed using the monitor call MON 200 with
parameters being passed in the registers. The T register contains the particular function
required, with option bits set in its high order byte when required."

That register protocol is the reason the library exists:

- **From PLANC or FORTRAN you cannot express it.** A MON 200B call needs a function code in
  the T register's low byte, option bits in its high byte, other parameters spread across
  registers, and results coming back the same way. PLANC's `MONITOR_CALL` mechanism does not
  cover that shape - so the practical statement in the PLANC guide is "MON 200B not directly
  available" *from the language*, and the XMP routines are the supported way. (From MAC or
  NPL you CAN issue MON 200B directly - the repo's own XMSG work does exactly that; see
  `SINTRAN/XMSG/DOC/XMSG-API.md` for the function-by-function MON 200B interface.)
- **On the ND-500 there is no MON 200B at all** - it is an ND-100 monitor call. The product
  ships a separate ND-500 relocatable (`XMP-500-B02:NRF`, see section 4), which is how an
  ND-500 program reaches XMSG. (That the -500 library exists is verified from the floppy
  inventory; how it bridges to the ND-100 side is not decoded here.)

So: one API, three transports - `XMPF*` PLANC routines, `XMF*` FORTRAN functions, and raw
MON 200B for assembly - all driving the same XMSG kernel functions (`XFOPN`, `XFGET`,
`XFSND`, `XFRCV`, ...; the routine's F is the function's F).

## 3. Call conventions (manual chapter 1.7 - all verbatim rules)

- **PLANC: status is the out-value.** `ROUTINE VOID,INTEGER (params)` -
  `xmpfrel(0,msgIdentifier) =: returnStatus`. FORTRAN: same routine as a function -
  `returnStatus = xmfrel(0,msgIdentifier)`.
- **Status 0 = OK** (symbol `XMOK`) - **and the library REMAPS statuses. SETTLED 2026-08-18
  from the real `XMP-B02:DEFS`:** raw XMSG's OK status is 1 (`XMOK = 0  % NOTE change from
  XMSG OK=1`), and the library returns errors as POSITIVE "STANDARD values" with base
  `XMXKXXX = 16896 = 41000B`: `XMXENTM = 16896` is "not terminated - NOT an error" (the
  guide's OCR garbled it to `MXNENTM`), `XMXENOT = 16897` "no more XT-blocks", and so on -
  the raw kernel's negative `XE...` symbols prefixed `XM` and rebased. So when programming
  through the LIBRARY: 0 = done, 16896 = not terminated, anything else = error. The negative
  symbols in `XMSG-PL-VALUES-L:INCL` belong to the RAW MON 200B interface, not to `XMPF*`
  returns.
- **EXCEPTION - `XMPFRCV` returns the MESSAGE TYPE as its success status** (`XMTNO=1` ..
  `XMTPS=6`). Test it with `> 0`, never against `XMOK` - comparing to zero throws away
  every message.
- **The first parameter of every call is `flags`, and flags are BIT POSITIONS, not
  values.** `XFWTF=15` means bit 15: write `2**XFWTF`, never `15`. `XFWAK=14` means
  `2**14` = `0x4000` (that value is oracle-confirmed in this repo's wire work). The flags
  parameter exists on every call even where no option is defined - pass 0 there.
- An RT program that wants to call XMSG as a SYSTEM task must set the `XFSYS` option.
- Special parameter types (`XmsgIdentifier`, `XmsgUserAddress`) come from the `XMP:IMPT`
  include - see section 5 for what to do when that file is not installed.

## 4. What ships, and what is actually on a real machine

**The product media (EXTRACTED 2026-08-18 - real floppy `10609B02-XX-01D`, image from the
[Norsk Data Software Archive](https://github.com/HackerCorpLabs/norskdata-software-archive)
product `ND-10609` (local: `/home/ronny/repos/norskdata-software-archive`; wiki:
[ndwiki.org/wiki/ND-10609B](https://www.ndwiki.org/wiki/ND-10609B)); all 24 files now kept at
[Installation/Software/ND-10609/files/](../../../Installation/Software/ND-10609/files/),
text files also as parity-stripped `.readable.txt` copies):**

| File | What |
|---|---|
| `XMP-B02:DEFS` (28,407 bytes) | PLANC constants include - note the real name carries the version, not the manual's bare `XMP:DEFS` |
| `XMP-B02:IMPT` (8,751 bytes) | PLANC types (2 TYPE lines) + IMPORT declarations for 54 routines (counted in the extracted file), one file "FOR THE ND100 / ND500 / MC68000" |
| `XMP-100-1-B02:BRF`, `XMP-100-2-B02:BRF` | the ND-100 library CODE, 1-bank / 2-bank variants - the 1-bank file's entries were listed on D100 itself, see 4a |
| `XMP-500-B02:NRF` | the ND-500 library (relocatable for NLL) |
| `COS-LIB-IN-B02:PROG` | the installer |

(plus the same pattern for XMF/TLP/TLF/RRP; RRP ships PLANC-only.)

**What D100 actually has (MEASURED by listing AND by asking the library itself; the names
DIFFER from the manual, and one earlier reading is corrected):**

| On D100 | What it really is |
|---|---|
| `(UTILITY)XMSG-LIBRARY-L03:BRF` | the NPL-level XMSG SUPPORT library - eleven entries, all internals (`XBINI XBARC XBAST XBAIN XBADB XBLOC XQVER XQTAB XWGER XMRER XMERR`). **NOT one `XMPF` symbol in it** - measured with `LIST-BRF-ENTRIES`, and loading it moves the linker's free address by exactly nothing |
| `(UTILITY)XMSG-PL-VALUES-L:INCL` | the constants - what the manual calls `XMP:DEFS` |
| - nothing - | `XMP:IMPT` is NOT installed |
| - nothing - | **the `XMPF*` code itself is NOT installed either** - a program written to the guide's style compiles and then dies with `LOADER MESSAGE: UNDEFINED ENTRIES` listing every `XMPF*` call |
| `(UTILITY)XMSG-SYMBOL-L03:SYMB` | symbol table |

So a real Release-L machine has the XMSG RUNTIME (kernel, XROUT, constants, the NPL support
library) but **not the COSMOS PLANC binding - neither the declarations nor the code behind
them**. Both halves come only from the ND-10609 product above. The full measurement is in
[PLANC-XMSG-COMMUNICATION.md](PLANC-XMSG-COMMUNICATION.md), which also notes the manuals
agree: the SINTRAN III Monitor Calls manual lists MON 200B as **"Not available"** for PLANC
(and FORTRAN/COBOL/PASCAL/MAC), and the Communication Guide documents every XMSG function as
a T/A REGISTER convention (`T::XFOPN; *MON 2XMSG`) - a shape PLANC's `MONITOR_CALL` cannot
produce, which is the real content of the PLANC guide's "MON 200B not directly available"
one-liner.

Also NOT shipped in the L values file: the success symbol `XMOK` itself. Define
`CONSTANT XMOK=0` yourself (the file's own error symbols being all negative, and XROUT's
`XRSOK=0`, are the evidence for 0).

## 4a. TWO SETS OF FILES ON THE MACHINE - WHICH IS THE TRUTH, AND WHAT ND-10609 GIVES

After the ND-10609 recovery there are two sources on/for D100 and they LOOK like competing
truths. They are not - they are truths about DIFFERENT LAYERS, plus one real version skew.
Everything in this section is MEASURED; each claim says how.

### What each ND-10609 XMP file GIVES - measured, no assumptions

| File | What it gives | How measured |
|---|---|---|
| `XMP-B02:DEFS` | 283 `CONSTANT` lines: MON 200B function codes, flag bit positions, message types, XROUT services, raw errors, AND the library-layer symbols that exist NOWHERE else - `XMOK=0` ("NOTE change from XMSG OK=1", the file's own comment), the `XMX...` STANDARD status values from base `XMXKXXX=16896=41000B` (`XMXENTM` = not terminated). 96 of its symbols are in no L-era file. | file read in full; constants parsed and diffed against VALUES-L by script, 2026-08-18 |
| `XMP-B02:IMPT` | The PLANC binding declarations: `TYPE XMMSGIDENTIFIER = INTEGER`, `TYPE XMUSERADDRESS = INTEGER` (comment: old MC68000 compiler needs INTEGER4), and `IMPORT` declarations for **54 routines** with exact parameter types and WRITE/READ WRITE markers. No other file on Earth that we hold declares these. | file read in full; routines counted |
| `XMP-100-1-B02:BRF` | **The `XMPF*` CODE, 1-bank.** 14 library-mode BRF units defining **55 `XMP*` entries** - every routine IMPT declares (XMPFOPN, XMPFRCV, XMPFGET, XMPBLET, XMPOPCN, XMPROUT, ... XMPXETS, XMPXRTS) plus `XMPBLEN`, and a support module `XMFSTAC`. This is the code whose absence made every earlier link die with UNDEFINED ENTRIES. | `LIST-BRF-ENTRIES (UTILITY)XMP-100-1-B02` run in BRF-LINKER-C01 **on D100 itself**, 2026-08-18; full output log kept from the session |
| `XMP-100-2-B02:BRF` | the 2-bank twin, by product naming convention | inventory only - its entries have NOT been listed; do not treat its contents as verified |
| `XMP-500-B02:NRF` | the ND-500-side relocatable | inventory only - content not verified |
| `COS-LIB-IN-B02:PROG` | the product's own installer | present; compiled PROG, not decoded, NOT yet run |

And what the (UTILITY) XMSG L03 kit gives, for contrast (all measured earlier, sources in
section 4): `XMSG-PL-VALUES-L:INCL` = 300 `CONSTANT` lines and NOTHING else - zero TYPE,
zero ROUTINE, zero IMPORT lines (file read in full); `XMSG-LIBRARY-L03:BRF` = 11 NPL-level
internal entries, not one `XMPF` symbol (LIST-BRF-ENTRIES on D100); plus the kernel/XROUT
BPUNs and symbol tables. **Constants alone cannot CALL anything - that is the whole reason
ND-10609 exists.**

### Which file is authoritative for what

Measured 2026-08-18 (constant-by-constant diff of the two files):

| Question | Authoritative source | Why |
|---|---|---|
| The RAW MON 200B interface (function codes in T, raw flags, RAW error statuses, XROUT services) against the kernel that is actually running | **`(UTILITY)XMSG-PL-VALUES-L:INCL`** - installed WITH the XMSG L03 product, same release as the running kernel | It is the L-era file: it carries 113 symbols the 1986 DEFS lacks, including XF function codes ADDED after B02. For NPL/MAC work and for our wire RE, this is the truth. |
| The `XMPF*` library layer - routine names, signatures, the special types, and the library's OWN status model (`XMOK=0`, `XMX...` standard values) | **`XMP-B02:DEFS` + `XMP-B02:IMPT`** (ND-10609) | It is the ONLY source that describes this layer at all. The L-era kit simply does not contain a PLANC binding - no declarations, no `XMPF*` code, and none of the `XMX` standard values (`XMOK`/`XMXENTM` appear in NO L-era file). |
| The 184 constants BOTH define (functions, flags, message types, raw errors) | **They AGREE - with exactly ONE conflict.** | Full value diff: 183 of 184 identical. The one difference: `X5FUN` = 43 in the J-era DEFS vs **48** in VALUES-L (highest-function-number sentinel - L added five functions). Every constant a program actually passes or tests is identical in both. |

So "which is correct" resolves to: **use the ND-10609 files when talking THROUGH the XMPF*
library** (they define that layer, and their shared constants match L anyway), **use
VALUES-L when talking raw MON 200B** (it knows the five L-era functions the old DEFS
predates). Never include both in one compile - 184 shared names = 184 redefinitions.

**Put as WHY in one breath:** ND-10609 is not a replacement for the (UTILITY) include and
not an alternative source of the same facts - it is the ONLY source of three things a PLANC
program cannot get anywhere else: (1) the `XMPF*` code (measured absent from every L03
file, measured present in `XMP-100-1-B02`), (2) the routine/type declarations (VALUES-L
contains zero of either - read in full), and (3) the library's own status vocabulary
(`XMOK`, `XMX...` - in no L-era file). The (UTILITY) VALUES-L file stays authoritative for
the RAW interface because it is newer than ND-10609 and matches the running kernel. One
file per layer; the layers do not compete.

**The one real skew, still unverified:** the B02 library code and DEFS are built against
XMSG version J (the product preface says so); the running kernel is Release L. `X5FUN`
43-vs-48 is the visible proof of the gap. Old-caller-on-newer-kernel is the compatible
direction on every ND product measured so far, but for XMSG it has not been PROVEN - the
first successful `XMPF*` link-and-run is the proof, and it has not happened yet.

**Install status: INSTALLED 2026-08-18.** The product's own installer
`COS-LIB-IN-B02:PROG` was run on D100 and copied all 24 module files (XMP/XMF/RRP/TLP/TLF) to
**`(PACK-ONE:SYSTEM)`** - "Installation completed". So the binding lives under **SYSTEM, not
UTILITY**: from PLANC `$INCLUDE XMP-B02:DEFS` / `$INCLUDE XMP-B02:IMPT` resolve bare (SYSTEM
is the default directory), and the link is `LOAD XMP-100-1-B02` (bare) or
`LOAD (SYSTEM)XMP-100-1-B02`. The earlier ndtool hand-copy (which put files under UTILITY and
SYSTEM) is superseded and its UTILITY copies are now orphans to delete. Full confirmed
procedure and the installer's SYSTEM-default prompt: see
[Installation/Software/ND-10609/README.md](../../../Installation/Software/ND-10609/README.md)
"Installing on a machine". Note the XMSG L03 RUNTIME kit (`XMSG-LIBRARY-L03`,
`XMSG-PL-VALUES-L`, `XMSG-SYMBOL-L03`) genuinely lives under `(UTILITY)` - that is a separate
product from the ND-10609 binding and its UTILITY references above are correct.

## 5. Declaring the routines when `XMP:IMPT` is missing

This repo carries a written-from-the-manual replacement:
**[`SINTRAN/XMSG/watch/XMPIMP.INCL`](../../../SINTRAN/XMSG/watch/XMPIMP.INCL)** (an earlier
copy: `SINTRAN/XMSG/SINTRAN-CHAT/XMSG-PL-IMPORT.INCL`). To be clear about provenance:
**XMPIMP.INCL is OUR OWN file** - written in this repo, never shipped by ND, not present on
D100 or any machine. It supplies the DECLARATIONS only; it cannot supply the missing `XMPF*`
CODE (see sections 4 and 8 for that problem). It is a different file from
`XMSG-PL-VALUES-L:INCL`, which is the genuine ND constants file fetched off D100. Every signature in it is transcribed
from the guide's per-routine parameter tables and cross-checked against the worked calls the
same manual prints; the PLANC declaration syntax is from the PLANC Reference Manual
(ND-60.117.5):

```planc
% ROUTINE <in-value>, <out-value> (<params>) : name  -  WRITE marks an output parameter
TYPE XMUSERADDRESS = INTEGER
TYPE XMMSGIDENTIFIER = INTEGER
CONSTANT XMOK=0

IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER WRITE) : xmpfopn )
IMPORT ( ROUTINE VOID, INTEGER &
    (INTEGER, INTEGER, XMMSGIDENTIFIER WRITE) : xmpfget )
IMPORT ( ROUTINE VOID, INTEGER (INTEGER, INTEGER, INTEGER4) : xmpfsnd )
% ... the full set is in XMPIMP.INCL
```

**VERDICT (2026-08-18): the real `XMP-B02:IMPT` was extracted and the transcription checked
against it.** Every core signature matches, and every open question is settled:

- `TYPE XMMSGIDENTIFIER = INTEGER` and `TYPE XMUSERADDRESS = INTEGER` - both guesses RIGHT.
  (The real file's own comment: the OLD MC68000 compiler, whose default INTEGER is
  INTEGER2, needs `XMUSERADDRESS = INTEGER4`.)
- `XMPBLET`'s second parameter is `INTEGER` - the manual's `Bytes` was the typesetting slip,
  as suspected. But its THIRD parameter (offSet) is **`INTEGER READ WRITE`**, not plain
  `WRITE` - the one place the transcription was wrong.
- `xmpfrea` and `xmpfwri` are declared identically (both `XMUSERADDRESS`) - the "declared
  alike" call was right.
- Extras the manual's catalog hinted at: the port-to-magic call is really spelled
  **`xmpfp2m`** (the manual table's `XMPF2PM` is a typo), and there are two routines with a
  DIFFERENT shape: `xmpxets` / `xmpxrts` are `ROUTINE INTEGER,VOID (INTEGER WRITE)` - an
  in-value, no out-value.

**For real builds, use the real file:**
`Installation/Software/ND-10609/files/XMP-B02-IMPT.readable.txt` (7-bit text, `$EOF`
already at the end). `XMPIMP.INCL` stays as the record of what the manual alone could
provide, with a superseded note in its header.

## 6. The routine catalog (manual chapter 1.8 - complete)

FORTRAN names are the same with `XMF` for `XMP`. Groups per the manual's own table:

**Core XMSG calls** - XMPFOPN (open port), XMPFCLS (close port), XMPFGET (get message
buffer), XMPFREL (release buffer), XMPFWRI (write into message), XMPFREA (read from
message), XMPFSND (send), XMPFRCV (receive), XMPFMST (message status), XMPFPST (port
status), XMPFGST (general status), XMPFRTN (return message to sender), XMPFRHD/XMPFWHD
(read/write header), XMPFRRE (receive+read), XMPFRRH (receive+read header), XMPFSCM (set
current message), XMPREAD/XMPSEND/XMPWRHD/XMPWRTE (same as read/send/write-header/write but
"not necessarily current" message), XMPFM2P / XMPF2PM (magic number <-> port number),
XMPFDCT (disconnect), XMPFDUM (dummy), XMPCONF (configuration), XMPFSMC (start multi-call -
the escape hatch toward raw MON 200 sequences).

**Privileged / driver / system calls** - XMPFPRV (make task privileged), XMPFCPV (check
privileges), XMPFABR (absolute read of physical memory), XMPFALM/XMPFFRM (allocate/free
message buffers), XMPFCRD (define driver), XMPFDBK (driver bank), XMPFDMM (max memory),
XMPFDUB (define user buffer), XMPFSIN (init system functions), XMPFSTD (start driver),
XMPFWDF (define wake-up context).

**XROUT calls (name service)** - XMPOPNM (open and NAME a port), XMPOPCN (create CONNECTION
port with a seat count), XMPINFC (increment free connection count - give a seat back),
XMPCLNM (clear name and close), XMPROUT (send a message to or via XROUT - lookup BY NAME).

**Buffer-formatting calls** (build/parse the parameter-block convention XROUT and the
COSMOS servers use; on error these return **-1**, not a negative symbol) - XMPBINI (build
header), XMPBLET (format an XSLET letter header), XMPBAIN / XMPBADB / XMPBAST (append
integer / double integer / string), XMPBLOC (locate parameter), XMPBRDY (buffer ready).

**The twelve a working client+server pair needs**, with parameter lists (W = written back):

| Routine | Parameters |
|---|---|
| `XMPFOPN` | flags, **portNumber W** |
| `XMPOPCN` | flags, portName BYTES, uniqueName BOOLEAN, maxConnections, **portNumber W** |
| `XMPINFC` | flags, portNumber, extraConn, serialNumber |
| `XMPFGET` | flags, sizeBuffer, **msgIdent W** |
| `XMPFWRI` | flags, msgDisp, userAddress, userDisp, userLength, **writtenLength W** |
| `XMPFREA` | flags, msgDisp, userAddress, userDisp, userLength, **readLength W** |
| `XMPFSND` | flags, localPort, remoteMagicNum (INTEGER4 - a magic number is a FULL port address and does not fit one word) |
| `XMPROUT` | flags, msgIdentifier, myPort |
| `XMPFRCV` | flags, localPort, **msgType W**, **remotePort W**, **msgIdentifier W**, **msgLengthOrStat W** |
| `XMPFMST` | flags, msgIdent, **msgType W**, **remoteMagic W**, **length W** |
| `XMPFREL` | flags, msgIdentifier |
| `XMPBLET` | headerBuffer BYTES, lengthBuffer, **offSet W**, serialNumber, systemName BYTES, portName BYTES |

Worked calls the manual prints verbatim (these fix the argument ORDER):

```planc
xmpfopn(0, portNumber)                                 =: returnStatus
xmpfget(0, sizeBuffer, msgIdent)                       =: returnStatus
xmpfsnd(flags, localPort, remoteMagicNum)              =: returnStatus
XMPFMST(FLAGS, MSGIDENT, MSGTYPE, REMOTEMAGIC, LENGTH) =: RETURNSTATUS
xmpfrel(0, msgIdentifier)                              =: returnStatus
xmprout(0, msgIdentifier, myPort)                      =: returnStatus
```

## 7. The two canonical call sequences

**A receiving task** (manual chapter 1.6): open port -> receive -> read into own buffer ->
release. A long-lived server blocks in `XMPFRCV` with `2**XFWTF`; a task that must also
watch something else (a keyboard) calls `XMPFRCV` WITHOUT `XFWTF`, which returns at once
with the not-terminated status when the port is empty.

**A sending task**: open unnamed port (`XMPFOPN`) -> get buffer (`XMPFGET`) -> write data
(`XMPFWRI`) -> send (`XMPFSND` when you hold the magic number, or format a letter with
`XMPBLET` and hand it to `XMPROUT` for delivery BY NAME).

**A connection server** (the shape used by the COSMOS file servers and this repo's chat
drafts): `XMPOPCN` with a seat count -> loop { `XMPFRCV` (blocking) -> `XMPFMST` to get the
sender's full magic number (XMPFRCV only hands back a hashed port) -> `XMPFGET`/`XMPFWRI`
the reply -> `XMPFSND` -> `XMPFREL` } -> `XMPINFC` to return a seat when a member leaves.
Note from this repo's wire work: **the XROUT lookup (`XMPROUT`) is the call that spends a
seat** - a name lookup is not free
(`SINTRAN/XMSG/DOC/CHAT-SEAT-LEAK-AND-XMROU-2026-08-11.md`).

## 8. Building - the two-step compile-then-link, and the RIGHT library

With the ND-10609 binding installed on SYSTEM (section 4), a PLANC program that `$INCLUDE`s
`XMP-B02:DEFS` + `XMP-B02:IMPT` compiles to a BRF, and BRF-LINKER-C01 resolves the `XMPF*`
calls against the installed library. `SINTRAN/XMSG/watch/CHATCC.MODE` is the worked procedure:

```
@PLANC-100-F00
COMPILE CHAT:PLNC,"CHAT:LIST","CHAT"      % no PROG-FILE - produce a real BRF
EXIT
@BRF-LINKER-C01
PROGRAM-FILE "CHAT"
LOAD CHAT
LIBRARY-MODE ON
LOAD XMP-100-1-B02                        % the ND-10609 code, bare = (SYSTEM)
LOAD PLANC-1BANK-F00                      % 5MON_P for MONITOR_CALL
LIST-ENTRIES-UNDEFINED
EXIT
```

**Load the RIGHT library.** The XMPF* code is `XMP-100-1-B02` (1-bank; `XMP-100-2-B02` is the
2-bank twin) - installed on SYSTEM by the product installer, so `LOAD XMP-100-1-B02` resolves
bare. It is NOT `(UTILITY)XMSG-LIBRARY-L03`: that is the NPL-level XMSG SUPPORT library (11
internal entries, no `XMPF` symbol - measured), and loading it leaves every `XMPF*` call
`UNDEFINED`. An earlier version of this recipe loaded `XMSG-LIBRARY-L03` and always ended at
`LOADER MESSAGE: UNDEFINED ENTRIES` for exactly that reason.

**The COMPILE-stage rules are proven on D100:**

1. **Sources must be CRLF, 7-bit.** A file carried over with bare LF makes the compiler
   answer `LINE IS TOO LONG` on every line. (Even parity is for terminal INPUT, not files -
   `XMSG-PL-VALUES-L:INCL` came off D100 with no byte over 127.)
2. **Every `$INCLUDE`d file must end with `$EOF`.** Without it the compiler runs off the end
   of the include, treats that as the end of the whole compile, and reports 0 DIAGNOSTICS -
   a clean-looking build that silently skipped the program. The tell: LINES COMPILED stops
   at the `$INCLUDE` line, and the next command answers `COMMAND NOT PERMITTED WITHIN
   MODULES`.
3. Loading a LIBRARY before the compile resolves nothing (accepted in silence). There is no
   NRL on D100 (the linker is BRF-LINKER-C01), but this build does not need it - the PLANC
   compiler links.

**Link-order note:** load the program first, THEN the library that satisfies it (BRF library
mode supplies only referenced units), THEN `PLANC-1BANK-F00` for `5MON_P` (what
`MONITOR_CALL` compiles into). Loading a library before there is anything to satisfy resolves
nothing, silently.

For ND-500 programs the library is `XMP-500-B02:NRF`, loaded with NLL like any other NRF
(see [NLL-LINKAGE-LOADER-OVERVIEW.md](../../../SINTRAN/ND500/NLL-LINKAGE-LOADER-OVERVIEW.md)).
UNVERIFIED here - no ND-500 XMSG program has been built in this repo yet.

## 9. Traps beyond the build (all found on real hardware)

- **`XMPFRCV`'s success value is the message type** - test `> 0` (section 3).
- **Flags are bit positions** - `2**XFWTF`, never 15 (section 3).
- **PLANC checks no array bounds.** `xmpfrea` takes its length FROM THE SENDER; pass it
  straight in and a peer that sends more than your buffer holds writes past the end. Clamp
  every wire length first
  ([PLANC-XMSG-COMMUNICATION.md](PLANC-XMSG-COMMUNICATION.md) has the worked cases).
- **Terminal input carries even parity in bit 7** - CR arrives as 141, not 13; mask with 127.
- **A monitor-call error in PLANC is a ROUTINEERROR** (`ON ROUTINEERROR DO ... ENDON`), not
  a sentinel return value.

## 10. Worked programs in this repo

| File | What |
|---|---|
| [`SINTRAN/XMSG/watch/CHAT.PLNC`](../../../SINTRAN/XMSG/watch/CHAT.PLNC) | chat client: XMPFOPN, XMPBLET+XMPROUT join, dual watch loop (port + keyboard) |
| [`SINTRAN/XMSG/SINTRAN-CHAT/CHATSV.PLNC`](../../../SINTRAN/XMSG/SINTRAN-CHAT/CHATSV.PLNC) | RT room server: XMPOPCN seats, blocking XMPFRCV, XMPFMST, reply path |
| [`SINTRAN/XMSG/watch/XMPIMP.INCL`](../../../SINTRAN/XMSG/watch/XMPIMP.INCL) | OUR routine/type declarations, written from the manual (replacement for the missing `XMP:IMPT` - declarations only, not the code) |
| [`SINTRAN/XMSG/SINTRAN-CHAT/XMSG-PL-VALUES-L.INCL`](../../../SINTRAN/XMSG/SINTRAN-CHAT/XMSG-PL-VALUES-L.INCL) | the constants file, fetched off D100 |
| [`SINTRAN/XMSG/watch/CHATCC.MODE`](../../../SINTRAN/XMSG/watch/CHATCC.MODE) | the build recipe (section 8) |

**Status, honestly:** the D100 COMPILE behavior in sections 5 and 8 (include names, `$EOF`,
CRLF, library-load ordering) is proven live, and so is the LINK FAILURE: with no `XMPF*`
code on the machine, every build ends at `UNDEFINED ENTRIES`. No XMP program has linked or
run yet. Unblocking requires installing the ND-10609 binding or writing NPL register-level
glue (section 8); the chat pair is the vehicle
(`SINTRAN/XMSG/DOC/PLAN-SINTRAN-NATIVE-CHAT-RT-AND-PROG.md`, task #40).

## 11. Open items

1. ~~Get the ND-10609B floppy~~ ~~Install it on D100~~ **DONE 2026-08-18** - the product
   installer `COS-LIB-IN-B02:PROG` ran on D100 and copied all 24 files to `(SYSTEM)`
   ("Installation completed"). Declarations settled (section 5), error model corrected
   (section 3), install procedure confirmed (section 4 / the ND-10609 README).
2. **Finish and run the chat pair** - the first end-to-end XMP program. Now fully unblocked:
   the binding is installed, includes and load path are corrected in `CHATCC.MODE`. This is
   the PLANC session's work.
3. **J-on-L compatibility unproven.** The B02 binding is XMSG-J era; the kernel is Release L
   (section 5, the `X5FUN` 43-vs-48 gap). The first successful `XMPF*` link-and-run is the
   proof - not yet done.
4. The ND-500 path (`XMP-500-B02:NRF`): how the -500 library reaches the ND-100-side XMSG
   kernel is undocumented.

## See also

- [PLANC-XMSG-COMMUNICATION.md](PLANC-XMSG-COMMUNICATION.md) - the D100 field notes this
  page distills, with the fuller trap discussion
- [PLANC-DEVELOPER-GUIDE.md](PLANC-DEVELOPER-GUIDE.md) - the language and compiler
- `SINTRAN/XMSG/DOC/XMSG-API.md` - the MON 200B function-level interface underneath
- `SINTRAN/XMSG/README.md` - the repo's XMSG hub (wire protocol, kernel RE, live findings)
- `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` - the manual itself
