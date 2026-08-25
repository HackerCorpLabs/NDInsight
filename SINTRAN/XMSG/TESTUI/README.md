# TESTUI — a screen UI in PLANC, and every way we could have built it

**What this folder is:** a working PLANC screen program for SINTRAN III, plus the survey
that picked how to build it and the install procedure for the library it needs.

**Date:** 2026-08-24. **Machine:** D100 (`F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG`),
SINTRAN III VSX/500 K.

---

## 1. What the program does

```
 1-Small window 2-Big window 3-Exit        CHOICE: _        <- row 1, above the main window
+----------------------------------------------------------+
|                                                           |
|   +--------+          +--------------------------------+  |
|   | Lorem  |          |      Lorem ipsum dolor sit amet|  |
|   | ipsum  |          |     consectetur adipiscing elit|  |
|   | dolor  |          |          sed do eiusmod tempor |  |
|   | ...    |          |  ...                           |  |
|   +--------+          +--------------------------------+  |
|                                                           |
|                        HELLO WORLD                        |
+----------------------------------------------------------+
```

| | |
|---|---|
| Row 1 | the status line, one line **above** the main window |
| Rows 2–24 | the main window, a box round the rest of the screen, `HELLO WORLD` centred in it |
| Key `1` | opens the **small** window (10 rows × 10 columns), text left aligned **4 characters in from the border**. `1` again closes it. |
| Key `2` | opens the **big** window (40 wide × 20 high), text right aligned ending **5 characters in from the right border**. `2` again closes it. |
| Key `3` | leaves, back to the SINTRAN command prompt |

Both windows are toggles and both can be open at once — the big one deliberately covers
`HELLO WORLD`, which is what makes the repaint rule in the source worth reading.

## 2. The files

| File | What it is |
|---|---|
| `TESTUI.PLNC` | the program. CRLF, 7-bit, no `$EOF` — a MODE-driven compile must not carry one |
| `TESTUI.MODE` | the build job: `@MODE TESTUI:MODE,,` |
| `SCREEN.SYMB` | the PLANC-SCREEN-H interface, `$INCLUDE`d by the source. Off the vendor floppy, de-parity'd |
| `INTRF1B.BRF` | the PLANC-SCREEN-H 1-bank runtime, linked at build time. Off the vendor floppy, byte-exact. **Renamed from `INTERF-1B` — see the ceiling below** |
| `deploy-over-xmsg.ps1` | **the normal deploy**: stages the four files for the sync daemon. Stops nothing, touches no image |
| `install-screen-h.ps1` | **the fallback only**: writes the disk image directly, and refuses while a machine is up |

### The 13-character ceiling, and why the runtime got renamed

The file transfer packs the specification, an apostrophe and the access letter into a **15-byte
QFORM string**, so a filespec may be at most **13 characters including its two quotes** — 11
characters of `NAME:TYPE`.

| Filespec | Characters | With quotes | |
|---|---|---|---|
| `TESTUI:PLNC` | 11 | 13 | fits |
| `TESTUI:MODE` | 11 | 13 | fits |
| `SCREEN:SYMB` | 11 | 13 | fits |
| `INTRF1B:BRF` | 11 | 13 | fits |
| ~~`INTERF-1B:BRF`~~ | 13 | **15** | **refused before a byte goes out** |

The vendor's own name does not fit. The BRF's name appears in exactly one place that matters —
the `LOAD` line in `TESTUI.MODE` — so it is carried as `INTRF1B:BRF`. `SCREEN:SYMB` cannot be
renamed the same way: `$INCLUDE screen` resolves to it by name, and it happens to fit anyway.

**Nothing in `Xmsg.Sync` checks this length**, so a too-long name is attempted and fails out on
the wire, which reads like a transport fault. `deploy-over-xmsg.ps1` checks it up front instead
and refuses before copying anything.

---

## 3. The survey — every route we actually had

The question was not "can this be done" but "which of the ways available is the one to build
on". Five were real candidates. **What was checked, not assumed:** all 844 files on D100 were
listed and searched, and the whole `norskdata-software-archive` in WSL was searched by product
and by floppy image.

| # | Route | On D100? | Callable from PLANC? | Verdict |
|---|---|---|---|---|
| **1** | **PLANC-SCREEN-H** over VTM | no — **but the floppy exists** and installs by file copy | **yes, it is the PLANC-native one** | **CHOSEN** |
| 2 | Hand-rolled escape sequences | needs nothing | yes | rejected — see below |
| 3 | NSHS (`ND-10013`) | no | **no** | rejected |
| 4 | FOCUS (`ND-10188`) | no | **no** | rejected |
| 5 | UNIQUE / UNIQUICK (`ND-210729`) | no | n/a — it is its own 4GL | rejected |

### Why each of the others lost

**2 — hand-rolled escape sequences.** Write our own box/cursor/clear routines and push bytes
at the terminal with `MON2`. It builds and runs today with nothing installed, and that was the
opening recommendation *before the floppy was found*. It loses on one point that matters more
than convenience: **it throws away terminal independence.** VTM exists precisely so a program
does not know whether it is talking to a Tandberg TDV, a DEC VT100 or a Facit — the terminal
type is set once per terminal and `DDBTABLES:VTM` holds the dialect. Hard-coding one dialect
means the program works on the terminal we happen to test on and quietly draws rubbish on the
next. Since the real library turned out to be available, paying that price bought nothing.

**3 and 4 — NSHS and FOCUS.** Both are richer than PLANC-SCREEN-H (field types, check digits,
date controls, an interactive picture editor). Both are **callable from FORTRAN, BASIC, COBOL
and RPG II — PLANC is in neither list.** Neither is on D100 and neither is in the archive.

**5 — UNIQUE.** Not a library at all; a 4GL interpreter with its own `.UNIQ` form format and no
host language. It answers a different question than "draw a box from a PLANC program".

### Why PLANC-SCREEN-H won

- It is **the** PLANC-native option — the only one of the five whose caller list is PLANC.
- The floppy was found and every file on it is intact:
  `norskdata-software-archive/images/3a0a2e81d2753bfc3e191bf459275b2b/8_nd_f17b_planc-screen-h.img.gz`
- **The floppy carries a complete working vendor demo**, `DEMO-SCREEN:SYMB`, 12,536 bytes of real
  PLANC using every routine. That turned the whole interface from inferred to observed — see §4.
- Install is a file copy. No installer, no article number, nothing to license or generate.
- Its prerequisites are already on D100: `MON-CALL-1B-A00:BRF` and `PLANC-1BANK-F00:BRF`.

### What is still open on this route

**~~Whether a VTM BRF has to be linked.~~ ANSWERED — it must.** The vendor demo's header says
*"Vtm, mon-call-lib, planc-lib must be loaded together with this program"*, and that turned out
to mean real code, not the data file. `LIST-ENTRIES-UNDEFINED` left nine `VT*` entries undefined
on the first build — full output and the library's whereabouts in **§7a**. D100 has the data
files (`DDBTABLES-C11/D11/E11/G06:VTM`) and none of the code, which is exactly why it failed.

What remains open is **which** of the FOCUS floppy's four `VTM-*:BRF` files supply those nine
symbols. `LIST-BRF-ENTRIES` on the machine answers that; a text grep cannot, because BRF symbol
names are packed.

**The terminal type must be set** or VTM has no dialect to use. `@QGET-TERMINAL-TYPE` reads it,
`@QSET-TERMINAL-TYPE` sets it; type `0` means unset, and the manual says a VTM application then
prompts for one. This has not been checked on D100.

---

## 4. The interface, now observed rather than inferred

The repo's own `PLANC-UI-VTM-GUIDE.md` had to mark `frame`'s parameter order as a
**working hypothesis** — nobody had a caller to read. The demo is that caller, and it settles it.

| Call | Signature, **as the vendor uses it** |
|---|---|
| `frame` | `frame(row, col, height, width, attributes)` — height counts rows **inclusive** from the start row: `frame(1,1,24,78,'')` is a full-screen box |
| `bytdis` | `bytdis(row, col, width, text, attributes)` — **width 0 means "the string's own length"** |
| `intdis` | `intdis(row, col, width, INTEGER4value, attributes)` |
| `bytacc` | `bytacc(row, col, width, var, 'PROMPT')` — paints, then lets the user type |
| `intacc` | `intacc(row, col, width, INTEGER4var, 'MUST,PROMPT')` — the demo's own menu selector |
| `fullbar` / `sparsebar` | `(row, col, height, width)` — solid and dashed fills |
| **`blankarea`** | **`blankarea(row1, row2, col1, col2)` — CORNERS, not row/col/height/width** |
| `blankscreen`, `resetscreen` | no parameters |

> **`blankarea` DOES NOT TAKE THE SAME SHAPE AS `frame`.** Four integers on both, and they mean
> different things. The demo proves it: `blankarea(22,22,1,80)` clears line 22 right across the
> screen, which is only possible if the arguments are two rows then two columns. Read as
> `(row,col,height,width)` that call would clear a 22-row block. Nothing warns you.

**The attribute vocabulary**, extracted from the strings inside `INTERF-1B:BRF` itself and
cross-checked against the demo's usage:

```
AUTO-ERASE  AUTO-SKIP  BLANK-WHEN-ZERO  BLINK  CANCEL  HEADING  INVERSE-VIDEO
INVISIBLE  JUSTIFIED-RIGHT  LENGTH-CHECK  LOW-INTENSITY  NORMAL  PROMPT
REMARKS  RIGHT  SPACE-FILL  UNDERLINE  UPDATE  UPPER-CASE
```

Comma-separated in one string: `'INVERSE-VIDEO,BLANK-WHEN-ZERO'`, `'MUST,PROMPT'`,
`'HEADING,REMARKS'`. `SPACE-FILL` on a `frame` blanks its interior as it draws — that is how a
window sits on top of what was underneath it.

**`JUSTIFIED-RIGHT` exists**, but this program does not use it: the right alignment is computed
instead (`start column = 65 - length`, written next to every line). An attribute whose exact
behaviour nobody here has watched is not the thing to hang a stated requirement on.

---

## 5. Installing PLANC-SCREEN-H

**There is no PD sheet, no PI sheet and no ND article number for this product** — confirmed
against both this repo and the archive's own metadata (`products/PLANC-SCREEN.yaml` carries a
name and a category and nothing else). No installer `:PROG`, `:MODE` or `:BATC` exists on the
floppy. **The install is a file copy**, which is what every small library floppy in the
catalogue does when it has no installer.

### The normal way — over XMSG/COSMOS, nothing stopped

**This is the route to use.** The sync daemon holds one link open and carries whatever is
dropped into `SINTRAN/XMSG/sync-out`; you then compile from a terminal. No machine is stopped,
no disk image is touched.

```powershell
cd E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\TESTUI
.\deploy-over-xmsg.ps1 -StartDaemon      # stage all four and start the daemon in a visible window
.\deploy-over-xmsg.ps1 -SourceOnly       # the rebuild loop: just TESTUI:PLNC and TESTUI:MODE
```

Without `-StartDaemon` it stages the files and prints the daemon command for you to run from
`SRC\Xmsg.Live.Runner`:

```
Xmsg.Live.Runner --config topology-d19999-hdlc-server.json --originate-from-seed \
                 --sync sync-out --sync-user SYSTEM --sync-to 100 \
                 127.0.0.1 10362 19999 3600
```

**A HELD LINK, NOT A PUSH PER BUILD.** A one-shot `--push` ends by sending `DISC`, and a link
teardown is where XMSG dies — push-then-compile killed XMSG **fourteen times out of fourteen**.
With the link held open the same compile ran through and the machine was still alive afterwards.

**NOT `--announce-restart` and NOT `--resync-hard`.** Both are known-harmful and poison the
conversation. `--originate-from-seed` is the one that works.

The library files only need carrying once; after that `-SourceOnly` is the loop.

### The fallback — writing the disk image directly

**Only when the machine cannot be talked to at all** (the transport is down), or for boot and
mode files that must exist before the machine can bring its network up.

```powershell
.\install-screen-h.ps1 -WhatIf        # preview, writes nothing, safe while machines run
.\install-screen-h.ps1                # for real - refuses while any RetroCore is up
```

It refuses while a machine is running — an image cannot be written underneath one, which would
write its own copy back over everything. **It does not close anything; it reports and stops.**
Close the machine windows yourself first. It costs a stop and a boot of every machine, which is
exactly why it is the fallback and not the method.

It verifies every file by reading it back off the image and comparing hashes, because
`ndtool --put` without `--overwrite` prints `skipped (exists)` and **still exits 0** — a run
that wrote nothing looks exactly like one that worked.

By hand, the same thing:

```powershell
$t   = 'E:\Dev\Ronny\norskdata-ndfs\ndfs-c\build\ndtool.exe'
$img = 'F:\RC\RonnyTest\HDLC1\BIGDISK0-K-100.IMG'
& $t --put .\SCREEN.SYMB  'SYSTEM/SCREEN:SYMB'  --overwrite $img
& $t --put .\INTRF1B.BRF  'SYSTEM/INTRF1B:BRF'  --overwrite $img
```

`INTERF-2B:BRF` (2-bank) and `INTERF:NRF` (ND-500) are the other two runtimes; this program is
1-bank and needs neither. `PLANC-GEN-A00:PROG` is the `.PICT` screen-picture compiler — not
needed here, since TESTUI draws by calling the routines directly.

### Where the floppy is

```
WSL: /home/ronny/repos/norskdata-software-archive/images/
     3a0a2e81d2753bfc3e191bf459275b2b/8_nd_f17b_planc-screen-h.img.gz
```

Contents, all eight files intact, dated 1986-07 to 1987-01:

```
DEMO-SCREEN:SYMB    12536   the working demo - the most valuable file on the disk
PLANC-GEN-A00:PROG 155648   the .PICT -> PLANC source generator
INTERF:NRF          34679   ND-500 runtime
SUM:PICT              335   a real screen-picture definition
SUM:SYMB              319   the program that consumes it
SCREEN:SYMB           926   the interface
INTERF-1B:BRF       34666   ND-100 1-bank runtime
INTERF-2B:BRF       35134   ND-100 2-bank runtime
```

---

## 6. Building and running

```
@MODE TESTUI:MODE,,
```

then, in this order, because each step lies in its own way:

1. **Read `LIST-ENTRIES-UNDEFINED`'s output.** An undefined entry does **not** fail the build —
   BRF-LINKER still writes a runnable `TESTUI:PROG` that then misbehaves in ways that look like
   a bug in the program.
2. **Fetch `TESTUI:LIST` back and check two things**: no line starting `***`, and that the
   listing **reaches the last source line**. A compile that stopped two thirds of the way
   through reports no errors at all for the third it never read.
3. **Do not trust the `0 DIAGNOSTICS` on screen** — on a long source the real count scrolls off,
   and the zero at the bottom belongs to the linker, sitting happily under a failed compile.
4. `@TESTUI`

### Before it will draw anything

The terminal needs a type VTM understands: `@QGET-TERMINAL-TYPE` to read, `@QSET-TERMINAL-TYPE`
to set. `0` means unset. **Not yet checked on D100.**

---

## 7. Status — what is proved and what is not

## 7a. Build result — it COMPILES, it does NOT link

Run on D100 2026-08-24 with `@MODE TESTUI:MODE,,`:

```
    212 LINES COMPILED. (PARITY ERRORS)       0 DIAGNOSTICS.
...
Brl: LIST-ENTRIES-UNDEFINED
VTEXIT....3722 U  VTCREC...11276 U  VTWBUF...20346 U  VTPCUR...20564 U
VTWRIT...20710 U  VTBREAD..11271 U  VTINIT...21020 U  VTINFO...21021 U
VTDBUF...21026 U
Brl message: Undefined entries
```

**The compile is clean.** The link is not: nine `VT*` entries are undefined, so **the open
question in section 3 is now ANSWERED - a VTM library really must be linked**, and
`DDBTABLES:VTM` (which IS on D100) is terminal DATA that defines none of those symbols.

An undefined entry does not fail the build, so `TESTUI:PROG` exists. **It has not been run, and
running it would prove nothing** - the skill's own rule is that a program built over an undefined
entry runs and misbehaves in ways that look like a bug in the program.

### Where the VTM library is

**No standalone VTM product exists** - the whole 1102-floppy archive catalogue was searched. The
`VTM-*:BRF` code ships with **FOCUS, `ND-10188`**:

```
volume ND-10188E-PART3   md5 028462b0f121
  norskdata-software-archive/images/028462b0f1219d922f41f2761694208f/NDDISK28.img.gz

  VTM-R-D:BRF            28052   the routines
  VTM-DATA-D:BRF          3416
  VTM-CPOS-D:BRF           694
  VTM-CPAR-D:BRF          1325
  VTM-1B-ARRAY-D-C:BRF    9585   1-bank terminal-type arrays (data)
  DDBTABLES-D-C:VTM       9458
```

**Which BRF defines which symbol is NOT established.** BRF symbol names are packed, so a text
grep finds nothing - only string literals are readable (that is how the attribute vocabulary in
section 4 was recovered). Ask the machine: `@BRF-LINKER-C01` then `LIST-BRF-ENTRIES VTM-R-D,,`.

**The vendor names do not fit the transfer** - `VTM-DATA-D:BRF` is 16 with quotes against the
ceiling of 13. Carry them as `VTMR:BRF`, `VTMDATA:BRF`, `VTMCPOS:BRF`, `VTMCPAR:BRF`,
`VTMARR:BRF` and match the `LOAD` lines.

---

**Proved:**
- The floppy exists, all eight files extract, and the two we need are staged here byte-exact.
- The interface and every parameter order in §4 come from a real vendor caller, not inference.
- `TESTUI.PLNC` passes `planc-lint.py` clean — and the linter still catches a deliberately
  introduced typo of each class, so "clean" means something.
- The prerequisites `MON-CALL-1B-A00:BRF` and `PLANC-1BANK-F00:BRF` are on D100.
- The fallback script's live-machine guard works: it refused, with all three machines up, and
  wrote nothing.
- `deploy-over-xmsg.ps1` staged all four files into `SINTRAN/XMSG/sync-out`, and its
  13-character guard was proved to fire by feeding it the vendor's `INTERF-1B:BRF`.

**NOT proved — nobody should read this folder as a working build:**
- ~~`TESTUI.PLNC` has never been through a compiler.~~ **It has: 212 lines, 0 diagnostics.**
  What it has NOT done is link or run - see 7a.
- ~~Nothing has reached D100.~~ **All four files are on D100**, carried over XMSG with the
  machines running and nothing stopped. Byte counts verified on the machine: `TESTUI:PLNC` 9126,
  `INTRF1B:BRF` 34666 - both exact against the repo.
- ~~Whether a VTM BRF must be linked is unknown.~~ **ANSWERED: it must** (7a). Which of the four
  FOCUS `VTM-*:BRF` files supply the nine symbols is the part still open.
- **Whether D100's terminal type is set is unknown** (§6).
- `frame`'s trailing `BYTES` is used the way the demo uses it; the full meaning of every
  attribute keyword is not independently confirmed.

---

## See also

- `Developer/Languages/Application/PLANC-UI-VTM-GUIDE.md` — the how-to page
- `Developer/Workflow/PLANC-VTM-UI-CATALOG.md` — the comparison of all five routes
- `Developer/Workflow/VTM-TERMINAL-INTERFACES.md` — the VTM layer underneath
- `Installation/Software/ND-PLANC-SCREEN-H/README.md` — floppy provenance
- skill `planc` section 10 — the short form of all of the above
