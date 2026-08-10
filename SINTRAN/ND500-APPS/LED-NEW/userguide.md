# LED-NEW - LED full-screen screen editor (B03)

## Overview

LED-NEW is the Norsk Data LED screen editor - a full-screen, VT-style
multi-window text editor with an integrated source-level debugger. [from disasm]
It is interactive and character-mapped: it drives a real terminal (windows,
marked areas/regions, tabulators, syntax-check "language mode"), so it cannot be
usefully driven by a simple line-oriented pipe. [from disasm]

Embedded UI strings confirm the feature set: multiple windows
(`KEY COMMANDS IN WINDOW MODE`), marked regions
(`Region  Lines Changed File-name`), browsing mode
(`Not allowed in browsing mode.`), and a debugger that connects to a running
process (`Connecting to a process.`, `Region not connected to a process.`),
ending with `<Exit LED Editor>`. [from disasm]

The editor loads. [verified] (load-sweep 2026-07-31 in nd500x)

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `LED-NEW.DOM` - the runnable ND-500 domain (the launcher/editor domain).
  [verified]
- `LED-B03.PSEG` / `LED-B03.DSEG` - the editor engine image (program segment +
  data segment), revision B03. [verified]
- `LED-DEBUGGER-B03.PSEG` / `LED-DEBUGGER-B03.DSEG` - the integrated debugger
  image (program + data segment), revision B03. [verified]

Keep all five files together in the same directory as the DOM. [from README]

## Requirements

- The DOM plus its four PSEG/DSEG segment files (above). [verified] (they ship
  together and the editor loads its engine/debugger from them)
- A terminal capable of full-screen operation (cursor addressing, function
  keys). [from disasm]
- Install: copy all of `files/*` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
LED-NEW
```

A file name can normally follow the name to open a file. [UNVERIFIED - the exact
argument syntax is not confirmed.]

Scripted drive is possible only to launch it; you cannot meaningfully edit over
a pipe because it is a full-screen program:

```
printf 'LOGIN GUEST\nLED-NEW\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

To actually use it you need an interactive terminal session, not a printf feed.
[from disasm]

## Commands and options

LED is key-driven, not command-driven; commands are function-key and control-key
combinations, plus a few named file commands. The following are read from the
DOM strings and are NOT run-verified. [from disasm] / [UNVERIFIED]

Named commands seen in the DOM: `WHERE-IS-FILE`, `READ-ALL-FILES`,
`WP-PRINTERS-LSYMB`. [from disasm]

Key handling seen in the DOM: keys can be re-bound (`Bound to key: Ctrl Func
F/C Space Del`, `Key already bound! Abort (Y/N)?`); there is a browsing mode, a
region/marked-area system, and value inspection that shows `Ascii:`, `Hex:`,
`Octal:` for a character. [from disasm]

The full function-key / control-key table is NOT reproduced here because it is
not verified for LED-NEW specifically; see the sibling
[../LED-FORTRAN-A01/userguide.md](../LED-FORTRAN-A01/userguide.md), whose DOM
embeds the same LED "WINDOW KEY COMMANDS" table in full, for the LED key map (it
is the same editor with a language mode added). Treat that table as
[from disasm], not verified.

## Verified behaviour in nd500x

- Loads. [verified] (load-sweep 2026-07-31)
- No interactive edit/debug session has been driven in nd500x. [verified]

## Known issues / status

- Full-screen interactive editor: needs a real terminal; not drivable by pipe.
  [from disasm]
- No `analysis/*.asm` disassembly is present for LED-NEW; the notes above come
  from strings inside the DOM. [verified] (analysis/ is empty)
- Command/key map is [UNVERIFIED] for this binary.

## References

- Shared conventions: [../README.md](../README.md)
- Related editor+compiler: [../LED-FORTRAN-A01/userguide.md](../LED-FORTRAN-A01/userguide.md)
- Runnable domain: [files/LED-NEW.DOM](files/LED-NEW.DOM)
- Editor segments: [files/LED-B03.PSEG](files/LED-B03.PSEG),
  [files/LED-B03.DSEG](files/LED-B03.DSEG)
- Debugger segments: [files/LED-DEBUGGER-B03.PSEG](files/LED-DEBUGGER-B03.PSEG),
  [files/LED-DEBUGGER-B03.DSEG](files/LED-DEBUGGER-B03.DSEG)
