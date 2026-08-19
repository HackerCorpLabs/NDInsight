# Doing SINTRAN monitor calls from PLANC

How to issue a SINTRAN III monitor (MON) call from a PLANC program: the way that actually
works on a real machine (the named `MONn` routines), why the manual's `MONITOR_CALL` does not
work on D100, how to find a call's number and parameters, error handling, and - the part that
trips builds - **which libraries to load and in what order**. Every rule here is quoted from a
manual in this repo or marked as measured on a real machine.

**Sources:** `Developer/MON/Monitor Calls.md` (the SINTRAN III Monitor Calls guide, ND-860228),
`Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md` (Appendix E "Using SINTRAN Monitor
Calls" and the `MONITOR_CALL` standard-routine page), and
`Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md` (call names). Where the
manual and a real machine disagree, this page says so - and on D100 they disagree about
`MONITOR_CALL`.

---

## 1. The way that works: the named `MONn` routines

**On a real SINTRAN III machine (measured on D100 2026-08-17) the reliable way to issue a MON
call from PLANC is the pre-declared `MONn` routines - NOT `MONITOR_CALL` (section 2 says why).**

The PLANC runtime provides one ordinary routine per supported monitor call, named `MONn` where
`n` is the call number, which you `IMPORT` and call like anything else. PLANC Reference Manual
Appendix E section 0.2 gives the exact signature of each:

```planc
IMPORT (ROUTINE VOID, BYTE (INTEGER) : MON1)     % INBT  - read a byte from a device
IMPORT (ROUTINE BYTE, VOID (INTEGER) : MON2)     % OUTBT - write a byte to a device
IMPORT (ROUTINE VOID, VOID           : MON0)     % LEAVE
...
INTEGER : dev := 1
BYTE : ch
MON1(dev) =: ch
```

(The COSMOS guide's sample line `IMPORT (ROUTINE VOID,VOID: MONO)` has always been exactly
this - `MON0`.) On D100 these are the `MN1`, `MN122`, `MN310`... units carried inside
`PLANC-1BANK-F00:BRF` itself, so the runtime already provides them.

**The set is limited.** Only these monitor calls have a `MONn` routine on D100 (App E 0.2 /
measured): **0, 1, 2, 3, 4, 11, 12, 17, 21, 45, 47, 50, 54, 62, 63, 64, 65, 104, 113, 114,
117, 120, 144, 161, 162, 167, 263.** A call outside this set - `TNOWAIT` (307B) is a common
example - has no `MONn` routine, and you must write your own interface routine for it and
**load it before the PLANC runtime library** (section 5).

## 2. `MONITOR_CALL` - documented by the manual, but it FAILS on D100

The PLANC Reference Manual (p.200) documents a `MONITOR_CALL` standard routine that takes the
call as a number (`INTEGER` first parameter) or a name (`BYTES` constant, "as found in the
SINTRAN III Reference Manual ND-60.128"), e.g. `MONITOR_CALL(164B, ErrCode)` or
`MONITOR_CALL('ERMSG', ErrCode)`. **Do not rely on it - both forms were measured to fail on
D100 (SINTRAN VSX/500 K, PLANC-100-F00):**

- **By NAME** (`MONITOR_CALL('InByte', ...)`, the form the Monitor Calls manual prints for
  PLANC) - the compiler needs a **MON-CALL-NAMES** file to resolve the mnemonic, and it exists
  under none of D100's users. The compile stops with `THE MON-CALL-NAMES FILE WAS NOT FOUND`.
- **By NUMBER** (`MONITOR_CALL(1B, ...)`) - compiles cleanly but **will not link**: it
  generates a reference to the runtime entry `5MON_P`, which is in **neither**
  `PLANC-1BANK-F00` nor `PLANC-2BANK-F00`, so the link ends with `5MON_P ... U` undefined.

So on this installation `MONITOR_CALL` is a dead end in both forms; use the `MONn` routines
(section 1). If you meet a machine whose PLANC runtime does contain `5MON_P` and a
MON-CALL-NAMES file, `MONITOR_CALL` is the tidier interface - but verify it links before
building on it.

## 3. Rules that apply to every MON call from PLANC

From `Developer/MON/Monitor Calls.md` (verbatim points), these hold however you issue the call:

- **Errors ERRETURN.** A monitor-call error code other than 0 causes an ERRETURN - the
  enclosing `ON ROUTINEERROR DO ... ENDON` runs. Guard every call that can fail:
  ```planc
  ON ROUTINEERROR DO
      IF ErrCode > 0 THEN ... ENDIF
  ENDON
  MON1(dev) =: ch
  ```
- **`ErrCode` is automatic - do NOT declare it.** It is an INTEGER variable the runtime fills
  with the error code; read it like any variable. Declaring it is an error.
- **`INTEGER4` parameters cannot be passed as constants** - put the value in a variable first.
- Complicated calls may take a **RECORD** instead of a string (e.g. an RT-description block).

The parity/no-wait traps that bite terminal MON calls (`InByte` returns ErrCode 3 in no-wait
mode, terminal input carries even parity so CR arrives as 141 not 13) are documented with the
XMSG field notes in [PLANC-XMSG-COMMUNICATION.md](PLANC-XMSG-COMMUNICATION.md).

## 4. Finding the correct name, number and parameters

Look the call up in one of these - in preference order:

1. **The repo's MON hub, `Developer/MON/`** - the fastest local reference. `ND MON Calls.md`
   is the generated, indexed catalog of all **230** monitor calls; `calls/` holds one YAML
   per call (name, number, registers, parameters). Start here to get a call's number, its
   mnemonic name, and its parameter list in one place.
2. **`Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md`** - the vendor Monitor
   Calls manual (the authority the PLANC manual points to for "details").
3. **PLANC Reference Manual Appendix E section 0.2** - the list of monitor calls that have a
   `MONn` routine, each with its exact PLANC signature (in-value / out-value / parameter
   types). Since the `MONn` routine IS the interface you call (section 1), this is the one you
   actually need to write the call.
4. **`Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference Manual.md`** - the vendor mnemonic
   short names (only relevant if you have a machine where the `MONITOR_CALL` name form works).

The number is the identifier that matters for the `MONn` routines: the call number `n` names
the routine (`MON1` = call 1 = INBT). The mnemonic (`INBT`, `SETBT`) and the PLANC name
(`GetStartByte`) are only used by the `MONITOR_CALL` name form, which does not work on D100
(section 2).

## 5. Libraries and link ORDER - the part that breaks builds

A PLANC program that uses monitor calls needs the **monitor-call library** in addition to the
PLANC runtime, and they must be loaded in the right order. This is the worked recipe from the
Monitor Calls guide, verbatim (ND-100, single bank):

```
@PLANC-100
  *COMPILE EX-PROG:SYMB, "EX-PROG:LIST", "EX-PROG:BRF"
  *EXIT
@BRF-LINKER
  Brl: PROGRAM-FILE "EX-PROG:PROG"
  Brl: LOAD EX-PROG:BRF, MON-CALL-1BANK:BRF, PLANC-1BANK:BRF
  Brl: EXIT
@EX-PROG
```

**The order is: your program first, then the MON-CALL library, then the PLANC runtime.**

| Target | Monitor-call library | PLANC runtime | Extra step |
|---|---|---|---|
| ND-100, 1 bank | `MON-CALL-1BANK:BRF` | `PLANC-1BANK:BRF` | - |
| ND-100, 2 bank (>128 KB, or `SEPARATE-DATA ON`) | `MON-CALL-2BANK:BRF` | `PLANC-2BANK:BRF` | give `SEPARATE-DATA ON` before `COMPILE` |
| ND-500 | `MON-CALL-LIB:NRF` | (PLANC-500 runtime) | link with NLL |

(On this repo's machines the runtime files carry a revision suffix, e.g.
`PLANC-1BANK-F00:BRF`.)

**Why the order matters, stated as a rule** (PLANC Ref Manual App E 0.1): a monitor call not
in the built-in list "may be called from a PLANC routine if a suitable interface routine is
constructed by the user. If this is done, **the user must load the interface routine before
the PLANC runtime library.**" Same principle for the standard MON-CALL library: it is
searched to satisfy the references your program made, so it comes after the program and before
the runtime that ultimately closes everything out. Loading a library before there is anything
to satisfy resolves nothing (it is accepted in silence - the worst kind of wrong).

**On D100 the `MONn` routines come from the runtime itself.** Measured: they are `MN1`,
`MN122`... units inside `PLANC-1BANK-F00:BRF`, so a program that uses only the section-1
`MONn` routines links with just the PLANC runtime - `MON-CALL-1BANK` was not needed (and was
not found on the machine). The manual's generic recipe still loads `MON-CALL-1BANK:BRF`
first; keep it in the load line if present, it is harmless, but the reason the build works on
D100 is the runtime, not that library.

**And `MON-CALL-1BANK:BRF` is where `5MON_P` would have come from** - which is the whole reason
`MONITOR_CALL` by number does not link here. It is not a redundant extra in the manual's recipe:
it carries the generic monitor-call machinery that the `MONITOR_CALL` standard routine compiles
into, and the `MONn` routines simply do not need it. `LIST-FILES MON-CALL,,` on D100 returns
nothing, so both halves of the picture are consistent - no library, no `5MON_P`, no
`MONITOR_CALL`. Installing that library is what would make section 2's forms usable.

**A call outside the available set** (section 1 - e.g. `TNOWAIT` 307B) has no `MONn` routine,
so you write your own interface routine (a small MAC/`$*`-assembly or NPL stub that issues the
raw `MON` and returns its status) and, per App E 0.1, **load your interface routine BEFORE the
PLANC runtime library** - program, then your interface routine, then `PLANC-1BANK`.

**On D100 that route is currently closed too**: `LIST-FILES MAC,,` finds only `MACM-AREA:DATA`
and `LIST-FILES NPL,,` finds nothing, so there is no assembler on the machine to build the stub
with. A program that needs a call outside the set has to get one installed - or be designed not
to need it. `CHAT:PLNC` took the second road: it drains its XMSG port with `xmpfrcv` flags 0,
which is XMSG's OWN no-wait and needs nothing from the terminal, and lets the keyboard block.

**The two-step compile-then-link caveat.** If you set `PROG-FILE`/`PROGRAM-FILE` and let the
compiler link, that works for simple cases. To control library order do it in two steps:
`COMPILE` to a real `:BRF` with no `PROG-FILE`, then in `BRF-LINKER` `LOAD` the program, any
interface/MON-CALL library, and the runtime in order. (Setting `PROG-FILE` makes the compiler
link into the program file and can leave the `:BRF` empty.) `LIBRARY-MODE ON` before loading a
library makes it contribute only referenced units.

## 6. A worked example (ND-100, 1 bank) - the way that links on D100

Uses `MON62` (call 62B, `SETBT`/set byte pointer family) via the named-routine form, and
`MON1` (INBT) - both in the available set:

```planc
MODULE example
    IMPORT (ROUTINE VOID, BYTE (INTEGER) : MON1)     % INBT - read a byte
    INTEGER ARRAY : stack(0:200)
    INTEGER : dev := 1
    BYTE : ch

    PROGRAM : start
        INISTACK stack
        ON ROUTINEERROR DO
            IF ErrCode > 0 THEN
                OUTPUT(1, 'AL30', 'MON call failed$')
            ENDIF
            RETURN
        ENDON
        MON1(dev) =: ch                              % read one byte from the terminal
    ENDROUTINE
ENDMODULE
```

Build (the `MONn` routines come from the runtime on D100, section 5):
```
@PLANC-100-F00
COMPILE EXAMPLE:PLNC,"EXAMPLE:LIST","EXAMPLE"
EXIT
@BRF-LINKER-C01
PROGRAM-FILE "EXAMPLE"
LOAD EXAMPLE
LIBRARY-MODE ON
LOAD PLANC-1BANK-F00
LIST-ENTRIES-UNDEFINED
EXIT
```

`LIST-ENTRIES-UNDEFINED` should report nothing left. A leftover `MN1`/`MNnn` unit means the
PLANC runtime was not loaded (or a call outside the available set was used - see section 1). A
leftover `5MON_P` means you used `MONITOR_CALL` by number - remove it and use the `MONn`
routine instead (section 2).

## See also

- [PLANC-DEVELOPER-GUIDE.md](PLANC-DEVELOPER-GUIDE.md) - the language and compiler
- `Developer/MON/README.md` + `Developer/MON/ND MON Calls.md` - the 230-call MON hub
- [COSMOS-XMP-LIBRARY.md](COSMOS-XMP-LIBRARY.md) - the special case of MON 200B (XMSG), which
  PLANC cannot issue directly and reaches through the COSMOS XMP library instead
- `Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md` - `MONITOR_CALL` (p.200),
  Appendix E (the per-call PLANC signatures)
- `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md` - the Monitor Calls manual
