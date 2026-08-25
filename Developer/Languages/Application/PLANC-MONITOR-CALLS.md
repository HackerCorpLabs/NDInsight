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

### THE COMPLETE SET - 54 CALLS, from ND-60.117.5

**This page previously carried a 27-entry list and the `planc` skill carried a different 30-entry
one. BOTH WERE TRUNCATED COPIES** of the manual's section (ND-60.117.5, "A number of SINTRAN
monitor call routines are available to be called from PLANC, provided as part of the PLANC runtime
system"). The real set, extracted from every `MONn` heading in that section:

| | | | | | |
|---|---|---|---|---|---|
| 0 LEAVE | 1 INBIT | 2 OUTBIT | 3 ECHOM | 4 BRKM | 11 TIME |
| 12 SETCM | 13 CIBUF | 14 COBUF | 16 MGTIY | 17 MSTIY | 21 M8INB |
| 22 W8OUT | 24 B8OUT | 30 GETRT | 32 MSG | 41 ROBJE | **43 CLOSE** |
| 44 RUSER | 45 DBRK | 47 SBRK | **50 OPEN** | **54 MDLFI** | 62 RMAX |
| 63 BAINV | 64 ERMSG | 65 QERMSG | 66 ISIZE | **70 COMND** | 71 DESCF |
| 72 EESCF | 73 SMAX | 74 SETBT | 75 REABT | 76 SETBS | 104 HOLD |
| 113 CLOCK | 114 TUSED | 117 RFILE | 120 WFILE | 122 RESRV | 123 RELES |
| 132 MCALL | 141 IOSET | 143 RSIO | 144 MACTP | 161 INSTR | 162 OUTST |
| 167 REKNT | 263 GDEVT | 310 T8INB | 312 MOINF | 412 FSCNT | 413 FSDCNT |

**THE MANUAL'S LIST IS ACCURATE AND PREDICTIVE - it was our copy that was broken.** Checked
against the linker on D100 2026-08-19:

 - `MON43` (CLOSE) and `MON70` (COMND) **are in it, and both link** - the earlier claim that they
   were "absent from the documented list" came from the truncated copy, not from the manual;
 - `MON221` (CreateFile), `MON317` (ExecuteCommand) and `MON307` (TNOWAIT) **are NOT in it, and
   none of them link**.

Four for four. **So use this list - and the linker is confirmation, not a substitute.** A call
outside the set has no `MONn` routine: write an interface routine and **load it before the PLANC
runtime library** (section 7), or link `MON-CALL-1BANK:BRF` and use `MONITOR_CALL` (section 2),
which reaches every call rather than these 54.

**And confirm at link time anyway** - `LIST-ENTRIES-UNDEFINED` costs nothing and catches a
mis-typed number or a machine whose runtime differs. Measured on D100 2026-08-19:

| Call | In the manual's list? | Linker |
|---|---|---|
| `MON43` CloseFile | yes | **resolves** |
| `MON70` CallCommand | yes | **resolves** |
| `MON221` CreateFile | no | **`MON221...10101 U`** |
| `MON317` ExecuteCommand | no | **`MON317...10020 U`** |

**A PLANC program therefore cannot create a file directly on D100.** The routes are
`MON-CALL-1BANK:BRF` with `Monitor_Call`, the command processor via `MON70` (section 5), or a MAC
interface routine for 221.

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

### THE PREREQUISITE: `MON-CALL-1BANK:BRF`, AND IT IS A LINK-TIME LIBRARY

**`MONITOR_CALL` is not built into the PLANC runtime. It needs a library that is not loaded by
default**, which is the whole reason both forms fail above. From `Developer/MON/Monitor Calls.md`,
in its own PLANC worked example:

> *"You need the library file `MON-CALL-1BANK:BRF`. It provides the monitor call routines."*

```
@PLANC-100
*COMPILE EX-PROG:SYMB, "EX-PROG:LIST", "EX-PROG:BRF"
*EXIT

@BRF-LINKER
Br1: PROGRAM-FILE "EX-PROG:PROG"
Br1: LOAD EX-PROG:BRF, MON-CALL-1BANK:BRF, PLANC-1BANK:BRF
Br1: EXIT
```

**The ORDER is the point: your program, then `MON-CALL-1BANK`, THEN the PLANC runtime.** Same rule
as any hand-written interface routine (section 7) - the thing that satisfies a reference is loaded
after the thing that makes it and before the runtime.

For a program built with `SEPARATE-DATA ON` (more than 128 KB), use the two-bank pair instead:
**`MON-CALL-2BANK:BRF`** with `PLANC-2BANK:BRF`.

**So, by form:**

| Form | Needs | When |
|---|---|---|
| `MONITOR_CALL(221B, ...)` - by NUMBER | **`MON-CALL-1BANK:BRF`** linked | link time |
| `MONITOR_CALL('CreateFile', ...)` - by NAME | the above **plus a `MON-CALL-NAMES` file** | compile time AND link time |

**NEITHER IS INSTALLED ON D100** - `@LIST-FILES MON-CALL,` under SYSTEM returns nothing, and the
compiler answers `THE MON-CALL-NAMES FILE WAS NOT FOUND`. That is why this repo's programs use the
`MONn` routines (section 1), which need no extra library at all. If you install
`MON-CALL-1BANK:BRF`, `MONITOR_CALL` is the tidier interface and gives you every call rather than
the `MONn` subset - but add it to the build's LOAD line and confirm with
`LIST-ENTRIES-UNDEFINED` before relying on it.

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

## 4. File I/O from PLANC - the four calls, and the two that are easy to swap

Measured on D100 and used in anger by the chat client:

| Call | `MONn` | What it does | Watch out for |
|---|---|---|---|
| `OpenFile` | **`MON50(name, defaultType, access)`** | opens, returns the file number | **RAISES rather than returning a status** - guard it, and read the number from the assignment |
| `InByte` | `MON1(dev)` | reads one byte | returns `ErrCode 3` in no-wait mode, which means "nothing there", not a fault |
| `OutByte` | `MON2(dev)` | writes one byte | in-VALUE form: `b MON2(dev)` |
| `CloseFile` | **`MON43(dev)`** | closes | see the pair below |
| `DeleteFile` | **`MON54(name, type)`** | **DELETES the file** | see the pair below |

**`MON43` IS CLOSE. `MON54` IS DELETE.** They sit close together in the numbering, both take a file,
and one of them destroys data. Getting them the wrong way round is silent until the file is gone.

**Access codes for `MON50`:** `0` = sequential write, `1` = sequential read.

**A file opened for sequential write is truncated at CLOSE, not at OPEN.** So a program that opens
and never closes leaves the OLD contents in place and the save silently does nothing - close on every
path, including after a failed write.

**An unqualified name resolves against whoever is logged in**, which is how a per-user config file
costs nothing: nothing in the source names a user, and two users each get their own.

## 5. Running a SINTRAN COMMAND from a program - the buffer is a fixed size

Some things have no monitor call of their own. **Creating a file is the common one**: `CreateFile`
is `221B` and is NOT in the `MONn` set above, so a PLANC program cannot call it. The way round is to
hand SINTRAN a command line exactly as if it had been typed.

There are two calls that do it, and the manual is clear about which to prefer:

| | Number | PLANC buffer | On error |
|---|---|---|---|
| `CallCommand` (COMND) | `70B` | **`BYTES : Command(0:79)`** | **terminates your program** |
| `ExecuteCommand` (UECOM) | `317B` | **`BYTES : Command(0:35)`** | prints a message, program continues |

**The manual says to use `ExecuteCommand`** - under CallCommand: *"You are advised to use the newer
monitor call, ExecuteCommand (UECOM), instead of CallCommand"* and *"the program may terminate if an
error occurs. Use ExecuteCommand (UECOM) to avoid this problem."*

**BUT `MON317` IS NOT AVAILABLE ON D100 - TESTED 2026-08-19, AND THIS PAGE RECOMMENDED IT BEFORE
CHECKING.** Importing it and calling it compiles cleanly and then fails at link:

```
Brl: LIST-ENTRIES-UNDEFINED
MON317...10020 U
```

So on this installation the choice is `MON70` or a hand-written MAC interface routine loaded ahead
of `PLANC-1BANK-F00`. Check your own machine the same way before designing around UECOM.

**AND THE BUILD DOES NOT FAIL ON AN UNDEFINED ENTRY.** `BRF-LINKER` wrote a runnable `CHAT:PROG`
with `MON317` still undefined; `@CHAT` started it and it misbehaved in a way that looked like
evidence about the file creation, and was evidence about nothing. **`LIST-ENTRIES-UNDEFINED` is in
the build recipe for exactly this, and its output has to be READ** - nothing else fails, and a
program built over an undefined entry runs and lies to you.

### The four rules that are easy to get wrong

**1. THE BUFFER IS A FIXED SIZE, NOT A STRING.** The manual gives the PLANC form as
`BYTES : Command(0:79)` for CallCommand - an eighty-byte ARRAY. A PLANC `BYTES` argument carries its
own length, so passing a short literal passes a short buffer where the call expects a full one.
Copy the text into a full-size array and pad the rest:

```planc
BYTES : cmdBuf(0:79)
BYTES : spaceChar := ' '
...
FOR i IN 1:80 DO  spaceChar(0) =: cmdBuf(i - 1)  ENDFOR
FOR i IN 1:cmdLen DO  cmdText(i - 1) =: cmdBuf(i - 1)  ENDFOR
MON70(cmdBuf)
```

**2. THE COMMAND IS TERMINATED BY AN APOSTROPHE.** The manual's own ASSEMBLY-500 example is

```
Command : STRINGDATA 'CLOSE-FILE 102'''
```

- that trailing triple apostrophe is the closing quote plus ONE apostrophe character, so the command
handed to SINTRAN is `CLOSE-FILE 102'`. In PLANC an apostrophe inside a literal is doubled, so write
`'CREATE-FILE MY:FILE,1'''`.

**3. NO `@`, AND A LOCAL FILESPEC TAKES NO QUOTES.** The manual says not to include the `@`.
And - MEASURED on D100 2026-08-19 - quoting a LOCAL name is fatal:

```
@CREATE-FILE "CHAT:CNFG",1
ILLEGAL CHARACTER IN PARAMETER
@CREATE-FILE CHAT:CNFG,1
@
```

**This is a genuine trap because the two routes take OPPOSITE quoting.** To `MON50` (OpenFile),
quoting the name means *create it if absent*; to the `CREATE-FILE` command, a quote is an illegal
character. A quoted name copied from one into the other fails every time. Quoting is right only for
a REMOTE filespec, which goes to another machine's command processor.

**4. TWO DEPENDENT COMMANDS NEED A WAIT BETWEEN THEM.** The manual names exactly the common case:
*"Use SuspendProgram to wait a specified time interval between two CallCommands which depend on each
other, e.g. CreateFile and OpenFile."* So create-then-open is a documented race, not a safe sequence.

### NOT RESOLVED: creating a file this way, from our own program

**Stated plainly because it is still open.** In the chat client on D100, creating `CHAT:CNFG` through
`MON70` does not work. Three separate defects were found and fixed - the quotes, the missing
apostrophe terminator, and the 25-byte buffer where eighty is required - and **each was real, and
none of them changed the outcome**: the file is still not created and the save still reports failure.

What IS established by measurement:

 - the WRITE path is sound. Create the file by hand with `@CREATE-FILE CHAT:CNFG,1` and the same
   program saves to it, and reads it back on the next run;
 - so the fault is confined to the create, not to `MON50`/`MON2`/`MON43`;
 - **`70B` is not in the `MONn` set** listed in section 1, which was measured on this machine. The
   link resolves `MON70` and reports no undefined entries, so something provides it - but it is
   outside the documented set and its behaviour is not guaranteed.

**The untried next steps, in order:** print `dev` and `ErrCode` around the calls rather than guessing
again; then `ExecuteCommand` (317B), which reports errors instead of swallowing them; then a wait
between create and open, per rule 4.

## 6. Finding the correct name, number and parameters

Look the call up in one of these - in preference order:

1. **The repo's MON hub, [`Developer/MON/`](../../MON/README.md) - LOOK HERE FIRST.**
   [`calls/`](../../MON/calls) holds **one YAML per call**, named `<octal>B_<Name>.yaml` (e.g.
   `221B_CreateFile.yaml`), carrying the number, the short name, every parameter with its type and
   direction, the error codes, **and a worked example in each language including PLANC**.
   [`ND MON Calls.md`](../../MON/ND%20MON%20Calls.md) is the generated catalogue of all 230.

   **This is faster and more complete than the manual** - the CreateFile YAML gives the PLANC
   declaration, the meaning of each parameter, `62` = file already exists, and notes on what the
   emulator does and does not model. Going to the manual first cost several build cycles on D100
   that reading `221B_CreateFile.yaml` would have saved.

   **CAVEAT: the language examples in the YAML are AI-generated and may be wrong.** Treat them as a
   strong hint, not as authority - check anything load-bearing against the manual. A real
   disagreement is already known: `221B_CreateFile.yaml` types both size parameters as `INTEGER2`
   in its table, while the MANUAL's own PLANC, FORTRAN (`INTEGER*4`) and COBOL (`S9(10)`) forms all
   say four bytes and its MAC example calls the size "a double word".
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

## 7. Libraries and link ORDER - the part that breaks builds

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

## 8. A worked example (ND-100, 1 bank) - the way that links on D100

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

Build (the `MONn` routines come from the runtime on D100, section 7):
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
