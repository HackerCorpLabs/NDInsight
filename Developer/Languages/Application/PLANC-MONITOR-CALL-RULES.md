# PLANC monitor-call rules - what a linter must check

Companion to [`monitor-calls.json`](monitor-calls.json), the machine-readable table of all 258
SINTRAN III monitor calls listed in the vendor manual **ND-860228.2 EN SINTRAN III Monitor
Calls**. This page holds the rules that do not fit in a table: which calls a PLANC program can
reach, in what order the libraries must be loaded, the shape of the error handler, the error
codes worth knowing by name, and the calls whose parameters people keep getting wrong.

**Sources.** Everything here is either quoted from a manual in this repo or marked as measured
on a real machine (D100, SINTRAN III VSX/500 K, PLANC-100-F00). Anything that is neither is
marked **UNVERIFIED**.

- `Reference-Manuals/ND-860228-2-EN SINTRAN III Monitor Calls.md` - the call table, the
  parameters, the error-code appendix.
- `Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md` - Appendix E, "Using SINTRAN
  Monitor Calls": which calls the PLANC runtime supplies and their exact signatures.
- `SINTRAN/XMSG/SINTRAN-CHAT/CHAT.PLNC` and `CHATSV.PLNC` - the programs that hit each of these
  traps in turn and carry the measurement notes.
- Longer prose version: [`PLANC-MONITOR-CALLS.md`](PLANC-MONITOR-CALLS.md).

---

## 1. Two ways to issue a call, and only one of them needs no extra library

### 1.1 The 54 `MONn` routines the PLANC runtime supplies directly

The PLANC runtime carries one ordinary routine per call for **54 calls, and only these 54**.
You `IMPORT` the routine and call it like any other routine. No extra library is needed.

The list, from ND-60.117.5 Appendix E section 0.2, in octal:

```
MON0   MON1   MON2   MON3   MON4   MON11  MON12  MON13  MON14  MON16
MON17  MON21  MON22  MON24  MON30  MON32  MON41  MON43  MON44  MON45
MON47  MON50  MON54  MON62  MON63  MON64  MON65  MON66  MON70  MON71
MON72  MON73  MON74  MON75  MON76  MON104 MON113 MON114 MON117 MON120
MON122 MON123 MON132 MON141 MON143 MON144 MON161 MON162 MON167 MON263
MON310 MON312 MON412 MON413
```

In `monitor-calls.json` this is the `plancRoutine` field: a string like `"MON50"` when the
runtime supplies one, `null` when it does not.

**Linter rule.** `IMPORT (... : MONnnn)` where `MONnnn` is not in this list is an error on a
plain build - the reference will not resolve. Measured on D100: `MON221` and `MON317` both come
out `U` (undefined) from `LIST-ENTRIES-UNDEFINED`, and `MON43` and `MON70`, which are in the
list, both resolve. Four for four.

Three of the 54 are machine specific and the manual says so on their own pages:

| Routine | Only on |
|---|---|
| `MON45`, `MON47`, `MON132`, `MON141`, `MON167`, `MON310`, `MON312` | ND-100 |
| `MON412`, `MON413` | ND-500 |

(`MON21` is on both, but the ND-100 and ND-500 forms take different parameters - the ND-100
form is `(dev, w1, w2, w3, w4, num)` with INTEGER WRITE words, the ND-500 form is
`(dev, num, inbytes)`.)

**The number is the identity.** `MON1` is call `1B` is `INBT` is `InByte`. The short name and
the long name matter only to `MONITOR_CALL`.

### 1.2 `MONITOR_CALL` - reaches every call, but needs a library and a names file

```planc
MONITOR_CALL(221B, nameBuf, startAddr, noOfPages)       % by NUMBER
MONITOR_CALL('CreateFile', nameBuf, startAddr, pages)   % by NAME
MONITOR_CALL('ERMSG', ErrCode)                          % by SHORT NAME
```

| Form | What it needs | When it bites |
|---|---|---|
| by NUMBER | `MON-CALL-1BANK:BRF` linked (it carries the entry `5MON_P`) | link time |
| by NAME or SHORT NAME | the above **plus a MON-CALL-NAMES file** on the machine | compile time **and** link time |

Both forms were measured to fail on a bare D100 before the MON-CALL package was installed:
by name the compiler answered `THE MON-CALL-NAMES FILE WAS NOT FOUND`, and by number the link
ended `5MON_P ... U`. With `MON-CALL-1B-A00` installed, both work - `CHATSV.PLNC` uses
`MONITOR_CALL(221B, ...)` and `MONITOR_CALL('TimeOut', ...)` on that machine today.

**Linter rule.** A file that uses `MONITOR_CALL` in any form must have `MON-CALL-1BANK:BRF`
(or `MON-CALL-2BANK:BRF`, or `MON-CALL-LIB:NRF` on the ND-500) on its `LOAD` line. A file
that uses the name form additionally depends on the MON-CALL-NAMES file, which is a property
of the machine, not of the source - the linter can only warn.

**The name to use is the `longName` key in `monitor-calls.json`.** Where the manual prints
two spellings for the same call, the JSON `notes` field says so and the entry is marked
UNVERIFIED - `HDLCFunction` / `HDLFunction` is the only real one.

---

## 2. LOAD ORDER - your program, then MON-CALL, then the PLANC runtime

Verbatim from the Monitor Calls manual's own PLANC worked example (ND-100, one bank):

```
@PLANC-100
  *COMPILE EX-PROG:SYMB, "EX-PROG:LIST", "EX-PROG:BRF"
  *EXIT
@BRF-LINKER
  Br1: PROGRAM-FILE "EX-PROG:PROG"
  Br1: LOAD EX-PROG:BRF, MON-CALL-1BANK:BRF, PLANC-1BANK:BRF
  Br1: EXIT
@EX-PROG
```

| Target | Monitor-call library | PLANC runtime | Extra |
|---|---|---|---|
| ND-100, one bank | `MON-CALL-1BANK:BRF` | `PLANC-1BANK:BRF` | - |
| ND-100, two bank (over 128 KB) | `MON-CALL-2BANK:BRF` | `PLANC-2BANK:BRF` | `SEPARATE-DATA ON` before `COMPILE` |
| ND-500 | `MON-CALL-LIB:NRF` | PLANC-500 runtime | link with the ND-500 Linkage Loader |

**The rule, and why.** A library is searched to satisfy references that already exist. Loading
it before there is anything to satisfy resolves nothing, and **the linker accepts that in
silence**. The PLANC Reference Manual states the same rule for hand-written interface routines
(Appendix E 0.1): *"the user must load the interface routine before the PLANC runtime library."*
So: the thing that makes a reference goes first, the thing that satisfies it next, the runtime
last.

**What goes wrong if the order is wrong.** Nothing visible. `BRF-LINKER` writes a runnable
`:PROG` with the entry still undefined and says nothing about it. The program then starts and
misbehaves in a way that looks like a bug in your logic. This happened on D100 with `MON317`
left undefined, and the resulting nonsense was mistaken for evidence about file creation.

**Therefore `LIST-ENTRIES-UNDEFINED` is part of the build, and its output has to be READ.**

```
Br1: LIST-ENTRIES-UNDEFINED
```

- a leftover `MNnn` / `MONnn` unit means the PLANC runtime was not loaded, or you used a call
  outside the 54;
- a leftover `5MON_P` means you used `MONITOR_CALL` and did not load `MON-CALL-1BANK`.

**Two-step compile, not one.** If you set `PROG-FILE` and let the compiler link, you cannot
control library order and the `:BRF` may come out empty. Compile to a real `:BRF` with no
`PROG-FILE`, then `LOAD` in `BRF-LINKER` in the order above. `LIBRARY-MODE ON` before a library
makes it contribute only the units actually referenced.

---

## 3. Every call needs `ON ROUTINEERROR ... ENDON`

**A refused monitor call does not return a status. It raises.** From the Monitor Calls manual,
section on PLANC, verbatim:

> *"Error codes different from 0 cause ERRETURN from monitor calls. That is, the current
> ON - ENDON statement is executed."*

> *"Error codes are automatically stored in the variable `ErrCode`. This integer variable
> `ErrCode` can be read as any other variable. It should not be declared."*

**Unhandled, the raise unwinds the WHOLE program.** There is no per-call status to ignore. A
first run of a program that opens a file that does not exist yet dies on the spot unless the
open is guarded.

### 3.1 The correct shape

```planc
ROUTINE VOID, INTEGER : openLog
    INTEGER : dev
    BOOLEAN : openFailed

    0 =: dev
    ON ROUTINEERROR DO
        TRUE =: openFailed
    ENDON
    FALSE =: openFailed          % <-- AFTER the ON block, never before

    MON50(logName, logType, logAppend) =: dev

    IF openFailed THEN
        0 RETURN
    ENDIF
    dev RETURN
ENDROUTINE
```

**Four rules the linter can check:**

1. **The flag is cleared AFTER the `ON ... ENDON` block, not before.** Measured on D100
   2026-08-19: cleared before, the handler body had already run and every open reported failure
   while returning a good device number. An `ON ROUTINEERROR DO ... ENDON` that is *preceded*
   by the initialisation of its own flag is a bug.
2. **One flag per logical operation, not per call.** A handler wrapped round a write loop that
   prints inside the handler prints once per character. Set a flag, test it after the loop.
3. **`ErrCode` is never declared.** `INTEGER : ErrCode` is an error - the runtime supplies it.
4. **Test the flag, not the returned value.** A failed `MON50` leaves a stale number in the
   destination variable (46 has been seen), which reads as a perfectly good device number. Any
   code that tests the returned number instead of the flag is wrong.

**Nesting note.** The handler in force is the innermost enclosing one. To do something and
deliberately ignore any failure, an empty handler is the idiom:

```planc
ON ROUTINEERROR DO
ENDON
makeLog                          % may fail; we test the next open instead
```

### 3.2 Which calls can raise

`canRaise` in the JSON. It is `true` for every call except the three that never return to the
caller at all: `ExitFromProgram` (`0B`), `ExitRTProgram` (`134B`) and `ErrorReturn` (`400B`).

**This is a deliberate conservative default and is marked here as such.** The manual's own
section 2.2 has an ERR column, but in this scan that column is damaged on most rows and cannot
be trusted. The safe lint rule is "guard every call"; a false "you need a handler here" costs
four lines, a missing one costs the program.

---

## 4. Error codes a linter should know

**`ErrCode` is DECIMAL. The manual's tables are OCTAL.** Appendix A of ND-860228.2 prints both
columns side by side; the decimal column is the one that matches `ErrCode`. Getting this
backwards has already produced a "refused push reported success".

The ones worth naming:

| ErrCode (decimal) | Octal | Meaning | Where you meet it |
|---|---|---|---|
| **3** | 003 | End of file / **nothing was typed** | `InByte` on a terminal in no-wait mode. Not a fault - it means "no input yet". A program that treats 3 as fatal cannot poll a keyboard. |
| 2 | 002 | Bad file number | closing or reading a file number you never opened |
| 5 | 005 | Device not reserved | |
| 17 | 021 | Illegal character in parameter | a **quoted local filespec** handed to a SINTRAN command - see section 5 |
| 21 | 025 | You are not authorized to do this | |
| **46** | 056 | **No such file name** | first-run `OpenFile` before the file has been created. The manual's own PLANC example tests exactly this. |
| 47 | 057 | Ambiguous file name | an abbreviation that matches more than one file |
| 48 | 060 | Wrong password | |
| 55 | 067 | Contiguous space not available | |
| 60 | 074 | No such file version | |
| 61 | 075 | No more pages available for this user | |
| **62** | 076 | **File already exists** | `CreateFile` on a file that is already there. Usually means "carry on and open it", not "stop". |
| 63 | 077 | Attempt to create too many files | |
| 68 | 104 | No such access code | a bad access code to `OpenFile` |
| 69 | 105 | File already open | |
| 70 | 106 | Not write access | |
| 71 | 107 | Attempt to open too many files | at most 18 files open at a time |
| 73 | 111 | Not read access | |
| 79 | 117 | No such user index | |
| 80 | 120 | Not append access | opened with access 5 without append rights |
| 83 | 123 | Not open for sequential write | writing to a file opened for read |
| 84 | 124 | Not open for sequential read | reading from a file opened for write |
| 90 | 132 | No file opened with this number | |
| 99 | 143 | No such block | |
| 108 | 154 | Source empty | |
| 111 | 157 | Missing parameter | too few parameters in the call |
| 174 | 254 | Illegal floppy format | |
| 175 | 255 | File not open | |
| 178 | 262 | File-access connection aborted by file server | remote (COSMOS) file access |
| 182 | 266 | Unknown remote system name | remote file access to a name not in the routing table |
| 202 | 312 | No termination handling defined | |
| 211 | 323 | File-access internal error; invalid parameter value | |

Codes above 511 (`1000B` and up) come only from programs running on the ND-500(0); `1003B` =
515 decimal is "Error in monitor call" and `1015B` = 525 decimal is "Wrong number of parameters
in monitor call" - the two a linter's own work is meant to prevent.

There is a second, separate numbering: the **RT-program error numbers** printed by messages of
the form `ERROR 14 IN BAK07 AT 114721`. Those are also decimal but they are a different table
(00 illegal monitor call, 14 outside segment bounds, 29 file-system error, 46 XMSG fatal error,
47 XMSG user error). Do not mix the two - RT error 46 is an XMSG failure, `ErrCode` 46 is "no
such file".

Two calls turn a code into text: `WarningMessage` (`64B`) prints it and continues,
`ErrorMessage` (`65B`) prints it and **stops the program**. `GetErrorMessage` (`334B`) hands
you the 128-byte text instead of printing it.

---

## 5. The calls people get wrong

### 5.1 `CloseFile` is 43B. `DeleteFile` is 54B.

```planc
MON43(dev)      % CLOSE
MON54(name)     % DELETE - destroys the file
```

They sit close together in the numbering, both take one parameter about a file, and one of them
destroys data. Getting them the wrong way round is silent until the file is gone. **A linter
should flag `MON54` anywhere the surrounding code looks like a close** (immediately after a
write loop, at the end of a save routine).

### 5.2 `OpenFile` - the PLANC routine is not the manual's parameter order

The manual's table (and `MONITOR_CALL`) order is:

```
1 FileNumber (I/O)   2 AccessCode (I)   3 FileName (STR)   4 DefaultFileType (STR)
```

The PLANC `MON50` routine is a different shape - it **returns** the file number as its
out-value and takes the other three in a different order:

```planc
IMPORT ( ROUTINE VOID, INTEGER (BYTES, BYTES, INTEGER) : MON50 )
MON50(fileName, defaultType, accessCode) =: dev
```

| Wrong | Right |
|---|---|
| `MON50(dev, 1, name, type)` | `MON50(name, type, 1) =: dev` |
| `IF dev > 0 THEN ... ` after a failed open | test the ROUTINEERROR flag; `dev` holds rubbish |

**`MON50` cannot create a file.** Quoting the name (`'"CHAT"'`) does *not* mean "create it if
absent" from the PLANC routine - measured, it did nothing at all and every subsequent write
raised. Use `CreateFile` (`221B`) through `MONITOR_CALL`, or the `CREATE-FILE` command through
`CallCommand`.

**A file opened for sequential write is truncated at CLOSE, not at OPEN.** A program that opens
and never closes leaves the OLD contents in place, and the save silently does nothing. Close on
every path, including after a failed write. A half-written source file therefore still
*compiles* - as its old contents.

Access codes: `0` sequential write, `1` sequential read, `5` sequential write append. The full
list is in the JSON `notes` for `OpenFile`.

### 5.3 `CreateFile` - one 64-byte buffer, and INT4 sizes cannot be constants

```planc
BYTES    : nameBuf(0:63)                 % 64 bytes, SPACE padded
INTEGER4 : startAddr := 0
INTEGER4 : noOfPages := 0
...
MONITOR_CALL(221B, nameBuf, startAddr, noOfPages)
```

| Wrong | Right |
|---|---|
| `MONITOR_CALL(221B, 'CHATLG:SYMB', 0, 0)` | copy the filespec into a full 64-byte buffer, pass INT4 **variables** |
| passing name and type separately, as `OpenFile` does | `CreateFile` takes the WHOLE filespec, name and type together, in one buffer |

**`INTEGER4` parameters cannot be given as constants** - the manual states this flatly for
PLANC. Put the value in a variable first. This applies to every `INT4` parameter in the JSON,
not just `CreateFile`: `SetMaxBytes`, `SetStartByte`, `ExpandFile`, `ReadFromFile`,
`WriteToFile`, `DeletePage`, `NewFileVersion`, `ReadDiskPage`, `WriteDiskPage`,
`DataTransfer`, `TransferData`, `DMAFunction`, `GetDeviceType`.

### 5.4 `CallCommand` (70B) and `ExecuteCommand` (317B) - fixed-size buffers

| | Number | PLANC buffer | On error |
|---|---|---|---|
| `CallCommand` (COMND) | `70B` | **`BYTES : Command(0:79)`** - eighty bytes | may **terminate your program** |
| `ExecuteCommand` (UECOM) | `317B` | **`BYTES : Command(0:35)`** - thirty-six bytes | prints a message, program continues |

The manual recommends `ExecuteCommand`. **But there is no `MON317` in the PLANC runtime** (it
is not in the 54), so from PLANC without the MON-CALL library, `MON70` is the only route.

**The buffer is a fixed size, not a string.** A PLANC `BYTES` argument carries its own length,
so a short literal passes a short buffer where the call expects a full one. Fill with spaces,
copy the text over the front:

```planc
BYTES : cmdBuf(0:79)
BYTES : spaceChar := ' '
FOR i IN 1:80      DO  spaceChar(0) =: cmdBuf(i - 1)  ENDFOR
FOR i IN 1:cmdLen  DO  cmdText(i - 1) =: cmdBuf(i - 1)  ENDFOR
MON70(cmdBuf)
```

**The command text ends with an apostrophe, and carries no `@`.** In PLANC an apostrophe inside
a literal is doubled, so a create is written `'CREATE-FILE MY:FILE,1'''` - closing quote plus
one apostrophe character.

**A LOCAL filespec takes NO quotes here, and this is the opposite of `MON50`.** Measured on
D100:

```
@CREATE-FILE "CHAT:CNFG",1
ILLEGAL CHARACTER IN PARAMETER          <- ErrCode 17
@CREATE-FILE CHAT:CNFG,1
@
```

Quotes are right only for a REMOTE filespec, which goes to another machine's command processor.

**Two dependent commands need a wait between them.** The manual names the case: *"Use
SuspendProgram to wait a specified time interval between two CallCommands which depend on each
other, e.g. CreateFile and OpenFile."* Create-then-open is a documented race.

### 5.5 `OutByte` - the byte is the IN-VALUE

```planc
IMPORT ( ROUTINE BYTE, VOID (INTEGER) : MON2 )
b MON2(dev)          % right
MON2(dev, b)         % wrong - will not compile
```

The same shape applies to the other in-value routines: `MON17` (SetTerminalType), `MON24`
(Out8Bytes), `MON64`/`MON65` (the error-message pair, where the error number is the in-value
and there are no parameters at all), `MON73`, `MON74`, `MON76`.

And the out-value routines return rather than fill: `MON1` (InByte) gives a `BYTE`, `MON50` an
`INTEGER`, `MON62`/`MON75` an `INTEGER4`, `MON11` an `INTEGER4`, `MON30` an `INTEGER`.

### 5.6 Terminal input carries parity

Terminal input on these machines arrives with even parity set, so a carriage return reads as
**141 decimal, not 13**. The manual's own PLANC example loops `WHILE Char > 215`. Code that
compares an input byte against 13 or 10 without masking is looking for something that never
arrives. Mask to seven bits (`value AND 127`) before comparing, and mask again before writing
a byte into a file you want a person to read.

**And a text file you WRITE needs even parity** or `LIST-FILE` shows nothing at all, silently.
That is a property of SINTRAN's own tools, not of the monitor calls.

### 5.7 Arrays that are really records

Several calls take a fixed-size block whose length the manual gives in bytes: `GetRTDescr` 52,
`ReadObjectEntry` / `GetObjectEntry` / `SetObjectEntry` / `GetUserEntry` 64, `GetDirEntry` 42
(but `WriteDirEntry` 48 - that difference is what the manual prints), `GetNameEntry` 28,
`GetSpoolingEntry` 272, `GetSystemInfo` 24, `TerminalStatus` 44, `GetErrorInfo` 12,
`GetUserRegisters` 154, `GetActiveSegment` 2048, `GetErrorMessage` 128, `GetOwnProcessInfo` 34.

The manual says outright: *"Monitor calls requiring large amounts of data may use records
instead of strings."* A linter should check the declared size of the array or record against
the documented byte count - an undersized buffer is overwritten with no diagnostic, because
**PLANC does no array-bound checking**.

### 5.8 A call outside the 54 and outside MON-CALL

Then you write an interface routine (a small MAC or NPL stub that issues the raw `MON` and
returns its status) and load it **before** the PLANC runtime. On a machine with no assembler
installed this route is closed, and the program has to be designed not to need the call.

---

## 6. What the JSON does and does not promise

`monitor-calls.json` is keyed by `longName` - the name `MONITOR_CALL('...')` takes. 258 entries.

| Field | Where it comes from | Confidence |
|---|---|---|
| `number`, `shortName`, `longName` | manual sections 2.1, 2.2, 2.15 cross-checked against each other | high; every disagreement between the three is recorded in `notes` |
| `params` | manual section 2.2, in order, with the manual's own types and directions | high for the documented calls; empty for the internal-use calls of section 2.14, which the manual deliberately does not document |
| `nd100`, `nd500` | each call's own AVAILABILITY line in chapter 3 | high for 186 calls; for the rest the chapter-3 page did not survive the scan and the value comes from the section 2.2 dot columns, and `notes` says so and says UNVERIFIED |
| `canRaise` | conservative default (section 3.2 above) | deliberate over-approximation, not a measurement |
| `plancRoutine` | ND-60.117.5 Appendix E section 0.2 | high; confirmed four for four against the linker on D100 |

**Not in the JSON, on purpose:** calls that appear in the repo's `Developer/MON/calls/` YAML
hub but in none of this manual's tables - `350B ReadWriteSegment`, `351B OpenFileByIndex`,
`352B EventSystem`, `441B PlaceSegment`, `511B DVIO`, `512B`/`513B XMSGCallA`/`B`. They may be
real on later SINTRAN versions; this manual does not list them, so they are not here.
`45B` is `DefineBreakpoint` (DBRK) per this manual's section 2.14; the YAML hub calls it
`GetTypeRing`. The manual is followed.

**The obsolete list.** Section 2.16 names 27 numbers as no longer supported: the calls still
exist for old programs, but **the monitor call package returns -2** when you use them, so
`MONITOR_CALL` cannot reach them at all. Their `notes` say so. The list includes `200B`
(XMSGFunction) and `201B` (HDLCFunction), which is why COSMOS work goes through the XMP
library and not through the monitor call directly.

## See also

- [`PLANC-MONITOR-CALLS.md`](PLANC-MONITOR-CALLS.md) - the same ground in prose, with the D100
  measurement history
- [`PLANC-DEVELOPER-GUIDE.md`](PLANC-DEVELOPER-GUIDE.md) - the language and the compiler
- [`COSMOS-XMP-LIBRARY.md`](COSMOS-XMP-LIBRARY.md) - MON 200B, which PLANC reaches through the
  XMP library instead
- `Developer/MON/calls/` - one YAML per call, with descriptions and worked examples (the
  language examples there are AI-generated; treat them as a hint, not authority)
