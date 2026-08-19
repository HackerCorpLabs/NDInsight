# Talking to XMSG from PLANC

> **Looking for how to WRITE one?** This page is about what the machine has installed and how to
> find it. The full programming guide - hello world, every call, the patterns, the traps and the
> error numbers - is **[PLANC-XMSG-PROGRAMMING-GUIDE.md](PLANC-XMSG-PROGRAMMING-GUIDE.md)**.

How a PLANC program on SINTRAN opens a port, sends a message and receives one, and what the machine
actually has installed to build against. Written while getting a chat client to compile on a real
ND-100 (D100, SINTRAN III VSX/500 K, XMSG Release L), so every claim here is either from
`ND-60.164.3 EN COSMOS Programmer Guide` or from listing the machine - and it says which.

## What has to exist on the machine

**VERIFIED on D100.** The XMSG product does not live under `SYSTEM`. It is installed under
`UTILITY`, and a listing is per user - `LIST-FILES XMSG,,` answers for whoever you are logged in as
and tells you nothing about the machine:

```
@LIST-USERS                          list the users first
@LIST-FILES (UTIL)XMSG,,             then ask the right one
```

What is there:

| File | What it is for |
| --- | --- |
| `(UTILITY)XMSG-LIBRARY-L03:BRF` | XMSG internals - **NOT** the `XMPF*` calls, see below |
| `(UTILITY)XMSG-PL-VALUES-L:INCL` | the PLANC constants - functions, XROUT services, error codes |
| `(UTILITY)XMSG-SYMBOL-L03:SYMB` | the symbol table |
| `(UTILITY)XMSG-KERNEL-L03:BPUN`, `XMSG-XROUT-L03:BPUN` | the kernel and XROUT themselves |

The PLANC compiler is under `SYSTEM`: `PLANC-100-F00:PROG`, with `PLANC-1BANK-F00:BRF` and
`PLANC-2BANK-F00:BRF` as its runtime.

## The `XMPF*` routines are not on this machine, and there is nothing to link against

**MEASURED on D100, and it corrects an earlier claim in this file.** `XMSG-LIBRARY-L03:BRF` was
described here as "the library the linker resolves `XMPF*` calls against". It is not. Asked what it
actually defines:

```
Brl: LIST-BRF-ENTRIES (UTILITY)XMSG-LIBRARY-L03,
Output file:TERMINAL
BRF unit: Library mode. Size:    260 P
    XBINI........0    XBARC.......32    XBAST.......46    XBAIN......127
    XBADB......136    XBLOC......213
BRF unit: Library mode. Size:  12521 P  XQVER........0    XQTAB........3
BRF unit: Library mode. Size:    115 P  XWGER.......50
BRF unit: Library mode. Size:   7726 P  XMRER.....7660    XMERR.....7666
```

Eleven entries, all XMSG internals - buffers, queues, error reporting. **Not one `XMPF` anything.**
Loading it changes nothing at all: the linker's free address is `P 004231` before and `P 004231`
after, with `LIBRARY-MODE ON` or off. The same search came up empty in `XMSG-SYMBOL-L03:SYMB`,
`XMSG-VALUES-L:SYMB` and the PLANC runtime.

So the picture is consistent with the missing `:IMPT` file rather than surprising: **what is
installed here is the XMSG runtime - the kernel, XROUT, the constants, the symbol tables - and not
the COSMOS PLANC developer binding.** Both halves of that binding are absent, the declarations and
the code behind them. A program written to the guide's `XMPFOPN(...)` style compiles cleanly and
then cannot be linked:

```
- LOADER MESSAGE: UNDEFINED ENTRIES
XMPBLET...3047 U  XMPFGET...3260 U  XMPFWRI...3265 U  XMPROUT...3072 U
XMPFSND...3271 U  XMPFOPN...3762 U  5MON_P....3776 U  XMPFRCV...4142 U
XMPFMST...4146 U  XMPFREA...4154 U  XMPFREL...4156 U
```

### What the monitor call documentation says, and what it does not

The underlying call is `XMSGFunction`, mnemonic `XMSG`, monitor call **`200B`** - octal 200, from the
tables in `ND-860228-2-EN SINTRAN III Monitor Calls`. Its own page is short and says three things
that matter here:

 - "The parameters varies from function to function." No layout is given.
 - Availability is listed per language, and for **PLANC it reads "Not available."** So is FORTRAN,
   COBOL, PASCAL and MAC.
 - For the parameters it refers the reader onward: to `ND-860164` (the COSMOS Programmer Guide) for
   "the communication facilities offered to high-level languages", and to `ND-860134` (the SINTRAN
   III Communication Guide).

Read together with the measurement above, that is a consistent story rather than a contradiction:
**the documented way for a PLANC program to reach XMSG is the `XMPF*` library described in the
COSMOS guide, and this installation does not have it.** The monitor call page does not offer a
PLANC-level alternative - it points back at the same missing library.

### And the Communication Guide says why PLANC is "not available"

`ND-60.134.2 EN SINTRAN III Communication Guide` documents every XMSG function, and it is explicit
about the level it documents them at:

> The calls will be described by showing the NPL code required to use them. The user must remember
> that the **T-register** always contains the status on return (which should be checked!)

The calls are register conventions. Opening a port, verbatim from section 4.3.1.1:

```
T::XFOPN (BONE XFPRM); *MON 2XMSG
A::PORTNO
```

and closing one, from 4.3.1.2:

```
T::XFCLS; A::PORTNO; *MON 2XMSG
```

So the function code goes in **T**, the argument in **A**, and the status comes back in **T**. That
is the whole reason the Monitor Calls manual lists PLANC as "Not available" for this call: PLANC's
`MONITOR_CALL` standard routine takes a call number and parameters, and gives no way to place a
value in T. The `XFDUM=0`, `XFDCT=1`, `XFGET=2` block in the constants file is this function-code
table - the values that go in T.

This also confirms what the library measurement showed rather than contradicting it. The guide names
`XMRERR` and `XMERR` as routines "in XMSG-LIBRARY" that turn an error code in A into a text pointer
in A - and `XMRER` and `XMERR` are two of the eleven entries that library really defines. It is the
NPL-level support library, and it is installed and intact. What is missing is only the layer the
COSMOS guide puts on top of it for high-level languages.

### Where that leaves a PLANC program

Two paths, and both are documented rather than invented. Which one to take is not a question this
file can answer - it depends on what can be got onto the machine:

 - **Write the interface at register level and link it in.** The Communication Guide gives the NPL
   for every function, so the routines a PLANC program imports can be written from it directly.
   This needs no missing product.
 - **Install the COSMOS PLANC binding.** That is the `XMP:IMPT` declarations and the code behind
   the `XMPF*` names, from the COSMOS product itself. It is what the COSMOS Programmer Guide
   assumes throughout, and it is what this installation does not have.

**Nothing here says `MONITOR_CALL(200B, ...)` works from PLANC.** The manual says the opposite, and
no parameter layout for such a form has been found in any manual in this repository.

## The include names differ from the manual

The guide's sample programs open with

```planc
$INCLUDE XMP:DEFS
$INCLUDE XMP:IMPT
```

**Neither name exists on this installation.** The constants file is `XMSG-PL-VALUES-L:INCL`, and
there is no `:IMPT` file under `UTILITY` at all. Copy the guide's `$INCLUDE` lines unchanged and the
compile fails before it starts.

What the two files hold, per the guide:

 - **DEFS** - `XMOK`, the flag BIT POSITIONS, message types, `XMMAXNameLength`, the appendix D error
   codes, the `XS...` XROUT services. This is what `XMSG-PL-VALUES-L:INCL` provides.
 - **IMPT** - the special PLANC data types, `XmsgIdentifier` being the one `XMPFRCV` takes. **Not
   present here**, so the declarations have to come from somewhere else.

## The constants file, and a warning about what it is not

`XMSG-PL-VALUES-L:INCL` is 300 `CONSTANT` lines and **nothing else** - no `ROUTINE` and no `TYPE`
declarations. A copy fetched off D100 is kept at
`SINTRAN/XMSG/SINTRAN-CHAT/XMSG-PL-VALUES-L.INCL`. Its sections are the XMSG function values, the
XMSG user error symbols, the XROUT service values, the XROUT/network-server errors, and the crash
codes. Samples:

```planc
CONSTANT XFDUM=0    % Dummy function
CONSTANT XFGET=2    % Get message space
CONSTANT XSNUL=64   % Null command returns 0 status to sender
CONSTANT XSLET=65   % Send a letter
CONSTANT XRUNN=2    % Unknown name (of server or system)
CONSTANT XFWTF=15   % If set then wait if operation not terminated
CONSTANT XFWAK=14   % Wake up task on status change
```

**FLAGS ARE BIT POSITIONS, NOT VALUES.** `XFWTF` is 15, meaning bit 15 - the guide's own samples
write `2**XFWTF =: FLAGS`. Passing 15 sets bits 0 to 3, four unrelated options, and the wait simply
does not happen. `XFWAK=14` likewise means `2**14`, which is `0x4000`.

## The shape of a client

From the guide's Appendix G sample (page 375), which is the authority for how a program is put
together:

```planc
MODULE EX_CLIENT_PL
$LIST OFF
$INCLUDE XMP:DEFS
$INCLUDE XMP:IMPT
$LIST ON
%
IMPORT (ROUTINE VOID, VOID: MONO)
%
INTEGER ARRAY: STACK(0:1000)
BYTES: INBUFFER(0:SBUFF-1)
```

Note it declares no `XMPF*` routine itself - the two includes do that. With `:IMPT` missing, a
program here must declare them, and the guide's reference section gives each one a parameter table
and a worked call. Confirmed call shapes:

```planc
xmpfopn(0, portNumber)                                    =: returnStatus
xmpfget(0, sizeBuffer, msgIdent)                          =: returnStatus
xmpfsnd(flags, localPort, remoteMagicNum)                 =: returnStatus
XMPFMST(FLAGS, MSGIDENT, MSGTYPE, REMOTEMAGIC, LENGTH)    =: RETURNSTATUS
xmpfrel(0, msgIdentifier)                                 =: returnStatus
xmprout(0, msgIdentifier, myPort)                          =: returnStatus
```

## The twelve routines a chat client and server need

Transcribed from the reference section of ND-60.164.3. `W` marks a parameter the routine writes
back. These are the parameter lists; the PLANC `IMPORT` syntax that wraps them is a separate
question, see below.

| Routine | Parameters |
| --- | --- |
| `XMPFOPN` | flags I, **portNumber I W** |
| `XMPOPCN` | flags I, portName BYTES, uniqueName BOOLEAN, maxConnections I, **portNumber I W** |
| `XMPINFC` | flags I, portNumber I, extraConn I, serialNumber I |
| `XMPFGET` | flags I, sizeBuffer I, **msgIdent XmsgIdentifier W** |
| `XMPFWRI` | flags I, msgDisp I, userAddress **XmsgUserAddress**, userDisp I, userLength I, **writtenLength I W** |
| `XMPFREA` | flags I, msgDisp I, userAddress I, userDisp I, userLength I, **readLength I W** |
| `XMPFSND` | flags I, localPort I, remoteMagicNum |
| `XMPROUT` | flags I, msgIdentifier, myPort I |
| `XMPFRCV` | flags I, localPort I, **msgType I W**, **remotePort I W**, **msgIdentifier XmsgIdentifier W**, **msgLengthOrStat I W** |
| `XMPFMST` | flags I, msgIdent, **msgType W**, **remoteMagic W**, **length W** |
| `XMPFREL` | flags I, msgIdentifier |
| `XMPBLET` | headerBuffer BYTES, lengthBuffer, **offSet I W**, serialNumber I, systemName BYTES, portName BYTES |

Worked calls the manual gives verbatim, which fix the argument ORDER beyond doubt:

```planc
xmpfopn(0, portNumber)                                 =: returnStatus
xmpfget(0, sizeBuffer, msgIdent)                       =: returnStatus
xmpfsnd(flags, localPort, remoteMagicNum)              =: returnStatus
XMPFMST(FLAGS, MSGIDENT, MSGTYPE, REMOTEMAGIC, LENGTH) =: RETURNSTATUS
xmpfrel(0, msgIdentifier)                              =: returnStatus
xmprout(0, msgIdentifier, myPort)                      =: returnStatus
```

**Both former open points are SETTLED (2026-08-18): the REAL `XMP-B02:IMPT` was recovered**
from the ND-10609B floppy (image in the WSL software archive) and is kept, with the whole
product, at `Installation/Software/ND-10609/files/` (raw + parity-stripped
`XMP-B02-IMPT.readable.txt`). From the real file:

 - The `IMPORT` syntax is exactly the transcribed form:
   `IMPORT (ROUTINE VOID,INTEGER(INTEGER,INTEGER WRITE): xmpfopn), (...), ...` -
   comma-chained inside one IMPORT, `&` continuations, `WRITE` on output parameters,
   `READ WRITE` where a parameter is both.
 - `XMPBLET`'s second parameter is `INTEGER` (the manual's `Bytes` was the table slip) - and
   its offSet parameter is `INTEGER READ WRITE`.
 - `TYPE XMMSGIDENTIFIER = INTEGER` and `TYPE XMUSERADDRESS = INTEGER` (old MC68000
   compiler: make XMUSERADDRESS INTEGER4) - so `CHAT.PLNC`'s spellings were right.
 - The library remaps statuses: `XMOK = 0` ("NOTE change from XMSG OK=1") and errors return
   as POSITIVE standard values from base `XMXKXXX = 16896 = 41000B` (`XMXENTM` 16896 = "not
   terminated", `XMXENOT` 16897, ...). The negative `XE...` symbols in the values file
   belong to the raw MON 200B interface, not to `XMPF*` returns.

Full verdict: [COSMOS-XMP-LIBRARY.md](COSMOS-XMP-LIBRARY.md) section 5.

## Two traps that cost real time

**A monitor-call error is a ROUTINEERROR.** `InByte` in no-wait mode does not return a sentinel
character - it raises a routine error with code 3, handled with `ON ROUTINEERROR DO ... ENDON` and
read from `ErrCode`. Assuming it returns a small value that can be masked away is a guess, and it is
wrong.

**Terminal input carries even parity in bit 7.** Carriage return arrives as 141, not 13. Mask every
byte read from a terminal with 127 before comparing, or the program never sees the user press
return and looks hung rather than wrong.

## PLANC checks no array bounds

Nothing in the language stops a write past the end of an array, so every length that arrives from
outside the program is a length to test first. Two shapes worth knowing, both found in real code
here:

 - **A length byte from the wire.** A name length is one byte, so a peer can say 255 into a
   sixteen-byte field. Test against the array size AND against how many bytes actually arrived.
 - **The read call itself.** `xmpfrea(0, 0, ADDR(buf[0]) FORCE XMUSERADDRESS, 0, nBytes, readLength)`
   takes `nBytes` from the SENDER via XMSG. Pass it straight in and a peer that sends more than the
   buffer holds writes past the end - before any field is parsed, so no later check can save it.
   Clamp it to the buffer size first.

## See also

 - [COSMOS-XMP-LIBRARY.md](COSMOS-XMP-LIBRARY.md) - the XMP library documented in full:
   routine catalog, call conventions, the ND-10609 product media, the compile rules and
   where the link fails today
 - [PLANC-DEVELOPER-GUIDE.md](PLANC-DEVELOPER-GUIDE.md) - the language and the compiler
 - `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` - every routine, appendix D error
   codes, appendix G sample client and server
 - `SINTRAN/XMSG/SINTRAN-CHAT/` - a client and an RT server being built against this, plus the
   constants file fetched off the machine

## Status

The two chat programs have NOT been compiled yet. What is settled: the library and constants exist
on D100 under `UTILITY`, the include names in the sources are wrong for this installation, and the
routine declarations still have to be written because `:IMPT` is not installed.
