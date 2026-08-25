# How to program PLANC on SINTRAN III

A working guide for somebody - human or model - writing PLANC for an ND-100. It is written to be
read top to bottom before touching a source file, and to be re-read at the point of each mistake.

It exists because a chat program of about two thousand lines took days, and every hour of that
delay was caused by the code and scripts in this repository.

XMSG went down eleven times in two days. That was not a flaky machine. It was three defects:

 - a server that armed a wake-up on two ports, parked, and stopped reading its own port, so
   messages piled up unread in a kernel with finite message space;
 - a client with an unguarded monitor call, which died *after* joining and leaked a seat every
   single time - dozens of times;
 - build tooling that hard-killed the process talking to the machine on every listing fetch,
   skipping the clean exit that tells the peer the link is going away.

Alongside those, broken binaries were installed for days because the compile listing was never
read, and several diagnoses were acted on before anything was measured.

**None of it was the machine.** Every rule below is one of those mistakes, written down so it
costs the next person nothing.

**The companion files in this folder, and what each is for:**

| file | what it is | use it when |
|---|---|---|
| `PLANC-LANGUAGE-RULES.md` | 113 numbered checkable rules from the four PLANC manuals | writing or reviewing any PLANC |
| `monitor-calls.json` | 258 monitor calls: number, parameters, types, directions | calling anything in SINTRAN |
| `PLANC-MONITOR-CALL-RULES.md` | how to call them from PLANC without dying | same |
| `xmp-api.json` | 54 XMP routines, 397 constants, from ND's own declarations | writing an XMSG program |
| `PLANC-XMSG-API-RULES.md` | the XMSG rules, each with a detection recipe | same |
| `../../SINTRAN/XMSG/tools/planc-lint.py` | checks most of the above automatically | before every push |

---

## 0. THE SHORT VERSION

1. **Lint before you push.** `python tools/planc-lint.py <source>` - seconds, on Windows.
2. **Read the LISTING after every compile.** A compile you have not read the listing for is not a
   build. This is the single most expensive lesson in the whole project.
3. **Guard every `MONITOR_CALL`.** Unguarded, a refused call unwinds the entire program.
4. **Never let a name be used that is not declared.** PLANC's answer to that is an error only in
   the listing, and it is easy to miss for days.
5. **Check the machine, not the screen.** `LIST-PORTS`, `LIST-NAMES`, `LIST-RT-DESCRIPTION`.
6. **Which PLANC?** This project's ND-100 runs **PLANC F**. Two of the manuals document later
   versions with a bigger language. Anything tagged `[NEWER ONLY]` in `PLANC-LANGUAGE-RULES.md`
   will not compile here.

---

## 1. THE SHAPE OF A PROGRAM

```planc
MODULE chatModule
    INTEGER ARRAY : stack(0:1000)
    INTEGER : counter := 0                  % module level MAY carry an initialiser

    IMPORT ( ROUTINE VOID, BYTE (INTEGER) : MON1 )

    ROUTINE VOID, VOID : sayHello
        INTEGER : i                          % a LOCAL may NOT carry an initialiser
        0 =: i                               % assign on entry instead
        OUTPUT(1, 'AL7', 'hello$')
    ENDROUTINE

    PROGRAM : myProgram
        INISTACK stack                       % MUST be the first statement, runs every start
        sayHello
    ENDPROGRAM
ENDMODULE
```

**`=:` STORES. `:=` INITIALISES AT DECLARATION.** `x =: y` puts x into y - left to right, which
is the opposite of most languages. `:=:` swaps. The manuals' example code garbles these
constantly; trust the rule, not a printed example.

**Identifiers are unique in their FIRST TEN CHARACTERS** in PLANC F. `terminalNoWait` and
`terminalWait` are fine (`terminalNo` / `terminalWa`); two names agreeing for ten characters are
one name and the second silently redefines the first.

**Comments are `%` to end of line. Continuation is a trailing `&`.**

---

## 2. THE TRAPS THAT COMPILE CLEAN AND GO WRONG LATER

These are the ones that cost days, because the compiler is happy.

**A name used but never declared.** In PLANC F, a bare call to a routine that does not exist is
`*** ERROR ILLEGAL SYNTAX` - **in the listing only**. A variable stored to but never declared can
compile with 0 diagnostics and the program then runs on a value nothing sets. The linter catches
both.

**`'ALn'` counts the trailing `$`.** `'AL7', 'hello$'` - that is 5 letters plus `$` is 6... and
the manual counts the leading `$` too when you use one. Get it wrong and PLANC either prints
asterisks or pads past the newline and indents the next line. Let the linter compute it.

**PLANC CHECKS NO ARRAY BOUNDS.** None. Every copy into a fixed array needs its own guard:

```planc
nBytes =: readAmount
IF readAmount > inBufSize THEN          % CLAMP FIRST. The size came from the SENDER.
    inBufSize =: readAmount
ENDIF
```

Without that, a peer choosing a longer message writes straight past the end of your buffer,
before any of your parsing gets a chance to object.

**There is no `MOD`.** Remainder is `x - (x / 10) * 10`.

**`WHILE` is a continue-test INSIDE `DO ... ENDDO`**, not a loop header, and there is no
`ENDWHILE`:

```planc
DO
    ... work ...
    WHILE stillGoing
ENDDO
```

**The out-value goes BEFORE `RETURN`:** `x RETURN`.

**`ON STACKERROR` is not implemented.** A stack overflow cannot be caught, only avoided.

---

## 3. MONITOR CALLS - THE RULE THAT KILLS PROGRAMS

**A monitor call SINTRAN refuses raises a `ROUTINEERROR`. Unhandled, it unwinds the WHOLE
program.** Measured: an unguarded no-wait switch killed the chat client the instant it joined -
and because the join had already been sent, it also left a seat spent in the room.

```
CHAT: joining (LOBBY) - type and press return
NO ROUTINEERROR HANDLER, ERRETURN=177777B
@
```

**The safe shape, which also tells you WHY it failed:**

```planc
ROUTINE VOID, VOID : terminalNoWait
    BOOLEAN : failed
    ON ROUTINEERROR DO
        TRUE =: failed
        ErrCode =: lastErr          % ErrCode comes from the runtime - do NOT declare it
    ENDON
    FALSE =: failed                 % CLEAR AFTER THE ON BLOCK, never before
    MONITOR_CALL('NoWaitSwitch', ownTerminal, inputFlag, noWaitOn)
    IF failed THEN
        OUTPUT(1, 'AL24', '$CHAT: NoWaitSwitch err ')
        OUTPUT(1, 'I6', lastErr)
        OUTPUT(1, 'AL1', '$')
    ENDIF
ENDROUTINE
```

Three things in that shape are load-bearing:

 - **clear the flag AFTER the `ON` block.** Cleared before, the handler body has already run and
   every call reports failure while returning a perfectly good result;
 - **keep `lastErr` at module level** if you want to print it after the block - a PLANC local
   cannot be read once its `ON` block has run;
 - **have a fallback.** A refused call should degrade, not kill.

**`MONn` versus `MONITOR_CALL`:** the runtime supplies **54** calls as ordinary routines
(`MON1`, `MON2`, `MON50`...) - `monitor-calls.json` says which, in `plancRoutine`. Everything else
goes through `MONITOR_CALL` **by name**, which needs the names file from **`MON-CALL-1B-A00`**.

**Load order: your program, then MON-CALL, then the PLANC runtime.** The manual's own example is
`LOAD EX-PROG:BRF, MON-CALL-1BANK:BRF, PLANC-1BANK:BRF`.

**Check `monitor-calls.json` for the parameter list before writing the call.** Do not copy a call
from another program and assume; several names in the manual are inconsistent between tables and
are recorded as such.

---

## 4. XMSG - THE RULES THAT COST THE MOST

Full detail in `PLANC-XMSG-API-RULES.md`. The four that actually bit:

**`XFRCV` RECEIVES ON ONE PORT. There is no receive-on-any.** A server with two ports must POLL
both with **flags ZERO** and sleep between passes. `XFWAK` looks like the answer and is not: it
arms a wake-up, and *the wake-up does not deliver the message*. Armed on two ports, the task
parks on one doorbell while a message sits on the other:

```
X-C LIST-PORTS
  6  ...BAK06  Qlen 1  WAK 0     <- ROOM, a message WAITING
 14  ...BAK06  Qlen 0  WAK 1     <- ADMIN, task parked HERE
```

```planc
0 =: waitFlags                                   % XMSG's own no-wait
...
xmpfrcv(waitFlags, roomPort, ...) =: returnStatus
IF returnStatus = XMXENTM THEN
    xmpfrcv(waitFlags, adminPort, ...) =: returnStatus
    IF returnStatus = XMXENTM THEN
        MONITOR_CALL('TimeOut', 50, 1, cause)    % one second. NEVER a bare loop.
    ENDIF
ENDIF
```

**BOTH OBVIOUS STATUS TESTS ARE WRONG.** A receive answers `XMOK` (0) **or** a message type
(`XMTNO..XMTPS`, 1..6). `XMXENTM` (16896) means the port is empty.
`> 0` accepts an empty port and every error; `>< XMOK` alone discards every real message.

**Flags are BIT POSITIONS, not values.** `XFWAK` is 14; the value is `2**XFWAK`.

**Decode error numbers, do not stare at them.** `library value = XKXXX(16896) + abs(negative XE
code)`. So `16918 = XEIPN` "illegal port number" and `16933 = XENRU` "XMSG not running". Those two
together are the signature of XMSG having died and come back under a running program. XROUT errors
use a second base, `XRXXX` = 16960.

---

## 5. RT PROGRAMS

**One bank, always.** The Real Time Guide: *"Under no circumstances should the 2-bank run time
library be used"*. Link `PLANC-1BANK-F00` and live inside **64KW for code, data and stack
together**.

**`OUTPUT(1, ...)` from an RT program goes nowhere you will find it.** Give it a log file, and
open/write/close **one line at a time** - a crash leaves an open file unflushed, and a crash is
exactly when the log has to survive. Access **5** is append; 0 truncates at CLOSE.

**An RT program's default user is RT**, so an unqualified file lands in `(PACK-ONE:RT)` where
SYSTEM cannot delete it. A FOREGROUND run of the same program is a different user and needs its
own log file to exist, or `logOpen` fails and it logs nothing at all, silently.

**Use `tools/rt-load.ps1`.** Do not type the loader sequence. `CHANGE-RT-DESCRIPTION` has eight
parameters and **slot 5 is START ADDRESS, not RING** - a ring value there starts the program
inside its own data and it dies as a phantom `STACK OVERFLOW`.

**After every XMSG restart, RT-LOAD AGAIN.** Restarting the program is not enough: a segment
linked before the restart goes `PASSIVE` with 0 CPU units. A foreground run works either way,
because it reloads from its `:PROG` each time.

---

## 6. THE BUILD LOOP

One iteration is about ten minutes. Do not waste one.

```
1. lint          python tools\planc-lint.py <source>
2. push          runner --push ... --push-overwrite        retry once on XRMFL - it is transient
3. verify push   FILE-STATISTICS <name> - byte count MATCHES the local file?
4. compile       @MODE <build>:MODE,,
5. VERIFY BUILD  tools\planc-build.ps1 -PullOnly -Listing <name>:LIST
6. install       tools\rt-load.ps1 ... -AndStart   or   DUMP-PROGRAM-REENTRANT
7. test          and read the machine, not the screen
```

**Step 3 matters because a stalled push leaves the OLD content, and the old content compiles
perfectly.** You then test yesterday's program and conclude something false about today's change.

**Step 5 is the gate.** The compiler prints diagnostics as it goes; on a long source they scroll
off a 24-line screen, and the `0 DIAGNOSTICS` you are left looking at belongs to the LINKER. A
routine that did not exist was reported on every build for days and never once seen.

**`LIST-FILE` does not print file contents** - its second parameter is an output file, and even
answered properly it prints only the file's identity. **QED refuses these files too.** To read a
log or a listing, pull it off the machine.

**NEVER hard-kill a process that is talking to the ND.** `Stop-Process` skips the clean exit that
sends `DISC`, so the machine is left believing a link and a conversation are still up. The build
tooling in this repository did that automatically on every listing fetch, and took XMSG down
repeatedly. `planc-build.ps1` now refuses; `-ForceStopDaemon` is the deliberate override.

---

## 7. HOW TO BE WRONG LESS

**WHEN SOMETHING BREAKS, IT IS YOUR CODE UNTIL MEASUREMENT SAYS OTHERWISE.** The last thing you
changed is the first suspect - not the emulator, not the hardware, not the operating system.

This is first in this section because it is the mistake that cost the most, and it took eleven
repetitions to see. "The machine is unstable" got written down eleven times as though it were a
finding. It is a story: it explains everything, predicts nothing, and stops you looking. All three
real causes were in this repository and every one was visible from the machine's own tables the
whole time - `LIST-PORTS` was showing `Qlen 3` unread on a server that had stopped reading.

Before writing "the machine did X", ask what you ran in the minutes before. And never put that
framing in a document: a file that blames the environment teaches the next reader to do the same.

**Run a control before believing a diagnosis.** Two theories died here after hours, and both
would have died in thirty seconds:

 - *"the log needs even parity - `LIST-FILE` shows nothing and QED cries PARITY ERROR"*. Run the
   same two commands against a file the machine uses every day: identical behaviour. The tools
   were never evidence about the file.
 - *"a failed transfer leaks a seat"*. Take the seat count before, fail one deliberately, look
   again: nothing moved.

**A tool that answers the same way for a known-GOOD input is not a test.**

**When a comment says something is impossible, check whether it still is.** A note saying
`MONITOR_CALL` by name could not work here outlived the installation of the package that makes it
work, and cost a day of designing around a wall that was not there.

**When a design changes, the flags set up for the old design do not announce themselves.**
`XFWAK` was correct for a loop that returned when both ports were empty. That loop was replaced
by a sleep years of edits ago. The flag survived, with a confident comment explaining why it was
right, and parked the server.

**Write down what you MEASURED, not what you concluded.** A number in a table survives. A
conclusion rots the moment the design around it changes.

**Add a linter rule every time the machine teaches you something.** That is what makes the next
program cheaper than this one.
