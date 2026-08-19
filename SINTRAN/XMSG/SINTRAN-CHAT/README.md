# The SINTRAN-native chat - CHATSV and CHAT

The chat that runs today lives on our C# node; a SINTRAN user reaches it by connecting to us.
This folder is the other direction: **the room runs ON the ND**, so two ND machines can chat with
the C# node switched off.

**CHAT.PLNC now COMPILES AND RUNS on D100** (2026-08-17): 1247 lines, 0 diagnostics, no
undefined entries, and it starts and reads the keyboard. `CHATSV.PLNC` has still never been
compiled. Build it with `CHATCC:MODE` - see `../tools/planc-build.ps1`, which also fetches the
listing back and greps it, because the compiler's diagnostics scroll off the screen long before
the summary appears.

## BEFORE ANY OF THIS WILL TALK TO ANOTHER MACHINE: the friend entry

**The remote system must be defined as a FRIEND on the machine being asked**, or every
conversation is refused in complete silence - no error, no XRUNN, nothing. Everything you would
check looks correct while it happens: the name is listed, the system shows `ON NET`, the link
shows `State Run` with thousands of frames received.

```
@X-COMM
X-C: DEFINE-FRIEND-SYSTEM
System? 19999
Ok
```

**And `DEF-REMOTE` CLEARS it**, so the order is always name first, access second - and an XMSG
restart clears it too, so it belongs in every bring-up. What a friend system actually is, why
XROUT answers `XRUNN` when it is missing, and how to ask the machine with `LIST-ROUTING-INFO`:
**[`../DOC/FRIEND-SYSTEMS.md`](../DOC/FRIEND-SYSTEMS.md)**.

Plan and reasoning: [`../DOC/PLAN-SINTRAN-NATIVE-CHAT-RT-AND-PROG.md`](../DOC/PLAN-SINTRAN-NATIVE-CHAT-RT-AND-PROG.md).

---

## Files

| File | What |
|---|---|
| `CHATSV.PLNC` | The room server, an RT program. Complete: opens a CONNECTION port with seats, waits, decodes, applies the room rules and answers. Every routine it calls is defined. Never compiled. |
| `CHAT.PLNC` | The client, an ordinary PROG. Drafted: joins by LETTER through XROUT, then watches the keyboard and the port in one loop. Never compiled, and its loop is a BUSY one - see below. |

## What is settled, and where it came from

Every one of these was answered from documents already in the repo, without touching a machine:

| Question | Answer | Source |
|---|---|---|
| How does a long-lived RT server wait? | `XMPFRCV` with `XFWTF` (bit 15) blocks until a message arrives | `DOC/XMSG-API.md` s.7; `XMSG-PL-VALUES-M.INCL` |
| The whole server shape | `XMPOPCN` (seats) -> `XMPFRCV` -> `XMPFMST` -> `XMPFGET`/`XMPFWRI` -> `XMPFSND` -> `XMPFREL` | COSMOS Programmer Guide routine reference |
| How does a client watch the terminal AND XMSG? | `XMPFRCV` WITHOUT `XFWTF` returns at once with status **`MXNEXTM`** ("not terminated") | COSMOS Programmer Guide |
| How does it take a keystroke without waiting? | `TerminalNoWait` (MON `307B`), then `InByte` (MON `1B`); device 1 is your own terminal | SINTRAN III Monitor Calls |

**The parity trap.** Terminal input sets bit 7 as an even-parity bit, so carriage return arrives
as **141, not 13** - the manual's own example. Mask with 127 before comparing, or the client never
sees the user press return and looks hung rather than wrong.

## What is NOT settled

 - **Each reason string carries its length as a separate constant**, so a string and its number
   must be changed together. That is a real risk, and it is why they sit on adjacent lines. It is
   there because PLANC has **no `LENGTH` routine** - the one it has is `SIZE` (syntax
   `SIZE ident =: var`, no parentheses), which returns the STORAGE size, and what that means for a
   `BYTES` value passed as a routine parameter is not established. Passing the count removes the
   question instead of trading one guess for another.
 - **The XMPINFC reply.** Returning a seat on Leave sends a request to XROUT whose reply arrives
   later on our own port as an ordinary message. `handleMessage` will see it and not understand it.
   Ignoring an unknown kind is already what it does, so this is safe - but it is safe by accident
   until somebody decides it on purpose.
 - **Case-insensitive names.** `ChatRoom` compares names ignoring case; the draft does not. One of
   the two changes deliberately, with the test updated - not left as a quiet difference.
 - **What `InByte` returns** with no-wait on and nothing typed. The manual says only that the
   program stops waiting, not what comes back. One five-minute measurement on the machine, and it
   is the only thing on the critical path that needs one.

## Order of work

1. Measure what `InByte` returns with no-wait on and nothing waiting.
2. Compile `CHATSV` until it builds. The calls come from the COSMOS Programmer Guide routine
   reference and the PLANC declarations are checked against the language reference, so the only
   type names come from Appendix G's sample programs - `XMMSGIDENTIFIER` and `XMUSERADDRESS` - so
   they are taken from working code rather than from a parameter table. `XMP:IMPT` still has to
   exist on the machine, but nothing here guesses at what it declares.
3. A one-way spike: `CHATSV` prints an arriving join on the operator console; a throwaway sender
   sends one. This proves the XMSG plumbing inside SINTRAN end to end.
4. Write `handleMessage` from `ChatRoom.cs`.
5. Give `CHAT.PLNC` a wait, by running ONE experiment (below). Its loop spins as fast as the
   machine allows, which on a shared SINTRAN is antisocial.
6. Two ND machines, one room, two terminals.
7. Interoperate with the C# node, both directions. Same message bytes, so it should work; if it
   does not, the message format is the first place to look.

## The message format is OURS

We chose it. No ND ever sent one, so no capture will ever confirm it, and `ChatMessage.cs` plus
its tests are the only specification it has. If this code and that file ever disagree, both are
wrong until one is changed on purpose.

```
byte 0      kind (1 Join, 2 Welcome, 3 Reject, 4 Say, 5 Said,
                  6 Leave, 7 Joined, 8 Left, 9 Rename, 10 Renamed)
byte 1      nickname length
bytes 2..   nickname
then        2 bytes text length, big endian
then        the text
```

## Building

See [`Developer/Languages/Application/PLANC-DEVELOPER-GUIDE.md`](../../../Developer/Languages/Application/PLANC-DEVELOPER-GUIDE.md).
The file must be **CRLF with even parity** before SINTRAN will read it, and files being created in
a SINTRAN command need quotes while existing ones do not.

```
@PLANC
PROG-FILE "CHATSV"
COMPILE CHATSV:PLNC,"CHATSV:LIST","CHATSV"
EXIT
```

## The routine reference is in the repo

`Operations/Cosmos/ND-60164-3-EN  COSMOS Programmer Guide.md` carries the full XMSG/PLANC routine
reference - every argument list used here came from it. Three calls in the first draft were wrong
and grepping that guide caught all three before a single compile:

| Was | Is | Why it mattered |
|---|---|---|
| `XMPOPNM` | **`XMPOPCN`** | A room has SEATS, and seats are the free-connection counter that only a CONNECTION port has. With `XMPOPNM` the ninth joiner walks straight in - while the C# side keeps enforcing the limit, so the two doors disagree about a rule they share. |
| `XMPFFRE` | **`XMPFREL`** | `XMPFFRE` does not exist. |
| `XMPFREA(.., msgIdent, ..)` | **`XMPFREA(flags, msgDisp, userAddress, userDisp, userLength, readLength)`** | It reads the CURRENT message and takes no identifier, exactly like `XMPFWRI` writes it and `XMPFSND` sends it. |
| `ADDR(buf)` | **`addr(buf(0)) FORCE Xmuseraddress`** | The guide's own worked examples. A plain address is the wrong type. |
| status ignored | **`xmpfxxx(...) =: returnStatus`** | Every routine RETURNS its status and every example in the guide assigns it. `XMOK` is zero. |

Also worth knowing: `$INCLUDE XMP:IMPT` is required for the special types, `XMP:DEFS` carries
`XMOK`, and `XMPFRCV` hands back only a *hashed* remote port - the full magic number needed to
reply comes from `XMPFMST`.

## PLANC facts confirmed against ND-60.117.5

Checked rather than assumed, because a plausible-sounding name is exactly what caught this file
out once already (`LENGTH`, which does not exist):

| Written | Verdict |
|---|---|
| `INTEGER4` | real keyword, the 32-bit integer type |
| `BYTES ARRAY : memberName(1:8, 1:16)` | legal - the manual gives `BYTES ARRAY : b1(1:2,0:3)` |
| `BYTES : inBuf(0:255)` | legal - "one extra ARRAY keyword is implicitly included in a BYTES declaration" |
| `BOOLEAN : uniqueName := TRUE` | ordinary form |
| `SIZE ident =: var` | the size routine, no parentheses - and NOT what this file uses, see above |

One idiom deliberately NOT used: `memberName(slot)` with the last subscript omitted refers to the
whole string, which would copy a name in one statement. Names here are variable length, so a
whole-row copy would carry all sixteen bytes including whatever follows. The explicit loops are
longer and correct.

## The client, and the one thing that could invalidate its loop

`CHAT.PLNC` watches the keyboard and the port in a single loop, which works because of two facts
from the manuals rather than any trickery: `XMPFRCV` without `XFWTF` returns at once (`MXNEXTM`
when the port is empty), and `TerminalNoWait` makes `InByte` return at once too.

**What `InByte` gives back when nothing was typed is ANSWERED, and it is not a character.** The
`NoWaitSwitch` entry in the Monitor Calls manual: *"In No Wait, the program does not wait for
input or output to complete. Monitor calls like InByte return **error code 3** instead."* In PLANC
that arrives as a `ROUTINEERROR`, handled with `ON ROUTINEERROR DO ... ENDON` and read from
`ErrCode` - the form that manual's own PLANC example for `InByte` uses. The loop now handles it;
an earlier version assumed a value that would mask to 31 or below, which was a guess and was
wrong.

Also unfinished by design: there is no way to leave (`kLeave` is declared and never sent), and the
nickname is fixed at `RONNY` - reading it from the command line means device 0, the SINTRAN
command buffer, which `InByte` can read.

## The one experiment that would let the client sleep

Both things the client waits on have a documented wake, and the question is only whether they meet:

| Waiting on | Documented wake |
|---|---|
| the terminal | after a no-wait call, *"SuspendProgram or WaitForRestart may passivate the program afterwards. The program restarts when input or output to the device is completed."* (Monitor Calls, `NoWaitSwitch`) |
| the port | `XMPFRCV` with **`XFWAK`** and without `XFWTF` returns at once and ARMS a wake - *"the next transmission to this port will lead to a wake up of the receiver task"* (COSMOS guide) |

So the shape to try: arm the port with `XFWAK`, call `WaitForRestart`, and on waking re-issue the
receive **and** re-read the keyboard, since either could have woken us. `XFWAK` also warns that
*"when the wake up is done, the message is not received, and so the receiving must be repeated"* -
so the re-issue is required, not tidiness.

**What is not established** is whether an XMSG wake-up actually restarts a background program
passivated that way. The call that makes wake-up context explicit, `XMPFWDF`, is for **drivers** -
and the guide adds *"not permitted for RT-programs"*. So this is one measurement, not a deduction.
If the wake does not arrive, a short `HOLD` in the loop is the fallback.

## Appendix G is a complete worked CLIENT and SERVER

`Operations/Cosmos/ND-60164-3-EN  COSMOS Programmer Guide.md`, Appendix G. It corrected three
things in these drafts that no parameter table would have:

| Was | Is | Consequence if left |
|---|---|---|
| `xmpfrcv(xfwtf, ...)` | **`2**XFWTF =: FLAGS`** then `xmpfrcv(FLAGS, ...)` | `XFWTF` is the BIT POSITION 15. Passing 15 sets bits 0-3 - four unrelated options - and **the wait simply does not happen**. The server would spin instead of blocking. |
| `XmsgIdentifier` | **`XMMSGIDENTIFIER`** | two M's; the sample declares `XMMSGIDENTIFIER: MSGIDENT` |
| `addr(buf(0)) force Xmuseraddress` | **`ADDR(BUF[0]) FORCE XMUSERADDRESS`** | square brackets for the index |

The samples also show the file shape: `$LIST OFF` / `$INCLUDE XMP:DEFS` / `$INCLUDE XMP:IMPT` /
`$LIST ON`, `IMPORT (ROUTINE VOID, VOID: MONO)` for the exit routine, `XMPFDCT(FLAGS)` to release
everything before leaving, and `IF RETURNSTATUS >< XMOK THEN` after every single call.

## A seat belongs to the arrival, not to the message

XROUT spends one of the port's free connections to FORWARD a letter, before this program has seen a
byte of what is in it. So the seat has to be settled against the arrival, in the receive loop - not
inside the join handler.

Doing it per kind is what the C# side got wrong, and `CHATSV.PLNC` had the same shape plus one
more: its `refuse` never returned a seat either, so it leaked on every refused join as well as on
every letter carrying something other than a join.

A leaked seat costs nothing at the time. The room is just one seat smaller, for good, and it
surfaces much later as XROUT refusing joins for a room that visibly has space.

The marker is `msgType = XMROU` ("routed message, via XROUT"), which is the only thing telling the
program a seat was spent. **Not measured on hardware** - see
`DOC/CHAT-SEAT-LEAK-AND-XMROU-2026-08-11.md`, which also explains why `xmRoutedMsg := 2` is written
as a literal instead of being imported from `XMP:IMPT`.

## RETRACTED: there was no build blocker - I listed the wrong user

**The XMSG kit is on D100 all along, under `UTILITY`.** I listed `SYSTEM`'s files, found nothing, and
wrote up a blocker. Ronny pointed at the file directly:

```
FILE 8  : (PACK-ONE:UTILITY)XMSG-LIBRARY-L03:BRF      the library to link against
FILE 7  : (PACK-ONE:UTILITY)XMSG-PL-VALUES-L:INCL     the PLANC include
FILE 6  : (PACK-ONE:UTILITY)XMSG-VALUES-L:SYMB
FILE 14 : (PACK-ONE:UTILITY)XMSG-SYMBOL-L03:SYMB
```

with the whole product beside them - kernel and XROUT BPUNs, the installer, the load and init MODE
files. So the `$INCLUDE` and the `XMPF*` calls have something to resolve against after all, and the
monitor-call layer described below is not needed.

**A SINTRAN file listing is per USER.** `LIST-FILES XMSG,,` answers for the user you are logged in
as and says nothing about the machine. Give the directory and user explicitly -
`LIST-FILES (PACK-ONE:UTILITY)XMSG,,` - before concluding that something is absent. This is the
third time in one day that a query answered the question I asked rather than the one I meant: batch
processor 0 that never exists, a symbol name that lives in several blocks, and now this.

The name to use is not `XMP:DEFS` on this installation - it is `XMSG-PL-VALUES-L:INCL`, and what it
covers has to be read before the `$INCLUDE` lines at the top of the two programs are settled.

## The original blocker note, kept because the route in it is still worth knowing

## The build blocker, measured on D100 on 2026-08-17

Neither program has been compiled, and on this machine neither can be yet. Measured rather than
assumed, by listing the machine:

 - **PLANC is there**: `PLANC-100-F00:PROG`, plus `PLANC-1BANK-F00:BRF` and `PLANC-2BANK-F00:BRF`.
 - **`XMP:IMPT` and `XMP:DEFS` are not.** `LIST-FILES XMP` and `LIST-FILES :IMPT` both return
   nothing.
 - **There is no XMSG library to link against.** The only `:BRF` files on the machine are the PLANC
   and FORT48 runtimes. The XMSG files present are `XMSG-IN-L:OUTP` and `XMSG-TERM-HAND:SAVE`.
 - They are not in this repository either, and not in `Installation/` (which has only
   `XMSG-START.MODE`).

What they contain, from ND-60.164.3 COSMOS Programmer Guide: `XMP:DEFS` has `XMOK`, the `XFWTF` /
`XFWAK` flag BIT POSITIONS, the message types, `XMMAXNameLength`, the appendix D error codes and the
`XS...` XROUT services. `XMP:IMPT` has the PLANC data types, such as the `XmsgIdentifier` that
`XMPFRCV` takes.

So the `$INCLUDE` lines at the top of both programs, and every `XMPF*` call in them, have nothing to
resolve against on D100 today.

### The route that does not need the kit

Half of it is already done. On 2026-08-17 every value in `XroutService` and `XroutError` was checked
against SINTRAN's own `SYMBOLS/*/XMSG-SYMBOL-LIST.SYMB.TXT` - `XSLET=0x41`, `XSNUL=0x40`,
`XRUNN=0x02`, `XSGMG=0x47`, `XSDRN=0x49`, `XSDSY=0x4A`, with 34 of 36 and 56 of 57 confirmed and the
one apparent mismatch explained. **The numbers a DEFS file would give us are already in hand and
cited**, so writing our own is transcription rather than research.

The other half is reaching XMSG without the `XMPF*` wrappers. Those wrappers sit on top of the XMSG
monitor call, and `DOC/` already carries the MON 200 notes - so calling it directly from PLANC is the
way in that owes nothing to a missing library.

The alternative is to find a real XMSG product distribution and install the programmer's kit, after
which these two files compile as written. Worth a look before writing a monitor-call layer, because
the kit is the supported path and the layer is ours to maintain forever.
