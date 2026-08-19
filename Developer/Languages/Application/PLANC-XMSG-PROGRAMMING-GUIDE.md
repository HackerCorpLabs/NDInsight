# Writing XMSG programs in PLANC

Everything needed to write a working XMSG client or server in PLANC on SINTRAN III, in the order you
need it: what the pieces are, a hello world you can type in, then each call, then the patterns, then
the traps.

**Every claim here is one of two things** and says which:

 - **VERIFIED** - quoted from `Operations/Cosmos/ND-60164-3-EN  COSMOS Programmer Guide.md`,
   `Reference-Manuals/ND-60.117.5 EN PLANC Reference Manual.md`, or the shipped `XMP-B02:DEFS`;
 - **MEASURED** - watched happening on a real ND-100 (D100, SINTRAN III VSX/500 K, XMSG Release L)
   during 2026-08. Where a measurement contradicted a belief, the belief is written down too, because
   a guide that hides its wrong turns sends you down them again.

The two working programs this is drawn from are
`SINTRAN/XMSG/SINTRAN-CHAT/CHAT.PLNC` (a client) and `SINTRAN/XMSG/SINTRAN-CHAT/CHATSV.PLNC`
(a server). They compile and run; read them when a paragraph here is not enough.

---

## 1. The idea, in four words: ports, magic numbers, messages, letters

**A PORT is where you receive.** A task opens one and XMSG queues messages there. A port is a small
integer, local to your task.

**A MAGIC NUMBER is where you send.** It identifies a port anywhere on the network - node, port and a
generation counter rolled into 32 bits. You cannot invent one and you cannot look one up.

```
Magic = (node << 16) | (port * 128 + generation)
```

The generation is why a magic number stops working when the owner dies rather than silently
addressing whoever gets that port next.

**A MESSAGE goes port to port.** You must already know the destination's magic number.

**A LETTER goes to a NAME.** XROUT, the routing task, looks the name up and forwards your letter to
whoever registered it. This is the only way to reach a task whose magic number you do not know.

> **VERIFIED**, and it is the whole design: *"XROUT never hands out somebody else's magic number; it
> forwards your letter, and the recipient learns YOUR address from it and may choose to answer."*

So every conversation has the same shape:

```
client  --letter to a NAME-->  XROUT  --forwards-->  server
client  <--------- ordinary message (the answer) ---- server     server now knows the client
client  ---------- ordinary message --------------->  server     client now knows the server
```

**After the first exchange nobody uses XROUT again.** That matters for performance and for seats
(section 8).

---

## 2. Hello world

A program that opens a named port, waits for one message, prints it, and stops.

```planc
MODULE hello

$INCLUDE XMP-B02:DEFS
$INCLUDE XMP-B02:IMPT

    INTEGER : myPort
    INTEGER : returnStatus
    INTEGER : msgType, remotePortHash, nBytes, readLength
    INTEGER : waitFlags := 0
    INTEGER4 : senderMagic
    BOOLEAN : uniqueName := TRUE

    INTEGER : bufSize := 256
    BYTES : inBuf(0:255)
    INTEGER ARRAY : stack(0:1000)

    PROGRAM : helloMain
        XMMSGIDENTIFIER : msgIdent
        INTEGER : readAmount

        INISTACK stack

        % A FLAGS WORD IS 2 TO THE BIT POSITION. XFWTF is 15, not 32768.
        2**XFWTF =: waitFlags

        xmpopnm(0, 'HELLO-PORT', uniqueName, myPort) =: returnStatus
        IF returnStatus >< XMOK THEN
            OUTPUT(1, 'AL20', 'HELLO: cannot open$')
            RETURN
        ENDIF

        OUTPUT(1, 'AL18', 'HELLO: waiting...$')

        xmpfrcv(waitFlags, myPort, msgType, remotePortHash, msgIdent, nBytes) &
            =: returnStatus

        IF returnStatus = XMOK OR &
           (returnStatus >= XMTNO AND returnStatus <= XMTPS) THEN
            xmpfmst(0, msgIdent, msgType, senderMagic, nBytes) =: returnStatus

            % CLAMP. nBytes is the SENDER'S size, not a size you chose.
            nBytes =: readAmount
            IF readAmount > bufSize THEN
                bufSize =: readAmount
            ENDIF

            xmpfrea(0, 0, ADDR(inBuf(0)) FORCE XMUSERADDRESS, &
                    0, readAmount, readLength) =: returnStatus
            xmpfrel(0, msgIdent) =: returnStatus

            OUTPUT(1, 'AL16', 'HELLO: got     $')
            OUTPUT(1, 'I6', readLength)
            OUTPUT(1, 'AL1', '$')
        ENDIF
    ENDROUTINE

ENDMODULE
```

Four things in that short program are the ones people get wrong; all four are in section 9.

---

## 3. Building it

**The XMPF calls are NOT in the XMSG library.** They come from product ND-10609, and the library is
linked in a SEPARATE step - the compiler will not pull it in.

```
@PLANC-100-F00
COMPILE HELLO:PLNC,"HELLO:LIST","HELLO"
EXIT

@BRF-LINKER-C01
PROGRAM-FILE "HELLO"
LOAD HELLO
LIBRARY-MODE ON
LOAD XMP-100-1-B02        <- the XMPF* code. NOT XMSG-LIBRARY-L03.
LOAD PLANC-1BANK-F00      <- the PLANC runtime
LIST-ENTRIES-UNDEFINED
EXIT
```

**MEASURED, each of these cost a build:**

 - `XMSG-LIBRARY-L03` holds **no XMPF code at all** - it is the NPL-level support library. Loading
   it moves the free address by exactly nothing.
 - **Do not set PROG-FILE in the compiler.** With it set, the compiler links straight into the
   program file and leaves the BRF empty, which then loads as nothing.
 - **LOAD before COMPILE is accepted and does nothing**, silently - a library only supplies units
   something already refers to, and at that point nothing does. LOAD after COMPILE is refused with
   `COMMAND NOT PERMITTED WITHIN MODULES`. Hence two steps.
 - **The source must be CRLF.** A file with bare LF makes the compiler answer `LINE IS TOO LONG` on
   every line, including a one-character line.
 - **Every INCLUDED file must end with `$EOF`** - without it the compiler treats the end of the
   include as the end of the whole compilation and reports **0 DIAGNOSTICS** while skipping your
   program. The tell is the line count.
 - **`$EOF` in the MAIN source is different**: it ends the compiler SESSION, so every command after
   it goes to SINTRAN instead. Do not put one there.

**Read the listing, not the screen.** Diagnostics scroll past, and the `0 DIAGNOSTICS` you can see
belongs to the SECOND pass. `Not one "*** ERROR" in the listing` is the only green light.

---

## 4. Opening and closing

### Two kinds of port

| Call | What you get | Use it for |
| --- | --- | --- |
| `xmpopnm(flags, name, unique, port)` | a NAMED port | anything - clients, servers, trunks |
| `xmpopcn(flags, name, unique, seats, port)` | a named CONNECTION port with a **seat counter** | a service with a limit on simultaneous users |

**A connection port is the one with seats.** XROUT decrements the counter to forward a letter and
**only the owner puts one back** (section 8). A normal named port has no counter at all - which is
exactly why a server-to-server trunk should use `XMPOPNM`, or every trunk permanently spends one of
the room's user seats.

### Your own magic number

```planc
xmpfp2m(0, myPort, myMagic) =: returnStatus
```

Worth having so an arrival can be tested against it. **MEASURED:** a server's own reply can arrive
back at its own port, and without this comparison that is indistinguishable from a peer's message.

### Closing - and the good news

```planc
xmpfcls(0, myPort) =: returnStatus     % close one port
xmpfdct(0) =: returnStatus             % disconnect: close everything
```

**You usually do not need either.** VERIFIED, and MEASURED:

> SINTRAN runs an automatic disconnect *"on return to the SINTRAN command processor"* and *"on log
> out or RT program termination"*, and a disconnect closes every port the task opened. Closing a
> port **clears its name from XROUT's name table**.

**MEASURED 2026-08-18:** a server was broken out of with ESC, returned to `@`, and its name was gone
from `LIST-NAMES` immediately. So a name that LINGERS means the task never terminated - usually
because the terminal was taken away underneath it with `STOP-TERMINAL`. That is an operating mistake,
not a missing `XMPFCLS`.

---

## 5. Sending

Three calls, in this order:

```planc
xmpfget(0, length, ident) =: returnStatus                   % reserve a buffer
xmpfwri(0, 0, ADDR(outBuf(0)) FORCE XMUSERADDRESS, &
        0, length, writtenLength) =: returnStatus           % fill it
xmpsend(sendFlags, ident, myPort, magic) =: returnStatus    % send it
```

### XMPSEND, not XMPFSND

**This is a real bug, not a tidy-up.** `XMPFSND` takes no message parameter and picks *"the 'port
current' message if one exists, or, if none, the 'task current' message"*. `XMPFGET` only makes the
new buffer TASK current. If your port has just received something, a 'port current' message may
still exist and **XMPFSND sends that instead of the reply you just built.**

`XMPSEND` names the message explicitly and removes the question. The vendor's own sample server gets
away with `XMPFSND` only because it never calls `XMPFGET` - it replies in the buffer that arrived.
You cannot copy that when you broadcast, because a broadcast needs one fresh buffer per recipient.

### The flags word, and why yours should be secure

```planc
2**XFSEC =: sendFlags        % secure: undeliverable messages come BACK
```

> **VERIFIED:** *"Non-secure messages are discarded and released by XMSG if they cannot be
> delivered."*

**MEASURED, and this is the most useful single fact in this guide:** `XMPSEND` **does not validate
the destination**. Sending to a magic number whose port has closed returns `XMOK`. Two chat clients,
one killed with ESC, the other spoke: the broadcast to the dead port succeeded and nothing was ever
reported. With `XFSEC` set, the same send comes back as a **returned message** (section 6) carrying
reason **16915 = XMXEIMA, "Invalid magic number"** - the very code that was being watched for on the
send's own status, in the wrong place.

**So: set XFSEC on anything whose failure you need to know about.**

Useful flag bits, all bit POSITIONS (use `2**bit`):

| Name | Meaning |
| --- | --- |
| `XFSEC` | secure - return it to me if it cannot be delivered |
| `XFWTF` | wait until something is there (on receive) / until queued (on a secure remote send) |
| `XFROU` | send this to XROUT instead of a magic number - this is what makes it a letter |
| `XFHIP` | high priority - put it at the head of the receiver's queue |
| `XFBNC` | bounce - return it to me when the receiver tries to take it |

---

## 6. Receiving

```planc
xmpfrcv(waitFlags, myPort, msgType, remotePortHash, msgIdent, nBytes) =: returnStatus
xmpfmst(0, msgIdent, msgType, senderMagic, nBytes) =: returnStatus
xmpfrea(0, 0, ADDR(inBuf(0)) FORCE XMUSERADDRESS, 0, readAmount, readLength) =: returnStatus
xmpfrel(0, msgIdent) =: returnStatus
```

`XMPFRCV` hands back only a **hashed** port. `XMPFMST` gives you the full magic number, which is what
you need to answer.

### Testing the status is where people go wrong

**A receive succeeded when the status is XMOK *or* a message type.** Both forms turn up.

```planc
IF returnStatus = XMOK OR &
   (returnStatus >= XMTNO AND returnStatus <= XMTPS) THEN
```

**MEASURED:** a client tested for the 1..6 range alone, `XMPFRCV` returned `XMOK`, and every welcome
was dequeued and thrown away - the server was perfect and the client looked broken. Accept both.

### Message types

| Constant | Value | Meaning |
| --- | --- | --- |
| `XMTNO` | 1 | ordinary message |
| `XMROU` | 2 | **came via XROUT - it is a letter, and it still has its header** |
| `XMTHI` | 3 | high priority |
| `XMTRE` | 4 | **RETURNED - one of yours that could not be delivered** |
| `XMKIK` | 5 | XROUT was kicked; no message |
| `XMTPS` | 6 | pseudo message |

### Handling a returned message (XMTRE)

**Catch it BEFORE your normal parser.** The bytes are your own outgoing message coming home; parsing
them as an arrival makes a server answer itself.

 - the **reason** is in `XMPFRCV`'s `nBytes` - VERIFIED: *"If msgType is XMTRE, msgLengthOrStat
   contains the error status."* Keep it before `XMPFMST` overwrites it.
 - **MEASURED, and not stated in the guide:** `XMPFMST` on a returned message hands back **the port
   that could not be reached**, not your own. That is what makes the whole mechanism usable - it
   tells you WHO died.

---

## 7. Letters: reaching a name

The client side:

```planc
xmpblet(letterBuf, 64, offSet, 123, hereSystem, roomName) =: returnStatus
% ... write your own data at letterBuf(offSet) onwards ...
% then send it with XFROU set
```

`XMPBLET` builds the XSLET header and tells you, in `offSet`, where your data goes.

### The server sees the header too - this is the classic trap

**VERIFIED** (COSMOS Programmer Guide, appendix B, "XROUT Message Format"):

```
byte 0     serial number, echoed back unchanged
byte 1     the service number (XSLET = 65). XROUT OVERWRITES IT WITH THE STATUS ON A REPLY
bytes 2-3  LENGTH OF THE REMAINDER, in bytes, followed by parameter blocks
```

So your data starts at:

```planc
payloadAt = 4 + inBuf(2) * 256 + inBuf(3)
```

**DERIVE IT, NEVER HARD-CODE IT.** For a ten-character room name it comes out as 16, and 16 is wrong
the moment the name changes length.

**MEASURED**, a real 25-byte join:

```
123  65  0 12 | 255 10 'CHAT-LOBBY' | 1 5 'RONNY' 0 0
 ^serial ^len=12   ^parameter block      ^payload at 16
```

The tempting alternative reading - `6 + name length` - fits this one capture and is wrong in general,
because it cannot see a second parameter block.

### Reading a refusal

Because XROUT overwrites byte 1 with the status, **a refused letter is your own letter coming back**.
Byte 0 is still your serial; byte 1 is no longer 65.

**MEASURED under load:** status **30 = XMXRBUS, "Service busy - try later!"**. A connection port does
not absorb a burst of joins - **a client that does not retry simply does not get in.** Treating these
as corrupt bodies hides the answer completely.

---

## 8. Seats, and the mistake everybody makes

A connection port opened with `XMPOPCN` has a **free-connection counter**, shown as `Free SPs` by
`X-C` -> `LIST-NAMES`.

 - **XROUT decrements it to FORWARD a letter** - before your program has seen a byte.
 - **Only the owner puts one back**, with `xmpinfc(0, myPort, 1, 0)`. That call is local and never
   crosses the network, so **a client cannot return a seat by sending anything**.

**THE RULE THAT TOOK TWO PROGRAMS TO LEARN: the seat belongs to the ARRIVAL, not to any kind of
message inside it.**

**MEASURED 2026-08-18**, twenty clients joining a real room at once: `CHAT-LOBBY` went from 16 free
seats to 2 and stayed there, although every welcomed member left cleanly. The server returned a seat
only on a clean leave, so an empty letter, a join refused for a duplicate name, and a letter from a
stranger each cost a seat for good.

The fix is one rule applied to every letter:

```planc
% after handling the letter
findByMagic(senderMagic) =: nowMember
IF nowMember = 0 THEN
    xmpinfc(0, myPort, 1, 0) =: returnStatus     % nobody is holding it - give it back
ENDIF
```

It cannot double-count, because **only the join is a letter** - everything afterwards is an ordinary
port-to-port message and never enters that branch.

**And when a client vanishes without leaving?** Its seat is held until something tries to send to it.
That is why servers should send with `XFSEC` and reap on the returned message (section 5 and 6).

---

## 9. The traps, collected

Each of these was paid for once.

**A flags word is `2**bit`.** `XFWTF` is 15. Passing 15 sets bits 0..3 - four unrelated options - and
the wait simply does not happen.

**Do not re-declare a constant from `XMP-B02:DEFS`.** Declaring your own `XFWTF` drew
`IDENTIFIER ALREADY SPECIFIED` and then `ASSERT VIOLATION AT 136747B`, taking the batch job with it.

**Do not include `XMSG-PL-VALUES-L:INCL` alongside `XMP-B02:DEFS`** - they share 184 names.

**Includes go INSIDE `MODULE`.** Above it they land in the outer scope, the body cannot see them, and
every call fails `NOT PREVIOUSLY DECLARED` while the include's own lines draw no diagnostic at all.

**`ADDR(buf)` is not enough** - the idiom is `ADDR(buf(0)) FORCE XMUSERADDRESS`, with ROUND brackets.
The guide prints square ones and PLANC F rejects them.

**PLANC identifiers are unique in TEN characters.** `memberName` and `memberNameLen` become the same
name, and the errors you get talk about subscripts rather than about the collision. The listing gives
it away by printing the truncated names.

**PLANC checks NO array bound.** Every length that arrived from the network must be checked against
both your buffer AND the message length before you use it.

**A string literal subscripts from ZERO.** `BYTES : name := 'RONNY'` has `R` at index 0, so a loop
over `1:len` drops the first letter - it put `ONNY` on the wire and only a raw dump found it.

**`'ALn'` formats a STRING.** Hand it a byte and you get the byte's NUMBER: `RONNY` printed as
`8279787889`. Copy the byte into a one-byte `BYTES` first.

**And the same conversion is needed going the OTHER way.** `' ' =: outBuf(at)` - putting one literal
character into a message you are building - does not compile either, because `'x'` is a STRING and
an element of a `BYTES` array is a BYTE:

```
   1082   (422)/BUILDWHO  *** ERROR   - ILLEGAL DATA TYPE "OUTBUF"
```

**Note that it blames the ARRAY**, so you go and stare at the declaration, where nothing is wrong.
Declare `BYTES : spaceByte := ' '` and store `spaceByte(0)`. One rule covers both directions: a
literal character and a buffer byte are different types, and a one-element `BYTES` is the bridge.

**`'ALn'` IS A FIELD WIDTH AND THE COUNT INCLUDES THE `$`.** `'CHAT: bye$'` is ten characters. Too
small cuts the line off, too large pads it, and **nothing checks it** - the build is clean and the
damage is only ever on someone's screen. Five wrong widths went into one source in a single sitting.

**A NAME PASSED TO XROUT MUST BE EXACTLY ITS OWN LENGTH.** A `BYTES` argument carries the array's
whole declared size, so a 21-byte buffer holding a 10-byte room name asks XROUT for a name with
eleven bytes of rubbish on the end. Pass a subarray - the bounds may be variables:

```planc
xmpblet(letterBuf, 64, offSet, 123, hereSystem, roomName(0:lenRoomName - 1))
```

This only bites once a name becomes a BUFFER rather than a literal, which is exactly what happens
the first time a program lets the user change rooms.

**Clamp before reading.** `nBytes` is the SENDER'S size. Passing it straight to `XMPFREA` lets any
peer write past the end of your buffer, before a single field is parsed.

**A blocking receive becomes a SPIN when XMSG stops.** `XMPFRCV` with `XFWTF` cannot wait on
something that is gone: it returns `16933 = XMXENRU` instantly, for ever. **MEASURED** - a server
printed that line as fast as the terminal could take it until somebody pressed ESC. Test for it and
stop; there is nothing to retry, an operator has to restart XMSG.

**ESC does not reach a program suspended in `XMPFRCV`.** The break is armed and lands only when a
message arrives and the call returns. So to stop such a server you must first make it RECEIVE
something. A server on a terminal line can otherwise pin that line indefinitely.

**AND `STOP-TERMINAL` GIVES YOU ONE ATTEMPT TO GET IT BACK.** It asks
`... DO YOU WANT TO REMOVE IT FROM THIS STATE AND STOP IT?` - which is a PROMPT, so whatever you
send next is read as the answer. Send anything but `YES` and every later attempt answers
**`ALREADY EXECUTED BY TERMINAL: nn`**, from every terminal, and the line stays held. Measured, and
it cost the tidy-up at the end of a session.

**All of which is the strongest argument for running a server as an RT PROGRAM**, where the whole
problem does not exist: an RT program holds no terminal and is stopped by **`@ABORT <name>`** - one
command, no prompt, nothing to time. See
`Developer/Languages/Application/PLANC-RT-AND-REENTRANT-PROGRAMS.md`.

---

## 10. Error numbers

There are **only two error bases** in the shipped COSMOS definitions:

```
XKXXX  / XMXKXXX = 16896 = 41000B     XMSG kernel
XRXXX  / XMXRXXX = 16960 = 41100B     XROUT
```

The library form is **base + n**, so `XMXRUNN = 16962`, `XMXRDDF = 16963`, `XMXRBUS = 16990`.

**That is useful as a negative:** any status at or above 16896 belongs to one of those two, and
**anything below is neither**, whatever it looks like.

**On the wire, XROUT errors are NEGATIVE.** The same fault therefore has two numbers: `XRMFL` is
`0xFFDE` = -34 in a control status, and `16994` through the library. Both are correct.

Ones you will actually meet:

| Status | Name | Meaning |
| --- | --- | --- |
| 16896 | `XMXENTM` | nothing there - the normal answer to a receive on an empty port |
| 16915 | `XMXEIMA` | invalid magic number - the reason on a RETURNED message |
| 16932 | `XMXEPCL` | remote port closed while the message was queued |
| 16933 | `XMXENRU` | **XMSG is not running** - stop, do not retry |
| 16962 | `XMXRUNN` | unknown name - is the server actually running? |
| 16963 | `XMXRDDF` | another port already has this name |
| 16990 | `XMXRBUS` | **service busy - try later**; retry is the caller's job |
| 16994 | `XMXRMFL` | not enough message table space in the remote system - transient, retry |

---

## 11. Two patterns, end to end

### A client

```
open a port                     xmpopnm
build a letter to the NAME      xmpblet, write your data at offSet
send it with XFROU              xmpfget / xmpfwri / xmpsend
receive the answer              xmpfrcv (accept XMOK too!)
keep the sender's magic         xmpfmst  <- this is the server's address
everything afterwards           ordinary messages to that magic - no XROUT
```

### A server

```
open a connection port          xmpopcn with a seat count
loop for ever:
    receive, waiting            xmpfrcv with 2**XFWTF
    stop if XMXENRU             XMSG has gone; nothing to retry
    handle XMTRE first          a returned message - reap the member it names
    if XMROU, skip the header   payloadAt = 4 + inBuf(2)*256 + inBuf(3)
    act on it
    settle the seat             if the sender is not seated, xmpinfc it back
```

---

## 12. Where to look next

| For | Read |
| --- | --- |
| **installing the finished program** - a server as an RT program with no terminal, a client as a subsystem every user shares | **`Developer/Languages/Application/PLANC-RT-AND-REENTRANT-PROGRAMS.md`** |
| what is installed and how to list it | `Developer/Languages/Application/PLANC-XMSG-COMMUNICATION.md` |
| the XMP library call by call | `Developer/Languages/Application/COSMOS-XMP-LIBRARY.md` |
| monitor calls from PLANC | `Developer/Languages/Application/PLANC-MONITOR-CALLS.md` |
| a working client | `SINTRAN/XMSG/SINTRAN-CHAT/CHAT.PLNC` |
| a working server | `SINTRAN/XMSG/SINTRAN-CHAT/CHATSV.PLNC` |
| the vendor reference | `Operations/Cosmos/ND-60164-3-EN  COSMOS Programmer Guide.md` |
| bringing the machine up, and its traps | `SINTRAN/XMSG/DOC/BRINGUP-ORDER-AND-TRAPS-2026-08-18.md` |
