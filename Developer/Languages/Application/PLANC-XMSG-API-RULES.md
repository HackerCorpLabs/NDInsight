# PLANC XMSG (COSMOS XMP library) - rules a linter can check

This is the companion to [`xmp-api.json`](xmp-api.json). The JSON says what every routine and
constant IS. This page says what a PROGRAM using them must DO, written as rules with a
detection recipe for each, so a linter author does not have to read four manuals first.

Every rule below states its source. Anything that could not be confirmed from a document or a
measurement on real hardware is marked **UNVERIFIED**.

**Sources**

| Source | What it settles |
|---|---|
| `Installation/Software/ND-10609/files/XMP-B02-IMPT.readable.txt` | routine signatures, parameter types, WRITE / READ WRITE direction |
| `Installation/Software/ND-10609/files/XMP-B02-DEFS.readable.txt` | the library's own status vocabulary: `XMOK`, `XMXENTM`, all `XMX...` values |
| `SINTRAN/XMSG/SINTRAN-CHAT/XMSG-PL-VALUES-L.INCL` | the release-L raw MON 200B constants, as installed on D100 |
| `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` | the per-routine reference: parameter names, purposes, options, rules |
| `Reference-Manuals/ND-60.134.2 EN SINTRAN III Communication Guide.md` | section 4.3.2.10, the raw `XFRCV` behaviour |
| `SINTRAN/XMSG/SINTRAN-CHAT/CHATSV.PLNC`, `CHAT.PLNC` | a server and a client that compiled and ran on D100 |

Counts in the JSON: **54 routines**, **397 constants**.

---

## 1. THE RECEIVE LOOP - the rules that cost the most when broken

### Rule 1.1 - XFRCV receives on ONE port. There is no receive-on-any.

**What the documents say.** ND-60.134.2 section 4.3.2.10 gives the raw call as
`T:=XFRCV; A:=PORTNO` - one port number, one call. The COSMOS guide's `XMPFRCV` table names
parameter 2 `localPort`, "Number of the receiving port", and adds only that a zero means the
task's default port. No form of the call takes a list.

**What a two-port program must do instead.** Poll. Call `xmpfrcv` on each port in turn with
**flags ZERO** (which returns at once, message or not), and when every port came back empty,
sleep and go round. That is what the working server does:

```planc
0 =: waitFlags
DO
    xmpfrcv(waitFlags, myPort, msgType, remotePortHash, msgIdent, nBytes) =: returnStatus
    IF returnStatus = XMXENTM THEN
        xmpfrcv(waitFlags, adminPort, msgType, remotePortHash, msgIdent, nBytes) =: returnStatus
        IF returnStatus = XMXENTM THEN
            50 =: sleepUnits                    % 50 basic time units = 1 second
            1  =: sleepKind
            MONITOR_CALL('TimeOut', sleepUnits, sleepKind, sleepWhy)
        ENDIF
    ENDIF
    ...
```

`XMPFGST` is the one call that looks at more than one port: "A task may have many open ports.
It does not always know on which one the next message is arriving. XMPFGST allows the task to
check all ports belonging to the task." But it only NAMES a port that has something waiting -
you still call `xmpfrcv` on that port afterwards. **UNVERIFIED**: no program in this repo has
used `xmpfgst` on real hardware; the polling loop above is the shape that has actually run.

### Rule 1.2 - XFWTF on a two-port loop blocks on the wrong port.

`2**XFWTF` suspends the task until a message arrives **on that one port**. A server that
blocks on port A never sees anything on port B - not failing, not logging, just sitting there.

### Rule 1.3 - XFWAK is a doorbell, not a delivery.

Both manuals say it in the same words. COSMOS guide, `XMPFRCV` options: "When the wake up is
done, the message is not received, and so the receiving must be repeated. This option can be
enabled on more than one port at a time." ND-60.134.2 4.3.2.10 repeats it.

Two traps follow:

1. **The wake-up delivers nothing.** After a wake-up you must call `xmpfrcv` again.
2. **Arming it on two ports does not make one call watch two ports.** MEASURED on D100
   2026-08-21. `X-C LIST-PORTS` during the failure:

   ```
    No  Address  Owner-task   Qhead  Qlen  Chain WAK
     6  160057 152616 BAK06  161762     1      0   0   <- the room port, a message SITTING on it
    14  160147 152616 BAK06       0     0      6   1   <- the admin port, task parked HERE
   ```

   The server had a message waiting on one port and was parked on the other's wake-up. The
   client waited for ever.

   The guide names a third trap in the same paragraph: if the task is already in an XMSG wait
   (for example a secure send with wait) when the wake-up should fire, the wake-up bit is
   cleared but the task is not - and cannot be - woken.

### Rule 1.4 - a task that terminates loses its ports.

MEASURED on D100 2026-08-20: a server that drained both empty ports and RETURNed came back
`PASSIVE`, and `LIST-NAMES` showed neither of its port names. SINTRAN closes a task's ports
when the task terminates, so the wake-up had nothing left to arrive on and the server could
never be restarted. The wake-up mechanism assumes a task that STAYS ALIVE to be woken.

### LINTER RECIPE for section 1

Build a per-routine list of `xmpfrcv` / `xmpfrre` / `xmpfrrh` calls, and for each, the
**localPort argument** (parameter 2) and the **flags argument** (parameter 1).

- **ERROR** - two or more receive calls in the same loop body naming *different* port
  variables, where any of their flags arguments mentions `XFWTF` or `XFWAK`.
  Message: "XFRCV receives on one port. Poll both with flags 0 and sleep between passes."
- **ERROR** - a receive whose flags argument is the bare identifier `XFWTF` / `XFWAK` /
  `XFSEC` (see Rule 4.1 - bit position used as a value).
- **WARN** - a receive with flags 0 inside a loop that has no `MONITOR_CALL('TimeOut', ...)`
  and no other blocking call anywhere in the loop body. That is a spin; it burns the machine.
  MEASURED: when XMSG died under a running server, `xmpfrcv` stopped blocking, returned
  `XMXENRU` instantly for ever, and the loop burned the machine until somebody pressed ESC.
- **WARN** - `XFWAK` used anywhere in a program whose main loop does not terminate. If the
  task stays alive there is nothing for a wake-up to do.
- **WARN** - `XFWAK` armed and no repeat receive after it.

---

## 2. THE STATUS TEST - both obvious tests are wrong

### Rule 2.1 - the library's status model

From `XMP-B02:DEFS`, which is the only file that describes this layer:

| Value | Symbol | Means |
|---|---|---|
| 0 | `XMOK` | success. (The raw MON 200B kernel's OK is 1; the library remaps it. The file says so: `CONSTANT XMOK = 0  % NOTE change from XMSG OK=1`.) |
| 16896 | `XMXENTM` | **not an error** - "not terminated". For a receive with flags 0, it means THE PORT IS EMPTY. |
| 16897 .. 16959 | `XMX E...` | real errors: `XKXXX` plus the size of the raw negative code |
| 16961 .. 17008 | `XMX R...` | XROUT errors: `XRXXX` (16960) plus the small positive XR number |
| -1 | (no symbol) | the `XMPB*` buffer builders' only failure answer |

### Rule 2.2 - a receive is the exception

The constants file says it in its own heading: "Message types: Returned as successfull status
from XFRCV", `XMTNO=1` through `XMTPS=6`. The vendor's Appendix G server tests
`IF RETURNSTATUS >< XMOK THEN ... bad status` and only then reads `MSGTYPE`.

Both forms have been seen, so **accept either**:

```planc
IF returnStatus = XMOK OR (returnStatus >= XMTNO AND returnStatus <= XMTPS) THEN
    % a real message; its type is in the msgType OUT-PARAMETER
```

D100 printed `SV: rcv st= 0  t= 2  len= 25` - status `XMOK`, type in `msgType`.

**The two wrong tests, and exactly what each does:**

- `IF returnStatus > 0` - **accepts an empty port**. `XMXENTM` is 16896, which is greater
  than zero, and so is every error. The loop then treats an empty port as a message and
  parses whatever was left in the buffer.
- `IF returnStatus = XMOK` alone, or `IF returnStatus >< XMOK THEN discard` - **throws away
  every message** on any kernel/library pair that hands the message type back as the status.

### Rule 2.3 - the length parameter is not always a length

`msgLengthOrStat` (parameter 6 of `xmpfrcv`): "Message length in bytes. If msgType is XMTRE,
msgLengthOrStat contains the error status." Keep it before anything overwrites it -
`xmpfmst` writes over the same variable in the usual server sequence.

### Rule 2.4 - stop, do not spin, on XMXENRU

`XMXENRU` (16933) is "XMSG not running". Nothing the program can do brings XMSG back; an
operator has to restart it. Retrying is a busy loop with no end. MEASURED on D100 2026-08-18.

### LINTER RECIPE for section 2

- **ERROR** - the result variable of an `xmpfrcv` / `xmpfrre` / `xmpfrrh` call compared with
  `> 0`, `>= 1`, `<> 0` or `>< XMOK` as the gate on "did I get a message".
  Message: "'> 0' accepts an empty port (XMXENTM = 16896); '>< XMOK' can discard every
  message. Accept XMOK OR XMTNO..XMTPS."
- **WARN** - a receive whose status is compared only against `XMOK` and never against
  `XMXENTM`. The program has no way to tell an empty port from a message.
- **WARN** - a receive loop that never mentions `XMXENRU` (16933) - no way out when XMSG dies.
- **WARN** - `msgType` never read after a successful receive. The type is where the answer is.
- **INFO** - `msgLengthOrStat` used as a length without `msgType` being tested for `XMTRE`
  first.
- **ERROR** - the result of an `xmpb*` call compared against `XMOK` or an `XMX...` symbol.
  Those routines answer -1, not a symbolic status.

---

## 3. SEATS - who spends one, and who has to give it back

### Rule 3.1 - a seat only exists on a CONNECTION port

| Opened with | Has a free-connection counter? |
|---|---|
| `xmpfopn` (unnamed port) | no |
| `xmpopnm` (plain named port) | **no** |
| `xmpopcn` (connection port) | **yes** - set to `maxConnections` |

### Rule 3.2 - XROUT spends the seat, before your program sees anything

COSMOS guide, `XMPOPCN` explanation: "When somebody contacts portName by sending a letter via
XROUT, XROUT looks at the free connection counter and if it is greater than zero, XROUT
decrements it and forwards the letter. If there are no free connections, XROUT tries to find
another port with the same name."

So the seat is gone by the time the letter arrives. The arrival looks like this:

- `msgType` = `XMROU` (2) - "routed message, via XROUT". This is the **only** thing that says
  a seat was spent.
- byte 1 of the arrived message = `XSLET` (65) means a client sent a letter and a seat went
  with it. Any other service byte is XROUT answering one of your own calls, which spent
  nothing.

The caller's side of the same fact: **`xmprout` is the call that spends a seat** - a name
lookup is not free.

### Rule 3.3 - only the port's owner puts a seat back, with xmpinfc

`xmpinfc(0, portNumber, +1, serialNumber)`. Guide: "After opening a connection port using
XMPOPCN, a task can later increment ... the free connection counter associated with that
port." Nothing else ever does it.

MEASURED on D100 2026-08-18, before the working server had a reaper: `CHAT-LOBBY` went from
16 free seats to 15 when a client was killed with ESC, and stayed at 15 for good. Clearing
only the program's own member table leaves the room looking empty while it refuses everybody.

**When a seat must go back:** whenever an arrival spent one and the sender did NOT end up as
a member - a refused join, a member who left, and a member found dead. Take the "was it a
member before / is it a member now" decision from the values BEFORE the message was handled.

### Rule 3.4 - xmpinfc does not wait, and its reply arrives as a message

"Note that this routine will not wait for a reply from XROUT, and so the caller will later
receive this reply from XROUT on the port specified by portNumber." A receive loop that does
not expect that reply will treat it as garbage. `serialNumber` goes into byte 0 of the request
so a caller with several outstanding can recognise which reply is which.

### Rule 3.5 - a send does not tell you the port is dead

MEASURED on D100 2026-08-18: two clients in a room, one killed with ESC, the other spoke.
`xmpsend` to the dead port **returned XMOK**. It does not validate the destination, and the
magic number's generation does not save you.

The cure is `2**XFSEC` on the send: "The message will be returned to the sending port if it
cannot be delivered, or if the receiving port is closed." It comes back later as a message of
type `XMTRE`, and `msgLengthOrStat` then holds the reason - measured as 16915, `XMXEIMA`.

### Rule 3.6 - the routines that quietly change the current message

The guide flags these because they have to reserve and send a message to XROUT to do their
job: `xmpopnm`, `xmpopcn`, `xmpinfc`. Each **changes the task's 'task current' message**, and
`xmpinfc` also changes the 'port current' message for its port. Anything you were building in
the current message before one of these calls is no longer the current message afterwards.

### LINTER RECIPE for section 3

- **ERROR** - `xmpinfc` called with a port variable that was assigned by `xmpopnm` or
  `xmpfopn` rather than by `xmpopcn`. There is no counter on those ports.
- **WARN** - a program that calls `xmpopcn` and never calls `xmpinfc` anywhere. It leaks a
  seat per refused or departed client and eventually refuses everybody while looking idle.
- **WARN** - a receive loop that tests `msgType` for `XMROU` and has no `xmpinfc` on the path
  where the sender does not become a member.
- **WARN** - `xmpsend` / `xmpfsnd` with a flags argument of `0` in a server that keeps a table
  of remote magic numbers. Without `2**XFSEC` a dead peer is written to for ever, in silence.
- **WARN** - a send with `2**XFSEC` in a program whose receive loop never handles `XMTRE`.
  The bounce comes back as a message and will be thrown away.
- **INFO** - a message buffer built with `xmpfget` and then `xmpopnm` / `xmpopcn` / `xmpinfc`
  called before it is sent. The current message has moved.

---

## 4. FLAGS

### Rule 4.1 - every XF... option is a BIT POSITION, not a value

`XFWTF = 15` means **bit 15**. Write `2**XFWTF` (= 32768). Writing `15` sets bits 0, 1, 2 and
3 - four unrelated options - and the wait simply does not happen. The guide's own Appendix G
samples both write `2**XFWTF =: FLAGS`.

| Symbol | Bit | Flags word |
|---|---|---|
| `XFWTF` | 15 | 32768 |
| `XFWAK` | 14 | 16384 (0x4000 - confirmed on the wire in this repo) |
| `XFSEC` | 9 | 512 |

### Rule 4.2 - flags is always parameter 1, and 0 is the right value when no option applies

Every routine except the `XMPB*` buffer builders takes `flags` first. Many routines document
"Options: not implemented, flags should be zero" - `xmpfopn`, `xmpopnm`, `xmpopcn`, `xmpinfc`,
`xmpfrea`, among others.

### Rule 4.3 - the same bit means different things on different calls

Bit 13 is `XFPON`, `XFUSG`, `XFHIP`, `XFRRO` and `XFEXC` depending on which routine you pass
it to. Bit 12 is `XFOPS`, `XFRES`, `XFRMR` and `XFBNC`. Bit 10 is `XFROU` and `XFRDI`. A
linter that knows the routine can check that the named option belongs to it; one that does not
should not guess.

### LINTER RECIPE for section 4

- **ERROR** - a bare `XF...` identifier (no `2**`) passed as parameter 1 of any XMP routine,
  or assigned to a variable that is later passed as parameter 1.
  Message: "XFWTF is bit 15. Write 2**XFWTF; passing 15 sets bits 0-3."
- **WARN** - an option named for a routine it does not belong to (for example `XFRES` on a
  receive). Needs the per-routine option table; skip if unsure rather than guess.

---

## 5. BUFFERS AND LENGTHS - PLANC checks no array bounds

### Rule 5.1 - clamp a received length BEFORE reading

`xmpfrea`'s parameter 5 is `userLength`, "Number of bytes you want to read". The number a
program naturally has to hand is `nBytes` from the receive - and that is the **SENDER'S**
message size, not a size the program chose. PLANC checks no array bound, so passing it
straight in lets any peer that sends more than your array holds write past the end of it. The
overflow happens before a single field is parsed, so no amount of careful field checking
downstream can save it.

```planc
INTEGER : inBufSize := 256
BYTES   : inBuf(0:255)
...
nBytes =: readAmount
IF readAmount > inBufSize THEN
    inBufSize =: readAmount
ENDIF
xmpfrea(0, 0, ADDR(inBuf(0)) FORCE XMUSERADDRESS, 0, readAmount, readLength) =: returnStatus
```

`xmpfrre` avoids the whole question, because its `userLength` is naturally your own buffer's
size.

### Rule 5.2 - clamp every length that came off the wire, not just the message length

A one-byte length field on the wire can ask for 255. A sixteen-byte name column plus an
unchecked copy writes through everything after it. Every copy driven by a wire number needs a
bound.

### Rule 5.3 - the user address idiom

`ADDR(buf(0)) FORCE XMUSERADDRESS` - **round** brackets. The guide prints square ones and
PLANC F rejects them with `EXPECTS ")" ILLEGAL SYNTAX "["`. Plain `ADDR(buf)` is not enough.

### Rule 5.4 - the buffer builders return -1 on a bad buffer

`xmpblet`: "headerBuffer must start on an even byte boundary and lengthBuffer must be big
enough ... If one of these checks fails, -1 will be returned as error code in returnStatus."
Same for `xmpbini` and friends. That is not a symbolic status and will not match any `XMX...`
comparison.

### Rule 5.5 - offSet is READ WRITE in the real declarations

`xmpblet`, `xmpbini`, `xmpbain`, `xmpbadb`, `xmpbast` all take `offSet` as `INTEGER READ
WRITE`, not plain `WRITE`. It carries the running "bytes used so far" through the whole build
sequence. Initialise it (via `xmpbini`) before the first append.

### Rule 5.6 - message displacements are rounded

"the displacement within the message is always rounded up to the next even byte and, on an
ND-100, userDisp is always rounded down to the previous even byte before the data is read."
A program that computes an odd displacement is not reading where it thinks it is.

### LINTER RECIPE for section 5

- **ERROR** - the `msgLengthOrStat` variable of a receive passed directly as `userLength` to
  `xmpfrea` / `xmpread`, with no comparison against a buffer-size constant between the two
  calls.
  Message: "Clamp the sender's length against your buffer size first - PLANC checks no array
  bounds."
- **ERROR** - `ADDR(x)` where `x` is an array name rather than `x(0)`, or `ADDR(x[0])` with
  square brackets, in a `userAddress` position.
- **WARN** - a `FOR` loop copying from a received buffer whose bound is a variable read out of
  that buffer, with no clamp on the path.
- **WARN** - `xmpbain` / `xmpbadb` / `xmpbast` called before `xmpbini` on the same buffer.
- **WARN** - a buffer-builder sequence with no `xmpbrdy` at the end.

---

## 6. CURRENT MESSAGE - which call acts on what

This is the quietest source of wrong behaviour, because everything returns `XMOK`.

### Rule 6.1 - the calls with NO message identifier act on the CURRENT message

`xmpfrea`, `xmpfwri`, `xmpfwhd`, `xmpfsnd`. Their `xmp*` twins take one explicitly:
`xmpread`, `xmpwrte`, `xmpwrhd`, `xmpsend`.

### Rule 6.2 - xmpfsnd picks a message in two steps, and the first step surprises people

Guide: "the current (default) message buffer is assumed, namely the 'port current' message if
one exists, or, if none, the 'task current' message."

`xmpfget` makes the new buffer **task** current only - it has no port parameter and cannot
make it port current. A port that has just received a secure message may still have a port
current message, and `xmpfsnd` will send **that** instead of your reply.

**So: a program that builds its own reply with `xmpfget` should send it with `xmpsend`**, which
"is set as 'task current' message and as 'port current' message for localPort" first. The
vendor's Appendix G server gets away with `xmpfsnd` only because it never calls `xmpfget` - it
replies in the buffer that arrived, which is already both.

### Rule 6.3 - what clears the current message

- 'task current': releasing it, sending it, or receiving another message.
- 'port current': releasing it, sending it, or receiving another **secure** message.
- `xmpfscm` sets it explicitly.
- `xmpopnm` / `xmpopcn` / `xmpinfc` change it as a side effect (Rule 3.6).

### Rule 6.4 - a received message must be released or sent

Every `xmpfget` and every successful receive puts a buffer in your hands. Read it out, then
`xmpfrel` it - or send it on. Buffers neither sent nor released are leaked message space, and
`XMXEMFL` (16916, "message space full") is what a leak eventually looks like.

### LINTER RECIPE for section 6

- **ERROR** - `xmpfsnd` in a routine that also calls `xmpfget`. Message: "xmpfget makes the
  buffer task current only; xmpfsnd prefers the port current message. Use xmpsend."
- **WARN** - a successful-receive branch with no `xmpfrel` and no send call on any path.
- **WARN** - `xmpfget` whose returned identifier is never passed to `xmpfrel`, `xmpsend`,
  `xmprout` or `xmpfrtn`.
- **INFO** - `xmpfrea` / `xmpfwri` called after `xmpopnm` / `xmpopcn` / `xmpinfc` without an
  intervening `xmpfscm` or receive. The current message moved underneath them.

---

## 7. THE ANSWER PATH - hashed port versus magic number

### Rule 7.1 - what a receive hands back cannot be used to answer

`xmpfrcv` parameter 4 is `remotePort`, "**Hashed** magic number of remote port". You cannot
send to a hash.

### Rule 7.2 - xmpfmst is how you get an address you can send to

`xmpfmst(0, msgIdent, msgType, senderMagic, nBytes)` - parameter 4 is the FULL
`INTEGER4` magic number of the sending port. The guide's own walkthrough of the letter
exchange says the same: the client "can then use the XMPFMST routine to extract the magic
number of the remote (server) port, and direct communication ... can begin".

### Rule 7.3 - a magic number is 32 bits

`INTEGER4` everywhere: `xmpfsnd` parameter 3, `xmpsend` parameter 4, `xmpfp2m` parameter 3,
`xmpfm2p` parameter 2. A magic number is a full port address and does not fit one word.

### Rule 7.4 - answering IS giving away your magic number

The guide is explicit: a server "can, and it normally will, check that the sending task is
allowed to use the server before it sends a (positive) reply to the requester, and thereby
gives away its own magic number. If the server task does not want to give away its own magic
number, it can do so by sending a (negative) reply with the XFFWD (forward message) option."
`xmpfcpv` is the call that does the checking.

### LINTER RECIPE for section 7

- **ERROR** - the `remotePort` out-variable of a receive passed as `remoteMagicNum` to
  `xmpfsnd` / `xmpsend`. Message: "That is a hashed port. Call xmpfmst for the full magic
  number."
- **ERROR** - a magic-number variable declared `INTEGER` rather than `INTEGER4`.
- **WARN** - `xmpfmst` overwriting a length variable that the receive filled and that is read
  again afterwards.

---

## 8. THE LETTER - reaching a server BY NAME

The sequence, from the guide's `XMPROUT` example and from the working client:

1. `xmpblet(letterBuf, 64, offSet, serial, systemName, portName)` - format an `XSLET` header
   into your OWN buffer. It sends nothing. A zero-length `systemName` means the local system.
2. `xmpfget(0, size, msgIdent)` - reserve a message.
3. `xmpfwri(0, 0, ADDR(letterBuf(0)) FORCE XMUSERADDRESS, 0, offSet, wLength)` - copy the
   header in.
4. `xmpfwri(0, wLength, ...your data..., 0, dataLen, offSet)` - your data goes **after** the
   header. XROUT never looks at it.
5. `xmpfopn(0, myPort)` - you need a port of your own for the answer.
6. `xmprout(0, msgIdent, myPort)` - hand it to XROUT.

### Rule 8.1 - a refusal comes back as a MESSAGE, not as a bad returnStatus

"if XMSG is unable to send the letter to XROUT in the specified system, or if the destination
XROUT does not know the name of the destination port ... or if portName is a connection port
with no free connections, then XROUT will return the letter (message) to the sending port with
an error status." The client tells a server reply from an XROUT refusal by the **message
type**.

### Rule 8.2 - the reply arrives with the letter header still in front of it

The server sees the XSLET header first, then the user data. Byte 0 of that header is the
serial number, not user data. Derive where the payload starts from the header's own length
word - never from a fixed number.

### LINTER RECIPE for section 8

- **WARN** - `xmpblet` with no following `xmprout` in the same routine.
- **WARN** - `xmprout` in a program that never calls `xmpfopn`, `xmpopnm` or `xmpopcn`. There
  is no port for the answer.
- **WARN** - a client that calls `xmprout` and whose receive loop never tests `msgType`. It
  cannot tell a server's answer from XROUT's refusal.
- **INFO** - a fixed constant used as the payload offset in a routed (`XMROU`) arrival.

---

## 9. DECLARATIONS AND THE BUILD

### Rule 9.1 - include ONE constants file, never both

`XMP-B02:DEFS` and `XMSG-PL-VALUES-L:INCL` share **184** constant names. A redeclaration in
this PLANC compiler is not a warning: it drew `IDENTIFIER ALREADY SPECIFIED/DECLARED`, then
`ASSERT VIOLATION AT 136747B`, and took the whole batch job with it.

- Talking through the `XMPF*` library: `XMP-B02:DEFS` + `XMP-B02:IMPT`.
- Talking raw MON 200B from NPL or MAC: `XMSG-PL-VALUES-L:INCL`.

The two files agree on 183 of the 184 shared names. The one real difference is `X5FUN`
(43 in the older DEFS, 48 in the L-era file) - a sentinel, not something a program passes.
`XSMAX` also differs, for the same reason: it is defined as "the highest service value", and
release L added services. Both differences are recorded in the JSON as `conflict`.

### Rule 9.2 - do not redeclare a constant the include already provides

The same fatal `ASSERT VIOLATION` came from a program declaring its own `XFWTF` and its own
`XSLET` when the include already had them.

### Rule 9.3 - the includes go INSIDE the MODULE

Above `MODULE` the declarations land in the outer scope, the body cannot see them, and every
call fails `NOT PREVIOUSLY DECLARED` while the include's own lines draw no diagnostic at all.

### Rule 9.4 - every included file must end with `$EOF`

Without it the compiler runs off the end of the include, treats that as the end of the whole
compilation, and reports **0 DIAGNOSTICS** - a clean-looking build that silently skipped the
program. The tell: LINES COMPILED stops at the `$INCLUDE` line, and the next command answers
`COMMAND NOT PERMITTED WITHIN MODULES`.

### Rule 9.5 - names are unique in their first TEN characters

`memberName` and `memberNameLen` both start `MEMBERNAME`, so the second silently became a
redeclaration of the first, and every `memberName(slot, i)` then drew `MORE SUBSCRIPTS THAN IN
THE ARRAY DECLARATION`. Four errors, one cause, and none of them mentioned the length. The
compiler listing gives it away: it prints the truncated names.

### Rule 9.6 - the link needs the right library

`LOAD XMP-100-1-B02` (1-bank) or `XMP-100-2-B02` (2-bank), from `(SYSTEM)` after the ND-10609
installer has run. It is **not** `(UTILITY)XMSG-LIBRARY-L03` - that is the NPL-level XMSG
support library, eleven internal entries, not one `XMPF` symbol, and loading it leaves every
call `UNDEFINED`. Load the program first, then the library, then `PLANC-1BANK-F00` for
`5MON_P`.

### Rule 9.7 - a monitor-call failure is a ROUTINEERROR, not a return value

`ON ROUTINEERROR DO ... ENDON`. A failed `MON50` leaves 46 (SINTRAN's "no such file") in the
device number, not zero, so testing the number is wrong. Clear the failure flag AFTER the
`ON ... ENDON` block, not before - cleared before, the handler body has already run and every
open reports failure while returning a good device.

### LINTER RECIPE for section 9

- **ERROR** - both `XMP-B02:DEFS` and `XMSG-PL-VALUES-L` included in one compilation unit.
- **ERROR** - a `CONSTANT` declaration whose name matches a constant in `xmp-api.json`.
- **ERROR** - an `$INCLUDE` of the XMP files placed before the `MODULE` line.
- **ERROR** - an included file whose last non-blank line is not `$EOF`.
- **ERROR** - two declared names in the same scope whose first ten characters match.
- **WARN** - a `MON50` result compared against 0 rather than guarded by `ON ROUTINEERROR`.

---

## 10. Small ones that still bite

- **Terminal input carries even parity in bit 7.** CR arrives as 141, not 13. Mask with 127.
- **A written SINTRAN text file needs the right parity too**, or `LIST-FILE` shows nothing at
  all, silently. Note the control run that matters here: `LIST-FILE` and `QED` answer
  "PARITY ERROR" for *every* file pushed onto the machine by an FA transfer, good or bad, so
  that symptom is not evidence about your bytes. Pull the file off and look at it.
- **A string literal cannot be stored into a `BYTES` element.** `' ' =: outBuf(at)` draws
  `ILLEGAL DATA TYPE`. Hold single characters in a one-element `BYTES` and copy byte to byte.
- **A local declaration cannot carry an initialiser.** `BYTES : b := ' '` inside a routine
  draws `INITIAL VALUE ILLEGAL HERE`; the identical line at MODULE level compiles.
- **PLANC string literals subscript from ZERO.** `name(0:len-1)` is the whole of a length-`len`
  name.
- **There is no `LENGTH`.** The routine is `SIZE`, spelled `SIZE ident =: var`.
- **`XMPFFRE` does not exist.** The release call is `XMPFREL`.
- **The manual's `XMPF2PM` is a typo.** The entry point is `xmpfp2m`.
- **`XMMAXNameLength` is 32 in `XMP-B02:DEFS`, "actually installation-dependent".** A longer
  name returns -1; a name over the generation-time limit is silently truncated by XMSG.

---

## 11. What is NOT settled

- **`xmpxets` / `xmpxrts`** - declared in `XMP-B02:IMPT` and exported by
  `XMP-100-1-B02:BRF`, documented nowhere in ND-60.164.3. Their shape is unusual:
  `ROUTINE INTEGER,VOID (INTEGER WRITE)` - an in-value and no out-value, so they cannot be
  written `xmpxets(x) =: status`. What they DO is **UNVERIFIED**.
- **`xmpfgst` on real hardware** - the documented multi-port scan, never exercised in this
  repo. **UNVERIFIED**.
- **J-on-L compatibility** - the B02 library and DEFS were built against XMSG version J; the
  running kernel on D100 is release L. `X5FUN` 43-vs-48 is the visible gap. Old-caller-on-
  newer-kernel is the compatible direction on every other ND product measured, but for XMSG
  it is **UNVERIFIED** beyond the calls the chat pair actually makes.
- **The ND-500 path** (`XMP-500-B02:NRF`) - how the ND-500 library reaches the ND-100-side
  XMSG kernel is undocumented and untried here. **UNVERIFIED**.
- **Which success form a given kernel returns from a receive** (`XMOK` vs the message type).
  D100 returned `XMOK`; the constants file documents the message type. Accept both.

---

## See also

- [`xmp-api.json`](xmp-api.json) - the machine-readable table: 54 routines, 397 constants,
  the error-base rule computable from `error_model`
- [COSMOS-XMP-LIBRARY.md](COSMOS-XMP-LIBRARY.md) - what the library is, what ships, how to build
- [PLANC-XMSG-COMMUNICATION.md](PLANC-XMSG-COMMUNICATION.md) - the D100 field notes
- `SINTRAN/XMSG/DOC/XMSG-API.md` - the MON 200B function interface underneath the library
