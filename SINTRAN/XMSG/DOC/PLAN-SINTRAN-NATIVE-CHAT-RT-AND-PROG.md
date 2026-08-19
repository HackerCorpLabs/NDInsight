# Plan: a SINTRAN-native chat - RT server and PROG client

The chat that exists today runs on OUR node in C#. A SINTRAN user reaches it by connecting to us.
This plan is the other way round: **the chat server runs ON the ND as a real-time program, and the
client is an ordinary SINTRAN PROG** - so two ND machines can chat with no C# in the path at all.

Written to be built together. Nothing here is started, and the open questions are marked as such
rather than guessed at.

---

## 1. What we already know, and it is a lot

The C# work is not thrown away - it is the specification, and it is verified against real machines:

 - **The room rules** are already isolated and transport-free (`ChatRoom`), with tests. The RT
   program implements the same rules; the tests say what "same" means.
 - **The message format** is ours to choose and is already small and byte-defined
   (`ChatMessage`: kind, nickname length, name, 2-byte text length, text). A PLANC record and an
   assembler layout can mirror it exactly.
 - **The XMSG calls** an RT server needs are the ones already modelled and exercised:
   `XSOPN` (open a port), `XSNAM` (claim a name), `XSCON` (a connection port with seats),
   `XSSND` / `XSRCV`, `XSCLS`. See `DOC/XMSG-API.md` for the MON 200B calling convention and
   `Xmsg.Api` for a working model of each.
 - **The seat mechanism** is XROUT's free-connection count, which is how the real file server
   builds its 30 seats. We do not have to invent admission control.

## 2. The two programs

### 2a. `CHATSV` - the server, an RT program

Runs under SINTRAN as an RT program (`@RT CHATSV`), like `XFTRAD` and `FSART` do.

Its whole life:

```
open a port
claim the name CHAT-<room> with N connection seats
loop forever:
    wait for a message
    decode it
    apply the room rules
    send what has to be sent to each member
```

Language: **PLANC** first. The room is a table of (magic number, name) and the rules are string
comparisons and list edits - exactly what PLANC is comfortable with, and the ND PLANC compiler is
already in use in this repo. Assembler only for anything PLANC cannot reach.

**How it waits: ANSWERED from the COSMOS guide, no machine needed.** `XFRCV` with the
`XFWTF` flag (bit 15, "wait if the operation is not terminated") blocks until a message arrives.
That is the whole RT server loop, and the guide gives the server shape outright:

```
XMPOPCN   open a CONNECTION port, name it, and set the seat count
XMPFRCV   wait for a message          (FLAGS + XFWTF)
XMPFMST   learn the sender's FULL magic number (XMPFRCV gives only a hash)
XMPFGET   reserve a buffer - it becomes the CURRENT message
XMPFWRI   write the reply into the current message
XMPFSND   send it
XMPFREL   release the message you received
```

**Use `XMPOPCN`, not `XMPOPNM`.** A room has seats, and the free-connection counter that enforces
them belongs to a CONNECTION port. `XMPOPNM` is a plain named port with no counter, so the seat
limit would be silently absent - and the C# side WOULD still be enforcing it, leaving the two
doors disagreeing about a rule they are meant to share. Signature:
`XMPOPCN(flags, portName, uniqueName, maxConnections, portNumber)`.

`XMSG-API.md` section 7 carries this and the flag table; `XFWTF=15` is in
`XMSG-PL-VALUES-M.INCL`. So the server does NOT need an RT wait plus polling, and nothing has to
be read off the machine to start.

Still open, and both are decisions rather than unknowns:

 - **Where the room table lives.** An RT program's data area, sized at compile time. Eight seats
   times a 16-character name is small; the number just has to be chosen and stated.
 - **What happens on restart.** An RT program that is aborted and restarted loses the room. That
   is acceptable and must be said out loud rather than discovered.

### 2b. `CHAT` - the client, an ordinary PROG

Runs from the terminal (`@CHAT`), like any other SINTRAN program.

```
open a port
find CHAT-<room> and send a join letter
loop:
    read a line from the terminal, non-blocking
    check for an arrived message, non-blocking
    print anything that arrived
until the user types a command that leaves
```

The hard part is not XMSG - it is **doing two things at once on a SINTRAN terminal**: waiting for
a keystroke while also waiting for a message.

**ANSWERED, and it is simpler than feared.** `XFWTF` is a FLAG, not a requirement: a receive
WITHOUT it returns immediately with status **`MXNEXTM`** - "not terminated", the COSMOS guide's own
example being "no message waiting when XMPFRCV is called without the XFWTF option". So XMSG can be polled without blocking, and the
client is an ordinary loop:

```
loop:
    no-wait read from the terminal   -> anything typed?
    XFRCV without XFWTF              -> T = 0 means nothing arrived, carry on
    print whatever did arrive
```

No second RT program, no shared buffer, no timeout trickery. The zero-means-not-terminated
convention is in `XMSG-API.md` section 2 and the flag table lists `XFWTF` as optional on every
waitable call.

**The terminal half is ANSWERED too**, from `Reference-Manuals/ND-860228-2-EN SINTRAN III
Monitor Calls.md`, so nothing in this plan now needs a program read off the machine.

`InByte` (`INBT`, MON `1B`) normally waits: *"The program waits if there is no bytes in the input
buffer of the device. You can change this with NoWaitSwitch or TerminalNoWait."* So switch no-wait
on first:

| Call | MON | Signature |
|---|---|---|
| `TerminalNoWait` | `307B` | `TerminalNoWait(DevNo, IOFlag, NoWaitFlag, RetStatus)` |
| `NoWaitSwitch` | `36B` | the general switch, same idea |
| `InByte` | `1B` | `InByte(DevNo, Char)` |

`IOFlag` is 0 for input and 1 for output; `NoWaitFlag` is 0 to switch no-wait off and anything
else to switch it on. **Use device number 1 for your own terminal.**

```
Monitor_Call('TerminalNoWait', 1, 0, 1, status)    % our terminal, input, no-wait ON
loop:
    Monitor_Call('InByte', 1, ch)                  % returns at once now
    ch := ch AND 127                               % strip the parity bit - see below
    XFRCV without XFWTF                            % T = 0 means nothing arrived
    print whatever did arrive
```

**The parity trap, and the manual warns about it in so many words.** Terminal input sets bit 7 as
an even-parity bit, so a carriage return arrives as **141, not 13** - the manual uses exactly that
example. A client comparing against ASCII 13 would never see the user press return, and would look
like a hung program rather than a bug. Mask with 127 before comparing anything.

**The one thing genuinely left to measure:** what `InByte` returns when no-wait is on and no byte
is waiting. The manual says only that the program stops waiting, not what comes back instead. That
is a five-minute experiment on the machine, not a design question, and everything else can be
written before it is answered.

## 3. Order of work

1. **A five-minute measurement on the machine:** what `InByte` returns with no-wait on and nothing
   typed. It is the last unknown, and it is a value to observe rather than a decision to make.
   Everything below can be written before it is answered.
2. **A one-way spike:** `CHATSV` opens and names a port, waits with `XFRCV + XFWTF`, and prints an
   arriving join on the operator console. `CHAT` sends one. Nothing else. This proves the XMSG
   plumbing end to end inside SINTRAN, and both halves are now fully specified.
3. **The client loop**, once the no-wait terminal read is known.
4. **The room rules**, ported from `ChatRoom` with its tests as the specification.
5. **Two ND machines**, one room, two terminals.
6. **Interoperate with the C# node**: our `ChatClient` joins a room served by `CHATSV`, and a
   SINTRAN `CHAT` joins a room served by ours. Same message bytes, so this should work - and if it
   does not, the message format is where to look first.

## 4. What would make this fail, stated in advance

 - **Polling where a wait belongs.** The server must use `XFRCV + XFWTF` and block. A polling RT
   program burns the machine for nothing, and the flag exists precisely so it does not have to.
 - **Inventing a name-listing exchange.** There is still no capture of one. `CHAT` should take the
   room name as a parameter rather than offer a menu it cannot honestly build.
 - **Assuming our message format is on the wire somewhere.** It is not - we chose it. That is fine
   for our own two programs, and it means no capture will ever confirm it. The C# tests are the
   only specification, so keep them.
 - **Forgetting the parity bit on terminal input.** Carriage return arrives as 141. Compare
   against 13 and the client never sees the user press return, and it looks hung rather than
   wrong.

## 5. What this gives that today's chat does not

Two ND machines can talk with the C# node switched off entirely. That is the point: the chat stops
being a thing our emulator offers SINTRAN and becomes a thing SINTRAN does.
