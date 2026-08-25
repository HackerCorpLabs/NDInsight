# The XMSG chat system - the C# library

> **THIS IS NOT THE SINTRAN CHAT PRODUCT.** This page describes the **C# chat library** and its
> terminal door - a Windows-side implementation with its own commands (`chat join` at a `#`
> prompt) and its own API (`ronny.Poll()`).
>
> **The product that runs on the ND-100s is a different program**: the PLANC server `CHATSER` and
> the PLANC client `CHAT`, whose commands are `/join`, `/who`, `/nick` and so on. For that, read
> **[CHAT-APP-SPECIFICATION.md](CHAT-APP-SPECIFICATION.md)**.
>
> Confusing the two is easy and this page caused it. The `tell` command mentioned below exists
> HERE and has no equivalent on the SINTRAN server.

A chat room that lives on an XMSG node. Two kinds of people can be in the same room at the same
time: somebody at a **SINTRAN terminal** who has connected to our node, and a **program** that
talks to the room port to port.

---

## 1. The quickest thing that works: a terminal user

Somebody on a SINTRAN machine connects to our node the ordinary way (`CONNECT-TO`, then log in at
our TAD prompt). At the `#` prompt they have these commands:

| Command | What it does |
|---|---|
| `chat join <name>` | Enter the room under a name. With no name, your login name is used. |
| `chat say <text>` | Say something. Everybody in the room sees it as `<name> text`. |
| `chat who` | List who is in the room. |
| `chat nick <name>` | Change the name you are known by. Everybody is told, with the old name. |
| `chat part` | Leave the room. |

`help` lists them alongside the other terminal commands.

A worked example, two people on two terminals:

```
# chat join RONNY
you are in the room as RONNY

                                  # chat join ANNA
                                  you are in the room as ANNA
ANNA joined

                                  # chat say god morgen
                                  <ANNA> god morgen
<ANNA> god morgen

# chat nick RH
you are now RH
                                  RONNY is now RH
```

The line you type is confirmed to you directly; everybody else is told separately. That is why
both sides see `<ANNA> god morgen` - the speaker included.

## 2. A program in the room

`Xmsg.Chat` holds the port-to-port half: `ChatServer` claims a name and admits joiners,
`ChatClient` joins and talks.

```csharp
XmsgKernel kernel = new XmsgKernel(ourNode, ourMagic, null);
XroutDirectory directory = new XroutDirectory();

ChatServer server = new ChatServer(kernel, directory);
server.Open(ChatRooms.NameFor("LOBBY"), seats: 8, greeting: "welcome to the lobby");

ChatClient ronny = new ChatClient(kernel, directory, "RONNY");
ronny.Join(ChatRooms.NameFor("LOBBY"));
server.Poll();                       // the server takes the join and answers

IReadOnlyList<ChatMessage> mine = ronny.Poll();   // Welcome, then Joined/Said/Renamed/Left
ronny.Say("god morgen");
ronny.Rename("RH");
ronny.Leave();
```

`Poll` on both sides is deliberate: nothing blocks, because the same loop tick also serves
everything else the node is doing.

### The message kinds

| Kind | Direction | Carries |
|---|---|---|
| `Join` / `Welcome` / `Reject` | client -> server -> client | the name, and a reason when refused |
| `Say` -> `Said` | client -> room | the speaker's name and the text |
| `Rename` -> `Renamed` | client -> room | the NEW name, and the OLD one in the text |
| `Leave` -> `Left` / `Joined` | client -> room | who came or went |

`Renamed` carries both names on purpose: everybody has a transcript on screen under the old one
and no way to connect the two without it.

## 3. Rooms (channels)

A room is a name in the XROUT table with a `CHAT-` prefix, so rooms can be told apart from every
other server on the node:

```csharp
ChatRooms.NameFor("LOBBY")            // -> "CHAT-LOBBY", what the server registers

IReadOnlyList<XroutNameEntry> rooms = ChatRooms.List(directory);
// each entry: the SHORT name ("LOBBY") and the free seats XROUT still has
```

The free-seat count comes from XROUT, not from the room. That is the same number that decides
whether a join is forwarded at all, so it is the true answer rather than what a room believes
about itself.

**Room listing does not cross the wire.** No capture in the corpus shows a name-listing exchange
between machines, so how one would look is unknown and has not been invented. A remote SINTRAN
user cannot ask us for the list of rooms today.

## 4. Seats, and where they are enforced

A room has a size, and on the port-to-port path SINTRAN enforces it before any of our code runs:
XROUT keeps a free-connection count, forwards a join only while it is above zero, and decrements
it each time. So "the room is full" is decided by the system, and the real file server builds its
30 seats exactly this way.

**The terminal path has no seat limit**, because a terminal user does not come through XROUT.
That is a real difference, not an oversight.

Two related rules worth knowing:

 - Every refused JOIN returns a seat, because XROUT already took one to forward it. Forget that
   and the room fills up permanently while appearing empty.
 - A refused RENAME returns nothing, because the member is still sitting in their seat. Handing
   one back there would let the room admit one person more than it holds.

## 5. What is shared, and why

The rules - who is in the room, which names are free, who is told what - live in **one** class,
`ChatRoom`, with no transport of any kind. Both doors carry them:

```
terminal user  ->  TadServer  ->  ChatRoom  <-  ChatServer  <-  port-to-port client
```

A member is an opaque number: a magic number on one path, a tty number on the other. Written
twice, the two doors would drift, and the drift would be in the awkward cases - a duplicate
nickname, a rename that collides, who hears about somebody leaving.

## 6. Status

**Built and tested:** the room rules, the port-to-port server and client, aliases, channels, and
the terminal commands. 37 tests in `Xmsg.Chat` plus the terminal-door tests in
`TwoNodeTerminalTests`.

**CORRECTED 2026-08-25 - this section was stale and said the opposite of the truth.** It read
"Nobody has yet sat at a real SINTRAN terminal, joined the room and talked to a second user."

That was overtaken long ago, by the PLANC implementation rather than this one. Two users have
talked on two different ND-100s; the rooms now span **three** machines over trunks, with a machine
in the middle relaying between two that have no trunk between them, and the whole thing starts
from a cold boot with nothing typed. See
[CHAT-APP-SPECIFICATION.md](CHAT-APP-SPECIFICATION.md).

**What remains true here** is that the C# library's own terminal door has not had that live run.

**A known limit of the test harness, not of the server:** it cannot drive two commands on one
session - the second renders nothing. The output window is held by the previous reply's final
frame, which the test client never acknowledges. Measured with a control (`who` then `stat`) that
has nothing to do with chat. See `OutputWindowDiagnosticTests` for what is proven and what is
still open.

**Not built:** persistent rooms (a room exists while the server object does), history (a joiner
sees nothing said before they arrived), and private messages between two members - though the
terminal `tell` command already does person-to-person messaging outside the room.

**`tell` IS THIS LIBRARY'S, NOT THE SINTRAN SERVER'S.** `CHATSV.PLNC` has no person-to-person
message kind at all - its kinds are 1-16 for room traffic, 32-38 for admin and 48-53 for trunks.
Anyone reading this page and expecting `tell` to work on an ND-100 will not find it.
