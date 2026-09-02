# NDCHAT - User Manual

Chat between people on the ND-100 machines. You type a line, everyone in the room sees it -
including people sitting at a different machine.

**Version:** NDCHAT 2.1 client, checked against the running lab on 2026-09-02.
**Where the screens come from:** every screen marked *captured* was read off a real terminal
(D100 / FJELL) on that date. Screens marked *from the program* show the exact wording the
client prints, taken from its source, but were not photographed in that exact situation.

Related: [the admin manual](CHAT-ADMIN-MANUAL.md) for whoever runs the server, and
[the specification](../CHAT-APP-SPECIFICATION.md) for what the server does behind the scenes.

---

## 1. Starting

Log in to SINTRAN as usual, then type `CHAT`.

```
ENTER SYSTEM
PASSWORD:
OK
@CHAT

Available terminal types are:

  2: Teletype ASR 33                     3: Tandberg TDV 2115
  6: DEC VT100 (80 columns)             53: Tandberg TDV 2200/9 ND-NOTIS
 80: Tandberg TDV 2200/9 ND-NET         83: Tandberg TDV 2200/9 V2 ND-NOTIS
 90: Tandberg TDV 2200/9S ND-NET        93: Tandberg TDV 2200/9S ND-NOTIS
100: Tandberg TDV 2200/9S 25 ND-NET    103: Tandberg TDV 2200/9S 25 ND-NOTIS
106: Nokia ND-Display Terminal 301     110: Tandberg TDV 1200/1 ND-NET
113: Tandberg TDV 1200/1 ND-NOTIS      121: Wordplex 80/90
131: DEC VT220 (Multinational mode)    132: DEC VT220 (National mode)
134: DEC VT100 (132 columns)           135: ANSI Standard Terminal

What is your terminal type?
```
*(captured)*

The program asks this only when SINTRAN does not already know what your terminal is. Answer
with the number - `6` for a VT100, `53` for a TDV 2200. On a screen terminal you get the
full-screen chat below. On a printing terminal (type 2) you get a plain line-by-line version
with the same commands.

A few seconds later the room appears:

```
+-- NDCHAT 2.1 --------------------------------------------------SYSTEM@FJELL--+
| LOBBY                     1 here  /help for commands         02 09 07:35     |
|------------------------------------------------------------------------------|
| 02:31  TESTER       india                                                    |
| 02:31  TESTER       juliet                                                   |
| 02:31  TESTER       kilo                                                     |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|                                                                              |
|------------------------------------------------------------------------------|
|>                                                                             |
| [1 LOBBY]                                                                    |
+------------------------------------------------------------------------------+
```
*(captured; the frame is drawn with line graphics on a real terminal)*

You are put straight into the room called **LOBBY**. The three lines already there were said
before you arrived - the server keeps the last lines of every room and shows them to a newcomer.

What the screen shows:

| Where | What it is |
|---|---|
| top right corner `SYSTEM@FJELL` | your name, and the name of the machine you are on |
| `LOBBY` | the room you are in |
| `1 here` | how many people are in the room (you included) |
| `02 09 07:35` | date and time |
| the middle | the conversation: time, who said it, what they said |
| `>` line | where you type |
| `[1 LOBBY]` | the window bar - see section 6 |

**Your name** is your SINTRAN user name the first time. Change it with `/nick` (section 3); the
client remembers the new name in a small file called `CHAT:CNFG` in your own file area and
offers it next time.

---

## 2. Talking

Type a line and press RETURN. It goes to everybody in the room. This is OLAV on FJELL and KARI
on VIDDA, another machine, talking:

```
+-- NDCHAT 2.1 --------------------------------------------------TESTER@FJELL--+
| LOBBY@trunk #manuals d    2 here  /help for commands         02 09 07:49     |
|------------------------------------------------------------------------------|
| 02:31  TESTER       india                                                    |
| 02:31  TESTER       juliet                                                   |
| 02:31  TESTER       kilo                                                     |
| 07:48  *            TESTER is now OLAV                                       |
| 07:48  OLAV         hello everyone, is anybody on the other machine?         |
| 07:49  *            SYSTEM@VIDDA has joined                                  |
| 07:49  *            KARI@VIDDA has joined                                    |
| 07:49  *            SYSTEM@VIDDA has left                                    |
| 07:49  KARI@VIDDA   yes, I am on VIDDA - I can read you fine                 |
| 07:49  *            topic: manuals day - say anything, it gets photographed  |
|                                                                              |
|------------------------------------------------------------------------------|
|>                                                                             |
| [1 LOBBY]                                                                    |
+------------------------------------------------------------------------------+
```
*(captured on D100)*

Reading it:

- **Your own line appears only when it comes back from the server.** That is normal - it is
  the server's copy, with the server's time on it, so everybody sees the lines in the same
  order. If you press RETURN and your line never shows up, the server is not hearing you: see
  section 8.
- Someone on **another machine** has the machine's name after theirs: `KARI@VIDDA`. People on
  your own machine are shown bare. `@VIDDA` means "not here".
- Lines with `*` in the name column are **notices** from the chat itself: who joined, who left,
  who changed name, a new topic. The `n here` count in the header follows them.
- A person on another machine who changes their name shows up as the new name joining and the
  old one leaving (`KARI@VIDDA has joined`, `SYSTEM@VIDDA has left`), not as `is now`.
- The topic goes into the header after the room name, cut to fit the field
  (`LOBBY@trunk #manuals d`). Ask `/topic` to read it in full.

Two things on this screen are wrong and known: after `/nick OLAV` the top-right corner still
says `TESTER`, and the header reads `LOBBY@trunk` for a room that is simply `LOBBY`. Both are
cosmetic and listed in the plan.

---

## 3. Commands

Every command starts with `/`. Anything else is said to the room. You can shorten a command
to the first letters as long as it is still unambiguous: `/j` is `/join`, `/n` is `/nick`.

| Command | What it does |
|---|---|
| `/help` | show this list on the screen |
| `/join <room>` | go to another room. Leaves the one you are in. A room that does not exist is created |
| `/nick <name>` | change your name (up to 16 characters). Everyone in the room is told |
| `/who` | who is in this room, including people on other machines. Outside a room: everybody, everywhere |
| `/list` | which rooms exist and how many people are in each |
| `/map` | which machines there are and which are connected right now |
| `/topic` | show the room's topic |
| `/topic <text>` | set the topic (up to 64 characters). Everyone is told |
| `/tell <name> <text>` | a private message to one person - section 5 |
| `/w <n>` or `/window <n>` | go to window n. A bare `/w` goes to the next one - section 6 |
| `/close` | close the private-conversation window you are looking at |
| `/leave` | leave the room. You stay in the program and can `/join` another |
| `/exit` (or `/quit`) | leave the room and end the program |

This is what `/help` shows - it opens over the room, and any key closes it:

```
+-- NDCHAT 2.1 --------------------------------------------------TESTER@FJELL--+
| LOBBY@trunk #manuals d    2 here  /help for commands         02 09 07:49     |
|---------+---------------------------------------------------------+----------|
| 02:31  T| CHAT commands                                           |          |
| 02:31  T|                                                         |          |
| 02:31  T|   /join <room>          join another room               |          |
| 07:48  *|   /nick <name>          change your nickname            |          |
| 07:48  O|   /who                  who is in this room             |?         |
| 07:49  *|   /list                 rooms, and how many in each     |          |
| 07:49  *|   /map                  the machines and their trunks   |          |
| 07:49  *|   /topic                show the room topic             |          |
| 07:49  K|   /topic <text>         set it                          |          |
| 07:49  *|   /tell <name> <text>   a message to one person         |ographed  |
|         |   /w <n>                go to window n  (ctrl-W cycles) |          |
|         |   /close                close a conversation window     |          |
|         |   /leave                leave the room                  |          |
|         |   /exit                 Exit chat program               |          |
|         |                                                         |          |
|         |   anything else is said to the room                     |          |
|         |                                                         |          |
|---------|   -- any key closes this --                             |----------|
|>        +---------------------------------------------------------+          |
| [1 LOBBY]                                                                    |
+------------------------------------------------------------------------------+
```
*(captured on D100 - the panel sizes itself to the widest line; the room stays visible around it)*

### Answers open in a panel

`/help`, `/who`, `/list`, `/map` and a bare `/topic` are answers **to you**, not to the room,
so they open in a panel in the middle of the screen instead of scrolling through the
conversation. **Press any key to close the panel.** That key does nothing else - it will not
end up in your typed line. Anything that arrived while the panel was open is shown the moment
it closes.

### Changing your name

```
/nick OLAV
```

Everyone sees `SYSTEM is now OLAV` and the corner of your screen changes. A name that someone
in the room already has is refused: `refused: that nickname is taken`. Pick another.

If you are refused on the way IN (your name is already taken in the room), you are not in the
room yet; `/nick <other name>` then tries again with the new name.

### Rooms

```
/join PROJECT
```

Rooms have short names (up to 16 characters). `/list` shows the rooms that exist right now and
how many people are in each; a room disappears when its last member leaves, though what was said
in it is kept and shown again to the next person who joins it.

---

## 4. Who is here, and where

`/who` opens a panel with the names in your room. People on another machine are shown with the
machine name:

```
| 07:49  *            SY+-----------------------------+                        |
| 07:49  *            KA| who is here                 |                        |
| 07:49  *            SY|                             |                        |
| 07:49  KARI@VIDDA   ye| OLAV KARI@VIDDA             |ou fine                 |
| 07:49  *            to|                             |, it gets photographed  |
|                       |   -- any key closes this -- |                        |
|                       +-----------------------------+                        |
```
*(captured)*

`/map` tells you which machines exist and whether your machine can reach them:

```
|                   +-------------------------------------+                    |
|                   | the machines                        |                    |
|                   |                                     |                    |
|                   | this is FJELL - VIDDA up  SKOGEN up |                    |
|                   |                                     |                    |
|                   |   -- any key closes this --         |                    |
|                   +-------------------------------------+                    |
```
*(captured)*

`up` means the connection between the two chat servers is working; `down` means people on that
machine cannot see you right now (and you cannot see them). Nothing you can do from the client
changes that; the operator's manual covers it.

`/list` shows the rooms **on your own machine** and how many people are in each - `LOBBY(1)`
while OLAV and KARI were both in LOBBY, because KARI's seat is on VIDDA. `/who` is the one
that counts across machines.

---

## 5. Private messages

```
/tell KARI are you free at three?
```

Goes to KARI only. If there is exactly one KARI anywhere the chat can see - on your machine or
on any connected one - it is delivered. You are told where it went:

```
sent to D102!KARI
```

If two people are called KARI you are **not** asked to guess - it is refused and both full names
are shown. Send it again with the machine in front:

```
/tell D102!KARI are you free at three?
```

If nobody of that name is there, it is refused and **nothing is kept**:

```
NOT sent to KARI: nobody of that name is here now, and nothing is kept for later
```
*(wording from the program; the same three outcomes were watched on D100 on 2026-08-25)*

A private message - sent or received - gets **its own window**, one per person (section 6). This
is what KARI saw on VIDDA after OLAV's `/tell KARI are you free at three? this one is private`,
then her own `/tell FJELL!OLAV yes, three is fine`:

```
+-- NDCHAT 2.1 --------------------------------------------------------KARI@VIDDA--+
| LOBBY                     2 here  /help for commands         02 09 07:50     |
|------------------------------------------------------------------------------|
| 07:49  *            *(FJELL!OLAV) are you free at three? this one is private |
| 07:50  *            *(SYSTEM) yes, three is fine                             |
|                                                                              |
|------------------------------------------------------------------------------|
|>                                                                             |
| 1 LOBBY[2 =FJELL!OLAV]                                                       |
+------------------------------------------------------------------------------+
```
*(captured on D102, looking at window 2; the corner is shown as it should read - see below)*

The stars mark a private line. The name in brackets is the **full name to type back** -
`FJELL!OLAV`, machine first. If every window is taken the line goes into the room instead,
still with the stars, so it cannot be mistaken for room traffic.

**Reply with `/tell`, not with a plain line.** A plain line goes to the whole room, whichever
window you are looking at.

Known cosmetic fault: your own half of the conversation is labelled with the name you logged
in as (`*(SYSTEM)` above, though KARI had done `/nick KARI`), and the corner of the screen
keeps the old name too. The other person sees your current name.

Limits: the machine name form only reaches machines your server talks to directly. A message to
someone two hops away is refused in words, never lost silently.

---

## 6. Windows

The bottom line of the screen is the **window bar**. Three real states of it, in order, from the
same session as section 5:

```
| [1 LOBBY] 2 =VIDDA!KARI                       OLAV sent a /tell; window 2 opened for KARI
| [1 LOBBY] 2 =VIDDA!KARI* 1                    KARI answered: one unread line in window 2
| 1 LOBBY[2 =VIDDA!KARI]                        after /w 2 (or Ctrl-W): looking at window 2
| 1 LOBBY 1[2 =FJELL!OLAV]                      a room line arrived meanwhile: 1 unread in window 1
```
*(captured)*

- Window 1 is always the room. It cannot be closed.
- A private conversation gets its own window, one per person, the first time a message arrives
  from them or you `/tell` them. The window is named after the person, machine first.
- The window you are looking at is in brackets. A `*` and a number after a name are unread lines
  in that window.

Move between windows with **`/w 2`**, **`/w`** (next window) or **Ctrl-W** (also next window). Lines
you type while looking at a conversation window go to the room, not to the person - use `/tell`.
`/close` puts the conversation window away; the room stays.

---

## 7. Leaving

```
/exit
```

says goodbye to the room and returns you to the `@` prompt. `/quit` does the same. `/leave`
leaves the room but keeps the program running so you can `/join` another one or use `/list`,
`/who`, `/map` while outside any room. On a screen terminal `/leave` gives no visible sign - the
header still names the room - but the others are told you left and the count drops on their
screens (watched on VIDDA: `2 here` became `1 here`). Known, cosmetic, in the plan.

If you just log out, or your terminal is stopped, the server notices within a minute or so and
tells the others you have left.

---

## 8. When something looks wrong

| What you see | What it means | What to do |
|---|---|---|
| you press RETURN and your own line never appears | the server is not hearing you - usually the operator reloaded the server and your seat is gone. The screen looks completely normal otherwise | `/exit`, then start `CHAT` again |
| `refused: that nickname is taken` | somebody already uses that name in the room | `/nick` another name |
| `refused: ...` on the way in and you never get a room | same, at join time | `/nick <other name>` retries the join |
| `not in a room yet - wait or /join` | you typed something that needs a room (say, `/topic`, `/tell`) while outside one | `/join LOBBY` |
| `/map` shows a machine `down` | the link between the two chat servers is not up | wait - it reconnects by itself, on a growing interval up to five minutes; or tell the operator |
| the program ends with `--EXIT--` or `ABORTED BY SYSTEM` at the `@` prompt | SINTRAN's idle timeout ended it | start `CHAT` again |
| the screen is garbled | wrong terminal type | `/exit`, start again, and give the right number |
| `AMBIGUOUS FILE NAME` when you type `CHAT` | the program is not installed on this machine, only files with similar names | tell the operator (admin manual, section 3) |

**Nothing is stored for you while you are away.** History shown on entry is the room's last
lines, not a mailbox. A private message to somebody who is not there is refused, not queued.

---

## 9. Limits worth knowing

- 16 people per machine at a time. The 17th is refused with a message.
- Names and room names: 16 characters. Topic: 64. A typed line: one screen line, it wraps in
  the room display when it has to.
- What you type is sent when you press RETURN; there is no line editing beyond backspace.
