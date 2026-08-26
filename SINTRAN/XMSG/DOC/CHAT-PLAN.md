# NDCHAT - the plan from here

**Written 2026-08-25** by comparing [CHAT-APP-SPECIFICATION.md](CHAT-APP-SPECIFICATION.md), which
is what the product does today, against Ronny's full-screen mockup in
[CHAT-UI-DESIGN.md](CHAT-UI-DESIGN.md), which is what it should do.

---

## 1. What the comparison found

### The spec had one thing wrong, and the comparison is what caught it

**History is BUILT.** `histSave` runs on every line said, `histReplay` on every join, the lines go
to a disk file, and they come back to a joiner as `kHist`. The first draft of the specification
said a joiner sees nothing said before they arrived, from memory rather than from the source.
Corrected. **Still not watched on a machine** - the one-minute check is in the spec.

Everything else in the specification matched the source. Every kind the client can send has a
handler; `kHist` and `kAllWho` have none because they are server-to-client only, which is correct.

### The gap between what exists and the mockup

| Mockup element | State |
|---|---|
| three sections, 80x24 | **prototype runs** - `CHATUI.PLNC` on D100 |
| scrolling chat with a cache | **prototype runs** - 100 lines, PAGE UP / PAGE DOWN |
| input line | **prototype runs** - typing, backspace, `/exit` |
| room name, member count, date and time | **prototype draws them, all hard-coded** |
| `@trunk` marker | not built - derivable from the member list |
| the window bar | **drawn but inert** |
| `=KARI` direct-message windows | **NO PROTOCOL AT ALL** |
| line mode for a terminal without cursor control | **not built** - the prototype refuses instead |

### The one defect that matters most

**`CHATUI` prints your own line locally when you press RETURN.** The server sends it back
(`broadcast(..., 0)` leaves nobody out), so wired up as-is **every message you send would appear
twice**. This is the first thing to fix and it is a deletion, not an addition.

---

## 2. The plan, in dependency order

### PHASE 1 - make the client render, keeping line mode - **DONE 2026-08-26**

```
┌── NDCHAT 2.1 ──────────────────────────────────TESTER@FJELL──┐
│ LOBBY@trunk               0 here  /help for commands  25 08 19:03
│--------------------------------------------------------------
│ 19:03  *            you are TESTER on FJELL
│ 19:03  SYSTEM@VIDDA final check from VIDDA
│--------------------------------------------------------------
│>
```

Every value on that screen is real. The renderer is chosen once from the CTYTP bits and the LINE
renderer is untouched underneath - it is still the reference for what should be shown and still
the fallback for a printing terminal.

**STILL HARD-CODED: the member count**, which reads 0 because nothing counts a `/who` answer yet.
That is the one part of step 5 not done.

**What it cost, in the order the machine taught it:**

| Symptom | Cause |
|---|---|
| joined, then never heard anything | **MON1 BLOCKS once the echo is off**, whatever TerminalNoWait was told. `SEATS 1/16` with `bounce` climbing said it; the screen could not. Poll `MON66` first. |
| lines smeared across rows | **two subarrays of ONE array** passed to one routine. A notice rendered correctly beside it - same code, one variable different. |
| `> ??g??` in the input line | bytes arrive DURING the drawing, so a drain before the paint cannot catch them. Drain again immediately before the first read. |
| the welcome landed on the input row | `OUTPUT` writes where the cursor is, and `drawInput` parks it there. On a screen it goes in the room. |

**The `??g??` was never cosmetic.** With it in the buffer the first command typed became
`??g??/quit`, matched nothing, and was **said to the room**.

### PHASE 1 as originally written

**The goal: the existing client, unchanged in behaviour, drawing on a screen.** No new features.
That is what makes it safe - the line renderer stays as the reference for what the screen should
be showing, and as the fallback for a printing terminal.

1. **Split every place the client prints into a `show*` routine.** One renderer chosen at
   start-up from the CTYTP bits, never asked again.
2. **Move the CHATUI drawing routines across** - they are proved on the machine already.
3. **DELETE the local echo on RETURN.** Send the line, print nothing, let the arriving `kSaid`
   draw it. This is the fix above.
4. **Feed the cache from the message loop** instead of the fake timer: `kSaid`, `kJoined`,
   `kLeft`, `kRenamed`, `kTopic`, `kHist` each become a formatted line.
5. **Fill the status line from real state** - room name, `/who` count, the clock.

*Proof:* two users on two machines, one on a screen terminal and one on a line terminal, in the
same room, each seeing the other's lines exactly once.

**Risk:** the client is 3400 lines and works. Every step above is reversible and the line renderer
is never removed.

### PHASE 2 - finish the main window - **DONE, and proved on D100 2026-08-25**

6. **`@trunk` marker** - DONE. A member table holds each speaker's system and `roomTrunked` scans
   it against ours; one member elsewhere is enough. Built as a scan rather than a flag so the rule
   is the one the real client will run.
7. **Line wrapping** - DONE. A long line becomes several cache lines, the first carrying the time
   and speaker and every continuation carrying neither.
8. **Grow the cache** - DONE, 300 lines. The linker's own `FREE:` line is the number to ask.
9. **Speaker column** - DONE. Local speakers show the bare nickname; remote ones show `NICK@SYS`,
   and when that overruns **the nickname is cut, never the system**. Inside one room you know a
   person from the first letters of their name; `D10` does not tell you which machine.

Measured on the machine:

```
 #sintran-dev@trunk       14 here  /help for commands         25 08 14:05
 14:05  KARI@D102    spooler queue is stuck on LP2 again
 14:05  SIGRID@D103  the ND-500 process table is at 48 of 151, the configured
                     maximum and not a ceiling
 14:05  BJORN        hold on, batch job on processor 3 until 09:20
```

BJORN is on D100, so he carries no qualifier. That is the rule working, not a missing field.

**Two faults found by RUNNING it, which reading the source had not shown:**

- the input line came up holding `> ??g??` every run. The keyboard was taken AFTER the screen was
  drawn, so SINTRAN had already echoed the waiting bytes onto it. Draining could never have fixed
  that - draining empties the buffer, not the screen. Take the keyboard first, draw second.
- a fast burst repainted the input line past its own right edge, over the frame border. Ask
  `MON66` whether more is waiting and repaint once at the end.

**And one thing that looks like a fault and is not.** A line pasted in one burst loses its RETURN
above 64 bytes: 63 characters plus RETURN is said, 72 plus RETURN is not. That is SINTRAN's
terminal input buffer, and the driver drops what will not fit before the program is offered a
single byte. It only happens because RetroTerm delivers a whole test line over TCP at once; a
person typing never approaches it. **The tell is the typed line sitting in the input field with
nothing said, which reads exactly like a broken RETURN key.** It is not - KEYPROB answers 13 for
RETURN every time. The RETURN never arrived.

### PHASE 3 - private messages - **DONE for one machine, proved on D100 2026-08-25**

10. **The kinds** went into `chat-wire.json` with golden bytes FIRST, then the server, then the
    client - the order the registry enforces. DONE.
11. **The routing question is answered, and the answer is NOT what a room line does.** A room line
    is flooded to every peer and de-duplicated at the far end. Flooding a PRIVATE message would
    hand it to machines with no business seeing it - a privacy failure dressed up as routing. So a
    direct message goes ONLY to the machine holding the person, ONE HOP, and anything further is
    refused in words. Kind 54 still carries the same origin/hops/id header as `kTrkRelId` so the
    de-duplication machinery is shared rather than duplicated.

Proved on the machine, server segment 2605B, START ADDRESS 32255B:

```
[TESTER/LOBBY] /tell NOBODYHERE are you there
not sent to NOBODYHERE: nobody of that name is here now, and nothing is kept for later

[TESTER/LOBBY] /tell TESTER talking to myself
*(D100!TESTER) talking to myself
sent to: D100!TESTER

[TESTER/LOBBY] /tell D102!GHOST hei
not sent to D102!GHOST: nobody of that name is here now, and nothing is kept for later
```

The middle one is the whole feature in three lines: the message came back MARKED and with the
sender QUALIFIED, and the receipt named who it actually reached.

**STILL NOT PROVED: a direct message CROSSING a trunk.** Kind 54 is built at both ends and no
direct message has ever gone over one. It needs a client joined on D102 at the same time.

### PHASE 4 - multiple windows

12. **One ring buffer per window** instead of one. The painting does not change; only which
    buffer it reads.
13. **The bar becomes live** - unread counts, and the `*` highlight when your nick appears.
14. **A key or command to switch.** Switching repaints the middle section only.

**Do not build a general window manager.** These windows do not overlap - they are alternative
contents for one rectangle, which is far simpler, and TESTUI already showed what overlapping costs
(clipping, a stack order, and three separate places that must agree on it).

### ALSO OUTSTANDING, not on the critical path

15. **Prove dedup** - needs a D102-D103 trunk to make a triangle. Until then `dupe` reads 0 and a
    broken implementation would look identical.
16. **Prove history live** - join, speak, quit, rejoin.
17. **D100's boot file has no `START-TRUNK 103`**, so D103 is not registered at boot.
18. **The separators are dashed** - `fullbar` draws hyphens where the frame uses line-drawing
    characters. Cosmetic.

---

## 3. What I would do first, and why

**Phase 1, and specifically step 3 before anything else.** Deleting the local echo is a
five-minute change that prevents a defect which would otherwise be discovered by a user seeing
everything twice and reported as "the chat is broken".

Then steps 1-2, because until the renderer is split, every further UI change has to be made twice
- once in the prototype and once in the client - and they will drift.

**Phase 3 is the one to plan properly rather than start.** A direct message that crosses a trunk
is the same problem as relaying a room line, and the answer should reuse `kTrkRelId`'s origin and
hop count rather than invent a second mechanism.
