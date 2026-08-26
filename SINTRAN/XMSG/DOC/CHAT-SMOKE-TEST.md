# Chat smoke test - the order that stops a false bug report

**Three separate "it does not arrive" findings were wrong on 2026-08-26, all for the same
reason: the receiving client was not seated.** A client with no seat shows a completely
normal screen - frame, room name, prompt, everything - and silently receives nothing. One
of those findings was written up as a trunk defect and had to be retracted.

**So the order below is not ceremony. Step 2 is the whole point.**

---

## Before anything

```powershell
tools\nd-preflight.ps1
```

Hub up, daemon up with time left in its window and a real topology, all three machines
running. If it is not clean, nothing below means anything.

## 1. Start the receiver FRESH

Never reuse a client left over from an earlier server generation. **An RT-load gives the
server a new segment with an empty member table**, and every client that was joined to the
old one keeps its seat flag, shows a normal prompt, and is talking to nobody.

```
@CHAT
```

## 2. PROVE IT IS SEATED - say one line and SEE ITS OWN ECHO

```
hello
```

**A working say comes back as `NAME  hello`. No echo means the client is orphaned, and
nothing you test after this tells you anything.** This is the single check that would have
prevented all three false findings.

The other tell, and it is decisive:

```
X-C: LIST-NAMES
   100    10      16     *CHAT.      <- SIXTEEN free seats means NOBODY is joined
```

## 3. Now test - and do NOT exit the receiver

**Leaving the chat client to run `CHAT-MON` un-seats it.** One of the three false findings
came from exactly that: exit the receiver to read `STATUS`, send from the other machine,
come back, find nothing, conclude the message was lost. Use a *second terminal* for
`CHAT-MON`, or read the counters before you start.

| What to prove | How | What right looks like |
|---|---|---|
| a room line, same machine | say on one client | appears on the other, no keypress needed |
| a room line, across a trunk | say on D100 | `TESTER@FJELL text` on D102 |
| membership crosses | `/who` on both | both list everyone, and `n here` agrees |
| a private message | `/tell NAME text` | opens ITS OWN window; bar gains `2 =NAME` with `*` and a count |
| switching | ctrl-W, or `/w 2` | pane, status line and bar change - the frame does NOT flicker |
| a reply is private | type a plain line in window 2 | it reaches that person only, and NOT the room |
| the room keeps arriving | say from the other machine while in window 2 | window 1's unread count rises on the bar |
| closing | `/close` in window 2 | back to window 1, bar loses the entry |
| the help panel | `/help`, then any key | panel measured to its text, and the room comes BACK intact underneath |

## 4. Counters, read correctly

`CHAT-MON` -> `STATUS`, before and after.

- **`relay` and `dupe` BOTH rising by one on a single say is CORRECT** in a triangle: the
  line arrives twice, direct and relayed, and dedup drops the second. That is dedup
  working, not a fault.
- **`bounce` rising** means a letter came back undeliverable. Worth looking at - but check
  step 2 first.

## 5. The repaint cost, if the screen feels slow

Take `bytes received` from the terminal before and after one arriving line.

| | bytes |
|---|---|
| one arriving line | **~191** |
| a full-screen repaint | ~3059 |

**Anything near 3000 for one line means something is calling `drawAll`** - see
`Developer/Languages/Application/PLANC-INTERACTIVE-SCREEN-PATTERNS.md`, rules 1b and 1c.

---

## The traps that make a normal-looking screen lie

| Looks like | Actually |
|---|---|
| a message was lost | the client is not seated - **no echo** |
| the client went deaf after a join | a repaint sitting unflushed in VTM's buffer; any keystroke reveals it |
| the trunk is broken | the peer aged out, or the receiver was restarted |
| the terminal is fine | SINTRAN logged it out on idle - look for `--EXIT--` before believing any command landed |
