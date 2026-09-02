# CHATXMS - the interface, decided before the sitting starts

**Written 2026-08-28.** Phase 5 of [CHAT-PLAN.md](CHAT-PLAN.md) is 35 call sites across two
sources of about 300KB each, with roughly twenty-minute compiles and three machines running the
product. The expensive part is not the typing - it is discovering the interface halfway through,
with `CHATSV` half-rewired. **This file exists so that discovery happens now, on Windows, for free.**

Nothing here has been compiled. It is a design, and it says so.

---

## What is being split, and why the line falls where it does

`CHATLIB` must stay linkable **alone**, because `CHATTST` links it alone and that is what gives a
64-check, XMSG-free, machine-free test run in seventeen seconds. The moment a kernel call enters
`CHATLIB`, that run needs XMSG, a port, and a machine - and the fast loop is gone.

So: **`CHATLIB` = things that can be tested with no machine. `CHATXMS` = the kernel calls.**
`CHAT` and `CHATSV` link both. `CHATTST` links `CHATLIB` only, unchanged.

---

## The blocker, and why it is no longer one

A shared transport routine is **handed** its buffer, so it must take `ADDR` of a PARAMETER.
Nothing in this product had ever done that - every existing `ADDR` is of a module-level array
(`inBuf`, `outBuf`, `letterBuf`) sitting in the same module as its call.

That is now measured, not assumed. `xrAddrOf` in `CHATLIB` answers with the caller's own array
(`ADDR param 1001 / ADDR here 1001`), and the listing pulled 2026-08-28 shows `FORCE xrAddr`
drawing **no warning**, where the withdrawn `FORCE INTEGER4` drew
`ILLEGAL DATA-ELEMENT TO BE CONVERTED`. **An address here is sixteen bits.**

Every routine below takes its buffer as `BYTES` and goes through `xrAddrOf`.

---

## The three shapes, read off the sources 2026-08-28

Counted: `CHAT.PLNC` 14 sites, `CHATSV.PLNC` 21.

### Shape A - SEND BY MAGIC (straight to a port we already know)

```
xmpfget(0, length, ident)
xmpfwri(0, 0, ADDR(buf(0)) FORCE XMUSERADDRESS, 0, length, wLength)
xmpsend(flags, ident, myPort, magic)          % server, CHATSV 3302
xmpfsnd(0, myPort, magic)                     % client, CHAT 1813
```

**The client and the server do not agree, and that is todo 5.7.** `xmpsend` names the message it
is sending; `xmpfsnd` does not, so it acts on whatever message the port is currently holding. One
of those can be got wrong by an intervening call and the other cannot. The server's form wins.

### Shape B - SEND BY NAME (through XROUT, when we have only a name)

```
xmpblet(letterBuf, 64, offSet, 123, systemName, portName)
xmpfget(0, 200, ident)
xmpfwri(0, 0,       letterBuf..., 0, offSet, wLength)   % the XROUT letter
xmpfwri(0, wLength, outBuf...,    0, length, offSet)    % our own bytes after it
xmprout(0, ident, port)
```

Two writes, and the second starts where the first finished. `wLength` is an OUT of the first call
and an IN of the second - a running offset, not a length. Used by the client's join (CHAT 1769)
and the server's trunk hello (CHATSV 3659).

**The name must be a subarray - `servName(0:lenServName - 1)`.** A `BYTES` argument carries its own
length, so passing the whole 21-byte array asks XROUT for a name with eleven bytes of rubbish on
the end.

### Shape C - RECEIVE

```
xmpfrcv(flags, port, msgType, remotePortHash, ident, nBytes)
xmpfmst(0, ident, msgType, senderMagic, nBytes)
xmpfrea(0, 0, ADDR(buf(0)) FORCE XMUSERADDRESS, 0, want, got)
xmpfrel(0, ident)
```

`xmpfrel` must run whatever happens after a successful `xmpfrcv`, or the message is never given
back. The server has three receive ports (room, admin, trunk) and the client one.

---

## The proposed routines

Seven names, all distinct **at seven characters** - the width that matters, because two exports
colliding at seven are ONE name to the linker and it resolves silently to whichever it met first.
The `xs` prefix is free: `CHATLIB` uses `cm`, `tr` and `xr`.

| Name | Shape | Answers |
|---|---|---|
| `xsSendM(port, magic, buf, length, flags)` | A | status |
| `xsSendN(port, sysName, prtName, buf, length)` | B | status |
| `xsRecv(port, flags, buf, bufMax, kind, magic, got)` | C | status |
| `xsOpenP(name, port)` | port open by name - `xmpopnm` | status |
| `xsOpenC(name, unique, seats, port)` | connection port - `xmpopcn` | status |
| `xsOpenF(port)` | the client's own port - `xmpfopn` | status |
| `xsInfoC(port)` | `xmpinfc(0, port, 1, 0)` - 3 sites, all identical | status |

**`xsRecv` clamps `want` with `xrClamp(bufMax, buf)` before `xmpfrea`.** That is the whole reason
the receive path is worth sharing: PLANC checks no array bounds, a length arriving from XMSG is a
length from outside the program, and one clamp in one place cannot be forgotten at site fourteen.

**`xsRecv` releases on every exit path**, including the failures. Four of the current sites can
return without reaching their `xmpfrel` if a status is bad.

---

## What this design does NOT decide, and must not pretend to

- **`sendFlags` and `waitFlags` are passed through, not chosen here.** The server varies both;
  the client does not. Hiding them would make the library decide something the callers disagree
  about.
- **`xmpinfc(0, port, 1, 0)` appears three times with identical arguments.** `xsInfoC` takes only
  the port, on the evidence of those three - if a fourth site ever needs different arguments, it
  gets its own routine rather than four parameters nobody reads.
- **The letter buffer in shape B stays the CALLER'S.** It is 64 bytes and module-level at both
  sites. Moving it into `CHATXMS` would make two callers share one buffer, and the server sends
  trunk hellos from a different path than the client sends joins.

---

## The order to do it in

Shape C first - it is the one with the clamp and the release, it is the most-repeated, and it is
the one where a mistake corrupts memory rather than just failing. Then A, then B. Rewire `CHAT`
before `CHATSV`: the client is smaller, and if the interface is wrong it is discovered on 14 sites
instead of 21.

**Gate every module's listing, not just the edited one.** `CHATLIB` sat four hours with a stale
listing showing a warning from a withdrawn attempt, because the gate had been run on `CHATTST`
alone.
