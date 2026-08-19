# Friend systems - what they are, and when you need one

Short version: **a friend system is a remote system your machine treats as PRIVILEGED.** Without
it the remote system is nonprivileged, XROUT will not resolve names on its behalf, and every
conversation it opens is refused - silently, with no error sent back.

This page exists because that silence cost a whole evening twice, on 2026-08-07 and again on
2026-08-17. Both times every listing looked correct.

## The definition, from the manual

`ND-60.164.3 EN COSMOS Programmer Guide`, in the `XMPFCPV` (check privileges) section:

> "An nonprivileged task is a task which has not (yet) successfully executed the XMPFPRV call.
> An nonprivileged system is a remote system which has not (yet) been defined as a **friend** to
> your system, see XROUT service **XSDAT**."

So there are two independent privilege axes, and they are easy to confuse:

| Axis | Who it is about | How it is granted | Checked with |
| --- | --- | --- | --- |
| privileged TASK | a program on THIS machine | `XMPFPRV` | `XMPFCPV` |
| privileged SYSTEM | a whole remote MACHINE | defined as a **friend** | `XMPFCPV`, XROUT `XSDAT` |

A friend system is the second. It is machine-to-machine, it is not per user, and it has nothing
to do with the FRIEND ACCESS bits on a SINTRAN file - those are a file-system concept about one
user being another user's friend. Same word, different subject.

## When you need one

**Whenever a remote system must be able to reach a NAMED server on yours, or you on theirs.**
In practice that is every COSMOS/XMSG service: file access (`*FA-SERVER`), remote batch
(`*XFTRA`), TAD (`*TADADM`), and any server of your own that XROUT looks up by name.

You need it on the machine that is being ASKED. D100 needs D19999 as a friend for our node to
open a conversation with D100's file server.

## The symptom, and why it is so confusing

**Total silence.** The connect letter goes out and nothing comes back - not an error, not an
XRUNN, nothing. Everything you would naturally check looks right:

 - `LIST-NAMES` lists the system.
 - `LIST-SYSTEMS` shows it `ON NET`.
 - `LIST-LINKS` shows the link `State Run` with thousands of frames received.
 - A second `DEF-REMOTE` even answers that the name is already taken.

The reason nothing comes back is that the rejection happens before XROUT is asked, so there is
no error to send. The only outward sign is a counter: `LIST-LINKS` shows `RXBad` climbing by
exactly one per connect letter (see [WHAT-WE-DO-NOT-KNOW.md](WHAT-WE-DO-NOT-KNOW.md)).

**When it DOES answer, it answers XRUNN** - "unknown name (of server or system)" - and that
means the SYSTEM half of the phrase, not the server half. Every reading that blamed
`*FA-SERVER` for being unresolvable was looking at the wrong half of the error's own name.

## Ask the machine

`X-C` -> `LIST-ROUTING-INFO`, blank for both prompts. It states the fault in plain English:

```
   To     Route
    100  L: *
         T: *
         A: *
  19999  L: *->19999
         T: *, but no access to system 19999
         A: *, but no route to system 19999
```

`L:` local tables, `T:` tables, `A:` actual path. **"no access" on the `T:` line is the missing
friend entry.** Compare node 100, which has all three.

## Granting it

```
@X-COMM
X-C: DEFINE-FRIEND-SYSTEM
System? 19999
Ok
```

`LIST-FRIEND-SYSTEMS` answering `* No friend exists *` is the same fact from another angle.

## THE ORDER MATTERS - this is the part that bit us

**`DEF-REMOTE` CLEARS THE FRIEND ENTRY.** Measured on D100 2026-08-17: with access granted, a
later `DEF-REMOTE` for the same system put `LIST-ROUTING-INFO` straight back to
"no access to system 19999".

So the bring-up order is:

1. `DEF-REMOTE` - gives the system a NAME in the local table.
2. `START-LINK`, and **COSMOS** (`COS-START-E04`).
3. `DEFINE-FRIEND-SYSTEM` - gives it ACCESS. **LAST. After COSMOS, not before.**

Never the other way round, and never `DEF-REMOTE` again afterwards without re-granting.

**COSMOS WIPES THE FRIEND ENTRIES, AND THAT IS WHY STEP 3 IS LAST.** Measured 2026-08-18: a full
clean run of `tools/restart-xmsg-cosmos.ps1` granted every friend in its X-C step and every one
answered `Ok` - and by the end of the same run they were gone:

```
   To     Route
  19999  L: *->19999
         T: *, but no access to system 19999
```

with the link up, the name defined, and `*FA-SERVER` registered with 30 free seats. Every transfer
was refused in total silence. Granting the SAME entry by hand at that point, changing nothing else,
made D100 answer the very next connect letter and the FA ladder started climbing.

`COS-START-E04` makes its own system definitions, and a `DEF-REMOTE` clears the friend flag of the
system it names - so a grant made before COSMOS is a grant to something about to be overwritten.
The script now has a step 6 that does the grants after COSMOS for exactly this reason.

**CHECK IT ON THE ROUTING LINE, NOT WITH `LIST-FRIEND-SYSTEMS`.** The two tables disagree: with
access broken, `LIST-FRIEND-SYSTEMS` still listed `19999` quite happily while `LIST-ROUTING-INFO`
said "no access". Only the routing line predicts behaviour.

**CORRECTION, LATER THE SAME NIGHT: "only the routing line is honest" was TOO STRONG.** With the
kernel's own advanced diagnostics on, THREE independent measures said the friend entry was present
while `LIST-ROUTING-INFO` said "no access":

```
X-C(Adv) -> LIST-UTILIZATION      Friend table.....:  limit 128, max used 5, IN USE 5
X-C      -> LIST-FRIEND-SYSTEMS   19999
X-C      -> LIST-SYSTEMS          Access *----F
```

Five friend entries for the five systems the bring-up grants, and 19999 among them by name. So
**"no access" on the `T:` line does NOT simply mean "the friend entry is missing"** - the entry is
demonstrably there. What the routing line has going for it is that it CORRELATES with the refusals;
what it does not have is a proved meaning. Treat it as the best available symptom, not as a
diagnosis, until something explains the disagreement.

The paragraph below is kept because the observation in it is still real - the three tables do read
healthy while traffic is refused - but its conclusion, that `LIST-SYSTEMS` is simply lying, is not
supported.

Measured 2026-08-18: with every transfer being refused, `LIST-SYSTEMS` showed

```
1211535  19999  ...  *----F
```

while `LIST-ROUTING-INFO` on the same machine, seconds apart, said

```
  19999  T: 100, but no access to system 19999
```

`LIST-NAMES` agreed with the optimistic reading too - `*FA-SERVER` on port 11 with 30 free
connections - and the link was `State Run`. **Three tables said healthy and the machine refused
every letter.** Only the routing line was right.

So there is exactly ONE trustworthy check, and it is `LIST-ROUTING-INFO` with the whole line read.
Reading half of it is its own trap: the fault text is appended AFTER `T: *`, so a screen caught
mid-write shows a clean `T: *` that means nothing.

**And an XMSG restart clears friend entries along with everything else**, so this is part of
every bring-up, not a one-time setup step. That is why the fault came back: the restart script
restored the names and not the access.

## GRANTING IT BY HAND TRADES "no access" FOR "no route" - REPRODUCED TWICE, 2026-08-18

On 2026-08-17 granting the friend by hand, changing nothing else, made D100 answer the very next
connect letter. **On 2026-08-18 the same move did not work, and made the routing worse - and it
did it twice, once on a machine whose tables I had already disturbed and once on a machine
seconds out of a clean `restart-xmsg-cosmos.ps1` run.** So it is the command's behaviour, not a
damaged table.

`DEFINE-FRIEND-SYSTEM 19999` answered `Ok` and removed the "no access" line - and in the same
moment the `L:` line LOST its path:

```
before:  19999  L: 100->19999
                T: 100, but no access to system 19999

after:   19999  L: 100, but no route to system 19999
                T: 100, but no route to system 19999
```

Transfers were still refused. Trying to put the name back is a dead end - `DEF-REMOTE` answers

```
XMSG Routing/Naming error: Another port already has this name
```

and no command clears that name. This is the standing rule and it is now proved twice:
**do not repair XMSG routing by hand; a broken route needs a full restart.** The hand-grant is a
diagnostic that tells you the friend entry was missing, not a repair you can rely on.

**A restart is not a guaranteed fix either.** Three full `restart-xmsg-cosmos.ps1` runs the same
night: after the first, pushes worked immediately; after the second and third,
`LIST-ROUTING-INFO` said **"no access"** within two minutes of the run finishing, despite step 6
granting every friend and every grant answering `Ok`.

So the two failure states are distinct, and both were seen:

| routing says | means | seen after |
| --- | --- | --- |
| `T: ..., but no access` | friend entry missing, route intact | a clean bring-up (runs 2 and 3) |
| `..., but no route` on all three lines | route gone, access granted | a hand `DEFINE-FRIEND-SYSTEM` |

**Neither state passes traffic.** What clears the friend entry between step 6 and the next
transfer is NOT established - see `WHAT-WE-DO-NOT-KNOW.md`. One suspect worth testing first: our
own `--announce-restart` (a ReachabilityRequest) may make D100 re-create the system entry, and
re-creating it clears the friend flag exactly as `DEF-REMOTE` does. That is testable by checking
the routing line before AND after a single announce, with nothing else running.

## Finding the commands at all, and the parameter trap

**`HELP` lists the whole command set** - it prompts `Commands?`, RETURN gives all of them, and
`More?` pages. The `?` command is NOT this: it lists only what is new in the release, which is
how `Remove-System` and `Remove-Friend-System` stay hidden while people invent names that do not
exist.

**THE FIRST PARAMETER IS USUALLY THE XROUT TO ASK, NOT THE SUBJECT.** Typed inline it is eaten
by the first prompt, and the command then goes to a system that may be exactly the unreachable
one:

```
X-C: REMOVE-SYSTEM 19999
System? 19999
XMSG Kernel error: Remote system is not accessible      <- asked XROUT ON 19999
```

Answer the prompts and name the LOCAL system first:

```
X-C: REMOVE-SYSTEM
XROUT system? 100
System? 19999
Ok
```

**But not every command is like that.** `DEFINE-FRIEND-SYSTEM 19999` inline answers `Ok`, so its
first parameter IS the system. Read the prompts before trusting an inline argument.

## One more trap on the command itself

The one-line form can aim at the wrong XROUT. When the target is unreachable,

```
X-C: DEF-REMOTE,,D19999 19999
Error in communicating with XROUT.
XMSG Kernel error: Remote system is not accessible
```

while answering the prompts and naming the LOCAL system explicitly works:

```
X-C: DEF-REMOTE
XROUT system? 100
Port or system name? D19999
Remote system or port no? 19999
Ok
```

## What this is NOT: the route line

`LIST-ROUTING-INFO` prints three lines and only the `T:` **access** complaint is the friend
entry. **"no route to system 19999" on an ADJACENT system is CORRECT and must be left alone.**
`ND-30.025.02 COSMOS Operator Guide` 2.5.4: "it is not necessary to define the route to the
adjacent system(s)... physically connected to the local system with an HDLC or Megalink cable."
Giving an adjacent system a route is wrong, and once stopped D103 reaching us
(`SINTRAN/XMSG/DOC/WHAT-WE-DO-NOT-KNOW.md`).

The healthy indicator is the `A:` line showing the path - `A: *->19999`.

## Proof it is the gate

2026-08-17, in order: transfers refused with no reply; `LIST-ROUTING-INFO` said "no access";
`DEFINE-FRIEND-SYSTEM` -> `Ok`; a stray `DEF-REMOTE` put the "no access" back and transfers
failed again; re-granting the friend after the name made **38466 bytes of CHAT:PLNC and 6104 of
CHATCC:MODE go across on the next attempt**.

## See also

 - [WHAT-WE-DO-NOT-KNOW.md](WHAT-WE-DO-NOT-KNOW.md) - the RXBad counter as a refusal oracle
 - `tools/restart-xmsg-cosmos.ps1` - the bring-up, both passes
 - `Reference-Manuals/ND-60.164.3 EN COSMOS Programmer Guide.md` - `XMPFCPV`, and XROUT service
   `XSDAT` (Define/Remove Attribute) which is what a friend entry really is
