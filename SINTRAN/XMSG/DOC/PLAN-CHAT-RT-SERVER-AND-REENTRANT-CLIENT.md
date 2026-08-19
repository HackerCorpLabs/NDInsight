# Plan: the chat server as an RT program, the client as a reentrant subsystem

**Written 2026-08-18.** Two changes that turn the chat from a demo into something the machine hosts.
Everything below is marked **VERIFIED** (measured or quoted from a manual) or **NOT ESTABLISHED**.

---

## Why, in one line each

**The server should be RT** because it currently eats one of D100's three terminal lines, needs
somebody logged in, and dies when that line goes.

**The client should be reentrant** because every user who types `@CHAT` today gets a private copy of
the whole program image, and the room seats sixteen.

---

## What is already true, and it is more than expected

**VERIFIED on D100, 2026-08-18** - three demo users created (`OLAV`, `ANNA`, `KARI`, 200 pages each,
blank passwords) and used:

 - **an ordinary user can already run the client.** Logged in as `OLAV`, `@CHAT` started, prompted
   for a nickname, joined, and the room answered. The program lives under `SYSTEM` and SINTRAN found
   it unqualified;
 - **an ordinary user can create files in their own area.** `@CREATE-FILE CHAT:CNFG,1` as `OLAV`
   succeeded. That is the whole storage requirement for task [59];
 - **the seat accounting survives it** - see `PLAN-2026-08-18.md`, phase 2.

**So neither change is needed to make the chat WORK for several users.** Both are about how the
machine hosts it, and saying that plainly matters: it stops either being sold as a bug fix.

---

## Part A - CHATSV as an RT program

### What is verified

 - **XMSG is available to RT programs.** The error list has `XEIRT`, "Illegal function for
   RT-programs (only drivers)", which by its own wording is the exception rather than the rule, and
   the COSMOS guide's sample server is an RT program.
 - **Termination still cleans up.** `XMPFDCT` runs automatically "on log out or RT program
   termination" and closes every port, which clears the XROUT name. Measured for the background
   case; the manual states the RT case in the same sentence.
 - **The loader exists and the recipe is on disk.** `@RT-LOADER`, then `DECLARE-PROG`,
   `CHANGE-RT-DES`, `END-LOAD` - a worked example is in
   `Reference-Manuals\210166F SIBAS II for ND-100.md` around line 1105.

### The one real problem: OUTPUT(1, ...)

CHATSV prints its banner, its refusals and now its reap lines to **device 1, the caller's terminal**.
An RT program has no caller and no terminal.

**NOT ESTABLISHED:** what device 1 does for an RT program on this machine. Three candidates, in the
order they should be tried, cheapest first:

 1. **Give the program a terminal number of its own** and print there - a spare line, or the console.
    Keeps every diagnostic exactly as it is.
 2. **Write to a file** opened at start-up. Survives a restart and can be read while the server runs,
    which the terminal version cannot.
 3. **Drop the prints.** Cheapest to write and the worst to live with - the reap line is how a
    vanished member is noticed at all.

**Recommended: 2, with 1 as the fallback.** A server nobody is watching needs a log, not a screen.

### Order of work

 1. decide the output route above, by measuring what device 1 does from an RT program;
 2. change `CHATSV.PLNC` to open its log once at start-up;
 3. build as now (the BRF and linker steps do not change);
 4. `@RT-LOADER` / `DECLARE-PROG` / `CHANGE-RT-DES` / `END-LOAD`;
 5. **prove it the same way phase 2 was proved**: `LIST-NAMES` shows `CHAT-LOBBY` with 16 free, two
    users join from two terminals, one is killed, the survivor speaks, the seat comes back;
 6. only then add it to the boot chain.

### What it buys, concretely

Three terminal lines is the whole of D100. The server holding one is why this session needed a third
telnet session to run a two-client test at all, and why an earlier `STOP-TERMINAL` on the server's
line produced the lingering name that task [61] was wrongly written about.

---

## Part B - CHAT as a reentrant subsystem

### What is verified

 - **The command is `DUMP-REENTRANT`**, and it takes a name, a start address, a restart address and
   a `:BPUN` file: `@DUMP-REENTRANT PLANC-100,0,1,<input-file>` - from
   `Reference-Manuals\ND-10309B PLANC FOR ND-100.md`, which is the PLANC compiler installing ITSELF.
   The four arguments line up with the four columns `LIST-REENTRANT` prints.
 - **`LIST-REENTRANT` on D100 already shows six of them** - MAC, NPL, PLANC-100, PED, FORTRAN-100 -
   so the mechanism is live on this machine, not theoretical.

### The one real problem: our data is in the code

A reentrant subsystem is **one copy of the code shared by every user, with each user's data
somewhere else**. `CHAT.PLNC` keeps everything at module level - `inBuf`, `outBuf`, `typed`,
`myName`, `myPort`, `serverMagic`, the lot. In a single-bank build those sit **in the same bank as
the code**, so two users sharing the code would share `myName` and `serverMagic` too.

**This is the whole of the work**, and it is why the change is bigger than part A despite sounding
smaller.

 - the build uses `PLANC-1BANK-F00`;
 - `PLANC-2BANK-F00` exists and is already referred to in `CHAT.PLNC`'s own notes (both were loaded
   when hunting `5MON_P`);
 - **NOT ESTABLISHED:** that the two-bank runtime is sufficient, or what else a PLANC program must
   do to be dumped reentrant. The PLANC manual documents the compiler doing it to itself, which is
   the best available worked example and should be read in full before any code is changed.

### Order of work

 1. **read the PLANC manual's own installation section end to end** - it is the only worked example
    of a PLANC program being made reentrant, and it is on disk;
 2. establish what separates code from data in a two-bank build, and what that means for
    module-level `BYTES` arrays;
 3. rebuild `CHAT` against `PLANC-2BANK-F00` and check it still runs as an ordinary program - a
    step that can fail cheaply and tells you a lot;
 4. produce a `:BPUN` and `DUMP-REENTRANT` it;
 5. **prove it with the demo users**: `OLAV` and `ANNA` in the room at the same time, each with
    their own nickname, and `LIST-REENTRANT` showing one `CHAT`.

**Step 5 is the point of the demo users.** Two people sharing one copy of the code while keeping
separate nicknames is exactly the failure a shared-data bug would produce, and nothing short of two
real users at once will show it.

---

## How this meets task [59], CHAT:CNFG

The three are one piece of work and should be sequenced together:

 - **[59] first, as an ordinary program.** `OLAV` already has a `CHAT:CNFG` in their own area; the
   client reads it at start-up instead of prompting, and writes it when `/nick` succeeds. No
   reentrancy needed, provable today, and it makes the demo users worth having.
 - **Part B second.** Per-user config is the strongest possible test of per-user data: if the
   reentrant build shares one `myName`, two users will disagree about who they are and the config
   file will say so.
 - **Part A whenever convenient.** It is independent of both.

---

## Demo users created 2026-08-18

| machine | users | space | password |
|---|---|---|---|
| D100 | `OLAV`, `ANNA`, `KARI` | 200 pages | blank |

**D102 and D103 are not done.** `terminal_connlist` holds D102 at `localhost:9102`; D103 has no
entry, so its port has to be read out of `RetroCore.ini` rather than guessed. Users there are only
needed for federation, task [58], and creating them before that work starts would be guessing at
what it needs.
