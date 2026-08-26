# Where AI-assisted work on XMSG/CHAT actually loses time

Read across four Claude sessions for this project (1570 user turns) plus the current
one. This is not a list of things that went wrong - it is the list of things that went
wrong **more than once**, with what would stop each one.

**The headline: a chat program of about two thousand lines took days. About a third of
that was the machine misbehaving. The rest was avoidable, and every avoidable part is a
rule or a script below.**

---

## 1. Reasoning where a measurement was available

The single most expensive pattern, and the one you have corrected most often -
*"do not assume anything ... analyse problem, get root cause, validate with oracle"*,
*"why dont you look at a real working capture or carve the code"*.

Three cases in one day, each three rounds of reading source before a number settled it:

| Symptom | What settled it | Cost of reading first |
|---|---|---|
| a said line never appeared | pressing one key that does nothing | 3 wrong theories |
| a window name never drew | printing the stored length | 3 readings of correct code |
| "lines do not cross the trunk" | starting a fresh client | a whole capture + a retracted finding |

**Why it keeps happening:** a 20-minute compile makes instrumenting feel expensive, so
reasoning gets one more round instead. Three rounds cost more than one build and are
wrong more often.

**Fix:** rule added to the `planc` skill - *after ONE failed explanation, stop reading
and add a number*. And the free experiments come first: press a dead key, run the tool
against a known-good input, take a counter before and after.

## 2. The build loop has four steps that feel like one

`lint -> stage -> DELIVER -> COMPILE -> GATE`. Each has been skipped while the screen
looked healthy:

- staged into `sync-out` while the daemon watched `sync-relay` - nothing carried;
- confirmed `done, NNNNN byte(s)` twice and **never started the compile** - the matching
  byte count reads as completion;
- compiled the previous source because the daemon's window had expired two minutes before;
- read "0 errors" off a listing that was still transferring.

**Fix now:** the proof-per-step table is in the `nd-build-loop` skill.
**Fix worth building: `tools/nd-build.ps1`** that does all five and exits non-zero at any
step - reads the daemon's real `--sync` argument from `Win32_Process`, waits for the
matching `done, N` line, starts the compile, waits for `LINE:` to stop rising, pulls the
listing, waits for it to stop growing, then gates. It removes four separate ways to
believe a build happened when it did not.

## 3. The gate itself gives false positives

`\)/[A-Z0-9]+ +\*\*\* ERROR` matches a source COMMENT quoting a past error, and these
sources are full of those deliberately. A clean build reported three "errors".

**Fix:** count only non-comment lines (`grep -v '%'`), and check the listing's last line
number reaches the source's last line - a truncated pull otherwise reads as clean.

## 4. One 6455-line source, one 20-minute compile

**This is the biggest structural lever left.** Every test of a one-line change costs
twenty minutes, which is what makes instrumenting feel too expensive (see 1) and what
turns a wrong guess into an hour.

The linker already loads several BRFs (`LOAD CHAT`, `LOAD XMP-100-1-B02`, `LOAD INTRF1B`
...), so **separate compilation is already how everything else here is built.** Splitting
`CHAT.PLNC` into three or four modules - wire/protocol, screen renderer, command
handling, main loop - and compiling only the changed one would cut the common iteration
from twenty minutes to a few.

Cost: the 7-character limit on names crossing a BRF boundary, and deciding the module
seams. Both are one-off.

## 5. Test discipline: an unseated client looks perfectly normal

Three separate "it does not arrive" findings this session were all taken against a client
that was not seated - one exited to run `CHAT-MON`, one left from an earlier server
generation. A client with no seat shows a completely normal screen and silently receives
nothing. One of those produced a written bug report that had to be retracted.

**Fix worth building: a written smoke sequence** (or `tools/chat-smoke.md`) - start the
receiver FRESH, make it say one line and SEE ITS OWN ECHO, only then test anything else,
and never exit the receiver mid-test. Already recorded in memory; belongs in the repo.

## 6. Machine state is invisible until it bites

Repeatedly discovered mid-test: a trunk had aged out, the daemon had exited, the hub was
not running, the terminal had been logged out by the idle timeout.

**Fix worth building: `tools/nd-preflight.ps1`** - one command printing hub alive,
daemon alive and minutes left in its window, `LIST-TRUNKS` per machine, free seats, and
whether the terminal session is still logged in. Thirty seconds that would have saved
several half-hours.

## 7. PLANC's silent failures need the linter to keep growing

Everything below compiled with `0 DIAGNOSTICS` and misbehaved at run time. The linter
catches most; two were added this week because they were not caught:

- undeclared name accepted as a constant nothing sets;
- routine used above its declaration (`ILLEGAL SYNTAX` on a name that exists);
- **module variable used above its declaration** - error blames a BRACKET (added);
- local with an initial value - compile fails, LINK SUCCEEDS, program runs with the flag unset;
- two subarrays of one array in one call - corrupts both;
- **a subarray with both bounds computed wrote nothing at all** - not yet linted; prefer
  an explicit byte loop.

## 8. Answer the question that was asked

*"i ask again, and please dont be a fucking idiot - answer my question"*,
*"when i ask, you fucking answer"*. Several turns went on status when a yes/no was
wanted. Not a tooling problem - a discipline one, and it is in memory.

---

## Recommended, in order of time returned

1. **Split `CHAT.PLNC` into separately compiled modules** - returns minutes on every
   single iteration, and makes instrumenting cheap, which fixes friction 1 as a side
   effect.
2. **`tools/nd-build.ps1`** - one command, five proofs, non-zero exit. Removes friction
   2 and 3 entirely.
3. **`tools/nd-preflight.ps1`** - removes friction 6.
4. **A written smoke sequence for the chat product** - removes friction 5.
5. **Keep `planc-lint.py` growing** - one new check per silent failure found.

