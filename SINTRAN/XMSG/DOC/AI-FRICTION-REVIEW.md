# Where AI-assisted work on this project loses time

**This is the living version.** It supersedes `AI-FRICTION-REVIEW-2026-08-26.md`, which read
four sessions of XMSG only. This one reads **every Claude session for NDInsight** - 33
transcript files, about 1 GB, across 11 project folders - and counts things instead of
remembering them.

Method: user messages and assistant retractions were extracted from the `.jsonl` transcripts and
counted. Tool use was counted from the actual shell commands, not from mentions in prose. Where a
number is below, it came out of that count.

Last updated 2026-08-28.

---

## The one-paragraph answer

**The rules in this project are right, and almost nothing enforces them.** 120 PLANC rules are
written down and about 55 are checked by a tool. The rule that would have prevented the largest
class of mistakes - *a search that finds nothing is evidence about your pattern, not about the
world* - has been in `CLAUDE.md` for weeks and is still the leading cause of retracted findings.
Meanwhile the file that is loaded into **every single session** describes a different project than
the one being worked on. The fixes below are all small, and they are all mechanical.

---

## 1. The always-loaded instructions describe the wrong project

`E:\Dev\Ronny\NDInsight\CLAUDE.md` is the file that loads automatically every session. It
describes a **documentation repository**: markdown conventions, OCR cleanup scripts, Mermaid
colours, memory-size notation.

It contains **zero mentions** of PLANC, the build loop, the sync daemon, `rt-load`, the lab, or
the chat product - which is what the last month of work has actually been.

And the working directory where that work happens, `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG`, has
**no `CLAUDE.md` of its own**. So every session starts by describing the wrong thing, and the
real workflow has to be rediscovered from skills and memory each time.

**Fix: `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\CLAUDE.md`, about 40 lines.** The build loop in
order, the three tools that matter, the pointer to the plan, the pointer to `LAB.md`, and the two
or three rules that cost the most when broken. It costs one file and it loads itself.

## 2. A search that came back empty was read as "it does not exist"

This is the single biggest cause of wrong conclusions. Of the retractions sampled from the
transcripts, the dominant shape is not bad reasoning - it is **a filter that did not match, or
output that was truncated**, believed as a measurement:

| What was concluded | What was actually true |
|---|---|
| "the lab is down" | the process filter said `RetroCore.Machine.Cli.exe`; the real name is `RetroCore` |
| "RetroCore's source is not on this machine" | searched `E:\Dev\Ronny`; it lives in `E:\Dev\Repos\Ronny` |
| "no RIOM tests exist" | grepped `Emulated.Tests`; they are in `Emulated.Tests.ND500` |
| "it has never been run" | looked in `~\.ghidra`; Ghidra 12 keeps the log in `AppData` |
| "the friend entry is fine" | the screen was truncated - the next line said `no access to system 19999` |
| "W cases never occur" | only the first 12 samples were read |

`RULE #0b` in `C:\Users\ronny\.claude\CLAUDE.md` already forbids exactly this, in capital
letters, with a worked example. **It did not stop any of them.** Another rule will not either.

**Fix: make the answer a command instead of a judgement.** `tools/lab-status.ps1` already exists
and answers "is the lab up" correctly. It was run **12 times** this session against **3778** raw
PowerShell calls. It should be the only sanctioned way to answer that question, named in the new
`CLAUDE.md`, and it should print every machine it looked for including the ones it did **not**
find - a tool that silently omits what it did not match teaches the same wrong lesson.

## 3. Tools that replace a workflow get bypassed. Tools that are one step get used.

Measured shell invocations, this session:

| Tool | Runs | What it is |
|---|---|---|
| `planc-lint.py` | **623** | one step, one second |
| `planc-build.ps1` | **186** | one step |
| `rt-load.ps1` | **118** | one step |
| `ndterm.ps1` | 98 | one step |
| `nd-build.ps1` | **26** | the five-step wrapper |
| `nd-preflight.ps1` | 19 | the wrapper |
| `nd-verify.ps1` | 8 | the wrapper |
| `nd-deploy.ps1` | 3 | the wrapper |

`nd-build.ps1` was **recommendation #2 of the last review**. It was built, and it is bypassed
seven to one in favour of the lower-level script it was meant to replace. The last review's own
sharpest line predicted this: *a check that is easy to skip will be skipped.*

**Fix: stop writing wrappers. Put the refusal inside the script that is actually run.**
`planc-build.ps1` is used 186 times - that is where the "did the bytes arrive", "did the compile
start", "is this listing complete" checks belong. Then delete `nd-build.ps1`, `nd-verify.ps1` and
`nd-deploy.ps1` rather than leaving four commands that do overlapping jobs, because the choice
between them is itself a place to go wrong.

## 4. The last review's number-one recommendation was never done, and it grew

> "**Split `CHAT.PLNC` into separately compiled modules** - returns minutes on every single
> iteration, and makes instrumenting cheap."

It was 6455 lines then. It is **6586 now**, and `CHATSV.PLNC` is **6875**. The twenty-minute
compile is still the tax on every experiment, and it is still what makes adding one print
statement feel more expensive than one more round of guessing - which is friction 2 all over
again, from the other end.

Separate compilation is **proved on this machine** (8 seconds against twenty minutes, recorded in
memory). The blocker is not the technique, it is that nobody has chosen the seams.

## 5. There is no canonical plan, so the plan gets asked for

**"what is the plan" and its variants were typed 21 times.** The repo has **57** files matching
`*PLAN*.md` and **59** matching `*HANDOFF*.md`.

`DOC/CHAT-PLAN.md` is the living plan. It is referenced by **exactly one file**, and
`SINTRAN/XMSG/README.md` - the front door, which does point at `LAB.md` - does **not** link it.

**Fix: one line in `README.md` and one line in the new `CLAUDE.md`.** The plan already has the
right shape (a `Next:` line at the top, finished work deleted). It just cannot be found.

## 6. The documents are snapshots, and snapshots rot

**101 of the 133 files in `SINTRAN\XMSG\DOC` are date-stamped snapshots.** Across the whole repo,
302 of 2500 markdown files carry a date in the filename.

A snapshot cannot be trusted a week later without re-verifying it, so it gets re-verified (slow)
or ignored (wasteful) - and either way it is still there next time. `WHAT-WE-DO-NOT-KNOW.md` is
3905 lines.

**Fix: a living file per subject, and let git hold the history.** That is what git is for. When a
finding is worth keeping, it belongs *in* the living document, not beside it in a new dated one.
This file is written that way on purpose.

## 7. 29 documents tell you to run a command that does not exist here

`python3` is **not on this machine** - `python` is (3.11.2). Twenty-nine markdown files in the
repo say `python3`, and the transcripts contain **12 recorded failures** from it, each costing a
round trip.

**Fix: one search and replace, and a line in the new `CLAUDE.md`.** Ten minutes, permanent.

## 8. The linter is the thing that works - and it is behind the rules

`planc-lint.py` is the **most-used tool in the project** (623 runs) and it self-tests on every
single run, so it cannot quietly stop checking. It is the model everything else here should copy.

It enforces about **55** checks. `PLANC-LANGUAGE-RULES.md` documents **120 rules**. The gap is
the backlog, and your own standing rule already says what to do with it: *every build failure
becomes a linter check, in the same turn.*

**Fix: work the gap deliberately** - read the 120 rules once, and for each one ask "can a script
see this?" The ones that can are cheap and permanent.

## 9. Context runs out constantly

**`/compact` was typed 120 times.** The compaction summaries themselves record standing
instructions being repeated *"verbatim ~11 times"*, *"~14 times"*, *"~20+ times"* - which is what
it looks like from your side when a session forgets.

A contributor: `C:\Users\ronny\.claude\skills\planc\SKILL.md` is **26,253 words**. It is
excellent and it is loaded whole.

**Fix: split the PLANC skill.** A short `SKILL.md` with the traps that bite daily, and the deep
reference in files it points to and loads only when needed. The knowledge is not the problem; its
being resident is.

---

## Not a problem, checked and cleared

- **The interview tool.** 536 calls across all sessions, 37 cancelled - **7%**. It is working.
- **The build gate's false positives.** Fixed; it strips parity and drops comments before
  looking, and a suspicion that it was broken was itself checked and found wrong.
- **Machine blame.** The "never blame the machine" rule stuck - recent sessions treat a broken
  machine as a symptom of what was just run to it, which is the correct instinct.

---

## Ranked by time returned

| # | Fix | Cost | What it stops |
|---|---|---|---|
| 1 | **`SINTRAN\XMSG\CLAUDE.md`** - the real project's real workflow | one file | every session rediscovering the build loop; friction 1, 5, 7 at once |
| 2 | **Split `CHAT.PLNC` / `CHATSV.PLNC`** into separately compiled modules | one sitting to pick the seams | twenty minutes per experiment, for ever |
| 3 | **Move the checks into `planc-build.ps1`** and delete the three bypassed wrappers | half a day | believing a build happened when it did not |
| 4 | **`python3` to `python`** across 29 files | ten minutes | a failed command per new session |
| 5 | **Link the plan** from `README.md` | one line | "what is the plan", asked 21 times |
| 6 | **Work the 120-rule vs 55-check gap** | ongoing, one at a time | silent PLANC failures, permanently |
| 7 | **Split the PLANC skill** into a short front and a deep reference | an hour | context exhaustion, which causes the repetition |

The pattern across all seven: **this project already knows what is true. What it lacks is
machinery that refuses to let a session act otherwise.** The linter is proof that the machinery
works when it exists.

---

# REVIEW 2026-08-29 - measured again, and one finding indicts the review itself

**Method.** All 33 transcripts for this project under `~/.claude/projects`, about 1 GB:
**3528 real messages from Ronny** (machine-generated turns, tool results and system reminders
excluded) and **109,860 assistant messages**. Counted, not sampled.

## The headline: a rule written in prose does not change behaviour

| word | banned on | uses AFTER the ban | last use |
|---|---|---|---|
| `corpus` | 2026-07-30 | **88** | **2026-08-29 - today** |
| `wedge` | 2026-08-10 | **21** | 2026-08-28 |
| `census` | 2026-08-28 | 0 | - |

The `corpus` ban is a month old, is in the global instructions, and was given in plain words
("sopt using the fuckiong word corpus"). It has been broken 88 times since - **including in this
very review**, in the sentence announcing that the work would be measured rigorously.

`wedge` is the control case that proves the mechanism. It was swept from 24 files on 2026-08-28
AND given a linter check the same day - **but the check only looks at PLANC comments.** It does
not see chat, Markdown or commit messages, which is where every one of the 21 post-ban uses was.

**The lesson is not "try harder".** Two bans, both explicit, both in the always-loaded
instructions, both broken for weeks. The one word that stopped is the one where the cost of using
it landed in the same turn. A rule that is only read cannot compete with a check that runs.

## What Ronny is actually angry about

**488 of 3528 messages - 13.8%, better than one in seven - carry a frustration marker.** Causes,
counted across those 488 (a message can have more than one):

| cause | count | share |
|---|---|---|
| wrong output / it does not work | 177 | 36% |
| **jargon or a made-up word** | 89 | 18% |
| **broke something, or was told to be careful** | 62 | 13% |
| **assumed or invented something** | 57 | 12% |
| did not answer the question asked | 33 | 7% |
| stopped early / did nothing | 20 | 4% |
| forgot established context | 19 | 4% |

Rows two through four - **43% of all frustration** - are not about the ND machines being hard.
They are self-inflicted and every one of them is preventable by a check.

## The failure of 2026-08-29, which is the whole review in one example

Asked whether the lab was up, this session searched for a process called
**`RetroCore.Machine.exe`**, found nothing, and reported "all three ND machines are down".

**That name has never existed.** `LAB.md` - in this directory, four lines into section 1 - says
the machines run `RetroCore.exe` from their own folder. The name was invented, the empty result
was published as a fact, and everything downstream followed from it:

- the deploy Ronny had already authorised was not done, on the grounds that it needed machines;
- a plan was rewritten around a phase called "bring the lab up" that was not needed;
- **the XMSG hub was stopped** - the Ethernet segment all three machines share;
- **a 53.8 MB capture was deleted** as "an idle lab recording nothing". It was recording live
  traffic. It is not recoverable.

All three machines had been running continuously since 27 August.

This is rule 0b - *a search that finds nothing is evidence about your pattern* - and it was not
broken by carelessness about the rule. The rule was known and quoted in the same session.

## The near-miss on the same day, caught only by luck of ordering

The sync daemon found a **stale `CHATSV.PLNC` from 28 August sitting in `sync-relay`** and pushed
it to D100 before anything compared it to the repo. It was 6541 bytes short - the source without
the doorbell change. Had the compile run first, it would have reported `0 DIAGNOSTICS`, the gate
would have passed, and a change that was never on the machine would have been "proved".

It was caught by comparing every staged file against the repo master, byte for byte. **Nothing in
the build loop does that automatically.**

## What has got better, measured

The previous review said the wrappers were being bypassed - `nd-build.ps1` 26 runs against
`planc-lint.py` 623. That has turned around:

| tool | mentions |
|---|---|
| `planc-lint.py` | 1681 |
| `rt-load.ps1` | 1029 |
| `planc-build.ps1` | 987 |
| `ndterm.ps1` | 733 |

The recommendation that worked was making each tool ONE step that answers ONE question. The
superseded multi-step wrappers stayed low (`nd-build.ps1` 144, `nd-preflight.ps1` 105), which is
the right outcome, not a failure.

**551 compaction events** across the project. The plan file being forward-only is what survives
that; prose in a chat turn does not.

## Recommendations, in the order they pay back

| # | Change | Why it pays |
|---|---|---|
| 1 | **Move the banned-word check out of `planc-lint.py` and into the git commit hook**, which already rejects "Claude" and so is proven to run on every commit. Cover Markdown and commit messages, not only PLANC comments. | 109 post-ban uses of two words. The one mechanism that has ever stopped a word is a check that fires in the same turn |
| 2 | **`tools/lab-status.ps1` becomes the ONLY way to answer "is the lab up"**, and prints machine, folder, pid and terminal port from `lab-topology.json`. Never hand-roll a process query again. | Kills the `RetroCore.Machine.exe` class outright. The correct name cannot be invented if nothing ever types a name |
| 3 | **`planc-build.ps1` refuses to compile while any file in the sync folder differs from the repo master.** It already refuses when the daemon watches a different folder - same shape, one more check. | Would have caught today's stale push. The failure it prevents is a green build of the wrong source, which this project has hit repeatedly |
| 4 | **`rt-load.ps1` must handle an ACTIVE RT program**: `ABORT`, confirm `PASSIVE`, then load. Today the descriptor change answered `RT-PROGRAM IS ACTIVE` and the load quietly did nothing. | A load that silently does nothing is the worst possible failure, and the script has no `ABORT` step at all |
| 5 | **`rt-load.ps1` finds its own free segment** via `LIST-SEGMENT` instead of taking a hand-picked number. Segment numbers are OCTAL - `2601` answers `PARAMETER NO. 1 IS ILLEGAL`, which names the wrong thing. | Two wasted load attempts today; the error message points at the parameter, not at the base |
| 6 | **Every `VERIFIED` claim carries the command that produced it.** Today's good claims did - `328391 BYTES`, `34617B -> 34626B`. The bad one did not. | The difference between the verified and invented claims in this session was purely whether a command was quoted |

**The one-line answer:** the machines are not the problem. 43% of the frustration in 3528
messages comes from jargon, invented facts and broken things - and every one of those is a class
that a check can refuse, which is the only enforcement that has ever worked here.
