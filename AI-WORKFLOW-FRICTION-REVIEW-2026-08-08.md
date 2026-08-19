# Where AI-assisted work keeps getting stuck - review of 46 sessions

**Written**: 2026-08-08
**Source**: all Claude session transcripts under `C:\Users\ronny\.claude\projects\*NDInsight*`
(46 sessions, about 700 MB of JSONL)

---

## How this was measured

A script walked every transcript and pulled out two things:

- **messages Ronny actually typed** (system-injected text - skill files, compaction
  summaries, `/loop` bodies, context reports - filtered out; anything over 1200
  characters treated as injected)
- **assistant messages admitting a wrong assumption** (matched on "I was wrong",
  "correction", "I assumed", "false alarm", "retract", and similar)

Script: `scratchpad/analyse2.py` (working file, not committed).

| Measure | Count |
|---|---|
| Real typed user messages | 3,153 |
| Of those, pushback / correction | **355 (11%)** |
| Assistant self-corrections | **613** |

**UNVERIFIED**: the theme buckets below are keyword matches, so the counts are
indicative, not exact. Some task-notification noise survived the filter. The
quoted examples are all genuine.

### What the pushback was about

| Count | Theme |
|---|---|
| 83 | hardware / emulator facts stated wrongly |
| 38 | assumed instead of verifying |
| 36 | wrong or missing file path |
| 27 | did not run or validate tests |
| 17 | commit hygiene (mentioning Claude) |
| 15 | asked in prose instead of the interview tool |
| 10 | processes killed or left running |

The striking number is **613 self-corrections against 355 pushbacks**. Most wrong
turns are caught internally - but only after the work was already done wrong. The
cost is wasted turns, not usually wrong final answers.

---

## The recurring patterns

### 1. Facts that decay are stored as prose, and prose does not update itself

The single most expensive pattern. Examples:

- The `unity-norskdata` skill says the player is *"Starter Assets PlayerArmature
  (the robot placeholder)"*. It is not, and has not been for some time. On
  2026-08-08 I repeated that claim and had to be corrected. `PlayerArmature`
  appears **0 times** in `SampleScene.unity`.
- The memory index already carries the warning *"read `unity-norskdata` FIRST
  (but it is STALE - see below)"*. A document that has to warn you it is wrong
  is not doing its job.
- Memory entries like `accp-control-store-model-corrected` exist specifically
  because *"5 old MEASURED facts"* turned out dead.
- `stale-status-headers-lie` - a memory whose whole content is "grep before
  believing OPEN".

The pattern: **hand-written descriptions of current state go stale silently.**
Invariants (a wire protocol, a register layout, a cabinet dimension) are fine in
prose. Current state (what is in the scene, what is wired, what is done) is not.

### 2. Measurements taken through the wrong API silently lie

Fresh evidence from 2026-08-07/08, all in one session:

- `UnityEditor.UnityStats` looked authoritative and gave draw-call and triangle
  counts. It was reporting a stale Game view frame. Proved it by hiding two huge
  objects and re-rendering - **the numbers did not move**. Every figure quoted
  from it had to be retracted.
- `Camera.Render()` timing gave 4.86 ms warm and 706 ms cold for the same scene.
  Useless as a benchmark, and it silently blew past the MCP call timeout,
  producing `success:false` with no message.
- Earlier in the project: play mode is **frozen** while the Editor is unfocused,
  so "velocity=0, all settled" measurements were measuring frame zero.
- Screenshot **capture ghosts** - the renderer drew objects at stale poses that
  disagreed with every transform, bound and raycast. Cost a whole session.

The pattern: **there is no blessed way to measure, so each session invents one,
and some of them are wrong.**

### 3. Rules exist only as instructions, with nothing enforcing them

`CLAUDE.md` forbids mentioning Claude in commits, forbids LINQ, requires full
paths for markdown, requires `dotnet build-server shutdown`. There are:

- **no git hooks at all** (`.git/hooks` holds only `.sample` files)
- **no doc or path lint** in `scripts/`

17 pushbacks about commit hygiene alone. An instruction that is violated 17 times
is not an instruction problem, it is a missing check.

### 4. Tooling failures diagnose themselves badly

On 2026-08-08 the Unity MCP bridge detached. `execute_code` returned
`{"success":false,"message":null,"data":null}` - no reason at all - about eight
times across roughly 40 minutes. Only `read_console` gave the actual cause:

```
Unity session not ready for 'read_console' (ping not answered); please retry
```

Time was also lost to `manage_editor action=get_state`, which is not a valid
action but took over 120 seconds to say so.

### 5. Answers are too long

*"too much text for me to process. what is you fucking question ?"* - and
*"i give a fuck about the plan"*, *"remember! when i ask for a plan, i dont give
a shit for what is done"*. This one is behavioural and mine to fix, not the
codebase's. Noted here so it is not lost.

---

## Recommendations, best payoff first

### R1. Generate the state descriptions instead of writing them

**Problem**: pattern 1 above - the largest single time sink.

**Fix**: a committed Editor script that dumps scene state to JSON, run on demand:

```
Assets/Editor/ProjectStateReport.cs   ->   ProjectState.json  (git-tracked)
```

Contents: scene roots with renderer and triangle counts, the player object and
its components, camera rig, LOD groups, static-flag and occlusion status, package
versions. Then **delete the "what we are building" prose from the
`unity-norskdata` skill** and have it say only: *"run ProjectStateReport and read
`ProjectState.json` - never describe the scene from memory."*

Same idea for the emulator side: `scripts/` already has `validate-mon-carves.py`
and `verify-dt-devices.py`, which is exactly the right shape. Extend that habit
to "what is currently wired" rather than only "is this carve correct".

**Cost**: a few hours. **Payoff**: kills the recurring stale-fact correction.

### R2. One blessed measurement tool per domain

**Problem**: pattern 2 - measurements that lie.

**Fix**: commit the correct method once, with the traps written into the file as
comments:

- `Assets/Editor/PerfReport.cs` - takes counts from a **real Game view frame**,
  not `UnityStats` after an off-screen `Camera.Render()`. Documents in comments
  that UnityStats does not observe off-screen renders and that play mode is
  frozen when the Editor is unfocused.
- Keep the "render to RenderTexture, ReadPixels, average luma per column" trick
  from the street-lamp work - it is the one thing that reliably answered "is this
  actually brighter". It currently exists only in a memory file.

**Cost**: half a day. **Payoff**: no more retracted numbers.

### R3. Mechanical enforcement of the rules that keep getting broken

**Fix**, in order of value for effort:

1. **`commit-msg` hook** rejecting any message matching `claude|Co-Authored-By`.
   Ten lines. Ends a 17-occurrence problem permanently.
2. **`scripts/check-docs.ps1`** - fails if a markdown file under the repo
   contains an absolute path (`E:\`, `C:\`, `/`), which `CLAUDE.md` already
   forbids but nothing checks.
3. **`scripts/dev-cleanup.ps1`** - `dotnet build-server shutdown`, then list any
   remaining `dotnet.exe` / `testhost.exe` / `MSBuild.exe` **with their command
   lines** so it is obvious which are Ronny's and which are strays. The
   locked-DLL-gives-fake-green-tests trap is already a memory; make it a script.

**Cost**: an hour for all three. **Payoff**: removes three whole categories.

### R4. Make the Unity bridge fail fast and reconnect itself

**Fix**:
- Turn on **"Auto-Start Server on Editor Load"** in the MCP for Unity Advanced
  Settings (the skill documents this exists; it is not on).
- Add `scripts/unity-ping.ps1` that calls `read_console` first - the only tool
  that returns a useful error - and prints either "bridge up" or the exact
  reason. Run it before any Unity batch instead of discovering the problem eight
  calls in.

**Cost**: fifteen minutes. **Payoff**: roughly 40 minutes recovered per incident.

### R5. Split every skill into invariants and current state

**Fix**: two headings in each skill file.

- **Invariants** - protocol facts, register layouts, measured dimensions,
  hard-won traps. These age well; keep writing them.
- **Current state** - dated, and every claim must name the command that checks
  it. If it cannot name one, it belongs in R1's generated file instead.

The traps sections are the genuinely valuable part and should be expanded, not
trimmed - `sintran-pads-bodies-with-f0` saved two days, `retrocore-ini-has-the-ports`
saves a guess every session.

### R6. A repo-root `paths.json`

**Problem**: 36 pushbacks about paths, and Rule #0 exists solely because of a
wrong-folder incident.

**Fix**: one file naming the canonical locations - models, manuals, photo archive
(`\\Nas9t\data\NorskData\Pictures`), disc images (`F:\ND`), the Unity project,
the RetroTerm binary, scratch. Resolve names from it instead of remembering
paths. It is also self-documenting for any future assistant.

---

## What is already working and should not change

- **The memory index.** Dense, cross-linked, and the "cost me two days" notes are
  what stop repeats. `ask-the-machine-before-carving-it` and
  `grep-reference-manuals-first` are worth more than most tooling.
- **`scripts/validate-*.py` / `verify-*.py`.** Exactly the right instinct -
  a script that checks a claim beats a paragraph asserting it.
- **Rule #0 and the honesty requirement.** The 613 self-corrections show they are
  landing; without them those would be confident wrong answers instead.
