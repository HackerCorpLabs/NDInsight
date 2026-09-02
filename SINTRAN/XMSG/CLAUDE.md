# CLAUDE.md - SINTRAN/XMSG

Guidance for this directory. It **overrides** the repository-level `CLAUDE.md` two levels up,
which describes NDInsight as a documentation repo. That is not what happens here.

**What happens here:** a chat product written in PLANC that runs on three live SINTRAN III
machines, and the C# XMSG/COSMOS transport that carries files to them. The chat product is the
deliverable; XMSG work is support for it.

---

## Read these first, in this order

| | |
|---|---|
| **[DOC/CHAT-PLAN.md](DOC/CHAT-PLAN.md)** | **The plan.** One `Next:` line at the top. Finished work is deleted, not ticked off - so everything in it is outstanding |
| **[LAB.md](LAB.md)** | The machines, ports and traps. Read before touching D100/D102/D103 |
| **[DOC/CHAT-APP-SPECIFICATION.md](DOC/CHAT-APP-SPECIFICATION.md)** | What the product does today |
| **[DOC/manuals/](DOC/manuals/)** | The manuals: user, admin (install, CHAT-MON, trunks, boot), build-and-deploy (this loop, written out with proofs), and CHAT-ARCHITECTURE (server, clients, trunks, limits with their constants, measured performance) |
| **[DOC/AI-FRICTION-REVIEW.md](DOC/AI-FRICTION-REVIEW.md)** | Where this work actually loses time, measured |

Skills that apply here: `planc`, `planc-safe`, `xmsg-safe`, `nd-build-loop`.

## The machines

| Machine | Chat name | Terminal | Trunks to |
|---|---|---|---|
| D100 (sys 100) | FJELL | 9010 | 102, 103 |
| D102 (sys 102) | VIDDA | 9102 | 100, 103 |
| D103 (sys 103) | SKOGEN | 9003 | 100, 102 |

Machine-readable in [lab-topology.json](lab-topology.json). Drive them with the **retroterm MCP**
(`mcp__retroterm__terminal_*`), never a hand-rolled TCP script.

## The build loop - do not reorder, do not skip

Each step has a **proof**. Every one of these has been skipped while the screen looked healthy,
and each time it produced a green build of the wrong thing.

| # | Step | Proof it actually happened |
|---|---|---|
| 0 | start the sync daemon once per session | it holds ONE link open all session and ends by itself |
| 1 | `python tools\planc-lint.py <source>` | seconds, on Windows, before anything crosses |
| 2 | stage into the daemon's **real** `--sync` folder | the daemon's log says `done, NNNNN byte(s)` |
| 3 | verify the push | `FILE-STATISTICS` on the machine returns the **same byte count** |
| 4 | compile (`@MODE <build>:MODE,,`) | roughly four minutes; the screen is not the evidence |
| 5 | **the gate** - `tools\planc-build.ps1 -PullOnly -Listing CHAT:LIST` | exits 1 on any real `*** ERROR`. **Never skip this** |
| 6 | install - `tools\rt-load.ps1 -Port <p> -Segment <n> -AndStart` | it checks the start address and the server's own replies |
| 7 | test | read the machine, not the screen |

**The listing is the only place a PLANC error survives.** The `0 DIAGNOSTICS` left on screen
belongs to the LINKER and sits happily under a compile that failed.

**Gate every module's listing, not just the one you edited.** A test suite has passed 139/139 on
top of ten `*** ERROR` lines.

## The tools that matter

| Command | What it is for |
|---|---|
| `python tools\planc-lint.py <src>` | the most valuable thing in this directory. Self-tests on every run |
| `tools\planc-build.ps1 -PullOnly -Listing <X:LIST>` | the build gate |
| `tools\rt-load.ps1 -Port <p> -Segment <n> -AndStart` | RT-load, then restore the machine's name and trunks |
| `tools\rt-load.ps1 -Port <p> -ChatSetupOnly` | just put the name and trunks back |
| `tools\rt-load.ps1 ... -ShowSteps` | print what would be typed, send nothing |
| `tools\lab-status.ps1` | is the lab up, and which .NET hosts are mine |
| `tools\ndterm.ps1 -Port <p> -Steps ...` | drive a terminal over one connection |
| `tools\run-all-tests.ps1` | the C# suites |

**`python`, not `python3`.** `python3` does not exist on this machine. Several documents in this
repo still say `python3`; they are wrong.

## The traps that cost the most

- **An RT-load orphans every joined client**, and gives the server an empty member table. A
  client with no seat shows a completely normal screen and silently receives nothing. The tell is
  a **missing echo** of your own line. Restart the clients after every load.
- **An RT program using XMSG must be RT-loaded again after every XMSG restart.** Restarting it is
  not enough.
- **`START-TRUNK` on a trunk that is already up knocks it down** for about a minute.
- **SINTRAN's idle timeout aborts the running program and logs the terminal out**, and the
  rendered screen still looks normal. Check for `--EXIT--` or `ABORTED BY SYSTEM` after any gap.
- **PLANC checks no array bounds**, accepts an **undeclared name** silently, and refuses an
  initial value on a local while the program still links and runs with the flag unset.
- **A search that came back empty is evidence about your pattern, not about the world.** This is
  the leading cause of wrong conclusions in this project - see the friction review. Before
  believing a zero result, run the same query against something known to be there.

## Processes

**Never kill anything you did not start.** Check `Win32_Process.CommandLine` first - most stray
`dotnet.exe` / `testhost.exe` / `MSBuild.exe` here belong to **RetroCore** or Unity. Killing one
breaks his work.

**Never hard-kill a runner that is talking to a machine** - it skips the `DISC` and leaves the
peer holding a half-open link, which is worse than the crash. Let its window end.

RetroTerm may be restarted (a named exception). RetroCore machines that are **down** may be
started; a **running** one may not be killed.

## Writing code here

No LINQ, no `foreach`, no FluentAssertions. Unit tests only - never a standalone test program.
Keep as many comments as possible; a comment is only removed when it is factually wrong.
Plain words - no jargon. ASCII only, no HTML escapes in comments. Never mention AI in a commit
message (a hook enforces it).
