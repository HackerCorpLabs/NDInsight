# SINTRAN III Boot, Initial Commands & Mode Files — Setup Guide

> **Purpose.** Explain how a SINTRAN III machine boots, how the INITIAL-COMMAND chain and the
> `HENT-MODE` / `LOAD-MODE` mode files fit together, and give a **working, automated** startup
> (XMSG + network links + TADADM + COSMOS Basic) so the machine comes up ready with no manual
> typing.
>
> **Provenance.** Statements marked `[manual]` are taken verbatim/adapted from
> *ND-30.003.7 EN SINTRAN III System Supervisor* (sections 3.2–3.3 and Appendix H).
> `[obs]` = observed in real extracted disk-image mode files (see
> [research/HDD-IMAGE-FINDINGS.md](research/HDD-IMAGE-FINDINGS.md)).
> `[your-config]` = tailored to this specific machine from the operator's current files and
> manual command session — **confirm the placeholder values before use.**

Related: [08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md](08-AUTOMATIC-BOOT-INITIAL-COMMANDS.md),
[06-STARTUP-AND-TERMINAL-CONFIG.md](06-STARTUP-AND-TERMINAL-CONFIG.md),
COSMOS Basic: [../Communication/COSMOS Basic/COSMOS-Basic-Install-Guide.md](../Communication/COSMOS%20Basic/COSMOS-Basic-Install-Guide.md).

> **Ready-to-use mode files** built from this guide (tailored to this machine) live in
> [mode-files/](mode-files/README.md): `HENT-MODE.MODE`, `LOAD-MODE.BATC`, `XMSG-START.MODE`,
> `DUMP-REENTRANT.MODE`. COSMOS boot wiring:
> [../Communication/COSMOS Basic/COS-BOOT-WIRING.md](../Communication/COSMOS%20Basic/COS-BOOT-WIRING.md).
> The sections below explain what those files do and why.

---

## 1. The two kinds of start `[manual]`

SINTRAN distinguishes **cold start** and **warm start**. Both end the same way — by running a
batch job called `LOAD-MODE` — but they differ in what precedes it.

### Cold start
A full reload of SINTRAN from disk (bootstrap / MACM → SINTRAN image). Everything that lives in
memory-resident segments must be rebuilt.

**Important: on a cold start the initial commands are NOT executed** `[operator-verified]`. The
freshly loaded image has no usable initial-command buffer, so nothing auto-runs. An **operator must
manually** get the system back on its feet:

1. Enter the main directory:  `@ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0`
2. Log in as SYSTEM:  `ESC` → `ENTER SYSTEM` → password.
3. Run the cold-start mode file by hand:  `@MODE (SYSTEM)HENT-MODE:MODE,,,`

`HENT-MODE:MODE` then does the actual cold-start work:

1. **Defines the initial commands** (`@INITIAL-COMMAND` / `@NEXT-INITIAL-COMMAND`) — these get
   stored inside the SINTRAN image and survive later **warm** starts. This is why subsequent warm
   starts *can* auto-run; the cold start is what populates the buffer in the first place.
2. Enters directories, defines the segment file, `INITIALIZE-BACKGROUND-PROGRAMS`, `RTENTER`.
3. Loads memory-resident drivers/subsystems reentrant (DMAC, XMSG, COSMOS `COS-HENT`, editors…).
4. Finishes with the same tail as a warm start: start batch processor 1 and append `LOAD-MODE`.

So cold start = **manual** (`ENTER-DIRECTORY`, log in, `@MODE HENT-MODE:MODE`); warm start =
**automatic** (SINTRAN runs the initial commands that HENT-MODE stored last time).

### Emulator persistence — which path you actually get `[operator-verified]`

Whether warm-start automation works depends on whether the run **mode** keeps the SINTRAN **image**
(and disk) between runs — this is a property of the mode, not the environment:

| Run mode | Session changes kept? | Boots as | Consequence |
|---|---|---|---|
| **Local binary emulator** | **Yes** | warm | Run the manual cold-start (`HENT-MODE`) **once**; it stores the initial commands into the persisted image, and every later launch auto-runs `LOAD-MODE`. Full automation. |
| **Web browser — persistent mode** | **Yes** | warm | Same as local binary: configure once, then it warm-boots automatically on later launches. |
| **Web browser — demo (ephemeral) mode** | **No** | **warm** (from a fixed pre-baked image) | Always warm-boots from the same baked image and discards session changes. It never cold-starts at runtime — so the image must be **set up correctly first**, and then every launch comes up automatically from that snapshot. |

**Key point:** in **all** modes the runtime boot is a **warm** start, so the initial commands +
`LOAD-MODE` do the automation. The only difference is where the **one-time cold-start setup**
happens and whether later changes stick:
- **Persistent modes** (local binary, persistent browser): do the cold-start (`HENT-MODE`) once
  live; it sticks, and you are done.
- **Ephemeral demo:** you cannot set it up at runtime (changes vanish). Instead **bake the
  fully-configured image once** — run the cold+warm setup in a persistent mode, then ship that
  snapshot as the demo image so it warm-boots ready every time.

### Warm start
The bootstrap copies the saved SINTRAN **image area** into the **memory area** and starts it;
the segment file (with all the reentrant dumps from the last cold start) is still intact. SINTRAN
then automatically executes the **initial commands** stored in the image. `[manual, 3.2.1–3.2.2]`

So: **reentrant/segment loading happens at cold start (HENT-MODE); starting the already-loaded
subsystems happens on every warm start (LOAD-MODE).** This is why COSMOS ships two files —
`COS-HENT-*` (cold) and `COS-START-*` (warm).

---

## 2. The INITIAL-COMMAND chain — the missing link on this machine `[manual]`

The initial commands are a small command buffer (**max 256 characters total** for all of them)
stored in the SINTRAN image. On a warm start SINTRAN runs them automatically. They are **defined**
by these commands (normally placed in `HENT-MODE:MODE`, but you can also type them live as SYSTEM):

```
@INITIAL-COMMAND      <command>        ← clears the buffer, defines command #1
@NEXT-INITIAL-COMMAND  <command>        ← appends the next command
@LIST-INITIAL-COMMANDS <output-file>    ← show what is currently defined
```

Notes `[manual, 3.2.2]`:
- The command after `@INITIAL-COMMAND` is **not** prefixed with `@`.
- `@INITIAL-COMMAND` **clears the whole buffer** — to change one entry you must retype the
  entire list.
- Keep it short (256-char limit); push everything else into `LOAD-MODE`.

### The canonical chain `[manual, 3.2.2 + Appendix H.3]`

```
ENTER-DIRECTORY <dir> <device> 0            ; enter the main/system directory
CONNECT-FILE SYSTEM-OUTPUT-1 105 W          ; open a log file for the batch output
CLOSE-FILE 105
SET-ERROR-DEVICE 1
BATCH 1                                      ; start batch processor 1
APPEND-BATCH 1 LOAD-MODE:BATC SYSTEM-OUTPUT-1  ; queue the warm-start job
```

### What is wrong on THIS machine `[your-config]`

Current initial-command list is only:

```
ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0
```

It enters the disk and **stops** — it never starts a batch processor and never appends
`LOAD-MODE`. That is exactly why, after boot, you land at a prompt and have to type
`SIN / START-X / … / SET-AVAIL / START-TADADM / …` by hand. Fix = extend the chain so it hands
off to `LOAD-MODE`.

**Corrected initial commands for this machine** (type these live as user SYSTEM once; they persist
in the image across warm starts, and are also written into `HENT-MODE:MODE` for cold starts):

```
@INITIAL-COMMAND ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0
@NEXT-INITIAL-COMMAND CONNECT-FILE SYSTEM-OUTPUT-1 105 W
@NEXT-INITIAL-COMMAND CLOSE-FILE 105
@NEXT-INITIAL-COMMAND SET-ERROR-DEVICE 1
@NEXT-INITIAL-COMMAND BATCH 1
@NEXT-INITIAL-COMMAND APPEND-BATCH 1 LOAD-MODE:BATC SYSTEM-OUTPUT-1
```

Verify with `@LIST-INITIAL-COMMANDS,` (trailing comma = output to terminal). Character count is
well under the 256 limit.

> **Recovery if a warm start ever runs with no initial commands** `[manual, 3.2.2]`: log in as
> SYSTEM (you will see `NO MAIN DIRECTORY OK`), then `@ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0`,
> `@LOGOUT`, log in again, `@BATCH`, `@APPEND-BATCH 1 LOAD-MODE:BATC SYS-OUTPUT-1`.

---

## 3. Mode file vs batch file — naming that matters

- A **mode file** (`:MODE`) is a script of `@`-commands run by `@MODE (user)file:MODE,,,`.
- A **batch file** (`:BATC`) is run by a batch processor. Its **first line must be
  `@ENTER SYSTEM,<password>,,<project>`** and it ends with the batch terminator. `[manual]`
- `LOAD-MODE` is appended to batch processor 1, so it must be a **batch job** — the manual always
  names it **`LOAD-MODE:BATC`**. Your current file is named `LOAD-MODE:MODE` but is *written* as a
  batch job (it starts with `@ENTER SYSTEM,...`). Rename/save it as `LOAD-MODE:BATC` so the
  `APPEND-BATCH` above finds it, or change the `APPEND-BATCH` line to match the name you keep.
  `[your-config]`

---

## 4. Your current `LOAD-MODE` — review `[your-config]`

Your file (annotated):

```
@ENTER SYSTEM,xxxxxx,,3200,,,        ; OK — batch job must start with ENTER SYSTEM
@BATCH                               ; starts a batch processor (see note)
@BATCH
@MODE XMSG-START:MODE,,,             ; starts XMSG — but see section 5: yours does almost nothing
@START-TADADM                       ; OK
@SET-AVAILABLE                      ; OK
@MAIL / @DIRECT-BROADCAST / @EXIT   ; OK — announces availability
@(UTIL)XMSG-COMMAND / LIST-ROUTING / EXIT
```

Problems:
1. **XMSG is started but the network is never configured** — no `DEF-REMOTE`, no `START-LINK`.
   That is why you must open XMSG-COMMAND by hand afterwards and type `DEF-REMOTE` / `START-LINK`.
   Move that configuration into `XMSG-START:MODE` (section 5).
2. **COSMOS Basic is never started** — there is no `@MODE (…)COS-START-…:MODE`. Add it (section 6).
3. `@BATCH` twice: batch processor 1 is already running this job; starting extra batch processors
   is only needed if you actually use batch numbers 2+. Keep only what you use. `[manual, H.2 uses
   a single `@BATCH 2` for one extra processor]`
4. `@SET-AVAILABLE` should come **after** all subsystems are up, not before COSMOS. Reorder.

---

## 5. A working `XMSG-START:MODE` `[your-config, modelled on your XMSG-STARTEX:MODE + manual]`

Your `XMSG-STARTEX:MODE` is the stock ND **example**, in which every configuration line is
apostrophe-commented (`'`), so it only starts XMSG and defines nothing. Below is an **active**
version that folds in exactly the commands you currently type by hand
(`DEF-REMOTE` D100/D102/D103, `START-LINK` 1360 and 1362, `LIST-ROUTING`).

Save as `(SYSTEM)XMSG-START:MODE`:

```
@CC ================================================================
@CC (SYSTEM)XMSG-START:MODE — start XMSG and bring up the network.
@CC ================================================================
@CC
@CC --- Start the XMSG kernel and wait for its segments to fix ---
@SINTRAN-SERVICE-PROGRAM
@STOP-XMSG
@EXIT
@HOLD 0 0
@HOLD 2 2
@SINTRAN-SERVICE-PROGRAM
@START-XMSG
@EXIT
@HOLD 0 0
@HOLD 3 2
@CC
@CC --- Configure names, routing and links ---
@XMSG-COMMAND
DEF-REMOTE,,D100 100
DEF-REMOTE,,D102 102
DEF-REMOTE,,D103 103
START-LINK,1360,,,-1,,
START-LINK,1362,,,-1,,
LIST-ROUTING-INFO
LIST-LINKS
EXIT
@CC ================================================================
```

Notes:
- `HOLD 0 0` / `HOLD n 2` = wait *n* seconds (2 = the time-unit code) `[obs, matches STARTEX]`.
  The two-stage stop→start with holds is straight from your STARTEX example.
- `@SINTRAN` / `@SIN` = `SINTRAN-SERVICE-PROGRAM`; `START-XMSG` / `STOP-XMSG` are its subcommands;
  `@EX` = `EXIT`. `@XMSG-COMMAND` = `@(UTILITY)XMSG-COMMAND` = `X-C:`. `[obs]`
- **XMSG-COMMAND verb aliases** (long form ⇄ the short form in your manual session) `[obs]`:
  | Short (manual session) | Long form | Purpose |
  |---|---|---|
  | `DEF-REMOTE,,<alias> <node>` | `DEFINE-REMOTE-NAME` | map an alias to a node number |
  | `DEF-SYS-ROU,,<dest> <via>` | `DEFINE-SYSTEM-ROUTE` | reach `<dest>` indirectly via neighbour `<via>` |
  | — | `DEFINE-NETWORK-CONNECTION <alias>,ENNS0,,0,0,0,0` | attach an alias to an **Ethernet** driver |
  | `START-LINK,<lu>,,,-1,,` | — | bring up an **HDLC** link on logical unit `<lu>` |
  | `SET-PRI` | `SET-PRIVILEGED` | make the command task privileged (needed to configure) |
  | — | `ENABLE-ROUTE-THROUGH` | let THIS node forward/relay traffic for others |
  | `LIST-LINK` / `LIST-ROUTING-INFO` / `LIST-NAMES` | — | inspect link / route / name state |
- `<alias>` vs `<node>`: `D100`/`D102`/`D103` are **aliases**; `100`/`102`/`103` are the **node
  numbers in decimal** (not octal). HDLC logical units `1360B`=HDLC1, `1362B`=HDLC2 **are** octal.
  `[your-config]`
- **HDLC vs Ethernet.** A neighbour reached over a serial **HDLC** line is brought up with
  `START-LINK,<lu>,...`. A neighbour reached over **Ethernet** (DLAN) is instead declared with
  `DEFINE-NETWORK-CONNECTION <alias>,ENNS0,,0,0,0,0` (`ENNS0` = the Ethernet driver) — no
  `START-LINK`. `[obs, from your DEFINE-ETH-DLAN.INCL]`
- If this node relays traffic for others (a routing node), add `ENABLE-ROUTE-THROUGH` after the
  links are up, and `DEFINE-SYSTEM-ROUTE,,<dest> <via>` for every non-neighbour. `[obs]`

---

## 6. Starting COSMOS Basic from `LOAD-MODE` `[your-config]`

COSMOS Basic E04 was installed to `PACK-ONE:COSMOS-BASIC` (see the COSMOS install guide). Two
things must be true for it to start:

1. The reentrant programs + segments must have been **dumped/loaded once** by
   `COS-HENT-E04:MODE`. This is cold-start work and it **persists in the segment file** — so run
   it once now (manually or via `HENT-MODE`), and thereafter warm starts only need `COS-START`.
2. **XMSG and TADADM must be running first** (LOAD-MODE order below guarantees this).

Add to `LOAD-MODE`, after XMSG is up:

```
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
```

`COS-START-E04:MODE` itself starts the file-transfer daemon, COSMOS spooling, the printer
definitions, and the file server (which runs `START-TADADM` internally). See the COSMOS guide for
the per-service breakdown and prerequisites (spooling configured, RFA option, TADs).

---

## 7. Complete, ordered `LOAD-MODE:BATC` — worked example `[your-config]`

> The **generic templates** (placeholder tokens + a substitution table) are in
> [mode-files/](mode-files/README.md). Note the templates split this into a thin batch **wrapper**
> [mode-files/LOAD-MODE.BATC](mode-files/LOAD-MODE.BATC) (login + `@MODE LOAD-MODE:MODE` + MAIL
> broadcast + terminator) and the real config [mode-files/LOAD-MODE.MODE](mode-files/LOAD-MODE.MODE)
> — so the password lives only in the `:BATC` and the config isn't duplicated. The single-file
> version below is the equivalent flattened out, to show the shape and order.


Replace `xxxxxx` with the real SYSTEM password and adjust project number `3200` if needed. This
merges: your directory entry, XMSG + links (section 5), TADADM, COSMOS Basic (section 6), then
availability + broadcast — in the correct order.

```
@ENTER SYSTEM,xxxxxx,,3200
@CC =================================================================
@CC (SYSTEM)LOAD-MODE:BATC — warm-start job (runs subsystems).
@CC =================================================================
@SET-UNAVAILABLE $THE COMPUTER IS BEING STARTED$
@CC
@CC --- Directories & floppy drives (main dir already entered by INITIAL-COMMAND) ---
@DEFINE-MASS-STORAGE-UNIT FLOPPY-DISC-1 0
@CC
@CC --- Extra batch processors (only if you use batch 2+, else delete) ---
@CC @BATCH 2
@CC
@CC --- Start XMSG and bring up the network links ---
@MODE (SYSTEM)XMSG-START:MODE,,,
@CC
@CC --- Start COSMOS Basic Module (needs XMSG up; starts TADADM internally) ---
@MODE (PACK-ONE:COSMOS-BASIC)COS-START-E04:MODE,,
@CC
@CC --- If COSMOS file server did not already start it, ensure TADADM is running ---
@CC @START-TADADM
@CC
@CC --- Make the system available and announce it ---
@SET-AVAILABLE
@OPERATOR $ *** SYSTEM IS AVAILABLE *** $
@MAIL
@DIRECT-BROADCAST
$$
SYSTEM IS AVAILABLE
$$
&
EXIT
&&
```

> Order rationale `[manual, H.2]`: XMSG before COSMOS (COSMOS depends on it); `SET-AVAILABLE` last,
> after everything is up; broadcast the availability message at the very end. `COS-START-E04:MODE`
> runs `START-TADADM` itself, so the standalone `@START-TADADM` is commented out — uncomment it
> only if you are **not** installing the COSMOS file-server part.

---

## 8. `HENT-MODE:MODE` for cold start — skeleton for this machine `[manual H.3 + your-config]`

Run this **by hand** in every **cold** start — the operator must first `@ENTER-DIRECTORY`, log in
as SYSTEM, then `@MODE (SYSTEM)HENT-MODE:MODE,,,` (the initial commands do **not** run on a cold
start; see section 1). It defines the initial commands (section 2), rebuilds the segment file,
loads reentrant subsystems, then hands off to `LOAD-MODE`. Adapt device/segment-file names to your
hardware.

```
@CC =================================================================
@CC (SYSTEM)HENT-MODE:MODE — cold start.
@CC =================================================================
@SET-UNAVAILABLE $THE COMPUTER IS BEING COLD-STARTED$
@CC --- Define the initial commands (persist in the image) ---
@INITIAL-COMMAND ENTER-DIRECTORY PACK-ONE DISC-75MB-1 0
@NEXT-INITIAL-COMMAND CONNECT-FILE SYSTEM-OUTPUT-1 105 W
@NEXT-INITIAL-COMMAND CLOSE-FILE 105
@NEXT-INITIAL-COMMAND SET-ERROR-DEVICE 1
@NEXT-INITIAL-COMMAND BATCH 1
@NEXT-INITIAL-COMMAND APPEND-BATCH 1 LOAD-MODE:BATC SYSTEM-OUTPUT-1
@CC --- Define segment file, background programs, RT ---
@SINTRAN-SERVICE-PROGRAM
@DEFINE-SEGMENT-FILE Y Y 0 SEGFILE0:DATA
@EXIT
@INITIALIZE-BACKGROUND-PROGRAMS
@RTENTER
@CC --- Load the DMA/comms driver into its segment (name/segment per your system) ---
@RT-LOADER
READ-BINARY (BPUN-FILES)DMAC-1915F:BPUN 7
YES
END-LOAD
EXIT
@CC --- Make editors/compilers reentrant (see section 12 for addresses) ---
@MODE (UTILITY)DUMP-REENTRANT:MODE,,,
@CC --- Initialise MAIL ---
@MAIL
@INITIALIZE 10
@RUN-MAIL
@EXIT
@CC --- Load XMSG reentrant (COSMOS needs the inter-system version) ---
@MODE (UTILITY)XMSG-LOAD:MODE,,,
@CC --- Load COSMOS Basic reentrant/segments (cold-start half) ---
@MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,
@CC --- Hand off to the warm-start job ---
@CONNECT-FILE SYSTEM-OUTPUT-1 105 W
@CLOSE-FILE 105
@SET-ERROR-DEVICE 1
@BATCH 1
@APPEND-BATCH 1 LOAD-MODE:BATC SYSTEM-OUTPUT-1
```

> `[verify before use]` The DMAC binary name/segment (`DMAC-1915F:BPUN 7`), the segment-file name
> (`SEGFILE0:DATA`), and whether `XMSG-LOAD:MODE` exists on your disk under `(UTILITY)` are
> **system-specific**. Confirm against your actual files (`@LIST-FILES (UTILITY)XMSG*` etc.)
> before running. The COSMOS line assumes the E04 install from the companion guide.

---

## 9. Bring-up order (one-time, to get from "typing by hand" to "automated")

1. **Load COSMOS once** (cold-start half), if not already done:
   `@MODE (PACK-ONE:COSMOS-BASIC)COS-HENT-E04:MODE,,`
2. **Create/save** `(SYSTEM)XMSG-START:MODE` (section 5) and `(SYSTEM)LOAD-MODE:BATC` (section 7).
3. **Define the initial commands** (section 2) live as SYSTEM, and verify with
   `@LIST-INITIAL-COMMANDS,`.
4. **Test `LOAD-MODE` without rebooting**: `@BATCH 1` then
   `@APPEND-BATCH 1 LOAD-MODE:BATC SYSTEM-OUTPUT-1`; watch the job, read `SYSTEM-OUTPUT-1:SYMB` if
   it aborts.
5. **Save `HENT-MODE:MODE`** (section 8) for the next cold start.
6. Next **warm** start should now come up fully automatically. A **cold** start still needs the
   manual operator steps (`ENTER-DIRECTORY`, log in, `@MODE HENT-MODE:MODE`) — that is by design;
   HENT-MODE re-populates the initial-command buffer so warm starts stay automatic afterwards.

---

## 9a. Manual test workflow (iterate before wiring the auto path)

Because the config lives in `LOAD-MODE:MODE` (not the batch wrapper), you can exercise the whole
warm-start by hand — no batch, no password:

1. Log in as SYSTEM.
2. Run the config directly:
   ```
   @MODE (SYSTEM)LOAD-MODE:MODE,,,
   ```
   No `@ENTER`/password needed (it runs in your logged-in context). The 'system available' MAIL
   broadcast does **not** fire — that is only in the `:BATC` wrapper.
3. Verify the network:
   ```
   @(UTILITY)XMSG-COMMAND
   LIST-LINKS,,,
   LIST-ROUTING-INFO,,,
   EXIT
   ```
4. Edit `XMSG-START:MODE` / `LOAD-MODE:MODE`, repeat step 2.

Re-run-safe: `XMSG-START` does `STOP-XMSG`→`START-XMSG` and `COS-START` aborts/restarts its
daemons, so re-running re-applies rather than stacking up. Note `LOAD-MODE:MODE` ends with
`@SET-AVAILABLE`. Only once the manual run is clean, save the `:BATC` wrapper + define the initial
commands (section 2) and test the full automatic warm-start path.

---

## 10. Things to confirm for THIS machine (not assumed)

- SYSTEM password and project number (`3200`) in `LOAD-MODE`.
- Node aliases/numbers: alias `D100` = node `100`, `D102` = `102`, `D103` = `103` (node numbers
  are **decimal**). Is that the full neighbour set? (taken from your manual session)
- HDLC logical units `1360B` / `1362B` — both links wanted at every boot?
- Whether any neighbour needs `DEF-SYS-ROU` (multi-hop routing) rather than a direct link.
- Which COSMOS parts you installed (file server present ⇒ TADADM handled by COSMOS; otherwise keep
  the explicit `@START-TADADM`).
- DMAC binary name/segment and segment-file name for the cold-start `HENT-MODE`.

---

## 11. Scaling XMSG config with include files (`.INCL`) `[obs, from D:\ND\xmsg]`

For a large network you do **not** inline dozens of `DEFINE-REMOTE-NAME` / `DEFINE-SYSTEM-ROUTE`
lines in `XMSG-START:MODE`. Instead factor them into **include files** and pull them in from
*inside* XMSG-COMMAND with its own `MODE` subcommand:

```
@(UTILITY)XMSG-COMMAND
SET-PRIVILEGED
MODE (UTILITY)DEFINE-REMOTE:INCL,1        ; all alias→node definitions
MODE (UTILITY)DEFINE-ETH-DLAN:INCL,1      ; Ethernet DLAN neighbours
MODE (UTILITY)DEF-ROUT-<node>:INCL,1      ; system routes for this node
START-LINK,1360,,,-1,,                    ; any HDLC links
ENABLE-ROUTE-THROUGH
LIST-ROUTING-INFO
EXIT
```

Include-file conventions `[obs]`:
- The `,1` after the name is the **file version** (`;1`).
- Inside an `.INCL` (which runs in the XMSG-COMMAND context, not SINTRAN), a leading **`'`**
  (apostrophe) comments out a line, and **`%`** starts a trailing comment. (In ordinary SINTRAN
  mode files the comment command is `@CC`.)
- Typical split seen in a real deployment: `DEFINE-REMOTE:INCL` (hundreds of alias→node lines),
  `DEFINE-ETH-DLAN:INCL` (`DEFINE-NETWORK-CONNECTION …,ENNS0,…`), and a per-node
  `DEF-ROUT-<node>:INCL` (`DEFINE-SYSTEM-ROUTE,,<dest> <via>` for every non-neighbour, plus
  `DEFINE-REMOTE,,REGION <node>` for the local region gateway).

> These `.INCL` files can be generated by the ND network-management tool (`XNM0001L`) or hand-kept.
> They belong to `(UTILITY)` alongside `XMSG-START:MODE`.

---

## 12. Making subsystems reentrant — `DUMP-REENTRANT` `[manual 3.x + PD sheets]`

Editors/compilers (QED, PED, MAC, FMAC, NPL, …) are made shareable by **dumping them reentrant**
once per **cold** start, in a mode file conventionally named `(UTILITY)DUMP-REENTRANT:MODE` and
called from `HENT-MODE`. The reentrant segments live in the segment file and survive warm starts.

### Command forms `[manual]`
```
@DUMP-REENTRANT <name> <start-address> <restart-address> <file> [<segment name/number>]
@DUMP-PROGRAM-REENTRANT <name> <file> [<segment>]     ; for :PROG files (e.g. COSMOS programs)
```
- `<start-address>` / `<restart-address>` are **octal** and come from the product's **PD sheet**.
  `@CONTINUE` resumes a subsystem at its restart address. Negative values appear as octal
  16-bit: `-1` = `177777`, `-3` = `177775`.
- Empty addresses (`,,`) use the file's own defaults — this is what the manual shows for
  ASSEMBLER-500: `@DUMP-REENTRANT ASSEMBLER,,(BPUN-FILES)ASSEMBLER-500:BPUN`.

### Authoritative addresses for the standard SUBSYSTEM PACKAGE II `[PD sheet 210400B]`
See [SUBSYSTEM/ND0117.md](SUBSYSTEM/ND0117.md):

| Name | Start Restart | BPUN file |
|---|---|---|
| DITAP | `70 70` | (BPUN-FILES)DITAP-1880D |
| F32-FMAC-1920C | `-1 -3` | (BPUN-FILES)F32-FMAC-1920C |
| F32-MAC-1626C | `-1 -3` | (BPUN-FILES)F32-MAC-1626C |
| FMAC-1408D | `-1 -3` | (BPUN-FILES)FMAC-1408D |
| MAC-1415C | `-1 -3` | (BPUN-FILES)MAC-1415C |
| NPL | `0 1` | (BPUN-FILES)NPL-1896D |
| QED | `0 1` | (BPUN-FILES)QED-1644L |

### Review of a real hand-built `DUMP-REENTRANT:MODE` `[operator file, 2023-10-03]`

The operator's file is **mostly correct** — every entry that has a PD sheet uses the right
addresses (`DITAP 70 70`, `FMAC/MAC 177777 177775` = `-1 -3`, `NPL/QED 0 1`). Issues to fix:

1. **Missing `@` on the first three lines.** In a mode/batch file every command line must start
   with `@`. The top three lines (`DUMP-REE NRL …` ×2 and `DUMP-REENTRANT BACKUP-SYSTEM-B …`)
   have no `@` and sit *above* the `@CC DUMP-REENTRANT FOR ND 03.10.2023` header — they look like
   a stray fragment. Delete them or give them `@` and fold into the list.
2. **`NRL` dumped three times**, from two different homes (`(PACK-ONE:SYSTEM)NRL-1935I` twice at
   the top, `(PACK-ONE:BPUN-FILES)NRL-1935I` once in the list). Keep **one** — decide which
   directory actually holds `NRL-1935I:BPUN` and drop the duplicates.
3. **`ASSEMBLER-500 0 1` is unverified** — this is the operator's own "frankenstein" entry from a
   copied BPUN. The manual dumps ASSEMBLER-500 with **empty** addresses (`,,`), not `0 1`. Use the
   real PD-sheet values, or `,,` for defaults, and confirm the BPUN is an ND-100 reentrant image.
4. **`FTN 0 1`** — verify against the FORTRAN PD sheet; compilers sometimes need non-default
   addresses. `[unverified]`
5. Entries with no PD sheet to hand (`PED`, `GPM`, `FIL-EXTR`, `BRF`, `LOOK-FILE`, `PERFORM-E`,
   `NRL`, `BACKUP-SYSTEM-B`) use `0 1` / `0 0`; those are plausible defaults but should be
   cross-checked against each product's PD sheet before being trusted. `[verify]`
6. The line-wrap breaks in the paste (`…FIL-EXTR-2221B:BPU` ↵ `N;1`, and
   `PERFORM-E:BPUN;1@DUMP-REE FTN`) must be single physical lines in the real file — confirm they
   are not literally broken.

> `DUMP-REENTRANT` is generic OS setup and belongs to the cold-start `HENT-MODE` chain (section 8),
> **not** to COSMOS. COSMOS uses `DUMP-PROGRAM-REENTRANT` for its `:PROG` files inside
> `COS-HENT-E04:MODE` — see the COSMOS guide.

---

## Sources
- *ND-30.003.7 EN SINTRAN III System Supervisor* — sections 3.2 (initial commands / warm start),
  3.3 (cold start), Appendix H.1–H.3 (STOP-MODE, `LOAD-MODE:BATC`, `HENT-MODE:MODE` examples).
  Path: [../../Operations/SINTRAN/ND-30.003.007 EN SINTRAN III System Supervisor.md](../../Operations/SINTRAN/ND-30.003.007%20EN%20SINTRAN%20III%20System%20Supervisor.md)
- Real extracted mode files: [research/HDD-IMAGE-FINDINGS.md](research/HDD-IMAGE-FINDINGS.md).
- XMSG commands: [../../SINTRAN/XMSG/DOC/XMSG-COMMAND-REFERENCE.md](../../SINTRAN/XMSG/DOC/XMSG-COMMAND-REFERENCE.md).
- `DUMP-REENTRANT` start/restart addresses: SUBSYSTEM PACKAGE II PD sheet
  [SUBSYSTEM/ND0117.md](SUBSYSTEM/ND0117.md) (210400B).
- Operator's real files in `D:\ND\xmsg`: `XMSG-START.MODE`, `DEFINE-REMOTE.INCL`,
  `DEF-ROUT-D104.INCL`, `DEFINE-ETH-DLAN.INCL`, `COSMOS-HENT.MODE`, `LOAD-MODE.MODE`,
  `div-commands.txt` (working XMSG-COMMAND session), and the hand-built `DUMP-REENTRANT:MODE`.
- Operator's current `LOAD-MODE`, `XMSG-STARTEX:MODE`, and manual startup session (2026-07-04).
