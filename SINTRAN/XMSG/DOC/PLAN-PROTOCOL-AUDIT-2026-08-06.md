# Planned audit: what we do not know, and whether the code says it well

**Status: NOT STARTED.** Four analysis agents were launched on 2026-08-06 and all four died
immediately on a weekly usage limit (resets 2026-08-08 20:00 Europe/Oslo). None produced any
output. This file exists so the work can be relaunched without re-deriving the briefs.

Nothing here is a finding. It is a plan.

---

## Why this audit

The COSMOS FA file server now lists, stats, opens, reads, writes, creates and deletes files
against a real D100. Getting there took seven defects in one day, and **every one of them was
invisible to a green test suite**. The pattern was always the same: something was inferred
rather than measured, and the inference held right up until a real machine disagreed.

So the next useful thing is not another feature. It is knowing exactly where the remaining
inferences are.

---

## The four sweeps

Relaunch these as four parallel read-only agents. Each is independent.

### 1. Protocol unknowns - what we do not know

Sweep BOTH `SRC\` and `DOC\` case-insensitively for the honesty markers this codebase already
uses: `UNKNOWN`, `UNVERIFIED`, `INFERRED`, `ASSUMPTION`, `never been captured`, `meaning
UNKNOWN`, `purpose unverified`, `not understood`, `opaque`, `residue`, `guess`, `fitted`,
`SUPERSEDED`. Also fields named `Unknown`, `Reserved`, `Pad`, `Opaque`.

For each: full path + line, what is unknown in one sentence, **what it blocks**, how it could be
closed (which capture, which live command, which manual), and whether it needs the machine.

Group and rank:
 - A. Operations we cannot serve at all (layout never captured)
 - B. Fields we can echo but not generate
 - C. Bits whose meaning is unknown (name the byte and bit)
 - D. State machines with unproven transitions
 - E. Constants observed once rather than derived
 - F. Cosmetic - say so, these are low value

Context so solved things are not re-reported as open: header word 6 IS a ones-complement
checksum over words 0-5 (solved); the FA server is complete and live-verified; the seed/channel
model in `XmsgEnvelope` is SUPERSEDED and anything still depending on it IS a gap.

### 2. Enums, [Flags] and magic numbers

Every bare hex or decimal on a frame build/parse path that should be a named enum member or
constant. Search `Enums\` first - `XmsgFunction`, `XmsgOption`, `XmsgError`, `XroutError`,
`FaOperation`, `FaSpecialFunction`, `FaMessageType`, `FaServerStatus`, `SintranPacketSubtype`,
`QformClass` already exist. Also: enums that should be `[Flags]` but are not, `[Flags]` members
not written `1 << n`, the same literal declared in two files, missing enum members the captures
show exist, and any LINQ or `foreach` on a hot path.

**Do not flag literals in tests.** A test that states an expected wire byte is doing its job -
pinning the wire independently of our own constants. That is the whole reason the byte-level
tests catch things the behavioural ones miss.

Plus the general house rules: explicit types over `var`, named delegates for events (never bare
`Action`/`Func`, never `EventArgs`), member and access-modifier ordering, `_field` / `s_static`
naming, `using` order, block-scoped namespaces. And flag loudly any `#pragma warning disable` or
`<NoWarn>` for a CS diagnostic - suppressing a warning to green the build is a regression here,
not a fix.

### 3. Source material inventory - where the answers already are

Before carving another byte off a live machine, know which document already holds the answer.

Survey, verifying each path exists first:
 - `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG` - any `*.INCL` / `*.SYMB` / `*.MODE` / `*.PROG` /
   `*.BPUN` / `*.NRF`. The official constants are in
   `DOC\COSMOS-RE\ENNS0-Startup-RE-2026-07-23\xmsg-L-binaries\`; check what else sits beside them.
 - `E:\Dev\Ronny\NDInsight\Reference-Manuals` - which cover XMSG, COSMOS, TAD, file access, XROUT?
 - `E:\Dev\Ronny\NDIX-C` - `kernel\MASTER\if\xmsg.h`, `baseline\bin\cps\`, other headers.
 - `E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE`
 - `E:\Dev\Ronny\mirror-sintran-com` - folder level only, do not crawl 7.3 GB.
 - `F:\ND` if reachable.

For each: what kind of question it can answer, whether the repo already cites it (grep `DOC\`
for the filename), and likely yield HIGH/MEDIUM/LOW. Highest-value unmined kinds are ND's own
`.INCL`/`.SYMB` files (authoritative, beat every reconstruction) and the COSMOS server binaries.

End with: "if you mine only three things, mine these, because they answer X".

### 4. Architecture and duplication

Anchor on a real case: `TadTerminalResponder.AssembleDataFrame` is a hand-copy of
`TadSession.AssembleDataFrame` - its own comment admits it - and because of that it carried an
identical header-checksum bug that had to be fixed twice on 2026-08-06. Find the rest of that
pattern.

Compare the frame builders: `Xmsg.Node\Tad\TadSession.cs`, `TadTerminalResponder.cs`,
`TadConnectClient.cs`, `Xmsg.Node\Services\XmsgServerHost.cs`,
`Xmsg.Protocol\Builders\XmsgFrameBuilder.cs`, `Xmsg.Protocol\ListRouting\*.cs`,
`Xmsg.Protocol\Packet\XmsgPacketBuilder.cs`. Also parallel parsers, constants declared twice,
layering violations (`Xmsg.Protocol` must not know about links or sockets), dead public API.

For each duplication say **whether the copies' XML documentation has diverged too**. Two copies
whose comments disagree are worse than two whose code disagrees - the comment is what the next
reader trusts.

Say what is already well factored, too. A report with only faults gives no sense of proportion.

---

## Two scope corrections for any agent

 - This project uses **xUnit**, not NUnit. Do not report `[Fact]` / `Assert.Equal` as violations.
 - Rules about CPU dispatch tables, `CpuState`, `MachineBuilder`, SourceLink and NuGet package
   layout belong to the RetroCore repository and do NOT apply here.

## One judgement call, stated so it is not "corrected" later

A long `<remarks>` is not automatically a defect. Several of the big ones here record a
hard-won measurement - the fragment rules, the counter sequence, the `0xF0` pad - in the one
place a reader will actually look. Moving those to `DOC\` would be a regression. Flag a remarks
block only when it has become a memory map or a full wire table that already exists in `DOC\`.

---

## Known open questions this audit should pick up

These are already tracked and should appear in sweep 1:

 - **The post-close XEIMA.** D100 answers our FA Close with `NetworkError` `Flags2 0xFFED` =
   -19 = XEIMA (invalid magic). Reproduced 2026-08-06. NOT the conversation number - that was
   fixed and verified in `8969943`. Now tagged `proto=Routing`. Everything user-visible
   completes, so it has never blocked anything. Compare our close frame against the captured
   teardown in `FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md` section 8 field by field, INCLUDING
   the sub-header endpoints, not just the eight-byte body.
 - **The first-connect stall.** Characterised, not proven. See the XENSE memory. Workaround:
   connect twice, or restart XMSG on D100.
 - **The DeleteFile name field's trailing byte.** Two live samples give name + `0x27` + one more
   byte that VARIES (`T`, then `W`) and is not word padding - it makes the field length odd both
   times. Meaning unknown; nothing depends on it.
 - **The data-message counter gap.** Our replies and data messages now share one counter, which
   reproduces the capture. Whether anything READS that byte is still unobserved.
