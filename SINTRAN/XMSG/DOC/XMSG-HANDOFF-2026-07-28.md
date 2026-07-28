# XMSG handoff (2026-07-28)

**START HERE.** Supersedes [XMSG-HANDOFF-2026-07-27.md](XMSG-HANDOFF-2026-07-27.md), whose
"the capture surface is exhausted" conclusion was wrong - a purpose-built SINTRAN K image
opened it back up and produced three findings in one day.

**Status: both COSMOS file servers' opening letters are decoded, and we know what a
successful answer looks like. The single remaining blocker is that nothing has ever
answered one.**

---

## 1. What changed today

Three things, in the order they were found.

**`*XFTRA` and `*FA-SERVER` are decoded** - the first COSMOS file-server traffic ever read
here. They do NOT use the same convention as each other:

| | `*XFTRA` (transfer) | `*FA-SERVER` (access) |
|---|---|---|
| application data | tagged XROUT parameters 8-13 | RAW bytes after the declared length |
| writes per message | one | **two** - the second APPENDS (displacement -1) |
| per-request counter | none (a repeat is byte-identical) | one trailer byte steps by 2 |

**The `*XFTRA` letter was then read field by field** by driving the same transfer six times
on the K image changing exactly one input per run. p2 = remote system, p12 = remote user,
p13 = password, p8 = destination spec. **The source file is not in the request at all.**
p9 `"SYMB"` is NOT the destination type - it survived both a `:BATC` and a `:SYMB`
destination - so its meaning is open again, as are p10 (1024) and p11 (2).

**The successful answer turned out to be in the April pcaps already.** `connect-to` sends
the same letter shape to `*TADADM` and gets accepted:

| Outcome | Service byte | Body | From |
|---|---|---|---|
| Accepted | left as sent | REPLACED by `01 02 0000 02 02 000A` | the SERVER's own port |
| Refused | overwritten with the error | whole original returned | the LOCAL XROUT |

Which also means **an `XRNRO` never crosses a wire** - it is generated locally and handed
back through MON 200. That is why one machine could never finish this job.

Docs: [XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md),
[XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md](XMSG-XFTRA-CONTROLLED-VARIATION-2026-07-28.md),
[XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md),
[XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md](XMSG-XSLET-ACCEPT-VS-XRNRO-2026-07-28.md).

## 2. The one blocker

**Nothing has answered a file-server letter.** Every capture ends at the refusal, so
everything past the opening letter is unknown for both servers, and the constants
(`p9`/`p10`/`p11` on transfer, `07E2`/`A200 FF92` on access) cannot be named - client-side
variation is exhausted, proven by the six-run experiment.

Two routes, and they are no longer mutually exclusive:

1. **Two real machines.** Multiple bootable K images capturing traffic between them. This is
   the faithful answer and it is what is being built now.
2. **A C# responder on node 102.** Cheaper, and the only route that gives *controlled*
   variation on the REPLY - vary one field in what we send back and watch the client. The
   envelope no longer needs guessing: the accept above is verified.

The Ghidra carve makes route 2 much less speculative than it was. All four COSMOS programs
are decoded in [COSMOS-RE/](COSMOS-RE/README.md), including `cos-fa-serv-e04` with its 13
operations and reply builders (`fa_build_full_entry_reply`,
`fa_build_typed_reply_dispatch`). Read those before writing a responder - then let a real
client tell you where the carve was wrong.

## 3. Which image, and why it matters

| Image | Use |
|---|---|
| `D:\BIGDISK0-L.IMG` | default for every harness test |
| `D:\BIGDISK0-K.IMG` | tests whose name ends `_OnK` |

SINTRAN gates the COSMOS File User at **revision F** and only E media exists here, so remote
file ACCESS cannot run on L or M at all. K's documented minimum is D, and the K image runs
the whole COSMOS product set from its boot batch job.

## 4. Traps this session added

- **A console message is not evidence about the wire.** Transfer failed instantly with an
  XROUT status; access sat ~56 s and said "NO ANSWER FROM REMOTE SYSTEM". That read as
  access getting further. Both got the IDENTICAL `XRNRO` - the difference was entirely
  client-side retry.
- **A client can fail LOCALLY and look like a network failure.** `TRANSFER-FILE` opens the
  SOURCE first, so a file missing from that pack produces a console error and emits no
  request at all. Four of five runs were lost this way before anyone noticed. Pick arguments
  from a live `LIST-FILES` on the pack under test, and confirm one `XFWRI` per attempt.
- **The per-capture `.md` files next to each `.pcapng` truncate payloads to 16 bytes**, and
  their embedded LAPB trace is truncated too. The `*TADADM` accept is invisible in both.
  Only `SRC\pcap-decode-report.txt` carries every frame in both directions with full hex.

## 5. Still open from before

- `XSGMG` (71) uncaptured. No XMSG-COMMAND command issues it and the raw-builder route is a
  dead end. Remaining route: Ghidra on `XMSG-COMMAND:PROG`'s generic MON 200 wrapper, or the
  ENNS0 network-server path. See
  [XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md](XMSG-XSGIN-NAME-LOOKUP-CAPTURED-2026-07-27.md).
- The enum audit: four wire-byte categories still decode as bare hex. See
  [XMSG-HANDOFF-2026-07-27.md](XMSG-HANDOFF-2026-07-27.md) section 5.

## 6. The library

Unchanged in shape - write clients and servers against `Xmsg.Api`, never raw frames;
`Xmsg.Chat` is the worked example. **347 tests green.** Build/test:
`dotnet test SRC\Xmsg.Protocol.slnx -c Release`.
