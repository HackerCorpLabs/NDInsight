# Priority plan: the COSMOS FA file server, from 2026-08-06

**This is a forward plan.** It says what is NEXT, in order. It is not a status report - for what
already landed and why, see the commit history and
[HANDOFF-FA-FILE-SERVER-2026-08-05.md](HANDOFF-FA-FILE-SERVER-2026-08-05.md).

**Baseline, measured 2026-08-06:** build clean (0 errors, 103 warnings), **634 tests pass**, branch
`5000x`, head `8969943`.

---

## The one-line version

Everything about **reading and writing a file is verified against captures only**. No real client
has ever read a file from us. Closing that gap is worth more than any new feature, so it is P1.

---

## P1 - Prove the file path against a real machine

**Why first.** We have built read, write, fragmentation, reassembly and set-length entirely from
captures. The listing is the only thing a real client has ever exercised. Every hour spent adding
operations on top of an unverified base multiplies the eventual debugging.

**Needs D100.** All three items are one live session. Read
[the terminal rule](#traps-that-will-cost-you-a-session) before starting.

| # | Do | Done when |
| --- | --- | --- |
| 1.1 | Capture the runner's **stdout** to a file, not just the log file | `spec-capture.log` exists and holds `[fa] the request names ...` lines |
| 1.2 | `FILE-STATISTICS d103(system).README:TXT` from D100 | The log shows the filespec that ACTUALLY arrived in the 62-byte spec block |
| 1.3 | Re-verify the close fix (`8969943`) | The teardown carries the client's conversation number and draws no `XEIMA` |
| 1.4 | Have D100 **read a file** from us | A real client receives file content - the first time ever |

### 1.1-1.2 The spec-block miss

Live on 2026-08-05, `FILE-STATISTICS` on a named file returned only `END OF FILE` - our matching
found nothing. The log for that run was overwritten and the bytes are gone.

**The diagnostic already exists.** `FaServer.cs:1595` logs
`[fa] the request names '<spec>': N of M file(s) match`, and `Xmsg.Live.Runner/Program.cs:401`
wires `server.Log` to `Console`. It goes to stdout, which is why it was lost. So:

```powershell
dotnet run --project SRC\Xmsg.Live.Runner 2>&1 | Tee-Object -FilePath spec-capture.log
```

**Do not fix this before capturing it.** Reading the parser shows two shapes that would miss - a
leading dot (`.README:TXT` parses to a name of `.README`) and a device prefix
(`D103(SYSTEM)README:TXT`). Which one arrives is **UNVERIFIED**, and guessing between them is how
this project has lost days before.

### 1.3 The close

The fix is in and unit-tested (`TheClosingCloseEchoesTheClientConversationNumber`), but the live
retry never completed: the D100 terminal stopped echoing after repeated reconnects and each attempt
stalled a step earlier. That is consistent with a wedged line, **not** with the fix. It needs a
clean machine to confirm.

### 1.4 The first real read

This is the real prize. If a live read works, the 1032-byte data messages, the counter and token
rule, the fragment split and the reassembler are all confirmed at once.

---

## P2 - Finish the operation set - DONE 2026-08-06

~~`FaServer` handles neither `CreateFile` nor `DeleteFile`, so both are refused.~~

Both are dispatched. `DeleteFile` (`0x000B`) has been driven live and deletes a real file in the
served folder. `CreateFile` (`0x000A`) is served but has NOT been driven live - `COPY-FILE` turned
out not to send it, opening with the quotes kept in the name instead - so no client we have
exercises the path. That is what is left of this phase, and it is tracked as its own item.

 - `UnsupportedOperation_IsRefusedRatherThanIgnored` no longer needs picking. After being rewritten
   twice as its chosen operation became supported, it now uses `0x7FFF` - deliberately not a member
   of `FaOperation` at all, so implementing a real operation can never invalidate it again.
 - `FolderFileStore` gained `SetLength`, `Create` and `Delete`.

---

## P3 - Hygiene, once the protocol work is settled

### 3.1 The `CS0618` sweep - bigger than previously recorded

The 2026-08-05 handoff said the server host's uses were gone and only a few remained. **That is
stale.** A full rebuild on 2026-08-06 reports `CS0618` across **29 files**, `XmsgServerHost.cs`
among them. They are the `ProtocolId` / `Counter` compatibility views over `Checksum`, plus
`XmsgFrame.ControlService`.

This is a real sweep, not a tidy-up. Treat it as its own commit and do not mix it with protocol work.

### 3.2 The LAPB spec still says 312

`LapbLayer.MaxInformationLength` is now a derived **622**. Two documents still say 312 and disagree
with the code deliberately:

 - [LAPB-REQUIREMENTS.md](LAPB-REQUIREMENTS.md) requirement A5
 - the authoritative spec in the X25Emulator repo, WSL `~/repos/os/x25emu/docs/lapb-nd-spec.md`

The measurement that justifies 622 is in commit `86eab68` and pinned by
`LapbInformationLengthTests`: over 3673 recorded information frames the largest is 622, and 452
exceed 312. Update the spec to match the wire.

---

## P4 - Optional, only if someone returns to the NDIX tree

[NDIX-XMSG-CROSS-CHECK-2026-08-05.md](NDIX-XMSG-CROSS-CHECK-2026-08-05.md) section 6 lists claims
that are **second-hand** and were never re-read line by line. The most useful is the `*XFTRA`
fragment framing - request codes 65 read / 66 write / 67 close, writes always 1030 bytes with no
short final fragment, and the true length carried as a 32-bit max byte pointer in the close.

If it holds up, it describes the read/write ladder in wire order and can be compared against our FA
paths. Verify before acting on it.

---

## Deliberately NOT doing

Each of these is a considered decision, not an oversight. Do not "fix" them without a reason.

 - **`PACK-ONE` / `SYSTEM` into `topology.json`.** The code says to move them "once there is a reason
   to vary it". There still isn't one.
 - **Merging the data-message counter with the reply counter.** Ours steps by two between deliveries
   where the capture steps by three. The gap is consistent with the reply drawing from the same
   space, but nothing has been observed to READ that byte, and fitting a shared counter to make one
   capture's spacing come out right is exactly the kind of guess that has cost days here. If a client
   ever objects, that is the fix.
 - **The three unmeasured codes in `FaServerStatus`.** Ours, not observed. Leave them until a capture
   shows one.
 - **`ResyncAcceptDown`.** Listed as dead code on an old cleanup list. It is not - `XmsgNode` calls
   it on the XENSE reject path. Deleting it breaks a recovery route.

---

## Traps that will cost you a session

 - **ONE terminal connection to D100.** Reconnecting mid-program wedges the line. Six reconnects on
   2026-08-05 left it not echoing at all and made the close fix impossible to verify.
 - **Stop the runner before building.** A running runner holds the DLLs, the build silently fails to
   replace them, and the test run reports green against a STALE build. Assert the build succeeded
   before believing any number.
 - **`dotnet test` does not rebuild the runner.** Restart XMSG on D100 before every live test, and
   stop the runner BEFORE restarting XMSG - order matters.
 - **Never update a listing golden silently.** `FaListingRegressionTests` compares every emitted frame
   byte for byte. Behavioural tests all passed while the listing was broken; the goldens are what
   actually caught things. Update one deliberately and say why in the commit message.
 - **Never fabricate header word 6.** It IS a checksum D100 validates. A wrong one kills the peer with
   `XMSG ERROR CODE 24`.
