# The capture session — what to drive, in what order, and what each one proves

**Date:** 2026-08-07
**Supersedes:** Phase 2 of `PLAN-COMPLETE-THE-PROTOCOLS-2026-08-06.md`, which had the METHOD but
not the trigger for most items. The version-L manual (`210373L`) has since named the operator
command behind several of them, which is the missing half.

**Everything left in this project that is not implemented is blocked on a capture.** Nothing is
blocked on effort or on reading — the carve route to the FA operations is closed (see
`FA-COMMAND-NAMES-READ-FROM-BINARY-2026-08-06.md`: that table is a command-NAME parser, not a
handler table), and every manual we hold has been mined.

---

## 1. What changed since the last plan

| Item | Was | Now |
|---|---|---|
| `XFGSM` 47 | "capture or carve, not documented" | **Implemented** from manual 210373L section 6.3. A capture would VERIFY, not unblock. |
| `XSLIN` reply | not modelled | **Modelled** from section 7.5. Capture verifies P16-P18 and the gateway bit. |
| `XSECS` / `XSDCS` | no builder | **Buildable** from section 7.6. Capture verifies the exchange. |
| FA `0x01`/`0x04`/`0x0D` | "carve the handlers" | Carve route **CLOSED**. Capture is the ONLY route. |
| `XFWRT` 43 | capture-or-carve | Unchanged. The last function with no source at all. |

---

## 2. The triggers we now know

This is the part Phase 2 could not fill in. Manual section 5 documents the `XMSG-COMMAND` program's
commands, and each maps onto a service we can now build:

| Command to type | Emits | Verifies |
|---|---|---|
| `LIST-LINKS`, answer the `XROUT system?` prompt with a system | `XSLIN` | The reply layout, P16-P18, and the version-K short-reply case if pointed at an older node. |
| `ENABLE-CHECKSUM` / `DISABLE-CHECKSUM`, privileged | `XSDAT` sub-service `XSECS` / `XSDCS` | The sub-service codes and the parameter-2 rule. Also whether the datagram checksum then appears on the wire, which nothing has ever observed. |
| `LIST-NETWORK-SERVERS` with an `XROUT system` | `XSNSI` | Already built; confirms the remote-privileged path new in L. |
| `LIST-SYSTEMS` with `XRout system?` + `System?` | `XSLSY` | New in L. We have the service code only. |
| `LIST-UTILIZATION` | `XSGSU` | New privileged command; we have the code only. |
| `LIST-GENERATION-VARIABLES` | `XSGSG` | Would confirm the 23 variables in `XMSG-GENERATION-VARIABLES-2026-08-07.md` against a live system. |
| `LIST-CONNECTIONS` | XMFIDO watch state | The `*XM-FIDO` letter format from section 4.2, never captured. |

**The checksum pair is the most valuable of these**, because it is the only way to see a
version-L-only wire feature that we have documented but never observed. If enabling it changes the
datagram bytes, that is a wire format we do not model at all.

> The manual spells the disable sub-service `XSDSCS`. That is an OCR error, not ND being
> inconsistent: every one of the 300 symbols in `XMSG-PL-VALUES-M.INCL` is EXACTLY five characters,
> and `XSDSCS` is six. Both machine-readable sources say `XSDCS`. The five-character rule settles
> this class of question outright — a symbol of any other length in a scanned manual is a scanning
> artefact.

---

## 3. Order for one sitting

Ordered so that a failure early does not waste the rest, and so the risky item comes last.

### 3.1 Before touching anything

 - **Start the capture FIRST.** `"C:\Program Files\Wireshark\tshark.exe"` is present and verified
   working this session. Capture the HDLC TCP port, and the Ethernet segment port if that path is
   in use.
 - **Stop the runner before restarting XMSG on D100**, never the other way round.
 - Take note of the current commit. A capture is worthless if it cannot be tied to the code that
   produced our half of it.

### 3.2 The zero-risk reads (do these first)

`LIST-LINKS`, `LIST-NETWORK-SERVERS`, `LIST-SYSTEMS`, `LIST-UTILIZATION`,
`LIST-GENERATION-VARIABLES`. All are enquiries. They cannot change the machine's state, they
exercise five services, and `LIST-GENERATION-VARIABLES` doubles as a check of a table we
transcribed from a file rather than from a running system.

### 3.3 The FA operations (the actual gap)

Per the method that has worked five times: **a refused request is still a captured request.** Run
the runner, drive the command, read the bytes — we do not need to implement anything first.

 - **`CreateFile` 0x0A.** `COPY-FILE` does NOT send it; it opens with the quotes kept in the name.
   Try an explicit `CREATE-FILE` against a remote spec, or the COSMOS file-transfer program.
 - **`0x0D` Device-function.** ND's own name suggests a device-level call on an open file. Try
   `DEVICE-FUNCTION` from a terminal against a remote file, if such a command exists.
 - **`0x04` Change-file-entry-id.** The carve says it edits an EXISTING reservation and requires the
   entry to be reserved and of type 8. So drive it while a file is open, not standalone.
 - **`0x01` File-entry-disconnect.** Its table slot holds the padding address, so it may never be
   dispatched. Try aborting a transfer mid-flight. **A capture showing it is never sent is a
   result** — it would close the item permanently.

**Vary ONE input and repeat each.** Two samples with different name lengths is what separates a
field from padding; that is exactly how `DeleteFile`'s trailing byte was settled.

### 3.4 The post-close XEIMA (task #18)

Capture a full FA session INCLUDING the close and the `XEIMA` answer, with the sub-header
endpoints. Compare against our close frame byte for byte. We now know `XEIMA` is `SIII_RETRY` —
"the conversation is already gone" — so the question is narrow: what does a real client's close
look like that ours does not?

### 3.5 Last, because it changes machine state

`ENABLE-CHECKSUM`, then repeat one file transfer, then `DISABLE-CHECKSUM`. Do this last so that if
it destabilises anything, every other capture is already recorded. Note ND's ~5 per cent
throughput cost and that BOTH systems must be version L.

---

## 4. Standing traps (all learned the hard way)

 - **One terminal connection.** Reconnecting mid-program hangs the line.
 - **The console logs out after a few minutes idle.** A retroterm session left sitting is no longer
   logged in even though the TCP session survives. Read the screen before assuming; log in with a
   SINGLE send of `SYSTEM\r\r`.
 - **`dotnet test` does NOT rebuild the runner.** Build it explicitly or you capture yesterday's code.
 - **An `XENSE` flood means a stale sequence**, not a protocol bug. Restart XMSG on D100.
 - **Never guess on XMSG/TAD.** Guessing crashed the live machine once already.

---

## 5. What to do with the bytes

1. Write them into a test **verbatim, before any parsing code**, with a guard asserting the constant
   is the length the frame declared, so a mistyped byte fails as itself.
2. Only then write the codec.

This is not a preference. Four defects survived a fully green suite because every test built its own
input; the capture-based tests are the ones that have caught real errors. See
`FaLiveRequestRegressionTests` and `DatagramRelayCaptureTests` for the pattern — the latter uses a
real D100's own relay output as the expected value.

---

## 6. What this session cannot settle

`XFWRT` 43. There is no operator command that obviously drives it, because it is a function a SERVER
calls, not a command a user types. It needs either a COSMOS server program carved, or a capture of a
real server replying — which means running a real COSMOS service and watching its side. Out of scope
for a terminal-driven sitting; recorded so nobody expects it from one.
