# Plan: implement every XMSG and COSMOS operation

**Forward-only.** What is NEXT, in phases. For what already works, read the commit history.

**Baseline 2026-08-06:** 658 tests pass, build clean. FA serves list, stat, open, close, block
size, read, write, create and delete against a real D100. XMSG kernel is at **27 of 48**
functions.

---

## The one idea that decides the whole order

Every remaining item is in exactly one of three states, and the state - not the importance -
decides what you can do about it:

| State | What it means | What you do |
| --- | --- | --- |
| **DOCUMENTED** | ND's manual specifies it | Implement it. No machine needed. |
| **CAPTURABLE** | Undocumented, but a real client can be made to send it | Capture FIRST, then implement from the bytes. |
| **CARVE-ONLY** | Undocumented and nothing we can drive will emit it | Read the server binary in Ghidra, or leave it. |

**Never implement across a state boundary.** Guessing a layout is what cost this project days -
the fitted word-6 model, the "contiguity is implied" claim, the echo that was arithmetic. A
refusal is a correct answer; an invented layout is a bug that hides until a real machine
disagrees.

---

## Phase 1 - finish what the manual already specifies (no machine, no risk)

Four XMSG functions remain DOCUMENTED and unimplemented. Each is one method plus tests, in the
pattern `XFALM`/`XFCPV`/`XFLMP`/`XFDMM` already follow.

| Function | Code | Manual | Note |
| --- | --- | --- | --- |
| `XFSMC` | 36 | Programmer Guide line 10532 | Executes a LIST of XMSG functions from one monitor call, four register-words each. Appendix C line 11568 names it one of four functions carrying a 32-bit address. |
| `XFSIN` | 16 | line 10690 | Returns the XMSG base-field address. Privileged. Only meaningful if we ever model the tables it points at - decide before building. |
| `XFDUB` | 33 | line 9994 | Attaches physical memory to a zero-size descriptor. Privileged, and physical memory means nothing here - likely NOT-APPLICABLE, confirm from the section then record the decision. |
| `XFABR` | 18 | line 10710 | Absolute read from physical memory. Same question as `XFDUB`. |

**Do `XFSMC` first** - it is the only one of the four a normal task uses. The other three are
privileged/driver-facing and the honest outcome may be "deliberately not implemented, here is
why", which is a perfectly good result as long as it is written down.

**Done when:** every one of the 48 is either implemented, or carries a comment saying which
category it is in and why it is absent.

---

## Phase 2 - capture what a real client will send us

These are UNDOCUMENTED but CAPTURABLE. **No manual we own describes the COSMOS FA wire protocol
at all** - every FA layout we have came from a capture. So this phase is about making a real
client emit the thing, not about reading.

### 2.1 The prerequisite problem, and how to solve it

The blocker is always the same: *what makes a real client send this operation?* Work it out
before touching the wire.

**The method, which has worked five times now:**

1. **Find the operator command.** Grep `Operations\Cosmos\` and the SINTRAN command lists for a
   command that would plausibly drive the operation. FA operation names came from the server's own
   command table (`COS-FA-SERV-E04:PROG` at `BANK2::8731`, transcribed in `FaOperation.cs`), and
   several map onto SINTRAN monitor calls of the same name - `RENAMEFILE`, `SetFileAccess`.
2. **Drive it and watch, even though we will refuse it.** Start the runner, run the command from
   D100, and read the log. A refused request is still a CAPTURED request - that is exactly how
   `SetBlockSize`, `SiiiSpecial 0x0021` and `DeleteFile` were all obtained.
3. **Vary ONE input and repeat.** Two samples with different lengths is what separates a field
   from padding. The `DeleteFile` name field needed `THIRD:TXT` and `NEWFILE:TXT` to show that the
   trailing byte was neither constant nor alignment.
4. **Write the bytes into a test verbatim before writing any parsing code.** Add a guard asserting
   the constant is the length the frame declared, so a mistyped byte fails as itself.

### 2.2 The queue, most likely to be reachable first

| Item | What to drive | Why it is worth it |
| --- | --- | --- |
| **`CreateFile` 0x000A live** | Not `COPY-FILE` - that uses quoted `OpenFile` instead. Try an explicit remote `CREATE-FILE`, or the COSMOS file-transfer program. | Implemented from one capture; never driven by a real client. |
| **FA `0x0001` FileEntryDisconnect** | Unknown. Try disconnecting a file entry mid-session, or aborting a transfer. | Its handler slot holds the same address as the table padding, so it may not be dispatched at all - which would itself be the answer. |
| **FA `0x0004` ChangeFileEntryId** | Unknown. | Never captured. |
| **FA `0x000D` DeviceFunction** | Unknown - possibly a device-level call on an open file. | Never captured. |
| **`XFWRT` 43** | A real COSMOS server replying to a request. | Ranked the most likely missing XMSG function real server code uses. Named in the M include, NO Appendix A section - so it is capture-or-carve, not documented. |
| **`XFGSM` 47** | A server watching several ports. | Same: M include only. |

### 2.3 Partial read and write - already answered, do not re-open

The monitor calls FA is named after settle it. `117B ReadFromFile RFILE` (Monitor Calls line
17826): reads *"any number of bytes"* but *"must start at the beginning of a block"*. So
**block-aligned start, arbitrary length** - half a block is legal, an offset into a block is not.
We already do this: `SetBlockSize` plus the position field, and the position counts in the
client's block size. Fine granularity comes from setting a SMALL block, not from a byte offset.

Whether the FA wire carries those same two fields is not documented; our implementation matches
the captures we have.

---

## Phase 3 - carve what nothing will send us

CARVE-ONLY. Offline Ghidra work on binaries already in the repository.

1. **`COS-FA-SERV-E04.PROG`** - the FA server itself. The handler address table is already
   transcribed in `FaOperation.cs:13-16`; `0x0001` is at `BANK2::1ead` and the rest follow. This
   answers phase 2.2's FA rows without needing a client at all, and it is the ONLY thing that can
   answer "what does the server do with this byte" as opposed to "what appears on the wire".
2. **`COS-FSART-E02.BPUN` (66 KB) and `COS-FAU-VSX-E03.BPUN` (90 KB)** - zero citations anywhere,
   never opened. FSART is implicated in the known d102 hang.
3. **`ENNS0-LNK.PROG`** - `Installation\Communication\Ethernet\x\linked\`, 40,960 bytes, entry
   `ENNS0` at octal 32241, staged for Ghidra and never disassembled. Parent of most Ethernet gaps:
   the wire bytes for NPDU types WO, DR-by-user and DC, and the `ENNS0_STARTED_FLAG` semantics our
   emulator comment currently gets wrong.

---

## Phase 4 - the cheap wins that are not new operations

Ordered by value, all offline.

1. **Import ND's error dispositions.** `E:\Dev\Ronny\NDIX-C\baseline\bin\cps\xmsgerrors.h` and
   `xrouterrors.h` carry a plain-English description AND a recovery disposition for all 99 codes -
   57 `GIVE_UP`, 35 `RETRY`, 2 each `UNKNOWN`/`SUSPEND`/`OK`, 1 `SLEEP`. Our enums have neither.
   Also `systemerrors.h` (20 KB) for the SINTRAN file-system codes FA replies carry.
2. **Clear the eight stale doc claims** the gap map found (G1-G8). One of them already caused an
   agent to report "the node cannot relay" as current fact.
3. **Wire up the relay.** `DatagramRelay` is complete and tested with NO production caller. Needs
   a node holding two links and a route table from `topology.json`.
4. **OCR `210373L_X-Message.pdf`** - `F:\ND\SINTRAN-L-XMSG\FLOPPY\`, 44 pages, ND's own X-Message
   manual for the revision on our wire, never cited. It is a scan with no text layer. This is the
   one document that might settle the post-close XEIMA.

---

## Standing traps

 - **One terminal connection.** Reconnecting mid-program hangs the line.
 - **The console logs out after a few minutes idle.** A retroterm session left sitting is not
   logged in any more - re-check before assuming, and log in with a single
   `terminal_send text="SYSTEM\r\r"`.
 - **`dotnet build-server shutdown` when finished.** A held DLL makes the next build silently keep
   the old binary and the tests report green against stale code.
 - **Restart XMSG on D100 if XENSE floods.** That is a Flags1 sequence desync, not a protocol bug.
 - **Never update a golden silently.** They compare what we EMIT; they cannot catch what we PARSE.
 - **A doc claiming something is missing is a LEAD, not a finding.** Check the code first.
