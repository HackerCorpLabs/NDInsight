# XMSG/TAD — open items and the plan to close them (status 2026-07-06)

Everything VERIFIED is in `XMSG-PROTOCOL.md` and `../../TAD/TAD-Message-Formats.md`; this
is only what remains open, after the 2026-07-06 Category-A analysis pass closed the
`00`-prefix, the reachability trailing form, the `0x0B` LU index, the list-systems TLV
bytes, and refuted the seed-per-responder hypothesis.

## Bucket 1 — cosmetic, mirror/tolerate is fully safe (no action needed)

| Item | State | If we ever want it |
|---|---|---|
| frameFlags `0x92` vs `0x96` | 82/86 mapped; 92/96 discriminator unknown, five candidate rules EXCLUDED by data | XMSG kernel listing, or accept forever |
| ACK `Flags2 0x0002` emitter condition | not pending-count (tested); emitting `0x0001` always is capture-consistent | more timed captures |
| LAPB address `0x07` on 6 ACKs | correlates tested, inconsistent | more samples arrive free with every capture; a targeted read of `MP-P2-HDLC-DRIV.NPL` address handling might name it |
| `0x0B` prefix (16-bit op vs pad) | ambiguous, both readings produce identical bytes | — |
| Accept tail `0202 000A`, prompt `52 40`, `0x15 = 0108`, via100's one mid-session `0xFD` | constant/mirrored | kernel listing |

## Bucket 2 — symbol-table agent results (DONE 2026-07-06)

| Item | Outcome |
|---|---|
| XXPER crash routine at `134265B` | **FOUND: `XHNRR + 6`** (receive-handler module); live machine matches the **L07** layout, not M06 — documented in `XMSG-PROTOCOL.md` §18.5 |
| Opcode names | **STRONG: `0x0B = 7LUN`** (converges with the LU=768+XX measurement), **`0x20 = 7ESRS`** escape response (matches the wire pairing), **`0xFD = 7POLL`** (unique value). CANDIDATES: `0x06/0x07 = 7CORQ/7CORS` connect request/response, `0x1B = 7KEYI`, `0x1C = 7BADT`, `0x15 = 7FBSI`. Never-seen family members catalogued (7PASS/7STRQ/7STRS/7IAM/7EDRS/7WHO). All in TAD doc §2.1 |
| Serialiser/seed hints | `XD5HS = 19 dec` = the wire sub-header length symbol; seed candidate `X4FRM = 24B` default with per-link variable at `X5FRM` [WEAK — needs live memory read]; the `XL*` per-link block (`XLRAC/XLTAC/XLRVR…`) and `XN*` per-node block (`XNSEQ=102B`) mapped for future work |

## Bucket 3 — closable with ONE live session ("the combo capture")

One sitting, one pcap, closes up to five items. Script for the operator:

1. Connect-to, log in, then **leave it idle 5+ minutes** → settles the keepalive-cadence
   question (are idle DUMMs periodic at all?).
2. Log out, reconnect, type a **wrong USERNAME** (valid-looking but unknown user) →
   the never-captured bad-username branch of the login ladder.
3. While logged in, **press the escape character** (ESCA path) → mid-session escape
   handling, never captured.
4. Run **list-systems with different arguments** (if the command takes any) → does the
   `04 02 0001` TLV vary (its value's meaning).
5. Finish with any **remote FILE operation** between the machines → the first
   `*XM-FIDO` letter + the whole file sub-protocol — THE most valuable capture, and it
   tests the framework's generality (envelope/ACK/odd-address rules should all carry
   over unchanged). Reverse-engineer it with the step-by-step method in
   **`LEARNING-A-NEW-PROTOCOL.md`**.

Also cheap in the same sitting, separate probe: send a letter to a **bogus name**
(`*NOSUCH`) and capture the reply — the empty/unregistered-name behaviour (expected: an
`XR*`-status error letter; letters are the recoverable path).

### Known status codes for `*XM-FIDO` (have BEFORE the capture)

The symbol file `xmsg-pl-values-l.incl` already gives the XMFIDO status block — these are
returned as **parameter 1 in an XROUT-format letter** (base `X412B = 41200B = 17024 dec`),
so when the `*XM-FIDO` traffic is finally captured these values should decode directly:

| Symbol | Value (dec) | Meaning |
|---|---|---|
| `XEFOK` | 0 | Ok |
| `XEFCL` | 17024 (`0+X412B`) | Watched port closed |
| `XEFSU` | 17025 | System connection **up** |
| `XEFSD` | 17026 | System connection **down** |
| `XEFTO` | 17027 | Timeout on request (no answer from remote XMFIDO) |
| `XEFAR` | 17028 | Start service already requested for this port |
| `XEFIP` | 17029 | Illegal parameters |
| `XEFOV` | 17030 | Time parameter outside legal values |
| `XEFNP` | 17031 | No corresponding port found |
| `XEFIE` | 17032 | Internal error in XMFIDO |
| `XEFLT` | 17033 | Local XMFIDO is terminating |
| `XEFRT` | 17034 | Remote XMFIDO has terminated |
| `XEFRU` | 17035 | Table resources used up in XMFIDO |

So the file-server family is `*XM-FIDO` (the "watch-a-port / system connection up-down"
service) — expect `XEFSU`/`XEFSD` as the connection-state signals and `XEFTO` on a dead
remote. This is the decode key to bring to the Bucket-3 file operation.

## Bucket 4 — closable with a scripted long run

Loop connect → login → **menu 5 (instant host-DCON disconnect)** ~25×+ without any
restart, capturing throughout. Crosses:
- the ACK channel's SECOND wrap (`DD → DC`, echoed F1 past `0x011E`) — the only part of
  the ACK formula not yet capture-proven;
- possibly a `0x0400-class letter at Counter 0xFF` (the wrap-boundary letter — still
  zero occurrences in 900+ frames; prime suspect in one historical 24B crash).
Menu 5 makes each cycle ~15 s, so this is ~10 minutes of wall time.

## Bucket 5 — needs config/infrastructure (defer)

| Item | Why deferred |
|---|---|
| What the link SEED encodes (0x14/0x13/0x11) | needs a 4th node number OR a comparison against the machines' SINTRAN/XROUT line-configuration values (cheap desk-check: run the XMSG-COMMAND link/config listings on both machines and compare the numbers against the seeds) |
| Relay with >1 hop; hop-budget semantics | needs a 3-node chain topology |
| SYCN `000A` alone vs full ladder for the 1-min timer | low value — we always do the full login |

## Suggested order

1. ~~Symbol agent~~ DONE — Bucket 2 results are in the specs.
2. Run the **combo capture** (Bucket 3) — one sitting, biggest yield, ends with the
   `*XM-FIDO` opener for the file-server work. Bonus now: watch for the newly named
   7\* opcodes (7PASS/7IAM/7WHO…) appearing in the file/remote traffic.
3. The **menu-5 loop** (Bucket 4) — 10 minutes, closes the last formula gap.
4. The seed desk-check (Bucket 5) whenever convenient — now with a concrete target:
   `X4FRM=24B` default / per-link variable at `X5FRM` (a live memory read on the ND, or
   the XMSG-COMMAND config listing, could confirm or kill it).
Bucket 1 needs nothing — the implementation is already correct while those stay open.
