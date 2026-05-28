# HDLC Buffer-Pool Setup, Receive Tuning, and Running Under nd100x

**Operational companion to the [HDLC Raw Programming Guide](../HDLC-Raw-Programming-Guide.md).**

The raw programming guide covers the MON 201B API, DCB layout, and
send/receive flow. This document covers the **operational layer around it**
that we had to get right to run a real, sustained HDLC application
(an HTTP server) on SINTRAN III VSX/500 L under the `nd100x` emulator:

1. Enlarging the driver buffer pool with `CHANGE-BUFFER-SIZE` (a one-time
   SINTRAN setup that is otherwise easy to miss).
2. Sizing the pool: how D-I-W, D-O-W, and the per-buffer max size relate,
   and what `status = 4` ("no buffer") really means.
3. The receive arm-size trap: an undersized receive buffer makes the
   driver *complete the arm immediately*, which turns a blocking receive
   into a busy flood.
4. Running and testing HDLC under `nd100x` (the `--hdlc` TCP tap), plus one
   emulator-specific caveat to be aware of.

> **Evidence tags** follow the repo convention: **[VERIFIED]** = observed
> directly in our runs; **[OBSERVED-nd100x]** = observed under the emulator
> and may differ on real hardware; **[FROM-MANUAL]** = stated in an ND
> manual in this repo.

**LDN convention used throughout:** `1360` = HDLC1 **input**, `1361` =
HDLC1 **output** (the first HDLC controller). Adjust for other units.

---

## 1. Enlarge the buffer pool first: `CHANGE-BUFFER-SIZE`

**[VERIFIED]** Out of the box, the HDLC logical devices have a small driver
buffer allocation, and a stock image will silently fail to sustain real
traffic (transfers abort before your program does useful work). The pool
must be enlarged **once** and the system restarted so SINTRAN reallocates.

**[FROM-MANUAL]** Per the
[COSMOS Operator Guide ND-30.025.02](<../../../../Operations/Cosmos/ND-30.025.02 COSMOS Operator Guide.md>)
§8.4.2 ("Example 61. Changing the HDLC Buffer Size"), use
`@SINTRAN-SERVICE` — **one LDN per line**, comma-separated — then a
**`@RESTART-SYSTEM` warm start** (not a cold boot). The guide's example
tests HDLC2 (LDNs 1362/1363) and notes: *"replace 1362 and 1363 with
n, n+1"* for another link, so for HDLC1 use 1360/1361:

```
@SINTRAN-SERVICE
*CHANGE-BUFFER-SIZE,1360,L,400,Y,N
*CHANGE-BUFFER-SIZE,1361,0,400,Y,N
*EXIT
@RESTART-SYSTEM
```

- one line per LDN (the link's `n` and `n+1`); the third field follows the
  manual's pattern (`L` on the first LDN, `0` on the second).
- `400` is the buffer size; this is the cap the pool math in §2 must respect.
- `@RESTART-SYSTEM` is a **warm start** — the manual states it is needed
  "to allow SINTRAN to reallocate the buffers." The allocation is
  established at restart, not live.

> If you skip this step, the most common symptom is that the first transfer
> after reservation returns a buffer-related error (`status = 4`, see §2)
> even though your DCBs look correct.

> **Note on exact syntax.** The form above is transcribed from the COSMOS
> guide (Example 61). An older note of ours used a different spelling
> (`*CHANGE-BUFFER-SIZE 1360/1361,INPUT/OUTPUT,400,Y,Y` + cold boot); treat
> the manual's comma form + `RESTART-SYSTEM` as authoritative and verify
> against your SINTRAN version's `SINTRAN-SERVICE` help if it differs.

---

## 2. Pool sizing: D-I-W, D-O-W, and `status = 4`

The driver keeps a **finite pool of fixed-size buffers** shared by the
in-flight transfers. Two configuration counts govern how many buffers are
reserved:

| Term | Meaning |
|------|---------|
| **D-I-W** (Driver Input Wait) | number of concurrent **input** (receive) buffers the driver pre-allocates |
| **D-O-W** (Driver Output Wait) | number of concurrent **output** (transmit) buffers |

**[OBSERVED-nd100x]** A usable rule of thumb for the per-LDN pool:

```
pool_words  >=  (D-I-W + D-O-W) x (per_buffer_max + overhead)
```

with the per-buffer max bounded by the `CHANGE-BUFFER-SIZE` value (§1, `400`
octal). Concretely, with **D-I-W = 4** a per-buffer max of about **50**
(`MRX = 50`) keeps the total within the `400` cap; pushing the per-buffer
max up to the full `400` while also holding several buffers exhausts the
pool. **D-I-W = 4** is the default that works on `nd100x`.

**`status = 4` ("No buffer available")** — documented in the raw guide's
status table — is **pool exhaustion**, *not* a queue-depth limit. It means
the driver could not get a free buffer for your DCB. Causes, in order of
likelihood:

1. The pool was never enlarged (§1).
2. The per-buffer max times (D-I-W + D-O-W) exceeds the pool (this section).
3. Frames are arriving faster than your program retrieves them, so the
   receive pool drains (see the raw guide's buffer-exhaustion flow).

See also: [Register-Reference](../reference/Register-Reference.md),
[HDLC Raw Programming Guide §6](../HDLC-Raw-Programming-Guide.md) (queuing
and buffer exhaustion).

---

## 3. The receive arm-size trap (blocking receive turns into a flood)

This one cost real time, so it is worth stating plainly.

When you use the **arm-then-collect** receive model (post an empty receive
DCB to arm the line, then collect the filled DCB — the multi-buffer DMA
queue model in the raw guide), the **maximum-size field on the arm must be
large enough to hold a frame**, specifically larger than the input
`FRSIZE` plus the channel header. If it is too small:

**[OBSERVED-nd100x]** the driver **completes the armed buffer immediately**
instead of waiting for data. A program that then loops "collect → process →
re-arm" never blocks — it spins, and on the wire this manifests as the
transmitter **flooding** (the serve loop runs continuously because the
receive never actually waits).

What we found works for small request frames (e.g. an 8-byte `GET /`):

| Field | Bad (insta-completes) | Good (blocks correctly) |
|-------|----------------------|--------------------------|
| input `FRSIZE` | `200` oct (128 dec) | `40` oct (32 dec) — fine for tiny requests |
| arm max-size (`MRX`/MSize) | `10` oct (too small) | `100` oct (64 dec), > FRSIZE+header |
| recv collect size (`USize`) | `10` oct | `100` oct |

Keep the arm max-size **above `FRSIZE` + header but small enough that
`(D-I-W + D-O-W) x max-size` still fits the pool** (§2). Those two
constraints together are the whole trick.

> Note the raw guide also documents a **simpler single-call receive** where
> the MSize parameter is the *wait flag* (`1` = block, `0` = poll). That is
> a different model from the arm-then-collect queue above; this section is
> about the queue model's arm step. See
> [HDLC Raw Programming Guide §6.11](../HDLC-Raw-Programming-Guide.md).

---

## 4. Running and testing HDLC under nd100x

**[OBSERVED-nd100x]** The emulator exposes an HDLC controller's wire as a
**TCP socket** (a "tap"). Launch with:

```
nd100x --boot=smd --smd0=<image> --telnet=<port> --hdlc=1:<tcp-port>
```

- `--hdlc=1:<tcp-port>` attaches HDLC controller **1** and serves its wire
  on `127.0.0.1:<tcp-port>`.
- Connect a TCP client to that port to act as the **far end**: bytes the ND
  transmits arrive as bytes on the socket; bytes you write to the socket are
  delivered to the ND as a received frame.
- Frames on the tap are real HDLC: `7E` flag, address, control, payload,
  FCS, `7E` flag, with byte-stuffing. A host-side decoder/encoder
  (de-stuff on read, frame on write) is all you need to drive it.

Minimal test pattern we used:

1. Boot the image, log in over `--telnet`, start your MAC HDLC program.
2. Connect to the `--hdlc` TCP port.
3. To test **transmit**: read the socket and decode frames the ND sends.
4. To test **receive**: write one framed payload (e.g.
   `7E 03 03 <bytes> <FCS> 7E`) to the socket and confirm your program's
   `FRCV` returns.

### Restarting transmit after the TX list drains (incl. after a receive)

**TX and RX are independent modules** and run independently. When a module
stops because its DCB list has been consumed, **it must be restarted by
loading fresh DCBs and triggering start again**. A continuously-running
stream (e.g. a broadcast loop) never stops, so it never needs the restart;
a stop/idle period — including the gap after a receive — does.

**[VERIFIED-nd100x] The emulator fully supports receive-then-transmit.** We
confirmed it with SINTRAN's own `XMSG-HDLC-TEST` (COSMOS guide §8.4.3) run
across **two nd100x instances** whose `--hdlc` taps were bridged by a TCP
relay: the **echo end** (`Loop on interface? N`, then `ECHO-MODE`) received
each frame and transmitted it back, and the **send end** (`SEND 5`) reported
`5 sent / 5 received / 0 errors`, with bidirectional traffic on the relay
(≈670 bytes each way). So RX→TX is **not** an emulator limitation.

A naive request/response server of ours *did* stall on the RX→TX turn — but
the cause was on our side: after the receive we issued a bare send **without
the explicit transmitter restart** (reload DCB + retrigger) that `ECHO-MODE`
performs. A broadcast design never stops the transmitter and so avoids the
issue; **a request/response design must restart the transmitter after each
receive**, exactly as XMSG-HDLC-TEST's echo mode does (and which the
emulator handles correctly).

---

## 5. Putting it together — checklist for a sustained HDLC app

1. **[ ]** `CHANGE-BUFFER-SIZE` both LDNs via `@SINTRAN-SERVICE` + `@RESTART-SYSTEM` warm start (§1).
2. **[ ]** Choose D-I-W / D-O-W and a per-buffer max that fit the pool (§2).
3. **[ ]** Reserve the LDNs (`MON 122B`), then DEVCL + DEVINI before any
   TRANS DCB (see raw guide); the input LDN arms only when the first empty
   receive DCB is posted.
4. **[ ]** Size the receive arm above `FRSIZE` + header so receive actually
   blocks (§3).
5. **[ ]** Under nd100x, drive/verify over the `--hdlc` TCP tap (§4); watch
   for the RX→TX caveat.

---

## See Also

- **[HDLC Raw Programming Guide](../HDLC-Raw-Programming-Guide.md)** — the MON 201B API, DCB layout, send/receive flow, queuing.
- **[Register Reference](../reference/Register-Reference.md)** — FRSIZE, RRTS/RTTS, WRTC/WTTC bit definitions.
- **[Debugging Guide](Debugging-Guide.md)** — general HDLC troubleshooting.
- **[COSMOS Operator Guide ND-30.025.02](<../../../../Operations/Cosmos/ND-30.025.02 COSMOS Operator Guide.md>)** — `CHANGE-BUFFER-SIZE` and operator commands.
- **[MAC Cookbook](../../../../Developer/Languages/System/MAC-COOKBOOK.md)** — writing the MAC program that drives all of the above (source encoding, monitor-call ABI, file I/O).

---

*Operational notes verified while building an HTTP server over HDLC on
SINTRAN III VSX/500 L under nd100x. Items tagged **[OBSERVED-nd100x]** were
seen under the emulator; confirm on your hardware/build before treating as
universal.*
