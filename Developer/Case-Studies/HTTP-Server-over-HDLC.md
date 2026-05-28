# Case Study: An HTTP Server on SINTRAN III, over HDLC

**A worked example tying together MAC programming, SINTRAN file I/O, and the
HDLC device — serving a web page and an image from an ND-100 to a browser.**

This case study is a map, not a code dump: it shows how the pieces
documented elsewhere in this repo combine into a real, running application,
and records the design decisions and lessons. Follow the links for the
how-to detail.

> **Verified** on SINTRAN III VSX/500 L under `nd100x`. Pages and a binary
> GIF were served to a normal browser, byte-for-byte intact.

---

## Goal

From a browser on a modern laptop, open `http://nd-server/` and get an
HTML 3.2 page **plus an image**, where the content lives on the ND-100's
SINTRAN file system and is served by a program running on the ND-100.

## Architecture

```
 browser ──HTTP──► host bridge ──HDLC frames (TCP tap)──► nd100x ──► MAC server (HSERVI)
                                                                         │
                                                          reads files from NDFS (MON 50/117/43)
```

Three layers:

1. **MAC server on the ND-100** — at startup it opens each file on the
   SINTRAN disk, reads it into memory, and then drives the HDLC controller to
   put the bytes on the wire.
2. **HDLC transport** — the ND's HDLC controller, exposed by the emulator as a
   TCP "tap."
3. **Host bridge** — a small program that speaks HTTP to the browser and
   HDLC frames to the tap, routing each URL to the right bytes.

## How each layer was built

### 1. The MAC server (read files, push frames)

- **Read files from NDFS.** Open with `MON 50` (access = 3, random), pull the
  whole file in one shot with `MON 117` (block read), close with `MON 43`.
  `MON 117` is essential here: the simpler sequential `MON 1` is capped at
  ~456 bytes, too small for a real page. Word counts, access codes, and the
  high-byte-first packing that lets a **binary GIF round-trip intact** are in
  the [MAC Cookbook — File system access from MAC](../Languages/System/MAC-COOKBOOK.md#7-file-system-access-from-mac-reading-files-to-serve-them).
- **Drive HDLC.** Reserve the device, DEVCL + DEVINI, then transmit the cached
  bytes as frames via `MON 201B`. The API is in the
  [HDLC Raw Programming Guide](../../SINTRAN/Devices/HDLC/HDLC-Raw-Programming-Guide.md);
  buffer-pool setup and the receive-arm trap are in
  [HDLC Buffer-Pool and Emulator Usage](../../SINTRAN/Devices/HDLC/implementation/Buffer-Pool-and-Emulator-Usage.md).
- The whole program is written and built with the
  [MAC Cookbook](../Languages/System/MAC-COOKBOOK.md) conventions and the
  [host cross-development loop](../Workflow/CROSS-DEVELOPMENT-WITH-ND100X.md).

### 2 & 3. Transport and host bridge

- The emulator's `--hdlc=1:<port>` exposes the controller's wire as TCP. The
  bridge connects there, de-stuffs/decodes HDLC frames the ND sends, and
  frames/encodes anything it sends back.
- The bridge serves HTTP to the browser and maps each request path to a slice
  of the bytes the ND produced (one HTML page, another page, the GIF), setting
  the right content type.

## Key design decision: broadcast vs request/response

The natural design is request/response: the ND receives the HTTP request,
reads the requested file, and replies. In our first attempt the reply didn't
reach the wire — after the receive, a plain send transmitted nothing. The
cause is a **missing transmitter restart**: a transmitter that has gone idle
must be explicitly restarted (reload DCB + retrigger) before it sends again;
TX and RX are independent modules. This restart path is **verified to work
in the emulator** — SINTRAN's own `XMSG-HDLC-TEST` echo mode does
receive-then-transmit between two relay-connected nd100x instances (5/5
frames echoed, 0 errors). So request/response is achievable; our broadcast
server simply never stops the transmitter, while a request/response server
must do the restart after each receive. See
[HDLC Buffer-Pool and Emulator Usage](../../SINTRAN/Devices/HDLC/implementation/Buffer-Pool-and-Emulator-Usage.md#restarting-transmit-after-the-tx-list-drains-incl-after-a-receive).

So the shipping design is **broadcast**: the ND continuously transmits all of
its cached files (the transmitter never goes idle, so no restart is needed);
the host bridge captures one full cycle and serves URLs from that capture.
Trade-offs:

| Model | Status | RAM footprint | Notes |
|-------|--------|---------------|-------|
| Broadcast (cache all, stream) | ✅ shipping | all files resident | simplest; transmitter never stops, wire always busy |
| Request/response (read on demand) | achievable — needs per-receive TX restart (verified via XMSG-HDLC-TEST) | one file at a time | the goal; scales past RAM |

On a 2 MB ND-110 the request/response model also matters for *scale* (you
can't cache an unbounded site), so it remains the target design — implemented
with a proper transmitter restart after each receive.

## Lessons worth carrying forward

- **`MON 117`, not `MON 1`, for whole files** — the ~456-byte sequential cap
  is a wall you will hit immediately on real content.
- **Binary survives if you don't byte-swap** — `MON 117` packs high-byte-first;
  emit words high-then-low and a GIF arrives intact. No special binary path.
- **Size the HDLC receive arm above `FRSIZE`+header** — too small and the
  driver insta-completes the arm, turning a blocking receive into a busy
  flood. (Buffer doc §3.)
- **Enlarge the HDLC buffer pool once, into a base image** — `CHANGE-BUFFER-SIZE`
  + `@RESTART-SYSTEM` (warm start), then reuse the snapshot. (Buffer doc §1;
  cross-dev workflow.)
- **A host bridge is a legitimate architecture** — pushing the HTTP/routing
  complexity to the host keeps the ND-side MAC program small and within the
  device's real constraints.

---

## See Also

- **[MAC Cookbook](../Languages/System/MAC-COOKBOOK.md)** — writing/building the MAC server; file I/O.
- **[HDLC Raw Programming Guide](../../SINTRAN/Devices/HDLC/HDLC-Raw-Programming-Guide.md)** — the MON 201B transmit/receive API.
- **[HDLC Buffer-Pool and Emulator Usage](../../SINTRAN/Devices/HDLC/implementation/Buffer-Pool-and-Emulator-Usage.md)** — pool setup, receive tuning, the RX→TX caveat.
- **[Cross-Development with nd100x](../Workflow/CROSS-DEVELOPMENT-WITH-ND100X.md)** — the host build/run loop.

---

*Built and verified on SINTRAN III VSX/500 L under nd100x. Transmit-after-
receive is a transmitter-restart requirement (reload + retrigger an idled
TX), verified working in the emulator via XMSG-HDLC-TEST echo mode between
two relay-connected instances — not an emulator limitation.*
