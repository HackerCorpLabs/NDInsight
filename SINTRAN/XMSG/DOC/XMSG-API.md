# SINTRAN III XMSG — Programming / API Reference (MON 200B)

The **application-programming** side of XMSG: how a program calls the message
system through SINTRAN monitor call **200B**, the register conventions, the XROUT
service "letter" format, and the complete constant definitions.

Companion to the wire-format reference [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md) (what
those messages look like on the HDLC line). This document is about the API a
program uses; that one is about the bytes on the wire.

**Machine-readable constants:** [XMSG/xmsg-constants.json](../xmsg-constants.json)
(generated from the official version-M include file) is the canonical value
source for tooling; the tables here are the human-readable copy.

## Sources and confidence

- **Official constants:** `XMSG/XMSG-PL-VALUES-M.INCL` and `XMSG/XMSG-VALUES-M.SYMB`
  — XMSG **version M** (dated 1988). Authoritative for numeric values.
- **Programming model / semantics:** the OCR'd
  [Operations/Cosmos/ND-60164-3-EN COSMOS Programmer Guide](../../../Operations/Cosmos/ND-60164-3-EN%20%20COSMOS%20Programmer%20Guide.md)
  — XMSG **version J**. Appendix A (functions, lines ~9337–10440) and Appendix B
  (XROUT services, lines ~10441–11096) carry the byte-level detail.
- **Version caveat:** the guide is version **J**, the constants file version **M**.
  Numeric codes may have shifted between J and M; the guide gives semantics in
  symbolic form only (it does **not** print numeric service values), so numbers
  come from the version-M file. Where the guide's text is OCR-garbled it is marked
  `[OCR]`.

---

## 1. Calling convention (MON 200B)

XMSG is invoked by `MON 200` (octal `200B`). One call = one function.

- **`T` register = function code** (the `XF*` values in Section 6), with **option
  bits OR-ed into its high-order byte** (Section 2).
- **Parameters in `A`, `D`, `X`** (function-specific; Section 3).
- **`A`, `D`, `X` are NOT preserved** across the call — reload them afterward.
- **Completion status returned in `T`:** positive = success (meaning is
  function-specific, e.g. a message type or port number), **zero = "operation not
  terminated"** (e.g. no message yet, no wait requested), negative = an `XE*`
  error code (Section 7).

A double-word `MAGNO` (magic number) occupies the `A` and `D` register pair
(`AD`). Register-parameter symbols the guide uses: `MESAD` (message identifier),
`MAGNO` (32-bit magic), `RPORT` (16-bit hashed magic), `PORTNO` (local port; `0`
= default/most-recently-opened), `NBYTES`, `DISP` (byte displacement in message),
`B0to3`/`B4to5` (header bytes), `XPASW` (password / version code).

---

## 2. T-register option bits

OR-ed into the high byte of `T` alongside the function code. **Values are bit
numbers (0–15), not masks.** From the version-M include file:

| Symbol | Bit | Applies to | Meaning |
|--------|----:|-----------|---------|
| `XFSYS` | 7 | (general) | System-mode call (system domain) |
| `XFTCM` | 8 | XFSND/SFM | Send the task-current message |
| `XFSEC` | 9 | XFSND/RTN/WRT | **Secure message** — return to sender if not delivered |
| `XFROU` | 10 | XFSND | Message is addressed to XROUT (see `XFRRO`) |
| `XFRDI` | 10 | XFWRT | Reset current-message displacement to 0 |
| `XFFWD` | 11 | XFSND/RTN/WRT/SFM | Forward message (keep original sender's identity) |
| `XFBNC` | 12 | XFSND/RTN/WRT | Bounce message |
| `XFRES` | 12 | XFWRI | Reset message length to 0 before writing |
| `XFRMR` | 12 | XFRRE | Release the message when its last byte is read |
| `XFHIP` | 13 | XFSND/RTN/WRT/SFM | **High-priority** message (queued at head → type `XMTHI`) |
| `XFRRO` | 13 | XFSND | If `XFROU`: non-local XROUT (system no. in `A`) |
| `XFEXC` | 13 | XFALM/GET | Allocate/reserve an exclusive buffer |
| `XFPON` | 13 | XFSTD | Driver runs with paging on |
| `XFWAK` | 14 | XFRCV/PST/GST/RRH/RRE | Wake task on status change |
| `XFWTF` | 15 | (waitable calls) | **Wait** if the operation is not terminated |

> `XFWTF` on a **secure** cross-system send suspends the caller until the message
> reaches the receiver's input queue; without it, an undeliverable secure message
> is returned to the sending port instead (guide Appendix A).

---

## 3. Function register maps

Selected key functions (from COSMOS guide Appendix A). `In:` = registers on
entry, `Out:` = registers on return; `T` always returns status.

| Function | In | Out | Notes |
|----------|----|----|-------|
| `XFOPN` open port | `T` | `A`=PORTNO | XMSG assigns the port number |
| `XFCLS` close port | `A`=PORTNO | — | `A<0` = close all task's ports; `A=0` = default port |
| `XFGET` get buffer | `A`=NBYTES `(+XFWTF/XFEXC)` | `A`=MESAD | size 0 = descriptor only (for XFDUB) |
| `XFREL` release buffer | (current msg) | — | returns buffer to pool |
| `XFWHD` write 6-byte header | `AD`=B0to3, `X`=B4to5 | — | writes the first 6 bytes; sets length/disp = 6 |
| `XFRHD` read 6-byte header | `A`=MESAD | `AD`=B0to3, `X`=B4to5 | first 6 bytes → A, D, X **in that order** |
| `XFWRI` write data | `D`=NBYTES, `A`=UADD, `X`=DISP `(+XFRES)` | `D`=written | `DISP=-1` = append; odd DISP rounded up (+fill byte) |
| `XFREA` read data | `D`=NBYTES, `A`=UADD, `X`=DISP | `D`=read | `DISP=-1` = resume at current displacement |
| `XFSND` send | `AD`=MAGNO, `X`=PORTNO `(+options)` | — | `AD=-1` = reply to last sender; uses current buffer |
| `XFRTN` return message | `D`=DATA0, `A`=MESAD, `X`=PORTNO `(+opts)` | — | like XFSND but writes 2 header bytes from `D` |
| `XFRCV` receive | `A`=PORTNO `(+XFWTF/XFWAK)` | `T`=METYP, `A`=RPORT, `D`=MESAD, `X`=NBYTES | on type `XMTRE`, `X` = negative return reason |
| `XFP2M` port→magic | `A`=PORTNO | `AD`=MAGNO | local ports only |
| `XFM2P` magic→port | `AD`=MAGNO | `A`=PORTNO, `D`=SYSNO, `X`=INDEX | `T` also classifies: 3=system, 2=priv owner, 1=remote |

### 3.1 The 6-byte header (XFWHD / XFRHD)

The "header" these two functions read/write is simply the **first six bytes of
the message**, taken from / delivered to the `A`, `D`, `X` registers in that
order (bytes 0–1 = A, 2–3 = D, 4–5 = X). **XMSG assigns no meaning to them** —
any structure (e.g. the XROUT serial/service/length descriptor in Section 4) is
imposed by the application protocol on top. This is distinct from the in-kernel
XM5 header and from the on-wire XMSG sub-header (see XMSG-PROTOCOL.md).

### 3.2 Multi-function call (XFSMC)

`XFSMC` executes a queued list of calls; each entry is **4 words = T, A, D, X**
(buffer length `8 × NCALLS` bytes). It stops at the first entry whose status ≤ 0.

---

## 4. XROUT standard message format (the "letter")

XROUT is the name/routing service. A program talks to it by sending a message
whose body follows the **ND standard message format** (COSMOS guide Appendix B,
lines 10468–10496). This is the format carried in the wire `XMCSM`+trailer.

### 4.1 Message header (first 4 bytes of the message body)

| Byte | Field | Meaning |
|-----:|-------|---------|
| 0 | **Serial number** | Echoed unchanged by XROUT so the caller can match replies. High bit (bit 7) should be 0. |
| 1 | **Service number** (`XS*`) | On request: the service code (Section 6.4). **On reply, XROUT overwrites it with the return status** (0 = OK, else an `XR*` error, Section 7.2). |
| 2–3 | **Length** | Length of the remainder of the message in bytes. |

Service/result values occupy 0–255; a program may set **bit 7** to mark
user-defined services/status. (The "bit 6 set = service" pattern seen on the wire
is just because the standard service codes fall in `0x40–0x5F` — it is **not** a
documented flag.)

### 4.2 Parameter blocks (TLV, even-aligned)

After the 4-byte header, a sequence of parameter blocks:

| Byte | Field | Meaning |
|-----:|-------|---------|
| 0 | **Param number + type** | `0` = fill/skip byte. **Integer param = positive** number; **string param = negative** (two's-complement of the param number). |
| 1 | **Length** | Parameter length in bytes. |
| 2… | **Data** | Parameter data. |

Every parameter block starts on an **even byte boundary**. XROUT **reuses the same
buffer** for its reply, so the send buffer must be large enough for the response.
Reply messages come back with message type `XMROU`.

**Worked example** (guide `XMPBLOC`, lines 1272–1290): serial `123`, status `0`,
remainder length `32`, param 1 = string (`-1`) length `2` = `"HI"`, param 2 length
`14`, … The `XMPBLET` letter example computes `offset = 22` for a 4-byte header +
`'s-port'` (param 1) + `'scholar'` (param 2, padded to even) — confirming the
layout byte-for-byte.

### 4.3 Reply parameter meanings for key services

- **`XSGSY` "get routing info for system N"** (guide lines 10724–10756) replies
  with four integer parameters — **this is the frame the wire dissector sees as
  the "LI ROUTING" record set**:
  1. First system number ≥ the requested one (`0` = none)
  2. **Connection type** enum: `0`=Unavailable, `1`=Neighbour, `2`=Via (a relay),
     `3`=Via network server, `4`=Local
  3. Extra info (Link Index / relay system number / subaddress / local system
     number — meaning depends on the connection type)
  4. **Network info**: value ≤ `0377₈` → hop count in the right byte; value ≥
     `0400₈` → number of WANs in the left byte and hop count in the right byte
- **`XSLET` "send a letter"**: param 1 = port/connection name (string), param 2 =
  system name (string), optional param 4 = LAN-only flag.
- **`XSGNM`/`XSGMG`/`XSGIN`**: name ↔ magic-number / info lookups (strings ↔
  integers as above).

---

## 5. Concepts

- **System** = an ND-100 CPU running its own XMSG kernel. A PIOC or ND-500 is
  **not** a system — each has a "shadow" task in the ND-100.
- **Port** = a message endpoint owned by a task, identified locally by a small
  port number; may be given a name.
- **Magic number** = 32-bit `{port + system + random}`; the random part stops
  stale references from being reused. `XFP2M`/`XFM2P` convert to/from it. The
  **hashed magic** is a 16-bit "almost unique" abbreviation returned by receive /
  port-status calls (`RPORT`).
- **Letter** = a message sent via XROUT to a *named* port, so the recipient sees
  your message **and** your magic number without you learning theirs (XROUT never
  hands out another task's magic number).
- **Named port** vs **connection port**: a named port forwards the whole message
  to its magic; a connection port is gated by a free-connection counter and can
  load-share across identically-named server ports.
- **Secure message** (`XFSEC`): returned to the sender (as type `XMTRE`, with the
  negative reason in `X` on `XFRCV`) if it cannot be delivered or the receiving
  port closes while the message is current. **Non-secure messages are silently
  discarded** if undeliverable. Messages sent via XROUT are delivered secure.
- **Friend system** (`XSDFR`): a system granted trust by the local system;
  **non-reciprocal**.
- **Network server / gateway** (`XSNET`): a program that carries XMSG link-layer
  frames over some non-HDLC network in place of the HDLC/Megalink link layer.
- **System numbering** (`XSDLO`): ND-100 = serial number; ND-500 = serial +
  5000; ND-10 = serial + 9000; satellites = serial + 10000 (decimal).
- **XMSG Time Unit (XTU)** = 0.1 s.

### 5.1 Inter-system link states (XSLKI)

Starting an HDLC/Megalink inter-system link (`XSLKI`; operator "Start-Link")
drives this state machine, reported in parameter 1 on a status request:

`0 = DEAD` (crashed) · `1 = INIT` · `2 = CALL` (sending SABM frames) ·
`3 = CONN` (SABM seen → sends RR) · `4 = RUN` (RR seen → data phase; neighbour
marked reachable) · `5 = KILL`.

`XSLKI` parameters: link logical-unit number; timeout in XTU; frames to allocate
(= window + 1); SABM-retry count (`< 0` = infinite). This is the in-manual anchor
for the wire-level window/timeout/retry behaviour that the application guide
otherwise does not expose.

### 5.2 Protocol layering (from the XSTDC trace taxonomy)

The guide's trace-event list confirms a **Link layer** (HDLC frames) beneath a
**Network layer** (complete datagrams, with fragmentation): event 11 = link frame
received, 13 = link send frame, 14 = network datagram queued, 15 = datagram
fragment received. So the **datagram sequence number and fragmentation seen on
the wire are Network-layer** concerns — not visible at this application API.

---

## 6. Complete constant definitions (version M)

Canonical machine-readable copy: [XMSG/xmsg-constants.json](../xmsg-constants.json).

### 6.1 Function codes (`XF*`, MON 200B function in `T`)

| Val | Symbol | Meaning | Val | Symbol | Meaning |
|----:|--------|---------|----:|--------|---------|
| 0 | XFDUM | Dummy / get config | 24 | XFRIN | Routing init (obsolete) |
| 1 | XFDCT | Disconnect from msg system | 25 | XFCRD | Create driver w/ context (priv) |
| 2 | XFGET | Get message space | 26 | XFSTD | Start driver (priv) |
| 3 | XFREL | Release message space | 27 | XFDIB | Define indirect buffer (obs) |
| 4 | XFRHD | Read 6-byte header | 28 | XFRIB | Read indirect buffer (obs) |
| 5 | XFWHD | Write 6-byte header | 29 | XFWIB | Write indirect buffer (obs) |
| 6 | XFREA | Read message → user buffer | 30 | XFPRV | Request privilege |
| 7 | XFWRI | Write user → message | 31 | XFRTN | Write word 0 and return message |
| 8 | XFSCM | Set current message | 32 | XFRRH | Receive and read word 0 |
| 9 | XFMST | Get message status | 33 | XFDUB | Define user buffer (priv) |
| 10 | XFOPN | Open port | 34 | XFWDF | Define wake-up context (drivers) |
| 11 | XFCLS | Close port | 35 | XFDBK | Define bank no (drivers) |
| 12 | XFSND | Send message to remote port | 36 | XFSMC | Start multi-function call |
| 13 | XFRCV | Receive message on a port | 37 | XFDMM | Define max memory for task (priv) |
| 14 | XFPST | Get local port status | 38 | XFALM | Allocate messages to a task |
| 15 | XFGST | General status or wait | 39 | XFFRM | Free allocated messages |
| 16 | XFSIN | Service init (priv) | 40 | XFLMP | List messages and ports |
| 17 | XFSRL | Service release (obsolete) | 41 | XFRRE | Receive and read message |
| 18 | XFABR | Absolute read block (priv) | 42 | XFCPV | Check privileges |
| 19 | XFABW | Absolute write block (obs) | 43 | XFWRT | Write and return message |
| 20 | XFMLK | Lock msg system (obs) | 44 | XFMRT | Modify routing tables (COSROUT) |
| 21 | XFMUL | Unlock msg system (obs) | 45 | XFSFM | Send via specified link (COSROUT) |
| 22 | XFM2P | Magic → system no. + port | 46 | XFCRR | COSROUT receive and read (COSROUT) |
| 23 | XFP2M | Port → magic number | 47 | XFGSM | General status multiple or wait |

### 6.2 Driver-interface functions (DRXMSG)

`XDINF`=1 get message info · `XDSBP`=2 set byte pointer · `XDPUS`=3 put byte
sequential · `XDPUR`=4 put byte random · `XDGES`=5 get byte sequential ·
`XDGER`=6 get byte random.

### 6.3 Message types (returned by `XFRCV` in `T`)

| Val | Symbol | Meaning |
|----:|--------|---------|
| 1 | XMTNO | Normal message |
| 2 | XMROU | Routed message (via XROUT) |
| 3 | XMTHI | High-priority message |
| 4 | XMTRE | Return message (abnormal condition; reason in `X`) |
| 5 | XMKIK | XROUT has been kicked (no message) |
| 6 | XMTPS | Pseudo message (not used) |

> **Do not confuse these with the `XSGSY` routing-reply parameters** (Section 4.3),
> which also use small integers 1–4 but mean system-number / connection-type /
> extra-info / network-info. The wire dissector historically mislabelled the
> routing-reply parameters with these `XM*` names; that is corrected in
> XMSG-PROTOCOL.md.

### 6.4 XROUT service codes (`XS*`, byte 1 of the message)

| Val | Symbol | Service | Val | Symbol | Service |
|----:|--------|---------|----:|--------|---------|
| 64 | XSNUL | Null (returns 0 status) | 82 | XSGIN | Get info about name |
| 65 | XSLET | Send a letter | 83 | XSDLO | Define local system (priv) |
| 66 | XSNAM | Give name to this port | 84 | XSLEK | Send letter and kick (priv) |
| 67 | XSCNM | Clear name of this port | 85 | XSNET | Start/stop network server (priv) |
| 68 | XSGNM | Get name of port (by magic) | 86 | XSSCI | Set crash information (priv) |
| 69 | XSGNI | Get name (by MC/port no.) | 87 | XSGAT | Get / check attribute |
| 71 | XSGMG | Get magic from name (priv) | 88 | XSDAT | Define / remove attribute (priv) |
| 73 | XSDRN | Define remote name (priv) | 89 | XSNSI | Get network-server info (priv) |
| 74 | XSDSY | Define routing for system N (priv) | 90 | XSLIN | Get link info (priv) |
| 75 | XSGSY | Get routing info for system N | 91 | XSPIN | Get named-port info |
| 76 | XSLKI | Start up specified link (priv) | 92 | XSLSY | Get system info (priv) |
| 77 | XSTIN | Init tracing (priv) | 93 | XSGSU | Get system utilization (priv) |
| 78 | XSTCL | Close tracing (priv) | 94 | XSCRM | Start/stop routing manager (COSROUT) |
| 79 | XSTDC | Define tracing conditions (priv) | 95 | XSGLI | Get link-table info (COSROUT) |
| 80 | XSCRS | Create service (name, #SPs) | 96 | XSGSG | Get sysgen variables (priv) |
| 81 | XSNSP | New service point(s) | | | |

Sub-services (parameter 1) — `XSSCI`: `XSDAR`=1 auto-restart, `XSDRF`=2
restart-files, `XSDDF`=3 dump-files, `XSGDF`=4, `XSDUX`=5 dump XMSG, `XSGRF`=6,
`XSGAR`=7. `XSGAT`: `XSGXV`=1 version, `XSCMG`=2 check magic, `XSGCN`=3
deabbreviate name, `XSGFR`=4 friend info, `XSGLO`=5 local system. `XSDAT`:
`XSDFR`=1 define friend, `XSRFR`=2 remove friend, `XSECS`=3 enable checksum,
`XSDCS`=4 disable checksum, `XSDAL`=5 alt link (COSROUT).

### 6.5 User error codes (`XE*`, returned negative in `T`)

| Val | Symbol | Meaning | Val | Symbol | Meaning |
|----:|--------|---------|----:|--------|---------|
| −1 | XENOT | No more XT-blocks free | −19 | XEIMA | Invalid magic number |
| −2 | XEIRM | Non-local remote port illegal | −20 | XEMFL | Message space full |
| −5 | XENIM | Facility not implemented | −21 | XEILM | Illegal message size |
| −6 | XEIBP | Illegal message buffer pointer | −22 | XEIPN | Illegal port number |
| −7 | XEBNY | Message buffer not yours | −23 | XEPRV | Privileged function w/o privilege |
| −9 | XENOP | No more ports available | −25 | XERNA | Remote system not available |
| −11 | XENDM | No default message | −33 | XENOS | Indirect buffer bad segment |
| −13 | XEBFC | Message is in a queue | −34 | XENSE | Network sequencing error (internal) |
| −15 | XEBNC | Return of a bounce message | −36 | XEPCL | Remote port closed, msg queued |
| −16 | XEWNA | Write not allowed (indirect) | −37 | XENRU | XMSG not running |
| −17 | XENVI | No valid indirect buffer | −38 | XENTO | Timeout (network layer) |
| −18 | XEILF | Illegal function code | −40 | XEREJ | Network remote reject (retransmit) |
| | | | −43 | XENCE | Network checksum error (internal) |
| | | | −63 | XECRA | XMSG crash (info in basefield) |

(Full set incl. `XETMM`, `XENIR`, `XEIRT`, `XEIDR`, `XEMCH`, `XEAIN`, `XEPVR`,
`XEROV`, `XEXBF`, `XEDRI`, `XENDP`, `XEITL`, `XEIDP`, `XEILR`, `XERND`, `XENUS`,
`XEIXT`, `XETMU`, `XEICM` — see the JSON.)

### 6.6 XROUT / network-server errors (`XR*`, byte 1 of reply)

`XRSOK`=0 OK · `XRISN`=1 illegal service · `XRUNN`=2 unknown name · `XRDDF`=3
name already used · `XRNSP`=4 no space for names · `XRIPT`=5 illegal param type ·
`XRMMP`=6 missing mandatory param · `XRUNM`=7 unknown magic · `XRMTL`=8 message
too short/long · `XRSMF`=9 bad message format · `XRPRV`=10 not privileged ·
`XRISY`=11 illegal system no. · `XRNRO`=12 no access to remote system · `XRIIV`=13
illegal integer · `XRNEI`=14 · … · `XRUKS`=33 unknown remote system · `XRRFU`=36
routing table full · `XRAMB`=47 ambiguous name · `XRFFU`=48 friend table full ·
… `XRILX`=55 (see JSON for the full 0–55 range).

### 6.7 XMFIDO (file-distribution) statuses and crash codes

`XMFIDO` statuses (base `X412B`=17024): connection up/down, timeout, remote
terminated, etc. `XX*` crash codes (0–57), e.g. `XXPER`=20 protocol error,
`XXHER`=23 HDLC driver error, `XXROU`=22 no legal routing port. Full lists in the
JSON.

---

## 7. Send/receive example (PLANC, from the COSMOS guide)

Client sends a letter to named port `s-port` in system `snorre` and waits for the
reply:

```
XMPBLET(OUTBUFFER, MESSLENGTH, OFFSET, SERIALNUMBER, SYSTEMNAME, PORTNAME)  % build letter header
XMPFOPN(FLAGS, PORTNUMBER)                                                  % open a port
XMPFGET(FLAGS, SIZEBUFFER, MSGIDENT)                                        % reserve a buffer
XMPFWRI(FLAGS, BUFOFFSET, ADDR(OUTBUFFER), MESOFFSET, MESSLENGTH, WRITTEN)  % copy letter into buffer
XMROUT(FLAGS, MSGIDENT, PORTNUMBER)                                         % send via XROUT (secure)
XMPFRCV(FLAGS + XFWTF, PORTNUMBER, ...)                                     % wait for the reply
```

Server side: `XMPOPNM` (open + name "s-port") → `XMPFRCV` (wait) → `XMPFMST`
(learn client's magic) → reuse the buffer with `XMPFWRI` → `XMPFSND` back to the
client. The `XMPB*` helpers build the standard-message bytes of Section 4.

---

## 8. OCR caveats (COSMOS guide)

The guide is an OCR scan. Confirmed manglings (corrected above against the
version-M symbols): `XFCIS`→XFCLS, `XFWHID`→XFWHD, `XMWRI`→XFWRI, `XFSPND`→XFSND,
`XMP2P`→XFM2P, `XSXNAM`→XSNAM, `XSGN1`→XSGNI, `XSLKK`/`XSLEX`→XSLEK,
`XFEWD`→XFFWD, `XR0UT`→XROUT (zero-for-O throughout Appendix B). A few pages are
unreadable in the scan; those details are lost. This revision (J) does **not**
contain checksum services, XMFIDO, or `XSLIN/XSPIN/XSLSY/XSGSU/XSGSG` — those
appear only in the version-M constants file.

---

**Document path:** `SINTRAN/XMSG-API.md`
**See also:** [XMSG-PROTOCOL.md](XMSG-PROTOCOL.md) (wire format),
[XMSG/xmsg-constants.json](../xmsg-constants.json) (machine-readable constants),
[XMSG-COMMAND-REFERENCE.md](XMSG-COMMAND-REFERENCE.md) (operator utility).
