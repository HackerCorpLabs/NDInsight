# NDIX's XMSG use vs what SINTRAN does to our Ethernet II card — cross-check findings (2026-08-08)

Sources read: `E:\Dev\Ronny\NDIX-C\kernel\MASTER\if\if_et.c`, `if_access.h`, `xmsg.h` (Release C,
1988), against our carve corpus — chiefly
`E:\Dev\Ronny\NDInsight\Installation\Communication\Ethernet\RE\PIOCOS\LOC-XMSG-CLIENT.md`,
`MP-P2-PIOC-DRIV.NPL`, `XMSG-VALUES-M.SYMB`, and the RetroCore decoder
`Emulated.HW\ND\CPU\ND100\Sintran\MON_200_XMSG.cs`. Tags: [V] verified, [I] inferred, [U] unknown.

## 1. The big match: NDIX and the Ethernet II card are the SAME KIND of thing [V]

Both are **remote XMSG clients of the SINTRAN kernel**. Neither can issue `MON 200` itself, so both
hand the kernel a function + the four ND-100 registers and get the registers written back:

| | NDIX (ND-500, 1988) | Ethernet II card (68K LOC-XMSG) |
|---|---|---|
| Transport | fecall generic dev 7, two rings @0x30000000/0x800 | MBOXH activation queue @ card DRAM 0x4C2 |
| Request unit | `xmsg_cmd {seq, subdev, func, T/A/D/X, magno}` (20 B) | 6-word param block: func(T), A, D, X, uaddr |
| Func encoding | low byte = function, options OR-ed high (`XFMASK 0xff`) | same (`0x800D` = XFRCV+wake, `0x040C` = XFSND+route) |
| Doorbell in | KICK, only on empty→non-empty | SCIP INT12 (0xEF0080/0xEF0180) |
| Doorbell out | interrupt per response | `PWCR.BNDC` → MFP GPIP-I6 → 0x250E |
| Reply | response packet echoing seq/subdev/func | registers written back IN PLACE, `NXFNC` bit1 |
| Outstanding | 1 receive + 1 non-receive per subdevice | one element in flight (classic bring-up) |
| Recovery on loss | NONE — wedges forever | NONE — same |

The function numbers agree exactly where both sides define them (octal in NDIX, decimal in our
symbol file): XFGET 2, XFREL 3, XFRHD 4, XFWHD 5, XFREA 6, XFWRI 7, XFMST 9, XFOPN 10, XFSND 12,
XFRCV 13, XFRTN 31, XFRRH 32, XFRRE 41. The card's own `XMPF*` wrapper table (embedded vendor
symbols) covers nearly the same list. Same dialect, three independent 1980s sources. [V]

## 2. The mismatch that matters: functions 45 and 48 [V both sides; resolution open]

| Code | NDIX `xmsg.h` (1988) | Our `XMSG-VALUES-M.SYMB` / decoder |
|---|---|---|
| 45 (055) | `XETHER` "special Ethernet call" — every packet NDIX transmits | `XFSFM` "send message via specified link/netserver (privileged, COSROUT only)" |
| 48 (060) | `XFRREN` "receive and read, don't wait" — NDIX's standing receive | `X5FUN` **END MARKER** — the M-era function table ends at 47 |

Reading of 45 [I]: these are probably the SAME function under two names — "send via a specified
netserver" is exactly what NDIX's XETHER does (A = buffer word addr, X = length, D = own port,
plus the interface's magno). Not proven; needs the kernel dispatch carve for T=45.

Reading of 48 [V for the M symbol file]: **an M-era XMSG kernel has no function 48.** NDIX re-arms
its receive with `XFRREN|XFWAK|XFRMR` (48) after every packet. Against an M-vintage XMSG that call
would return `XEILF` (invalid function) — and by NDIX's own no-recovery design, receive would then
be dead. Any attempt to serve NDIX from a SINTRAN emulation must use a kernel generation whose
function table includes 48 (the K/L-era ND-500-serving kernels need checking; the version that
shipped with NDIX obviously had it). This is a **compatibility gate**, found only because the two
sources were put side by side.

Also new to us from NDIX: option bit `XFRMR 0x1000` on XFRRE/XFRREN = "release after read" —
that is the buffer-recycling bit; our option-bit table has 0x1000 as XFBNC (bounce) for XFSND
context only. Same bit, function-dependent meaning, matches our "role byte" experience. [V]

## 3. magno confirmed by running 1988 code [V]

`if_et.c` etinit: after attach it calls `XFMST(A=-1)` (current message), stores
`es_magno = A<<16 | D`, and later reaches the interface DIRECTLY with
`XFSND(A = magno>>16, D = magno & 0xffff)`. That is a live consumer treating A as SYSTEM and D as
the WIRE PORT WORD — our carved `MAGNO = system<<16 | port<<7 | random` layout exactly, and the
strongest possible confirmation of the LOC-XMSG doc's handle mapping (`XFRRE.D = MESAD`,
XFMST takes it as input; section 8b there). [V]

## 4. The media-access server: where NDIX's letters GO — still not located [U]

NDIX's attach letter (XSLET to `*ENUM<unit>`) is "forwarded to the Ethernet interface" — the
server lives card-side, per the 1985 manual ("a separate process running in the Ethernet
Controller"). But a raw byte scan of BOTH our 512 KB firmware images
(`encos-ser-all-banks-68k.bin`, `tcp-ser-all-banks-b05-68k.bin`) finds **zero occurrences of the
ASCII string `ENUM`** [V]. So neither image registers `*ENUM<i>` by literal name. Consistent with
the `*XM-ENNS0` resolution: the card registers names only LOCALLY (`XMSGIOCGAT`) and the global
XROUT name is created HOST-side. Either the media-access server builds/receives the name without
storing it as a literal, or it lives in firmware we do not have (Ethernet-I-era "Ethernet Master").
The EXMTY message-type family (128..132 = data/attach/status/detach/defineMulti, from
`if_access.h`) has NOT been searched for in the images (16-bit constants, too noisy for a blind
scan) — a targeted Ghidra pass over tcp-ser's message dispatcher is the next step if we ever need
to serve NDIX. [U]

### 4a. UPDATE 2026-08-08 — tcp-ser's media-access dispatcher enumerated, and it SHARPENS #4 [V]

Done, while carving tcp-ser's `*TCP` seam. tcp-ser DOES contain a media-access server, but it is
the WRONG layer for NDIX and that is the point:

- `MACMDPORTH` @0x6D2E (vendor name from the embedded symbol table) is the MEDIA-ACCESS command
  port handler. Its client is **AIP — the card's OWN on-card IP stack**, not a remote XMSG peer.
- Its dispatch table `tbl_maCommandDispatch` @0x24A86 (maxindex 0x1A = 26) uses a **6-bit opcode**
  `(RB[0] >> 2) & 0x3F`. Only EVEN opcodes are populated (0,2,4,6,8,10,12,14,22,24,26); every odd
  index and every unassigned even index goes to the default error handler 0x727C. **This is the
  live confirmation of the RB rule the NDIX side stated: request types are even, response =
  request+1, reply written in place.**
- Because that opcode space is 0..26, the **EXMTY message types 128..132 can never appear here** —
  they belong to the XMSG media-access USER↔SERVER layer, which is a different (XMSG-facing) server.

So the negative result in #4 is not just "no ENUM string" — it now has a mechanism: **tcp-ser is
the TCP/IP product; it wires AIP→MA on-card and does not export the media-access service over
XMSG.** The `*ENUM<unit>` XMSG server (Ethernet Basic Software, product 210582A) is genuinely a
different product that is not in either image we hold. Serving NDIX still requires that product;
carving tcp-ser further will not produce it.

Handlers of interest for anyone continuing: opcodes 22/24/26 (@0x71EA / 0x6FF2 / 0x7050) are the
attach/start/stop family. [V for the table; those three opcode meanings partly I — verify per
handler.]

### 4b. UPDATE 2026-08-08 — opcode 12 VERIFIED, and the old "set DIX mode" label is WRONG [V]

Fully decoded (0x7096-0x715E, plus workers 0x5522 search and 0x5486 allocate). **Opcode 12 =
DEFINE / ADD MULTICAST ADDRESS**, not "set DIX mode". Argument = a 6-byte MAC address at RB+0x08:

1. `arg[0] & 1` must be 1 — this is the Ethernet **I/G (group) bit**; a non-multicast address is
   rejected with status −18. This is exactly the "bit 0 of byte 0" the earlier note saw, but its
   meaning is the multicast bit of a real MAC, and **the other 5 bytes are the rest of the address,
   not padding**.
2. Search the active list at 0x24A08 (worker 0x5522, a 6-byte compare down a linked list). Already
   present → −18.
3. Allocate a node from the free pool 0x24A04 and append it to 0x24A08 (worker 0x5486, `#REMV` from
   free list + `#APPD` to active list). Pool empty → −20.
4. Require an active user — `ACTIVEMAUS` (0x2493E) or `ACTIVEDIXU` (0x24942) nonzero — else −16.
5. Copy the 6 MAC bytes into node+4.
6. If `LNMAIOACTI` (0x24860) is set, STOPMA (0x5C6E) + STARTMA (0x5C46) to reprogram the LANCE
   multicast filter. (This STOPMA/STARTMA pair is what made the earlier reader guess "set DIX
   mode".)
7. Reply OK; response TYPE stamped request+1 = **13** (`andi #0x3FF / ori #0x3400` at 0x7152) —
   a byte-exact confirmation of the even-request / odd-response rule.

This maps to NDIX `if_access.h` EXMTY "defineMulti". Sibling **opcode 14 @0x7162 = REMOVE MULTICAST
ADDRESS** (same 6-byte key, search via 0x5522 then delete). The real DIX-vs-802.3 mode gate is the
0x1888A mode word (a separate mechanism, already documented) — it is NOT an MA command opcode. The
handoff's `set DIX mode = AIP request-block type 12` line should be struck.

## 5. Receive discipline — what it says about our starvation hunt [V facts, I application]

NDIX keeps exactly ONE outstanding `XFRREN` and re-arms it inside the completion interrupt; if the
command ring is full it retries on a 40 ms timeout; buffers recycle via the XFRMR release bit.
There is no watchdog: one lost completion = receive dead forever, by design. Combined with the
card's SCIP one-shot RFT latch and the host-fed rx pool, the family invariant is:

> **Completions and doorbells are assumed exactly-once. Consumers drain to empty. Producers are
> re-armed by the consumer's own completion path. NOTHING times out.**

For the emulator this is a hard obligation: our card/bus emulation must never drop a SCIP, a
mailbox completion, or a kick under any interleaving — there is no protocol-level recovery to save
us. It also sharpens the starvation question: SINTRAN's driver-side analog of NDIX's
"re-arm in the completion handler" is the POST-BUFFER (opcode 0x12) refill; the new
`[68K-RXPOOL]` watches (0x67DA/0x6CEE, landed 2026-08-08) will show whether that re-arm chain
breaks, and this comparison says WHERE to look if it does: an error path that returns without
re-arming (NDIX carefully re-arms even on `T<0` — does SINTRAN's ENNS0?). [I]

## 6. The wedge contract — a concrete lead for the HLE burst-2 ordering bug [V contract, I application]

NDIX `xg.c`/`etintr` route completions **by the response's `func` field**, with two independent
slots per subdevice: the receive slot (XFRRE/XFRREN) and the other slot (XETHER, XFMST, ...). A
response whose func does not match unbalances the flags and wedges the subdevice permanently —
their words. Our HLE conn-to blocker is exactly an XFRRE-vs-XFRCV ordering race
(project_nd_ethernetii_hle_burst2_ordering). The 1988 contract says the fix shape: completions
must be matched to waiters by FUNCTION (receive-class vs other-class), never by arrival order.
Worth auditing `XmsgClient`/`MboxhTransport` completion routing against this rule. [I]

## 7. Action list

1. (Done 2026-08-08) Reply to NDIX updated with the 45/48 version gate + the no-ENUM-string scan.
2. When next in `MON_200_XMSG.cs`: annotate 45/48 with both names (M-era `XFSFM`/end-marker vs
   NDIX `XETHER`/`XFRREN`) so the decoder does not mislabel a future NDIX-side trace. Add option
   name `XFRMR` (0x1000 on XFRRE/XFRREN).
3. If we ever serve NDIX from RetroCore SINTRAN: check the kernel generation's function-table
   length (>= 49) FIRST; then the targeted EXMTY dispatch search in tcp-ser.
4. HLE burst-2: audit completion routing for func-matched (receive-class vs other-class) delivery.
