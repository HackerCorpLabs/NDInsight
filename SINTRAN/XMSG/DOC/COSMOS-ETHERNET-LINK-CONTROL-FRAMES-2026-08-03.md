# COSMOS over Ethernet: the link control frames 0x0F and 0x6F (2026-08-03)

Captured live from two real ND-100 machines (D100 and D102) sharing one emulated Ethernet segment
through `Xmsg.Hub`, with a third node observing every frame. Every byte below is from that capture.

The observer is `Xmsg.Live.Runner` as node 19999; the hub is
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Hub`. The hub reported **zero dropped frames** for the
whole session, so nothing here is a capture artefact.

---

## 1. What was already known

`NdLinkLayer` models two frame kinds, both confirmed again here:

 - `0x20` DATA, answered by
 - `0x3F` ACK carrying the received sequence PLUS ONE.

The 11-byte ND link header was documented as:

```
0b 02 | kind | 00 | seq | senderLinkId(2) | receiverLinkId(2) | plen(2)
```

That is correct for DATA and ACK. It is **wrong for the control frames**, see section 4.

---

## 2. Link ids are real and stable

On an established link each node has a 16-bit id, and both ends carry both ids on every frame:

| Node | Link id |
| --- | --- |
| D100 | `0x65CF` = 26063 |
| D102 | `0x582E` = 22574 |

A working exchange:

```
node102 -> node100  data seq=75  snd=582E rcv=65CF  plen=62
node100 -> node102  ack  seq=76  snd=65CF rcv=582E  plen=0
node100 -> node102  data seq=75  snd=65CF rcv=582E  plen=14
node102 -> node100  ack  seq=77  snd=582E rcv=65CF  plen=0
```

Data payload lengths observed: 14, 36, 46, 60, 62, 68, 98, 126, 128, 140. ACKs always carry 0.

---

## 2b. BOTH SIDES CAPTURED 2026-08-03: the frames are header-only, and the link ids are pinned

`net dump 20` was taken on D100 and on D102 within seconds of each other while `li-fi d102(sys).,,`
from D100 was returning silence. The two dumps cover the SAME 20 frames, numbered #48061-#48080 on
both machines, TX on one side exactly where the other shows RX, byte for byte. Neither card lost
anything. The counters agreed too: D100 TX 23961 packets / 1725780 bytes equalled D102 RX to the
byte, and D102 TX 24119 / 1726148 equalled D100 RX. D102 reported `missed(no buffer)=0`; D100
reported 19, and 24100 accepted + 19 missed = 24119 received, so its arithmetic is self-consistent.

**VERIFIED - a 0x0F frame carries NO payload.** Its 802.3 length is `0x000E` = 14 = 3 (LLC) + 11
(ND link header) + 0. Every byte after the eleventh is Ethernet padding up to the 60-byte minimum.
Earlier readings that assigned meaning to the `00 01 CF 32 00 05 ...` tail were reading padding.
The same applies to `0x6F`, also length `0x000E`.

**VERIFIED - the link ids.** One real exchange appears in the capture:

```
#48079  D100 -> D102   0B 02 20 00 71 | 2F 48 | 28 A4 | 00 0E    802.3 len 0x1C -> 14-byte payload
#48080  D102 -> D100   0B 02 6F 00 65 | 28 A4 | 2F 48 | 01 01    802.3 len 0x0E -> no payload
```

Sender and receiver swap cleanly, so **D100's link id is `0x2F48` and D102's is `0x28A4`**. The
`0x20` payload is the documented 7-word datagram header - `2113 0019 0066 0064 FFFF 0001 DE08` -
with `0x66` = 102 and `0x64` = 100, the two system numbers, and `DE08` in word 6. On a data frame
the last header field is `0x000E` = the payload length.

**VERIFIED - the last field means different things per kind.** It is `0x000E` (a length) on the
`0x20` data frame, `0x0066` (= 102, the SENDER's own system number) on D102's `0x0F` frames, and
`0x0101` on the `0x6F`. Section 5 called this; here it is on the wire.

**The failure, as far as the wire shows it.** 18 of the last 20 frames are D102 sending `0x0F` to
D100 and getting nothing back:

```
batch A, 7 identical frames:    0B 02 0F 00 63 | 00 00 | 2F 4E | 00 66
batch B, 11 identical frames:   0B 02 0F 00 64 | 00 00 | 2F 4F | 00 66
```

The sender slot is `0000` - no link yet. Each batch is the same frame repeated, so this is a retry
that never gets answered.

**NOT settled: what `2F4E` / `2F4F` is.** Two readings fit and the wire cannot separate them:

 - (a) it is the receiver link id, and D102 is computing it wrong - `2F4E` is D100's real `2F48`
   plus 6, `2F4F` plus 7 - so D100 answers nothing because it owns no such link;
 - (b) it is not a link id on this frame kind at all. Between batch A and batch B the sequence
   number goes `0x63` -> `0x64` and this slot goes `2F4E` -> `2F4F`: **they increment by one in
   lockstep**, which is counter behaviour, not identity behaviour, and makes the resemblance to
   `2F48` possibly coincidence.

Reading (a) was asserted first and is NOT safe. The lockstep increment is the stronger observation.
Deciding between them needs the driver, not more captures.

## 2c. THE FRAME KINDS ARE NPDU TYPES - found in the ENCOS monitor, 2026-08-03

`encos-mon-ii-b01.prog` (already loaded in Ghidra as an ND-100 :PROG, 74 functions, unannotated)
contains a TRACE DECODER for this exact protocol. Its format strings at `ram:2642`-`ram:26a0`:

```
" in      from ND out     to   ND out RI2 to   ND locI6 remI6 CR from ND"
" CC from ND"
" DT nrI4 length"
"I5 AK nrI4 credit"
"I5 WO nextNRI4 credit"
" DR by user reasonI3 DR BY NS reasonI3 DC"
" !!! UNKNOWN NPDU TYPE !!!"
```

**These are ISO-transport NPDU type names**: CR connection request, CC connection confirm, DT data,
AK acknowledge, WO window, DR disconnect request, DC disconnect confirm. So the "frame kinds" in
section 1 are not ND-private mystery bytes - they are NPDU types, and this program can name them.

Note what the CR line prints: **`loc` and `rem`** - a local and a remote reference. That is the
pair of link ids, and it is the vocabulary the protocol itself uses.

**NOT YET DONE - do not guess the mapping.** It is tempting to write `0x0F` = CR, `0x20` = DT,
`0x3F` = AK and be done. The evidence does not support that yet: no code has been read that maps a
type CODE to one of these names. Naming `0x0F` on plausibility is exactly the mistake section 3
already had to withdraw once. The mapping must come from the decoder.

**CORRECTION: there is no jumptable at `0x27d1`.** This section first said Ghidra's
`Could not recover jumptable at 0x27d1` warning marked the type switch, and that recovering it was
the answer. Wrong. `ram:27ce`-`27d1` is `LDX -0x7a,B` / `LDA 0x1,X` / `BSKP 0xf` / `JMP` - a plain
bit-15 test on entry word 1, and Ghidra simply mis-described the branch. Nothing to recover.

**THE REAL DISPATCH, found at `ram:2556`:**

```
ram:2556: LDX -0x7a,B        ; X = trace entry
ram:2557: LDA 0x3,X          ; A = entry word 3
ram:2558: SHA ZIN SHR 0x8    ; A = HIGH BYTE of word 3   <- the NPDU type
ram:2559: STA -0x76,B
ram:255a: LDT I *0x25c6      ; T = [ram:26ad] = 7        <- bound
ram:255b: SKP DT, MGRE, SA   ; skip if 7 >= A
ram:255c: JMP 0x25c7         ; out of range -> "!!! UNKNOWN NPDU TYPE !!!"
ram:255d: COPY SA, DX        ; X = type
ram:255e: LDX ,X I *0x25c8   ; X = table[type], base ram:26ae
ram:255f: JMP 0x0,X          ; jump to the arm
```

The type is a **direct index 0-7** into an eight-entry arm table at `ram:26ae`:
`2560, 256c, 2578, 2598, 25ba, 25f0, 25f8, 2620`. Arm 0 loads string length 10 = `"CR from ND"`
and arm 1 length 10 = `"CC from ND"`, both read off the code, so the order is:

| index | type | meaning |
|-------|------|---------|
| 0 | CR | connection request |
| 1 | CC | connection confirm |
| 2 | DT | data |
| 3 | AK | acknowledge |
| 4 | WO | window |
| 5 | DR | disconnect request, by user |
| 6 | DR | disconnect request, by NS |
| 7 | DC | disconnect confirm |

Indices 0 and 1 are verified from the arm code. Indices 2-7 follow the order of the format-string
blob and are STRONGLY INDICATED but not individually read yet.

**THE LIMIT THAT MATTERS - this is NOT the wire encoding.** The dispatch bound is 7, so these are
the driver's INTERNAL type numbers 0-7. The wire carries `0x0F`, `0x20`, `0x3F`, `0x6F`, which are
not in 0-7. So this table names the types and fixes their order, but it does NOT say `0x0F` = CR.
Whatever converts a wire byte to this index, or back, has not been found. Until it is, the mapping
stays open - exactly as section 3 insists.

Also learned: **a trace entry is 9 words** (`ram:2979` = 9, used as the stride by
`trace_print_entry_by_index`), the type lives in the high byte of entry word 3, and the direction
lives in the high byte of entry word 0.

Once the wire-to-index conversion is known, the open question from section 2b - whether
`2F4E`/`2F4F` is a receiver link id or a counter - should fall out of the CR layout, since CR is
the frame that carries `loc` and `rem`.

## 2d. THE WIRE ENCODING: high nibble of the kind byte IS the NPDU type

With the type table from section 2c in hand, line up the four kinds seen on the wire against it:

| wire byte | high nibble | table index | type |
|-----------|-------------|-------------|------|
| `0x0F`    | 0           | 0           | **CR** connection request |
| `0x20`    | 2           | 2           | **DT** data |
| `0x3F`    | 3           | 3           | **AK** acknowledge |
| `0x6F`    | 6           | 6           | **DR** disconnect request, by NS |

**Why this is not circular reasoning.** Two of these were pinned from the wire BEFORE the table was
found, and two come from code read hours later, and they agree:

 - the both-ends capture showed `0x20` is the frame carrying the 14-byte datagram payload. The arm
   table, found separately, puts **DT (data)** at index 2.
 - `0x3F` was identified as the acknowledgement from behaviour alone back in section 1. The arm
   table puts **AK (acknowledge)** at index 3.

Independent derivation, same answer. That is what section 3 asked for and never had.

**So `0x0F` is CR - a connection request.** The reading withdrawn this morning as "not earned" is
now earned, by a route that can be checked instead of one that merely sounded right. The low nibble
is NOT explained: `0x0F`, `0x3F` and `0x6F` all end in `F` while `0x20` ends in `0`. Leave it open.

**What the failure actually is.** Re-reading the capture with the types substituted:

 - 18 of the last 20 frames are D102 sending **CR** - "open a link" - and getting no answer at all.
 - D100 sends **DT** on the old link.
 - D102 answers that with **DR, disconnect request by network service** - a teardown, not an ack.

So D102 is simultaneously tearing the old link down and asking for a new one, and D100 answers
neither. "The link is wedged" was too vague; this is a specific, testable state.

Still open: why D100 ignores an incoming CR. That question now has a home - the CR handler inside
ENNS0, which is linked and loaded (see below).

**ENNS0 is now in Ghidra.** `ENCOS-ERR-0-B01:BRF` was linked on D100 with the vendor's own
`BRF-LINKER-C01` (`PROGRAM-FILE` + `LOAD`, no unresolved entries, PLANC-1BANK-G00 pulled in,
P-space used to `047036`). Output kept at
`Installation/Communication/Ethernet/x/linked/ENNS0-LNK.PROG` with the entry map beside it in
`ENNS0-ENT.SYMB`. The map's octal offsets land directly on Ghidra addresses - entry `ENNS0` at
octal 32241 is `ram:34a1` - so the symbol table can be applied as-is. CAVEAT: this is our link, not
ND's. The real ENNS0 is placed by `RT-LOADER` on segment `ENCOSE0`, so absolute addresses may
differ from a running system even though the code does not.

## 2e. THE CARD RECEIVE PATH, CARVED - and where the doorbell is NOT

Carved from `encos-ser-all-banks-68k.bin` (68K firmware, already in Ghidra and heavily annotated by
earlier sessions). Read because of the question "are we failing to ring the doorbell or set an
interrupt flip-flop, so SINTRAN never wakes?".

**The chain, step by step:**

1. The LANCE fills a receive descriptor and asserts INTR (68K level 2).
2. The interrupt path reaches `RCVCOMPLETE` (`jsr` at `0x58b4`). It is a
   `do { ... } while(true)` LOOP: it drains EVERY filled descriptor in one call, returning only
   when it meets a descriptor still owned by the chip.
3. Per frame it checks, in order: RMD1 error bit; size 60..1514; the 802.3 length field; then the
   MAC filter - an exact six-byte compare against the station address, no masking, no
   protocol-family scheme.
4. **GATE3**: `rxpool_count` (`0x188c6`) must be non-zero or the frame is DISCARDED. That counter is
   set only by host command opcode `0x12`; firmware init leaves it 0. So if the ND-100 stops posting
   buffers, the card silently drops everything.
5. It takes a node from `rxpool_freehead`. If the pool is empty it bumps
   `STAT_rxDroppedNoPoolNode` - this is the `missed(no buffer)` figure that `net stat` prints.
6. On success it appends the node to `g_hostReadyRing` (`0x188d6`) through the generic tail-append
   at `0x134e6`, then loops for the next descriptor. On any discard it re-appends the buffer to the
   LANCE ring and, in the words of the existing annotation, gives "no host notification of any kind".

**The finding: nothing in that path rings the ND-100 doorbell.**

The doorbell is a byte write to `0xEF0080`. The whole image contains exactly three writes to it:

 - `post_and_signal_nd100_scip` (`0x1a48`) - callers are `reset_entry`, an error path and
   `NdMonitorReportErrorAndWait`. Startup and error only, not the receive path.
 - `0x2248` - inside a primitive that takes ONE node parameter, links it onto the list at `0x4c2`,
   restores `SR`, rings the bell and returns 1. This is per-message: one node, one doorbell.
 - `0x249a` - not yet examined.

So there are TWO host queues, and the receive path uses the one that does not ring:
`g_hostReadyRing` (`0x188d6`, appended by `RCVCOMPLETE`, no bell) versus the list at `0x4c2`
(appended by the `0x2248` primitive, which does ring). Separately, code at `0x6e7c` tests
`g_hostReadyRing` and, when non-empty, calls `0x518e` and then `0x6db8`; `0x6dcc` drains the ring
with `clr.l (0x188d6)`.

**NOT PROVEN - what this does and does not show.** It shows that `RCVCOMPLETE` itself never signals
the host, and that received frames land on a queue whose append does not ring. It does NOT show that
a received frame is never signalled: the drain path at `0x6e7c`/`0x518e`/`0x6db8` has not been
traced to its end, and it may well reach the `0x2248` primitive or `0x249a`. Do not conclude "the
doorbell is missing" from this section. What is worth testing is the SHAPE: a receive interrupt
drains an unbounded number of frames into one queue, so if the notification happens once per drain
rather than once per frame, N frames produce one interrupt - which is exactly the coalescing
described as a hypothesis in section 6c.

**Evidence against a systemic wake failure.** On 2026-08-03 D100 reported `accepted=24100` with
`missed(no buffer)=19` - 0.08 percent. The firmware was plainly running and draining the ring nearly
perfectly, so a wholesale missing doorbell does not fit. Whatever is wrong is narrower than that.

## 2f. THE NOTIFY INTERLOCK - the card notifies ONCE and waits to be re-armed

Continuing the receive carve of section 2e, the drain path was followed to its end:

```
0x6e7c   tst.l (g_hostReadyRing 0x188d6)   ; anything to deliver?
0x6e90   -> jsr 0x518e                     ; with the ring head
0x51ac   -> jsr 0x11c66 ; if result != 0 -> bsr 0x4d1a
0x4d46   move.l #-2, (0x18872)             ; mailbox opcode
0x4d50   move.l <node>, (0x18876)          ; mailbox pointer
0x4d58   -> bsr 0x4c26                     ; NOTIFY
```

and `0x4c26` is an interlock:

```
0x4c34   tst.w (0x18884)          ; notify-in-progress flag
0x4c3a   bne.b 0x4ca4             ; ALREADY SET -> skip the entire notify and return
         ... build and post the message ...
0x4c5a   move.w #1, (0x18884)     ; set the flag
```

**The card notifies the host only while `0x18884` is zero, and sets it to one immediately after.**
While it is set, every later attempt returns without notifying - and `RCVCOMPLETE` carries on
appending frames to `g_hostReadyRing` regardless, because nothing in the receive path checks it.

**Only one place clears it.** `0x18884` has exactly three references: the read at `0x4c34`, the set
at `0x4c5a`, and a single clear at `0x6ef6`. That clear sits in a routine which also clears the
mailbox words `0x18880`, `0x18872`, `0x18876`, sets `0x18868` to 1, and reports code `0x11` through
the same message path used by other host-command handlers. It has the shape of a HOST COMMAND
handler - the ND-100 telling the card "I have taken that message, re-arm" - not something the card
does on its own.

**Why this matters.** If the re-arm never arrives, the card delivers ONE message and then goes quiet
for ever, while the LANCE keeps accepting frames and the ready ring keeps growing. The observable
signature would be exactly what D100 showed on 2026-08-03: `accepted` climbing into the tens of
thousands, `missed(no buffer)` creeping up as pool nodes are consumed and never returned, and
SINTRAN silent - answering neither the CR nor the DR.

**NOT PROVEN.** Two things are still open and must not be assumed:
 - that `0x6ef6`'s enclosing routine really is a host-command handler, and which opcode reaches it.
   Its entry point and callers have not been identified yet.
 - whether our emulated ND-100 side and SINTRAN actually issue that re-arm. If SINTRAN issues it and
   our emulator drops or mishandles the command, the fault is ours; if SINTRAN never issues it, the
   model of the interlock is wrong somewhere.

This is the most specific mechanism found so far for "SINTRAN stops picking up data after a while".
Next: identify the entry point of the routine containing `0x6ef6`, find its command opcode, and then
check whether that opcode is ever seen on the host interface in a live session.

## 2g. THE COMPLETE RECEIVE MODEL - and why both failure modes are HOST-driven

Closing the receive carve. Two cells decide whether a received frame ever reaches SINTRAN, and the
ND-100 owns both of them.

**GATE3, the pool-enable flag (`0x188c6`).** The handler at `0x67da` is an ENABLE command, not a
per-buffer post:

```
lea (0x188c6),A0 ; tst.w (A0) ; beq -> 0x67fc          ; not enabled yet -> go enable
move.w (0x2,A0),D0 ; cmp.w (0x4,A1),D0 ; beq -> 0x67fc ; already enabled with the SAME parameter: OK
move.l #-0x11,(0xc,A1)                                  ; enabled with a DIFFERENT parameter: error -17
0x67fc: move.w (0x4,A1),(0x2,A0) ; move.w #1,(A0) ; clr.l (0xc,A1)
```

So it is idempotent - re-enabling with the same parameter succeeds, with a different one it returns
-17 and does NOT change anything. While the flag is zero `RCVCOMPLETE` discards every frame.

**The free pool (`rxpool_freehead`, `0x188ca`).** Six references, and they tell the whole story:
`0x552c` writes it at init; `0x5e0e`/`0x5e1e` are `RCVCOMPLETE` popping a node per delivered frame;
`0x51e4`/`0x51f0` are another pop path; and `0x6cfe` is inside the host POST-BUFFER command handler
at `0x6cee` - the ONLY place a node is pushed back.

**The cycle, therefore:** the ND-100 posts buffers, the card pops one per delivered frame, and the
ND-100 must keep posting or the pool runs dry. When it is dry `RCVCOMPLETE` bumps
`STAT_rxDroppedNoPoolNode` - the `missed(no buffer)` figure in `net stat`.

**Both ways receive can die are host-driven:**

 - the notify interlock of section 2f: the card notifies once and needs the ND-100 to clear
   `0x18884` before it will notify again;
 - the buffer pool here: the card consumes nodes and needs the ND-100 to post more.

Neither has a card-side timeout or self-recovery. That is a coherent explanation for a link that
runs fine and then goes deaf while the LANCE keeps accepting frames - and it points at the ND-100
side, which is either SINTRAN or our emulation of the ND-100-to-card memory path.

**WHERE THIS STOPS BEING ANSWERABLE ON PAPER.** Static reading cannot tell "SINTRAN never does it"
from "SINTRAN does it and our emulator loses the write". Those need a live trace. As of commit
`3ec9215d4` in RetroCore the cells are named and the mailbox lookup no longer truncates addresses
above 64KB, so a machine started on that build or later will log every ND-100 access to
`RXDLV_NOTIFY_BUSY`, `RX_POOL_COUNT` and `HOST_READY_RING` by name at Device level.

The measurement to make, on a freshly started machine:
 1. does the ND-100 ever CLEAR `RXDLV_NOTIFY_BUSY` after the card sets it?
 2. does the ND-100 keep issuing POST-BUFFER, or does `missed(no buffer)` climb one-for-one with
    arrivals once it stops?

Answer those two and the receive side is settled either way.

## 2h. FIRST LIVE TRACE - the 0x188xx block is NOT the host handshake

First trace taken with the named cells (RetroCore `3ec9215d4`), D100 on `debugtrace 2 4`, both
machines booted 23:52, trace from 23:54, link WORKING (a full `li-fi d102(sys).,,` listing returned
at 23:59).

**What the ND-100 actually touches**, counted over the window:

```
ENNS0_MON_COUNTER          972 R
ENNS0_MON_COUNTER2         648 R
ENNS0_STARTED_FLAG         648 R
ENNS0_MON_CODE             647 R
RX_POOL_COUNT              622 R
ENNS0_REQUEST              324 R
ENNS0_SUBFUNCTION          324 R
(a handful of startup writes to STAT_SEMAPHORE, CMD_BUFFER, ERR_CODE, FUNC_CODE, STAT_CODE)
```

**CORRECTION to sections 2f and 2g.** Those sections presented the `0x188xx` delivery block - the
notify-busy flag, the mailbox opcode and pointer, the host ready ring - as the handshake by which
the card hands a received frame to the ND-100, and suggested a stalled re-arm there could explain
SINTRAN going deaf. The live trace does not support that: **the ND-100 never reads or writes any of
those cells.** Not once, while the link was carrying traffic normally. So they are card-INTERNAL
bookkeeping between the firmware's own tasks, not the host interface. The reasoning in 2f and 2g
about the firmware side still stands as a reading of the code; the leap to "this is how SINTRAN is
notified" does not, and is withdrawn.

The host path is therefore something else - most likely DMA into ND-100 memory plus the SCIP
interrupt, with the low-memory postbox at card-DRAM `0x4C2` (the MBOXH activation queue, already
documented in the emulator and fed by the ringing primitive at firmware `0x2248`).

**Also do NOT read `ENNS0_STARTED_FLAG` as a health signal.** It read `0x0000` on all 324 polls
while the link was working. The emulator comment describing `0x4C0` as "firmware sets = 1 on valid
start = server ready" is either wrong or applies only to the diagnostic firmware. `ENNS0_MON_CODE`
read `1`, which is not among the documented codes 2, 3 or 4 either.

**What the trace DOES establish:**
 - `RX_POOL_COUNT` reads `0x0001` throughout - GATE3 is open, so frames are not being dropped for
   want of an enabled pool.
 - The ND-100 side is overwhelmingly a POLLING loop; it writes almost nothing after startup.

**Still to capture: the failure itself.** This trace covers a healthy link only. The failure appears
after an idle period, so the run has to continue: leave both machines idle, retry the listing, and
compare the same counts before and after. Only that comparison can show what changes.

## 2i. THE VENDOR MONITOR DECODED OUR OWN FRAME - 0x1F IS CC, VERIFIED

The ENCOS monitor on D100 (`trac`, server 0, `E`, then `R`) dumps a decoded NPDU trace to
`(SYSTEM)ENCOS-TRACE:DATA`, readable off the disk image with `ndtool`. With D19999 answering
connection requests, it printed:

```
268447 MESG in      from XGATE   ID   556416 send to ND19999 length   14
268447 NPDU out     to   ND19999 loc 11557 rem     0 CR from ND  100
268449 NPDU in      from ND19999 loc     1 rem 11557 CC from ND19999
268449 NPDU out     to   ND19999 loc     1 rem 11557 DR BY NS reason  1
268467 NPDU out R 1 to   ND19999 loc 11557 rem     0 CR from ND  100
   ... ten retries ...
268667 MESG out     to   XGATE   ID   556566 FAILED TRANSMIT to ND 19999
```

**VERIFIED: `0x1F` is the connection confirm.** We sent kind `0x1F` and ND's own software decoded it
as `CC from ND19999`. That is the vendor's decoder naming our frame, not us reasoning about it. The
low-nibble guess in section 2d - control frames end in `F`, data ends in `0` - is upheld for CC.

**VERIFIED: the two reference fields are ordered destination-then-source.** D100's outgoing CR reads
`loc 11557 rem 0` and the frame carries `0x0000` in bytes 5-6 with `11557` in bytes 7-8. It knows
its own reference and not ours, so:

 - **bytes 5-6 = the DESTINATION's reference** (zero when not yet known)
 - **bytes 7-8 = the SENDER'S OWN reference**

The C# property names `SenderLinkId` and `ReceiverLinkId` are therefore the wrong way round. They
have NOT been renamed yet - callers depend on them - but anything reasoning about them must use the
order above.

**CORRECTION to section 2b.** That section concluded D100 = `0x2F48` and D102 = `0x28A4` from the
`0x20`/`0x6F` pair. With the field order settled it is the reverse: **D100 = `0x28A4`,
D102 = `0x2F48`**. The old pair fitted both readings because it is symmetric; the CR's zero is what
breaks the tie.

**Also settled: the "counter" question from section 2b.** The field that stepped in lockstep with
the sequence is the sender's own reference, and it does move per connection attempt - `loc 11557`
then `loc 11558` on the next request. It is a reference, not a link id in the sense used earlier,
and not a sequence counter either.

**STILL FAILING: `DR BY NS reason 1`.** D100 refuses every confirm with reason 1, retries ten times
and gives up with `FAILED TRANSMIT`. A first attempt filled the two reference fields the other way
round; correcting them changed the refusal's shape - D100 now sends far fewer connection requests
and its disconnect carries `0001 0001` rather than `2D24 0001` - but the refusal remains. What
`reason 1` means is NOT known. The monitor prints it as a decimal, so the next step is to read the
trace again after the field correction and see whether the reason changed with it.

**Unrelated but worth recording:** the same trace shows the D100 to D102 link perfectly healthy at
that moment - `DT nr 61 length 128`, `AK nr 62 credit 15` - so acknowledgements carry a CREDIT
field, which is flow control this document has not looked at.

## 3. A THIRD kind: 0x0F - meaning UNKNOWN, name not earned

**Status 2026-08-03: the carve was attempted and did NOT reach an answer.** The heading below used
to read "the link open request". That was inference from behaviour, and behaviour is exactly what
this project has been burned by before, so the name is withdrawn. What follows is the observed
shape of the frame and a reading that fits it - nothing more.

What the carve DID establish, and these two are verified:

 - **The card firmware never touches the ND link header.** The ENCOS 68K image has zero occurrences
   of the header bytes, and `RCVCOMPLETE` reads only the MAC address, the 802.3 length and the LLC
   before handing the frame to the host. So `0x0F` is not the card's protocol and cannot be carved
   from `encos-ser-all-banks-68k.bin`.
 - **`0x0B` is not a magic number, it is the header LENGTH (11 bytes).** The 802.3 length field is
   always 3 (LLC) + 11 (header) + payload. This is why searching any binary for a fixed `0B02`
   constant finds nothing: the code writes a computed length. Every earlier search that assumed a
   magic constant was looking for something that does not exist.

Where the answer must be: the ND-100 side, in the COSMOS Ethernet driver (ENNS0). No standalone
load module for it has been found - it appears to live inside the SINTRAN image, so getting at it
means carving `BIGDISK0-K-100.IMG` first. That is a session of its own and is NOT blocking the
transport fixes.

```
0B 02 0F 00 74 0000 65D0 0066      from node 102
0B 02 0F 00 2C 0000 1C59 0064      from node 100 (an earlier session)
```

 - The 802.3 LENGTH field is `0x000E` = 14, i.e. 3 LLC bytes plus the 11-byte header and NOTHING
   else. The rest of the 60-byte frame is padding to the Ethernet minimum. **These frames carry no
   payload.**
 - The SENDER link id is **zero**. On every other frame it is the sender's real id.
 - The RECEIVER field is close to, but not equal to, the peer's real link id, and it **increments
   with each attempt**: 26064, then 26065, then 26066, while the peer's actual id stayed 26063.
 - The last field carries the SENDER'S NODE NUMBER - `0x0066` = 102 from D102, `0x0064` = 100 from
   D100 - not a payload length.
 - They are retransmitted about every 100 ms and, in this capture, were **never answered**.

**INFERRED, not proven, and NOT to be built on:** a reading where `0x0F` asks to open (or reopen) a
link fits every sample - a sender id of zero would say "I have no link yet", and the receiver field
would be the id being proposed, bumped on each retry. But no answer to a `0x0F` has ever been
captured, so the reply shape is UNKNOWN, and no code has been read that builds or parses one. Do
not implement against this reading. Carve ENNS0 out of the SINTRAN image first.

---

## 4. A FOURTH kind: 0x6F, sent on an ESTABLISHED link

```
0B 02 6F 00 76 582E 65CF 0101
0B 02 6F 00 77 582E 65CF 0101
```

 - Both link ids are the real, correct ones - unlike `0x0F`. So `0x6F` is sent by a node that
   already has a working link.
 - The 802.3 LENGTH is again `0x000E` = 14: header only, no payload.
 - The last field is `0x0101` on every sample.

Purpose UNKNOWN. It is NOT a length and NOT a node number.

---

## 5. The last field is not one field

`NdLinkHeader` calls the final 16-bit word `plen` unconditionally. Across the four kinds it is:

| Kind | Last field | Meaning |
| --- | --- | --- |
| `0x20` data | 14 .. 140 | the real payload length |
| `0x3F` ack | 0 | no payload |
| `0x0F` | `0x0064` / `0x0066` | the SENDER'S NODE NUMBER |
| `0x6F` | `0x0101` | UNKNOWN |

Reading it as a length on a control frame gives a nonsense value - 257 for `0x6F`, 102 for `0x0F` -
while the 802.3 length field says the frame has no payload at all. **The 802.3 length is the
authority on how many payload bytes are present.**

---

## 6. How the link dies, byte by byte

This is the failure that has been chased all night, and the capture shows it plainly.

```
03:07:52..57  node102 -> node100  kind0x0F seq=120 snd=0 rcv=26066   x33, unanswered
03:08:17      node100 -> node102  data     seq=126 snd=26063 rcv=22574
```

**The two ends disagree about whether the link exists.** D102 has torn its link down and is asking
to open a new one, retrying with a fresh proposed id each time. D100 never answers those requests
and carries on sending DATA on the OLD link ids as though nothing happened. Neither side recovers,
and SINTRAN reports `NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS CONNECTION ABORTED`.

So the missing piece is whatever should answer a `0x0F`.

---

## 6a. CORRECTION: D100 is NOT frozen, and route-through on Ethernet is CONFIRMED

Section 6 read the silence as D100 having stopped. A `net dump 20` taken from D100 during the
`0x0F` storm shows that is wrong. Among the ignored `0x0F` frames sits this, sent BY D100:

```
#590 TX  802.3 len=28
08 00 26 66 00 00 | 08 00 26 64 00 00 | 00 1C | A8 A8 03
0B 02 20 00 03 65CF 582E 000E
21 12 00 19 00 66 00 67 FF FF 00 02 DE 05
```

So D100 is alive, its firmware is running, and it is still transmitting. It simply does not answer
`0x0F`. **The failure is a link-state disagreement, not a crash**: D102 has torn the link down and
keeps asking to reopen it, while D100 believes the link is fine and ignores the requests.

The payload settles two things that were open:

**Route-through works on Ethernet.** Word 0 is `0x2112`, the RELAYED marker, not `0x2113`. The
endpoints are destination `0x0066` = 102 and source `0x0067` = **103**. D103 has no Ethernet - it
reaches the segment only over its HDLC line to D100 - so this is D103's datagram being relayed by
D100 onto Ethernet. `COSMOS-ETHERNET-TRANSPORT-FRAMING-2026-08-01.md` lists a relayed frame on this
transport as an open question and names this exact topology as the experiment. It is now answered:
the HDLC-derived relay rule holds unchanged on Ethernet.

**The word-6 checksum holds on a relayed Ethernet frame.** Checked by hand over the six header
words with end-around carry:

```
2112 + 0019 + 0066 + 0067 + FFFF + 0002 = 21FA
~21FA = DE05
```

The frame carries `DE 05`. Exact. That model had never been tested on a relayed frame on this
transport.

Flags 1 is `0xFFFF`, the broadcast/reachability value, consistent with a reachability announcement
being passed on.

**Also corrected: the burst theory is dead.** D100's `net stat` reports `accepted=340`,
`missed(no buffer)=0`, `filtered=0`, `rx-off=0` - every frame delivered and accepted, including the
four-frame burst that preceded the failure. Frame gaps of 7-9 ms were routine throughout the healthy
period, so that burst was not unusual. Nothing was dropped anywhere.

## 6b. ROOT CAUSE FOUND: the card's rx buffer pool runs dry (carved from ENCOS firmware)

`RCVCOMPLETE` at `0x5c42` in `encos-ser-all-banks-68k.bin` has THREE gates, not two. The third is
the one nobody had looked at:

```
if (addressOk == 0 || frameFormatOk == 0 || rxpool_count == 0)   -> DISCARD
else {
    node = rxpool_freehead;
    if (node == 0) { STAT_rxDroppedNoPoolNode++;  -> DISCARD }   // pool exhausted
    else { rxpool_freehead = *node; ... APPD to host_ready_ring }
}
```

**SCOPE LIMIT added 2026-08-03.** Pool starvation is real and was really measured in the first
failure, but it does NOT explain the SECOND failure on the same day. In that one D102 reported
`accepted=23961, missed(no buffer)=0` - nothing starved, nothing frozen, ~24000 packets moved
cleanly - and `li-fi` still returned silence. So starvation is one failure mode, not the failure
mode. See section 2b for what the second failure actually looks like on the wire.

**A frame is handed to the host ONLY if a free node can be taken from `rxpool_freehead`.** When that
pool is empty the frame is dropped INSIDE THE CARD, counted in `STAT_rxDroppedNoPoolNode`, and the
card carries on completely normally - it still transmits, still relays, still answers nothing.

That is the failure, and it explains every observation at once:

 - D100 kept transmitting all through the storm (frame `#590`, relaying D103) - an empty rx pool
   does not touch the transmit path.
 - D100 never answered a single `0x0F` because **it never saw one**.
 - `net stat` reported `accepted=340, missed=0` - and that is not a contradiction. That counter is
   the EMULATOR's, and it counts frames reaching the LANCE descriptor ring. Delivery to the host
   happens one step later, in firmware, and nothing in `net stat` observes it.
 - Identical failure on UDP and on TCP, because the transport was never involved.
 - HDLC unaffected - a different device entirely.

**The pool is refilled by the HOST, not the firmware.** `rxpool_count` is set only by the host
command opcode `0x12` handler (near `0x67da`); firmware init (`rxpool_init` at `0x5512`) leaves it
zero. So SINTRAN's driver hands receive buffers down to the card, and when it stops doing so - or
never hands down enough - the card silently starves and the link dies for good.

**Why every earlier measurement pointed elsewhere:** each one was taken at the wrong layer. The
transport counters proved delivery to the emulator. The LANCE counters proved delivery to the
descriptor ring. Neither can see a frame that the firmware accepts and then discards for want of a
buffer node.

### What to measure next

`STAT_rxDroppedNoPoolNode` and `rxpool_count` (`0x188c6`) live in card DRAM and can be read out of a
running machine. If `STAT_rxDroppedNoPoolNode` is climbing on D100 while the `0x0F` storm runs, this
is confirmed outright rather than inferred.

### What is NOT claimed

 - WHY the host stops replenishing the pool. It may be a driver bug in SINTRAN, a host command the
   emulated card never answers, or a buffer leak. Not established.
 - That the emulated card implements opcode `0x12` correctly. If our emulation mishandles the host
   command that refills the pool, the starvation would be OUR bug rather than SINTRAN's - that is
   the first thing to check, and it has NOT been checked yet.

## 6c. SCIP interrupt path audited - correct, but one mechanism remains

Audited `NDBusEthernetII.MemoryMap_OnNDInterrupt` (0x3B6) and the control-word path (0x63A). The
model matches the schematic and I found NO lost-interrupt bug:

 - a SCIP arriving while interrupts are disabled sets `scipPending` and is held, not dropped;
 - the RFT latch is deliberately NOT cleared by a control write, only by IDENT or Master Clear;
 - when the driver re-enables interrupts, a pending doorbell is re-asserted (line 1613-1626).

**But `scipPending` is a single flip-flop, not a counter** - correctly, because RFT on the real card
is one ALS74. So if two frames are delivered before the ND-100 answers the IDENT, the second
doorbell is ABSORBED by the already-set latch. That is faithful hardware behaviour, and it obliges
the host driver to drain the WHOLE ready-ring on each interrupt. A driver that takes only one
message per interrupt leaves the rest in the ring, never reposts their buffers, and the rx pool
leaks a node at a time until it is empty - which is exactly the starvation in section 6b.

**Why this would bite the emulator and not the real machine:** we deliver frames far faster than
10 Mbit Ethernet can. There is no wire time and no inter-frame gap - a frame goes from a loopback
socket into the descriptor ring immediately. Doorbell coalescing that would be rare on real hardware
becomes routine here. Same latch, same driver, completely different timing.

**HYPOTHESIS, not a finding.** SINTRAN's driver has not been read to see how many messages it drains
per interrupt. If it drains the ring fully, this mechanism is wrong and the leak is elsewhere.

Two ways to tell them apart, both cheap:

 - Watch PC `0x5ECA` on d100 (already in the firmware PC watch list). It fires on exactly the
   "pool enabled but starved" discard.
 - Count deliveries against IDENTs. If deliveries outnumber IDENT answers, doorbells are being
   coalesced and the driver is losing messages.

## 7. What this rules out

The same failure appeared on **two independent transports**:

 - UDP multicast: `net stat` showed D100 TX 251 packets / 17082 bytes and D102 RX 251 packets /
   17082 bytes - equal to the byte, both directions. Nothing was lost.
 - The TCP hub: zero dropped frames of any kind for the whole session, no member ever
   disconnected.

**CORRECTION 2026-08-03: "the transport is not the cause" was wrong.** The reasoning above only
shows that no frame was lost *once it was on the wire*. It says nothing about frames never handed to
the wire in the first place. Four real defects were then found and fixed in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\Common\Network\UdpEthernetBackend.cs` (commit
`dc224296e`): delivery ran on the receive thread so one slow consumer stalled the whole segment; no
explicit multicast interface; no frame counters; no socket teardown/restart. `TcpEthernetBackend.cs`
got the same delivery fix. After those fixes, a machine restart, and COSMOS started on D102,
`li-fi d102(sys).,,` from D100 returned a full remote directory over the TCP hub - the first working
cross-machine file access since the failure.

What survives from this section: the emulated card's receive filter is not the cause - D102 reported
`filtered(wrong MAC)=0` and `rx-off=0`. What D102 did report was `accepted` frozen at 223 while
`missed(no buffer)=28` then 50 climbed one-for-one with every arriving frame, and its TX frozen at
108 - both directions stopping at the same moment, which is a firmware that has stopped, not a ring
that is merely full.

---

## 8. What is NOT claimed

 - That `0x0F` is definitely "open a link". It fits every sample; no reply has ever been seen.
 - What `0x6F` is for, or what `0x0101` means.
 - Why the receiver field on `0x0F` is the peer's id plus a small increment rather than the id
   itself, or what value a correct requester should put there.
 - Why D100 does not answer. It may not process `0x0F` while it believes a link is up, or it may
   not have received them - though the hub delivered every frame to it.
 - Anything about a LANCE `RINT`-on-MISS defect. **WITHDRAWN 2026-08-03.** There was never a defect.
   Our code set `RINT | INTR` on a missed packet; I argued that was wrong and changed it to
   `MISS | ERR | INTR`. MAME's `am79c90.cpp` `recv_complete_cb` disproves it: the `-2` (missed
   packet) case does `break`, NOT `return`, and falls through to `m_csr[0] |= CSR0_RINT | CSR0_INTR`.
   Real MAME sets RINT on a miss too. Our original code already matched the oracle, so the change was
   reverted (`git checkout --` on
   `E:\Dev\Repos\Ronny\RetroCore\Nuget\HackerCorpLabs.Emulation.Chips.Amd\src\Am7990Lance.cs`).
   The reasoning died with it: MAME derives `INTR == BABL || MISS || MERR || RINT || TINT || IDON`,
   so MISS alone raises INTR anyway - the driver was interrupted either way, and the
   "woken to look at an unchanged ring" argument would apply to MAME just as much, and MAME works.
