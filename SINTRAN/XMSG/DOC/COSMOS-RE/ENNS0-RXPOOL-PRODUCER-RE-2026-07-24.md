# ENCOS Ethernet-II (PCB 3094) 68K firmware - RX-pool producer + TX dst-MAC RE

Date: 2026-07-24
Program (Ghidra, ACTIVE): `encos-ser-all-banks-68k.bin`  (Raw Binary, 68000:BE:32)
Anchor confirmed: `get_disassembly ram:5c42` returns real 68K code (RCVCOMPLETE). This is the
68K controller firmware, NOT an ND-100 BPUN image. All edits below were applied only to this program.

This report supersedes the earlier blocker note.

---

## Executive verdicts (A1 / A2)

### A1 - RX pool producer

A non-empty rx pool is NOT guaranteed by firmware init. It REQUIRES the ND-100 host to post
buffers via a command.

- `rxpool_init` (0x5512) is the one-time init and ZEROES the whole control block at 0x188c6:
  count (+0), size param (+2), freehead (+4), tx list (+8). So after init the pool is DISABLED
  (count=0) and EMPTY (freehead=0).
- Two distinct host commands are needed at run time:
  1. ENABLE - handler at 0x67da sets `rxpool_count` (0x188c6) = 1 and stores a buffer-size param at
     0x188c8. This flips gate3 on, but supplies NO buffers.
  2. POST-BUFFER - host command opcode `0x12`, dispatched at 0x6cee, is the ONLY code path that
     PUSHES a buffer node onto `rxpool_freehead` (0x188ca = 0x188c6+4), via `PLANC_APPD` (0x134e6).
- RCVCOMPLETE (0x5e0e) and the drain routines only POP from the freehead. Nothing else pushes.

Consequence for the observed drops: if the host enables the pool but posts too few / no buffers,
gate3 (count!=0) passes yet the freehead is null, so RCVCOMPLETE takes the no-buffer path at 0x5eca
(bumps a miss counter at `nma_stats_block`+0x14) and DISCARDS the frame with no SCIP.

### A2 - Transmit dst MAC source

The destination MAC in an outgoing 802.3 frame is NOT built by this firmware. It is copied verbatim
as part of a host-supplied frame buffer.

- The TRANSMIT command handler (dispatch target at 0x6b9e) receives a buffer that already contains
  the fully-formed Ethernet frame (dst[6]+src[6]+len[2]+payload) placed in shared DRAM by the ND-100
  host driver. The firmware only VALIDATES: frame-type long==1; header length >=14 when
  `mode_include_length_field` (0x1888a) is set else >=12; total length <=1500 (0x5dc). It never
  assembles or rewrites the dst MAC.
- It then calls `lance_tx_enqueue` (0x6c1c): append the buffer to `txpending_list` (0x188ce) via
  `PLANC_APPD` and kick the LANCE. `XMTCOMPLETE` (0x61d2) later reports TX status; it also never
  touches the dst MAC.
- Corroboration: the ND OUI bytes `08 00 26` appear NOWHERE as a literal in the 512KB image. The
  only two byte-matches (0x131be, 0x6bac8) are incidental instruction/parameter bytes, not a MAC
  template. The firmware therefore cannot be synthesising `08:00:26:...`.

CONCLUSION: the bad dst `08:00:26:B8:45:00` (should be `08:00:26:<sysno-LE16>:00`) originates in the
ND-100 host frame-build (the ENNS0 / driver side), not in the 68K firmware. Prime suspect moves
back to the host driver's header assembly.

---

## Data structures (renamed in Ghidra)

Control block base 0x188c6 (`rxpool_count`):
- 0x188c6 `rxpool_count`      word  - pool ENABLE flag (0/1), not a running count
- 0x188c8  (+2)                word  - host buffer-size param (compared in several handlers)
- 0x188ca `rxpool_freehead`   long  - head of rx free-buffer list (node.next at node+0)
- 0x188ce `txpending_list`    long  - buffers handed to LANCE TX (rxpool_count+8)
- 0x188d2 `txretry_list`      long  - TX retry/holding list
- 0x188d6 `host_ready_ring`   long  - frames/completions handed up to the ND-100 host

Other:
- 0x18000 `lance_rx_ring_ctrl` - LANCE RX ring control (word0 counter, word4 index mod 0x80, base+8)
- 0x18408 `lance_tx_ring_ctrl` - LANCE TX ring control
- 0x1888c `nma_stats_block`   - NMA statistics counters (58/0x3a bytes; +0x14 = rx miss counter)
- 0x18848 `host_cmd_ring`     - incoming host command message ring
- 0x18834 `host_resp_mailbox` - response context used by the command tail
- 0x189e0 `host_cmd_jumptable`- runtime-built opcode dispatch table (index = opcode*4)
- 0x1888a `mode_include_length_field` - 0=raw/no length, 1=802.3 with dst+src+length header
- 0x18866 / 0x18880 `station_state_flag_*` - station/adapter state gates

---

## RX path walk (RCVCOMPLETE 0x5c42)

Coroutine style; each leg ends `jmp (A5)`; per-frame loop re-enters at 0x5c54.

- 0x5c54..: read LANCE RX descriptor (ctrl 0x18000, base+8, index word+4, stride 8); OWN bit15 check.
- gate1 (0x5cc8 `btst #14`): 802.3 length-framed class vs DIX/other.
- length classification sets flag@frame+0x36 (checked by gate2 at 0x5dfa) and flag@0x38 (dst-addr
  match, from the 6-byte compare at 0x5d7e against station addr @0x1885e / broadcast).
- gate2 0x5dfa: `tst.w (0x36,A6)` must be set (length-framed) else discard.
- gate3 0x5e02: `tst.w rxpool_count` must be non-zero (pool enabled) else discard.
- rxpool_alloc 0x5e0e: pop head from `rxpool_freehead`; if null -> 0x5eca no-buffer stats path
  (bump `nma_stats_block`+0x14) -> discard. rxpool_count is NOT decremented (enable flag only).
- fill 0x5e24..0x5ec4: set buffer flag word (0xa) = (x & 0x3ff)|0x4c00, len fields, and copy the
  frame's dst MAC + src/len into the buffer descriptor.
- 0x601a `tst.l (0x18,A6)`; bne 0x6020 DELIVER: `A0=&host_ready_ring; jsr PLANC_APPD`.
- 0x6034 DISCARD: re-append LANCE rx descriptor (bsr 0x5b60), no SCIP.
- Note UNVERIFIED: the INT12/SCIP doorbell (EF_0080..EF_009F) is not emitted inside PLANC_APPD or
  visibly in the deliver leg; it must be raised elsewhere. Not located in this pass.

## Host command dispatch (host_cmd_dispatch 0x6aca)

- Dequeue next command node from `host_cmd_ring` (0x18848) via 0x514a; loop until empty, then flush
  accumulated completion list (frame 0x1c) to `host_ready_ring`.
- opcode = (cmd.byte[0xa] >> 2) & 0x3f; validates pool/station state; dispatch via
  `host_cmd_jumptable` (0x189e0) at 0x6b8c/0x6b9c.
- Result/response code written back into cmd.word[0xa] low byte via `ori.w #0xNN00`
  (seen: 0x1c,0x24,0x2c,0x34,0x3c,0x44,0x4c,0x54,0x5c).

Enumerated handlers (dispatch legs):
- 0x6b9e  TRANSMIT (see A2) -> lance_tx_enqueue 0x6c1c -> append to `txpending_list`.
- 0x6cee  opcode 0x12  POST-RX-BUFFER -> append to `rxpool_freehead` (the A1 producer).
- 0x67da  ENABLE-RX-POOL -> rxpool_count=1, size param @0x188c8.
- 0x6820  variant check of size param vs pool word2 (err -0x11 mismatch).
- 0x68ea  SET/COPY 6-byte MAC from cmd param into a work buffer (+4); guarded by 0x18866.
- 0x69be / 0x69a4  station-address / list-manipulation family (worker 0x53ec).
- 0x6a2c  READ-STATS -> copy 0x3a bytes of `nma_stats_block` to a host buffer (guard len>=0x3a).
Common tail 0x6aa8 -> 0x518e posts the response (host_resp_mailbox 0x18834).

## TX-complete + drains

- XMTCOMPLETE 0x61d2: pop `txpending_list`, decode LANCE TX status, update `nma_stats_block`,
  deliver buffer to `host_ready_ring`.
- drain_freehead_to_readyring 0x51d0, drain_txlists_to_readyring 0x5264,
  move_txretry_to_txpending 0x617a: housekeeping that walks the pool sub-lists / retry list and
  delivers or re-queues via PLANC_APPD. None of these push fresh buffers to `rxpool_freehead`.

---

## Primitives

- `PLANC_APPD` (0x134e6) - generic singly-linked TAIL append. A0=&head, A1=node, D0w=next-link
  offset in node. No doorbell. (Was already named PLANC_APPD - confirms it is the PLANC runtime
  list primitive, i.e. "deliver" is just a list append.)
- `PLANC_REMV` (0x13500) - generic list remove.

---

## Ghidra changes applied (this session)

Data labels renamed: rxpool_count, rxpool_freehead, txpending_list, txretry_list, host_ready_ring,
nma_stats_block, host_cmd_ring, host_resp_mailbox, host_cmd_jumptable, mode_include_length_field,
station_state_flag_18866, station_state_flag_18880, lance_rx_ring_ctrl, lance_tx_ring_ctrl.

Functions renamed/created: rxpool_init (was clear_struct_188c6), drain_freehead_to_readyring
(0x51d0), drain_txlists_to_readyring (created 0x5264), move_txretry_to_txpending (0x617a),
host_cmd_dispatch (created 0x6aca). PLANC_APPD/PLANC_REMV were already named.

Comments added: plate comments on RCVCOMPLETE, XMTCOMPLETE, rxpool_init, host_cmd_dispatch,
PLANC_APPD; PRE comments at gate3 (0x5e02), rxpool_alloc (0x5e0e), DELIVER (0x6020), DISCARD
(0x6034), ENABLE-RX-POOL (0x67da), lance_tx_enqueue (0x6c1c), TRANSMIT/dst-MAC verdict (0x6b9e),
POOL PRODUCER opcode 0x12 (0x6cee).

Large undefined code regions in the 0x5dd2-0x5f2x, 0x5ffc-0x6048, 0x67da-0x6da4 ranges were
force-disassembled to recover the RX/TX/command logic (the coroutine `jmp (A5)` style had left them
as raw data).
