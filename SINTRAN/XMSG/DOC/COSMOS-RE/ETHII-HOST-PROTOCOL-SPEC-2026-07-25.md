# ND Ethernet II card - host protocol spec (for the C# HLE) - 2026-07-25

Reverse-engineered from the MC68000 ENCOS firmware (`encos-ser-all-banks-68k.bin`, 68000
big-endian, 152 functions) via Ghidra. This is the contract the native C# high-level-emulation
controller (`NDBusEthernetIIHle`) reimplements, and the reference for the later nd100x C port.

Convention: **[V]** verified from firmware disassembly (Ghidra address cited). **[U]** unverified /
inferred - flagged for live-trace or deeper RE before relying on it.

All DRAM offsets are 68K-address-space offsets into the 512 KB card DRAM. In the emulator that maps
to ND-100 physical byte `PhysicalPageStart (0x0080_0000) + offset`. Words are big-endian.

---

## Two separate host <-> card channels (do NOT conflate)

1. **Low-DRAM mailbox / monitor postbox** `0x404-0x4c0` - boot/init + supervisory channel. Fixed
   word cells, request-word dispatch, drives the SCIP doorbell + STOP-wait scheduler.
2. **High-DRAM command ring + buffer pool** `0x18000+` - the per-packet networking channel
   (TX / POST-RX-BUFFER / ENABLE-POOL / SET-ADDR / SET-MODE / READ-STATS). Linked-list nodes
   dispatched through a runtime jumptable.

---

## 1. Command tables

### 1a. Mailbox request table (low DRAM) - dispatcher `0x1b40`, jumptable `0x512` [V]
Scheduler reads the **request/subfunction word at `0x408`**, validates range 0..5, dispatches via
`mailbox_req_jumptable_0_5` @ `0x512` (`jsr 0x512[req*4]`). Requests 0 (START) / 1 (STOP) are also
special-cased above the table.

| req @0x408 | handler | meaning |
|---|---|---|
| 0 | `0x1b58` START path | INITIATE: if `0x4c0==0`, save context, arm RTC, set STARTED |
| 1 | `0x1c1a` | STOP/TERMINATE: `clr 0x4c0`, postbox=1, signal SCIP |
| 2 | `0x1c38` | common handler leg (shared 2/3/4) **[U] body not fully traced** |
| 3 | `0x1c38` | (same) [U] |
| 4 | `0x1c38` | (same) [U] |
| 5 | `0x1c48` | separate leg [U] |
Range violation -> postbox code `-2` (`0x1bfc`).

### 1b. Networking command ring (high DRAM) - dispatcher `host_cmd_dispatch` @ `0x6aca` [V]
Loop dequeues command nodes from **`host_cmd_ring` @ `0x18848`**. Per node:
- **Opcode = `(node.byte[0xa] >> 2) & 0x3f`** (`0x6b72`, re-derived `0x6ce4`/`0x6d24`).
- Pre-validated against pool/station state (`0x188c6` count, flags `0x18866`/`0x18880`) at
  `0x6b08-0x6b64`; error codes `-0x10/-0x11/-0xa/-0x8`.
- Dispatched through runtime-built jumptable `0x189e0` indexed by `opcode*4`. Table lives in BSS and
  is populated at runtime, so opcodes are enumerated from handler legs, not the table.

| Opcode | Handler | Action | Completion code -> `word[0xa]` |
|---|---|---|---|
| **0x12 POST-RX-BUFFER** | `0x6cee` (fast-path `0x6c6a`) | Append host buffer node to `rxpool_freehead` @ `0x188ca` via `PLANC_APPD` @0x134e6. Sole producer of RX pool buffers. If pool count>0, buffer goes straight to the LANCE receive ring via `RCVRINGAPPEND` @0x5b60. | `0x4c00` ok / `0x5400` |
| **TRANSMIT** (node.long[0x14]==1) | `0x6b9e` | Validate type==1, hdr len (>=14 if mode_8023 `0x1888a` set else >=12), total <=1500. If TX enabled (`0x18408`>0): append to `txpending_list` @ `0x188ce`, then `XMTRINGAPPEND` @0x6054 builds the LANCE TX descriptor + kicks the chip. Else append to `txretry_list` @ `0x188d2`. **Host pre-builds the whole frame incl. dst/src MAC + length; firmware does NOT assemble the dst MAC.** | `0x4400`; err `-0x15` bad type / `-0x17` bad hdr / `-0x16` too long |
| **ENABLE-RX-POOL** | `0x67da` | If pool not enabled: store negotiated buffer size `node.word[0x4]` into `0x188c8`, set `rxpool_count` (`0x188c6`)=1. If already enabled, size must match else `-0x11`. | `0x2400` |
| **SET-MODE-FLAGS** | `0x6786` | Copy `node.word[0x14/0x16/0x18]` -> `0x18886` / `0x18888` (promisc) / `0x1888a` (mode-8023); restart MA (`STARTMA` @0x5850). | `0x1c00` |
| **SET-STATION/GROUP-ADDR** | `0x6880`/`0x68a8`->`0x542c` | Validate flag bit, program group/station addr via `maybe_handle_group_address` @0x542c; copies the 6-byte MAC from the command param. | via `0x542c` |
| **READ-STATS** | `0x6a2c` | Copy 0x3a (58) bytes of `nma_stats_block` @ `0x1888c` into host buffer at `node+0x16` (base `(A2)`, end `+4`, offset `+6`); needs >=0x3a room else `-0x18`. | `0x5c00` |

Completed nodes are appended and flushed to **`host_ready_ring` @ `0x188d6`** at loop end, then SCIP.

---

## 2. DRAM control-structure map

### Low mailbox region (word cells) [V]
| Off | Sz | Dir | Meaning |
|---|---|---|---|
| `0x404` | word | card | **Firmware-alive signature = `0x5473` ('St')** (`0x1cf4`) |
| `0x406` | word | host | REQUEST base ptr; cleared 0 at reset (`0x1d1c`) |
| `0x408` | word | host | **Request/subfunction word** (0..5) (`0x1bda`) |
| `0x40a` | word | card | Postbox counter A - inc on every SCIP (`0x1a50`) |
| `0x40c` | word | card | Postbox status/response code (1,2,3,4,-2,-4,0x1f,0x2a) |
| `0x40e` | word | card | Postbox second param word |
| `0x410` | word | card | Postbox counter B - inc with A (`0x1a54`) |
| `0x412` | word | card | Monitor flag, set=1 by `nd_monitor_set_flag` @0x1a30 |
| `0x454` | 0x4c | card | Full 68K register/context save area (`movem` @0x1d60) |
| `0x4ba` | long | card | Warm/running marker = `0x55555555` when running |
| `0x4be` | word | card | Reset/restart counter |
| **`0x4c0`** | word | card | **STARTED flag: 1 = started, 0 = stopped** (written 1 @`0x1db0`/`0x1e1a`, cleared @`0x1c1a`, tested `==0` @`0x1b4e`/`0x1bb2`) |
| `0x500` | long | card | Saved SP / resume ctx ptr for STOP-wait scheduler |

**CORRECTION:** prior note "0x4c0 STARTED = 0x0EAA" is **NOT confirmed** by the firmware. Firmware
writes `0x4c0 = 1`/`0` and only tests `==0`. `0x0EAA` appears only inside XMP* code, never as a store
to `0x4c0`. If `0x0EAA` is real it is a host-written magic the firmware does not test in the read
paths. **[U]**

### High networking region (`0x18000+`) [V]
| Off | Sz | Dir | Meaning |
|---|---|---|---|
| `0x18000` | long | shared | TX-enable/control region; 4-byte aligned (init asserts) |
| `0x18408` | long | shared | TX-enable flag; >0 -> transmit allowed |
| `0x1885c` | word | card | Station addr low word, from config `0x64e` |
| **`0x1885e`** | 6 B | host/card | **Station MAC address** |
| `0x18848` | ptr | shared | **`host_cmd_ring`** head |
| `0x18864/66` | word | card | Station-state flags init 1/0 |
| `0x18880/82` | word | card | Station-state flags init 0/1 |
| `0x18886` | word | host | Mode word (SET-MODE), init 4; value 4 enables the 60-byte TX pad (0x6110) |
| **`0x18888`** | word | host | **Address-filter enable**, init 1. See CORRECTION below - this is NOT a promiscuous flag |
| **`0x1888a`** | word | host | **Length-field / framing mode**, init 1. See CORRECTION below |

**CORRECTION (2026-07-25, Ghidra RE pass):** the two mode-word descriptions above were wrong or
incomplete. Full decode in
[ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md](ETHERNET-II-FEASIBILITY-AND-MODE-WORD-RE-2026-07-25.md).

- **`0x18888` polarity was INVERTED.** It is not "promiscuous". Nonzero = address filtering
  ENFORCED; zero = promiscuous. Evidence: `0x5d8c tst.w 0x18888 / bne 0x5d9c` - on a unicast MAC
  mismatch the NONZERO case branches to the reject block, while the zero case falls into
  `move.w #1,(0x38,A6)` = accept. Same polarity on the group path at `0x5dde`. Init 1 therefore
  means filtering is ON, i.e. NOT promiscuous. Renamed `g_addressFilterEnable` in Ghidra.
- **`0x1888a` is a "is there a length field" switch, not an 802.3-vs-DIX selector.** Nonzero =
  14-byte header, RX requires bytes12-13 == payload length (`0x5d20`), TX writes the length word
  (`0x60aa`). Zero = 12-byte header, RX length check SKIPPED ENTIRELY (`0x5d0a`), TX writes no
  length word (`0x60bc`) so the host owns bytes 12-13. Mode 0 is thus a full raw pass-through and
  is what makes Ethernet II / TCP-IP possible with no firmware patch. It also changes what the host
  is shown on receive (hdrlen 14 vs 12, `0x5e7c`). Renamed `g_mode8023LengthField` in Ghidra.
| `0x1888c` | 0x3a | card | `nma_stats_block` (READ-STATS source) |
| **`0x188c6`** | word | card | **rxpool_count** (0=disabled, 1=enabled) |
| `0x188c8` | word | shared | rxpool negotiated buffer size |
| **`0x188ca`** | ptr | shared | **rxpool_freehead** - POST-RX-BUFFER appends here |
| `0x188ce` | ptr | shared | **txpending_list** |
| `0x188d2` | ptr | shared | **txretry_list** |
| **`0x188d6`** | ptr | card | **host_ready_ring** - completions delivered here |

**Correction to prior notes:** `0x188c8` (= `0x188c6`+2) is the **RX buffer size**, not a
station-address cell.

### Command-node field layout (from handler code) [U - partly inferred]
| Field | Meaning |
|---|---|
| `node.byte[0xa]` | opcode word: bits 2-7 = opcode `(>>2)&0x3f`; top bits overwritten with completion code |
| `node.long[0xc]` | result/error long (negative error codes -0x11..-0x19) |
| `node.long[0x14]` | node type / param (TX requires ==1) |
| `node.word[0x4]` | buffer-size / station param |
| `node.[0x16..]` | descriptor (stats/MAC): base ptr, end word +4, offset +6 |
| `node.[0x18]` | TX frame descriptor: header len +6, total len +8 |

The exact linked-list node header (next-pointer offset, how `PLANC_APPD` @0x134e6 threads
freehead/ready-ring/txpending) is **[U]** - the single biggest gate for reimplementing the command
channel. A focused follow-up RE of `PLANC_APPD` + `posi_getall_wrapper` @0x514a is required.

---

## 3. Host register / doorbell contract (68K side) [V] / ND-100 side [U]
Firmware touches two memory-mapped board registers:
- **`0xEF0080`** - write byte 1 to raise the SCIP/INT12 doorbell to the ND-100
  (`post_and_signal_nd100_scip` @ `0x1a5c`). Only outbound-interrupt trigger.
- **`0xEF0040`** - write byte 0 at reset (`0x1d84`); board/interrupt control or ack-enable.

Incoming ND-100 -> card doorbell arrives as a 68K MFP/GPIP interrupt (`nd_host_interrupt_handler`
@ `0x250e`), acks MFP, scans flag table `0xb56[]`, `trap #2 D0=9` to wake the addressed coroutine.

**[U]** ND-100-side IOX status/control bit numbers (status bit2 = INT12/SCIP, bit0 = SCIP-enable;
control bits SCIP-enable/ND-interrupt/reset/halt) live in the ND-100 DRIVER, not the 68K image.
Confirmed independently by the emulator's working 68K controller register model (ND-12.055.1).

---

## 4. SCIP-raise conditions [V]
All go through `post_and_signal_nd100_scip` @ `0x1a48` (inc `0x40a`+`0x410`, then `0xEF0080=1`):
| Site | Trigger | Card state |
|---|---|---|
| `0x1dca` reset | Boot complete | `0x4c0=1` STARTED, `0x412=1`, postbox `0x40c=3` |
| `0x1e30`/`0x1e1a` | Self-test / re-started report | `0x4c0=1`, `0x40c=2`, `0x40e=0x2a` |
| `0x1bce` | STOP when already stopped | `0x40c=-4` |
| `0x1c0c` | Mailbox request completed | `0x40c` = handler result |
| `0x1c2e` | STOP acknowledged | `0x4c0=0`, `0x40c=1` |
| ready-ring path (`~0x6020`) | Networking completion (RX delivered / TX done / stats / error) flushed to `host_ready_ring` | node `word[0xa]` completion code, `long[0xc]` result |

---

## 5. Init / station-address handshake [V]
**Reset `reset_entry` @ `0x1cfe`:** save ctx->`0x500`; `0x40e=1`,`0x40c=0`,`0x406=0`; warm check
`0x4ba==0x55555555`; cold: build save area `0x454`, `0xEF0040=0`, install vector; `init_mfp_registers`
@0x396a; `maybe_startup_check_406` @0x1c6a writes **signature `0x5473`->`0x404`**; set **STARTED
`0x4c0=1`**, `0x412=1`, `0x40c=3`, raise SCIP; save SP->`0x500`, `stop #0x2500` (wait IPL5).

**Networking bring-up `LNMAINIT_main` @ `0x6ece`:** init station-state words
(`0x18864=1,66=0,82=1,80=0,68=1`); `PIOCOS`; assert `0x18000`/`0x18408` 4-byte aligned else FATAL -5;
`ASRCONNECT`; `rxpool_init` @0x5512; `init_rcvring`; `append_rx_buffers_to_ring` @0x5bca; station MAC
config `0x64e`->`0x1885c`, `LNNDTOMAAPPEND` @0xf05a with MAC ptr `0x1885e`; set `0x18888=1` promisc,
`0x18886=4`, `0x1888a=1` mode-8023; **`STARTMA` @0x5850** enables the LANCE MA engine.

**Per-command runtime:** host writes a command node into `host_cmd_ring` @0x18848, rings the
ND->card doorbell; firmware wakes `host_cmd_dispatch`; POST-RX-BUFFER grows the pool at `0x188ca`;
ENABLE-RX-POOL sets `0x188c6=1`; completions flush to `host_ready_ring` @0x188d6 and raise SCIP.

---

## 6. Linked-list node layout + ring threading [V] (RESOLVED 2026-07-25, second RE pass)

**PLANC_APPD @0x134e6** = generic singly-linked TAIL append. Args: `A0` = address of the head-pointer
cell (`&head`), `A1` = node, `D0.w` = byte offset of the `next` link. **Every** software list passes
`D0=0`, so **`node->next` is a `long` at offset 0** of every node (command ring, rxpool free list,
txpending, host_ready). No tail pointer (walks to tail). Pure pointer surgery - no SCIP/counter.

**Command mailbox `host_cmd_ring` @0x18848** is a "posi" struct, NOT a bare pointer:
- `+0` word = magic `0xAAAF`; `+4` long = chain head. `POSIGETALL` @0x1199e checks the magic and
  **steals the whole chain** (returns `+4`, clears it). `host_cmd_dispatch` @0x6aca then walks it via
  `next`@0.

**Command / delivery NODE field map** [V] (confirmed vs handlers + the RX node RCVCOMPLETE builds):
| Off | Type | Field |
|---|---|---|
| `0x00` | long | **next** (list link) |
| `0x04` | word | param (requested rx buffer size) |
| `0x0a` | word | **opcode/completion**: opcode = bits[15:10] (`hibyte>>2 & 0x3f`); completion ORs `0x4c00` ok / `0x5400` err into top 6 bits, low 10 bits = seq/id preserved |
| `0x0c` | long | result/status |
| `0x14` | long | type (1 = frame) |
| `0x18` | long | **descriptor.base** -> pool buffer (frame data; NOT inline in node) |
| `0x1c` | word | descriptor.len (2's-comp) |
| `0x1e` | word | descriptor.hdrlen (0xe Ethernet II / 0xc 802.3) |
| `0x20` | word | descriptor.totallen |
| `0x22` | 6 B | dst MAC |
| `0x28` | 6 B | src MAC (header end 0x2e) |

**rxpool @0x188c6:** `+0` word count/availability gate, `+2` word buffer size, `+4` long freehead
(`=0x188ca`, next@0). Pop: `node=freehead; freehead=node->next; clr node`. Push (POST-RX-BUFFER):
`PLANC_APPD(&0x188ca, node, 0)`.

**host_ready_ring @0x188d6:** bare long head, tail-append. Despite the name it is a plain FIFO
linked list, NOT a counted ring. `txpending_list @0x188ce` = bare long head. `txretry_list @0x188d2`
= structurally a bare long head but its producer/consumer were **[U]** not decoded.

**RCVCOMPLETE @0x5c42 (RX, ZERO-COPY):** reads the LANCE hw RX descriptor (rings at `0x18000` RX /
`0x18408` TX - the counted 128-entry AMD DMA rings, `+0` count / `+4` idx mod 128, 8-byte descriptors
with OWN `0x8000`, 2's-comp BCNT); length = `-(desc+4)`; gate2 802.3 len = `(desc+6)&0xfff - 4`.
gate1 dst-MAC vs `0x1885e` (+ broadcast via `0x18888`); gate3 pool `tst.w 0x188c6`; on pass **pops a
free buffer from rxpool** and builds the delivery node IN that buffer (`word[0xa]|=0x4c00`,
`long[0x14]=1`, descriptor.base = LANCE data ptr - NO memcpy, len/hdrlen/total, dst/src MAC), then
`PLANC_APPD(&host_ready_ring, node, 0)` and SCIP. Pool empty -> recycle descriptor, drop.

**TX (XMTRINGAPPEND @0x6054):** writes the 14-byte Ethernet header into the pool buffer at
`base-0xe` (802.3: `base-0xc`), hands `base-0xe`/BCNT to the LANCE hw TX ring, kicks the chip
(`0xEF00A0 = 0x48`). So a TX buffer reserves header room BEFORE `descriptor.base`.

Note: RCVRINGAPPEND/XMTRINGAPPEND drive the **AMD LANCE hardware descriptor rings** (0x18000/0x18408),
which are DISTINCT from the software head-cell lists above.

## Open items (remaining)
- **[U] txretry_list @0x188d2** producer/consumer not decoded.
- **[U] rxpool.count decrement site** (pop path does not decrement +0).
- **[U] mailbox legs `0x1c38` (req 2-4) and `0x1c48` (req 5)** bodies.
- **[U] `0x0EAA`** - not confirmed anywhere; contradicts the `0x4c0=1` STARTED convention.
- **[U] ND-100 IOX bit numbers** - taken from the working 68K controller model, not this image.

## Source
Ghidra program `encos-ser-all-banks-68k.bin` (active). Other loaded program `po-pwrfail-a00.prog`
(ND-100 executable) is inactive. Ghidra annotations added this session: renamed `0x512` ->
`mailbox_req_jumptable_0_5`; EOL comments at `0x1a5c` (SCIP doorbell), `0x1cf4` (signature),
`0x1db0` (STARTED=1).
