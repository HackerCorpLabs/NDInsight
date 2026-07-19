# CARVE ANSWER - Octobus own-station number: read from INPUT STATUS (+2) bits 13:8, statically

Answers `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-OCTOBUS-STATION-NUMBER-2026-07-18.md`
(how does SINTRAN/TPE read and validate the ND-100 octobus own station number).

Sources of truth:
- `D:\ND\S\testprog\x\octobus-b00.test` (TPE OCTOBUS B00), full fresh disassembly
  this session via nd100-dis. File-to-VA mapping **file word offset = VA - 0x6B00**,
  double-validated against two independently known words from the 2026-07-16 carve
  (magic 0x71C7 at VA 0xd080 = file word 0x6580; station mask 0x3F00 at VA 0xd3cb =
  file word 0x68cb). All `ram:xxxx` addresses below are the same hex VAs as the
  Ghidra project.
- `SINTRAN\NPL-SOURCE\NPL\PH-P2-OPPSTART.NPL` (OCSTART, CH5CPUPRESENT),
  `MP-P2-N500.NPL` / `RP-P2-N500.NPL` (5STATION usage),
  `SINTRAN\NPL-SOURCE\SYMBOLS\L07\l07-kallsyms.txt` (ASTAT symbol).

Tags: [V] = byte/source-cited, [I] = inferred with basis.

---

## TL;DR - the four answers

1. **The own station number is read from the receive (INPUT) STATUS register (+2),
   bits 13:8 - a STATIC hardware readback of the thumbwheels.** Routine ram:d704:
   `read(+2); AND 0x3F00; SHR 8`. It is read IMMEDIATELY AFTER an input
   master-clear and BEFORE any transmit, and stored to ctx[6]. The dest-0
   loopback seen in your traces is the subsequent CROSS-CHECK, not the read. [V]
2. **The self-send cross-check transmits frame word 0x0000 (dest = station 0) and
   compares the PRE-TRANSMIT +2 station field against the received frame's
   bits 13:8 from +0.** Master-clear comes FIRST (before both reads); +2 is read
   BEFORE the transmit, +0 once AFTER; there is NO master-clear between transmit
   and the reads, and +2 is NOT re-read after the frame arrives. Reply detection
   is a POLL of +2 bit 3 (up to 100 status reads); the input interrupt (ident 40B)
   is enabled and cross-checked as a diagnostic, but is not the detection
   mechanism. [V]
3. **The +2 station field is NOT a frame field at all.** TPE requires it to hold
   the own station right after master-clear with an EMPTY input FIFO. Reads of
   +0 and master-clears must not change it. [V]
4. **Self-sends use TWO destination flavors: dest 0 (the LIST-HARDWARE-CONFIGURATION
   cross-check) and dest = own station (test 1).** Both must be delivered back
   into the sender's own input side (RFT in +2, frame in +0 with hardware-stamped
   source station, input ident if enabled). Local-vs-bus routing is not
   observable from this binary; the polling window (~100 status reads) tolerates
   either. [V for the requirements; routing [I]]

**Why your emulator prints STATION NO. = 0 and (patched) the cross-check error:**
ctx[6] comes from the +2 read at step 2 below. Your +2 station field is 0 unless
a frame arrived, so ctx[6] = 0. Unpatched, the raw 0x0000 echo also has source 0,
so 0 == 0, no error, prints 0. Patched to stamp station 1 into the echoed +0
data, the check compares ctx[6] = 0 (from +2, pre-transmit) vs 1 (from +0) and
prints exactly "Receive status: 0 / Receive data: 1". **The single correct fix is:
+2 bits 13:8 must ALWAYS read the configured station number (thumbwheels),
statically, independent of any received frame.** That fixes the STATION NO.
column, the Clear-Device status check, and the cross-check in one move. [V]

---

## 1. Q1 - where the own station number comes from

### 1.1 The read primitive [V]

```
ram:d704 (octobus_read_own_station_from_input_status):
  d707  rec.arg1 := dev            ; hardware device number (ctx[5])
  d70a  call d473                  ; = IOX read INPUT STATUS (+2) (d473 body: SAA 2, d733 IOXT)
  d70d  AND 0x3F00                 ; literal word at ram:d713 = 3f00
  d70e  SHA ZIN SHR 10(oct)        ; >> 8
  d70f  return A                   ; = station number
```

Companion ram:d715 is identical but `AND 0x0030; >> 4` = **+2 bits 5:4 = the
"Speed" field** (see section 3.3), stored to ctx[7]. [V]

### 1.2 Where it is called and stored [V]

`cmd_list_hardware_configuration` @ ram:7b24 calls the 3-phase controller prober
**ram:7242** three times with phase arguments 0, 1, 2 (computed jump through the
table at ram:731e: phase 0 -> 7248, phase 1 -> 727d, phase 2 -> 72cf).

Phase 1 loops controllers 1..4 (index global [0x71e4], ctx = 0x7198 + 14*i) and
for each PRESENT controller (ctx[1] != 0) calls **ram:c1f2** - the self-send
cross-check - at ram:72a6. Inside c1f2:

```
c1f7-c1fc  write INPUT CONTROL (+3) := 020B      ; input master clear (d3f8)
c1fe-c203  A := d704(ctx[5])                     ; read +2, extract bits 13:8
c204-c205  ctx[6] := A                           ; <- THE own-station store
c206-c20a  ctx[7] := d715(ctx[5])                ; +2 bits 5:4 (Speed)
```

**ctx[6] is populated from +2 BEFORE any transmit.** The LIST-HARDWARE-
CONFIGURATION "STATION NO." column is printed from ctx[6] afterwards. [V]

### 1.3 SINTRAN never reads it at all [V - NPL]

- `OCSTART` (PH-P2-OPPSTART.NPL:4039, addr 063367): presence check = IOX read of
  `HDEV+2` with IOX-error trap (TRR IIE / TRA IIC, error code 7 = absent), then
  `T:=HDEV+DCONT; 20; IOXT` and `T+4; IOXT` = **020B written to +3 and +7**
  ("CLEAR INTERFACE"). No station read anywhere. Comment: "THIS OCTOBUS DRIVER
  ONLY HANDLE ONE OCTOBUS INTERFACE (DEVICE 0)".
- The ND-100 driver never needs its own station in software: the hardware
  inserts the source station into transmitted frames (manual section 3.2/3.3.1,
  and the TPE receive decode at ram:d3ae relies on it).
- **ND-5000 station numbers are CONSTANTS, not hardware reads**: `CH5CPUPRESENT`
  (PH-P2-OPPSTART.NPL:063104) probes IOX 100406 (output status) for presence,
  polls bit 3 (ready), then `ASTATION\/COMD=:5STATION`. ASTATION resolves in the
  L07 symbol list to **ASTAT = 0x38 = 070B** (l07-kallsyms.txt:1653) [V value;
  the ASTAT=ASTATION 5-char truncation is [I], strong]. With COMD = CPU index
  0..3 this yields stations 70B..73B for CPU 1..4 - matching the manual's
  station table. It then sends the "masterclear Samson system" kick:
  `A SH 10 BONE CBIT BONE EBIT` = station<<8 | C | E, `\/CMMACLE` -> IOX 100405,
  then `\/CMACONT` (continue ACCP). `RP-P2-N500.NPL:976` confirms:
  `X.CPUNO+FN5DEST-1=:X.5STATION`.

(The requester's point that the ND-5000 CPU reads its own station from the
backplane EEPROM {BADAP, Speed, Station no} is the other side of the same coin:
each card has a local static source for its own number; on the ND-100 octobus
interface that static source is exposed to software as +2 bits 13:8.)

---

## 2. Q2 - the exact self-send cross-check sequence [V]

Function ram:c1f2 (octobus_selfsend_station_crosscheck), called per present
controller from LIST-HARDWARE-CONFIGURATION phase 1. Complete IOX order:

```
 1. +3 := 020B                     input master clear
 2. read +2 -> ctx[6] := bits13:8  OWN STATION (before any transmit!)
 3. read +2 -> ctx[7] := bits5:4   Speed field
 4. "Clear Device" verify (ram:c180 = c132 + c159):
      output side (ram:d676): +7 := 004B ; +7 := 0x50 (120B) ;
        poll +6 until (status AND 0x01FD) == 0x0008   (only RFT set), <=100 polls
        else "Wrong transmit status after Clear Device"
      input side (ram:d633):  +3 := 020B (master clear again) ;
        poll +2 until (status AND 0x3F3D) ==
                      (ctx[6] << 8) | (ctx[7] << 4) | 0x04, <=100 polls
        else "Wrong receive status after Clear Device"
        -- i.e. station field present, Speed unchanged, bit2 = 1, bit3 = 0,
           bits 0,1 = 0, with the FIFO EMPTY
 5. +7 := 0xFC01 (176001B)         output control: bit0 = interrupt enable +
                                   high-bit field(s) not decoded here [I]
 6. +3 := 041B  (0x21)             input control: bit0 = interrupt enable +
                                   bit5 (0x20; meaning not decodable from this
                                   binary [I])
 7. poll +6 bit3 (output RFT) <=100 (ram:c07a/d591); timeout ->
      "Not ready for transfer on transmit", abort (return 0)
 8. +5 := 0x0000                   THE SELF-SEND: pack {C=0,B=0,dest=0,byte=0}
                                   (ram:d6bf -> pack d3df -> write d3ce).
                                   Destination station field = 0.
 9. wait receive (ram:c0a9 mode 1 -> d5b8):
      poll +2 bit3 (input RFT) <=100 (ram:d591)
      PLUS interrupt cross-check: the level-13 ident service records input/output
      idents in flags [0x71e0]/[0x71df] (+ ident code in [0x71e2]); c0a9 reports
      "enabled but no interrupt" / "interrupt but not enabled" style diagnostics
      and spurious idents via [0x71de]. Detection itself is the POLL.
10. read +0 ONCE -> decode (ram:d3ae): srcStation = bits13:8 (mask 0x3F00)
11. compare ctx[6] (from step 2) vs srcStation (from step 10):
      not equal -> report ram:b0a3 = "The station number read from the receive
      status register is not equal the source station number read from the
      receive data register, when the controller sends a message to itself."
      printed values: "Receive status" = ctx[6], "Receive data" = srcStation.
```

Direct answers to the sub-questions:

- **What is transmitted:** one frame word 0x0000 - dest station 0, data byte 0,
  C = B = 0. (ram:c22b-c235: d6bf(dev, 0, 0, 0, 0); pack proven at ram:d3df:
  frame = C<<15 | B<<14 | station<<8 | byte.)
- **Interrupt or poll:** POLL of +2 bit 3. The input interrupt is ENABLED
  (041B, bit0) and its arrival is verified as a diagnostic through the level-13
  ident bookkeeping, but a reply is accepted purely on the RFT poll.
- **Order of +2 vs +0:** +2 FIRST - and crucially BEFORE the transmit (step 2);
  +0 once, after the frame arrives (step 10). The error text's "read from the
  receive status register" refers to the pre-transmit read. +2 is NOT re-read
  after reception for this check.
- **Master-clear position:** BEFORE the reads/transmit (steps 1 and 4-input).
  Never between the transmit and the +0 read. Your emulator's observation
  ("TPE serviced 41B, master-cleared input, then read +2 = 0") fits step 4 of
  the NEXT controller iteration or a re-run - within one c1f2 pass there is no
  post-transmit master-clear. [V for the sequence; the trace alignment is [I]]

---

## 3. Q3 - receive STATUS (+2) station field semantics [V]

### 3.1 Lifetime: static, not frame-related

The station field in +2 is a **constant readback of the configured own station
number** (hardware thumbwheels TH3/TH4 on PCB 3096):

- It must be correct IMMEDIATELY AFTER an input master-clear (020B) with the
  FIFO empty: d633's expected value requires bit3 = 0 (no frame waiting) and
  bit2 = 1 while bits 13:8 = own station (step 4 above). So it cannot be "frame
  at FIFO head" or "last frame received" - there IS no frame.
- It is read for the STATION NO. column before any traffic (step 2).
- Neither master-clear nor reads of +0 may change it (the same value must
  satisfy step 2, step 4's poll, and the step 11 comparison).

### 3.2 What the "source station of a received frame" actually is

The per-frame source station lives in bits 13:8 **of the received DATA word
read from +0** (decode ram:d3ae, mask 0x3F00 at ram:d3cb) - hardware inserts it
into every frame. The +2 field is a different, static thing that happens to use
the same bit positions.

### 3.3 TPE's own register documentation (DECODE-STATUS-REGISTER strings) [V]

INPUT status (+2), string block at ~ram:a5f4:

| Bit | Meaning (TPE text) | Note |
|---|---|---|
| 0 | Interrupt enable | readback of control bit 0 (shadowed by TPE at [0xd39e]) |
| 1 | Not used | |
| 2 | "Fifo full" | label text; the Clear-Device check REQUIRES bit2 = 1 on an EMPTY fifo, so the wire sense is "room available" - your FifoNotFull is right |
| 3 | Ready for transfer | = frame available (RFT) |
| 4-5 | Speed | text says "bit 3-5 - Speed"; the code mask is 0x0030 (ram:d715), so bits 5:4 |
| 6-7 | Not used | |
| 8-13 | **Station number** | the static own-station field |
| 14-15 | Not used | |

OUTPUT status (+6), string block just before it:

| Bit | Meaning |
|---|---|
| 0 | Interrupt enable |
| 1 | Not used |
| 2 | Request on (RQ) |
| 3 | Ready for transfer (RT) |
| 4 | Error |
| 5 | Retry counter 0 (RE) |
| 6 | Not present (NP) |
| 7 | Busy (BU) |
| 8 | Parity error (PA) |
| 9-14 | Not used |
| 15 | **Master** |

(Bits 2-8 match the transmission-statistics mapping already verified in
OCTOBUS-TEST-PROTOCOL-RE.md section 2. Bit 15 "Master" is new - it ties to the
"No new master is selected / Master, Octobus station number:" strings at
~ram:bd6e; that master-selection test was not carved this pass.)

---

## 4. Q4 - self-send routing [V requirements / [I] mechanism]

TPE uses two self-send flavors:

1. **dest = 0** (frame 0x0000) - the LIST-HARDWARE-CONFIGURATION cross-check
   (ram:c1f2, described above). The error string calls this "sends a message to
   itself", i.e. **destination 0 is treated as self**.
2. **dest = own station** (frame = ctx[6]<<8) - ram:c1a3, whose literal-pool
   reference sits in test 1 (`test1_transmit_receive_loop` @ ram:7d9d). Sequence:
   Clear Device -> +7 := 0xFC01 -> +3 := 1 (int enable only, NO bit5) ->
   wait output RFT -> transmit ownStation<<8 -> wait INPUT frame (c0a9 mode 1)
   -> also wait OUTPUT completion (c0a9 mode 0). [V]

What the hardware must do (either flavor): deliver the frame back into the
sender's OWN input side - input RFT sets, the frame appears in +0 with the
hardware-stamped source station (= own station), and the input ident (40B)
fires if input control bit0 is set. The output side completes normally (41B).

Immediate vs bus-delayed: **not observable from this binary.** Both waits are
100-iteration status polls, so a bus round-trip latency is fully tolerated.
Emulator recommendation: immediate-consistent delivery is the simplest model
that satisfies every check; nothing in TPE requires bus delay. [I]

Related self-referential behavior proven in the same pass: the presence scanner
ram:c479 (fills ctx[10..13], the "STATIONS SEEN" bitmap) **skips its own station**
(`if station == ctx[6] -> next`, ram:c49b-c49e) - self-presence is never probed
on the bus. Each other station 1..62 is probed with: Clear Device -> +7 := 1 ->
wait output RFT -> transmit C-frame `0x8000 | station<<8 | 0xA2` -> wait output
completion -> station present = output status bit6 (Not present) CLEAR
(ram:d56a). [V]

---

## 5. New/renamed functions for the Ghidra map [V]

| Address | Suggested name | What it is |
|---|---|---|
| ram:7242 | octobus_probe_controllers_phase | switch(arg 0/1/2) via table ram:731e; loops controllers 1..4 over 0x7198 ctx table |
| ram:7248 | .phase0: presence probe per controller (ctx[1] := c196 result; c196 -> e48d = IOX-trap probe) |
| ram:727d | .phase1: calls c1f2 per present controller |
| ram:72cf | .phase2: calls c4f3 per present controller (needs ctx[1] AND ctx[2]) |
| ram:7321 | octobus_ensure_probed_select_first | runs phase 0 once ([0x71eb] guard), selects first present controller into [0x71e5]/[0x71e6] |
| ram:c1f2 | octobus_selfsend_station_crosscheck | section 2; the ONLY caller of reporter b0a3 |
| ram:c1a3 | octobus_selfsend_to_own_station | test-1 helper; dest = ctx[6] |
| ram:c180 | octobus_clear_device_verify_both | = c132 (output) + c159 (input), ANDs results |
| ram:c132 / ram:d676 | clear-device check, OUTPUT side | +7:=4; +7:=0x50; poll +6 (mask 0x01FD == 0x08); reporter a3fb |
| ram:c159 / ram:d633 | clear-device check, INPUT side | +3:=020B; poll +2 (mask 0x3F3D == st<<8\|spd<<4\|4); reporter a64b |
| ram:c479 | octobus_probe_presence_bitmap | per-station probe loop, skips own station |
| ram:c4f3 / ram:c51f | phase2 wrapper / bitmap stability compare | c479 into local, copy to ctx[10..13]; c51f re-reads and compares (reporter b035) |
| ram:c07a | octobus_wait_output_rft | d591(side=0) + timeout reporters a7fe/a74c |
| ram:c0a9 | octobus_wait_frame_and_check_ints | d5b8 + spurious-ident ([0x71de]) and int-expectation diagnostics |
| ram:d591 | octobus_poll_rft | poll +2 or +6 bit 3, <=100 ([0x71e3] counter) |
| ram:d5b8 | octobus_wait_transfer_record_ints | d591 twice + snapshots [0x71df]/[0x71e0]/[0x71e1]/[0x71e2] |
| ram:d704 | octobus_read_own_station_from_input_status | (+2 AND 0x3F00) >> 8 |
| ram:d715 | octobus_read_speed_from_input_status | (+2 AND 0x0030) >> 4 |
| ram:d6bf | octobus_send_single_frame | pack {C,B,station,byte} (d3df) -> +5 (d3ce) |
| ram:d6e2 | octobus_receive_single_frame_decode | +0 (d3a0) -> decode (d3ae) -> {C,B,srcStation,byte} |
| ram:d56a | octobus_station_present_from_np | read +6, return NOT bit6 (Not present) |
| ram:d57e | octobus_output_error_bit | read +6, return bit4 (Error) |
| ram:b0a3 | report_selfsend_station_mismatch | owns the section-2 error string (~ram:b136) |
| ram:b526 | report_response_station_vs_receive_data | owns "...station number in the Test Protocol response ... not equal ... receive data register" (~ram:b5c7) - refines the earlier generic "header reporter" label |

Globals refined: [0x71de] spurious/unexpected-ident flag (cleared by ident
service d740 and by c0a9), [0x71df] output-ident-received flag,
[0x71e0] input-ident-received flag, [0x71e1] ident-arrived flag,
[0x71e2] captured ident code, [0x71e3] poll countdown, [0x71e4] controller loop
index, [0x71e5] first-present controller, [0x71eb] probed-once guard,
[0xd39e]/[0xd39f] input/output interrupt-enable shadows (set by d3f8/d434 from
value bit 0), [0x71a5] per-controller bookkeeping bit words (EXR-computed
bit set/clear). Register wrappers: d726 iox_write(dev+off), d733 iox_read,
d3a0 read +0, d473 read +2, d3f8 write +3, d3ce write +5, d4ce read +6,
d434 write +7, d740 level-13 IDENT (clears [0x71de], returns ident code).

## 6. What was NOT carved (honest gaps)

- The meaning of output-control value 0xFC01's high bits (retry count is the
  obvious candidate for a multi-bit field, but that is a GUESS - marked [I];
  the manual's register description should settle it).
- Input-control bit 5 (0x20) in the 041B write: not decodable from this binary.
- The LIST-HARDWARE-CONFIGURATION row printer ram:78ef (which ctx fields feed
  the IDENT CODES columns) - not walked; STATION NO. = ctx[6] and STATIONS
  SEEN = ctx[10..13] popcount are established from the probe side.
- The master-selection test (bit 15 / "No new master is selected") and the
  probe byte 0xA2's protocol-level meaning (E+M flags, OMD 2 per the manual's
  low-byte flag layout) - identified, not walked.
- SOCTO/SOCTW receive-path handling of the +2 station field in SINTRAN: OCSTART
  and CH5CPUPRESENT are source-verified above; the interrupt-time driver body
  was not re-carved this pass.
