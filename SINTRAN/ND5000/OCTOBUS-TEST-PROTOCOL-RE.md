# Octobus Test Protocol - TPE OCTOBUS B00 reverse engineering

**Status: REQUEST + REPLY FORMATS BYTE-VERIFIED** (2026-07-16, second pass).
All command codes, the reply header (magic / cmd+1 / station / status), every
per-command reply payload layout, and the exact fields TPE validates are now
proven from disassembly (sections 1, 3.3, 3.4); the emulator reply recipe is
in section 3.6. Goal: recover the OMD-0 "Octobus Test Protocol"
request/response formats so the RetroCore emulated stations can answer TPE
tests 4-6 (Check Octobus configuration / Echo single word / Echo multi word)
and the CONFIG/LIST-HARDWARE-CONFIGURATION probes.

Source of truth: `octobus-b00.test` loaded in Ghidra (from the test floppy
`D:\ND\S\Nd-210523I01-XX-01D.img`, path `/D:/ND/S/testprog/x/octobus-b00.test`),
plus live traces from the RetroCore run (`%LOCALAPPDATA%\trace\file-trace.txt`).
The program loads at base VA 0x6B88; TPE-MON library vector slots at 0x69xx.

## 1. Request wire format [V - live trace + disassembly agree]

A protocol exchange is a multibyte octobus message to the target station,
OMD 0. On the wire (16-bit software frames written to IOX 100405):

```
SOMB  frame: C | M | S | OMD          (low byte 0x30 for OMD 0)
data  frame: source OMD byte           (0x00 = reply to OMD 0)
data  frame: byte count N
data  frame x N: payload bytes
EOMB  frame: C | M | OMD               (low byte 0x20 for OMD 0)
```

Payload always starts with the magic word **0x71C7** (two bytes 71 C7),
followed by a 16-bit command word:

ALL command codes byte-verified 2026-07-16 (SAA immediates in the builders;
menu-handler mapping via xrefs). Request byte counts are the builder's 7,X arg.

| Command word | Meaning | Extra payload words (after magic+cmd) | Req bytes | Builder fn (renamed in Ghidra) | Menu handler caller |
|---|---|---|---|---|---|
| 0x0000 | Identify yourself | - | 4 | otp_build_identify_yourself_cmd00 @ ram:cea2 | 87bd (otp_cmd_identify_yourself) |
| 0x000A | Get present stations | - | 4 | otp_build_get_present_stations_cmd0A @ ram:cf35 | 8901; also c6e8 (test-4 scan) |
| 0x000C | Echo single word | [2]=pattern NUMBER (incrementing counter, 0x0001 = first), [3]=pattern word | 8 | otp_build_echo_single_word_cmd0C @ ram:cf5a | 8988; also c85b (test-5 helper) |
| 0x000E | Echo multi word | [2]=string number, [3]=word count N, [4..3+N]=string words | 8+2N (max 250 @ [cfcc]) | otp_build_echo_multi_word_cmd0E @ ram:cf83 | 8a68; also c936 (test-6 helper) |
| 0x0010 | Read Octobus register | [2]=register function (prompt: "Register function (0,2,6)."), [3]=UNINITIALIZED stack junk (byte count is still 8!) | 8 | otp_build_read_octo_register_cmd10 @ ram:cfd0 | 8ae4 (otp_cmd_read_octo_register) |
| 0x0012 | Write Octobus register | [2]=register function (prompt: "(3,5,7)."), [3]=register content | 8 | otp_build_write_octo_register_cmd12 @ ram:cff7 | 8b7c; also cb8a (scan_emergency) |
| 0x0016 | Get Domino Information | - | 4 | otp_build_get_domino_info_cmd16 @ ram:cec6 | 880e (otp_cmd_get_domino_information) |
| 0x0018 | Get test version | - | 4 | otp_build_get_test_version_cmd18 @ ram:ceeb | 885f (otp_cmd_get_test_version) |
| 0x001A | Get module type | - | 4 | otp_build_get_module_type_cmd1A @ ram:cf10 | 88b0 (otp_cmd_get_module_type) |

(ram:d020 is NOT a builder - it is the reply parser, see 3.3. The earlier names
otp_build_cmd16_word_unk / otp_build_cmd18_twoword_unk were WRONG: those
builders carry cmd 0x10 and 0x12, byte-verified at ram:cfd8 `SAA 0x10` and
ram:cfff `SAA 0x12`.)

Live-captured requests (trace, sent to station 10):
- Identify yourself:    body `00 04 71 C7 00 00`
- Get present stations: body `00 04 71 C7 00 0A`
- Echo single word:     body `00 08 71 C7 00 0C 00 01 FF FF`
  (= pattern number 1, pattern 0xFFFF. Byte-verified 2026-07-16 in the caller at
  ram:c84c-c85a: first extra word is a per-iteration counter `LDA -0x77,B ; AAA 0x1`,
  second is the pattern from -0x73,B; validators compare "pattern number" and "pattern".)

## 2. Send path [V]

- `octobus_send_multibyte_message` @ ram:d16a: clears 6 result counters,
  sends SOMB / srcOMD / count / payload bytes (LBYT, byte-packed buffer) /
  EOMB via the frame sender at **ram:d13e**; counts per-frame ack results.
- IOX primitives: `iox_write` @ ram:d726 (T=base+off, A=value, IOXT),
  `iox_read` @ ram:d733, IDENT level 13 @ ram:d740 (also clears global 0x71de).
- Transmission statistics decoded by `print_transmission_statistics`
  @ ram:863d with labels (ram:8734+): "Transmission OK/NOT OK",
  "No. of PA (PArity error)", "No. of BU (BUsy)", "No. of NP (Not Present)",
  "No. of RE (REtry counter 0)", "No. of not RT (Ready for Transfer)",
  "No. of RQ (ReQuest on)". **This confirms the output-status bits TPE
  reads after transmit: RQ=bit2, RT=bit3, RE=bit5, NP=bit6, BU=bit7,
  PA=bit8 (matches RetroCore TransmitStatusBits).**

## 3. Reply wire format [recovered - V/[I] as marked]

### 3.1 Reply envelope [V - symmetric to the request]

A station's reply is an ordinary Octobus multibyte message, sent back to the
asker's OMD (0). It uses the SAME envelope as the request (section 1):

```
SOMB  frame: (0x30 | OMD)   low byte 0x30 for OMD 0
data  frame: source OMD byte
data  frame: byte count N
data  frame x N: payload bytes
EOMB  frame: (0x20 | OMD)   low byte 0x20 for OMD 0
```

Send side proven in `octobus_send_multibyte_message` @ ram:d16a: SOMB built
at ram:d1ae (`SAA 30B ; ORA reg[8]=OMD`), EOMB at ram:d1f1
(`SAA 20B ; ORA reg[8]=OMD`). The reply direction reuses this exact framing.

### 3.2 Frame-word bit layout as RECEIVED [V - disassembly of ram:d3ae]

`octobus_decode_frame_word` @ ram:d3ae unpacks each 16-bit word read from the
Octobus INPUT-DATA register (offset +0) into a 4-word struct:

| Bits | Field | Meaning |
|---|---|---|
| 15 | C | control / marker flag |
| 14 | B | Broadcast/TYPE flag (manual App. 2; M is bit 5 in the LOW byte, not here) |
| 13:8 | srcStation | 6-bit SOURCE STATION number (0..62), `(raw AND 37400B) SHR 8` |
| 7:0 | dataByte | payload byte, `raw AND 377B` |

Mask byte-verified 2026-07-16: the AND at ram:d3c3 references the data word at
ram:d3cb = 0x3F00 = 37400B (full 6-bit station field). An earlier draft cited
3400B - a dropped octal digit, same failure mode as the 1404B->140400B trace bug.

So the responding station number is carried in bits 13:8 of EVERY reply frame
word (hardware inserts it). The emulated station must present reply frames as
`(flags<<14) | (srcStation<<8) | dataByte`. [V]

### 3.3 Reply receive/parse path [V - fully byte-verified 2026-07-16]

Corrections to the earlier draft: d020 parses a WHOLE message (not single
frames); 0x7198 is NOT a reply table; the "offsets 0,2,4,6" copy is the SEND
result (transmission counters), not the reply.

- `octobus_receive_multibyte_message` @ ram:d2be (was octobus_receive_frame_wait)
  receives one complete multibyte message into a 6-word struct R:
  R[0]=status flags (bits 15/14 initialized to 1 = OK, CLEARED by the error
  continuation at ram:d2c4 which also records error code n as bit n via a
  computed BSTA/EXR); R[1]=source station (bits 13:8 of the SOMB frame word,
  ram:d343-d345); R[2]=source OMD byte AND 0x000F ([d38d], ram:d34b-d34e);
  R[3..5]={payload buffer word address 0xd082 ([d38e]), 0, byteCount-1}
  (ram:d36b-d372). It waits for a frame with low-byte flags AND 0x30 == 0x30
  (SOMB, [d342] @ ram:d329-d32c), reads the srcOMD frame, the count frame,
  then count payload bytes (SBYT into the static buffer at word address
  ram:d082, byte-indexed, HIGH byte first = big-endian byte order,
  ram:d35d-d36a), then requires an EOMB frame: flags AND 0x20 == 0x20
  ([d38f] @ ram:d37b-d37e, else error bit 9 @ d380-d389). [V]
- `octobus_parse_received_message` @ ram:d020 (was octobus_parse_received_frame)
  calls d2be once ([d07d]) and produces a 3-word outrec for the caller:
  outrec[0] = response TYPE: 0 = Test Protocol response received OK,
              1 = no response message received, 2 = received but not a Test
              Protocol message, 3 = part of the response message is lost
              (string mapping proven in otp_read_response_and_display, see 3.4a);
  outrec[1] = source station (R[1]);
  outrec[2] = payload WORD pointer = R[3] + R[4]>>1 = ram:d082.
  Type derivation: R[0] bit15|bit14 set -> message present (candidate type 0);
  both clear -> bit11|bit10 set = type 3 else type 1 (ram:d02e-d048).
  Type 0 requires srcOMD R[2]==0 (ram:d04e-d052) AND first payload word ==
  0x71C7 (magic constant AT ram:d080, compare @ ram:d05c-d062), else type 2. [V]
- `otp_response_classify` @ ram:c58a seeds timeout ctr 0x71e3 with 100.
  (ram:c58f-c590), loops d020 until type 0 or timeout, then validates the
  reply HEADER (see 3.4). On type 2 it restarts the whole listen loop
  (JMP [c641] -> ram:c58d). Returns 1 = reply valid. [V]
- The 14-word-per-entry table at ram:0x7198 (hex; earlier "7198B" was a bogus
  suffix) is the per-CONTROLLER context table, indexed by
  [0x71e6] = selected Octobus CONTROLLER number (written by cmd_select_device
  @ ram:7bac, prompt "Octobus controller number"): ctx[5] = receive parameter,
  ctx[6] = OWN station number (ram:c77d), ctx[10..13] = 62-bit hardware
  presence bitmap read via computed-shift EXR (ram:c756-c760). Replies are
  parsed IN PLACE in the byte buffer at ram:d082 - nothing is unpacked into
  0x7198. [V]

### 3.4 Reply payload layout - BYTE-VERIFIED (2026-07-16)

Every reply payload starts with a fixed 4-word header, then command-specific
data. All offsets below are payload WORD indices (payload bytes are stored
big-endian, high byte first, into the buffer at ram:d082; payload word j =
bytes 2j..2j+1).

#### 3.4a Common reply header [V - otp_response_classify @ c58a]

| Word | Content | Verification |
|---|---|---|
| 0 | 0x71C7 magic | d020 @ ram:d05c-d062, constant at ram:d080 |
| 1 | response code = REQUEST COMMAND + 1 (0x0000->0x0001, 0x000A->0x000B, ... 0x001A->0x001B) | compared vs expectedCmd at ram:c61b-c61d; expectedCmd literals: 1 @ c7ec (identify), 0xB @ c710, 0xD @ c882, 0xF @ c95e; display switch @ 8bd2/8be0/8c14/8c5f/8cc5/8cf7/8d05/8d51/8d79 |
| 2 | responder's STATION number | must equal BOTH the frame source station outrec[1] (ram:c5ca-c5cd, reporter [c60a]=b526) AND the expected station param (ram:c5e1-c5e3, reporter [c60b]=b633) |
| 3 | STATUS word | ram:c5f3-c5f7 + display @ 8f50-8f5c/8f77-8f99: 0="Ok" -> classify success; 1="Illegal Octobus register function"; 2="Illegal Test Protocol command code"; other="Status code not defined" |

A reply is accepted by the automated tests only if: type 0 (message received,
srcOMD==0, magic OK), word1 == cmd+1, word2 == expected station == frame
source station, word3 == 0. Then classify returns 1 (ram:c62b-c62c).

#### 3.4b Per-command reply data [V - otp_decode_response_fields @ 8bc8 + test validators]

| Response code | Command | Reply payload words after the header | Evidence |
|---|---|---|---|
| 0x0001 | Identify yourself | NONE - header only (8 bytes total). Neither the display (prints title only, ram:8bd2-8bde) nor test 4 (otp_scan_identify_yourself @ c7a1, classify-only) reads anything past word 3. Test 4 additionally checks that EXACTLY ONE identify reply arrives (duplicate counter loop ram:c7f2-c80d, reporter [c82c]=bf8a). | 8bd2, c7a1 |
| 0x000B | Get present stations | word[3+j] for j=1..62: ONE WORD PER STATION, value EXACTLY 1 = station j present, anything else = absent. Full reply = 66 words = 132 bytes. Test 4 (otp_scan_get_present_stations @ c6c2) compares (word[3+j]==1) XOR ownHwPresenceBit(j) for ALL j=1..62 (loop ram:c716-c78f, XOR via REXO @ ram:c763), skipping only j == ctx[6] (own station, ram:c77b-c77e). Display loop ram:8bee-8c12 prints "Present station j" for each ==1. | c6c2, 8bee |
| 0x000D | Echo single word | [4] = pattern number (must equal the sent pattern number, cmp ram:c88a-c88c, reporter [c8b7]=b8c5), [5] = pattern word (must equal sent pattern, cmp ram:c89d-c89f, reporter [c8b8]=b971). Reply = 12 bytes. | c886-c8ad, 8c17-8c5c |
| 0x000F | Echo multi word | [4] = string number (cmp ram:c966-c968, reporter [c9b5]=ba19), [5] = string length in WORDS (must equal sent count, cmp ram:c979-c97d, reporter [c9b6]=bac4), [6..5+N] = the N string words (per-word cmp loop ram:c99d-c9a4, reporter [c9e8]=bb74). Reply = (N+6)*2 bytes - the no-response path prints exactly this expected size (ram:c9d2-c9e3). | c92b/c962-c9d1, 8c62-8cb1 |
| 0x0011 | Read Octobus register | [4] = Register content (printed octal, ram:8ce5-8cf3). Reply = 10 bytes. | 8cc8-8cf5 |
| 0x0013 | Write Octobus register | NONE - header only (status ack). Display prints title only (ram:8cfa-8d03). | 8cf6-8d03 |
| 0x0017 | Get Domino Information | [4..5] = "Type of processor" as a 32-bit number (LDD payload[4] @ ram:8d21-8d23, printed via 32-bit formatter, format word 0x35f - e.g. 68000. needs 32 bits), [6..7] = "OPCOM version" as a 4-char ASCII string (print {payload+6,0,3} @ ram:8d30-8d38), [8..17] = "Compile time" as a 19-char ASCII string (print {payload+8,0,0x12} @ ram:8d45-8d4d). Reply = 36 bytes. | 8d08-8d4f |
| 0x0019 | Get test version | [4] = "Octobus test version" (16-bit number, printed decimal, ram:8d5d-8d76). Reply = 10 bytes. | 8d54-8d77 |
| 0x001B | Get module type | [4] = module type code: 1 = "Domino controller" (str @ ram:8ebe), 2 = "MFBus controller" (@ ram:8ec7), 3 = "ACCP" (@ ram:8ecf); other values print the title only. Reply = 10 bytes. | 8d85-8dae |

Unknown response code -> "Illegal Test Protocol response code." (@ ram:8ed1).

NOTE the asymmetry: request parameters sit at payload words 2..3 (no
station/status), replies insert station+status at words 2..3 and echo the
request parameters starting at word 4.

Response-level reader states printed by READ-RESPONSE (type -> string):
type 1 -> "No response message received" (@ 8fc6), type 2 -> "Message
received, but not a Test Protocol message" (@ 8fd6), type 3 -> "Part of the
response message is lost" (@ 9001), type 0 -> "Test Protocol response message
received" (@ 9015). [V - ram:8ef8-8f34]

### 3.5 Validator field comparisons [V - error strings + printers]

The pass/fail validators compare reply fields against transmitted values and
print (each printer sits just before its data string, data-after-code):

| Printer fn | Error string | Field compared | Call site (byte-verified) |
|---|---|---|---|
| report_pattern_number_error @ b8c5 | "Not expected pattern number received" (@ b93c) | echo-single reply word 4 vs sent pattern number | ram:c88a-c899, [c8b7] |
| report_pattern_value_error @ b971 (renamed this pass) | "Received pattern is not equal transmitted pattern" (@ b9e4) | echo-single reply word 5 vs sent pattern | ram:c89d-c8ac, [c8b8] |
| report_string_number_error @ ba19 | "Not expected string number received" (@ ba90) | echo-multi reply word 4 vs sent string number | ram:c966-c975, [c9b5] |
| report_string_length_error @ bac4 | "Length of received string is not equal..." (@ bb2e) + "String length - transmitted/received" (@ bb55) | echo-multi reply word 5 vs sent word count | ram:c979-c98b, [c9b6] |
| report_word_pattern_error @ bb74 | "Received pattern is not equal..." (@ bbff) + "Word number in string / Pattern transmitted/received" (@ bc1b) | echo-multi reply word 6+k vs sent word k | ram:c99d-c9bf, [c9e8] |
| b526 / b633 (header reporters) | "Received message from not expected station" family | reply word 2 vs frame srcStation / vs expected station | ram:c5cf-c5de / c5e5-c5f0 |
| b6e8 / b76e (status reporters) | "Illegal Octobus register function" / "Illegal Test Protocol command code" | reply word 3 (status) | ram:c5f3-c60e |
| b7db (cmd-echo reporter) | wrong response code | reply word 1 vs expectedCmd | ram:c61b-c628 |

### 3.6 Emulator reply recipe [V] + remaining open items

To satisfy TPE tests 4-6, an emulated station at number S must, on receiving
an OMD-0 multibyte message whose payload starts 71 C7, send back one multibyte
message (envelope of section 3.1, srcOMD byte = 0, station S in bits 13:8 of
every frame word, payload big-endian per word):

```
word 0: 0x71C7
word 1: request command + 1
word 2: S (own station number)
word 3: 0 (Ok)   [1 = illegal register function, 2 = illegal TP command code]
then per command:
  cmd 0x0000 identify:      nothing              (count = 8 bytes)
  cmd 0x000A present:       62 words, word[3+j] = 1 if station j present else 0,
                            j = 1..62             (count = 132 bytes)
  cmd 0x000C echo single:   echo request words 2,3 (count = 12 bytes)
  cmd 0x000E echo multi:    echo request words 2,3 and the N string words
                            (count = 12 + 2N bytes)
  cmd 0x0010 read reg:      1 word register content (count = 10 bytes)
  cmd 0x0012 write reg:     nothing              (count = 8 bytes)
  cmd 0x0016 domino info:   2 words processor type (32-bit, e.g. 68000.),
                            2 words OPCOM version (4 ASCII chars),
                            10 words compile time (19 ASCII chars + pad)
                            (count = 36 bytes)
  cmd 0x0018 test version:  1 word version number (count = 10 bytes)
  cmd 0x001A module type:   1 word: 1=Domino ctrl, 2=MFBus ctrl, 3=ACCP
                            (count = 10 bytes)
```

For test 4 the present-station words MUST agree with which stations actually
answer ident on the emulated bus (TPE XORs each word against its own hardware
presence bitmap, own station excepted). Timing: the reply is polled by
classify with a 100-iteration countdown ([0x71e3] seed @ ram:c58f); the reply
just needs to be queued in the controller input FIFO when READ-RESPONSE / the
test's receive loop runs.

Remaining open items:

1. [UNCERTAIN] Whether a REAL ACCP pads the identify / write-reg replies
   beyond the 4 header words. TPE reads nothing past word 3, so 8-byte
   replies are sufficient for the tests, but a live capture from real
   hardware would settle the actual byte counts.
2. [UNCERTAIN] Sub-function semantics of "Register function" 0,2,6 (read)
   and 3,5,7 (write) - which physical Octobus/station registers they map to
   on a Domino/ACCP module. The values come from the menu prompts
   (ram:8b05+/8bc0+); the station-side meaning is not derivable from this
   binary. Status 1 = "Illegal Octobus register function" is the reject code.
3. [UNCERTAIN] Exact "Type of processor" value a real ACCP returns in the
   domino-info reply (displayed as a 32-bit number; 68000. = 0x109A0 is the
   obvious candidate for the MC68000 baby card, but not byte-provable here).
4. Station scan `otp_station_scan_emergency` @ ram:cb4f iterates stations
   0..62 reading the per-station Octobus status via `EXR ST` and, for present
   stations, sends WRITE-OCTO-REGISTER (cmd 0x12) probes via cff7 @ cb8a. The
   presence test is the hardware status read, not a get-present-stations
   reply. [I]
5. Note: `BSET 0x3` @ ram:c850 on the echo-single pattern-number counter -
   sub-function (set/clear bit 3) not decoded from the mnemonic; the live
   trace (pattern number 0x0001 first) implies it does not set bit 3.
   [UNCERTAIN - cosmetic only, TPE side]

## 4. Function map so far (renamed in Ghidra)

| Address | Name | Evidence |
|---|---|---|
| ram:cea2 | otp_build_identify_yourself_cmd00 | 0x71C7 + STZ cmd, len 4; matches trace |
| ram:cec6 | otp_build_get_domino_info_cmd16 (renamed) | 0x71C7 + SAA 0x16; caller 880e = GET-DOMINO-INFORMATION [V] |
| ram:ceeb | otp_build_get_test_version_cmd18 (renamed) | 0x71C7 + SAA 0x18; caller 885f = GET-TEST-VERSION [V] |
| ram:cf10 | otp_build_get_module_type_cmd1A (renamed) | 0x71C7 + SAA 0x1A; caller 88b0 = GET-MODULE-TYPE [V] |
| ram:cf35 | otp_build_get_present_stations_cmd0A | 0x71C7 + SAA 0x0A; matches trace |
| ram:cf5a | otp_build_echo_single_word_cmd0C | 0x71C7 + SAA 0x0C + 2 param words; matches trace |
| ram:cf83 | otp_build_echo_multi_word_cmd0E (renamed) | SAA 0x0E @ cf8b; stringNum + count + words; cap 250 bytes [V] |
| ram:cfd0 | otp_build_read_octo_register_cmd10 (renamed; WAS mislabeled cmd16) | SAA 0x10 @ cfd8; 1 param (register function 0/2/6); word3 uninitialized, len still 8 [V] |
| ram:cff7 | otp_build_write_octo_register_cmd12 (renamed; WAS mislabeled cmd18) | SAA 0x12 @ cfff; 2 params (register function 3/5/7, content) [V] |
| ram:ce6b | otp_send_test_protocol_request (renamed) | SEND ONLY - does NOT await the reply (reply is read separately by c58a/8ee4); calls d16a, returns 4-word transmission result; slot [0x6912] resolves to 0x55d7 in TPE-MON (byte-move) [V] |
| ram:d16a | octobus_send_multibyte_message | SOMB(0x30\|OMD)/srcOMD/count/bytes/EOMB(0x20\|OMD) + counters [V] |
| ram:d13e | (frame sender, garbled body) | sends one framed word |
| ram:863d | print_transmission_statistics | strings at 8734+ |

### Reply-path functions [renamed this pass]

| Address | Name | Evidence |
|---|---|---|
| ram:d020 | octobus_parse_received_message (renamed) | receives one whole message via d2be, outputs {type, srcStation, payloadWordPtr}; magic 0x71C7 @ d080 [V - see 3.3] |
| ram:d2be | octobus_receive_multibyte_message (renamed) | full SOMB/srcOMD/count/bytes/EOMB receive into 6-word struct; payload bytes to buffer @ d082 [V - see 3.3] |
| ram:d082 | otp_reply_payload_buffer (data, renamed) | static reply payload byte buffer, word-addressed base [V] |
| ram:d3ae | octobus_decode_frame_word | bit15=C,14=B,13:8=srcStation (mask 0x3F00 @ d3cb),7:0=byte [V] |
| ram:d3a0 | octobus_read_input_data | reg offset +0 [V] |
| ram:d473 | octobus_read_input_status | reg offset +2 [I] |
| ram:d3f8 | octobus_write_input_control | reg offset +3 [I] |
| ram:d3ce | octobus_write_output_command | reg offset +5 [V] |
| ram:d4ce | octobus_read_output_status | reg offset +6 [I] |
| ram:d434 | octobus_write_output_control | reg offset +7 [I] |
| ram:d726/d733/d740 | octobus_iox_write / octobus_iox_read / octobus_ident_level13 | IOXT / IDENT 0x23 [V] |
| ram:d482 | octobus_decode_output_status_bits | unpacks 3 flag bits [I] |
| ram:d4dd | octobus_decode_input_status_bits | unpacks 16 bits into array [I] |
| ram:ce0e/ce28/ce35/ce54 | otp_buf_dequeue / _avail / _enqueue / _init | linked-buffer helpers on DAT_ram_ce0d [I] |

### Interactive TP subcommands [V - from command table @ 6cef, menu names @ 929f]

| Handler | Menu name |
|---|---|
| ram:8796 | otp_cmd_identify_yourself |
| ram:88da | otp_cmd_get_present_stations |
| ram:892b | otp_cmd_echo_single_word |
| ram:89d4 | otp_cmd_echo_multi_word |
| ram:8aa2 | otp_cmd_read_octo_register |
| ram:8b1f | otp_cmd_write_octo_register |
| ram:87e7 | otp_cmd_get_domino_information |
| ram:8838 | otp_cmd_get_test_version |
| ram:8889 | otp_cmd_get_module_type |
| ram:8ee4 | otp_read_response_and_display (READ-RESPONSE) |
| ram:8bc8 | otp_decode_response_fields |

### Scan / validator / report functions [renamed this pass]

| Address | Name | Evidence |
|---|---|---|
| ram:c58a | otp_response_classify | validates reply header (magic/cmd+1/station/status), args (ctx, reportFlag, &outrec, expStation, expCmd) -> 1 = OK [V - see 3.4a] |
| ram:c643 | otp_scan_present_bits | scans the 6-word transmission result counters (NP bits etc.) [V] |
| ram:c6ac | otp_station_num_in_range_8_55 (renamed; WAS otp_scan_stations_test4) | returns 1 if 7 < station < 0x38; small helper, NOT a scan [V] |
| ram:c6c2 | otp_scan_get_present_stations (new fn this pass) | test-4 half: get-present-stations to each in-range station, XOR-checks 62 reply words vs hw bitmap [V] |
| ram:c7a1 | otp_scan_identify_yourself (renamed; WAS otp_scan_stations_test5) | test-4 half: identify to each present station, expectedCmd=1, counts duplicate replies [V] |
| ram:c830 | otp_test_check_octobus_config (renamed; WAS otp_check_rft_and_range) | TEST 4 body = calls c7a1 [c846] then c6c2 [c848] [V] |
| ram:c84a | otp_echo_single_send_validate (new fn this pass) | sends cmd 0x0C, classify expCmd=0x0D, cmp payload[4]/[5] vs sent [V] |
| ram:c8b9 | otp_scan_echo_single_word (renamed; WAS otp_probe_all_stations) | TEST 5 body: per station sends FFFF/0000/FFFF/0000 + 16 walking-one + 16 walking-zero patterns via c84a [V] |
| ram:c92b | otp_echo_multi_send_validate (new fn this pass) | sends cmd 0x0E, classify expCmd=0x0F, cmp payload[4]/[5]/[6+k] vs sent [V] |
| ram:c9ea | otp_scan_echo_multi_word (renamed; WAS otp_fill_pattern_buffer) | TEST 6 body: fills 121-word up/down pattern buffer, sends lengths 1..([71ed]/2-6) via c92b [V] |
| ram:b971 | report_pattern_value_error (renamed) | owns str b9e4 "Received pattern is not equal transmitted pattern"; called from [c8b8] [V] |
| ram:cb4f | otp_station_scan_emergency | iterate 0..0x3e presence via EXR + probe [V] |
| ram:ae5b | report_parity_error | owns str aea0 "Parity error when transmitting" [V] |
| ram:aebc | report_dest_fifo_full | owns str af01 "Receive fifo...full" [V] |
| ram:af1d | report_no_answer_from_station | owns str af5d "No answer from station" [V] |
| ram:af73 | report_retry_counter_zero | owns str afb8 "Retry counter equal to zero" [V] |
| ram:ab36 | report_unexpected_source_station_unk | str aba5 "Received message from not expected station" [I] |
| ram:b8c5 | report_pattern_number_error | owns str b93c/b9e4 [V] |
| ram:ba19 | report_string_number_error | owns str ba90 [V] |
| ram:bac4 | report_string_length_error | owns str bb2e/bb55 [V] |
| ram:bb74 | report_word_pattern_error | owns str bbff/bc1b [V] |
| ram:7d9d | test1_transmit_receive_loop | first dispatched, station-table iterate [I] |
| ram:7eda/7f41/7fa8 | test_body_echo_single/multi/config_scan_unk | call c830/c8b9/cb4f [I] |

Loader-named commands: cmd_run @ 7d90, cmd_octobus_test_protocol @ 8617,
cmd_list_octobus_devices @ 7cf6, cmd_list_hardware_configuration @ 7b24,
cmd_select_octobus_station @ 7be2, cmd_decode_status_register @ 7d03,
cmd_force_errors @ 8073, cmd_octobus_facilities @ 80a6, cmd_select_device @ 7b7b.

Key globals (all addresses HEX; earlier draft wrongly suffixed them "B"):
0x71e6 = selected Octobus CONTROLLER number (renamed
selected_octobus_controller_num; written by cmd_select_device @ 7bac, prompt
"Octobus controller number" - NOT the destination station, which is prompted
per command); 0x7198 = per-CONTROLLER context table, 14 words (0xE) per entry
(renamed octobus_controller_ctx_table_14w; ctx[5]=receive param, ctx[6]=own
station number, ctx[10..13]=62-bit hw presence bitmap) - NOT a reply table;
0x71de = ident/interrupt flag cleared by d740; 0x71e3 (otp_recv_timeout_ctr) =
receive countdown, seeded 100. by classify @ c58f; 0x71e7
(otp_all_devices_flag); 0x71e8/0x71e9 (otp_single_station_filter /
otp_filter_station_num) = scan single-station filter; 0x71ed
(otp_max_message_bytes_param, renamed) = max message size in bytes, test 6
uses string lengths up to [71ed]/2 - 6 words.

## 4b. Alignment vs ND-05.020.01 manual + ND-5800 microcode (added 2026-07-16)

Cross-checked against `E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware
Description.md` (Appendix 2, Octobus Protocol v5, frame table ~line 10898-10959 + ACCP
chapter ~line 3928) and the B30 microcode disassembly.

**CONFIRMED by the manual [V]:**
- Frame layout: bit 15 = C, bit 14 = B, bits 13:8 = SOURCE (on receive) / DEST (on send),
  low byte = code with flag bits E=7, K=6, M=5, S=4. SOU/DEST is a **6-bit** field.
- SOMB = flags M+S in the low byte = `0x30 | OMD` (S=1 start); EOMB = M only = `0x20 | OMD`
  (S=0 end) - exactly the envelope in section 1/3.1. (The manual's row for EOMB appears to
  mark the K column - table-conversion artifact; the Appendix-2 bit-value table shows
  E,K,M,S = 0010 for EOMB and 0011 for SOMB, and the definitions text says S=1 start /
  S=0 end of message.)
- **Who answers OMD 0: the ACCP (baby card MC68000 firmware), NOT the microprogram.**
  Manual ~3930: "The ACCP itself responds only to multibyte messages to OMD numbers 0 and 3
  ... OMD 0 is reserved for octobus test programs, OMD 3 for the ACCP library commands.
  Kicks, idents and multibyte messages to OMD other than 0 and 3 are written directly to
  the microprogram via AOB." And ~4071: "there is no octobus driver for handling multibyte
  messages in the microprogram" - corroborated independently by the B30 microcode
  disassembly: OCB_DECODE handles only kick codes (1/2=activate, 3, 4/5, 6), no 0x71C7 /
  multibyte parsing exists anywhere in the control store.
  => Emulator placement in section 5 (station/ACCP class = OctobusND5000Station) is correct;
  the mailbox/microcode engine must never see OMD 0 traffic. The "ACCP" module-type string
  in 3.4 fits: on a 5000 station the test-protocol responder IS the ACCP.
- OMD 3 replies use the Messack/Messnak convention (single byte ack, or nak + error code +
  2 status bytes; returned data follows Messack in the same multibyte message) - relevant
  when the ACCP library commands get implemented.

**DISCREPANCIES - ALL THREE RESOLVED by Ghidra byte-verification 2026-07-16
(sections 1 and 3.2 above are corrected; kept here for the audit trail):**
1. Section 3.2 claims srcStation = bits 13:8 (6-bit, 0..62) but cites the extraction as
   `(raw AND 3400B) SHR 8`. **3400B masks only bits 10:8 (3 bits, max station 7)** - it
   cannot represent the live-tested station 10 (decimal). Either the mask in the binary is
   really 37400B (= 0x3F00, bits 13:8) and the doc dropped a digit (same failure mode as
   the earlier 1404B->140400B trailing-zeros bug in the +5 write format), or the decode fn
   is genuinely 3-bit and something else supplies the station. Byte-verify at ram:d3ae.
   **RESOLVED [V]: data word at ram:d3cb is 0x3F00 = 37400B; full 6-bit field. Doc fixed.**
2. Section 3.2 names bit 14 "M - second marker". Per the manual, **bit 14 is B
   (broadcast/TYPE flag); M is bit 5 in the low byte**. Rename to avoid future confusion -
   the type classification "bits 15/14/5/4" in 3.3 already uses the right positions.
   **RESOLVED [V]: section 3.2 renamed to B; consistent with the proven frame format
   (C=15, B=14, station=13:8, E=7, K=6, M=5, S=4).**
3. Echo-single payload `... 00 0C 00 01 FF FF` reads as cmd 0x000C + pattern NUMBER 0x0001
   + pattern 0xFFFF (reply fields in 3.4 are "Pattern number","Pattern"), not
   "word count + data word" as section 1's table says. Align the two tables after
   byte-checking the builder at ram:cf5a.
   **RESOLVED [V]: caller at ram:c84c-c85a passes an incrementing counter (pattern
   number) + pattern word into otp_build_echo_single_word_cmd0C. Section 1 table fixed.**

## 4c. Station-number / LIST-HARDWARE-CONFIGURATION carve (added 2026-07-18)

Full answer in
[CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md](CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md)
(fresh whole-binary disassembly; file word offset = VA - 0x6B00, validated
against the d080/d3cb constants above). Headlines [all V]:

- **Own station number = INPUT STATUS (+2) bits 13:8, a STATIC thumbwheel
  readback** (ram:d704: read +2, AND 0x3F00, SHR 8), read right after input
  master-clear and BEFORE any transmit; stored to ctx[6] (the 0x7198 table).
  +2 bits 5:4 = Speed -> ctx[7] (ram:d715, mask 0x0030).
- cmd_list_hardware_configuration @ 7b24 runs ram:7242 with phase args 0/1/2
  (jump table ram:731e): phase 0 = controller presence (ctx[1]), phase 1 =
  ram:c1f2 station read + dest-0 self-send cross-check (frame word 0x0000;
  compares pre-transmit ctx[6] vs received +0 bits 13:8; reporter ram:b0a3),
  phase 2 = ram:c4f3/c479 presence-bitmap scan into ctx[10..13] (probes every
  station EXCEPT its own with C-frame 0x8000|st<<8|0xA2; present = +6 bit6
  NP clear, ram:d56a).
- "Clear Device" verify (ram:c180): output +7:=4 then 0x50, poll +6 until
  (s AND 0x01FD)==0x08; input +3:=020B, poll +2 until (s AND 0x3F3D) ==
  ctx[6]<<8 | ctx[7]<<4 | 0x04 - i.e. the +2 station field MUST be valid on an
  EMPTY FIFO.
- Reply/transfer detection everywhere = POLL of status bit 3 (<=100 iterations,
  ram:d591); interrupts (idents 40B/41B -> flags [0x71df]/[0x71e0], code
  [0x71e2]) are enabled and cross-checked as DIAGNOSTICS only (ram:d5b8/c0a9).
- Full +2 and +6 bit maps from TPE's own DECODE-STATUS-REGISTER strings,
  including +6 bit 15 = Master (section 3.3 of the answer doc).
- Refinement to section 3.5's table: reporter b526's own string is "The Octobus
  station number in the Test Protocol response message is not equal the
  station number in the Octobus receive data register" (~ram:b5c7) - it is the
  reply-word-2 vs frame-source check, more specific than the generic
  "not expected station family" label above.
- SINTRAN side: OCSTART never reads a station number (presence = IOX-error
  trap on HDEV+2; 020B to +3 and +7); ND-5000 stations are CONSTANTS
  (CH5CPUPRESENT: 5STATION := ASTAT 070B + cpu index; kicks CMMACLE/CMACONT
  to that station via IOX 100405).

## 5. Emulator context (why this matters)

**IMPLEMENTED 2026-07-16** in RetroCore
(E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\, uncommitted):

- `OctobusStationBase.SendMultibyteMessage` (NDBusOctobus.cs) - the verified
  envelope of section 3.1, one fabric frame per byte, source station in
  bits 13:8 of every delivered frame.
- `OctobusND5000Station.AnswerTestProtocolMessage` + `BuildTestProtocolReply` -
  a GENERIC responder per the section 3.6 recipe: parses any OMD-0 message,
  checks the 71C7 magic, and COMPUTES the reply (present-station list from the
  live fabric registry, echoes from the actual request words, module type 3 =
  ACCP; unknown commands get status 2, unknown register functions status 1).
  Placeholder identity values (processor type 68000, OPCOM version "EMU0",
  test version 1) are settable properties, each marked [UNCERTAIN] pending a
  real-ACCP capture.
- `NDBusOctobus` busy-retry queue - the 136-frame present-stations reply
  overflows the 16-word receive FIFO; frames now park receiver-side and land
  as the FIFO drains, modelling the sender's hardware retry after Ack=10
  (destination busy).
- REMOVED the CONFIG-era hack that forced input ReadyForTransfer after every
  +5 command write: RFT is FIFO status only (the verified interrupt model);
  the stale RFT made a receiver read garbage when a request got NO reply.

Unit tests: 8 new OMD-0 responder tests (identify single-reply rule,
present-stations through the busy-retry path, echo single/multi, module type,
stateful register write + status-1 reject, status-2 unknown command, silent
non-magic) in Emulated.Tests.ND100\ControllerOctobus\OctobusND5000Tests.cs.
Octobus suite 60/60 green, full ND100 suite 289/289 green.
NEXT: rerun TPE OCTOBUS B00 tests 4-6 and CONFIGURATION D05 live as the
oracle (watch specifically for any CONFIG regression from the RFT-hack
removal).

Tests 1-3 pass live; CONFIGURATION D05 reports NO ERRORS (idents 40B/41B
level 13, event-latched one-shot interrupts) - see
[OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md).
