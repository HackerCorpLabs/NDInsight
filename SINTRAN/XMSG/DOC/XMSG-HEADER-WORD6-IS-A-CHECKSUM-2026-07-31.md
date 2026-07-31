# SOLVED: the "channel" and "Counter" are one header CHECKSUM

**Date:** 2026-07-31
**Status:** CARVED from the kernel and verified on **3595 of 3595** frames in the corpus -
every capture, every subtype, both directions, every link.

This supersedes the entire envelope "seed / baseLow / epoch / channel" model, the separate
secure-ACK closed form, and the `baseLow` sign fix committed earlier today. All of it was
curve-fitting to checksum arithmetic.

---

## 1. The result

The SINTRAN header is **seven words**. Word 6 is a **ones-complement checksum over the other
six**, with the checksum field itself counted as zero:

```
w6 == ~ones_complement_sum(w0, w1, w2, w3, w4, w5, 0)      (16-bit, end-around carry)
```

and on the wire that single word is read as two bytes:

```
offset 12 = "Protocol ID" / "channel"   = the checksum's HIGH byte
offset 13 = "Counter"                   = the checksum's LOW  byte
```

**There is no channel. There is no epoch. There is no per-link seed.** Those were all
artifacts of fitting a formula to a checksum whose inputs happen to be stable per link.

## 2. The code

Kernel routine at `137314`, reached from `XSDGM` through the pointer at `137744`; the caller
stores the returned value at `137675  STA ,X 32`, which is w6 itself.

```
137314  RADD CLD SX DD     ; D := X            save block pointer
137315  LDT 16             ; T := mem[137333] = 0o32     the LIMIT
137316  RADD SX DT         ; T := X + 0o32
137317  RADD CLD 0 DA      ; A := 0            accumulator
137320  AAX 24             ; X += 0o24         the START
137321  SKP IF DT MGRE SX  ; while X <= T                0o24..0o32 = SEVEN words
137322  JMP -> 137327
137323  ADD ,X 0           ;   A += mem[X]
137324  RADD ADC CLD SA DA ;   A += carry      END-AROUND CARRY
137325  AAX 1
137326  JMP -> 137321
137327  JAZ -> 137331
137330  RADD CM1 CLD SA DA ; A := ~A           ONE'S COMPLEMENT
137331  RADD CLD SD DX     ; X := D            restore
137332  EXIT
```

The header lives at descriptor offsets `0o24`..`0o32`, which is why `XSCTR` writes `,X 24`,
`,X 25`, `,X 26`, `,X 27` consecutively and `XSACK` assembles subtype `0x03` bit-by-bit with
`BSET` before storing it - these routines are filling in header words, and the checksum is
computed over the finished set.

## 3. Verification

Computed `~ones_sum(w0..w5, 0)` for every FCS-valid frame with Marker1 `0x21` in
`E:\Dev\Ronny\X25Emulator\pcap\`:

| Subtype | Frames | Match |
|---|---:|---:|
| `0x03` Ack | 1671 | **1671** |
| `0x07` NetworkError | 3 | **3** |
| `0x0A` transfer data | 226 | **226** |
| `0x0C` transfer data | 226 | **226** |
| `0x0E` Data | 1449 | **1449** |
| `0x13` ReachReply | 6 | **6** |
| `0x17` (the `0xFD`/`0xFE` family) | 4 | **4** |
| `0x19` ReachRequest | 10 | **10** |
| **total** | **3595** | **3595** |

One formula, no exceptions, no per-subtype special cases. For comparison, the model it
replaces needed a Data-frame formula (1449 frames), a separate ACK closed form (1671), and
had nothing at all to say about the other 475.

## 4. What this explains

- **Why the old formula fit so well.** The checksum is a deterministic function of the same
  header fields the formula used as inputs, so any sufficiently flexible expression over
  `Flags1`/`Flags2`/class was going to correlate.
- **Why a "per-link seed" appeared.** On a given link, `w0`-`w3` (markers, subtype, dest, src)
  are near-constant, so the checksum's dependence on them looks like a constant offset. The
  "seed" was the contribution of the fields nobody was varying.
- **Why an "epoch" term was needed.** That was carry propagation out of the low byte.
- **Why the masked-vs-signed `baseLow` mattered** (fixed earlier today, now moot): that
  distinction was a borrow, i.e. the same carry seen from the other side.
- **Why the receiver validates it and crashes on a wrong value.** It is a header checksum. A
  bad `w6` means a corrupt header, which is exactly the `PERF_CONNCT` / XMSG 24B failure mode
  documented against a "wrong ACK channel".
- **Why `Flags2 == XMCSM` had to hold.** Both are checksum inputs; a mismatch would corrupt
  `w6`.

## 5. What to change

- `XMSG-PROTOCOL.md` section 18.5 - the seed/baseLow/epoch/channel block should be replaced
  by the one-line checksum, keeping the old model only as a historical note.
- The secure-ACK closed form (section on subtype `0x03`) is a special case of the same
  checksum and needs no separate treatment.
- `Xmsg.Protocol\Packet\XmsgEnvelope.cs` - `BaseLow`, `BaseLowSigned`, `ComputeEpoch`,
  `DeriveChannel`, `ComputeCounter` all collapse into one checksum function. **Not yet done.**
- `SINTRAN\Devices\HDLC\WireShark\hdlc_tcp.lua` - the envelope validator should check the
  checksum instead. **Not yet done.**
- `SintranProtocolId` as an enum of "channels" (`0xD8`..`0xDE`, labelled TAD/ROUTING/PAD) is
  meaningless - those are just common checksum high bytes. **Not yet done.**

## 6. Honest note

The value `0xDE` in the old formula, the `0xD8`-`0xDE` "channel" range, the class anchors and
the epoch stepping were all real *observations* - they just had the wrong explanation
attached. The lesson is the one that recurs in this file: an expression that predicts the
wire is not the same as the mechanism, and only the carve settles which you have.
