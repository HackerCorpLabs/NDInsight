# Octobus ND-5000 CPU-presence detection - CARVE ANSWER (2026-07-18)

Answers `E:\Dev\Ronny\ND5000UC\CARVER-REQUEST-OCTOBUS-CPU-PRESENCE-2026-07-18.md`.
Sources: `../NPL-SOURCE/NPL/MP-P2-N500.NPL` (5OMBREAD 3453-3536, CON5IDENT 3614-3634),
`../NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL` (CH5CPUPRESENT 3903-3945), and
[`SINTRAN-OCTOBUS-MESSAGE-CATALOG.md`](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md) (sections 4.3/5/6).
Tags: **[V]** = byte/source-cited; **[I]** = inferred; **[OPEN]** = not carved.

## VERDICT (one line)

Mechanism **(a): an octobus multibyte ACK frame.** SINTRAN's "CPU present" = the `5ALIVE` bit in
`CPUAVAILABLE`, and that bit is set **only** by `5OMBREAD` when the ND-100 receives a multibyte
message from the SAMSON station whose `ETYPE` high byte = `MFACK (0)` - the Ack to the `CMSYSPAR`
("WriteSysPar / alive") message. Your station consumes the CMSYSPAR but sends no such Ack, so
`5ALIVE` never sets and the monitor prints "No ND-500(0) CPU found." **NOT** a `3RMICV` mailbox
answer, **NOT** a GIVEINT, **NOT** an ACCP status bit. [V]

## Q1 - the presence flag and who sets it

"CPU present" is decided by `CPUAVAILABLE BONE 5ALIVE` - set at **MP-P2-N500.NPL:3470**
(octal `146616`), inside `5OMBREAD`:
```
146565  IF "LMFIELD".MOCTSTATION>=FN5DEST AND A<=LN5DEST THEN   % source is a SAMSON (70B..73B)
146575     X.ETYPE=:D SHZ -10=:CSTS ; A:=D/\377=:CMICP           % CSTS = ETYPE high byte
146607     IF CSTS=MFACK OR A=MFNACK THEN                        % Ack/Nack on WriteSysPar
146616        CPUAVAILABLE BONE 5ALIVE=:CPUAVAILABLE ; GO I5OMBR % <-- "I'm present"
```
[V]. `CON5IDENT` sends the alive message and returns via `GO I5OMBR` to wait for exactly this Ack
(its own comment: *"Send 'alive' message to the ACCP to verify that it's present ... Ack/Nack answer
is handled by 5OMBREAD"*, 3606-3609). [V]

The **J04 monitor's** "No ND-500(0) CPU found" print-site is not itself carved (the message catalog
section 9 notes J04's octobus paths are unanalyzed) - but the flag it reflects is `5ALIVE`. The J04
monitor runs the ND-500 bring-up through SINTRAN; with `5ALIVE` clear it declares the CPU absent.
[V for the flag; [OPEN] for the exact J04 test address.]

## Q2 - what response is expected after the `@nd-500` CMSYSPAR

**(a) An octobus reply frame** - specifically a **multibyte message** that `5OMBREAD` reads off the
reserved OMD (`5OMDNO`) via `OMBREAD` (3462), from the SAMSON source station, carrying:
- source station in `MOCTSTATION` in range `FN5DEST..LN5DEST` (the SAMSON, 70B). [V, 3463]
- word at the `ETYPE` position with **high byte = `MFACK` (= 0)** (low byte `CMICP` = reporting
  source, e.g. 1). `CSTS = ETYPE >> 8 = MFACK` is the trigger. [V, 3464-3467]

That is all `5OMBREAD` needs to set `5ALIVE`. Not (b)/(c)/(d):
- (b) `3RMICV`/CPU-type is a LATER mailbox exchange, not the presence gate. [I]
- (c) no GIVEINT/level-13 doorbell is tested for presence; the Ack rides the normal multibyte-message
  receive path (whatever OMD interrupt/`ID12` already drives `5OMBREAD`). [V - 5OMBREAD is the only
  5ALIVE writer]
- (d) no ACCP status-register bit is read for presence. [V]

**Emulator action (OctobusND5000Station):** after consuming the CMSYSPAR, send a multibyte message
**back on OMD `5OMDNO`** (= 3 here; see Q4) from your SAMSON station (70B) with the command/ETYPE
word high byte = `MFACK` (0). A minimal `ETYPE = 0x0000` (or `0x0001` with CMICP=1) satisfies
`CSTS=MFACK`, `MMSGLENGTH = 1`. Then `5OMBREAD` sets `5ALIVE` and `@nd-500` finds the CPU. [V for the
trigger condition; the exact SOMB/EOMB byte framing is the same multibyte envelope your station
already PARSES for the inbound CMSYSPAR, emitted in reverse.]

## Q3 - why boot differs from `@nd-500`

They mark presence by **two different bits**:
- **Boot `CH5CPUPRESENT`** (PH-P2-OPPSTART.NPL:3903-3945) probes the card (`IOXT 100406`, wait RFT
  bit 3), sends emergencies `241B` (CMMACLE master-clear) + `242B` (CMACONT continue-ACCP), then
  sets `MIFLAG MUDOM` + **`CPUAVAILABLE := SAMSON`** - the *interface-type* bit. It does **not** set
  `5ALIVE` and does **not** send CMSYSPAR. (Contrast: the OLD-500/3022 branch sets `OLD500 + 5ALIVE`
  immediately, because a 3022 is alive the moment it answers `RSTA5`.) [V, catalog 6.1 / lines 178,186]
- **`@nd-500`** runs the 5PIT warm bring-up `XX5CONOMD` (RP-P2-N500.NPL:944-999): `CON5OMD` (connect
  receive OMD) -> `MFPREPARE` per MF station -> **`CON5IDENT` per SAMSON**, whose Ack via `5OMBREAD`
  sets `5ALIVE`. [V, catalog 6.3]

So boot establishes *"a SAMSON interface exists"* (card + emergencies), and `@nd-500` performs the
*alive handshake* that the monitor actually gates "CPU found" on. This is exactly your trace: boot =
`241B`/`242B` only; `@nd-500` = the `CMSYSPAR` whose missing Ack is the failure. Boot does not
re-verify aliveness; `@nd-500` is where presence is truly confirmed for a SAMSON. [V]

## Q4 - decode of CMSYSPAR body `03 07 0E 01 03 00 00 00 00`

Against the `CON5IDENT` builder (MP-P2-N500.NPL:3622-3632):
| byte | value | field | meaning |
|---|---|---|---|
| [0] | `03` | `MOCTOMD` | OMD number = OMDACCP = **3** (matches the SOMB/EOMB `OMD 3` frames) |
| [1] | `07` | `MMSGLENGTH` | message length = 7 |
| [2] | `0E` | `MCOMMAND` hi | `CMSYSPAR` = `016B` = 0x0E ("write system parameters") |
| [3] | `01` | `MCOMMAND` lo | `N100IDENT` = 1 (the ND-100's octobus ident) |
| [4] | `03` | `S5` hi | **`5OMDNO SHZ 10` = 5OMDNO = 3 = the reply-to OMD** the host listens on |
| [5..8] | `00 00 00 00` | `S5` lo / `S6` / `S7` | zero (`0=:X.S6=:X.S7`, 3629) |

[V]. **Yes - the message carries the reply-to OMD**: field `S5` (byte[4]) = `5OMDNO` (= 3). The ACCP
must send its Ack back on that OMD. In this config `OMDACCP = 5OMDNO = 3`, so replying on the OMD you
received it on is also correct, but the authoritative source is `S5`/byte[4]. [V]

(`MBROADCAST = 0` at builder line 3624 is folded into the `MMSGLENGTH`/`MOCTOMD` framing, not a
separate visible payload byte here.) [I]

## Bottom line for the emulator

Add to `OctobusND5000Station`: on receiving a `CMSYSPAR` (016B) on OMD 3, reply with a multibyte
message **on OMD `5OMDNO` (= `S5`/body[4] = 3)** from station 70B whose command/ETYPE word has high
byte `MFACK (0)`. That single Ack sets `CPUAVAILABLE.5ALIVE` in `5OMBREAD`, and `@nd-500` will find
CPU 0. No mailbox `3RMICV`, no GIVEINT, no status bit is required for presence. [V]

## Still OPEN (honest)
- The exact J04 monitor instruction that reads `5ALIVE`/prints "No ND-500(0) CPU found" (J04 octobus
  paths not carved; the SINTRAN-side flag is proven). [OPEN]
- Whether the ACCP's real Ack also carries a nonzero `CMICP`/payload (`5OMBREAD` ignores it for the
  present/absent decision - only `CSTS=MFACK` matters). [I]
- Exact on-wire SOMB/EOMB byte packing of the reply (mirror of the inbound envelope your station
  already parses). [I]

*Status-of-record: [`SINTRAN-OCTOBUS-MESSAGE-CATALOG.md`](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md) section
4.3/10 (reply shape now [V] - see update).*
