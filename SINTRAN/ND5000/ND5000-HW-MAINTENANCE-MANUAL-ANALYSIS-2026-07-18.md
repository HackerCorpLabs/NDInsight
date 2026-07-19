# ND-5000 Hardware Maintenance Manual — Emulation-Relevant Analysis

**Source:** `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-05.017.01 EN ND-5000 HARDWARE MAINTENANCE.md`
(ND-05.017.01, OCR'd 2026-07-18; ~12250 lines). Grade **[DOC-manual]** unless noted; where
it CONFIRMS our independent carve I mark **[V-confirms]**.

This is an extraction of the parts that matter for the ND-100↔ND-5000 emulation
(octobus, mailbox, CPU model/backplane, init/CS-load). It is NOT a full read of the manual;
§6 is a chapter map pointing to what remains for deeper dives.

---

## 1. Octobus station-number map (ch 3.1)  [V-confirms + extends carve]

"A maximum of **62 stations** (processors) can be connected to one bus… all devices are
given a **unique station number**. The octobus is not visible above the low-level OS."

| Station no. (octal) | Device |
|---|---|
| **1** | **ND-100** |
| 2–7 | MFB controllers |
| 10–13 | SCSI controllers (disk) |
| 14–15 | Matra VME |
| 16–17 | Multifunction communication |
| 20 | Hyperchannel |
| 21–23 | FDDI (Fibermet) |
| 24–27 | FPS-5000 |
| 30–33 | Graphic controller |
| 34–67 | Free for expansion |
| **70–76** | **ND-5000** |

- CONFIRMS + EXTENDS our carve: hardware reserves **70–76** for ND-5000 CPUs; SINTRAN's
  carved `FN5DEST=070B / LN5DEST=073B` (4 CPUs) is the SOFTWARE subset it actually addresses
  (CPU 1–4 = 70B/71B/72B/73B, ch 2.1.5). ND-100 = station **1**.
- MASTER arbitration (ch 3.1): lowest station number becomes MASTER → ND-100 (station 1) is
  the natural bus master. Bus lines XRREQ/XCLK/XDAT/XRFO; MASTER pulses XRFO every 15 µs;
  collision = give-up-and-retry with priority increment (the "Lost Access Counter").

## 2. Octobus frame + message format (ch 3.2 / 3.3)  [V-confirms carve]

30-bit frame (+Start/Stop): `Priority | Destination | C | B | Source | Information |
Parity | Ack`. The **16-bit FIFO/wire form** the driver sees:

```
| 15 | 14 13 12 11 10 09 | 08 07 06 05 04 03 02 01 00 |
| C  | B  Dest/Source    | INFORMATION                |
```
- **Dest/Source**: on SEND = destination (or broadcast type if B=1); on RECEIVE = **source
  station number**. ← this is how the receiver learns who sent it.
- **C=1** = information is a command; **B=1** = broadcast to all stations of a type.
- Ack codes: `00`=timeout(15 retries), `01`=OK, `10`=dest busy(255 retries), `11`=parity
  error / ambiguous.

Information-byte sub-decode (CONFIRMS our OCB/E-K-M-S model):
```
E K M S | meaning
1 . . . | Emergency (power-fail, master-clear, reset AQP) — hardware/driver decoded
0 1 . . | Kick  (wake a handler; activate/terminate ND-5000)
0 0 0 0 | Ident (interrupt ND-100 from ND-5000)
0 0 1 1 | Start of multibyte  (SOMB)
0 0 1 0 | End of multibyte     (EOMB)
0 0 . . | (C=0) data byte — part of multibyte
```

**Four message streams (ch 3.3)** — confirms our whole model:
1. **IDENT** → IDENT ENTRIES; "immediately activates a process with correct working set…
   **used to interrupt the ND-100 from the ND-5000 CPU**." ← the answer-back / GIVEINT path.
2. **KICK** → HANDLER ENTRIES; "**Kick number 1** starts scanning the execution queue" in
   the ND-5000 CPU. ← CONFIRMS carved `N100KICK=1 = ACTIVATE` (execution-queue walk).
3. **MULTIBYTE** → OMD; "CMD number **3** handled by the **Access Processor (ACP/ACCP)**";
   used for control-store load + AQP commands. ← CONFIRMS OMD/CS-load path.
4. **EMERGENCY** → hardware/driver decoded.

## 3. THE BACKPLANE-ID ANSWER — who reports the CPU's id over octobus (ch 3.4 + 2.1.5/2.1.6)

Question: with multiple CPUs, who reports the correct backplane/station id back to the
ND-100 over octobus?

**Answer: each ND-5000 CPU reports its OWN id, independently — there is no central
arbiter.** Mechanism, layer by layer [DOC + V-confirms]:

1. **Per-slot MF-backplane EEPROM holds `STATION NO`** (ch 2.1.6; `LIST-CONFIGURATION`
   shows `STATION NO: 0000070B` for a slot, alongside CPU MODEL / POWER FAIL DESTINATION /
   BROADCAST TYPE / SPEED / MASTER CONTROL REG). It is set per slot with the updating tool
   (`SET-CPU-MODEL` + `CONFIGURATE-SLOT`); a 4-CPU ND-5900 is configured CPU 1→70B, 2→71B,
   3→72B, 4→73B (ch 2.1.5). Clearing/losing the EEPROM loses it (ch 2.1.6).
2. **The CPU's LOCAL octobus interface is INITIALIZED with that Station no.** (ch 3.4): the
   octobus hardware on the ND-5000 CPU takes **initiation parameters = {BADAP reg., Speed,
   Station no.}**. So each CPU's octobus transmitter is armed with its own station number at
   init (loaded from the slot's backplane config; exact loader = MF-bus controller / ACCP
   reading the EEPROM [D]).
3. **On transmit, the hardware stamps the `Source` field** with that station number (ch 3.2:
   "Source = station number of transmitting device"). When a CPU answers the ND-100, it
   sends an **IDENT** (ch 3.3 stream 1), and the frame carries its source station.
4. **The ND-100 (station 1) reads the `Source` field** of the received frame to know which
   CPU answered.

So responsibility is DISTRIBUTED: each CPU owns its identity via (its slot's EEPROM →
its octobus interface init → the Source field it stamps). No CPU reports for another; the
octobus guarantees unique station numbers. The microcode's role is only to inherit its
identity from the ACCP/backplane (SYS_READ of SYSPAR/CON5IDENT at INIT_SAMSON; `SAMSON_CPU`
patch constant is the CPU's own number) — it is NOT the authority for the station id.

EMULATION MAPPING [V — our code already models this]: `OctobusFabric` does dest→source
rewrite so each answer frame carries the originating `OctobusND5000Station.StationNumber`;
each station instance carries its own number = the "per-slot EEPROM station no." Multi-CPU
= multiple station instances, each with its own number 70B–73B — matching the hardware
"each CPU reports its own" model. The one thing NOT modeled is the EEPROM/init-parameter
plumbing (BADAP/Speed/Station-no load) — in the emulator the station number is assigned
directly, which is behaviorally equivalent [D].

## 4. CPU model / EEPROM (ch 2.1.6)  [DOC — pairs with the microcode word-7 finding]

- `SET-CPU-MODEL` writes the model (values 2/4/5/7/8) to the per-slot MF-backplane EEPROM;
  `LIST-CONFIGURATION` reads it back (`CPU MODEL: 000007B`). Needs the updating tool's
  special PROMs (350156 Compact / 350157 large cabinet).
- WARNING (ch 2.1.6): `INITIATE-EEPROM` on the MF-controller slot or `CONFIGURATE-SLOT` on
  the CPU slot (with config saved) DESTROYS the CPU-model setting.
- Cross-link: the model the MICROCODE reports to SINTRAN comes from control-store **word 7**
  of the loaded image (see `docs/MC/README.md` §3 + `ND5000-FAMILY-MODELS-REFERENCE.md`
  §1.6); the EEPROM model is the MF-bus hardware's separate copy — an upgrade sets BOTH
  (new microprogram floppy carries word 7 + updating tool writes the EEPROM), which is why
  every upgrade step pairs "replace microprogram version 14xxx" with "set CPU model".

## 5. Init / control-store load / ACCP (ch 3.6, 6.2)  [DOC — relevant to CS-load gate]
- ACCP = Access Processor, with a console (ch 3.6.1) and an ND-5000 self-test (ch 3.6.2);
  self-test failure is a startup error (ch 6.2 "ND-5000 Self-Test Fails").
- Control-store loading (ch 6.2.1) + startup error messages (ch 6.2.2/6.2.3) — the live
  "Loading Control Store" behavior we model on the classic 3022 has an ND-5000/ACCP analog
  here; worth a targeted read when closing CS-load fidelity on the octobus generation.
- MF-bus TEST AND MAINTENANCE PROGRAM (ch 8.5): commands incl. `INIATE-EEPROM` (212),
  `LIST-OCTOBUS-STATION` (215), `OCTOBUS-SELFTEST` (215) — the tooling that reads/writes the
  backplane config discussed in §3–4.

## 6. Chapter map — where to dig next (NOT yet analysed in depth)
- **Ch 4 Hardware Trace Module** — trace memory, software control, dump-on-error, decoding
  tools. Could inform an emulator trace/debug facility.
- **Ch 6.2.2/6.2.3 error messages** — the authoritative ND-5000 startup/error vocabulary;
  useful for matching emulator behavior to real diagnostics.
- **Ch 7 ND-5000 Monitor debug commands** — LOOK-AT-PROGRAM/DATA/STACK/REGISTER/SRF/
  RESIDENT-MEMORY, breakpoints, address trace, trap handling, LIST-MEMORY-CONFIGURATION,
  "List E00 level and version". Directly relevant to the register-window / SRF model and to
  the live-oracle debugging approach (these are the commands to drive in a live capture).
- **Ch 8.5 + card sections (5151/5155/5156/5454/5465/5152)** — MF-bus controller/port cards;
  backplane part 324801; relevant if MF-bus/MPM-5 memory config is ever modeled.
