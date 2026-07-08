# The REAL connect-to session-setup — byte-exact reference (two independent captures)

Date: 2026-07-07
Purpose: the multi-chunk display investigation has exhausted burst-shaping (DUMM pacing and the
46 ms gap are both disproven live). The remaining hypothesis set includes **session state
negotiated at SETUP time** (e.g. the OPSV protocol-version handshake) gating the client's
continuation-display path. This document is the byte-exact reference of what a REAL host
(machine 102, genuine SINTRAN/COSMOS) sends during setup, to diff our emulated host's setup
against, field by field.

Sources (in the X25Emulator repo, `pcap/`):
- **Session A**: `conn-to-d102-from-100.pcapng` (the list-files session behind TAD spec §22.16)
- **Session B**: `new-conn-to-102-from-100.pcapng` (first session, connect letter at t=4.84)

All frames FCS-verified, decoded from raw TCP payload. Everything below is [MEASURED].

## 1. The setup ladder (Session A, t=9.62–11.68)

| # | Dir | Frame (F1, chan) | ff/role | Payload (decoded) |
|---|-----|------------------|---------|-------------------|
| 1 | 100→102 | connect letter (00F8, D9) | 86/E4 | `FF 07 2A "TADADM" 00 FE 04 "D102"` |
| 2 | 102→100 | ACCEPT (012F, D8, from port 342) | 86/40 | `01 02 0000 02 02 000A` |
| 3 | 100→102 | SESSION-SETUP (00F9, addr 0x89) | 86/84 | `06 00 1B 00 1C 01 00 FF 00` |
| 4 | 102→100 | **PORT-ASSIGN** (0130, D8, from 342) | 86/40 | `07 05 00 00 66 04C2` + `1F 03 4C 00 00` + `0B 02 03 04` + `15 02 01 08` + `FF 00` |
| 5 | 102→100 | priming DUMM (0131, DB, from 1218) | **92**/00 | `18 00` |
| 6 | 100→102 | terminal setup (00FA) | 86/84 | `0C 01 08` `0D 02 0000` `0F 01 1B` `1F 03 4C 01 04` |
| 7 | 100→102 | ESCA (00FB, class 0008) | 82/94 | `08 00` |
| 8 | 102→100 | ESRS (0132, class 0008) | 86/00 | `20 00` |
| 9 | 102→100 | RESE (0133) → RECO from 100 | **96**/00 | `16 00` / `17 00` |
| 10 | 102→100 | RESE again (0134) → RECO | **92**/00 | `16 00` |
| 11 | 102→100 | BANNER (0135) | **96**/00 | `04 03 010000` + **`00 03 01 01`** + `01 60 <96B banner>` + `13 02 0002` + `01 08 "\r\nENTER "` + `02 00` |

## 2. Full hex, Session A (host frames; FCS = last 2 bytes)

```
ACCEPT      09 22 21 13 00 0e 00 64 00 66 01 2f 04 00 d8 e5 21 00 86 40 00 64 02 ab 00 66 01 56
            04 00 00 41 00 08 01 02 00 00 02 02 00 0a 30 9d

PORT-ASSIGN 09 66 21 13 00 0e 00 64 00 66 01 30 04 00 d8 e4 21 00 86 40 00 64 02 ab 00 66 01 56
            04 00 00 00 00 18 00 07 05 00 00 66 04 c2 1f 03 4c 00 00 00 0b 02 03 04 15 02 01 08
            ff 00 1b b8

prim. DUMM  09 68 21 13 00 0e 00 64 00 66 01 31 01 08 db db 21 00 92 00 00 64 02 ab 00 66 04 c2
            01 08 00 00 00 02 18 00 e2 45

ESRS        09 ee 21 13 00 0e 00 64 00 66 01 32 00 08 dc da 21 00 86 00 00 64 02 ab 00 66 04 c2
            00 08 00 00 00 02 20 00 6a 15

RESE (1st)  09 e0 21 13 00 0e 00 64 00 66 01 33 01 08 db d9 21 00 96 00 00 64 02 ab 00 66 04 c2
            01 08 00 00 00 02 16 00 5d 87

RESE (2nd)  09 44 21 13 00 0e 00 64 00 66 01 34 01 08 db d8 21 00 92 00 00 64 02 ab 00 66 04 c2
            01 08 00 00 00 02 16 00 cc 61

BANNER      09 88 21 13 00 0e 00 64 00 66 01 35 01 08 db d7 21 00 96 00 00 64 02 ab 00 66 04 c2
            01 08 00 00 00 7c 00 04 03 01 00 00 00 03 01 01 01 60 0d 0a 20 32 32 2e 32 37 2e 32
            32 20 20 20 20 20 20 38 20 41 50 52 49 4c 20 20 20 31 39 39 38 0d 0a 20 53 49 4e 54
            52 41 4e 20 49 49 49 20 2d 20 56 53 58 2f 35 30 30 20 4c 0d 0a 2d 2d 2d 20 52 45 54
            52 4f 43 4f 52 45 20 45 4d 55 4c 41 54 45 44 20 4c 20 49 44 3a 31 30 32 20 2d 2d 2d
            0d 0a 13 02 00 02 01 08 0d 0a 45 4e 54 45 52 20 02 00 4c 89
```

And the asker's frames (for the client-role recipe):

```
connect     09 00 21 13 00 0e 00 66 00 64 00 f8 04 00 d9 1c 21 00 86 e4 00 66 00 00 00 64 02 ab
            04 00 00 41 00 10 ff 07 2a 54 41 44 41 44 4d 00 fe 04 44 31 30 32 c2 ae
sess-setup  89 44 21 13 00 0e 00 66 00 64 00 f9 04 00 d9 1b 21 00 86 84 00 66 01 56 00 64 02 ab
            04 00 00 00 00 09 06 00 1b 00 1c 01 00 ff 00 6b 85
term-setup  89 aa 21 13 00 0e 00 66 00 64 00 fa 01 08 dc 12 21 00 86 84 00 66 04 c2 00 64 02 ab
            01 08 00 00 00 0f 0c 01 08 0d 02 00 00 0f 01 1b 1f 03 4c 01 04 d5 52
ESCA        09 cc 21 13 00 0e 00 66 00 64 00 fb 00 08 dd 11 21 00 82 94 00 66 04 c2 00 64 02 ab
            00 08 00 00 00 02 08 00 bd a9
```

## 3. Session B cross-check (independent session, different boot)

Same shapes, byte-for-byte where session-independent:

```
ACCEPT      ... 04 00 00 41 00 08 01 02 00 00 02 02 00 0a          (identical payload)
PORT-ASSIGN ... 07 05 00 00 66 03 41  1f 03 4c 00 00  0b 02 03 02  15 02 01 08  ff 00
prim. DUMM  ... ff=92 role=00, 18 00                                (identical shape)
RESE #1/#2  ... ff=96 then ff=92                                    (same alternation)
BANNER      ... 04 03 01 00 00  00 03 01 01  01 60 <banner> 13 02 00 02  01 08 <ENTER> 02 00
client OPSV ... 1f 03 4c 01 04                                      (identical)
```

Per-session variation (the ONLY differences): terminal port (`04C2`=1218 vs `0341`=833) and
the LUN value byte (`03 04` vs `03 02` — the TAD LU index, LU = 768+XX).

## 4. The invariants to diff an emulated host against (checklist)

1. **OPSV in the port-assign is `1F 03 4C 00 00`** — OS `L` (0x4C), sub-version 0, **protocol
   version 0** — in BOTH captures, while the client offers `4C 01 04` (protocol 4). The host
   NEVER echoes the client's version. Version-gated receive behavior (7UMOD/78MOD are
   documented protocol-≥4 features) makes this the single most suspect setup byte for the
   continuation-display gate. [MEASURED both; the gating consequence is HYPOTHESIS]
2. **CORS payload layout `07 05 00 00 <node> <port16>`** — port is bytes 3–4 of the payload.
   A malformed CORS makes 100 mis-learn the terminal port → wrong magic later (candidate
   explanation for repeating XEIMA -19 after bursts).
3. **Element order in the port-assign is fixed**: CORS, OPSV, LUN, FBSI (`15 02 01 08`), EOP
   (`FF 00`).
4. **The banner's ECKM is the 16-bit form `00 03 01 01`** (00-prefixed), and BMMX is
   `04 03 01 00 00`. The banner chain order: BMMX, ECKM, BDAT(banner), SYCN 0002,
   BDAT("\r\nENTER "), RFI.
5. **frameFlags on the host's 0x0108-class setup frames alternate 92/96**
   (DUMM 92 → RESE 96 → RESE 92 → BANNER 96) in both sessions, while all burst continuations
   are 96 (§22.16). Rule still UNKNOWN — but an emulated host can simply mirror this observed
   per-position pattern.
6. The priming DUMM comes **from the terminal port**, class 0x0108, ff 92, role 00,
   immediately after the port-assign without waiting for its ACK.

## 5. How to use this

Paste the emulated host's logged setup chain (accept → port-assign → priming DUMM → ESRS/RESE →
banner, full hex) next to section 2/3 and diff field-by-field. If all six checklist points match
byte-for-byte, setup is exonerated and the receiver decode (`tad_receive_and_dispatch` @
ram:0x46db, dispatch table @ ram:0x330e in the annotated `cos-conn-to-e02.prog` Ghidra DB) is the
remaining path. If any differ — especially the OPSV protocol byte — fix that first and re-test the
3-frame echo diagnostic.
