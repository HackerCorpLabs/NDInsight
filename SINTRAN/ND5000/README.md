# ND-5000 (SAMSON) Documentation Index

Documentation for the ND-5000 generation (SAMSON CPUs, Octobus, ACCP access
module, ND-5800 microcode) and the RetroCore emulation of the
ND-100 <-> ND-5000 communication path.

Evidence marking used throughout: [V] = byte/live-verified, [I] =
interpretation/inference, [C] = contradiction, [UNCERTAIN] = explicitly open.

## Reading order

1. **[OCTOBUS-ND100-ND5000-REFERENCE.md](OCTOBUS-ND100-ND5000-REFERENCE.md)** -
   START HERE. The master protocol reference: one true frame format
   (C=15, B=14, station=13:8, E/K/M/S=7/6/5/4), CM* command codes,
   emergencies, kick numbers 1-6, ident resolution (40B/41B level 13),
   debunks + corrections-to-prior-analyses section.
2. **[HANDOFF-OCTOBUS-EMULATION.md](HANDOFF-OCTOBUS-EMULATION.md)** -
   State of the RetroCore emulation and the agreed remaining plan
   (phase 1 shared-memory mailbox, phase 2 mailbox-to-kick wiring,
   phase 3 MON 60B bring-up validation, phase 4 ND-5000-originated
   messages). Includes the lessons/rules that cost real time.
3. **[OCTOBUS-TEST-PROTOCOL-RE.md](OCTOBUS-TEST-PROTOCOL-RE.md)** -
   The OMD-0 "Octobus Test Protocol" (TPE OCTOBUS B00) fully reverse
   engineered: request + reply wire formats, per-command payload layouts,
   status codes, and the emulator reply recipe (section 3.6). The generic
   responder in RetroCore is built from this document.
4. **[CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md](CARVE-ANSWER-OCTOBUS-STATION-NUMBER-2026-07-18.md)** -
   How the own station number is obtained [V]: TPE reads INPUT STATUS (+2)
   bits 13:8 (a STATIC thumbwheel readback, valid right after master-clear
   with an empty FIFO) BEFORE any transmit; the dest-0 self-send (frame word
   0x0000) is only the cross-check (+2-pre-transmit vs +0 frame source).
   Reply detection = poll of +2 bit 3, interrupts cross-checked as
   diagnostics. SINTRAN never reads a station number (ND-5000 stations =
   constant ASTAT 070B + cpu index). Full +2/+6 bit maps from TPE's
   DECODE-STATUS-REGISTER text; new function map for octobus-b00.
5. **[ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md](ND5800-MICROCODE-ACCP-OCTOBUS-CATALOG.md)** -
   Microcode side: every ACCP-touching routine in the ND-5800 control store
   (ACCP_READ/ACCP_WRITE/TRAP_OMESS, OCB_DEC_K kick dispatch), AFLAG
   handshake bits, TRAP_OCBM report formats.
6. **[SINTRAN-OCTOBUS-MESSAGE-CATALOG.md](SINTRAN-OCTOBUS-MESSAGE-CATALOG.md)** -
   The OS side: every octobus message SINTRAN sends/expects (kick call
   sites, CMSYSPAR/CMCPURES multibyte builders, 5OMBREAD receive dispatch,
   CH5CPUPRESENT/OCSTART/XX5CONOMD startup ladders, MF-controller error
   records), with the LMFIELD record layout and M06 constant values.

## Related documentation elsewhere

| Topic | Where |
|---|---|
| Octobus card register layout + protocol intro | [..\Devices\Octobus\](../Devices/Octobus/README.md) |
| ND-500 side of octobus HW + MON 60 subfunction table | [..\ND500\ND500-BUS-OCTOBUS-HW-INTERFACE.md](../ND500/ND500-BUS-OCTOBUS-HW-INTERFACE.md) |
| Shared-memory mailbox / X500DF / X5FIF spec (phase 1) | [..\ND500\ND500-BUS-INTERFACE-REFERENCE.md](../ND500/ND500-BUS-INTERFACE-REFERENCE.md) sections 6.5 + 7.5 |
| ND-500 mailbox message catalog (MON call stop records) | [..\ND500\ND500-MAILBOX-MESSAGE-CATALOG.md](../ND500/ND500-MAILBOX-MESSAGE-CATALOG.md) |
| Carved SINTRAN driver bodies (SOCTO/SKICK/MBSEND/...) | ..\..\tools\sintran-segment-carver\versions\L-VSX-500\re\segments-ref\026-S3IMPIT\026-S3IMPIT.asm |
| NPL sources (PH-P2-OPPSTART, MP-P2-N500, 5P-P2-MON60) | [..\NPL-SOURCE\NPL\](../NPL-SOURCE/README.md) |

External (outside this repo):
- Hardware manual: E:\Dev\Ronny\ND5000UC\manual\ND-05.020.01 EN ND-5000 Hardware Description.md (ch. 5 The Access Module + Appendix 2 Octobus Protocol v5)
- Microcode disassemblies: E:\Dev\Ronny\ND5000UC\microcode\MICRO-5800-B30.md / -A30.md + MAILBOX-MICROCODE-PSEUDOCODE.md
- Emulator + tests: E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\NDBUS\ (NDBusOctobus.cs, OctobusFabric.cs, OctobusND5000Station.cs) + Emulated.Tests.ND100\ControllerOctobus\

## Status snapshot (2026-07-16)

- Octobus frame/card/ACCP layer: DONE and live-verified (TPE tests 1-3 pass,
  CONFIGURATION D05 NO ERRORS, idents 40B/41B level 13).
- OMD-0 Test Protocol responder: implemented from the byte-verified spec;
  unit tests green; awaiting live TPE tests 4-6 rerun.
- Shared-memory mailbox layer (X5SEMA/X5FIF): NOT implemented - phase 1 of
  the handoff plan.

---

*Last Updated: 2026-07-16*
