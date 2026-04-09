# Octobus Interface Documentation

This folder contains documentation for the Norsk Data Octobus communication interface used for ND-100/ND-500 inter-processor communication.

## Contents

| File | Description |
|------|-------------|
| [OCTOBUS-PROTOCOL-REFERENCE.md](OCTOBUS-PROTOCOL-REFERENCE.md) | Complete protocol reference including register layout, command codes, message structure, and transmission mechanisms |
| [octobus_protocol_frame_format_and_introduction.md](octobus_protocol_frame_format_and_introduction.md) | Frame format and protocol introduction |

## Overview

The Octobus is a high-speed serial interface used to connect the ND-100 control processor with ND-500 computation processors (SAMSON). It uses dual 4-register controllers:

- **Input Controller** (Base 100400): Receives data from Octobus
- **Output Controller** (Base 100404): Transmits data to Octobus

### Key Features

- 16-bit frame-based communication
- Interrupt-driven with ident codes 40 (input) and 41 (output)
- Support for single-frame and multi-byte messages
- "Kick" mechanism for CPU signaling
- Loopback mode for diagnostics

## Related Documentation

- [../ND500/](../ND500/) - ND-500 processor documentation
- [../../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md](../../OS/06-MULTIPORT-MEMORY-AND-ND500-COMMUNICATION.md) - 5MPM shared memory
- [../../NPL-SOURCE/NPL/MP-P2-N500.NPL](../../NPL-SOURCE/NPL/MP-P2-N500.NPL) - ND-500 monitor routines
- [../../NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL](../../NPL-SOURCE/NPL/PH-P2-OPPSTART.NPL) - Octobus initialization (OCSTART)

## Source References

The documentation in this folder is derived from analysis of:
- `PH-P2-OPPSTART.NPL` - Octobus startup routines
- `MP-P2-N500.NPL` - ND-500 monitor routines including XKICK500, XRS5CPU, 5OMBREAD
- `CC-P2-N500.NPL` - ND-500 command processing
- `N500-SYMBOLS.SYMB.TXT` - Symbol definitions for command codes

---

*Last Updated: 2026-01-31*
