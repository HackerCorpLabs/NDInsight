# XMSG — SINTRAN III message system

Everything about **XMSG** (eXchange MeSsaGe), the SINTRAN III / COSMOS / NORD-NET
inter-process and inter-node message system invoked through monitor call **200B**:
the wire format, the programming API, the official ND constant definitions, a C#
implementation, and the superseded notes it was distilled from.

## Folder layout

| Path | Contents |
|------|----------|
| **[DOC/](DOC/)** | The documentation set (below) |
| **[SRC/](SRC/)** | C# protocol library + xUnit tests + the enum generator — see [SRC/README.md](SRC/README.md) |
| **[OLD/](OLD/)** | Superseded / historical notes, kept for provenance only ([OLD/README.md](OLD/README.md)) |
| `XMSG-PL-VALUES-M.INCL` | The **official ND include file** (PLANC `CONSTANT`s) — XMSG version M, 1988-08-18. The source of truth for all numeric values. |
| `XMSG-VALUES-M.SYMB` | The same value set as assembler `SYMBOL`s (flagged `@DEC` = decimal). |
| `xmsg-constants.json` | Machine-readable constants generated from the `.INCL` (consumed by the docs and the C# enum generator). |

## Documentation (DOC/)

| Document | Scope |
|----------|-------|
| [DOC/XMSG-PROTOCOL.md](DOC/XMSG-PROTOCOL.md) | **Wire format** — HDLC framing, LAPB, the 13-byte SINTRAN header, packet subtypes (incl. the `0x03` ACK), the XMSG sub-header, and the ROUTING/TAD/DC/PAD sub-protocols. What the bytes on the line mean. |
| [DOC/XMSG-API.md](DOC/XMSG-API.md) | **Programming / API** — the MON 200B calling convention, T-register option bits, the XROUT "letter" / standard-message format, magic numbers / ports / names, secure-message semantics, and the **complete constant catalog**. |
| [DOC/XMSG-COMMAND-REFERENCE.md](DOC/XMSG-COMMAND-REFERENCE.md) | **Operator utility** — the `XMSG-COMMAND` program (network management commands, tracing, dumps). |

The HDLC hardware / COM5025 / DMA layer *beneath* XMSG is documented separately in
[../HDLC-Frame-Format-Reference.md](../Devices/HDLC/HDLC-Frame-Format-Reference.md).

## Where this knowledge came from (provenance)

- **Official ND symbol files** (`XMSG-PL-VALUES-M.INCL` / `.SYMB`, version M) — authoritative for every constant.
- **[COSMOS Programmer Guide ND-60.164](../../Operations/Cosmos/ND-60164-3-EN%20%20COSMOS%20Programmer%20Guide.md)** (XMSG version J, OCR) — the programming model, the XROUT letter/standard-message byte format, and the service semantics.
- **FCS-validated packet captures** (nodes 100/102/103) in the sibling **X25Emulator** repository (`pcap/`), decoded independently to confirm the wire format and discover the `0x03` ACK.
- **The Wireshark dissector** [../Devices/HDLC/WireShark/hdlc_tcp.lua](../Devices/HDLC/WireShark/hdlc_tcp.lua), which implements `DOC/XMSG-PROTOCOL.md`.

Each documented fact is tagged VERIFIED / SYMBOLS / INFERRED / CAPTURE-SPECIFIC,
with the source-precedence rule (captures > dissector > symbol tables > earlier
prose) recorded in the docs.

## Regenerating the constants / enums

`xmsg-constants.json` is generated from the `.INCL`; the C# enums are generated
from the JSON. See [SRC/README.md](SRC/README.md) for the commands.
