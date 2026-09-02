# ND-100/CE

*An ND-100 with decimal arithmetic, for commercial and administrative work.*

|  |  |
|---|---|
| **Introduced** | 1981 |
| **Type** | Configuration of [ND-100](ND-100.md) |
| **Word length** | 16-bit |
| **Needs a host?** | No, runs on its own |
| **Replaced** | ND-100 |
| **Replaced by** | ND-100/CX (1982) |
| **Survivors** | Not separately recorded |

## The short version

CE stands for Commercial Extended. Scientific code wants floating point;
business code wants decimal arithmetic, because money does not round the way binary
does. The CE option added decimal instructions and stack handling by replacing the
microcode chip - no hardware change. First deliveries started in 1981.

## Specification

| Item | Value |
|---|---|
| CPU | ND-100 CPU, unchanged |
| Control store | Larger microcode PROM in place of the standard one |
| Added instructions | Decimal arithmetic and conversion; stack handling |
| Everything else | As [ND-100](ND-100.md) |

## What was new

- Decimal arithmetic in hardware, aimed squarely at commercial customers.
- Delivered as a chip swap, which set the pattern the CX option followed.

## Sources

- **Primary**: `Reference-Manuals/ND-06.014.2A EN ND-100 Reference Manual.md`
- **Secondary**: [ndwiki ND-100](../../../sources/ndwiki-nd-100.md). Note English Wikipedia wrongly credits the CX instructions to CE - see [ND-100/CX](ND-100-CX.md)

---

*Full context: [MACHINE-TIMELINE.md](../../../MACHINE-TIMELINE.md).*
