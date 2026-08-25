# Extracted distribution files — COSMOS TCP/IP

Two complete kits, recovered from real SINTRAN packs rather than from distribution floppies.
Every file verified; **nothing here is damaged**. Per-page evidence in each kit's `TRUST.md`.

| Folder | Product | Version | Source pack |
|---|---|---|---|
| `D02-gateway-and-clients/` | Gateway 211185 + Telnet/FTP/RSH clients 211154 | **D02** / **D01** | c3 |
| `B05-gateway-and-telnet-server/` | Gateway 211185 + clients 211154 | **B05** | Tingo |

Use **B05** if you need an incoming telnet server on the ND-100 side; D02 has no `TNSERV`.

## Layout inside each kit

| Subfolder | Goes to SINTRAN user |
|---|---|
| `TCP-IP/` | `TCP-IP` |
| `SYSTEM/` | `SYSTEM` — the `AIP-*` tables (plus the original install log, reference only) |
| `TCP-COMM/` (D02 only) | `TCP-COMM` — comms module **source**, not needed to run |

## Before you copy any of this onto a pack

Read **[`../COPYING-FILES-TO-SINTRAN.md`](../COPYING-FILES-TO-SINTRAN.md)**.

Two things about these files specifically:

- **Every file has a `.xat` sidecar** carrying its real SINTRAN name, type, access bits,
  dates and sparse-hole map. Windows cannot put `:` in a filename, so
  `TCP-START-D02:MODE` is stored as `TCP-START-D02.MODE` — drive any restore from the
  sidecar, never from the Windows filename.
- **The text files are 7-bit — parity was stripped during extraction.** The original pack
  was *mixed*: `AIP-CONFIG`, `AIP-HOSTS` and `AIP-SERVICES` carried parity while
  `AIP-NETWORKS` and `AIP-PROTOCOL` did not, and `SKP-C00` differs between its own `:DEFS`,
  `:IMPT` and `:INTL`. That distinction is not recoverable from these files or their
  sidecars — the per-file table is in `COPYING-FILES-TO-SINTRAN.md` §2, and a byte-exact
  copy from the source image avoids the question entirely.

**Parent:** [../README.md](../README.md)
