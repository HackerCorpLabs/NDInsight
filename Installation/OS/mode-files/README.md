# SINTRAN boot mode files — generic templates

Drop-onto-the-machine templates for the boot/startup chain. The **mechanics** are documented in
[../SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md](../SINTRAN-BOOT-AND-MODE-FILES-GUIDE.md).

> **These files are GENERIC.** Every site-specific value is a `<TOKEN>`. Replace all `<TOKEN>`s
> (and edit the OPTIONAL / VERIFY blocks) before the files will run. Nothing here is tied to one
> machine — the "Example" column below is only a reference, not a default.

## The files

| File | Runs | Save on machine as | Purpose |
|------|------|--------------------|---------|
| `HENT-MODE.MODE` | cold start, **by hand** | `(SYSTEM)HENT-MODE:MODE` | define initial commands, segment file, DMAC, DUMP-REENTRANT, XMSG-LOAD, (COSMOS cold half); hand off to LOAD-MODE |
| `LOAD-MODE.BATC` | warm start, **auto** (via initial commands) | `(SYSTEM)LOAD-MODE:BATC` | thin batch wrapper: log in (password), `@MODE LOAD-MODE:MODE`, MAIL broadcast, terminate |
| `LOAD-MODE.MODE` | called from LOAD-MODE.BATC (or by hand) | `(SYSTEM)LOAD-MODE:MODE` | the real warm-start config: XMSG + links, (COSMOS), SET-AVAILABLE — no password |
| `XMSG-START.MODE` | called from LOAD-MODE.MODE | `(SYSTEM)XMSG-START:MODE` | start XMSG kernel, define names, START-LINK, routing |
| `DUMP-REENTRANT.MODE` | called from HENT-MODE | `(UTILITY)DUMP-REENTRANT:MODE` | make editors/compilers reentrant (start/restart from PD sheets) |

## What to change — substitution table

| Token | Meaning | Where | Example |
|-------|---------|-------|---------|
| `<MAIN-DIR>` | main/system directory name | HENT-MODE | `PACK-ONE` |
| `<DISC-DEVICE>` | boot disk device name | HENT-MODE | `DISC-75MB-1`, `DISC-SCSI-1` |
| `<OTHER-DIR>` / `<UNIT>` | extra disk pack + its unit | HENT-MODE, LOAD-MODE | `PACK-TWO` / `1` |
| `<SEGMENT-FILE>` | segment file name | HENT-MODE | `SEGFILE0:DATA` |
| `<DMAC-BINARY>` | comms/DMA driver BPUN name | HENT-MODE | `DMAC-1915F:BPUN` |
| `<DMAC-SEG>` | segment number for that driver | HENT-MODE | `7` |
| `<SYSTEM-PASSWORD>` | SYSTEM user password | LOAD-MODE, DUMP-REENTRANT | — |
| `<PROJECT>` | project / accounting number | LOAD-MODE, DUMP-REENTRANT | `3200`, `99` |
| `<COSMOS-USER>` | user where COSMOS Basic is installed | HENT-MODE, LOAD-MODE | `COSMOS-BASIC` |
| `<REV>` | COSMOS Basic revision suffix | HENT-MODE, LOAD-MODE | `E04` |
| `<alias-N>` | XMSG symbolic name for a node | XMSG-START | `D100` |
| `<node-nr-N>` | that node's number (**decimal**) | XMSG-START | `100` |
| `<HDLC-UNIT-N>` | HDLC logical unit (**octal**) | XMSG-START | `1360`, `1362` |
| `<via>` | neighbour used to relay to an indirect node | XMSG-START | `D102` |
| `<term-no>` / `<type>` | terminal number / type | LOAD-MODE | `36` / `-5067` |

## Per-file edits beyond the tokens

- **HENT-MODE.MODE** — delete the COSMOS line if COSMOS is not installed; add one
  `@ENTER-DIRECTORY`/`@SET-DEFAULT-DIRECTORY` pair per extra pack. Confirm `XMSG-LOAD:MODE` and the
  DMAC driver actually exist where referenced.
- **LOAD-MODE.BATC / LOAD-MODE.MODE** — split so config isn't duplicated: the `:BATC` is a thin
  wrapper holding only the login (`<SYSTEM-PASSWORD>` — then set `PUBLIC=N FRIEND=N` on it) and the
  MAIL broadcast; the `:MODE` holds the actual config. Edit the COSMOS line / `@START-TADADM` in the
  **`:MODE`**. The initial commands still append the **`:BATC`**.
- **XMSG-START.MODE** — add one `DEFINE-REMOTE-NAME` per node (direct **and** indirect), one
  `START-LINK` per **direct HDLC** neighbour, `DEFINE-NETWORK-CONNECTION …,ENNS0` per **Ethernet**
  neighbour, and a `DEFINE-SYSTEM-ROUTE` for every node reached *through* another. Keep
  `ENABLE-ROUTE-THROUGH` only if this node relays for others. For big networks use `.INCL` files
  (boot guide section 11).
- **DUMP-REENTRANT.MODE** — edit the product list to what the site has. Addresses are product-
  generic (from PD sheets), so they are the same on any machine; the ones marked VERIFY (`FTN`,
  `ASSEMBLER-500`) and those with no PD sheet still need a cross-check. Pick one home for `NRL`.

## COSMOS Basic wiring

COSMOS-specific pieces live with the COSMOS Basic Module, referenced by one line each in HENT-MODE
(`COS-HENT-<REV>`) and LOAD-MODE (`COS-START-<REV>`). See
[../../Communication/COSMOS Basic/COS-BOOT-WIRING.md](../../Communication/COSMOS%20Basic/COS-BOOT-WIRING.md).
