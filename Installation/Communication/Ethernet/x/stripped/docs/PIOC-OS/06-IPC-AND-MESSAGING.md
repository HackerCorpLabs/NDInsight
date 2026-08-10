# PIOC-OS - IPC, ports and messaging

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: **PARTIAL.** Structures, signatures, limits and error codes established. Message buffer
format and the send/receive bodies are NOT yet transcribed - marked clearly below.

---

## 1. Three layers of message passing

PIOC-OS has three distinct mechanisms. They are easy to confuse and are NOT interchangeable.

| Layer | Unit | Used for |
|---|---|---|
| **Events** | a 32-bit bit-set per process | fast wakeup signalling. No payload |
| **Ports** | named endpoints with queued messages | general IPC, `PORT*` / `PONA*` / `POMS*` API |
| **Sub-process queues** | `LNMASPCOMM`, `LNMASPDATA`, `LNCNSPCOMM`, `POCSSPCOMM` | work handoff inside a process, via the `Posi*` wrappers |

Events are documented in `03-PROCESS-MODEL.md` (pending set at `desc+0x76`, wait mask at
`desc+0x7A`) and `05-KERNEL-API.md` (traps 0x09/0x0A/0x0B/0x0C). This document covers the other two.

---

## 2. The port API

| Routine | Address |
|---|---|
| `PORTCREATE` | 0xE73C |
| `PORTNAME` | 0xE8F4 |
| `PORTCONNEC` | 0xE940 |
| `PORTRECEIV` | 0xE994 |
| `PORTSEND` | 0xEAA6 |
| `PONAREGISTER` | 0xED10 |
| `PONALOOKUP` | 0xEE48 |
| `POMSGETMES` | 0xEF68 |
| `POWAITFORLAN` | 0xE6B0 |

This maps closely onto the documented NUCLEUS `nk*` port API (`nkCrePort`, `nkCreName`,
`nkOpenPort`, `nkCreMessage`, `nkSend`, `nkReceive`), which is the ND-wide primitive set - worth
reading alongside, but the names here are the PIOC-OS spelling.

### 2.1 The control tables - all share an 0xAAAA signature

Three statically-addressed tables, each validated the same way: a **word `0xAAAA` at offset +4**.
A mismatch returns **-2** immediately.

| Address | Table |
|---|---|
| **0x2D354** | **name registry** (`PONAREGISTER` / `PONALOOKUP`) |
| **0x2D472** | port pool, **class 0 and 1** |
| **0x2D4F4** | port pool, **class 2** |

The signature is a genuine runtime check, not a constant the compiler folded - it is the "has this
table been initialised" gate. For an emulator it is a cheap way to tell whether the port subsystem
came up.

### 2.2 `PORTCREATE` (0xE73C)

```
class = (byte)arg                     port class
if (class == 0 || class == 1)  pool = 0x2D472
else if (class == 2)           pool = 0x2D4F4
else                           return -9        /* invalid class */

if (*(word *)(pool + 4) != 0xAAAA) return -2     /* pool not initialised */

/* builds an array descriptor over (pool + 6) and calls 0x12168 with a name/id word */
if (result != 0) return -4                       /* already present */
...
```

So there are exactly **three port classes (0, 1, 2)** collapsing onto **two pools**.

### 2.3 `PONAREGISTER` (0xED10) - the name registry

```
registry = 0x2D354
if (*(word *)(registry + 4) != 0xAAAA) return -2

nameLen = descriptor.upper - descriptor.lower + 1
if (nameLen > 0x10) return -8                    /* name too long */
```

**Port names are up to 16 characters** and are passed as a standard PLANC array descriptor
(`{long origo, word lower, word upper}` - 8 bytes, see the frame documentation). This is the
mechanism `*XM-ENNS0` and friends register through.

### 2.4 Error codes at this layer

| Code | Meaning |
|---|---|
| -2 | table signature wrong - subsystem not initialised |
| -4 | entry already present |
| -8 | name longer than 16 characters |
| -9 | invalid port class (not 0, 1 or 2) |

**NOT YET ESTABLISHED**: the message buffer format, the queue link layout inside a port, the
`PORTSEND` / `PORTRECEIV` bodies, and what routine 0x12168 does (it is called with the port name/id
and its non-zero result means "already present").

---

## 3. The sub-process queue layer

Sub-processes are **not** schedulable entities (see `03-PROCESS-MODEL.md`). They are named work
queues serviced from inside one of the three real processes.

| Wrapper | Address | Use |
|---|---|---|
| `PosiGetNextWrapper` | 0x5106 | pop one item |
| `posi_getall_wrapper` | 0x514A | drain the queue |
| `posi_return_wrapper` | 0x518E | return a buffer to its owner |

The canonical consumer loop is `CMDSERVICE` (0x659C):

```
forever {
    buf = PosiGetNext(LNMASPCOMM)
    if (buf == 0) break                       /* queue drained */
    code = buf[0x0A] >> 2
    if (code > maxCode) { buf[0x0C] = -14; posi_return(buf); continue }
    dispatch g_hostCommandJumpTable[code]
}
```

Two request-buffer fields are fixed by that loop: **+0x0A** the command byte and **+0x0C** the
longword status written back. That is the same buffer shape the host command set uses, so the
sub-process queue is carrying host request buffers directly.

Known sub-processes: `LNMASPCOMM` (LNMA command), `LNMASPDATA` (LNMA data), `LNCNSPCOMM` (LNCN
command), `POCSSPCOMM` (POCS command). Created by `POSIINITIALIZE` / `POSISTART`.

---

## 4. The XMSG gateway, and what currently blocks ENNS0

`LOC-XMSG` is one of the nine modules (see `01-MODULE-INVENTORY.md`). Its gateway routines:
`XMSGIOCGAT` 0xBD32, `XGateOpenXmsgAndRegisterPort` 0xBD94, `XMRECEIVER` 0xBED8,
`XGateFreeListInit` 0xC31E, `PocsOpenXmsgServerName` 0xCCCC.

**Known blocker, carried forward from earlier work:** the ENNS0 "Unknown name" failure is not in
the datagram path. The card (RTCOMMON) registers `*XM-ENNS0` through an **MBOXH XMSG conversation
(MON 200)**, and an implementation that posts nothing on the MBOXH queue at 0x4C2 never registers
the name. The fix is record-and-replay of that sequence.

Note the connection to section 2.3: registration goes through the 16-character name registry at
0x2D354, so `*XM-ENNS0` (9 chars) is well within the limit - the failure is that nothing is posted,
not that the name is rejected.

---

## 5. Open questions

- Message buffer format and the port queue link layout
- `PORTSEND` / `PORTRECEIV` / `PORTCONNEC` bodies
- What 0x12168 is (name hash? table search?)
- How a port wakeup reaches the event mechanism - presumably `PORTSEND` ends up at the event poster
  0x2562, but that path has not been traced
- Whether port classes 0/1/2 map to local / remote / gateway, which the shared pool for 0 and 1
  hints at but does not prove

---

## Provenance

Sections 2.1-2.4 were read from the image on 2026-07-26. Section 3's buffer offsets come from
`CMDSERVICE`, also read. Section 4's blocker is a prior finding, restated. Everything not verified
is listed in section 5 rather than guessed at.
