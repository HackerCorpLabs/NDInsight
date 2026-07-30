# PIOC-OS - Module inventory

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: module directory COMPLETE (walked, circular list closed). Symbol-to-module assignment is
partly inferred - see section 3, where inference is marked as such.

---

## 1. The module directory

A **circular** singly-linked list of 32-byte records. Head at **0x05C8**, which sits immediately
above the initial supervisor stack pointer value (also 0x05C8) - the stack grows DOWN from there,
the directory grows UP.

Record layout (verified against all nine records):

| Offset | Size | Field |
|---|---|---|
| +0x00 | 4 | `next` - pointer to the next record; the last one points back to the head |
| +0x04 | 8 | `name` - 8 characters, space-padded, NOT NUL-terminated |
| +0x0C | 14 | `buildDate` - ASCII, e.g. `"APRIL 21, 1986"` |
| +0x1A | 6 | padding / zero |

Type `/PIOCOS/PiocOsModule` applies to these.

**The list terminates by wrapping, not by a NULL `next`.** A walker that stops on NULL will loop
forever. Stop when `next` equals the head address.

---

## 2. The nine modules, in chain order

| # | Address | Name | Build date | What it is |
|---|---|---|---|---|
| 1 | 0x05C8 | `* NCOM *` | APRIL 21, 1986 | communications nucleus - the `*...*` naming marks it as a core module |
| 2 | 0x05EC | `HDLC-DR.` | **JULY 8, 1986** | HDLC driver |
| 3 | 0x060C | `ASYN-DR.` | APRIL 21, 1986 | asynchronous (serial) driver |
| 4 | 0x062C | `LOC-XMSG` | APRIL 21, 1986 | local XMSG - the message system |
| 5 | 0x0CE0 | `* MAIN *` | APRIL 21, 1986 | main / startup core module |
| 6 | 0x0D0C | `M-MANAG.` | APRIL 21, 1986 | memory manager |
| 7 | 0x0D2C | `PHLS-GEN` | APRIL 21, 1986 | physical-layer general |
| 8 | 0x12B4 | `RT-CLOCK` | **AUGUST 29, 1986** | real-time clock / tick |
| 9 | 0x12D4 | `SHORTLIB` | APRIL 21, 1986 | short (leaf) runtime library |
| — | → 0x05C8 | | | wraps to the head |

### What the build dates tell us

Seven of the nine were built on **APRIL 21, 1986** - a full-system build. Two were rebuilt later
and are therefore the parts that changed after that baseline:

- `HDLC-DR.` on **JULY 8, 1986**
- `RT-CLOCK` on **AUGUST 29, 1986** - the newest module in the image

If a defect is suspected in timing or in HDLC framing, those two carry changes the rest of the
image was not rebuilt against. That is a lead, not a conclusion.

### Process names sit just before their module record

`"RTC "` is at **0x12B0**, immediately before the `RT-CLOCK` record at 0x12B4. This is the
module-owns-its-process-name pattern. It does NOT hold for every module - the `"FREEPRO1"` literal
(sliced into the two process names `FREE` and `PRO1`) is at 0x0C62, nowhere near a record.

---

## 3. Assigning routines to modules

The image carries ND's own linker symbol table at file offset 0x663E0-0x689FF, fully extracted to
`..\ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md` (255 records, 241 unique names). The symbol records
carry an address and a segment (0x10 code / 0x16 data) but **not** a module id, so module ownership
has to come from the name prefix.

**INFERRED from naming - not proven by the symbol table:**

| Prefix | Count (approx) | Likely module |
|---|---|---|
| `LNMA*` | ~40 | LAN Manager - the LANCE/Ethernet MAC layer |
| `LNCN*` | ~30 | LAN Connection layer |
| `POCS*` | ~25 | PIOC Communication Server |
| `POMN*`, `PO*` | ~20 | PIOC Monitor / nucleus (`* NCOM *` / `* MAIN *`) |
| `XM*`, `XG*`, `XROUT*` | ~20 | `LOC-XMSG` |
| `PORT*`, `PONA*`, `POMS*` | ~10 | port / name / message API - nucleus |
| `Heap*`, `Planc*`, `#*` | ~15 | `M-MANAG.` and `SHORTLIB` |
| `Rtc*` | ~5 | `RT-CLOCK` |
| `Lance*`, `INITLANCE`, `STARTMA` | ~15 | `PHLS-GEN` / LNMA boundary |

Treat this table as a navigation aid. Where a routine's module actually matters, confirm it by
address range against the module record addresses rather than trusting the prefix.

---

## 4. What is NOT in this inventory

- **Bank 3** (0x68BDE-0x6C077) has no directory record and its code is unreachable in this build -
  no call, jump or stored pointer to any bank-3 address exists in the image. It is Ethernet
  diagnostic/reporting code whose message tables ARE used by low-bank routines. Do not treat it as
  a tenth module.
- The **three copies of the PLANC leaf runtime** (around 0x134xx, 0x44xx, 0x6BCF0) are per-linked-
  bank duplicates of `SHORTLIB`, not separate modules. 0x4478 is byte-identical to 0x13524.

---

## Provenance

The directory walk, the record layout, the circular termination and all nine names and dates were
read from the image on 2026-07-26. Section 3's module assignment is explicitly inference from
naming and is labelled as such.
