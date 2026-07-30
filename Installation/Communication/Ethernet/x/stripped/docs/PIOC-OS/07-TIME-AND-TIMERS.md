# PIOC-OS - Time, timers and the tick

**Image**: `encos-ser-all-banks-68k.bin`
**Date**: 2026-07-26
**Status**: cancel path fully decoded; arm path partial.

---

## 1. The tick source - MFP timer

`RtcInit` (0x3AD2 region) programs the MFP:

- clock **2457600 Hz**
- period descriptor **{1, 200}**
- enable with `*(byte *)0x00EF00D5 |= 0x20`

The ISR is at **0x3A68**, reached through **68000 vector 69** (table offset 0x114). It bumps the
two tick counters:

| Address | Role |
|---|---|
| **0x0FC2** | primary tick counter |
| **0x0FCA** | second counter |
| 0x0FC6 | snapshot destination - trap 0x0D copies 0x0FC2 here when arming |

The MFP register window is at 0x5E8.

`RT-CLOCK` is a module in its own right (record at 0x12B4) and carries the **newest build date in
the image, AUGUST 29 1986** - four months after the rest. If timing behaviour ever looks
inconsistent with the other modules, that is where the divergence would be.

The RTC **process** (handle 3, entry 0x3BFE) runs at **priority 14**, the highest of the three, so
timer work preempts everything else. See `04-SCHEDULER.md`.

---

## 2. Timer element storage

- **32 elements of 18 bytes each**
- **0x0FC0** - active timer count (decremented on unlink)
- **0x0FD6** - list head
- **0x0FCE** and **0x0FD2** - two separate lists an element is threaded on simultaneously

Element fields established from the cancel path:

| Offset | Size | Meaning |
|---|---|---|
| +0x00 | 4 | `next` link (link offset 0 - `#REMV` is called with D0 = 0) |
| +0x04 | 2 | **id / owner word** - matched against `arg[0]` on cancel |
| +0x06 | 4 | **key** - matched against `arg+2` on cancel |

---

## 3. Cancel - trap 0x0E, handler 0x3FBE, unlink at 0x3D9E

```
found = 0
for (e = *(0x0FCE); e != NULL; e = e->next) {
    if (e->id != arg[0])                    continue
    if (e->key != arg.key && arg.key != 0)  continue     /* key 0 = WILDCARD */
    unlink(e)                                            /* 0x3D9E */
    found = 1
}
if (!found) return -6
```

The unlink at **0x3D9E** does three things:

1. `subq.w #1, (0x0FC0)` - decrement the active count
2. if the element **is** the head at 0x0FD6, advance the head to `element->next` **first**
3. `#REMV` (0x4432) with D0 = 0 against list **0x0FCE**, then again against list **0x0FD2**

Both removals happen unconditionally. An emulator that threads elements on only one list will
diverge.

**`arg+2 == 0` is a wildcard** that cancels every timer with the matching id regardless of key.
That is an observable behaviour worth reproducing exactly.

---

## 4. Arm - traps 0x0D and 0x18

### Trap 0x0D (handler 0x3EA6)

```
(0x0FC6) = (0x0FC2)                       snapshot the tick counter
resolve handle arg[0] via 0x29B0          must be a valid object
require arg[0x0E] in {1, 3}               mode word
require arg[0x0A] <= 0xA3D70A             range check on the time value
```

- The **mode word at +0x0E accepts only 1 or 3.** What distinguishes them is not established;
  one-shot versus repeating is the obvious guess and is **not** confirmed.
- The constant **0xA3D70A = 10,737,418** is the upper bound on the time value at +0x0A. Note
  `2^32 / 400 = 10,737,418.24`, so the bound is plausibly "the largest value that will not overflow
  when scaled by 400", but that relationship is arithmetic coincidence unless the scaling is found.
  **Not confirmed.**

### Trap 0x18 (handler 0x403A)

Requires the argument-block version word to be **2**, not the 1 that most services expect, then
calls **0x3E00**. Same timer family; the distinct version number suggests a later-added or extended
form of the arm call.

**0x3E00 has not been transcribed** - this is the main remaining gap in the timer subsystem.

---

## 5. The label "wait / await event" for trap 0x0D is wrong or at least unproven

Earlier analysis recorded trap 0x0D as "wait / await event, descriptor `{word id, long mask, long,
long, word}`". Its body is timer work and it pairs structurally with trap 0x0E (cancel), so
**"arm timer" fits the evidence better** - but a wait-with-timeout would look very similar from
outside, and the descriptor shape quoted is compatible with both.

Recorded as unresolved. Do not treat either label as settled.

---

## 6. Open

- `0x3E00`, the trap 0x18 arm path
- What mode values 1 and 3 select
- Whether the 0xA3D70A bound corresponds to a 400x scaling
- How an expiring timer signals its owner - presumably by posting an event through 0x2562, which
  would make timer expiry preempt via the mechanism in `04-SCHEDULER.md`, but that link has not
  been traced
- The relationship between the two tick counters 0x0FC2 and 0x0FCA

---

## Provenance

Sections 2 and 3 were read from the image on 2026-07-26. Section 1's MFP figures and section 4's
constants are prior findings re-confirmed against the handlers. Every unproven inference in this
document is labelled.
