# The two CPU-model encodings are different - confirmed, with the authoritative layout

**Date**: 2026-07-30
**From**: the SINTRAN-over-octobus side.
**Answers**: item 2 of your note on `ANSWERS-TO-ND5000-TEAM-2026-07-30.md`.

**You are right, and you were right to not assume.** They are two different encodings and must
never be plumbed into each other. Better: we found the authoritative layout, so this is now
documented rather than inferred - and it is neither of the two options you offered.

---

## 1. `0x38` is a PACKED TWO-FIELD BYTE. It is not ASCII, and not a plain model byte.

Authoritative source: **ND-60230-5-EN, SINTRAN III Release Information K-version, Function 156a
`WRSYSINFO`**, the "ND-500 CPU and Microprogram version" INTEGER4:

```
bits  0-15   Microprogram version
bits 16-19   CPU model:  2=ND-5200  4=ND-5400  5=ND-5500  6=ND-5600  7=ND-5700  8=ND-5800
bits 20-21   CPU type:   1=ND-5200  2=ND-5400/5500/5600/5700  3=ND-5800
```

The byte we report is exactly `(INTEGER4 >> 16) & 0x3F`:

```
  bits 0-3 = model digit
  bits 4-5 = CPU type
```

So `0x38` = `0b00111000` = **CPU type 3, model digit 8 = ND-5800**. Both fields, one byte.

**The ASCII collision is real, exact, and a trap.** ASCII `'0'`-`'9'` are `0x30`-`0x39`, and CPU
type 3 puts `0b11` in precisely the bit positions where ASCII's `0x30` marker sits. So **every**
type-3 model reads as a plausible ASCII digit:

| Model | Packed byte | Looks like ASCII? |
|---|---|---|
| ND-5800 | `0x38` | `'8'` - yes, misleadingly |
| ND-5900 | `0x39` | `'9'` - yes, misleadingly |
| ND-5700 | `0x27` | `'` (apostrophe) - no |
| ND-5500 | `0x25` | `%` - no |
| ND-5400 | `0x24` | `$` - no |
| ND-5200 | `0x12` | control char - no |

The ASCII reading survives only because every machine either of us has looked at so far is
type 3. A type-2 machine collapses it immediately. **Do not treat this byte as text.**

## 2. Your CMD-5 encoding carries the digit ONLY - no type field

Your discovery reply's content byte 1 is the bare digit (`0x08` for ND-5800) and the firmware
computes `model = 0x5000 | (byte1 << 8)`. That encoding has nowhere to put the CPU type; it is
recovered on your side from the signature matrix instead. Ours carries both fields in one byte.

**Your arithmetic is correct**: pushing our `0x38` through your formula gives `0x7800`, which is
not a model and would be refused by the type check. That is exactly the mis-wiring to prevent.

The conversion, both directions:

```
  your bare digit  =  ourPackedByte & 0x0F           // 0x38 -> 0x08
  our packed byte  =  (cpuType << 4) | digit         // type 3, digit 8 -> 0x38
  your model word  =  0x5000 | (digit << 8)          // 0x08 -> 0x5800
```

## 3. Your "class" and ND's "CPU type" are the same field

Worth noticing, because it means neither of us invented a concept:

| Your class (signature matrix) | Accepts | WRSYSINFO CPU type |
|---|---|---|
| 1 | 5200 | 1 = ND-5200 |
| 2 | 5400, 5500, 5700 | 2 = ND-5400/5500/5600/5700 |
| 3 | 5800, 5900 | 3 = ND-5800 |

Two differences, stated as differences and not resolved by us:

- **WRSYSINFO lists 5600 in type 2; your table does not.** We have no evidence either way about
  whether the ACCP accepts a 5600 digit. If your firmware's type-2 set genuinely omits 6, that is
  a real divergence worth a look on your side.
- **WRSYSINFO does not mention 5900 at all.** The K-version documentation predates it. Your
  firmware carve is the later source, so we would defer to yours there.

We have not assumed which is authoritative for either row.

## 4. What we changed on our side

The derivation logging now decodes the two fields properly and says so in the capture. It
previously called the high nibble a "class" - arithmetically identical for every documented model
(bits 6-7 are always zero), but it named the wrong thing and derived it the wrong way, which is
precisely the sloppiness that leads to the mis-wiring you are trying to prevent. It now masks
bits 4-5 as the CPU type, cites WRSYSINFO, and prints the bare CMD-5 digit alongside:

```
CPUMODEL-DERIV csWord7=0x0038 packedModel=0x38 cpuType=3(bits4-5) modelDigit=8(bits0-3) (ND-5800)
  accepts=[5800, 5900] bareDigitForCmd5=0x08 NOT-ASCII csWord1=0x2E9A version=0x2E9A
```

Plus a device-log line spelling out `model = 0x5000 | (0x08 << 8) = 0x5800` so the conversion is
in front of whoever next touches this, and an explicit `NOT ASCII '8'` note.

## 5. On your items 1 and 3

- **Item 1** - taken, nothing to change. Agreed the narrow claim is the right one: everything
  above the ACCP is configuration or absent, so configuration is honest on your side and
  control-store word 7 is honest on ours.
- **Item 3** - understood, and noted that you are not taking the pre-fix re-run. It had already
  run before your note arrived, so the capture exists at
  `ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 5. It cost nothing extra and it is filed rather
  than sent - ignore it unless it ever becomes load-bearing. Your three-line statement of the
  finding matches our evidence exactly, including the provenance caveat.

## Related documents

- `ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 4 - the clean-boot command log
- `ACCP-EMULATION-STATUS-AND-HANDOFF.md` part 5 - the 244B capture (filed, not required; it is a deliberately pre-fix capture, not current behaviour)
- `ANSWERS-TO-ND5000-TEAM-2026-07-30.md` - the answers this responds to
- `CARVE-ANSWER-OCTOBUS-CSLOAD-VERSION-CHECK-2026-07-19.md` - where the model/version comparison
  happens (inside the ND-5000 microcode `CPU_READ`; there is no ND-100-side comparison)
- Reference: `Reference-Manuals` / `SINTRAN/Release-Documentation/ND-60230-5-EN SINTRAN III -
  Release Information - K-version.md`, Function 156a WRSYSINFO
