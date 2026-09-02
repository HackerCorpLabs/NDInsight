# Satellite cabinet

*The smallest ND-100-line machine, and the one that gave COSMOS its name.*

|  |  |
|---|---|
| **Used from** | 1981 |
| **Type** | Cabinet |
| **Family** | [ND-1xx standalone](../cpu/nd-1xx/) |
| **Takes which CPU** | Any one [ND-1xx CPU board](../cpu/nd-1xx/) |
| **Coprocessor?** | No |

## The short version

A small desk-side unit introduced as its own range in 1981, and refreshed with
the ND-110 in 1986. It is the reason ND's networking system was called **COSMOS** -
the name was an internal pun on the recently released Satellite.

## Specification

| Item | Value |
|---|---|
| Format | Small desk-side unit |
| Backplanes | **One** - the ND-100 bus |
| CPU | One 1xx CPU board |
| Sold as | "ND-100 Satellite" from 1981, "ND-110 Satellite" from 1986 |
| Drawings | ND Book 2: B2C4 assembly, B2C5 cable and wiring |
| 3D model | `Hardware/3D-Models/ND100-Satellite.FCStd` and STL parts - measured |
| Named after | ND-COSMOS took its name from this cabinet, as a joke |

## What goes in it

| Slot / backplane | Holds |
|---|---|
| ND-100 bus backplane | 1xx CPU, memory, I/O controllers |

## Disk interface

The ND-110 Satellite product sheet (June 1987) specifies a **"Fixed Winchester
disk drive with controller"** on all four models - 60 MB on the S5 and S9, 125 MB
on the T9 and T17.

In ND's vocabulary **"Winchester" means ST-506**, not merely "a fixed disk".
`Reference-Manuals/Devices/ND-11.015.01 Winchester Disk Controller.md` (1983) is
titled *"5 1/4 inch (ST506) & 8 inch Winchester Disk Controller"* and says: *"The
ST506 (5 1/4 inch Disk Controller, **card 3041**, controls one or two Winchester
Disk drives with the standard 5 1/4 inch interface connection ST506."* The
ND-5000 ES System Administrator Guide writes the pairing as one term -
**"ST-506 (Winchester) disk"**. **[P]**

So **all four Satellite models shipped with ST-506 (MFM) disks**. The S and T
grades differ in capacity, not interface.

ND kept the two interfaces on different cards:

| Card | Interface |
|---|---|
| **3041** | ST-506 Winchester (also prepared for ESDI) |
| **3201** | SCSI and floppy, combined |

**A trap for anyone reading a preserved machine**: ND-110 Satellite 9883.21005 on
ndwiki carries a `3201 SCSI/FLOPPY` card and a modern SCSI drive - but that machine
**arrived with no hard drive at all**, and the SCSI parts are the current owner's
retrofit. It is evidence that the ND-110/120 *could* take SCSI, not evidence of
what the Satellite shipped with.

## Notes

- A photograph survives of an ND-110 Satellite booting SINTRAN III VSX/500 K.
- There is also an **ND-5000 Satellite / Technostation** cabinet (drawings B2C20) - a different, larger thing sharing the name.

## Sources

- **Primary**: cabinet drawings B2C4, B2C5 in the mirror - **not yet imported**
- **Secondary**: [ND-COSMOS](../../sources/en-wikipedia-nd-cosmos.md)

---

*Index: [../README.md](../../README.md). Full context: [../../MACHINE-TIMELINE.md](../../MACHINE-TIMELINE.md).*
