# Handoff: ND cabinet reconstruction for 3D modelling

**Date**: 2026-08-02
**Goal**: dimensions and geometry sufficient to build and 3D print scale models of Norsk Data
machines, starting with the ND-100/CX Compact.

---

## 1. Where everything is

| What | Where |
|---|---|
| **The record of findings** | `E:\Dev\Ronny\NDInsight\Hardware\ND-PHYSICAL-MODELS.md` |
| Drawing set (sections, isometrics, plans, elevations, panels, logo) | https://claude.ai/code/artifact/b38e0d7d-8a21-475e-b309-79f5799bcbb8 |
| **Open measurement worksheet** with coded callouts | https://claude.ai/code/artifact/fc3c69c7-6625-43db-a93a-58cecfd4176c |
| Photographs of the real machines | `\\Nas9t\data\NorskData\Pictures\ronny\` |
| Other ND photos, brochure, panel parts, logo artwork | `\\Nas9t\data\NorskData\Pictures\` |
| Manual library (7.3 GB) | `E:\Dev\Ronny\mirror-sintran-com` |
| Panel PROM / part-number notes | `E:\Dev\Repos\Ronny\nd-120\Code\68705\readme.md` |

The repository author **owns a running ND-100/CX Compact and an ND-100 Satellite**. Direct
measurement of those machines outranks every manual, and has already been shown to.

---

## 2. State of the ND-100/CX Compact

### Measured, trust these

| Item | Value |
|---|---|
| Overall | 540 wide x 690 high |
| Depth, cabinet only | **710 mm** (620 box + 90 wedge) |
| Depth, including rear fans | **745 mm** |
| Front projection at the roof | +50 mm |
| Front projection at the lip | +90 mm, at 370 mm above the floor |
| Front projection at the grille face | +45 mm |
| Chamfer at the base | 45 degrees, 20 mm run, down to +25 mm at 10 mm height |
| Grille: flat top / louvred / count | 80 mm flat, 270 mm louvred, **17 louvres**, about 16 mm pitch |
| Base frame | 40 mm metal, side panels rest on it |
| Castor clearance | about 10 mm, wheels not modelled |
| Rear fans | 3 off, 120 mm square, 35 mm out the back, 30 mm apart, tops 40 mm below the roof |
| Operator panel fascia | **455 x 53 mm** (from the PCB pad grid, +/- 2 percent) |
| Cabinet plan | square - upper and lower sections are the **same width** |

### Structure

- **Five mouldings** per ND-830102.1B section 2.1.1: front (one screw at the top), two side
  panels (two screws, half turn), top panel (four screws), rear panel (four screws).
- **Two-tone**: dark warm brown on the front panel and base frame; lighter greyer brown on
  the side and top panels; cream badge; black operator panel and recess linings.
- **The top section is three planes**: outer wedge face (badge), a **vertical** recess back
  wall carrying the black modules, and an **outward-angled** operator panel face.
- The badge is an **applied label with rounded ends**, 5 mm clear above and below - a decal,
  not moulded geometry. Model text varies: ND-100/CX, ND-110/CX, ND-5000/CX, tpServer A25.
- The **ND mark is a dot matrix**: 18 x 8 square grid, 120 dots, dot diameter 0.93 of pitch,
  and the gap between the N and the D is 1.5 pitches rather than 1.

### Not measured

Everything on the open worksheet. The single most valuable one is **C1, the recess depth** -
with it, the sloping recess wall angle can be calculated from the known 10 mm run instead of
guessed.

---

## 3. State of the ND-100 Satellite

Only the **230 mm width** has been measured, and it matches the published table. Front face
layout is known from photographs: badge, drive bay with a **5 1/4 inch floppy turned 90
degrees on its side**, cream keyswitch plate (LOCKED / ON / STANDBY) and cream button plate
with six buttons in two groups of three, then the same grille profile as the Compact but
narrower. Colours match the Compact, but the control plates are **cream with dark legends**,
the inverse of the Compact's black panel.

Its section drawing is entirely borrowed from the Compact and is drawn dashed for that
reason. 19 values are open on the worksheet.

---

## 4. Traps - things that already went wrong

1. **The grille face is +45, not +20.** The 20 mm figure is the run of the base chamfer. This
   error propagated through several revisions and made the lip look twice as deep as it is.
2. **The fans are on the REAR, not the top.** Their 35 mm projection is what reconciles the
   measured depth with the published 740 mm. Before that was known, the manuals looked wrong;
   they were not - the comparison was.
3. **Back-face culling matters in the isometric.** The underside of the overhang faces
   downward and must not be drawn from an above viewpoint. Drawing it produced geometry that
   read as physically impossible.
4. **Always rasterise the SVG and look at it before publishing.** Doing so caught label
   collisions and a blank grille face that inspection of the markup did not.
5. **The brochure cabinet photo cannot be measured.** It is a phone photo of a *bound*
   brochure, so the page curves; width-scaling and height-scaling disagree by 4 percent and
   perspective rectification cannot fix page curvature.
6. **Two operator panel variants exist**, same size, different keyswitch legends:
   ND-323163 / PCB 1835B reads LOCKED / ON / STANDBY; ND-323165 / PCB 1844B-2 reads
   ON LOCK / ON / OFF. The machine in the room is the 1835 variant.

---

## 5. The one technique worth reusing

**PCB photographs contain their own ruler.** Through-hole pads sit on a 2.54 mm grid.
Autocorrelate the brightness along a pad row, fit about 30 autocorrelation harmonics through
the origin, and the image yields an absolute mm/px scale with no assumed dimension anywhere.
Two independent photographs of the same board agreed to 0.2 percent, which is what makes the
455 x 53 mm operator panel trustworthy.

It fails on sparsely populated boards - it was tried on the ND-322691 panel board and
returned a broad envelope rather than a comb, so that panel has **no** published size, only a
measured aspect ratio of 1.847 and an assumed width.

---

## 6. Next steps

1. Collect the worksheet answers, starting with **C1**.
2. Recompute the recess wall angle and redraw the top section as three correct planes.
3. Confirm whether the dark area is one opening with a bar or two separate openings.
4. Confirm whether the three ribs on the side panel top are open vents or solid mouldings.
5. Then the Compact is complete enough to model and print; the Satellite follows once its 19
   values are in.

Longer term and untouched: the **large 1690 x 600 cabinet** has no front detail at all, and
the **ND-100 6-module cabinet**'s six bay heights were never recovered.
