# HANDOFF - ND 246 NOTIS Terminal 3D model (2026-08-04)

Goal: put 1980s ND terminals on the office desks in the Unity NorskData sim,
replacing the modern flat screens from the Brick Project Studio pack.

## What exists now

| File | What |
|---|---|
| `E:\Dev\Ronny\NDInsight\Hardware\3D-Models\ND246-Terminal.FCStd` | FreeCAD source, 11 objects |
| `E:\Dev\Ronny\NDInsight\Hardware\3D-Models\ND246-Terminal.obj` | Unity mesh, 21124 tris, full mm |
| `E:\Dev\Ronny\NDInsight\Hardware\3D-Models\ND246-Terminal.mtl` | Colours (one material per object) |

NOT yet committed to git. NOT yet imported into the Unity project.

## Import into Unity

Same convention as the cabinets: copy BOTH .obj and .mtl to Assets/Models,
scale 0.001, rotation X=-90, machine front faces -Z after that. The terminal
stands on z=0 (floor/desk plane); put it on a desk top, not the floor.
Monitor is centred on x=0; the keyboard lies in front of it (its rear edge
120 mm in front of the monitor foot edge). In Unity you can move the
keyboard objects separately if the desk layout needs it.

## Dimensions - FACTORY numbers, not estimates

From the ND-246-A1 product sheet and the TDV 2215 datasheet (both print the
mechanical dimensions):

- Monitor cabinet: 380 W x 310 H x 362 D mm, 14 kg.
- Stand: round base dia 340 mm; cabinet bottom 130-220 mm above the desk
  (model uses 175); tilt 10 fwd / 15 back, swivel 30 both ways; 4 kg.
- Keyboard: 486 W x 235 D, 30 mm at the middle row, 6 degree slope,
  121 keys, 2 kg.
- Screen: 15 inch CRT, text area 260 x 195 mm, green text on dark green.

Sources (local files):
- `E:\Dev\Ronny\mirror-sintran-com\mirror\library\libpdpi\ND-246-A1-EN.pdf`
  (page 1 photo = the look; page 3 = dimensions)
- `E:\Dev\Ronny\mirror-sintran-com\mirror\library\libother\TDV-2215-01-EN.pdf`
- Keyboard colour photo: `E:\Dev\Ronny\RetroTerm\spec\TDV2200\kbd-ND246-1.jpeg`
- Whole terminal spec corpus (protocol, ROM disassembly, per-key pixel
  widths): `E:\Dev\Ronny\RetroTerm\spec\`

## Model contents (objects and colours)

| Object | Colour | Notes |
|---|---|---|
| Stand | beige 0.84/0.81/0.72 | round foot dia 340 + neck column |
| Cabinet | beige | filleted box, bezel recess, top vent slots |
| Bezel | near-black 0.13/0.11/0.10 | dark panel in the front recess |
| Screen | dark green 0.06/0.13/0.09 | flat glass 280x210 |
| Badge + BadgeText | silver / dark | "NORSK DATA" below the screen |
| KeyboardCase | beige | 6-degree wedge with raised rear ledge |
| LegendStrip | orange 0.85/0.58/0.18 | the PUSH-key legend band |
| KeysWhite (68) | 0.90/0.88/0.82 | alphanumerics + numpad digits + space |
| KeysOrange (30) | 0.90/0.62/0.20 | ESC, MARK..WORD, edit block, F-keys |
| KeysTan (21) | 0.64/0.55/0.45 | P1..P8, LOCAL, arrows |

Key colour split matches the real ND-246 keyboard photo.

## EYEBALLED (marked, fix if a real unit is ever taped)

- Bezel recess size (340x280), vent slot field, neck column cross-section.
- Key row placement and counts per block (the photo layout, simplified:
  round cylinder keycaps at 19 mm pitch; real per-key widths exist in
  `E:\Dev\Ronny\RetroTerm\spec\Keyboards\KeyBoard-Size-ND246.md` if a
  true-layout keyboard is ever wanted).
- No coiled keyboard cable modelled.

## Next steps (not done)

1. Commit the three files to git (NDInsight repo).
2. Copy .obj+.mtl into `E:\Dev\Ronny\UnityDev\NorskData\Assets\Models`,
   place one on each office desk, remove/hide the flat screens.
3. Optional later: emissive green screen material in Unity, or a live
   RetroTerm texture on the screen quad.
