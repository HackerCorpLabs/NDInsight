# 3D print models of the ND cabinets

**These are the FIRST-CUT models, generated from tape measurements by script on
2026-08-03. They are NOT the current models.** The hand-built FreeCAD models beside
them - `ND100-Satellite.FCStd`, `ND100-Compact.FCStd`, `ND5900.FCStd`,
`ND246-Terminal.FCStd` and the STLs exported from them - came the next day and carry
far more detail. Use those to print.

What is kept here is the two generator SCRIPTS, because they build a cabinet from raw
numbers in code: change a measurement, re-run, get a new STL at any scale. That is
useful when a measurement is corrected, and it is not something the FreeCAD files do.

Test-print models at **1:10**, generated from the owner's tape measurements recorded in
[ND-PHYSICAL-MODELS.md](../ND-PHYSICAL-MODELS.md). Watertight binary STL - drag straight
into Bambu Studio. Print flat on the base, no supports needed.

| File | Machine | Printed size (W x D x H) |
|---|---|---|
| `satellite-1to10.stl` | ND-100 Satellite | 23 x 71.7 x 50 mm |
| `compact-1to10.stl` | ND-100/CX Compact | 54 x 74.5 x 69 mm |

Regenerate with `python satellite_stl.py` / `python compact_stl.py`
(needs `pip install trimesh manifold3d shapely numpy`; output paths are set at the
bottom of each script). Change the `SCALE` / `apply_scale` value for other sizes.

## What is measured vs assumed

Everything dimensioned in the record file is used as measured. Marked assumptions:

**Satellite**
- Lip height set at 240 mm (projections +25/+68/+55 are measured, the HEIGHT of the
  lip nose is not)
- Grille louvres: 14 indicative - count and top edge still open
- Recess depth 16 taken from the Compact
- No stand - the model is the box only, per the owner
- Side air-filter cuts omitted (0.6 mm at this scale, unprintable)

**Compact**
- Lip drawn as a simple diagonal return - the nose radius and concave fillet are
  unmeasured (items A-G on the worksheet)
- Rear fan group drawn centred - not confirmed
- Operator panel drawn on the main slope; its extra outward angle (~16 mm at the end)
  is not modelled at this scale
- The 10 mm castor gap is not modelled; the model base is the front panel bottom

At 1:10 most of these are below one layer height. They matter for the final large-scale
model, not the test print.
