"""Build a 1:10 test-print model of the ND-100/CX Compact from the owner's tape
measurements (2026-08-02 + the 2026-08-03 night interview). Full-scale mm, scaled 0.1.

Measured: 540 wide x 690 high; box depth 620; wedge +50 roof / +90 lip at 370 / +45
grille face; 45 deg chamfer 20 run at the base to +25; recess depth 16, opening 170 tall
starting 33 below the roof edge, borders 20 outer; left black 230, divider 12, right 212;
operator panel 455 x 53, 25 below the bay; badge 460 x 22 at 18 left / 5 top; grille 80
flat + 17 louvres pitch ~16; base frame 40 (colour only, no geometry step); 3 rear fans
120 sq, 35 proud, tops 40 below the roof, spacing 30 (centring ASSUMED).
Model base = front panel bottom (the 10 mm castor gap is not modelled).
NOTE: the lip is drawn as a simple diagonal return - nose/fillet radii are unmeasured.
"""
import numpy as np
import trimesh
from trimesh.creation import box as mkbox
from trimesh.transformations import rotation_matrix
from shapely.geometry import Polygon

W, D, H = 540.0, 620.0, 690.0
FRONT = D
Z_LIP = 370.0

def bx(x0, x1, y0, y1, z0, z1):
    b = mkbox(extents=[x1 - x0, y1 - y0, z1 - z0])
    b.apply_translation([(x0 + x1) / 2, (y0 + y1) / 2, (z0 + z1) / 2])
    return b

# ---- front panel profile in (y, z): wedge, return, grille face, chamfer ----
prof = np.array([
    [FRONT, 0.0],
    [FRONT, H],
    [FRONT + 50.0, H],          # +50 at the roof
    [FRONT + 90.0, Z_LIP],      # +90 at the lip
    [FRONT + 45.0, 330.0],      # diagonal return to the grille face
    [FRONT + 45.0, 30.0],       # grille face +45 down to the chamfer
    [FRONT + 25.0, 10.0],       # 45 deg chamfer, 20 run
    [FRONT + 25.0, 0.0],
])
panel2d = trimesh.creation.extrude_polygon(Polygon(prof), height=W)
M = np.array([[0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1]], dtype=float)
panel = panel2d.copy(); panel.apply_transform(M)

body = trimesh.boolean.union([bx(0, W, 0, D, 0, H), panel], engine="manifold")

# ---- sloped upper face between roof (+50 @690) and lip (+90 @370) ----------
def yface(z):
    return FRONT + 50.0 + 40.0 * (H - z) / (H - Z_LIP)

TILT = float(np.arctan(40.0 / (H - Z_LIP)))

def face_box(xc, zc, wx, hz, thick, embed):
    b = mkbox(extents=[wx, thick, hz])
    b.apply_transform(rotation_matrix(-TILT, [1, 0, 0]))
    b.apply_translation([xc, yface(zc) - embed, zc])
    return b

# ---- recess: opening 500 wide (20 borders), 170 tall, top 33 below the roof,
#      16 deep (C1, measured 2026-08-03) --------------------------------------
rec_z1 = H - 33.0
rec_z0 = rec_z1 - 170.0
rec_zc = (rec_z0 + rec_z1) / 2
body = trimesh.boolean.difference(
    [body, face_box(W / 2, rec_zc, 500.0, 170.0, 60.0, embed=30.0 + 16.0)],
    engine="manifold")
# divider between the drive module (left, 230) and the blank panel (right, 212)
body = trimesh.boolean.union(
    [body, face_box(286.0, rec_zc, 12.0, 160.0, 60.0, embed=30.0 + 12.0)],
    engine="manifold")

# ---- operator panel 455 x 53, 25 below the bay, angled out (drawn on-slope) -
op_z1 = rec_z0 - 25.0
op_zc = op_z1 - 53.0 / 2
body = trimesh.boolean.union(
    [body, face_box(W / 2, op_zc, 455.0, 53.0, 14.0, embed=3.0)], engine="manifold")
# display window (left) + six buttons in two groups (right)
body = trimesh.boolean.union(
    [body, face_box(141.0, op_zc, 150.0, 26.0, 20.0, embed=6.0)], engine="manifold")
for xb in (330.0, 358.0, 386.0, 436.0, 464.0, 492.0):
    body = trimesh.boolean.union(
        [body, face_box(xb, op_zc, 20.0, 24.0, 20.0, embed=6.0)], engine="manifold")

# ---- badge 460 x 22, 18 from the left, 5 below the top ---------------------
body = trimesh.boolean.union(
    [body, face_box(18.0 + 230.0, H - 5.0 - 11.0, 460.0, 22.0, 14.0, embed=3.0)],
    engine="manifold")

# ---- grille: 17 louvres pitch 16 engraved into the +45 face, 80 flat above -
g_face = FRONT + 45.0
grooves = []
for i in range(17):
    z0 = 34.0 + i * 16.0
    grooves.append(bx(20.0, W - 20.0, g_face - 4.0, g_face + 2.0, z0, z0 + 8.0))
body = trimesh.boolean.difference(
    [body, trimesh.boolean.union(grooves, engine="manifold")], engine="manifold")

# ---- 3 rear fans, 120 sq, 35 proud, tops 40 below the roof (centring ASSUMED)
fans = [bx(x0, x0 + 120.0, -35.0, 5.0, H - 40.0 - 120.0, H - 40.0)
        for x0 in (60.0, 210.0, 360.0)]
body = trimesh.boolean.union([body] + fans, engine="manifold")

# ---- export ---------------------------------------------------------------
out = body.copy()
out.apply_scale(0.1)
assert out.is_watertight, "not watertight"
out.export(r"C:\Users\ronny\.claude\jobs\63f404bf\tmp\compact-1to10.stl")
print("bounds:", out.bounds.round(2).tolist(), "watertight", out.is_watertight,
      "tris", len(out.faces))
