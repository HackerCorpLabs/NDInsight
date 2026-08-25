"""Build a 1:10 test-print model of the ND-100 Satellite from the 2026-08-03 tape
measurements. All numbers in mm at full scale; scaled 0.1 at export.

Measured: box 230 x 500 x 647; panel projections +25 roof / +68 lip / +55 grille face;
badge 210x20 (7 down, 8 to bay); bay 88x148 at 40 left / 35 down; key plate 37x55
(28 right of bay, 38 below bay top); button plate 165x50 (~20 below bay), buttons 16,
groups 35 apart; grille side margins 10; stand 4 feet 145 tall.
ASSUMED/OPEN: lip height (set 240), grille louvres (14 indicative), stand footprint
(22 mm feet, inset positions), recess depth ~16 like the Compact.
"""
import numpy as np
import trimesh
from trimesh.creation import box as mkbox
from trimesh.transformations import rotation_matrix, translation_matrix

# ---- full-scale dimensions -------------------------------------------------
W, D, H = 230.0, 647.0, 500.0          # box width / depth / height
P_ROOF, P_LIP, P_GRILLE = 25.0, 68.0, 55.0
Z_LIP, Z_GRILLE_FACE = 240.0, 200.0    # lip nose height ASSUMED, return end
FRONT = D                              # box front face at y = 647

def bx(x0, x1, y0, y1, z0, z1):
    b = mkbox(extents=[x1 - x0, y1 - y0, z1 - z0])
    b.apply_translation([(x0 + x1) / 2, (y0 + y1) / 2, (z0 + z1) / 2])
    return b

# ---- front panel prism: profile in (y, z), extruded across x ---------------
# profile: box front -> roof +25 -> lip +68 at Z_LIP -> +55 at Z_GRILLE_FACE -> bottom
prof = np.array([
    [FRONT, 0.0],
    [FRONT, H],
    [FRONT + P_ROOF, H],
    [FRONT + P_LIP, Z_LIP],
    [FRONT + P_GRILLE, Z_GRILLE_FACE],
    [FRONT + P_GRILLE, 0.0],
])
try:
    from shapely.geometry import Polygon
    panel2d = trimesh.creation.extrude_polygon(Polygon(prof), height=W)
    # extrude_polygon: polygon in (x,y) extruded along z. Map (px,py,pz)->(x=pz,y=px,z=py)
    M = np.array([[0, 0, 1, 0], [1, 0, 0, 0], [0, 1, 0, 0], [0, 0, 0, 1]], dtype=float)
    panel = panel2d.copy()
    panel.apply_transform(M)
except Exception:
    raise SystemExit("shapely missing - install it")

body = trimesh.boolean.union([bx(0, W, 0, D, 0, H), panel], engine="manifold")

# ---- the sloped upper face: y(z) between roof and lip ----------------------
def yface(z):
    return FRONT + P_ROOF + (P_LIP - P_ROOF) * (H - z) / (H - Z_LIP)

TILT = float(np.arctan((P_LIP - P_ROOF) / (H - Z_LIP)))  # forward lean of the face

def face_box(xc, zc, wx, hz, thick, embed):
    """A box lying on the sloped face: centred (xc, zc), half-proud."""
    b = mkbox(extents=[wx, thick, hz])
    b.apply_transform(rotation_matrix(-TILT, [1, 0, 0]))
    b.apply_translation([xc, yface(zc) - embed, zc])
    return b

# ---- drive bay recess: 88 x 148, 40 from the left, 35 below the panel top --
bay_x0, bay_x1 = 40.0, 128.0
bay_z1 = H - 35.0
bay_z0 = bay_z1 - 148.0
bay_zc = (bay_z0 + bay_z1) / 2
recess = face_box((bay_x0 + bay_x1) / 2, bay_zc, 88.0, 148.0, 40.0, embed=20.0 + 16.0)
body = trimesh.boolean.difference([body, recess], engine="manifold")
# divider ridge between the two vertical drives (streamer | floppy on its side)
divider = face_box((bay_x0 + bay_x1) / 2, bay_zc, 4.0, 140.0, 40.0, embed=20.0 + 12.0)
body = trimesh.boolean.union([body, divider], engine="manifold")

# ---- badge 210 x 20, 7 below the top, raised ------------------------------
body = trimesh.boolean.union(
    [body, face_box(W / 2, H - 7.0 - 10.0, 210.0, 20.0, 12.0, embed=3.0)], engine="manifold")

# ---- key plate 37 x 55: 28 right of the bay, top 38 below the bay top ------
kp_x0 = bay_x1 + 28.0
kp_z1 = bay_z1 - 38.0
body = trimesh.boolean.union(
    [body, face_box(kp_x0 + 37.0 / 2, kp_z1 - 55.0 / 2, 37.0, 55.0, 12.0, embed=3.0)],
    engine="manifold")

# ---- button plate 165 x 50, ~20 below the bay, centred --------------------
bp_z1 = bay_z0 - 20.0
bp_zc = bp_z1 - 25.0
bp_x0 = (W - 165.0) / 2
body = trimesh.boolean.union(
    [body, face_box(W / 2, bp_zc, 165.0, 50.0, 12.0, embed=3.0)], engine="manifold")
# six 16 mm buttons: margins 5, gaps 6, groups 35 apart (measured)
for off in (5.0, 27.0, 49.0, 100.0, 122.0, 144.0):
    body = trimesh.boolean.union(
        [body, face_box(bp_x0 + off + 8.0, bp_zc, 16.0, 16.0, 20.0, embed=6.0)],
        engine="manifold")

# ---- grille: engraved louvres on the lower vertical face (count OPEN, 14) --
g_face = FRONT + P_GRILLE
grooves = []
for i in range(14):
    z0 = 15.0 + i * 13.0
    grooves.append(bx(10.0, W - 10.0, g_face - 3.0, g_face + 2.0, z0, z0 + 6.0))
body = trimesh.boolean.difference([body] , engine="manifold") if False else body
body = trimesh.boolean.difference([body, trimesh.boolean.union(grooves, engine="manifold")],
                                  engine="manifold")

# ---- exports ---------------------------------------------------------------
SCALE = 0.1
out = body.copy()
out.apply_scale(SCALE)
assert out.is_watertight, "body not watertight"
out.export(r"C:\Users\ronny\.claude\jobs\63f404bf\tmp\satellite-1to10.stl")
print("body:", out.bounds.round(2).tolist(), "watertight", out.is_watertight,
      "tris", len(out.faces))

# stand variant: 4 feet 22 sq, 145 tall - footprint ASSUMED (inset 15 side / 40 fr+bk)
legs = [
    bx(15, 37, 40, 62, -145, 1), bx(W - 37, W - 15, 40, 62, -145, 1),
    bx(15, 37, D - 82, D - 60, -145, 1), bx(W - 37, W - 15, D - 82, D - 60, -145, 1),
]
with_stand = trimesh.boolean.union([body] + legs, engine="manifold")
ws = with_stand.copy()
ws.apply_scale(SCALE)
assert ws.is_watertight, "stand variant not watertight"
ws.export(r"C:\Users\ronny\.claude\jobs\63f404bf\tmp\satellite-1to10-with-stand.stl")
print("stand:", ws.bounds.round(2).tolist(), "watertight", ws.is_watertight,
      "tris", len(ws.faces))
