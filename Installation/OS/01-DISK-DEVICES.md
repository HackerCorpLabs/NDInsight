# 01 — Disk Devices & CREATE-DIRECTORY Mapping

> Status: SCAFFOLD.

## Scope / precondition

**This guide assumes the target disk is already correctly formatted.** Disk formatting
(with the proper ND tools) is **out of scope** and currently undocumented.

For the **emulator**, formatting is irrelevant — all that matters is a disk **image whose
size matches the device's total number of blocks**. That total **differs between SMD and
WD (Winchester/ST-506) disk images** and between capacity classes.

**Authoritative sizing tool:** `ndtool` (the NDFS disk-image tool) creates
correctly-sized images via `--create <template>`. Verified templates (from `ndtool -h`):

| Template | Device class | Capacity | Total pages (verified) | Cross-check |
|----------|--------------|----------|------------------------|-------------|
| `floppy360` | floppy | 360 KB | **154** | matches ndtool -h |
| `floppy12` | floppy | 1.2 MB | **616** | matches `VSXK1.txt` (616 pages); ⚠ ndtool -h annotation says "512" but the tool produces 616 |
| `smd75` | SMD | 75 MB | **38400** | matches `BIGDISK0-K/L` (38400 pages) |
| `winchester74` | WD / Winchester | 74 MB | **36360** | — |
| `custom` | any | `--pages N` | N | — |

> Page counts above are **verified** by running `ndtool --create <template>` then `ndtool -i`
> (temp images, removed after). **SMD ≠ WD:** `smd75` = 38400 pages vs `winchester74` = 36360
> pages — the total differs by controller class even at similar nominal capacity, so the image
> must be created with the template matching the real device.

To create and verify an image:
```
ndtool --create smd75 --name PACK-ONE newdisk.ndfs
ndtool -i newdisk.ndfs        # confirm page count / sizes
```

MACM's own disk-type octal table (used at load time) is in
[02-BOOTSTRAP-MACM.md §5](02-BOOTSTRAP-MACM.md#5-disk-type-selection-verified) [DOC §3.4].

Verified data point: the golden `PACK-ONE` directory on a 75 MB pack reports
**Filesystem image size = 38400 pages** (`BIGDISK0-K.txt`, `BIGDISK0-L.txt`).
> Note: "pages" (file-system pages) is **not** the same unit as device "blocks" — keep the
> distinction explicit when deriving image size. (Relationship: TODO/verify.)

## Device → directory mapping

The disk device chosen drives the `CREATE-DIRECTORY` / `ENTER-DIRECTORY` arguments
(verified, `System initialisation.txt`):

```
CREATE-DIRECTORY PACK-ONE DISC-75MB-1 0
ENTER-DIRECTORY  PACK-ONE DISC-75MB-1 0
```

- `PACK-ONE` — directory (volume) name
- `DISC-75MB-1` — device + unit
- `0` — subunit

Device-type tokens seen across versions: `DISC-75MB`, `DISC-38MB`, `D-75-1` (M HENT-MODE),
ST-506/Winchester, SCSI.

K05 "D version" (SINTRAN-K05 archive, `FLOPPY/readme.txt`, verified):
> "D version does not have support for ST-506/WINCHESTER. But it does have support for SCSI."

## TODO
- Confirm exact device tokens accepted per SINTRAN version.
- Block counts / image sizes per SMD and WD device (see table).
- Page↔block relationship.
