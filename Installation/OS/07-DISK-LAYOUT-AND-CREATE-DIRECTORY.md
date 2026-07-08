# 07 — Disk Layout & What CREATE-DIRECTORY Does

> Status: SCAFFOLD. **No assumptions.** Facts below are transcribed from
> the SINTRAN-K/L archives (`FILE-INFO/BIGDISK0-K.txt`, `BIGDISK0-L.TXT`). Anything not present in
> source is left as an explicit TODO, **not** inferred.

## On-disk directory structure (verified header fields)

A SINTRAN directory dump reports these header fields (verbatim from `BIGDISK0-K.txt`):

| Field | Value (PACK-ONE, 75 MB, K) | Source |
|-------|----------------------------|--------|
| Directory name | `PACK-ONE` | BIGDISK0-K.txt |
| Filesystem image size | 38400 pages | BIGDISK0-K.txt |
| Object file index pointer | 18684 (SI: 0x1, indexed) | BIGDISK0-K.txt |
| User file index pointer | 18686 (SI: 0x1, indexed) | BIGDISK0-K.txt |
| Bit file pointer | 18468 (SI: 0x0, contiguous) | BIGDISK0-K.txt |
| No. of unreserved pages | 18438 | BIGDISK0-K.txt |

> These four pointers (object-file index, user-file index, bit file, plus the unreserved
> page count) are the structures a directory must contain. **TODO:** confirm from an ND
> manual that CREATE-DIRECTORY is what initialises exactly these — currently only observed
> in the resulting dump, not yet tied to the command in a primary source.

## CREATE-DIRECTORY command form (verified)

From `System initialisation.txt`:

```
CREATE-DIRECTORY PACK-ONE DISC-75MB-1 0
```

| Argument | Value | Meaning (TODO: confirm against command reference) |
|----------|-------|---------------------------------------------------|
| 1 | `PACK-ONE` | directory (volume) name |
| 2 | `DISC-75MB-1` | device + unit |
| 3 | `0` | subunit |

## Diagrams (TODO — to be authored, following MERMAID_COLOR_STANDARDS.md)

- [ ] Disk layout after install (directory header → object/user/bit files → reserved files).
- [ ] Flowchart: what executing CREATE-DIRECTORY does, step by step.

> Diagrams will be added **only** once each step is verified against a primary source
> (ND manual or source-code), per the no-assumptions rule. The dump above gives the *result*;
> the *mechanism* must be confirmed before being drawn.

## TODO
- Page ↔ block relationship for the device.
- Whether CREATE-DIRECTORY formats or assumes a formatted/sized image (scope note in 01).
- Cross-link command to OPCOM / System Supervisor reference.
