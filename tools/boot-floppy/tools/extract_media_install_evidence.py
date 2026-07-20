#!/usr/bin/env python3
"""Extract the SINTRAN III installation evidence from the distribution floppies.

Reproduces, read-only, every primary-source quote used by
``tools/boot-floppy/INSTALL-PROCEDURE.md``:

  1. The printable MACM command header of each ``SINTRAN*:DATA`` generation
     stream (everything up to the first NUL byte).  This header holds the
     ``)9BYTT`` parameter block, the disc-variant conditional guards
     (``"BD288`` / ``"BDFIX`` / ``"W8INC`` / ``"SCASI`` / ``"REMOV`` /
     ``"FIXED``), the DEVNO assignments and the ``)MCDEF`` patch macros.

  2. The disc-type tables and operator prompts held as literal text inside
     ``MACM-1718L:BPUN`` -- the "GIVE DISK TYPE AS ONE OF THE FOLLOWING OCTAL
     NUMBERS" table, the "MSTYP / SINTRAN DEVICE NAME" table, and the
     "PLEASE DEFINE THE DISC TYPE (MSTYP) !" / "ENTER MSTYP:" prompts.
     ND text on these media carries even parity, so bit 7 is masked off.

  3. The SINTRAN mass-storage device-name table carried inside the loaded
     system image (``DISC-...`` and ``DISC-n-SCSI-u`` names).

Input is a directory of files already extracted from the .img/.image floppies
with::

    ndtool -x -p -o <outdir> <image>

`-p` (strip ND parity) is safe here because this script reads only the
printable ASCII of the generation streams.  Do NOT copy this recipe for
binaries: `-p` clears bit 7 and destroys `:BPUN`/`:PROG` files and the 8-bit
`)9READ` payloads.  Use ``ndtool -x -o <outdir> <image>`` for those.

Nothing here writes to the floppy images.  Usage::

    python extract_media_install_evidence.py <extracted-media-dir> [--out DIR]
"""

from __future__ import annotations

import argparse
import pathlib
import re
import sys

# Text strings that identify the disc-type tables / prompts inside MACM.
MACM_PATTERNS = re.compile(
    rb"DISC-|SCSI|DISK TYPE|MSTYP|INITIALIZED FOR|REMOVABLE|FIXED|"
    rb"REDEF|HENT|START SINTRAN|LOAD SINTRAN FROM DISKETTE"
)

PRINTABLE = re.compile(rb"[\x20-\x7e]{4,}")


def strip_parity(data: bytes) -> bytes:
    """Mask off the even-parity bit 7 so ND text becomes plain ASCII."""
    return bytes(b & 0x7F for b in data)


def stream_header(path: pathlib.Path) -> str:
    """Return the printable MACM command header of a :DATA stream."""
    raw = path.read_bytes()
    end = raw.find(b"\x00")
    if end < 0:
        end = len(raw)
    return raw[:end].replace(b"\r", b"").decode("latin-1")


def tagged_strings(path: pathlib.Path, pattern: re.Pattern) -> list[tuple[int, str]]:
    """Parity-strip a binary file and return (offset, text) for matching runs."""
    data = strip_parity(path.read_bytes())
    hits = []
    for m in PRINTABLE.finditer(data):
        if pattern.search(m.group()):
            hits.append((m.start(), m.group().decode("ascii")))
    return hits


def main() -> int:
    ap = argparse.ArgumentParser(description=__doc__)
    ap.add_argument("media", help="directory holding the ndtool-extracted files")
    ap.add_argument("--out", default=None, help="write results here instead of stdout")
    args = ap.parse_args()

    media = pathlib.Path(args.media)
    out = pathlib.Path(args.out) if args.out else None
    if out:
        out.mkdir(parents=True, exist_ok=True)

    def emit(name: str, text: str) -> None:
        if out:
            (out / name).write_text(text, encoding="utf-8")
        else:
            print(f"\n########## {name}\n{text}")

    for data in sorted(media.rglob("*.DATA")):
        emit(f"{data.parent.name}_{data.name}.header.txt", stream_header(data))
        # Device-name table lives in the loaded image, not the header.
        names = tagged_strings(data, re.compile(rb"DISC-[0-9A-Z-]"))
        emit(
            f"{data.parent.name}_{data.name}.devicenames.txt",
            "\n".join(f"{off:8d}  {txt}" for off, txt in names),
        )

    for macm in sorted(media.rglob("MACM-*.BPUN")):
        hits = tagged_strings(macm, MACM_PATTERNS)
        emit(
            f"{macm.parent.name}_{macm.name}.disctypes.txt",
            "\n".join(f"{off:8d}  {txt}" for off, txt in hits),
        )

    return 0


if __name__ == "__main__":
    sys.exit(main())
