#!/usr/bin/env python3
"""
make_disc_boot_docs.py -- emit the boot-sectors/installed-*.bin blobs and their
companion .md provenance/disassembly/patch-point files.

Reads the four base64 page-0 regions out of
E:\\Dev\\Ronny\\RetroFS\\src\\RetroFS.NDFS\\Creation\\NdfsBootBlobs.cs (via
extract_disc_boot.read_blobs), writes each as
    boot-sectors/installed-<device>-<sha8>.bin
and a matching .md containing:
    * provenance (which real image on this machine is byte-identical)
    * SHA-256, meaningful length, boot record format
    * the full word-by-word octal dump of the meaningful part
    * the ND-100 disassembly of the two relocated code bodies, at their real
      RUNTIME addresses (the page-0 words are not executed where they sit --
      the prologue copies them elsewhere first; see DISC-BOOT-SECTOR-ANATOMY.md)
    * the patch-point table

The narrative text lives in NOTES below and in DISC-BOOT-SECTOR-ANATOMY.md;
everything numeric here is computed from the bytes.

Usage:  python make_disc_boot_docs.py [outdir]
"""

import hashlib
import os
import sys

HERE = os.path.dirname(os.path.abspath(__file__))
sys.path.insert(0, HERE)

from extract_disc_boot import read_blobs, words, meaningful_len, classify, BLOBS_CS  # noqa: E402
from nd100_disasm import load_table, disasm_word  # noqa: E402

OUTDIR = os.path.join(os.path.dirname(HERE), "boot-sectors")

# Per-blob facts established in DISC-BOOT-SECTOR-ANATOMY.md. All of these were
# read out of the blob itself (relocation constants at page words 0o27..0o35);
# they are repeated here so the generator can label the disassembly with the
# correct runtime addresses.
#   partA = (runtime base, first page word, word count)
#   partB = (runtime base, first page word, word count)
LAYOUT = {
    "Smd":        dict(partA=(0o176546, 0o36, 154), partB=(0o062520, 0o270, 744), B=0o176726),
    "Winchester": dict(partA=(0o176536, 0o36, 162), partB=(0o101160, 0o300, 752), B=0o176741),
    "Scsi":       dict(partA=(0o176546, 0o36, 154), partB=(0o064252, 0o270, 744), B=0o176726),
    "Floppy":     dict(partA=None, partB=None, B=None),
}

PROVENANCE = {
    "Smd": [
        r"D:\ND\HDD\BIGDISK0-K.IMG", r"D:\ND\HDD\BIGDISK0-K2.IMG",
        r"D:\ND\img-backup\BIGDISK0-K.IMG", r"D:\ND\cv\BIGDISK0-K.IMG",
    ],
    "Winchester": [
        r"D:\ND\HDD\1325.img", r"D:\ND\c3\1325.img", r"D:\ND\img-backup\1325.img",
        r"D:\ND\HDD\c3_2024_1.img", r"D:\ND\HDD\disk-dump-1k.img",
        r"D:\ND\c3\2024\c3-recovered.img",
    ],
    "Scsi": [
        r"D:\ND\HDD\scsi-1.img", r"D:\ND\HDD\disk.image", r"D:\ND\HDD\MacDisk.img",
        r"D:\ND\HDD\test.IMG", r"D:\ND\img-backup\scsi-k.img",
    ],
    "Floppy": [],
}

NOTES = {
    "Smd": """\
**[VERIFIED]** SMD/ECC controller, literal `IOX 1540`-`1547` (32 instructions).
Structural end of the code is page word 927; the rest of the blob is zero.
**[VERIFIED]** geometry block at page words 0o255..0o265: words-per-sector 512,
then 18, 90, 822, 821.
**[INFERRED]** 18 = sectors/track and 90 = sectors/cylinder, hence 5 heads --
an 18-sector, 5-surface, ~823-cylinder 80 MB SMD pack. The roles of 822 and 821
are not proven.
""",
    "Winchester": """\
**[VERIFIED]** ST-506/MFM Winchester controller, literal `IOX 0500`-`0507`
(24 instructions). Structural end of the code is page word 943 (944 words);
from page word 944 the blob is the repeating filler `155555 133333 066666`, so
the "meaningful content" figure in the table above (which strips only a single
repeated value) reads too high -- trust 944 words / 1888 bytes.
**[VERIFIED]** geometry block at page words 0o132..0o142: 512, 9, 72, 1021, 1011.
**[INFERRED]** 9 = sectors/track and 72 = sectors/cylinder, hence 8 heads,
consistent with the Micropolis 1325 named in the RetroFS source comment.
**[VERIFIED]** this page also carries a SECOND, `IOXT`-based driver body (page
words 0o1312..0o1641) whose device base is the data word at B-3; its opening
instruction sequence is identical to the SCSI blob's driver.
""",
    "Scsi": """\
**[VERIFIED]** SCSI (NCR-5386 / ND-3201), indirect `IOXT` (0150415) only --
42 occurrences, and the device number comes from T at run time, computed as
`LDT -3,B; AAT <reg>; IOXT`, i.e. from the data word at B-3 (page word 0o213).
The two literal `IOX 0012`/`IOX 0013` in the body are the real-time clock, not
the disk.
**[VERIFIED]** the geometry slots are ZERO: SCSI addressing is linear, so no
cylinder/head/sector translation happens in the bootstrap. Only the
words-per-sector slot (512, page word 0o255) and the unidentified slot +8
(30, page word 0o265) are set.
Structural end of the code is page word 927; page words 0o1640..0o1747 then
hold an 18 x 4-word table of unknown meaning.
""",
    "Floppy": """\
NOT a hard-disk boot sector. This is the FLOMON floppy stream, kept in the
RetroFS blob set for completeness. Byte-expanded (every payload byte is
preceded by a 0x00), ASCII preamble `0/2 CR LF 2 !`, then Address=0 Count=0
Checksum=0 (the FLOMON marker) and a word-count byte of 0. Floppy boot
loaders are analysed elsewhere and are deliberately out of scope here.
""",
}


def octal_dump(w, lo, hi, per_line=8):
    out = []
    for i in range(lo, hi, per_line):
        chunk = w[i:min(i + per_line, hi)]
        out.append("%06o:  %s" % (i, " ".join("%06o" % x for x in chunk)))
    return "\n".join(out)


def disasm_block(w, base, pw, cnt, ent, ireg):
    out = []
    for k in range(cnt):
        v = w[pw + k]
        a = base + k
        txt, note = disasm_word(v, a, ent, ireg)
        line = "%06o  %06o  %-28s" % (a, v, txt or "??")
        if note:
            line += "; " + note
        out.append("  [page %04o] " % (pw + k) + line.rstrip())
    return "\n".join(out)


def main(argv):
    outdir = argv[1] if len(argv) > 1 else OUTDIR
    os.makedirs(outdir, exist_ok=True)
    ent, ireg = load_table()
    blobs = read_blobs()
    for name in ("Smd", "Winchester", "Scsi", "Floppy"):
        data = blobs[name]
        w = words(data)
        sha = hashlib.sha256(data).hexdigest()
        ml = meaningful_len(data)
        stem = "installed-%s-%s" % (name.lower(), sha[:8])
        binpath = os.path.join(outdir, stem + ".bin")
        with open(binpath, "wb") as f:
            f.write(data)

        lay = LAYOUT[name]
        md = []
        md.append("# %s page-0 boot region -- `%s.bin`\n" % (name, stem))
        md.append("Generated by `tools/make_disc_boot_docs.py`. Narrative and the")
        md.append("cross-media comparison live in `../DISC-BOOT-SECTOR-ANATOMY.md`.\n")
        md.append("## Provenance\n")
        md.append("Extracted from the base64 constant `%sB64` in\n" % name)
        md.append("`%s`\n" % BLOBS_CS)
        md.append("(the RetroFS source comment names the donor drive).\n")
        if PROVENANCE[name]:
            md.append("**[VERIFIED]** bytes 0..1999 of page 0 of these real images on this")
            md.append("machine are byte-identical to this blob (SHA-256 of the first 2000")
            md.append("bytes matches):\n")
            for p in PROVENANCE[name]:
                md.append("- `%s`" % p)
            md.append("")
        else:
            md.append("No matching installed hard-disk image (this is floppy media).\n")
        md.append("## Identity\n")
        md.append("| field | value |")
        md.append("|---|---|")
        md.append("| length | %d bytes (%d words) |" % (len(data), len(data) // 2))
        md.append("| SHA-256 | `%s` |" % sha)
        md.append("| meaningful content | %d bytes (%d words) |" % (ml, ml // 2))
        md.append("| record format | %s |" % classify(data))
        md.append("")
        md.append("## Notes\n")
        md.append(NOTES[name])
        md.append("## Octal dump of the meaningful part (word address : words)\n")
        md.append("```")
        md.append(octal_dump(w, 0, ml // 2))
        md.append("```\n")
        if lay["partA"]:
            md.append("## Disassembly\n")
            md.append("The page-0 words are **not** executed where they sit. Page words")
            md.append("0o0..0o35 run in place (the firmware copied the page to address 0);")
            md.append("they then relocate two bodies elsewhere. Both bodies are shown below")
            md.append("at their RUNTIME addresses, with the page word they came from.\n")
            md.append("### Prologue -- runs in place at address 0\n")
            md.append("```")
            md.append(disasm_block(w, 0, 0, 0o36, ent, ireg))
            md.append("```\n")
            b0, p0, c0 = lay["partA"]
            md.append("### Body A -- page words %04o..%04o -> runtime %06o..%06o\n"
                      % (p0, p0 + c0 - 1, b0, b0 + c0 - 1))
            md.append("```")
            md.append(disasm_block(w, b0, p0, c0, ent, ireg))
            md.append("```\n")
            b1, p1, c1 = lay["partB"]
            md.append("### Body B (device driver) -- page words %04o..%04o -> runtime %06o..%06o\n"
                      % (p1, p1 + c1 - 1, b1, b1 + c1 - 1))
            md.append("```")
            md.append(disasm_block(w, b1, p1, c1, ent, ireg))
            md.append("```\n")
            md.append("## Patch points (page word offsets)\n")
            B = lay["B"]
            def pwof(rt):
                return p0 + (rt - b0)
            rows = [
                ("relocation: body-A entry / destination end", 0o27, w[0o27]),
                ("relocation: body-A source end (page word)", 0o30, w[0o30]),
                ("relocation: body-A destination end", 0o31, w[0o31]),
                ("relocation: -(body-A word count)", 0o32, w[0o32]),
                ("relocation: body-B word count", 0o34, w[0o34]),
                ("relocation: -(body-B word count)", 0o35, w[0o35]),
                ("controller IOX base (data word at B-3)", pwof(B - 3), w[pwof(B - 3)]),
            ]
            md.append("| meaning | page word (octal) | current value (octal) |")
            md.append("|---|---|---|")
            for label, i, v in rows:
                md.append("| %s | `%04o` | `%06o` |" % (label, i, v))
            md.append("")
        with open(os.path.join(outdir, stem + ".md"), "w", encoding="utf-8") as f:
            f.write("\n".join(md))
        print("wrote %s(.bin/.md)" % os.path.join(outdir, stem))
    return 0


if __name__ == "__main__":
    sys.exit(main(sys.argv))
