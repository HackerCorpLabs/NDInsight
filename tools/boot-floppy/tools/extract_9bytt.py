#!/usr/bin/env python3
# ---------------------------------------------------------------------------
# extract_9bytt.py
#
# Extracts the MASS-STORAGE / DISK-GEOMETRY parameters that Norsk Data
# SINTRAN generation streams (the SINTRAN*:DATA file on a distribution
# floppy) carry, by interpreting the MACM installation script.
#
# BACKGROUND
#   SINTRAN*:DATA is a MACM (MAC Macro) installation script followed by the
#   compiled system.  The text part is 7-bit ASCII with the parity bit set,
#   so every byte must be masked with 0x7F before it reads as text.
#
#   MACM's )9BYTT command sets TEN mass-storage parameters.  From manual
#   ND-60.009.02 section 3.13, in this exact order:
#       1  MSTYP  Mass storage type.  Drum=0, NCR-disc=1, CDC-disc=2,
#                 Large disc=3
#       2  DEVNO  Primary mass storage device number
#       3  CORAD  Start address of coreload in core
#       4  LONG   Length of coreload in words
#       5  CLM    Upper limit for coreload numbers (inclusive)
#       6  BLST   Mass storage address of coreload number one
#       7  DRES   Mass storage address of core resident core image
#       8  CRMAX  End of core address (37777 for 16K core)
#       9  MACAD  Mass storage address of area where MACM is saved
#      10  DASA   Mass storage address of GJEM/HENT area
#   Syntax:  )9BYTT <10 symbol names separated by spaces>
#   The symbols must be defined earlier in the stream with NAME=value lines.
#   All values are OCTAL.
#
# THE CONDITIONAL-ASSEMBLY MODEL (verified against the streams)
#   The generation stream is *one* script that covers several disk types.
#   A line starting with a double quote is a variant guard:
#
#       "BD288+BDFIX -MADEF      <- following lines active for BD288 or BDFIX
#       G=1540
#       "W8INC+REMOV+FIXED       <- following lines active for those variants
#       G=500
#       "                        <- bare quote: unconditional again
#
#   The trailing "-MADEF" flag is a MACM directive that does not change which
#   variants the guard selects, so it is ignored here.
#
#   The variant names seen across all volumes are:
#       BD288, BDFIX, W8INC, REMOV, FIXED, SCASI  (plus DEBUG, not a disk)
#
# EXPRESSION SYNTAX (verified)
#   Values are OCTAL.  Operators seen:
#       +  -   addition / subtraction              e.g.  PFS=200+621-13
#       @  left shift by N bits (N may be negative -> right shift)
#              e.g.  H=1@1  ->  2       (page number -> block number)
#                    OPSG=LLL@-HH      (block number -> page number)
#       ' ' (space) between terms means addition
#              e.g.  H=1@1 FR  with FR=100000  ->  100002
#          This is proven by SINTRAN H, where the same quantity is written
#          out literally as H=100002 for the BDFIX variant.
#
#   )KILL <names>  deletes symbols (they are then redefined).
#
# UNRESOLVED SYMBOLS
#   Some operands are *not* defined in the stream; they come from the symbol
#   table of the loaded SINTRAN image (MSTYP, 9SMRE, ...).  Those are reported
#   as unresolved rather than guessed.
#
# USAGE
#   python extract_9bytt.py <dir-with-extracted-.DATA-files> [-o out.json]
#   python extract_9bytt.py <file1.DATA> <file2.DATA> ...
# ---------------------------------------------------------------------------

import sys
import os
import re
import json
import argparse

# The ten )9BYTT parameters, in the order MACM consumes them.
PARAM_NAMES = ["MSTYP", "DEVNO", "CORAD", "LONG", "CLM",
               "BLST", "DRES", "CRMAX", "MACAD", "DASA"]

# MSTYP decoding from ND-60.009.02 section 3.13.
MSTYP_NAMES = {0: "Drum", 1: "NCR disc", 2: "CDC disc", 3: "Large disc"}

# Disk-variant guard names that actually select a mass-storage configuration.
# DEBUG is a build-option guard, not a disk type, and is excluded.
KNOWN_VARIANTS = ["BD288", "BDFIX", "W8INC", "REMOV", "FIXED", "SCASI"]

# Where each extracted volume came from.  The keys are the directory names
# used when the floppies were unpacked with:
#     ndtool -x -p -o <dir>\<key> <image>
#
# NOTE: `-p` (strip ND parity) is correct HERE because this script only parses
# the printable ASCII header of the :DATA stream.  It is WRONG as a general
# recipe -- `-p` clears bit 7 and destroys every :BPUN/:PROG binary and the
# 8-bit )9READ payloads.  For binaries use `ndtool -x -o <dir> <image>`.
VOLUME_SOURCES = {
    "VSXL1":        r"D:\ND\S\VSXL1.IMG                       (250305L07-01D)",
    "VSXL2":        r"D:\ND\S\VSXL2.IMG                       (250305L07-02D)",
    "M06-01D":      r"D:\ND\S\250306M06-XX-01D.image",
    "M06-02D":      r"D:\ND\S\250306M06-XX-02D.image",
    "VSXK1":        r"D:\ND\S\VSXK1.img                       (N-220046K03--01D)",
    "K05-01D":      r"D:\ND\S\N-250306K05--01D.img",
    "N-102-292-I":  r"D:\ND\S\N-102-292-I.img",
    "J-900-188-I":  r"D:\ND\Frode\Sintran III Version J 86-12-09\N-900-188-I.img",
    "J-900-188-II": r"D:\ND\Frode\Sintran III Version J 86-12-09\N-900-188-II.img",
    "COSMOS-1":     r"D:\ND\Frode\Standard Satellite-9 83.01.06 ver H\COSMOS Version Diskette 1.img",
    "COSMOS-2":     r"D:\ND\Frode\Standard Satellite-9 83.01.06 ver H\COSMOS Version Diskette 2.img",
    "DISK3":        r"D:\ND\S\DISK3.img                       (N-10-102-I)",
    "DISK4":        r"D:\ND\S\DISK4.img                       (N-10-102-II)",
    "H-10-203-I":   r"D:\ND\Frode\Sintran III Version H 85-04-17\SINTRAN III H, N-10-203-I, 85.04.17.img",
}


def strip_parity(raw: bytes) -> str:
    """Remove the parity bit from a SINTRAN distribution text file."""
    return bytes(b & 0x7F for b in raw).decode("latin-1")


def text_region(txt: str) -> str:
    """
    Return only the leading plain-text region of the stream.

    The MACM script sits at the front; the compiled binary system follows.
    We cut at the first line that is mostly non-printable, which reliably
    lands after the last )9BYTT / assignment line.
    """
    out = []
    for line in txt.split("\n"):
        if not line:
            out.append(line)
            continue
        printable = 0
        for ch in line:
            if ch == "\r" or ch == "\t" or (0x20 <= ord(ch) < 0x7F):
                printable += 1
        # A binary line has a meaningful fraction of control/high bytes.
        if printable * 10 < len(line) * 9:
            break
        out.append(line)
    return "\n".join(out)


# --------------------------------------------------------------------------
# Octal expression evaluator
# --------------------------------------------------------------------------

# A term is a name or an octal literal, optionally followed by @<shift>.
TOKEN_RE = re.compile(r"\s*([+\-]?)\s*([A-Z0-9][A-Z0-9]*)((?:@-?[A-Z0-9]+)*)")


class Unresolved(Exception):
    """Raised when an expression references a symbol not defined in the stream."""

    def __init__(self, name):
        super().__init__(name)
        self.name = name


def _value_of(tok, syms):
    """Resolve a single token: octal literal, or symbol lookup."""
    if re.fullmatch(r"[0-7]+", tok):
        return int(tok, 8)
    if tok in syms:
        return syms[tok]
    # A decimal-looking literal with 8/9 in it would be illegal octal; treat
    # anything else as a symbol reference.
    raise Unresolved(tok)


def eval_expr(expr, syms):
    """
    Evaluate a MACM octal expression.

    Handles: octal literals, symbol references, + and -, implicit + between
    space-separated terms, and the @ left-shift operator (negative = right).
    """
    expr = expr.split("%")[0].strip()          # strip trailing comment
    if not expr:
        raise Unresolved("<empty>")
    total = 0
    pos = 0
    while pos < len(expr):
        m = TOKEN_RE.match(expr, pos)
        if not m:
            raise Unresolved(expr[pos:])
        sign, base, shifts = m.group(1), m.group(2), m.group(3)
        val = _value_of(base, syms)
        # Apply each @shift left to right.
        for sm in re.finditer(r"@(-?)([A-Z0-9]+)", shifts):
            neg, amt_tok = sm.group(1), sm.group(2)
            amt = _value_of(amt_tok, syms)
            if neg:
                val >>= amt
            else:
                val <<= amt
        total = total - val if sign == "-" else total + val
        pos = m.end()
    return total


# --------------------------------------------------------------------------
# Stream interpreter
# --------------------------------------------------------------------------

ASSIGN_RE = re.compile(r"^([A-Z0-9][A-Z0-9]*)=(.*)$")
BYTT_RE = re.compile(r"^\)9BYTT\s+(.*)$")
KILL_RE = re.compile(r"^\)KILL\s+(.*)$")
GUARD_RE = re.compile(r'^"(.*)$')


def scan_variants(lines):
    """Collect every guard name that appears in the stream."""
    found = []
    for line in lines:
        g = GUARD_RE.match(line)
        if not g:
            continue
        body = g.group(1).replace("-MADEF", "").strip()
        if not body:
            continue
        for name in body.split("+"):
            name = name.strip()
            if name and name not in found:
                found.append(name)
    return found


def run_stream(lines, variant):
    """
    Interpret the stream for one disk variant.

    Returns (invocations, symbol_table).  Each invocation is a dict with the
    macro context (if any), the ten symbol names, and the resolved values.
    """
    syms = {}
    invocations = []
    active = True          # currently inside an enabled guarded region
    macro = None           # name of the )MCDEF macro we are inside, if any

    for raw in lines:
        line = raw.replace("\r", "").strip()
        if not line:
            continue

        g = GUARD_RE.match(line)
        if g:
            body = g.group(1).replace("-MADEF", "").strip()
            if not body:
                active = True                       # bare " -> unconditional
            else:
                names = [n.strip() for n in body.split("+")]
                active = variant in names
            continue

        if line.startswith("%"):                    # comment
            continue

        if line.startswith(")MCDEF"):
            parts = line.split()
            macro = parts[1] if len(parts) > 1 else "?"
            continue
        if line == "]":
            macro = None
            continue

        if not active:
            continue

        k = KILL_RE.match(line)
        if k:
            for name in k.group(1).split():
                syms.pop(name, None)
            continue

        b = BYTT_RE.match(line)
        if b:
            names = b.group(1).split()
            if len(names) != 10:
                continue
            values = []
            unresolved = []
            for n in names:
                try:
                    values.append(eval_expr(n, syms))
                except Unresolved as u:
                    values.append(None)
                    unresolved.append(u.name)
            invocations.append({
                "macro": macro,
                "symbols": names,
                "values": values,
                "unresolved": unresolved,
            })
            continue

        a = ASSIGN_RE.match(line)
        if a:
            name, expr = a.group(1), a.group(2)
            try:
                syms[name] = eval_expr(expr, syms)
            except Unresolved:
                syms.pop(name, None)                # keep it undefined
            continue

        # Anything else (other MACM directives, data lines) is ignored.

    return invocations, syms


def octal(v):
    return None if v is None else format(v, "o")


def analyse_file(path):
    raw = open(path, "rb").read()
    txt = strip_parity(raw)
    head = text_region(txt)
    lines = head.split("\n")

    all_guards = scan_variants(lines)
    variants = [v for v in KNOWN_VARIANTS if v in all_guards]

    # Try to pick up the human-readable version banner.
    banner = None
    for line in lines:
        if "VERSION" in line and line.lstrip().startswith("%%"):
            banner = line.strip("% \r").strip()
            break

    result = {
        "file": os.path.abspath(path),
        "volume": os.path.basename(os.path.dirname(os.path.abspath(path))),
        "banner": banner,
        "guards_seen": all_guards,
        "disk_variants": variants,
        "configs": {},
    }

    for v in (variants if variants else ["<none>"]):
        invs, syms = run_stream(lines, v)
        decoded = []
        for inv in invs:
            row = {"macro": inv["macro"]}
            for i, pname in enumerate(PARAM_NAMES):
                row[pname] = octal(inv["values"][i])
            row["MSTYP_name"] = (MSTYP_NAMES.get(inv["values"][0], "?")
                                 if inv["values"][0] is not None else None)
            row["unresolved"] = sorted(set(inv["unresolved"]))
            decoded.append(row)
        result["configs"][v] = {
            "invocations": decoded,
            # the interesting scalars, straight from the symbol table
            "symbols": {k: octal(val) for k, val in sorted(syms.items())},
        }
    return result


# --------------------------------------------------------------------------
# Reporting
# --------------------------------------------------------------------------

def build_catalogue(results):
    """
    Collect every distinct (MSTYP, DEVNO) pair seen, plus the per-variant
    geometry, across all analysed streams.
    """
    pairs = {}
    for r in results:
        for variant, cfg in r.get("configs", {}).items():
            for inv in cfg["invocations"]:
                key = (inv["MSTYP"], inv["DEVNO"])
                ent = pairs.setdefault("%s/%s" % key, {
                    "MSTYP": inv["MSTYP"],
                    "MSTYP_name": inv["MSTYP_name"],
                    "DEVNO_octal": inv["DEVNO"],
                    "variants": [],
                    "sources": [],
                })
                if variant not in ent["variants"]:
                    ent["variants"].append(variant)
                src = r.get("volume") or os.path.basename(r["file"])
                if src not in ent["sources"]:
                    ent["sources"].append(src)
    return pairs


def write_markdown(results, catalogue, path):
    L = []
    L.append("# SINTRAN mass-storage / disk-geometry parameters (`)9BYTT`)\n")
    L.append("Generated by `tools/extract_9bytt.py` from the `SINTRAN*:DATA` "
             "MACM generation streams on the ND distribution floppies.\n")
    L.append("All values are OCTAL.\n")
    L.append("\n## Distinct (MSTYP, DEVNO) catalogue\n")
    L.append("| MSTYP | MSTYP decoded | DEVNO (octal) | disk variants | sources |")
    L.append("|---|---|---|---|---|")
    for ent in catalogue.values():
        L.append("| %s | %s | %s | %s | %s |" % (
            ent["MSTYP"] if ent["MSTYP"] is not None else "*unresolved*",
            ent["MSTYP_name"] or "*unknown*",
            ent["DEVNO_octal"] or "*unresolved*",
            ", ".join(ent["variants"]),
            ", ".join(ent["sources"])))

    L.append("\n## Per-stream detail\n")
    for r in results:
        L.append("\n### %s  (%s)" % (r.get("volume"), os.path.basename(r["file"])))
        L.append("")
        L.append("- source image: `%s`" % VOLUME_SOURCES.get(r.get("volume"), "?"))
        L.append("- extracted file: `%s`" % r["file"])
        L.append("- banner: %s" % (r.get("banner") or "*none*"))
        L.append("- guards seen: %s" % ", ".join(r.get("guards_seen") or []) or "*none*")
        L.append("")
        L.append("| variant | DEVNO | DASA | MACAD | DRES/BLST | fixed-bit | CRMAX values |")
        L.append("|---|---|---|---|---|---|---|")
        for v, cfg in r.get("configs", {}).items():
            s = cfg["symbols"]
            crmax = []
            for inv in cfg["invocations"]:
                if inv["CRMAX"] and inv["CRMAX"] not in crmax:
                    crmax.append(inv["CRMAX"])
            L.append("| %s | %s | %s | %s | %s | %s | %s |" % (
                v, s.get("G", "-"), s.get("H", "-"), s.get("L", "-"),
                s.get("M", "-"), s.get("FR", "-"), ", ".join(crmax) or "-"))
    L.append("""
## Notes and interpretation

### MSTYP is NOT set by the stream (VERIFIED)

`MSTYP` is passed to `)9BYTT` as the symbol `F`, and every stream contains
exactly `F=MSTYP`. **No stream anywhere assigns a value to `MSTYP`.** The
symbol is resolved by MACM from the symbol table of the loaded SINTRAN
image. Therefore the numeric mass-storage type (Drum=0 / NCR=1 / CDC=2 /
Large=3) cannot be recovered from the generation stream text, and this tool
reports it as unresolved rather than guessing.

Consequence for RetroFS: **disk-type identification in the generation stream
is carried entirely by the conditional variant guards and by DEVNO plus the
mass-storage address layout**, not by MSTYP.

### Disk-variant guard names (VERIFIED, meaning INFERRED)

| guard | DEVNO (octal) | INFERRED meaning |
|---|---|---|
| BD288 | 1540 | big disc, 288 Mb class - removable/whole-pack addressing |
| BDFIX | 1540 | same controller, FIXED half of the pack (bit 100000 set) |
| W8INC | 500 | Winchester (8-inch) |
| REMOV | 500 | removable platter of a cartridge drive |
| FIXED | 500 | fixed platter of a cartridge drive (bit 100000 set) |
| SCASI | 144300 | SCSI disc (K version onwards only) |

The DEVNO values and the guard-to-DEVNO mapping are read directly from the
streams and are VERIFIED. The English expansions of the guard names are
INFERRED from context (the `% BIT 17 IS SET IF FIXED DISK` comment, the
device numbers, and the SINTRAN version timeline) - they are not stated in
the streams.

### The fixed-disk bit (VERIFIED)

Every stream from K onwards contains, literally:

    % BIT 17 IS SET IF FIXED DISK
    "BD288+W8INC+REMOV+SCASI
    FR=0
    "BDFIX+FIXED
    FR=100000

and every mass-storage address is then formed as `<page>@<shift> FR`, i.e.
`(page << shift) + FR`. In H and J the same effect appears written out
literally (`H=2` vs `H=100002`, `L=200` vs `L=100200`).

Note the comment says "bit 17" but the constant is octal 100000, which is
bit 15 of a 16-bit word (ND counts bits 0..15). The comment's numbering is
inconsistent with the value; the VALUE 100000 is what the stream actually
uses.

### Page-to-block shift (VERIFIED)

Mass-storage addresses in `)9BYTT` are BLOCK addresses, while the stream
tabulates segment positions as PAGE numbers (`PFS=200+621-13` etc.). The
conversion is a left shift, and the shift count is the per-disk-type
geometry factor:

| version | BD288 / BDFIX / W8INC / SCASI | REMOV / FIXED |
|---|---|---|
| H       | shift 1 (`H=2`)  | shift 3 (`H=10` octal = 8) |
| J       | `HH=1`           | `HH=3` |
| K       | `@1`             | `@4` (`H=20` octal = 16) |
| L, M    | `@1`             | *no block defined - see below* |

INFERRED: blocks-per-page = 2^shift, so a "page" (1K words) maps to 2 blocks
on the big-disc/Winchester/SCSI geometry and to 8 (H/J) or 16 (K) blocks on
the cartridge geometry.

### L and M dropped REMOV/FIXED (VERIFIED)

In SINTRAN L and M the address-building block is guarded by
`"BD288+BDFIX+W8INC+SCASI` and there is **no** matching `"REMOV+FIXED`
block (K still has one). For the REMOV and FIXED variants the address
symbols are therefore never defined and `)9BYTT` cannot be resolved. The
`G=500` assignment for REMOV/FIXED survives as a leftover. Practical
reading: L and M support BD288, BDFIX, W8INC and SCASI only.

### CRMAX (core size)

| CRMAX (octal) | words | meaning |
|---|---|---|
| 37777 | 16K | manual's example value; NOT seen in any of these streams |
| 77777 | 32K | used by the `PEND` macro (H, L, M) to reset the core image |
| 137777 | 48K | J only |
| 173777 | ~64K-1K | J only |
| 175777 | ~64K-512 | the standard value in ALL versions H..M |
| 177677 | ~64K-64 | K, L, M - used for the micro-code / `FA` variant |

175777 is the normal CRMAX everywhere; the others are per-macro overrides
(`)KILL A` followed by a new `A=`), not different machine sizes. INFERRED:
these are all 64K-word ND-100 core images with different amounts reserved
at the top, not a range of physical core sizes.

### Diskette-II streams

`SINTRAN-II:DATA` (H and J families) contains `)9BYTT` invocations but no
symbol definitions and no variant guards - it is loaded as a continuation of
diskette I, and MACM still holds the symbols defined there. All ten operands
are reported unresolved for those files, which is correct, not a parse
failure.
""")
    open(path, "w").write("\n".join(L) + "\n")


def main():
    ap = argparse.ArgumentParser()
    ap.add_argument("paths", nargs="+")
    ap.add_argument("-o", "--out", default=None)
    ap.add_argument("--md", default=None, help="also write a markdown report")
    args = ap.parse_args()

    files = []
    for p in args.paths:
        if os.path.isdir(p):
            for root, _, names in os.walk(p):
                for n in names:
                    if n.upper().startswith("SINTRAN"):
                        files.append(os.path.join(root, n))
        else:
            files.append(p)
    files.sort()

    out = []
    for f in files:
        try:
            out.append(analyse_file(f))
        except Exception as e:                     # keep going on bad input
            out.append({"file": os.path.abspath(f), "error": str(e)})

    catalogue = build_catalogue(out)
    doc = {
        "parameter_order": PARAM_NAMES,
        "mstyp_decoding": {str(k): v for k, v in MSTYP_NAMES.items()},
        "note": ("MSTYP is never assigned in any generation stream; the "
                 "symbol MSTYP is resolved from the loaded system's symbol "
                 "table, so its numeric value cannot be recovered from the "
                 "text. Disk-type selection in the stream is done entirely "
                 "by the conditional variant guards (BD288/BDFIX/W8INC/"
                 "REMOV/FIXED/SCASI), which set DEVNO and the mass-storage "
                 "addresses."),
        "volume_sources": VOLUME_SOURCES,
        "device_catalogue": catalogue,
        "streams": out,
    }
    js = json.dumps(doc, indent=2)
    if args.out:
        open(args.out, "w").write(js)
    else:
        sys.stdout.write(js)
    if args.md:
        write_markdown(out, catalogue, args.md)


if __name__ == "__main__":
    main()
