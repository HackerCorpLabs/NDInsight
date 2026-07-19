# Load carved SINTRAN segments into Ghidra, one program per segment, and apply
# the matching ND symbol table as labels.
#
# Ghidra Jython script (Script Manager or headless analyzeHeadless).
# It reads manifest.json produced by carve.py and, for each carved .bin:
#   * imports it as a raw big-endian ND-100 image at its load address
#   * disassembles from the load address
#   * applies labels from the segment's symbol table (SYMBOL-1-LIST, FILSYS-SYMBOLS, ...)
#
# CONFIG: set LANG_ID to your ND-100 Ghidra processor spec (big-endian, 16-bit).
# If your ND-100 Ghidra module uses a different id, change LANG_ID below.
# The symbol .SYMB.TXT files ship in NDInsight under
#   SINTRAN/NPL-SOURCE/SYMBOLS/L07/  (lines like  NAME=012345  in octal).
#
# @category SINTRAN
import json, os
from ghidra.program.model.symbol import SourceType
from ghidra.program.model.address import AddressFactory

LANG_ID   = "ndata100:BE:16:default"   # <-- adjust to your ND-100 Ghidra language id
COMPILER  = "default"

def ask(prompt, default):
    try:
        return askString("SINTRAN loader", prompt, default)
    except:
        return default

def load_symbols(program, symb_path, base_word):
    """Apply NAME=octaladdr labels from a .SYMB.TXT file. base_word=segment load addr (words)."""
    if not os.path.isfile(symb_path):
        print("  (no symbol file: %s)" % symb_path); return 0
    fa = program.getAddressFactory().getDefaultAddressSpace()
    st = program.getSymbolTable()
    n = 0
    for line in open(symb_path):
        line = line.strip()
        if "=" not in line: continue
        name, _, val = line.partition("=")
        name = name.strip(); val = val.strip()
        if not name or not val: continue
        try:
            addr_word = int(val, 8)             # symbol values are OCTAL word addresses
        except ValueError:
            continue
        # ND-100 is word-addressed; Ghidra byte address = word*2 for a 16-bit space
        try:
            a = fa.getAddress(addr_word)        # if language is word-addressed
        except:
            continue
        try:
            st.createLabel(a, name, SourceType.IMPORTED); n += 1
        except:
            pass
    print("  applied %d labels from %s" % (n, os.path.basename(symb_path)))
    return n

def run():
    manifest = ask("Path to manifest.json", "")
    symbols_dir = ask("Path to SYMBOLS/L07 dir", "")
    data = json.load(open(manifest))
    segdir = os.path.dirname(manifest)
    for seg in data["segments"]:
        if "file" not in seg or not seg.get("nonzero"): continue
        if seg.get("load_address") is None: continue
        binpath = os.path.join(segdir, seg["file"])
        print("Loading %s at %sB (conf=%s)" % (seg["name"], seg.get("load_address_oct"), seg.get("confidence")))
        # NOTE: multi-program import is easiest via the GUI (File>Import, raw binary,
        # language=%s, base=load_address). This script documents the exact params and
        # applies symbols to the *currently open* program if its name matches a segment.
        cur = getCurrentProgram()
        if cur is not None and seg["name"] in cur.getName():
            load_symbols(cur, os.path.join(symbols_dir, seg["symbol_file"] + ".SYMB.TXT"),
                         seg["load_address"])
    print("Done. Import each .bin as raw %s at its load_address, then re-run to label." % LANG_ID)

run()
