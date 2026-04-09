#!/usr/bin/env python3
"""
Decode SINTRAN III structures from physical memory dump using DPIT #7 translations.

The DPIT (Data PIT) is the Alternative Page Table used by all kernel levels for
data access. L07 symbol addresses are DPIT logical addresses. This script uses
the DPIT page translations (extracted from the emulator's Page Tables window)
to translate those logical addresses to physical addresses in the dump.

RT program names and STADR entry points are loaded from the JSON reference file
(sintran-rt-programs.json) for automatic name resolution.

Usage: python decode-with-dpit.py [dump_file] [json_file]
"""

import sys
import json
from pathlib import Path

# ═══════════════════════════════════════════════════════════════════════════════
# DPIT #7 PAGE TABLE (from emulator Page Tables window)
# Format: VPN -> Physical Page Number (PPN)
# Physical address = PPN * 1024 (words), each page = 1KW
# ═══════════════════════════════════════════════════════════════════════════════

DPIT = {
    0o00: 0o00000, 0o01: 0o00001, 0o02: 0o00102, 0o03: 0o00103,
    0o04: 0o00104, 0o05: 0o00105, 0o06: 0o00106, 0o07: 0o00107,
    0o10: 0o00110, 0o11: 0o00111, 0o12: 0o00112, 0o13: 0o00113,
    0o14: 0o00114, 0o15: 0o00115, 0o16: 0o00116, 0o17: 0o00117,
    0o20: 0o00120, 0o21: 0o00121, 0o22: 0o00122, 0o23: 0o00123,
    0o24: 0o00124, 0o25: 0o00125, 0o26: 0o00126, 0o27: 0o00127,
    0o30: 0o00130, 0o31: 0o00131, 0o32: 0o00132, 0o33: 0o00133,
    0o34: 0o00134, 0o35: 0o00135, 0o36: 0o00136, 0o37: 0o00137,
    0o40: 0o00140, 0o41: 0o00141, 0o42: 0o00142, 0o43: 0o00143,
    0o44: 0o00144, 0o45: 0o00145, 0o46: 0o00146, 0o47: 0o00147,
    0o50: 0o00150, 0o51: 0o00151, 0o52: 0o00152, 0o53: 0o00153,
    0o54: 0o00154, 0o55: 0o00155, 0o56: 0o00156, 0o57: 0o00540,
    # 0o60: unmapped
    0o61: 0o00540, 0o62: 0o03715, 0o63: 0o03712,
    0o64: 0o03706, 0o65: 0o03705, 0o66: 0o03704,
    # 0o67-0o71: unmapped
    0o72: 0o00576,
    # 0o73-0o77: unmapped
}


# ═══════════════════════════════════════════════════════════════════════════════
# L07 SYMBOL ADDRESSES (all in DPIT logical space)
# ═══════════════════════════════════════════════════════════════════════════════

# Global queue heads and pointers (page 2, offsets verified from SYMBOL-1-LIST L07)
GLOBALS = {
    'RTREF':  0o004007,  # Current RT program reference
    'CURPR':  0o004010,  # Current program
    'MQUEU':  0o004011,  # Monitor queue head
    'BTIMQ':  0o004012,  # Time queue head
    'BEXQU':  0o004013,  # Execution queue head
    'RTSTA':  0o004020,  # RT table start
    'SEGST':  0o004321,  # Segment table start offset
    'RTEND':  0o004323,  # RT table end
    'SEGTB':  0o004320,  # Segment table bank
    'CORMB':  0o004322,  # Core map bank
    'CORMS':  0o004021,  # Core map start
    'SGMAX':  0o004015,  # Max segment number
}

# RT-Description field offsets (verified from SYMBOL-1-LIST, all versions identical)
RT_FIELDS = {
    'TLINK': 0o00,  # Time queue link
    'STATU': 0o01,  # Status word
    'TYPRI': 0o03,  # Type/priority/ring
    'DTIM1': 0o04,  # Timer high word
    'DTIM2': 0o05,  # Timer low word
    'STADR': 0o10,  # Start address
    'SEGM1': 0o11,  # Program segment
    'SEGM2': 0o12,  # Data segment
    'WLINK': 0o13,  # Wait/exec queue link
    'ACT1S': 0o14,  # Active segment 1
    'ACT2S': 0o15,  # Active segment 2
    'ACTPR': 0o17,  # Active priority/PCR
    'BRESL': 0o20,  # Reservation link
    'RSEGM': 0o21,  # Reentrant segment
    'BUFWI': 0o22,  # Buffer window
    'N5WIN': 0o24,  # ND-500 window
    'RTDLG': 0o25,  # Register block address
}

RT_SIZE = 0o26  # 22 decimal words

# Status bit definitions (in STATU word)
STATUS_BITS = {
    0:  '5BACK',   # Background program
    2:  '5TSLI',   # Timesliced
    3:  '5ESCF',   # Escape priority
    4:  '5BRKF',   # Break flag
    6:  '5XMSY',   # XMSG sync
    9:  '5RTOF',   # RT off/inhibited
    13: '5RWAI',   # Resource wait
    15: '5WAIT',   # I/O wait
}

# Segment table field offsets (verified from SYMBOL-1-LIST, all versions identical)
# Each segment entry = 8 words (5SEGS=010₈)
SEG_SIZE = 0o10  # 8 words per segment entry

SEG_FIELDS = {
    'SEGLI': 0o00,  # Segment link
    'PRESE': 0o01,  # Previous segment
    'LOGAD': 0o02,  # Logical address (first page)
    'SEGLE': 0o03,  # Segment length (pages)
    'MADR':  0o04,  # Mass storage address
    'FLAG':  0o05,  # Status flags
    'SGSTA': 0o06,  # Protection/ring bits
    'BPAGL': 0o07,  # Begin page link (core map)
}

# System segment names and descriptions
# Source: ND-860230-8-EN SINTRAN III Release Information N-version, Section 18.3
# Segment numbers in OCTAL. NPL symbol names in parentheses where known.
SYSTEM_SEGMENTS = {
    # Segment 1 (5BCOM) is NOT in the release manual's system segment table.
    # It is the base common code segment but has no user-visible SEGFIL name.
    # 0o001: no SEGFIL name — NPL symbol is 5BCOM
    0o002: ('S3IMAGE',  'Image of common code, start/restart'),
    0o003: ('S3CP',     'Command segment'),
    0o004: ('S3RTL',    'RT-Loader segment'),
    0o005: ('S3ERRS',   'System segment for error program'),          # 5PIT
    0o006: ('S3FS',     'File system segment'),
    0o007: ('S3DMAC',   'DMAC segment'),
    0o010: ('S3RTFIL',  'RTFIL segment'),
    0o011: ('S3ERRL',   'Error log segment'),
    0o012: ('S3SFS',    'Save of file system segment'),
    0o013: ('S3SCP',    'Save of command segment'),
    0o014: ('S3ERRP',   'Error program segment'),
    0o015: ('S3BFLY',   'Reserved, not used'),
    0o016: ('S3SRPIT',  'Save of RPIT'),
    0o017: ('S3SMPIT',  'Save of MPIT'),
    0o020: ('S3SDT5',   'ND-500/5000 standard domains seg'),
    0o021: ('S3NM5',    'ND-500/5000 name-tables segment'),
    0o022: ('S3RFAC',   'Remote file access segment'),
    0o023: ('S3DPIT',   'DPIT segment'),                              # 5DPIT
    0o024: ('S3SGST',   'Save of segment table'),                     # 5SSGT
    0o025: ('S3IRPIT',  'Image of RPIT'),
    0o026: ('S3IMPIT',  'Image of MPIT'),
    0o027: ('S3ISGT',   'Image of segment table'),                    # 5ISGT
    0o030: ('S3SM5',    'ND-500/5000 System Monitor seg'),
    0o031: ('S3SSPD',   'Save of spooling data fields'),
    # 032-034: Reserved
    0o035: ('S3MPIT',   'MPIT segment'),                              # 5MPIT
    0o036: ('S3TAD',    'TADADM segment'),
    0o037: ('S3RTD',    'RT-Loader data segment'),
    0o040: ('S3FUDRT',  'File user data seg for RT prog'),
    0o041: ('S3IMED',   'Image of edit routines'),
    0o042: ('S3ED',     'Edit routines'),
    0o043: ('S3PATCH',  'Used for patching purposes'),
    0o044: ('S3IDPIT',  'Image of DPIT'),
    0o045: ('S3ISYS',   'Image of system segment'),
    0o046: ('S3S5PIT',  'Save of 5PIT segment'),
    0o047: ('S3RPIT',   'RPIT segment'),                              # 5RPIT
    0o050: ('S3IS5PIT', 'Image of 5PIT segment'),
    0o051: ('S35PIT',   '5PIT segment'),                              # 55PIT
    0o052: ('S3SAVE',   'Save of common code & start/restart'),
    0o053: ('S3SDPIT',  'Save of DPIT'),
    0o054: ('S3SSYS',   'Save of system segment'),
    0o055: ('S3SERRP',  'Save of error program'),
    0o056: ('S3SRTC',   'Save of RT-Loader code segment'),
    0o057: ('S3SRTD',   'Save of RT-Loader data segment'),
    0o060: ('S3SECOM',  'Save of extended common'),
    0o061: ('S3IECOM',  'Image of extended common'),
    0o062: ('S3SSM5',   'Save of ND-500/5000 System Monitor'),
    0o063: ('S3MEMTF',  'MEMTOF segment'),
    0o064: ('S3ECOM',   'Extended common segment'),                   # 5ECOM
    0o065: ('S3SIPIT',  'Save of IPIT'),
    0o066: ('S3IIPIT',  'Image of IPIT'),
    0o067: ('S3IPIT',   'IPIT segment'),                              # 5IPIT
    0o070: ('S3SSM',    'Save service/mail segment'),
    0o071: ('S3SM',     'Service/mail segment'),
    0o072: ('S3SDMWD',  'Save of disk mirroring WD segment'),
    0o073: ('S3IDMWD',  'Image of disk mirroring WD segment'),
    0o074: ('S3SXMK',   'Save of XMSG kernel'),
    0o075: ('S3SXROU',  'Save of XMSG XROUT segment'),
    0o076: ('S3XMK',    'XMSG kernel'),
    0o077: ('S3XROU',   'XMSG XROUT segment'),
    0o100: ('S3SDNAM',  'Save of device-name table'),
    # 0o101: release manual says "SDNAM" but that's the NPL symbol, not a valid SINTRAN segment name
    0o102: ('S3SXMFI',  'Save of XMSG watchdog (XMFIDO)'),
    0o103: ('S3XMFI',   'XMSG watchdog (XMFIDO)'),
    0o104: ('S3SNKSE',  'Save of NUCLEUS server'),
    0o105: ('S3INKSE',  'Image of NUCLEUS server'),
    0o106: ('S3SNKNA',  'Save of NUCLEUS name server'),
    0o107: ('S3INKNA',  'Image of NUCLEUS name server'),
    0o110: ('S3SU110',  'Save of ND-110 Microprogram'),
    0o111: ('S3IU110',  'Image of ND-110 Microprogram'),
    0o112: ('S3SU120',  'Save of ND-120 Microprogram'),
    0o113: ('S3IU120',  'Image of ND-120 Microprogram'),
    0o114: ('S3SERWC',  'Save of ERS Watchdog program'),
    0o115: ('S3IERWC',  'Image of ERS Watchdog program'),
    0o116: ('S3SERWD',  'Save of ERS Watchdog data'),
    0o117: ('S3IERWD',  'Image of ERS Watchdog data'),
    0o120: ('S3SPPRMA', 'Save of Processor Manager server'),
    0o121: ('S3IPRMA',  'Image of Processor Manager server'),
    0o122: ('S3SPWRS',  'Save of PFTCON server'),
    0o123: ('S3IPWRS',  'Image of PFTCON server'),
    0o124: ('S3SBOPC',  'Save of BOPCOM Server'),
    0o125: ('S3IBOPC',  'Image of BOPCOM Server'),
    0o126: ('S3SMTSE',  'Save of MT server'),
    0o127: ('S3IMTSE',  'Image of MT server'),
    0o130: ('S3SHDM',   'Save of HDLC-DMAC segment'),
    0o131: ('S3IHDM',   'Image of HDLC-DMAC segment'),
    0o132: ('S3SFAC',   'Save of remote file access segment'),
    0o133: ('S3IFAC',   'Image of remote file access segment'),
    0o134: ('S3SNKDAT', 'Save of NUCLEUS data segment'),
    0o135: ('S3INKDAT', 'Image of NUCLEUS data segment'),
}

# FLAG bit definitions (offset 005₈)
FLAG_BITS = {
    0: ('5OK',   'OK'),
    1: ('5INHB', 'INHIBITED'),
    3: ('5NORE', 'PROTECT'),
    4: ('5SREE', 'SHARED'),
    5: ('5FIXC', 'FIXED'),
    6: ('5DEMA', 'DEMAND'),
}

# SGSTA bit definitions (offset 006₈)
SGSTA_BITS = {
    15: ('5WPM',  'WPM'),    # Write permit
    14: ('5RPM',  'RPM'),    # Read permit
    13: ('5FPM',  'FPM'),    # Fetch permit
    0:  ('5NCLS', 'NOCLEAR'),
}


def decode_flag(flag_word):
    """Decode segment FLAG field into text."""
    parts = []
    for bit, (sym, text) in sorted(FLAG_BITS.items(), reverse=True):
        if flag_word & (1 << bit):
            parts.append(text)
    return "+".join(parts) if parts else "-"


def decode_sgsta(sgsta_word):
    """Decode segment SGSTA field into text."""
    parts = []
    for bit in (15, 14, 13):
        if sgsta_word & (1 << bit):
            sym, text = SGSTA_BITS[bit]
            parts.append(text)
    # Ring level from bits 10-9
    ring = (sgsta_word >> 9) & 0x3
    parts.append(f"RING{ring}")
    # Kernel PIT flag (bit 3)
    if sgsta_word & (1 << 3):
        parts.append("KPIT")
    # NOCLEAR (bit 0)
    if sgsta_word & 1:
        parts.append("NOCLEAR")
    return "+".join(parts) if parts else "-"


# ═══════════════════════════════════════════════════════════════════════════════
# NAME RESOLUTION from JSON reference
# ═══════════════════════════════════════════════════════════════════════════════

class NameResolver:
    """Resolves RT slot addresses and STADR values to program names using the JSON reference."""

    def __init__(self, json_path=None):
        self.slot_to_name = {}      # slot address (int) -> program name
        self.stadr_to_name = {}     # STADR address (int) -> entry point name
        self.slot_to_group = {}     # slot address (int) -> group label
        self.slot_to_desc = {}      # slot address (int) -> description
        self.slot_to_runtime = {}   # slot address (int) -> runtime display name
        self.bg_start = None        # first background program address
        self.bg_end = None          # last background program address
        self.bg_stadr_symbol = None # background STADR symbol name (9ENTO)

        if json_path and Path(json_path).exists():
            self._load_json(json_path)

    def _parse_octal(self, s):
        """Parse an octal string like '0o012071' to int."""
        if s is None:
            return None
        s = str(s)
        if s.startswith('0o'):
            return int(s[2:], 8)
        return int(s, 8)

    def _load_json(self, json_path):
        with open(json_path, 'r', encoding='utf-8') as f:
            data = json.load(f)

        # Load L07 slot-to-name lookup table
        lookup = data.get('address_lookup_tables', {})
        for addr_str, name in lookup.get('L07_slot_to_name', {}).items():
            addr = self._parse_octal(addr_str)
            if addr is not None:
                self.slot_to_name[addr] = name

        # Load L07 STADR-to-name lookup table
        for addr_str, name in lookup.get('L07_stadr_to_name', {}).items():
            addr = self._parse_octal(addr_str)
            if addr is not None:
                self.stadr_to_name[addr] = name

        # Load group and description from rt_programs array
        group_labels = {}
        groups = data.get('grouping_methodology', {}).get('groups', {})
        for gid, ginfo in groups.items():
            group_labels[gid] = ginfo.get('label', gid)

        for prog in data.get('rt_programs', []):
            addrs = prog.get('addresses', {}).get('L07', {})
            slot_str = addrs.get('slot') if isinstance(addrs, dict) else None
            if slot_str:
                addr = self._parse_octal(slot_str)
                if addr is not None:
                    gid = prog.get('group', '')
                    self.slot_to_group[addr] = group_labels.get(gid, gid)
                    self.slot_to_desc[addr] = prog.get('description', '')
                    rt_name = prog.get('runtime_name', prog.get('symbol', ''))
                    self.slot_to_runtime[addr] = rt_name

        # Load background program range
        bg = data.get('background_programs', {})
        bg_ranges = bg.get('ranges', {}).get('L07', {})
        fbpr = bg_ranges.get('9FBPR')
        lbpr = bg_ranges.get('9LBPR')
        if fbpr:
            self.bg_start = self._parse_octal(fbpr)
        if lbpr:
            self.bg_end = self._parse_octal(lbpr)
        self.bg_stadr_symbol = bg.get('stadr_symbol', '9ENTO')

        print(f"  Loaded {len(self.slot_to_name)} slot names, {len(self.stadr_to_name)} STADR names from JSON")

    def resolve_slot(self, addr):
        """Resolve a slot address to a program name. Returns name or None."""
        name = self.slot_to_name.get(addr)
        if name:
            return name
        # Check if it falls in the background range
        if self.bg_start and self.bg_end and self.bg_start <= addr <= self.bg_end:
            slot_in_bg = (addr - self.bg_start) // RT_SIZE
            return f"BAK{slot_in_bg + 1:02d}"
        return None

    def resolve_stadr(self, stadr):
        """Resolve a STADR value to an entry point name. Returns name or None."""
        return self.stadr_to_name.get(stadr)

    def get_group(self, addr):
        """Get the group label for a slot address."""
        grp = self.slot_to_group.get(addr)
        if grp:
            return grp
        if self.bg_start and self.bg_end and self.bg_start <= addr <= self.bg_end:
            return "Background User Programs"
        return None

    def get_runtime_name(self, addr):
        """Get the runtime display name (may differ from linker symbol)."""
        return self.slot_to_runtime.get(addr)

    def format_name(self, addr, stadr=None):
        """Format a combined name string: 'SYMBOL (STADR_entry)' or just address."""
        name = self.resolve_slot(addr)
        stadr_name = self.resolve_stadr(stadr) if stadr else None
        if name and stadr_name:
            return f"{name} ({stadr_name})"
        if name:
            return name
        if stadr_name:
            return f"?({stadr_name})"
        return None


def ofmt(v):
    """Format as 6-digit octal."""
    return f"{v:06o}"


class MemoryDump:
    def __init__(self, filepath):
        self.data = Path(filepath).read_bytes()
        self.word_count = len(self.data) // 2
        print(f"Dump: {len(self.data)} bytes = {self.word_count} words ({self.word_count // 1024}KW)")

    def read_phys_word(self, phys_word_addr):
        """Read a 16-bit word at a physical word address."""
        byte_off = phys_word_addr * 2
        if byte_off + 1 >= len(self.data):
            return None
        return (self.data[byte_off] << 8) | self.data[byte_off + 1]

    def translate_dpit(self, logical_addr):
        """Translate a DPIT logical address to physical word address."""
        vpn = (logical_addr >> 10) & 0o77  # upper 6 bits
        dip = logical_addr & 0o1777         # lower 10 bits
        ppn = DPIT.get(vpn)
        if ppn is None:
            return None
        return ppn * 1024 + dip

    def read_dpit_word(self, logical_addr):
        """Read a word using DPIT translation."""
        phys = self.translate_dpit(logical_addr)
        if phys is None:
            return None
        return self.read_phys_word(phys)

    def read_dpit_words(self, logical_addr, count):
        """Read multiple consecutive words using DPIT translation."""
        words = []
        for i in range(count):
            w = self.read_dpit_word(logical_addr + i)
            words.append(w)
        return words


def decode_status(statu):
    """Decode RT-Description status word into flag names."""
    flags = []
    for bit, name in sorted(STATUS_BITS.items()):
        if statu & (1 << bit):
            flags.append(name)
    return flags


def decode_typri(typri):
    """Decode TYPRI word: bits 15-8 = priority, bits 2-0 = ring."""
    priority = (typri >> 8) & 0xFF
    ring = typri & 0x07
    return priority, ring


def print_separator(title, char='='):
    width = 100
    print()
    print(char * width)
    print(f"  {title}")
    print(char * width)


def main():
    dump_file = sys.argv[1] if len(sys.argv) > 1 else r"C:\Users\ronny\Downloads\nd100_physmem_256k.bin"

    # Find JSON reference file (check multiple locations)
    json_file = None
    if len(sys.argv) > 2:
        json_file = sys.argv[2]
    else:
        candidates = [
            Path(__file__).parent.parent / "SINTRAN" / "Release-Documentation" / "sintran-rt-programs.json",
            Path(r"E:\Dev\Ronny\NDInsight\SINTRAN\Release-Documentation\sintran-rt-programs.json"),
            Path("sintran-rt-programs.json"),
        ]
        for c in candidates:
            if c.exists():
                json_file = str(c)
                break

    # Load name resolver
    print_separator("LOADING REFERENCES")
    if json_file:
        print(f"  JSON reference: {json_file}")
        names = NameResolver(json_file)
    else:
        print("  WARNING: sintran-rt-programs.json not found — running without name resolution")
        names = NameResolver()

    mem = MemoryDump(dump_file)

    # ═══════════════════════════════════════════════════════════════════
    # DPIT ADDRESS TRANSLATION VERIFICATION
    # ═══════════════════════════════════════════════════════════════════
    print_separator("DPIT #7 ADDRESS TRANSLATION MAP")
    print(f"  {'VPN':>3}  {'PPN(oct)':>10}  {'Phys Range (oct)':>25}  {'Phys Range (dec)':>25}  {'In Dump?'}")
    print(f"  {'---':>3}  {'--------':>10}  {'---------------':>25}  {'---------------':>25}  {'--------'}")
    for vpn in range(64):
        ppn = DPIT.get(vpn)
        if ppn is not None:
            phys_start = ppn * 1024
            phys_end = phys_start + 1023
            in_dump = "YES" if phys_end < mem.word_count else "NO"
            print(f"  {vpn:03o}  {ppn:>10o}  {ofmt(phys_start)}-{ofmt(phys_end):>12}  {phys_start:>10}-{phys_end:<10}  {in_dump}")
        else:
            print(f"  {vpn:03o}  {'(unmapped)':>10}")

    # ═══════════════════════════════════════════════════════════════════
    # GLOBAL POINTERS (translated via DPIT)
    # ═══════════════════════════════════════════════════════════════════
    print_separator("GLOBAL POINTERS (DPIT-translated)")
    for name, logical_addr in sorted(GLOBALS.items(), key=lambda x: x[1]):
        phys = mem.translate_dpit(logical_addr)
        val = mem.read_dpit_word(logical_addr)
        phys_str = ofmt(phys) if phys is not None else "(unmapped)"
        val_str = ofmt(val) if val is not None else "(unreadable)"
        in_dump = "OK" if phys is not None and phys < mem.word_count else "OUTSIDE"
        # Try to resolve the value as an RT slot name
        val_name = ""
        if val is not None and name in ('CURPR', 'BEXQU', 'BTIMQ', 'RTREF'):
            resolved = names.resolve_slot(val)
            if resolved:
                val_name = f"  -> {resolved}"
        print(f"  {name:<8} logical={ofmt(logical_addr)}  phys={phys_str}  value={val_str}  [{in_dump}]{val_name}")

    # ═══════════════════════════════════════════════════════════════════
    # RT TABLE - Decode all RT-Descriptions
    # ═══════════════════════════════════════════════════════════════════
    rtsta_val = mem.read_dpit_word(GLOBALS['RTSTA'])
    rtend_val = mem.read_dpit_word(GLOBALS['RTEND'])
    curpr_val = mem.read_dpit_word(GLOBALS['CURPR'])
    bexqu_val = mem.read_dpit_word(GLOBALS['BEXQU'])
    btimq_val = mem.read_dpit_word(GLOBALS['BTIMQ'])
    mqueu_val = mem.read_dpit_word(GLOBALS['MQUEU'])

    print_separator("RT TABLE")
    if rtsta_val is not None and rtend_val is not None:
        print(f"  RTSTA = {ofmt(rtsta_val)} (first RT slot)")
        print(f"  RTEND = {ofmt(rtend_val)} (end of RT table)")
        cur_name = names.resolve_slot(curpr_val) or "?"
        exq_name = names.resolve_slot(bexqu_val) or "?"
        tim_name = names.resolve_slot(btimq_val) or "?"
        print(f"  CURPR = {ofmt(curpr_val)} ({cur_name})")
        print(f"  BEXQU = {ofmt(bexqu_val)} ({exq_name})")
        print(f"  BTIMQ = {ofmt(btimq_val)} ({tim_name})")
        print(f"  MQUEU = {ofmt(mqueu_val)} ({'(empty)' if mqueu_val == 0o177777 else ofmt(mqueu_val)})")

        rt_count = (rtend_val - rtsta_val) // RT_SIZE
        print(f"  RT table: {rt_count} slots ({ofmt(rtsta_val)} to {ofmt(rtend_val)}, size {RT_SIZE} words each)")

        print()
        hdr = f"  {'#':>3} {'Name':<8} {'Address':>8} {'STATU':>8} {'FLAGS':<28} {'PRI':>3} {'R':>1} {'STADR':>8} {'STADR Name':<14} {'SEG1':>6} {'SEG2':>6} {'WLINK':>8} {'WL->':>8}"
        print(hdr)
        print(f"  {'---':>3} {'----':<8} {'--------':>8} {'------':>8} {'-----':<28} {'---':>3} {'-':>1} {'------':>8} {'----------':<14} {'----':>6} {'----':>6} {'------':>8} {'----':>8}")

        active_count = 0
        for i in range(rt_count):
            rt_addr = rtsta_val + i * RT_SIZE
            words = mem.read_dpit_words(rt_addr, RT_SIZE)

            if all(w is None for w in words):
                continue

            # Check if slot is used (has any non-zero content)
            if all((w or 0) == 0 for w in words):
                continue

            statu = words[RT_FIELDS['STATU']] or 0
            typri = words[RT_FIELDS['TYPRI']] or 0
            stadr = words[RT_FIELDS['STADR']] or 0
            segm1 = words[RT_FIELDS['SEGM1']] or 0
            segm2 = words[RT_FIELDS['SEGM2']] or 0
            wlink = words[RT_FIELDS['WLINK']] or 0

            flags = decode_status(statu)
            pri, ring = decode_typri(typri)
            flags_str = ",".join(flags) if flags else "-"

            rt_name = names.resolve_slot(rt_addr) or ""
            stadr_name = names.resolve_stadr(stadr) or ""
            wlink_name = names.resolve_slot(wlink) or ""

            marker = ""
            if rt_addr == curpr_val:
                marker = " <-- CURRENT"
            elif rt_addr == bexqu_val:
                marker = " <-- EXEC-Q"
            elif rt_addr == btimq_val:
                marker = " <-- TIME-Q"

            print(f"  {i:3d} {rt_name:<8} {ofmt(rt_addr)} {ofmt(statu)} {flags_str:<28} {pri:3d} {ring:1d} {ofmt(stadr)} {stadr_name:<14} {segm1:06o} {segm2:06o} {ofmt(wlink)} {wlink_name:<8}{marker}")
            active_count += 1

        print(f"\n  Total: {active_count} active RT slots out of {rt_count}")
    else:
        print("  ERROR: Could not read RTSTA/RTEND")

    # ═══════════════════════════════════════════════════════════════════
    # EXECUTION QUEUE - Follow WLINK chain from BEXQU
    # ═══════════════════════════════════════════════════════════════════
    if bexqu_val and bexqu_val != 0:
        print_separator("EXECUTION QUEUE (BEXQU chain via WLINK)")
        addr = bexqu_val
        visited = set()
        pos = 0
        while addr and addr != 0 and pos < 50:
            if addr in visited:
                print(f"  [{pos}] CYCLE detected at {ofmt(addr)} -- circular queue end")
                break
            visited.add(addr)

            statu = mem.read_dpit_word(addr + RT_FIELDS['STATU']) or 0
            typri = mem.read_dpit_word(addr + RT_FIELDS['TYPRI']) or 0
            stadr = mem.read_dpit_word(addr + RT_FIELDS['STADR']) or 0
            wlink = mem.read_dpit_word(addr + RT_FIELDS['WLINK']) or 0
            segm1 = mem.read_dpit_word(addr + RT_FIELDS['SEGM1']) or 0

            flags = decode_status(statu)
            pri, ring = decode_typri(typri)
            flags_str = ",".join(flags) if flags else "-"

            rt_name = names.resolve_slot(addr) or "?"
            stadr_name = names.resolve_stadr(stadr) or ""
            group = names.get_group(addr) or ""

            marker = " <-- CURRENT" if addr == curpr_val else ""
            entry_info = f"({stadr_name})" if stadr_name else ""
            print(f"  [{pos}] {rt_name:<8} {ofmt(addr)} pri={pri:3d} ring={ring} stadr={ofmt(stadr)} {entry_info:<16} seg1={segm1:06o} flags={flags_str:<28} [{group}]{marker}")

            # Check if the WLINK loops back or exits the RT table
            if wlink == bexqu_val:
                wl_name = names.resolve_slot(wlink) or "BEXQU"
                print(f"  [{pos+1}] -> back to {wl_name} ({ofmt(bexqu_val)}) -- end of circular queue")
                break
            if rtsta_val and rtend_val and (wlink < rtsta_val or wlink >= rtend_val) and wlink != 0:
                print(f"  [{pos+1}] -> {ofmt(wlink)} (OUTSIDE RT table, chain ends)")
                break
            addr = wlink
            pos += 1

    # ═══════════════════════════════════════════════════════════════════
    # TIME QUEUE - Follow TLINK chain from BTIMQ
    # ═══════════════════════════════════════════════════════════════════
    if btimq_val and btimq_val != 0:
        print_separator("TIME QUEUE (BTIMQ chain via TLINK)")
        addr = btimq_val
        visited = set()
        pos = 0
        while addr and addr != 0 and pos < 50:
            if addr in visited:
                print(f"  [{pos}] CYCLE at {ofmt(addr)}")
                break
            visited.add(addr)

            # Bail if address is outside RT table
            if rtsta_val and rtend_val and (addr < rtsta_val or addr >= rtend_val):
                if addr != 0o177777:
                    print(f"  [{pos}] {ofmt(addr)} (OUTSIDE RT table, chain ends)")
                break

            tlink = mem.read_dpit_word(addr + RT_FIELDS['TLINK']) or 0
            statu = mem.read_dpit_word(addr + RT_FIELDS['STATU']) or 0
            dtim1 = mem.read_dpit_word(addr + RT_FIELDS['DTIM1']) or 0
            dtim2 = mem.read_dpit_word(addr + RT_FIELDS['DTIM2']) or 0
            typri = mem.read_dpit_word(addr + RT_FIELDS['TYPRI']) or 0

            time_val = (dtim1 << 16) | dtim2
            pri, ring = decode_typri(typri)
            flags = decode_status(statu)
            flags_str = ",".join(flags) if flags else "-"

            rt_name = names.resolve_slot(addr) or "?"
            group = names.get_group(addr) or ""

            print(f"  [{pos}] {rt_name:<8} {ofmt(addr)} time={time_val:>10d} ({ofmt(dtim1)}:{ofmt(dtim2)}) pri={pri:3d} flags={flags_str:<28} [{group}]")

            addr = tlink
            pos += 1
        if addr == 0:
            print(f"  (end of chain, {pos} entries)")

    # ═══════════════════════════════════════════════════════════════════
    # DETAILED RT-DESCRIPTION DUMPS (top few active ones)
    # ═══════════════════════════════════════════════════════════════════
    if rtsta_val is not None and rtend_val is not None:
        print_separator("DETAILED RT-DESCRIPTION DUMPS (first 10 active)")
        rt_count = (rtend_val - rtsta_val) // RT_SIZE
        detailed = 0
        for i in range(rt_count):
            if detailed >= 10:
                break
            rt_addr = rtsta_val + i * RT_SIZE
            words = mem.read_dpit_words(rt_addr, RT_SIZE)
            if all((w or 0) == 0 for w in words):
                continue

            statu = words[RT_FIELDS['STATU']] or 0
            typri = words[RT_FIELDS['TYPRI']] or 0
            stadr_val = words[RT_FIELDS['STADR']] or 0
            pri, ring = decode_typri(typri)
            flags = decode_status(statu)

            rt_name = names.resolve_slot(rt_addr) or f"RT#{i}"
            stadr_name = names.resolve_stadr(stadr_val) or ""
            group = names.get_group(rt_addr) or ""

            marker = ""
            if rt_addr == curpr_val:
                marker = "  *** CURRENT PROGRAM ***"

            print(f"\n  RT #{i} = {rt_name} @ {ofmt(rt_addr)}  [{group}]{marker}")
            if stadr_name:
                print(f"  Entry point: {stadr_name} ({ofmt(stadr_val)})")
            print(f"  {'Field':<8} {'Offset':>6} {'Value(oct)':>10} {'Value(dec)':>10}  Description")
            print(f"  {'-----':<8} {'------':>6} {'----------':>10} {'----------':>10}  -----------")

            for fname, foff in sorted(RT_FIELDS.items(), key=lambda x: x[1]):
                val = words[foff]
                if val is None:
                    val_str = "(none)"
                    dec_str = ""
                else:
                    val_str = ofmt(val)
                    dec_str = f"{val:>10d}"

                desc = ""
                if fname == 'STATU':
                    desc = ", ".join(flags) if flags else "(no flags)"
                elif fname == 'TYPRI':
                    desc = f"priority={pri}, ring={ring}"
                elif fname == 'STADR':
                    sn = names.resolve_stadr(val) if val else None
                    if sn:
                        desc = f"entry: {sn}"
                elif fname == 'WLINK' and val:
                    wl_name = names.resolve_slot(val)
                    if wl_name:
                        desc = f"-> {wl_name}"
                    elif val == bexqu_val:
                        desc = "-> BEXQU (exec queue head)"
                    elif rtsta_val <= val < rtend_val:
                        slot = (val - rtsta_val) // RT_SIZE
                        desc = f"-> RT #{slot}"
                elif fname == 'TLINK' and val:
                    tl_name = names.resolve_slot(val)
                    if tl_name:
                        desc = f"-> {tl_name}"
                    elif val == 0o177777:
                        desc = "-> END (177777)"
                    elif rtsta_val <= val < rtend_val:
                        slot = (val - rtsta_val) // RT_SIZE
                        desc = f"-> RT #{slot}"

                print(f"  {fname:<8} {foff:06o} {val_str:>10} {dec_str}  {desc}")

            detailed += 1

    # ═══════════════════════════════════════════════════════════════════
    # SUMMARY BY GROUP
    # ═══════════════════════════════════════════════════════════════════
    if rtsta_val is not None and rtend_val is not None:
        print_separator("RT PROGRAMS BY GROUP")
        rt_count = (rtend_val - rtsta_val) // RT_SIZE
        groups = {}
        for i in range(rt_count):
            rt_addr = rtsta_val + i * RT_SIZE
            words = mem.read_dpit_words(rt_addr, RT_SIZE)
            if all((w or 0) == 0 for w in words):
                continue

            statu = words[RT_FIELDS['STATU']] or 0
            stadr = words[RT_FIELDS['STADR']] or 0
            typri = words[RT_FIELDS['TYPRI']] or 0
            pri, ring = decode_typri(typri)
            flags = decode_status(statu)

            rt_name = names.resolve_slot(rt_addr) or f"RT#{i}"
            group = names.get_group(rt_addr) or f"Ring {ring} (unclassified)"
            flags_str = ",".join(flags) if flags else "-"

            if group not in groups:
                groups[group] = []
            groups[group].append((i, rt_name, rt_addr, pri, ring, flags_str, stadr))

        for group_name, members in groups.items():
            print(f"\n  --- {group_name} ({len(members)} programs) ---")
            for slot, name, addr, pri, ring, flags_str, stadr in members:
                stadr_name = names.resolve_stadr(stadr) or ""
                inhibited = " [OFF]" if "5RTOF" in flags_str else ""
                waiting = " [WAIT]" if "5WAIT" in flags_str else ""
                print(f"    {name:<8} #{slot:<3d} @{ofmt(addr)} ring={ring} pri={pri:3d} {stadr_name:<14} {flags_str}{inhibited}{waiting}")

    # ═══════════════════════════════════════════════════════════════════
    # SEGMENT TABLE - Decode all segment entries
    # ═══════════════════════════════════════════════════════════════════
    segtb_val = mem.read_dpit_word(GLOBALS['SEGTB'])
    segst_val = mem.read_dpit_word(GLOBALS['SEGST'])
    sgmax_val = mem.read_dpit_word(GLOBALS['SGMAX'])
    cormb_val = mem.read_dpit_word(GLOBALS['CORMB'])
    corms_val = mem.read_dpit_word(GLOBALS['CORMS'])

    print_separator("SEGMENT TABLE")
    if segtb_val is not None and segst_val is not None and sgmax_val is not None:
        # Compute physical base address of segment table
        # SEGTB = bank number, SEGST = offset within bank
        # Physical word address = (SEGTB << 16) + SEGST
        seg_table_phys = (segtb_val << 16) + segst_val

        print(f"  SEGTB   = {segtb_val} (bank number)")
        print(f"  SEGST   = {ofmt(segst_val)} (offset within bank)")
        print(f"  SGMAX   = {ofmt(sgmax_val)} ({sgmax_val} decimal)")
        print(f"  CORMB   = {cormb_val} (core map bank)")
        print(f"  CORMS   = {ofmt(corms_val) if corms_val is not None else '?'} (core map offset)")
        print(f"  Seg table physical base = ({segtb_val} << 16) + {segst_val} = {seg_table_phys} = {ofmt(seg_table_phys)}")
        print(f"  Each segment entry = {SEG_SIZE} words")
        print(f"  Max segment entries = {sgmax_val} (0 to {ofmt(sgmax_val)})")

        # Verify segment 0 is empty and segment 1 has expected data
        seg0_phys = seg_table_phys + 0 * SEG_SIZE
        seg0_all_zero = True
        for w in range(SEG_SIZE):
            val = mem.read_phys_word(seg0_phys + w)
            if val is not None and val != 0:
                seg0_all_zero = False
                break

        seg1_phys = seg_table_phys + 1 * SEG_SIZE
        seg1_logad = mem.read_phys_word(seg1_phys + SEG_FIELDS['LOGAD'])
        seg1_segle = mem.read_phys_word(seg1_phys + SEG_FIELDS['SEGLE'])
        seg1_sgsta = mem.read_phys_word(seg1_phys + SEG_FIELDS['SGSTA'])

        print(f"\n  Verification:")
        if seg0_all_zero:
            print(f"    Segment 0: all zeros (CORRECT)")
        else:
            print(f"    Segment 0: HAS DATA (ERROR - segment 0 should be empty!)")
            print(f"    This usually means the segment table base address is wrong.")
            print(f"    Check that DPIT translation was applied to read SEGTB/SEGST.")

        if seg1_logad is not None:
            seg1_ok = (seg1_logad == 0 and seg1_segle == 0o376 and seg1_sgsta == 0o161000)
            if seg1_ok:
                print(f"    Segment 1: LOGAD={ofmt(seg1_logad)} SEGLE={ofmt(seg1_segle)} SGSTA={ofmt(seg1_sgsta)} (CORRECT)")
            else:
                print(f"    Segment 1: LOGAD={ofmt(seg1_logad)} SEGLE={ofmt(seg1_segle)} SGSTA={ofmt(seg1_sgsta)}")
                print(f"    Expected:  LOGAD=000000 SEGLE=000376 SGSTA=161000")

        # Build segment-to-RT-program mapping
        seg_users = {}  # segment number -> list of RT program names
        if rtsta_val is not None and rtend_val is not None:
            rt_count = (rtend_val - rtsta_val) // RT_SIZE
            for i in range(rt_count):
                rt_addr = rtsta_val + i * RT_SIZE
                words = mem.read_dpit_words(rt_addr, RT_SIZE)
                if all((w or 0) == 0 for w in words):
                    continue
                segm1 = words[RT_FIELDS['SEGM1']] or 0
                segm2 = words[RT_FIELDS['SEGM2']] or 0
                rt_name = names.resolve_slot(rt_addr) or f"RT#{i}"
                if segm1 > 0:
                    if segm1 not in seg_users:
                        seg_users[segm1] = []
                    seg_users[segm1].append(rt_name)
                if segm2 > 0 and segm2 != segm1:
                    if segm2 not in seg_users:
                        seg_users[segm2] = []
                    seg_users[segm2].append(rt_name)

        # Print segment table header
        print()
        hdr = (f"  {'Seg#':>5s} {'SEGLI':>7s} {'PRESE':>7s} {'LOGAD':>7s} {'SEGLE':>7s} "
               f"{'MADR':>7s} {'FLAG':>7s} {'SGSTA':>7s} {'BPAGL':>7s} "
               f"{'Pages':>5s} {'FLAG Decode':<22s} {'SGSTA Decode':<28s} {'Name/Users'}")
        print(hdr)
        print(f"  {'-----':>5s} {'-------':>7s} {'-------':>7s} {'-------':>7s} {'-------':>7s} "
              f"{'-------':>7s} {'-------':>7s} {'-------':>7s} {'-------':>7s} "
              f"{'-----':>5s} {'----------':<22s} {'------------':<28s} {'----------'}")

        non_zero_count = 0
        in_core_count = 0
        scan_limit = min(sgmax_val + 1, 2000)  # Safety limit

        for seg_num in range(scan_limit):
            entry_phys = seg_table_phys + seg_num * SEG_SIZE
            if entry_phys + SEG_SIZE > mem.word_count:
                print(f"  Segment {seg_num}: OUTSIDE DUMP (physical {ofmt(entry_phys)})")
                break

            segli = mem.read_phys_word(entry_phys + SEG_FIELDS['SEGLI'])
            prese = mem.read_phys_word(entry_phys + SEG_FIELDS['PRESE'])
            logad = mem.read_phys_word(entry_phys + SEG_FIELDS['LOGAD'])
            segle = mem.read_phys_word(entry_phys + SEG_FIELDS['SEGLE'])
            madr  = mem.read_phys_word(entry_phys + SEG_FIELDS['MADR'])
            flag  = mem.read_phys_word(entry_phys + SEG_FIELDS['FLAG'])
            sgsta = mem.read_phys_word(entry_phys + SEG_FIELDS['SGSTA'])
            bpagl = mem.read_phys_word(entry_phys + SEG_FIELDS['BPAGL'])

            # Handle None reads
            if any(v is None for v in (segli, prese, logad, segle, madr, flag, sgsta, bpagl)):
                continue

            # Skip empty entries
            if segli == 0 and prese == 0 and logad == 0 and segle == 0 and madr == 0 and flag == 0 and sgsta == 0 and bpagl == 0:
                continue

            non_zero_count += 1
            in_core = bpagl != 0
            if in_core:
                in_core_count += 1

            flag_str = decode_flag(flag)
            sgsta_str = decode_sgsta(sgsta)

            # Name: kernel segment or RT program users
            name_str = SYSTEM_SEGMENTS.get(seg_num, ('', ''))[0]
            users = seg_users.get(seg_num, [])
            if users and not name_str:
                # Show up to 4 user names
                if len(users) <= 4:
                    name_str = ",".join(users)
                else:
                    name_str = ",".join(users[:3]) + f"+{len(users)-3}more"
            elif users and name_str:
                name_str = f"{name_str} ({','.join(users[:3])})"

            core_marker = "*" if in_core else " "

            print(f"  {seg_num:5o} {ofmt(segli)} {ofmt(prese)} {ofmt(logad)} {ofmt(segle)} "
                  f"{ofmt(madr)} {ofmt(flag)} {ofmt(sgsta)} {ofmt(bpagl)} "
                  f"{segle:5d}{core_marker}{flag_str:<22s} {sgsta_str:<28s} {name_str}")

        print(f"\n  Total: {non_zero_count} non-zero segments, {in_core_count} in core (marked with *)")
        print(f"  Scanned segments 0 to {scan_limit - 1} (octal 0 to {ofmt(scan_limit - 1)})")
    else:
        print("  ERROR: Could not read SEGTB/SEGST/SGMAX from DPIT-translated globals")

    # ═══════════════════════════════════════════════════════════════════
    # RAW HEX DUMP of page 2 (system globals area)
    # ═══════════════════════════════════════════════════════════════════
    print_separator("PAGE 2 GLOBALS RAW DUMP (logical 004000-004077)")
    for off in range(0, 64, 8):
        logical = 0o004000 + off
        vals = []
        for j in range(8):
            w = mem.read_dpit_word(logical + j)
            vals.append(ofmt(w) if w is not None else "------")
        print(f"  {ofmt(logical)}: {' '.join(vals)}")

    print()
    print("Done.")


if __name__ == '__main__':
    main()
