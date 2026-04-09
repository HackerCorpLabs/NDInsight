"""
decode-memdump.py - Decode SINTRAN III memory dump structures

Reads an ND-100 physical memory dump and decodes internal SINTRAN structures
based on verified definitions from SINTRAN-STRUCTURES.md.

Uses SYMBOL-2-LIST.SYMB.TXT for reverse lookup: address -> RT program name.
RT program names are NOT stored as strings in memory; instead, each RT-Description
address is a symbol in SYMBOL-2-LIST (e.g., DUMMY=012071, BAK01=023337).

NOTE: This dump is PHYSICAL memory. Global pointers at 0o4000+ (page 2) are
identity-mapped, but the values they contain are LOGICAL addresses that require
MMU page table translation. We therefore use KNOWN addresses from the symbol
tables to locate RT-Descriptions, not the pointer values from the dump.

Usage:
    python decode-memdump.py [dump_file] [symbol_dir]
    Default dump: C:\\Users\\ronny\\Downloads\\nd100_physmem_256k.bin
    Default symbols: E:\\Dev\\Ronny\\NDInsight\\SINTRAN\\NPL-SOURCE\\SYMBOLS\\L07
"""

import sys
import os
import re
from pathlib import Path

# =============================================================================
# Memory dump reader
# =============================================================================

class ND100Memory:
    """Read ND-100 physical memory dump (big-endian 16-bit words)."""

    def __init__(self, filepath):
        with open(filepath, 'rb') as f:
            self.data = f.read()
        self.word_count = len(self.data) // 2
        print(f"Loaded {len(self.data)} bytes ({self.word_count} words / {self.word_count // 1024}KW)")

    def read_word(self, word_addr):
        """Read a 16-bit word at the given word address (big-endian)."""
        byte_off = word_addr * 2
        if byte_off + 1 >= len(self.data) or byte_off < 0:
            return -1
        return (self.data[byte_off] << 8) | self.data[byte_off + 1]

    def read_dword(self, word_addr):
        """Read a 32-bit value from two consecutive words (high word first)."""
        hi = self.read_word(word_addr)
        lo = self.read_word(word_addr + 1)
        if hi < 0 or lo < 0:
            return -1
        return (hi << 16) | lo


# =============================================================================
# Symbol table loader - reverse lookup (address -> name)
# =============================================================================

class SymbolTable:
    """Load SYMBOL-2-LIST and build address -> name reverse lookup."""

    def __init__(self, symbol_dir):
        self.addr_to_name = {}   # octal_addr (int) -> name (str)
        self.name_to_addr = {}   # name (str) -> octal_addr (int)
        self.rt_entries = []     # sorted list of (addr, name) for RT programs

        sym2_path = os.path.join(symbol_dir, "SYMBOL-2-LIST.SYMB.TXT")
        if not os.path.exists(sym2_path):
            print(f"WARNING: Symbol file not found: {sym2_path}")
            return

        count = 0
        with open(sym2_path, 'r') as f:
            for line in f:
                line = line.strip()
                m = re.match(r'^(\S+)=(\d+)$', line)
                if m:
                    name = m.group(1)
                    addr = int(m.group(2), 8)  # Parse as octal
                    self.addr_to_name[addr] = name
                    self.name_to_addr[name] = addr
                    count += 1

        # Also load SYMBOL-1-LIST for additional mappings
        sym1_path = os.path.join(symbol_dir, "SYMBOL-1-LIST.SYMB.TXT")
        if os.path.exists(sym1_path):
            with open(sym1_path, 'r') as f:
                for line in f:
                    line = line.strip()
                    m = re.match(r'^(\S+)=(\d+)$', line)
                    if m:
                        name = m.group(1)
                        addr = int(m.group(2), 8)
                        if addr not in self.addr_to_name:
                            self.addr_to_name[addr] = name
                        if name not in self.name_to_addr:
                            self.name_to_addr[name] = addr
                        count += 1

        print(f"Loaded {count} symbols from {symbol_dir}")

        # Build sorted RT entry list (addresses that are 26 octal apart)
        self._find_rt_entries()

    def _find_rt_entries(self):
        """Identify RT program entries by finding sequences spaced 26 octal apart."""
        # Known RT program names from SYMBOL-2-LIST
        # Start from DUMMY (first system RT) and follow 26-octal spacing
        if 'DUMMY' not in self.name_to_addr:
            return

        dummy_addr = self.name_to_addr['DUMMY']
        rt_size = 0o26  # 22 decimal

        # Collect all entries that fall on RT-Description boundaries
        # from DUMMY onwards
        self.rt_entries = []
        addr = dummy_addr
        max_scan = 500  # Safety limit

        while max_scan > 0:
            name = self.addr_to_name.get(addr)
            if name:
                self.rt_entries.append((addr, name))
            else:
                break  # Gap in the sequence = end of known RT table
            addr += rt_size
            max_scan -= 1

        # Also add background programs (BAK01 onwards) if there's a gap
        if '9FBPR' in self.name_to_addr:
            bak_start = self.name_to_addr['9FBPR']
            if bak_start != dummy_addr:  # If BAK01 isn't contiguous
                addr = bak_start
                max_scan = 500
                while max_scan > 0:
                    name = self.addr_to_name.get(addr)
                    if name:
                        if (addr, name) not in self.rt_entries:
                            self.rt_entries.append((addr, name))
                    else:
                        break
                    addr += rt_size
                    max_scan -= 1

        self.rt_entries.sort()
        print(f"Found {len(self.rt_entries)} RT program entries")

    def lookup(self, addr):
        """Look up a name for an address. Returns name or None."""
        return self.addr_to_name.get(addr)

    def lookup_rt(self, addr):
        """Look up RT program name for an RT-Description address."""
        name = self.addr_to_name.get(addr)
        if name:
            # Skip meta-symbols like 9FBPR, 9LTBP etc.
            if name.startswith('9') or name.startswith('THISS') or name == 'ERTBS' or name == '2THSS':
                # Check if there's another name at same address
                for a, n in self.rt_entries:
                    if a == addr and n != name:
                        return n
            return name
        return None


# =============================================================================
# Formatting helpers
# =============================================================================

def oct6(val):
    """Format as 6-digit octal."""
    return format(val & 0xFFFF, '06o')

def hex4(val):
    """Format as 4-digit hex with 0x prefix."""
    return f"0x{val & 0xFFFF:04X}"

def bin16(val):
    """Format as 16-bit binary."""
    return format(val & 0xFFFF, '016b')


# =============================================================================
# RT-Description structure definition
# Verified from SINTRAN-STRUCTURES.md section 1
# =============================================================================

RT_DESC_SIZE = 0o26  # 22 decimal words

RT_FIELDS = {
    0o00: ('TLINK', 'Time queue link'),
    0o01: ('STATU', 'Status word'),
    0o02: ('INPRI', 'Initial priority'),
    0o03: ('PRITY', 'Priority / Type+Ring'),
    0o04: ('DTIM1', 'Delay time high'),
    0o05: ('DTIM2', 'Delay time low'),
    0o06: ('DTIN1', 'DT interval high'),
    0o07: ('DTIN2', 'DT interval low'),
    0o10: ('STADR', 'Start address'),
    0o11: ('SEGM1', 'Program segment'),
    0o12: ('SEGM2', 'Data segment'),
    0o13: ('WLINK', 'Exec/wait queue link'),
    0o14: ('ACT1S', 'Active segment 1'),
    0o15: ('ACT2S', 'Active segment 2'),
    0o16: ('INIPR', 'Initial priority reg'),
    0o17: ('ACTPR', 'Active priority/PCR'),
    0o20: ('BRESL', 'Reservation queue head'),
    0o21: ('RSEGM', 'Reentrant segment'),
    0o22: ('BUFWI', 'Buffer window'),
    0o23: ('TRMWI', 'Terminal window'),
    0o24: ('N5WIN', 'ND-500 window'),
    0o25: ('RTDLG', 'Register save block ptr'),
}

# Status bits in STATU word (offset 001)
STATUS_BITS = {
    0o00: '5BACK',   # Background program
    0o01: '5USED',   # RT-Description in use
    0o02: '5TSLI',   # Time-sliced
    0o03: '5ESCF',   # Escape priority
    0o04: '5BRKF',   # Break flag
    0o05: '5SPRF',   # Spool/special
    0o06: '5XMSY',   # XMSG sync
    0o10: '5SWWA',   # Swap wait
    0o11: '5RTOF',   # RT program OFF
    0o12: '5TMOU',   # Timeout
    0o13: '5ABS',    # Absolute addressing
    0o14: '5INT',    # Interrupt-level
    0o15: '5RWAI',   # Resource wait
    0o17: '5WAIT',   # I/O wait
}

# I/O Datafield fields
IODF_FIELDS = {
    0o00: ('RESLI', 'Reservation queue link'),
    0o01: ('RTRES', 'Owning RT program'),
    0o02: ('BWLIN', 'Wait queue head'),
    0o03: ('(unk)', 'Unknown/semaphore'),
    0o04: ('ISTAT', 'I/O status word'),
    0o05: ('MLINK', 'Monitor queue link'),
    0o06: ('MFUNC', 'Monitor function addr'),
}

# Global root pointers (absolute word addresses, identical K03/L07/M06)
GLOBALS = {
    'RTREF': 0o4007,
    'CURPR': 0o4010,
    'MQUEU': 0o4011,
    'BTIMQ': 0o4012,
    'BEXQU': 0o4013,
    'RTSTA': 0o4020,
    'SEGST': 0o4321,
    'RTEND': 0o4323,
}

SEG_ENTRY_SIZE = 0o10  # 8 words

SEG_FIELDS = {
    0o00: ('SEGLI', 'Segment link'),
    0o01: ('PRESE', 'Previous segment'),
    0o02: ('LOGAD', 'Logical address'),
    0o03: ('SEGLE', 'Segment length'),
    0o04: ('MADR',  'Mass storage addr'),
    0o05: ('FLAG',  'Segment flags'),
    0o06: ('SGSTA', 'Segment status'),
    0o07: ('BPAGL', 'Begin page link'),
}


# =============================================================================
# Decode helpers
# =============================================================================

def decode_status(status_word):
    """Decode STATU word into flag names."""
    flags = []
    for bit_pos, name in sorted(STATUS_BITS.items()):
        if status_word & (1 << bit_pos):
            flags.append(name)
    return flags


def decode_status_text(status_word):
    """Decode status to SINTRAN-style text (READY/PASSIVE/IO-WAIT etc)."""
    if status_word & (1 << 0o17):  # 5WAIT
        return "IO-WAIT"
    if status_word & (1 << 0o15):  # 5RWAI
        return "RES-WAIT"
    if status_word & (1 << 0o10):  # 5SWWA
        return "SW-WAIT"
    if status_word & (1 << 0o11):  # 5RTOF
        return "RTOFF"
    # If none of the above, program is either READY (in exec queue) or PASSIVE
    return "READY/PASSIVE"


# =============================================================================
# RT-Description display
# =============================================================================

def print_rt_description(mem, base_addr, syms, label=""):
    """Print a full RT-Description at the given address."""
    rt_name = syms.lookup_rt(base_addr) if syms else None
    hdr = rt_name if rt_name else f"@{oct6(base_addr)}"
    if label:
        hdr = f"{hdr} ({label})"

    print(f"\n  --- {hdr} at {oct6(base_addr)} ---")

    for offset in sorted(RT_FIELDS.keys()):
        name, desc = RT_FIELDS[offset]
        val = mem.read_word(base_addr + offset)
        extra = ""

        if name == 'STATU':
            flags = decode_status(val)
            state = decode_status_text(val)
            if flags:
                extra = f"  {state} [{', '.join(flags)}]"
            else:
                extra = f"  {state}"
        elif name == 'TLINK' and val != 0:
            tname = syms.lookup_rt(val) if syms else None
            extra = f"  -> {tname}" if tname else f"  -> {oct6(val)}"
        elif name == 'WLINK' and val != 0:
            wname = syms.lookup_rt(val) if syms else None
            extra = f"  -> {wname}" if wname else f"  -> {oct6(val)}"
        elif name in ('DTIM1', 'DTIN1'):
            lo = mem.read_word(base_addr + offset + 1)
            dword = (val << 16) | lo
            extra = f"  (32-bit: {dword})"

        print(f"    +{format(offset, '02o')} {name:6s} = {oct6(val)}  {hex4(val)}{extra}")


def print_rt_summary(mem, base_addr, syms):
    """Print a one-line summary matching LIST-RT-PROGRAMS format."""
    rt_name = syms.lookup_rt(base_addr) if syms else None
    status = mem.read_word(base_addr + 0o01)
    prity = mem.read_word(base_addr + 0o03)
    stadr = mem.read_word(base_addr + 0o10)
    segm1 = mem.read_word(base_addr + 0o11)
    segm2 = mem.read_word(base_addr + 0o12)
    actpr = mem.read_word(base_addr + 0o17)
    dtim1 = mem.read_word(base_addr + 0o04)
    dtim2 = mem.read_word(base_addr + 0o05)
    dtin1 = mem.read_word(base_addr + 0o06)
    dtin2 = mem.read_word(base_addr + 0o07)

    flags = decode_status(status)
    state = decode_status_text(status)
    flag_str = ','.join(flags) if flags else '-'

    name_str = f"{rt_name:8s}" if rt_name else f"@{oct6(base_addr)} "
    time_left = (dtim1 << 16) | dtim2
    interval = (dtin1 << 16) | dtin2

    print(f"  {name_str} {oct6(base_addr)}  PRI={prity:3d}  {state:12s}  "
          f"P={oct6(stadr)}  T.LEFT={time_left:8d}  INTV={interval:8d}  "
          f"SEG={oct6(segm1)}/{oct6(segm2)}")


# =============================================================================
# SYSEVAL table
# =============================================================================

SYSEVAL_FIELDS = [
    (0o4051, 'SYSNO  ', 'System number'),
    (0o4052, 'HWINFO0', 'Hardware info 0'),
    (0o4053, 'HWINFO1', 'Hardware info 1 (microprog ver)'),
    (0o4054, 'HWINFO2', 'Hardware info 2 (system type)'),
    (0o4055, 'SINVER0', 'SINTRAN version 0'),
    (0o4056, 'SINVER1', 'SINTRAN version 1'),
    (0o4057, 'REVLEV ', 'Revision level'),
    (0o4060, 'GENDAT0', 'Generation date 0'),
    (0o4061, 'GENDAT1', 'Generation date 1'),
    (0o4062, 'GENDAT2', 'Generation date 2'),
    (0o4063, 'GENDAT3', 'Generation date 3'),
    (0o4064, 'GENDAT4', 'Generation date 4'),
]


def dump_syseval(mem):
    """Dump the SYSEVAL system information table."""
    print("\n" + "=" * 70)
    print("SYSEVAL TABLE (System Information)")
    print("=" * 70)

    for addr, name, desc in SYSEVAL_FIELDS:
        val = mem.read_word(addr)
        print(f"  {name} @ {oct6(addr)} = {oct6(val)}  {hex4(val)}  ({val:6d})  {desc}")

    # Decode HWINFO0
    hwinfo0 = mem.read_word(0o4052)
    cpu_type = (hwinfo0 >> 8) & 0x07
    instr_set = hwinfo0 & 0xFF

    cpu_names = {
        0: 'NORD-10 (48-bit FP)', 1: 'NORD-10 (32-bit FP)',
        2: 'ND-100 (48-bit FP)',  3: 'ND-100 (32-bit FP)',
        4: 'ND-110 (48-bit FP)',  5: 'ND-110 (32-bit FP)',
        6: 'ND-120 (48-bit FP)',  7: 'ND-120 (32-bit FP)',
    }
    print(f"\n  CPU Type: {cpu_type} = {cpu_names.get(cpu_type, 'Unknown')}")
    print(f"  Instruction Set: {instr_set}")

    sinver0 = mem.read_word(0o4055)
    version_char = sinver0 & 0x7F
    os_type = (sinver0 >> 8) & 0x07
    os_names = {0: 'VS', 1: 'VSE', 2: 'VSE/500', 3: 'RTP', 4: 'VSX', 5: 'VSX/500'}
    if os_type > 5:
        os_type = (sinver0 >> 12) & 0x07
    if 0x41 <= version_char <= 0x5A:
        letter = chr(version_char)
    else:
        letter = f'? (0x{version_char:02X})'
    print(f"  OS Type: {os_type} = {os_names.get(os_type, 'Unknown')}")
    print(f"  Version Letter: {letter}")
    print(f"  -> SINTRAN III {os_names.get(os_type, '')} version {letter}")

    unaflag = mem.read_word(0o4107)
    available = 'Unavailable' if (unaflag & 0x8000) else 'Available'
    print(f"\n  UNAFLAG @ {oct6(0o4107)} = {oct6(unaflag)}  {hex4(unaflag)}  -> {available}")


# =============================================================================
# Global state dump
# =============================================================================

def dump_globals(mem, syms):
    """Dump all global root pointers with symbol name resolution."""
    print("\n" + "=" * 70)
    print("GLOBAL ROOT POINTERS (values are LOGICAL addresses)")
    print("=" * 70)

    vals = {}
    for name, addr in sorted(GLOBALS.items(), key=lambda x: x[1]):
        val = mem.read_word(addr)
        sym_name = syms.lookup(val) if syms else None
        extra = f"  ({sym_name})" if sym_name else ""
        print(f"  {name:6s} @ {oct6(addr)} = {oct6(val)}  {hex4(val)}{extra}")
        vals[name] = val

    print("\n  NOTE: These are logical addresses. To follow them in a physical")
    print("  dump, we use known addresses from the symbol table instead.")
    return vals


# =============================================================================
# Decode RT entries using known symbol table addresses
# =============================================================================

def decode_rt_from_symbols(mem, syms):
    """Decode all RT-Descriptions using known addresses from symbol table."""
    if not syms.rt_entries:
        print("\n*** No RT entries found in symbol table ***")
        return

    print("\n" + "=" * 70)
    print(f"RT PROGRAMS (from SYMBOL-2-LIST, {len(syms.rt_entries)} entries)")
    print(f"  RT-Description size = {oct(RT_DESC_SIZE)} octal = {RT_DESC_SIZE} decimal words")
    print(f"  Addresses are from symbol table (LOGICAL = PHYSICAL for kernel pages)")
    print("=" * 70)

    # Summary table matching LIST-RT-PROGRAMS format
    print(f"\n  {'NAME':8s} {'RT-DESC':8s}  {'PRI':>3s}  {'STATUS':12s}  "
          f"{'P-REG':8s}  {'T.LEFT':>8s}  {'INTV':>8s}  {'SEG1':8s} {'SEG2':8s}")
    print("  " + "-" * 90)

    in_use = 0
    for addr, name in syms.rt_entries:
        # Skip meta-symbols
        if name.startswith('9') or name in ('THISS', 'ERTBS', '2THSS', 'NZRTP', 'NXRTP', 'RTBES'):
            continue
        if name.startswith(' '):
            continue

        # Check if address is within dump
        if addr * 2 + 1 >= len(mem.data):
            continue

        status = mem.read_word(addr + 0o01)
        prity = mem.read_word(addr + 0o03)
        stadr = mem.read_word(addr + 0o10)
        segm1 = mem.read_word(addr + 0o11)
        segm2 = mem.read_word(addr + 0o12)
        dtim1 = mem.read_word(addr + 0o04)
        dtim2 = mem.read_word(addr + 0o05)
        dtin1 = mem.read_word(addr + 0o06)
        dtin2 = mem.read_word(addr + 0o07)

        time_left = (dtim1 << 16) | dtim2
        interval = (dtin1 << 16) | dtin2
        state = decode_status_text(status)

        # Check for in-use (5USED bit)
        if status & (1 << 0o01):
            in_use += 1

        print(f"  {name:8s} {oct6(addr)}  {prity:3d}  {state:12s}  "
              f"{oct6(stadr)}  {time_left:8d}  {interval:8d}  "
              f"{oct6(segm1)}  {oct6(segm2)}")

    print(f"\n  Total RT entries: {len(syms.rt_entries)}, In-use (5USED): {in_use}")


def decode_rt_details(mem, syms, names_to_show=None):
    """Show full RT-Description details for specific programs."""
    if not syms.rt_entries:
        return

    # If no specific names, show all non-PASSIVE entries
    entries = []
    for addr, name in syms.rt_entries:
        if name.startswith('9') or name in ('THISS', 'ERTBS', '2THSS', 'NZRTP', 'NXRTP', 'RTBES'):
            continue
        if addr * 2 + 1 >= len(mem.data):
            continue

        status = mem.read_word(addr + 0o01)
        if names_to_show:
            if name in names_to_show:
                entries.append((addr, name))
        else:
            # Show entries that are in-use or have non-zero status
            if status != 0:
                entries.append((addr, name))

    if not entries:
        return

    print("\n" + "=" * 70)
    print("RT-DESCRIPTION DETAILS (non-zero status entries)")
    print("=" * 70)

    for addr, name in entries:
        print_rt_description(mem, addr, syms, name)


# =============================================================================
# Queue walkers using symbol table addresses
# =============================================================================

def walk_queue_from_symbols(mem, syms, queue_name, head_global, link_offset, link_name):
    """Walk a queue using LOGICAL addresses, resolving names from symbol table."""
    print(f"\n" + "=" * 70)
    print(f"{queue_name}")
    print("=" * 70)

    head_addr = GLOBALS[head_global]
    head_val = mem.read_word(head_addr)
    head_sym = syms.lookup_rt(head_val) if syms else None
    extra = f"  ({head_sym})" if head_sym else ""
    print(f"  {head_global} @ {oct6(head_addr)} = {oct6(head_val)}{extra}")
    print(f"  (This is a LOGICAL address - may not match physical dump)")

    if head_val == 0:
        print("  (empty queue)")
        return

    # Try to walk the queue anyway - if pages happen to be identity-mapped, it works
    visited = set()
    current = head_val
    count = 0
    max_entries = 200

    is_circular = (queue_name.find("circular") >= 0)
    first = head_val

    print(f"\n  Attempting to follow chain (identity-mapped assumption):")

    while count < max_entries:
        if current == 0 and not is_circular:
            print(f"  (end of chain)")
            break
        if current in visited:
            if is_circular and current == first:
                print(f"  (circular: back to start)")
            else:
                print(f"  *** LOOP at {oct6(current)} ***")
            break
        visited.add(current)

        if current * 2 + 1 >= len(mem.data):
            print(f"  *** Address {oct6(current)} outside dump ***")
            break

        name = syms.lookup_rt(current) if syms else None
        name_str = name if name else f"@{oct6(current)}"
        status = mem.read_word(current + 0o01)
        state = decode_status_text(status)
        prity = mem.read_word(current + 0o03)
        next_val = mem.read_word(current + link_offset)
        next_name = syms.lookup_rt(next_val) if syms and next_val != 0 else None
        next_str = next_name if next_name else oct6(next_val)

        print(f"    {name_str:8s}  PRI={prity:3d}  {state:12s}  "
              f"{link_name}={oct6(next_val)} ({next_str})")
        count += 1
        current = next_val

    print(f"\n  Entries found: {count}")


# =============================================================================
# Register save block
# =============================================================================

REG_NAMES = ['P', 'X', 'T', 'A', 'D', 'L', 'S', 'B']

def dump_register_block(mem, rtdlg_addr):
    """Dump the register save block pointed to by RTDLG."""
    if rtdlg_addr == 0 or rtdlg_addr * 2 + 1 >= len(mem.data):
        return
    print(f"    Register save block @ {oct6(rtdlg_addr)}:")
    for i in range(8):
        val = mem.read_word(rtdlg_addr + i)
        print(f"      {REG_NAMES[i]:1s} = {oct6(val)}  {hex4(val)}")


# =============================================================================
# Main
# =============================================================================

def main():
    dump_file = sys.argv[1] if len(sys.argv) > 1 else r"C:\Users\ronny\Downloads\nd100_physmem_256k.bin"
    symbol_dir = sys.argv[2] if len(sys.argv) > 2 else r"E:\Dev\Ronny\NDInsight\SINTRAN\NPL-SOURCE\SYMBOLS\L07"

    if not Path(dump_file).exists():
        print(f"Error: File not found: {dump_file}")
        sys.exit(1)

    mem = ND100Memory(dump_file)
    syms = SymbolTable(symbol_dir)

    # Dump SYSEVAL first to identify the system
    dump_syseval(mem)

    # Show global pointers (values are logical, for reference)
    globals_vals = dump_globals(mem, syms)

    # Decode ALL RT entries using known symbol table addresses
    decode_rt_from_symbols(mem, syms)

    # Show full details for non-passive entries
    decode_rt_details(mem, syms)

    # Walk queues (using logical addresses - works if identity-mapped)
    walk_queue_from_symbols(mem, syms,
        "TIME QUEUE (BTIMQ -> TLINK chain, linear)",
        'BTIMQ', 0o00, 'TLINK')

    walk_queue_from_symbols(mem, syms,
        "EXECUTION QUEUE (BEXQU -> WLINK chain, circular)",
        'BEXQU', 0o13, 'WLINK')

    walk_queue_from_symbols(mem, syms,
        "MONITOR QUEUE (MQUEU -> MLINK chain, I/O datafields)",
        'MQUEU', 0o05, 'MLINK')


if __name__ == '__main__':
    main()
