"""
analyze_ldnt.py - Analyze LDNT (Logical Device Number Table) from SINTRAN III memory dump.

The LDNT maps logical device numbers to I/O datafield addresses.

Access path:
  1. LGTFPHPAGE at physical 170223 (written during OPPSTART with PIT#0 active)
     gives the first physical page of the LDNT.
  2. CNVRT[32] at DPIT logical 004327 contains bank-relative offsets for each
     device group. Physical address = bank_base + CNVRT[group].
  3. Each group entry: count word, then (datafield_addr, reserved) pairs.
  4. CFLOGDV (CC-P2-COMMON.NPL) uses LDATX to search the table at runtime.
"""
import sys, os

DUMP_PATH = r'C:\Users\ronny\Downloads\nd100_physmem_256k.bin'

# DPIT #7 page table (from emulator, used for CNVRT access)
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
    0o61: 0o00540, 0o62: 0o03715, 0o63: 0o03712,
    0o64: 0o03706, 0o65: 0o03705, 0o66: 0o03704,
    0o72: 0o00576,
}

# Device group names (from CCNVRT initialization in PH-P2-START-BASE.NPL)
GROUP_NAMES = {
    0: 'DV000', 1: '(reserved)', 2: 'DV200', 3: 'DV300',
    4: 'DV400', 5: 'DV500', 6: 'DV600', 7: 'DV700',
    8: 'D1000', 9: 'D1100', 10: 'D1200', 11: 'D1300',
    12: 'D1400', 13: 'D1500', 14: 'D1600', 15: 'D1700',
    16: 'D2000', 17: 'D2100', 18: 'D2200', 19: 'D2300',
    20: 'D2400', 21: 'D2500', 22: 'D2600', 23: 'D2700',
    24: 'D3000', 25: 'D3100', 26: '(none)', 27: 'D3300',
    28: 'D3400', 29: 'D3500', 30: 'D3600', 31: 'D3700',
}


def read_phys_word(data, phys_word_addr):
    """Read 16-bit word from physical memory (big-endian)."""
    off = phys_word_addr * 2
    if off + 2 > len(data):
        return None
    return (data[off] << 8) | data[off + 1]


def read_dpit_word(data, word_count, logical_addr):
    """Read a word via DPIT translation."""
    vpn = (logical_addr >> 10) & 0o77
    offset = logical_addr & 0o1777
    ppn = DPIT.get(vpn)
    if ppn is None:
        return None
    phys = ppn * 1024 + offset
    if phys >= word_count:
        return None
    return read_phys_word(data, phys)


def oct6(v):
    return '------' if v is None else '%06o' % v


def main():
    if not os.path.exists(DUMP_PATH):
        print('ERROR: File not found: ' + DUMP_PATH)
        sys.exit(1)

    with open(DUMP_PATH, 'rb') as f:
        data = f.read()

    file_bytes = len(data)
    word_count = file_bytes // 2

    print('=== ND-100 LDNT Analysis ===')
    print('File: ' + DUMP_PATH)
    print('Size: %d KW (%d KB)' % (word_count // 1024, file_bytes // 1024))
    print()

    # =========================================================================
    # 1. Read LGTFPHPAGE from physical memory (PIT#0 area, no DPIT needed)
    # =========================================================================
    LGTFPHPAGE_ADDR = 0o170223
    LGTLPHPAGE_ADDR = 0o170224

    lgtf = read_phys_word(data, LGTFPHPAGE_ADDR)
    lgtl = read_phys_word(data, LGTLPHPAGE_ADDR)

    print('=== 1. LDNT Physical Location ===')
    print('    LGTFPHPAGE = %s (page %d)' % (oct6(lgtf), lgtf))
    print('    LGTLPHPAGE = %s (page %d)' % (oct6(lgtl), lgtl))

    if lgtf is None or lgtf == 0o177777 or lgtf == 0:
        print('    ** LDNT not allocated - cannot continue **')
        return

    table_pages = lgtl - lgtf + 1
    table_phys_start = lgtf * 1024
    table_phys_end = (lgtl + 1) * 1024 - 1
    bank = table_phys_start // 65536
    bank_base = bank * 65536

    print('    Pages: %d (%d KW)' % (table_pages, table_pages))
    print('    Physical: %06o - %06o' % (table_phys_start, table_phys_end))
    print('    Bank: %d, offset in bank: %06o' % (bank, table_phys_start - bank_base))

    if table_phys_end >= word_count:
        print('    ** Table extends beyond dump range! **')
        return

    print()

    # =========================================================================
    # 2. Read CNVRT array via DPIT (32 words at logical 004327)
    # =========================================================================
    CNVRT_BASE = 0o4327
    CNVRT_COUNT = 32

    print('=== 2. CNVRT Array (device group offsets) ===')
    cnvrt = []
    for i in range(CNVRT_COUNT):
        val = read_dpit_word(data, word_count, CNVRT_BASE + i)
        cnvrt.append(val)

    # Show all entries with group names
    print('  %5s  %-10s  %8s  %8s' % ('Index', 'Group', 'Offset', 'PhysAddr'))
    print('  %5s  %-10s  %8s  %8s' % ('-----', '----------', '--------', '--------'))
    non_zero = []
    for i in range(CNVRT_COUNT):
        gname = GROUP_NAMES.get(i, '?')
        if cnvrt[i] is not None and cnvrt[i] != 0:
            phys = bank_base + cnvrt[i]
            in_table = table_phys_start <= phys <= table_phys_end
            mark = '' if in_table else ' ** OUT OF TABLE RANGE **'
            print('  %5d  %-10s  %06o    %06o%s' % (i, gname, cnvrt[i], phys, mark))
            non_zero.append(i)
        elif cnvrt[i] == 0:
            print('  %5d  %-10s  %06o    (zero)' % (i, gname, 0))
        else:
            print('  %5d  %-10s  ------    (unreadable)' % (i, gname))

    print()
    print('  Active groups: %d' % len(non_zero))
    print()

    # =========================================================================
    # 3. Read LDNT entries for each active group
    # =========================================================================
    print('=== 3. LDNT Device Entries ===')
    print()

    total_devices = 0

    for gi in non_zero:
        gname = GROUP_NAMES.get(gi, 'group_%d' % gi)
        group_phys = bank_base + cnvrt[gi]

        # Read count word
        count = read_phys_word(data, group_phys)
        if count is None:
            print('  --- %s (CNVRT[%d]): UNREADABLE ---' % (gname, gi))
            print()
            continue

        if count > 512:
            print('  --- %s (CNVRT[%d]): count = %d -- too large, skipping ---' % (gname, gi, count))
            print()
            continue

        print('  --- %s (CNVRT[%d]) at physical %06o: %d entries ---' % (gname, gi, group_phys, count))

        if count == 0:
            print('      (empty group)')
            print()
            continue

        # Read entries: each is 2 words
        # Word 0: I/O datafield address (DPIT logical)
        # Word 1: appears to be reserved/zero in many cases
        has_devices = False
        for j in range(count):
            w0_addr = group_phys + 1 + j * 2
            w1_addr = group_phys + 2 + j * 2
            w0 = read_phys_word(data, w0_addr)
            w1 = read_phys_word(data, w1_addr)

            if w0 is not None and w0 != 0:
                has_devices = True
                total_devices += 1
                # Logical device number = group_base + index
                # Group base depends on group numbering convention
                print('      entry[%3d]  datafield=%s  word1=%s' % (j, oct6(w0), oct6(w1)))

        if not has_devices:
            print('      (all entries zero - no configured devices in this group)')
        print()

    print('=== 4. Summary ===')
    print('    LDNT location: physical pages %d-%d (%d KW)' % (lgtf, lgtl, table_pages))
    print('    Active groups: %d of %d' % (len(non_zero), CNVRT_COUNT))
    print('    Non-zero device entries: %d' % total_devices)
    print()
    print('=== Done ===')


if __name__ == '__main__':
    main()
