"""
Check the physical page allocation table and LDNT access path.
Reads directly from physical memory (no DPIT needed for startup-time variables).
"""
import sys

DUMP_PATH = r'C:\Users\ronny\Downloads\nd100_physmem_256k.bin'

with open(DUMP_PATH, 'rb') as f:
    data = f.read()

word_count = len(data) // 2

def rw(addr):
    """Read word at physical word address (big-endian)."""
    off = addr * 2
    if off + 2 > len(data):
        return None
    return (data[off] << 8) | data[off + 1]

def oct6(v):
    return '------' if v is None else '%06o' % v

print('Dump: %d words (%dKW = %d pages)' % (word_count, word_count // 1024, word_count // 1024))
print()

# =========================================================================
# 1. Physical page allocation table (written during OPPSTART with PIT #0)
#    These are at PHYSICAL addresses 170213-170245 (no DPIT needed)
# =========================================================================
print('=== 1. Physical Page Allocation Table (raw physical reads) ===')
print('    (Written during OPPSTART when PIT #0 is active, virtual = physical)')
print()

page_ptrs = [
    ('MMFPAGE',    0o170213, 'First page of memory map'),
    ('MMLPAGE',    0o170214, 'Last page of memory map'),
    ('DBFPAGE',    0o170215, 'First page for device buffers'),
    ('DBLPAGE',    0o170216, 'Last page for device buffers'),
    ('FLAMPAGE',   0o170217, 'First page for LAMU tables'),
    ('LLAMPAGE',   0o170220, 'Last page for LAMU tables'),
    ('SGTFPHPAGE', 0o170221, 'First page for segment table'),
    ('SGTLPHPAGE', 0o170222, 'Last page for segment table'),
    ('LGTFPHPAGE', 0o170223, 'First page for logical number table'),
    ('LGTLPHPAGE', 0o170224, 'Last page for logical number table'),
    ('ECOFPHPAGE', 0o170225, 'First page for ext. common'),
    ('ECOLPHPAGE', 0o170226, 'Last page for ext. common'),
    ('RPIFPHPAGE', 0o170227, 'First page for RPIT'),
    ('RPILPHPAGE', 0o170230, 'Last page for RPIT'),
    ('MPIFPHPAGE', 0o170231, 'First page for MPIT'),
    ('MPILPHPAGE', 0o170232, 'Last page for MPIT'),
    ('IPIFPHPAGE', 0o170233, 'First page for IPIT'),
    ('IPILPHPAGE', 0o170234, 'Last page for IPIT'),
    ('SYMFPHPAGE', 0o170235, 'First page for sync-modem buffer'),
    ('SYMLPHPAGE', 0o170236, 'Last page for sync-modem buffer'),
    ('CMFPHPAGE',  0o170237, 'First page for common code'),
    ('CMLPHPAGE',  0o170240, 'Last page for common code'),
    ('DPIFPHPAGE', 0o170241, 'First page for DPIT'),
    ('DPILPHPAGE', 0o170242, 'Last page for DPIT'),
    ('RSFPHPAGE',  0o170243, 'First page for restart routine'),
    ('RSLPHPAGE',  0o170244, 'Last page for restart/regblocks'),
]

print('  %-12s  %-8s  %-8s  %-6s  %s' % ('Name', 'PhysAddr', 'Value', 'Dec', 'Description'))
print('  %-12s  %-8s  %-8s  %-6s  %s' % ('-' * 12, '-' * 8, '-' * 8, '-' * 6, '-' * 30))
for name, addr, desc in page_ptrs:
    val = rw(addr)
    val_str = oct6(val)
    if val is not None and val == 0o177777:
        extra = '(-1 = not allocated)'
    elif val is not None and val > 0 and val < 16384:
        word_start = val * 1024
        in_dump = 'YES' if word_start < word_count else 'NO'
        extra = 'page %d -> word %06o (in dump: %s)' % (val, word_start, in_dump)
    elif val is not None and val == 0:
        extra = '(zero)'
    else:
        extra = ''
    print('  %-12s  %06o    %s  %5s  %s  %s' % (name, addr, val_str, val if val is not None else '-', desc, extra))

print()

# =========================================================================
# 2. Verify LGTFPHPAGE makes sense
# =========================================================================
lgtfphpage = rw(0o170223)
lgtlphpage = rw(0o170224)
print('=== 2. LGTFPHPAGE Analysis ===')
if lgtfphpage is not None and lgtfphpage != 0o177777 and lgtfphpage != 0:
    print('    LGTFPHPAGE = %s = page %d' % (oct6(lgtfphpage), lgtfphpage))
    print('    LGTLPHPAGE = %s = page %d' % (oct6(lgtlphpage), lgtlphpage))
    num_pages = lgtlphpage - lgtfphpage + 1
    print('    Table size = %d pages = %d KW' % (num_pages, num_pages))
    phys_start = lgtfphpage * 1024
    phys_end = (lgtlphpage + 1) * 1024 - 1
    print('    Physical range: %06o - %06o' % (phys_start, phys_end))
    in_dump = phys_start < word_count and phys_end < word_count
    print('    Fully in dump: %s' % ('YES' if in_dump else 'NO (need %dKW dump)' % ((phys_end + 1) // 1024)))
    print()

    # What bank is this in?
    bank = phys_start // 65536
    offset_in_bank = phys_start % 65536
    print('    Bank: %d (of 64KW each)' % bank)
    print('    Offset in bank: %d = %06o' % (offset_in_bank, offset_in_bank))
    print()

    # =========================================================================
    # 3. LOGDBANK computation (tracing AD SH 12)
    # =========================================================================
    print('=== 3. LOGDBANK Computation ===')
    print('    Source: A:=LGTFPHPAGE=:D:=0; AD SH 12; A=:LOGDBANK')
    print('    A = %06o = %d' % (lgtfphpage, lgtfphpage))
    print('    D = 0')
    ad = (lgtfphpage << 16) | 0
    print('    AD (32-bit) = 0x%08X' % ad)

    # Try both shift directions
    ad_left12 = (ad << 12) & 0xFFFFFFFF
    ad_right12 = ad >> 12
    a_left = (ad_left12 >> 16) & 0xFFFF
    d_left = ad_left12 & 0xFFFF
    a_right = (ad_right12 >> 16) & 0xFFFF
    d_right = ad_right12 & 0xFFFF

    print()
    print('    If SH 12 = LEFT shift 12:')
    print('      AD = 0x%08X' % ad_left12)
    print('      A = 0x%04X = %06o  D = 0x%04X = %06o' % (a_left, a_left, d_left, d_left))
    print('      LOGDBANK = %06o' % a_left)
    # LDATX with T=a_left
    t_bank_lower8 = a_left & 0xFF
    t_bank_upper8 = (a_left >> 8) & 0xFF
    print('      LDATX bank (T[7:0])  = %d' % t_bank_lower8)
    print('      LDATX bank (T[15:8]) = %d' % t_bank_upper8)

    print()
    print('    If SH 12 = RIGHT shift 12:')
    print('      AD = 0x%08X' % ad_right12)
    print('      A = 0x%04X = %06o  D = 0x%04X = %06o' % (a_right, a_right, d_right, d_right))
    print('      LOGDBANK = %06o' % a_right)

    # Also try: maybe SH 12 on A alone (16-bit)
    a_alone_left12 = (lgtfphpage << 12) & 0xFFFF
    a_alone_right12 = lgtfphpage >> 12
    print()
    print('    If "A SH 12" (16-bit, LEFT):')
    print('      A = 0x%04X = %06o' % (a_alone_left12, a_alone_left12))
    print('    If "A SH 12" (16-bit, RIGHT):')
    print('      A = 0x%04X = %06o' % (a_alone_right12, a_alone_right12))

    # What LOGDBANK SHOULD be for bank 1 access:
    print()
    print('    For table at bank %d, LOGDBANK should encode bank %d somehow.' % (bank, bank))

    # =========================================================================
    # 4. Probe: what's at the table's physical location?
    # =========================================================================
    if phys_start < word_count:
        print()
        print('=== 4. Data at LDNT physical location ===')
        print('    Reading first 32 words at physical %06o (page %d):' % (phys_start, lgtfphpage))
        for i in range(32):
            w = rw(phys_start + i)
            if w is not None:
                nz = ' <--' if w != 0 else ''
                print('      [%2d] phys=%06o  value=%s (dec %5d)%s' % (
                    i, phys_start + i, oct6(w), w, nz))
            else:
                print('      [%2d] phys=%06o  OUT OF RANGE' % (i, phys_start + i))

    # =========================================================================
    # 5. Also check what CNVRT[0] points to
    # =========================================================================
    # CNVRT[0] was read as 0o136000 via DPIT. If that's a direct physical
    # address (bank 0), let's check what's there.
    cnvrt0_phys = 0o136000
    print()
    print('=== 5. Data at CNVRT[0] as physical address (%06o) ===' % cnvrt0_phys)
    if cnvrt0_phys < word_count:
        print('    Reading first 16 words:')
        for i in range(16):
            w = rw(cnvrt0_phys + i)
            if w is not None:
                nz = ' <--' if w != 0 else ''
                print('      [%2d] phys=%06o  value=%s (dec %5d)%s' % (
                    i, cnvrt0_phys + i, oct6(w), w, nz))
    else:
        print('    Address %06o is beyond dump range (%d words)' % (cnvrt0_phys, word_count))

    # =========================================================================
    # 6. Try CNVRT[0] + LGTFPHPAGE*1024 (table_base + offset interpretation)
    # =========================================================================
    combo_addr = phys_start + cnvrt0_phys
    print()
    print('=== 6. Data at table_base + CNVRT[0] (%06o + %06o = %06o) ===' % (
        phys_start, cnvrt0_phys, combo_addr))
    if combo_addr < word_count:
        print('    Reading first 16 words:')
        for i in range(16):
            w = rw(combo_addr + i)
            if w is not None:
                nz = ' <--' if w != 0 else ''
                print('      [%2d] phys=%06o  value=%s (dec %5d)%s' % (
                    i, combo_addr + i, oct6(w), w, nz))
    else:
        print('    Address %06o (%d) is beyond dump range (%d)' % (combo_addr, combo_addr, word_count))

else:
    print('    LGTFPHPAGE = %s - not allocated or invalid' % oct6(lgtfphpage))

print()
print('=== Done ===')
