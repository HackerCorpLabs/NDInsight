"""
investigate-dump.py - Investigate physical dump address mapping

The RT-Description addresses from SYMBOL-2-LIST are LOGICAL addresses.
The dump file is PHYSICAL memory. We need MMU translation.

This script:
1. Reads PIT RAM (174000-177777 octal) to extract MPIT page translations
2. Builds a logical->physical translation table
3. Uses that to read RT-Descriptions at their correct physical locations
4. Also scans physical memory for RT-Description patterns as verification

Usage:
    python investigate-dump.py [dump_file]
"""

import sys
import os
from pathlib import Path

DUMP_FILE = sys.argv[1] if len(sys.argv) > 1 else r"C:\Users\ronny\Downloads\nd100_physmem_256k.bin"

with open(DUMP_FILE, 'rb') as f:
    data = f.read()

word_count = len(data) // 2
print(f"Loaded {len(data)} bytes ({word_count} words / {word_count // 1024}KW)")


def read_word(addr):
    off = addr * 2
    if off + 1 >= len(data) or off < 0:
        return -1
    return (data[off] << 8) | data[off + 1]


def oct6(v):
    return format(v & 0xFFFF, '06o')


# ============================================================================
# Part 1: Read PIT RAM entries for all PITs
# ============================================================================
print("\n" + "=" * 70)
print("PART 1: PIT RAM ANALYSIS (174000-177777 octal)")
print("=" * 70)

PIT_BASE = 0o174000  # 63488 decimal
PIT_NAMES = {
    0: "PIT#0 (identity)", 3: "FUPIT#3", 4: "FPIT#4",
    5: "5PIT#5", 6: "XPIT#6", 7: "DPIT#7",
    8: "RPIT#10", 9: "SPIT#11", 10: "MPIT#12",
    13: "IPIT#15", 15: "PIT#17"
}

# Read all PIT entries
pit_data = {}  # pit_num -> list of (log_page, prote, phys_page) for non-zero entries
for pit in range(16):
    pit_start = PIT_BASE + (pit * 128)  # 128 words per PIT (64 entries * 2 words)
    entries = []
    for pg in range(64):
        addr = pit_start + (pg * 2)
        w0 = read_word(addr)      # protection/flags word
        w1 = read_word(addr + 1)  # physical page number
        if w0 != 0 or w1 != 0:
            entries.append((pg, w0, w1))
    pit_data[pit] = entries

# Show summary
print("\nPIT Summary:")
for pit in range(16):
    name = PIT_NAMES.get(pit, f"PIT#{pit:o}")
    count = len(pit_data[pit])
    marker = " <-- HAS DATA" if count > 0 else ""
    print(f"  {name:20s}: {count:2d}/64 entries{marker}")

# Dump MPIT in detail
mpit_num = 10  # decimal (= 12 octal)
mpit_entries = pit_data[mpit_num]
print(f"\nMPIT (#12 octal) - {len(mpit_entries)} entries:")
print(f"  {'LogPg':>5s}  {'PROTE':>8s}  {'PhysPg':>8s}  {'Prote(bin)':>18s}  Notes")
print(f"  {'-----':>5s}  {'--------':>8s}  {'--------':>8s}  {'------------------':>18s}  -----")

for log_pg, prote, phys_pg in mpit_entries:
    prote_bin = format(prote & 0xFFFF, '016b')
    # Check if bit 15 set (page present in typical MMU)
    present = "PRESENT" if (prote & 0x8000) else "not-present"
    identity = "IDENTITY" if phys_pg == log_pg else f"REMAP({log_pg:o}->{phys_pg:o})"
    print(f"  {log_pg:5o}  {oct6(prote):>8s}  {oct6(phys_pg):>8s}  {prote_bin}  {present} {identity}")

# Also dump any other non-empty PITs
for pit in range(16):
    if pit == mpit_num:
        continue
    if not pit_data[pit]:
        continue
    name = PIT_NAMES.get(pit, f"PIT#{pit:o}")
    print(f"\n{name} - {len(pit_data[pit])} entries:")
    for log_pg, prote, phys_pg in pit_data[pit]:
        identity = "IDENTITY" if phys_pg == log_pg else f"REMAP"
        print(f"  LogPg {log_pg:02o} -> PhysPg {oct6(phys_pg)}  PROTE={oct6(prote)}  {identity}")


# ============================================================================
# Part 2: Build translation table from MPIT
# ============================================================================
print("\n" + "=" * 70)
print("PART 2: MPIT LOGICAL->PHYSICAL PAGE TRANSLATION TABLE")
print("=" * 70)

# Build the mapping: logical_page -> physical_page
mpit_map = {}
for log_pg, prote, phys_pg in mpit_entries:
    mpit_map[log_pg] = phys_pg

# For pages NOT in MPIT, assume identity mapping (logical = physical)
print("\nTranslation (non-identity only):")
for log_pg in sorted(mpit_map.keys()):
    phys_pg = mpit_map[log_pg]
    if phys_pg != log_pg:
        print(f"  Logical page {log_pg:02o} ({log_pg:3d}) -> Physical page {phys_pg:o} ({phys_pg:d})")


def translate_logical_to_physical(logical_addr):
    """Translate a logical word address to physical using MPIT."""
    log_page = (logical_addr >> 10) & 0x3F  # upper 6 bits = page (0-63)
    offset = logical_addr & 0x3FF  # lower 10 bits = offset within page (0-1023)

    if log_page in mpit_map:
        phys_page = mpit_map[log_page]
    else:
        phys_page = log_page  # identity mapping assumed

    phys_addr = (phys_page << 10) | offset
    return phys_addr


# ============================================================================
# Part 3: Translate and read system RT-Descriptions
# ============================================================================
print("\n" + "=" * 70)
print("PART 3: TRANSLATED RT-DESCRIPTION READ (L07 symbols)")
print("=" * 70)

# L07 system RT addresses (from SYMBOL-2-LIST)
L07_SYSTEM_RTS = [
    (0o012071, "DUMMY"), (0o012117, "STSIN"), (0o012145, "RTERR"),
    (0o012173, "1SWAP"), (0o012221, "TIMRT"), (0o012247, "RTDIL"),
    (0o012275, "DIMWD"), (0o012323, "BPTMP"), (0o012351, "RTSLI"),
    (0o012377, "ACCRT"), (0o012425, "TERMP"), (0o012453, "5SWAP"),
    (0o012501, "RWRT1"), (0o012527, "RWRT2"), (0o012555, "RWRT3"),
]

RT_SIZE = 22  # words (0o26)

STATUS_BITS = {
    0o00: '5BACK', 0o01: '5USED', 0o02: '5TSLI', 0o03: '5ESCF',
    0o04: '5BRKF', 0o06: '5XMSY', 0o10: '5SWWA', 0o11: '5RTOF',
    0o14: '5INT', 0o15: '5RWAI', 0o17: '5WAIT',
}


def decode_status_flags(status):
    flags = []
    for bit, name in sorted(STATUS_BITS.items()):
        if status & (1 << bit):
            flags.append(name)
    return flags


def decode_state(status):
    if status & (1 << 0o17):
        return "IO-WAIT"
    if status & (1 << 0o15):
        return "RES-WAIT"
    if status & (1 << 0o10):
        return "SW-WAIT"
    if status & (1 << 0o11):
        return "RTOFF"
    return "READY"


print(f"\n  {'NAME':8s} {'LogAddr':>8s} {'PhysAddr':>8s}  {'STATUS':>6s}  {'State':10s}  "
      f"{'PRI':>6s}  {'STADR':>6s}  {'SEG1':>6s} {'SEG2':>6s}  Flags")
print("  " + "-" * 100)

for log_addr, name in L07_SYSTEM_RTS:
    phys_addr = translate_logical_to_physical(log_addr)

    log_page = (log_addr >> 10) & 0x3F
    phys_page = (phys_addr >> 10) & 0x3F

    if phys_addr + RT_SIZE >= word_count:
        print(f"  {name:8s} {oct6(log_addr):>8s} {oct6(phys_addr):>8s}  OUTSIDE DUMP")
        continue

    status = read_word(phys_addr + 0o01)
    prity = read_word(phys_addr + 0o03)
    stadr = read_word(phys_addr + 0o10)
    segm1 = read_word(phys_addr + 0o11)
    segm2 = read_word(phys_addr + 0o12)
    flags = decode_status_flags(status)
    state = decode_state(status)
    flag_str = ','.join(flags) if flags else '-'

    mapped = "" if log_page == phys_page else f" (pg {log_page:o}->{phys_page:o})"
    print(f"  {name:8s} {oct6(log_addr):>8s} {oct6(phys_addr):>8s}  {oct6(status):>6s}  "
          f"{state:10s}  {oct6(prity):>6s}  {oct6(stadr):>6s}  "
          f"{oct6(segm1):>6s} {oct6(segm2):>6s}  {flag_str}{mapped}")


# ============================================================================
# Part 4: Verify by looking at raw page 2 pointers
# ============================================================================
print("\n" + "=" * 70)
print("PART 4: GLOBAL POINTERS (page 2, identity-mapped)")
print("=" * 70)

GLOBALS = {
    0o4007: "RTREF", 0o4010: "CURPR", 0o4011: "MQUEU",
    0o4012: "BTIMQ", 0o4013: "BEXQU", 0o4020: "RTSTA",
    0o4321: "SEGST", 0o4323: "RTEND",
    0o4320: "SEGTB", 0o4322: "CORMB",
}

for addr in sorted(GLOBALS.keys()):
    name = GLOBALS[addr]
    val = read_word(addr)
    # Try to translate the value as a logical address
    if val > 0 and val < 0xFFFF:
        phys = translate_logical_to_physical(val)
        trans = f"  -> phys {oct6(phys)}" if phys != val else "  (identity)"
    else:
        trans = ""
    print(f"  {name:6s} @ {oct6(addr)} = {oct6(val)}  (dec {val:6d}){trans}")

# Check consistency: RTSTA value should be close to DUMMY address
rtsta_val = read_word(0o4020)
dummy_l07 = 0o012071
print(f"\n  RTSTA value: {oct6(rtsta_val)} = {rtsta_val} decimal")
print(f"  L07 DUMMY addr: {oct6(dummy_l07)} = {dummy_l07} decimal")
print(f"  Difference: {rtsta_val - dummy_l07} words")
if rtsta_val > 0 and rtsta_val < word_count:
    # Check if RTSTA matches DUMMY
    rt_count = (rtsta_val - dummy_l07) // RT_SIZE if rtsta_val > dummy_l07 else -1
    print(f"  RT entries between RTSTA and DUMMY: {rt_count}")


# ============================================================================
# Part 5: Brute-force scan for RT-Description table
# ============================================================================
print("\n" + "=" * 70)
print("PART 5: SCANNING FOR RT-DESCRIPTION TABLE PATTERN")
print("=" * 70)

# Strategy: look for sequences of 22-word blocks where:
# - offset +1 (STATU) has plausible status bits (some known bits set, others clear)
# - offset +3 (PRITY) is a small number (< 256)
# - offset +8 (STADR) is a reasonable code address (< 64K)
# - offset +9 (SEGM1) is a reasonable segment number (< 256)
# - Multiple consecutive matches = high confidence

def score_rt_block(addr):
    """Score how likely a 22-word block at 'addr' is an RT-Description."""
    if addr + RT_SIZE >= word_count:
        return -1

    score = 0
    status = read_word(addr + 0o01)
    prity = read_word(addr + 0o03)
    stadr = read_word(addr + 0o10)
    segm1 = read_word(addr + 0o11)
    segm2 = read_word(addr + 0o12)
    actpr = read_word(addr + 0o17)

    # All zeros = possibly unused RT slot (common for background programs)
    all_zero = True
    for i in range(RT_SIZE):
        if read_word(addr + i) != 0:
            all_zero = False
            break
    if all_zero:
        return 50  # Could be an empty slot

    # Status word: only known bits should be set
    known_mask = 0
    for bit in STATUS_BITS:
        known_mask |= (1 << bit)
    unknown_bits = status & ~known_mask
    if unknown_bits == 0 and status != 0:
        score += 30  # Only known status bits set
    elif status == 0:
        score += 5

    # Priority should be reasonable (< 256, typical 0-200)
    if 0 < prity < 256:
        score += 20
    elif prity == 0:
        score += 5
    elif prity > 10000:
        score -= 30  # Very suspicious

    # Start address should be in code range
    if 0 < stadr < 0o200000:
        score += 10
    elif stadr == 0:
        score += 2

    # Segment numbers should be reasonable (< 128 typically)
    if 0 < segm1 < 128:
        score += 10
    elif segm1 == 0:
        score += 2
    if 0 < segm2 < 128:
        score += 10
    elif segm2 == 0:
        score += 2

    return score


# Scan every 22-word boundary for runs of RT-Description-like blocks
print("\nScanning for runs of 5+ consecutive RT-Description-like blocks...")
best_runs = []

addr = 0
while addr + RT_SIZE * 5 < word_count:
    # Score a run of blocks starting here
    run_score = 0
    run_length = 0
    for i in range(50):  # Check up to 50 consecutive blocks
        block_addr = addr + (i * RT_SIZE)
        s = score_rt_block(block_addr)
        if s < 0:
            break
        if s >= 10:  # Minimum threshold to count as plausible
            run_score += s
            run_length += 1
        else:
            break

    if run_length >= 5:
        best_runs.append((run_score, run_length, addr))

    addr += 1  # Slide by 1 word for thorough search

# Sort by score (highest first), show top 10
best_runs.sort(reverse=True)
print(f"\nFound {len(best_runs)} candidate runs. Top 10:")
print(f"  {'Score':>6s}  {'RunLen':>6s}  {'StartAddr':>10s}  {'StartPage':>10s}  Notes")
print(f"  {'------':>6s}  {'------':>6s}  {'----------':>10s}  {'----------':>10s}  -----")

shown = 0
seen_pages = set()
for score, length, start in best_runs[:50]:
    start_page = start >> 10
    # Deduplicate runs that start on the same page
    if start_page in seen_pages:
        continue
    seen_pages.add(start_page)

    # Show first few entries at this location
    name0_status = read_word(start + 0o01)
    name0_prity = read_word(start + 0o03)
    name1_status = read_word(start + RT_SIZE + 0o01)
    name1_prity = read_word(start + RT_SIZE + 0o03)

    print(f"  {score:6d}  {length:6d}  {oct6(start):>10s}  page {start_page:>4o}  "
          f"[0]status={oct6(name0_status)} pri={name0_prity:d}  "
          f"[1]status={oct6(name1_status)} pri={name1_prity:d}")
    shown += 1
    if shown >= 10:
        break

# If we found a good candidate, show the first few entries
if best_runs:
    best_score, best_len, best_start = best_runs[0]
    print(f"\n--- Best candidate: {best_len} entries at {oct6(best_start)} (score {best_score}) ---")
    print(f"  {'#':>3s}  {'Addr':>8s}  {'STATUS':>6s}  {'State':10s}  {'PRI':>5s}  "
          f"{'STADR':>6s}  {'SEG1':>6s} {'SEG2':>6s}  Flags")
    print("  " + "-" * 85)

    for i in range(min(best_len, 40)):
        ba = best_start + (i * RT_SIZE)
        status = read_word(ba + 0o01)
        prity = read_word(ba + 0o03)
        stadr = read_word(ba + 0o10)
        segm1 = read_word(ba + 0o11)
        segm2 = read_word(ba + 0o12)
        flags = decode_status_flags(status)
        state = decode_state(status)
        flag_str = ','.join(flags) if flags else '-'
        marker = ""
        # Check if this could be DUMMY (first system RT should have specific properties)
        if i == 0 and status == 0:
            marker = " (idle?)"
        print(f"  {i:3d}  {oct6(ba):>8s}  {oct6(status):>6s}  {state:10s}  {prity:5d}  "
              f"{oct6(stadr):>6s}  {oct6(segm1):>6s} {oct6(segm2):>6s}  {flag_str}{marker}")


# ============================================================================
# Part 6: Quick hex dump of specific areas for eyeballing
# ============================================================================
print("\n" + "=" * 70)
print("PART 6: HEX DUMP OF KEY AREAS")
print("=" * 70)

def hexdump_words(start, count, label=""):
    """Dump 'count' words starting at word address 'start'."""
    if label:
        print(f"\n{label} ({oct6(start)}-{oct6(start+count-1)}):")
    for i in range(0, count, 8):
        addr = start + i
        words = []
        for j in range(min(8, count - i)):
            w = read_word(addr + j)
            words.append(f"{w:06o}" if w >= 0 else "??????")
        print(f"  {oct6(addr)}: {' '.join(words)}")

# Dump around DUMMY's L07 logical address and its translation
dummy_log = 0o012071
dummy_phys = translate_logical_to_physical(dummy_log)
hexdump_words(dummy_log, 22, f"At DUMMY logical addr {oct6(dummy_log)} (UNTRANSLATED)")
if dummy_phys != dummy_log:
    hexdump_words(dummy_phys, 22, f"At DUMMY translated addr {oct6(dummy_phys)}")

# Dump page 2 globals area
hexdump_words(0o4000, 32, "Page 2 globals (004000-004037)")
hexdump_words(0o4310, 32, "Page 2 globals (004310-004347)")


if __name__ == '__main__':
    pass
