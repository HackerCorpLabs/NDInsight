#!/usr/bin/env python3
"""
Verify I/O device enumeration from DT symbol addresses in physical memory dump.

Reads device terminal datafield addresses (DTxxR/DTxxW) from L07 SYMBOL-2-LIST
and checks the memory dump to see if they contain valid I/O datafield structures.

I/O Datafield fields (from SYMBOL-1-LIST L07):
  RESLI=000000  Reservation chain link
  RTRES=000001  Owning RT program address
  BWLIN=000002  Wait queue head
  TYPRI=000003  Device type and ring
  (ISTAT)=0004  I/O status (position 4)
  MLINK=000005  Monitor queue link
  MFUNC=000006  Monitor function code/address

Each DT device has two halves:
  DTxxR = read datafield  (13 octal = 11 decimal words)
  DTxxW = write datafield (13 octal = 11 decimal words)
"""

import sys
from pathlib import Path

# DPIT #7 page table (from emulator, same as decode-with-dpit.py)
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

# I/O Datafield field offsets (from SYMBOL-1-LIST L07)
DF_FIELDS = {
    'RESLI': 0o00,  # Reservation chain link
    'RTRES': 0o01,  # Owning RT program address
    'BWLIN': 0o02,  # Wait queue head
    'TYPRI': 0o03,  # Device type and ring
    'ISTAT': 0o04,  # I/O status
    'MLINK': 0o05,  # Monitor queue link
    'MFUNC': 0o06,  # Monitor function code
}

# L07 Device Terminal addresses from SYMBOL-2-LIST
# Format: (device_number, read_addr, write_addr)
# Note: devices 2-4 have no DT entries in L07
DT_DEVICES = [
    ( 1, 0o053607, 0o053622),
    ( 5, 0o053635, 0o053650),
    ( 6, 0o053663, 0o053676),
    ( 7, 0o053711, 0o053724),
    ( 8, 0o053737, 0o053752),
    ( 9, 0o053765, 0o054000),
    (10, 0o054013, 0o054026),
    (11, 0o054041, 0o054054),
    (12, 0o054067, 0o054102),
    (13, 0o054115, 0o054130),
    (14, 0o054143, 0o054156),
    (15, 0o054171, 0o054204),
    (16, 0o054217, 0o054232),
    (17, 0o054245, 0o054260),
    (18, 0o054273, 0o054306),
    (19, 0o054321, 0o054334),
    (20, 0o054347, 0o054362),
    (21, 0o054375, 0o054410),
    (22, 0o054423, 0o054436),
    (23, 0o054451, 0o054464),
    (24, 0o054477, 0o054512),
    (25, 0o054525, 0o054540),
    (26, 0o054553, 0o054566),
    (27, 0o054601, 0o054614),
    (28, 0o054627, 0o054642),
    (29, 0o054655, 0o054670),
    (30, 0o054703, 0o054716),
    (31, 0o054731, 0o054744),
    (32, 0o054757, 0o054772),
    (33, 0o055005, 0o055020),
    (34, 0o055033, 0o055046),
    (35, 0o055061, 0o055074),
    (36, 0o055107, 0o055122),
    (37, 0o055135, 0o055150),
    (38, 0o055163, 0o055176),
    (39, 0o055211, 0o055224),
    (40, 0o055237, 0o055252),
    (41, 0o055265, 0o055300),
    (42, 0o055313, 0o055326),
    (43, 0o055341, 0o055354),
    (44, 0o055367, 0o055402),
    (45, 0o055415, 0o055430),
    (46, 0o055443, 0o055456),
    (47, 0o055471, 0o055504),
    (48, 0o055517, 0o055532),
    (49, 0o055545, 0o055560),
    (50, 0o055573, 0o055606),
    (51, 0o055621, 0o055634),
    (52, 0o055647, 0o055662),
    (65, 0o055675, 0o055710),
    (66, 0o055723, 0o055736),
    (67, 0o055751, 0o055764),
    (68, 0o055777, 0o056012),
    (69, 0o056025, 0o056040),
    (70, 0o056053, 0o056066),
    (71, 0o056101, 0o056114),
    (72, 0o056127, 0o056142),
    (73, 0o056155, 0o056170),
    (74, 0o056203, 0o056216),
    (75, 0o056231, 0o056244),
    (76, 0o056257, 0o056272),
    (77, 0o056305, 0o056320),
    (78, 0o056333, 0o056346),
    (79, 0o056361, 0o056374),
    (80, 0o056407, 0o056422),
    (81, 0o056435, 0o056450),
    (82, 0o056463, 0o056476),
    (83, 0o056511, 0o056524),
    (84, 0o056537, 0o056552),
    (85, 0o056565, 0o056600),
    (86, 0o056613, 0o056626),
    (87, 0o056641, 0o056654),
    (88, 0o056667, 0o056702),
    (89, 0o056715, 0o056730),
    (90, 0o056743, 0o056756),
    (91, 0o056771, 0o057004),
    (92, 0o057017, 0o057032),
    (93, 0o057045, 0o057060),
    (94, 0o057073, 0o057106),
    (95, 0o057121, 0o057134),
    (96, 0o057147, 0o057162),
    (97, 0o057175, 0o057210),
    (98, 0o057223, 0o057236),
    (99, 0o057251, 0o057264),
]

# Extended terminals T100-T140
T_DEVICES = [
    (100, 0o057277, 0o057312),
    (101, 0o057325, 0o057340),
    (102, 0o057353, 0o057366),
    (103, 0o057401, 0o057414),
    (104, 0o057427, 0o057442),
    (105, 0o057455, 0o057470),
    (106, 0o057503, 0o057516),
    (107, 0o057531, 0o057544),
    (108, 0o057557, 0o057572),
    (109, 0o057605, 0o057620),
    (110, 0o057633, 0o057646),
    (111, 0o057661, 0o057674),
    (112, 0o057707, 0o057722),
    (113, 0o057735, 0o057750),
    (114, 0o057763, 0o057776),
    (115, 0o060011, 0o060024),
    (116, 0o060037, 0o060052),
    (117, 0o060065, 0o060100),
    (118, 0o060113, 0o060126),
    (119, 0o060141, 0o060154),
    (120, 0o060167, 0o060202),
    (121, 0o060215, 0o060230),
    (122, 0o060243, 0o060256),
    (123, 0o060271, 0o060304),
    (124, 0o060317, 0o060332),
    (125, 0o060345, 0o060360),
    (126, 0o060373, 0o060406),
    (127, 0o060421, 0o060434),
    (128, 0o060447, 0o060462),
    (129, 0o060475, 0o060510),
    (130, 0o060523, 0o060536),
    (131, 0o060551, 0o060564),
    (132, 0o060577, 0o060612),
    (133, 0o060625, 0o060640),
    (134, 0o060653, 0o060666),
    (135, 0o060701, 0o060714),
    (136, 0o060727, 0o060742),
    (137, 0o060755, 0o060770),
    (138, 0o061003, 0o061016),
    (139, 0o061031, 0o061044),
    (140, 0o061057, 0o061072),
]

# Disk controller datafields from SYMBOL-2-LIST L07
DISK_DEVICES = [
    ('D1DF0', 0o031631), ('D1DF1', 0o031644), ('D1DF2', 0o031657), ('D1DF3', 0o031672),
    ('D2DF0', 0o032075), ('D2DF1', 0o032110), ('D2DF2', 0o032123), ('D2DF3', 0o032136),
    ('D3DF0', 0o032341), ('D3DF1', 0o032354), ('D3DF2', 0o032367), ('D3DF3', 0o032402),
    ('D4DF0', 0o032605), ('D4DF1', 0o032620), ('D4DF2', 0o032633), ('D4DF3', 0o032646),
]

# RT program name lookup (subset, from SYMBOL-2-LIST L07)
RT_NAMES = {
    0o012071: 'DUMMY',  0o012117: 'STSIN',  0o012145: 'RTERR',
    0o012173: '1SWAP',  0o012221: 'TIMRT',  0o012247: 'RTDIL',
    0o012275: 'DIMWD',  0o012323: 'BPTMP',  0o012351: 'RTSLI',
    0o012377: 'ACCRT',  0o012425: 'TERMP',  0o012453: '5SWAP',
    0o012501: 'RWRT1',  0o012527: 'RWRT2',  0o012555: 'RWRT3',
    0o012603: 'RWRT5',  0o012631: 'RWRT7',  0o012657: 'RWRT8',
    0o012705: 'RWRT9',  0o012733: 'RTRFA',  0o012761: 'DUMM2',
    0o013007: 'SPRT1',  0o013035: 'SPRT2',  0o013063: 'SPRT3',
    0o013111: 'SPRT4',  0o013137: 'SPRT5',  0o013165: 'SPRT6',
    0o013213: 'SPRT7',  0o013241: 'SPRT8',  0o013267: 'SPRT9',
    0o013315: 'SPR10',  0o013343: 'SPR11',  0o013371: 'SPR12',
    0o013417: 'SPR13',  0o013445: 'SPR14',  0o013473: 'SPR15',
    0o013521: 'SPR16',  0o013547: 'COSPO',  0o013575: 'RWR10',
    0o013623: 'RWR11',  0o013651: 'RWR12',  0o013677: 'RWR13',
    0o013725: 'RWR20',  0o013753: 'RWR14',  0o014001: 'RWR21',
    0o014027: 'RWR25',  0o014055: 'RWR26',  0o014103: 'RWR41',
    0o014131: 'RWR42',  0o014157: 'TADAD',  0o014205: 'UDR01',
    0o014233: 'UDR02',  0o014261: 'UDR03',  0o014307: 'UDR04',
    0o014335: 'UDR05',  0o014363: 'UDR06',  0o014411: 'XROUT',
    0o014437: 'XTRAC',  0o014465: 'XMFID',  0o014513: 'NKSER',
    0o014541: 'NKNAM',  0o014567: 'ERSWD',  0o014615: 'PROMA',
    0o014643: 'EVMES',  0o014671: 'BOPCO',  0o014717: 'MTSER',
    0o014745: 'RTREC',
    # Background programs
    0o023337: 'BAK01',  0o023365: 'BAK02',  0o023413: 'BAK03',
    0o023441: 'BAK04',  0o023467: 'BAK05',  0o023515: 'BAK06',
    0o023543: 'BAK07',
}


def ofmt(v):
    """Format as 6-digit octal."""
    if v is None:
        return "------"
    return f"{v:06o}"


class MemoryDump:
    def __init__(self, filepath):
        self.data = Path(filepath).read_bytes()
        self.word_count = len(self.data) // 2

    def read_phys_word(self, phys_word_addr):
        byte_off = phys_word_addr * 2
        if byte_off + 1 >= len(self.data):
            return None
        return (self.data[byte_off] << 8) | self.data[byte_off + 1]

    def translate_dpit(self, logical_addr):
        vpn = (logical_addr >> 10) & 0o77
        dip = logical_addr & 0o1777
        ppn = DPIT.get(vpn)
        if ppn is None:
            return None
        return ppn * 1024 + dip

    def read_dpit_word(self, logical_addr):
        phys = self.translate_dpit(logical_addr)
        if phys is None:
            return None
        return self.read_phys_word(phys)


def resolve_rt_name(addr):
    """Try to resolve an RT-Description address to a name."""
    name = RT_NAMES.get(addr)
    if name:
        return name
    # Check background range (BAK01=023337, each +26 octal)
    if 0o023337 <= addr <= 0o030457:
        slot = (addr - 0o023337) // 0o26
        remainder = (addr - 0o023337) % 0o26
        if remainder == 0:
            return f"BAK{slot + 1:02d}"
    return None


def read_datafield(mem, base_addr):
    """Read I/O datafield fields from a DPIT logical address."""
    fields = {}
    for name, offset in DF_FIELDS.items():
        w = mem.read_dpit_word(base_addr + offset)
        fields[name] = w
    return fields


def is_nonzero(fields):
    """Check if any field is non-zero (indicating the datafield is in use or initialized)."""
    for name, val in fields.items():
        if val is not None and val != 0:
            return True
    return False


def main():
    dump_file = sys.argv[1] if len(sys.argv) > 1 else r"C:\Users\ronny\Downloads\nd100_physmem_256k.bin"
    mem = MemoryDump(dump_file)
    print(f"Dump: {mem.word_count} words ({mem.word_count // 1024}KW)")

    # ═══════════════════════════════════════════════════════════════
    # CHARACTER DEVICE TERMINALS (DT01-DT99, T100-T140)
    # ═══════════════════════════════════════════════════════════════
    print()
    print("=" * 120)
    print("  CHARACTER DEVICE TERMINALS (from L07 SYMBOL-2-LIST DTxxR/DTxxW)")
    print("=" * 120)
    print(f"  {'Dev':>4s} {'Dir':<5s} {'Address':>8s} {'Phys':>8s} "
          f"{'RESLI':>7s} {'RTRES':>7s} {'BWLIN':>7s} {'TYPRI':>7s} "
          f"{'ISTAT':>7s} {'MLINK':>7s} {'MFUNC':>7s} {'Owner':<8s} {'Notes'}")
    print(f"  {'----':>4s} {'-----':<5s} {'--------':>8s} {'--------':>8s} "
          f"{'------':>7s} {'------':>7s} {'------':>7s} {'------':>7s} "
          f"{'------':>7s} {'------':>7s} {'------':>7s} {'--------':<8s} {'-----'}")

    active_count = 0
    accessible_count = 0
    inaccessible_count = 0

    all_devices = []
    for devno, raddr, waddr in DT_DEVICES:
        all_devices.append((devno, 'R', raddr))
        all_devices.append((devno, 'W', waddr))

    for devno, raddr, waddr in T_DEVICES:
        all_devices.append((devno, 'R', raddr))
        all_devices.append((devno, 'W', waddr))

    for devno, direction, addr in all_devices:
        phys = mem.translate_dpit(addr)
        if phys is None or phys >= mem.word_count:
            inaccessible_count += 1
            continue
        accessible_count += 1

        fields = read_datafield(mem, addr)
        if not is_nonzero(fields):
            continue  # Skip completely empty datafields

        active_count += 1
        rtres = fields['RTRES']
        owner = resolve_rt_name(rtres) if rtres else "-"
        if owner is None:
            owner = ofmt(rtres) if rtres else "-"

        notes = ""
        if fields['MFUNC'] and fields['MFUNC'] != 0:
            notes += "has_mfunc "
        if fields['BWLIN'] and fields['BWLIN'] != 0:
            notes += "has_waitq "

        label = f"DT{devno:02d}" if devno < 100 else f"T{devno}"
        print(f"  {label:>4s} {direction + 'ead' if direction == 'R' else direction + 'rite':<5s} "
              f"{ofmt(addr):>8s} {ofmt(phys):>8s} "
              f"{ofmt(fields['RESLI']):>7s} {ofmt(fields['RTRES']):>7s} "
              f"{ofmt(fields['BWLIN']):>7s} {ofmt(fields['TYPRI']):>7s} "
              f"{ofmt(fields['ISTAT']):>7s} {ofmt(fields['MLINK']):>7s} "
              f"{ofmt(fields['MFUNC']):>7s} {owner:<8s} {notes}")

    print(f"\n  Accessible: {accessible_count}, Non-zero: {active_count}, "
          f"Inaccessible (outside dump): {inaccessible_count}")

    # ═══════════════════════════════════════════════════════════════
    # DISK CONTROLLER DATAFIELDS
    # ═══════════════════════════════════════════════════════════════
    print()
    print("=" * 120)
    print("  DISK CONTROLLER DATAFIELDS (from L07 SYMBOL-2-LIST D1DF0-D4DF3)")
    print("=" * 120)
    print(f"  {'Name':<6s} {'Address':>8s} {'Phys':>8s} "
          f"{'RESLI':>7s} {'RTRES':>7s} {'BWLIN':>7s} {'TYPRI':>7s} "
          f"{'ISTAT':>7s} {'MLINK':>7s} {'MFUNC':>7s} {'Owner':<8s}")
    print(f"  {'------':<6s} {'--------':>8s} {'--------':>8s} "
          f"{'------':>7s} {'------':>7s} {'------':>7s} {'------':>7s} "
          f"{'------':>7s} {'------':>7s} {'------':>7s} {'--------':<8s}")

    for name, addr in DISK_DEVICES:
        phys = mem.translate_dpit(addr)
        if phys is None or phys >= mem.word_count:
            print(f"  {name:<6s} {ofmt(addr):>8s} INACCESSIBLE")
            continue

        fields = read_datafield(mem, addr)
        rtres = fields['RTRES']
        owner = resolve_rt_name(rtres) if rtres else "-"
        if owner is None:
            owner = ofmt(rtres) if rtres else "-"

        print(f"  {name:<6s} {ofmt(addr):>8s} {ofmt(phys):>8s} "
              f"{ofmt(fields['RESLI']):>7s} {ofmt(fields['RTRES']):>7s} "
              f"{ofmt(fields['BWLIN']):>7s} {ofmt(fields['TYPRI']):>7s} "
              f"{ofmt(fields['ISTAT']):>7s} {ofmt(fields['MLINK']):>7s} "
              f"{ofmt(fields['MFUNC']):>7s} {owner:<8s}")

    # ═══════════════════════════════════════════════════════════════
    # CROSS-CHECK: Verify BAK01's reserved devices match DT01
    # ═══════════════════════════════════════════════════════════════
    print()
    print("=" * 120)
    print("  CROSS-CHECK: BAK01 reservation chain (BRESL at offset 020)")
    print("=" * 120)

    # BAK01 is at 023337
    bak01_addr = 0o023337
    bresl = mem.read_dpit_word(bak01_addr + 0o20)
    print(f"  BAK01 address: {ofmt(bak01_addr)}")
    print(f"  BAK01 BRESL (offset 020): {ofmt(bresl) if bresl is not None else 'UNREADABLE'}")

    if bresl and bresl != 0:
        print(f"  Following RESLI chain from {ofmt(bresl)}:")
        visited = set()
        addr = bresl
        chain_len = 0
        while addr and addr != 0 and chain_len < 20:
            if addr in visited:
                print(f"    CYCLE detected at {ofmt(addr)}!")
                break
            visited.add(addr)

            fields = read_datafield(mem, addr)
            resli = fields['RESLI']
            rtres = fields['RTRES']
            owner = resolve_rt_name(rtres) if rtres else "-"
            is_dt = "** IS A DT ADDRESS **" if any(addr == r or addr == w for _, r, w in DT_DEVICES) else ""

            print(f"    [{chain_len}] addr={ofmt(addr)} RESLI={ofmt(resli)} "
                  f"RTRES={ofmt(rtres)} TYPRI={ofmt(fields['TYPRI'])} "
                  f"MFUNC={ofmt(fields['MFUNC'])} owner={owner} {is_dt}")

            addr = resli
            chain_len += 1
    else:
        print("  BAK01 has no reserved devices (BRESL=0)")

    print()
    print("Done.")


if __name__ == '__main__':
    main()
