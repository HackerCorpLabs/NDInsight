#!/usr/bin/env python3
"""
Automatic I/O device discovery from SINTRAN III physical memory dump.

Discovery strategy:
  Phase 1 - KNOWN SYMBOLS: Read ALL named device datafield addresses from L07
            symbol tables and validate in memory
  Phase 2 - ARRAY SCAN: Scan contiguous device arrays (DT terminals, BD block
            devices) using known step sizes and stop boundaries
  Phase 3 - REGION SCAN: Walk the controller region (9BBHD..9EEHD) using known
            sub-region boundaries, checking only at valid datafield-start offsets
  Phase 4 - CHAIN WALK: Walk BRESL reservation chains and MQUEU monitor queue
            for dynamically-linked devices

All starting points verified from L07 SYMBOL-1-LIST and SYMBOL-2-LIST.
TYPRI bit field decoding from GDEVTY subroutine in RP-P2-MONCALLS.NPL.
Filter levels from SINTRAN-STRUCTURES.md Section 18.

Usage: python discover-io-devices.py [dump_file] [--filter LEVEL]
       LEVEL: all | configured | initialized | active  (default: all)
"""

import sys
import argparse
from pathlib import Path

# =============================================================================
# DPIT #7 PAGE TABLE (from emulator)
# =============================================================================

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


# =============================================================================
# STRUCTURE OFFSETS (stable across SINTRAN versions, from SYMBOL-1-LIST)
# =============================================================================

# I/O Datafield standard layout
RESLI  = 0o00   # Reservation chain link
RTRES  = 0o01   # Owning RT program address
BWLIN  = 0o02   # Wait queue head
TYPRI  = 0o03   # Device type and ring bits
ISTAT  = 0o04   # I/O status word
MLINK  = 0o05   # Monitor queue link
MFUNC  = 0o06   # Monitor function code address

# RT-Description layout
TLINK  = 0o00   # Time queue link (SAME offset as RESLI!)
STATU  = 0o01   # Status word
BRESL  = 0o20   # Reservation queue head
RT_SIZE = 0o26  # RT-Description size (22 decimal words)

# Device array step sizes (from symbol address differences)
DT_HALF_SIZE = 0o13   # 11 words per terminal R or W half
BD_HALF_SIZE = 0o13   # 11 words per block device R or W half
DISK_DF_SIZE = 0o13   # 11 words per disk controller datafield
CDF_SIZE     = 0o13   # 11 words per CDF channel


# =============================================================================
# GLOBAL POINTERS (DPIT logical addresses from SYMBOL-1-LIST L07)
# =============================================================================

RTSTA_ADDR = 0o004020  # RT table start pointer
RTEND_ADDR = 0o004323  # RT table end pointer
MQUEU_ADDR = 0o004011  # Monitor queue head


# =============================================================================
# TYPRI BIT DEFINITIONS (from GDEVTY in RP-P2-MONCALLS.NPL + SYMBOL-1-LIST)
# =============================================================================

# Bit positions in TYPRI word (all from SYMBOL-1-LIST, identical K03/L07/M06)
# Primary type bits (tested in GDEVTY priority order, first match wins)
BIT_5TERM  = 5    # Terminal device (9BTERM=1)
BIT_5BAD   = 4    # TAD - Terminal Adapter Device (9BBAD=2)
BIT_5IBDV  = 6    # Indexed Block Device / disk (9BIBDV=4)
BIT_5FLOP  = 8    # Floppy disk (9BFLOP=5)
BIT_5MT    = 9    # Magnetic tape (9BMT=6)
BIT_5RFIL  = 14   # Remote file (9BRFILE=7)
# Attribute bits (tested after primary type)
BIT_5IOBT  = 15   # I/O block transfer capable
BIT_5CONC  = 13   # Concurrent I/O
BIT_5ISET  = 12   # I/O initialization complete
BIT_5SPLI  = 11   # Split datafield (separate R/W halves)
BIT_M144B  = 10   # 144-byte block format
BIT_5NORES = 3    # No reservation required
BIT_5CLDV  = 2    # Closable device
BIT_5HDMA  = 7    # HDMA/X.21 flag
# Aliases for backwards compat
BIT_5SPLITDF = BIT_5SPLI


def decode_typri_type(typri):
    """Return primary device type string from TYPRI using GDEVTY priority order.

    Source: RP-P2-MONCALLS.NPL lines 2623-2629.
    Tests bits in this exact order; first match wins.
    """
    if typri is None or typri == 0:
        return ""
    if typri & (1 << BIT_5TERM):
        return "TERM"
    if typri & (1 << BIT_5BAD):
        return "TAD"
    if typri & (1 << BIT_5IBDV):
        return "IBDV"
    if typri & (1 << BIT_5FLOP):
        return "FLOP"
    if typri & (1 << BIT_5MT):
        return "MT"
    if typri & (1 << BIT_5RFIL):
        return "RFIL"
    return ""


def decode_typri_attrs(typri):
    """Return attribute flags string from TYPRI (excluding primary type bit).

    Source: RP-P2-MONCALLS.NPL lines 2636-2642.
    """
    if typri is None or typri == 0:
        return ""
    parts = []
    if typri & (1 << BIT_5IOBT):
        parts.append("IOBT")
    if typri & (1 << BIT_5CONC):
        parts.append("CONC")
    if typri & (1 << BIT_5ISET):
        parts.append("ISET")
    if typri & (1 << BIT_5SPLI):
        parts.append("SPLI")
    if typri & (1 << BIT_M144B):
        parts.append("M144")
    if typri & (1 << BIT_5NORES):
        parts.append("NORE")
    if typri & (1 << BIT_5CLDV):
        parts.append("CLDV")
    if typri & (1 << BIT_5HDMA):
        parts.append("HDMA")
    return "+".join(parts)


def decode_typri(typri):
    """Decode TYPRI bits into combined type+attributes string."""
    if typri is None or typri == 0:
        return ""
    primary = decode_typri_type(typri)
    attrs = decode_typri_attrs(typri)
    if primary and attrs:
        return f"{primary}|{attrs}"
    return primary or attrs or f"x{typri:04x}"


# =============================================================================
# DEVICE REGION BOUNDARIES (L07 SYMBOL-2-LIST, verified in NPL source)
# =============================================================================

# Controller region (checked by kernel: IF X >= 9BBHD AND X < 9EEHD)
REGION_9BBHD = 0o031441  # = 9FDFD = 9FSTR - start of ALL controllers
REGION_9ESTR = 0o033315  # End of disk sorting range
REGION_9EDFD = 0o041062  # End of disk datafields (SCODE marks this)
REGION_9EEHD = 0o042312  # End of ALL controller datafields (= DEMFI)

# Character terminal boundaries
TERM_5TTST   = 0o053603  # Terminal Table Start
TERM_5TTSZ   = 0o26      # Terminal Table entry Size (R+W pair = 22 words)
DT01R_ADDR   = 0o053607  # First terminal datafield (DT01 Read half)
T140W_ADDR   = 0o061072  # Last terminal datafield (T140 Write half)

# Block device boundaries
BD_9BDST     = 0o061203  # Block Device Structure table header
BD01R_ADDR   = 0o061207  # First block device (BD01 Read half)
BD50W_ADDR   = 0o063310  # Last block device (BD50 Write half)
BD_9BDSL     = 0o063317  # Block Device Structure List end

# Background program boundaries
BP_9FBPR = 0o023337  # First Background Program Register
BP_9LBPR = 0o031041  # Last Background Program Register


# =============================================================================
# NAMED DEVICE SYMBOLS (L07 SYMBOL-2-LIST)
# All addresses are datafield start addresses in DPIT logical space
# =============================================================================

# Disk Controller Datafields (within 9BBHD..9EDFD range)
# Disk 1-4: SMD controllers, 4 datafields each at 13₈-word spacing
# Winchester 1-2: 2 datafields each
DISK_SYMBOLS = {
    0o031631: 'D1DF0', 0o031644: 'D1DF1', 0o031657: 'D1DF2', 0o031672: 'D1DF3',
    0o032075: 'D2DF0', 0o032110: 'D2DF1', 0o032123: 'D2DF2', 0o032136: 'D2DF3',
    0o032341: 'D3DF0', 0o032354: 'D3DF1', 0o032367: 'D3DF2', 0o032402: 'D3DF3',
    0o032605: 'D4DF0', 0o032620: 'D4DF1', 0o032633: 'D4DF2', 0o032646: 'D4DF3',
    0o033051: 'W1DF0', 0o033064: 'W1DF1',
    0o033267: 'W2DF0', 0o033302: 'W2DF1',
}

# Floppy disk ring buffer pointers (within controller region)
FLOPPY_RING_SYMBOLS = {
    0o031452: 'FDRI2', 0o031542: 'FDRI1',
}

# SCSI Disk I/O Datafields (within 9ESTR..9EDFD range)
SCSI_DISK_SYMBOLS = {
    0o036350: 'SCDDB',
    0o036442: 'SCDI1', 0o036645: 'SCDI2', 0o037050: 'SCDI3', 0o037253: 'SCDI4',
    0o037456: 'SCDI5', 0o037661: 'SCDI6', 0o040064: 'SCDI7', 0o040267: 'SCDI8',
    0o040454: 'SCODB',
    0o040472: 'SCOD1', 0o040675: 'SCOD2',
}

# Domain Datafields (within 9EDFD..9EEHD range)
DOMAIN_SYMBOLS = {
    0o041064: 'DOMDF',
    0o041340: 'DOM01', 0o041377: 'DOM02', 0o041436: 'DOM03', 0o041475: 'DOM04',
    0o041534: 'DOM05', 0o041573: 'DOM06', 0o041632: 'DOM07', 0o041671: 'DOM10',
    0o041730: 'DOM11', 0o041767: 'DOM12', 0o042026: 'DOM13', 0o042065: 'DOM14',
    0o042124: 'DOM15', 0o042163: 'DOM16', 0o042222: 'DOM17', 0o042261: 'DOM20',
}

# Magnetic Tape Datafields (outside controller region)
MAGTAPE_SYMBOLS = {
    0o045275: 'MTDI1', 0o045337: 'MTDO1',
    0o045410: 'MTDI2', 0o045452: 'MTDO2',
    0o045523: 'MTDI3', 0o045565: 'MTDO3',
    0o045636: 'MTDI4', 0o045700: 'MTDO4',
    0o045751: 'M2DI1', 0o046013: 'M2DO1',
    0o046064: 'M2DI2', 0o046127: 'M2DO2',
    0o046177: 'M2DI3', 0o046241: 'M2DO3',
    0o046312: 'M2DI4', 0o046355: 'M2DO4',
}

# SCSI Controller Datafields (channel-level structures)
SCSI_CTRL_SYMBOLS = {
    0o046530: 'SCSI1', 0o046661: 'SCSI2',
}

# SCSI Status Structures
SCSI_STATUS_SYMBOLS = {
    0o046706: 'SCSDB',
    0o046754: 'SCST1',
    0o047005: 'SS1I0', 0o047047: 'SS1O0', 0o047110: 'S1U0R',
    0o047236: 'SCST2',
    0o047267: 'SS2I0', 0o047331: 'SS2O0', 0o047372: 'S2U0R',
}

# Floppy Disk Datafields (outside controller region)
FLOPPY_SYMBOLS = {
    0o050615: 'FDID1',
    0o050646: 'F1U0I', 0o050711: 'F1U0O',
    0o050756: 'F1U1I', 0o051021: 'F1U1O',
    0o051066: 'F1U2I', 0o051131: 'F1U2O',
    0o051353: 'FDID2',
    0o051404: 'F2U0I', 0o051447: 'F2U0O',
    0o051514: 'F2U1I', 0o051557: 'F2U1O',
    0o051624: 'F2U2I', 0o051667: 'F2U2O',
}

# ND-500 Interface Datafield
ND500_SYMBOLS = {
    0o051767: 'N500D',
    0o052222: 'S5CPU',
    0o052270: '5CPU2', 0o052336: '5CPU3', 0o052404: '5CPU4',
}

# HDLC/HDFI Communication Devices
HDLC_SYMBOLS = {
    0o052733: 'HDMI1',
    0o053024: 'HDMO1',
    0o053052: 'HDFI1',
    0o053100: 'HDFO1',
}

# Multi-Net Devices
MNDF_SYMBOLS = {
    0o053151: 'MNDF0',
    0o053165: 'MNNA0', 0o053200: 'MNID0', 0o053240: 'MNOD0',
    0o053306: 'MNDF1',
    0o053322: 'MNNA1', 0o053335: 'MNID1', 0o053375: 'MNOD1',
    0o053443: 'MNDF2',
    0o053457: 'MNNA2', 0o053472: 'MNID2', 0o053532: 'MNOD2',
}

# Vector Event Devices
VECTOR_SYMBOLS = {
    0o050172: 'VEFIE', 0o050224: 'VEDO1',
    0o050343: 'VE2FI', 0o050375: 'VEDO2',
}

# SCSI Boot Datafields
SCSI_BOOT_SYMBOLS = {
    0o050024: 'SCBDF', 0o050034: 'SCDDF',
    0o050070: 'SCODF', 0o050100: 'SCSDF', 0o050104: 'SCTDF',
}

# Character Terminal Symbols (DT01-DT99, T100-T140)
# Built as a single dict: address -> name
DT_SYMBOLS = {}
_DT_LIST = [
    (1, 0o053607, 0o053622),
    (5, 0o053635, 0o053650), (6, 0o053663, 0o053676),
    (7, 0o053711, 0o053724), (8, 0o053737, 0o053752),
    (9, 0o053765, 0o054000), (10, 0o054013, 0o054026),
    (11, 0o054041, 0o054054), (12, 0o054067, 0o054102),
    (13, 0o054115, 0o054130), (14, 0o054143, 0o054156),
    (15, 0o054171, 0o054204), (16, 0o054217, 0o054232),
    (17, 0o054245, 0o054260), (18, 0o054273, 0o054306),
    (19, 0o054321, 0o054334), (20, 0o054347, 0o054362),
    (21, 0o054375, 0o054410), (22, 0o054423, 0o054436),
    (23, 0o054451, 0o054464), (24, 0o054477, 0o054512),
    (25, 0o054525, 0o054540), (26, 0o054553, 0o054566),
    (27, 0o054601, 0o054614), (28, 0o054627, 0o054642),
    (29, 0o054655, 0o054670), (30, 0o054703, 0o054716),
    (31, 0o054731, 0o054744), (32, 0o054757, 0o054772),
    (33, 0o055005, 0o055020), (34, 0o055033, 0o055046),
    (35, 0o055061, 0o055074), (36, 0o055107, 0o055122),
    (37, 0o055135, 0o055150), (38, 0o055163, 0o055176),
    (39, 0o055211, 0o055224), (40, 0o055237, 0o055252),
    (41, 0o055265, 0o055300), (42, 0o055313, 0o055326),
    (43, 0o055341, 0o055354), (44, 0o055367, 0o055402),
    (45, 0o055415, 0o055430), (46, 0o055443, 0o055456),
    (47, 0o055471, 0o055504), (48, 0o055517, 0o055532),
    (49, 0o055545, 0o055560), (50, 0o055573, 0o055606),
    (51, 0o055621, 0o055634), (52, 0o055647, 0o055662),
    (65, 0o055675, 0o055710), (66, 0o055723, 0o055736),
    (67, 0o055751, 0o055764), (68, 0o055777, 0o056012),
    (69, 0o056025, 0o056040), (70, 0o056053, 0o056066),
    (71, 0o056101, 0o056114), (72, 0o056127, 0o056142),
    (73, 0o056155, 0o056170), (74, 0o056203, 0o056216),
    (75, 0o056231, 0o056244), (76, 0o056257, 0o056272),
    (77, 0o056305, 0o056320), (78, 0o056333, 0o056346),
    (79, 0o056361, 0o056374), (80, 0o056407, 0o056422),
    (81, 0o056435, 0o056450), (82, 0o056463, 0o056476),
    (83, 0o056511, 0o056524), (84, 0o056537, 0o056552),
    (85, 0o056565, 0o056600), (86, 0o056613, 0o056626),
    (87, 0o056641, 0o056654), (88, 0o056667, 0o056702),
    (89, 0o056715, 0o056730), (90, 0o056743, 0o056756),
    (91, 0o056771, 0o057004), (92, 0o057017, 0o057032),
    (93, 0o057045, 0o057060), (94, 0o057073, 0o057106),
    (95, 0o057121, 0o057134), (96, 0o057147, 0o057162),
    (97, 0o057175, 0o057210), (98, 0o057223, 0o057236),
    (99, 0o057251, 0o057264),
]
_T_LIST = [
    (100, 0o057277, 0o057312), (101, 0o057325, 0o057340),
    (102, 0o057353, 0o057366), (103, 0o057401, 0o057414),
    (104, 0o057427, 0o057442), (105, 0o057455, 0o057470),
    (106, 0o057503, 0o057516), (107, 0o057531, 0o057544),
    (108, 0o057557, 0o057572), (109, 0o057605, 0o057620),
    (110, 0o057633, 0o057646), (111, 0o057661, 0o057674),
    (112, 0o057707, 0o057722), (113, 0o057735, 0o057750),
    (114, 0o057763, 0o057776), (115, 0o060011, 0o060024),
    (116, 0o060037, 0o060052), (117, 0o060065, 0o060100),
    (118, 0o060113, 0o060126), (119, 0o060141, 0o060154),
    (120, 0o060167, 0o060202), (121, 0o060215, 0o060230),
    (122, 0o060243, 0o060256), (123, 0o060271, 0o060304),
    (124, 0o060317, 0o060332), (125, 0o060345, 0o060360),
    (126, 0o060373, 0o060406), (127, 0o060421, 0o060434),
    (128, 0o060447, 0o060462), (129, 0o060475, 0o060510),
    (130, 0o060523, 0o060536), (131, 0o060551, 0o060564),
    (132, 0o060577, 0o060612), (133, 0o060625, 0o060640),
    (134, 0o060653, 0o060666), (135, 0o060701, 0o060714),
    (136, 0o060727, 0o060742), (137, 0o060755, 0o060770),
    (138, 0o061003, 0o061016), (139, 0o061031, 0o061044),
    (140, 0o061057, 0o061072),
]
for _devno, _raddr, _waddr in _DT_LIST:
    _label = f"DT{_devno:02d}"
    DT_SYMBOLS[_raddr] = f"{_label}R"
    DT_SYMBOLS[_waddr] = f"{_label}W"
for _devno, _raddr, _waddr in _T_LIST:
    DT_SYMBOLS[_raddr] = f"T{_devno}R"
    DT_SYMBOLS[_waddr] = f"T{_devno}W"

# Block Device Symbols (BD01-BD50)
BD_SYMBOLS = {}
_BD_LIST = [
    (1, 0o061207, 0o061222), (2, 0o061235, 0o061250),
    (3, 0o061263, 0o061276), (4, 0o061311, 0o061324),
    (5, 0o061337, 0o061352), (6, 0o061365, 0o061400),
    (7, 0o061413, 0o061426), (8, 0o061441, 0o061454),
    (9, 0o061467, 0o061502), (10, 0o061515, 0o061530),
    (11, 0o061543, 0o061556), (12, 0o061571, 0o061604),
    (13, 0o061617, 0o061632), (14, 0o061645, 0o061660),
    (15, 0o061673, 0o061706), (16, 0o061721, 0o061734),
    (17, 0o061747, 0o061762), (18, 0o061775, 0o062010),
    (19, 0o062023, 0o062036), (20, 0o062051, 0o062064),
    (21, 0o062077, 0o062112), (22, 0o062125, 0o062140),
    (23, 0o062153, 0o062166), (24, 0o062201, 0o062214),
    (25, 0o062227, 0o062242), (26, 0o062255, 0o062270),
    (27, 0o062303, 0o062316), (28, 0o062331, 0o062344),
    (29, 0o062357, 0o062372), (30, 0o062405, 0o062420),
    (31, 0o062433, 0o062446), (32, 0o062461, 0o062474),
    (33, 0o062507, 0o062522), (34, 0o062535, 0o062550),
    (35, 0o062563, 0o062576), (36, 0o062611, 0o062624),
    (37, 0o062637, 0o062652), (38, 0o062665, 0o062700),
    (39, 0o062713, 0o062726), (40, 0o062741, 0o062754),
    (41, 0o062767, 0o063002), (42, 0o063015, 0o063030),
    (43, 0o063043, 0o063056), (44, 0o063071, 0o063104),
    (45, 0o063117, 0o063132), (46, 0o063145, 0o063160),
    (47, 0o063173, 0o063206), (48, 0o063221, 0o063234),
    (49, 0o063247, 0o063262), (50, 0o063275, 0o063310),
]
for _devno, _raddr, _waddr in _BD_LIST:
    BD_SYMBOLS[_raddr] = f"BD{_devno:02d}R"
    BD_SYMBOLS[_waddr] = f"BD{_devno:02d}W"

# CDF Channel Datafields
CDF_SYMBOLS = {
    0o064566: 'CDF01', 0o064601: 'CDF02', 0o064614: 'CDF03', 0o064627: 'CDF04',
    0o064642: 'CDF05', 0o064655: 'CDF06', 0o064670: 'CDF07', 0o064703: 'CDF08',
    0o064716: 'CDF09', 0o064731: 'CDF10', 0o064744: 'CDF11', 0o064757: 'CDF12',
    0o064772: 'CDF13', 0o065005: 'CDF14', 0o065020: 'CDF15', 0o065033: 'CDF16',
}


# =============================================================================
# MASTER SYMBOL LOOKUP: address -> (name, category)
# =============================================================================

DEVICE_LOOKUP = {}

def _register(symbols, category):
    for addr, name in symbols.items():
        DEVICE_LOOKUP[addr] = (name, category)

_register(DISK_SYMBOLS, 'Disk')
_register(SCSI_DISK_SYMBOLS, 'SCSI-Disk')
_register(DOMAIN_SYMBOLS, 'Domain')
_register(MAGTAPE_SYMBOLS, 'MagTape')
_register(SCSI_CTRL_SYMBOLS, 'SCSI-Ctrl')
_register(SCSI_STATUS_SYMBOLS, 'SCSI-Stat')
_register(FLOPPY_SYMBOLS, 'Floppy')
_register(ND500_SYMBOLS, 'ND-500')
_register(HDLC_SYMBOLS, 'HDLC')
_register(MNDF_SYMBOLS, 'MultiNet')
_register(VECTOR_SYMBOLS, 'Vector')
_register(SCSI_BOOT_SYMBOLS, 'SCSI-Boot')
_register(DT_SYMBOLS, 'CharDev')
_register(BD_SYMBOLS, 'BlockDev')
_register(CDF_SYMBOLS, 'CDF')


# =============================================================================
# RT PROGRAM NAME LOOKUP (from SYMBOL-2-LIST L07)
# =============================================================================

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
}

# Add background programs (BAK01-BAK07 known, rest computed)
_bak_addr = 0o023337
for _i in range(1, 60):
    _addr = _bak_addr + (_i - 1) * RT_SIZE
    if _addr > BP_9LBPR:
        break
    if _i <= 7:
        RT_NAMES[_addr] = f'BAK{_i:02d}'
    else:
        # BCH and other background programs - compute address
        RT_NAMES[_addr] = f'BG{_i:02d}'


# =============================================================================
# MEMORY ACCESS
# =============================================================================

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
        phys = ppn * 1024 + dip
        if phys >= self.word_count:
            return None
        return phys

    def read_dpit_word(self, logical_addr):
        phys = self.translate_dpit(logical_addr)
        if phys is None:
            return None
        return self.read_phys_word(phys)


def ofmt(v):
    """Format value as 6-digit octal, or '------' for None."""
    if v is None:
        return "------"
    return f"{v:06o}"


# =============================================================================
# DEVICE DATA READING
# =============================================================================

def read_device_fields(mem, addr):
    """Read standard I/O datafield fields from a DPIT logical address."""
    fields = {}
    for name, offset in [('RESLI', RESLI), ('RTRES', RTRES), ('BWLIN', BWLIN),
                          ('TYPRI', TYPRI), ('ISTAT', ISTAT), ('MLINK', MLINK),
                          ('MFUNC', MFUNC)]:
        fields[name] = mem.read_dpit_word(addr + offset)
    return fields


def is_readable(fields):
    """Check if all fields were successfully read from memory."""
    for v in fields.values():
        if v is None:
            return False
    return True


def is_initialized(fields):
    """Check if any field is non-zero (device has been set up)."""
    for v in fields.values():
        if v is not None and v != 0:
            return True
    return False


def resolve_rt_name(addr):
    """Resolve an RT-Description address to a name."""
    if addr is None or addr == 0:
        return None
    name = RT_NAMES.get(addr)
    if name:
        return name
    # Try to compute from RT table base (system RT programs)
    if 0o012071 <= addr < 0o023337:
        slot = (addr - 0o012071) // RT_SIZE
        remainder = (addr - 0o012071) % RT_SIZE
        if remainder == 0:
            return f"RT{slot:03d}"
    # Background program range
    if BP_9FBPR <= addr <= BP_9LBPR:
        slot = (addr - BP_9FBPR) // RT_SIZE
        remainder = (addr - BP_9FBPR) % RT_SIZE
        if remainder == 0:
            return f"BG{slot + 1:02d}"
    return f"@{addr:06o}"


def resolve_device_name(addr):
    """Resolve a device datafield address to a symbol name."""
    entry = DEVICE_LOOKUP.get(addr)
    if entry:
        return entry[0]
    return None


# =============================================================================
# DEVICE FILTERING (from SINTRAN-STRUCTURES.md Section 18)
# =============================================================================

# Filter levels:
#   0 = all         Show everything found by discovery
#   1 = configured  MFUNC != 0 OR TYPRI != 0
#   2 = initialized MFUNC != 0 AND 5ISET (bit 12) set in TYPRI
#   3 = active      MFUNC != 0 AND RTRES != 0
FILTER_ALL = 0
FILTER_CONFIGURED = 1
FILTER_INITIALIZED = 2
FILTER_ACTIVE = 3

FILTER_NAMES = {
    FILTER_ALL: "all",
    FILTER_CONFIGURED: "configured",
    FILTER_INITIALIZED: "initialized",
    FILTER_ACTIVE: "active",
}


def check_false_positive(addr, fields, rt_start, rt_end):
    """Check if a discovered device entry is likely a false positive.

    Returns (is_valid, reason) tuple.
    Source: SINTRAN-STRUCTURES.md Section 18.5
    """
    bwlin = fields.get('BWLIN', 0) or 0
    rtres = fields.get('RTRES', 0) or 0
    istat = fields.get('ISTAT', 0) or 0
    mfunc = fields.get('MFUNC', 0) or 0
    mlink = fields.get('MLINK', 0) or 0

    # NOTE: BWLIN=self (pointing to device's own address) is a NORMAL
    # SINTRAN initialization pattern meaning "no one waiting". It is NOT
    # a false positive indicator. Many valid devices have BWLIN=self.

    # RTRES should point to an RT-Description if non-zero
    if rtres != 0:
        in_system_rt = rt_start <= rtres < rt_end
        in_bg_rt = BP_9FBPR <= rtres <= BP_9LBPR
        if not in_system_rt and not in_bg_rt:
            # RTRES points outside all RT ranges — suspicious
            # Could be a pointer table entry, not a device
            return False, f"RTRES={rtres:06o} outside RT ranges"

    # Sequential small values in MFUNC/ISTAT/BWLIN suggest a data table
    if (mfunc != 0 and istat != 0 and bwlin != 0
            and abs(mfunc - istat) == 1 and abs(istat - bwlin) == 1):
        return False, "sequential values (data table)"

    # Known boundary markers that are NOT devices
    BOUNDARY_ADDRS = {
        REGION_9EDFD,   # SCODE — boundary between SCSI disk and domain
        REGION_9EEHD,   # DEMFI — end of controller region
        REGION_9BBHD,   # 9FDFD — start of controller region
        REGION_9ESTR,   # End of disk sorting range
    }
    if addr in BOUNDARY_ADDRS:
        return False, "boundary marker"

    return True, ""


def passes_filter(fields, filter_level, addr=0):
    """Check if a device passes the given filter level.

    Filter levels (from SINTRAN-STRUCTURES.md Section 18.2):
      0 = all         Everything
      1 = configured  MFUNC != 0 OR TYPRI != 0
      2 = initialized MFUNC != 0 AND 5ISET (bit 12) in TYPRI
      3 = active      MFUNC != 0 AND RTRES != 0
    """
    if filter_level == FILTER_ALL:
        return True

    mfunc = fields.get('MFUNC', 0) or 0
    typri = fields.get('TYPRI', 0) or 0
    rtres = fields.get('RTRES', 0) or 0

    if filter_level == FILTER_CONFIGURED:
        return mfunc != 0 or typri != 0

    if filter_level == FILTER_INITIALIZED:
        if mfunc == 0:
            return False
        # 5ISET = bit 12
        has_iset = bool(typri & (1 << BIT_5ISET))
        if has_iset:
            return True
        # Exceptions: some device categories don't use ISET but are still valid
        # if they have a driver connected (MFUNC != 0):
        #   - Disk controller DFs (classified by address range, TYPRI may be 0)
        #   - Domain entries (TYPRI=000002, no ISET)
        #   - CDF channels (TYPRI=020000, no ISET)
        #   - SCSI disk DFs (TYPRI=000002, no ISET)
        if REGION_9BBHD <= addr < REGION_9EEHD:
            return True   # Controller region — classified by address, not TYPRI
        # Check for non-ISET categories that still have TYPRI set
        if typri != 0:
            # Has type bits but no ISET — include if MFUNC is connected
            # (covers DOM, CDF, SCSI-disk, etc.)
            return True
        return False

    if filter_level == FILTER_ACTIVE:
        return mfunc != 0 and rtres != 0

    return True


def classify_device(addr, fields):
    """Classify device type from address range and TYPRI bits."""
    # First check symbol lookup
    entry = DEVICE_LOOKUP.get(addr)
    if entry:
        return entry[1]

    # Classify by address range
    if REGION_9BBHD <= addr < REGION_9ESTR:
        return "Disk/Sort"
    if REGION_9ESTR <= addr < REGION_9EDFD:
        return "Disk/SCSI"
    if REGION_9EDFD <= addr < REGION_9EEHD:
        return "Domain"
    if DT01R_ADDR <= addr <= T140W_ADDR + DT_HALF_SIZE:
        return "CharDev"
    if BD01R_ADDR <= addr <= BD50W_ADDR + BD_HALF_SIZE:
        return "BlockDev"

    # Fall back to TYPRI decoding
    typri = fields.get('TYPRI', 0) or 0
    if typri & (1 << BIT_5TERM):
        return "Terminal"
    if typri & (1 << BIT_5BAD):
        return "TAD"
    if typri & (1 << BIT_5FLOP):
        return "Floppy"
    if typri & (1 << BIT_5MT):
        return "MagTape"
    return "Unknown"


def is_in_rt_range(addr, rt_start, rt_end):
    """Check if address falls within the RT program table or background table."""
    if rt_start <= addr <= rt_end:
        return True
    if BP_9FBPR <= addr <= BP_9LBPR:
        return True
    return False


# =============================================================================
# PHASE 1: VALIDATE ALL KNOWN SYMBOL ADDRESSES
# =============================================================================

def phase1_known_symbols(mem, devices):
    """Read and validate all named device datafield addresses from symbol tables."""
    print("\n  Phase 1: Validating known symbol addresses")

    accessible = 0
    initialized = 0
    inaccessible = 0

    for addr in sorted(DEVICE_LOOKUP.keys()):
        name, category = DEVICE_LOOKUP[addr]
        fields = read_device_fields(mem, addr)

        if not is_readable(fields):
            inaccessible += 1
            continue
        accessible += 1

        if is_initialized(fields):
            initialized += 1

        if addr not in devices:
            devices[addr] = {
                'fields': fields,
                'source': 'symbol',
                'name': name,
                'category': category,
            }

    print(f"    Known symbols:  {len(DEVICE_LOOKUP)}")
    print(f"    Accessible:     {accessible}")
    print(f"    Initialized:    {initialized}")
    print(f"    Inaccessible:   {inaccessible}")


# =============================================================================
# PHASE 2: SCAN CONTIGUOUS DEVICE ARRAYS
# =============================================================================

def scan_array(mem, devices, start_addr, step, max_addr, source_tag):
    """Scan a contiguous array of device datafields.

    Stops at max_addr or when hitting unmapped memory.
    Returns (accessible_count, initialized_count).
    """
    addr = start_addr
    accessible = 0
    initialized = 0

    while addr <= max_addr:
        fields = read_device_fields(mem, addr)
        if not is_readable(fields):
            break  # Hit unmapped memory
        accessible += 1

        if is_initialized(fields):
            initialized += 1

        if addr not in devices:
            name = resolve_device_name(addr)
            category = classify_device(addr, fields)
            devices[addr] = {
                'fields': fields,
                'source': source_tag,
                'name': name or f"?@{addr:06o}",
                'category': category,
            }

        addr += step

    return accessible, initialized


def phase2_array_scan(mem, devices):
    """Scan contiguous device arrays with known step sizes and boundaries."""
    print("\n  Phase 2: Scanning contiguous device arrays")

    # 2a: Character terminals DT01R through T140W
    dt_acc, dt_init = scan_array(
        mem, devices, DT01R_ADDR, DT_HALF_SIZE,
        T140W_ADDR, 'scan:terminal')
    print(f"    Terminals (DT01R..T140W):  {dt_acc} accessible, {dt_init} initialized")

    # 2b: Block devices BD01R through BD50W
    bd_acc, bd_init = scan_array(
        mem, devices, BD01R_ADDR, BD_HALF_SIZE,
        BD50W_ADDR, 'scan:blockdev')
    print(f"    Block devs (BD01R..BD50W): {bd_acc} accessible, {bd_init} initialized")

    # 2c: CDF channels CDF01 through CDF16
    cdf_acc, cdf_init = scan_array(
        mem, devices, 0o064566, CDF_SIZE,
        0o065033, 'scan:cdf')
    print(f"    CDF channels (01..16):     {cdf_acc} accessible, {cdf_init} initialized")


# =============================================================================
# PHASE 3: WALK CONTROLLER REGION WITH SUB-RANGE AWARENESS
# =============================================================================

def phase3_controller_region(mem, devices):
    """Walk the controller region 9BBHD..9EEHD checking at known datafield starts.

    Instead of scanning word-by-word, we check at known datafield addresses
    and also at 13₈-word intervals within known sub-ranges.
    """
    print("\n  Phase 3: Controller region scan (9BBHD..9EEHD)")
    print(f"    Range: {ofmt(REGION_9BBHD)} to {ofmt(REGION_9EEHD)}")

    # Known datafield sub-ranges within the controller region
    # Each defines: (start, end, step, type_label)
    sub_ranges = [
        # Disk 1 datafields (D1DF0-D1DF3 + controller state)
        (0o031631, 0o031672, DISK_DF_SIZE, 'Disk1'),
        # Disk 2 datafields
        (0o032075, 0o032136, DISK_DF_SIZE, 'Disk2'),
        # Disk 3 datafields
        (0o032341, 0o032402, DISK_DF_SIZE, 'Disk3'),
        # Disk 4 datafields
        (0o032605, 0o032646, DISK_DF_SIZE, 'Disk4'),
        # Winchester 1 datafields
        (0o033051, 0o033064, DISK_DF_SIZE, 'Win1'),
        # Winchester 2 datafields
        (0o033267, 0o033302, DISK_DF_SIZE, 'Win2'),
        # SCSI disk input datafields (SCDI1-SCDI8)
        (0o036442, 0o040267, 0o203, 'SCSI-In'),  # Step from SCDI1 to SCDI2
        # SCSI disk output datafields (SCOD1-SCOD2)
        (0o040472, 0o040675, 0o203, 'SCSI-Out'),
        # Domain datafields (DOM01-DOM20, 37₈ step)
        (0o041340, 0o042261, 0o37, 'Domain'),
    ]

    new_devices = 0
    for start, end, step, label in sub_ranges:
        addr = start
        while addr <= end:
            if addr not in devices:
                fields = read_device_fields(mem, addr)
                if is_readable(fields) and is_initialized(fields):
                    name = resolve_device_name(addr)
                    devices[addr] = {
                        'fields': fields,
                        'source': f'region:{label}',
                        'name': name or f"?@{addr:06o}",
                        'category': label,
                    }
                    new_devices += 1
            addr += step

    print(f"    New devices from region scan: {new_devices}")


# =============================================================================
# PHASE 4: CHAIN WALKING (BRESL + MQUEU)
# =============================================================================

def phase4_bresl_chains(mem, devices, rt_start, rt_end):
    """Walk BRESL reservation chains from all RT programs."""
    print("\n  Phase 4a: Walking RT BRESL reservation chains")
    print(f"    RT range: {ofmt(rt_start)} to {ofmt(rt_end)}")

    rt_scanned = 0
    rt_with_reservations = 0
    new_devices = 0
    skipped_rt_addrs = 0

    # Walk both system RT programs and background programs
    ranges = [
        (rt_start, rt_end, "system"),
        (BP_9FBPR, BP_9LBPR, "background"),
    ]

    for range_start, range_end, range_label in ranges:
        addr = range_start
        while addr <= range_end:
            statu = mem.read_dpit_word(addr + STATU)
            if statu is None or statu == 0:
                addr += RT_SIZE
                continue

            rt_scanned += 1
            bresl = mem.read_dpit_word(addr + BRESL)
            if bresl is None or bresl == 0:
                addr += RT_SIZE
                continue

            rt_with_reservations += 1
            rt_name = resolve_rt_name(addr)

            # Follow RESLI chain
            chain_addr = bresl
            visited = set()
            chain_len = 0

            while chain_addr and chain_addr != 0 and chain_len < 30:
                if chain_addr in visited:
                    break
                visited.add(chain_addr)

                # CRITICAL: skip if address is an RT-Description
                if is_in_rt_range(chain_addr, rt_start, rt_end):
                    skipped_rt_addrs += 1
                    break

                fields = read_device_fields(mem, chain_addr)
                if not is_readable(fields):
                    break

                if chain_addr not in devices:
                    name = resolve_device_name(chain_addr)
                    category = classify_device(chain_addr, fields)
                    devices[chain_addr] = {
                        'fields': fields,
                        'source': f'bresl:{rt_name}',
                        'name': name or f"?@{chain_addr:06o}",
                        'category': category,
                    }
                    new_devices += 1
                else:
                    # Add this RT as additional reserver
                    existing = devices[chain_addr]
                    if rt_name and rt_name not in existing['source']:
                        existing['source'] += f'+{rt_name}'

                chain_addr = fields['RESLI']
                chain_len += 1

            addr += RT_SIZE

    print(f"    RT programs scanned:       {rt_scanned}")
    print(f"    RT with reservations:      {rt_with_reservations}")
    print(f"    New devices from BRESL:    {new_devices}")
    print(f"    Skipped RT addrs in chain: {skipped_rt_addrs}")


def phase4_mqueu_chain(mem, devices):
    """Walk the monitor queue (MQUEU) chain via MLINK."""
    print("\n  Phase 4b: Walking MQUEU monitor queue chain")

    mqueu_head = mem.read_dpit_word(MQUEU_ADDR)
    print(f"    MQUEU head: {ofmt(mqueu_head)}")

    if mqueu_head is None or mqueu_head == 0 or mqueu_head == 0o177777:
        print("    Monitor queue is empty (head=0 or 177777)")
        return

    addr = mqueu_head
    visited = set()
    new_devices = 0

    while addr and addr != 0 and addr != 0o177777 and len(visited) < 50:
        if addr in visited:
            print(f"    CYCLE at {ofmt(addr)}")
            break
        visited.add(addr)

        fields = read_device_fields(mem, addr)
        if not is_readable(fields):
            break

        if addr not in devices:
            name = resolve_device_name(addr)
            category = classify_device(addr, fields)
            devices[addr] = {
                'fields': fields,
                'source': 'mqueu',
                'name': name or f"?@{addr:06o}",
                'category': category,
            }
            new_devices += 1

        addr = fields['MLINK']

    print(f"    Devices in monitor queue: {len(visited)}")
    print(f"    New devices from MQUEU:  {new_devices}")


# =============================================================================
# REPORT GENERATION
# =============================================================================

def print_report(devices, filter_level, rt_start, rt_end):
    """Print detailed and summary reports of all discovered devices.

    Applies filter level and false positive detection per
    SINTRAN-STRUCTURES.md Section 18.
    """
    filter_name = FILTER_NAMES.get(filter_level, "?")

    # ─── Pre-pass: classify, filter, detect false positives ───
    display_devices = []
    false_positives = []
    filtered_out = []

    # Track statistics across ALL devices (pre-filter)
    total = 0
    total_false_pos = 0
    # Per-level counts (computed on valid devices only)
    count_configured = 0    # Level 1: MFUNC!=0 OR TYPRI!=0
    count_initialized = 0   # Level 2: passes_filter(..., FILTER_INITIALIZED)
    count_active = 0        # Level 3: MFUNC!=0 AND RTRES!=0
    by_category_all = {}
    by_source_all = {}

    for addr in sorted(devices.keys()):
        dev = devices[addr]
        fields = dev['fields']
        total += 1

        category = dev.get('category', '?')
        src_key = dev['source'].split(':')[0]
        by_category_all[category] = by_category_all.get(category, 0) + 1
        by_source_all[src_key] = by_source_all.get(src_key, 0) + 1

        # False positive check
        is_valid, fp_reason = check_false_positive(addr, fields, rt_start, rt_end)
        if not is_valid:
            total_false_pos += 1
            false_positives.append((addr, dev, fp_reason))
            continue

        # Count per-level totals (pre-filter, excluding false positives)
        if passes_filter(fields, FILTER_CONFIGURED, addr):
            count_configured += 1
        if passes_filter(fields, FILTER_INITIALIZED, addr):
            count_initialized += 1
        if passes_filter(fields, FILTER_ACTIVE, addr):
            count_active += 1

        # Filter check
        if not passes_filter(fields, filter_level, addr):
            filtered_out.append((addr, dev))
            continue

        display_devices.append((addr, dev))

    # ─── Detailed listing ───
    print("\n" + "=" * 140)
    print(f"  DISCOVERED DEVICES — filter: {filter_name.upper()}"
          f"  ({len(display_devices)} shown, {len(filtered_out)} filtered,"
          f" {len(false_positives)} false positives)")
    print("=" * 140)

    hdr = (f"  {'Address':>8s} {'Name':<8s} {'Category':<11s} "
           f"{'Type':<5s} {'Attributes':<20s} "
           f"{'MFUNC':>7s} {'Owner':<8s} {'ISTAT':>7s} {'BWLIN':>7s} "
           f"{'Source'}")
    print(hdr)
    print("  " + "-" * 138)

    by_category_shown = {}
    for addr, dev in display_devices:
        fields = dev['fields']
        name = dev.get('name', '?')
        category = dev.get('category', '?')
        source = dev['source']

        by_category_shown[category] = by_category_shown.get(category, 0) + 1

        # Owner
        rtres = fields.get('RTRES', 0) or 0
        owner = "-"
        if rtres != 0:
            owner = resolve_rt_name(rtres)

        # TYPRI decode — separate type and attributes
        typri = fields.get('TYPRI')
        type_str = decode_typri_type(typri)
        attr_str = decode_typri_attrs(typri)

        print(f"  {ofmt(addr):>8s} {name:<8s} {category:<11s} "
              f"{type_str:<5s} {attr_str:<20s} "
              f"{ofmt(fields.get('MFUNC')):>7s} {owner:<8s} "
              f"{ofmt(fields.get('ISTAT')):>7s} {ofmt(fields.get('BWLIN')):>7s} "
              f"{source}")

    # ─── False positives ───
    if false_positives:
        print(f"\n  FALSE POSITIVES EXCLUDED ({len(false_positives)}):")
        for addr, dev, reason in false_positives:
            name = dev.get('name', '?')
            print(f"    {ofmt(addr)} {name:<8s} — {reason}")

    # ─── Summary ───
    print("\n" + "=" * 140)
    print("  SUMMARY")
    print("=" * 140)
    print(f"  Total known symbol addresses:   {len(DEVICE_LOOKUP)}")
    print(f"  Total discovered (all phases):  {total}")
    print(f"  False positives excluded:       {total_false_pos}")
    print()
    print(f"  Filter level counts (pre-filter, excluding false positives):")
    valid_total = total - total_false_pos
    print(f"    Level 0 -- All:                {valid_total}")
    print(f"    Level 1 -- Configured:         {count_configured}"
          f"  (MFUNC!=0 OR TYPRI!=0)")
    print(f"    Level 2 -- Initialized:        {count_initialized}"
          f"  (MFUNC!=0 AND 5ISET)")
    print(f"    Level 3 -- Active:             {count_active}"
          f"  (MFUNC!=0 AND RTRES!=0)")
    print()
    print(f"  Current filter: {filter_name.upper()}"
          f"  -> showing {len(display_devices)} devices")
    print()
    print("  By category (shown):")
    for cat in sorted(by_category_shown.keys()):
        print(f"    {cat:<16s}: {by_category_shown[cat]:4d}")
    print()
    print("  By category (all discovered):")
    for cat in sorted(by_category_all.keys()):
        print(f"    {cat:<16s}: {by_category_all[cat]:4d}")
    print()
    print("  By discovery source (all):")
    for src in sorted(by_source_all.keys()):
        print(f"    {src:<16s}: {by_source_all[src]:4d}")


# =============================================================================
# MAIN
# =============================================================================

def parse_args():
    parser = argparse.ArgumentParser(
        description="Discover I/O devices from SINTRAN III physical memory dump")
    parser.add_argument(
        "dump_file", nargs="?",
        default=r"C:\Users\ronny\Downloads\nd100_physmem_256k.bin",
        help="Path to physical memory dump file")
    parser.add_argument(
        "--filter", "-f", dest="filter_level",
        choices=["all", "configured", "initialized", "active"],
        default="all",
        help="Filter level: all, configured, initialized, active (default: all)")
    return parser.parse_args()


def main():
    args = parse_args()

    filter_map = {
        "all": FILTER_ALL,
        "configured": FILTER_CONFIGURED,
        "initialized": FILTER_INITIALIZED,
        "active": FILTER_ACTIVE,
    }
    filter_level = filter_map[args.filter_level]

    mem = MemoryDump(args.dump_file)
    print(f"Dump: {mem.word_count} words ({mem.word_count // 1024}KW)")
    print(f"Filter: {args.filter_level.upper()}")

    # Read RT table boundaries
    rt_start = mem.read_dpit_word(RTSTA_ADDR)
    rt_end = mem.read_dpit_word(RTEND_ADDR)
    print(f"RT table: {ofmt(rt_start)} to {ofmt(rt_end)}")

    # Collect all discovered devices (keyed by datafield address)
    devices = {}

    print("\n" + "=" * 140)
    print("  AUTOMATIC I/O DEVICE DISCOVERY (L07)")
    print("=" * 140)

    phase1_known_symbols(mem, devices)
    phase2_array_scan(mem, devices)
    phase3_controller_region(mem, devices)
    phase4_bresl_chains(mem, devices, rt_start, rt_end)
    phase4_mqueu_chain(mem, devices)

    print_report(devices, filter_level, rt_start, rt_end)
    print("\nDone.")


if __name__ == '__main__':
    main()
