#!/usr/bin/env python3
"""
mstyp_swtyp_join.py -- Reproduce and re-verify the MACM MSTYP <-> SINTRAN SWTYP bridge.

Background
----------
MACM (the SINTRAN system-generation program, D:\\ND\\BPUN\\MACM-1718L.BPUN) turns
the operator's octal DISK TYPE menu answer into TWO numbers, via the two-word
table at Ghidra address ram:9483 (loaded here as MACM_9483_TABLE):

    word0 -> MSTYP          (ram:8342)  small class index; picks device no. + library mark
    word1 -> disc-type code (ram:833b)  == the kernel's SWTYP (== DISPN geometry index)

The running kernel selects disc support with SWTYP (valid range 7..36B), which
indexes DISPE (geometry, the DTxxx records) and MDISCS (driver). This script
holds both byte-verified tables and prints the join, demonstrating the identity

    SWTYP(octal value)  ==  disc-type-code(decimal value)

for every disc type the L07 kernel supports, and flags the legacy discs whose
code falls below the SWTYP>=7 floor (DISC-33MB / DISC-66MB), which no carved
kernel supports.

All data below is [VERIFIED] from binaries:
  * MACM_9483_TABLE  -- hexdump of ram:9483 in MACM-1718L.BPUN (this session).
  * DISPN_BY_DTNAME  -- carved DISPE/DTxxx records, CARVED-DISC-SUPPORT.md sec.1.3.
No manual was consulted. This script does not read any binary; it is a pure
re-derivation harness so the bridge can be regression-checked if either table is
re-carved. Run:  python mstyp_swtyp_join.py
"""

# MACM ram:9483 table: indexed by octal DISK TYPE menu answer 0..24B.
# (menu_octal, disc_name, MSTYP_octal, disc_type_code_decimal)
# Bytes: 0008 0008 / 0008 0009 / 000a 000a / ... / 0013 001e   (SCSI last).
MACM_9483_TABLE = [
    (0o0,  "DISC-14MB",    0o10, 8),
    (0o1,  "DISC-21MB",    0o10, 9),
    (0o2,  "DISC-23MB",    0o12, 10),
    (0o3,  "DISC-28MB",    0o14, 12),
    (0o4,  "DISC-30MB",    0o6,  16),
    (0o5,  "DISC-33MB",    0o3,  2),   # legacy CDC SMD -- code below SWTYP floor
    (0o6,  "DISC-38MB",    0o4,  17),
    (0o7,  "DISC-45MB",    0o11, 11),
    (0o10, "DISC-66MB",    0o3,  3),   # legacy CDC SMD -- code below SWTYP floor
    (0o11, "DISC-70MB",    0o4,  18),
    (0o12, "DISC-74MB",    0o13, 13),
    (0o13, "DISC-75MB",    0o4,  19),
    (0o14, "DISC-140MB",   0o15, 20),
    (0o15, "DISC-2-75MB",  0o7,  22),
    (0o16, "DISC-288MB-R", 0o5,  23),
    (0o17, "DISC-288MB-F", 0o16, 25),
    (0o20, "DISC-450MB-F", 0o20, 26),
    (0o21, "DISC-288MB-E", 0o17, 15),
    (0o22, "DISC-450MB-N", 0o21, 28),
    (0o23, "DISC-288MB-N", 0o22, 29),
    (0o24, "SCSI",         0o23, 30),
]

# MACM MSTYP -> (device number octal, library mark).  CARVED join keys; from
# MACM-DIALOGUE.md sec.6.6.  MSTYP 1 is the rejected placeholder (omitted).
MSTYP_DEVNO_MARK = {
    0o0:  (0o540,    "DRUM"),
    0o2:  (0o500,    "REMOV/FIXED"),
    0o3:  (0o1540,   "BD288"),
    0o4:  (0o1540,   "BD288"),
    0o5:  (0o1540,   "BD288"),
    0o6:  (0o1540,   "BD288/BDFIX"),
    0o7:  (0o1540,   "BD288"),
    0o10: (0o500,    "W8INC"),
    0o11: (0o500,    "W8INC"),
    0o12: (0o500,    "W8INC"),
    0o13: (0o500,    "W8INC"),
    0o14: (0o500,    "W8INC"),
    0o15: (0o1540,   "BD288"),
    0o16: (0o1540,   "BD288"),
    0o17: (0o1540,   "BD288"),
    0o20: (0o1540,   "BD288"),
    0o21: (0o1540,   "BD288"),
    0o22: (0o1540,   "BD288"),
    0o23: (0o144300, "SCASI"),
}

# Carved kernel DISPE: DISPN (octal) -> DTxxx name.  CARVED-DISC-SUPPORT.md sec.1.3
# (L07/M06).  DISPN == the DISPE index == SWTYP.  In K05, DISPN 10..15B are absent.
DISPN_TO_DTNAME = {
    0o10: "DT014", 0o11: "DT021", 0o12: "DT023", 0o13: "DT045",
    0o14: "DT028", 0o15: "DT074", 0o17: "DT310", 0o20: "DT030",
    0o21: "DT037", 0o22: "DT070", 0o23: "DT075", 0o24: "DT140",
    0o25: "DT135", 0o26: "DT160", 0o27: "DT288", 0o30: "DT285",
    0o31: "DT300", 0o32: "DT450", 0o33: "DT460", 0o34: "DT470",
    0o35: "DT290", 0o36: "DTSSS", 0o40: "DTOD1", 0o41: "DTOD2",
}
K05_MISSING_DISPN = {0o10, 0o11, 0o12, 0o13, 0o14, 0o15}  # ST-506/Winchester group
SWTYP_FLOOR = 7   # PH-P2-OPPSTART.NPL:722  IF SWTYP<<7 OR>>36 THEN ERRFATAL
SWTYP_CEIL = 0o36


def main():
    hdr = ("menu", "disc", "MSTYP", "dev", "mark", "code(dec)",
           "SWTYP(oct)", "DTxxx", "K05", "L07/M06")
    print("%-4s %-13s %-5s %-7s %-11s %-9s %-10s %-6s %-4s %-7s" % hdr)
    print("-" * 92)
    mismatches = 0
    for menu, disc, mstyp, code in MACM_9483_TABLE:
        devno, mark = MSTYP_DEVNO_MARK.get(mstyp, (0, "?"))
        swtyp_oct = code                      # SWTYP octal value == code decimal value
        dtname = DISPN_TO_DTNAME.get(swtyp_oct, "none")
        # Verify the identity: is there a DISPE record at this SWTYP, and is it in range?
        in_range = SWTYP_FLOOR <= swtyp_oct <= SWTYP_CEIL
        l_ok = "yes" if (in_range and dtname != "none") else "NO"
        k_ok = "yes" if (l_ok == "yes" and swtyp_oct not in K05_MISSING_DISPN) else "no"
        if dtname == "none" and in_range:
            # code within range but no record -> would break the identity claim
            mismatches += 1
        print("%-4s %-13s %-5s %-7s %-11s %-9d %-10s %-6s %-4s %-7s" % (
            oct(menu)[2:], disc, oct(mstyp)[2:], oct(devno)[2:], mark,
            code, oct(swtyp_oct)[2:], dtname, k_ok, l_ok))
    print("-" * 92)
    print("Legacy discs below the SWTYP>=7 floor (unsupported by any carved kernel):")
    for menu, disc, mstyp, code in MACM_9483_TABLE:
        if code < SWTYP_FLOOR:
            print("    menu %sB  %-12s  code=%d  -> no DTxxx, ERRFATAL at cold start"
                  % (oct(menu)[2:], disc, code))
    # The bridge holds iff every in-range code maps to a real DISPE record.
    print("\nIdentity check (SWTYP octal == disc-type code decimal, for codes 7..36B):")
    print("    in-range codes with NO DISPE record: %d  (expected 0)" % mismatches)
    print("    result:", "OK -- bridge holds" if mismatches == 0 else "FAIL")


if __name__ == "__main__":
    main()
