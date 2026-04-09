"""Generate logical device number JSON lookup from Appendix B data."""
import json, sys

D = {}

def a(n, desc, grp, cat="system"):
    D[format(n, 'o')] = {"desc": desc, "group": grp, "cat": cat}

def ar(start, count, fn, grp, cat="system"):
    for i in range(count):
        a(start + i, fn(i), grp, cat)

def unused(start, count, grp):
    ar(start, count, lambda i: "Not used", grp)

# === Group 0: DV000 (0-77) ===
G = "DV000"
a(0o0, "INBT/INCH: edited input (background) / dummy", G, "character")
a(0o1, "Terminal 1 (console) / background own terminal", G, "terminal")
a(0o2, "Error device (output) / Paper tape reader 1 (input)", G, "character")
a(0o3, "Paper tape punch 1", G, "character")
a(0o4, "Card reader 1", G, "character")
a(0o5, "Line printer 1", G, "printer")
a(0o6, "Synchronous modem 1", G, "modem")
a(0o7, "Terminal 17", G, "terminal")
a(0o10, "Plotter 1", G, "character")
a(0o11, "Terminal 2", G, "terminal")
a(0o12, "Paper tape reader 2 / mode file internal device", G, "character")
a(0o13, "Paper tape punch 2 / mode file internal device", G, "character")
a(0o14, "Bus switch device", G, "character")
a(0o15, "Line printer 2", G, "printer")
a(0o16, "Synchronous modem 2", G, "modem")
a(0o17, "Terminal 18", G, "terminal")
a(0o20, "Cassette drive 1", G, "tape")
a(0o21, "Cassette drive 2", G, "tape")
a(0o22, "Versatec printer/plotter 1 (DMA) / IBM communication", G, "printer")
a(0o23, "Versatec printer/plotter 2 (DMA)", G, "printer")
a(0o24, "Tektronix display", G, "character")
a(0o25, "Magnetic tape controller 1, unit 2", G, "tape")
a(0o26, "Synchronous modem 5", G, "modem")
a(0o27, "Synchronous modem 6", G, "modem")
a(0o30, "Synchronous modem 3", G, "modem")
a(0o31, "Synchronous modem 4", G, "modem")
a(0o32, "Magnetic tape controller 2, unit 0", G, "tape")
a(0o33, "Magnetic tape controller 1, unit 3", G, "tape")
a(0o34, "Magnetic tape controller 2, unit 1", G, "tape")
a(0o35, "Card punch 3", G, "character")
a(0o36, "CDC link / TTY link sender", G, "communication")
a(0o37, "TTY link receiver", G, "communication")
a(0o40, "Magnetic tape controller 1, unit 0", G, "tape")
a(0o41, "Magnetic tape controller 1, unit 1", G, "tape")
a(0o42, "Terminal 3", G, "terminal")
a(0o43, "Terminal 4 / barcode reader", G, "terminal")
a(0o44, "Terminal 5 / barcode reader", G, "terminal")
a(0o45, "Terminal 6", G, "terminal")
a(0o46, "Terminal 7", G, "terminal")
a(0o47, "Terminal 8", G, "terminal")
a(0o50, "Card punch 1", G, "character")
a(0o51, "Card punch 2", G, "character")
a(0o52, "Terminal 19", G, "terminal")
a(0o53, "Terminal 20", G, "terminal")
a(0o54, "Terminal 21", G, "terminal")
a(0o55, "Terminal 22", G, "terminal")
a(0o56, "Terminal 23", G, "terminal")
a(0o57, "Terminal 24", G, "terminal")
a(0o60, "Terminal 9", G, "terminal")
a(0o61, "Terminal 10", G, "terminal")
a(0o62, "Terminal 11", G, "terminal")
a(0o63, "Terminal 12", G, "terminal")
a(0o64, "Terminal 13", G, "terminal")
a(0o65, "Terminal 14", G, "terminal")
a(0o66, "Terminal 15", G, "terminal")
a(0o67, "Terminal 16", G, "terminal")
a(0o70, "Terminal 25 / special sync modem 5", G, "terminal")
a(0o71, "Terminal 26 / special sync modem 6", G, "terminal")
a(0o72, "Terminal 27 / special sync modem 7 / Graf cassette 1", G, "terminal")
a(0o73, "Terminal 28 / special sync modem 8 / Graf cassette 2", G, "terminal")
a(0o74, "Terminal 29 / special sync modem 9 / photosetter 1", G, "terminal")
a(0o75, "Terminal 30 / photosetter 2", G, "terminal")
a(0o76, "Terminal 31 / photosetter 3", G, "terminal")
a(0o77, "Terminal 32", G, "terminal")

# === Group 1: (reserved) (100-177) ===
ar(0o100, 64, lambda i: "Open mass storage file", "(reserved)", "file")

# === Group 2: DV200 (200-277) ===
G = "DV200"
ar(0o200, 32, lambda i: "Internal device %d" % (i+1), G, "internal")
ar(0o240, 24, lambda i: "SIBAS internal device %d" % (i+1), G, "internal")
unused(0o270, 6, G)
a(0o276, "Internal device for ERS/SINTRAN III Watchdog", G, "internal")
a(0o277, "Internal device for FTX error logger", G, "internal")

# === Group 3: DV300 (300-377) ===
G = "DV300"
ar(0o300, 59, lambda i: "User semaphore %d" % (i+1), G, "semaphore")
ar(0o373, 5, lambda i: "User semaphore %d (also Backup-System)" % (60+i), G, "semaphore")

# === Group 4: DV400 (400-477) ===
G = "DV400"
camac = [
    (0o400, "CAMAC 1 / Special DMA DF 1 / Digital I/O 1 / DR11C 1 / Aristogrid 1"),
    (0o401, "CAMAC 2 / Special DMA DF 2 / Digital I/O 2 / DR11C 2 / Aristogrid 1"),
    (0o402, "CAMAC 3 / Special DMA DF 3 / Digital I/O 3 / DR11C 3 / Aristogrid 2"),
    (0o403, "CAMAC 4 / Special DMA DF 4 / Digital I/O 4 / DR11C 4 / Aristogrid 2"),
    (0o404, "CAMAC 5 / Special DMA DF 5 / Digital I/O 5 / DR11C 5 / Aristogrid 3"),
    (0o405, "CAMAC 6 / Special DMA DF 6 / Digital I/O 6 / DR11C 6 / Aristogrid 3"),
    (0o406, "CAMAC 7 / Digital I/O 7 / DR11C 7 / Aristogrid 4"),
    (0o407, "CAMAC 8 / Digital I/O 8 / DR11C 8 / Aristogrid 4"),
    (0o410, "CAMAC 9 / Special DMA DF 7 / Digital I/O 9 / DR11C 9 / Norcontrol PIO 1 / Aristogrid 5"),
    (0o411, "CAMAC 10 / Special DMA DF 8 / Digital I/O 10 / DR11C 10 / Norcontrol PIO 2 / Aristogrid 5"),
    (0o412, "CAMAC 11 / Digital I/O 11 / DR11C 11 / Norcontrol PIO 3 / Aristogrid 6"),
    (0o413, "CAMAC 12 / Special DMA DF 9 / Digital I/O 12 / DR11C 12 / Norcontrol PIO 4 / Aristogrid 6"),
    (0o414, "CAMAC 13 / Special DMA DF 10 / Digital I/O 13 / DR11C 13 / Norcontrol PIO 5 / Aristogrid 7"),
    (0o415, "CAMAC 14 / Special DMA DF 11 / Digital I/O 14 / DR11C 14 / Norcontrol PIO 6 / Aristogrid 7"),
    (0o416, "CAMAC 15 / Digital I/O 15 / DR11C 15 / Norcontrol PIO 7 / Aristogrid 8"),
    (0o417, "CAMAC 16 / Digital I/O 16 / DR11C 16 / Norcontrol PIO 8 / Aristogrid 8"),
    (0o420, "CAMAC 17 / Digital I/O 17 / DR11C 17 / Norcontrol PIO 9 / Aristogrid 9"),
    (0o421, "Digital I/O 18 / DR11C 18 / Norcontrol PIO 10 / Aristogrid 9"),
    (0o422, "Digital I/O 19 / DR11C 19 / Norcontrol PIO 11 / Aristogrid 10"),
    (0o423, "Digital I/O 20 / DR11C 20 / Norcontrol PIO 12 / Aristogrid 10"),
    (0o424, "Norcontrol process I/O 13"),
    (0o425, "Norcontrol process I/O 14"),
    (0o426, "Norcontrol process I/O 15"),
]
for n, d in camac:
    a(n, d, G, "process")
a(0o427, "Not used", G)
ar(0o430, 8, lambda i: "Analog input unit %d" % (i+1), G, "process")
ar(0o440, 4, lambda i: "Direct task level %d" % (6+i), G, "process")
unused(0o444, 4, G)
ar(0o450, 16, lambda i: "CONNECT device %d" % (i+1), G, "process")
a(0o470, "ND 23 - programmed clock", G, "process")
unused(0o471, 7, G)

# === Group 5: DV500 (500-577) ===
G = "DV500"
a(0o500, "Internal device for error message RT-program", G, "system")
a(0o501, "Semaphore for segment transfer", G, "semaphore")
a(0o502, "Not used", G)
a(0o503, "RT-Loader command semaphore", G, "semaphore")
a(0o504, "General semaphore for file system", G, "semaphore")
a(0o505, "User-file-buffer semaphore", G, "semaphore")
a(0o506, "Object-file-buffer semaphore", G, "semaphore")
a(0o507, "RT-open-file-table semaphore", G, "semaphore")
unused(0o510, 5, G)
a(0o515, "DF1, file-transfer for RT, semaphore for disk 1-4", G, "disk")
a(0o516, "DF2, open-file mon call from RT-program data field", G, "system")
a(0o517, "RTFIL semaphore", G, "semaphore")
a(0o520, "NOTIS-IR semaphore 2", G, "semaphore")
a(0o521, "Device buffer allocation semaphore", G, "semaphore")
unused(0o522, 4, G)
a(0o526, "DF3, transfer semaphore for magnetic tape 1", G, "tape")
a(0o527, "Spooling queue semaphore", G, "spooling")
a(0o530, "Accounting semaphore", G, "semaphore")
a(0o531, "CDC link monitor call data field", G, "communication")
a(0o532, "Spooling device 4, queue semaphore", G, "spooling")
a(0o533, "Spooling device 4, I/O semaphore", G, "spooling")
a(0o534, "Spooling device 5, queue semaphore", G, "spooling")
a(0o535, "Spooling device 5, I/O semaphore", G, "spooling")
a(0o536, "Spooling device 6, queue semaphore", G, "spooling")
a(0o537, "Spooling device 6, I/O semaphore", G, "spooling")
a(0o540, "Internal device Remote Batch IBM", G, "communication")
a(0o541, "Internal device Remote Batch UNIVAC", G, "communication")
a(0o542, "Internal device Remote Batch Honeywell Bull", G, "communication")
a(0o543, "Internal device Remote Batch CDC", G, "communication")
for i in range(4):
    a(0o544+i*2, "ECC disk ctrl 3, unit %d, directory semaphore" % i, G, "disk")
    a(0o545+i*2, "ECC disk ctrl 3, unit %d, bit file semaphore" % i, G, "disk")
unused(0o554, 4, G)
a(0o560, "Magnetic tape controller 1, data field", G, "tape")
a(0o561, "All magnetic tapes, directory semaphore", G, "tape")
a(0o562, "Spooling device 11, queue semaphore", G, "spooling")
a(0o563, "Magnetic tape controller 2, unit 2, I/O data field", G, "tape")
a(0o564, "Magnetic tape controller 2, unit 3, I/O data field", G, "tape")
a(0o565, "ECC disk controller 3, data field", G, "disk")
a(0o566, "ECC disk controller 4, data field", G, "disk")
a(0o567, "CDC link data field", G, "communication")
unused(0o570, 4, G)
a(0o574, "Monitor call data field for cassette", G, "tape")
a(0o575, "Cassette data field", G, "tape")
a(0o576, "DF5, monitor call data field for Versatec 1", G, "printer")
a(0o577, "Versatec data field", G, "printer")

# === Group 6: DV600 (600-677) ===
G = "DV600"
a(0o600, "BADMIN semaphores", G, "semaphore")
a(0o601, "BASEM", G, "semaphore")
a(0o602, "Default subsystem semaphore", G, "semaphore")
a(0o603, "Not used", G)
for i in range(30):
    a(0o604+i*2, "Spooling device %d, queue semaphore" % (31+i), G, "spooling")
    a(0o605+i*2, "Spooling device %d, I/O semaphore" % (31+i), G, "spooling")

# === Group 7: DV700 (700-777) ===
G = "DV700"
hdlc = [
    (0o700, "HDLC DMA link 7 input / NORCOM sys 1 semigraphic buf 0"),
    (0o701, "HDLC DMA link 7 output / NORCOM sys 1 semigraphic buf 1"),
    (0o702, "HDLC DMA link 8 input / NORCOM sys 1 semigraphic buf 2"),
    (0o703, "HDLC DMA link 8 output / NORCOM sys 1 semigraphic buf 3"),
    (0o704, "HDLC DMA link 9 input / NORCOM sys 1 semigraphic buf 4"),
    (0o705, "HDLC DMA link 9 output / NORCOM sys 1 semigraphic buf 5"),
    (0o706, "HDLC DMA link 10 input / NORCOM sys 1 semigraphic buf 6"),
    (0o707, "HDLC DMA link 10 output / NORCOM sys 1 semigraphic buf 7"),
    (0o710, "HDLC DMA link 11 input / NORCOM sys 1 graphic buf 1"),
    (0o711, "HDLC DMA link 11 output / NORCOM sys 1 graphic buf 3"),
    (0o712, "HDLC DMA link 12 input / NORCOM sys 1 graphic buf 5"),
    (0o713, "HDLC DMA link 12 output / NORCOM sys 1 graphic buf 7"),
    (0o714, "HDLC DMA link 13 input / NORCOM sys 1 selector 1"),
    (0o715, "HDLC DMA link 13 output / NORCOM sys 1 selector 2"),
    (0o716, "HDLC DMA link 14 input / NORCOM sys 1 selector 3"),
    (0o717, "HDLC DMA link 14 output / NORCOM sys 1 selector 4"),
    (0o720, "HDLC DMA link 15 input / NORCOM sys 1 selector 5"),
    (0o721, "HDLC DMA link 15 output / NORCOM sys 1 selector 6"),
    (0o722, "HDLC DMA link 16 input / NORCOM sys 1 selector 7"),
    (0o723, "HDLC DMA link 16 output / NORCOM sys 1 selector 8"),
    (0o724, "HDLC DMA link 17 input"),
    (0o725, "HDLC DMA link 17 output"),
    (0o726, "HDLC DMA link 18 input / NORCOM sys 2 semigraphic buf 0"),
    (0o727, "HDLC DMA link 18 output / NORCOM sys 2 semigraphic buf 1"),
    (0o730, "HDLC DMA link 19 input / NORCOM sys 2 semigraphic buf 2"),
    (0o731, "HDLC DMA link 19 output / NORCOM sys 2 semigraphic buf 3"),
    (0o732, "HDLC DMA link 20 input / NORCOM sys 2 semigraphic buf 4"),
    (0o733, "HDLC DMA link 20 output / NORCOM sys 2 semigraphic buf 5"),
    (0o734, "HDLC DMA link 21 input / NORCOM sys 2 semigraphic buf 6 / ACM 1"),
    (0o735, "HDLC DMA link 21 output / NORCOM sys 2 semigraphic buf 7 / ACM 2"),
    (0o736, "HDLC DMA link 22 input / NORCOM sys 2 graphic buf 1 / ACM 3"),
    (0o737, "HDLC DMA link 22 output / NORCOM sys 2 graphic buf 3 / ACM 4"),
    (0o740, "HDLC DMA link 23 input / NORCOM sys 2 graphic buf 5 / ACM 5"),
    (0o741, "HDLC DMA link 23 output / NORCOM sys 2 graphic buf 7"),
    (0o742, "HDLC DMA link 24 input / NORCOM sys 2 selector 1"),
    (0o743, "HDLC DMA link 24 output / NORCOM sys 2 selector 2"),
    (0o744, "HDLC DMA link 25 input / NORCOM sys 2 selector 3"),
    (0o745, "HDLC DMA link 25 output / NORCOM sys 2 selector 4"),
    (0o746, "HDLC DMA link 26 input / NORCOM sys 2 selector 5"),
    (0o747, "HDLC DMA link 26 output / NORCOM sys 2 selector 6"),
    (0o750, "HDLC DMA link 27 input / NORCOM sys 2 selector 7"),
    (0o751, "HDLC DMA link 27 output / NORCOM sys 2 selector 8"),
    (0o752, "HDLC DMA link 28 input"),
    (0o753, "HDLC DMA link 28 output"),
    (0o754, "HDLC DMA link 29 input / NORCOM sys 3 semigraphic buf 0"),
    (0o755, "HDLC DMA link 29 output / NORCOM sys 3 semigraphic buf 1"),
    (0o756, "HDLC DMA link 30 input / NORCOM sys 3 semigraphic buf 2"),
    (0o757, "HDLC DMA link 30 output / NORCOM sys 3 semigraphic buf 3"),
    (0o760, "HDLC DMA link 31 input / NORCOM sys 3 semigraphic buf 4"),
    (0o761, "HDLC DMA link 31 output / NORCOM sys 3 semigraphic buf 5"),
    (0o762, "HDLC DMA link 32 input / NORCOM sys 3 semigraphic buf 6"),
    (0o763, "HDLC DMA link 32 output / NORCOM sys 3 semigraphic buf 7"),
    (0o764, "NORCOM system 3, graphic buffer 1"),
    (0o765, "NORCOM system 3, graphic buffer 3"),
    (0o766, "NORCOM system 3, graphic buffer 5"),
    (0o767, "NORCOM system 3, graphic buffer 7"),
]
for n, d in hdlc:
    a(n, d, G, "communication")
ar(0o770, 8, lambda i: "NORCOM system 3, selector module %d" % (i+1), G, "communication")

# === Group 8: D1000 (1000-1077) ===
G = "D1000"
ar(0o1000, 3, lambda i: "Floppy disk ctrl 1, unit %d, I/O data field" % i, G, "disk")
ar(0o1003, 3, lambda i: "Floppy disk ctrl 2, unit %d, I/O data field" % i, G, "disk")
ar(0o1006, 6, lambda i: "HASP DMA %d, I/O data field" % (i+1), G, "communication")
a(0o1014, "Line printer 3, I/O data field", G, "printer")
a(0o1015, "Line printer 4, I/O data field", G, "printer")
unused(0o1016, 18, G)
ar(0o1040, 32, lambda i: "Terminal %d" % (33+i), G, "terminal")

# === Group 9: D1100 (1100-1177) ===
G = "D1100"
a(0o1100, "ECC disk controller 1, data field", G, "disk")
a(0o1101, "ECC disk ctrl 1, unit 0, directory semaphore", G, "disk")
a(0o1102, "ECC disk ctrl 1, unit 0, bit file semaphore", G, "disk")
unused(0o1103, 6, G)
a(0o1111, "Magnetic tape controller 2, data field", G, "tape")
a(0o1112, "ECC disk ctrl 4, unit 0, directory semaphore", G, "disk")
a(0o1113, "Floppy disk ctrl 1, unit 3, I/O data field", G, "disk")
a(0o1114, "ECC disk ctrl 4, unit 0, bit file semaphore", G, "disk")
a(0o1115, "Floppy disk ctrl 2, unit 3, I/O data field", G, "disk")
a(0o1116, "DR 7, transfer semaphore for mag tape ctrl 2", G, "tape")
a(0o1117, "ECC disk ctrl 1, unit 1, directory semaphore", G, "disk")
a(0o1120, "ECC disk ctrl 1, unit 1, bit file semaphore", G, "disk")
a(0o1121, "ECC disk ctrl 1, unit 2, directory semaphore", G, "disk")
a(0o1122, "ECC disk ctrl 1, unit 2, bit file semaphore", G, "disk")
a(0o1123, "ECC disk ctrl 1, unit 3, directory semaphore", G, "disk")
a(0o1124, "ECC disk ctrl 1, unit 3, bit file semaphore", G, "disk")
a(0o1125, "Versatec controller 2", G, "printer")
a(0o1126, "Monitor call DF for Versatec controller 2", G, "printer")
a(0o1127, "DF 39, mag tape ctrl 3, monitor call data field", G, "tape")
unused(0o1130, 4, G)
a(0o1134, "Floppy disk ctrl 1, unit 3, directory semaphore", G, "disk")
a(0o1135, "Floppy disk ctrl 1, unit 3, bit file semaphore", G, "disk")
a(0o1136, "Spooling device 1, queue semaphore", G, "spooling")
a(0o1137, "Spooling device 1, I/O semaphore", G, "spooling")
a(0o1140, "Spooling device 2, queue semaphore", G, "spooling")
a(0o1141, "Spooling device 2, I/O semaphore", G, "spooling")
a(0o1142, "Spooling system general semaphore", G, "spooling")
a(0o1143, "Spooling system wait for used pages semaphore", G, "spooling")
a(0o1144, "Spooling system wait for free pages semaphore", G, "spooling")
a(0o1145, "Floppy disk controller 1, data field", G, "disk")
a(0o1146, "Monitor call DF for floppy disk ctrl 1", G, "disk")
a(0o1147, "Floppy disk ctrl 2, unit 3, directory semaphore", G, "disk")
a(0o1150, "Floppy disk ctrl 1, unit 0, directory semaphore", G, "disk")
a(0o1151, "Floppy disk ctrl 1, unit 0, bit file semaphore", G, "disk")
a(0o1152, "Floppy disk ctrl 1, unit 1, directory semaphore", G, "disk")
a(0o1153, "Floppy disk ctrl 1, unit 1, bit file semaphore", G, "disk")
a(0o1154, "Floppy disk ctrl 1, unit 2, directory semaphore", G, "disk")
a(0o1155, "Floppy disk ctrl 1, unit 2, bit file semaphore", G, "disk")
a(0o1156, "Floppy disk controller 2, data field", G, "disk")
a(0o1157, "Monitor call DF for floppy disk ctrl 2", G, "disk")
a(0o1160, "Floppy disk ctrl 2, unit 3, bit file semaphore", G, "disk")
a(0o1161, "Floppy disk ctrl 2, unit 0, directory semaphore", G, "disk")
a(0o1162, "Floppy disk ctrl 2, unit 0, bit file semaphore", G, "disk")
a(0o1163, "Floppy disk ctrl 2, unit 1, directory semaphore", G, "disk")
a(0o1164, "Floppy disk ctrl 2, unit 1, bit file semaphore", G, "disk")
a(0o1165, "Floppy disk ctrl 2, unit 2, directory semaphore", G, "disk")
a(0o1166, "Floppy disk ctrl 2, unit 2, bit file semaphore", G, "disk")
a(0o1167, "DMA line printer 1, data field", G, "printer")
a(0o1170, "Monitor call DF for DMA line printer 1", G, "printer")
a(0o1171, "ECC disk ctrl 4, unit 2, directory semaphore", G, "disk")
a(0o1172, "ECC disk ctrl 4, unit 2, bit file semaphore", G, "disk")
a(0o1173, "Spooling device 3, queue semaphore", G, "spooling")
a(0o1174, "Spooling device 3, I/O semaphore", G, "spooling")
a(0o1175, "DMA line printer 2, data field", G, "printer")
a(0o1176, "Monitor call DF for DMA line printer 2", G, "printer")
a(0o1177, "Spooling semaphore for id data buffer", G, "spooling")

# === Group 10: D1200 (1200-1277) ===
G = "D1200"
a(0o1200, "NOTPS system semaphore", G, "semaphore")
a(0o1201, "DMAC command semaphore", G, "semaphore")
a(0o1202, "RT-PROGRAM-LOG semaphore", G, "semaphore")
a(0o1203, "Histogram commands semaphore", G, "semaphore")
a(0o1204, "SINTRAN Service Program command semaphore", G, "semaphore")
a(0o1205, "Mail system semaphore", G, "semaphore")
a(0o1206, "Terminal 1, data field", G, "terminal")
a(0o1207, "ECC disk controller 2, data field", G, "disk")
for i in range(5):
    a(0o1210+i*2, "Internal device %d, data field" % (i+1), G, "internal")
    a(0o1211+i*2, "Monitor call DF for internal device %d" % (i+1), G, "internal")
a(0o1222, "Accounting semaphore", G, "semaphore")
a(0o1223, "NOTIS-IR semaphore", G, "semaphore")
a(0o1224, "ST-506 Winchester disk ctrl 1 DF / STC mag tape ctrl 4", G, "disk")
a(0o1225, "Winchester ctrl 1 unit 0 dir sema / STC mag tape 4 unit 0 I/O DF", G, "disk")
a(0o1226, "Winchester ctrl 1 unit 0 bit sema / STC mag tape 4 unit 1 I/O DF", G, "disk")
a(0o1227, "Winchester ctrl 1 unit 1 dir sema / STC mag tape 4 unit 2 I/O DF", G, "disk")
a(0o1230, "Winchester ctrl 1 unit 1 bit sema / STC mag tape 4 unit 3 I/O DF", G, "disk")
a(0o1231, "ST-506 Winchester disk ctrl 2 DF / STC mag tape ctrl 3", G, "disk")
a(0o1232, "Winchester ctrl 2 unit 0 dir sema / STC mag tape 3 unit 0 I/O DF", G, "disk")
a(0o1233, "Winchester ctrl 2 unit 0 bit sema / STC mag tape 3 unit 1 I/O DF", G, "disk")
a(0o1234, "Winchester ctrl 2 unit 1 dir sema / STC mag tape 3 unit 2 I/O DF", G, "disk")
a(0o1235, "Winchester ctrl 2 unit 1 bit sema / STC mag tape 3 unit 3 I/O DF", G, "disk")
for i in range(10):
    a(0o1236+i*2, "Batch process %d, data field" % (i+1), G, "batch")
    a(0o1237+i*2, "Batch process %d, internal device" % (i+1), G, "batch")
for i in range(4):
    a(0o1262+i*2, "Spooling device %d, queue semaphore" % (7+i), G, "spooling")
    a(0o1263+i*2, "Spooling device %d, I/O semaphore" % (7+i), G, "spooling")
for i in range(5):
    a(0o1272+i, "Monitor call DF for internal device %d" % (i+1), G, "internal")
a(0o1277, "DF 40, mag tape ctrl 4, monitor call data field", G, "tape")

# === Group 11: D1300 (1300-1377) ===
G = "D1300"
a(0o1300, "ECC disk ctrl 4, unit 3, directory semaphore", G, "disk")
a(0o1301, "ECC disk ctrl 4, unit 3, bit file semaphore", G, "disk")
a(0o1302, "Device buffer semaphore", G, "semaphore")
for i in range(6):
    b = 0o1303 + i*4
    a(b,   "HASP DMA %d, input data field" % (i+1), G, "communication")
    a(b+1, "HASP DMA %d, output data field" % (i+1), G, "communication")
    a(b+2, "Monitor call DF for HASP DMA %d, input" % (i+1), G, "communication")
    a(b+3, "Monitor call DF for HASP DMA %d, output" % (i+1), G, "communication")
for i in range(4):
    a(0o1333+i*2, "ECC disk ctrl 2, unit %d, directory semaphore" % i, G, "disk")
    a(0o1334+i*2, "ECC disk ctrl 2, unit %d, bit file semaphore" % i, G, "disk")
a(0o1343, "DMA line printer 3, data field", G, "printer")
a(0o1344, "Monitor call DF for DMA line printer 3", G, "printer")
a(0o1345, "DMA line printer 4, data field", G, "printer")
a(0o1346, "Monitor call DF for DMA line printer 4", G, "printer")
a(0o1347, "Spooling device 11, I/O semaphore", G, "spooling")
a(0o1350, "Spooling device 12, queue semaphore", G, "spooling")
a(0o1351, "Spooling device 12, I/O semaphore", G, "spooling")
a(0o1352, "RT-PROGRAM-LOG command semaphore", G, "semaphore")
unused(0o1353, 5, G)
for i in range(6):
    a(0o1360+i*2, "HDLC DMA link %d input / sync modem %d for HDLC" % (i+1, i+1), G, "communication")
    a(0o1361+i*2, "HDLC DMA link %d output" % (i+1), G, "communication")
ar(0o1374, 4, lambda i: "X.21 line number %d" % (i+1), G, "communication")

# === Group 12: D1400 (1400-1477) ===
ar(0o1400, 64, lambda i: "Terminal access device (TAD) %d" % (i+1), "D1400", "tad")

# === Group 13: D1500 (1500-1577) ===
G = "D1500"
ar(0o1500, 32, lambda i: "Terminal access device (TAD) %d" % (65+i), G, "tad")
ar(0o1540, 16, lambda i: "Telefix terminal %d" % (i+1), G, "terminal")
ar(0o1560, 16, lambda i: "Telefix background terminal %d" % (i+1), G, "terminal")

# === Group 14: D1600 (1600-1677) ===
ar(0o1600, 64, lambda i: "DMA device buffer header semaphore, header %s" % format(i, 'o'), "D1600", "semaphore")

# === Group 15: D1700 (1700-1777) ===
G = "D1700"
ar(0o1700, 15, lambda i: "PIOC number %d" % (i+1), G, "system")
a(0o1717, "PIOC number 16 / virtual disk driver 1", G, "system")
a(0o1720, "Virtual disk driver 3", G, "disk")
a(0o1721, "Virtual disk driver 4", G, "disk")
for i in range(3):
    a(0o1722+i*2, "Spooling device %d, queue semaphore" % (13+i), G, "spooling")
    a(0o1723+i*2, "Spooling device %d, I/O semaphore" % (13+i), G, "spooling")
a(0o1730, "COSMOS file access, DF data field", G, "system")
a(0o1731, "COSMOS Spooling, peripheral device", G, "spooling")
a(0o1732, "Winchester disk ctrl 1, unit 0, directory semaphore", G, "disk")
a(0o1733, "Winchester disk ctrl 1, unit 0, bit file semaphore", G, "disk")
a(0o1734, "Winchester disk ctrl 1, unit 1, directory semaphore", G, "disk")
a(0o1735, "Winchester disk ctrl 1, unit 1, bit file semaphore", G, "disk")
a(0o1736, "Not used", G)
a(0o1737, "Winchester disk ctrl 2, unit 0, directory semaphore", G, "disk")
a(0o1740, "Winchester disk ctrl 2, unit 0, bit file semaphore", G, "disk")
a(0o1741, "Winchester disk ctrl 2, unit 1, directory semaphore", G, "disk")
a(0o1742, "Winchester disk ctrl 2, unit 1, bit file semaphore", G, "disk")
unused(0o1743, 5, G)
ar(0o1750, 24, lambda i: "SIBAS number %d" % i, G, "system")

# === Group 16: D2000 (2000-2077) ===
ar(0o2000, 64, lambda i: "Terminal %d" % (65+i), "D2000", "terminal")

# === Group 17: D2100 (2100-2177) ===
G = "D2100"
ar(0o2100, 16, lambda i: "Universal DMA / Vicom interface %d" % (i+1), G, "system")
ar(0o2120, 8, lambda i: "GPIB interface number %d" % i, G, "system")
for i in range(15):
    a(0o2130+i*2, "Spooling device %d, queue semaphore" % (16+i), G, "spooling")
    a(0o2131+i*2, "Spooling device %d, I/O semaphore" % (16+i), G, "spooling")
a(0o2166, "COSMOS Spooling, queue semaphore", G, "spooling")
a(0o2167, "COSMOS Spooling, I/O semaphore", G, "spooling")
unused(0o2170, 8, G)

# === Group 18: D2200 (2200-2277) ===
G = "D2200"
a(0o2200, "Disk access log data field", G, "disk")
a(0o2201, "Disk access log buffer semaphore", G, "disk")
ar(0o2202, 4, lambda i: "SCSI adaptor number %d" % (i+1), G, "disk")
a(0o2206, "SCSI streamer tape drive 1", G, "tape")
a(0o2207, "SCSI streamer tape drive 2", G, "tape")
ar(0o2210, 14, lambda i: "SCSI magnetic disk drive %d" % (i+1), G, "disk")
a(0o2226, "SCSI streamer tape 1, I/O data field", G, "tape")
a(0o2227, "SCSI streamer tape 2, I/O data field", G, "tape")
a(0o2230, "SCSI streamer tape 1, DF data field", G, "tape")
a(0o2231, "SCSI streamer tape 2, DF data field", G, "tape")
ar(0o2232, 4, lambda i: "SCSI optical disk drive %d" % (i+1), G, "disk")
unused(0o2236, 2, G)
ar(0o2240, 4, lambda i: "Ethernet interface %d" % (i+1), G, "network")
unused(0o2244, 11, G)
a(0o2257, "Domino allocation semaphore", G, "semaphore")
ar(0o2260, 16, lambda i: "BDIO pool %d" % (i+1), G, "system")

# === Group 19: D2300 (2300-2377) ===
ar(0o2300, 64, lambda i: "User-defined logical device number", "D2300", "user")

# === Group 20: D2400 (2400-2477) ===
for u in range(4):
    ar(0o2400+u*16, 16, lambda i, u=u: "Octobus unit %d (J/K-version compatible)" % u, "D2400", "system")

# === Group 21: D2500 (2500-2577) ===
G = "D2500"
for i in range(32):
    a(0o2500+i*2, "Directory entry %d, directory semaphore" % i, G, "semaphore")
    a(0o2501+i*2, "Directory entry %d, bit file semaphore" % i, G, "semaphore")

# === Group 22: D2600 (2600-2677) ===
G = "D2600"
for i in range(16):
    a(0o2600+i*2, "Directory entry %d, directory semaphore" % (32+i), G, "semaphore")
    a(0o2601+i*2, "Directory entry %d, bit file semaphore" % (32+i), G, "semaphore")
unused(0o2640, 32, G)

# === Group 23: D2700 (2700-2777) ===
ar(0o2700, 64, lambda i: "Terminal %d" % (129+i), "D2700", "terminal")

# === Group 24: D3000 (3000-3077) ===
ar(0o3000, 64, lambda i: "Terminal %d" % (193+i), "D3000", "terminal")

# === Group 25: D3100 (3100-3177) ===
G = "D3100"
for i in range(20):
    a(0o3100+i*2, "Batch process %d, data field" % (11+i), G, "batch")
    a(0o3101+i*2, "Batch process %d, internal device" % (11+i), G, "batch")
unused(0o3150, 24, G)

# === Group 26: (none) (3200-3277) - CNVRT[26]=0 ===
ar(0o3200, 64, lambda i: "Used for remote open files", "(none)", "file")

# === Group 27: D3300 (3300-3377) ===
G = "D3300"
ar(0o3300, 30, lambda i: "Batch process %d, extra batch queue device" % (i+1), G, "batch")
unused(0o3336, 34, G)

# === Validate entry count ===
expected = 0
for g in range(28):
    expected += 64
# Groups 0-27 = 28 groups x 64 = 1792
assert len(D) == expected, "Expected %d entries, got %d" % (expected, len(D))

# Sort by numeric value for output
sorted_keys = sorted(D.keys(), key=lambda k: int(k, 8))
out = {}
for k in sorted_keys:
    out[k] = D[k]

json.dump(out, sys.stdout, indent=2, ensure_ascii=False)
print()  # trailing newline
