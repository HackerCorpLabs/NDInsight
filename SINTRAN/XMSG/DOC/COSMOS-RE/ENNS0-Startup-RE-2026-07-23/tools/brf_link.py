#!/usr/bin/env python3
# Minimal BRF linker for ND-100 relocatable object files.
# Implements the rules in SINTRAN/File-Formats/BRF-FILE-FORMAT.md
# Produces a flat 16-bit-word code image + symbol table.
import sys, struct

class Reader:
    def __init__(self, data):
        self.d = data
        self.p = 0
    def eof(self):
        return self.p >= len(self.d)
    def byte(self):
        b = self.d[self.p]; self.p += 1; return b
    def word(self):
        # big-endian 16-bit P-group
        hi = self.d[self.p]; lo = self.d[self.p+1]; self.p += 2
        return (hi << 8) | lo

def sixbit_decode(words, n):
    # words: list of 16-bit words; concatenate MSB-first into a bitstream,
    # take 6-bit chars. value 0=space, ASCII = val + 0o40
    bits = 0
    nbits = 0
    for w in words:
        bits = (bits << 16) | w
        nbits += 16
    s = ''
    total_chars = nbits // 6
    # right-justified; leftover top bits are pad
    # Extract chars from the top
    for i in range(total_chars):
        shift = nbits - 6*(i+1)
        c = (bits >> shift) & 0o77
        # ND trimmed 6-bit: 0=space; c<0o40 -> letters/punct (c|0o100); else literal (digits)
        if c == 0:
            s += ' '
        elif c < 0o40:
            s += chr(c | 0o100)
        else:
            s += chr(c)
    return s.strip()

class Image:
    def __init__(self, size=0o200000):
        self.mem = [0]*size
        self.loaded = [False]*size
    def store(self, addr, val):
        self.mem[addr] = val & 0xFFFF
        self.loaded[addr] = True

def link(data):
    r = Reader(data)
    img = Image()
    symbols = {}   # name -> addr (defined)
    refs = {}      # name -> list of ref addresses (unresolved chain not simulated; we just record)
    main_sym = None
    CLC = 0
    PB = 0
    longf = False
    units = 0
    unit_start_clc = 0
    while not r.eof():
        ctrl = r.byte()
        if ctrl == 0:  # FEED
            continue
        if ctrl == 0o23:  # EOF
            break
        if ctrl == 0o17:  # BEG
            PB = CLC
            longf = False
            units += 1
            unit_start_clc = CLC
            continue
        if ctrl == 0o32:  # LONGF
            longf = True
            continue
        if ctrl == 0o22:  # INHB
            continue
        if ctrl == 0o47:  # DBG
            continue
        if ctrl in (0o50,0o51):  # PMO/DMO
            continue
        # S-group helper
        def sgroup():
            nw = 3 if longf else 2
            ws = [r.word() for _ in range(nw)]
            return sixbit_decode(ws, nw), ws
        if ctrl == 0o1:  # LF  W1->((CLC)); CLC+1
            w = r.word(); img.store(CLC, w); CLC += 1; continue
        if ctrl == 0o2:  # LR  W1+PB
            w = r.word(); img.store(CLC, (w+PB)&0xFFFF); CLC += 1; continue
        if ctrl == 0o3:  # LC
            w = r.word(); img.store(CLC, w); CLC += 1; continue
        if ctrl == 0o4:  # AFF W1+(W2)->(W2)
            w1=r.word(); w2=r.word(); img.store(w2,(w1+img.mem[w2])&0xFFFF); continue
        if ctrl == 0o5:  # ARF W1+PB+(W2)->(W2)
            w1=r.word(); w2=r.word(); img.store(w2,(w1+PB+img.mem[w2])&0xFFFF); continue
        if ctrl == 0o6:  # AFR W1+(W2+PB)->(W2+PB)
            w1=r.word(); w2=r.word(); a=(w2+PB)&0xFFFF; img.store(a,(w1+img.mem[a])&0xFFFF); continue
        if ctrl == 0o7:  # ARR
            w1=r.word(); w2=r.word(); a=(w2+PB)&0xFFFF; img.store(a,(w1+PB+img.mem[a])&0xFFFF); continue
        if ctrl == 0o10:  # SFL W1->(CLC)
            w=r.word(); CLC=w; continue
        if ctrl == 0o11:  # AFL W1+CLC (fill zeros)
            w=r.word(); CLC=(w+CLC)&0xFFFF; continue
        if ctrl == 0o12:  # SRL W1+PB->(CLC)
            w=r.word(); CLC=(w+PB)&0xFFFF; continue
        if ctrl == 0o14:  # MAIN
            name,_=sgroup(); main_sym=name; symbols.setdefault(name, CLC); continue
        if ctrl == 0o15:  # LIBR
            name,_=sgroup(); continue
        if ctrl == 0o16:  # ENTR sym=CLC
            name,_=sgroup(); symbols[name]=CLC; continue
        if ctrl == 0o20:  # REF referenced at CLC
            name,_=sgroup(); refs.setdefault(name,[]).append(CLC); continue
        if ctrl == 0o21:  # END + checksum
            cs=r.word(); continue
        if ctrl == 0o24:  # LNF W1=count then words
            n=r.word()
            for _ in range(n):
                img.store(CLC, r.word()); CLC+=1
            continue
        if ctrl == 0o25:  # RT
            r.word(); continue
        if ctrl == 0o26:  # ASF sym + len
            name,_=sgroup(); ln=r.word(); continue
        if ctrl == 0o27:  # ADS sym
            name,_=sgroup(); continue
        if ctrl == 0o34:  # INL W2->(W1+PB)
            w1=r.word(); w2=r.word(); img.store((w1+PB)&0xFFFF,w2); continue
        if ctrl == 0o35:  # DBL
            w1=r.word()
            for i in range(2): img.store((w1+PB+i)&0xFFFF, r.word())
            continue
        if ctrl == 0o36:  # RLL
            w1=r.word()
            for i in range(3): img.store((w1+PB+i)&0xFFFF, r.word())
            continue
        if ctrl == 0o37:  # CXL
            w1=r.word()
            for i in range(6): img.store((w1+PB+i)&0xFFFF, r.word())
            continue
        if ctrl == 0o44:  # BYL W1+PB, W2
            w1=r.word(); w2=r.word()
            a=(w1+PB)&0xFFFF; cur=img.mem[a]
            if w2 & 0x8000:
                cur=(cur & 0x00FF)|((w2&0xFF)<<8)
            else:
                cur=(cur & 0xFF00)|(w2&0xFF)
            img.store(a,cur); continue
        if ctrl == 0o46:  # NWL line number
            r.word(); continue
        if ctrl == 0o30:  # observed ident stamp: count + payload
            n=r.word()
            for _ in range(n): r.word()
            continue
        if ctrl == 0o54:  # DIC dictionary until -1
            while True:
                w=r.word()
                if w==0o177777: break
            continue
        # 40-43,45 COMMON stores - unlikely here; parse defensively
        if ctrl in (0o40,0o41,0o42,0o43,0o45):
            name,_=sgroup()
            counts={0o40:2,0o41:3,0o42:4,0o43:7,0o45:3}
            # 40:W4,W5 ; but W1-3 are sgroup. remaining plain words:
            extra={0o40:2,0o41:3,0o42:4,0o43:7,0o45:3}
            for _ in range(extra[ctrl]): r.word()
            continue
        raise ValueError("Unknown control %o at byte %d" % (ctrl, r.p-1))
    return img, symbols, refs, main_sym, units

if __name__=='__main__':
    fn=sys.argv[1]
    data=open(fn,'rb').read()
    img,symbols,refs,main_sym,units=link(data)
    print("units=%d main=%s nsym=%d nref=%d" % (units, main_sym, len(symbols), len(refs)))
    # dump symbol table sorted by address
    items=sorted(symbols.items(), key=lambda kv: kv[1])
    for name,addr in items:
        print("%06o  %s" % (addr, name))
