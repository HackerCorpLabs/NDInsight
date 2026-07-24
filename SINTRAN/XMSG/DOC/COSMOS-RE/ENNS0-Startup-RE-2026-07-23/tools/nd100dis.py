#!/usr/bin/env python3
# Minimal ND-100 disassembler, focused on control flow / MON / IOX / status polls.
import sys
from brf_link import link

MODES = ["*{d}", "{d},B", "I *{d}", "I {d},B", "{d},X", "{d},B,X", ",X I *{d}", ",X I {d},B"]

REG = {0:'A',1:'D',2:'T',3:'X',4:'0',5:'B',6:'L',7:'P'}

def s8(v):
    return v-256 if v & 0x80 else v

def mref(name, w, addr):
    mode = (w >> 8) & 7
    disp = s8(w & 0xFF)
    # compute target for P-relative modes (0,2,6)
    tgt=''
    if mode in (0,2,6):
        t=(addr+disp)&0xFFFF
        tgt='  ; =%06o' % t
    md = MODES[mode].format(d=("%d"%disp) if disp<0 else ("%o"%disp))
    return "%-5s %s%s" % (name, md, tgt)

def disp_skp(w):
    cond=(w>>3)&7
    src=(w>>0)&7  # actually src in bits 0-2, dest bits 3-5? Let's decode standard
    # SKP: 140000 | (dr<<3) | (cond? ) ... Use ND encoding: 140000 + sr*1 + conds
    # Standard: bits 0-2 = source reg, bits 3-5 = condition, bits 6-8 = dest reg
    sr=w&7
    cc=(w>>3)&7
    dr=(w>>6)&7
    conds=['EQL','GEQ','GRE','MGRE','UEQ','LSS','LST','MLST']
    return "SKP  IF D%s %s S%s" % (REG[dr], conds[cc], REG[sr])

def decode(w, addr):
    op = (w >> 11) & 0o37  # top 5 bits -> but ND uses different grouping
    hi = w & 0o170000  # top bits
    # memory reference group: opcodes 000..174 in steps, top 6 bits define
    top6 = w & 0o174000
    memops = {
        0o000000:'STZ',0o004000:'STA',0o010000:'STT',0o014000:'STX',
        0o020000:'STD',0o024000:'LDD',0o030000:'STF',0o034000:'LDF',
        0o044000:'LDA',0o050000:'LDT',0o054000:'LDX',
        0o060000:'ADD',0o064000:'SUB',0o040000:'MIN',0o120000:'MPY',
        0o070000:'AND',0o074000:'ORA',0o124000:'JMP',0o134000:'JPL',
        0o100000:'FAD',0o104000:'FSB',0o110000:'FMU',0o114000:'FDV',
    }
    if top6 in memops:
        return mref(memops[top6], w, addr)
    # conditional jumps 130/131/132/133
    cj = w & 0o177400
    cjmap={0o130000:'JAP',0o130400:'JAN',0o131000:'JAZ',0o131400:'JAF',
           0o132000:'JPC',0o132400:'JNC',0o133000:'JXZ',0o133400:'JXN'}
    if cj in cjmap:
        disp=s8(w&0xFF); t=(addr+disp)&0xFFFF
        return "%-5s *%d  ; =%06o" % (cjmap[cj], disp, t)
    # argument/immediate group 170-173
    imm = w & 0o177400
    immmap={0o170000:'SAB',0o170400:'SAA',0o171000:'SAT',0o171400:'SAX',
            0o172000:'AAB',0o172400:'AAA',0o173000:'AAT',0o173400:'AAX'}
    if imm in immmap:
        return "%-5s %d" % (immmap[imm], s8(w&0xFF))
    # MON 153 0nn
    if (w & 0o177400)==0o153000:
        return "MON  %o" % (w & 0xFF)
    # IOX 164 nnn
    if (w & 0o170000)==0o164000:
        return "IOX  %o" % (w & 0o7777)
    # SKP group 140000 with bit patterns; SKP = 140000..140777? Actually skip is 140000|...
    # ND: SKP occupies 140000 range with sub 000? Use: if (w & 0o177000)==0o140000 and low has cond bits -> but many 140xxx are misc.
    # Register operations 144000-147777
    if (w & 0o170000)==0o144000:
        return decode_reg(w)
    # SKP is actually 140000 + ... let me treat 140500-140677 etc. Use explicit:
    # bit-ops BSET/BSKP 150xxx? Actually 174000 range.
    # system 150 xxx
    sysm={0o150400:'OPCOM',0o150401:'IOF',0o150402:'ION',0o150404:'POF',
          0o150405:'PIOF',0o150406:'SEX',0o150407:'REX',0o150410:'PON',
          0o150412:'PION',0o150415:'IOXT',0o150416:'EXAM',0o150417:'DEPO'}
    if w in sysm:
        return sysm[w]
    # TRA/TRR/MCL/MST 150 0nn/1nn/2nn/3nn
    if (w & 0o177400)==0o150000:
        sub=(w>>6)&3; reg=w&0o77
        nm=['TRA','TRR','MCL','MST'][sub]
        regs={0:'PANS',1:'STS',2:'OPR',3:'PGS',4:'PVL',5:'IIC',6:'PID',7:'PIE',
              0o10:'CSR',0o11:'ACTL',0o12:'ALD',0o13:'PES',0o14:'PCR',0o15:'PEA'}
        return "%-5s %s" % (nm, regs.get(reg, "%o"%reg))
    # WAIT 151 0nn
    if (w & 0o177400)==0o151000:
        return "WAIT %o" % (w&0xFF)
    # BSET/BSKP 174000/175/176/177 region (bit ops) opcode 174000..177777
    if (w & 0o170000)==0o170000:
        pass
    # bit ops: BSKP=175xxx? BSET=? Use 174 group
    bop = w & 0o177000
    if bop in (0o174000,0o175000,0o176000,0o177000):
        return "BITOP %06o" % w
    # SKP fallback: 140000 pure with cond fields (140000-140077 plus reg combos)
    if (w & 0o170000)==0o140000:
        # could be SKP / RMPY / RDIV / misc
        if (w & 0o177700)==0o141200: return "RMPY S%s D%s"%(REG[w&7],REG[(w>>3)&7])
        if (w & 0o177700)==0o141600: return "RDIV S%s D%s"%(REG[w&7],REG[(w>>3)&7])
        if w==0o140200: return "HALT"
        if w==0o140120+3: pass
        # SKP: 140000 + dr<<6? Actually SKP opcode base is 140000 with cond at bits3-5
        # but 140000 also = SKP with dr=0 cond=0 sr=0 => "SKP DA EQL SA"? ambiguous.
        # Heuristic: treat 140000..140077 & 141000..141177 as SKP if bits 9-11 zero
        return disp_skp(w)
    return ".WORD %06o" % w

def decode_reg(w):
    # 144000 base ROP: sub-function bits. rough decode of COPY/SWAP/RADD/RSUB/RCLR/EXIT
    # bits: 146xxx COPY? Use common encodings.
    sr=(w>>3)&7; dr=w&7
    if w==0o146142: return "EXIT"
    # COPY = 146 100? RADD=146000, etc. Provide raw + guess
    grp=(w>>8)&0o17
    return "ROP  %06o (S%s D%s)" % (w, REG[(w>>3)&7], REG[w&7])

def dis_range(img, start, count, symbols):
    addr2sym={v:k for k,v in symbols.items()}
    out=[]
    a=start
    for _ in range(count):
        w=img.mem[a]
        lbl=addr2sym.get(a,'')
        lbls = (lbl+':') if lbl else ''
        out.append("%06o  %06o  %-10s %s" % (a, w, lbls, decode(w,a)))
        a=(a+1)&0xFFFF
    return "\n".join(out)

if __name__=='__main__':
    fn=sys.argv[1]
    start=int(sys.argv[2],8)
    count=int(sys.argv[3]) if len(sys.argv)>3 else 60
    data=open(fn,'rb').read()
    img,symbols,refs,main_sym,units=link(data)
    print(dis_range(img,start,count,symbols))
