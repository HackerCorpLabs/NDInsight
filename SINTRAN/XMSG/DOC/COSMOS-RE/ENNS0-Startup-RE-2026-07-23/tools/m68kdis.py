# Minimal MC68000 disassembler - covers the opcodes present in the ND EthII server firmware
# regions of interest. Big-endian. Not exhaustive; unknown words shown as DC.W.
import sys
d=open(sys.argv[1],'rb').read()
def w(a): return (d[a]<<8)|d[a+1]
def l(a): return (w(a)<<16)|w(a+2)
def s16(x): return x-0x10000 if x&0x8000 else x
def s8(x): return x-0x100 if x&0x80 else x
An=lambda n:'A%d'%n; Dn=lambda n:'D%d'%n
def ea(mode,reg,a):
    # returns (text, extra_words_consumed)
    if mode==0: return Dn(reg),0
    if mode==1: return An(reg),0
    if mode==2: return '(%s)'%An(reg),0
    if mode==3: return '(%s)+'%An(reg),0
    if mode==4: return '-(%s)'%An(reg),0
    if mode==5: return '(%d,%s)'%(s16(w(a)),An(reg)),1
    if mode==6:
        ext=w(a); da=ext>>15; xr=(ext>>12)&7; wl='L' if ext&0x800 else 'W'; disp=s8(ext&0xff)
        idx=('A%d' if da else 'D%d')%xr
        return '(%d,%s,%s.%s)'%(disp,An(reg),idx,wl),1
    if mode==7:
        if reg==0: return '($%x).W'%w(a),1
        if reg==1: return '($%x).L'%l(a),2
        if reg==2: return '(%d,PC)'%s16(w(a)),1
        if reg==3: return '(pc-idx)',1
        if reg==4: return '#imm',1
    return '?',0
def dis(a):
    op=w(a); nxt=a+2
    def imm(sz):
        nonlocal nxt
        if sz==1: v=w(nxt)&0xff; nxt+=2
        elif sz==2: v=w(nxt); nxt+=2
        else: v=l(nxt); nxt+=4
        return v
    # RTS/RTE/NOP etc
    simple={0x4e75:'rts',0x4e73:'rte',0x4e71:'nop',0x4e77:'rtr',0x4e70:'reset'}
    if op in simple: return simple[op],2
    # BSR/BRA/Bcc
    if (op&0xf000)==0x6000:
        cc=(op>>8)&0xf; disp=op&0xff
        names=['ra','sr','hi','ls','cc','cs','ne','eq','vc','vs','pl','mi','ge','lt','gt','le']
        if disp==0: tgt=a+2+s16(w(nxt)); nxt+=2
        elif disp==0xff: tgt=a+2+l(nxt); nxt+=4
        else: tgt=a+2+s8(disp)
        return 'b%s $%06x'%(names[cc],tgt),nxt-a
    # JSR/JMP  0100 1110 10/11 mode reg
    if (op&0xffc0)==0x4e80 or (op&0xffc0)==0x4ec0:
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return ('jsr ' if (op&0x40)==0 else 'jmp ')+t,nxt-a
    # LEA  0100 rrr 111 mode reg
    if (op&0xf1c0)==0x41c0:
        an=(op>>9)&7; t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return 'lea %s,A%d'%(t,an),nxt-a
    # MOVEQ
    if (op&0xf100)==0x7000:
        return 'moveq #%d,D%d'%(s8(op&0xff),(op>>9)&7),2
    # MOVEM 0100 1d00 1s mode reg
    if (op&0xfb80)==0x4880:
        rl=w(nxt); nxt+=2; sz='L' if op&0x40 else 'W'; dr=(op>>10)&1
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return 'movem.%s %s mask=%04x %s'%(sz,('->'if dr else '<-'),rl,t),nxt-a
    # TST 0100 1010 sz mode reg
    if (op&0xff00)==0x4a00:
        sz=(op>>6)&3; szc='bwl'[sz] if sz<3 else '?'
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return 'tst.%s %s'%(szc,t),nxt-a
    # CMPI 0000 1100 sz mode reg
    if (op&0xff00)==0x0c00:
        sz=(op>>6)&3; v=imm(sz+1 if sz<2 else 3)
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return 'cmpi.%s #$%x,%s'%('bwl'[sz],v,t),nxt-a
    # MOVE.x  size 1=byte(01),3=word(11),2=long(10) top2=00
    if (op&0xc000)==0x0000 and (op>>12)&3 in (1,2,3):
        szf=(op>>12)&3; szc={1:'b',3:'w',2:'l'}[szf]
        srcmode=(op>>3)&7; srcreg=op&7
        if srcmode==7 and srcreg==4:
            v=imm({'b':1,'w':2,'l':3}[szc]); src='#$%x'%v
        else:
            src,ex=ea(srcmode,srcreg,nxt); nxt+=ex*2
        dmode=(op>>6)&7; dreg=(op>>9)&7
        dst,ex=ea(dmode,dreg,nxt); nxt+=ex*2
        return 'move.%s %s,%s'%(szc,src,dst),nxt-a
    # ADDQ/SUBQ 0101 ddd 0 sz  ... 
    if (op&0xf000)==0x5000 and ((op>>6)&3)!=3:
        data=(op>>9)&7; data=8 if data==0 else data; sub=(op>>8)&1; sz='bwl'[(op>>6)&3]
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return '%s.%s #%d,%s'%('subq'if sub else 'addq',sz,data,t),nxt-a
    # DBcc 0101 cccc 11001 reg
    if (op&0xf0f8)==0x50c8:
        cc=(op>>8)&0xf; tgt=a+2+s16(w(nxt)); nxt+=2
        return 'db%02x D%d,$%06x'%(cc,op&7,tgt),nxt-a
    # BTST/BSET etc bit ops with immediate 0000 1000
    if (op&0xff00)==0x0800:
        bit=w(nxt); nxt+=2; sub=(op>>6)&3
        t,ex=ea((op>>3)&7,op&7,nxt); nxt+=ex*2
        return '%s #%d,%s'%(['btst','bchg','bclr','bset'][sub],bit,t),nxt-a
    return 'DC.W $%04x'%op,2
if __name__=='__main__':
    a=int(sys.argv[2],0); cnt=int(sys.argv[3]) if len(sys.argv)>3 else 40
    for _ in range(cnt):
        txt,n=dis(a)
        print('%06x: %s   %s'%(a,' '.join('%04x'%w(a+2*k) for k in range(n//2)).ljust(20),txt))
        a+=n
