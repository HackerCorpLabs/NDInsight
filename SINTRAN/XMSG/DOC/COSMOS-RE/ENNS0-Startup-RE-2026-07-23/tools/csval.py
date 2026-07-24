# Validate BRF checksums exactly per spec: sum BEG-byte..END-byte incl S-groups, checksum=one's-complement
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
p=0; ok=0; bad=0; units=0
def w16(i): return (data[i]<<8)|data[i+1]
longf=False
while p<len(data):
    c=data[p]
    if c==0: p+=1; continue
    if c==0o23: break
    if c==0o17:  # BEG - start sum
        s=c; q=p+1; longf=False
        # walk to END accumulating
        while q<len(data):
            cc=data[q]; s=(s+cc)&0xFFFF; q+=1
            if cc==0o32: longf=True; continue
            sg=3 if longf else 2
            # determine group layout to know how many words follow and whether s-group
            def addw(n):
                nonlocal q,s
                for _ in range(n):
                    s=(s+w16(q))&0xFFFF; q+=2
            if cc in (0o1,0o2,0o3,0o10,0o11,0o12,0o25,0o46): addw(1)
            elif cc in (0o4,0o5,0o6,0o7,0o34): addw(2)
            elif cc==0o35: addw(3)
            elif cc==0o36: addw(4)
            elif cc==0o37: addw(7)
            elif cc in (0o14,0o15,0o16,0o20): addw(sg)
            elif cc==0o26: addw(sg); addw(1)
            elif cc==0o27: addw(sg)
            elif cc==0o24:  # LNF count+words
                n=w16(q); s=(s+n)&0xFFFF; q+=2; addw(n)
            elif cc==0o30:  # ident stamp
                n=w16(q); s=(s+n)&0xFFFF; q+=2; addw(n)
            elif cc in (0o17,0o22,0o32,0o47,0o50,0o51,0o23): pass
            elif cc==0o21:  # END: next word is checksum, do NOT add it
                cs=w16(q); q+=2
                if ((s+cs)&0xFFFF)==0xFFFF: 
                    pass
                break
            else:
                addw(0)
        return_q=q
        # record
        if ((s+cs)&0xFFFF)==0xFFFF: ok+=1
        else: bad+=1
        units+=1
        p=q; continue
    p+=1
print("units=%d checksum_ok=%d bad=%d"%(units,ok,bad))
