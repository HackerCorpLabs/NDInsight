from brf_link import link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main_sym,units=link(data)
mem=img.mem; ld=img.loaded
iox=ioxt=0; mons={}
for a in range(len(mem)):
    if not ld[a]: continue
    w=mem[a]
    if (w&0o170000)==0o164000: iox+=1
    if w==0o150415: ioxt+=1
    if (w&0o177400)==0o153000:
        mons[w&0xFF]=mons.get(w&0xFF,0)+1
print("IOX=%d IOXT=%d"%(iox,ioxt))
print("MON calls (octal fn: count):")
for k in sorted(mons): print("  MON %o : %d"%(k,mons[k]))
