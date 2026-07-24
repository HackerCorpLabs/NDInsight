from brf_link import link
from collections import Counter
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main,units=link(data)
mem=img.mem; loaded=img.loaded
monc=Counter(); iox=0; ioxt=0
for a in range(len(mem)):
    if not loaded[a]: continue
    w=mem[a]
    if (w&0o177400)==0o153000: monc[w&0xFF]+=1
    if (w&0o170000)==0o164000: iox+=1
    if w==0o150415: ioxt+=1
print("ENNS0 image: IOX=%d IOXT=%d"%(iox,ioxt))
print("MON calls (octal operand : count):")
for op in sorted(monc): print("  MON %03o = %d"%(op,monc[op]))
