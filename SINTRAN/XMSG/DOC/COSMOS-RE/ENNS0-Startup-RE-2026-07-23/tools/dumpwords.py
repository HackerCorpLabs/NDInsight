import sys
from brf_link import link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main_sym,units=link(data)
a2s={v:k for k,v in symbols.items()}
start=int(sys.argv[1],8); n=int(sys.argv[2])
for a in range(start,start+n):
    w=img.mem[a]
    tgt=a2s.get(w,'')
    print("%06o: %06o  %s"%(a,w,("-> "+tgt) if tgt else ""))
