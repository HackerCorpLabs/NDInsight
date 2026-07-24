from brf_link import link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main,units=link(data)
mem=img.mem
a2s={v:k for k,v in symbols.items()}
for a in range(0o30776,0o31012):
    w=mem[a]
    tgt=a2s.get(w,'')
    print("%06o: %06o  %s" % (a,w,tgt))
print("--- callee resolution ---")
for p in (0o30777,0o31000,0o31001,0o31002,0o31003):
    v=mem[p]; print("[%06o]=%06o  callee=%s" % (p,v,a2s.get(v,'?')))
