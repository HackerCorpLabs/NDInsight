from brf_link import link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main,units=link(data)
mem=img.mem; loaded=img.loaded
targets={v:k for k,v in symbols.items() if k in
    ('READPIO','SEGLOAD','UNLOAD','START_P','STOP_PI','RES_SLO','REL_SLO','SEND_KI','REC_KIC','INT2GET','POSUERR','ENNS0')}
for addr,name in sorted(targets.items()):
    ptrs=[a for a in range(len(mem)) if loaded[a] and mem[a]==addr]
    print("%-8s @%06o  pointer-words-at: %s" % (name,addr,' '.join('%06o'%p for p in ptrs)))
# Also show refs recorded (external REF chains)
print("=== REF names count ===")
for n in ('READPIO','START_P','INT2GET','SEGLOAD'):
    print(n, len(refs.get(n,[])), [ '%06o'%x for x in refs.get(n,[])[:12]])
