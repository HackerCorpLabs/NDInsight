import brf_link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main_sym,units=brf_link.link(data)
mem=img.mem
loaded=img.loaded
# ND text: two 7/8-bit chars per word, hi byte first
def isprint(c): return 32<=c<127
run=[]
start=None
out=[]
for a in range(len(mem)):
    w=mem[a]
    hi=(w>>8)&0xFF; lo=w&0xFF
    for half,c in ((0,hi),(1,lo)):
        if loaded[a] and isprint(c):
            if start is None: start=a
            run.append(chr(c))
        else:
            if len(run)>=4:
                out.append((start,''.join(run)))
            run=[]; start=None
for s,t in out:
    print("%06o  %s"%(s,t))
