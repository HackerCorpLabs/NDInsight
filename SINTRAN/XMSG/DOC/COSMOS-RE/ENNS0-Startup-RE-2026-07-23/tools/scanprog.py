import sys
fn=sys.argv[1]
data=open(fn,'rb').read()
# PROG: assume flat 16-bit big-endian words after some header; scan raw words both endian
def words(be):
    w=[]
    for i in range(0,len(data)-1,2):
        if be: w.append((data[i]<<8)|data[i+1])
        else: w.append((data[i+1]<<8)|data[i])
    return w
for be in (True,False):
    ws=words(be)
    ioxt=[i for i,v in enumerate(ws) if v==0o150415]
    dev=[i for i,v in enumerate(ws) if 0o140360<=v<=0o140377]
    mon255=[i for i,v in enumerate(ws) if v==0o153255]
    print("endian=%s IOXT=%d dev140360-77=%d MON255B=%d" % ('BE' if be else 'LE',len(ioxt),len(dev),len(mon255)))
    if dev: print("  dev word offsets(dec):", dev[:20])
    if ioxt: print("  ioxt offsets(dec):", ioxt[:20])
