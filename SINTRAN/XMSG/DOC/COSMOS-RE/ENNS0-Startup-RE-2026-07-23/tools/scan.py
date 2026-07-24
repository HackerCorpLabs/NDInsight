from brf_link import link
data=open("E:/Dev/Ronny/NDInsight/Installation/Communication/Ethernet/x/encos-err-i-b01.brf",'rb').read()
img,symbols,refs,main,units=link(data)
mem=img.mem; loaded=img.loaded
# find IOXT (0150415) and IOX (164xxx) and MON 255 (0153377)
print("=== IOXT 0150415 occurrences ===")
for a in range(len(mem)):
    if loaded[a] and mem[a]==0o150415:
        print("%06o" % a)
print("=== IOX 164xxx occurrences ===")
n=0
for a in range(len(mem)):
    if loaded[a] and (mem[a]&0o170000)==0o164000:
        print("%06o  IOX %o" % (a, mem[a]&0o7777)); n+=1
        if n>40: break
print("=== MON 255 (0153377) occurrences ===")
for a in range(len(mem)):
    if loaded[a] and mem[a]==0o153377:
        print("%06o" % a)
print("=== constant words of interest ===")
consts={0o1226:'0x40A mon_counter',0o1214:'0x40C mon_code',0o1236:'0x412 req_flag',0o1300:'0x4C0 started',0o10001:'0x1001 status',0o1300:'0x4C0'}
for c,nm in consts.items():
    hits=[a for a in range(len(mem)) if loaded[a] and mem[a]==c]
    print("%s (=%06o): %s" % (nm,c,' '.join('%06o'%h for h in hits[:30])))
