import sys
from brf_link import link
data=open(sys.argv[1],'rb').read()
img,symbols,refs,main_sym,units=link(data)
print("MAIN=",main_sym, "octal", oct(symbols.get(main_sym,0)))
items=sorted(symbols.items(), key=lambda kv: kv[1])
for k,v in items:
    print("%06o  %s" % (v,k))
