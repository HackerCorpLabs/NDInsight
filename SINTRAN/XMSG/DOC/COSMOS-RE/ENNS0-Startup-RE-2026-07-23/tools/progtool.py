#!/usr/bin/env python3
"""Helpers over a loaded :PROG image: string->addr, xref scan, word dump."""
import sys
from prog_load import load_prog

def a2f(addr):   # memory word addr -> file byte offset
    return 0x200 + 2*addr
def f2a(foff):   # file byte offset -> memory word addr
    return (foff - 0x200)//2

def find_str(data, needle):
    """Return list of (fileoff, memaddr) where ascii(7bit) needle begins."""
    nb = needle.encode('ascii')
    out=[]
    n=len(data)
    for i in range(n-len(nb)):
        ok=True
        for j,ch in enumerate(nb):
            if (data[i+j]&0x7f)!=ch: ok=False;break
        if ok: out.append((i, f2a(i)))
    return out

def xref(mem, target, lo=0, hi=None):
    """Find memory addresses whose word == target (absolute pointer)."""
    if hi is None: hi=len(mem)
    return [a for a in range(lo,hi) if mem[a]==target]

if __name__=='__main__':
    path=sys.argv[1]; cmd=sys.argv[2]
    data=open(path,'rb').read()
    mem,info=load_prog(path)
    if cmd=='str':
        for off,a in find_str(data, sys.argv[3]):
            print("file 0x%05x  mem %06o (%d)"%(off,a,a))
    elif cmd=='xref':
        t=int(sys.argv[3],8)
        print("xrefs to %06o:"%t, [oct(a) for a in xref(mem,t)])
    elif cmd=='words':
        a=int(sys.argv[3],8); n=int(sys.argv[4]) if len(sys.argv)>4 else 16
        for i in range(n):
            print("%06o: %06o"%(a+i, mem[a+i]))
