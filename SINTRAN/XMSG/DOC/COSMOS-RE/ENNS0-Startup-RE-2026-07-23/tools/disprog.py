#!/usr/bin/env python3
"""Disassemble a range of a loaded :PROG flat image using nd100dis.decode."""
import sys
from prog_load import load_prog
import nd100dis

def run(path, start, count):
    mem,info=load_prog(path)
    a=start
    for _ in range(count):
        w=mem[a] if a<len(mem) else 0
        print("%06o  %06o   %s"%(a,w,nd100dis.decode(w,a)))
        a=(a+1)&0xFFFF

if __name__=='__main__':
    run(sys.argv[1], int(sys.argv[2],8), int(sys.argv[3]) if len(sys.argv)>3 else 40)
