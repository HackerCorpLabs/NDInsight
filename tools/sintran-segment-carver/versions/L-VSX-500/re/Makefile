# Makefile for the SINTRAN III password tools (Linux)
#   make        - build all tools
#   make clean  - remove built binaries
#
# See PASSWORD-ALGORITHM.md for the algorithm and usage.

CC      := gcc
CFLAGS  := -O2 -Wall -Wextra
PROGS   := passcrack passdb

.PHONY: all clean
all: $(PROGS)

passcrack: sintran-passcrack.c
	$(CC) $(CFLAGS) -o $@ $<

passdb: sintran-passdb.c
	$(CC) $(CFLAGS) -o $@ $<

clean:
	rm -f $(PROGS)
