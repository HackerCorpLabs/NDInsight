; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 47846 (0xBAE6) length 82 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a proven single contiguous ND-500 region.
; ================================================================
; MON 410B (fixseg) - 410B-FixInMemory
; ND-500 System Monitor handler - real SINTRAN L machine code
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), big-endian, as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*410B = 0x0270
;                  vector value (16-bit BE)  = 0xbae1
; Entry (file off): 0xbae6   End (next entry): 0xbb38   Length: 82 bytes
;   NOTE: raw 0x60 vector value = 0xbae1 lands INSIDE the inline
;         error message "...direct transfer'" (message 0xbaaf-0xbae4,
;         terminator 0x27 @ 0xbae5). Raw value 0xbae1 = the 's' in "transfer".
;         Corrected entry = 0xbae6 : first instruction boundary right after
;         the string terminator, immediately preceding the 411B entry (0xbb38).
; Tool           : nd500-dis -a -noansi -s bae6
; ================================================================

0000BAE6: 48 13                         by stz       $0x13
0000BAE8: BA 13 2C                      entsn        $0x13,$0x2C
0000BAEB: 10 BA                         f1 :=        r.0xE8
0000BAED: 12 48                         f3 :=        b.0x20
0000BAEF: 12 BA                         f3 :=        r.0xE8
0000BAF1: 0F 59                         w4 :=        b.0x64
0000BAF3: 84                            bi1 clr
0000BAF4: 4C 06                         d stz        $0x6
0000BAF6: D9 01 BA                      if << go     $0x1BA
0000BAF9: 0E 49                         w3 :=        b.0x24
0000BAFB: 11 09                         f2 :=        $0x9
0000BAFD: 10 48                         f1 :=        b.0x20
0000BAFF: 0C AA                         w1 :=        r.0xA8
0000BB01: 0C 6B                         w1 :=        b.0xAC
0000BB03: AD 5F 93 5F                   w2 lind      b.0x7C,r.0x4C,b.0x7C
0000BB07: 36 64                         w3 comp      b.0x90
0000BB09: 85                            bi2 clr
0000BB0A: 62 B3                         w3 -         r.0xCC
0000BB0C: 47 0E                         f set1       $0xE
0000BB0E: 62 AE                         w3 -         r.0xB8
0000BB10: 61 32                         w2 -         $0x32
0000BB12: 46 CB 61 8E 02 09             d test       r.0x618E0209
0000BB18: 6A FB 21 94                   d3 -         r4.(0x2194)
0000BB1C: 59 C9 2C                      f2 +         r.0x2C
0000BB1F: 46 FA 01 AA                   d test       r3.(0x1AA)
0000BB23: 1C 21                         by1 =:       $0x21
0000BB25: 82                            retd
0000BB26: 21 34                         w2 =:        $0x34
0000BB28: DD FF 59 84 54                entf         $0xFFFFFFFFFF598454
0000BB2D: 06 F8 18 C3                   by3 :=       r1.(0x18C3)
0000BB31: B0 C4 01 A8 1D 49 DC B2 0F BA 11 48       w1 cind      $0x1A81D49,b.0xFFFFFFFFB20FBA11+,b.0x20
