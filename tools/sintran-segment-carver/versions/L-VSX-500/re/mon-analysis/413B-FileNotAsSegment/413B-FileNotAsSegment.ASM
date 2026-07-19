; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 47987 (0xBB73) length 43 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a proven single contiguous ND-500 region.
; ================================================================
; MON 413B (FSCDNT / FileNotAsSegment) - 413B-FileNotAsSegment
; ND-500 System Monitor handler - real SINTRAN L bytes (ND-500)
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), big-endian, as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*413B = 0x0276
;                  vector value (16-bit BE)  = 0xbb73
; Entry (file off): 0xbb73   End (next entry): 0xbb9e   Length: 43 bytes
;   The region end 0xbb9e = the vector target of MON 414B (slot 0x0278),
;   the address-sorted successor of 0xbb73 in the 0x60 vector table -
;   the same next-entry rule used for 410B/411B.
;   The trailing entsn at 0xbb9c is a 3-byte op whose last byte falls on
;   the 0xbb9e boundary; it is shown because it STARTS inside the region.
; Tool           : nd500-dis -a -noansi -s bb73
; ================================================================

0000BB73: DA 29                         if <<= go    $0x29
0000BB75: 94                            f1 neg
0000BB76: C8 0D                         if > go      $0xD
0000BB78: 61 8F                         w2 -         r.0x3C
0000BB7A: C8 0D                         if > go      $0xD
0000BB7C: BA 19 FA 90 A8                entsn        $0x19,r3.(0xFFFFFFFFFFFF90A8)
0000BB81: 22 49                         w3 =:        b.0x24
0000BB83: DC B2 13 BA 15 48 15          init         $0xFFFFFFFFB213BA15,b.0x20,$0x15
0000BB8A: BA 15 48                      entsn        $0x15,b.0x20
0000BB8D: 15 BA                         d2 :=        r.0xE8
0000BB8F: 13 59                         f4 :=        b.0x64
0000BB91: 84                            bi1 clr
0000BB92: 2C 10 BA                      d move       $0x10,r.0xE8
0000BB95: 12 48                         f3 :=        b.0x20
0000BB97: 12 BA                         f3 :=        r.0xE8
0000BB99: 0E 49                         w3 :=        b.0x24
0000BB9B: 8F                            d4 clr
0000BB9C: BA 10 48                      entsn        $0x10,b.0x20
