; ================================================================
; GENERATED single-region slice of segments-ref/030-S3SM5/030-S3SM5.bin
; at byte-offset 47928 (0xbb38) length 59 - regenerate from canonical,
; do not hand-edit. Body is one proven contiguous region.
; ================================================================
; MON 411B (unfix) - 411B-MemoryUnfix
; ND-500 System Monitor handler - real SINTRAN L bytes (ND-500)
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), big-endian, as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*411B = 0x0272
;                  vector value (16-bit BE)  = 0xbb38
; Entry (file off): 0xbb38   End (next entry): 0xbb73   Length: 59 bytes
; Tool           : nd500-dis -a -noansi -s bb38
; ================================================================

0000BB38: B2 0F BA 11                   w3 cind      $0xF,r.0xE8,$0x11
0000BB3C: 48 11                         by stz       $0x11
0000BB3E: BA 11 48                      entsn        $0x11,b.0x20
0000BB41: 11 BA                         f2 :=        r.0xE8
0000BB43: 0F 4C                         w4 :=        b.0x30
0000BB45: 06 BA                         by3 :=       r.0xE8
0000BB47: 0F 48                         w4 :=        b.0x20
0000BB49: 0F BA                         w4 :=        r.0xE8
0000BB4B: 0B 59                         h4 :=        b.0x64
0000BB4D: C9 2C 46                      if > go      $0x2C46
0000BB50: BA 0C 49                      entsn        $0xC,b.0x24
0000BB53: 11 09                         f2 :=        $0x9
0000BB55: 10 48                         f1 :=        b.0x20
0000BB57: 0A AA                         h3 :=        r.0xA8
0000BB59: 0A 9A                         h3 :=        r.0x68
0000BB5B: D3 64 85                      if -k go     $0x6485
0000BB5E: 46 DE 62 AE 46 CB             d test       b.0x62AE46CB+
0000BB64: 61 8E                         w2 -         r.0x38
0000BB66: 46 D7 61                      d test       b.0x61+
0000BB69: 32 02                         by3 comp     $0x2
0000BB6B: 07 6A                         by4 :=       b.0xA8
0000BB6D: FB                    ??? ; opcode 0x00FB
0000BB6E: 09 8F                         h2 :=        r.0x3C
0000BB70: 2C 0A 21                      d move       $0xA,$0x21
