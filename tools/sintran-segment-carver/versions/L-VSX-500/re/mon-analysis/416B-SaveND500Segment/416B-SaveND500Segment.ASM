; ================================================================
; GENERATED single-region slice of ../../segments-ref/030-S3SM5/
;   (canonical bin ../../../segments/030-S3SM5.bin)
;   at byte-offset 48496 (0xbd70), length 134 bytes.
;   Regenerate from canonical - do NOT hand-edit.
; ================================================================
; MON 416B (wsegn) - 416B-SaveND500Segment
; ND-500 System Monitor handler - REAL SINTRAN L machine code
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), big-endian, as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*416B = 0x027c
;                  vector value (16-bit BE)  = 0xbd70
; Entry (file off): 0xbd70   End (next entry): 0xbdf6   Length: 134 bytes
; Tool           : nd500-dis -a -noansi -s bd70
; ================================================================

0000BD70: 2C 44 B3                      d move       b.0x10,r.0xCC
0000BD73: 04 F2 0F                      by1 :=       DESC(r3) $0xF
0000BD76: C7 0E A8                      if >< go     $0xEA8
0000BD79: 03                            noop
0000BD7A: F1                    ??? ; opcode 0x00F1
0000BD7B: 03                            noop
0000BD7C: AA 2D CC 4D D9 06 09 C2 2C 46 B3    w3 mulad     $0x2D,$<double>
0000BD87: 04 F2 1F                      by1 :=       DESC(r3) $0x1F
0000BD8A: C7 0E A8                      if >< go     $0xEA8
0000BD8D: 03                            noop
0000BD8E: F1                    ??? ; opcode 0x00F1
0000BD8F: 04 AA                         by1 :=       r.0xA8
0000BD91: 23 CC 4D D9 09 09 C1 2C 48 B3 w4 =:        $<double>
0000BD9B: 04 F2 0F                      by1 :=       DESC(r3) $0xF
0000BD9E: C7 0E A8                      if >< go     $0xEA8
0000BDA1: 03                            noop
0000BDA2: F1                    ??? ; opcode 0x00F1
0000BDA3: 05 AA                         by2 :=       r.0xA8
0000BDA5: 19 CC 4D D9 05 79 C1 09 C1 2C 4A    by move      $<double>,b.0x28
0000BDB0: B3 04 F2 1F C7 0E A8 03 F1    w4 cind      $0x4,DESC(r3) $0x1F,@b.0xEA803F1
0000BDB9: 06 AA                         by3 :=       r.0xA8
0000BDBB: 0E CC 4D 79 C1 09 C1 54 41 49 w3 :=        $<double>
0000BDC5: C2 CC 69 49 C1                go           $0xFFFFFFFFCC6949C1
0000BDCA: D6 67                         if >>= go    $0x67
0000BDCC: CC 75                         if >= go     $0x75
0000BDCE: F2                    ??? ; opcode 0x00F2
0000BDCF: 01                    ??? ; opcode 0x0001
0000BDD0: BA 04 C9 00                   entsn        $0x4,r.0x0
0000BDD4: AA 03 6B                      w3 mulad     $0x3,b.0xAC
0000BDD7: AD 6C A6 6A                   w2 lind      b.0xB0,r.0x98,b.0xA8
0000BDDB: DA 2C                         if <<= go    $0x2C
0000BDDD: 42 F2 FF C4 31 A8 07          by test      DESC(r3) r4.(0xFFFFFFFFC431A807)
0000BDE4: B3 04 F2 0F C7 0E A8 03 F1    w4 cind      $0x4,DESC(r3) $0xF,@b.0xEA803F1
0000BDED: 02                            bp
0000BDEE: AA 08 D8 86 4C                w3 mulad     $0x8,b.0xFFFFFFFFFFFF864C+
0000BDF3: 43 D6 68                      h test       b.0x68+
