; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; whole-segment disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 42925 (0xA7AD) length 120 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a single contiguous ND-500 region.
; ================================================================
; MON 264B (octal) ND500ReadFile (500RF) - real SINTRAN L machine code
; ND-500 System Monitor handler carved from the S3SM5 segment image.
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*decimal(264B) = 0x60 + 2*180 = 0x01C8
;                  vector value (16-bit)  = 0xA7AD  (handler file offset)
; Entry (file off): 0xA7AD   End (next entry 265B): 0xA825   Length: 120 bytes
;   The 264B slot sits inside the contiguous 260B-277B block of real handler
;   offsets in the S3SM5 vector table (260=0xA75D 261=0xA773 262=0xA79F
;   264=0xA7AD 265=0xA825 266=0xA89D 270=0xA8AE ...), so the ROUTING is
;   byte-verified the same way as the 410B-421B block.
;   COHERENCE: the linear decode from the raw vector offset is only PARTLY
;   coherent - several 'go' targets are out of range and some bytes decode as
;   unknown opcodes, so 0xA7AD is not a proven instruction boundary. The RAW
;   BYTES are ground truth; the mnemonics below are unreliable where marked.
; Tool           : nd500-dis -a -noansi -s a7ad
; Addresses are hex file byte offsets into 030-S3SM5.bin.
; ================================================================

0000A7AD: 62 F2 30                      w3 -         DESC(r3) $0x30
0000A7B0: C2 35 A8 05 F2                go           $0x35A805F2   ; UNVERIFIED target (misalignment)
0000A7B5: 39 C2 2E A8                   f2 comp      b.0x2EA8
0000A7B9: 02                            bp
0000A7BA: CD 62 F2                      if >= go     $0x62F2
0000A7BD: 2D C4 35 A8 02 CC 62          by comp2     $0x35A802CC,b.0x88
0000A7C4: CD 62 51                      if >= go     $0x6251
0000A7C7: 84                            bi1 clr
0000A7C8: 59 86                         f2 +         r.0x18
0000A7CA: C5 80 F7                      if = go      $0xFFFFFFFFFFFF80F7
0000A7CD: 01                            ??? ; opcode 0x0001   ; UNVERIFIED
0000A7CE: 19 86 F2 68                   by move      r.0x18,DESC(r3) b.0xA0
0000A7D2: C3 37 A8 02 CC 62             call         $0x37A802CC,b.0x88
0000A7D8: CD 62 2C                      if >= go     $0x622C
0000A7DB: 40 B3 04                      d comp2      r.0xCC,$0x4
0000A7DE: 50 4C                         f incr       b.0x30
0000A7E0: C7 0E A8                      if >< go     $0xEA8
0000A7E3: 03                            noop
0000A7E4: 48 49                         by stz       b.0x24
0000A7E6: A8 02 CC 4D 09 80 2C 20 F2 04 CC    w1 mulad     $0x2,$<double>
0000A7F1: 31 CE 6D BA                   by2 comp     $0x6DBA
0000A7F5: 42 AA                         by test      r.0xA8
0000A7F7: 42 21                         by test      $0x21
0000A7F9: 81                            retk                       ; return-to-caller opcode
0000A7FA: 51 80                         w decr       r.0x0
0000A7FC: BA 40 AA                      entsn        b.0x0,r.0xA8   ; frame prologue (mid-body)
0000A7FF: 40 29 81                      d comp2      $0x29,r.0x4
0000A802: 51 80                         w decr       r.0x0
0000A804: BA 3E AA                      entsn        $0x3E,r.0xA8   ; frame prologue (mid-body)
0000A807: 3A 51                         f3 comp      b.0x44
0000A809: F8                            ??? ; opcode 0x00F8   ; UNVERIFIED
0000A80A: 49 80                         h stz        r.0x0
0000A80C: F5                            ??? ; opcode 0x00F5   ; UNVERIFIED
0000A80D: 01                            ??? ; opcode 0x0001   ; UNVERIFIED
0000A80E: DD 7F CC 6C 29                entf         $0x7FCC6C29
0000A813: CB C6 47                      if < go      $0xFFFFFFFFFFFFC647
0000A816: 51 F8 59 80                   w decr       r1.(0x5980)
0000A81A: F1                            ??? ; opcode 0x00F1   ; UNVERIFIED
0000A81B: 27 C5 80                      f4 =:        @b.0xFFFFFFFFFFFFFF80
0000A81E: F1                            ??? ; opcode 0x00F1   ; UNVERIFIED
0000A81F: 01                            ??? ; opcode 0x0001   ; UNVERIFIED
0000A820: 58 31                         f1 +         $0x31
0000A822: BA 31 A8                      entsn        $0x31,r.0xA0   ; runs up to the 265B entry (0xA825)
