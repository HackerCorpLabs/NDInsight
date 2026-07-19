; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; whole-segment disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 43045 (0xA825) length 120 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a single contiguous ND-500 region.
; ================================================================
; MON 265B (octal) ND500WriteFile (500WF) - real SINTRAN L machine code
; ND-500 System Monitor handler carved from the S3SM5 segment image.
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*decimal(265B) = 0x60 + 2*181 = 0x01CA
;                  vector value (16-bit)  = 0xA825  (handler file offset)
; Entry (file off): 0xA825   End (next entry 266B): 0xA89D   Length: 120 bytes
;   The 265B slot sits inside the contiguous 260B-277B block of real handler
;   offsets in the S3SM5 vector table, so the ROUTING is byte-verified the same
;   way as the 410B-421B block. It is the sibling of the 264B (read) handler.
;   COHERENCE: the linear decode from the raw vector offset is only PARTLY
;   coherent - several 'go' targets are out of range and some bytes decode as
;   unknown opcodes, so 0xA825 is not a proven instruction boundary. The RAW
;   BYTES are ground truth; the mnemonics below are unreliable where marked.
; Tool           : nd500-dis -a -noansi -s a825
; Addresses are hex file byte offsets into 030-S3SM5.bin.
; ================================================================

0000A825: 27 01                         f4 =:        $0x1
0000A827: 86                            bi3 clr
0000A828: 01                            ??? ; opcode 0x0001   ; UNVERIFIED
0000A829: 85                            bi2 clr
0000A82A: F8                            ??? ; opcode 0x00F8   ; UNVERIFIED
0000A82B: 00                            ??? ; opcode 0x0000   ; UNVERIFIED
0000A82C: 4C 00                         d stz        $0x0
0000A82E: F8                            ??? ; opcode 0x00F8   ; UNVERIFIED
0000A82F: 80                            ret                        ; return opcode
0000A830: 09 83                         h2 :=        r.0xC
0000A832: 49 F8 60 29                   h stz        r1.(0x6029)
0000A836: 09 84                         h2 :=        r.0x10
0000A838: B8 8D                         ents         r.0x34         ; enter-subroutine (frame)
0000A83A: A8 04 B8                      w1 mulad     $0x4,r.0xE0
0000A83D: C5 A8 1A                      if = go      $0xFFFFFFFFFFFFA81A
0000A840: A8 FC 01 85 49 F8 09          w1 mulad     r1.(0x18549F8),$0x9
0000A847: 83                            rett                       ; return opcode
0000A848: B8 A3                         ents         r.0x8C         ; enter-subroutine (frame)
0000A84A: A8 02 A8                      w1 mulad     $0x2,r.0xA0
0000A84D: FE B8 BC                      w1 wpconv    r.0xF0
0000A850: A8 04 B8                      w1 mulad     $0x4,r.0xE0
0000A853: 9E                            ??? ; opcode 0x009E   ; UNVERIFIED
0000A854: C9 00 A8                      if > go      $0xA8
0000A857: FC F1 27 B8                   f2 mulad     $0x27,r.0xE0
0000A85B: B6                            ??? ; opcode 0x00B6   ; UNVERIFIED
0000A85C: C9 00 BA                      if > go      $0xBA
0000A85F: 15 49                         d2 :=        b.0x24
0000A861: 84                            bi1 clr
0000A862: D6 38                         if >>= go    $0x38
0000A864: BA 13 48                      entsn        $0x13,b.0x20   ; frame prologue (mid-body)
0000A867: 13 59                         f4 :=        b.0x64
0000A869: F8                            ??? ; opcode 0x00F8   ; UNVERIFIED
0000A86A: 0C 00                         w1 :=        $0x0
0000A86C: 49 F8 D6 0A                   h stz        r1.(0xFFFFFFFFFFFFD60A)
0000A870: AA 0F F1 01                   w3 mulad     $0xF,DESC(r2) $0x1
0000A874: AA 0E 02                      w3 mulad     $0xE,$0x2
0000A877: 00                            ??? ; opcode 0x0000   ; UNVERIFIED
0000A878: 67 E6 6B                      f4 -         @b.0x6B+
0000A87B: DE 57 B7                      entfn        $0x57,r.0xDC
0000A87E: 6C 31                         w1 *         $0x31
0000A880: 67 A7                         f4 -         r.0x9C
0000A882: 93                            w4 neg
0000A883: 8B                            f4 clr
0000A884: 60 49                         w1 -         b.0x24
0000A886: 02                            bp
0000A887: 02                            bp
0000A888: 09 0C                         h2 :=        $0xC
0000A88A: 08 BB                         h1 :=        r.0xEC
0000A88C: 27 27                         f4 =:        $0x27
0000A88E: 6A D9 6B AD                   d3 -         b.0x6BAD+
0000A892: 2C 40 C0                      d move       b.0x0,[UNKNOWN:0xC0]   ; UNVERIFIED
0000A895: 05 AA                         by2 :=       r.0xA8
0000A897: 05 BA                         by2 :=       r.0xE8
0000A899: 05 C9 00                      by2 :=       r.0x0
0000A89C: D6 39                         if >>= go    $0x39          ; runs up to the 266B entry (0xA89D)
