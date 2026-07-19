; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 39133 (0x98DD) length 90 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a proven single contiguous ND-500 region.
; ================================================================
; MON 412B (FSCNT / FileAsSegment) - 412B-FileAsSegment
; ND-500 System Monitor handler - real SINTRAN L bytes (ND-500)
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), big-endian, as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*412B = 0x0274
;                  vector value (16-bit BE)  = 0x98dd
; Entry (file off): 0x98dd   End (next entry): 0x9937   Length: 90 bytes
;   NOTE: this entry 0x98dd is SHARED - the vector slot for MON 127B
;         (octal 127 -> slot 0x60 + 2*127B = 0x010e) holds the SAME
;         value 0x98dd (byte-verified). Both "connect file as segment"
;         entries route here; which call OWNS this body is INFERRED
;         (see the folder README caveats).
;   The region end 0x9937 = the next-higher handler entry in the whole
;   0x60 vector table (address-sorted successor of 0x98dd), the same
;   next-entry rule used for 410B/411B.
; Tool           : nd500-dis -a -noansi -s 98dd
; ================================================================

000098DD: 13 AA                         f4 :=        r.0xA8
000098DF: 13 BA                         f4 :=        r.0xE8
000098E1: 13 AA                         f4 :=        r.0xA8
000098E3: 13 29                         f4 :=        $0x29
000098E5: CB BA 12                      if < go      $0xFFFFFFFFFFFFBA12
000098E8: 09 CA 2C 40                   h2 :=        r.0x2C40
000098EC: BA 10 C9 00                   entsn        $0x10,r.0x0
000098F0: 59 C9 4C                      f2 +         r.0x4C
000098F3: 47 CC 69 4C 43 59 CA D6 04 AA f set1       $<double>
000098FD: 09 6B                         h2 :=        b.0xAC
000098FF: F1                    ??? ; opcode 0x00F1
00009900: 6B AD                         d4 -         r.0xB4
00009902: 57 B7                         w4 +         r.0xDC
00009904: 6C 31                         w1 *         $0x31
00009906: 67 A7                         f4 -         r.0x9C
00009908: 6B DE 3C 03 82 48             d4 -         b.0x3C038248+
0000990E: 6A D9 2A 02                   d3 -         b.0x2A02+
00009912: AA 02 08                      w3 mulad     $0x2,$0x8
00009915: 5E 6A                         d3 +         b.0xA8
00009917: DC 2C 40 B3 04 50 20          init         $0x2C40B304,b.0x40,$0x20
0000991E: C7 0E A8                      if >< go     $0xEA8
00009921: 03                            noop
00009922: 48 1D                         by stz       $0x1D
00009924: A8 02 CC 4D 09 80 F2 04 2C 20 CC    w1 mulad     $0x2,$<double>
0000992F: 31 CE 6D BA                   by2 comp     $0x6DBA
00009933: 16 AA                         d3 :=        r.0xA8
00009935: 16 51                         d3 :=        b.0x44
