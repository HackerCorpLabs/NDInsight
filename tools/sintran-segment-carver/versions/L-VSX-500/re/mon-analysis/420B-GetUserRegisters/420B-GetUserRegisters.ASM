; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 48655 (0xBE0F) length 448 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a single contiguous ND-500 region (routing proven).
; ================================================================
; MON 420B (GRBLK) - 420B-GetUserRegisters
; ND-500 System Monitor handler - real SINTRAN L machine code
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), as carved from the native segment
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*420B = 0x0280
;                  vector value (16-bit)     = 0xBE0F  (points here directly)
; Entry (file off): 0xBE0F   End (next slot 421B): 0xBFCF   Length: 448 bytes
;   Manual behaviour: copy back the 39 saved ND-500 registers (154 bytes / 77
;   words) that SwitchUserBreak (405B) captured on an ESCAPE termination, into a
;   caller buffer.
;   NOTE: the 0x60 vector value 0xBE0F points AT this offset directly (no +5
;         correction). The stream does NOT decode as a single clean subroutine
;         (several undecodable opcodes, no ENT* at entry), so the instruction
;         alignment is NOT proven. HOWEVER the region shows a REPEATING unrolled
;         pattern with DESCENDING immediates (... $0x1A,$0x18,$0x16 ... near the
;         end $0x35,$0x33 ...) bracketed by 'retd'/'rett' returns - structurally
;         consistent with an unrolled register-copy loop over the 39 registers.
;         The RAW BYTES are ground truth; individual mnemonics are UNVERIFIED.
; Tool           : nd500-dis -a -noansi -s be0f
; ================================================================

0000BE0F: 08 A8                         h1 :=        r.0xA0
0000BE11: 02                            bp
0000BE12: CC 4D                         if >= go     $0x4D
0000BE14: 09 80                         h2 :=        r.0x0
0000BE16: 2C 20 F2 04                   d move       $0x20,DESC(r3) $0x4
0000BE1A: CC 31                         if >= go     $0x31
0000BE1C: CE 6D                         if <= go     $0x6D
0000BE1E: BA 1A AA                      entsn        $0x1A,r.0xA8
0000BE21: 1A 21 81                      w move       $0x21,r.0x4
0000BE24: 51 80                         w decr       r.0x0
0000BE26: BA 18 AA                      entsn        $0x18,r.0xA8
0000BE29: 18 29                         r:=          $0x29
0000BE2B: 81                            retk
0000BE2C: 51 80                         w decr       r.0x0
0000BE2E: BA 16 AA                      entsn        $0x16,r.0xA8
0000BE31: 12 51                         f3 :=        b.0x44
0000BE33: F8                    ??? ; opcode 0x00F8
0000BE34: 49 80                         h stz        r.0x0
0000BE36: F5                    ??? ; opcode 0x00F5
0000BE37: 01                    ??? ; opcode 0x0001
0000BE38: DD 7F CC 6C 29                entf         $0x7FCC6C29
0000BE3D: CB C6 47                      if < go      $0xFFFFFFFFFFFFC647
0000BE40: 51 F8 59 80                   w decr       r1.(0x5980)
0000BE44: F1                    ??? ; opcode 0x00F1
0000BE45: 27 C5 80                      f4 =:        @b.0xFFFFFFFFFFFFFF80
0000BE48: 48 0A                         by stz       $0xA
0000BE4A: D6 69                         if >>= go    $0x69
0000BE4C: C2 05 AA 03 AA                go           $0x5AA03AA
0000BE51: 07 67                         by4 :=       b.0x9C
0000BE53: E6 6B                         w3 and       b.0xAC
0000BE55: DE 57 B7                      entfn        $0x57,r.0xDC
0000BE58: 6C 31                         w1 *         $0x31
0000BE5A: 67 A7                         f4 -         r.0x9C
0000BE5C: FC 78 6A DA 2C 22 F2 06       w div3       b.0xA8,b.0x2C22+,DESC(r3) $0x6
0000BE64: BA 1B AA                      entsn        $0x1B,r.0xA8
0000BE67: 1B 2C 40                      f move       $0x2C,b.0x0
0000BE6A: B2 03 F1 01 AA                w3 cind      $0x3,DESC(r2) $0x1,r.0xA8
0000BE6F: 18 CC 4D 09 E8 BA 16 C9 00 49 r:=          $<double>
0000BE79: F6                    ??? ; opcode 0x00F6
0000BE7A: D6 6A                         if >>= go    $0x6A
0000BE7C: C4 01                         if = go      $0x1
0000BE7E: AA 12 31                      w3 mulad     $0x12,$0x31
0000BE81: C0 29                         go           $0x29
0000BE83: CB BA 10                      if < go      $0xFFFFFFFFFFFFBA10
0000BE86: 09 CA 39 C0                   h2 :=        r.0x39C0
0000BE8A: 59 CA 34 00                   f2 +         r.0x3400
0000BE8E: 59 C9 2C                      f2 +         r.0x2C
0000BE91: 22 24                         w3 =:        $0x24
0000BE93: 0D F1 06                      w2 :=        DESC(r2) $0x6
0000BE96: 0C 0F                         w1 :=        $0xF
0000BE98: AA 07 57                      w3 mulad     $0x7,b.0x5C
0000BE9B: C1 6C 31                      go           $0x6C31
0000BE9E: 6B AD                         d4 -         r.0xB4
0000BEA0: 5F F7 6B                      d4 +         r4.(0x6B)
0000BEA3: DE 3C 03                      entfn        $0x3C,$0x3
0000BEA6: 6A C2 2C 42                   d3 -         b.0x2C42
0000BEAA: B3 07 50 0E                   w4 cind      $0x7,b.0x40,$0xE
0000BEAE: C2 31 A8 04 50                go           $0x31A80450
0000BEB3: 0C C2 31 A8                   w1 :=        b.0x31A8
0000BEB7: 03                            noop
0000BEB8: F1                    ??? ; opcode 0x00F1
0000BEB9: 02                            bp
0000BEBA: AA 09 4C                      w3 mulad     $0x9,b.0x30
0000BEBD: 41 D6 6B                      bi test      b.0x6B+
0000BEC0: F2                    ??? ; opcode 0x00F2
0000BEC1: 01                    ??? ; opcode 0x0001
0000BEC2: BA 06 C9 00                   entsn        $0x6,r.0x0
0000BEC6: AA 05 04                      w3 mulad     $0x5,$0x4
0000BEC9: 00                    ??? ; opcode 0x0000
0000BECA: 06 00                         by3 :=       $0x0
0000BECC: 6B AD                         d4 -         r.0xB4
0000BECE: 6C A6                         w1 *         r.0x98
0000BED0: 6A DA 2C 40                   d3 -         b.0x2C40+
0000BED4: B2 03 F1 01 AA                w3 cind      $0x3,DESC(r2) $0x1,r.0xA8
0000BED9: 18 BA                         r:=          r.0xE8
0000BEDB: 18 C9 00                      r:=          r.0x0
0000BEDE: 11 C1 59                      f2 :=        b.0x59
0000BEE1: C9 2C 42                      if > go      $0x2C42
0000BEE4: B2 03 F1 02 AA                w3 cind      $0x3,DESC(r2) $0x2,r.0xA8
0000BEE9: 10 09                         f1 :=        $0x9
0000BEEB: C0 2C                         go           $0x2C
0000BEED: 44 B3                         w test       r.0xCC
0000BEEF: 04 F2 0F                      by1 :=       DESC(r3) $0xF
0000BEF2: C7 0E A8                      if >< go     $0xEA8
0000BEF5: 03                            noop
0000BEF6: F1                    ??? ; opcode 0x00F1
0000BEF7: 03                            noop
0000BEF8: AA 08 49                      w3 mulad     $0x8,b.0x24
0000BEFB: C1 51 C0                      go           $0x51C0
0000BEFE: D6 6C                         if >>= go    $0x6C
0000BF00: F2                    ??? ; opcode 0x00F2
0000BF01: FF C4                 ??? ; opcode 0xFFC4
0000BF03: 35 AA                         w2 comp      r.0xA8
0000BF05: 04 AA                         by1 :=       r.0xA8
0000BF07: 04 6B                         by1 :=       b.0xAC
0000BF09: AD 82 48 6B                   w2 lind      r.0x8,b.0x20,b.0xAC
0000BF0D: DE 6A DA BA 24                entfn        $0x6A,b.0xFFFFFFFFFFFFBA24+
0000BF12: C9 00 2C                      if > go      $0x2C
0000BF15: 46 C0                         d test       [UNKNOWN:0xC0]
0000BF17: 05 AA                         by2 :=       r.0xA8
0000BF19: 21 BA                         w2 =:        r.0xE8
0000BF1B: 21 C9 00                      w2 =:        r.0x0
0000BF1E: 11 EB 59 C9                   f2 :=        @b.0x59C9+
0000BF22: 2C 40 B2                      d move       b.0x0,r.0xC8
0000BF25: 03                            noop
0000BF26: F1                    ??? ; opcode 0x00F1
0000BF27: 01                    ??? ; opcode 0x0001
0000BF28: AA 1B CC 4D 09 E8 2C 42 B2 03 F1    w3 mulad     $0x1B,$<double>
0000BF33: 02                            bp
0000BF34: AA 15 CC 4D 09 E9 2C 44 B2 03 F1    w3 mulad     $0x15,$<double>
0000BF3F: 03                            noop
0000BF40: AA 0F CC 4D 09 EA 2C 48 B2 03 F1    w3 mulad     $0xF,$<double>
0000BF4B: 05 AA                         by2 :=       r.0xA8
0000BF4D: 09 CC 4D 09 EC 49 F6 D6 6D AA h2 :=        $<double>
0000BF57: 05 5F                         by2 :=       b.0x7C
0000BF59: F7                    ??? ; opcode 0x00F7
0000BF5A: 6B F1 82                      d4 -         DESC(r2) r.0x8
0000BF5D: 48 6B                         by stz       b.0xAC
0000BF5F: AD 6A DA 2C 40 C0             w2 lind      b.0xA8,b.0x2C40+,[UNKNOWN:0xC0]
0000BF65: 05 AA                         by2 :=       r.0xA8
0000BF67: 5B BA                         f4 +         r.0xE8
0000BF69: 5B C9 00                      f4 +         r.0x0
0000BF6C: 4A 5A                         w stz        b.0x68
0000BF6E: B2 0A 49 36                   w3 cind      $0xA,b.0x24,$0x36
0000BF72: F5                    ??? ; opcode 0x00F5
0000BF73: FF B2                 ??? ; opcode 0xFFB2
0000BF75: 07 C4 06 A8 04 F1             by4 :=       $0x6A804F1
0000BF7B: 01                    ??? ; opcode 0x0001
0000BF7C: C0 35                         go           $0x35
0000BF7E: A8 02 51                      w1 mulad     $0x2,b.0x44
0000BF81: 36 11                         w3 comp      $0x11
0000BF83: 80                            ret
0000BF84: C0 01                         go           $0x1
0000BF86: AA 4E 59                      w3 mulad     b.0x38,b.0x64
0000BF89: C9 2C 44                      if > go      $0x2C44
0000BF8C: B3 04 50 4B                   w4 cind      $0x4,b.0x40,b.0x2C
0000BF90: C7 0E A8                      if >< go     $0xEA8
0000BF93: 03                            noop
0000BF94: F1                    ??? ; opcode 0x00F1
0000BF95: 03                            noop
0000BF96: AA 48 CC 4D 09 82 2C 46 B2 03 F1    w3 mulad     b.0x20,$<double>
0000BFA1: 04 AA                         by1 :=       r.0xA8
0000BFA3: 42 CC 4D 09 81 2C 42 B3 06 51 by test      $<double>
0000BFAD: 82                            retd
0000BFAE: C7 31 A8                      if >< go     $0x31A8
0000BFB1: 03                            noop
0000BFB2: CC 4D                         if >= go     $0x4D
0000BFB4: 09 82                         h2 :=        r.0x8
0000BFB6: 2C 22 F2 04                   d move       $0x22,DESC(r3) $0x4
0000BFBA: CC 31                         if >= go     $0x31
0000BFBC: CE 6D                         if <= go     $0x6D
0000BFBE: BA 35 AA                      entsn        $0x35,r.0xA8
0000BFC1: 35 21                         w2 comp      $0x21
0000BFC3: 83                            rett
0000BFC4: 51 82                         w decr       r.0x8
0000BFC6: BA 33 AA                      entsn        $0x33,r.0xA8
0000BFC9: 33 BA                         by4 comp     r.0xE8
0000BFCB: 33 C9 00                      by4 comp     r.0x0
; --- next 0x60 vector slot (421B) begins at 0xBFCF ---
