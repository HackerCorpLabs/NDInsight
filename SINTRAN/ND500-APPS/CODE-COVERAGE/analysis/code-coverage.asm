; ═══════════════════════════════════════════════════════════════
; ND-500 Disassembly
; ═══════════════════════════════════════════════════════════════
; File: code-coverage/code-coverage.dom
;
; File Type:    Domain (DOM)
; Domain Type:  SINTRAN III Root Domain
; Linker:       v97.2
; Entry Point:  0x08001CA9
; Segments:     1 used
;
; Segment  1:
;   Program: 0x3F42 bytes at virtual 0x00000000
;   Data:    0x7035 bytes at virtual 0x00000000
;
; ═══════════════════════════════════════════════════════════════

; -- Segment 1 Program at 0x08000000 --
;
08001CA9: DC 08 00 4E 50 CF 00 00 00 94 CE 10 00          init         $0x8004E50,$0x94,$0x1000
08001CB6: C0 0F                         go           $0xF
08001CB8: 9C                            entd
08001CB9: FD C0 59                      l=:          b.0x64
08001CBC: 20 43                         w1 =:        b.0xC
08001CBE: C1 10 BD                      go           $0x10BD
08001CC1: FE 03                         clrk
08001CC3: B4 59                         jumpg        b.0x64
08001CC5: 18 42                         r:=          b.0x8
08001CC7: 4A 85                         w stz        r.0x14
08001CC9: C3 08 00 3D 9E 00             call         $0x8003D9E,$0x0
08001CCF: D2 08                         if -k go     $0x8
08001CD1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001CD7: 18 42                         r:=          b.0x8
08001CD9: 4D 85                         w set1       r.0x14
08001CDB: FD 20 C4 08 00 62 64 86 0C    by bmove     $0x8006264,r.0x18,$0xC
08001CE4: FD 20 C4 08 00 62 70 89 0C    by bmove     $0x8006270,r.0x24,$0xC
08001CED: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001CF3: D2 08                         if -k go     $0x8
08001CF5: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001CFB: 18 42                         r:=          b.0x8
08001CFD: 4D 85                         w set1       r.0x14
08001CFF: FD 20 C4 08 00 62 80 86 0C    by bmove     $0x8006280,r.0x18,$0xC
08001D08: FD 20 C4 08 00 62 8C 89 0C    by bmove     $0x800628C,r.0x24,$0xC
08001D11: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001D17: D2 08                         if -k go     $0x8
08001D19: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001D1F: C0 28                         go           $0x28
08001D21: 18 42                         r:=          b.0x8
08001D23: 4D 85                         w set1       r.0x14
08001D25: FD 20 C4 08 00 62 A4 86 0C    by bmove     $0x80062A4,r.0x18,$0xC
08001D2E: FD 20 C4 08 00 62 B0 89 0C    by bmove     $0x80062B0,r.0x24,$0xC
08001D37: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001D3D: D2 08                         if -k go     $0x8
08001D3F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001D45: C0 26                         go           $0x26
08001D47: 18 42                         r:=          b.0x8
08001D49: 4D 85                         w set1       r.0x14
08001D4B: FD 20 C4 08 00 62 C8 86 0C    by bmove     $0x80062C8,r.0x18,$0xC
08001D54: FD 20 C4 08 00 62 D4 89 0C    by bmove     $0x80062D4,r.0x24,$0xC
08001D5D: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001D63: D2 08                         if -k go     $0x8
08001D65: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001D6B: 18 42                         r:=          b.0x8
08001D6D: 4D 85                         w set1       r.0x14
08001D6F: FD 20 C4 08 00 62 E4 86 0C    by bmove     $0x80062E4,r.0x18,$0xC
08001D78: FD 20 C4 08 00 62 F0 89 0C    by bmove     $0x80062F0,r.0x24,$0xC
08001D81: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001D87: D2 08                         if -k go     $0x8
08001D89: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001D8F: 18 42                         r:=          b.0x8
08001D91: 4D 85                         w set1       r.0x14
08001D93: FD 20 C4 08 00 63 38 86 0C    by bmove     $0x8006338,r.0x18,$0xC
08001D9C: FD 20 C4 08 00 63 44 89 0C    by bmove     $0x8006344,r.0x24,$0xC
08001DA5: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001DAB: D2 08                         if -k go     $0x8
08001DAD: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001DB3: 18 42                         r:=          b.0x8
08001DB5: 4D 85                         w set1       r.0x14
08001DB7: FD 20 C4 08 00 63 84 86 0C    by bmove     $0x8006384,r.0x18,$0xC
08001DC0: FD 20 C4 08 00 63 90 89 0C    by bmove     $0x8006390,r.0x24,$0xC
08001DC9: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001DCF: D2 08                         if -k go     $0x8
08001DD1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001DD7: 18 42                         r:=          b.0x8
08001DD9: 4D 85                         w set1       r.0x14
08001DDB: FD 20 C4 08 00 63 C4 86 0C    by bmove     $0x80063C4,r.0x18,$0xC
08001DE4: FD 20 C4 08 00 63 D0 89 0C    by bmove     $0x80063D0,r.0x24,$0xC
08001DED: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001DF3: D2 08                         if -k go     $0x8
08001DF5: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001DFB: 18 42                         r:=          b.0x8
08001DFD: 4D 85                         w set1       r.0x14
08001DFF: FD 20 C4 08 00 63 E0 86 0C    by bmove     $0x80063E0,r.0x18,$0xC
08001E08: FD 20 C4 08 00 63 EC 89 0C    by bmove     $0x80063EC,r.0x24,$0xC
08001E11: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001E17: D2 08                         if -k go     $0x8
08001E19: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001E1F: 18 42                         r:=          b.0x8
08001E21: 4D 85                         w set1       r.0x14
08001E23: FD 20 C4 08 00 64 28 86 0C    by bmove     $0x8006428,r.0x18,$0xC
08001E2C: FD 20 C4 08 00 64 34 89 0C    by bmove     $0x8006434,r.0x24,$0xC
08001E35: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001E3B: D2 08                         if -k go     $0x8
08001E3D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001E43: 18 42                         r:=          b.0x8
08001E45: 4D 85                         w set1       r.0x14
08001E47: FD 20 C4 08 00 64 44 86 0C    by bmove     $0x8006444,r.0x18,$0xC
08001E50: FD 20 C4 08 00 64 50 89 0C    by bmove     $0x8006450,r.0x24,$0xC
08001E59: C3 08 00 37 08 00             call         $0x8003708,$0x0
08001E5F: D2 08                         if -k go     $0x8
08001E61: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001E67: C3 08 00 00 59 00             call         $0x8000059,$0x0
08001E6D: D2 08                         if -k go     $0x8
08001E6F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001E75: 20 C4 08 00 34 DC             w1 =:        $0x80034DC
08001E7B: 18 42                         r:=          b.0x8
08001E7D: FD 20 C4 08 00 64 70 85 0C    by bmove     $0x8006470,r.0x14,$0xC
08001E86: FD 20 C4 08 00 64 7C 88 0C    by bmove     $0x800647C,r.0x20,$0xC
08001E8F: 1A C4 08 00 34 F0 8B          w move       $0x80034F0,r.0x2C
08001E96: 0D CF 08 00 35 0C             w2 :=        $0x800350C
08001E9C: 21 8C                         w2 =:        r.0x30
08001E9E: FD 20 C4 08 00 64 88 8D 0C    by bmove     $0x8006488,r.0x34,$0xC
08001EA7: 1A C4 08 00 34 C8 90          w move       $0x80034C8,r.0x40
08001EAE: C3 08 00 02 D5 00             call         $0x80002D5,$0x0
08001EB4: D2 08                         if -k go     $0x8
08001EB6: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001EBC: 18 42                         r:=          b.0x8
08001EBE: 1A 8B C4 08 00 34 F0          w move       r.0x2C,$0x80034F0
08001EC5: 1A 90 C4 08 00 34 C8          w move       r.0x40,$0x80034C8
08001ECC: 20 C4 08 00 34 60             w1 =:        $0x8003460
08001ED2: FD 20 CD 20 4D CD 2A          by bmove     $0x20,b.0x34,$0x2A
08001ED9: 4A 47                         w stz        b.0x1C
08001EDB: 4A 46                         w stz        b.0x18
08001EDD: 18 CF 08 00 35 0C             r:=          $0x800350C
08001EE3: FD 3C C9 02                   w1 laddr     r.0x2
08001EE7: 20 5A                         w1 =:        b.0x68
08001EE9: 0D 46                         w2 :=        b.0x18
08001EEB: 2D E5 68 CD 27                by comp2     @b.0x68+,$0x27
08001EF0: C4 16                         if = go      $0x16
08001EF2: FD 3F C9 02                   w4 laddr     r.0x2
08001EF6: 23 5A                         w4 =:        b.0x68
08001EF8: 06 E5 68                      by3 :=       @b.0x68+
08001EFB: 0C 47                         w1 :=        b.0x1C
08001EFD: 1E D4 34                      by3 =:       b.0x34+
08001F00: 4F 47                         w incr       b.0x1C
08001F02: BF 46 0F DB                   d loopi      b.0x18,$0xF,$0xFFFFFFFFFFFFFFDB
08001F06: 0C 47                         w1 :=        b.0x1C
08001F08: 19 CD 3A D4 34                by move      $0x3A,b.0x34+
08001F0D: 4F 47                         w incr       b.0x1C
08001F0F: 4A 46                         w stz        b.0x18
08001F11: 18 CF 08 00 35 0C             r:=          $0x800350C
08001F17: FD 3C C9 12                   w1 laddr     r.0x12
08001F1B: 20 5A                         w1 =:        b.0x68
08001F1D: 0D 46                         w2 :=        b.0x18
08001F1F: 2D E5 68 CD 27                by comp2     @b.0x68+,$0x27
08001F24: C4 16                         if = go      $0x16
08001F26: FD 3F C9 12                   w4 laddr     r.0x12
08001F2A: 23 5A                         w4 =:        b.0x68
08001F2C: 06 E5 68                      by3 :=       @b.0x68+
08001F2F: 0C 47                         w1 :=        b.0x1C
08001F31: 1E D4 34                      by3 =:       b.0x34+
08001F34: 4F 47                         w incr       b.0x1C
08001F36: BF 46 03 DB                   d loopi      b.0x18,$0x3,$0xFFFFFFFFFFFFFFDB
08001F3A: 18 42                         r:=          b.0x8
08001F3C: FD 20 C4 08 00 64 AC 85 0C    by bmove     $0x80064AC,r.0x14,$0xC
08001F45: FD 20 C4 08 00 64 B8 88 0C    by bmove     $0x80064B8,r.0x20,$0xC
08001F4E: 1A C4 08 00 34 F4 8B          w move       $0x80034F4,r.0x2C
08001F55: 0C CF 08 00 35 4C             w1 :=        $0x800354C
08001F5B: 20 8C                         w1 =:        r.0x30
08001F5D: FD 20 C4 08 00 64 C4 8D 0C    by bmove     $0x80064C4,r.0x34,$0xC
08001F66: 1A C4 08 00 34 CC 90          w move       $0x80034CC,r.0x40
08001F6D: C3 08 00 02 D5 00             call         $0x80002D5,$0x0
08001F73: D2 08                         if -k go     $0x8
08001F75: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001F7B: 18 42                         r:=          b.0x8
08001F7D: 1A 8B C4 08 00 34 F4          w move       r.0x2C,$0x80034F4
08001F84: 1A 90 C4 08 00 34 CC          w move       r.0x40,$0x80034CC
08001F8B: 20 C4 08 00 34 64             w1 =:        $0x8003464
08001F91: FD 20 C4 08 00 64 E0 85 0C    by bmove     $0x80064E0,r.0x14,$0xC
08001F9A: FD 20 C4 08 00 64 EC 88 0C    by bmove     $0x80064EC,r.0x20,$0xC
08001FA3: 1A C4 08 00 35 00 8B          w move       $0x8003500,r.0x2C
08001FAA: 0D CF 08 00 36 0C             w2 :=        $0x800360C
08001FB0: 21 8C                         w2 =:        r.0x30
08001FB2: FD 20 C4 08 00 64 F8 8D 0C    by bmove     $0x80064F8,r.0x34,$0xC
08001FBB: 1A C4 08 00 34 D8 90          w move       $0x80034D8,r.0x40
08001FC2: C3 08 00 02 D5 00             call         $0x80002D5,$0x0
08001FC8: D2 08                         if -k go     $0x8
08001FCA: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08001FD0: 18 42                         r:=          b.0x8
08001FD2: 1A 8B C4 08 00 35 00          w move       r.0x2C,$0x8003500
08001FD9: 1A 90 C4 08 00 34 D8          w move       r.0x40,$0x80034D8
08001FE0: 20 C4 08 00 34 5C             w1 =:        $0x800345C
08001FE6: 85                            bi2 clr
08001FE7: 21 C4 08 00 34 28             w2 =:        $0x8003428
08001FED: 21 C4 08 00 34 2C             w2 =:        $0x800342C
08001FF3: FD 20 CD 20 C4 08 00 29 30 CD 64    by bmove     $0x20,$0x8002930,$0x64
08001FFE: 49 C4 08 00 34 58             h stz        $0x8003458
08002004: 4A C4 08 00 34 70             w stz        $0x8003470
0800200A: 18 CF 08 00 35 4C             r:=          $0x800354C
08002010: FD 3C C9 02                   w1 laddr     r.0x2
08002014: 20 5A                         w1 =:        b.0x68
08002016: 0D C4 08 00 34 70             w2 :=        $0x8003470
0800201C: 2D E5 68 CD 27                by comp2     @b.0x68+,$0x27
08002021: C4 29                         if = go      $0x29
08002023: FD 50 C4 08 00 34 58 D2       h wconv      $0x8003458,r3
0800202B: FD 3C C9 02                   w1 laddr     r.0x2
0800202F: 20 5A                         w1 =:        b.0x68
08002031: 07 E5 68                      by4 :=       @b.0x68+
08002034: 1F E2 08 00 29 30             by4 =:       $0x8002930+
0800203A: FC 54 C4 08 00 34 58 02       h add2       $0x8003458,$0x2
08002042: BF C4 08 00 34 70 0F C8       d loopi      $0x8003470,$0xF,$0xFFFFFFFFFFFFFFC8
0800204A: FD 50 C4 08 00 34 58 D0       h wconv      $0x8003458,r1
08002052: 19 CD 3A E0 08 00 29 30       by move      $0x3A,$0x8002930+
0800205A: FC 54 C4 08 00 34 58 02       h add2       $0x8003458,$0x2
08002062: 4A C4 08 00 34 70             w stz        $0x8003470
08002068: 18 CF 08 00 35 4C             r:=          $0x800354C
0800206E: FD 3C C9 12                   w1 laddr     r.0x12
08002072: 20 5A                         w1 =:        b.0x68
08002074: 0D C4 08 00 34 70             w2 :=        $0x8003470
0800207A: 2D E5 68 CD 27                by comp2     @b.0x68+,$0x27
0800207F: C4 29                         if = go      $0x29
08002081: FD 50 C4 08 00 34 58 D2       h wconv      $0x8003458,r3
08002089: FD 3C C9 12                   w1 laddr     r.0x12
0800208D: 20 5A                         w1 =:        b.0x68
0800208F: 07 E5 68                      by4 :=       @b.0x68+
08002092: 1F E2 08 00 29 30             by4 =:       $0x8002930+
08002098: FC 54 C4 08 00 34 58 02       h add2       $0x8003458,$0x2
080020A0: BF C4 08 00 34 70 03 C8       d loopi      $0x8003470,$0x3,$0xFFFFFFFFFFFFFFC8
080020A8: FC 59 C4 08 00 34 58 02       h sub2       $0x8003458,$0x2
080020B0: 0C CD 4B                      w1 :=        $0x4B
080020B3: FE 25 E0 08 00 29 30          by2 laddr    $0x8002930+
080020BA: 21 5C                         w2 =:        b.0x70
080020BC: 0E CD 4F                      w3 :=        $0x4F
080020BF: 56 CD B6                      w3 +         $0xB6
080020C2: 22 5B                         w3 =:        b.0x6C
080020C4: CA 0C                         if < go      $0xC
080020C6: 84                            bi1 clr
080020C7: 85                            bi2 clr
080020C8: FD 67 C4 08 00 65 0C 5B       by smove     $0x800650C,b.0x6C
080020D0: C3 08 00 0F 2F 00             call         $0x8000F2F,$0x0
080020D6: D2 08                         if -k go     $0x8
080020D8: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080020DE: 4A C4 08 00 34 24             w stz        $0x8003424
080020E4: 2D C4 08 00 34 38 CD 24       by comp2     $0x8003438,$0x24
080020EC: C7 03 71                      if >< go     $0x371
080020EF: 19 00 60                      by move      $0x0,b.0x80
080020F2: FE 25 60                      by2 laddr    b.0x80
080020F5: 18 42                         r:=          b.0x8
080020F7: 21 85                         w2 =:        r.0x14
080020F9: 0E CF 08 00 29 A4             w3 :=        $0x80029A4
080020FF: 22 86                         w3 =:        r.0x18
08002101: 4A 87                         w stz        r.0x1C
08002103: 1A CD 3F 88                   w move       $0x3F,r.0x20
08002107: 1A CD 2A 89                   w move       $0x2A,r.0x24
0800210B: 4A 8A                         w stz        r.0x28
0800210D: 1A CD 29 8B                   w move       $0x29,r.0x2C
08002111: 0C CF 00 01 00 02             w1 :=        $0x10002
08002117: C3 08 00 3C 67 00             call         $0x8003C67,$0x0
0800211D: 4A 48                         w stz        b.0x20
0800211F: C3 08 00 10 3D 00             call         $0x800103D,$0x0
08002125: D2 08                         if -k go     $0x8
08002127: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800212D: 44 D0                         w test       r1
0800212F: C5 00 A1                      if = go      $0xA1
08002132: 1A 06 5D                      w move       $0x6,b.0x74
08002135: 4A 5C                         w stz        b.0x70
08002137: 0C CF 08 00 29 94             w1 :=        $0x8002994
0800213D: 20 5B                         w1 =:        b.0x6C
0800213F: 18 42                         r:=          b.0x8
08002141: FD 20 C4 08 00 65 1C 85 0C    by bmove     $0x800651C,r.0x14,$0xC
0800214A: FD 20 5B 88 0C                by bmove     b.0x6C,r.0x20,$0xC
0800214F: C3 08 00 01 A6 00             call         $0x80001A6,$0x0
08002155: D2 08                         if -k go     $0x8
08002157: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800215D: 44 D0                         w test       r1
0800215F: C4 12                         if = go      $0x12
08002161: C3 08 00 10 C3 00             call         $0x80010C3,$0x0
08002167: D2 08                         if -k go     $0x8
08002169: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800216F: C0 4C                         go           $0x4C
08002171: 18 42                         r:=          b.0x8
08002173: 4D 85                         w set1       r.0x14
08002175: FD 20 C4 08 00 65 3C 86 0C    by bmove     $0x800653C,r.0x18,$0xC
0800217E: FD 20 C4 08 00 65 48 89 0C    by bmove     $0x8006548,r.0x24,$0xC
08002187: C3 08 00 37 08 00             call         $0x8003708,$0x0
0800218D: D2 08                         if -k go     $0x8
0800218F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002195: 18 42                         r:=          b.0x8
08002197: 4D 85                         w set1       r.0x14
08002199: FD 20 C4 08 00 65 58 86 0C    by bmove     $0x8006558,r.0x18,$0xC
080021A2: FD 20 C4 08 00 65 64 89 0C    by bmove     $0x8006564,r.0x24,$0xC
080021AB: C3 08 00 37 08 00             call         $0x8003708,$0x0
080021B1: D2 08                         if -k go     $0x8
080021B3: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080021B9: 4D 48                         w set1       b.0x20
080021BB: C3 08 00 0F F2 00             call         $0x8000FF2,$0x0
080021C1: D2 08                         if -k go     $0x8
080021C3: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080021C9: 44 D0                         w test       r1
080021CB: C4 05                         if = go      $0x5
080021CD: C1 FF 52                      go           $0xFFFFFFFFFFFFFF52
080021D0: 18 42                         r:=          b.0x8
080021D2: 4D 85                         w set1       r.0x14
080021D4: FD 20 C4 08 00 65 74 86 0C    by bmove     $0x8006574,r.0x18,$0xC
080021DD: FD 20 C4 08 00 65 80 89 0C    by bmove     $0x8006580,r.0x24,$0xC
080021E6: C3 08 00 37 08 00             call         $0x8003708,$0x0
080021EC: D2 08                         if -k go     $0x8
080021EE: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080021F4: 44 48                         w test       b.0x20
080021F6: C4 29                         if = go      $0x29
080021F8: 18 42                         r:=          b.0x8
080021FA: 4D 85                         w set1       r.0x14
080021FC: FD 20 C4 08 00 65 CC 86 0C    by bmove     $0x80065CC,r.0x18,$0xC
08002205: FD 20 C4 08 00 65 D8 89 0C    by bmove     $0x80065D8,r.0x24,$0xC
0800220E: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002214: D2 08                         if -k go     $0x8
08002216: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800221C: C1 0B 5F                      go           $0xB5F
0800221F: 0D C4 08 00 34 60             w2 :=        $0x8003460
08002225: 18 42                         r:=          b.0x8
08002227: 1A C4 08 00 34 C8 85          w move       $0x80034C8,r.0x14
0800222E: 08 D1                         h1 :=        r2
08002230: C3 08 00 30 31 00             call         $0x8003031,$0x0
08002236: D2 08                         if -k go     $0x8
08002238: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800223E: 18 42                         r:=          b.0x8
08002240: 1A C4 08 00 34 60 85          w move       $0x8003460,r.0x14
08002247: C3 08 00 35 4C 00             call         $0x800354C,$0x0
0800224D: D2 08                         if -k go     $0x8
0800224F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002255: 18 42                         r:=          b.0x8
08002257: 4D 85                         w set1       r.0x14
08002259: FD 20 C4 08 00 65 E8 86 0C    by bmove     $0x80065E8,r.0x18,$0xC
08002262: FD 20 C4 08 00 65 F4 89 0C    by bmove     $0x80065F4,r.0x24,$0xC
0800226B: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002271: D2 08                         if -k go     $0x8
08002273: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002279: 18 42                         r:=          b.0x8
0800227B: 4D 85                         w set1       r.0x14
0800227D: FD 20 C4 08 00 66 38 86 0C    by bmove     $0x8006638,r.0x18,$0xC
08002286: FD 20 C4 08 00 66 44 89 0C    by bmove     $0x8006644,r.0x24,$0xC
0800228F: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002295: D2 08                         if -k go     $0x8
08002297: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800229D: 18 42                         r:=          b.0x8
0800229F: 4D 85                         w set1       r.0x14
080022A1: FD 20 C4 08 00 66 74 86 0C    by bmove     $0x8006674,r.0x18,$0xC
080022AA: FD 20 C4 08 00 66 80 89 0C    by bmove     $0x8006680,r.0x24,$0xC
080022B3: C3 08 00 37 08 00             call         $0x8003708,$0x0
080022B9: D2 08                         if -k go     $0x8
080022BB: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080022C1: 18 42                         r:=          b.0x8
080022C3: 4D 85                         w set1       r.0x14
080022C5: FD 20 C4 08 00 66 90 86 0C    by bmove     $0x8006690,r.0x18,$0xC
080022CE: FD 20 C4 08 00 66 9C 89 0C    by bmove     $0x800669C,r.0x24,$0xC
080022D7: C3 08 00 37 08 00             call         $0x8003708,$0x0
080022DD: D2 08                         if -k go     $0x8
080022DF: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080022E5: 18 42                         r:=          b.0x8
080022E7: FD 20 C4 08 00 66 BC 85 0C    by bmove     $0x80066BC,r.0x14,$0xC
080022F0: FD 20 C4 08 00 66 C8 88 0C    by bmove     $0x80066C8,r.0x20,$0xC
080022F9: 1A C4 08 00 34 F0 8B          w move       $0x80034F0,r.0x2C
08002300: 0D CF 08 00 35 0C             w2 :=        $0x800350C
08002306: 21 8C                         w2 =:        r.0x30
08002308: FD 20 C4 08 00 66 D4 8D 0C    by bmove     $0x80066D4,r.0x34,$0xC
08002311: 1A C4 08 00 34 C8 90          w move       $0x80034C8,r.0x40
08002318: C3 08 00 02 D5 00             call         $0x80002D5,$0x0
0800231E: D2 08                         if -k go     $0x8
08002320: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002326: 18 42                         r:=          b.0x8
08002328: 1A 8B C4 08 00 34 F0          w move       r.0x2C,$0x80034F0
0800232F: 1A 90 C4 08 00 34 C8          w move       r.0x40,$0x80034C8
08002336: 20 C4 08 00 34 60             w1 =:        $0x8003460
0800233C: 1A CD 40 C4 08 00 34 68       w move       $0x40,$0x8003468
08002344: 1A C4 08 00 34 68 85          w move       $0x8003468,r.0x14
0800234B: 84                            bi1 clr
0800234C: C3 08 00 35 5F 00             call         $0x800355F,$0x0
08002352: D2 08                         if -k go     $0x8
08002354: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800235A: 18 42                         r:=          b.0x8
0800235C: 1A C4 08 00 34 68 85          w move       $0x8003468,r.0x14
08002363: 0C CE 08 00                   w1 :=        $0x800
08002367: C3 08 00 32 F2 00             call         $0x80032F2,$0x0
0800236D: D2 08                         if -k go     $0x8
0800236F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002375: 0D C4 08 00 34 68             w2 :=        $0x8003468
0800237B: 08 D1                         h1 :=        r2
0800237D: C3 08 00 2F E5 00             call         $0x8002FE5,$0x0
08002383: D2 08                         if -k go     $0x8
08002385: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800238B: 20 C4 08 00 34 D0             w1 =:        $0x80034D0
08002391: 18 42                         r:=          b.0x8
08002393: 1A C4 08 00 34 68 85          w move       $0x8003468,r.0x14
0800239A: FD 20 C4 08 00 66 E0 86 0C    by bmove     $0x80066E0,r.0x18,$0xC
080023A3: C3 08 00 3D C7 00             call         $0x8003DC7,$0x0
080023A9: D2 08                         if -k go     $0x8
080023AB: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080023B1: 18 CF 08 00 35 8C             r:=          $0x800358C
080023B7: 4A 8E                         w stz        r.0x38
080023B9: 4D C4 08 00 34 F8             w set1       $0x80034F8
080023BF: 4D 45                         w set1       b.0x14
080023C1: 2E 45 C4 08 00 34 24          w comp2      b.0x14,$0x8003424
080023C8: C9 00 95                      if > go      $0x95
080023CB: 0C 45                         w1 :=        b.0x14
080023CD: 6C CD 2A                      w1 *         $0x2A
080023D0: 1A CD 29 5D                   w move       $0x29,b.0x74
080023D4: 4A 5C                         w stz        b.0x70
080023D6: FE 25 E0 08 00 29 A4          by2 laddr    $0x80029A4+
080023DD: 21 5B                         w2 =:        b.0x6C
080023DF: 1A CD 29 63                   w move       $0x29,b.0x8C
080023E3: 4A 62                         w stz        b.0x88
080023E5: 0E 5B                         w3 :=        b.0x6C
080023E7: 22 61                         w3 =:        b.0x84
080023E9: 18 42                         r:=          b.0x8
080023EB: FD 20 61 85 0C                by bmove     b.0x84,r.0x14,$0xC
080023F0: C3 08 00 05 AD 00             call         $0x80005AD,$0x0
080023F6: D2 08                         if -k go     $0x8
080023F8: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080023FE: 44 D0                         w test       r1
08002400: C4 54                         if = go      $0x54
08002402: C3 08 00 0C 78 00             call         $0x8000C78,$0x0
08002408: D2 08                         if -k go     $0x8
0800240A: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002410: C3 08 00 0E 77 00             call         $0x8000E77,$0x0
08002416: D2 08                         if -k go     $0x8
08002418: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800241E: 0D C4 08 00 34 6C             w2 :=        $0x800346C
08002424: 18 42                         r:=          b.0x8
08002426: 1A C4 08 00 34 D4 85          w move       $0x80034D4,r.0x14
0800242D: 08 D1                         h1 :=        r2
0800242F: C3 08 00 30 31 00             call         $0x8003031,$0x0
08002435: D2 08                         if -k go     $0x8
08002437: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800243D: 18 42                         r:=          b.0x8
0800243F: 1A C4 08 00 34 6C 85          w move       $0x800346C,r.0x14
08002446: C3 08 00 35 4C 00             call         $0x800354C,$0x0
0800244C: D2 08                         if -k go     $0x8
0800244E: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002454: E1 45 C4 08 00 34 24 FF       loopi        b.0x14,$0x8003424,$0xFFFFFFFFFFFFFFFF
0800245C: 77 18                         d4 *         $0x18
0800245E: 42 4D                         by test      b.0x34
08002460: 85                            bi2 clr
08002461: FD 20 C4 08 00 67 04 86 0C    by bmove     $0x8006704,r.0x18,$0xC
0800246A: FD 20 C4 08 00 67 10 89 0C    by bmove     $0x8006710,r.0x24,$0xC
08002473: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002479: D2 08                         if -k go     $0x8
0800247B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002481: 18 42                         r:=          b.0x8
08002483: 4A 85                         w stz        r.0x14
08002485: FD 20 C4 08 00 67 20 86 0C    by bmove     $0x8006720,r.0x18,$0xC
0800248E: FD 3D C1 5E                   w2 laddr     b.0x5E
08002492: 21 89                         w2 =:        r.0x24
08002494: 4A 8A                         w stz        r.0x28
08002496: 1A 04 8B                      w move       $0x4,r.0x2C
08002499: C3 08 00 36 2F 00             call         $0x800362F,$0x0
0800249F: D2 08                         if -k go     $0x8
080024A1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080024A7: 86                            bi3 clr
080024A8: 05 D6 5E                      by2 :=       b.0x5E+
080024AB: FC 91 CD 5F                   by2 and      $0x5F
080024AF: 87                            bi4 clr
080024B0: 1D D7 5E                      by2 =:       b.0x5E+
080024B3: 4A 5A                         w stz        b.0x68
080024B5: 84                            bi1 clr
080024B6: 2D D4 5E CD 59                by comp2     b.0x5E+,$0x59
080024BB: C6 04                         if >< go     $0x4
080024BD: 4D 5A                         w set1       b.0x68
080024BF: 4A 64                         w stz        b.0x90
080024C1: 86                            bi3 clr
080024C2: 2D D6 5E CD 4A                by comp2     b.0x5E+,$0x4A
080024C7: C6 04                         if >< go     $0x4
080024C9: 4D 64                         w set1       b.0x90
080024CB: 0D 5A                         w2 :=        b.0x68
080024CD: A1 64                         w2 or        b.0x90
080024CF: 21 C4 08 00 35 08             w2 =:        $0x8003508
080024D5: 18 42                         r:=          b.0x8
080024D7: 4D 85                         w set1       r.0x14
080024D9: FD 20 C4 08 00 67 30 86 0C    by bmove     $0x8006730,r.0x18,$0xC
080024E2: FD 20 C4 08 00 67 3C 89 0C    by bmove     $0x800673C,r.0x24,$0xC
080024EB: C3 08 00 37 08 00             call         $0x8003708,$0x0
080024F1: D2 08                         if -k go     $0x8
080024F3: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080024F9: 18 42                         r:=          b.0x8
080024FB: 4D 85                         w set1       r.0x14
080024FD: FD 20 C4 08 00 67 60 86 0C    by bmove     $0x8006760,r.0x18,$0xC
08002506: FD 20 C4 08 00 67 6C 89 0C    by bmove     $0x800676C,r.0x24,$0xC
0800250F: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002515: D2 08                         if -k go     $0x8
08002517: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800251D: 18 42                         r:=          b.0x8
0800251F: 4D 85                         w set1       r.0x14
08002521: FD 20 C4 08 00 67 7C 86 0C    by bmove     $0x800677C,r.0x18,$0xC
0800252A: FD 20 C4 08 00 67 88 89 0C    by bmove     $0x8006788,r.0x24,$0xC
08002533: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002539: D2 08                         if -k go     $0x8
0800253B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002541: 18 42                         r:=          b.0x8
08002543: 4D 85                         w set1       r.0x14
08002545: FD 20 C4 08 00 67 98 86 0C    by bmove     $0x8006798,r.0x18,$0xC
0800254E: FD 20 C4 08 00 67 A4 89 0C    by bmove     $0x80067A4,r.0x24,$0xC
08002557: C3 08 00 37 08 00             call         $0x8003708,$0x0
0800255D: D2 08                         if -k go     $0x8
0800255F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002565: 49 C4 08 00 4E 4C             h stz        $0x8004E4C
0800256B: FC 69 CF 08 00 4A 4A 02 5F    w add3       $0x8004A4A,$0x2,b.0x7C
08002574: 1A CE 02 00 5E                w move       $0x200,b.0x78
08002579: CA 07                         if < go      $0x7
0800257B: 85                            bi2 clr
0800257C: 86                            bi3 clr
0800257D: FD 86 5E                      h3 sfill     b.0x78
08002580: 04 CD 20                      by1 :=       $0x20
08002583: 1C 60                         by1 =:       b.0x80
08002585: FE 25 60                      by2 laddr    b.0x80
08002588: 18 42                         r:=          b.0x8
0800258A: 21 85                         w2 =:        r.0x14
0800258C: 0C CF 08 00 36 42             w1 :=        $0x8003642
08002592: 20 86                         w1 =:        r.0x18
08002594: 4D 87                         w set1       r.0x1C
08002596: 1A CE 02 00 88                w move       $0x200,r.0x20
0800259B: 1A 0A 89                      w move       $0xA,r.0x24
0800259E: 4A 8A                         w stz        r.0x28
080025A0: 1A 09 8B                      w move       $0x9,r.0x2C
080025A3: 0C CF 00 01 00 02             w1 :=        $0x10002
080025A9: C3 08 00 3C 67 00             call         $0x8003C67,$0x0
080025AF: 4A C4 08 00 34 88             w stz        $0x8003488
080025B5: 1A 3F C4 08 00 34 8C          w move       $0x3F,$0x800348C
080025BC: 4A C4 08 00 34 90             w stz        $0x8003490
080025C2: 4A C4 08 00 34 94             w stz        $0x8003494
080025C8: 84                            bi1 clr
080025C9: 20 C4 08 00 34 28             w1 =:        $0x8003428
080025CF: 20 C4 08 00 34 2C             w1 =:        $0x800342C
080025D5: 85                            bi2 clr
080025D6: FC 11 C4 08 00 34 54          h2 =:        $0x8003454
080025DD: FC 11 C4 08 00 34 56          h2 =:        $0x8003456
080025E4: C3 08 00 1A 39 00             call         $0x8001A39,$0x0
080025EA: D2 08                         if -k go     $0x8
080025EC: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080025F2: C3 08 00 0F 2F 00             call         $0x8000F2F,$0x0
080025F8: D2 08                         if -k go     $0x8
080025FA: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002600: C3 08 00 11 9B 00             call         $0x800119B,$0x0
08002606: D2 08                         if -k go     $0x8
08002608: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800260E: 44 D0                         w test       r1
08002610: C4 68                         if = go      $0x68
08002612: C3 08 00 14 0F 00             call         $0x800140F,$0x0
08002618: D2 08                         if -k go     $0x8
0800261A: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002620: 44 D0                         w test       r1
08002622: C4 54                         if = go      $0x54
08002624: 44 C4 08 00 35 08             w test       $0x8003508
0800262A: C4 10                         if = go      $0x10
0800262C: C3 08 00 1B 6A 00             call         $0x8001B6A,$0x0
08002632: D2 08                         if -k go     $0x8
08002634: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800263A: 4A 5A                         w stz        b.0x68
0800263C: 84                            bi1 clr
0800263D: 85                            bi2 clr
0800263E: FD BE C4 08 00 67 B0 C4 08 00 67 B8 00          by scopa     $0x80067B0,$0x80067B8,$0x0
0800264B: C6 04                         if >< go     $0x4
0800264D: 4D 5A                         w set1       b.0x68
0800264F: 0C 5A                         w1 :=        b.0x68
08002651: 18 42                         r:=          b.0x8
08002653: 20 85                         w1 =:        r.0x14
08002655: C3 08 00 16 DA 00             call         $0x80016DA,$0x0
0800265B: D2 08                         if -k go     $0x8
0800265D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002663: 84                            bi1 clr
08002664: 85                            bi2 clr
08002665: FD BE C4 08 00 67 C0 C4 08 00 67 C8 00          by scopa     $0x80067C0,$0x80067C8,$0x0
08002672: C4 04                         if = go      $0x4
08002674: C0 9E                         go           $0xFFFFFFFFFFFFFF9E
08002676: C0 8A                         go           $0xFFFFFFFFFFFFFF8A
08002678: C3 08 00 14 0F 00             call         $0x800140F,$0x0
0800267E: D2 08                         if -k go     $0x8
08002680: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002686: 44 D0                         w test       r1
08002688: C4 2C                         if = go      $0x2C
0800268A: 44 C4 08 00 35 08             w test       $0x8003508
08002690: C4 10                         if = go      $0x10
08002692: C3 08 00 1B 6A 00             call         $0x8001B6A,$0x0
08002698: D2 08                         if -k go     $0x8
0800269A: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026A0: 18 42                         r:=          b.0x8
080026A2: 4A 85                         w stz        r.0x14
080026A4: C3 08 00 16 DA 00             call         $0x80016DA,$0x0
080026AA: D2 08                         if -k go     $0x8
080026AC: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026B2: C0 C6                         go           $0xFFFFFFFFFFFFFFC6
080026B4: 04 0C                         by1 :=       $0xC
080026B6: C3 08 00 15 5B 00             call         $0x800155B,$0x0
080026BC: D2 08                         if -k go     $0x8
080026BE: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026C4: 18 42                         r:=          b.0x8
080026C6: FD 20 C4 08 00 67 D0 85 0C    by bmove     $0x80067D0,r.0x14,$0xC
080026CF: C3 08 00 3E BF 00             call         $0x8003EBF,$0x0
080026D5: D2 08                         if -k go     $0x8
080026D7: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026DD: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080026E3: D2 08                         if -k go     $0x8
080026E5: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026EB: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080026F1: D2 08                         if -k go     $0x8
080026F3: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080026F9: 18 42                         r:=          b.0x8
080026FB: FD 20 C4 08 00 68 04 85 0C    by bmove     $0x8006804,r.0x14,$0xC
08002704: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
0800270A: D2 08                         if -k go     $0x8
0800270C: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002712: 0E 03                         w3 :=        $0x3
08002714: 0D E2 08 00 00 14             w2 :=        $0x8000014+
0800271A: 08 D1                         h1 :=        r2
0800271C: C3 08 00 16 78 00             call         $0x8001678,$0x0
08002722: D2 08                         if -k go     $0x8
08002724: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800272A: 04 CD 3A                      by1 :=       $0x3A
0800272D: C3 08 00 15 5B 00             call         $0x800155B,$0x0
08002733: D2 08                         if -k go     $0x8
08002735: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800273B: 0E 02                         w3 :=        $0x2
0800273D: 0D E2 08 00 00 14             w2 :=        $0x8000014+
08002743: 08 D1                         h1 :=        r2
08002745: C3 08 00 16 78 00             call         $0x8001678,$0x0
0800274B: D2 08                         if -k go     $0x8
0800274D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002753: 18 42                         r:=          b.0x8
08002755: FD 20 C4 08 00 68 18 85 0C    by bmove     $0x8006818,r.0x14,$0xC
0800275E: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002764: D2 08                         if -k go     $0x8
08002766: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800276C: 0E 04                         w3 :=        $0x4
0800276E: 0D E2 08 00 00 14             w2 :=        $0x8000014+
08002774: 08 D1                         h1 :=        r2
08002776: C3 08 00 16 78 00             call         $0x8001678,$0x0
0800277C: D2 08                         if -k go     $0x8
0800277E: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002784: 04 CD 2F                      by1 :=       $0x2F
08002787: C3 08 00 15 5B 00             call         $0x800155B,$0x0
0800278D: D2 08                         if -k go     $0x8
0800278F: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002795: 0E 05                         w3 :=        $0x5
08002797: 0D E2 08 00 00 14             w2 :=        $0x8000014+
0800279D: 08 D1                         h1 :=        r2
0800279F: C3 08 00 16 78 00             call         $0x8001678,$0x0
080027A5: D2 08                         if -k go     $0x8
080027A7: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080027AD: 04 CD 2D                      by1 :=       $0x2D
080027B0: C3 08 00 15 5B 00             call         $0x800155B,$0x0
080027B6: D2 08                         if -k go     $0x8
080027B8: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080027BE: 0E 06                         w3 :=        $0x6
080027C0: 0D E2 08 00 00 14             w2 :=        $0x8000014+
080027C6: 08 D1                         h1 :=        r2
080027C8: C3 08 00 16 1C 00             call         $0x800161C,$0x0
080027CE: D2 08                         if -k go     $0x8
080027D0: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080027D6: 4A 46                         w stz        b.0x18
080027D8: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080027DE: D2 08                         if -k go     $0x8
080027E0: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080027E6: BF 46 0A F2                   d loopi      b.0x18,$0xA,$0xFFFFFFFFFFFFFFF2
080027EA: 18 42                         r:=          b.0x8
080027EC: FD 20 C4 08 00 68 30 85 0C    by bmove     $0x8006830,r.0x14,$0xC
080027F5: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
080027FB: D2 08                         if -k go     $0x8
080027FD: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002803: FD 50 C4 08 00 34 58 D1       h wconv      $0x8003458,r2
0800280B: 1A D1 5D                      w move       r2,b.0x74
0800280E: 4A 5C                         w stz        b.0x70
08002810: 0E CF 08 00 29 30             w3 :=        $0x8002930
08002816: 22 5B                         w3 =:        b.0x6C
08002818: 18 42                         r:=          b.0x8
0800281A: FD 20 5B 85 0C                by bmove     b.0x6C,r.0x14,$0xC
0800281F: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002825: D2 08                         if -k go     $0x8
08002827: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800282D: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002833: D2 08                         if -k go     $0x8
08002835: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800283B: 18 42                         r:=          b.0x8
0800283D: FD 20 C4 08 00 68 48 85 0C    by bmove     $0x8006848,r.0x14,$0xC
08002846: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
0800284C: D2 08                         if -k go     $0x8
0800284E: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002854: 4A 46                         w stz        b.0x18
08002856: FD 50 C4 08 00 34 58 D1       h wconv      $0x8003458,r2
0800285E: 2E 46 D1                      w comp2      b.0x18,r2
08002861: C8 22                         if > go      $0x22
08002863: 04 CD 2D                      by1 :=       $0x2D
08002866: C3 08 00 15 5B 00             call         $0x800155B,$0x0
0800286C: D2 08                         if -k go     $0x8
0800286E: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002874: 4F 46                         w incr       b.0x18
08002876: FD 50 C4 08 00 34 58 D1       h wconv      $0x8003458,r2
0800287E: 2E 46 D1                      w comp2      b.0x18,r2
08002881: CE E2                         if <= go     $0xFFFFFFFFFFFFFFE2
08002883: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002889: D2 08                         if -k go     $0x8
0800288B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002891: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002897: D2 08                         if -k go     $0x8
08002899: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800289F: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080028A5: D2 08                         if -k go     $0x8
080028A7: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080028AD: 18 42                         r:=          b.0x8
080028AF: FD 20 C4 08 00 68 70 85 0C    by bmove     $0x8006870,r.0x14,$0xC
080028B8: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
080028BE: D2 08                         if -k go     $0x8
080028C0: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080028C6: FD 3D 4D                      w2 laddr     b.0x34
080028C9: 18 42                         r:=          b.0x8
080028CB: 21 85                         w2 =:        r.0x14
080028CD: 4A 86                         w stz        r.0x18
080028CF: 1A CD 29 87                   w move       $0x29,r.0x1C
080028D3: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
080028D9: D2 08                         if -k go     $0x8
080028DB: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080028E1: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080028E7: D2 08                         if -k go     $0x8
080028E9: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080028EF: 44 C4 08 00 34 24             w test       $0x8003424
080028F5: CF 00 8D                      if <= go     $0x8D
080028F8: 18 42                         r:=          b.0x8
080028FA: FD 20 C4 08 00 68 98 85 0C    by bmove     $0x8006898,r.0x14,$0xC
08002903: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002909: D2 08                         if -k go     $0x8
0800290B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002911: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002917: D2 08                         if -k go     $0x8
08002919: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800291F: 4D 45                         w set1       b.0x14
08002921: 2E 45 C4 08 00 34 24          w comp2      b.0x14,$0x8003424
08002928: C8 5A                         if > go      $0x5A
0800292A: 18 42                         r:=          b.0x8
0800292C: FD 20 C4 08 00 68 C0 85 0C    by bmove     $0x80068C0,r.0x14,$0xC
08002935: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
0800293B: D2 08                         if -k go     $0x8
0800293D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002943: 0D 45                         w2 :=        b.0x14
08002945: 6D CD 2A                      w2 *         $0x2A
08002948: 1A CD 29 5D                   w move       $0x29,b.0x74
0800294C: 4A 5C                         w stz        b.0x70
0800294E: FE 26 E1 08 00 29 A4          by3 laddr    $0x80029A4+
08002955: 22 5B                         w3 =:        b.0x6C
08002957: 18 42                         r:=          b.0x8
08002959: FD 20 5B 85 0C                by bmove     b.0x6C,r.0x14,$0xC
0800295E: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002964: D2 08                         if -k go     $0x8
08002966: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800296C: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002972: D2 08                         if -k go     $0x8
08002974: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800297A: BF 45 C4 08 00 34 24 B0       d loopi      b.0x14,$0x8003424,$0xFFFFFFFFFFFFFFB0
08002982: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002988: D2 08                         if -k go     $0x8
0800298A: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002990: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002996: D2 08                         if -k go     $0x8
08002998: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
0800299E: 18 42                         r:=          b.0x8
080029A0: FD 20 C4 08 00 68 F4 85 0C    by bmove     $0x80068F4,r.0x14,$0xC
080029A9: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
080029AF: D2 08                         if -k go     $0x8
080029B1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080029B7: 08 C4 08 00 34 54             h1 :=        $0x8003454
080029BD: C3 08 00 16 1C 00             call         $0x800161C,$0x0
080029C3: D2 08                         if -k go     $0x8
080029C5: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080029CB: C3 08 00 16 9A 00             call         $0x800169A,$0x0
080029D1: D2 08                         if -k go     $0x8
080029D3: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080029D9: 18 42                         r:=          b.0x8
080029DB: FD 20 C4 08 00 69 4C 85 0C    by bmove     $0x800694C,r.0x14,$0xC
080029E4: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
080029EA: D2 08                         if -k go     $0x8
080029EC: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
080029F2: 08 C4 08 00 34 56             h1 :=        $0x8003456
080029F8: C3 08 00 16 1C 00             call         $0x800161C,$0x0
080029FE: D2 08                         if -k go     $0x8
08002A00: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A06: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002A0C: D2 08                         if -k go     $0x8
08002A0E: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A14: 18 42                         r:=          b.0x8
08002A16: FD 20 C4 08 00 69 78 85 0C    by bmove     $0x8006978,r.0x14,$0xC
08002A1F: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002A25: D2 08                         if -k go     $0x8
08002A27: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A2D: 09 C4 08 00 34 56             h2 :=        $0x8003456
08002A33: FC 41 C4 08 00 34 54          h2 -         $0x8003454
08002A3A: FD 52 D1 D0                   h dconv      r2,r1
08002A3E: 28 49                         d1 =:        b.0x24
08002A40: FD 52 C4 08 00 34 56 D1       h dconv      $0x8003456,r2
08002A48: 29 4B                         d2 =:        b.0x2C
08002A4A: FD 5C CD 64 D2                f dconv      $0x64,r3
08002A4F: 74 D2                         d1 *         r3
08002A51: E8 D1                         d1 /         r2
08002A53: 28 49                         d1 =:        b.0x24
08002A55: FD 5F D0 D2                   d hconv      r1,r3
08002A59: 08 D2                         h1 :=        r3
08002A5B: C3 08 00 16 1C 00             call         $0x800161C,$0x0
08002A61: D2 08                         if -k go     $0x8
08002A63: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A69: 18 42                         r:=          b.0x8
08002A6B: FD 20 C4 08 00 69 8C 85 0C    by bmove     $0x800698C,r.0x14,$0xC
08002A74: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002A7A: D2 08                         if -k go     $0x8
08002A7C: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A82: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002A88: D2 08                         if -k go     $0x8
08002A8A: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002A90: FD 50 C4 08 00 4E 4C D1       h wconv      $0x8004E4C,r2
08002A98: 44 D1                         w test       r2
08002A9A: CF 01 63                      if <= go     $0x163
08002A9D: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002AA3: D2 08                         if -k go     $0x8
08002AA5: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002AAB: 18 42                         r:=          b.0x8
08002AAD: FD 20 C4 08 00 69 D4 85 0C    by bmove     $0x80069D4,r.0x14,$0xC
08002AB6: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002ABC: D2 08                         if -k go     $0x8
08002ABE: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002AC4: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002ACA: D2 08                         if -k go     $0x8
08002ACC: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002AD2: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002AD8: D2 08                         if -k go     $0x8
08002ADA: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002AE0: 4D 46                         w set1       b.0x18
08002AE2: FD 50 C4 08 00 4E 4C D1       h wconv      $0x8004E4C,r2
08002AEA: 2E 46 D1                      w comp2      b.0x18,r2
08002AED: C9 01 10                      if > go      $0x110
08002AF0: FC 7C 46 CD 32 D1             w1 div4      b.0x18,$0x32,r2
08002AF6: 34 01                         w1 comp      $0x1
08002AF8: C6 50                         if >< go     $0x50
08002AFA: 18 42                         r:=          b.0x8
08002AFC: FD 20 C4 08 00 6A 14 85 0C    by bmove     $0x8006A14,r.0x14,$0xC
08002B05: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002B0B: D2 08                         if -k go     $0x8
08002B0D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B13: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002B19: D2 08                         if -k go     $0x8
08002B1B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B21: 18 42                         r:=          b.0x8
08002B23: FD 20 C4 08 00 6A 54 85 0C    by bmove     $0x8006A54,r.0x14,$0xC
08002B2C: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002B32: D2 08                         if -k go     $0x8
08002B34: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B3A: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002B40: D2 08                         if -k go     $0x8
08002B42: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B48: 18 42                         r:=          b.0x8
08002B4A: FD 20 C4 08 00 6A 6C 85 0C    by bmove     $0x8006A6C,r.0x14,$0xC
08002B53: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002B59: D2 08                         if -k go     $0x8
08002B5B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B61: 0D 46                         w2 :=        b.0x18
08002B63: 6D 0A                         w2 *         $0xA
08002B65: 1A 09 5D                      w move       $0x9,b.0x74
08002B68: 4A 5C                         w stz        b.0x70
08002B6A: FE 26 E1 08 00 36 42          by3 laddr    $0x8003642+
08002B71: 22 5B                         w3 =:        b.0x6C
08002B73: 18 42                         r:=          b.0x8
08002B75: FD 20 5B 85 0C                by bmove     b.0x6C,r.0x14,$0xC
08002B7A: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002B80: D2 08                         if -k go     $0x8
08002B82: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002B88: 18 42                         r:=          b.0x8
08002B8A: FD 20 C4 08 00 6A 88 85 0C    by bmove     $0x8006A88,r.0x14,$0xC
08002B93: C3 08 00 15 E1 00             call         $0x80015E1,$0x0
08002B99: D2 08                         if -k go     $0x8
08002B9B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002BA1: 0C 46                         w1 :=        b.0x18
08002BA3: 08 E0 08 00 4A 4A             h1 :=        $0x8004A4A+
08002BA9: C3 08 00 16 1C 00             call         $0x800161C,$0x0
08002BAF: D2 08                         if -k go     $0x8
08002BB1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002BB7: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002BBD: D2 08                         if -k go     $0x8
08002BBF: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002BC5: FC 7D 46 CD 32 D2             w2 div4      b.0x18,$0x32,r3
08002BCB: 44 D1                         w test       r2
08002BCD: C6 20                         if >< go     $0x20
08002BCF: 04 0C                         by1 :=       $0xC
08002BD1: C3 08 00 15 5B 00             call         $0x800155B,$0x0
08002BD7: D2 08                         if -k go     $0x8
08002BD9: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002BDF: C3 08 00 16 9A 00             call         $0x800169A,$0x0
08002BE5: D2 08                         if -k go     $0x8
08002BE7: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002BED: 4F 46                         w incr       b.0x18
08002BEF: FD 50 C4 08 00 4E 4C D1       h wconv      $0x8004E4C,r2
08002BF7: 2E 46 D1                      w comp2      b.0x18,r2
08002BFA: CF FE F6                      if <= go     $0xFFFFFFFFFFFFFEF6
08002BFD: C3 08 00 1B 2A 00             call         $0x8001B2A,$0x0
08002C03: D2 08                         if -k go     $0x8
08002C05: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002C0B: 44 C4 08 00 35 00             w test       $0x8003500
08002C11: C4 5D                         if = go      $0x5D
08002C13: 0D C4 08 00 34 80             w2 :=        $0x8003480
08002C19: FE 26 E1 08 00 20 30          by3 laddr    $0x8002030+
08002C20: 22 5C                         w3 =:        b.0x70
08002C22: 0F CE 07 FF                   w4 :=        $0x7FF
08002C26: 63 D1                         w4 -         r2
08002C28: 57 01                         w4 +         $0x1
08002C2A: 23 5B                         w4 =:        b.0x6C
08002C2C: CA 07                         if < go      $0x7
08002C2E: 85                            bi2 clr
08002C2F: 84                            bi1 clr
08002C30: FD 80 5B                      by1 sfill    b.0x6C
08002C33: 0C C4 08 00 34 5C             w1 :=        $0x800345C
08002C39: 18 42                         r:=          b.0x8
08002C3B: FC 10 85                      h1 =:        r.0x14
08002C3E: 1A C4 08 00 34 D8 86          w move       $0x80034D8,r.0x18
08002C45: FC 14 CE 04 00 87             h move       $0x400,r.0x1C
08002C4B: 0D C4 08 00 34 7C             w2 :=        $0x800347C
08002C51: FC 11 C9 1E                   h2 =:        r.0x1E
08002C55: FD 20 C4 08 00 6A 94 88 0C    by bmove     $0x8006A94,r.0x20,$0xC
08002C5E: 08 01                         h1 :=        $0x1
08002C60: C3 08 00 30 52 00             call         $0x8003052,$0x0
08002C66: D2 08                         if -k go     $0x8
08002C68: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002C6E: 0D C4 08 00 34 84             w2 :=        $0x8003484
08002C74: 61 01                         w2 -         $0x1
08002C76: 21 5A                         w2 =:        b.0x68
08002C78: 18 42                         r:=          b.0x8
08002C7A: 1A C4 08 00 34 5C 85          w move       $0x800345C,r.0x14
08002C81: 0C D1                         w1 :=        r2
08002C83: C3 08 00 35 5F 00             call         $0x800355F,$0x0
08002C89: D2 08                         if -k go     $0x8
08002C8B: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002C91: 0D C4 08 00 34 60             w2 :=        $0x8003460
08002C97: 18 42                         r:=          b.0x8
08002C99: 1A C4 08 00 34 C8 85          w move       $0x80034C8,r.0x14
08002CA0: 08 D1                         h1 :=        r2
08002CA2: C3 08 00 30 31 00             call         $0x8003031,$0x0
08002CA8: D2 08                         if -k go     $0x8
08002CAA: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002CB0: 18 42                         r:=          b.0x8
08002CB2: 1A C4 08 00 34 60 85          w move       $0x8003460,r.0x14
08002CB9: C3 08 00 35 4C 00             call         $0x800354C,$0x0
08002CBF: D2 08                         if -k go     $0x8
08002CC1: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002CC7: 0D C4 08 00 34 64             w2 :=        $0x8003464
08002CCD: 18 42                         r:=          b.0x8
08002CCF: 1A C4 08 00 34 CC 85          w move       $0x80034CC,r.0x14
08002CD6: 08 D1                         h1 :=        r2
08002CD8: C3 08 00 30 31 00             call         $0x8003031,$0x0
08002CDE: D2 08                         if -k go     $0x8
08002CE0: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002CE6: 18 42                         r:=          b.0x8
08002CE8: 1A C4 08 00 34 64 85          w move       $0x8003464,r.0x14
08002CEF: C3 08 00 35 4C 00             call         $0x800354C,$0x0
08002CF5: D2 08                         if -k go     $0x8
08002CF7: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002CFD: 0D C4 08 00 34 5C             w2 :=        $0x800345C
08002D03: 18 42                         r:=          b.0x8
08002D05: 1A C4 08 00 34 D8 85          w move       $0x80034D8,r.0x14
08002D0C: 08 D1                         h1 :=        r2
08002D0E: C3 08 00 30 31 00             call         $0x8003031,$0x0
08002D14: D2 08                         if -k go     $0x8
08002D16: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002D1C: 18 42                         r:=          b.0x8
08002D1E: 1A C4 08 00 34 5C 85          w move       $0x800345C,r.0x14
08002D25: C3 08 00 35 4C 00             call         $0x800354C,$0x0
08002D2B: D2 08                         if -k go     $0x8
08002D2D: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002D33: 18 42                         r:=          b.0x8
08002D35: 4D 85                         w set1       r.0x14
08002D37: FD 20 C4 08 00 6A C8 86 0C    by bmove     $0x8006AC8,r.0x18,$0xC
08002D40: FD 20 C4 08 00 6A D4 89 0C    by bmove     $0x8006AD4,r.0x24,$0xC
08002D49: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002D4F: D2 08                         if -k go     $0x8
08002D51: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002D57: 18 42                         r:=          b.0x8
08002D59: 4D 85                         w set1       r.0x14
08002D5B: FD 20 C4 08 00 6A E4 86 0C    by bmove     $0x8006AE4,r.0x18,$0xC
08002D64: FD 20 C4 08 00 6A F0 89 0C    by bmove     $0x8006AF0,r.0x24,$0xC
08002D6D: C3 08 00 37 08 00             call         $0x8003708,$0x0
08002D73: D2 08                         if -k go     $0x8
08002D75: C3 08 00 1C B8 00             call         $0x8001CB8,$0x0
08002D7B: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
08002D81: 9C                            entd
08002D82: FD C0 5A                      l=:          b.0x68
08002D85: FC 10 5E                      h1 =:        b.0x78
08002D88: FC 14 C1 32 C1 36             h move       b.0x32,b.0x36
08002D8E: FD 50 C1 36 D1                h wconv      b.0x36,r2
08002D93: 86                            bi3 clr
08002D94: FE 27 E6 6C                   by4 laddr    @b.0x6C+
08002D98: 23 60                         w4 =:        b.0x80
08002D9A: 55 01                         w2 +         $0x1
08002D9C: 21 5F                         w2 =:        b.0x7C
08002D9E: FD 50 C1 36 D0                h wconv      b.0x36,r1
08002DA3: 86                            bi3 clr
08002DA4: FE 27 D6 3E                   by4 laddr    b.0x3E+
08002DA8: 23 63                         w4 =:        b.0x8C
08002DAA: 54 01                         w1 +         $0x1
08002DAC: 20 62                         w1 =:        b.0x88
08002DAE: CA 08                         if < go      $0x8
08002DB0: 84                            bi1 clr
08002DB1: 85                            bi2 clr
08002DB2: FD 67 5F 62                   by smove     b.0x7C,b.0x88
08002DB6: 08 C1 36                      h1 :=        b.0x36
08002DB9: FC 38 01                      h1 +         $0x1
08002DBC: FD 50 D0 D1                   h wconv      r1,r2
08002DC0: 19 CD 2D D5 3E                by move      $0x2D,b.0x3E+
08002DC5: 0A 5E                         h3 :=        b.0x78
08002DC7: FC 3A CD 30                   h3 +         $0x30
08002DCB: 0B C1 36                      h4 :=        b.0x36
08002DCE: FC 3B 02                      h4 +         $0x2
08002DD1: FD 50 D3 D0                   h wconv      r4,r1
08002DD5: 1E D4 3E                      by3 =:       b.0x3E+
08002DD8: FC 54 C1 36 02                h add2       b.0x36,$0x2
08002DDD: FE 03                         clrk
08002DDF: B4 5A                         jumpg        b.0x68
08002DE1: 9C                            entd
08002DE2: FD C0 69                      l=:          b.0xA4
08002DE5: FC 14 C1 36 6A                h move       b.0x36,b.0xA8
08002DEA: FD 50 6A D0                   h wconv      b.0xA8,r1
08002DEE: 44 D0                         w test       r1
08002DF0: CA 1A                         if < go      $0x1A
08002DF2: FD 50 6A D0                   h wconv      b.0xA8,r1
08002DF6: 09 6A                         h2 :=        b.0xA8
08002DF8: FC 39 01                      h2 +         $0x1
08002DFB: FD 50 D1 D2                   h wconv      r2,r3
08002DFF: 07 D4 3E                      by4 :=       b.0x3E+
08002E02: 1F D6 3E                      by4 =:       b.0x3E+
08002E05: FD 24 6A 00 ED 04 CD 22       w loopd      b.0xA8,$0x0,$0xFFFFFFFFED04CD22
08002E0D: 85                            bi2 clr
08002E0E: 1C D5 3E                      by1 =:       b.0x3E+
08002E11: 0A C1 36                      h3 :=        b.0x36
08002E14: FC 3A 02                      h3 +         $0x2
08002E17: FD 50 D2 D3                   h wconv      r3,r4
08002E1B: 1C D7 3E                      by1 =:       b.0x3E+
08002E1E: FC 54 C1 36 02                h add2       b.0x36,$0x2
08002E23: FE 03                         clrk
08002E25: B4 69                         jumpg        b.0xA4
08002E27: B8 CF 00 00 00 D0             ents         $0xD0
08002E2D: C0 2F                         go           $0x2F
08002E2F: 9C                            entd
08002E30: FD C0 6B                      l=:          b.0xAC
08002E33: 20 43                         w1 =:        b.0xC
08002E35: FC 10 4C                      h1 =:        b.0x30
08002E38: 2E 43 CD 2E                   w comp2      b.0xC,$0x2E
08002E3C: C6 0B                         if >< go     $0xB
08002E3E: C3 08 00 2D E1 00             call         $0x8002DE1,$0x0
08002E44: 9D                            ifkret
08002E45: C0 13                         go           $0x13
08002E47: 2E 43 CD 6D                   w comp2      b.0xC,$0x6D
08002E4B: C4 08                         if = go      $0x8
08002E4D: 2E 43 CD 6E                   w comp2      b.0xC,$0x6E
08002E51: C6 04                         if >< go     $0x4
08002E53: C0 05                         go           $0x5
08002E55: 0C 43                         w1 :=        b.0xC
08002E57: 81                            retk
08002E58: FE 03                         clrk
08002E5A: B4 6B                         jumpg        b.0xAC
08002E5C: FC 6E 4A 49 6C                w sub3       b.0x28,b.0x24,b.0xB0
08002E61: FC 69 49 48 6D                w add3       b.0x24,b.0x20,b.0xB4
08002E66: 4F 6C                         w incr       b.0xB0
08002E68: FD 3D C1 3A                   w2 laddr     b.0x3A
08002E6C: 21 6F                         w2 =:        b.0xBC
08002E6E: 1A 04 6E                      w move       $0x4,b.0xB8
08002E71: CA 08                         if < go      $0x8
08002E73: 84                            bi1 clr
08002E74: 85                            bi2 clr
08002E75: FD 67 6C 6E                   by smove     b.0xB0,b.0xB8
08002E79: 84                            bi1 clr
08002E7A: FC 10 4D                      h1 =:        b.0x34
08002E7D: FC 10 C1 32                   h1 =:        b.0x32
08002E81: 0D 47                         w2 :=        b.0x1C
08002E83: FC 11 C1 2E                   h2 =:        b.0x2E
08002E87: 0E 46                         w3 :=        b.0x18
08002E89: 22 70                         w3 =:        b.0xC0
08002E8B: FD 50 D1 D3                   h wconv      r2,r4
08002E8F: 37 D2                         w4 comp      r3
08002E91: CA 4D                         if < go      $0x4D
08002E93: FD 50 C1 2E D0                h wconv      b.0x2E,r1
08002E98: 05 CD 20                      by2 :=       $0x20
08002E9B: 2D E4 14 D1                   by comp2     @b.0x14+,r2
08002E9F: C4 11                         if = go      $0x11
08002EA1: FD 50 C1 32 D2                h wconv      b.0x32,r3
08002EA6: 44 D2                         w test       r3
08002EA8: C6 08                         if >< go     $0x8
08002EAA: FC 14 C1 2E C1 32             h move       b.0x2E,b.0x32
08002EB0: FD 50 C1 2E D2                h wconv      b.0x2E,r3
08002EB5: 07 CD 3A                      by4 :=       $0x3A
08002EB8: 2D E6 14 D3                   by comp2     @b.0x14+,r4
08002EBC: C6 0F                         if >< go     $0xF
08002EBE: FD 50 4D D0                   h wconv      b.0x34,r1
08002EC2: 44 D0                         w test       r1
08002EC4: C6 07                         if >< go     $0x7
08002EC6: FC 14 C1 2E 4D                h move       b.0x2E,b.0x34
08002ECB: FD 50 C1 2E D0                h wconv      b.0x2E,r1
08002ED0: 60 01                         w1 -         $0x1
08002ED2: FC 10 C1 2E                   h1 =:        b.0x2E
08002ED6: FD 50 D0 D1                   h wconv      r1,r2
08002EDA: 35 70                         w2 comp      b.0xC0
08002EDC: CC B7                         if >= go     $0xFFFFFFFFFFFFFFB7
08002EDE: FD 50 4D D0                   h wconv      b.0x34,r1
08002EE2: 44 D0                         w test       r1
08002EE4: CE 47                         if <= go     $0x47
08002EE6: FE 79 00 C1 3A 01             w bmove      $0x0,b.0x3A,$0x1
08002EEC: 49 C1 2E                      h stz        b.0x2E
08002EEF: 08 C1 2E                      h1 :=        b.0x2E
08002EF2: FC 38 4D                      h1 +         b.0x34
08002EF5: FC 38 01                      h1 +         $0x1
08002EF8: FD 50 D0 D1                   h wconv      r1,r2
08002EFC: 06 0D                         by3 :=       $0xD
08002EFE: 2D E5 14 D2                   by comp2     @b.0x14+,r3
08002F02: C4 20                         if = go      $0x20
08002F04: 0B C1 2E                      h4 :=        b.0x2E
08002F07: FC 3B 4D                      h4 +         b.0x34
08002F0A: FC 3B 01                      h4 +         $0x1
08002F0D: FD 50 D3 D0                   h wconv      r4,r1
08002F11: FD 50 C1 2E D1                h wconv      b.0x2E,r2
08002F16: 06 E4 14                      by3 :=       @b.0x14+
08002F19: 1E D5 3A                      by3 =:       b.0x3A+
08002F1C: FC DF C1 2E 03 D3 08 4D FC    w loopi      b.0x2E,$0x3,$0xFFFFFFFFD3084DFC
08002F25: 40 01 FC 10 C1 32 49          d comp2      $0x1,r1.(0x10C13249)
08002F2C: 4E FD 20 45 5B 0C             h incr       r2.(0x20455B0C)
08002F32: 08 4E                         h1 :=        b.0x38
08002F34: C3 08 00 2D 81 00             call         $0x8002D81,$0x0
08002F3A: D2 08                         if -k go     $0x8
08002F3C: C3 08 00 2E 2F 00             call         $0x8002E2F,$0x0
08002F42: 49 4C                         h stz        b.0x30
08002F44: 18 42                         r:=          b.0x8
08002F46: FD 20 C4 08 00 6B 00 86 0C    by bmove     $0x8006B00,r.0x18,$0xC
08002F4F: FD 50 C1 36 D1                h wconv      b.0x36,r2
08002F54: 1A D1 73                      w move       r2,b.0xCC
08002F57: 4A 72                         w stz        b.0xC8
08002F59: FD 3E C1 3E                   w3 laddr     b.0x3E
08002F5D: 22 71                         w3 =:        b.0xC4
08002F5F: FD 20 71 89 0C                by bmove     b.0xC4,r.0x24,$0xC
08002F64: FD 3F C1 3A                   w4 laddr     b.0x3A
08002F68: 23 8C                         w4 =:        r.0x30
08002F6A: 4A 8D                         w stz        r.0x34
08002F6C: 1A 03 8E                      w move       $0x3,r.0x38
08002F6F: C3 08 00 33 0B 00             call         $0x800330B,$0x0
08002F75: D2 08                         if -k go     $0x8
08002F77: C3 08 00 2E 2F 00             call         $0x8002E2F,$0x0
08002F7D: 18 42                         r:=          b.0x8
08002F7F: FD 55 85 C1 2E                w hconv      r.0x14,b.0x2E
08002F84: FD 50 4C D1                   h wconv      b.0x30,r2
08002F88: 44 D1                         w test       r2
08002F8A: C6 06                         if >< go     $0x6
08002F8C: 08 C1 2E                      h1 :=        b.0x2E
08002F8F: 80                            ret
08002F90: 49 4C                         h stz        b.0x30
08002F92: FD 20 C4 08 00 6B 10 86 0C    by bmove     $0x8006B10,r.0x18,$0xC
08002F9B: FD 50 C1 36 D1                h wconv      b.0x36,r2
08002FA0: 1A D1 73                      w move       r2,b.0xCC
08002FA3: 4A 72                         w stz        b.0xC8
08002FA5: FD 3E C1 3E                   w3 laddr     b.0x3E
08002FA9: 22 71                         w3 =:        b.0xC4
08002FAB: FD 20 71 89 0C                by bmove     b.0xC4,r.0x24,$0xC
08002FB0: FD 3F C1 3A                   w4 laddr     b.0x3A
08002FB4: 23 8C                         w4 =:        r.0x30
08002FB6: 4A 8D                         w stz        r.0x34
08002FB8: 1A 03 8E                      w move       $0x3,r.0x38
08002FBB: C3 08 00 33 0B 00             call         $0x800330B,$0x0
08002FC1: D2 08                         if -k go     $0x8
08002FC3: C3 08 00 2E 2F 00             call         $0x8002E2F,$0x0
08002FC9: 18 42                         r:=          b.0x8
08002FCB: FD 55 85 C1 2E                w hconv      r.0x14,b.0x2E
08002FD0: FD 50 4C D1                   h wconv      b.0x30,r2
08002FD4: 44 D1                         w test       r2
08002FD6: C6 06                         if >< go     $0x6
08002FD8: 08 C1 2E                      h1 :=        b.0x2E
08002FDB: 80                            ret
08002FDC: FD 1F 4E 09 FF                f loopi      b.0x38,$0x9,$0xFFFFFFFFFFFFFFFF
08002FE1: 51 08                         w decr       $0x8
08002FE3: 01                    ??? ; opcode 0x0001
08002FE4: 80                            ret
08002FE5: B8 CF 00 00 00 24             ents         $0x24
08002FEB: FC 10 45                      h1 =:        b.0x14
08002FEE: 4A 46                         w stz        b.0x18
08002FF0: 85                            bi2 clr
08002FF1: 21 47                         w2 =:        b.0x1C
08002FF3: 21 48                         w2 =:        b.0x20
08002FF5: FD 50 D0 D2                   h wconv      r1,r3
08002FF9: 18 42                         r:=          b.0x8
08002FFB: 22 85                         w3 =:        r.0x14
08002FFD: 1A 47 86                      w move       b.0x1C,r.0x18
08003000: 1A 46 87                      w move       b.0x18,r.0x1C
08003003: 21 88                         w2 =:        r.0x20
08003005: C3 08 00 3F 1F 00             call         $0x8003F1F,$0x0
0800300B: 9D                            ifkret
0800300C: 18 42                         r:=          b.0x8
0800300E: 1A 88 48                      w move       r.0x20,b.0x20
08003011: 2E 48 01                      w comp2      b.0x20,$0x1
08003014: CA 07                         if < go      $0x7
08003016: 2E 48 13                      w comp2      b.0x20,$0x13
08003019: CE 15                         if <= go     $0x15
0800301B: FD 50 45 D1                   h wconv      b.0x14,r2
0800301F: 21 85                         w2 =:        r.0x14
08003021: 1A 48 86                      w move       b.0x20,r.0x18
08003024: C3 08 00 3F 31 00             call         $0x8003F31,$0x0
0800302A: 9D                            ifkret
0800302B: 0C 3F                         w1 :=        $0x3F
0800302D: 80                            ret
0800302E: 0C 48                         w1 :=        b.0x20
08003030: 80                            ret
08003031: B8 CF 00 00 00 1C             ents         $0x1C
08003037: FC 10 46                      h1 =:        b.0x18
0800303A: 2E 45 3F                      w comp2      b.0x14,$0x3F
0800303D: C4 14                         if = go      $0x14
0800303F: FD 50 D0 D1                   h wconv      r1,r2
08003043: 18 42                         r:=          b.0x8
08003045: 21 85                         w2 =:        r.0x14
08003047: 1A 45 86                      w move       b.0x14,r.0x18
0800304A: C3 08 00 3F 31 00             call         $0x8003F31,$0x0
08003050: 9D                            ifkret
08003051: 80                            ret
08003052: B8 CF 00 00 00 68             ents         $0x68
08003058: FC 10 4B                      h1 =:        b.0x2C
0800305B: 2E 46 3F                      w comp2      b.0x18,$0x3F
0800305E: C6 04                         if >< go     $0x4
08003060: C0 60                         go           $0x60
08003062: 0D 4A                         w2 :=        b.0x28
08003064: 0E 49                         w3 :=        b.0x24
08003066: 61 D2                         w2 -         r3
08003068: 55 01                         w2 +         $0x1
0800306A: 21 4D                         w2 =:        b.0x34
0800306C: FD 20 48 53 0C                by bmove     b.0x20,b.0x4C,$0xC
08003071: 0F 49                         w4 :=        b.0x24
08003073: 57 53                         w4 +         b.0x4C
08003075: 23 50                         w4 =:        b.0x40
08003077: 0C 1B                         w1 :=        $0x1B
08003079: 0D 46                         w2 :=        b.0x18
0800307B: FC AA D1 D0                   w shl        r2,r1
0800307F: 21 4F                         w2 =:        b.0x3C
08003081: FD 50 47 D0                   h wconv      b.0x1C,r1
08003085: FD 50 C1 1E D1                h wconv      b.0x1E,r2
0800308A: 6D D0                         w2 *         r1
0800308C: 6D 02                         w2 *         $0x2
0800308E: 21 4E                         w2 =:        b.0x38
08003090: FC 16 4B 02                   h comp2      b.0x2C,$0x2
08003094: C6 0A                         if >< go     $0xA
08003096: E5 CF 07 FF FF FF             w2 and       $0x7FFFFFF
0800309C: 21 4E                         w2 =:        b.0x38
0800309E: 53 4F D1                      w add2       b.0x3C,r2
080030A1: FC 16 4B 01                   h comp2      b.0x2C,$0x1
080030A5: C6 0A                         if >< go     $0xA
080030A7: 1A 50 51                      w move       b.0x40,b.0x44
080030AA: 1A 4F 52                      w move       b.0x3C,b.0x48
080030AD: C0 08                         go           $0x8
080030AF: 1A 4F 51                      w move       b.0x3C,b.0x44
080030B2: 1A 50 52                      w move       b.0x40,b.0x48
080030B5: FD 20 C5 44 C5 48 4D          by bmove     @b.0x44,@b.0x48,b.0x34
080030BC: FD B9 00                      st1:=        $0x0
080030BF: 80                            ret
080030C0: C0 0D                         go           $0xD
080030C2: 9C                            entd
080030C3: FD C0 56                      l=:          b.0x58
080030C6: 20 43                         w1 =:        b.0xC
080030C8: 81                            retk
080030C9: FE 03                         clrk
080030CB: B4 56                         jumpg        b.0x58
080030CD: 0D 49                         w2 :=        b.0x24
080030CF: 21 4C                         w2 =:        b.0x30
080030D1: 0E 4A                         w3 :=        b.0x28
080030D3: 62 D1                         w3 -         r2
080030D5: 22 4D                         w3 =:        b.0x34
080030D7: FC 16 4B 01                   h comp2      b.0x2C,$0x1
080030DB: C6 32                         if >< go     $0x32
080030DD: FD 50 45 D3                   h wconv      b.0x14,r4
080030E1: 18 42                         r:=          b.0x8
080030E3: 23 85                         w4 =:        r.0x14
080030E5: FD 50 C1 1E D0                h wconv      b.0x1E,r1
080030EA: 20 86                         w1 =:        r.0x18
080030EC: 56 D1                         w3 +         r2
080030EE: 1A D2 59                      w move       r3,b.0x64
080030F1: 1A D1 58                      w move       r2,b.0x60
080030F4: 0D 48                         w2 :=        b.0x20
080030F6: 21 57                         w2 =:        b.0x5C
080030F8: FD 20 57 87 0C                by bmove     b.0x5C,r.0x1C,$0xC
080030FD: C3 08 00 35 DB 00             call         $0x80035DB,$0x0
08003103: D2 08                         if -k go     $0x8
08003105: C3 08 00 30 C2 00             call         $0x80030C2,$0x0
0800310B: C0 30                         go           $0x30
0800310D: FD 50 45 D3                   h wconv      b.0x14,r4
08003111: 18 42                         r:=          b.0x8
08003113: 23 85                         w4 =:        r.0x14
08003115: FD 50 C1 1E D1                h wconv      b.0x1E,r2
0800311A: 21 86                         w2 =:        r.0x18
0800311C: 56 4C                         w3 +         b.0x30
0800311E: 1A D2 59                      w move       r3,b.0x64
08003121: 1A 4C 58                      w move       b.0x30,b.0x60
08003124: 0C 48                         w1 :=        b.0x20
08003126: 20 57                         w1 =:        b.0x5C
08003128: FD 20 57 87 0C                by bmove     b.0x5C,r.0x1C,$0xC
0800312D: C3 08 00 35 87 00             call         $0x8003587,$0x0
08003133: D2 08                         if -k go     $0x8
08003135: C3 08 00 30 C2 00             call         $0x80030C2,$0x0
0800313B: 80                            ret
0800313C: 9C                            entd
0800313D: FD C0 51                      l=:          b.0x44
08003140: 0C 49                         w1 :=        b.0x24
08003142: 2E 4D D0                      w comp2      b.0x34,r1
08003145: CA 22                         if < go      $0x22
08003147: 0D 4A                         w2 :=        b.0x28
08003149: 2E 4D D1                      w comp2      b.0x34,r2
0800314C: C8 1B                         if > go      $0x1B
0800314E: 0F 4D                         w4 :=        b.0x34
08003150: 06 E7 20                      by3 :=       @b.0x20+
08003153: 1E C1 49                      by3 =:       b.0x49
08003156: 41 E2 08 00 6B 1C             bi test      $0x8006B1C+
0800315C: C4 0B                         if = go      $0xB
0800315E: 04 E7 20                      by1 :=       @b.0x20+
08003161: FE 03                         clrk
08003163: B4 51                         jumpg        b.0x44
08003165: C0 08                         go           $0x8
08003167: 04 00                         by1 :=       $0x0
08003169: FE 03                         clrk
0800316B: B4 51                         jumpg        b.0x44
0800316D: 9C                            entd
0800316E: FD C0 53                      l=:          b.0x4C
08003171: 0C 46                         w1 :=        b.0x18
08003173: 2E 4C D0                      w comp2      b.0x30,r1
08003176: CA 22                         if < go      $0x22
08003178: 0D 47                         w2 :=        b.0x1C
0800317A: 2E 4C D1                      w comp2      b.0x30,r2
0800317D: C8 1B                         if > go      $0x1B
0800317F: 0F 4C                         w4 :=        b.0x30
08003181: 06 E7 14                      by3 :=       @b.0x14+
08003184: 1E C1 51                      by3 =:       b.0x51
08003187: 41 E2 08 00 6B 1C             bi test      $0x8006B1C+
0800318D: C4 0B                         if = go      $0xB
0800318F: 04 E7 14                      by1 :=       @b.0x14+
08003192: FE 03                         clrk
08003194: B4 53                         jumpg        b.0x4C
08003196: C0 08                         go           $0x8
08003198: 04 00                         by1 :=       $0x0
0800319A: FE 03                         clrk
0800319C: B4 53                         jumpg        b.0x4C
0800319E: B8 CF 00 00 00 54             ents         $0x54
080031A4: 0C 46                         w1 :=        b.0x18
080031A6: 20 4C                         w1 =:        b.0x30
080031A8: 0D 49                         w2 :=        b.0x24
080031AA: 21 4D                         w2 =:        b.0x34
080031AC: 0E 4A                         w3 :=        b.0x28
080031AE: 0F 49                         w4 :=        b.0x24
080031B0: 62 D3                         w3 -         r4
080031B2: 56 01                         w3 +         $0x1
080031B4: 36 01                         w3 comp      $0x1
080031B6: C6 3B                         if >< go     $0x3B
080031B8: 0E 47                         w3 :=        b.0x1C
080031BA: 0C 46                         w1 :=        b.0x18
080031BC: 62 D0                         w3 -         r1
080031BE: 56 01                         w3 +         $0x1
080031C0: 22 4E                         w3 =:        b.0x38
080031C2: 36 01                         w3 comp      $0x1
080031C4: C4 18                         if = go      $0x18
080031C6: 36 01                         w3 comp      $0x1
080031C8: CE 29                         if <= go     $0x29
080031CA: 0D 4C                         w2 :=        b.0x30
080031CC: 55 01                         w2 +         $0x1
080031CE: 07 E5 14                      by4 :=       @b.0x14+
080031D1: 1F C1 52                      by4 =:       b.0x52
080031D4: 41 E3 08 00 6B 1C             bi test      $0x8006B1C+
080031DA: C6 17                         if >< go     $0x17
080031DC: 0F 4D                         w4 :=        b.0x34
080031DE: 05 E7 20                      by2 :=       @b.0x20+
080031E1: 0C 4C                         w1 :=        b.0x30
080031E3: 2D E4 14 D1                   by comp2     @b.0x14+,r2
080031E7: C6 07                         if >< go     $0x7
080031E9: 0C 02                         w1 :=        $0x2
080031EB: 80                            ret
080031EC: C0 05                         go           $0x5
080031EE: 0C 00                         w1 :=        $0x0
080031F0: 80                            ret
080031F1: C3 08 00 31 6D 00             call         $0x800316D,$0x0
080031F7: 9D                            ifkret
080031F8: 1C 50                         by1 =:       b.0x40
080031FA: 44 D0                         w test       r1
080031FC: C4 14                         if = go      $0x14
080031FE: C3 08 00 31 3C 00             call         $0x800313C,$0x0
08003204: 9D                            ifkret
08003205: 2D 50 D0                      by comp2     b.0x40,r1
08003208: C6 08                         if >< go     $0x8
0800320A: 4F 4C                         w incr       b.0x30
0800320C: 4F 4D                         w incr       b.0x34
0800320E: C0 E3                         go           $0xFFFFFFFFFFFFFFE3
08003210: C3 08 00 31 3C 00             call         $0x800313C,$0x0
08003216: 9D                            ifkret
08003217: 44 D0                         w test       r1
08003219: C6 16                         if >< go     $0x16
0800321B: C3 08 00 31 6D 00             call         $0x800316D,$0x0
08003221: 9D                            ifkret
08003222: 44 D0                         w test       r1
08003224: C6 07                         if >< go     $0x7
08003226: 1A 02 4F                      w move       $0x2,b.0x3C
08003229: C0 04                         go           $0x4
0800322B: 4A 4F                         w stz        b.0x3C
0800322D: C0 65                         go           $0x65
0800322F: C3 08 00 31 6D 00             call         $0x800316D,$0x0
08003235: 9D                            ifkret
08003236: 1C 50                         by1 =:       b.0x40
08003238: 30 CD 20                      by1 comp     $0x20
0800323B: C4 07                         if = go      $0x7
0800323D: 30 CD 2D                      by1 comp     $0x2D
08003240: C6 46                         if >< go     $0x46
08003242: 4F 4D                         w incr       b.0x34
08003244: C3 08 00 31 3C 00             call         $0x800313C,$0x0
0800324A: 9D                            ifkret
0800324B: 1C C1 41                      by1 =:       b.0x41
0800324E: 44 D0                         w test       r1
08003250: C4 0A                         if = go      $0xA
08003252: 2D C1 41 50                   by comp2     b.0x41,b.0x40
08003256: C4 04                         if = go      $0x4
08003258: C0 EA                         go           $0xFFFFFFFFFFFFFFEA
0800325A: C3 08 00 31 3C 00             call         $0x800313C,$0x0
08003260: 9D                            ifkret
08003261: 44 D0                         w test       r1
08003263: C4 23                         if = go      $0x23
08003265: 4F 4C                         w incr       b.0x30
08003267: 4F 4D                         w incr       b.0x34
08003269: C3 08 00 31 6D 00             call         $0x800316D,$0x0
0800326F: 9D                            ifkret
08003270: 1C 50                         by1 =:       b.0x40
08003272: 44 D0                         w test       r1
08003274: C4 10                         if = go      $0x10
08003276: C3 08 00 31 3C 00             call         $0x800313C,$0x0
0800327C: 9D                            ifkret
0800327D: 2D 50 D0                      by comp2     b.0x40,r1
08003280: C6 04                         if >< go     $0x4
08003282: C0 E3                         go           $0xFFFFFFFFFFFFFFE3
08003284: C0 AB                         go           $0xFFFFFFFFFFFFFFAB
08003286: 05 50                         by2 :=       b.0x40
08003288: 44 D1                         w test       r2
0800328A: C6 06                         if >< go     $0x6
0800328C: 4D 4F                         w set1       b.0x3C
0800328E: C0 04                         go           $0x4
08003290: 4A 4F                         w stz        b.0x3C
08003292: 0C 4F                         w1 :=        b.0x3C
08003294: 80                            ret
08003295: B8 CF 00 00 00 34             ents         $0x34
0800329B: 0C 46                         w1 :=        b.0x18
0800329D: 20 48                         w1 =:        b.0x20
0800329F: 0D 47                         w2 :=        b.0x1C
080032A1: 21 49                         w2 =:        b.0x24
080032A3: 21 4B                         w2 =:        b.0x2C
080032A5: 35 D0                         w2 comp      r1
080032A7: CA 1C                         if < go      $0x1C
080032A9: 0D 4B                         w2 :=        b.0x2C
080032AB: 04 E5 14                      by1 :=       @b.0x14+
080032AE: 1C 4C                         by1 =:       b.0x30
080032B0: 30 CD 20                      by1 comp     $0x20
080032B3: C4 0B                         if = go      $0xB
080032B5: 42 D0                         by test      r1
080032B7: C4 07                         if = go      $0x7
080032B9: 30 CD A0                      by1 comp     $0xA0
080032BC: C6 07                         if >< go     $0x7
080032BE: FD 25 4B 48 EB                d loopd      b.0x2C,b.0x20,$0xFFFFFFFFFFFFFFEB
080032C3: 1A 4B 49                      w move       b.0x2C,b.0x24
080032C6: 1A 48 4B                      w move       b.0x20,b.0x2C
080032C9: 2E 4B 49                      w comp2      b.0x2C,b.0x24
080032CC: C8 1C                         if > go      $0x1C
080032CE: 0D 4B                         w2 :=        b.0x2C
080032D0: 04 E5 14                      by1 :=       @b.0x14+
080032D3: 1C C1 31                      by1 =:       b.0x31
080032D6: 30 CD 20                      by1 comp     $0x20
080032D9: C4 0B                         if = go      $0xB
080032DB: 42 D0                         by test      r1
080032DD: C4 07                         if = go      $0x7
080032DF: 30 CD A0                      by1 comp     $0xA0
080032E2: C6 06                         if >< go     $0x6
080032E4: BF 4B 49 EA                   d loopi      b.0x2C,b.0x24,$0xFFFFFFFFFFFFFFEA
080032E8: 1A 4B 48                      w move       b.0x2C,b.0x20
080032EB: 0C 49                         w1 :=        b.0x24
080032ED: 60 48                         w1 -         b.0x20
080032EF: 54 01                         w1 +         $0x1
080032F1: 80                            ret
080032F2: B8 CF 00 00 00 1C             ents         $0x1C
080032F8: 20 46                         w1 =:        b.0x18
080032FA: FC AD D0 3F                   w sha        r1,$0x3F
080032FE: 18 42                         r:=          b.0x8
08003300: 1A 45 85                      w move       b.0x14,r.0x14
08003303: C3 08 00 3E A3 00             call         $0x8003EA3,$0x0
08003309: 9D                            ifkret
0800330A: 80                            ret
0800330B: B8 CF 00 00 01 18             ents         $0x118
08003311: C0 0D                         go           $0xD
08003313: 9C                            entd
08003314: FD C0 7D                      l=:          b.0xF4
08003317: 20 43                         w1 =:        b.0xC
08003319: 81                            retk
0800331A: FE 03                         clrk
0800331C: B4 7D                         jumpg        b.0xF4
0800331E: 4A 55                         w stz        b.0x54
08003320: 0D 4E                         w2 :=        b.0x38
08003322: 35 3F                         w2 comp      $0x3F
08003324: C6 1B                         if >< go     $0x1B
08003326: FD 3D 56                      w2 laddr     b.0x58
08003329: 21 7F                         w2 =:        b.0xFC
0800332B: 1A 05 7E                      w move       $0x5,b.0xF8
0800332E: CA 0C                         if < go      $0xC
08003330: 84                            bi1 clr
08003331: 85                            bi2 clr
08003332: FD 67 C4 08 00 6B C4 7E       by smove     $0x8006BC4,b.0xF8
0800333A: 1A 04 55                      w move       $0x4,b.0x54
0800333D: C0 5A                         go           $0x5A
0800333F: 0C 4D                         w1 :=        b.0x34
08003341: 20 51                         w1 =:        b.0x44
08003343: 0D 4E                         w2 :=        b.0x38
08003345: 21 C2 01 00                   w2 =:        b.0x100
08003349: 34 D1                         w1 comp      r2
0800334B: C8 4C                         if > go      $0x4C
0800334D: 0D 51                         w2 :=        b.0x44
0800334F: 04 E5 30                      by1 :=       @b.0x30+
08003352: FC 90 0F                      by1 and      $0xF
08003355: 20 54                         w1 =:        b.0x50
08003357: 06 E5 30                      by3 :=       @b.0x30+
0800335A: FC A8 D2 3C                   by shl       r3,$0x3C
0800335E: 22 53                         w3 =:        b.0x4C
08003360: FD D3 E2 08 00 6B 90 D0       w4 getbi     $0x8006B90+,r1
08003368: C6 04                         if >< go     $0x4
0800336A: C0 2D                         go           $0x2D
0800336C: 07 E5 30                      by4 :=       @b.0x30+
0800336F: 0C 55                         w1 :=        b.0x54
08003371: 1F D4 58                      by4 =:       b.0x58+
08003374: 05 D4 58                      by2 :=       b.0x58+
08003377: FC 91 CD 60                   by2 and      $0x60
0800337B: 31 CD 60                      by2 comp     $0x60
0800337E: C6 0C                         if >< go     $0xC
08003380: 05 D4 58                      by2 :=       b.0x58+
08003383: FC 3D CD 20                   by2 -        $0x20
08003387: 1D D4 58                      by2 =:       b.0x58+
0800338A: 4F 55                         w incr       b.0x54
0800338C: 2E 55 04                      w comp2      b.0x54,$0x4
0800338F: CC 08                         if >= go     $0x8
08003391: BF 51 C2 01 00 BC             d loopi      b.0x44,b.0x100,$0xFFFFFFFFFFFFFFBC
08003397: 0C 55                         w1 :=        b.0x54
08003399: 19 CD 27 D4 58                by move      $0x27,b.0x58+
0800339E: 4A 55                         w stz        b.0x54
080033A0: FD 3C 50                      w1 laddr     b.0x40
080033A3: 20 7F                         w1 =:        b.0xFC
080033A5: 1A 02 7E                      w move       $0x2,b.0xF8
080033A8: CA 09                         if < go      $0x9
080033AA: 85                            bi2 clr
080033AB: 06 CD 20                      by3 :=       $0x20
080033AE: FD 82 7E                      by3 sfill    b.0xF8
080033B1: 0C 47                         w1 :=        b.0x1C
080033B3: 20 51                         w1 =:        b.0x44
080033B5: 0D 48                         w2 :=        b.0x20
080033B7: 21 C2 01 04                   w2 =:        b.0x104
080033BB: 34 D1                         w1 comp      r2
080033BD: C8 48                         if > go      $0x48
080033BF: 04 CD 20                      by1 :=       $0x20
080033C2: 0D 51                         w2 :=        b.0x44
080033C4: 2D E5 18 D0                   by comp2     @b.0x18+,r1
080033C8: C4 32                         if = go      $0x32
080033CA: 06 E5 18                      by3 :=       @b.0x18+
080033CD: FD 3F 50                      w4 laddr     b.0x40
080033D0: 23 C2 01 08                   w4 =:        b.0x108
080033D4: 0C 55                         w1 :=        b.0x54
080033D6: 1E E8 01 08                   by3 =:       @b.0x108+
080033DA: 05 E4 18                      by2 :=       @b.0x18+
080033DD: FC 91 CD 60                   by2 and      $0x60
080033E1: 31 CD 60                      by2 comp     $0x60
080033E4: C6 14                         if >< go     $0x14
080033E6: 05 E4 18                      by2 :=       @b.0x18+
080033E9: FC 3D CD 20                   by2 -        $0x20
080033ED: FD 3E 50                      w3 laddr     b.0x40
080033F0: 22 C2 01 08                   w3 =:        b.0x108
080033F4: 1D E8 01 08                   by2 =:       @b.0x108+
080033F8: 4F 55                         w incr       b.0x54
080033FA: 2E 55 02                      w comp2      b.0x54,$0x2
080033FD: CC 08                         if >= go     $0x8
080033FF: BF 51 C2 01 04 C0             d loopi      b.0x44,b.0x104,$0xFFFFFFFFFFFFFFC0
08003405: 4A 55                         w stz        b.0x54
08003407: 0C 4A                         w1 :=        b.0x28
08003409: 20 51                         w1 =:        b.0x44
0800340B: 0D 4B                         w2 :=        b.0x2C
0800340D: 21 C2 01 08                   w2 =:        b.0x108
08003411: 34 D1                         w1 comp      r2
08003413: C8 4E                         if > go      $0x4E
08003415: 0D 51                         w2 :=        b.0x44
08003417: 04 E5 24                      by1 :=       @b.0x24+
0800341A: FC 90 0F                      by1 and      $0xF
0800341D: 20 54                         w1 =:        b.0x50
0800341F: 06 E5 24                      by3 :=       @b.0x24+
08003422: FC A8 D2 3C                   by shl       r3,$0x3C
08003426: 22 53                         w3 =:        b.0x4C
08003428: FD D3 E2 08 00 6B 90 D0       w4 getbi     $0x8006B90+,r1
08003430: C6 04                         if >< go     $0x4
08003432: C0 2F                         go           $0x2F
08003434: 07 E5 24                      by4 :=       @b.0x24+
08003437: 0C 55                         w1 :=        b.0x54
08003439: 1F D4 5D                      by4 =:       b.0x5D+
0800343C: 06 D4 5D                      by3 :=       b.0x5D+
0800343F: FC 92 CD 60                   by3 and      $0x60
08003443: 32 CD 60                      by3 comp     $0x60
08003446: C6 0C                         if >< go     $0xC
08003448: 06 D4 5D                      by3 :=       b.0x5D+
0800344B: FC 3E CD 20                   by3 -        $0x20
0800344F: 1E D4 5D                      by3 =:       b.0x5D+
08003452: 4F 55                         w incr       b.0x54
08003454: 2E 55 CE 00 95                w comp2      b.0x54,$0x95
08003459: CC 08                         if >= go     $0x8
0800345B: BF 51 C2 01 08 BA             d loopi      b.0x44,b.0x108,$0xFFFFFFFFFFFFFFBA
08003461: 0C 55                         w1 :=        b.0x54
08003463: 19 CD 27 D4 5D                by move      $0x27,b.0x5D+
08003468: 0D 00                         w2 :=        $0x0
0800346A: FE 25 E1 08 00 6B 40          by2 laddr    $0x8006B40+
08003471: 21 4F                         w2 =:        b.0x3C
08003473: 0E CD 48                      w3 :=        $0x48
08003476: FE 26 E2 08 00 6B 40          by3 laddr    $0x8006B40+
0800347D: 22 C2 01 0C                   w3 =:        b.0x10C
08003481: 35 D2                         w2 comp      r3
08003483: D4 1D                         if >> go     $0x1D
08003485: 08 50                         h1 :=        b.0x40
08003487: 18 4F                         r:=          b.0x3C
08003489: FC 16 80 D0                   h comp2      r.0x0,r1
0800348D: C4 0E                         if = go      $0xE
0800348F: 53 4F 08                      w add2       b.0x3C,$0x8
08003492: 2E 4F C2 01 0C                w comp2      b.0x3C,b.0x10C
08003497: DA EE                         if <<= go    $0xFFFFFFFFFFFFFFEE
08003499: C0 07                         go           $0x7
0800349B: 1A 81 52                      w move       r.0x4,b.0x48
0800349E: C0 05                         go           $0x5
080034A0: 1A 3F 52                      w move       $0x3F,b.0x48
080034A3: 2E 52 3F                      w comp2      b.0x48,$0x3F
080034A6: C7 00 79                      if >< go     $0x79
080034A9: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
080034AE: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
080034B3: 4F 7E                         w incr       b.0xF8
080034B5: 84                            bi1 clr
080034B6: 85                            bi2 clr
080034B7: FD BE 7E C4 08 00 6B CC 00    by scopa     b.0xF8,$0x8006BCC,$0x0
080034C0: C6 07                         if >< go     $0x7
080034C2: 1A 0B 52                      w move       $0xB,b.0x48
080034C5: C0 5A                         go           $0x5A
080034C7: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
080034CC: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
080034D1: 4F 7E                         w incr       b.0xF8
080034D3: 84                            bi1 clr
080034D4: 85                            bi2 clr
080034D5: FD BE 7E C4 08 00 6B D4 00    by scopa     b.0xF8,$0x8006BD4,$0x0
080034DE: C6 07                         if >< go     $0x7
080034E0: 1A 0A 52                      w move       $0xA,b.0x48
080034E3: C0 3C                         go           $0x3C
080034E5: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
080034EA: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
080034EF: 4F 7E                         w incr       b.0xF8
080034F1: 84                            bi1 clr
080034F2: 85                            bi2 clr
080034F3: FD BE 7E C4 08 00 6B DC 00    by scopa     b.0xF8,$0x8006BDC,$0x0
080034FC: C6 07                         if >< go     $0x7
080034FE: 1A 13 52                      w move       $0x13,b.0x48
08003501: C0 1E                         go           $0x1E
08003503: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
08003508: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
0800350D: 4F 7E                         w incr       b.0xF8
0800350F: 84                            bi1 clr
08003510: 85                            bi2 clr
08003511: FD BE 7E C4 08 00 6B E4 00    by scopa     b.0xF8,$0x8006BE4,$0x0
0800351A: C6 05                         if >< go     $0x5
0800351C: 1A 12 52                      w move       $0x12,b.0x48
0800351F: FD 3C C1 5D                   w1 laddr     b.0x5D
08003523: 18 42                         r:=          b.0x8
08003525: 20 85                         w1 =:        r.0x14
08003527: 4A 86                         w stz        r.0x18
08003529: 1A CE 00 95 87                w move       $0x95,r.0x1C
0800352E: FD 3D 56                      w2 laddr     b.0x58
08003531: 21 88                         w2 =:        r.0x20
08003533: 4A 89                         w stz        r.0x24
08003535: 1A 04 8A                      w move       $0x4,r.0x28
08003538: 1A 52 8B                      w move       b.0x48,r.0x2C
0800353B: C3 08 00 3D EF 00             call         $0x8003DEF,$0x0
08003541: D2 08                         if -k go     $0x8
08003543: C3 08 00 33 13 00             call         $0x8003313,$0x0
08003549: 20 45                         w1 =:        b.0x14
0800354B: 80                            ret
0800354C: B8 CF 00 00 00 18             ents         $0x18
08003552: 18 42                         r:=          b.0x8
08003554: 1A 45 85                      w move       b.0x14,r.0x14
08003557: C3 08 00 3D E0 00             call         $0x8003DE0,$0x0
0800355D: 9D                            ifkret
0800355E: 80                            ret
0800355F: B8 CF 00 00 00 1C             ents         $0x1C
08003565: 20 46                         w1 =:        b.0x18
08003567: 18 42                         r:=          b.0x8
08003569: 1A 45 85                      w move       b.0x14,r.0x14
0800356C: C3 08 00 3E 8F 00             call         $0x8003E8F,$0x0
08003572: 9D                            ifkret
08003573: 80                            ret
08003574: B8 CF 00 00 00 1C             ents         $0x1C
0800357A: 18 42                         r:=          b.0x8
0800357C: 1A 45 85                      w move       b.0x14,r.0x14
0800357F: C3 08 00 3E 67 00             call         $0x8003E67,$0x0
08003585: 9D                            ifkret
08003586: 80                            ret
08003587: B8 CF 00 00 00 44             ents         $0x44
0800358D: 0C 49                         w1 :=        b.0x24
0800358F: 0D 48                         w2 :=        b.0x20
08003591: 60 D1                         w1 -         r2
08003593: 54 01                         w1 +         $0x1
08003595: 54 01                         w1 +         $0x1
08003597: FC AD D0 3F                   w sha        r1,$0x3F
0800359B: 20 4B                         w1 =:        b.0x2C
0800359D: 0E 48                         w3 :=        b.0x20
0800359F: 7A 04                         w3 /         $0x4
080035A1: 22 4C                         w3 =:        b.0x30
080035A3: 0F 49                         w4 :=        b.0x24
080035A5: 7B 04                         w4 /         $0x4
080035A7: 23 4D                         w4 =:        b.0x34
080035A9: 18 42                         r:=          b.0x8
080035AB: 1A 45 85                      w move       b.0x14,r.0x14
080035AE: 1A C4 08 00 6B EC 86          w move       $0x8006BEC,r.0x18
080035B5: 1A D3 50                      w move       r4,b.0x40
080035B8: 1A D2 4F                      w move       r3,b.0x3C
080035BB: 0D 47                         w2 :=        b.0x1C
080035BD: 21 4E                         w2 =:        b.0x38
080035BF: FD 20 4E 87 0C                by bmove     b.0x38,r.0x1C,$0xC
080035C4: 1A 46 8A                      w move       b.0x18,r.0x28
080035C7: 20 8B                         w1 =:        r.0x2C
080035C9: C3 08 00 3E D7 00             call         $0x8003ED7,$0x0
080035CF: 9D                            ifkret
080035D0: 0D 49                         w2 :=        b.0x24
080035D2: 0E 48                         w3 :=        b.0x20
080035D4: 61 D2                         w2 -         r3
080035D6: 55 01                         w2 +         $0x1
080035D8: 0C D1                         w1 :=        r2
080035DA: 80                            ret
080035DB: B8 CF 00 00 00 44             ents         $0x44
080035E1: 0C 49                         w1 :=        b.0x24
080035E3: 0D 48                         w2 :=        b.0x20
080035E5: 60 D1                         w1 -         r2
080035E7: 54 01                         w1 +         $0x1
080035E9: 54 01                         w1 +         $0x1
080035EB: FC AD D0 3F                   w sha        r1,$0x3F
080035EF: 20 4B                         w1 =:        b.0x2C
080035F1: 0E 48                         w3 :=        b.0x20
080035F3: 7A 04                         w3 /         $0x4
080035F5: 22 4C                         w3 =:        b.0x30
080035F7: 0F 49                         w4 :=        b.0x24
080035F9: 7B 04                         w4 /         $0x4
080035FB: 23 4D                         w4 =:        b.0x34
080035FD: 18 42                         r:=          b.0x8
080035FF: 1A 45 85                      w move       b.0x14,r.0x14
08003602: 1A C4 08 00 6B F0 86          w move       $0x8006BF0,r.0x18
08003609: 1A D3 50                      w move       r4,b.0x40
0800360C: 1A D2 4F                      w move       r3,b.0x3C
0800360F: 0D 47                         w2 :=        b.0x1C
08003611: 21 4E                         w2 =:        b.0x38
08003613: FD 20 4E 87 0C                by bmove     b.0x38,r.0x1C,$0xC
08003618: 1A 46 8A                      w move       b.0x18,r.0x28
0800361B: 20 8B                         w1 =:        r.0x2C
0800361D: C3 08 00 3E FB 00             call         $0x8003EFB,$0x0
08003623: 9D                            ifkret
08003624: 0D 49                         w2 :=        b.0x24
08003626: 0E 48                         w3 :=        b.0x20
08003628: 61 D2                         w2 -         r3
0800362A: 55 01                         w2 +         $0x1
0800362C: 0C D1                         w1 :=        r2
0800362E: 80                            ret
0800362F: B8 CF 00 00 00 54             ents         $0x54
08003635: 0C 47                         w1 :=        b.0x1C
08003637: 20 51                         w1 =:        b.0x44
08003639: 0D 4A                         w2 :=        b.0x28
0800363B: 21 53                         w2 =:        b.0x4C
0800363D: 54 01                         w1 +         $0x1
0800363F: 20 4F                         w1 =:        b.0x3C
08003641: 0E 00                         w3 :=        $0x0
08003643: 22 4E                         w3 =:        b.0x38
08003645: 22 52                         w3 =:        b.0x48
08003647: 18 42                         r:=          b.0x8
08003649: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
0800364E: 20 88                         w1 =:        r.0x20
08003650: C3 08 00 3B F6 00             call         $0x8003BF6,$0x0
08003656: 9D                            ifkret
08003657: 18 42                         r:=          b.0x8
08003659: 1A 88 4F                      w move       r.0x20,b.0x3C
0800365C: 20 50                         w1 =:        b.0x40
0800365E: 0E 51                         w3 :=        b.0x44
08003660: 05 E6 18                      by2 :=       @b.0x18+
08003663: FC 91 CD DF                   by2 and      $0xDF
08003667: 31 CD 41                      by2 comp     $0x41
0800366A: C4 0A                         if = go      $0xA
0800366C: 0D 4B                         w2 :=        b.0x2C
0800366E: 61 53                         w2 -         b.0x4C
08003670: 55 01                         w2 +         $0x1
08003672: 21 50                         w2 =:        b.0x40
08003674: 44 50                         w test       b.0x40
08003676: C6 17                         if >< go     $0x17
08003678: 0C 4B                         w1 :=        b.0x2C
0800367A: 60 53                         w1 -         b.0x4C
0800367C: 54 01                         w1 +         $0x1
0800367E: 20 50                         w1 =:        b.0x40
08003680: 34 CE 00 87                   w1 comp      $0x87
08003684: CE 07                         if <= go     $0x7
08003686: 1A CE 00 88 50                w move       $0x88,b.0x40
0800368B: 4D 52                         w set1       b.0x48
0800368D: 4A 51                         w stz        b.0x44
0800368F: 4A 4F                         w stz        b.0x3C
08003691: 0C 50                         w1 :=        b.0x40
08003693: 60 01                         w1 -         $0x1
08003695: 20 54                         w1 =:        b.0x50
08003697: 2E 4F D0                      w comp2      b.0x3C,r1
0800369A: C8 65                         if > go      $0x65
0800369C: C3 08 00 3C 46 02 45 4D       call         $0x8003C46,$0x2,b.0x14,b.0x34
080036A4: 9D                            ifkret
080036A5: 4F 4E                         w incr       b.0x38
080036A7: 2E 4E 01                      w comp2      b.0x38,$0x1
080036AA: C6 0B                         if >< go     $0xB
080036AC: 2E 4D 0A                      w comp2      b.0x34,$0xA
080036AF: C6 06                         if >< go     $0x6
080036B1: 4D 51                         w set1       b.0x44
080036B3: C0 E9                         go           $0xFFFFFFFFFFFFFFE9
080036B5: 2E 4E 01                      w comp2      b.0x38,$0x1
080036B8: C6 0F                         if >< go     $0xF
080036BA: 44 52                         w test       b.0x48
080036BC: C6 0B                         if >< go     $0xB
080036BE: 2E 4D 0D                      w comp2      b.0x34,$0xD
080036C1: C6 06                         if >< go     $0x6
080036C3: 51 4E                         w decr       b.0x38
080036C5: C0 D7                         go           $0xFFFFFFFFFFFFFFD7
080036C7: 2E 4D 0D                      w comp2      b.0x34,$0xD
080036CA: C4 11                         if = go      $0x11
080036CC: 0D 4D                         w2 :=        b.0x34
080036CE: 0E 53                         w3 :=        b.0x4C
080036D0: 1D E6 24                      by2 =:       @b.0x24+
080036D3: 4F 53                         w incr       b.0x4C
080036D5: BF 4F 54 C7                   d loopi      b.0x3C,b.0x50,$0xFFFFFFFFFFFFFFC7
080036D9: C0 26                         go           $0x26
080036DB: 2E 4E 50                      w comp2      b.0x38,b.0x40
080036DE: CC 1B                         if >= go     $0x1B
080036E0: 44 52                         w test       b.0x48
080036E2: C6 17                         if >< go     $0x17
080036E4: 4F 4F                         w incr       b.0x3C
080036E6: 2E 4F 50                      w comp2      b.0x3C,b.0x40
080036E9: C8 10                         if > go      $0x10
080036EB: 04 CD 20                      by1 :=       $0x20
080036EE: 0D 53                         w2 :=        b.0x4C
080036F0: 1C E5 24                      by1 =:       @b.0x24+
080036F3: 4F 53                         w incr       b.0x4C
080036F5: BF 4F 50 F6                   d loopi      b.0x3C,b.0x40,$0xFFFFFFFFFFFFFFF6
080036F9: 44 4E                         w test       b.0x38
080036FB: CE 04                         if <= go     $0x4
080036FD: 51 4E                         w decr       b.0x38
080036FF: 44 51                         w test       b.0x44
08003701: C4 04                         if = go      $0x4
08003703: 51 4E                         w decr       b.0x38
08003705: 0C 4E                         w1 :=        b.0x38
08003707: 80                            ret
08003708: B8 CF 00 00 00 7C             ents         $0x7C
0800370E: 0C 4B                         w1 :=        b.0x2C
08003710: 0D 4A                         w2 :=        b.0x28
08003712: 21 4E                         w2 =:        b.0x38
08003714: 60 D1                         w1 -         r2
08003716: 54 01                         w1 +         $0x1
08003718: 20 53                         w1 =:        b.0x4C
0800371A: 20 51                         w1 =:        b.0x44
0800371C: 0E 48                         w3 :=        b.0x20
0800371E: 22 4F                         w3 =:        b.0x3C
08003720: 0F 47                         w4 :=        b.0x1C
08003722: 23 50                         w4 =:        b.0x40
08003724: 62 D3                         w3 -         r4
08003726: 22 4D                         w3 =:        b.0x34
08003728: 57 01                         w4 +         $0x1
0800372A: 23 56                         w4 =:        b.0x58
0800372C: 4A 54                         w stz        b.0x50
0800372E: 4D 58                         w set1       b.0x60
08003730: 1A CD 20 52                   w move       $0x20,b.0x48
08003734: 4A 5A                         w stz        b.0x68
08003736: 2E 4F 3F                      w comp2      b.0x3C,$0x3F
08003739: C6 04                         if >< go     $0x4
0800373B: 4D 5A                         w set1       b.0x68
0800373D: 0C 50                         w1 :=        b.0x40
0800373F: 05 E4 18                      by2 :=       @b.0x18+
08003742: FC 91 CD DF                   by2 and      $0xDF
08003746: 4A 5B                         w stz        b.0x6C
08003748: 31 CD 41                      by2 comp     $0x41
0800374B: C4 04                         if = go      $0x4
0800374D: 4D 5B                         w set1       b.0x6C
0800374F: 0D 5A                         w2 :=        b.0x68
08003751: A1 5B                         w2 or        b.0x6C
08003753: 4A 5C                         w stz        b.0x70
08003755: 44 D2                         w test       r3
08003757: C6 04                         if >< go     $0x4
08003759: 4D 5C                         w set1       b.0x70
0800375B: A1 5C                         w2 or        b.0x70
0800375D: 4A 5D                         w stz        b.0x74
0800375F: 36 01                         w3 comp      $0x1
08003761: C6 04                         if >< go     $0x4
08003763: 4D 5D                         w set1       b.0x74
08003765: 06 E7 18                      by3 :=       @b.0x18+
08003768: FC 92 CD DF                   by3 and      $0xDF
0800376C: 4A 5E                         w stz        b.0x78
0800376E: 32 CD 4C                      by3 comp     $0x4C
08003771: C6 04                         if >< go     $0x4
08003773: 4D 5E                         w set1       b.0x78
08003775: 0E 5D                         w3 :=        b.0x74
08003777: E6 5E                         w3 and       b.0x78
08003779: A2 D1                         w3 or        r2
0800377B: 22 59                         w3 =:        b.0x64
0800377D: 44 D2                         w test       r3
0800377F: C6 4F                         if >< go     $0x4F
08003781: 04 E7 18                      by1 :=       @b.0x18+
08003784: FC 90 CD DF                   by1 and      $0xDF
08003788: 30 CD 4C                      by1 comp     $0x4C
0800378B: C6 06                         if >< go     $0x6
0800378D: 4A 58                         w stz        b.0x60
0800378F: 4F 56                         w incr       b.0x58
08003791: 18 42                         r:=          b.0x8
08003793: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
08003798: 1A 56 88                      w move       b.0x58,r.0x20
0800379B: C3 08 00 3B F6 00             call         $0x8003BF6,$0x0
080037A1: 9D                            ifkret
080037A2: 18 42                         r:=          b.0x8
080037A4: 1A 88 56                      w move       r.0x20,b.0x58
080037A7: 20 51                         w1 =:        b.0x44
080037A9: 44 58                         w test       b.0x60
080037AB: C4 23                         if = go      $0x23
080037AD: 34 53                         w1 comp      b.0x4C
080037AF: CE 1F                         if <= go     $0x1F
080037B1: 20 85                         w1 =:        r.0x14
080037B3: 1A 52 86                      w move       b.0x48,r.0x18
080037B6: 1A 45 87                      w move       b.0x14,r.0x1C
080037B9: 0C 53                         w1 :=        b.0x4C
080037BB: C3 08 00 3B 54 00             call         $0x8003B54,$0x0
080037C1: 9D                            ifkret
080037C2: 54 54                         w1 +         b.0x50
080037C4: 20 54                         w1 =:        b.0x50
080037C6: 1A 53 51                      w move       b.0x4C,b.0x44
080037C9: 44 D0                         w test       r1
080037CB: CC 03                         if >= go     $0x3
080037CD: 80                            ret
080037CE: 44 58                         w test       b.0x60
080037D0: C6 0C                         if >< go     $0xC
080037D2: 2E 51 53                      w comp2      b.0x44,b.0x4C
080037D5: CE 07                         if <= go     $0x7
080037D7: 1A 53 57                      w move       b.0x4C,b.0x5C
080037DA: C0 05                         go           $0x5
080037DC: 1A 51 57                      w move       b.0x44,b.0x5C
080037DF: 4A 56                         w stz        b.0x58
080037E1: 2E 56 57                      w comp2      b.0x58,b.0x5C
080037E4: CC 55                         if >= go     $0x55
080037E6: 04 CD 24                      by1 :=       $0x24
080037E9: 0D 4E                         w2 :=        b.0x38
080037EB: 2D E5 24 D0                   by comp2     @b.0x24+,r1
080037EF: C6 34                         if >< go     $0x34
080037F1: 55 01                         w2 +         $0x1
080037F3: 0E 4B                         w3 :=        b.0x2C
080037F5: 35 D2                         w2 comp      r3
080037F7: C8 19                         if > go      $0x19
080037F9: 0D 4E                         w2 :=        b.0x38
080037FB: 55 01                         w2 +         $0x1
080037FD: 07 CD 24                      by4 :=       $0x24
08003800: 2D E5 24 D3                   by comp2     @b.0x24+,r4
08003804: C6 0C                         if >< go     $0xC
08003806: 4F 4E                         w incr       b.0x38
08003808: 1A CD 24 55                   w move       $0x24,b.0x54
0800380C: 4F 56                         w incr       b.0x58
0800380E: C0 13                         go           $0x13
08003810: 1A 0D 55                      w move       $0xD,b.0x54
08003813: C3 08 00 3C 2E 02 45 55       call         $0x8003C2E,$0x2,b.0x14,b.0x54
0800381B: 9D                            ifkret
0800381C: 4F 54                         w incr       b.0x50
0800381E: 1A 0A 55                      w move       $0xA,b.0x54
08003821: C0 07                         go           $0x7
08003823: 06 E5 24                      by3 :=       @b.0x24+
08003826: 22 55                         w3 =:        b.0x54
08003828: C3 08 00 3C 2E 02 45 55       call         $0x8003C2E,$0x2,b.0x14,b.0x54
08003830: 9D                            ifkret
08003831: 4F 54                         w incr       b.0x50
08003833: 4F 4E                         w incr       b.0x38
08003835: 4F 56                         w incr       b.0x58
08003837: C0 AA                         go           $0xFFFFFFFFFFFFFFAA
08003839: 44 58                         w test       b.0x60
0800383B: C6 1F                         if >< go     $0x1F
0800383D: 2E 53 51                      w comp2      b.0x4C,b.0x44
08003840: C4 1A                         if = go      $0x1A
08003842: 18 42                         r:=          b.0x8
08003844: 1A 51 85                      w move       b.0x44,r.0x14
08003847: 1A 52 86                      w move       b.0x48,r.0x18
0800384A: 1A 45 87                      w move       b.0x14,r.0x1C
0800384D: 0C 53                         w1 :=        b.0x4C
0800384F: C3 08 00 3B 54 00             call         $0x8003B54,$0x0
08003855: 9D                            ifkret
08003856: 54 54                         w1 +         b.0x50
08003858: 20 54                         w1 =:        b.0x50
0800385A: 0C 54                         w1 :=        b.0x50
0800385C: 80                            ret
0800385D: B8 CF 00 00 00 5C             ents         $0x5C
08003863: 0C 49                         w1 :=        b.0x24
08003865: 20 54                         w1 =:        b.0x50
08003867: 20 55                         w1 =:        b.0x54
08003869: 0D 00                         w2 :=        $0x0
0800386B: 21 4E                         w2 =:        b.0x38
0800386D: 21 51                         w2 =:        b.0x44
0800386F: 0E 47                         w3 :=        b.0x1C
08003871: 22 4B                         w3 =:        b.0x2C
08003873: 56 01                         w3 +         $0x1
08003875: 22 4C                         w3 =:        b.0x30
08003877: 18 42                         r:=          b.0x8
08003879: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
0800387E: 22 88                         w3 =:        r.0x20
08003880: C3 08 00 3B F6 00             call         $0x8003BF6,$0x0
08003886: 9D                            ifkret
08003887: 18 42                         r:=          b.0x8
08003889: 1A 88 4C                      w move       r.0x20,b.0x30
0800388C: 20 4D                         w1 =:        b.0x34
0800388E: 0E 4B                         w3 :=        b.0x2C
08003890: 05 E6 18                      by2 :=       @b.0x18+
08003893: FC 91 CD DF                   by2 and      $0xDF
08003897: 31 CD 42                      by2 comp     $0x42
0800389A: C6 05                         if >< go     $0x5
0800389C: C1 02 40                      go           $0x240
0800389F: FD D1 49 1F                   w2 getbi     b.0x24,$0x1F
080038A3: C4 54                         if = go      $0x54
080038A5: 0F 49                         w4 :=        b.0x24
080038A7: E7 CF 7F FF FF FF             w4 and       $0x7FFFFFFF
080038AD: 44 D3                         w test       r4
080038AF: C6 48                         if >< go     $0x48
080038B1: 1A 0B 51                      w move       $0xB,b.0x44
080038B4: 07 E6 18                      by4 :=       @b.0x18+
080038B7: FC 93 CD DF                   by4 and      $0xDF
080038BB: 33 CD 42                      by4 comp     $0x42
080038BE: C6 1D                         if >< go     $0x1D
080038C0: 4A 4C                         w stz        b.0x30
080038C2: 0D 4C                         w2 :=        b.0x30
080038C4: 04 E1 08 00 6C 27             by1 :=       $0x8006C27+
080038CA: 20 56                         w1 =:        b.0x58
080038CC: C3 08 00 3C 2E 02 45 56       call         $0x8003C2E,$0x2,b.0x14,b.0x58
080038D4: 9D                            ifkret
080038D5: BF 4C 0A ED                   d loopi      b.0x30,$0xA,$0xFFFFFFFFFFFFFFED
080038D9: C0 1B                         go           $0x1B
080038DB: 4A 4C                         w stz        b.0x30
080038DD: 0D 4C                         w2 :=        b.0x30
080038DF: 04 E1 08 00 6C 1C             by1 :=       $0x8006C1C+
080038E5: 20 56                         w1 =:        b.0x58
080038E7: C3 08 00 3C 2E 02 45 56       call         $0x8003C2E,$0x2,b.0x14,b.0x58
080038EF: 9D                            ifkret
080038F0: BF 4C 0A ED                   d loopi      b.0x30,$0xA,$0xFFFFFFFFFFFFFFED
080038F4: C1 02 5D                      go           $0x25D
080038F7: 1A 54 53                      w move       b.0x50,b.0x4C
080038FA: 1A CD 20 4F                   w move       $0x20,b.0x3C
080038FE: 05 E6 18                      by2 :=       @b.0x18+
08003901: FC 91 CD DF                   by2 and      $0xDF
08003905: 31 CD 4F                      by2 comp     $0x4F
08003908: C4 0D                         if = go      $0xD
0800390A: 05 E6 18                      by2 :=       @b.0x18+
0800390D: FC 91 3F                      by2 and      $0x3F
08003910: 31 CD 5A                      by2 comp     $0x5A
08003913: C6 05                         if >< go     $0x5
08003915: C1 00 F4                      go           $0xF4
08003918: FD D1 54 1F                   w2 getbi     b.0x50,$0x1F
0800391C: C4 0F                         if = go      $0xF
0800391E: 0F 54                         w4 :=        b.0x50
08003920: 93                            w4 neg
08003921: 23 54                         w4 =:        b.0x50
08003923: 23 55                         w4 =:        b.0x54
08003925: 23 53                         w4 =:        b.0x4C
08003927: 1A CD 2D 4F                   w move       $0x2D,b.0x3C
0800392B: 0F 48                         w4 :=        b.0x20
0800392D: 37 3F                         w4 comp      $0x3F
0800392F: C4 0E                         if = go      $0xE
08003931: 07 E6 18                      by4 :=       @b.0x18+
08003934: FC 93 CD DF                   by4 and      $0xDF
08003938: 33 CD 49                      by4 comp     $0x49
0800393B: C4 04                         if = go      $0x4
0800393D: 4A 4D                         w stz        b.0x34
0800393F: 4A 4C                         w stz        b.0x30
08003941: 0C 53                         w1 :=        b.0x4C
08003943: 0D 4C                         w2 :=        b.0x30
08003945: 78 E1 08 00 6B F4             w1 /         $0x8006BF4+
0800394B: 20 50                         w1 =:        b.0x40
0800394D: 44 D0                         w test       r1
0800394F: C8 07                         if > go      $0x7
08003951: 2E 53 55                      w comp2      b.0x4C,b.0x54
08003954: C4 11                         if = go      $0x11
08003956: 4F 4E                         w incr       b.0x38
08003958: 0E E1 08 00 6B F4             w3 :=        $0x8006BF4+
0800395E: FC 7F 53 D2 D0                w4 div4      b.0x4C,r3,r1
08003963: 23 53                         w4 =:        b.0x4C
08003965: BF 4C 09 DC                   d loopi      b.0x30,$0x9,$0xFFFFFFFFFFFFFFDC
08003969: 44 4D                         w test       b.0x34
0800396B: C6 1E                         if >< go     $0x1E
0800396D: 44 4E                         w test       b.0x38
0800396F: C6 07                         if >< go     $0x7
08003971: 4D 4E                         w set1       b.0x38
08003973: 1A 3F 4B                      w move       $0x3F,b.0x2C
08003976: 2E 4F CD 2D                   w comp2      b.0x3C,$0x2D
0800397A: C6 0A                         if >< go     $0xA
0800397C: 0C 4E                         w1 :=        b.0x38
0800397E: 54 01                         w1 +         $0x1
08003980: 20 4D                         w1 =:        b.0x34
08003982: C0 07                         go           $0x7
08003984: 1A 4E 4D                      w move       b.0x38,b.0x34
08003987: C0 2A                         go           $0x2A
08003989: 44 4E                         w test       b.0x38
0800398B: C6 07                         if >< go     $0x7
0800398D: 4D 4E                         w set1       b.0x38
0800398F: 1A 3F 4B                      w move       $0x3F,b.0x2C
08003992: 18 42                         r:=          b.0x8
08003994: 1A 4D 85                      w move       b.0x34,r.0x14
08003997: 1A 4F 86                      w move       b.0x3C,r.0x18
0800399A: 1A 45 87                      w move       b.0x14,r.0x1C
0800399D: 0C 4E                         w1 :=        b.0x38
0800399F: C3 08 00 3B 54 00             call         $0x8003B54,$0x0
080039A5: 9D                            ifkret
080039A6: 54 51                         w1 +         b.0x44
080039A8: 20 51                         w1 =:        b.0x44
080039AA: 44 D0                         w test       r1
080039AC: CC 05                         if >= go     $0x5
080039AE: C1 01 A3                      go           $0x1A3
080039B1: 4A 4C                         w stz        b.0x30
080039B3: 0C 54                         w1 :=        b.0x50
080039B5: 0D 4C                         w2 :=        b.0x30
080039B7: 78 E1 08 00 6B F4             w1 /         $0x8006BF4+
080039BD: 20 50                         w1 =:        b.0x40
080039BF: 44 D0                         w test       r1
080039C1: C8 07                         if > go      $0x7
080039C3: 2E 54 55                      w comp2      b.0x50,b.0x54
080039C6: C4 28                         if = go      $0x28
080039C8: 54 CD 30                      w1 +         $0x30
080039CB: 20 52                         w1 =:        b.0x48
080039CD: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
080039D5: 9D                            ifkret
080039D6: 4F 51                         w incr       b.0x44
080039D8: 0D 54                         w2 :=        b.0x50
080039DA: 0E 4C                         w3 :=        b.0x30
080039DC: 79 E2 08 00 6B F4             w2 /         $0x8006BF4+
080039E2: 6D E2 08 00 6B F4             w2 *         $0x8006BF4+
080039E8: 0F 54                         w4 :=        b.0x50
080039EA: 63 D1                         w4 -         r2
080039EC: 23 54                         w4 =:        b.0x50
080039EE: BF 4C 09 C5                   d loopi      b.0x30,$0x9,$0xFFFFFFFFFFFFFFC5
080039F2: 2E 4B 3F                      w comp2      b.0x2C,$0x3F
080039F5: C6 11                         if >< go     $0x11
080039F7: 1A CD 30 52                   w move       $0x30,b.0x48
080039FB: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003A03: 9D                            ifkret
08003A04: 4F 51                         w incr       b.0x44
08003A06: C1 01 4B                      go           $0x14B
08003A09: 4A 4C                         w stz        b.0x30
08003A0B: 0C 1F                         w1 :=        $0x1F
08003A0D: 60 4C                         w1 -         b.0x30
08003A0F: 20 4E                         w1 =:        b.0x38
08003A11: FD D1 53 1F                   w2 getbi     b.0x4C,$0x1F
08003A15: C6 0E                         if >< go     $0xE
08003A17: 0E 53                         w3 :=        b.0x4C
08003A19: FC AD D2 01                   w sha        r3,$0x1
08003A1D: 22 53                         w3 =:        b.0x4C
08003A1F: BF 4C 1F EC                   d loopi      b.0x30,$0x1F,$0xFFFFFFFFFFFFFFEC
08003A23: 0C 4E                         w1 :=        b.0x38
08003A25: 78 03                         w1 /         $0x3
08003A27: 54 01                         w1 +         $0x1
08003A29: 20 4E                         w1 =:        b.0x38
08003A2B: 44 4D                         w test       b.0x34
08003A2D: C6 04                         if >< go     $0x4
08003A2F: 20 4D                         w1 =:        b.0x34
08003A31: 44 4E                         w test       b.0x38
08003A33: C6 04                         if >< go     $0x4
08003A35: 4D 4E                         w set1       b.0x38
08003A37: 04 CD 5A                      by1 :=       $0x5A
08003A3A: 0D 4B                         w2 :=        b.0x2C
08003A3C: 2D E5 18 D0                   by comp2     @b.0x18+,r1
08003A40: C6 2A                         if >< go     $0x2A
08003A42: 0E 4D                         w3 :=        b.0x34
08003A44: 62 4E                         w3 -         b.0x38
08003A46: 22 50                         w3 =:        b.0x40
08003A48: 44 D2                         w test       r3
08003A4A: CC 07                         if >= go     $0x7
08003A4C: 1A 4D 4E                      w move       b.0x34,b.0x38
08003A4F: 4A 50                         w stz        b.0x40
08003A51: 44 50                         w test       b.0x40
08003A53: C4 15                         if = go      $0x15
08003A55: 1A CD 30 52                   w move       $0x30,b.0x48
08003A59: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003A61: 9D                            ifkret
08003A62: 4F 51                         w incr       b.0x44
08003A64: 51 50                         w decr       b.0x40
08003A66: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
08003A68: C0 21                         go           $0x21
08003A6A: 18 42                         r:=          b.0x8
08003A6C: 1A 4D 85                      w move       b.0x34,r.0x14
08003A6F: 1A 4F 86                      w move       b.0x3C,r.0x18
08003A72: 1A 45 87                      w move       b.0x14,r.0x1C
08003A75: 0C 4E                         w1 :=        b.0x38
08003A77: C3 08 00 3B 54 00             call         $0x8003B54,$0x0
08003A7D: 9D                            ifkret
08003A7E: 54 51                         w1 +         b.0x44
08003A80: 20 51                         w1 =:        b.0x44
08003A82: 44 D0                         w test       r1
08003A84: CC 05                         if >= go     $0x5
08003A86: C1 00 CB                      go           $0xCB
08003A89: 2E 4E 0B                      w comp2      b.0x38,$0xB
08003A8C: C6 1D                         if >< go     $0x1D
08003A8E: 0C 54                         w1 :=        b.0x50
08003A90: FC AD D0 22                   w sha        r1,$0x22
08003A94: E4 03                         w1 and       $0x3
08003A96: 54 CD 30                      w1 +         $0x30
08003A99: 20 52                         w1 =:        b.0x48
08003A9B: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003AA3: 9D                            ifkret
08003AA4: 4F 51                         w incr       b.0x44
08003AA6: 1A 0A 4E                      w move       $0xA,b.0x38
08003AA9: 0D 4E                         w2 :=        b.0x38
08003AAB: 61 01                         w2 -         $0x1
08003AAD: 6D 03                         w2 *         $0x3
08003AAF: 21 50                         w2 =:        b.0x40
08003AB1: 4D 4C                         w set1       b.0x30
08003AB3: 2E 4C 4E                      w comp2      b.0x30,b.0x38
08003AB6: C8 24                         if > go      $0x24
08003AB8: 0C 50                         w1 :=        b.0x40
08003ABA: 90                            w1 neg
08003ABB: 0D 54                         w2 :=        b.0x50
08003ABD: FC AD D1 D0                   w sha        r2,r1
08003AC1: E5 07                         w2 and       $0x7
08003AC3: 55 CD 30                      w2 +         $0x30
08003AC6: 21 52                         w2 =:        b.0x48
08003AC8: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003AD0: 9D                            ifkret
08003AD1: 4F 51                         w incr       b.0x44
08003AD3: E0 50 03                      w sub2       b.0x40,$0x3
08003AD6: BF 4C 4E E2                   d loopi      b.0x30,b.0x38,$0xFFFFFFFFFFFFFFE2
08003ADA: C0 77                         go           $0x77
08003ADC: 44 4D                         w test       b.0x34
08003ADE: C6 04                         if >< go     $0x4
08003AE0: 4D 4D                         w set1       b.0x34
08003AE2: 2E 4D 04                      w comp2      b.0x34,$0x4
08003AE5: CE 05                         if <= go     $0x5
08003AE7: 1A 04 4D                      w move       $0x4,b.0x34
08003AEA: 2E 4D 03                      w comp2      b.0x34,$0x3
08003AED: CE 19                         if <= go     $0x19
08003AEF: 0C 54                         w1 :=        b.0x50
08003AF1: FC AD D0 28                   w sha        r1,$0x28
08003AF5: E4 CE 00 FF                   w1 and       $0xFF
08003AF9: 20 52                         w1 =:        b.0x48
08003AFB: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003B03: 9D                            ifkret
08003B04: 4F 51                         w incr       b.0x44
08003B06: 2E 4D 02                      w comp2      b.0x34,$0x2
08003B09: CE 19                         if <= go     $0x19
08003B0B: 0D 54                         w2 :=        b.0x50
08003B0D: FC AD D1 30                   w sha        r2,$0x30
08003B11: E5 CE 00 FF                   w2 and       $0xFF
08003B15: 21 52                         w2 =:        b.0x48
08003B17: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003B1F: 9D                            ifkret
08003B20: 4F 51                         w incr       b.0x44
08003B22: 2E 4D 01                      w comp2      b.0x34,$0x1
08003B25: CE 19                         if <= go     $0x19
08003B27: 0D 54                         w2 :=        b.0x50
08003B29: FC AD D1 38                   w sha        r2,$0x38
08003B2D: E5 CE 00 FF                   w2 and       $0xFF
08003B31: 21 52                         w2 =:        b.0x48
08003B33: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003B3B: 9D                            ifkret
08003B3C: 4F 51                         w incr       b.0x44
08003B3E: 0D 54                         w2 :=        b.0x50
08003B40: E5 CE 00 FF                   w2 and       $0xFF
08003B44: 21 52                         w2 =:        b.0x48
08003B46: C3 08 00 3C 2E 02 45 52       call         $0x8003C2E,$0x2,b.0x14,b.0x48
08003B4E: 9D                            ifkret
08003B4F: 4F 51                         w incr       b.0x44
08003B51: 0C 51                         w1 :=        b.0x44
08003B53: 80                            ret
08003B54: B8 CF 00 00 00 34             ents         $0x34
08003B5A: 20 48                         w1 =:        b.0x20
08003B5C: 20 49                         w1 =:        b.0x24
08003B5E: 4A 4C                         w stz        b.0x30
08003B60: 2E 46 CD 20                   w comp2      b.0x18,$0x20
08003B64: C6 04                         if >< go     $0x4
08003B66: 51 49                         w decr       b.0x24
08003B68: 0C 49                         w1 :=        b.0x24
08003B6A: 54 01                         w1 +         $0x1
08003B6C: 34 45                         w1 comp      b.0x14
08003B6E: CE 17                         if <= go     $0x17
08003B70: 18 42                         r:=          b.0x8
08003B72: 1A 47 85                      w move       b.0x1C,r.0x14
08003B75: 0C 45                         w1 :=        b.0x14
08003B77: C3 08 00 3B BB 00             call         $0x8003BBB,$0x0
08003B7D: 9D                            ifkret
08003B7E: 20 4C                         w1 =:        b.0x30
08003B80: 90                            w1 neg
08003B81: 20 4C                         w1 =:        b.0x30
08003B83: C0 35                         go           $0x35
08003B85: 0C 45                         w1 :=        b.0x14
08003B87: 60 49                         w1 -         b.0x24
08003B89: 60 01                         w1 -         $0x1
08003B8B: 20 4A                         w1 =:        b.0x28
08003B8D: 44 4A                         w test       b.0x28
08003B8F: C4 15                         if = go      $0x15
08003B91: 1A CD 20 4B                   w move       $0x20,b.0x2C
08003B95: C3 08 00 3C 2E 02 47 4B       call         $0x8003C2E,$0x2,b.0x1C,b.0x2C
08003B9D: 9D                            ifkret
08003B9E: 4F 4C                         w incr       b.0x30
08003BA0: 51 4A                         w decr       b.0x28
08003BA2: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
08003BA4: 2E 46 CD 20                   w comp2      b.0x18,$0x20
08003BA8: C4 10                         if = go      $0x10
08003BAA: 1A 46 4B                      w move       b.0x18,b.0x2C
08003BAD: C3 08 00 3C 2E 02 47 4B       call         $0x8003C2E,$0x2,b.0x1C,b.0x2C
08003BB5: 9D                            ifkret
08003BB6: 4F 4C                         w incr       b.0x30
08003BB8: 0C 4C                         w1 :=        b.0x30
08003BBA: 80                            ret
08003BBB: B8 CF 00 00 00 28             ents         $0x28
08003BC1: 20 46                         w1 =:        b.0x18
08003BC3: 60 01                         w1 -         $0x1
08003BC5: 20 47                         w1 =:        b.0x1C
08003BC7: 4A 48                         w stz        b.0x20
08003BC9: 44 D0                         w test       r1
08003BCB: CE 11                         if <= go     $0x11
08003BCD: 1A CD 20 49                   w move       $0x20,b.0x24
08003BD1: C3 08 00 3C 2E 02 45 49       call         $0x8003C2E,$0x2,b.0x14,b.0x24
08003BD9: 9D                            ifkret
08003BDA: 4F 48                         w incr       b.0x20
08003BDC: 44 47                         w test       b.0x1C
08003BDE: C4 15                         if = go      $0x15
08003BE0: 1A CD 2A 49                   w move       $0x2A,b.0x24
08003BE4: C3 08 00 3C 2E 02 45 49       call         $0x8003C2E,$0x2,b.0x14,b.0x24
08003BEC: 9D                            ifkret
08003BED: 4F 48                         w incr       b.0x20
08003BEF: 51 47                         w decr       b.0x1C
08003BF1: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
08003BF3: 0C 48                         w1 :=        b.0x20
08003BF5: 80                            ret
08003BF6: B8 CF 00 00 00 3C             ents         $0x3C
08003BFC: 4A 4A                         w stz        b.0x28
08003BFE: 0C 47                         w1 :=        b.0x1C
08003C00: 20 4E                         w1 =:        b.0x38
08003C02: 2E 48 D0                      w comp2      b.0x20,r1
08003C05: C8 26                         if > go      $0x26
08003C07: 0D 48                         w2 :=        b.0x20
08003C09: 04 E5 14                      by1 :=       @b.0x14+
08003C0C: FC 90 CD 7F                   by1 and      $0x7F
08003C10: 1C 4D                         by1 =:       b.0x34
08003C12: 30 CD 30                      by1 comp     $0x30
08003C15: D8 16                         if << go     $0x16
08003C17: 30 CD 39                      by1 comp     $0x39
08003C1A: D4 11                         if >> go     $0x11
08003C1C: 0E 4A                         w3 :=        b.0x28
08003C1E: 6E 0A                         w3 *         $0xA
08003C20: FC 90 0F                      by1 and      $0xF
08003C23: 54 D2                         w1 +         r3
08003C25: 20 4A                         w1 =:        b.0x28
08003C27: BF 48 4E E0                   d loopi      b.0x20,b.0x38,$0xFFFFFFFFFFFFFFE0
08003C2B: 0C 4A                         w1 :=        b.0x28
08003C2D: 80                            ret
08003C2E: B8 CF 00 00 00 1C             ents         $0x1C
08003C34: 0C C5 14                      w1 :=        @b.0x14
08003C37: 18 42                         r:=          b.0x8
08003C39: 20 85                         w1 =:        r.0x14
08003C3B: 0C C5 18                      w1 :=        @b.0x18
08003C3E: C3 08 00 3D B3 00             call         $0x8003DB3,$0x0
08003C44: 9D                            ifkret
08003C45: 80                            ret
08003C46: B8 CF 00 00 00 1C             ents         $0x1C
08003C4C: 0C C5 14                      w1 :=        @b.0x14
08003C4F: 18 42                         r:=          b.0x8
08003C51: 20 85                         w1 =:        r.0x14
08003C53: C3 08 00 3D 9E 00             call         $0x8003D9E,$0x0
08003C59: 9D                            ifkret
08003C5A: 20 C5 18                      w1 =:        @b.0x18
08003C5D: 0D C5 18                      w2 :=        @b.0x18
08003C60: E5 CD 7F                      w2 and       $0x7F
08003C63: 21 C5 18                      w2 =:        @b.0x18
08003C66: 80                            ret
08003C67: B8 CF 00 00 00 7C             ents         $0x7C
08003C6D: 20 52                         w1 =:        b.0x48
08003C6F: E4 07                         w1 and       $0x7
08003C71: 20 59                         w1 =:        b.0x64
08003C73: 0D 52                         w2 :=        b.0x48
08003C75: FC AD D1 30                   w sha        r2,$0x30
08003C79: E5 CF 00 00 FF FF             w2 and       $0xFFFF
08003C7F: 21 5A                         w2 =:        b.0x68
08003C81: 4D 57                         w set1       b.0x5C
08003C83: 2E 57 D0                      w comp2      b.0x5C,r1
08003C86: C8 2E                         if > go      $0x2E
08003C88: 0C 57                         w1 :=        b.0x5C
08003C8A: 60 01                         w1 -         $0x1
08003C8C: 6C 03                         w1 *         $0x3
08003C8E: 54 02                         w1 +         $0x2
08003C90: 0D 57                         w2 :=        b.0x5C
08003C92: 61 01                         w2 -         $0x1
08003C94: 6D 03                         w2 *         $0x3
08003C96: 55 01                         w2 +         $0x1
08003C98: FD 3F 46                      w4 laddr     b.0x18
08003C9B: 23 5D                         w4 =:        b.0x74
08003C9D: 0E E5 74                      w3 :=        @b.0x74+
08003CA0: 20 5D                         w1 =:        b.0x74
08003CA2: FD 3C 46                      w1 laddr     b.0x18
08003CA5: 20 5E                         w1 =:        b.0x78
08003CA7: 0F 5D                         w4 :=        b.0x74
08003CA9: 2E E7 78 D2                   w comp2      @b.0x78+,r3
08003CAD: CC 03                         if >= go     $0x3
08003CAF: 80                            ret
08003CB0: BF 57 59 D8                   d loopi      b.0x5C,b.0x64,$0xFFFFFFFFFFFFFFD8
08003CB4: 4D 57                         w set1       b.0x5C
08003CB6: 2E 57 59                      w comp2      b.0x5C,b.0x64
08003CB9: C8 1B                         if > go      $0x1B
08003CBB: 0C 57                         w1 :=        b.0x5C
08003CBD: 60 01                         w1 -         $0x1
08003CBF: 6C 03                         w1 *         $0x3
08003CC1: 54 01                         w1 +         $0x1
08003CC3: FD 3E 46                      w3 laddr     b.0x18
08003CC6: 22 5D                         w3 =:        b.0x74
08003CC8: 0D E4 74                      w2 :=        @b.0x74+
08003CCB: 0F 57                         w4 :=        b.0x5C
08003CCD: 21 D7 48                      w2 =:        b.0x48+
08003CD0: BF 57 59 EB                   d loopi      b.0x5C,b.0x64,$0xFFFFFFFFFFFFFFEB
08003CD4: 0C 01                         w1 :=        $0x1
08003CD6: 1A D4 48 58                   w move       b.0x48+,b.0x60
08003CDA: FD 4B 02 57                   by wconv     $0x2,b.0x5C
08003CDE: 2E 57 59                      w comp2      b.0x5C,b.0x64
08003CE1: C8 1D                         if > go      $0x1D
08003CE3: 0C 57                         w1 :=        b.0x5C
08003CE5: 60 01                         w1 -         $0x1
08003CE7: 6C 03                         w1 *         $0x3
08003CE9: 0D 58                         w2 :=        b.0x60
08003CEB: FD 3E 46                      w3 laddr     b.0x18
08003CEE: 22 5D                         w3 =:        b.0x74
08003CF0: 6D E4 74                      w2 *         @b.0x74+
08003CF3: 0F 57                         w4 :=        b.0x5C
08003CF5: 55 D7 48                      w2 +         b.0x48+
08003CF8: 21 58                         w2 =:        b.0x60
08003CFA: BF 57 59 E9                   d loopi      b.0x5C,b.0x64,$0xFFFFFFFFFFFFFFE9
08003CFE: 44 5A                         w test       b.0x68
08003D00: C6 0D                         if >< go     $0xD
08003D02: 0C C5 14                      w1 :=        @b.0x14
08003D05: 0D 58                         w2 :=        b.0x60
08003D07: FC 0C E5 18                   bi1 =:       @b.0x18+
08003D0B: C0 39                         go           $0x39
08003D0D: 2E 5A 01                      w comp2      b.0x68,$0x1
08003D10: C6 0C                         if >< go     $0xC
08003D12: 05 C5 14                      by2 :=       @b.0x14
08003D15: 0E 58                         w3 :=        b.0x60
08003D17: 1D E6 18                      by2 =:       @b.0x18+
08003D1A: C0 2A                         go           $0x2A
08003D1C: FC 5F 58 5A                   w mul2       b.0x60,b.0x68
08003D20: 4A 57                         w stz        b.0x5C
08003D22: 0E 5A                         w3 :=        b.0x68
08003D24: 62 01                         w3 -         $0x1
08003D26: 22 5D                         w3 =:        b.0x74
08003D28: 2E 57 D2                      w comp2      b.0x5C,r3
08003D2B: C8 19                         if > go      $0x19
08003D2D: 0C 58                         w1 :=        b.0x60
08003D2F: 54 57                         w1 +         b.0x5C
08003D31: 18 45                         r:=          b.0x14
08003D33: FD 3E 80                      w3 laddr     r.0x0
08003D36: 22 5E                         w3 =:        b.0x78
08003D38: 0F 57                         w4 :=        b.0x5C
08003D3A: 05 E7 78                      by2 :=       @b.0x78+
08003D3D: 1D E4 18                      by2 =:       @b.0x18+
08003D40: BF 57 5D ED                   d loopi      b.0x5C,b.0x74,$0xFFFFFFFFFFFFFFED
08003D44: 1A 59 57                      w move       b.0x64,b.0x5C
08003D47: 44 57                         w test       b.0x5C
08003D49: CE 44                         if <= go     $0x44
08003D4B: 0C 57                         w1 :=        b.0x5C
08003D4D: 60 01                         w1 -         $0x1
08003D4F: 6C 03                         w1 *         $0x3
08003D51: 54 02                         w1 +         $0x2
08003D53: FD 3E 46                      w3 laddr     b.0x18
08003D56: 22 5E                         w3 =:        b.0x78
08003D58: 0D E4 78                      w2 :=        @b.0x78+
08003D5B: 0F 57                         w4 :=        b.0x5C
08003D5D: 2E D7 48 D1                   w comp2      b.0x48+,r2
08003D61: C4 0E                         if = go      $0xE
08003D63: 0E D7 48                      w3 :=        b.0x48+
08003D66: 56 01                         w3 +         $0x1
08003D68: 22 D7 48                      w3 =:        b.0x48+
08003D6B: 4A 57                         w stz        b.0x5C
08003D6D: C0 1E                         go           $0x1E
08003D6F: 37 01                         w4 comp      $0x1
08003D71: CE 19                         if <= go     $0x19
08003D73: 63 01                         w4 -         $0x1
08003D75: 6F 03                         w4 *         $0x3
08003D77: 57 01                         w4 +         $0x1
08003D79: FD 3D 46                      w2 laddr     b.0x18
08003D7C: 21 5E                         w2 =:        b.0x78
08003D7E: 0C E7 78                      w1 :=        @b.0x78+
08003D81: 0E 57                         w3 :=        b.0x5C
08003D83: 20 D6 48                      w1 =:        b.0x48+
08003D86: 51 57                         w decr       b.0x5C
08003D88: C0 03                         go           $0x3
08003D8A: 80                            ret
08003D8B: C0 BC                         go           $0xFFFFFFFFFFFFFFBC
08003D8D: C1 FF 47                      go           $0xFFFFFFFFFFFFFF47
08003D90: 80                            ret
08003D91: B8 CF 00 00 00 14             ents         $0x14
08003D97: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
08003D9D: 80                            ret
08003D9E: B8 CF 00 00 00 20             ents         $0x20
08003DA4: C3 F8 00 00 01 02 45 47       call         $0xFFFFFFFFF8000001,$0x2,b.0x14,b.0x1C ; MON 1B INBT
08003DAC: 9D                            ifkret
08003DAD: 0C 47                         w1 :=        b.0x1C
08003DAF: 80                            ret
08003DB0: 04 47                         by1 :=       b.0x1C
08003DB2: 80                            ret
08003DB3: B8 CF 00 00 00 20             ents         $0x20
08003DB9: 20 46                         w1 =:        b.0x18
08003DBB: 20 47                         w1 =:        b.0x1C
08003DBD: C3 F8 00 00 02 02 45 47       call         $0xFFFFFFFFF8000002,$0x2,b.0x14,b.0x1C ; MON 2B OUTBT
08003DC5: 9D                            ifkret
08003DC6: 80                            ret
08003DC7: B8 CF 00 00 00 28             ents         $0x28
08003DCD: 0C 47                         w1 :=        b.0x1C
08003DCF: FD 3D E4 18                   w2 laddr     @b.0x18+
08003DD3: 21 49                         w2 =:        b.0x24
08003DD5: C3 F8 00 00 21 02 45 C5 24    call         $0xFFFFFFFFF8000021,$0x2,b.0x14,@b.0x24 ; MON 41B ROBJE
08003DDE: 9D                            ifkret
08003DDF: 80                            ret
08003DE0: B8 CF 00 00 00 18             ents         $0x18
08003DE6: C3 F8 00 00 23 01 45          call         $0xFFFFFFFFF8000023,$0x1,b.0x14 ; MON 43B CLOSE
08003DED: 9D                            ifkret
08003DEE: 80                            ret
08003DEF: B8 CF 00 00 00 F8             ents         $0xF8
08003DF5: 4A 7A                         w stz        b.0xE8
08003DF7: 0C 46                         w1 :=        b.0x18
08003DF9: 20 79                         w1 =:        b.0xE4
08003DFB: 0D 47                         w2 :=        b.0x1C
08003DFD: 21 7C                         w2 =:        b.0xF0
08003DFF: 34 D1                         w1 comp      r2
08003E01: C8 12                         if > go      $0x12
08003E03: 0D 79                         w2 :=        b.0xE4
08003E05: 04 E5 14                      by1 :=       @b.0x14+
08003E08: 0E 7A                         w3 :=        b.0xE8
08003E0A: 1C D6 44                      by1 =:       b.0x44+
08003E0D: 4F 7A                         w incr       b.0xE8
08003E0F: BF 79 7C F4                   d loopi      b.0xE4,b.0xF0,$0xFFFFFFFFFFFFFFF4
08003E13: 0C 7A                         w1 :=        b.0xE8
08003E15: 19 CD 27 D4 44                by move      $0x27,b.0x44+
08003E1A: 54 01                         w1 +         $0x1
08003E1C: 20 4D                         w1 =:        b.0x34
08003E1E: 4A 7A                         w stz        b.0xE8
08003E20: 0D 49                         w2 :=        b.0x24
08003E22: 21 79                         w2 =:        b.0xE4
08003E24: 0E 4A                         w3 :=        b.0x28
08003E26: 22 7D                         w3 =:        b.0xF4
08003E28: 35 D2                         w2 comp      r3
08003E2A: C8 12                         if > go      $0x12
08003E2C: 0D 79                         w2 :=        b.0xE4
08003E2E: 04 E5 20                      by1 :=       @b.0x20+
08003E31: 0E 7A                         w3 :=        b.0xE8
08003E33: 1C D6 DB                      by1 =:       b.0xFFFFFFFFFFFFFFDB+
08003E36: 4F 7A                         w incr       b.0xE8
08003E38: BF 79 7D F4                   d loopi      b.0xE4,b.0xF4,$0xFFFFFFFFFFFFFFF4
08003E3C: 0C 7A                         w1 :=        b.0xE8
08003E3E: 19 CD 27 D4 DB                by move      $0x27,b.0xFFFFFFFFFFFFFFDB+
08003E43: 54 01                         w1 +         $0x1
08003E45: 20 4F                         w1 =:        b.0x3C
08003E47: 0D 00                         w2 :=        $0x0
08003E49: FE 25 D5 44                   by2 laddr    b.0x44+
08003E4D: 21 4E                         w2 =:        b.0x38
08003E4F: 0E 00                         w3 :=        $0x0
08003E51: FE 26 D6 DB                   by3 laddr    b.0xFFFFFFFFFFFFFFDB+
08003E55: 22 50                         w3 =:        b.0x40
08003E57: 4A 7B                         w stz        b.0xEC
08003E59: C3 F8 00 00 28 04 7B 4B 4D 4F call         $0xFFFFFFFFF8000028,$0x4,b.0xEC,b.0x2C,b.0x34,b.0x3C ; MON 50B OPEN
08003E63: 9D                            ifkret
08003E64: 0C 7B                         w1 :=        b.0xEC
08003E66: 80                            ret
08003E67: B8 CF 00 00 00 20             ents         $0x20
08003E6D: C3 F8 00 00 32 02 45 47       call         $0xFFFFFFFFF8000032,$0x2,b.0x14,b.0x1C ; MON 62B RMAX
08003E75: 9D                            ifkret
08003E76: 0C 47                         w1 :=        b.0x1C
08003E78: 80                            ret
08003E79: 0C 47                         w1 :=        b.0x1C
08003E7B: 80                            ret
08003E7C: B8 CF 00 00 00 1C             ents         $0x1C
08003E82: 20 45                         w1 =:        b.0x14
08003E84: 20 46                         w1 =:        b.0x18
08003E86: C3 F8 00 00 34 01 46          call         $0xFFFFFFFFF8000034,$0x1,b.0x18 ; MON 64B ERMSG
08003E8D: 9D                            ifkret
08003E8E: 80                            ret
08003E8F: B8 CF 00 00 00 20             ents         $0x20
08003E95: 20 46                         w1 =:        b.0x18
08003E97: 20 47                         w1 =:        b.0x1C
08003E99: C3 F8 00 00 3B 02 45 47       call         $0xFFFFFFFFF800003B,$0x2,b.0x14,b.0x1C ; MON 73B SMAX
08003EA1: 9D                            ifkret
08003EA2: 80                            ret
08003EA3: B8 CF 00 00 00 20             ents         $0x20
08003EA9: 20 46                         w1 =:        b.0x18
08003EAB: 20 47                         w1 =:        b.0x1C
08003EAD: 0C 47                         w1 :=        b.0x1C
08003EAF: FC AD D0 01                   w sha        r1,$0x1
08003EB3: 20 47                         w1 =:        b.0x1C
08003EB5: C3 F8 00 00 3E 02 45 47       call         $0xFFFFFFFFF800003E,$0x2,b.0x14,b.0x1C ; MON 76B SETBS
08003EBD: 9D                            ifkret
08003EBE: 80                            ret
08003EBF: B8 CF 00 00 00 24             ents         $0x24
08003EC5: 0C 46                         w1 :=        b.0x18
08003EC7: FD 3D E4 14                   w2 laddr     @b.0x14+
08003ECB: 21 48                         w2 =:        b.0x20
08003ECD: C3 F8 00 00 4B 01 C5 20       call         $0xFFFFFFFFF800004B,$0x1,@b.0x20 ; MON 113B CLOCK
08003ED5: 9D                            ifkret
08003ED6: 80                            ret
08003ED7: B8 CF 00 00 00 38             ents         $0x38
08003EDD: 0C 48                         w1 :=        b.0x20
08003EDF: FD 3D E4 1C                   w2 laddr     @b.0x1C+
08003EE3: 21 4C                         w2 =:        b.0x30
08003EE5: 0E 4B                         w3 :=        b.0x2C
08003EE7: FC AD D2 01                   w sha        r3,$0x1
08003EEB: 22 4D                         w3 =:        b.0x34
08003EED: C3 F8 00 00 4F 05 45 46 C5 30 4A 4D       call         $0xFFFFFFFFF800004F,$0x5,b.0x14,b.0x18,@b.0x30,b.0x28,b.0x34 ; MON 117B RFILE
08003EF9: 9D                            ifkret
08003EFA: 80                            ret
08003EFB: B8 CF 00 00 00 38             ents         $0x38
08003F01: 0C 48                         w1 :=        b.0x20
08003F03: FD 3D E4 1C                   w2 laddr     @b.0x1C+
08003F07: 21 4C                         w2 =:        b.0x30
08003F09: 0E 4B                         w3 :=        b.0x2C
08003F0B: FC AD D2 01                   w sha        r3,$0x1
08003F0F: 22 4D                         w3 =:        b.0x34
08003F11: C3 F8 00 00 50 05 45 46 C5 30 4A 4D       call         $0xFFFFFFFFF8000050,$0x5,b.0x14,b.0x18,@b.0x30,b.0x28,b.0x34 ; MON 120B WFILE
08003F1D: 9D                            ifkret
08003F1E: 80                            ret
08003F1F: B8 CF 00 00 00 24             ents         $0x24
08003F25: C3 F8 00 01 0A 04 45 46 47 48 call         $0xFFFFFFFFF800010A,$0x4,b.0x14,b.0x18,b.0x1C,b.0x20 ; MON 412B FSCNT
08003F2F: 9D                            ifkret
08003F30: 80                            ret
08003F31: B8 CF 00 00 00 1C             ents         $0x1C
08003F37: C3 F8 00 01 0B 02 45 46       call         $0xFFFFFFFFF800010B,$0x2,b.0x14,b.0x18 ; MON 413B FSCDNT
08003F3F: 9D                            ifkret
08003F40: 80                            ret
08003F41: 00                    ??? ; opcode 0x0000
