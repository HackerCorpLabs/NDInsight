; ═══════════════════════════════════════════════════════════════
; ND-500 Disassembly
; ═══════════════════════════════════════════════════════════════
; File: automake-500-c00/automake-500-c00.dom
;
; File Type:    Domain (DOM)
; Domain Type:  SINTRAN III Root Domain
; Linker:       v97.251
; Entry Point:  0x08001305
; Segments:     1 used
;
; Segment  1:
;   Program: 0xCB66 bytes at virtual 0x00000000
;   Data:    0xAF31 bytes at virtual 0x00000000
;
; ═══════════════════════════════════════════════════════════════

; -- Segment 1 Program at 0x08000000 --
;
08001305: DC 08 00 00 08 CF 00 00 00 84 CE 22 60          init         $0x8000008,$0x84,$0x2260
08001312: 44 C4 08 00 00 04             w test       $0x8000004
08001318: C4 0A                         if = go      $0xA
0800131A: 4A C4 08 00 00 04             w stz        $0x8000004
08001320: C0 08                         go           $0x8
08001322: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
08001328: C0 19                         go           $0x19
0800132A: 9C                            entd
0800132B: FD C0 60                      l=:          b.0x80
0800132E: 20 43                         w1 =:        b.0xC
08001330: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08001336: 9D                            ifkret
08001337: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
0800133D: FE 03                         clrk
0800133F: B4 60                         jumpg        b.0x80
08001341: 0D CF 08 00 10 72             w2 :=        $0x8001072
08001347: 18 42                         r:=          b.0x8
08001349: 21 85                         w2 =:        r.0x14
0800134B: C3 08 00 AF 68 00             call         $0x800AF68,$0x0
08001351: D2 08                         if -k go     $0x8
08001353: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001359: C3 08 00 AE 4D 00             call         $0x800AE4D,$0x0
0800135F: D2 08                         if -k go     $0x8
08001361: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001367: C3 08 00 83 41 00             call         $0x8008341,$0x0
0800136D: D2 08                         if -k go     $0x8
0800136F: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001375: C3 08 00 A7 A3 00             call         $0x800A7A3,$0x0
0800137B: D2 08                         if -k go     $0x8
0800137D: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001383: C3 08 00 12 92 00             call         $0x8001292,$0x0
08001389: D2 08                         if -k go     $0x8
0800138B: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001391: C3 08 00 8F EF 00             call         $0x8008FEF,$0x0
08001397: D2 08                         if -k go     $0x8
08001399: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800139F: C3 08 00 A4 BD 00             call         $0x800A4BD,$0x0
080013A5: D2 08                         if -k go     $0x8
080013A7: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080013AD: FD 20 CD 20 47 CD 64          by bmove     $0x20,b.0x1C,$0x64
080013B4: 18 42                         r:=          b.0x8
080013B6: 4A 85                         w stz        r.0x14
080013B8: FE 79 C4 08 00 26 14 86 03    w bmove      $0x8002614,r.0x18,$0x3
080013C1: FD 3D 47                      w2 laddr     b.0x1C
080013C4: 21 89                         w2 =:        r.0x24
080013C6: 4A 8A                         w stz        r.0x28
080013C8: 1A CD 63 8B                   w move       $0x63,r.0x2C
080013CC: C3 08 00 C3 D5 00             call         $0x800C3D5,$0x0
080013D2: D2 08                         if -k go     $0x8
080013D4: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080013DA: FD 3D 47                      w2 laddr     b.0x1C
080013DD: 18 42                         r:=          b.0x8
080013DF: 21 85                         w2 =:        r.0x14
080013E1: 4A 86                         w stz        r.0x18
080013E3: 1A CD 63 87                   w move       $0x63,r.0x1C
080013E7: C3 08 00 85 BA 00             call         $0x80085BA,$0x0
080013ED: D2 08                         if -k go     $0x8
080013EF: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080013F5: FD 3D 47                      w2 laddr     b.0x1C
080013F8: 18 42                         r:=          b.0x8
080013FA: 21 85                         w2 =:        r.0x14
080013FC: 4A 86                         w stz        r.0x18
080013FE: 1A CD 63 87                   w move       $0x63,r.0x1C
08001402: 86                            bi3 clr
08001403: 22 46                         w3 =:        b.0x18
08001405: 22 88                         w3 =:        r.0x20
08001407: C3 08 00 00 04 00             call         $0x8000004,$0x0
0800140D: D2 08                         if -k go     $0x8
0800140F: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001415: 18 42                         r:=          b.0x8
08001417: 1A 88 46                      w move       r.0x20,b.0x18
0800141A: 44 D0                         w test       r1
0800141C: C4 33                         if = go      $0x33
0800141E: 0C 46                         w1 :=        b.0x18
08001420: 2D D4 1C CD 2C                by comp2     b.0x1C+,$0x2C
08001425: C6 07                         if >< go     $0x7
08001427: 19 CD 20 D4 1C                by move      $0x20,b.0x1C+
0800142C: FD 3D 47                      w2 laddr     b.0x1C
0800142F: 21 85                         w2 =:        r.0x14
08001431: 4A 86                         w stz        r.0x18
08001433: 1A CD 63 87                   w move       $0x63,r.0x1C
08001437: 20 88                         w1 =:        r.0x20
08001439: C3 08 00 0A 78 00             call         $0x8000A78,$0x0
0800143F: D2 08                         if -k go     $0x8
08001441: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001447: 18 42                         r:=          b.0x8
08001449: 1A 88 46                      w move       r.0x20,b.0x18
0800144C: C1 02 18                      go           $0x218
0800144F: FD 3C 47                      w1 laddr     b.0x1C
08001452: 18 42                         r:=          b.0x8
08001454: 20 85                         w1 =:        r.0x14
08001456: 4A 86                         w stz        r.0x18
08001458: 1A CD 63 87                   w move       $0x63,r.0x1C
0800145C: 1A 46 88                      w move       b.0x18,r.0x20
0800145F: C3 08 00 11 FA 00             call         $0x80011FA,$0x0
08001465: D2 08                         if -k go     $0x8
08001467: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800146D: 18 42                         r:=          b.0x8
0800146F: 1A 88 46                      w move       r.0x20,b.0x18
08001472: 1C 45                         by1 =:       b.0x14
08001474: 30 C4 08 00 26 D0             by1 comp     $0x80026D0
0800147A: D5 01 BA                      if >> go     $0x1BA
0800147D: B4 E0 08 00 26 D4             jumpg        $0x80026D4+
08001483: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
08001489: C1 01 D8                      go           $0x1D8
0800148C: C3 08 00 06 FE 00             call         $0x80006FE,$0x0
08001492: D2 08                         if -k go     $0x8
08001494: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800149A: C1 01 C7                      go           $0x1C7
0800149D: C3 08 00 07 0C 00             call         $0x800070C,$0x0
080014A3: D2 08                         if -k go     $0x8
080014A5: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080014AB: C1 01 B6                      go           $0x1B6
080014AE: 18 42                         r:=          b.0x8
080014B0: 19 45 85                      by move      b.0x14,r.0x14
080014B3: FE 79 C4 08 00 26 28 86 03    w bmove      $0x8002628,r.0x18,$0x3
080014BC: FD 3C 47                      w1 laddr     b.0x1C
080014BF: 20 89                         w1 =:        r.0x24
080014C1: 4A 8A                         w stz        r.0x28
080014C3: 1A CD 63 8B                   w move       $0x63,r.0x2C
080014C7: 1A 46 8C                      w move       b.0x18,r.0x30
080014CA: C3 08 00 07 21 00             call         $0x8000721,$0x0
080014D0: D2 08                         if -k go     $0x8
080014D2: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080014D8: 18 42                         r:=          b.0x8
080014DA: 1A 8C 46                      w move       r.0x30,b.0x18
080014DD: C1 01 84                      go           $0x184
080014E0: 18 42                         r:=          b.0x8
080014E2: 19 CD 8C 85                   by move      $0x8C,r.0x14
080014E6: FE 79 C4 08 00 26 5C 86 03    w bmove      $0x800265C,r.0x18,$0x3
080014EF: FD 3C 47                      w1 laddr     b.0x1C
080014F2: 20 89                         w1 =:        r.0x24
080014F4: 4A 8A                         w stz        r.0x28
080014F6: 1A CD 63 8B                   w move       $0x63,r.0x2C
080014FA: 1A 46 8C                      w move       b.0x18,r.0x30
080014FD: C3 08 00 07 21 00             call         $0x8000721,$0x0
08001503: D2 08                         if -k go     $0x8
08001505: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800150B: 18 42                         r:=          b.0x8
0800150D: 1A 8C 46                      w move       r.0x30,b.0x18
08001510: C1 01 51                      go           $0x151
08001513: 18 42                         r:=          b.0x8
08001515: 19 CD 96 85                   by move      $0x96,r.0x14
08001519: FE 79 C4 08 00 26 78 86 03    w bmove      $0x8002678,r.0x18,$0x3
08001522: FD 3C 47                      w1 laddr     b.0x1C
08001525: 20 89                         w1 =:        r.0x24
08001527: 4A 8A                         w stz        r.0x28
08001529: 1A CD 63 8B                   w move       $0x63,r.0x2C
0800152D: 1A 46 8C                      w move       b.0x18,r.0x30
08001530: C3 08 00 07 21 00             call         $0x8000721,$0x0
08001536: D2 08                         if -k go     $0x8
08001538: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800153E: 18 42                         r:=          b.0x8
08001540: 1A 8C 46                      w move       r.0x30,b.0x18
08001543: C1 01 1E                      go           $0x11E
08001546: 18 42                         r:=          b.0x8
08001548: 19 CD 9A 85                   by move      $0x9A,r.0x14
0800154C: FE 79 C4 08 00 26 9C 86 03    w bmove      $0x800269C,r.0x18,$0x3
08001555: FD 3C 47                      w1 laddr     b.0x1C
08001558: 20 89                         w1 =:        r.0x24
0800155A: 4A 8A                         w stz        r.0x28
0800155C: 1A CD 63 8B                   w move       $0x63,r.0x2C
08001560: 1A 46 8C                      w move       b.0x18,r.0x30
08001563: C3 08 00 07 21 00             call         $0x8000721,$0x0
08001569: D2 08                         if -k go     $0x8
0800156B: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001571: 18 42                         r:=          b.0x8
08001573: 1A 8C 46                      w move       r.0x30,b.0x18
08001576: C1 00 EB                      go           $0xEB
08001579: FD 3C 47                      w1 laddr     b.0x1C
0800157C: 18 42                         r:=          b.0x8
0800157E: 20 85                         w1 =:        r.0x14
08001580: 4A 86                         w stz        r.0x18
08001582: 1A CD 63 87                   w move       $0x63,r.0x1C
08001586: 1A 46 88                      w move       b.0x18,r.0x20
08001589: C3 08 00 07 C2 00             call         $0x80007C2,$0x0
0800158F: D2 08                         if -k go     $0x8
08001591: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001597: 18 42                         r:=          b.0x8
08001599: 1A 88 46                      w move       r.0x20,b.0x18
0800159C: C1 00 C5                      go           $0xC5
0800159F: FD 3C 47                      w1 laddr     b.0x1C
080015A2: 18 42                         r:=          b.0x8
080015A4: 20 85                         w1 =:        r.0x14
080015A6: 4A 86                         w stz        r.0x18
080015A8: 1A CD 63 87                   w move       $0x63,r.0x1C
080015AC: 1A 46 88                      w move       b.0x18,r.0x20
080015AF: C3 08 00 08 B4 00             call         $0x80008B4,$0x0
080015B5: D2 08                         if -k go     $0x8
080015B7: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080015BD: 18 42                         r:=          b.0x8
080015BF: 1A 88 46                      w move       r.0x20,b.0x18
080015C2: C1 00 9F                      go           $0x9F
080015C5: FD 3C 47                      w1 laddr     b.0x1C
080015C8: 18 42                         r:=          b.0x8
080015CA: 20 85                         w1 =:        r.0x14
080015CC: 4A 86                         w stz        r.0x18
080015CE: 1A CD 63 87                   w move       $0x63,r.0x1C
080015D2: 1A 46 88                      w move       b.0x18,r.0x20
080015D5: C3 08 00 0A 78 00             call         $0x8000A78,$0x0
080015DB: D2 08                         if -k go     $0x8
080015DD: C3 08 00 13 2A 00             call         $0x800132A,$0x0
080015E3: 18 42                         r:=          b.0x8
080015E5: 1A 88 46                      w move       r.0x20,b.0x18
080015E8: C0 79                         go           $0x79
080015EA: FD 3C 47                      w1 laddr     b.0x1C
080015ED: 18 42                         r:=          b.0x8
080015EF: 20 85                         w1 =:        r.0x14
080015F1: 4A 86                         w stz        r.0x18
080015F3: 1A CD 63 87                   w move       $0x63,r.0x1C
080015F7: 1A 46 88                      w move       b.0x18,r.0x20
080015FA: C3 08 00 0D CE 00             call         $0x8000DCE,$0x0
08001600: D2 08                         if -k go     $0x8
08001602: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001608: 18 42                         r:=          b.0x8
0800160A: 1A 88 46                      w move       r.0x20,b.0x18
0800160D: C0 54                         go           $0x54
0800160F: FD 3C 47                      w1 laddr     b.0x1C
08001612: 18 42                         r:=          b.0x8
08001614: 20 85                         w1 =:        r.0x14
08001616: 4A 86                         w stz        r.0x18
08001618: 1A CD 63 87                   w move       $0x63,r.0x1C
0800161C: 1A 46 88                      w move       b.0x18,r.0x20
0800161F: C3 08 00 0E EA 00             call         $0x8000EEA,$0x0
08001625: D2 08                         if -k go     $0x8
08001627: C3 08 00 13 2A 00             call         $0x800132A,$0x0
0800162D: 18 42                         r:=          b.0x8
0800162F: 1A 88 46                      w move       r.0x20,b.0x18
08001632: C0 2F                         go           $0x2F
08001634: 4D 85                         w set1       r.0x14
08001636: FE 79 C4 08 00 26 B8 86 03    w bmove      $0x80026B8,r.0x18,$0x3
0800163F: FE 79 C4 08 00 26 C4 89 03    w bmove      $0x80026C4,r.0x24,$0x3
08001648: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
0800164E: D2 08                         if -k go     $0x8
08001650: C3 08 00 13 2A 00             call         $0x800132A,$0x0
08001656: 0C CD C6                      w1 :=        $0xC6
08001659: 18 40                         r:=          b.0x0
0800165B: 18 82                         r:=          r.0x8
0800165D: FD BD 80                      tos:=        r.0x0
08001660: 81                            retk
08001661: C1 FD EE                      go           $0xFFFFFFFFFFFFFDEE
08001664: C3 F8 00 00 00 00             call         $0xFFFFFFFFF8000000,$0x0 ; MON 0B LEAVE
0800166A: B8 CF 00 00 00 20             ents         $0x20
08001670: 44 C4 08 00 29 60             w test       $0x8002960
08001676: C6 1D                         if >< go     $0x1D
08001678: 18 42                         r:=          b.0x8
0800167A: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
08001683: 0C 08                         w1 :=        $0x8
08001685: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
0800168B: 9D                            ifkret
0800168C: 20 47                         w1 =:        b.0x1C
0800168E: 1A 47 46                      w move       b.0x1C,b.0x18
08001691: C0 12                         go           $0x12
08001693: 0C C4 08 00 29 60             w1 :=        $0x8002960
08001699: 20 46                         w1 =:        b.0x18
0800169B: 1A F4 04 C4 08 00 29 60       w move       r1.(0x4),$0x8002960
080016A3: 84                            bi1 clr
080016A4: 20 C5 18                      w1 =:        @b.0x18
080016A7: 18 46                         r:=          b.0x18
080016A9: 4A 81                         w stz        r.0x4
080016AB: 0C 46                         w1 :=        b.0x18
080016AD: 80                            ret
080016AE: B8 CF 00 00 00 18             ents         $0x18
080016B4: 44 45                         w test       b.0x14
080016B6: C4 2A                         if = go      $0x2A
080016B8: 18 45                         r:=          b.0x14
080016BA: 0C 81                         w1 :=        r.0x4
080016BC: 18 42                         r:=          b.0x8
080016BE: 20 85                         w1 =:        r.0x14
080016C0: C3 08 00 16 AE 00             call         $0x80016AE,$0x0
080016C6: 9D                            ifkret
080016C7: 18 42                         r:=          b.0x8
080016C9: 0D 85                         w2 :=        r.0x14
080016CB: 18 45                         r:=          b.0x14
080016CD: 21 81                         w2 =:        r.0x4
080016CF: 1A C4 08 00 29 60 81          w move       $0x8002960,r.0x4
080016D6: 86                            bi3 clr
080016D7: 52 45 D2                      w swap       b.0x14,r3
080016DA: 22 C4 08 00 29 60             w3 =:        $0x8002960
080016E0: 80                            ret
080016E1: B8 CF 00 00 00 20             ents         $0x20
080016E7: 44 C4 08 00 29 6C             w test       $0x800296C
080016ED: C6 1D                         if >< go     $0x1D
080016EF: 18 42                         r:=          b.0x8
080016F1: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080016FA: 0C 08                         w1 :=        $0x8
080016FC: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
08001702: 9D                            ifkret
08001703: 20 47                         w1 =:        b.0x1C
08001705: 1A 47 46                      w move       b.0x1C,b.0x18
08001708: C0 12                         go           $0x12
0800170A: 0C C4 08 00 29 6C             w1 :=        $0x800296C
08001710: 20 46                         w1 =:        b.0x18
08001712: 1A F4 04 C4 08 00 29 6C       w move       r1.(0x4),$0x800296C
0800171A: 84                            bi1 clr
0800171B: 20 C5 18                      w1 =:        @b.0x18
0800171E: 18 46                         r:=          b.0x18
08001720: 4A 81                         w stz        r.0x4
08001722: 0C 46                         w1 :=        b.0x18
08001724: 80                            ret
08001725: B8 CF 00 00 00 18             ents         $0x18
0800172B: 44 45                         w test       b.0x14
0800172D: C4 2A                         if = go      $0x2A
0800172F: 18 45                         r:=          b.0x14
08001731: 0C 81                         w1 :=        r.0x4
08001733: 18 42                         r:=          b.0x8
08001735: 20 85                         w1 =:        r.0x14
08001737: C3 08 00 17 25 00             call         $0x8001725,$0x0
0800173D: 9D                            ifkret
0800173E: 18 42                         r:=          b.0x8
08001740: 0D 85                         w2 :=        r.0x14
08001742: 18 45                         r:=          b.0x14
08001744: 21 81                         w2 =:        r.0x4
08001746: 1A C4 08 00 29 6C 81          w move       $0x800296C,r.0x4
0800174D: 86                            bi3 clr
0800174E: 52 45 D2                      w swap       b.0x14,r3
08001751: 22 C4 08 00 29 6C             w3 =:        $0x800296C
08001757: 80                            ret
08001758: B8 CF 00 00 00 20             ents         $0x20
0800175E: 44 C4 08 00 29 70             w test       $0x8002970
08001764: C6 1E                         if >< go     $0x1E
08001766: 18 42                         r:=          b.0x8
08001768: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
08001771: 0C CD 24                      w1 :=        $0x24
08001774: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
0800177A: 9D                            ifkret
0800177B: 20 47                         w1 =:        b.0x1C
0800177D: 1A 47 46                      w move       b.0x1C,b.0x18
08001780: C0 12                         go           $0x12
08001782: 0C C4 08 00 29 70             w1 :=        $0x8002970
08001788: 20 46                         w1 =:        b.0x18
0800178A: 1A F4 20 C4 08 00 29 70       w move       r1.(0x20),$0x8002970
08001792: 84                            bi1 clr
08001793: 20 C5 18                      w1 =:        @b.0x18
08001796: 18 46                         r:=          b.0x18
08001798: 4A 83                         w stz        r.0xC
0800179A: 0D 01                         w2 :=        $0x1
0800179C: 21 81                         w2 =:        r.0x4
0800179E: 4D 82                         w set1       r.0x8
080017A0: 4A 84                         w stz        r.0x10
080017A2: 86                            bi3 clr
080017A3: 22 85                         w3 =:        r.0x14
080017A5: 4A 86                         w stz        r.0x18
080017A7: 22 87                         w3 =:        r.0x1C
080017A9: 4A 88                         w stz        r.0x20
080017AB: 0C 46                         w1 :=        b.0x18
080017AD: 80                            ret
080017AE: B8 CF 00 00 00 18             ents         $0x18
080017B4: 44 45                         w test       b.0x14
080017B6: C4 54                         if = go      $0x54
080017B8: 18 45                         r:=          b.0x14
080017BA: 0C 85                         w1 :=        r.0x14
080017BC: 18 42                         r:=          b.0x8
080017BE: 20 85                         w1 =:        r.0x14
080017C0: C3 08 00 87 17 00             call         $0x8008717,$0x0
080017C6: 9D                            ifkret
080017C7: 18 42                         r:=          b.0x8
080017C9: 0D 85                         w2 :=        r.0x14
080017CB: 18 45                         r:=          b.0x14
080017CD: 21 85                         w2 =:        r.0x14
080017CF: 0E 87                         w3 :=        r.0x1C
080017D1: 18 42                         r:=          b.0x8
080017D3: 22 85                         w3 =:        r.0x14
080017D5: C3 08 00 87 17 00             call         $0x8008717,$0x0
080017DB: 9D                            ifkret
080017DC: 18 42                         r:=          b.0x8
080017DE: 0D 85                         w2 :=        r.0x14
080017E0: 18 45                         r:=          b.0x14
080017E2: 21 87                         w2 =:        r.0x1C
080017E4: 0E 86                         w3 :=        r.0x18
080017E6: 18 42                         r:=          b.0x8
080017E8: 22 85                         w3 =:        r.0x14
080017EA: C3 08 00 17 25 00             call         $0x8001725,$0x0
080017F0: 9D                            ifkret
080017F1: 18 42                         r:=          b.0x8
080017F3: 0D 85                         w2 :=        r.0x14
080017F5: 18 45                         r:=          b.0x14
080017F7: 21 86                         w2 =:        r.0x18
080017F9: 1A C4 08 00 29 70 88          w move       $0x8002970,r.0x20
08001800: 86                            bi3 clr
08001801: 52 45 D2                      w swap       b.0x14,r3
08001804: 22 C4 08 00 29 70             w3 =:        $0x8002970
0800180A: 80                            ret
0800180B: B8 CF 00 00 00 20             ents         $0x20
08001811: 44 C4 08 00 29 64             w test       $0x8002964
08001817: C6 1D                         if >< go     $0x1D
08001819: 18 42                         r:=          b.0x8
0800181B: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
08001824: 0C 0C                         w1 :=        $0xC
08001826: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
0800182C: 9D                            ifkret
0800182D: 20 47                         w1 =:        b.0x1C
0800182F: 1A 47 46                      w move       b.0x1C,b.0x18
08001832: C0 12                         go           $0x12
08001834: 0C C4 08 00 29 64             w1 :=        $0x8002964
0800183A: 20 46                         w1 =:        b.0x18
0800183C: 1A F4 08 C4 08 00 29 64       w move       r1.(0x8),$0x8002964
08001844: 4A C5 18                      w stz        @b.0x18
08001847: 84                            bi1 clr
08001848: 18 46                         r:=          b.0x18
0800184A: 20 81                         w1 =:        r.0x4
0800184C: 4A 82                         w stz        r.0x8
0800184E: 0C 46                         w1 :=        b.0x18
08001850: 80                            ret
08001851: B8 CF 00 00 00 18             ents         $0x18
08001857: 44 45                         w test       b.0x14
08001859: C4 3F                         if = go      $0x3F
0800185B: 18 45                         r:=          b.0x14
0800185D: 0C 81                         w1 :=        r.0x4
0800185F: 18 42                         r:=          b.0x8
08001861: 20 85                         w1 =:        r.0x14
08001863: C3 08 00 17 AE 00             call         $0x80017AE,$0x0
08001869: 9D                            ifkret
0800186A: 18 42                         r:=          b.0x8
0800186C: 0D 85                         w2 :=        r.0x14
0800186E: 18 45                         r:=          b.0x14
08001870: 21 81                         w2 =:        r.0x4
08001872: 0E 82                         w3 :=        r.0x8
08001874: 18 42                         r:=          b.0x8
08001876: 22 85                         w3 =:        r.0x14
08001878: C3 08 00 18 51 00             call         $0x8001851,$0x0
0800187E: 9D                            ifkret
0800187F: 18 42                         r:=          b.0x8
08001881: 0D 85                         w2 :=        r.0x14
08001883: 18 45                         r:=          b.0x14
08001885: 21 82                         w2 =:        r.0x8
08001887: 1A C4 08 00 29 64 82          w move       $0x8002964,r.0x8
0800188E: 86                            bi3 clr
0800188F: 52 45 D2                      w swap       b.0x14,r3
08001892: 22 C4 08 00 29 64             w3 =:        $0x8002964
08001898: 80                            ret
08001899: B8 CF 00 00 00 28             ents         $0x28
0800189F: 44 C4 08 00 29 68             w test       $0x8002968
080018A5: C6 1F                         if >< go     $0x1F
080018A7: 18 42                         r:=          b.0x8
080018A9: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080018B2: 0C CE 02 28                   w1 :=        $0x228
080018B6: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080018BC: 9D                            ifkret
080018BD: 20 47                         w1 =:        b.0x1C
080018BF: 1A 47 46                      w move       b.0x1C,b.0x18
080018C2: C0 13                         go           $0x13
080018C4: 0C C4 08 00 29 68             w1 :=        $0x8002968
080018CA: 20 46                         w1 =:        b.0x18
080018CC: 1A F8 02 24 C4 08 00 29 68    w move       r1.(0x224),$0x8002968
080018D5: FD 3C C5 18                   w1 laddr     @b.0x18
080018D9: 20 49                         w1 =:        b.0x24
080018DB: 85                            bi2 clr
080018DC: 1A CE 00 89 48                w move       $0x89,b.0x20
080018E1: CA 06                         if < go      $0x6
080018E3: 86                            bi3 clr
080018E4: FD 8A 48                      w3 sfill     b.0x20
080018E7: 84                            bi1 clr
080018E8: 18 46                         r:=          b.0x18
080018EA: 20 CA 02 24                   w1 =:        r.0x224
080018EE: 0C 46                         w1 :=        b.0x18
080018F0: 80                            ret
080018F1: B8 CF 00 00 00 20             ents         $0x20
080018F7: 44 45                         w test       b.0x14
080018F9: C4 6A                         if = go      $0x6A
080018FB: 4A 46                         w stz        b.0x18
080018FD: FD 3D C5 14                   w2 laddr     @b.0x14
08001901: 21 47                         w2 =:        b.0x1C
08001903: 0E 46                         w3 :=        b.0x18
08001905: 0C E6 1C                      w1 :=        @b.0x1C+
08001908: 18 42                         r:=          b.0x8
0800190A: 20 85                         w1 =:        r.0x14
0800190C: C3 08 00 18 51 00             call         $0x8001851,$0x0
08001912: 9D                            ifkret
08001913: 18 42                         r:=          b.0x8
08001915: 0D 85                         w2 :=        r.0x14
08001917: FD 3E C5 14                   w3 laddr     @b.0x14
0800191B: 22 47                         w3 =:        b.0x1C
0800191D: 0F 46                         w4 :=        b.0x18
0800191F: 21 E7 1C                      w2 =:        @b.0x1C+
08001922: BF 46 CE 00 88 DB             d loopi      b.0x18,$0x88,$0xFFFFFFFFFFFFFFDB
08001928: 18 45                         r:=          b.0x14
0800192A: 0C CA 02 24                   w1 :=        r.0x224
0800192E: 18 42                         r:=          b.0x8
08001930: 20 85                         w1 =:        r.0x14
08001932: C3 08 00 18 F1 00             call         $0x80018F1,$0x0
08001938: 9D                            ifkret
08001939: 18 42                         r:=          b.0x8
0800193B: 0D 85                         w2 :=        r.0x14
0800193D: 18 45                         r:=          b.0x14
0800193F: 21 CA 02 24                   w2 =:        r.0x224
08001943: 1A C4 08 00 29 68 CA 02 24    w move       $0x8002968,r.0x224
0800194C: 86                            bi3 clr
0800194D: 52 45 D2                      w swap       b.0x14,r3
08001950: 22 C4 08 00 29 68             w3 =:        $0x8002968
08001956: 87                            bi4 clr
08001957: 23 C4 08 00 29 54             w4 =:        $0x8002954
0800195D: 23 C4 08 00 29 58             w4 =:        $0x8002958
08001963: 80                            ret
08001964: B8 CF 00 00 00 3C             ents         $0x3C
0800196A: 44 45                         w test       b.0x14
0800196C: C4 6C                         if = go      $0x6C
0800196E: 1A 45 48                      w move       b.0x14,b.0x20
08001971: C3 08 00 86 91 00             call         $0x8008691,$0x0
08001977: 9D                            ifkret
08001978: 20 49                         w1 =:        b.0x24
0800197A: 20 4A                         w1 =:        b.0x28
0800197C: FD 3C C5 20                   w1 laddr     @b.0x20
08001980: 20 4C                         w1 =:        b.0x30
08001982: 1A 14 4B                      w move       $0x14,b.0x2C
08001985: FD 3D C5 28                   w2 laddr     @b.0x28
08001989: 21 4E                         w2 =:        b.0x38
0800198B: 1A 14 4D                      w move       $0x14,b.0x34
0800198E: CA 08                         if < go      $0x8
08001990: 84                            bi1 clr
08001991: 85                            bi2 clr
08001992: FD 67 4B 4D                   by smove     b.0x2C,b.0x34
08001996: 18 48                         r:=          b.0x20
08001998: 44 85                         w test       r.0x14
0800199A: C4 15                         if = go      $0x15
0800199C: 1A 85 48                      w move       r.0x14,b.0x20
0800199F: C3 08 00 86 91 00             call         $0x8008691,$0x0
080019A5: 9D                            ifkret
080019A6: 18 4A                         r:=          b.0x28
080019A8: 20 85                         w1 =:        r.0x14
080019AA: 1A 85 4A                      w move       r.0x14,b.0x28
080019AD: C0 CF                         go           $0xFFFFFFFFFFFFFFCF
080019AF: 44 46                         w test       b.0x18
080019B1: C4 24                         if = go      $0x24
080019B3: 4A 47                         w stz        b.0x1C
080019B5: FD 3C C5 28                   w1 laddr     @b.0x28
080019B9: 54 47                         w1 +         b.0x1C
080019BB: 2D F4 00 0D                   by comp2     r1.(0x0),$0xD
080019BF: C4 06                         if = go      $0x6
080019C1: 4F 47                         w incr       b.0x1C
080019C3: C0 F2                         go           $0xFFFFFFFFFFFFFFF2
080019C5: 05 09                         by2 :=       $0x9
080019C7: FD 3E C5 28                   w3 laddr     @b.0x28
080019CB: 56 47                         w3 +         b.0x1C
080019CD: 1D F6 00                      by2 =:       r3.(0x0)
080019D0: 18 4A                         r:=          b.0x28
080019D2: 1A 46 85                      w move       b.0x18,r.0x14
080019D5: 1A 49 46                      w move       b.0x24,b.0x18
080019D8: 80                            ret
080019D9: B8 CF 00 00 00 CC             ents         $0xCC
080019DF: 18 42                         r:=          b.0x8
080019E1: 1A 45 85                      w move       b.0x14,r.0x14
080019E4: FD 3C 4B                      w1 laddr     b.0x2C
080019E7: 20 86                         w1 =:        r.0x18
080019E9: 4A 87                         w stz        r.0x1C
080019EB: 1A CD 63 88                   w move       $0x63,r.0x20
080019EF: FE 79 00 89 03                w bmove      $0x0,r.0x24,$0x3
080019F4: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
080019FA: 9D                            ifkret
080019FB: 4A 49                         w stz        b.0x24
080019FD: 0C 49                         w1 :=        b.0x24
080019FF: 2D D4 2C 0D                   by comp2     b.0x2C+,$0xD
08001A03: C4 06                         if = go      $0x6
08001A05: 4F 49                         w incr       b.0x24
08001A07: C0 F6                         go           $0xFFFFFFFFFFFFFFF6
08001A09: 1A CD 72 64                   w move       $0x72,b.0x90
08001A0D: 4D 65                         w set1       b.0x94
08001A0F: 60 01                         w1 -         $0x1
08001A11: 1A D0 6B                      w move       r1,b.0xAC
08001A14: 4A 6A                         w stz        b.0xA8
08001A16: FD 3D 4B                      w2 laddr     b.0x2C
08001A19: 21 69                         w2 =:        b.0xA4
08001A1B: FD 20 69 66 0C                by bmove     b.0xA4,b.0x98,$0xC
08001A20: 1A 3F 6C                      w move       $0x3F,b.0xB0
08001A23: C3 08 00 B9 7C 07 64 65 C5 98 C5 9C C5 A0 6C 4A call         $0x800B97C,$0x7,b.0x90,b.0x94,@b.0xFFFFFFFFFFFFFF98,@b.0xFFFFFFFFFFFFFF9C,@b.0xFFFFFFFFFFFFFFA0,b.0xB0,b.0x28
08001A33: 9D                            ifkret
08001A34: 1A CD 72 64                   w move       $0x72,b.0x90
08001A38: 4D 65                         w set1       b.0x94
08001A3A: FE 79 C4 08 00 29 E8 69 03    w bmove      $0x80029E8,b.0xA4,$0x3
08001A43: 1A 3F 6C                      w move       $0x3F,b.0xB0
08001A46: C3 08 00 B9 7C 07 64 65 C5 A4 C5 A8 C5 AC 6C 4A call         $0x800B97C,$0x7,b.0x90,b.0x94,@b.0xFFFFFFFFFFFFFFA4,@b.0xFFFFFFFFFFFFFFA8,@b.0xFFFFFFFFFFFFFFAC,b.0xB0,b.0x28
08001A56: 9D                            ifkret
08001A57: 18 42                         r:=          b.0x8
08001A59: 4D 85                         w set1       r.0x14
08001A5B: FE 79 C4 08 00 29 F8 86 03    w bmove      $0x80029F8,r.0x18,$0x3
08001A64: 1A 46 89                      w move       b.0x18,r.0x24
08001A67: C3 08 00 C6 02 00             call         $0x800C602,$0x0
08001A6D: 9D                            ifkret
08001A6E: 1A CD 72 64                   w move       $0x72,b.0x90
08001A72: 4D 65                         w set1       b.0x94
08001A74: FE 79 C4 08 00 2A 10 6D 03    w bmove      $0x8002A10,b.0xB4,$0x3
08001A7D: 1A 3F 6C                      w move       $0x3F,b.0xB0
08001A80: C3 08 00 B9 7C 07 64 65 C5 B4 C5 B8 C5 BC 6C 4A call         $0x800B97C,$0x7,b.0x90,b.0x94,@b.0xFFFFFFFFFFFFFFB4,@b.0xFFFFFFFFFFFFFFB8,@b.0xFFFFFFFFFFFFFFBC,b.0xB0,b.0x28
08001A90: 9D                            ifkret
08001A91: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08001A97: 9D                            ifkret
08001A98: 1A CD 72 64                   w move       $0x72,b.0x90
08001A9C: 4D 65                         w set1       b.0x94
08001A9E: FE 79 C4 08 00 2A 24 70 03    w bmove      $0x8002A24,b.0xC0,$0x3
08001AA7: 1A 3F 6C                      w move       $0x3F,b.0xB0
08001AAA: C3 08 00 B9 7C 07 64 65 C5 C0 C5 C4 C5 C8 6C 4A call         $0x800B97C,$0x7,b.0x90,b.0x94,@b.0xFFFFFFFFFFFFFFC0,@b.0xFFFFFFFFFFFFFFC4,@b.0xFFFFFFFFFFFFFFC8,b.0xB0,b.0x28
08001ABA: 9D                            ifkret
08001ABB: 18 42                         r:=          b.0x8
08001ABD: 1A 47 85                      w move       b.0x1C,r.0x14
08001AC0: C3 08 00 89 F2 00             call         $0x80089F2,$0x0
08001AC6: 9D                            ifkret
08001AC7: 1A CD 34 64                   w move       $0x34,b.0x90
08001ACB: C3 08 00 B9 7C 02 64 48       call         $0x800B97C,$0x2,b.0x90,b.0x20
08001AD3: 9D                            ifkret
08001AD4: 80                            ret
08001AD5: B8 CF 00 00 00 30             ents         $0x30
08001ADB: 1A 45 4B                      w move       b.0x14,b.0x2C
08001ADE: 84                            bi1 clr
08001ADF: 20 48                         w1 =:        b.0x20
08001AE1: 20 4A                         w1 =:        b.0x28
08001AE3: 20 49                         w1 =:        b.0x24
08001AE5: FD 3D C5 2C                   w2 laddr     @b.0x2C
08001AE9: 55 48                         w2 +         b.0x20
08001AEB: 04 F5 00                      by1 :=       r2.(0x0)
08001AEE: 1C 47                         by1 =:       b.0x1C
08001AF0: 30 0D                         by1 comp     $0xD
08001AF2: C4 32                         if = go      $0x32
08001AF4: 30 09                         by1 comp     $0x9
08001AF6: C4 16                         if = go      $0x16
08001AF8: FC 3C CD 20                   by1 -        $0x20
08001AFC: FD 4B D0 D2                   by wconv     r1,r3
08001B00: 0F 49                         w4 :=        b.0x24
08001B02: 57 01                         w4 +         $0x1
08001B04: 23 49                         w4 =:        b.0x24
08001B06: 6F D2                         w4 *         r3
08001B08: 57 4A                         w4 +         b.0x28
08001B0A: 23 4A                         w4 =:        b.0x28
08001B0C: 0C 48                         w1 :=        b.0x20
08001B0E: 54 01                         w1 +         $0x1
08001B10: 20 48                         w1 =:        b.0x20
08001B12: 34 13                         w1 comp      $0x13
08001B14: C8 07                         if > go      $0x7
08001B16: 2D 47 09                      by comp2     b.0x1C,$0x9
08001B19: C6 09                         if >< go     $0x9
08001B1B: 18 4B                         r:=          b.0x2C
08001B1D: 1A 85 4B                      w move       r.0x14,b.0x2C
08001B20: 4A 48                         w stz        b.0x20
08001B22: C0 C3                         go           $0xFFFFFFFFFFFFFFC3
08001B24: 0C 4A                         w1 :=        b.0x28
08001B26: 80                            ret
08001B27: B8 CF 00 00 00 14             ents         $0x14
08001B2D: 44 C4 08 00 29 54             w test       $0x8002954
08001B33: C6 18                         if >< go     $0x18
08001B35: 1A CE 00 89 C4 08 00 29 54    w move       $0x89,$0x8002954
08001B3E: C3 08 00 18 99 00             call         $0x8001899,$0x0
08001B44: 9D                            ifkret
08001B45: 20 C4 08 00 29 5C             w1 =:        $0x800295C
08001B4B: 80                            ret
08001B4C: B8 CF 00 00 00 58             ents         $0x58
08001B52: 18 42                         r:=          b.0x8
08001B54: 1A 45 85                      w move       b.0x14,r.0x14
08001B57: C3 08 00 1A D5 00             call         $0x8001AD5,$0x0
08001B5D: 9D                            ifkret
08001B5E: 20 46                         w1 =:        b.0x18
08001B60: 0D C4 08 00 29 5C             w2 :=        $0x800295C
08001B66: 21 4D                         w2 =:        b.0x34
08001B68: 44 D1                         w test       r2
08001B6A: C5 00 8B                      if = go      $0x8B
08001B6D: FC 7E D0 C4 08 00 29 54 D3    w3 div4      r1,$0x8002954,r4
08001B76: 22 4A                         w3 =:        b.0x28
08001B78: 2E 4A CE 00 88                w comp2      b.0x28,$0x88
08001B7D: CE 10                         if <= go     $0x10
08001B7F: 18 4D                         r:=          b.0x34
08001B81: 1A CA 02 24 4D                w move       r.0x224,b.0x34
08001B86: E0 4A CE 00 89                w sub2       b.0x28,$0x89
08001B8B: C0 ED                         go           $0xFFFFFFFFFFFFFFED
08001B8D: FD 3C C5 34                   w1 laddr     @b.0x34
08001B91: 20 51                         w1 =:        b.0x44
08001B93: 0D 4A                         w2 :=        b.0x28
08001B95: 1A E5 44 4E                   w move       @b.0x44+,b.0x38
08001B99: 44 4E                         w test       b.0x38
08001B9B: C4 5A                         if = go      $0x5A
08001B9D: 1A 45 4F                      w move       b.0x14,b.0x3C
08001BA0: 18 4E                         r:=          b.0x38
08001BA2: 18 81                         r:=          r.0x4
08001BA4: 1A 85 50                      w move       r.0x14,b.0x40
08001BA7: FD 3C C5 3C                   w1 laddr     @b.0x3C
08001BAB: 20 53                         w1 =:        b.0x4C
08001BAD: 1A 14 52                      w move       $0x14,b.0x48
08001BB0: FD 3D C5 40                   w2 laddr     @b.0x40
08001BB4: 21 55                         w2 =:        b.0x54
08001BB6: 1A 14 54                      w move       $0x14,b.0x50
08001BB9: 84                            bi1 clr
08001BBA: 85                            bi2 clr
08001BBB: FD BE 52 54 00                by scopa     b.0x48,b.0x50,$0x0
08001BC0: C6 2C                         if >< go     $0x2C
08001BC2: 18 4F                         r:=          b.0x3C
08001BC4: 0C 85                         w1 :=        r.0x14
08001BC6: 20 4F                         w1 =:        b.0x3C
08001BC8: 44 D0                         w test       r1
08001BCA: C6 13                         if >< go     $0x13
08001BCC: 18 50                         r:=          b.0x40
08001BCE: 0D 85                         w2 :=        r.0x14
08001BD0: 21 50                         w2 =:        b.0x40
08001BD2: 44 D1                         w test       r2
08001BD4: C6 07                         if >< go     $0x7
08001BD6: 18 4E                         r:=          b.0x38
08001BD8: 0C 81                         w1 :=        r.0x4
08001BDA: 80                            ret
08001BDB: C0 07                         go           $0x7
08001BDD: 18 50                         r:=          b.0x40
08001BDF: 1A 85 50                      w move       r.0x14,b.0x40
08001BE2: 44 4F                         w test       b.0x3C
08001BE4: C4 08                         if = go      $0x8
08001BE6: 44 50                         w test       b.0x40
08001BE8: C4 04                         if = go      $0x4
08001BEA: C0 BD                         go           $0xFFFFFFFFFFFFFFBD
08001BEC: 18 4E                         r:=          b.0x38
08001BEE: 1A 82 4E                      w move       r.0x8,b.0x38
08001BF1: 44 4E                         w test       b.0x38
08001BF3: C6 AA                         if >< go     $0xFFFFFFFFFFFFFFAA
08001BF5: 84                            bi1 clr
08001BF6: 80                            ret
08001BF7: B8 CF 00 00 00 30             ents         $0x30
08001BFD: 0C C4 08 00 29 58             w1 :=        $0x8002958
08001C03: 54 01                         w1 +         $0x1
08001C05: 20 C4 08 00 29 58             w1 =:        $0x8002958
08001C0B: 34 C4 08 00 29 54             w1 comp      $0x8002954
08001C11: CE 09                         if <= go     $0x9
08001C13: C3 08 00 1B 27 00             call         $0x8001B27,$0x0
08001C19: 9D                            ifkret
08001C1A: 1A C4 08 00 29 5C 49          w move       $0x800295C,b.0x24
08001C21: FC 7C 46 C4 08 00 29 54 D1    w1 div4      b.0x18,$0x8002954,r2
08001C2A: 20 48                         w1 =:        b.0x20
08001C2C: 2E 48 CE 00 88                w comp2      b.0x20,$0x88
08001C31: CE 10                         if <= go     $0x10
08001C33: 18 49                         r:=          b.0x24
08001C35: 1A CA 02 24 49                w move       r.0x224,b.0x24
08001C3A: E0 48 CE 00 89                w sub2       b.0x20,$0x89
08001C3F: C0 ED                         go           $0xFFFFFFFFFFFFFFED
08001C41: C3 08 00 18 0B 00             call         $0x800180B,$0x0
08001C47: 9D                            ifkret
08001C48: 20 4A                         w1 =:        b.0x28
08001C4A: 1A 46 F4 00                   w move       b.0x18,r1.(0x0)
08001C4E: FD 3D C5 24                   w2 laddr     @b.0x24
08001C52: 21 4B                         w2 =:        b.0x2C
08001C54: 0E 48                         w3 :=        b.0x20
08001C56: FD 3D E6 2C                   w2 laddr     @b.0x2C+
08001C5A: 0C 4A                         w1 :=        b.0x28
08001C5C: 0E 08                         w3 :=        $0x8
08001C5E: FE 03                         clrk
08001C60: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08001C66: 9D                            ifkret
08001C67: C3 08 00 17 58 00             call         $0x8001758,$0x0
08001C6D: 9D                            ifkret
08001C6E: 18 4A                         r:=          b.0x28
08001C70: 20 81                         w1 =:        r.0x4
08001C72: 0D 45                         w2 :=        b.0x14
08001C74: 18 4A                         r:=          b.0x28
08001C76: 18 81                         r:=          r.0x4
08001C78: 21 85                         w2 =:        r.0x14
08001C7A: 18 4A                         r:=          b.0x28
08001C7C: 0C 81                         w1 :=        r.0x4
08001C7E: 80                            ret
08001C7F: B8 CF 00 00 00 28             ents         $0x28
08001C85: 1A 45 49                      w move       b.0x14,b.0x24
08001C88: 4A 48                         w stz        b.0x20
08001C8A: FD 3D C5 24                   w2 laddr     @b.0x24
08001C8E: 55 48                         w2 +         b.0x20
08001C90: 04 F5 00                      by1 :=       r2.(0x0)
08001C93: 1C 47                         by1 =:       b.0x1C
08001C95: 30 0D                         by1 comp     $0xD
08001C97: C4 1D                         if = go      $0x1D
08001C99: 30 CD 2E                      by1 comp     $0x2E
08001C9C: C6 05                         if >< go     $0x5
08001C9E: 0C 01                         w1 :=        $0x1
08001CA0: 80                            ret
08001CA1: 0C 48                         w1 :=        b.0x20
08001CA3: 54 01                         w1 +         $0x1
08001CA5: 20 48                         w1 =:        b.0x20
08001CA7: 34 13                         w1 comp      $0x13
08001CA9: CE 09                         if <= go     $0x9
08001CAB: 18 49                         r:=          b.0x24
08001CAD: 1A 85 49                      w move       r.0x14,b.0x24
08001CB0: 4A 48                         w stz        b.0x20
08001CB2: C0 D8                         go           $0xFFFFFFFFFFFFFFD8
08001CB4: 84                            bi1 clr
08001CB5: 80                            ret
08001CB6: B8 CF 00 00 00 28             ents         $0x28
08001CBC: 1A 45 49                      w move       b.0x14,b.0x24
08001CBF: 4A 48                         w stz        b.0x20
08001CC1: FD 3D C5 24                   w2 laddr     @b.0x24
08001CC5: 55 48                         w2 +         b.0x20
08001CC7: 04 F5 00                      by1 :=       r2.(0x0)
08001CCA: 1C 47                         by1 =:       b.0x1C
08001CCC: 30 0D                         by1 comp     $0xD
08001CCE: C4 26                         if = go      $0x26
08001CD0: 30 CD 29                      by1 comp     $0x29
08001CD3: C6 07                         if >< go     $0x7
08001CD5: 0C 01                         w1 :=        $0x1
08001CD7: 80                            ret
08001CD8: C0 09                         go           $0x9
08001CDA: 30 CD 2E                      by1 comp     $0x2E
08001CDD: C6 04                         if >< go     $0x4
08001CDF: 84                            bi1 clr
08001CE0: 80                            ret
08001CE1: 0C 48                         w1 :=        b.0x20
08001CE3: 54 01                         w1 +         $0x1
08001CE5: 20 48                         w1 =:        b.0x20
08001CE7: 34 13                         w1 comp      $0x13
08001CE9: CE 09                         if <= go     $0x9
08001CEB: 18 49                         r:=          b.0x24
08001CED: 1A 85 49                      w move       r.0x14,b.0x24
08001CF0: 4A 48                         w stz        b.0x20
08001CF2: C0 CF                         go           $0xFFFFFFFFFFFFFFCF
08001CF4: 84                            bi1 clr
08001CF5: 80                            ret
08001CF6: 9C                            entd
08001CF7: FD C0 52                      l=:          b.0x48
08001CFA: 1C 53                         by1 =:       b.0x4C
08001CFC: 2E 4A 13                      w comp2      b.0x28,$0x13
08001CFF: C6 16                         if >< go     $0x16
08001D01: C3 08 00 86 91 00             call         $0x8008691,$0x0
08001D07: D2 04                         if -k go     $0x4
08001D09: B4 52                         jumpg        b.0x48
08001D0B: 18 4D                         r:=          b.0x34
08001D0D: 20 85                         w1 =:        r.0x14
08001D0F: 1A 3F 4A                      w move       $0x3F,b.0x28
08001D12: 1A 85 4D                      w move       r.0x14,b.0x34
08001D15: 0C 4A                         w1 :=        b.0x28
08001D17: 54 01                         w1 +         $0x1
08001D19: 20 4A                         w1 =:        b.0x28
08001D1B: 05 53                         by2 :=       b.0x4C
08001D1D: FD 3E C5 34                   w3 laddr     @b.0x34
08001D21: 56 D0                         w3 +         r1
08001D23: 1D F6 00                      by2 =:       r3.(0x0)
08001D26: FE 03                         clrk
08001D28: B4 52                         jumpg        b.0x48
08001D2A: B8 CF 00 00 00 70             ents         $0x70
08001D30: 20 45                         w1 =:        b.0x14
08001D32: C3 08 00 86 91 00             call         $0x8008691,$0x0
08001D38: 9D                            ifkret
08001D39: 20 4C                         w1 =:        b.0x30
08001D3B: 20 4D                         w1 =:        b.0x34
08001D3D: 1A 3F 4A                      w move       $0x3F,b.0x28
08001D40: 18 42                         r:=          b.0x8
08001D42: 1A 45 85                      w move       b.0x14,r.0x14
08001D45: C3 08 00 1C B6 00             call         $0x8001CB6,$0x0
08001D4B: 9D                            ifkret
08001D4C: 44 D0                         w test       r1
08001D4E: C4 38                         if = go      $0x38
08001D50: 1A 45 4B                      w move       b.0x14,b.0x2C
08001D53: 44 4B                         w test       b.0x2C
08001D55: C4 2F                         if = go      $0x2F
08001D57: 4A 49                         w stz        b.0x24
08001D59: FD 3D C5 2C                   w2 laddr     @b.0x2C
08001D5D: 55 49                         w2 +         b.0x24
08001D5F: 04 F5 00                      by1 :=       r2.(0x0)
08001D62: 1C 46                         by1 =:       b.0x18
08001D64: C3 08 00 1C F6 00             call         $0x8001CF6,$0x0
08001D6A: 9D                            ifkret
08001D6B: 2D 46 CD 29                   by comp2     b.0x18,$0x29
08001D6F: C4 06                         if = go      $0x6
08001D71: BF 49 13 E8                   d loopi      b.0x24,$0x13,$0xFFFFFFFFFFFFFFE8
08001D75: 2D 46 CD 29                   by comp2     b.0x18,$0x29
08001D79: C4 0B                         if = go      $0xB
08001D7B: 18 4B                         r:=          b.0x2C
08001D7D: 1A 85 4B                      w move       r.0x14,b.0x2C
08001D80: 44 4B                         w test       b.0x2C
08001D82: C6 D5                         if >< go     $0xFFFFFFFFFFFFFFD5
08001D84: C0 7A                         go           $0x7A
08001D86: 0C C4 08 00 7A 04             w1 :=        $0x8007A04
08001D8C: FC AD D0 38                   w sha        r1,$0x38
08001D90: 20 47                         w1 =:        b.0x1C
08001D92: 0D C4 08 00 7A 04             w2 :=        $0x8007A04
08001D98: E5 CE 00 FF                   w2 and       $0xFF
08001D9C: 21 48                         w2 =:        b.0x20
08001D9E: 1A CE 00 8C 54                w move       $0x8C,b.0x50
08001DA3: FD 3E 4E                      w3 laddr     b.0x38
08001DA6: 22 55                         w3 =:        b.0x54
08001DA8: 4A 56                         w stz        b.0x58
08001DAA: 1A 0F 57                      w move       $0xF,b.0x5C
08001DAD: 4A 58                         w stz        b.0x60
08001DAF: FE 79 C4 08 00 2A 30 59 03    w bmove      $0x8002A30,b.0x64,$0x3
08001DB8: C3 08 00 B9 7C 0A 54 C5 54 C5 58 C5 5C 47 48 58 C5 64 C5 68 C5 6C call         $0x800B97C,$0xA,b.0x50,@b.0x54,@b.0x58,@b.0x5C,b.0x1C,b.0x20,b.0x60,@b.0x64,@b.0x68,@b.0x6C
08001DCE: 9D                            ifkret
08001DCF: 04 CD 28                      by1 :=       $0x28
08001DD2: C3 08 00 1C F6 00             call         $0x8001CF6,$0x0
08001DD8: 9D                            ifkret
08001DD9: 4A 49                         w stz        b.0x24
08001DDB: 0C 49                         w1 :=        b.0x24
08001DDD: 2D D4 38 CD 27                by comp2     b.0x38+,$0x27
08001DE2: C4 12                         if = go      $0x12
08001DE4: 0D 49                         w2 :=        b.0x24
08001DE6: 04 D5 38                      by1 :=       b.0x38+
08001DE9: C3 08 00 1C F6 00             call         $0x8001CF6,$0x0
08001DEF: 9D                            ifkret
08001DF0: BF 49 0F EB                   d loopi      b.0x24,$0xF,$0xFFFFFFFFFFFFFFEB
08001DF4: 04 CD 29                      by1 :=       $0x29
08001DF7: C3 08 00 1C F6 00             call         $0x8001CF6,$0x0
08001DFD: 9D                            ifkret
08001DFE: 04 0D                         by1 :=       $0xD
08001E00: C3 08 00 1C F6 00             call         $0x8001CF6,$0x0
08001E06: 9D                            ifkret
08001E07: 0C 4C                         w1 :=        b.0x30
08001E09: 80                            ret
08001E0A: 9C                            entd
08001E0B: FD C0 71                      l=:          b.0xC4
08001E0E: 1C 72                         by1 =:       b.0xC8
08001E10: 2E 4D 13                      w comp2      b.0x34,$0x13
08001E13: C6 16                         if >< go     $0x16
08001E15: C3 08 00 86 91 00             call         $0x8008691,$0x0
08001E1B: D2 04                         if -k go     $0x4
08001E1D: B4 71                         jumpg        b.0xC4
08001E1F: 18 53                         r:=          b.0x4C
08001E21: 20 85                         w1 =:        r.0x14
08001E23: 1A 3F 4D                      w move       $0x3F,b.0x34
08001E26: 1A 85 53                      w move       r.0x14,b.0x4C
08001E29: 0C 4D                         w1 :=        b.0x34
08001E2B: 54 01                         w1 +         $0x1
08001E2D: 20 4D                         w1 =:        b.0x34
08001E2F: 05 72                         by2 :=       b.0xC8
08001E31: FD 3E C5 4C                   w3 laddr     @b.0x4C
08001E35: 56 D0                         w3 +         r1
08001E37: 1D F6 00                      by2 =:       r3.(0x0)
08001E3A: FE 03                         clrk
08001E3C: B4 71                         jumpg        b.0xC4
08001E3E: B8 CF 00 00 01 1C             ents         $0x11C
08001E44: C0 47                         go           $0x47
08001E46: 9C                            entd
08001E47: FD C0 73                      l=:          b.0xCC
08001E4A: 20 43                         w1 =:        b.0xC
08001E4C: 34 CD 2E                      w1 comp      $0x2E
08001E4F: C6 08                         if >< go     $0x8
08001E51: 0C CD 2E                      w1 :=        $0x2E
08001E54: 80                            ret
08001E55: C0 32                         go           $0x32
08001E57: 44 C4 08 00 79 F8             w test       $0x80079F8
08001E5D: C6 09                         if >< go     $0x9
08001E5F: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08001E65: 9D                            ifkret
08001E66: 18 42                         r:=          b.0x8
08001E68: FE 79 C4 08 00 2A 44 85 03    w bmove      $0x8002A44,r.0x14,$0x3
08001E71: 1A 45 88                      w move       b.0x14,r.0x20
08001E74: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
08001E7A: 9D                            ifkret
08001E7B: 0C 43                         w1 :=        b.0xC
08001E7D: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08001E83: 9D                            ifkret
08001E84: 0C 43                         w1 :=        b.0xC
08001E86: 81                            retk
08001E87: FE 03                         clrk
08001E89: B4 73                         jumpg        b.0xCC
08001E8B: 18 42                         r:=          b.0x8
08001E8D: 1A 45 85                      w move       b.0x14,r.0x14
08001E90: C3 08 00 1C 7F 00             call         $0x8001C7F,$0x0
08001E96: D2 08                         if -k go     $0x8
08001E98: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001E9E: 44 D0                         w test       r1
08001EA0: C7 02 84                      if >< go     $0x284
08001EA3: 18 42                         r:=          b.0x8
08001EA5: 1A 45 85                      w move       b.0x14,r.0x14
08001EA8: FD 3C 58                      w1 laddr     b.0x60
08001EAB: 20 86                         w1 =:        r.0x18
08001EAD: 4A 87                         w stz        r.0x1C
08001EAF: 1A CD 63 88                   w move       $0x63,r.0x20
08001EB3: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08001EB9: D2 08                         if -k go     $0x8
08001EBB: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001EC1: FD 3D 58                      w2 laddr     b.0x60
08001EC4: 18 42                         r:=          b.0x8
08001EC6: 21 85                         w2 =:        r.0x14
08001EC8: 4A 86                         w stz        r.0x18
08001ECA: 1A CD 63 87                   w move       $0x63,r.0x1C
08001ECE: FD 20 46 88 0C                by bmove     b.0x18,r.0x20,$0xC
08001ED3: C3 08 00 84 18 00             call         $0x8008418,$0x0
08001ED9: D2 08                         if -k go     $0x8
08001EDB: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001EE1: 85                            bi2 clr
08001EE2: 21 4E                         w2 =:        b.0x38
08001EE4: 21 4C                         w2 =:        b.0x30
08001EE6: 0C 4C                         w1 :=        b.0x30
08001EE8: 2D D4 60 0D                   by comp2     b.0x60+,$0xD
08001EEC: C4 19                         if = go      $0x19
08001EEE: 05 D4 60                      by2 :=       b.0x60+
08001EF1: 1D 74                         by2 =:       b.0xD0
08001EF3: 31 CD 29                      by2 comp     $0x29
08001EF6: C4 07                         if = go      $0x7
08001EF8: 31 CD 2E                      by2 comp     $0x2E
08001EFB: C6 06                         if >< go     $0x6
08001EFD: 54 01                         w1 +         $0x1
08001EFF: 20 4E                         w1 =:        b.0x38
08001F01: 4F 4C                         w incr       b.0x30
08001F03: C0 E3                         go           $0xFFFFFFFFFFFFFFE3
08001F05: 1A CE 00 BC 75                w move       $0xBC,b.0xD4
08001F0A: 60 01                         w1 -         $0x1
08001F0C: 1A D0 7B                      w move       r1,b.0xEC
08001F0F: 4A 7A                         w stz        b.0xE8
08001F11: FD 3D 58                      w2 laddr     b.0x60
08001F14: 21 79                         w2 =:        b.0xE4
08001F16: FD 20 79 76 0C                by bmove     b.0xE4,b.0xD8,$0xC
08001F1B: FE 79 C4 08 00 2A 50 79 03    w bmove      $0x8002A50,b.0xE4,$0x3
08001F24: C3 08 00 B9 7C 0B 75 C5 D8 C5 DC C5 E0 C5 E4 C5 E8 C5 EC 4F 50 51 52 call         $0x800B97C,$0xB,b.0xD4,@b.0xFFFFFFFFFFFFFFD8,@b.0xFFFFFFFFFFFFFFDC,@b.0xFFFFFFFFFFFFFFE0,@b.0xFFFFFFFFFFFFFFE4,@b.0xFFFFFFFFFFFFFFE8,@b.0xFFFFFFFFFFFFFFEC,b.0x3C,b.0x40,b.0x44,b.0x48
08001F3B: D2 08                         if -k go     $0x8
08001F3D: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001F43: 1A CE 00 8D 75                w move       $0x8D,b.0xD4
08001F48: 86                            bi3 clr
08001F49: FE 29 E2 08 00 29 74          h2 laddr     $0x8002974+
08001F50: 21 7C                         w2 =:        b.0xF0
08001F52: 4A 7D                         w stz        b.0xF4
08001F54: FE 79 C4 08 00 2A 5C 7E 03    w bmove      $0x8002A5C,b.0xF8,$0x3
08001F5D: C3 08 00 B9 7C 09 75 C5 F0 4F 50 51 7D C5 F8 C5 FC C6 01 00 call         $0x800B97C,$0x9,b.0xD4,@b.0xFFFFFFFFFFFFFFF0,b.0x3C,b.0x40,b.0x44,b.0xF4,@b.0xFFFFFFFFFFFFFFF8,@b.0xFFFFFFFFFFFFFFFC,@b.0x100
08001F71: D2 08                         if -k go     $0x8
08001F73: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001F79: 44 49                         w test       b.0x24
08001F7B: C4 2D                         if = go      $0x2D
08001F7D: 0D 47                         w2 :=        b.0x1C
08001F7F: 21 4B                         w2 =:        b.0x2C
08001F81: 0E 48                         w3 :=        b.0x20
08001F83: 22 75                         w3 =:        b.0xD4
08001F85: 35 D2                         w2 comp      r3
08001F87: C8 21                         if > go      $0x21
08001F89: 18 CF 08 00 29 74             r:=          $0x8002974
08001F8F: FD 3D C9 12                   w2 laddr     r.0x12
08001F93: 55 4B                         w2 +         b.0x2C
08001F95: 04 F5 00                      by1 :=       r2.(0x0)
08001F98: 0E 4B                         w3 :=        b.0x2C
08001F9A: 2D E6 18 D0                   by comp2     @b.0x18+,r1
08001F9E: C4 06                         if = go      $0x6
08001FA0: 0C CD 2E                      w1 :=        $0x2E
08001FA3: 80                            ret
08001FA4: BF 4B 75 E5                   d loopi      b.0x2C,b.0xD4,$0xFFFFFFFFFFFFFFE5
08001FA8: 18 42                         r:=          b.0x8
08001FAA: 1A 45 85                      w move       b.0x14,r.0x14
08001FAD: C3 08 00 87 17 00             call         $0x8008717,$0x0
08001FB3: D2 08                         if -k go     $0x8
08001FB5: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001FBB: 18 42                         r:=          b.0x8
08001FBD: 1A 85 45                      w move       r.0x14,b.0x14
08001FC0: C3 08 00 86 91 00             call         $0x8008691,$0x0
08001FC6: D2 08                         if -k go     $0x8
08001FC8: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08001FCE: 20 45                         w1 =:        b.0x14
08001FD0: 20 53                         w1 =:        b.0x4C
08001FD2: 85                            bi2 clr
08001FD3: 21 4D                         w2 =:        b.0x34
08001FD5: 06 CD 28                      by3 :=       $0x28
08001FD8: FD 3F F4 00                   w4 laddr     r1.(0x0)
08001FDC: 57 D1                         w4 +         r2
08001FDE: 1E F7 00                      by3 =:       r4.(0x0)
08001FE1: 44 C4 08 00 29 44             w test       $0x8002944
08001FE7: C6 14                         if >< go     $0x14
08001FE9: 2E 4F C4 08 00 29 48          w comp2      b.0x3C,$0x8002948
08001FF0: C6 0B                         if >< go     $0xB
08001FF2: 2E 50 C4 08 00 29 4C          w comp2      b.0x40,$0x800294C
08001FF9: C4 6A                         if = go      $0x6A
08001FFB: 1A CE 00 8C 7C                w move       $0x8C,b.0xF0
08002000: FD 3D 54                      w2 laddr     b.0x50
08002003: 21 C2 01 04                   w2 =:        b.0x104
08002007: 4A C2 01 08                   w stz        b.0x108
0800200B: 1A 0F C2 01 0C                w move       $0xF,b.0x10C
08002010: 4A 7D                         w stz        b.0xF4
08002012: FE 79 C4 08 00 2A 68 C2 01 10 03    w bmove      $0x8002A68,b.0x110,$0x3
0800201D: C3 08 00 B9 7C 0A 7C C6 01 04 C6 01 08 C6 01 0C 4F 50 7D C6 01 10 C6 01 14 C6 01 18 call         $0x800B97C,$0xA,b.0xF0,@b.0x104,@b.0x108,@b.0x10C,b.0x3C,b.0x40,b.0xF4,@b.0x110,@b.0x114,@b.0x118
08002039: D2 08                         if -k go     $0x8
0800203B: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08002041: 4A 4B                         w stz        b.0x2C
08002043: 0C 4B                         w1 :=        b.0x2C
08002045: 2D D4 50 CD 27                by comp2     b.0x50+,$0x27
0800204A: C4 19                         if = go      $0x19
0800204C: 0D 4B                         w2 :=        b.0x2C
0800204E: 04 D5 50                      by1 :=       b.0x50+
08002051: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
08002057: D2 08                         if -k go     $0x8
08002059: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
0800205F: BF 4B 0F E4                   d loopi      b.0x2C,$0xF,$0xFFFFFFFFFFFFFFE4
08002063: 04 CD 29                      by1 :=       $0x29
08002066: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
0800206C: D2 08                         if -k go     $0x8
0800206E: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08002074: 44 C4 08 00 29 40             w test       $0x8002940
0800207A: C4 75                         if = go      $0x75
0800207C: 4A 4B                         w stz        b.0x2C
0800207E: 18 CF 08 00 29 74             r:=          $0x8002974
08002084: FD 3C C9 02                   w1 laddr     r.0x2
08002088: 54 4B                         w1 +         b.0x2C
0800208A: 2D F4 00 CD 27                by comp2     r1.(0x0),$0x27
0800208F: C4 1D                         if = go      $0x1D
08002091: FD 3D C9 02                   w2 laddr     r.0x2
08002095: 55 4B                         w2 +         b.0x2C
08002097: 04 F5 00                      by1 :=       r2.(0x0)
0800209A: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
080020A0: D2 08                         if -k go     $0x8
080020A2: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
080020A8: BF 4B 0F D6                   d loopi      b.0x2C,$0xF,$0xFFFFFFFFFFFFFFD6
080020AC: 04 CD 3A                      by1 :=       $0x3A
080020AF: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
080020B5: D2 08                         if -k go     $0x8
080020B7: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
080020BD: 4A 4B                         w stz        b.0x2C
080020BF: 18 CF 08 00 29 74             r:=          $0x8002974
080020C5: FD 3C C9 12                   w1 laddr     r.0x12
080020C9: 54 4B                         w1 +         b.0x2C
080020CB: 2D F4 00 CD 27                by comp2     r1.(0x0),$0x27
080020D0: C4 1D                         if = go      $0x1D
080020D2: FD 3D C9 12                   w2 laddr     r.0x12
080020D6: 55 4B                         w2 +         b.0x2C
080020D8: 04 F5 00                      by1 :=       r2.(0x0)
080020DB: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
080020E1: D2 08                         if -k go     $0x8
080020E3: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
080020E9: BF 4B 03 D6                   d loopi      b.0x2C,$0x3,$0xFFFFFFFFFFFFFFD6
080020ED: C0 27                         go           $0x27
080020EF: 1A 4E 4B                      w move       b.0x38,b.0x2C
080020F2: 0C 4C                         w1 :=        b.0x30
080020F4: 60 01                         w1 -         $0x1
080020F6: 20 7C                         w1 =:        b.0xF0
080020F8: 2E 4B D0                      w comp2      b.0x2C,r1
080020FB: C8 19                         if > go      $0x19
080020FD: 0D 4B                         w2 :=        b.0x2C
080020FF: 04 D5 60                      by1 :=       b.0x60+
08002102: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
08002108: D2 08                         if -k go     $0x8
0800210A: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08002110: BF 4B 7C ED                   d loopi      b.0x2C,b.0xF0,$0xFFFFFFFFFFFFFFED
08002114: 04 0D                         by1 :=       $0xD
08002116: C3 08 00 1E 0A 00             call         $0x8001E0A,$0x0
0800211C: D2 08                         if -k go     $0x8
0800211E: C3 08 00 1E 46 00             call         $0x8001E46,$0x0
08002124: 84                            bi1 clr
08002125: 80                            ret
08002126: B8 CF 00 00 00 2C             ents         $0x2C
0800212C: 18 42                         r:=          b.0x8
0800212E: FD 20 45 85 0C                by bmove     b.0x14,r.0x14,$0xC
08002133: C3 08 00 87 DD 00             call         $0x80087DD,$0x0
08002139: 9D                            ifkret
0800213A: 18 42                         r:=          b.0x8
0800213C: 1A 88 48                      w move       r.0x20,b.0x20
0800213F: 1A 48 85                      w move       b.0x20,r.0x14
08002142: FE 79 C4 08 00 2A 78 86 03    w bmove      $0x8002A78,r.0x18,$0x3
0800214B: 4D 89                         w set1       r.0x24
0800214D: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
08002153: 9D                            ifkret
08002154: 18 42                         r:=          b.0x8
08002156: 1A 85 48                      w move       r.0x14,b.0x20
08002159: 44 D0                         w test       r1
0800215B: C6 08                         if >< go     $0x8
0800215D: 0C 01                         w1 :=        $0x1
0800215F: 80                            ret
08002160: C1 00 BE                      go           $0xBE
08002163: 1A 48 85                      w move       b.0x20,r.0x14
08002166: FE 79 C4 08 00 2A 88 86 03    w bmove      $0x8002A88,r.0x18,$0x3
0800216F: 4A 89                         w stz        r.0x24
08002171: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
08002177: 9D                            ifkret
08002178: 18 42                         r:=          b.0x8
0800217A: 1A 85 48                      w move       r.0x14,b.0x20
0800217D: 44 D0                         w test       r1
0800217F: C6 07                         if >< go     $0x7
08002181: 84                            bi1 clr
08002182: 80                            ret
08002183: C1 00 9B                      go           $0x9B
08002186: 1A 48 85                      w move       b.0x20,r.0x14
08002189: FE 79 C4 08 00 2A 98 86 03    w bmove      $0x8002A98,r.0x18,$0x3
08002192: 4A 89                         w stz        r.0x24
08002194: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
0800219A: 9D                            ifkret
0800219B: 18 42                         r:=          b.0x8
0800219D: 1A 85 48                      w move       r.0x14,b.0x20
080021A0: 44 D0                         w test       r1
080021A2: C6 06                         if >< go     $0x6
080021A4: 84                            bi1 clr
080021A5: 80                            ret
080021A6: C0 78                         go           $0x78
080021A8: 1A 48 85                      w move       b.0x20,r.0x14
080021AB: FE 79 C4 08 00 2A A8 86 03    w bmove      $0x8002AA8,r.0x18,$0x3
080021B4: 4A 89                         w stz        r.0x24
080021B6: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
080021BC: 9D                            ifkret
080021BD: 18 42                         r:=          b.0x8
080021BF: 1A 85 48                      w move       r.0x14,b.0x20
080021C2: 44 D0                         w test       r1
080021C4: C6 06                         if >< go     $0x6
080021C6: 84                            bi1 clr
080021C7: 80                            ret
080021C8: C0 56                         go           $0x56
080021CA: 1A 48 85                      w move       b.0x20,r.0x14
080021CD: FE 79 C4 08 00 2A B8 86 03    w bmove      $0x8002AB8,r.0x18,$0x3
080021D6: 4A 89                         w stz        r.0x24
080021D8: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
080021DE: 9D                            ifkret
080021DF: 18 42                         r:=          b.0x8
080021E1: 1A 85 48                      w move       r.0x14,b.0x20
080021E4: 20 4A                         w1 =:        b.0x28
080021E6: 44 D0                         w test       r1
080021E8: C6 06                         if >< go     $0x6
080021EA: 84                            bi1 clr
080021EB: 80                            ret
080021EC: C0 32                         go           $0x32
080021EE: 44 C4 08 00 79 F8             w test       $0x80079F8
080021F4: C6 09                         if >< go     $0x9
080021F6: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
080021FC: 9D                            ifkret
080021FD: 18 42                         r:=          b.0x8
080021FF: FE 79 C4 08 00 2A CC 85 03    w bmove      $0x8002ACC,r.0x14,$0x3
08002208: 1A 48 88                      w move       b.0x20,r.0x20
0800220B: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
08002211: 9D                            ifkret
08002212: 0C 4A                         w1 :=        b.0x28
08002214: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
0800221A: 9D                            ifkret
0800221B: 0C 4A                         w1 :=        b.0x28
0800221D: 81                            retk
0800221E: 9C                            entd
0800221F: FD C0 6F                      l=:          b.0xBC
08002222: 1A 07 70                      w move       $0x7,b.0xC0
08002225: 4A 47                         w stz        b.0x1C
08002227: 0C 47                         w1 :=        b.0x1C
08002229: 2D D4 57 0D                   by comp2     b.0x57+,$0xD
0800222D: C4 1C                         if = go      $0x1C
0800222F: 2E 70 CD 30                   w comp2      b.0xC0,$0x30
08002233: CC 16                         if >= go     $0x16
08002235: 0D 70                         w2 :=        b.0xC0
08002237: 55 01                         w2 +         $0x1
08002239: 21 70                         w2 =:        b.0xC0
0800223B: 06 D4 57                      by3 :=       b.0x57+
0800223E: 1E E1 08 00 29 B4             by3 =:       $0x80029B4+
08002244: BF 47 CD 63 E3                d loopi      b.0x1C,$0x63,$0xFFFFFFFFFFFFFFE3
08002249: 2E 70 CD 30                   w comp2      b.0xC0,$0x30
0800224D: CC 12                         if >= go     $0x12
0800224F: 0C 70                         w1 :=        b.0xC0
08002251: 54 01                         w1 +         $0x1
08002253: 20 70                         w1 =:        b.0xC0
08002255: 19 CD 20 E0 08 00 29 B4       by move      $0x20,$0x80029B4+
0800225D: C0 EC                         go           $0xFFFFFFFFFFFFFFEC
0800225F: 0C CD 31                      w1 :=        $0x31
08002262: 19 0D E0 08 00 29 B4          by move      $0xD,$0x80029B4+
08002269: 1A CD 72 72                   w move       $0x72,b.0xC8
0800226D: 4D 73                         w set1       b.0xCC
0800226F: FE 79 C4 08 00 2A D8 74 03    w bmove      $0x8002AD8,b.0xD0,$0x3
08002278: 1A 3F 77                      w move       $0x3F,b.0xDC
0800227B: C3 08 00 B9 7C 07 72 73 C5 D0 C5 D4 C5 D8 77 71 call         $0x800B97C,$0x7,b.0xC8,b.0xCC,@b.0xFFFFFFFFFFFFFFD0,@b.0xFFFFFFFFFFFFFFD4,@b.0xFFFFFFFFFFFFFFD8,b.0xDC,b.0xC4
0800228B: D2 04                         if -k go     $0x4
0800228D: B4 6F                         jumpg        b.0xBC
0800228F: FE 03                         clrk
08002291: B4 6F                         jumpg        b.0xBC
08002293: 9C                            entd
08002294: FD C0 78                      l=:          b.0xE0
08002297: 18 42                         r:=          b.0x8
08002299: 1A 4E 85                      w move       b.0x38,r.0x14
0800229C: C3 08 00 B4 3F 00             call         $0x800B43F,$0x0
080022A2: D2 04                         if -k go     $0x4
080022A4: B4 78                         jumpg        b.0xE0
080022A6: 1C C1 E5                      by1 =:       b.0xFFFFFFFFFFFFFFE5
080022A9: 30 0A                         by1 comp     $0xA
080022AB: C6 06                         if >< go     $0x6
080022AD: 4F 48                         w incr       b.0x20
080022AF: C0 29                         go           $0x29
080022B1: 2D C1 E5 CD 61                by comp2     b.0xFFFFFFFFFFFFFFE5,$0x61
080022B6: D8 1A                         if << go     $0x1A
080022B8: 2D C1 E5 CD 7A                by comp2     b.0xFFFFFFFFFFFFFFE5,$0x7A
080022BD: D4 13                         if >> go     $0x13
080022BF: 05 C1 E5                      by2 :=       b.0xFFFFFFFFFFFFFFE5
080022C2: FC 3D CD 61                   by2 -        $0x61
080022C6: FC 35 CD 41                   by2 +        $0x41
080022CA: 04 D1                         by1 :=       r2
080022CC: C0 09                         go           $0x9
080022CE: C0 07                         go           $0x7
080022D0: 04 C1 E5                      by1 :=       b.0xFFFFFFFFFFFFFFE5
080022D3: C0 02                         go           $0x2
080022D5: 1C C1 E5                      by1 =:       b.0xFFFFFFFFFFFFFFE5
080022D8: FE 03                         clrk
080022DA: B4 78                         jumpg        b.0xE0
080022DC: B8 CF 00 00 01 08             ents         $0x108
080022E2: C0 24                         go           $0x24
080022E4: 9C                            entd
080022E5: FD C0 7A                      l=:          b.0xE8
080022E8: 20 43                         w1 =:        b.0xC
080022EA: 44 4E                         w test       b.0x38
080022EC: C4 13                         if = go      $0x13
080022EE: 18 42                         r:=          b.0x8
080022F0: 1A 4E 85                      w move       b.0x38,r.0x14
080022F3: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
080022F9: 9D                            ifkret
080022FA: 18 42                         r:=          b.0x8
080022FC: 1A 85 4E                      w move       r.0x14,b.0x38
080022FF: 0C 43                         w1 :=        b.0xC
08002301: 81                            retk
08002302: FE 03                         clrk
08002304: B4 7A                         jumpg        b.0xE8
08002306: 4A 4E                         w stz        b.0x38
08002308: 18 45                         r:=          b.0x14
0800230A: 0D 85                         w2 :=        r.0x14
0800230C: 18 42                         r:=          b.0x8
0800230E: 21 85                         w2 =:        r.0x14
08002310: FD 3E C1 57                   w3 laddr     b.0x57
08002314: 22 86                         w3 =:        r.0x18
08002316: 4A 87                         w stz        r.0x1C
08002318: 1A CD 63 88                   w move       $0x63,r.0x20
0800231C: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08002322: D2 08                         if -k go     $0x8
08002324: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800232A: 44 C4 08 00 79 F8             w test       $0x80079F8
08002330: C6 10                         if >< go     $0x10
08002332: C3 08 00 22 1E 00             call         $0x800221E,$0x0
08002338: D2 08                         if -k go     $0x8
0800233A: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002340: 4A 4B                         w stz        b.0x2C
08002342: 0C 4B                         w1 :=        b.0x2C
08002344: 2D D4 57 CD 29                by comp2     b.0x57+,$0x29
08002349: C4 06                         if = go      $0x6
0800234B: 4F 4B                         w incr       b.0x2C
0800234D: C0 F5                         go           $0xFFFFFFFFFFFFFFF5
0800234F: 1A D0 7D                      w move       r1,b.0xF4
08002352: 4A 7C                         w stz        b.0xF0
08002354: FD 3D C1 57                   w2 laddr     b.0x57
08002358: 21 7B                         w2 =:        b.0xEC
0800235A: 18 42                         r:=          b.0x8
0800235C: FD 20 7B 85 0C                by bmove     b.0xEC,r.0x14,$0xC
08002361: C3 08 00 87 DD 00             call         $0x80087DD,$0x0
08002367: D2 08                         if -k go     $0x8
08002369: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800236F: 18 42                         r:=          b.0x8
08002371: 1A 88 51                      w move       r.0x20,b.0x44
08002374: FD 3D C1 57                   w2 laddr     b.0x57
08002378: 21 85                         w2 =:        r.0x14
0800237A: 4A 86                         w stz        r.0x18
0800237C: 1A CD 63 87                   w move       $0x63,r.0x1C
08002380: 19 CD 52 88                   by move      $0x52,r.0x20
08002384: C3 08 00 AF 76 00             call         $0x800AF76,$0x0
0800238A: D2 08                         if -k go     $0x8
0800238C: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002392: 20 4E                         w1 =:        b.0x38
08002394: 4D 48                         w set1       b.0x20
08002396: 4A 4D                         w stz        b.0x34
08002398: C3 08 00 22 93 00             call         $0x8002293,$0x0
0800239E: D2 08                         if -k go     $0x8
080023A0: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080023A6: 1C 46                         by1 =:       b.0x18
080023A8: 0D 4D                         w2 :=        b.0x34
080023AA: B4 E1 08 00 2B 34             jumpg        $0x8002B34+
080023B0: 2D 46 CD 24                   by comp2     b.0x18,$0x24
080023B4: C6 4B                         if >< go     $0x4B
080023B6: FD 20 CD 20 54 07             by bmove     $0x20,b.0x50,$0x7
080023BC: 4A 47                         w stz        b.0x1C
080023BE: C3 08 00 22 93 00             call         $0x8002293,$0x0
080023C4: D2 08                         if -k go     $0x8
080023C6: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080023CC: 1C 46                         by1 =:       b.0x18
080023CE: 30 CD 41                      by1 comp     $0x41
080023D1: D8 10                         if << go     $0x10
080023D3: 30 CD 5A                      by1 comp     $0x5A
080023D6: D4 0B                         if >> go     $0xB
080023D8: 0D 47                         w2 :=        b.0x1C
080023DA: 1C D5 50                      by1 =:       b.0x50+
080023DD: BF 47 06 E1                   d loopi      b.0x1C,$0x6,$0xFFFFFFFFFFFFFFE1
080023E1: FD 3C 54                      w1 laddr     b.0x50
080023E4: 20 7F                         w1 =:        b.0xFC
080023E6: 1A 07 7E                      w move       $0x7,b.0xF8
080023E9: 84                            bi1 clr
080023EA: 85                            bi2 clr
080023EB: FD BE 7E C4 08 00 2A EC 00    by scopa     b.0xF8,$0x8002AEC,$0x0
080023F4: C6 06                         if >< go     $0x6
080023F6: 4D 4D                         w set1       b.0x34
080023F8: C0 05                         go           $0x5
080023FA: 1A 03 4D                      w move       $0x3,b.0x34
080023FD: C0 0B                         go           $0xB
080023FF: 2D 46 CD 20                   by comp2     b.0x18,$0x20
08002403: C4 05                         if = go      $0x5
08002405: 1A 03 4D                      w move       $0x3,b.0x34
08002408: C1 02 9E                      go           $0x29E
0800240B: 2D 46 CD 41                   by comp2     b.0x18,$0x41
0800240F: D8 08                         if << go     $0x8
08002411: 2D 46 CD 5A                   by comp2     b.0x18,$0x5A
08002415: DA 38                         if <<= go    $0x38
08002417: 2D 46 CD 2D                   by comp2     b.0x18,$0x2D
0800241B: C4 32                         if = go      $0x32
0800241D: 2D 46 CD 30                   by comp2     b.0x18,$0x30
08002421: D8 08                         if << go     $0x8
08002423: 2D 46 CD 39                   by comp2     b.0x18,$0x39
08002427: DA 26                         if <<= go    $0x26
08002429: 2D 46 CD 28                   by comp2     b.0x18,$0x28
0800242D: C4 20                         if = go      $0x20
0800242F: 2D 46 CD 29                   by comp2     b.0x18,$0x29
08002433: C4 1A                         if = go      $0x1A
08002435: 2D 46 CD 3A                   by comp2     b.0x18,$0x3A
08002439: C4 14                         if = go      $0x14
0800243B: 2D 46 CD 3B                   by comp2     b.0x18,$0x3B
0800243F: C4 0E                         if = go      $0xE
08002441: 2D 46 CD 2E                   by comp2     b.0x18,$0x2E
08002445: C4 08                         if = go      $0x8
08002447: 2D 46 CD 22                   by comp2     b.0x18,$0x22
0800244B: C6 27                         if >< go     $0x27
0800244D: C3 08 00 86 91 00             call         $0x8008691,$0x0
08002453: D2 08                         if -k go     $0x8
08002455: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800245B: 20 4F                         w1 =:        b.0x3C
0800245D: 20 50                         w1 =:        b.0x40
0800245F: 85                            bi2 clr
08002460: 21 4A                         w2 =:        b.0x28
08002462: 06 46                         by3 :=       b.0x18
08002464: FD 3F F4 00                   w4 laddr     r1.(0x0)
08002468: 57 D1                         w4 +         r2
0800246A: 1E F7 00                      by3 =:       r4.(0x0)
0800246D: 1A 02 4D                      w move       $0x2,b.0x34
08002470: C0 0B                         go           $0xB
08002472: 2D 46 CD 20                   by comp2     b.0x18,$0x20
08002476: C4 05                         if = go      $0x5
08002478: 1A 03 4D                      w move       $0x3,b.0x34
0800247B: C1 02 2B                      go           $0x22B
0800247E: 2E 4A 13                      w comp2      b.0x28,$0x13
08002481: C6 1A                         if >< go     $0x1A
08002483: C3 08 00 86 91 00             call         $0x8008691,$0x0
08002489: D2 08                         if -k go     $0x8
0800248B: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002491: 18 50                         r:=          b.0x40
08002493: 20 85                         w1 =:        r.0x14
08002495: 1A 3F 4A                      w move       $0x3F,b.0x28
08002498: 1A 85 50                      w move       r.0x14,b.0x40
0800249B: 2D 46 CD 41                   by comp2     b.0x18,$0x41
0800249F: D8 08                         if << go     $0x8
080024A1: 2D 46 CD 5A                   by comp2     b.0x18,$0x5A
080024A5: DA 38                         if <<= go    $0x38
080024A7: 2D 46 CD 2D                   by comp2     b.0x18,$0x2D
080024AB: C4 32                         if = go      $0x32
080024AD: 2D 46 CD 30                   by comp2     b.0x18,$0x30
080024B1: D8 08                         if << go     $0x8
080024B3: 2D 46 CD 39                   by comp2     b.0x18,$0x39
080024B7: DA 26                         if <<= go    $0x26
080024B9: 2D 46 CD 28                   by comp2     b.0x18,$0x28
080024BD: C4 20                         if = go      $0x20
080024BF: 2D 46 CD 29                   by comp2     b.0x18,$0x29
080024C3: C4 1A                         if = go      $0x1A
080024C5: 2D 46 CD 3A                   by comp2     b.0x18,$0x3A
080024C9: C4 14                         if = go      $0x14
080024CB: 2D 46 CD 3B                   by comp2     b.0x18,$0x3B
080024CF: C4 0E                         if = go      $0xE
080024D1: 2D 46 CD 2E                   by comp2     b.0x18,$0x2E
080024D5: C4 08                         if = go      $0x8
080024D7: 2D 46 CD 22                   by comp2     b.0x18,$0x22
080024DB: C6 16                         if >< go     $0x16
080024DD: 0D 4A                         w2 :=        b.0x28
080024DF: 55 01                         w2 +         $0x1
080024E1: 21 4A                         w2 =:        b.0x28
080024E3: 06 46                         by3 :=       b.0x18
080024E5: FD 3F C5 40                   w4 laddr     @b.0x40
080024E9: 57 D1                         w4 +         r2
080024EB: 1E F7 00                      by3 =:       r4.(0x0)
080024EE: C1 01 AD                      go           $0x1AD
080024F1: 0D 4A                         w2 :=        b.0x28
080024F3: 55 01                         w2 +         $0x1
080024F5: 21 4A                         w2 =:        b.0x28
080024F7: 06 0D                         by3 :=       $0xD
080024F9: FD 3C C5 40                   w1 laddr     @b.0x40
080024FD: 54 D1                         w1 +         r2
080024FF: 1E F4 00                      by3 =:       r1.(0x0)
08002502: 18 42                         r:=          b.0x8
08002504: 1A 4F 85                      w move       b.0x3C,r.0x14
08002507: C3 08 00 1C B6 00             call         $0x8001CB6,$0x0
0800250D: D2 08                         if -k go     $0x8
0800250F: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002515: 44 D0                         w test       r1
08002517: C6 1D                         if >< go     $0x1D
08002519: 18 42                         r:=          b.0x8
0800251B: 1A 51 85                      w move       b.0x44,r.0x14
0800251E: 1A 4F 86                      w move       b.0x3C,r.0x18
08002521: C3 08 00 19 64 00             call         $0x8001964,$0x0
08002527: D2 08                         if -k go     $0x8
08002529: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800252F: 18 42                         r:=          b.0x8
08002531: 1A 86 4F                      w move       r.0x18,b.0x3C
08002534: 18 42                         r:=          b.0x8
08002536: 1A 4F 85                      w move       b.0x3C,r.0x14
08002539: FE 79 C4 08 00 2A F8 86 03    w bmove      $0x8002AF8,r.0x18,$0x3
08002542: 4A 89                         w stz        r.0x24
08002544: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
0800254A: D2 08                         if -k go     $0x8
0800254C: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002552: 18 42                         r:=          b.0x8
08002554: 1A 85 4F                      w move       r.0x14,b.0x3C
08002557: 20 49                         w1 =:        b.0x24
08002559: 44 D0                         w test       r1
0800255B: C6 05                         if >< go     $0x5
0800255D: C1 00 B1                      go           $0xB1
08002560: 1A 4F 85                      w move       b.0x3C,r.0x14
08002563: FE 79 C4 08 00 2B 08 86 03    w bmove      $0x8002B08,r.0x18,$0x3
0800256C: 4A 89                         w stz        r.0x24
0800256E: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
08002574: D2 08                         if -k go     $0x8
08002576: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800257C: 18 42                         r:=          b.0x8
0800257E: 1A 85 4F                      w move       r.0x14,b.0x3C
08002581: 20 49                         w1 =:        b.0x24
08002583: 44 D0                         w test       r1
08002585: C6 05                         if >< go     $0x5
08002587: C1 00 87                      go           $0x87
0800258A: 1A 4F 85                      w move       b.0x3C,r.0x14
0800258D: FE 79 C4 08 00 2B 18 86 03    w bmove      $0x8002B18,r.0x18,$0x3
08002596: 4A 89                         w stz        r.0x24
08002598: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
0800259E: D2 08                         if -k go     $0x8
080025A0: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080025A6: 18 42                         r:=          b.0x8
080025A8: 1A 85 4F                      w move       r.0x14,b.0x3C
080025AB: 20 49                         w1 =:        b.0x24
080025AD: 44 D0                         w test       r1
080025AF: C6 04                         if >< go     $0x4
080025B1: C0 5D                         go           $0x5D
080025B3: 1A 4F 85                      w move       b.0x3C,r.0x14
080025B6: FE 79 C4 08 00 2B 28 86 03    w bmove      $0x8002B28,r.0x18,$0x3
080025BF: 4A 89                         w stz        r.0x24
080025C1: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
080025C7: D2 08                         if -k go     $0x8
080025C9: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080025CF: 18 42                         r:=          b.0x8
080025D1: 1A 85 4F                      w move       r.0x14,b.0x3C
080025D4: 20 49                         w1 =:        b.0x24
080025D6: 44 D0                         w test       r1
080025D8: C6 04                         if >< go     $0x4
080025DA: C0 34                         go           $0x34
080025DC: 44 C4 08 00 79 F8             w test       $0x80079F8
080025E2: C6 10                         if >< go     $0x10
080025E4: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
080025EA: D2 08                         if -k go     $0x8
080025EC: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080025F2: 18 42                         r:=          b.0x8
080025F4: 1A 4E 85                      w move       b.0x38,r.0x14
080025F7: 1A 48 86                      w move       b.0x20,r.0x18
080025FA: 1A 4F 87                      w move       b.0x3C,r.0x1C
080025FD: 1A 49 88                      w move       b.0x24,r.0x20
08002600: C3 08 00 19 D9 00             call         $0x80019D9,$0x0
08002606: D2 08                         if -k go     $0x8
08002608: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800260E: 44 49                         w test       b.0x24
08002610: C4 1C                         if = go      $0x1C
08002612: 18 42                         r:=          b.0x8
08002614: 1A 4F 85                      w move       b.0x3C,r.0x14
08002617: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800261D: D2 08                         if -k go     $0x8
0800261F: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002625: 18 42                         r:=          b.0x8
08002627: 1A 85 4F                      w move       r.0x14,b.0x3C
0800262A: C0 6E                         go           $0x6E
0800262C: 18 42                         r:=          b.0x8
0800262E: 1A 4F 85                      w move       b.0x3C,r.0x14
08002631: C3 08 00 1B 4C 00             call         $0x8001B4C,$0x0
08002637: D2 08                         if -k go     $0x8
08002639: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800263F: 18 42                         r:=          b.0x8
08002641: 1A 86 4C                      w move       r.0x18,b.0x30
08002644: 20 52                         w1 =:        b.0x48
08002646: 44 D0                         w test       r1
08002648: C6 2A                         if >< go     $0x2A
0800264A: 1A 4F 85                      w move       b.0x3C,r.0x14
0800264D: 1A 4C 86                      w move       b.0x30,r.0x18
08002650: C3 08 00 1B F7 00             call         $0x8001BF7,$0x0
08002656: D2 08                         if -k go     $0x8
08002658: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
0800265E: 20 52                         w1 =:        b.0x48
08002660: 18 42                         r:=          b.0x8
08002662: 20 85                         w1 =:        r.0x14
08002664: C3 08 00 22 DC 00             call         $0x80022DC,$0x0
0800266A: D2 08                         if -k go     $0x8
0800266C: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002672: C3 08 00 16 E1 00             call         $0x80016E1,$0x0
08002678: D2 08                         if -k go     $0x8
0800267A: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
08002680: 20 53                         w1 =:        b.0x4C
08002682: 1A 52 F4 00                   w move       b.0x48,r1.(0x0)
08002686: 18 45                         r:=          b.0x14
08002688: FD 3D 86                      w2 laddr     r.0x18
0800268B: 0C 53                         w1 :=        b.0x4C
0800268D: 0E 04                         w3 :=        $0x4
0800268F: FE 03                         clrk
08002691: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002697: 9D                            ifkret
08002698: 1A 03 4D                      w move       $0x3,b.0x34
0800269B: C0 0B                         go           $0xB
0800269D: 2D 46 0A                      by comp2     b.0x18,$0xA
080026A0: C6 04                         if >< go     $0x4
080026A2: 4A 4D                         w stz        b.0x34
080026A4: C0 02                         go           $0x2
080026A6: 2D 46 17                      by comp2     b.0x18,$0x17
080026A9: C4 05                         if = go      $0x5
080026AB: C1 FC ED                      go           $0xFFFFFFFFFFFFFCED
080026AE: 18 42                         r:=          b.0x8
080026B0: 1A 4E 85                      w move       b.0x38,r.0x14
080026B3: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
080026B9: D2 08                         if -k go     $0x8
080026BB: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080026C1: 18 42                         r:=          b.0x8
080026C3: 1A 85 4E                      w move       r.0x14,b.0x38
080026C6: 1A 51 85                      w move       b.0x44,r.0x14
080026C9: C3 08 00 87 17 00             call         $0x8008717,$0x0
080026CF: D2 08                         if -k go     $0x8
080026D1: C3 08 00 22 E4 00             call         $0x80022E4,$0x0
080026D7: 18 42                         r:=          b.0x8
080026D9: 1A 85 51                      w move       r.0x14,b.0x44
080026DC: 80                            ret
080026DD: 9C                            entd
080026DE: FD C0 6B                      l=:          b.0xAC
080026E1: 18 42                         r:=          b.0x8
080026E3: 1A 4A 85                      w move       b.0x28,r.0x14
080026E6: C3 08 00 B4 3F 00             call         $0x800B43F,$0x0
080026EC: D2 04                         if -k go     $0x4
080026EE: B4 6B                         jumpg        b.0xAC
080026F0: 1C C1 B1                      by1 =:       b.0xFFFFFFFFFFFFFFB1
080026F3: 30 0A                         by1 comp     $0xA
080026F5: C6 06                         if >< go     $0x6
080026F7: 4F 47                         w incr       b.0x1C
080026F9: C0 29                         go           $0x29
080026FB: 2D C1 B1 CD 61                by comp2     b.0xFFFFFFFFFFFFFFB1,$0x61
08002700: D8 1A                         if << go     $0x1A
08002702: 2D C1 B1 CD 7A                by comp2     b.0xFFFFFFFFFFFFFFB1,$0x7A
08002707: D4 13                         if >> go     $0x13
08002709: 05 C1 B1                      by2 :=       b.0xFFFFFFFFFFFFFFB1
0800270C: FC 3D CD 61                   by2 -        $0x61
08002710: FC 35 CD 41                   by2 +        $0x41
08002714: 04 D1                         by1 :=       r2
08002716: C0 09                         go           $0x9
08002718: C0 07                         go           $0x7
0800271A: 04 C1 B1                      by1 :=       b.0xFFFFFFFFFFFFFFB1
0800271D: C0 02                         go           $0x2
0800271F: 1C C1 B1                      by1 =:       b.0xFFFFFFFFFFFFFFB1
08002722: FE 03                         clrk
08002724: B4 6B                         jumpg        b.0xAC
08002726: 9C                            entd
08002727: FD C0 66                      l=:          b.0x98
0800272A: C3 08 00 26 DD 00             call         $0x80026DD,$0x0
08002730: D2 04                         if -k go     $0x4
08002732: B4 66                         jumpg        b.0x98
08002734: 1C 68                         by1 =:       b.0xA0
08002736: 30 17                         by1 comp     $0x17
08002738: C6 07                         if >< go     $0x7
0800273A: 84                            bi1 clr
0800273B: FE 03                         clrk
0800273D: B4 66                         jumpg        b.0x98
0800273F: 2D 68 CD 41                   by comp2     b.0xA0,$0x41
08002743: D8 08                         if << go     $0x8
08002745: 2D 68 CD 5A                   by comp2     b.0xA0,$0x5A
08002749: DA 3A                         if <<= go    $0x3A
0800274B: 2D 68 CD 2D                   by comp2     b.0xA0,$0x2D
0800274F: C4 34                         if = go      $0x34
08002751: 2D 68 CD 30                   by comp2     b.0xA0,$0x30
08002755: D8 08                         if << go     $0x8
08002757: 2D 68 CD 39                   by comp2     b.0xA0,$0x39
0800275B: DA 28                         if <<= go    $0x28
0800275D: 2D 68 CD 28                   by comp2     b.0xA0,$0x28
08002761: C4 22                         if = go      $0x22
08002763: 2D 68 CD 29                   by comp2     b.0xA0,$0x29
08002767: C4 1C                         if = go      $0x1C
08002769: 2D 68 CD 3A                   by comp2     b.0xA0,$0x3A
0800276D: C4 16                         if = go      $0x16
0800276F: 2D 68 CD 3B                   by comp2     b.0xA0,$0x3B
08002773: C4 10                         if = go      $0x10
08002775: 2D 68 CD 2E                   by comp2     b.0xA0,$0x2E
08002779: C4 0A                         if = go      $0xA
0800277B: 2D 68 CD 22                   by comp2     b.0xA0,$0x22
0800277F: C4 04                         if = go      $0x4
08002781: C0 A9                         go           $0xFFFFFFFFFFFFFFA9
08002783: C3 08 00 86 91 00             call         $0x8008691,$0x0
08002789: D2 04                         if -k go     $0x4
0800278B: B4 66                         jumpg        b.0x98
0800278D: 20 4B                         w1 =:        b.0x2C
0800278F: 20 6A                         w1 =:        b.0xA8
08002791: 1A 3F 69                      w move       $0x3F,b.0xA4
08002794: 2E 69 13                      w comp2      b.0xA4,$0x13
08002797: C6 16                         if >< go     $0x16
08002799: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800279F: D2 04                         if -k go     $0x4
080027A1: B4 66                         jumpg        b.0x98
080027A3: 18 6A                         r:=          b.0xA8
080027A5: 20 85                         w1 =:        r.0x14
080027A7: 1A 3F 69                      w move       $0x3F,b.0xA4
080027AA: 1A 85 6A                      w move       r.0x14,b.0xA8
080027AD: 0D 69                         w2 :=        b.0xA4
080027AF: 55 01                         w2 +         $0x1
080027B1: 21 69                         w2 =:        b.0xA4
080027B3: 06 68                         by3 :=       b.0xA0
080027B5: FD 3F C5 A8                   w4 laddr     @b.0xFFFFFFFFFFFFFFA8
080027B9: 57 D1                         w4 +         r2
080027BB: 1E F7 00                      by3 =:       r4.(0x0)
080027BE: C3 08 00 26 DD 00             call         $0x80026DD,$0x0
080027C4: D2 04                         if -k go     $0x4
080027C6: B4 66                         jumpg        b.0x98
080027C8: 1C 68                         by1 =:       b.0xA0
080027CA: 30 17                         by1 comp     $0x17
080027CC: C6 07                         if >< go     $0x7
080027CE: 84                            bi1 clr
080027CF: FE 03                         clrk
080027D1: B4 66                         jumpg        b.0x98
080027D3: 2D 68 CD 41                   by comp2     b.0xA0,$0x41
080027D7: D8 08                         if << go     $0x8
080027D9: 2D 68 CD 5A                   by comp2     b.0xA0,$0x5A
080027DD: DA 38                         if <<= go    $0x38
080027DF: 2D 68 CD 2D                   by comp2     b.0xA0,$0x2D
080027E3: C4 32                         if = go      $0x32
080027E5: 2D 68 CD 30                   by comp2     b.0xA0,$0x30
080027E9: D8 08                         if << go     $0x8
080027EB: 2D 68 CD 39                   by comp2     b.0xA0,$0x39
080027EF: DA 26                         if <<= go    $0x26
080027F1: 2D 68 CD 28                   by comp2     b.0xA0,$0x28
080027F5: C4 20                         if = go      $0x20
080027F7: 2D 68 CD 29                   by comp2     b.0xA0,$0x29
080027FB: C4 1A                         if = go      $0x1A
080027FD: 2D 68 CD 3A                   by comp2     b.0xA0,$0x3A
08002801: C4 14                         if = go      $0x14
08002803: 2D 68 CD 3B                   by comp2     b.0xA0,$0x3B
08002807: C4 0E                         if = go      $0xE
08002809: 2D 68 CD 2E                   by comp2     b.0xA0,$0x2E
0800280D: C4 08                         if = go      $0x8
0800280F: 2D 68 CD 22                   by comp2     b.0xA0,$0x22
08002813: C6 05                         if >< go     $0x5
08002815: C1 FF 7F                      go           $0xFFFFFFFFFFFFFF7F
08002818: 2E 69 13                      w comp2      b.0xA4,$0x13
0800281B: C6 16                         if >< go     $0x16
0800281D: C3 08 00 86 91 00             call         $0x8008691,$0x0
08002823: D2 04                         if -k go     $0x4
08002825: B4 66                         jumpg        b.0x98
08002827: 18 6A                         r:=          b.0xA8
08002829: 20 85                         w1 =:        r.0x14
0800282B: 1A 3F 69                      w move       $0x3F,b.0xA4
0800282E: 1A 85 6A                      w move       r.0x14,b.0xA8
08002831: 0D 69                         w2 :=        b.0xA4
08002833: 55 01                         w2 +         $0x1
08002835: 06 0D                         by3 :=       $0xD
08002837: FD 3F C5 A8                   w4 laddr     @b.0xFFFFFFFFFFFFFFA8
0800283B: 57 D1                         w4 +         r2
0800283D: 1E F7 00                      by3 =:       r4.(0x0)
08002840: 0C 01                         w1 :=        $0x1
08002842: FE 03                         clrk
08002844: B4 66                         jumpg        b.0x98
08002846: B8 CF 00 00 00 B8             ents         $0xB8
0800284C: C0 24                         go           $0x24
0800284E: 9C                            entd
0800284F: FD C0 6D                      l=:          b.0xB4
08002852: 20 43                         w1 =:        b.0xC
08002854: 44 4A                         w test       b.0x28
08002856: C4 13                         if = go      $0x13
08002858: 18 42                         r:=          b.0x8
0800285A: 1A 4A 85                      w move       b.0x28,r.0x14
0800285D: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
08002863: 9D                            ifkret
08002864: 18 42                         r:=          b.0x8
08002866: 1A 85 4A                      w move       r.0x14,b.0x28
08002869: 0C 43                         w1 :=        b.0xC
0800286B: 81                            retk
0800286C: FE 03                         clrk
0800286E: B4 6D                         jumpg        b.0xB4
08002870: 4A 4A                         w stz        b.0x28
08002872: 18 42                         r:=          b.0x8
08002874: 1A 45 85                      w move       b.0x14,r.0x14
08002877: FD 3D 4D                      w2 laddr     b.0x34
0800287A: 21 86                         w2 =:        r.0x18
0800287C: 4A 87                         w stz        r.0x1C
0800287E: 1A CD 63 88                   w move       $0x63,r.0x20
08002882: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08002888: D2 08                         if -k go     $0x8
0800288A: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002890: FD 3D 4D                      w2 laddr     b.0x34
08002893: 18 42                         r:=          b.0x8
08002895: 21 85                         w2 =:        r.0x14
08002897: 4A 86                         w stz        r.0x18
08002899: 1A CD 63 87                   w move       $0x63,r.0x1C
0800289D: 19 CD 52 88                   by move      $0x52,r.0x20
080028A1: C3 08 00 AF 76 00             call         $0x800AF76,$0x0
080028A7: D2 08                         if -k go     $0x8
080028A9: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080028AF: 20 4A                         w1 =:        b.0x28
080028B1: 4D 47                         w set1       b.0x1C
080028B3: C3 08 00 27 26 00             call         $0x8002726,$0x0
080028B9: D2 08                         if -k go     $0x8
080028BB: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080028C1: 44 D0                         w test       r1
080028C3: C5 01 A3                      if = go      $0x1A3
080028C6: 18 42                         r:=          b.0x8
080028C8: 1A 4B 85                      w move       b.0x2C,r.0x14
080028CB: C3 08 00 1C B6 00             call         $0x8001CB6,$0x0
080028D1: D2 08                         if -k go     $0x8
080028D3: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080028D9: 44 D0                         w test       r1
080028DB: C6 21                         if >< go     $0x21
080028DD: 18 42                         r:=          b.0x8
080028DF: 1A C4 08 00 29 50 85          w move       $0x8002950,r.0x14
080028E6: 1A 4B 86                      w move       b.0x2C,r.0x18
080028E9: C3 08 00 19 64 00             call         $0x8001964,$0x0
080028EF: D2 08                         if -k go     $0x8
080028F1: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080028F7: 18 42                         r:=          b.0x8
080028F9: 1A 86 4B                      w move       r.0x18,b.0x2C
080028FC: 18 42                         r:=          b.0x8
080028FE: 1A 4B 85                      w move       b.0x2C,r.0x14
08002901: FE 79 C4 08 00 2B 48 86 03    w bmove      $0x8002B48,r.0x18,$0x3
0800290A: 4A 89                         w stz        r.0x24
0800290C: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
08002912: D2 08                         if -k go     $0x8
08002914: C3 08 00 28 4E 00             call         $0x800284E,$0x0
0800291A: 18 42                         r:=          b.0x8
0800291C: 1A 85 4B                      w move       r.0x14,b.0x2C
0800291F: 20 48                         w1 =:        b.0x20
08002921: 44 D0                         w test       r1
08002923: C6 05                         if >< go     $0x5
08002925: C1 00 B1                      go           $0xB1
08002928: 1A 4B 85                      w move       b.0x2C,r.0x14
0800292B: FE 79 C4 08 00 2B 58 86 03    w bmove      $0x8002B58,r.0x18,$0x3
08002934: 4A 89                         w stz        r.0x24
08002936: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
0800293C: D2 08                         if -k go     $0x8
0800293E: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002944: 18 42                         r:=          b.0x8
08002946: 1A 85 4B                      w move       r.0x14,b.0x2C
08002949: 20 48                         w1 =:        b.0x20
0800294B: 44 D0                         w test       r1
0800294D: C6 05                         if >< go     $0x5
0800294F: C1 00 87                      go           $0x87
08002952: 1A 4B 85                      w move       b.0x2C,r.0x14
08002955: FE 79 C4 08 00 2B 68 86 03    w bmove      $0x8002B68,r.0x18,$0x3
0800295E: 4A 89                         w stz        r.0x24
08002960: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
08002966: D2 08                         if -k go     $0x8
08002968: C3 08 00 28 4E 00             call         $0x800284E,$0x0
0800296E: 18 42                         r:=          b.0x8
08002970: 1A 85 4B                      w move       r.0x14,b.0x2C
08002973: 20 48                         w1 =:        b.0x20
08002975: 44 D0                         w test       r1
08002977: C6 04                         if >< go     $0x4
08002979: C0 5D                         go           $0x5D
0800297B: 1A 4B 85                      w move       b.0x2C,r.0x14
0800297E: FE 79 C4 08 00 2B 78 86 03    w bmove      $0x8002B78,r.0x18,$0x3
08002987: 4A 89                         w stz        r.0x24
08002989: C3 08 00 1E 3E 00             call         $0x8001E3E,$0x0
0800298F: D2 08                         if -k go     $0x8
08002991: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002997: 18 42                         r:=          b.0x8
08002999: 1A 85 4B                      w move       r.0x14,b.0x2C
0800299C: 20 48                         w1 =:        b.0x20
0800299E: 44 D0                         w test       r1
080029A0: C6 04                         if >< go     $0x4
080029A2: C0 34                         go           $0x34
080029A4: 44 C4 08 00 79 F8             w test       $0x80079F8
080029AA: C6 10                         if >< go     $0x10
080029AC: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
080029B2: D2 08                         if -k go     $0x8
080029B4: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080029BA: 18 42                         r:=          b.0x8
080029BC: 1A 4A 85                      w move       b.0x28,r.0x14
080029BF: 1A 47 86                      w move       b.0x1C,r.0x18
080029C2: 1A 4B 87                      w move       b.0x2C,r.0x1C
080029C5: 1A 48 88                      w move       b.0x20,r.0x20
080029C8: C3 08 00 19 D9 00             call         $0x80019D9,$0x0
080029CE: D2 08                         if -k go     $0x8
080029D0: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080029D6: 44 48                         w test       b.0x20
080029D8: C4 1C                         if = go      $0x1C
080029DA: 18 42                         r:=          b.0x8
080029DC: 1A 4B 85                      w move       b.0x2C,r.0x14
080029DF: C3 08 00 87 17 00             call         $0x8008717,$0x0
080029E5: D2 08                         if -k go     $0x8
080029E7: C3 08 00 28 4E 00             call         $0x800284E,$0x0
080029ED: 18 42                         r:=          b.0x8
080029EF: 1A 85 4B                      w move       r.0x14,b.0x2C
080029F2: C0 71                         go           $0x71
080029F4: 18 42                         r:=          b.0x8
080029F6: 1A 4B 85                      w move       b.0x2C,r.0x14
080029F9: C3 08 00 1B 4C 00             call         $0x8001B4C,$0x0
080029FF: D2 08                         if -k go     $0x8
08002A01: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A07: 18 42                         r:=          b.0x8
08002A09: 1A 86 49                      w move       r.0x18,b.0x24
08002A0C: 20 4C                         w1 =:        b.0x30
08002A0E: 44 D0                         w test       r1
08002A10: C6 53                         if >< go     $0x53
08002A12: 1A 4B 85                      w move       b.0x2C,r.0x14
08002A15: 1A 49 86                      w move       b.0x24,r.0x18
08002A18: C3 08 00 1B F7 00             call         $0x8001BF7,$0x0
08002A1E: D2 08                         if -k go     $0x8
08002A20: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A26: 20 4C                         w1 =:        b.0x30
08002A28: FD 3D 46                      w2 laddr     b.0x18
08002A2B: 0C 4C                         w1 :=        b.0x30
08002A2D: 0E CD 20                      w3 :=        $0x20
08002A30: FE 03                         clrk
08002A32: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002A38: 9D                            ifkret
08002A39: 18 42                         r:=          b.0x8
08002A3B: 1A 4B 85                      w move       b.0x2C,r.0x14
08002A3E: C3 08 00 1C 7F 00             call         $0x8001C7F,$0x0
08002A44: D2 08                         if -k go     $0x8
08002A46: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A4C: 44 D0                         w test       r1
08002A4E: C6 15                         if >< go     $0x15
08002A50: 18 42                         r:=          b.0x8
08002A52: 1A 4C 85                      w move       b.0x30,r.0x14
08002A55: C3 08 00 22 DC 00             call         $0x80022DC,$0x0
08002A5B: D2 08                         if -k go     $0x8
08002A5D: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A63: C1 FE 50                      go           $0xFFFFFFFFFFFFFE50
08002A66: 18 42                         r:=          b.0x8
08002A68: 1A 4A 85                      w move       b.0x28,r.0x14
08002A6B: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
08002A71: D2 08                         if -k go     $0x8
08002A73: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A79: 18 42                         r:=          b.0x8
08002A7B: 1A 85 4A                      w move       r.0x14,b.0x28
08002A7E: 44 C4 08 00 79 F8             w test       $0x80079F8
08002A84: C6 10                         if >< go     $0x10
08002A86: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08002A8C: D2 08                         if -k go     $0x8
08002A8E: C3 08 00 28 4E 00             call         $0x800284E,$0x0
08002A94: 80                            ret
08002A95: B8 CF 00 00 00 1C             ents         $0x1C
08002A9B: C3 08 00 17 58 00             call         $0x8001758,$0x0
08002AA1: 9D                            ifkret
08002AA2: 20 46                         w1 =:        b.0x18
08002AA4: 1A 45 F4 14                   w move       b.0x14,r1.(0x14)
08002AA8: 0D F4 14                      w2 :=        r1.(0x14)
08002AAB: 18 42                         r:=          b.0x8
08002AAD: 21 85                         w2 =:        r.0x14
08002AAF: C3 08 00 1C 7F 00             call         $0x8001C7F,$0x0
08002AB5: 9D                            ifkret
08002AB6: 44 D0                         w test       r1
08002AB8: C6 1D                         if >< go     $0x1D
08002ABA: 18 42                         r:=          b.0x8
08002ABC: 1A 46 85                      w move       b.0x18,r.0x14
08002ABF: C3 08 00 22 DC 00             call         $0x80022DC,$0x0
08002AC5: 9D                            ifkret
08002AC6: 44 C4 08 00 79 F8             w test       $0x80079F8
08002ACC: C6 09                         if >< go     $0x9
08002ACE: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08002AD4: 9D                            ifkret
08002AD5: 80                            ret
08002AD6: B8 CF 00 00 00 88             ents         $0x88
08002ADC: C0 0D                         go           $0xD
08002ADE: 9C                            entd
08002ADF: FD C0 56                      l=:          b.0x58
08002AE2: 20 43                         w1 =:        b.0xC
08002AE4: 80                            ret
08002AE5: FE 03                         clrk
08002AE7: B4 56                         jumpg        b.0x58
08002AE9: 85                            bi2 clr
08002AEA: 21 C4 08 00 79 F0             w2 =:        $0x80079F0
08002AF0: 21 C4 08 00 79 F4             w2 =:        $0x80079F4
08002AF6: 18 42                         r:=          b.0x8
08002AF8: 1A C4 08 00 29 5C 85          w move       $0x800295C,r.0x14
08002AFF: C3 08 00 18 F1 00             call         $0x80018F1,$0x0
08002B05: D2 08                         if -k go     $0x8
08002B07: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002B0D: 18 42                         r:=          b.0x8
08002B0F: 1A 85 C4 08 00 29 5C          w move       r.0x14,$0x800295C
08002B16: 4A 57                         w stz        b.0x5C
08002B18: 2D 4B CD B3                   by comp2     b.0x2C,$0xB3
08002B1C: C6 04                         if >< go     $0x4
08002B1E: 4D 57                         w set1       b.0x5C
08002B20: 1A 57 C4 08 00 29 40          w move       b.0x5C,$0x8002940
08002B27: 4A 57                         w stz        b.0x5C
08002B29: 2D C1 2D CD B3                by comp2     b.0x2D,$0xB3
08002B2E: C6 04                         if >< go     $0x4
08002B30: 4D 57                         w set1       b.0x5C
08002B32: 1A 57 C4 08 00 29 44          w move       b.0x5C,$0x8002944
08002B39: 0D C4 08 00 7A 04             w2 :=        $0x8007A04
08002B3F: FC AD D1 38                   w sha        r2,$0x38
08002B43: 21 4F                         w2 =:        b.0x3C
08002B45: 0E C4 08 00 7A 04             w3 :=        $0x8007A04
08002B4B: E6 CE 00 FF                   w3 and       $0xFF
08002B4F: 22 50                         w3 =:        b.0x40
08002B51: 1A CE 00 8C 57                w move       $0x8C,b.0x5C
08002B56: FD 3F 52                      w4 laddr     b.0x48
08002B59: 23 58                         w4 =:        b.0x60
08002B5B: 4A 59                         w stz        b.0x64
08002B5D: 1A 0F 5A                      w move       $0xF,b.0x68
08002B60: 4A 5B                         w stz        b.0x6C
08002B62: FE 79 C4 08 00 2B 84 5C 03    w bmove      $0x8002B84,b.0x70,$0x3
08002B6B: C3 08 00 B9 7C 0A 57 C5 60 C5 64 C5 68 4F 50 5B C5 70 C5 74 C5 78 call         $0x800B97C,$0xA,b.0x5C,@b.0x60,@b.0x64,@b.0x68,b.0x3C,b.0x40,b.0x6C,@b.0x70,@b.0x74,@b.0x78
08002B81: D2 08                         if -k go     $0x8
08002B83: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002B89: 1A CE 00 A8 57                w move       $0xA8,b.0x5C
08002B8E: FD 3D 52                      w2 laddr     b.0x48
08002B91: 21 5F                         w2 =:        b.0x7C
08002B93: 4A 60                         w stz        b.0x80
08002B95: 1A 0F 61                      w move       $0xF,b.0x84
08002B98: C3 08 00 B9 7C 06 57 C5 7C C5 80 C5 84 C4 08 00 29 48 C4 08 00 29 4C call         $0x800B97C,$0x6,b.0x5C,@b.0x7C,@b.0xFFFFFFFFFFFFFF80,@b.0xFFFFFFFFFFFFFF84,$0x8002948,$0x800294C
08002BAF: D2 08                         if -k go     $0x8
08002BB1: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002BB7: 18 42                         r:=          b.0x8
08002BB9: FD 20 45 85 0C                by bmove     b.0x14,r.0x14,$0xC
08002BBE: 1A 51 88                      w move       b.0x44,r.0x20
08002BC1: C3 08 00 21 26 00             call         $0x8002126,$0x0
08002BC7: D2 08                         if -k go     $0x8
08002BC9: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002BCF: 18 42                         r:=          b.0x8
08002BD1: 1A 88 51                      w move       r.0x20,b.0x44
08002BD4: 20 4E                         w1 =:        b.0x38
08002BD6: 44 C4 08 00 29 50             w test       $0x8002950
08002BDC: C6 18                         if >< go     $0x18
08002BDE: 0C 51                         w1 :=        b.0x44
08002BE0: C3 08 00 1D 2A 00             call         $0x8001D2A,$0x0
08002BE6: D2 08                         if -k go     $0x8
08002BE8: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002BEE: 20 C4 08 00 29 50             w1 =:        $0x8002950
08002BF4: 18 42                         r:=          b.0x8
08002BF6: 1A 51 85                      w move       b.0x44,r.0x14
08002BF9: C3 08 00 1C B6 00             call         $0x8001CB6,$0x0
08002BFF: D2 08                         if -k go     $0x8
08002C01: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C07: 44 D0                         w test       r1
08002C09: C6 21                         if >< go     $0x21
08002C0B: 18 42                         r:=          b.0x8
08002C0D: 1A C4 08 00 29 50 85          w move       $0x8002950,r.0x14
08002C14: 1A 51 86                      w move       b.0x44,r.0x18
08002C17: C3 08 00 19 64 00             call         $0x8001964,$0x0
08002C1D: D2 08                         if -k go     $0x8
08002C1F: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C25: 18 42                         r:=          b.0x8
08002C27: 1A 86 51                      w move       r.0x18,b.0x44
08002C2A: 44 4E                         w test       b.0x38
08002C2C: C4 1F                         if = go      $0x1F
08002C2E: 18 42                         r:=          b.0x8
08002C30: 1A 51 85                      w move       b.0x44,r.0x14
08002C33: 1A 4C 86                      w move       b.0x30,r.0x18
08002C36: C3 08 00 28 46 00             call         $0x8002846,$0x0
08002C3C: D2 08                         if -k go     $0x8
08002C3E: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C44: 18 42                         r:=          b.0x8
08002C46: 1A 86 4C                      w move       r.0x18,b.0x30
08002C49: C0 1D                         go           $0x1D
08002C4B: 18 42                         r:=          b.0x8
08002C4D: 1A 51 85                      w move       b.0x44,r.0x14
08002C50: 1A 4C 86                      w move       b.0x30,r.0x18
08002C53: C3 08 00 2A 95 00             call         $0x8002A95,$0x0
08002C59: D2 08                         if -k go     $0x8
08002C5B: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C61: 18 42                         r:=          b.0x8
08002C63: 1A 86 4C                      w move       r.0x18,b.0x30
08002C66: 1A 51 85                      w move       b.0x44,r.0x14
08002C69: FD 20 45 86 0C                by bmove     b.0x14,r.0x18,$0xC
08002C6E: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08002C74: D2 08                         if -k go     $0x8
08002C76: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C7C: 18 42                         r:=          b.0x8
08002C7E: 1A 51 85                      w move       b.0x44,r.0x14
08002C81: C3 08 00 87 17 00             call         $0x8008717,$0x0
08002C87: D2 08                         if -k go     $0x8
08002C89: C3 08 00 2A DE 00             call         $0x8002ADE,$0x0
08002C8F: 18 42                         r:=          b.0x8
08002C91: 1A 85 51                      w move       r.0x14,b.0x44
08002C94: 84                            bi1 clr
08002C95: 80                            ret
08002C96: B8 CF 00 00 00 3C             ents         $0x3C
08002C9C: 18 42                         r:=          b.0x8
08002C9E: 1A 47 85                      w move       b.0x1C,r.0x14
08002CA1: C3 08 00 16 AE 00             call         $0x80016AE,$0x0
08002CA7: 9D                            ifkret
08002CA8: 18 42                         r:=          b.0x8
08002CAA: 1A 85 47                      w move       r.0x14,b.0x1C
08002CAD: 4D 48                         w set1       b.0x20
08002CAF: 44 45                         w test       b.0x14
08002CB1: C5 01 10                      if = go      $0x110
08002CB4: 0C 45                         w1 :=        b.0x14
08002CB6: 20 4B                         w1 =:        b.0x2C
08002CB8: 1A F4 18 45                   w move       r1.(0x18),b.0x14
08002CBC: 4A F4 18                      w stz        r1.(0x18)
08002CBF: FD 3D F4 00                   w2 laddr     r1.(0x0)
08002CC3: 2D F5 00 CD 81                by comp2     r2.(0x0),$0x81
08002CC8: C6 17                         if >< go     $0x17
08002CCA: 18 42                         r:=          b.0x8
08002CCC: 20 85                         w1 =:        r.0x14
08002CCE: C3 08 00 87 17 00             call         $0x8008717,$0x0
08002CD4: 9D                            ifkret
08002CD5: 18 42                         r:=          b.0x8
08002CD7: 1A 85 4B                      w move       r.0x14,b.0x2C
08002CDA: 4A 48                         w stz        b.0x20
08002CDC: C1 00 E2                      go           $0xE2
08002CDF: 18 42                         r:=          b.0x8
08002CE1: 20 85                         w1 =:        r.0x14
08002CE3: C3 08 00 1B 4C 00             call         $0x8001B4C,$0x0
08002CE9: 9D                            ifkret
08002CEA: 18 42                         r:=          b.0x8
08002CEC: 1A 86 4A                      w move       r.0x18,b.0x28
08002CEF: 20 4C                         w1 =:        b.0x30
08002CF1: 44 D0                         w test       r1
08002CF3: C6 24                         if >< go     $0x24
08002CF5: 1A 4B 85                      w move       b.0x2C,r.0x14
08002CF8: 1A 4A 86                      w move       b.0x28,r.0x18
08002CFB: C3 08 00 1B F7 00             call         $0x8001BF7,$0x0
08002D01: 9D                            ifkret
08002D02: 20 4C                         w1 =:        b.0x30
08002D04: FD 3D 46                      w2 laddr     b.0x18
08002D07: 0C 4C                         w1 :=        b.0x30
08002D09: 0E CD 20                      w3 :=        $0x20
08002D0C: FE 03                         clrk
08002D0E: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002D14: 9D                            ifkret
08002D15: C0 11                         go           $0x11
08002D17: 1A 4B 85                      w move       b.0x2C,r.0x14
08002D1A: C3 08 00 87 17 00             call         $0x8008717,$0x0
08002D20: 9D                            ifkret
08002D21: 18 42                         r:=          b.0x8
08002D23: 1A 85 4B                      w move       r.0x14,b.0x2C
08002D26: 44 48                         w test       b.0x20
08002D28: C4 25                         if = go      $0x25
08002D2A: 18 4C                         r:=          b.0x30
08002D2C: 4A 82                         w stz        r.0x8
08002D2E: C3 08 00 16 6A 00             call         $0x800166A,$0x0
08002D34: 9D                            ifkret
08002D35: 20 4E                         w1 =:        b.0x38
08002D37: 1A 4C F4 00                   w move       b.0x30,r1.(0x0)
08002D3B: FD 3D 47                      w2 laddr     b.0x1C
08002D3E: 0C 4E                         w1 :=        b.0x38
08002D40: 0E 04                         w3 :=        $0x4
08002D42: FE 03                         clrk
08002D44: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002D4A: 9D                            ifkret
08002D4B: C0 73                         go           $0x73
08002D4D: 4D 49                         w set1       b.0x24
08002D4F: 1A 47 4E                      w move       b.0x1C,b.0x38
08002D52: 44 4E                         w test       b.0x38
08002D54: C4 62                         if = go      $0x62
08002D56: 18 4E                         r:=          b.0x38
08002D58: 2E 4C C5 38                   w comp2      b.0x30,@b.0x38
08002D5C: C6 31                         if >< go     $0x31
08002D5E: 18 42                         r:=          b.0x8
08002D60: FE 79 C4 08 00 2B 98 85 03    w bmove      $0x8002B98,r.0x14,$0x3
08002D69: 18 C5 38                      r:=          @b.0x38
08002D6C: 0C 85                         w1 :=        r.0x14
08002D6E: 18 42                         r:=          b.0x8
08002D70: 20 88                         w1 =:        r.0x20
08002D72: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
08002D78: 9D                            ifkret
08002D79: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08002D7F: 9D                            ifkret
08002D80: 0C 3C                         w1 :=        $0x3C
08002D82: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08002D88: 9D                            ifkret
08002D89: 4A 49                         w stz        b.0x24
08002D8B: C0 22                         go           $0x22
08002D8D: C3 08 00 16 E1 00             call         $0x80016E1,$0x0
08002D93: 9D                            ifkret
08002D94: 20 4D                         w1 =:        b.0x34
08002D96: 1A 4C F4 00                   w move       b.0x30,r1.(0x0)
08002D9A: 18 C5 38                      r:=          @b.0x38
08002D9D: FD 3D 86                      w2 laddr     r.0x18
08002DA0: 0C 4D                         w1 :=        b.0x34
08002DA2: 0E 04                         w3 :=        $0x4
08002DA4: FE 03                         clrk
08002DA6: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002DAC: 9D                            ifkret
08002DAD: 18 4E                         r:=          b.0x38
08002DAF: 1A 81 4E                      w move       r.0x4,b.0x38
08002DB2: 44 4E                         w test       b.0x38
08002DB4: C6 A2                         if >< go     $0xFFFFFFFFFFFFFFA2
08002DB6: 44 49                         w test       b.0x24
08002DB8: C4 06                         if = go      $0x6
08002DBA: 18 4C                         r:=          b.0x30
08002DBC: 4A 81                         w stz        r.0x4
08002DBE: C1 FE F1                      go           $0xFFFFFFFFFFFFFEF1
08002DC1: 80                            ret
08002DC2: 9C                            entd
08002DC3: FD C0 53                      l=:          b.0x4C
08002DC6: 1C 54                         by1 =:       b.0x50
08002DC8: 44 49                         w test       b.0x24
08002DCA: C4 07                         if = go      $0x7
08002DCC: 2E 4C 13                      w comp2      b.0x30,$0x13
08002DCF: C6 24                         if >< go     $0x24
08002DD1: C3 08 00 86 91 00             call         $0x8008691,$0x0
08002DD7: D2 04                         if -k go     $0x4
08002DD9: B4 53                         jumpg        b.0x4C
08002DDB: 20 4E                         w1 =:        b.0x38
08002DDD: 1A 3F 4C                      w move       $0x3F,b.0x30
08002DE0: FD 3D 49                      w2 laddr     b.0x24
08002DE3: 0C 4E                         w1 :=        b.0x38
08002DE5: 0E 14                         w3 :=        $0x14
08002DE7: FE 03                         clrk
08002DE9: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002DEF: D2 04                         if -k go     $0x4
08002DF1: B4 53                         jumpg        b.0x4C
08002DF3: 0C 4C                         w1 :=        b.0x30
08002DF5: 54 01                         w1 +         $0x1
08002DF7: 20 4C                         w1 =:        b.0x30
08002DF9: 05 54                         by2 :=       b.0x50
08002DFB: FD 3E C5 38                   w3 laddr     @b.0x38
08002DFF: 56 D0                         w3 +         r1
08002E01: 1D F6 00                      by2 =:       r3.(0x0)
08002E04: FE 03                         clrk
08002E06: B4 53                         jumpg        b.0x4C
08002E08: 9C                            entd
08002E09: FD C0 55                      l=:          b.0x54
08002E0C: 0C 57                         w1 :=        b.0x5C
08002E0E: 20 59                         w1 =:        b.0x64
08002E10: 0D 58                         w2 :=        b.0x60
08002E12: 21 5A                         w2 =:        b.0x68
08002E14: 34 D1                         w1 comp      r2
08002E16: C8 15                         if > go      $0x15
08002E18: 0D 59                         w2 :=        b.0x64
08002E1A: 04 E5 58                      by1 :=       @b.0x58+
08002E1D: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002E23: D2 04                         if -k go     $0x4
08002E25: B4 55                         jumpg        b.0x54
08002E27: BF 59 5A F1                   d loopi      b.0x64,b.0x68,$0xFFFFFFFFFFFFFFF1
08002E2B: FE 03                         clrk
08002E2D: B4 55                         jumpg        b.0x54
08002E2F: B8 CF 00 00 00 7C             ents         $0x7C
08002E35: 4A 4F                         w stz        b.0x3C
08002E37: 44 45                         w test       b.0x14
08002E39: C5 01 28                      if = go      $0x128
08002E3C: 18 42                         r:=          b.0x8
08002E3E: 19 CD AB 85                   by move      $0xAB,r.0x14
08002E42: 04 CD 96                      by1 :=       $0x96
08002E45: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08002E4B: 9D                            ifkret
08002E4C: 44 D0                         w test       r1
08002E4E: C4 18                         if = go      $0x18
08002E50: FD 3C 51                      w1 laddr     b.0x44
08002E53: 20 5C                         w1 =:        b.0x70
08002E55: 1A 05 5B                      w move       $0x5,b.0x6C
08002E58: CA 0C                         if < go      $0xC
08002E5A: 84                            bi1 clr
08002E5B: 85                            bi2 clr
08002E5C: FD 67 C4 08 00 2B AC 5B       by smove     $0x8002BAC,b.0x6C
08002E64: C0 16                         go           $0x16
08002E66: FD 3C 51                      w1 laddr     b.0x44
08002E69: 20 5C                         w1 =:        b.0x70
08002E6B: 1A 05 5B                      w move       $0x5,b.0x6C
08002E6E: CA 0C                         if < go      $0xC
08002E70: 84                            bi1 clr
08002E71: 85                            bi2 clr
08002E72: FD 67 C4 08 00 2B BC 5B       by smove     $0x8002BBC,b.0x6C
08002E7A: 4A 49                         w stz        b.0x24
08002E7C: 4A 4D                         w stz        b.0x34
08002E7E: 0C 47                         w1 :=        b.0x1C
08002E80: 20 4B                         w1 =:        b.0x2C
08002E82: 0D 48                         w2 :=        b.0x20
08002E84: 21 5D                         w2 =:        b.0x74
08002E86: 34 D1                         w1 comp      r2
08002E88: C9 00 A4                      if > go      $0xA4
08002E8B: 0D 4B                         w2 :=        b.0x2C
08002E8D: 04 E5 18                      by1 :=       @b.0x18+
08002E90: 1C 4A                         by1 =:       b.0x28
08002E92: 0E 4D                         w3 :=        b.0x34
08002E94: B4 E2 08 00 2B C4             jumpg        $0x8002BC4+
08002E9A: 2D 4A CD 28                   by comp2     b.0x28,$0x28
08002E9E: C6 0F                         if >< go     $0xF
08002EA0: 04 4A                         by1 :=       b.0x28
08002EA2: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002EA8: 9D                            ifkret
08002EA9: 4D 4D                         w set1       b.0x34
08002EAB: C0 4A                         go           $0x4A
08002EAD: 2D 4A CD 3A                   by comp2     b.0x28,$0x3A
08002EB1: C6 18                         if >< go     $0x18
08002EB3: FD 3D 51                      w2 laddr     b.0x44
08002EB6: 21 56                         w2 =:        b.0x58
08002EB8: 4A 57                         w stz        b.0x5C
08002EBA: 1A 04 58                      w move       $0x4,b.0x60
08002EBD: C3 08 00 2E 08 00             call         $0x8002E08,$0x0
08002EC3: 9D                            ifkret
08002EC4: 1A 02 4D                      w move       $0x2,b.0x34
08002EC7: C0 2E                         go           $0x2E
08002EC9: 2D 4A CD 3B                   by comp2     b.0x28,$0x3B
08002ECD: C6 1F                         if >< go     $0x1F
08002ECF: FD 3D 51                      w2 laddr     b.0x44
08002ED2: 21 56                         w2 =:        b.0x58
08002ED4: 4A 57                         w stz        b.0x5C
08002ED6: 1A 04 58                      w move       $0x4,b.0x60
08002ED9: C3 08 00 2E 08 00             call         $0x8002E08,$0x0
08002EDF: 9D                            ifkret
08002EE0: 04 CD 3B                      by1 :=       $0x3B
08002EE3: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002EE9: 9D                            ifkret
08002EEA: C0 0B                         go           $0xB
08002EEC: 04 4A                         by1 :=       b.0x28
08002EEE: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002EF4: 9D                            ifkret
08002EF5: C0 2D                         go           $0x2D
08002EF7: 04 4A                         by1 :=       b.0x28
08002EF9: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002EFF: 9D                            ifkret
08002F00: 2D 4A CD 29                   by comp2     b.0x28,$0x29
08002F04: C6 04                         if >< go     $0x4
08002F06: 4A 4D                         w stz        b.0x34
08002F08: C0 1A                         go           $0x1A
08002F0A: 2D 4A CD 3B                   by comp2     b.0x28,$0x3B
08002F0E: C4 07                         if = go      $0x7
08002F10: 2D 4A 0D                      by comp2     b.0x28,$0xD
08002F13: C6 0D                         if >< go     $0xD
08002F15: 04 4A                         by1 :=       b.0x28
08002F17: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002F1D: 9D                            ifkret
08002F1E: 4A 4D                         w stz        b.0x34
08002F20: C0 02                         go           $0x2
08002F22: 2D 4A 0D                      by comp2     b.0x28,$0xD
08002F25: C4 07                         if = go      $0x7
08002F27: E1 4B 5D FF                   loopi        b.0x2C,b.0x74,$0xFFFFFFFFFFFFFFFF
08002F2B: 64 FD 3D 4F 0C 49             f1 -         r2.(0x3D4F0C49)
08002F31: 0E 18                         w3 :=        $0x18
08002F33: FE 03                         clrk
08002F35: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002F3B: 9D                            ifkret
08002F3C: 4A 49                         w stz        b.0x24
08002F3E: 04 CD 81                      by1 :=       $0x81
08002F41: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002F47: 9D                            ifkret
08002F48: 04 0D                         by1 :=       $0xD
08002F4A: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002F50: 9D                            ifkret
08002F51: FD 3D 4F                      w2 laddr     b.0x3C
08002F54: 0C 49                         w1 :=        b.0x24
08002F56: 0E 18                         w3 :=        $0x18
08002F58: FE 03                         clrk
08002F5A: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08002F60: 9D                            ifkret
08002F61: 18 42                         r:=          b.0x8
08002F63: 19 CD AB 85                   by move      $0xAB,r.0x14
08002F67: 04 CD 96                      by1 :=       $0x96
08002F6A: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08002F70: 9D                            ifkret
08002F71: 44 D0                         w test       r1
08002F73: C4 18                         if = go      $0x18
08002F75: FD 3C 50                      w1 laddr     b.0x40
08002F78: 20 5C                         w1 =:        b.0x70
08002F7A: 1A 04 5B                      w move       $0x4,b.0x6C
08002F7D: CA 0C                         if < go      $0xC
08002F7F: 84                            bi1 clr
08002F80: 85                            bi2 clr
08002F81: FD 67 C4 08 00 2B D4 5B       by smove     $0x8002BD4,b.0x6C
08002F89: C0 16                         go           $0x16
08002F8B: FD 3C 50                      w1 laddr     b.0x40
08002F8E: 20 5C                         w1 =:        b.0x70
08002F90: 1A 04 5B                      w move       $0x4,b.0x6C
08002F93: CA 0C                         if < go      $0xC
08002F95: 84                            bi1 clr
08002F96: 85                            bi2 clr
08002F97: FD 67 C4 08 00 2B E0 5B       by smove     $0x8002BE0,b.0x6C
08002F9F: 4A 49                         w stz        b.0x24
08002FA1: 4A 4D                         w stz        b.0x34
08002FA3: 0C 47                         w1 :=        b.0x1C
08002FA5: 20 4B                         w1 =:        b.0x2C
08002FA7: 0D 48                         w2 :=        b.0x20
08002FA9: 21 5E                         w2 =:        b.0x78
08002FAB: 34 D1                         w1 comp      r2
08002FAD: C9 00 A4                      if > go      $0xA4
08002FB0: 0D 4B                         w2 :=        b.0x2C
08002FB2: 04 E5 18                      by1 :=       @b.0x18+
08002FB5: 1C 4A                         by1 =:       b.0x28
08002FB7: 0E 4D                         w3 :=        b.0x34
08002FB9: B4 E2 08 00 2B E8             jumpg        $0x8002BE8+
08002FBF: 2D 4A CD 28                   by comp2     b.0x28,$0x28
08002FC3: C6 0F                         if >< go     $0xF
08002FC5: 04 4A                         by1 :=       b.0x28
08002FC7: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08002FCD: 9D                            ifkret
08002FCE: 4D 4D                         w set1       b.0x34
08002FD0: C0 4A                         go           $0x4A
08002FD2: 2D 4A CD 3A                   by comp2     b.0x28,$0x3A
08002FD6: C6 18                         if >< go     $0x18
08002FD8: FD 3D 50                      w2 laddr     b.0x40
08002FDB: 21 56                         w2 =:        b.0x58
08002FDD: 4A 57                         w stz        b.0x5C
08002FDF: 1A 03 58                      w move       $0x3,b.0x60
08002FE2: C3 08 00 2E 08 00             call         $0x8002E08,$0x0
08002FE8: 9D                            ifkret
08002FE9: 1A 02 4D                      w move       $0x2,b.0x34
08002FEC: C0 2E                         go           $0x2E
08002FEE: 2D 4A CD 3B                   by comp2     b.0x28,$0x3B
08002FF2: C6 1F                         if >< go     $0x1F
08002FF4: FD 3D 50                      w2 laddr     b.0x40
08002FF7: 21 56                         w2 =:        b.0x58
08002FF9: 4A 57                         w stz        b.0x5C
08002FFB: 1A 03 58                      w move       $0x3,b.0x60
08002FFE: C3 08 00 2E 08 00             call         $0x8002E08,$0x0
08003004: 9D                            ifkret
08003005: 04 CD 3B                      by1 :=       $0x3B
08003008: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
0800300E: 9D                            ifkret
0800300F: C0 0B                         go           $0xB
08003011: 04 4A                         by1 :=       b.0x28
08003013: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08003019: 9D                            ifkret
0800301A: C0 2D                         go           $0x2D
0800301C: 04 4A                         by1 :=       b.0x28
0800301E: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08003024: 9D                            ifkret
08003025: 2D 4A CD 29                   by comp2     b.0x28,$0x29
08003029: C6 04                         if >< go     $0x4
0800302B: 4A 4D                         w stz        b.0x34
0800302D: C0 1A                         go           $0x1A
0800302F: 2D 4A CD 3B                   by comp2     b.0x28,$0x3B
08003033: C4 07                         if = go      $0x7
08003035: 2D 4A 0D                      by comp2     b.0x28,$0xD
08003038: C6 0D                         if >< go     $0xD
0800303A: 04 4A                         by1 :=       b.0x28
0800303C: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08003042: 9D                            ifkret
08003043: 4A 4D                         w stz        b.0x34
08003045: C0 02                         go           $0x2
08003047: 2D 4A 0D                      by comp2     b.0x28,$0xD
0800304A: C4 07                         if = go      $0x7
0800304C: E1 4B 5E FF                   loopi        b.0x2C,b.0x78,$0xFFFFFFFFFFFFFFFF
08003050: 64 FD 3D 4F 0C 49             f1 -         r2.(0x3D4F0C49)
08003056: 0E 18                         w3 :=        $0x18
08003058: FE 03                         clrk
0800305A: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08003060: 9D                            ifkret
08003061: 44 45                         w test       b.0x14
08003063: C6 4A                         if >< go     $0x4A
08003065: 4A 49                         w stz        b.0x24
08003067: 04 CD 81                      by1 :=       $0x81
0800306A: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08003070: 9D                            ifkret
08003071: 04 0D                         by1 :=       $0xD
08003073: C3 08 00 2D C2 00             call         $0x8002DC2,$0x0
08003079: 9D                            ifkret
0800307A: FD 3D 4F                      w2 laddr     b.0x3C
0800307D: 0C 49                         w1 :=        b.0x24
0800307F: 0E 18                         w3 :=        $0x18
08003081: FE 03                         clrk
08003083: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08003089: 9D                            ifkret
0800308A: 18 42                         r:=          b.0x8
0800308C: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
08003091: C3 08 00 87 DD 00             call         $0x80087DD,$0x0
08003097: 9D                            ifkret
08003098: 18 42                         r:=          b.0x8
0800309A: 1A 88 49                      w move       r.0x20,b.0x24
0800309D: FD 3D 4F                      w2 laddr     b.0x3C
080030A0: 0C 49                         w1 :=        b.0x24
080030A2: 0E 18                         w3 :=        $0x18
080030A4: FE 03                         clrk
080030A6: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080030AC: 9D                            ifkret
080030AD: 1A 4F 49                      w move       b.0x3C,b.0x24
080030B0: 80                            ret
080030B1: B8 CF 00 00 00 D8             ents         $0xD8
080030B7: C0 24                         go           $0x24
080030B9: 9C                            entd
080030BA: FD C0 71                      l=:          b.0xC4
080030BD: 20 43                         w1 =:        b.0xC
080030BF: 44 52                         w test       b.0x48
080030C1: C4 13                         if = go      $0x13
080030C3: 18 42                         r:=          b.0x8
080030C5: 1A 52 85                      w move       b.0x48,r.0x14
080030C8: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
080030CE: 9D                            ifkret
080030CF: 18 42                         r:=          b.0x8
080030D1: 1A 85 52                      w move       r.0x14,b.0x48
080030D4: 0C 43                         w1 :=        b.0xC
080030D6: 80                            ret
080030D7: FE 03                         clrk
080030D9: B4 71                         jumpg        b.0xC4
080030DB: 85                            bi2 clr
080030DC: 21 4D                         w2 =:        b.0x34
080030DE: 21 4E                         w2 =:        b.0x38
080030E0: 4A 50                         w stz        b.0x40
080030E2: 86                            bi3 clr
080030E3: 22 56                         w3 =:        b.0x58
080030E5: 22 53                         w3 =:        b.0x4C
080030E7: 22 52                         w3 =:        b.0x48
080030E9: 22 55                         w3 =:        b.0x54
080030EB: 44 48                         w test       b.0x20
080030ED: C6 4D                         if >< go     $0x4D
080030EF: 87                            bi4 clr
080030F0: 23 C4 08 00 79 F0             w4 =:        $0x80079F0
080030F6: 23 C4 08 00 79 F4             w4 =:        $0x80079F4
080030FC: 18 42                         r:=          b.0x8
080030FE: 1A C4 08 00 29 5C 85          w move       $0x800295C,r.0x14
08003105: C3 08 00 18 F1 00             call         $0x80018F1,$0x0
0800310B: D2 08                         if -k go     $0x8
0800310D: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003113: 18 42                         r:=          b.0x8
08003115: 1A 85 C4 08 00 29 5C          w move       r.0x14,$0x800295C
0800311C: 4A 85                         w stz        r.0x14
0800311E: C3 08 00 34 BE 00             call         $0x80034BE,$0x0
08003124: D2 08                         if -k go     $0x8
08003126: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
0800312C: C3 08 00 AD B3 00             call         $0x800ADB3,$0x0
08003132: D2 08                         if -k go     $0x8
08003134: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
0800313A: 18 42                         r:=          b.0x8
0800313C: FD 20 45 85 0C                by bmove     b.0x14,r.0x14,$0xC
08003141: FE 79 C4 08 00 2B F8 88 03    w bmove      $0x8002BF8,r.0x20,$0x3
0800314A: C3 08 00 84 18 00             call         $0x8008418,$0x0
08003150: D2 08                         if -k go     $0x8
08003152: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003158: 18 42                         r:=          b.0x8
0800315A: FD 20 45 85 0C                by bmove     b.0x14,r.0x14,$0xC
0800315F: 19 CD 52 88                   by move      $0x52,r.0x20
08003163: C3 08 00 AF 76 00             call         $0x800AF76,$0x0
08003169: D2 08                         if -k go     $0x8
0800316B: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003171: 20 52                         w1 =:        b.0x48
08003173: 18 42                         r:=          b.0x8
08003175: 20 85                         w1 =:        r.0x14
08003177: FE 79 00 86 03                w bmove      $0x0,r.0x18,$0x3
0800317C: FE 79 C4 08 00 2C 04 89 03    w bmove      $0x8002C04,r.0x24,$0x3
08003185: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
0800318B: D2 08                         if -k go     $0x8
0800318D: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003193: 44 48                         w test       b.0x20
08003195: C4 0D                         if = go      $0xD
08003197: 18 CF 08 00 29 74             r:=          $0x8002974
0800319D: 2E 8C 4A                      w comp2      r.0x30,b.0x28
080031A0: DA 0B                         if <<= go    $0xB
080031A2: 18 CF 08 00 29 74             r:=          $0x8002974
080031A8: 1A 8C 4A                      w move       r.0x30,b.0x28
080031AB: 44 48                         w test       b.0x20
080031AD: C7 00 D7                      if >< go     $0xD7
080031B0: FD 3D C9 12                   w2 laddr     r.0x12
080031B4: 21 73                         w2 =:        b.0xCC
080031B6: 1A 04 72                      w move       $0x4,b.0xC8
080031B9: 84                            bi1 clr
080031BA: 85                            bi2 clr
080031BB: FD BE 72 C4 08 00 2C 14 00    by scopa     b.0xC8,$0x8002C14,$0x0
080031C4: C5 00 C0                      if = go      $0xC0
080031C7: 18 42                         r:=          b.0x8
080031C9: 1A 52 85                      w move       b.0x48,r.0x14
080031CC: FD 3C 58                      w1 laddr     b.0x60
080031CF: 20 86                         w1 =:        r.0x18
080031D1: 4A 87                         w stz        r.0x1C
080031D3: 1A CD 63 88                   w move       $0x63,r.0x20
080031D7: FE 79 00 89 03                w bmove      $0x0,r.0x24,$0x3
080031DC: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
080031E2: D2 08                         if -k go     $0x8
080031E4: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080031EA: 18 42                         r:=          b.0x8
080031EC: 4D 85                         w set1       r.0x14
080031EE: FD 3D 58                      w2 laddr     b.0x60
080031F1: 21 86                         w2 =:        r.0x18
080031F3: 4A 87                         w stz        r.0x1C
080031F5: 1A CD 63 88                   w move       $0x63,r.0x20
080031F9: C3 08 00 2E 2F 00             call         $0x8002E2F,$0x0
080031FF: D2 08                         if -k go     $0x8
08003201: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003207: 18 42                         r:=          b.0x8
08003209: 1A 89 54                      w move       r.0x24,b.0x50
0800320C: 1A 54 85                      w move       b.0x50,r.0x14
0800320F: 1A 49 86                      w move       b.0x24,r.0x18
08003212: 1A 56 87                      w move       b.0x58,r.0x1C
08003215: C3 08 00 2C 96 00             call         $0x8002C96,$0x0
0800321B: D2 08                         if -k go     $0x8
0800321D: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003223: 18 42                         r:=          b.0x8
08003225: 1A 85 54                      w move       r.0x14,b.0x50
08003228: 1A 86 49                      w move       r.0x18,b.0x24
0800322B: 1A 87 56                      w move       r.0x1C,b.0x58
0800322E: 4A 85                         w stz        r.0x14
08003230: FD 3D 58                      w2 laddr     b.0x60
08003233: 21 86                         w2 =:        r.0x18
08003235: 4A 87                         w stz        r.0x1C
08003237: 1A CD 63 88                   w move       $0x63,r.0x20
0800323B: C3 08 00 2E 2F 00             call         $0x8002E2F,$0x0
08003241: D2 08                         if -k go     $0x8
08003243: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003249: 18 42                         r:=          b.0x8
0800324B: 1A 89 54                      w move       r.0x24,b.0x50
0800324E: 1A 54 85                      w move       b.0x50,r.0x14
08003251: 1A 49 86                      w move       b.0x24,r.0x18
08003254: 1A 56 87                      w move       b.0x58,r.0x1C
08003257: C3 08 00 2C 96 00             call         $0x8002C96,$0x0
0800325D: D2 08                         if -k go     $0x8
0800325F: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003265: 18 42                         r:=          b.0x8
08003267: 1A 85 54                      w move       r.0x14,b.0x50
0800326A: 1A 86 49                      w move       r.0x18,b.0x24
0800326D: 1A 87 56                      w move       r.0x1C,b.0x58
08003270: 1A 02 85                      w move       $0x2,r.0x14
08003273: C3 08 00 34 BE 00             call         $0x80034BE,$0x0
08003279: D2 08                         if -k go     $0x8
0800327B: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003281: C1 02 04                      go           $0x204
08003284: 4F 50                         w incr       b.0x40
08003286: 4A 4C                         w stz        b.0x30
08003288: 18 42                         r:=          b.0x8
0800328A: 1A 52 85                      w move       b.0x48,r.0x14
0800328D: 1A 54 86                      w move       b.0x50,r.0x18
08003290: 1A 50 87                      w move       b.0x40,r.0x1C
08003293: 19 4F 88                      by move      b.0x3C,r.0x20
08003296: C3 08 00 63 1D 00             call         $0x800631D,$0x0
0800329C: D2 08                         if -k go     $0x8
0800329E: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080032A4: 18 42                         r:=          b.0x8
080032A6: 1A 86 54                      w move       r.0x18,b.0x50
080032A9: 1A 87 50                      w move       r.0x1C,b.0x40
080032AC: 19 88 4F                      by move      r.0x20,b.0x3C
080032AF: 20 51                         w1 =:        b.0x44
080032B1: B4 E0 08 00 2C 1C             jumpg        $0x8002C1C+
080032B7: C1 01 A0                      go           $0x1A0
080032BA: 44 4D                         w test       b.0x34
080032BC: C4 5D                         if = go      $0x5D
080032BE: 44 C4 08 00 79 F0             w test       $0x80079F0
080032C4: C6 53                         if >< go     $0x53
080032C6: FD 3D 53                      w2 laddr     b.0x4C
080032C9: 0C 54                         w1 :=        b.0x50
080032CB: 0E 18                         w3 :=        $0x18
080032CD: FE 03                         clrk
080032CF: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080032D5: 9D                            ifkret
080032D6: 44 56                         w test       b.0x58
080032D8: C4 3D                         if = go      $0x3D
080032DA: 1A 56 57                      w move       b.0x58,b.0x5C
080032DD: 44 57                         w test       b.0x5C
080032DF: C4 1E                         if = go      $0x1E
080032E1: 18 C5 5C                      r:=          @b.0x5C
080032E4: FD 3D 87                      w2 laddr     r.0x1C
080032E7: 0C 53                         w1 :=        b.0x4C
080032E9: 0E 18                         w3 :=        $0x18
080032EB: FE 03                         clrk
080032ED: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080032F3: 9D                            ifkret
080032F4: 18 57                         r:=          b.0x5C
080032F6: 1A 81 57                      w move       r.0x4,b.0x5C
080032F9: 44 57                         w test       b.0x5C
080032FB: C6 E6                         if >< go     $0xFFFFFFFFFFFFFFE6
080032FD: 18 42                         r:=          b.0x8
080032FF: 1A 56 85                      w move       b.0x58,r.0x14
08003302: C3 08 00 16 AE 00             call         $0x80016AE,$0x0
08003308: D2 08                         if -k go     $0x8
0800330A: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003310: 18 42                         r:=          b.0x8
08003312: 1A 85 56                      w move       r.0x14,b.0x58
08003315: 4D 4E                         w set1       b.0x38
08003317: C0 1E                         go           $0x1E
08003319: 44 51                         w test       b.0x44
0800331B: C6 1A                         if >< go     $0x1A
0800331D: 18 42                         r:=          b.0x8
0800331F: 1A 54 85                      w move       b.0x50,r.0x14
08003322: 1A 02 86                      w move       $0x2,r.0x18
08003325: C3 08 00 3F 37 00             call         $0x8003F37,$0x0
0800332B: D2 08                         if -k go     $0x8
0800332D: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003333: 4D 4C                         w set1       b.0x30
08003335: C1 01 22                      go           $0x122
08003338: C3 08 00 37 30 00             call         $0x8003730,$0x0
0800333E: D2 08                         if -k go     $0x8
08003340: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003346: 44 D0                         w test       r1
08003348: C4 2B                         if = go      $0x2B
0800334A: 44 C4 08 00 79 F0             w test       $0x80079F0
08003350: C6 23                         if >< go     $0x23
08003352: 18 42                         r:=          b.0x8
08003354: 1A 02 85                      w move       $0x2,r.0x14
08003357: 1A 54 86                      w move       b.0x50,r.0x18
0800335A: 1A 55 87                      w move       b.0x54,r.0x1C
0800335D: C3 08 00 AC 3B 00             call         $0x800AC3B,$0x0
08003363: D2 08                         if -k go     $0x8
08003365: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
0800336B: 18 42                         r:=          b.0x8
0800336D: 1A 86 54                      w move       r.0x18,b.0x50
08003370: 1A 87 55                      w move       r.0x1C,b.0x54
08003373: C1 00 E4                      go           $0xE4
08003376: C3 08 00 37 30 00             call         $0x8003730,$0x0
0800337C: D2 08                         if -k go     $0x8
0800337E: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003384: 44 D0                         w test       r1
08003386: C4 2B                         if = go      $0x2B
08003388: 44 C4 08 00 79 F0             w test       $0x80079F0
0800338E: C6 23                         if >< go     $0x23
08003390: 18 42                         r:=          b.0x8
08003392: 1A 02 85                      w move       $0x2,r.0x14
08003395: 1A 55 86                      w move       b.0x54,r.0x18
08003398: 1A 54 87                      w move       b.0x50,r.0x1C
0800339B: C3 08 00 AC 3B 00             call         $0x800AC3B,$0x0
080033A1: D2 08                         if -k go     $0x8
080033A3: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080033A9: 18 42                         r:=          b.0x8
080033AB: 1A 86 55                      w move       r.0x18,b.0x54
080033AE: 1A 87 54                      w move       r.0x1C,b.0x50
080033B1: C1 00 A6                      go           $0xA6
080033B4: 44 C4 08 00 79 F0             w test       $0x80079F0
080033BA: C6 26                         if >< go     $0x26
080033BC: 18 42                         r:=          b.0x8
080033BE: 1A 54 85                      w move       b.0x50,r.0x14
080033C1: 1A 49 86                      w move       b.0x24,r.0x18
080033C4: 1A 56 87                      w move       b.0x58,r.0x1C
080033C7: C3 08 00 2C 96 00             call         $0x8002C96,$0x0
080033CD: D2 08                         if -k go     $0x8
080033CF: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080033D5: 18 42                         r:=          b.0x8
080033D7: 1A 85 54                      w move       r.0x14,b.0x50
080033DA: 1A 86 49                      w move       r.0x18,b.0x24
080033DD: 1A 87 56                      w move       r.0x1C,b.0x58
080033E0: 4D 4D                         w set1       b.0x34
080033E2: 44 4E                         w test       b.0x38
080033E4: C4 08                         if = go      $0x8
080033E6: 4A 53                         w stz        b.0x4C
080033E8: 4A 4E                         w stz        b.0x38
080033EA: C0 1A                         go           $0x1A
080033EC: 18 42                         r:=          b.0x8
080033EE: 1A 53 85                      w move       b.0x4C,r.0x14
080033F1: C3 08 00 87 17 00             call         $0x8008717,$0x0
080033F7: D2 08                         if -k go     $0x8
080033F9: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080033FF: 18 42                         r:=          b.0x8
08003401: 1A 85 53                      w move       r.0x14,b.0x4C
08003404: C0 53                         go           $0x53
08003406: 18 42                         r:=          b.0x8
08003408: 1A 54 85                      w move       b.0x50,r.0x14
0800340B: FD 3C 58                      w1 laddr     b.0x60
0800340E: 20 86                         w1 =:        r.0x18
08003410: 4A 87                         w stz        r.0x1C
08003412: 1A CD 63 88                   w move       $0x63,r.0x20
08003416: C3 08 00 88 5B 00             call         $0x800885B,$0x0
0800341C: D2 08                         if -k go     $0x8
0800341E: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003424: FD 3D 58                      w2 laddr     b.0x60
08003427: 18 42                         r:=          b.0x8
08003429: 21 85                         w2 =:        r.0x14
0800342B: 4A 86                         w stz        r.0x18
0800342D: 1A CD 63 87                   w move       $0x63,r.0x1C
08003431: 0E 48                         w3 :=        b.0x20
08003433: 56 01                         w3 +         $0x1
08003435: 22 88                         w3 =:        r.0x20
08003437: 1A 49 89                      w move       b.0x24,r.0x24
0800343A: 1A 4A 8A                      w move       b.0x28,r.0x28
0800343D: C3 08 00 30 B1 00             call         $0x80030B1,$0x0
08003443: D2 08                         if -k go     $0x8
08003445: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
0800344B: 18 42                         r:=          b.0x8
0800344D: 1A 89 49                      w move       r.0x24,b.0x24
08003450: 1A 8A 4A                      w move       r.0x28,b.0x28
08003453: 4D 4C                         w set1       b.0x30
08003455: C0 02                         go           $0x2
08003457: 44 4C                         w test       b.0x30
08003459: C6 0A                         if >< go     $0xA
0800345B: 44 C4 08 00 79 F0             w test       $0x80079F0
08003461: C4 1A                         if = go      $0x1A
08003463: 18 42                         r:=          b.0x8
08003465: 1A 54 85                      w move       b.0x50,r.0x14
08003468: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800346E: D2 08                         if -k go     $0x8
08003470: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003476: 18 42                         r:=          b.0x8
08003478: 1A 85 54                      w move       r.0x14,b.0x50
0800347B: 2D 4F 17                      by comp2     b.0x3C,$0x17
0800347E: C4 05                         if = go      $0x5
08003480: C1 FE 04                      go           $0xFFFFFFFFFFFFFE04
08003483: 4A 50                         w stz        b.0x40
08003485: 18 42                         r:=          b.0x8
08003487: 1A 52 85                      w move       b.0x48,r.0x14
0800348A: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
08003490: D2 08                         if -k go     $0x8
08003492: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
08003498: 18 42                         r:=          b.0x8
0800349A: 1A 85 52                      w move       r.0x14,b.0x48
0800349D: 44 C4 08 00 79 F0             w test       $0x80079F0
080034A3: C4 19                         if = go      $0x19
080034A5: 0C CD 9A                      w1 :=        $0x9A
080034A8: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
080034AE: D2 08                         if -k go     $0x8
080034B0: C3 08 00 30 B9 00             call         $0x80030B9,$0x0
080034B6: 0C CD 9A                      w1 :=        $0x9A
080034B9: 80                            ret
080034BA: C0 04                         go           $0x4
080034BC: 84                            bi1 clr
080034BD: 80                            ret
080034BE: B8 CF 00 00 00 18             ents         $0x18
080034C4: 1A 45 C4 08 00 2C 3C          w move       b.0x14,$0x8002C3C
080034CB: 80                            ret
080034CC: B8 CF 00 00 00 20             ents         $0x20
080034D2: 44 C4 08 00 2C 44             w test       $0x8002C44
080034D8: C6 22                         if >< go     $0x22
080034DA: 18 42                         r:=          b.0x8
080034DC: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080034E5: 0C 14                         w1 :=        $0x14
080034E7: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080034ED: 9D                            ifkret
080034EE: 20 47                         w1 =:        b.0x1C
080034F0: 1A 47 46                      w move       b.0x1C,b.0x18
080034F3: 18 46                         r:=          b.0x18
080034F5: 4A 81                         w stz        r.0x4
080034F7: C1 00 7A                      go           $0x7A
080034FA: 18 C4 08 00 2C 44             r:=          $0x8002C44
08003500: 44 83                         w test       r.0xC
08003502: C4 3E                         if = go      $0x3E
08003504: 0C C4 08 00 2C 44             w1 :=        $0x8002C44
0800350A: 20 46                         w1 =:        b.0x18
0800350C: 1A F4 0C C4 08 00 2C 44       w move       r1.(0xC),$0x8002C44
08003514: 44 F4 10                      w test       r1.(0x10)
08003517: C4 1C                         if = go      $0x1C
08003519: 0D F4 10                      w2 :=        r1.(0x10)
0800351C: 18 C4 08 00 2C 44             r:=          $0x8002C44
08003522: 21 82                         w2 =:        r.0x8
08003524: 0E F4 08                      w3 :=        r1.(0x8)
08003527: 18 C4 08 00 2C 44             r:=          $0x8002C44
0800352D: 18 82                         r:=          r.0x8
0800352F: 22 82                         w3 =:        r.0x8
08003531: C0 0D                         go           $0xD
08003533: 0F F4 08                      w4 :=        r1.(0x8)
08003536: 18 C4 08 00 2C 44             r:=          $0x8002C44
0800353C: 23 82                         w4 =:        r.0x8
0800353E: C0 33                         go           $0x33
08003540: 44 84                         w test       r.0x10
08003542: C4 1F                         if = go      $0x1F
08003544: 0C C4 08 00 2C 44             w1 :=        $0x8002C44
0800354A: 20 46                         w1 =:        b.0x18
0800354C: 1A F4 0C C4 08 00 2C 44       w move       r1.(0xC),$0x8002C44
08003554: 0D F4 08                      w2 :=        r1.(0x8)
08003557: 18 C4 08 00 2C 44             r:=          $0x8002C44
0800355D: 21 82                         w2 =:        r.0x8
0800355F: C0 12                         go           $0x12
08003561: 0C C4 08 00 2C 44             w1 :=        $0x8002C44
08003567: 20 46                         w1 =:        b.0x18
08003569: 1A F4 08 C4 08 00 2C 44       w move       r1.(0x8),$0x8002C44
08003571: 84                            bi1 clr
08003572: 18 46                         r:=          b.0x18
08003574: 20 82                         w1 =:        r.0x8
08003576: 20 83                         w1 =:        r.0xC
08003578: 4A 84                         w stz        r.0x10
0800357A: 0E 81                         w3 :=        r.0x4
0800357C: 18 42                         r:=          b.0x8
0800357E: 22 85                         w3 =:        r.0x14
08003580: C3 08 00 87 17 00             call         $0x8008717,$0x0
08003586: 9D                            ifkret
08003587: 18 42                         r:=          b.0x8
08003589: 0D 85                         w2 :=        r.0x14
0800358B: 18 46                         r:=          b.0x18
0800358D: 21 81                         w2 =:        r.0x4
0800358F: 0C 46                         w1 :=        b.0x18
08003591: 80                            ret
08003592: B8 CF 00 00 00 1C             ents         $0x1C
08003598: 44 45                         w test       b.0x14
0800359A: C4 21                         if = go      $0x21
0800359C: 1A 45 46                      w move       b.0x14,b.0x18
0800359F: 18 46                         r:=          b.0x18
080035A1: 44 82                         w test       r.0x8
080035A3: C4 07                         if = go      $0x7
080035A5: 1A 82 46                      w move       r.0x8,b.0x18
080035A8: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
080035AA: 1A C4 08 00 2C 44 82          w move       $0x8002C44,r.0x8
080035B1: 84                            bi1 clr
080035B2: 52 45 D0                      w swap       b.0x14,r1
080035B5: 20 C4 08 00 2C 44             w1 =:        $0x8002C44
080035BB: 80                            ret
080035BC: B8 CF 00 00 00 20             ents         $0x20
080035C2: 44 C4 08 00 2C 50             w test       $0x8002C50
080035C8: C6 1D                         if >< go     $0x1D
080035CA: 18 42                         r:=          b.0x8
080035CC: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080035D5: 0C 0C                         w1 :=        $0xC
080035D7: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080035DD: 9D                            ifkret
080035DE: 20 47                         w1 =:        b.0x1C
080035E0: 1A 47 46                      w move       b.0x1C,b.0x18
080035E3: C0 12                         go           $0x12
080035E5: 0C C4 08 00 2C 50             w1 :=        $0x8002C50
080035EB: 20 46                         w1 =:        b.0x18
080035ED: 1A F4 08 C4 08 00 2C 50       w move       r1.(0x8),$0x8002C50
080035F5: 4A C5 18                      w stz        @b.0x18
080035F8: 18 46                         r:=          b.0x18
080035FA: 4A 81                         w stz        r.0x4
080035FC: 4A 82                         w stz        r.0x8
080035FE: 0C 46                         w1 :=        b.0x18
08003600: 80                            ret
08003601: B8 CF 00 00 00 18             ents         $0x18
08003607: 44 45                         w test       b.0x14
08003609: C4 2A                         if = go      $0x2A
0800360B: 18 45                         r:=          b.0x14
0800360D: 0C 82                         w1 :=        r.0x8
0800360F: 18 42                         r:=          b.0x8
08003611: 20 85                         w1 =:        r.0x14
08003613: C3 08 00 36 01 00             call         $0x8003601,$0x0
08003619: 9D                            ifkret
0800361A: 18 42                         r:=          b.0x8
0800361C: 0D 85                         w2 :=        r.0x14
0800361E: 18 45                         r:=          b.0x14
08003620: 21 82                         w2 =:        r.0x8
08003622: 1A C4 08 00 2C 50 82          w move       $0x8002C50,r.0x8
08003629: 86                            bi3 clr
0800362A: 52 45 D2                      w swap       b.0x14,r3
0800362D: 22 C4 08 00 2C 50             w3 =:        $0x8002C50
08003633: 80                            ret
08003634: B8 CF 00 00 00 3C             ents         $0x3C
0800363A: 1A 45 4C                      w move       b.0x14,b.0x30
0800363D: 1A 47 4D                      w move       b.0x1C,b.0x34
08003640: 84                            bi1 clr
08003641: 20 4A                         w1 =:        b.0x28
08003643: 20 4B                         w1 =:        b.0x2C
08003645: FD 3D C5 30                   w2 laddr     @b.0x30
08003649: 55 4A                         w2 +         b.0x28
0800364B: 04 F5 00                      by1 :=       r2.(0x0)
0800364E: 1C 49                         by1 =:       b.0x24
08003650: 30 09                         by1 comp     $0x9
08003652: C6 0B                         if >< go     $0xB
08003654: 18 4C                         r:=          b.0x30
08003656: 1A 85 4C                      w move       r.0x14,b.0x30
08003659: 4A 4A                         w stz        b.0x28
0800365B: C0 EA                         go           $0xFFFFFFFFFFFFFFEA
0800365D: FD 3D C5 34                   w2 laddr     @b.0x34
08003661: 55 4B                         w2 +         b.0x2C
08003663: 04 F5 00                      by1 :=       r2.(0x0)
08003666: 1C C1 25                      by1 =:       b.0x25
08003669: 30 09                         by1 comp     $0x9
0800366B: C6 0B                         if >< go     $0xB
0800366D: 18 4D                         r:=          b.0x34
0800366F: 1A 85 4D                      w move       r.0x14,b.0x34
08003672: 4A 4B                         w stz        b.0x2C
08003674: C0 E9                         go           $0xFFFFFFFFFFFFFFE9
08003676: 2D 49 D0                      by comp2     b.0x24,r1
08003679: C6 2B                         if >< go     $0x2B
0800367B: 2D 49 0D                      by comp2     b.0x24,$0xD
0800367E: C4 26                         if = go      $0x26
08003680: 0E 4A                         w3 :=        b.0x28
08003682: 56 01                         w3 +         $0x1
08003684: 22 4A                         w3 =:        b.0x28
08003686: 36 13                         w3 comp      $0x13
08003688: CE 09                         if <= go     $0x9
0800368A: 18 4C                         r:=          b.0x30
0800368C: 1A 85 4C                      w move       r.0x14,b.0x30
0800368F: 4A 4A                         w stz        b.0x28
08003691: 0E 4B                         w3 :=        b.0x2C
08003693: 56 01                         w3 +         $0x1
08003695: 22 4B                         w3 =:        b.0x2C
08003697: 36 13                         w3 comp      $0x13
08003699: CE 09                         if <= go     $0x9
0800369B: 18 4D                         r:=          b.0x34
0800369D: 1A 85 4D                      w move       r.0x14,b.0x34
080036A0: 4A 4B                         w stz        b.0x2C
080036A2: C0 A3                         go           $0xFFFFFFFFFFFFFFA3
080036A4: 2D 46 C4 08 00 2D 00          by comp2     b.0x18,$0x8002D00
080036AB: D4 64                         if >> go     $0x64
080036AD: 06 46                         by3 :=       b.0x18
080036AF: B4 E2 08 00 2D 04             jumpg        $0x8002D04+
080036B5: 4A 4E                         w stz        b.0x38
080036B7: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
080036BB: D6 04                         if >>= go    $0x4
080036BD: 4D 4E                         w set1       b.0x38
080036BF: 0C 4E                         w1 :=        b.0x38
080036C1: 80                            ret
080036C2: C0 6E                         go           $0x6E
080036C4: 4A 4E                         w stz        b.0x38
080036C6: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
080036CA: D4 04                         if >> go     $0x4
080036CC: 4D 4E                         w set1       b.0x38
080036CE: 0C 4E                         w1 :=        b.0x38
080036D0: 80                            ret
080036D1: C0 5F                         go           $0x5F
080036D3: 4A 4E                         w stz        b.0x38
080036D5: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
080036D9: C4 04                         if = go      $0x4
080036DB: 4D 4E                         w set1       b.0x38
080036DD: 0C 4E                         w1 :=        b.0x38
080036DF: 80                            ret
080036E0: C0 50                         go           $0x50
080036E2: 4A 4E                         w stz        b.0x38
080036E4: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
080036E8: C6 04                         if >< go     $0x4
080036EA: 4D 4E                         w set1       b.0x38
080036EC: 0C 4E                         w1 :=        b.0x38
080036EE: 80                            ret
080036EF: C0 41                         go           $0x41
080036F1: 4A 4E                         w stz        b.0x38
080036F3: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
080036F7: DA 04                         if <<= go    $0x4
080036F9: 4D 4E                         w set1       b.0x38
080036FB: 0C 4E                         w1 :=        b.0x38
080036FD: 80                            ret
080036FE: C0 32                         go           $0x32
08003700: 4A 4E                         w stz        b.0x38
08003702: 2D 49 C1 25                   by comp2     b.0x24,b.0x25
08003706: D8 04                         if << go     $0x4
08003708: 4D 4E                         w set1       b.0x38
0800370A: 0C 4E                         w1 :=        b.0x38
0800370C: 80                            ret
0800370D: C0 23                         go           $0x23
0800370F: 18 42                         r:=          b.0x8
08003711: 4D 85                         w set1       r.0x14
08003713: FE 79 C4 08 00 2C E8 86 03    w bmove      $0x8002CE8,r.0x18,$0x3
0800371C: FE 79 C4 08 00 2C F4 89 03    w bmove      $0x8002CF4,r.0x24,$0x3
08003725: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
0800372B: 9D                            ifkret
0800372C: 0C CD C6                      w1 :=        $0xC6
0800372F: 81                            retk
08003730: B8 CF 00 00 00 18             ents         $0x18
08003736: 44 C4 08 00 79 F0             w test       $0x80079F0
0800373C: C4 06                         if = go      $0x6
0800373E: 84                            bi1 clr
0800373F: 80                            ret
08003740: C0 18                         go           $0x18
08003742: 44 C4 08 00 2C 48             w test       $0x8002C48
08003748: C6 07                         if >< go     $0x7
0800374A: 0C 01                         w1 :=        $0x1
0800374C: 80                            ret
0800374D: C0 0B                         go           $0xB
0800374F: 18 C4 08 00 2C 48             r:=          $0x8002C48
08003755: 0C 80                         w1 :=        r.0x0
08003757: 80                            ret
08003758: B8 CF 00 00 00 34             ents         $0x34
0800375E: 4A 4C                         w stz        b.0x30
08003760: 1A 3F 4B                      w move       $0x3F,b.0x2C
08003763: FD 3D C5 14                   w2 laddr     @b.0x14
08003767: 55 46                         w2 +         b.0x18
08003769: 04 F5 00                      by1 :=       r2.(0x0)
0800376C: 1C 4A                         by1 =:       b.0x28
0800376E: 30 0D                         by1 comp     $0xD
08003770: C4 5F                         if = go      $0x5F
08003772: 0E 4C                         w3 :=        b.0x30
08003774: B4 E2 08 00 2F 98             jumpg        $0x8002F98+
0800377A: 2D 4A CD 83                   by comp2     b.0x28,$0x83
0800377E: D8 07                         if << go     $0x7
08003780: 1A 02 4C                      w move       $0x2,b.0x30
08003783: C0 12                         go           $0x12
08003785: 2D 4A CD 20                   by comp2     b.0x28,$0x20
08003789: C4 0C                         if = go      $0xC
0800378B: 84                            bi1 clr
0800378C: 20 4B                         w1 =:        b.0x2C
0800378E: 05 4A                         by2 :=       b.0x28
08003790: 1D E4 1C                      by2 =:       @b.0x1C+
08003793: 4D 4C                         w set1       b.0x30
08003795: C0 22                         go           $0x22
08003797: 2D 4A CD 20                   by comp2     b.0x28,$0x20
0800379B: C4 08                         if = go      $0x8
0800379D: 2D 4A CD 83                   by comp2     b.0x28,$0x83
080037A1: D8 07                         if << go     $0x7
080037A3: 1A 02 4C                      w move       $0x2,b.0x30
080037A6: C0 0D                         go           $0xD
080037A8: 0C 4B                         w1 :=        b.0x2C
080037AA: 54 01                         w1 +         $0x1
080037AC: 20 4B                         w1 =:        b.0x2C
080037AE: 05 4A                         by2 :=       b.0x28
080037B0: 1D E4 1C                      by2 =:       @b.0x1C+
080037B3: C0 04                         go           $0x4
080037B5: C0 02                         go           $0x2
080037B7: 0C 46                         w1 :=        b.0x18
080037B9: 54 01                         w1 +         $0x1
080037BB: 20 46                         w1 =:        b.0x18
080037BD: 34 13                         w1 comp      $0x13
080037BF: CE 09                         if <= go     $0x9
080037C1: 18 45                         r:=          b.0x14
080037C3: 1A 85 45                      w move       r.0x14,b.0x14
080037C6: 4A 46                         w stz        b.0x18
080037C8: 2E 4C 02                      w comp2      b.0x30,$0x2
080037CB: C4 04                         if = go      $0x4
080037CD: C0 96                         go           $0xFFFFFFFFFFFFFF96
080037CF: 0C 4B                         w1 :=        b.0x2C
080037D1: 54 01                         w1 +         $0x1
080037D3: 05 0D                         by2 :=       $0xD
080037D5: 1D E4 1C                      by2 =:       @b.0x1C+
080037D8: 80                            ret
080037D9: B8 CF 00 00 00 54             ents         $0x54
080037DF: 04 C5 14                      by1 :=       @b.0x14
080037E2: 1C 54                         by1 =:       b.0x50
080037E4: 30 C4 08 00 2F E8             by1 comp     $0x8002FE8
080037EA: D5 02 CB                      if >> go     $0x2CB
080037ED: B4 E0 08 00 2F EC             jumpg        $0x8002FEC+
080037F3: 18 45                         r:=          b.0x14
080037F5: 0C 83                         w1 :=        r.0xC
080037F7: 18 42                         r:=          b.0x8
080037F9: 20 85                         w1 =:        r.0x14
080037FB: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
08003801: 9D                            ifkret
08003802: 44 D0                         w test       r1
08003804: C4 07                         if = go      $0x7
08003806: 0C 01                         w1 :=        $0x1
08003808: 80                            ret
08003809: C0 12                         go           $0x12
0800380B: 18 45                         r:=          b.0x14
0800380D: 0D 84                         w2 :=        r.0x10
0800380F: 18 42                         r:=          b.0x8
08003811: 21 85                         w2 =:        r.0x14
08003813: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
08003819: 9D                            ifkret
0800381A: 80                            ret
0800381B: C1 02 BB                      go           $0x2BB
0800381E: 18 45                         r:=          b.0x14
08003820: 0C 83                         w1 :=        r.0xC
08003822: 18 42                         r:=          b.0x8
08003824: 20 85                         w1 =:        r.0x14
08003826: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
0800382C: 9D                            ifkret
0800382D: 44 D0                         w test       r1
0800382F: C6 06                         if >< go     $0x6
08003831: 84                            bi1 clr
08003832: 80                            ret
08003833: C0 12                         go           $0x12
08003835: 18 45                         r:=          b.0x14
08003837: 0D 84                         w2 :=        r.0x10
08003839: 18 42                         r:=          b.0x8
0800383B: 21 85                         w2 =:        r.0x14
0800383D: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
08003843: 9D                            ifkret
08003844: 80                            ret
08003845: C1 02 91                      go           $0x291
08003848: 18 45                         r:=          b.0x14
0800384A: 0C 83                         w1 :=        r.0xC
0800384C: 18 42                         r:=          b.0x8
0800384E: 20 85                         w1 =:        r.0x14
08003850: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
08003856: 9D                            ifkret
08003857: FE 10                         bi1 inv
08003859: 80                            ret
0800385A: C1 02 7C                      go           $0x27C
0800385D: 18 45                         r:=          b.0x14
0800385F: 18 83                         r:=          r.0xC
08003861: 0C 81                         w1 :=        r.0x4
08003863: 20 4D                         w1 =:        b.0x34
08003865: 18 42                         r:=          b.0x8
08003867: 20 85                         w1 =:        r.0x14
08003869: C3 08 00 A2 53 00             call         $0x800A253,$0x0
0800386F: 9D                            ifkret
08003870: 18 42                         r:=          b.0x8
08003872: 1A 85 4D                      w move       r.0x14,b.0x34
08003875: 18 45                         r:=          b.0x14
08003877: 18 84                         r:=          r.0x10
08003879: 0D 81                         w2 :=        r.0x4
0800387B: 21 4E                         w2 =:        b.0x38
0800387D: 18 42                         r:=          b.0x8
0800387F: 21 85                         w2 =:        r.0x14
08003881: C3 08 00 A2 53 00             call         $0x800A253,$0x0
08003887: 9D                            ifkret
08003888: 18 42                         r:=          b.0x8
0800388A: 1A 85 4E                      w move       r.0x14,b.0x38
0800388D: 18 45                         r:=          b.0x14
0800388F: 18 83                         r:=          r.0xC
08003891: 0C 81                         w1 :=        r.0x4
08003893: 18 42                         r:=          b.0x8
08003895: 20 85                         w1 =:        r.0x14
08003897: 19 CD A2 86                   by move      $0xA2,r.0x18
0800389B: 1A 4E 87                      w move       b.0x38,r.0x1C
0800389E: C3 08 00 36 34 00             call         $0x8003634,$0x0
080038A4: 9D                            ifkret
080038A5: 20 48                         w1 =:        b.0x20
080038A7: 44 D0                         w test       r1
080038A9: C7 00 AE                      if >< go     $0xAE
080038AC: 85                            bi2 clr
080038AD: 21 47                         w2 =:        b.0x1C
080038AF: 21 49                         w2 =:        b.0x24
080038B1: 1A 4E 4F                      w move       b.0x38,b.0x3C
080038B4: 86                            bi3 clr
080038B5: 22 52                         w3 =:        b.0x48
080038B7: 22 53                         w3 =:        b.0x4C
080038B9: 44 4F                         w test       b.0x3C
080038BB: C5 00 95                      if = go      $0x95
080038BE: 4A 4A                         w stz        b.0x28
080038C0: FD 3D C5 3C                   w2 laddr     @b.0x3C
080038C4: 55 4A                         w2 +         b.0x28
080038C6: 04 F5 00                      by1 :=       r2.(0x0)
080038C9: 1C 4B                         by1 =:       b.0x2C
080038CB: 30 09                         by1 comp     $0x9
080038CD: C4 0A                         if = go      $0xA
080038CF: 30 0D                         by1 comp     $0xD
080038D1: C4 06                         if = go      $0x6
080038D3: BF 4A 13 ED                   d loopi      b.0x28,$0x13,$0xFFFFFFFFFFFFFFED
080038D7: 18 4F                         r:=          b.0x3C
080038D9: 44 86                         w test       r.0x18
080038DB: C4 0B                         if = go      $0xB
080038DD: 1A 53 52                      w move       b.0x4C,b.0x48
080038E0: 0C 01                         w1 :=        $0x1
080038E2: 20 47                         w1 =:        b.0x1C
080038E4: 20 49                         w1 =:        b.0x24
080038E6: 2D 4B 09                      by comp2     b.0x2C,$0x9
080038E9: C6 57                         if >< go     $0x57
080038EB: 44 49                         w test       b.0x24
080038ED: C4 53                         if = go      $0x53
080038EF: 44 52                         w test       b.0x48
080038F1: C6 07                         if >< go     $0x7
080038F3: 1A 4E 53                      w move       b.0x38,b.0x4C
080038F6: C0 07                         go           $0x7
080038F8: 18 52                         r:=          b.0x48
080038FA: 1A 85 53                      w move       r.0x14,b.0x4C
080038FD: 84                            bi1 clr
080038FE: 18 53                         r:=          b.0x4C
08003900: 52 86 D0                      w swap       r.0x18,r1
08003903: 20 50                         w1 =:        b.0x40
08003905: 20 51                         w1 =:        b.0x44
08003907: 18 51                         r:=          b.0x44
08003909: 44 85                         w test       r.0x14
0800390B: C4 07                         if = go      $0x7
0800390D: 1A 85 51                      w move       r.0x14,b.0x44
08003910: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
08003912: 84                            bi1 clr
08003913: 18 4F                         r:=          b.0x3C
08003915: 52 85 D0                      w swap       r.0x14,r1
08003918: 18 51                         r:=          b.0x44
0800391A: 20 85                         w1 =:        r.0x14
0800391C: 44 52                         w test       b.0x48
0800391E: C6 07                         if >< go     $0x7
08003920: 1A 50 4E                      w move       b.0x40,b.0x38
08003923: C0 07                         go           $0x7
08003925: 18 52                         r:=          b.0x48
08003927: 1A 50 85                      w move       b.0x40,r.0x14
0800392A: 18 42                         r:=          b.0x8
0800392C: 1A 53 85                      w move       b.0x4C,r.0x14
0800392F: C3 08 00 87 17 00             call         $0x8008717,$0x0
08003935: 9D                            ifkret
08003936: 18 42                         r:=          b.0x8
08003938: 1A 85 53                      w move       r.0x14,b.0x4C
0800393B: 1A 51 4F                      w move       b.0x44,b.0x3C
0800393E: 4A 49                         w stz        b.0x24
08003940: 0D 4F                         w2 :=        b.0x3C
08003942: 21 53                         w2 =:        b.0x4C
08003944: 1A F5 14 4F                   w move       r2.(0x14),b.0x3C
08003948: 2D 4B 0D                      by comp2     b.0x2C,$0xD
0800394B: C4 05                         if = go      $0x5
0800394D: C1 FF 6C                      go           $0xFFFFFFFFFFFFFF6C
08003950: 44 47                         w test       b.0x1C
08003952: C4 05                         if = go      $0x5
08003954: C1 FF 39                      go           $0xFFFFFFFFFFFFFF39
08003957: 0D 4D                         w2 :=        b.0x34
08003959: 18 45                         r:=          b.0x14
0800395B: 18 83                         r:=          r.0xC
0800395D: 21 81                         w2 =:        r.0x4
0800395F: 0E 4E                         w3 :=        b.0x38
08003961: 18 45                         r:=          b.0x14
08003963: 18 84                         r:=          r.0x10
08003965: 22 81                         w3 =:        r.0x4
08003967: 0C 48                         w1 :=        b.0x20
08003969: 80                            ret
0800396A: C1 01 6C                      go           $0x16C
0800396D: 18 45                         r:=          b.0x14
0800396F: 18 83                         r:=          r.0xC
08003971: 0C 81                         w1 :=        r.0x4
08003973: 20 4D                         w1 =:        b.0x34
08003975: 18 42                         r:=          b.0x8
08003977: 20 85                         w1 =:        r.0x14
08003979: C3 08 00 A2 53 00             call         $0x800A253,$0x0
0800397F: 9D                            ifkret
08003980: 18 42                         r:=          b.0x8
08003982: 1A 85 4D                      w move       r.0x14,b.0x34
08003985: 18 45                         r:=          b.0x14
08003987: 18 84                         r:=          r.0x10
08003989: 0D 81                         w2 :=        r.0x4
0800398B: 21 4E                         w2 =:        b.0x38
0800398D: 18 42                         r:=          b.0x8
0800398F: 21 85                         w2 =:        r.0x14
08003991: C3 08 00 A2 53 00             call         $0x800A253,$0x0
08003997: 9D                            ifkret
08003998: 18 42                         r:=          b.0x8
0800399A: 1A 85 4E                      w move       r.0x14,b.0x38
0800399D: 1A 4D 85                      w move       b.0x34,r.0x14
080039A0: 05 C5 14                      by2 :=       @b.0x14
080039A3: 1D 86                         by2 =:       r.0x18
080039A5: 1A 4E 87                      w move       b.0x38,r.0x1C
080039A8: C3 08 00 36 34 00             call         $0x8003634,$0x0
080039AE: 9D                            ifkret
080039AF: 20 48                         w1 =:        b.0x20
080039B1: 0D 4D                         w2 :=        b.0x34
080039B3: 18 45                         r:=          b.0x14
080039B5: 18 83                         r:=          r.0xC
080039B7: 21 81                         w2 =:        r.0x4
080039B9: 0E 4E                         w3 :=        b.0x38
080039BB: 18 45                         r:=          b.0x14
080039BD: 18 84                         r:=          r.0x10
080039BF: 22 81                         w3 =:        r.0x4
080039C1: 80                            ret
080039C2: C1 01 14                      go           $0x114
080039C5: 18 45                         r:=          b.0x14
080039C7: 0C 81                         w1 :=        r.0x4
080039C9: 18 42                         r:=          b.0x8
080039CB: 20 85                         w1 =:        r.0x14
080039CD: C3 08 00 A2 53 00             call         $0x800A253,$0x0
080039D3: 9D                            ifkret
080039D4: 18 42                         r:=          b.0x8
080039D6: 0D 85                         w2 :=        r.0x14
080039D8: 18 45                         r:=          b.0x14
080039DA: 21 81                         w2 =:        r.0x4
080039DC: 1A 81 4C                      w move       r.0x4,b.0x30
080039DF: FD 3D C5 30                   w2 laddr     @b.0x30
080039E3: 04 F5 00                      by1 :=       r2.(0x0)
080039E6: 1C 4B                         by1 =:       b.0x2C
080039E8: 30 09                         by1 comp     $0x9
080039EA: C6 09                         if >< go     $0x9
080039EC: 18 4C                         r:=          b.0x30
080039EE: 1A 85 4C                      w move       r.0x14,b.0x30
080039F1: C0 EE                         go           $0xFFFFFFFFFFFFFFEE
080039F3: 34 CE 00 AA                   w1 comp      $0xAA
080039F7: C6 08                         if >< go     $0x8
080039F9: 0C 01                         w1 :=        $0x1
080039FB: 80                            ret
080039FC: C1 00 B7                      go           $0xB7
080039FF: 2D 4B CD A7                   by comp2     b.0x2C,$0xA7
08003A03: C6 07                         if >< go     $0x7
08003A05: 84                            bi1 clr
08003A06: 80                            ret
08003A07: C1 00 AC                      go           $0xAC
08003A0A: 18 42                         r:=          b.0x8
08003A0C: 1A 4C 85                      w move       b.0x30,r.0x14
08003A0F: 19 CD A2 86                   by move      $0xA2,r.0x18
08003A13: 1A C4 08 00 2C C4 87          w move       $0x8002CC4,r.0x1C
08003A1A: C3 08 00 36 34 00             call         $0x8003634,$0x0
08003A20: 9D                            ifkret
08003A21: 44 D0                         w test       r1
08003A23: C4 08                         if = go      $0x8
08003A25: 0C 01                         w1 :=        $0x1
08003A27: 80                            ret
08003A28: C1 00 8B                      go           $0x8B
08003A2B: 18 42                         r:=          b.0x8
08003A2D: 1A 4C 85                      w move       b.0x30,r.0x14
08003A30: 19 CD A2 86                   by move      $0xA2,r.0x18
08003A34: 1A C4 08 00 2C C8 87          w move       $0x8002CC8,r.0x1C
08003A3B: C3 08 00 36 34 00             call         $0x8003634,$0x0
08003A41: 9D                            ifkret
08003A42: 44 D0                         w test       r1
08003A44: C4 06                         if = go      $0x6
08003A46: 84                            bi1 clr
08003A47: 80                            ret
08003A48: C0 6B                         go           $0x6B
08003A4A: 18 42                         r:=          b.0x8
08003A4C: 1A 4C 85                      w move       b.0x30,r.0x14
08003A4F: 19 CD A2 86                   by move      $0xA2,r.0x18
08003A53: 1A C4 08 00 2C CC 87          w move       $0x8002CCC,r.0x1C
08003A5A: C3 08 00 36 34 00             call         $0x8003634,$0x0
08003A60: 9D                            ifkret
08003A61: 44 D0                         w test       r1
08003A63: C4 07                         if = go      $0x7
08003A65: 0C 01                         w1 :=        $0x1
08003A67: 80                            ret
08003A68: C0 4B                         go           $0x4B
08003A6A: 18 42                         r:=          b.0x8
08003A6C: 1A 4C 85                      w move       b.0x30,r.0x14
08003A6F: 19 CD A2 86                   by move      $0xA2,r.0x18
08003A73: 1A C4 08 00 2C D0 87          w move       $0x8002CD0,r.0x1C
08003A7A: C3 08 00 36 34 00             call         $0x8003634,$0x0
08003A80: 9D                            ifkret
08003A81: 44 D0                         w test       r1
08003A83: C4 06                         if = go      $0x6
08003A85: 84                            bi1 clr
08003A86: 80                            ret
08003A87: C0 2C                         go           $0x2C
08003A89: 18 42                         r:=          b.0x8
08003A8B: FE 79 C4 08 00 2F B0 85 03    w bmove      $0x8002FB0,r.0x14,$0x3
08003A94: 1A 4C 88                      w move       b.0x30,r.0x20
08003A97: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
08003A9D: 9D                            ifkret
08003A9E: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08003AA4: 9D                            ifkret
08003AA5: 0C CD B4                      w1 :=        $0xB4
08003AA8: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08003AAE: 9D                            ifkret
08003AAF: 0C CD B4                      w1 :=        $0xB4
08003AB2: 81                            retk
08003AB3: C0 23                         go           $0x23
08003AB5: 18 42                         r:=          b.0x8
08003AB7: 4D 85                         w set1       r.0x14
08003AB9: FE 79 C4 08 00 2F D0 86 03    w bmove      $0x8002FD0,r.0x18,$0x3
08003AC2: FE 79 C4 08 00 2F DC 89 03    w bmove      $0x8002FDC,r.0x24,$0x3
08003ACB: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
08003AD1: 9D                            ifkret
08003AD2: 0C CD C6                      w1 :=        $0xC6
08003AD5: 81                            retk
08003AD6: B8 CF 00 00 00 48             ents         $0x48
08003ADC: 4A 46                         w stz        b.0x18
08003ADE: 44 45                         w test       b.0x14
08003AE0: C6 07                         if >< go     $0x7
08003AE2: 84                            bi1 clr
08003AE3: 80                            ret
08003AE4: C1 00 89                      go           $0x89
08003AE7: 1A 45 48                      w move       b.0x14,b.0x20
08003AEA: 44 48                         w test       b.0x20
08003AEC: C5 00 7E                      if = go      $0x7E
08003AEF: C3 08 00 86 91 00             call         $0x8008691,$0x0
08003AF5: 9D                            ifkret
08003AF6: 20 4B                         w1 =:        b.0x2C
08003AF8: FD 3D C5 20                   w2 laddr     @b.0x20
08003AFC: 21 4F                         w2 =:        b.0x3C
08003AFE: 1A 14 4E                      w move       $0x14,b.0x38
08003B01: FD 3E F4 00                   w3 laddr     r1.(0x0)
08003B05: 22 51                         w3 =:        b.0x44
08003B07: 1A 14 50                      w move       $0x14,b.0x40
08003B0A: CA 08                         if < go      $0x8
08003B0C: 84                            bi1 clr
08003B0D: 85                            bi2 clr
08003B0E: FD 67 4E 50                   by smove     b.0x38,b.0x40
08003B12: 18 48                         r:=          b.0x20
08003B14: 1A 85 49                      w move       r.0x14,b.0x24
08003B17: 44 46                         w test       b.0x18
08003B19: C6 07                         if >< go     $0x7
08003B1B: 1A 4B 46                      w move       b.0x2C,b.0x18
08003B1E: C0 07                         go           $0x7
08003B20: 18 4D                         r:=          b.0x34
08003B22: 1A 4B 86                      w move       b.0x2C,r.0x18
08003B25: 0C 4B                         w1 :=        b.0x2C
08003B27: 20 4D                         w1 =:        b.0x34
08003B29: 20 4C                         w1 =:        b.0x30
08003B2B: 44 49                         w test       b.0x24
08003B2D: C4 34                         if = go      $0x34
08003B2F: C3 08 00 86 91 00             call         $0x8008691,$0x0
08003B35: 9D                            ifkret
08003B36: 20 4A                         w1 =:        b.0x28
08003B38: FD 3D C5 24                   w2 laddr     @b.0x24
08003B3C: 21 4F                         w2 =:        b.0x3C
08003B3E: 1A 14 4E                      w move       $0x14,b.0x38
08003B41: FD 3E F4 00                   w3 laddr     r1.(0x0)
08003B45: 22 51                         w3 =:        b.0x44
08003B47: 1A 14 50                      w move       $0x14,b.0x40
08003B4A: CA 08                         if < go      $0x8
08003B4C: 84                            bi1 clr
08003B4D: 85                            bi2 clr
08003B4E: FD 67 4E 50                   by smove     b.0x38,b.0x40
08003B52: 18 4C                         r:=          b.0x30
08003B54: 1A 4A 85                      w move       b.0x28,r.0x14
08003B57: 1A 4A 4C                      w move       b.0x28,b.0x30
08003B5A: 18 49                         r:=          b.0x24
08003B5C: 1A 85 49                      w move       r.0x14,b.0x24
08003B5F: C0 CC                         go           $0xFFFFFFFFFFFFFFCC
08003B61: 18 48                         r:=          b.0x20
08003B63: 1A 86 48                      w move       r.0x18,b.0x20
08003B66: 44 48                         w test       b.0x20
08003B68: C6 87                         if >< go     $0xFFFFFFFFFFFFFF87
08003B6A: 0C 01                         w1 :=        $0x1
08003B6C: 80                            ret
08003B6D: B8 CF 00 00 00 48             ents         $0x48
08003B73: 84                            bi1 clr
08003B74: 20 46                         w1 =:        b.0x18
08003B76: 20 47                         w1 =:        b.0x1C
08003B78: 44 45                         w test       b.0x14
08003B7A: C6 07                         if >< go     $0x7
08003B7C: 84                            bi1 clr
08003B7D: 80                            ret
08003B7E: C1 00 90                      go           $0x90
08003B81: 1A 45 49                      w move       b.0x14,b.0x24
08003B84: 44 49                         w test       b.0x24
08003B86: C4 3F                         if = go      $0x3F
08003B88: C3 08 00 86 91 00             call         $0x8008691,$0x0
08003B8E: 9D                            ifkret
08003B8F: 20 4A                         w1 =:        b.0x28
08003B91: FD 3D C5 24                   w2 laddr     @b.0x24
08003B95: 21 4F                         w2 =:        b.0x3C
08003B97: 1A 14 4E                      w move       $0x14,b.0x38
08003B9A: FD 3E F4 00                   w3 laddr     r1.(0x0)
08003B9E: 22 51                         w3 =:        b.0x44
08003BA0: 1A 14 50                      w move       $0x14,b.0x40
08003BA3: CA 08                         if < go      $0x8
08003BA5: 84                            bi1 clr
08003BA6: 85                            bi2 clr
08003BA7: FD 67 4E 50                   by smove     b.0x38,b.0x40
08003BAB: 44 46                         w test       b.0x18
08003BAD: C6 07                         if >< go     $0x7
08003BAF: 1A 4A 46                      w move       b.0x28,b.0x18
08003BB2: C0 07                         go           $0x7
08003BB4: 18 4B                         r:=          b.0x2C
08003BB6: 1A 4A 85                      w move       b.0x28,r.0x14
08003BB9: 1A 4A 4B                      w move       b.0x28,b.0x2C
08003BBC: 18 49                         r:=          b.0x24
08003BBE: 1A 85 49                      w move       r.0x14,b.0x24
08003BC1: 44 49                         w test       b.0x24
08003BC3: C6 C5                         if >< go     $0xFFFFFFFFFFFFFFC5
08003BC5: 18 45                         r:=          b.0x14
08003BC7: 1A 86 49                      w move       r.0x18,b.0x24
08003BCA: 44 49                         w test       b.0x24
08003BCC: C4 3F                         if = go      $0x3F
08003BCE: C3 08 00 86 91 00             call         $0x8008691,$0x0
08003BD4: 9D                            ifkret
08003BD5: 20 4C                         w1 =:        b.0x30
08003BD7: FD 3D C5 24                   w2 laddr     @b.0x24
08003BDB: 21 4F                         w2 =:        b.0x3C
08003BDD: 1A 14 4E                      w move       $0x14,b.0x38
08003BE0: FD 3E F4 00                   w3 laddr     r1.(0x0)
08003BE4: 22 51                         w3 =:        b.0x44
08003BE6: 1A 14 50                      w move       $0x14,b.0x40
08003BE9: CA 08                         if < go      $0x8
08003BEB: 84                            bi1 clr
08003BEC: 85                            bi2 clr
08003BED: FD 67 4E 50                   by smove     b.0x38,b.0x40
08003BF1: 44 47                         w test       b.0x1C
08003BF3: C6 07                         if >< go     $0x7
08003BF5: 1A 4C 47                      w move       b.0x30,b.0x1C
08003BF8: C0 07                         go           $0x7
08003BFA: 18 4D                         r:=          b.0x34
08003BFC: 1A 4C 85                      w move       b.0x30,r.0x14
08003BFF: 1A 4C 4D                      w move       b.0x30,b.0x34
08003C02: 18 49                         r:=          b.0x24
08003C04: 1A 85 49                      w move       r.0x14,b.0x24
08003C07: 44 49                         w test       b.0x24
08003C09: C6 C5                         if >< go     $0xFFFFFFFFFFFFFFC5
08003C0B: 0C 01                         w1 :=        $0x1
08003C0D: 80                            ret
08003C0E: 9C                            entd
08003C0F: FD C0 63                      l=:          b.0x8C
08003C12: 2D 64 C4 08 00 32 F8          by comp2     b.0x90,$0x80032F8
08003C19: D4 31                         if >> go     $0x31
08003C1B: 04 64                         by1 :=       b.0x90
08003C1D: B4 E0 08 00 32 FC             jumpg        $0x80032FC+
08003C23: 84                            bi1 clr
08003C24: FE 03                         clrk
08003C26: B4 63                         jumpg        b.0x8C
08003C28: C0 49                         go           $0x49
08003C2A: 0C 01                         w1 :=        $0x1
08003C2C: FE 03                         clrk
08003C2E: B4 63                         jumpg        b.0x8C
08003C30: C0 41                         go           $0x41
08003C32: 0C 02                         w1 :=        $0x2
08003C34: FE 03                         clrk
08003C36: B4 63                         jumpg        b.0x8C
08003C38: C0 39                         go           $0x39
08003C3A: 0C 03                         w1 :=        $0x3
08003C3C: FE 03                         clrk
08003C3E: B4 63                         jumpg        b.0x8C
08003C40: C0 31                         go           $0x31
08003C42: 0C 04                         w1 :=        $0x4
08003C44: FE 03                         clrk
08003C46: B4 63                         jumpg        b.0x8C
08003C48: C0 29                         go           $0x29
08003C4A: 18 42                         r:=          b.0x8
08003C4C: 4D 85                         w set1       r.0x14
08003C4E: FE 79 C4 08 00 32 E0 86 03    w bmove      $0x80032E0,r.0x18,$0x3
08003C57: FE 79 C4 08 00 32 EC 89 03    w bmove      $0x80032EC,r.0x24,$0x3
08003C60: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
08003C66: D2 04                         if -k go     $0x4
08003C68: B4 63                         jumpg        b.0x8C
08003C6A: 0C CD C6                      w1 :=        $0xC6
08003C6D: FE 02                         setk
08003C6F: B4 63                         jumpg        b.0x8C
08003C71: 9C                            entd
08003C72: FD C0 66                      l=:          b.0x98
08003C75: 2D C5 24 CD A8                by comp2     @b.0x24,$0xA8
08003C7A: C6 1C                         if >< go     $0x1C
08003C7C: 18 49                         r:=          b.0x24
08003C7E: 4A 84                         w stz        r.0x10
08003C80: 1A 47 83                      w move       b.0x1C,r.0xC
08003C83: 0C 49                         w1 :=        b.0x24
08003C85: 20 67                         w1 =:        b.0x9C
08003C87: 1A F4 08 49                   w move       r1.(0x8),b.0x24
08003C8B: 85                            bi2 clr
08003C8C: 18 47                         r:=          b.0x1C
08003C8E: 52 82 D1                      w swap       r.0x8,r2
08003C91: 21 F4 08                      w2 =:        r1.(0x8)
08003C94: C0 26                         go           $0x26
08003C96: 18 49                         r:=          b.0x24
08003C98: 1A 47 84                      w move       b.0x1C,r.0x10
08003C9B: 18 47                         r:=          b.0x1C
08003C9D: 0C 82                         w1 :=        r.0x8
08003C9F: 18 49                         r:=          b.0x24
08003CA1: 20 83                         w1 =:        r.0xC
08003CA3: 0E 49                         w3 :=        b.0x24
08003CA5: 22 67                         w3 =:        b.0x9C
08003CA7: 1A F6 08 49                   w move       r3.(0x8),b.0x24
08003CAB: 87                            bi4 clr
08003CAC: 18 47                         r:=          b.0x1C
08003CAE: 18 82                         r:=          r.0x8
08003CB0: 52 82 D3                      w swap       r.0x8,r4
08003CB3: 23 F6 08                      w4 =:        r3.(0x8)
08003CB6: 18 47                         r:=          b.0x1C
08003CB8: 4A 82                         w stz        r.0x8
08003CBA: 1A 67 47                      w move       b.0x9C,b.0x1C
08003CBD: FE 03                         clrk
08003CBF: B4 66                         jumpg        b.0x98
08003CC1: 9C                            entd
08003CC2: FD C0 68                      l=:          b.0xA0
08003CC5: C3 08 00 34 CC 00             call         $0x80034CC,$0x0
08003CCB: D2 04                         if -k go     $0x4
08003CCD: B4 68                         jumpg        b.0xA0
08003CCF: FD 3D 47                      w2 laddr     b.0x1C
08003CD2: 20 6F                         w1 =:        b.0xBC
08003CD4: 0C 6F                         w1 :=        b.0xBC
08003CD6: C4 0A                         if = go      $0xA
08003CD8: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
08003CDD: 20 F5 00                      w1 =:        r2.(0x0)
08003CE0: 18 47                         r:=          b.0x1C
08003CE2: 19 CD B7 C5 1C                by move      $0xB7,@b.0x1C
08003CE7: 4A 6E                         w stz        b.0xB8
08003CE9: 0C 6A                         w1 :=        b.0xA8
08003CEB: 20 6C                         w1 =:        b.0xB0
08003CED: 0D 6B                         w2 :=        b.0xAC
08003CEF: 21 6F                         w2 =:        b.0xBC
08003CF1: 34 D1                         w1 comp      r2
08003CF3: C8 4B                         if > go      $0x4B
08003CF5: 44 6E                         w test       b.0xB8
08003CF7: C4 07                         if = go      $0x7
08003CF9: 2E 6D 13                      w comp2      b.0xB4,$0x13
08003CFC: C6 26                         if >< go     $0x26
08003CFE: C3 08 00 86 91 00             call         $0x8008691,$0x0
08003D04: D2 04                         if -k go     $0x4
08003D06: B4 68                         jumpg        b.0xA0
08003D08: 20 6E                         w1 =:        b.0xB8
08003D0A: 1A 3F 6D                      w move       $0x3F,b.0xB4
08003D0D: 18 47                         r:=          b.0x1C
08003D0F: FD 3D 81                      w2 laddr     r.0x4
08003D12: 0C 6E                         w1 :=        b.0xB8
08003D14: 0E 14                         w3 :=        $0x14
08003D16: FE 03                         clrk
08003D18: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08003D1E: D2 04                         if -k go     $0x4
08003D20: B4 68                         jumpg        b.0xA0
08003D22: 0C 6D                         w1 :=        b.0xB4
08003D24: 54 01                         w1 +         $0x1
08003D26: 20 6D                         w1 =:        b.0xB4
08003D28: 0E 6C                         w3 :=        b.0xB0
08003D2A: 05 E6 A4                      by2 :=       @b.0xFFFFFFFFFFFFFFA4+
08003D2D: FD 3F C5 B8                   w4 laddr     @b.0xFFFFFFFFFFFFFFB8
08003D31: 57 D0                         w4 +         r1
08003D33: 1D F7 00                      by2 =:       r4.(0x0)
08003D36: 31 0D                         by2 comp     $0xD
08003D38: C4 06                         if = go      $0x6
08003D3A: BF 6C 6F BB                   d loopi      b.0xB0,b.0xBC,$0xFFFFFFFFFFFFFFBB
08003D3E: FE 03                         clrk
08003D40: B4 68                         jumpg        b.0xA0
08003D42: 9C                            entd
08003D43: FD C0 70                      l=:          b.0xC0
08003D46: 2D 71 CD 9E                   by comp2     b.0xC4,$0x9E
08003D4A: C6 36                         if >< go     $0x36
08003D4C: 2D C5 24 CD 9D                by comp2     @b.0x24,$0x9D
08003D51: C4 0E                         if = go      $0xE
08003D53: C3 08 00 3C 71 00             call         $0x8003C71,$0x0
08003D59: D2 04                         if -k go     $0x4
08003D5B: B4 70                         jumpg        b.0xC0
08003D5D: C0 EF                         go           $0xFFFFFFFFFFFFFFEF
08003D5F: 0D 49                         w2 :=        b.0x24
08003D61: 21 72                         w2 =:        b.0xC8
08003D63: 1A F5 08 49                   w move       r2.(0x8),b.0x24
08003D67: 4A F5 08                      w stz        r2.(0x8)
08003D6A: 18 42                         r:=          b.0x8
08003D6C: 21 85                         w2 =:        r.0x14
08003D6E: C3 08 00 35 92 00             call         $0x8003592,$0x0
08003D74: D2 04                         if -k go     $0x4
08003D76: B4 70                         jumpg        b.0xC0
08003D78: 18 42                         r:=          b.0x8
08003D7A: 1A 85 72                      w move       r.0x14,b.0xC8
08003D7D: C1 00 7F                      go           $0x7F
08003D80: 2D 71 CD 9D                   by comp2     b.0xC4,$0x9D
08003D84: C6 25                         if >< go     $0x25
08003D86: C3 08 00 34 CC 00             call         $0x80034CC,$0x0
08003D8C: D2 04                         if -k go     $0x4
08003D8E: B4 70                         jumpg        b.0xC0
08003D90: FD 3D 49                      w2 laddr     b.0x24
08003D93: 20 73                         w1 =:        b.0xCC
08003D95: 0C 73                         w1 :=        b.0xCC
08003D97: C4 0A                         if = go      $0xA
08003D99: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
08003D9E: 20 F5 00                      w1 =:        r2.(0x0)
08003DA1: 18 49                         r:=          b.0x24
08003DA3: 19 71 C5 24                   by move      b.0xC4,@b.0x24
08003DA7: C0 55                         go           $0x55
08003DA9: 44 49                         w test       b.0x24
08003DAB: C4 30                         if = go      $0x30
08003DAD: 19 71 64                      by move      b.0xC4,b.0x90
08003DB0: C3 08 00 3C 0E 00             call         $0x8003C0E,$0x0
08003DB6: D2 04                         if -k go     $0x4
08003DB8: B4 70                         jumpg        b.0xC0
08003DBA: 19 C5 24 64                   by move      @b.0x24,b.0x90
08003DBE: 20 73                         w1 =:        b.0xCC
08003DC0: C3 08 00 3C 0E 00             call         $0x8003C0E,$0x0
08003DC6: D2 04                         if -k go     $0x4
08003DC8: B4 70                         jumpg        b.0xC0
08003DCA: 2E 73 D0                      w comp2      b.0xCC,r1
08003DCD: C8 0E                         if > go      $0xE
08003DCF: C3 08 00 3C 71 00             call         $0x8003C71,$0x0
08003DD5: D2 04                         if -k go     $0x4
08003DD7: B4 70                         jumpg        b.0xC0
08003DD9: C0 D0                         go           $0xFFFFFFFFFFFFFFD0
08003DDB: C3 08 00 34 CC 00             call         $0x80034CC,$0x0
08003DE1: D2 04                         if -k go     $0x4
08003DE3: B4 70                         jumpg        b.0xC0
08003DE5: FD 3D 49                      w2 laddr     b.0x24
08003DE8: 20 73                         w1 =:        b.0xCC
08003DEA: 0C 73                         w1 :=        b.0xCC
08003DEC: C4 0A                         if = go      $0xA
08003DEE: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
08003DF3: 20 F5 00                      w1 =:        r2.(0x0)
08003DF6: 18 49                         r:=          b.0x24
08003DF8: 19 71 C5 24                   by move      b.0xC4,@b.0x24
08003DFC: FE 03                         clrk
08003DFE: B4 70                         jumpg        b.0xC0
08003E00: 9C                            entd
08003E01: FD C0 74                      l=:          b.0xD0
08003E04: 2D C1 21 C4 08 00 35 DC       by comp2     b.0x21,$0x80035DC
08003E0C: D4 34                         if >> go     $0x34
08003E0E: 04 C1 21                      by1 :=       b.0x21
08003E11: B4 E0 08 00 35 E0             jumpg        $0x80035E0+
08003E17: 19 CD B7 71                   by move      $0xB7,b.0xC4
08003E1B: C3 08 00 3D 42 00             call         $0x8003D42,$0x0
08003E21: D2 04                         if -k go     $0x4
08003E23: B4 74                         jumpg        b.0xD0
08003E25: C0 47                         go           $0x47
08003E27: FD 3C 4A                      w1 laddr     b.0x28
08003E2A: 20 69                         w1 =:        b.0xA4
08003E2C: 4A 6A                         w stz        b.0xA8
08003E2E: 1A CD 63 6B                   w move       $0x63,b.0xAC
08003E32: C3 08 00 3C C1 00             call         $0x8003CC1,$0x0
08003E38: D2 04                         if -k go     $0x4
08003E3A: B4 74                         jumpg        b.0xD0
08003E3C: C0 30                         go           $0x30
08003E3E: C0 2E                         go           $0x2E
08003E40: 2D C1 21 CD 83                by comp2     b.0x21,$0x83
08003E45: D8 12                         if << go     $0x12
08003E47: 19 C1 21 71                   by move      b.0x21,b.0xC4
08003E4B: C3 08 00 3D 42 00             call         $0x8003D42,$0x0
08003E51: D2 04                         if -k go     $0x4
08003E53: B4 74                         jumpg        b.0xD0
08003E55: C0 17                         go           $0x17
08003E57: FD 3D 4A                      w2 laddr     b.0x28
08003E5A: 21 69                         w2 =:        b.0xA4
08003E5C: 4A 6A                         w stz        b.0xA8
08003E5E: 1A CD 63 6B                   w move       $0x63,b.0xAC
08003E62: C3 08 00 3C C1 00             call         $0x8003CC1,$0x0
08003E68: D2 04                         if -k go     $0x4
08003E6A: B4 74                         jumpg        b.0xD0
08003E6C: FE 03                         clrk
08003E6E: B4 74                         jumpg        b.0xD0
08003E70: B8 CF 00 00 00 D4             ents         $0xD4
08003E76: 84                            bi1 clr
08003E77: 20 47                         w1 =:        b.0x1C
08003E79: 20 49                         w1 =:        b.0x24
08003E7B: FD 3C C5 14                   w1 laddr     @b.0x14
08003E7F: 54 46                         w1 +         b.0x18
08003E81: 2D F4 00 0D                   by comp2     r1.(0x0),$0xD
08003E85: C4 61                         if = go      $0x61
08003E87: 18 42                         r:=          b.0x8
08003E89: 1A 45 85                      w move       b.0x14,r.0x14
08003E8C: 1A 46 86                      w move       b.0x18,r.0x18
08003E8F: FD 3D 4A                      w2 laddr     b.0x28
08003E92: 21 87                         w2 =:        r.0x1C
08003E94: 4A 88                         w stz        r.0x20
08003E96: 1A CD 63 89                   w move       $0x63,r.0x24
08003E9A: 19 48 8A                      by move      b.0x20,r.0x28
08003E9D: C3 08 00 37 58 00             call         $0x8003758,$0x0
08003EA3: 9D                            ifkret
08003EA4: 18 42                         r:=          b.0x8
08003EA6: 1A 85 45                      w move       r.0x14,b.0x14
08003EA9: 1A 86 46                      w move       r.0x18,b.0x18
08003EAC: 19 8A 48                      by move      r.0x28,b.0x20
08003EAF: 85                            bi2 clr
08003EB0: 19 D5 28 C1 21                by move      b.0x28+,b.0x21
08003EB5: C3 08 00 3E 00 00             call         $0x8003E00,$0x0
08003EBB: 9D                            ifkret
08003EBC: 05 C1 21                      by2 :=       b.0x21
08003EBF: 35 CE 00 9C                   w2 comp      $0x9C
08003EC3: C4 23                         if = go      $0x23
08003EC5: 05 48                         by2 :=       b.0x20
08003EC7: 1D C1 21                      by2 =:       b.0x21
08003ECA: 86                            bi3 clr
08003ECB: 1D D6 28                      by2 =:       b.0x28+
08003ECE: 0F 01                         w4 :=        $0x1
08003ED0: 19 0D D7 28                   by move      $0xD,b.0x28+
08003ED4: C3 08 00 3E 00 00             call         $0x8003E00,$0x0
08003EDA: 9D                            ifkret
08003EDB: 05 C1 21                      by2 :=       b.0x21
08003EDE: 35 CE 00 9C                   w2 comp      $0x9C
08003EE2: C4 04                         if = go      $0x4
08003EE4: C0 97                         go           $0xFFFFFFFFFFFFFF97
08003EE6: 18 42                         r:=          b.0x8
08003EE8: 1A 49 85                      w move       b.0x24,r.0x14
08003EEB: C3 08 00 35 92 00             call         $0x8003592,$0x0
08003EF1: 9D                            ifkret
08003EF2: 18 42                         r:=          b.0x8
08003EF4: 1A 85 49                      w move       r.0x14,b.0x24
08003EF7: 80                            ret
08003EF8: 9C                            entd
08003EF9: FD C0 4F                      l=:          b.0x3C
08003EFC: 0C 48                         w1 :=        b.0x20
08003EFE: 54 01                         w1 +         $0x1
08003F00: 20 48                         w1 =:        b.0x20
08003F02: 34 13                         w1 comp      $0x13
08003F04: CE 09                         if <= go     $0x9
08003F06: 18 49                         r:=          b.0x24
08003F08: 1A 85 49                      w move       r.0x14,b.0x24
08003F0B: 4A 48                         w stz        b.0x20
08003F0D: FE 03                         clrk
08003F0F: B4 4F                         jumpg        b.0x3C
08003F11: 9C                            entd
08003F12: FD C0 50                      l=:          b.0x40
08003F15: FD 3C C5 24                   w1 laddr     @b.0x24
08003F19: 54 48                         w1 +         b.0x20
08003F1B: 19 F4 00 47                   by move      r1.(0x0),b.0x1C
08003F1F: C3 08 00 3E F8 00             call         $0x8003EF8,$0x0
08003F25: D2 04                         if -k go     $0x4
08003F27: B4 50                         jumpg        b.0x40
08003F29: 05 47                         by2 :=       b.0x1C
08003F2B: 35 CE 00 9C                   w2 comp      $0x9C
08003F2F: C4 04                         if = go      $0x4
08003F31: C0 E4                         go           $0xFFFFFFFFFFFFFFE4
08003F33: FE 03                         clrk
08003F35: B4 50                         jumpg        b.0x40
08003F37: B8 CF 00 00 00 48             ents         $0x48
08003F3D: 1A 45 49                      w move       b.0x14,b.0x24
08003F40: 4A 48                         w stz        b.0x20
08003F42: FD 3D C5 24                   w2 laddr     @b.0x24
08003F46: 55 48                         w2 +         b.0x20
08003F48: 04 F5 00                      by1 :=       r2.(0x0)
08003F4B: 1C 47                         by1 =:       b.0x1C
08003F4D: 30 0D                         by1 comp     $0xD
08003F4F: C5 02 AF                      if = go      $0x2AF
08003F52: C3 08 00 3E F8 00             call         $0x8003EF8,$0x0
08003F58: 9D                            ifkret
08003F59: 2D 47 C4 08 00 38 B8          by comp2     b.0x1C,$0x80038B8
08003F60: D5 02 7A                      if >> go     $0x27A
08003F63: 05 47                         by2 :=       b.0x1C
08003F65: B4 E1 08 00 38 BC             jumpg        $0x80038BC+
08003F6B: C1 02 90                      go           $0x290
08003F6E: C3 08 00 37 30 00             call         $0x8003730,$0x0
08003F74: 9D                            ifkret
08003F75: 44 D0                         w test       r1
08003F77: C4 10                         if = go      $0x10
08003F79: C3 08 00 93 C2 00             call         $0x80093C2,$0x0
08003F7F: 9D                            ifkret
08003F80: C3 08 00 A6 E8 00             call         $0x800A6E8,$0x0
08003F86: 9D                            ifkret
08003F87: C1 02 74                      go           $0x274
08003F8A: C3 08 00 37 30 00             call         $0x8003730,$0x0
08003F90: 9D                            ifkret
08003F91: 44 D0                         w test       r1
08003F93: C4 09                         if = go      $0x9
08003F95: C3 08 00 AB 36 00             call         $0x800AB36,$0x0
08003F9B: 9D                            ifkret
08003F9C: C1 02 5F                      go           $0x25F
08003F9F: 44 C4 08 00 2C 48             w test       $0x8002C48
08003FA5: C4 0C                         if = go      $0xC
08003FA7: 18 C4 08 00 2C 48             r:=          $0x8002C48
08003FAD: 44 80                         w test       r.0x0
08003FAF: C4 6C                         if = go      $0x6C
08003FB1: 18 42                         r:=          b.0x8
08003FB3: 1A 49 85                      w move       b.0x24,r.0x14
08003FB6: 1A 48 86                      w move       b.0x20,r.0x18
08003FB9: 1A 4D 87                      w move       b.0x34,r.0x1C
08003FBC: C3 08 00 3E 70 00             call         $0x8003E70,$0x0
08003FC2: 9D                            ifkret
08003FC3: 18 42                         r:=          b.0x8
08003FC5: 1A 85 49                      w move       r.0x14,b.0x24
08003FC8: 1A 86 48                      w move       r.0x18,b.0x20
08003FCB: 1A 87 4D                      w move       r.0x1C,b.0x34
08003FCE: C3 08 00 35 BC 00             call         $0x80035BC,$0x0
08003FD4: 9D                            ifkret
08003FD5: 0D CF 08 00 2C 48             w2 :=        $0x8002C48
08003FDB: 20 51                         w1 =:        b.0x44
08003FDD: 0C 51                         w1 :=        b.0x44
08003FDF: C4 0A                         if = go      $0xA
08003FE1: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
08003FE6: 20 F5 00                      w1 =:        r2.(0x0)
08003FE9: 18 42                         r:=          b.0x8
08003FEB: 1A 4D 85                      w move       b.0x34,r.0x14
08003FEE: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
08003FF4: 9D                            ifkret
08003FF5: 18 C4 08 00 2C 48             r:=          $0x8002C48
08003FFB: 20 80                         w1 =:        r.0x0
08003FFD: 44 D0                         w test       r1
08003FFF: C4 07                         if = go      $0x7
08004001: 1A 02 81                      w move       $0x2,r.0x4
08004004: C0 04                         go           $0x4
08004006: 4D 81                         w set1       r.0x4
08004008: 18 42                         r:=          b.0x8
0800400A: 1A 4D 85                      w move       b.0x34,r.0x14
0800400D: C3 08 00 35 92 00             call         $0x8003592,$0x0
08004013: 9D                            ifkret
08004014: 18 42                         r:=          b.0x8
08004016: 1A 85 4D                      w move       r.0x14,b.0x34
08004019: C0 2F                         go           $0x2F
0800401B: C3 08 00 3F 11 00             call         $0x8003F11,$0x0
08004021: 9D                            ifkret
08004022: C3 08 00 35 BC 00             call         $0x80035BC,$0x0
08004028: 9D                            ifkret
08004029: 0D CF 08 00 2C 48             w2 :=        $0x8002C48
0800402F: 20 51                         w1 =:        b.0x44
08004031: 0C 51                         w1 :=        b.0x44
08004033: C4 0A                         if = go      $0xA
08004035: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
0800403A: 20 F5 00                      w1 =:        r2.(0x0)
0800403D: 18 C4 08 00 2C 48             r:=          $0x8002C48
08004043: 4A 80                         w stz        r.0x0
08004045: 1A 02 81                      w move       $0x2,r.0x4
08004048: C1 01 B3                      go           $0x1B3
0800404B: 18 C4 08 00 2C 48             r:=          $0x8002C48
08004051: 2E 81 01                      w comp2      r.0x4,$0x1
08004054: C6 4B                         if >< go     $0x4B
08004056: 18 42                         r:=          b.0x8
08004058: 1A 49 85                      w move       b.0x24,r.0x14
0800405B: 1A 48 86                      w move       b.0x20,r.0x18
0800405E: 1A 4D 87                      w move       b.0x34,r.0x1C
08004061: C3 08 00 3E 70 00             call         $0x8003E70,$0x0
08004067: 9D                            ifkret
08004068: 18 42                         r:=          b.0x8
0800406A: 1A 85 49                      w move       r.0x14,b.0x24
0800406D: 1A 86 48                      w move       r.0x18,b.0x20
08004070: 1A 87 4D                      w move       r.0x1C,b.0x34
08004073: 1A 4D 85                      w move       b.0x34,r.0x14
08004076: C3 08 00 37 D9 00             call         $0x80037D9,$0x0
0800407C: 9D                            ifkret
0800407D: 18 C4 08 00 2C 48             r:=          $0x8002C48
08004083: 20 80                         w1 =:        r.0x0
08004085: 44 D0                         w test       r1
08004087: C4 05                         if = go      $0x5
08004089: 1A 02 81                      w move       $0x2,r.0x4
0800408C: 18 42                         r:=          b.0x8
0800408E: 1A 4D 85                      w move       b.0x34,r.0x14
08004091: C3 08 00 35 92 00             call         $0x8003592,$0x0
08004097: 9D                            ifkret
08004098: 18 42                         r:=          b.0x8
0800409A: 1A 85 4D                      w move       r.0x14,b.0x34
0800409D: C0 11                         go           $0x11
0800409F: C3 08 00 3F 11 00             call         $0x8003F11,$0x0
080040A5: 9D                            ifkret
080040A6: 18 C4 08 00 2C 48             r:=          $0x8002C48
080040AC: 4A 80                         w stz        r.0x0
080040AE: C1 01 4D                      go           $0x14D
080040B1: 18 C4 08 00 2C 48             r:=          $0x8002C48
080040B7: 2E 81 01                      w comp2      r.0x4,$0x1
080040BA: C6 09                         if >< go     $0x9
080040BC: 4D 80                         w set1       r.0x0
080040BE: 1A 02 81                      w move       $0x2,r.0x4
080040C1: C0 04                         go           $0x4
080040C3: 4A 80                         w stz        r.0x0
080040C5: C1 01 36                      go           $0x136
080040C8: 0C C4 08 00 2C 48             w1 :=        $0x8002C48
080040CE: 20 4E                         w1 =:        b.0x38
080040D0: 1A F4 08 C4 08 00 2C 48       w move       r1.(0x8),$0x8002C48
080040D8: 4A F4 08                      w stz        r1.(0x8)
080040DB: 18 42                         r:=          b.0x8
080040DD: 20 85                         w1 =:        r.0x14
080040DF: C3 08 00 36 01 00             call         $0x8003601,$0x0
080040E5: 9D                            ifkret
080040E6: 18 42                         r:=          b.0x8
080040E8: 1A 85 4E                      w move       r.0x14,b.0x38
080040EB: C1 01 10                      go           $0x110
080040EE: C3 08 00 37 30 00             call         $0x8003730,$0x0
080040F4: 9D                            ifkret
080040F5: 44 D0                         w test       r1
080040F7: C4 1B                         if = go      $0x1B
080040F9: FD 3D C5 24                   w2 laddr     @b.0x24
080040FD: 55 48                         w2 +         b.0x20
080040FF: 04 F5 00                      by1 :=       r2.(0x0)
08004102: 18 42                         r:=          b.0x8
08004104: 1C 85                         by1 =:       r.0x14
08004106: 1A 46 86                      w move       b.0x18,r.0x18
08004109: 04 47                         by1 :=       b.0x1C
0800410B: C3 08 00 90 E7 00             call         $0x80090E7,$0x0
08004111: 9D                            ifkret
08004112: C3 08 00 3E F8 00             call         $0x8003EF8,$0x0
08004118: 9D                            ifkret
08004119: C1 00 E2                      go           $0xE2
0800411C: C3 08 00 37 30 00             call         $0x8003730,$0x0
08004122: 9D                            ifkret
08004123: 44 D0                         w test       r1
08004125: C4 2B                         if = go      $0x2B
08004127: 18 49                         r:=          b.0x24
08004129: 1A 85 49                      w move       r.0x14,b.0x24
0800412C: 18 42                         r:=          b.0x8
0800412E: 1A 49 85                      w move       b.0x24,r.0x14
08004131: C3 08 00 3A D6 00             call         $0x8003AD6,$0x0
08004137: 9D                            ifkret
08004138: 18 42                         r:=          b.0x8
0800413A: 1A 86 4A                      w move       r.0x18,b.0x28
0800413D: 44 D0                         w test       r1
0800413F: C4 11                         if = go      $0x11
08004141: 1A 4A 85                      w move       b.0x28,r.0x14
08004144: C3 08 00 AC 02 00             call         $0x800AC02,$0x0
0800414A: 9D                            ifkret
0800414B: 18 42                         r:=          b.0x8
0800414D: 1A 85 4A                      w move       r.0x14,b.0x28
08004150: 80                            ret
08004151: C1 00 AA                      go           $0xAA
08004154: C3 08 00 37 30 00             call         $0x8003730,$0x0
0800415A: 9D                            ifkret
0800415B: 44 D0                         w test       r1
0800415D: C4 37                         if = go      $0x37
0800415F: 18 49                         r:=          b.0x24
08004161: 1A 85 49                      w move       r.0x14,b.0x24
08004164: 18 42                         r:=          b.0x8
08004166: 1A 49 85                      w move       b.0x24,r.0x14
08004169: C3 08 00 3B 6D 00             call         $0x8003B6D,$0x0
0800416F: 9D                            ifkret
08004170: 18 42                         r:=          b.0x8
08004172: 1A 86 4B                      w move       r.0x18,b.0x2C
08004175: 1A 87 4C                      w move       r.0x1C,b.0x30
08004178: 44 D0                         w test       r1
0800417A: C4 1A                         if = go      $0x1A
0800417C: 1A 4B 85                      w move       b.0x2C,r.0x14
0800417F: 1A 4C 86                      w move       b.0x30,r.0x18
08004182: 1A 46 87                      w move       b.0x18,r.0x1C
08004185: C3 08 00 A3 75 00             call         $0x800A375,$0x0
0800418B: 9D                            ifkret
0800418C: 18 42                         r:=          b.0x8
0800418E: 1A 85 4B                      w move       r.0x14,b.0x2C
08004191: 1A 86 4C                      w move       r.0x18,b.0x30
08004194: 80                            ret
08004195: C0 66                         go           $0x66
08004197: C3 08 00 37 30 00             call         $0x8003730,$0x0
0800419D: 9D                            ifkret
0800419E: 44 D0                         w test       r1
080041A0: C4 37                         if = go      $0x37
080041A2: 18 49                         r:=          b.0x24
080041A4: 1A 85 49                      w move       r.0x14,b.0x24
080041A7: 18 42                         r:=          b.0x8
080041A9: 1A 49 85                      w move       b.0x24,r.0x14
080041AC: C3 08 00 3B 6D 00             call         $0x8003B6D,$0x0
080041B2: 9D                            ifkret
080041B3: 18 42                         r:=          b.0x8
080041B5: 1A 86 4B                      w move       r.0x18,b.0x2C
080041B8: 1A 87 4C                      w move       r.0x1C,b.0x30
080041BB: 44 D0                         w test       r1
080041BD: C4 1A                         if = go      $0x1A
080041BF: 1A 4B 85                      w move       b.0x2C,r.0x14
080041C2: 1A 4C 86                      w move       b.0x30,r.0x18
080041C5: 1A 02 87                      w move       $0x2,r.0x1C
080041C8: C3 08 00 A3 75 00             call         $0x800A375,$0x0
080041CE: 9D                            ifkret
080041CF: 18 42                         r:=          b.0x8
080041D1: 1A 85 4B                      w move       r.0x14,b.0x2C
080041D4: 1A 86 4C                      w move       r.0x18,b.0x30
080041D7: 80                            ret
080041D8: C0 23                         go           $0x23
080041DA: 18 42                         r:=          b.0x8
080041DC: 4D 85                         w set1       r.0x14
080041DE: FE 79 C4 08 00 38 A0 86 03    w bmove      $0x80038A0,r.0x18,$0x3
080041E7: FE 79 C4 08 00 38 AC 89 03    w bmove      $0x80038AC,r.0x24,$0x3
080041F0: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
080041F6: 9D                            ifkret
080041F7: 0C CD C6                      w1 :=        $0xC6
080041FA: 81                            retk
080041FB: C1 FD 47                      go           $0xFFFFFFFFFFFFFD47
080041FE: 80                            ret
080041FF: B8 CF 00 00 00 28             ents         $0x28
08004205: 44 45                         w test       b.0x14
08004207: C6 0D                         if >< go     $0xD
08004209: 19 CD 83 C1 19                by move      $0x83,b.0x19
0800420E: 0C 01                         w1 :=        $0x1
08004210: 80                            ret
08004211: C1 00 93                      go           $0x93
08004214: 2E 45 01                      w comp2      b.0x14,$0x1
08004217: C6 2F                         if >< go     $0x2F
08004219: 04 C1 19                      by1 :=       b.0x19
0800421C: 6C 0C                         w1 *         $0xC
0800421E: FD 20 E0 08 00 74 A8 49 04    by bmove     $0x80074A8+,b.0x24,$0x4
08004227: 0E 01                         w3 :=        $0x1
08004229: 05 E6 24                      by2 :=       @b.0x24+
0800422C: 2D 46 D1                      by comp2     b.0x18,r2
0800422F: DA 15                         if <<= go    $0x15
08004231: 07 C1 19                      by4 :=       b.0x19
08004234: 37 CE 00 9C                   w4 comp      $0x9C
08004238: C6 06                         if >< go     $0x6
0800423A: 84                            bi1 clr
0800423B: 80                            ret
0800423C: C0 06                         go           $0x6
0800423E: FC 8A C1 19                   by incr      b.0x19
08004242: C0 D7                         go           $0xFFFFFFFFFFFFFFD7
08004244: C0 60                         go           $0x60
08004246: 06 C1 19                      by3 :=       b.0x19
08004249: 6E 0C                         w3 *         $0xC
0800424B: 0F 45                         w4 :=        b.0x14
0800424D: 63 01                         w4 -         $0x1
0800424F: FD 20 E2 08 00 74 A8 49 04    by bmove     $0x80074A8+,b.0x24,$0x4
08004258: 05 E7 24                      by2 :=       @b.0x24+
0800425B: 1D 48                         by2 =:       b.0x20
0800425D: 04 C1 19                      by1 :=       b.0x19
08004260: 6C 0C                         w1 *         $0xC
08004262: FD 20 E0 08 00 74 A8 49 04    by bmove     $0x80074A8+,b.0x24,$0x4
0800426B: 0E 45                         w3 :=        b.0x14
0800426D: 05 E6 24                      by2 :=       @b.0x24+
08004270: 2D 46 D1                      by comp2     b.0x18,r2
08004273: DA 31                         if <<= go    $0x31
08004275: 07 C1 19                      by4 :=       b.0x19
08004278: 37 CE 00 9C                   w4 comp      $0x9C
0800427C: C6 06                         if >< go     $0x6
0800427E: 84                            bi1 clr
0800427F: 80                            ret
08004280: C0 22                         go           $0x22
08004282: 07 C1 19                      by4 :=       b.0x19
08004285: FC 37 01                      by4 +        $0x1
08004288: 1F C1 19                      by4 =:       b.0x19
0800428B: 6F 0C                         w4 *         $0xC
0800428D: 62 01                         w3 -         $0x1
0800428F: 05 48                         by2 :=       b.0x20
08004291: FD 20 E3 08 00 74 A8 49 04    by bmove     $0x80074A8+,b.0x24,$0x4
0800429A: 2D E6 24 D1                   by comp2     @b.0x24+,r2
0800429E: C4 04                         if = go      $0x4
080042A0: 84                            bi1 clr
080042A1: 80                            ret
080042A2: C0 BB                         go           $0xFFFFFFFFFFFFFFBB
080042A4: 05 C1 19                      by2 :=       b.0x19
080042A7: 6D 0C                         w2 *         $0xC
080042A9: FD 20 E1 08 00 74 A8 49 04    by bmove     $0x80074A8+,b.0x24,$0x4
080042B2: 0F 45                         w4 :=        b.0x14
080042B4: 06 E7 24                      by3 :=       @b.0x24+
080042B7: 2D 46 D2                      by comp2     b.0x18,r3
080042BA: C6 07                         if >< go     $0x7
080042BC: 0C 01                         w1 :=        $0x1
080042BE: 80                            ret
080042BF: C0 04                         go           $0x4
080042C1: 84                            bi1 clr
080042C2: 80                            ret
080042C3: B8 CF 00 00 00 40             ents         $0x40
080042C9: FD 54 CE 00 83 48             w byconv     $0x83,b.0x20
080042CF: 04 48                         by1 :=       b.0x20
080042D1: 6C 0C                         w1 *         $0xC
080042D3: 05 CD 2D                      by2 :=       $0x2D
080042D6: FD 20 E0 08 00 74 A8 4C 04    by bmove     $0x80074A8+,b.0x30,$0x4
080042DF: 86                            bi3 clr
080042E0: 2D E6 30 D1                   by comp2     @b.0x30+,r2
080042E4: C6 3C                         if >< go     $0x3C
080042E6: 07 48                         by4 :=       b.0x20
080042E8: 6F 0C                         w4 *         $0xC
080042EA: 04 48                         by1 :=       b.0x20
080042EC: 6C 0C                         w1 *         $0xC
080042EE: FC 5A E0 08 00 74 A8          by rladdr    $0x80074A8+
080042F5: 0D 82                         w2 :=        r.0x8
080042F7: 1A D1 4F                      w move       r2,b.0x3C
080042FA: 1A 01 4E                      w move       $0x1,b.0x38
080042FD: FD 20 E3 08 00 74 A8 4D 04    by bmove     $0x80074A8+,b.0x34,$0x4
08004306: 18 42                         r:=          b.0x8
08004308: FD 20 4D 85 0C                by bmove     b.0x34,r.0x14,$0xC
0800430D: FD 20 45 88 0C                by bmove     b.0x14,r.0x20,$0xC
08004312: C3 08 00 85 F3 00             call         $0x80085F3,$0x0
08004318: 9D                            ifkret
08004319: 44 D0                         w test       r1
0800431B: C4 05                         if = go      $0x5
0800431D: 0C 01                         w1 :=        $0x1
0800431F: 80                            ret
08004320: 05 48                         by2 :=       b.0x20
08004322: 55 01                         w2 +         $0x1
08004324: 1D 48                         by2 =:       b.0x20
08004326: FC 1D CE 00 9C                h2 comp      $0x9C
0800432B: CE A4                         if <= go     $0xFFFFFFFFFFFFFFA4
0800432D: FD 54 CE 00 83 48             w byconv     $0x83,b.0x20
08004333: 04 48                         by1 :=       b.0x20
08004335: 6C 0C                         w1 *         $0xC
08004337: 05 CD 2B                      by2 :=       $0x2B
0800433A: FD 20 E0 08 00 74 A8 4C 04    by bmove     $0x80074A8+,b.0x30,$0x4
08004343: 86                            bi3 clr
08004344: 2D E6 30 D1                   by comp2     @b.0x30+,r2
08004348: C6 3C                         if >< go     $0x3C
0800434A: 07 48                         by4 :=       b.0x20
0800434C: 6F 0C                         w4 *         $0xC
0800434E: 04 48                         by1 :=       b.0x20
08004350: 6C 0C                         w1 *         $0xC
08004352: FC 5A E0 08 00 74 A8          by rladdr    $0x80074A8+
08004359: 0D 82                         w2 :=        r.0x8
0800435B: 1A D1 4F                      w move       r2,b.0x3C
0800435E: 1A 01 4E                      w move       $0x1,b.0x38
08004361: FD 20 E3 08 00 74 A8 4D 04    by bmove     $0x80074A8+,b.0x34,$0x4
0800436A: 18 42                         r:=          b.0x8
0800436C: FD 20 4D 85 0C                by bmove     b.0x34,r.0x14,$0xC
08004371: FD 20 45 88 0C                by bmove     b.0x14,r.0x20,$0xC
08004376: C3 08 00 86 21 00             call         $0x8008621,$0x0
0800437C: 9D                            ifkret
0800437D: 44 D0                         w test       r1
0800437F: C4 05                         if = go      $0x5
08004381: 0C 01                         w1 :=        $0x1
08004383: 80                            ret
08004384: 05 48                         by2 :=       b.0x20
08004386: 55 01                         w2 +         $0x1
08004388: 1D 48                         by2 =:       b.0x20
0800438A: FC 1D CE 00 9C                h2 comp      $0x9C
0800438F: CE A4                         if <= go     $0xFFFFFFFFFFFFFFA4
08004391: 4A 4A                         w stz        b.0x28
08004393: FD 54 CE 00 83 48             w byconv     $0x83,b.0x20
08004399: 04 48                         by1 :=       b.0x20
0800439B: 6C 0C                         w1 *         $0xC
0800439D: 05 CD 20                      by2 :=       $0x20
080043A0: FD 20 E0 08 00 74 A8 4C 04    by bmove     $0x80074A8+,b.0x30,$0x4
080043A9: 86                            bi3 clr
080043AA: 2D E6 30 D1                   by comp2     @b.0x30+,r2
080043AE: C6 46                         if >< go     $0x46
080043B0: 07 48                         by4 :=       b.0x20
080043B2: 6F 0C                         w4 *         $0xC
080043B4: 04 48                         by1 :=       b.0x20
080043B6: 6C 0C                         w1 *         $0xC
080043B8: FC 5A E0 08 00 74 A8          by rladdr    $0x80074A8+
080043BF: 0D 82                         w2 :=        r.0x8
080043C1: 1A D1 4F                      w move       r2,b.0x3C
080043C4: 1A 01 4E                      w move       $0x1,b.0x38
080043C7: FD 20 E3 08 00 74 A8 4D 04    by bmove     $0x80074A8+,b.0x34,$0x4
080043D0: 18 42                         r:=          b.0x8
080043D2: FD 20 4D 85 0C                by bmove     b.0x34,r.0x14,$0xC
080043D7: FD 20 45 88 0C                by bmove     b.0x14,r.0x20,$0xC
080043DC: C3 08 00 86 21 00             call         $0x8008621,$0x0
080043E2: 9D                            ifkret
080043E3: 44 D0                         w test       r1
080043E5: C4 0F                         if = go      $0xF
080043E7: 44 4A                         w test       b.0x28
080043E9: C6 09                         if >< go     $0x9
080043EB: 19 48 4B                      by move      b.0x20,b.0x2C
080043EE: 4D 4A                         w set1       b.0x28
080043F0: C0 04                         go           $0x4
080043F2: 84                            bi1 clr
080043F3: 80                            ret
080043F4: 05 48                         by2 :=       b.0x20
080043F6: 55 01                         w2 +         $0x1
080043F8: 1D 48                         by2 =:       b.0x20
080043FA: FC 1D CE 00 9C                h2 comp      $0x9C
080043FF: CE 9A                         if <= go     $0xFFFFFFFFFFFFFF9A
08004401: 44 4A                         w test       b.0x28
08004403: C4 08                         if = go      $0x8
08004405: 19 4B 48                      by move      b.0x2C,b.0x20
08004408: 0C 01                         w1 :=        $0x1
0800440A: 80                            ret
0800440B: 84                            bi1 clr
0800440C: 80                            ret
0800440D: B8 CF 00 00 00 30             ents         $0x30
08004413: 4A 4A                         w stz        b.0x28
08004415: FD 54 CE 00 AB 48             w byconv     $0xAB,b.0x20
0800441B: 04 48                         by1 :=       b.0x20
0800441D: 6C 0C                         w1 *         $0xC
0800441F: 18 42                         r:=          b.0x8
08004421: FD 20 E0 08 00 74 3C 85 0C    by bmove     $0x800743C+,r.0x14,$0xC
0800442A: FD 20 45 88 0C                by bmove     b.0x14,r.0x20,$0xC
0800442F: C3 08 00 86 21 00             call         $0x8008621,$0x0
08004435: 9D                            ifkret
08004436: 44 D0                         w test       r1
08004438: C4 0F                         if = go      $0xF
0800443A: 44 4A                         w test       b.0x28
0800443C: C6 09                         if >< go     $0x9
0800443E: 19 48 4B                      by move      b.0x20,b.0x2C
08004441: 4D 4A                         w set1       b.0x28
08004443: C0 04                         go           $0x4
08004445: 84                            bi1 clr
08004446: 80                            ret
08004447: 05 48                         by2 :=       b.0x20
08004449: 55 01                         w2 +         $0x1
0800444B: 1D 48                         by2 =:       b.0x20
0800444D: FC 1D CE 00 B6                h2 comp      $0xB6
08004452: CE C9                         if <= go     $0xFFFFFFFFFFFFFFC9
08004454: 44 4A                         w test       b.0x28
08004456: C4 0A                         if = go      $0xA
08004458: 19 4B 48                      by move      b.0x2C,b.0x20
0800445B: 0C 01                         w1 :=        $0x1
0800445D: 80                            ret
0800445E: C0 04                         go           $0x4
08004460: 84                            bi1 clr
08004461: 80                            ret
08004462: B8 CF 00 00 00 28             ents         $0x28
08004468: 44 45                         w test       b.0x14
0800446A: C6 33                         if >< go     $0x33
0800446C: 19 CD A6 C1 19                by move      $0xA6,b.0x19
08004471: 04 C1 19                      by1 :=       b.0x19
08004474: 6C 0C                         w1 *         $0xC
08004476: FD 20 E0 08 00 74 3C 49 04    by bmove     $0x800743C+,b.0x24,$0x4
0800447F: 86                            bi3 clr
08004480: 05 E6 24                      by2 :=       @b.0x24+
08004483: 2D 46 D1                      by comp2     b.0x18,r2
08004486: DA 15                         if <<= go    $0x15
08004488: 07 C1 19                      by4 :=       b.0x19
0800448B: 37 CE 00 AA                   w4 comp      $0xAA
0800448F: C6 06                         if >< go     $0x6
08004491: 84                            bi1 clr
08004492: 80                            ret
08004493: C0 06                         go           $0x6
08004495: FC 8A C1 19                   by incr      b.0x19
08004499: C0 D8                         go           $0xFFFFFFFFFFFFFFD8
0800449B: C0 60                         go           $0x60
0800449D: 06 C1 19                      by3 :=       b.0x19
080044A0: 6E 0C                         w3 *         $0xC
080044A2: 0F 45                         w4 :=        b.0x14
080044A4: 63 01                         w4 -         $0x1
080044A6: FD 20 E2 08 00 74 3C 49 04    by bmove     $0x800743C+,b.0x24,$0x4
080044AF: 05 E7 24                      by2 :=       @b.0x24+
080044B2: 1D 48                         by2 =:       b.0x20
080044B4: 04 C1 19                      by1 :=       b.0x19
080044B7: 6C 0C                         w1 *         $0xC
080044B9: FD 20 E0 08 00 74 3C 49 04    by bmove     $0x800743C+,b.0x24,$0x4
080044C2: 0E 45                         w3 :=        b.0x14
080044C4: 05 E6 24                      by2 :=       @b.0x24+
080044C7: 2D 46 D1                      by comp2     b.0x18,r2
080044CA: DA 31                         if <<= go    $0x31
080044CC: 07 C1 19                      by4 :=       b.0x19
080044CF: 37 CE 00 AA                   w4 comp      $0xAA
080044D3: C6 06                         if >< go     $0x6
080044D5: 84                            bi1 clr
080044D6: 80                            ret
080044D7: C0 22                         go           $0x22
080044D9: 07 C1 19                      by4 :=       b.0x19
080044DC: FC 37 01                      by4 +        $0x1
080044DF: 1F C1 19                      by4 =:       b.0x19
080044E2: 6F 0C                         w4 *         $0xC
080044E4: 62 01                         w3 -         $0x1
080044E6: 05 48                         by2 :=       b.0x20
080044E8: FD 20 E3 08 00 74 3C 49 04    by bmove     $0x800743C+,b.0x24,$0x4
080044F1: 2D E6 24 D1                   by comp2     @b.0x24+,r2
080044F5: C4 04                         if = go      $0x4
080044F7: 84                            bi1 clr
080044F8: 80                            ret
080044F9: C0 BB                         go           $0xFFFFFFFFFFFFFFBB
080044FB: 05 C1 19                      by2 :=       b.0x19
080044FE: 6D 0C                         w2 *         $0xC
08004500: FD 20 E1 08 00 74 3C 49 04    by bmove     $0x800743C+,b.0x24,$0x4
08004509: 0F 45                         w4 :=        b.0x14
0800450B: 06 E7 24                      by3 :=       @b.0x24+
0800450E: 2D 46 D2                      by comp2     b.0x18,r3
08004511: C6 07                         if >< go     $0x7
08004513: 0C 01                         w1 :=        $0x1
08004515: 80                            ret
08004516: C0 04                         go           $0x4
08004518: 84                            bi1 clr
08004519: 80                            ret
0800451A: 9C                            entd
0800451B: FD C0 52                      l=:          b.0x48
0800451E: 4A 53                         w stz        b.0x4C
08004520: 18 42                         r:=          b.0x8
08004522: 1A 45 85                      w move       b.0x14,r.0x14
08004525: C3 08 00 B4 3F 00             call         $0x800B43F,$0x0
0800452B: D2 04                         if -k go     $0x4
0800452D: B4 52                         jumpg        b.0x48
0800452F: 1C 48                         by1 =:       b.0x20
08004531: 30 17                         by1 comp     $0x17
08004533: C5 00 A1                      if = go      $0xA1
08004536: 4F 50                         w incr       b.0x40
08004538: 0D 53                         w2 :=        b.0x4C
0800453A: B4 E1 08 00 3F 20             jumpg        $0x8003F20+
08004540: 2D 48 0D                      by comp2     b.0x20,$0xD
08004543: C6 04                         if >< go     $0x4
08004545: 4A 53                         w stz        b.0x4C
08004547: C1 00 86                      go           $0x86
0800454A: 2D 48 0D                      by comp2     b.0x20,$0xD
0800454D: C6 07                         if >< go     $0x7
0800454F: 4F 47                         w incr       b.0x1C
08004551: 1A 03 53                      w move       $0x3,b.0x4C
08004554: C1 00 79                      go           $0x79
08004557: 4A 53                         w stz        b.0x4C
08004559: 2D 48 C4 08 00 3B 28          by comp2     b.0x20,$0x8003B28
08004560: D4 20                         if >> go     $0x20
08004562: 04 48                         by1 :=       b.0x20
08004564: B4 E0 08 00 3B 2C             jumpg        $0x8003B2C+
0800456A: 19 0D 48                      by move      $0xD,b.0x20
0800456D: 1A 03 53                      w move       $0x3,b.0x4C
08004570: C0 10                         go           $0x10
08004572: 04 48                         by1 :=       b.0x20
08004574: FC 3C CD 61                   by1 -        $0x61
08004578: FC 34 CD 41                   by1 +        $0x41
0800457C: 1C 48                         by1 =:       b.0x20
0800457E: C0 02                         go           $0x2
08004580: C0 4D                         go           $0x4D
08004582: 4A 53                         w stz        b.0x4C
08004584: 2D 48 C4 08 00 3D 24          by comp2     b.0x20,$0x8003D24
0800458B: D4 40                         if >> go     $0x40
0800458D: 04 48                         by1 :=       b.0x20
0800458F: B4 E0 08 00 3D 28             jumpg        $0x8003D28+
08004595: 19 0D 48                      by move      $0xD,b.0x20
08004598: 1A 03 53                      w move       $0x3,b.0x4C
0800459B: C0 30                         go           $0x30
0800459D: 4D 53                         w set1       b.0x4C
0800459F: C0 2C                         go           $0x2C
080045A1: 1A 02 53                      w move       $0x2,b.0x4C
080045A4: C0 27                         go           $0x27
080045A6: 1A 04 53                      w move       $0x4,b.0x4C
080045A9: C0 22                         go           $0x22
080045AB: 19 CD 80 48                   by move      $0x80,b.0x20
080045AF: C0 1C                         go           $0x1C
080045B1: 19 CD 81 48                   by move      $0x81,b.0x20
080045B5: C0 16                         go           $0x16
080045B7: 19 CD 82 48                   by move      $0x82,b.0x20
080045BB: C0 10                         go           $0x10
080045BD: 04 48                         by1 :=       b.0x20
080045BF: FC 3C CD 61                   by1 -        $0x61
080045C3: FC 34 CD 41                   by1 +        $0x41
080045C7: 1C 48                         by1 =:       b.0x20
080045C9: C0 02                         go           $0x2
080045CB: C0 02                         go           $0x2
080045CD: 44 53                         w test       b.0x4C
080045CF: C4 05                         if = go      $0x5
080045D1: C1 FF 4F                      go           $0xFFFFFFFFFFFFFF4F
080045D4: FE 03                         clrk
080045D6: B4 52                         jumpg        b.0x48
080045D8: 9C                            entd
080045D9: FD C0 54                      l=:          b.0x50
080045DC: 20 55                         w1 =:        b.0x54
080045DE: 18 42                         r:=          b.0x8
080045E0: 1A 45 85                      w move       b.0x14,r.0x14
080045E3: FD 3D 58                      w2 laddr     b.0x60
080045E6: 21 86                         w2 =:        r.0x18
080045E8: 4A 87                         w stz        r.0x1C
080045EA: 1A CD 63 88                   w move       $0x63,r.0x20
080045EE: FE 79 00 89 03                w bmove      $0x0,r.0x24,$0x3
080045F3: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
080045F9: D2 04                         if -k go     $0x4
080045FB: B4 54                         jumpg        b.0x50
080045FD: 4A 57                         w stz        b.0x5C
080045FF: 0C 57                         w1 :=        b.0x5C
08004601: 2D D4 60 0D                   by comp2     b.0x60+,$0xD
08004605: C4 06                         if = go      $0x6
08004607: 4F 57                         w incr       b.0x5C
08004609: C0 F6                         go           $0xFFFFFFFFFFFFFFF6
0800460B: 1A CD 72 71                   w move       $0x72,b.0xC4
0800460F: 4D 72                         w set1       b.0xC8
08004611: 60 01                         w1 -         $0x1
08004613: 1A D0 78                      w move       r1,b.0xE0
08004616: 4A 77                         w stz        b.0xDC
08004618: FD 3D 58                      w2 laddr     b.0x60
0800461B: 21 76                         w2 =:        b.0xD8
0800461D: FD 20 76 73 0C                by bmove     b.0xD8,b.0xCC,$0xC
08004622: 1A 3F 79                      w move       $0x3F,b.0xE4
08004625: C3 08 00 B9 7C 07 71 72 C5 CC C5 D0 C5 D4 79 56 call         $0x800B97C,$0x7,b.0xC4,b.0xC8,@b.0xFFFFFFFFFFFFFFCC,@b.0xFFFFFFFFFFFFFFD0,@b.0xFFFFFFFFFFFFFFD4,b.0xE4,b.0x58
08004635: D2 04                         if -k go     $0x4
08004637: B4 54                         jumpg        b.0x50
08004639: 1A CD 72 71                   w move       $0x72,b.0xC4
0800463D: 4D 72                         w set1       b.0xC8
0800463F: FE 79 C4 08 00 3F 38 76 03    w bmove      $0x8003F38,b.0xD8,$0x3
08004648: 1A 3F 79                      w move       $0x3F,b.0xE4
0800464B: C3 08 00 B9 7C 07 71 72 C5 D8 C5 DC C5 E0 79 56 call         $0x800B97C,$0x7,b.0xC4,b.0xC8,@b.0xFFFFFFFFFFFFFFD8,@b.0xFFFFFFFFFFFFFFDC,@b.0xFFFFFFFFFFFFFFE0,b.0xE4,b.0x58
0800465B: D2 04                         if -k go     $0x4
0800465D: B4 54                         jumpg        b.0x50
0800465F: 18 42                         r:=          b.0x8
08004661: 4D 85                         w set1       r.0x14
08004663: FE 79 C4 08 00 3F 48 86 03    w bmove      $0x8003F48,r.0x18,$0x3
0800466C: 1A 47 89                      w move       b.0x1C,r.0x24
0800466F: C3 08 00 C6 02 00             call         $0x800C602,$0x0
08004675: D2 04                         if -k go     $0x4
08004677: B4 54                         jumpg        b.0x50
08004679: 1A CD 72 71                   w move       $0x72,b.0xC4
0800467D: 4D 72                         w set1       b.0xC8
0800467F: FE 79 C4 08 00 3F 58 7A 03    w bmove      $0x8003F58,b.0xE8,$0x3
08004688: 1A 3F 79                      w move       $0x3F,b.0xE4
0800468B: C3 08 00 B9 7C 07 71 72 C5 E8 C5 EC C5 F0 79 56 call         $0x800B97C,$0x7,b.0xC4,b.0xC8,@b.0xFFFFFFFFFFFFFFE8,@b.0xFFFFFFFFFFFFFFEC,@b.0xFFFFFFFFFFFFFFF0,b.0xE4,b.0x58
0800469B: D2 04                         if -k go     $0x4
0800469D: B4 54                         jumpg        b.0x50
0800469F: 0C 55                         w1 :=        b.0x54
080046A1: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
080046A7: D2 04                         if -k go     $0x4
080046A9: B4 54                         jumpg        b.0x50
080046AB: FE 03                         clrk
080046AD: B4 54                         jumpg        b.0x50
080046AF: 9C                            entd
080046B0: FD C0 7D                      l=:          b.0xF4
080046B3: 1C 7E                         by1 =:       b.0xF8
080046B5: 44 46                         w test       b.0x18
080046B7: C6 15                         if >< go     $0x15
080046B9: C3 08 00 86 91 00             call         $0x8008691,$0x0
080046BF: D2 04                         if -k go     $0x4
080046C1: B4 7D                         jumpg        b.0xF4
080046C3: 20 46                         w1 =:        b.0x18
080046C5: 20 51                         w1 =:        b.0x44
080046C7: 1A 3F 4F                      w move       $0x3F,b.0x3C
080046CA: C0 1B                         go           $0x1B
080046CC: 2E 4F 13                      w comp2      b.0x3C,$0x13
080046CF: C6 16                         if >< go     $0x16
080046D1: C3 08 00 86 91 00             call         $0x8008691,$0x0
080046D7: D2 04                         if -k go     $0x4
080046D9: B4 7D                         jumpg        b.0xF4
080046DB: 18 51                         r:=          b.0x44
080046DD: 20 85                         w1 =:        r.0x14
080046DF: 1A 3F 4F                      w move       $0x3F,b.0x3C
080046E2: 1A 85 51                      w move       r.0x14,b.0x44
080046E5: 0C 4F                         w1 :=        b.0x3C
080046E7: 54 01                         w1 +         $0x1
080046E9: 20 4F                         w1 =:        b.0x3C
080046EB: 05 7E                         by2 :=       b.0xF8
080046ED: FD 3E C5 44                   w3 laddr     @b.0x44
080046F1: 56 D0                         w3 +         r1
080046F3: 1D F6 00                      by2 =:       r3.(0x0)
080046F6: FE 03                         clrk
080046F8: B4 7D                         jumpg        b.0xF4
080046FA: 9C                            entd
080046FB: FD C0 7F                      l=:          b.0xFC
080046FE: 4A C2 01 00                   w stz        b.0x100
08004702: 4A C2 01 08                   w stz        b.0x108
08004706: 2D 48 C4 08 00 3F 64          by comp2     b.0x20,$0x8003F64
0800470D: D4 74                         if >> go     $0x74
0800470F: 04 48                         by1 :=       b.0x20
08004711: B4 E0 08 00 3F 68             jumpg        $0x8003F68+
08004717: 04 CD 80                      by1 :=       $0x80
0800471A: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004720: D2 04                         if -k go     $0x4
08004722: B4 7F                         jumpg        b.0xFC
08004724: 4F C2 01 08                   w incr       b.0x108
08004728: C0 65                         go           $0x65
0800472A: 04 48                         by1 :=       b.0x20
0800472C: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004732: D2 04                         if -k go     $0x4
08004734: B4 7F                         jumpg        b.0xFC
08004736: C0 57                         go           $0x57
08004738: 4D C2 01 00                   w set1       b.0x100
0800473C: 04 48                         by1 :=       b.0x20
0800473E: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004744: D2 04                         if -k go     $0x4
08004746: B4 7F                         jumpg        b.0xFC
08004748: C0 45                         go           $0x45
0800474A: 04 CD 3B                      by1 :=       $0x3B
0800474D: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004753: D2 04                         if -k go     $0x4
08004755: B4 7F                         jumpg        b.0xFC
08004757: 0D C2 01 08                   w2 :=        b.0x108
0800475B: 61 01                         w2 -         $0x1
0800475D: 21 C2 01 08                   w2 =:        b.0x108
08004761: 44 D1                         w test       r2
08004763: C6 08                         if >< go     $0x8
08004765: 0C 01                         w1 :=        $0x1
08004767: FE 03                         clrk
08004769: B4 7F                         jumpg        b.0xFC
0800476B: C0 22                         go           $0x22
0800476D: 0C CD B1                      w1 :=        $0xB1
08004770: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004776: D2 04                         if -k go     $0x4
08004778: B4 7F                         jumpg        b.0xFC
0800477A: 84                            bi1 clr
0800477B: FE 03                         clrk
0800477D: B4 7F                         jumpg        b.0xFC
0800477F: C0 0E                         go           $0xE
08004781: 0C 39                         w1 :=        $0x39
08004783: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004789: D2 04                         if -k go     $0x4
0800478B: B4 7F                         jumpg        b.0xFC
0800478D: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08004793: D2 04                         if -k go     $0x4
08004795: B4 7F                         jumpg        b.0xFC
08004797: 2D 48 17                      by comp2     b.0x20,$0x17
0800479A: C4 05                         if = go      $0x5
0800479C: C1 FF 6A                      go           $0xFFFFFFFFFFFFFF6A
0800479F: 0C CD B1                      w1 :=        $0xB1
080047A2: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080047A8: D2 04                         if -k go     $0x4
080047AA: B4 7F                         jumpg        b.0xFC
080047AC: 84                            bi1 clr
080047AD: FE 03                         clrk
080047AF: B4 7F                         jumpg        b.0xFC
080047B1: 9C                            entd
080047B2: FD C0 C2 01 0C                l=:          b.0x10C
080047B7: 4A C2 01 78                   w stz        b.0x178
080047BB: 0C C2 01 78                   w1 :=        b.0x178
080047BF: B4 E0 08 00 44 74             jumpg        $0x8004474+
080047C5: 2D 48 C4 08 00 41 6C          by comp2     b.0x20,$0x800416C
080047CC: D4 31                         if >> go     $0x31
080047CE: 04 48                         by1 :=       b.0x20
080047D0: B4 E0 08 00 41 70             jumpg        $0x8004170+
080047D6: 84                            bi1 clr
080047D7: 1C C2 01 11                   by1 =:       b.0x111
080047DB: 19 48 D8 01 13                by move      b.0x20,b.0x113+
080047E0: 4D C2 01 78                   w set1       b.0x178
080047E4: C0 2D                         go           $0x2D
080047E6: 0C 3B                         w1 :=        $0x3B
080047E8: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080047EE: D2 06                         if -k go     $0x6
080047F0: B4 C2 01 0C                   jumpg        b.0x10C
080047F4: 1A 03 C2 01 78                w move       $0x3,b.0x178
080047F9: C0 18                         go           $0x18
080047FB: C0 16                         go           $0x16
080047FD: 0C CD C1                      w1 :=        $0xC1
08004800: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004806: D2 06                         if -k go     $0x6
08004808: B4 C2 01 0C                   jumpg        b.0x10C
0800480C: 1A 02 C2 01 78                w move       $0x2,b.0x178
08004811: C1 01 0E                      go           $0x10E
08004814: 2D 48 C4 08 00 42 F0          by comp2     b.0x20,$0x80042F0
0800481B: D5 00 DA                      if >> go     $0xDA
0800481E: 04 48                         by1 :=       b.0x20
08004820: B4 E0 08 00 42 F4             jumpg        $0x80042F4+
08004826: 04 C2 01 11                   by1 :=       b.0x111
0800482A: FC 34 01                      by1 +        $0x1
0800482D: 1C C2 01 11                   by1 =:       b.0x111
08004831: 19 48 D8 01 13                by move      b.0x20,b.0x113+
08004836: C1 00 D3                      go           $0xD3
08004839: 04 C2 01 11                   by1 :=       b.0x111
0800483D: FC 34 01                      by1 +        $0x1
08004840: 19 0D D8 01 13                by move      $0xD,b.0x113+
08004845: 2D C2 01 10 CD 96             by comp2     b.0x110,$0x96
0800484B: C4 0A                         if = go      $0xA
0800484D: 2D C2 01 10 CD 8C             by comp2     b.0x110,$0x8C
08004853: C6 1C                         if >< go     $0x1C
08004855: 44 C4 08 00 2C 3C             w test       $0x8002C3C
0800485B: C4 14                         if = go      $0x14
0800485D: 0C CD B3                      w1 :=        $0xB3
08004860: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004866: D2 06                         if -k go     $0x6
08004868: B4 C2 01 0C                   jumpg        b.0x10C
0800486C: C1 00 82                      go           $0x82
0800486F: FD 3D C2 01 13                w2 laddr     b.0x113
08004874: 18 42                         r:=          b.0x8
08004876: 21 85                         w2 =:        r.0x14
08004878: 4A 86                         w stz        r.0x18
0800487A: 1A CD 63 87                   w move       $0x63,r.0x1C
0800487E: C3 08 00 44 0D 00             call         $0x800440D,$0x0
08004884: D2 06                         if -k go     $0x6
08004886: B4 C2 01 0C                   jumpg        b.0x10C
0800488A: 18 42                         r:=          b.0x8
0800488C: 19 88 C2 01 12                by move      r.0x20,b.0x112
08004891: 44 D0                         w test       r1
08004893: C6 12                         if >< go     $0x12
08004895: 0C 3A                         w1 :=        $0x3A
08004897: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800489D: D2 06                         if -k go     $0x6
0800489F: B4 C2 01 0C                   jumpg        b.0x10C
080048A3: C0 4B                         go           $0x4B
080048A5: 19 C2 01 12 85                by move      b.0x112,r.0x14
080048AA: 04 C2 01 10                   by1 :=       b.0x110
080048AE: C3 08 00 90 56 00             call         $0x8009056,$0x0
080048B4: D2 06                         if -k go     $0x6
080048B6: B4 C2 01 0C                   jumpg        b.0x10C
080048BA: 44 D0                         w test       r1
080048BC: C6 12                         if >< go     $0x12
080048BE: 0C 3A                         w1 :=        $0x3A
080048C0: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080048C6: D2 06                         if -k go     $0x6
080048C8: B4 C2 01 0C                   jumpg        b.0x10C
080048CC: C0 22                         go           $0x22
080048CE: 04 C2 01 10                   by1 :=       b.0x110
080048D2: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080048D8: D2 06                         if -k go     $0x6
080048DA: B4 C2 01 0C                   jumpg        b.0x10C
080048DE: 04 C2 01 12                   by1 :=       b.0x112
080048E2: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080048E8: D2 06                         if -k go     $0x6
080048EA: B4 C2 01 0C                   jumpg        b.0x10C
080048EE: 1A 03 C2 01 78                w move       $0x3,b.0x178
080048F3: C0 16                         go           $0x16
080048F5: 0C CD C1                      w1 :=        $0xC1
080048F8: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080048FE: D2 06                         if -k go     $0x6
08004900: B4 C2 01 0C                   jumpg        b.0x10C
08004904: 1A 02 C2 01 78                w move       $0x2,b.0x178
08004909: C0 16                         go           $0x16
0800490B: 2D 48 CD 20                   by comp2     b.0x20,$0x20
0800490F: C4 07                         if = go      $0x7
08004911: 2D 48 0D                      by comp2     b.0x20,$0xD
08004914: C6 07                         if >< go     $0x7
08004916: 1A 03 C2 01 78                w move       $0x3,b.0x178
0800491B: C0 04                         go           $0x4
0800491D: C0 02                         go           $0x2
0800491F: 2E C2 01 78 03                w comp2      b.0x178,$0x3
08004924: C4 16                         if = go      $0x16
08004926: C3 08 00 45 1A 00             call         $0x800451A,$0x0
0800492C: D2 06                         if -k go     $0x6
0800492E: B4 C2 01 0C                   jumpg        b.0x10C
08004932: 2D 48 17                      by comp2     b.0x20,$0x17
08004935: C4 05                         if = go      $0x5
08004937: C1 FE 84                      go           $0xFFFFFFFFFFFFFE84
0800493A: FE 03                         clrk
0800493C: B4 C2 01 0C                   jumpg        b.0x10C
08004940: 9C                            entd
08004941: FD C0 C2 01 7C                l=:          b.0x17C
08004946: 4A C2 01 84                   w stz        b.0x184
0800494A: 2D 48 0D                      by comp2     b.0x20,$0xD
0800494D: C5 00 BD                      if = go      $0xBD
08004950: 0C C2 01 84                   w1 :=        b.0x184
08004954: B4 E0 08 00 47 64             jumpg        $0x8004764+
0800495A: 2D 48 C4 08 00 44 84          by comp2     b.0x20,$0x8004484
08004961: D4 20                         if >> go     $0x20
08004963: 04 48                         by1 :=       b.0x20
08004965: B4 E0 08 00 44 88             jumpg        $0x8004488+
0800496B: 04 48                         by1 :=       b.0x20
0800496D: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004973: D2 06                         if -k go     $0x6
08004975: B4 C2 01 7C                   jumpg        b.0x17C
08004979: 4D C2 01 84                   w set1       b.0x184
0800497D: C0 1A                         go           $0x1A
0800497F: C0 18                         go           $0x18
08004981: 0C CD C1                      w1 :=        $0xC1
08004984: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800498A: D2 06                         if -k go     $0x6
0800498C: B4 C2 01 7C                   jumpg        b.0x17C
08004990: 84                            bi1 clr
08004991: FE 03                         clrk
08004993: B4 C2 01 7C                   jumpg        b.0x17C
08004997: C0 5F                         go           $0x5F
08004999: 2D 48 C4 08 00 45 F4          by comp2     b.0x20,$0x80045F4
080049A0: D4 21                         if >> go     $0x21
080049A2: 04 48                         by1 :=       b.0x20
080049A4: B4 E0 08 00 45 F8             jumpg        $0x80045F8+
080049AA: 04 48                         by1 :=       b.0x20
080049AC: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080049B2: D2 06                         if -k go     $0x6
080049B4: B4 C2 01 7C                   jumpg        b.0x17C
080049B8: C0 1F                         go           $0x1F
080049BA: 1A 02 C2 01 84                w move       $0x2,b.0x184
080049BF: C0 18                         go           $0x18
080049C1: 0C CD C1                      w1 :=        $0xC1
080049C4: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080049CA: D2 06                         if -k go     $0x6
080049CC: B4 C2 01 7C                   jumpg        b.0x17C
080049D0: 84                            bi1 clr
080049D1: FE 03                         clrk
080049D3: B4 C2 01 7C                   jumpg        b.0x17C
080049D7: C0 1F                         go           $0x1F
080049D9: 2D 48 CD 20                   by comp2     b.0x20,$0x20
080049DD: C4 17                         if = go      $0x17
080049DF: 0C 3D                         w1 :=        $0x3D
080049E1: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080049E7: D2 06                         if -k go     $0x6
080049E9: B4 C2 01 7C                   jumpg        b.0x17C
080049ED: 84                            bi1 clr
080049EE: FE 03                         clrk
080049F0: B4 C2 01 7C                   jumpg        b.0x17C
080049F4: C0 02                         go           $0x2
080049F6: C3 08 00 45 1A 00             call         $0x800451A,$0x0
080049FC: D2 06                         if -k go     $0x6
080049FE: B4 C2 01 7C                   jumpg        b.0x17C
08004A02: 2D 48 17                      by comp2     b.0x20,$0x17
08004A05: C4 05                         if = go      $0x5
08004A07: C1 FF 43                      go           $0xFFFFFFFFFFFFFF43
08004A0A: 0C 01                         w1 :=        $0x1
08004A0C: FE 03                         clrk
08004A0E: B4 C2 01 7C                   jumpg        b.0x17C
08004A12: 9C                            entd
08004A13: FD C0 C2 01 88                l=:          b.0x188
08004A18: 84                            bi1 clr
08004A19: 20 C2 01 90                   w1 =:        b.0x190
08004A1D: 20 C2 01 94                   w1 =:        b.0x194
08004A21: 20 C2 01 98                   w1 =:        b.0x198
08004A25: 4A C2 01 8C                   w stz        b.0x18C
08004A29: 0C C2 01 8C                   w1 :=        b.0x18C
08004A2D: B4 E0 08 00 4E 88             jumpg        $0x8004E88+
08004A33: 2D 48 C4 08 00 47 70          by comp2     b.0x20,$0x8004770
08004A3A: D4 46                         if >> go     $0x46
08004A3C: 04 48                         by1 :=       b.0x20
08004A3E: B4 E0 08 00 47 74             jumpg        $0x8004774+
08004A44: 04 09                         by1 :=       $0x9
08004A46: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004A4C: D2 06                         if -k go     $0x6
08004A4E: B4 C2 01 88                   jumpg        b.0x188
08004A52: 85                            bi2 clr
08004A53: 52 46 D1                      w swap       b.0x18,r2
08004A56: 21 C2 01 98                   w2 =:        b.0x198
08004A5A: 04 48                         by1 :=       b.0x20
08004A5C: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004A62: D2 06                         if -k go     $0x6
08004A64: B4 C2 01 88                   jumpg        b.0x188
08004A68: 4D C2 01 8C                   w set1       b.0x18C
08004A6C: C0 28                         go           $0x28
08004A6E: C0 26                         go           $0x26
08004A70: 04 0D                         by1 :=       $0xD
08004A72: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004A78: D2 06                         if -k go     $0x6
08004A7A: B4 C2 01 88                   jumpg        b.0x188
08004A7E: C0 16                         go           $0x16
08004A80: 0C CD C1                      w1 :=        $0xC1
08004A83: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004A89: D2 06                         if -k go     $0x6
08004A8B: B4 C2 01 88                   jumpg        b.0x188
08004A8F: 1A 04 C2 01 8C                w move       $0x4,b.0x18C
08004A94: C1 01 1C                      go           $0x11C
08004A97: 2D 48 C4 08 00 48 F4          by comp2     b.0x20,$0x80048F4
08004A9E: D4 37                         if >> go     $0x37
08004AA0: 04 48                         by1 :=       b.0x20
08004AA2: B4 E0 08 00 48 F8             jumpg        $0x80048F8+
08004AA8: 04 48                         by1 :=       b.0x20
08004AAA: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004AB0: D2 06                         if -k go     $0x6
08004AB2: B4 C2 01 88                   jumpg        b.0x188
08004AB6: C0 33                         go           $0x33
08004AB8: 04 0D                         by1 :=       $0xD
08004ABA: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004AC0: D2 06                         if -k go     $0x6
08004AC2: B4 C2 01 88                   jumpg        b.0x188
08004AC6: 85                            bi2 clr
08004AC7: 52 46 D1                      w swap       b.0x18,r2
08004ACA: 21 C2 01 90                   w2 =:        b.0x190
08004ACE: 1A 02 C2 01 8C                w move       $0x2,b.0x18C
08004AD3: C0 16                         go           $0x16
08004AD5: 0C CD C1                      w1 :=        $0xC1
08004AD8: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004ADE: D2 06                         if -k go     $0x6
08004AE0: B4 C2 01 88                   jumpg        b.0x188
08004AE4: 1A 04 C2 01 8C                w move       $0x4,b.0x18C
08004AE9: C1 00 C7                      go           $0xC7
08004AEC: 2D 48 C4 08 00 4A 78          by comp2     b.0x20,$0x8004A78
08004AF3: D4 4C                         if >> go     $0x4C
08004AF5: 04 48                         by1 :=       b.0x20
08004AF7: B4 E0 08 00 4A 7C             jumpg        $0x8004A7C+
08004AFD: 1A 4A C2 01 00                w move       b.0x28,b.0x100
08004B02: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08004B08: D2 06                         if -k go     $0x6
08004B0A: B4 C2 01 88                   jumpg        b.0x188
08004B0E: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08004B13: 44 D0                         w test       r1
08004B15: C6 09                         if >< go     $0x9
08004B17: 1A 04 C2 01 8C                w move       $0x4,b.0x18C
08004B1C: C0 07                         go           $0x7
08004B1E: 1A 03 C2 01 8C                w move       $0x3,b.0x18C
08004B23: C0 2F                         go           $0x2F
08004B25: C0 2D                         go           $0x2D
08004B27: 04 0D                         by1 :=       $0xD
08004B29: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004B2F: D2 06                         if -k go     $0x6
08004B31: B4 C2 01 88                   jumpg        b.0x188
08004B35: 85                            bi2 clr
08004B36: 52 46 D1                      w swap       b.0x18,r2
08004B39: 21 C2 01 94                   w2 =:        b.0x194
08004B3D: C0 15                         go           $0x15
08004B3F: 04 48                         by1 :=       b.0x20
08004B41: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004B47: D2 06                         if -k go     $0x6
08004B49: B4 C2 01 88                   jumpg        b.0x188
08004B4D: 1A 03 C2 01 8C                w move       $0x3,b.0x18C
08004B52: C0 5E                         go           $0x5E
08004B54: 2D 48 C4 08 00 4C 80          by comp2     b.0x20,$0x8004C80
08004B5B: D4 43                         if >> go     $0x43
08004B5D: 04 48                         by1 :=       b.0x20
08004B5F: B4 E0 08 00 4C 84             jumpg        $0x8004C84+
08004B65: 1A 4A C2 01 00                w move       b.0x28,b.0x100
08004B6A: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08004B70: D2 06                         if -k go     $0x6
08004B72: B4 C2 01 88                   jumpg        b.0x188
08004B76: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08004B7B: 44 D0                         w test       r1
08004B7D: C6 07                         if >< go     $0x7
08004B7F: 1A 04 C2 01 8C                w move       $0x4,b.0x18C
08004B84: C0 28                         go           $0x28
08004B86: 04 0D                         by1 :=       $0xD
08004B88: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004B8E: D2 06                         if -k go     $0x6
08004B90: B4 C2 01 88                   jumpg        b.0x188
08004B94: 85                            bi2 clr
08004B95: 52 46 D1                      w swap       b.0x18,r2
08004B98: 21 C2 01 94                   w2 =:        b.0x194
08004B9C: C0 10                         go           $0x10
08004B9E: 04 48                         by1 :=       b.0x20
08004BA0: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004BA6: D2 06                         if -k go     $0x6
08004BA8: B4 C2 01 88                   jumpg        b.0x188
08004BAC: C0 04                         go           $0x4
08004BAE: C0 02                         go           $0x2
08004BB0: 2D 48 0D                      by comp2     b.0x20,$0xD
08004BB3: C4 16                         if = go      $0x16
08004BB5: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08004BBB: D2 06                         if -k go     $0x6
08004BBD: B4 C2 01 88                   jumpg        b.0x188
08004BC1: 2D 48 17                      by comp2     b.0x20,$0x17
08004BC4: C4 05                         if = go      $0x5
08004BC6: C1 FE 63                      go           $0xFFFFFFFFFFFFFE63
08004BC9: 2E C2 01 8C 04                w comp2      b.0x18C,$0x4
08004BCE: C6 4A                         if >< go     $0x4A
08004BD0: 18 42                         r:=          b.0x8
08004BD2: 1A C2 01 98 85                w move       b.0x198,r.0x14
08004BD7: C3 08 00 87 17 00             call         $0x8008717,$0x0
08004BDD: D2 06                         if -k go     $0x6
08004BDF: B4 C2 01 88                   jumpg        b.0x188
08004BE3: 18 42                         r:=          b.0x8
08004BE5: 1A 85 C2 01 98                w move       r.0x14,b.0x198
08004BEA: 1A C2 01 90 85                w move       b.0x190,r.0x14
08004BEF: C3 08 00 87 17 00             call         $0x8008717,$0x0
08004BF5: D2 06                         if -k go     $0x6
08004BF7: B4 C2 01 88                   jumpg        b.0x188
08004BFB: 18 42                         r:=          b.0x8
08004BFD: 1A 85 C2 01 90                w move       r.0x14,b.0x190
08004C02: 1A 46 85                      w move       b.0x18,r.0x14
08004C05: C3 08 00 87 17 00             call         $0x8008717,$0x0
08004C0B: D2 06                         if -k go     $0x6
08004C0D: B4 C2 01 88                   jumpg        b.0x188
08004C11: 18 42                         r:=          b.0x8
08004C13: 1A 85 46                      w move       r.0x14,b.0x18
08004C16: C0 2F                         go           $0x2F
08004C18: 44 C2 01 90                   w test       b.0x190
08004C1C: C4 29                         if = go      $0x29
08004C1E: 18 C2 01 90                   r:=          b.0x190
08004C22: 1A C2 01 94 86                w move       b.0x194,r.0x18
08004C27: FD 3D C2 01 98                w2 laddr     b.0x198
08004C2C: 0C C2 01 90                   w1 :=        b.0x190
08004C30: 0E 14                         w3 :=        $0x14
08004C32: FE 03                         clrk
08004C34: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08004C3A: D2 06                         if -k go     $0x6
08004C3C: B4 C2 01 88                   jumpg        b.0x188
08004C40: 1A C2 01 98 46                w move       b.0x198,b.0x18
08004C45: FE 03                         clrk
08004C47: B4 C2 01 88                   jumpg        b.0x188
08004C4B: 9C                            entd
08004C4C: FD C0 C2 01 9C                l=:          b.0x19C
08004C51: 04 CD 81                      by1 :=       $0x81
08004C54: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004C5A: D2 06                         if -k go     $0x6
08004C5C: B4 C2 01 9C                   jumpg        b.0x19C
08004C60: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08004C66: D2 06                         if -k go     $0x6
08004C68: B4 C2 01 9C                   jumpg        b.0x19C
08004C6C: 2D 48 17                      by comp2     b.0x20,$0x17
08004C6F: C5 00 7C                      if = go      $0x7C
08004C72: 2D 48 CD 81                   by comp2     b.0x20,$0x81
08004C76: C6 1A                         if >< go     $0x1A
08004C78: 0C CD C1                      w1 :=        $0xC1
08004C7B: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004C81: D2 06                         if -k go     $0x6
08004C83: B4 C2 01 9C                   jumpg        b.0x19C
08004C87: 84                            bi1 clr
08004C88: FE 03                         clrk
08004C8A: B4 C2 01 9C                   jumpg        b.0x19C
08004C8E: C0 55                         go           $0x55
08004C90: 2D 48 CD 80                   by comp2     b.0x20,$0x80
08004C94: C6 41                         if >< go     $0x41
08004C96: 1A 4A C2 01 00                w move       b.0x28,b.0x100
08004C9B: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08004CA1: D2 06                         if -k go     $0x6
08004CA3: B4 C2 01 9C                   jumpg        b.0x19C
08004CA7: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08004CAC: 44 D0                         w test       r1
08004CAE: C6 0B                         if >< go     $0xB
08004CB0: 84                            bi1 clr
08004CB1: FE 03                         clrk
08004CB3: B4 C2 01 9C                   jumpg        b.0x19C
08004CB7: C0 1C                         go           $0x1C
08004CB9: 44 4A                         w test       b.0x28
08004CBB: C4 18                         if = go      $0x18
08004CBD: 0C CD BE                      w1 :=        $0xBE
08004CC0: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004CC6: D2 06                         if -k go     $0x6
08004CC8: B4 C2 01 9C                   jumpg        b.0x19C
08004CCC: 84                            bi1 clr
08004CCD: FE 03                         clrk
08004CCF: B4 C2 01 9C                   jumpg        b.0x19C
08004CD3: C0 10                         go           $0x10
08004CD5: 04 48                         by1 :=       b.0x20
08004CD7: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004CDD: D2 06                         if -k go     $0x6
08004CDF: B4 C2 01 9C                   jumpg        b.0x19C
08004CE3: 2D 48 0D                      by comp2     b.0x20,$0xD
08004CE6: C4 05                         if = go      $0x5
08004CE8: C1 FF 78                      go           $0xFFFFFFFFFFFFFF78
08004CEB: 18 42                         r:=          b.0x8
08004CED: 1A 46 85                      w move       b.0x18,r.0x14
08004CF0: C3 08 00 A2 53 00             call         $0x800A253,$0x0
08004CF6: D2 06                         if -k go     $0x6
08004CF8: B4 C2 01 9C                   jumpg        b.0x19C
08004CFC: 18 42                         r:=          b.0x8
08004CFE: 1A 85 46                      w move       r.0x14,b.0x18
08004D01: 0D 46                         w2 :=        b.0x18
08004D03: 21 C2 01 AC                   w2 =:        b.0x1AC
08004D07: 21 C2 01 B0                   w2 =:        b.0x1B0
08004D0B: 86                            bi3 clr
08004D0C: 22 46                         w3 =:        b.0x18
08004D0E: 22 C2 01 B4                   w3 =:        b.0x1B4
08004D12: 87                            bi4 clr
08004D13: 1F C2 01 A5                   by4 =:       b.0x1A5
08004D17: 23 C2 01 A8                   w4 =:        b.0x1A8
08004D1B: FC 87 C2 01 A4                by set1      b.0x1A4
08004D20: 04 C2 01 A5                   by1 :=       b.0x1A5
08004D24: FD 3E C6 01 B0                w3 laddr     @b.0x1B0
08004D29: 56 D0                         w3 +         r1
08004D2B: 05 F6 00                      by2 :=       r3.(0x0)
08004D2E: 1D 48                         by2 =:       b.0x20
08004D30: 31 09                         by2 comp     $0x9
08004D32: C5 01 17                      if = go      $0x117
08004D35: 2D C2 01 A4 01                by comp2     b.0x1A4,$0x1
08004D3A: C6 6E                         if >< go     $0x6E
08004D3C: 31 C4 08 00 4E 9C             by2 comp     $0x8004E9C
08004D42: D4 4D                         if >> go     $0x4D
08004D44: B4 E1 08 00 4E A0             jumpg        $0x8004EA0+
08004D4A: 04 48                         by1 :=       b.0x20
08004D4C: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004D52: D2 06                         if -k go     $0x6
08004D54: B4 C2 01 9C                   jumpg        b.0x19C
08004D58: 19 02 C2 01 A4                by move      $0x2,b.0x1A4
08004D5D: C0 48                         go           $0x48
08004D5F: C0 46                         go           $0x46
08004D61: 04 CD 81                      by1 :=       $0x81
08004D64: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004D6A: D2 06                         if -k go     $0x6
08004D6C: B4 C2 01 9C                   jumpg        b.0x19C
08004D70: 04 0D                         by1 :=       $0xD
08004D72: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004D78: D2 06                         if -k go     $0x6
08004D7A: B4 C2 01 9C                   jumpg        b.0x19C
08004D7E: 18 C2 01 B8                   r:=          b.0x1B8
08004D82: 1A 46 86                      w move       b.0x18,r.0x18
08004D85: 85                            bi2 clr
08004D86: 52 46 D1                      w swap       b.0x18,r2
08004D89: 21 C2 01 B8                   w2 =:        b.0x1B8
08004D8D: C0 18                         go           $0x18
08004D8F: 0C CD C1                      w1 :=        $0xC1
08004D92: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004D98: D2 06                         if -k go     $0x6
08004D9A: B4 C2 01 9C                   jumpg        b.0x19C
08004D9E: 84                            bi1 clr
08004D9F: FE 03                         clrk
08004DA1: B4 C2 01 9C                   jumpg        b.0x19C
08004DA5: C1 00 A4                      go           $0xA4
08004DA8: 2D C2 01 A4 02                by comp2     b.0x1A4,$0x2
08004DAD: C7 00 9C                      if >< go     $0x9C
08004DB0: 31 C4 08 00 50 A8             by2 comp     $0x80050A8
08004DB6: D5 00 7D                      if >> go     $0x7D
08004DB9: B4 E1 08 00 50 AC             jumpg        $0x80050AC+
08004DBF: 04 48                         by1 :=       b.0x20
08004DC1: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004DC7: D2 06                         if -k go     $0x6
08004DC9: B4 C2 01 9C                   jumpg        b.0x19C
08004DCD: C1 00 7C                      go           $0x7C
08004DD0: 04 0D                         by1 :=       $0xD
08004DD2: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004DD8: D2 06                         if -k go     $0x6
08004DDA: B4 C2 01 9C                   jumpg        b.0x19C
08004DDE: 44 C2 01 B4                   w test       b.0x1B4
08004DE2: C6 09                         if >< go     $0x9
08004DE4: 1A 46 C2 01 B4                w move       b.0x18,b.0x1B4
08004DE9: C0 09                         go           $0x9
08004DEB: 18 C2 01 B8                   r:=          b.0x1B8
08004DEF: 1A 46 86                      w move       b.0x18,r.0x18
08004DF2: 85                            bi2 clr
08004DF3: 52 46 D1                      w swap       b.0x18,r2
08004DF6: 21 C2 01 B8                   w2 =:        b.0x1B8
08004DFA: 2D 48 CD 81                   by comp2     b.0x20,$0x81
08004DFE: C6 2E                         if >< go     $0x2E
08004E00: 04 CD 81                      by1 :=       $0x81
08004E03: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004E09: D2 06                         if -k go     $0x6
08004E0B: B4 C2 01 9C                   jumpg        b.0x19C
08004E0F: 04 0D                         by1 :=       $0xD
08004E11: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004E17: D2 06                         if -k go     $0x6
08004E19: B4 C2 01 9C                   jumpg        b.0x19C
08004E1D: 18 C2 01 B8                   r:=          b.0x1B8
08004E21: 1A 46 86                      w move       b.0x18,r.0x18
08004E24: 85                            bi2 clr
08004E25: 52 46 D1                      w swap       b.0x18,r2
08004E28: 21 C2 01 B8                   w2 =:        b.0x1B8
08004E2C: FC 87 C2 01 A4                by set1      b.0x1A4
08004E31: C0 18                         go           $0x18
08004E33: 0C CD C1                      w1 :=        $0xC1
08004E36: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004E3C: D2 06                         if -k go     $0x6
08004E3E: B4 C2 01 9C                   jumpg        b.0x19C
08004E42: 84                            bi1 clr
08004E43: FE 03                         clrk
08004E45: B4 C2 01 9C                   jumpg        b.0x19C
08004E49: 2D 48 0D                      by comp2     b.0x20,$0xD
08004E4C: C4 26                         if = go      $0x26
08004E4E: 05 C2 01 A5                   by2 :=       b.0x1A5
08004E52: FC 35 01                      by2 +        $0x1
08004E55: 1D C2 01 A5                   by2 =:       b.0x1A5
08004E59: 31 13                         by2 comp     $0x13
08004E5B: D4 07                         if >> go     $0x7
08004E5D: 2D 48 09                      by comp2     b.0x20,$0x9
08004E60: C6 0F                         if >< go     $0xF
08004E62: 18 C2 01 B0                   r:=          b.0x1B0
08004E66: 1A 85 C2 01 B0                w move       r.0x14,b.0x1B0
08004E6B: 48 C2 01 A5                   by stz       b.0x1A5
08004E6F: C1 FE B1                      go           $0xFFFFFFFFFFFFFEB1
08004E72: 18 42                         r:=          b.0x8
08004E74: 1A C2 01 AC 85                w move       b.0x1AC,r.0x14
08004E79: C3 08 00 87 17 00             call         $0x8008717,$0x0
08004E7F: D2 06                         if -k go     $0x6
08004E81: B4 C2 01 9C                   jumpg        b.0x19C
08004E85: 18 42                         r:=          b.0x8
08004E87: 1A 85 C2 01 AC                w move       r.0x14,b.0x1AC
08004E8C: 1A C2 01 B4 46                w move       b.0x1B4,b.0x18
08004E91: 0C 01                         w1 :=        $0x1
08004E93: FE 03                         clrk
08004E95: B4 C2 01 9C                   jumpg        b.0x19C
08004E99: 9C                            entd
08004E9A: FD C0 C2 01 D0                l=:          b.0x1D0
08004E9F: 04 C2 01 C1                   by1 :=       b.0x1C1
08004EA3: 34 CE 00 83                   w1 comp      $0x83
08004EA7: CA 23                         if < go      $0x23
08004EA9: 04 C2 01 C1                   by1 :=       b.0x1C1
08004EAD: 34 CE 00 9C                   w1 comp      $0x9C
08004EB1: C8 19                         if > go      $0x19
08004EB3: 04 C2 01 C1                   by1 :=       b.0x1C1
08004EB7: 6C 0C                         w1 *         $0xC
08004EB9: FE 25 E0 08 00 74 A8          by2 laddr    $0x80074A8+
08004EC0: FD 20 F5 00 C2 01 D8 0C       by bmove     r2.(0x0),b.0x1D8,$0xC
08004EC8: C0 42                         go           $0x42
08004ECA: 06 C2 01 C1                   by3 :=       b.0x1C1
08004ECE: 36 CE 00 A6                   w3 comp      $0xA6
08004ED2: CA 23                         if < go      $0x23
08004ED4: 06 C2 01 C1                   by3 :=       b.0x1C1
08004ED8: 36 CE 00 AA                   w3 comp      $0xAA
08004EDC: C8 19                         if > go      $0x19
08004EDE: 06 C2 01 C1                   by3 :=       b.0x1C1
08004EE2: 6E 0C                         w3 *         $0xC
08004EE4: FE 27 E2 08 00 74 3C          by4 laddr    $0x800743C+
08004EEB: FD 20 F7 00 C2 01 D8 0C       by bmove     r4.(0x0),b.0x1D8,$0xC
08004EF3: C0 17                         go           $0x17
08004EF5: 04 C2 01 C1                   by1 :=       b.0x1C1
08004EF9: 6C 0C                         w1 *         $0xC
08004EFB: FE 25 E0 08 00 74 3C          by2 laddr    $0x800743C+
08004F02: FD 20 F5 00 C2 01 D8 0C       by bmove     r2.(0x0),b.0x1D8,$0xC
08004F0A: 4A C2 01 D4                   w stz        b.0x1D4
08004F0E: 0E C2 01 C4                   w3 :=        b.0x1C4
08004F12: 62 01                         w3 -         $0x1
08004F14: 22 C2 01 E4                   w3 =:        b.0x1E4
08004F18: 2E C2 01 D4 D2                w comp2      b.0x1D4,r3
08004F1D: C8 1E                         if > go      $0x1E
08004F1F: 0D C2 01 D4                   w2 :=        b.0x1D4
08004F23: 04 E9 01 D8                   by1 :=       @b.0x1D8+
08004F27: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004F2D: D2 06                         if -k go     $0x6
08004F2F: B4 C2 01 D0                   jumpg        b.0x1D0
08004F33: BF C2 01 D4 C2 01 E4 EC       d loopi      b.0x1D4,b.0x1E4,$0xFFFFFFFFFFFFFFEC
08004F3B: FE 03                         clrk
08004F3D: B4 C2 01 D0                   jumpg        b.0x1D0
08004F41: 9C                            entd
08004F42: FD C0 C2 01 E8                l=:          b.0x1E8
08004F47: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08004F4C: 2D 48 C4 08 00 52 B4          by comp2     b.0x20,$0x80052B4
08004F53: D4 5B                         if >> go     $0x5B
08004F55: 04 48                         by1 :=       b.0x20
08004F57: B4 E0 08 00 52 B8             jumpg        $0x80052B8+
08004F5D: 84                            bi1 clr
08004F5E: 20 C2 01 C4                   w1 =:        b.0x1C4
08004F62: 18 42                         r:=          b.0x8
08004F64: 20 85                         w1 =:        r.0x14
08004F66: 19 48 86                      by move      b.0x20,r.0x18
08004F69: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08004F6F: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
08004F75: D2 06                         if -k go     $0x6
08004F77: B4 C2 01 E8                   jumpg        b.0x1E8
08004F7B: 18 42                         r:=          b.0x8
08004F7D: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08004F83: 1A 0A C2 01 CC                w move       $0xA,b.0x1CC
08004F88: C0 26                         go           $0x26
08004F8A: 0C 38                         w1 :=        $0x38
08004F8C: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08004F92: D2 06                         if -k go     $0x6
08004F94: B4 C2 01 E8                   jumpg        b.0x1E8
08004F98: 04 CD 9C                      by1 :=       $0x9C
08004F9B: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08004FA1: D2 06                         if -k go     $0x6
08004FA3: B4 C2 01 E8                   jumpg        b.0x1E8
08004FA7: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
08004FAC: C0 02                         go           $0x2
08004FAE: FE 03                         clrk
08004FB0: B4 C2 01 E8                   jumpg        b.0x1E8
08004FB4: 9C                            entd
08004FB5: FD C0 C2 01 EC                l=:          b.0x1EC
08004FBA: 1A 0A C2 01 CC                w move       $0xA,b.0x1CC
08004FBF: 2D 48 C4 08 00 54 C4          by comp2     b.0x20,$0x80054C4
08004FC6: D4 5E                         if >> go     $0x5E
08004FC8: 04 48                         by1 :=       b.0x20
08004FCA: B4 E0 08 00 54 C8             jumpg        $0x80054C8+
08004FD0: 0C C2 01 C4                   w1 :=        b.0x1C4
08004FD4: 54 01                         w1 +         $0x1
08004FD6: 20 C2 01 C4                   w1 =:        b.0x1C4
08004FDA: 18 42                         r:=          b.0x8
08004FDC: 20 85                         w1 =:        r.0x14
08004FDE: 19 48 86                      by move      b.0x20,r.0x18
08004FE1: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08004FE7: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
08004FED: D2 06                         if -k go     $0x6
08004FEF: B4 C2 01 EC                   jumpg        b.0x1EC
08004FF3: 18 42                         r:=          b.0x8
08004FF5: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08004FFB: 44 D0                         w test       r1
08004FFD: C6 24                         if >< go     $0x24
08004FFF: 0C 36                         w1 :=        $0x36
08005001: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005007: D2 06                         if -k go     $0x6
08005009: B4 C2 01 EC                   jumpg        b.0x1EC
0800500D: 04 CD 9C                      by1 :=       $0x9C
08005010: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005016: D2 06                         if -k go     $0x6
08005018: B4 C2 01 EC                   jumpg        b.0x1EC
0800501C: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
08005021: C1 00 9E                      go           $0x9E
08005024: 0C C2 01 C4                   w1 :=        b.0x1C4
08005028: 54 01                         w1 +         $0x1
0800502A: 20 C2 01 C4                   w1 =:        b.0x1C4
0800502E: 18 42                         r:=          b.0x8
08005030: 20 85                         w1 =:        r.0x14
08005032: 19 0D 86                      by move      $0xD,r.0x18
08005035: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
0800503B: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
08005041: D2 06                         if -k go     $0x6
08005043: B4 C2 01 EC                   jumpg        b.0x1EC
08005047: 18 42                         r:=          b.0x8
08005049: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
0800504F: 44 D0                         w test       r1
08005051: C6 21                         if >< go     $0x21
08005053: 0C 36                         w1 :=        $0x36
08005055: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800505B: D2 06                         if -k go     $0x6
0800505D: B4 C2 01 EC                   jumpg        b.0x1EC
08005061: 04 CD 9C                      by1 :=       $0x9C
08005064: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800506A: D2 06                         if -k go     $0x6
0800506C: B4 C2 01 EC                   jumpg        b.0x1EC
08005070: C0 4A                         go           $0x4A
08005072: 05 C2 01 C0                   by2 :=       b.0x1C0
08005076: 35 CE 00 9C                   w2 comp      $0x9C
0800507A: C6 13                         if >< go     $0x13
0800507C: 04 CD 9C                      by1 :=       $0x9C
0800507F: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005085: D2 06                         if -k go     $0x6
08005087: B4 C2 01 EC                   jumpg        b.0x1EC
0800508B: C0 2F                         go           $0x2F
0800508D: 0C 38                         w1 :=        $0x38
0800508F: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005095: D2 06                         if -k go     $0x6
08005097: B4 C2 01 EC                   jumpg        b.0x1EC
0800509B: 04 CD 9C                      by1 :=       $0x9C
0800509E: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080050A4: D2 06                         if -k go     $0x6
080050A6: B4 C2 01 EC                   jumpg        b.0x1EC
080050AA: 04 C2 01 C0                   by1 :=       b.0x1C0
080050AE: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080050B4: D2 06                         if -k go     $0x6
080050B6: B4 C2 01 EC                   jumpg        b.0x1EC
080050BA: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
080050BF: FE 03                         clrk
080050C1: B4 C2 01 EC                   jumpg        b.0x1EC
080050C5: 9C                            entd
080050C6: FD C0 C2 01 F0                l=:          b.0x1F0
080050CB: 1A 08 C2 01 CC                w move       $0x8,b.0x1CC
080050D0: 2D 48 C4 08 00 56 40          by comp2     b.0x20,$0x8005640
080050D7: D5 00 ED                      if >> go     $0xED
080050DA: 04 48                         by1 :=       b.0x20
080050DC: B4 E0 08 00 56 44             jumpg        $0x8005644+
080050E2: 0C C2 01 C8                   w1 :=        b.0x1C8
080050E6: 60 01                         w1 -         $0x1
080050E8: 20 C2 01 C8                   w1 =:        b.0x1C8
080050EC: 44 D0                         w test       r1
080050EE: CC 13                         if >= go     $0x13
080050F0: 0C CD AB                      w1 :=        $0xAB
080050F3: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080050F9: D2 06                         if -k go     $0x6
080050FB: B4 C2 01 F0                   jumpg        b.0x1F0
080050FF: C0 11                         go           $0x11
08005101: 04 CD 9E                      by1 :=       $0x9E
08005104: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800510A: D2 06                         if -k go     $0x6
0800510C: B4 C2 01 F0                   jumpg        b.0x1F0
08005110: C1 00 C8                      go           $0xC8
08005113: C1 00 C5                      go           $0xC5
08005116: 84                            bi1 clr
08005117: 20 C2 01 C4                   w1 =:        b.0x1C4
0800511B: 18 42                         r:=          b.0x8
0800511D: 20 85                         w1 =:        r.0x14
0800511F: 19 48 86                      by move      b.0x20,r.0x18
08005122: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005128: C3 08 00 44 62 00             call         $0x8004462,$0x0
0800512E: D2 06                         if -k go     $0x6
08005130: B4 C2 01 F0                   jumpg        b.0x1F0
08005134: 18 42                         r:=          b.0x8
08005136: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
0800513C: 44 D0                         w test       r1
0800513E: C6 18                         if >< go     $0x18
08005140: 0C CD C1                      w1 :=        $0xC1
08005143: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005149: D2 06                         if -k go     $0x6
0800514B: B4 C2 01 F0                   jumpg        b.0x1F0
0800514F: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005154: C0 07                         go           $0x7
08005156: 1A 09 C2 01 CC                w move       $0x9,b.0x1CC
0800515B: C1 00 7D                      go           $0x7D
0800515E: 44 C2 01 C8                   w test       b.0x1C8
08005162: CE 11                         if <= go     $0x11
08005164: 0C CD AC                      w1 :=        $0xAC
08005167: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800516D: D2 06                         if -k go     $0x6
0800516F: B4 C2 01 F0                   jumpg        b.0x1F0
08005173: 85                            bi2 clr
08005174: 21 C2 01 C4                   w2 =:        b.0x1C4
08005178: 18 42                         r:=          b.0x8
0800517A: 21 85                         w2 =:        r.0x14
0800517C: 19 48 86                      by move      b.0x20,r.0x18
0800517F: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005185: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
0800518B: D2 06                         if -k go     $0x6
0800518D: B4 C2 01 F0                   jumpg        b.0x1F0
08005191: 18 42                         r:=          b.0x8
08005193: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005199: 1A 0A C2 01 CC                w move       $0xA,b.0x1CC
0800519E: C0 3A                         go           $0x3A
080051A0: 0C 38                         w1 :=        $0x38
080051A2: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080051A8: D2 06                         if -k go     $0x6
080051AA: B4 C2 01 F0                   jumpg        b.0x1F0
080051AE: 04 CD 9C                      by1 :=       $0x9C
080051B1: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080051B7: D2 06                         if -k go     $0x6
080051B9: B4 C2 01 F0                   jumpg        b.0x1F0
080051BD: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
080051C2: C0 16                         go           $0x16
080051C4: 0C CD C1                      w1 :=        $0xC1
080051C7: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080051CD: D2 06                         if -k go     $0x6
080051CF: B4 C2 01 F0                   jumpg        b.0x1F0
080051D3: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080051D8: FE 03                         clrk
080051DA: B4 C2 01 F0                   jumpg        b.0x1F0
080051DE: 9C                            entd
080051DF: FD C0 C2 01 F4                l=:          b.0x1F4
080051E4: 1A 07 C2 01 CC                w move       $0x7,b.0x1CC
080051E9: 2D 48 C4 08 00 58 50          by comp2     b.0x20,$0x8005850
080051F0: D5 00 DF                      if >> go     $0xDF
080051F3: 04 48                         by1 :=       b.0x20
080051F5: B4 E0 08 00 58 54             jumpg        $0x8005854+
080051FB: 44 C2 01 C4                   w test       b.0x1C4
080051FF: CA 1E                         if < go      $0x1E
08005201: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
08005208: 4F C2 01 C4                   w incr       b.0x1C4
0800520C: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
08005212: D2 06                         if -k go     $0x6
08005214: B4 C2 01 F4                   jumpg        b.0x1F4
08005218: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
0800521D: 1A 4A C2 01 00                w move       b.0x28,b.0x100
08005222: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08005228: D2 06                         if -k go     $0x6
0800522A: B4 C2 01 F4                   jumpg        b.0x1F4
0800522E: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08005233: 44 D0                         w test       r1
08005235: C6 09                         if >< go     $0x9
08005237: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
0800523C: C0 22                         go           $0x22
0800523E: 44 4A                         w test       b.0x28
08005240: C4 1E                         if = go      $0x1E
08005242: 2D C2 01 C2 CD A5             by comp2     b.0x1C2,$0xA5
08005248: C4 16                         if = go      $0x16
0800524A: 0C CD BB                      w1 :=        $0xBB
0800524D: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005253: D2 06                         if -k go     $0x6
08005255: B4 C2 01 F4                   jumpg        b.0x1F4
08005259: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
0800525E: C1 01 05                      go           $0x105
08005261: 44 C2 01 C4                   w test       b.0x1C4
08005265: CA 59                         if < go      $0x59
08005267: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
0800526E: 0C C2 01 C4                   w1 :=        b.0x1C4
08005272: 54 01                         w1 +         $0x1
08005274: 20 C2 01 C4                   w1 =:        b.0x1C4
08005278: 18 42                         r:=          b.0x8
0800527A: 20 85                         w1 =:        r.0x14
0800527C: 19 48 86                      by move      b.0x20,r.0x18
0800527F: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005285: C3 08 00 44 62 00             call         $0x8004462,$0x0
0800528B: D2 06                         if -k go     $0x6
0800528D: B4 C2 01 F4                   jumpg        b.0x1F4
08005291: 18 42                         r:=          b.0x8
08005293: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005299: 44 D0                         w test       r1
0800529B: C6 21                         if >< go     $0x21
0800529D: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
080052A3: D2 06                         if -k go     $0x6
080052A5: B4 C2 01 F4                   jumpg        b.0x1F4
080052A9: 04 48                         by1 :=       b.0x20
080052AB: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080052B1: D2 06                         if -k go     $0x6
080052B3: B4 C2 01 F4                   jumpg        b.0x1F4
080052B7: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
080052BC: C0 10                         go           $0x10
080052BE: 04 48                         by1 :=       b.0x20
080052C0: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080052C6: D2 06                         if -k go     $0x6
080052C8: B4 C2 01 F4                   jumpg        b.0x1F4
080052CC: C1 00 97                      go           $0x97
080052CF: 44 C2 01 C4                   w test       b.0x1C4
080052D3: CB 00 84                      if < go      $0x84
080052D6: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
080052DD: 0C C2 01 C4                   w1 :=        b.0x1C4
080052E1: 54 01                         w1 +         $0x1
080052E3: 20 C2 01 C4                   w1 =:        b.0x1C4
080052E7: 18 42                         r:=          b.0x8
080052E9: 20 85                         w1 =:        r.0x14
080052EB: 19 0D 86                      by move      $0xD,r.0x18
080052EE: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
080052F4: C3 08 00 44 62 00             call         $0x8004462,$0x0
080052FA: D2 06                         if -k go     $0x6
080052FC: B4 C2 01 F4                   jumpg        b.0x1F4
08005300: 18 42                         r:=          b.0x8
08005302: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005308: 44 D0                         w test       r1
0800530A: C6 15                         if >< go     $0x15
0800530C: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
08005312: D2 06                         if -k go     $0x6
08005314: B4 C2 01 F4                   jumpg        b.0x1F4
08005318: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
0800531D: C0 3A                         go           $0x3A
0800531F: 2D C2 01 C0 CD A7             by comp2     b.0x1C0,$0xA7
08005325: C4 0C                         if = go      $0xC
08005327: 05 C2 01 C0                   by2 :=       b.0x1C0
0800532B: 35 CE 00 AA                   w2 comp      $0xAA
0800532F: C6 14                         if >< go     $0x14
08005331: 04 C2 01 C0                   by1 :=       b.0x1C0
08005335: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800533B: D2 06                         if -k go     $0x6
0800533D: B4 C2 01 F4                   jumpg        b.0x1F4
08005341: C0 16                         go           $0x16
08005343: 0C CD C7                      w1 :=        $0xC7
08005346: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800534C: D2 06                         if -k go     $0x6
0800534E: B4 C2 01 F4                   jumpg        b.0x1F4
08005352: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005357: C3 08 00 50 C5 00             call         $0x80050C5,$0x0
0800535D: D2 06                         if -k go     $0x6
0800535F: B4 C2 01 F4                   jumpg        b.0x1F4
08005363: FE 03                         clrk
08005365: B4 C2 01 F4                   jumpg        b.0x1F4
08005369: 9C                            entd
0800536A: FD C0 C2 01 F8                l=:          b.0x1F8
0800536F: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
08005374: 2D 48 C4 08 00 5A 58          by comp2     b.0x20,$0x8005A58
0800537B: D5 01 5B                      if >> go     $0x15B
0800537E: 04 48                         by1 :=       b.0x20
08005380: B4 E0 08 00 5A 5C             jumpg        $0x8005A5C+
08005386: 04 CD 9D                      by1 :=       $0x9D
08005389: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800538F: D2 06                         if -k go     $0x6
08005391: B4 C2 01 F8                   jumpg        b.0x1F8
08005395: 4F C2 01 C8                   w incr       b.0x1C8
08005399: C1 01 51                      go           $0x151
0800539C: C1 01 4E                      go           $0x14E
0800539F: 1A 4A C2 01 00                w move       b.0x28,b.0x100
080053A4: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
080053AA: D2 06                         if -k go     $0x6
080053AC: B4 C2 01 F8                   jumpg        b.0x1F8
080053B0: 1A C2 01 00 4A                w move       b.0x100,b.0x28
080053B5: 44 D0                         w test       r1
080053B7: C6 09                         if >< go     $0x9
080053B9: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080053BE: C0 2E                         go           $0x2E
080053C0: 44 4A                         w test       b.0x28
080053C2: C4 20                         if = go      $0x20
080053C4: 2D C2 01 C2 CD A5             by comp2     b.0x1C2,$0xA5
080053CA: C4 18                         if = go      $0x18
080053CC: 0C CD BB                      w1 :=        $0xBB
080053CF: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080053D5: D2 06                         if -k go     $0x6
080053D7: B4 C2 01 F8                   jumpg        b.0x1F8
080053DB: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080053E0: C0 0C                         go           $0xC
080053E2: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
080053E7: 1A 07 C2 01 CC                w move       $0x7,b.0x1CC
080053EC: C1 00 FE                      go           $0xFE
080053EF: 84                            bi1 clr
080053F0: 20 C2 01 C4                   w1 =:        b.0x1C4
080053F4: 18 42                         r:=          b.0x8
080053F6: 20 85                         w1 =:        r.0x14
080053F8: 19 48 86                      by move      b.0x20,r.0x18
080053FB: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005401: C3 08 00 44 62 00             call         $0x8004462,$0x0
08005407: D2 06                         if -k go     $0x6
08005409: B4 C2 01 F8                   jumpg        b.0x1F8
0800540D: 18 42                         r:=          b.0x8
0800540F: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005415: 44 D0                         w test       r1
08005417: C6 15                         if >< go     $0x15
08005419: 04 48                         by1 :=       b.0x20
0800541B: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005421: D2 06                         if -k go     $0x6
08005423: B4 C2 01 F8                   jumpg        b.0x1F8
08005427: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
0800542C: 1A 07 C2 01 CC                w move       $0x7,b.0x1CC
08005431: C1 00 B9                      go           $0xB9
08005434: 0C CD B2                      w1 :=        $0xB2
08005437: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800543D: D2 06                         if -k go     $0x6
0800543F: B4 C2 01 F8                   jumpg        b.0x1F8
08005443: 04 CD 30                      by1 :=       $0x30
08005446: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800544C: D2 06                         if -k go     $0x6
0800544E: B4 C2 01 F8                   jumpg        b.0x1F8
08005452: 44 C2 01 C8                   w test       b.0x1C8
08005456: CE 11                         if <= go     $0x11
08005458: 0C CD AC                      w1 :=        $0xAC
0800545B: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005461: D2 06                         if -k go     $0x6
08005463: B4 C2 01 F8                   jumpg        b.0x1F8
08005467: 85                            bi2 clr
08005468: 21 C2 01 C4                   w2 =:        b.0x1C4
0800546C: 18 42                         r:=          b.0x8
0800546E: 21 85                         w2 =:        r.0x14
08005470: 19 48 86                      by move      b.0x20,r.0x18
08005473: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005479: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
0800547F: D2 06                         if -k go     $0x6
08005481: B4 C2 01 F8                   jumpg        b.0x1F8
08005485: 18 42                         r:=          b.0x8
08005487: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
0800548D: 1A 0A C2 01 CC                w move       $0xA,b.0x1CC
08005492: C0 58                         go           $0x58
08005494: 0C CD B2                      w1 :=        $0xB2
08005497: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800549D: D2 06                         if -k go     $0x6
0800549F: B4 C2 01 F8                   jumpg        b.0x1F8
080054A3: 04 CD 30                      by1 :=       $0x30
080054A6: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080054AC: D2 06                         if -k go     $0x6
080054AE: B4 C2 01 F8                   jumpg        b.0x1F8
080054B2: 0C 38                         w1 :=        $0x38
080054B4: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080054BA: D2 06                         if -k go     $0x6
080054BC: B4 C2 01 F8                   jumpg        b.0x1F8
080054C0: 04 CD 9C                      by1 :=       $0x9C
080054C3: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080054C9: D2 06                         if -k go     $0x6
080054CB: B4 C2 01 F8                   jumpg        b.0x1F8
080054CF: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
080054D4: C0 16                         go           $0x16
080054D6: 0C CD C1                      w1 :=        $0xC1
080054D9: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080054DF: D2 06                         if -k go     $0x6
080054E1: B4 C2 01 F8                   jumpg        b.0x1F8
080054E5: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080054EA: FE 03                         clrk
080054EC: B4 C2 01 F8                   jumpg        b.0x1F8
080054F0: 9C                            entd
080054F1: FD C0 C2 01 FC                l=:          b.0x1FC
080054F6: 1A 04 C2 01 CC                w move       $0x4,b.0x1CC
080054FB: 2D 48 C4 08 00 5C 68          by comp2     b.0x20,$0x8005C68
08005502: D4 42                         if >> go     $0x42
08005504: 04 48                         by1 :=       b.0x20
08005506: B4 E0 08 00 5C 6C             jumpg        $0x8005C6C+
0800550C: 19 CD A1 C2 01 C2             by move      $0xA1,b.0x1C2
08005512: 04 CD A1                      by1 :=       $0xA1
08005515: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800551B: D2 06                         if -k go     $0x6
0800551D: B4 C2 01 FC                   jumpg        b.0x1FC
08005521: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
08005526: C0 3F                         go           $0x3F
08005528: 19 CD A4 C2 01 C2             by move      $0xA4,b.0x1C2
0800552E: 04 CD A4                      by1 :=       $0xA4
08005531: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005537: D2 06                         if -k go     $0x6
08005539: B4 C2 01 FC                   jumpg        b.0x1FC
0800553D: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
08005542: C0 23                         go           $0x23
08005544: 19 CD A3 C2 01 C2             by move      $0xA3,b.0x1C2
0800554A: 04 CD A3                      by1 :=       $0xA3
0800554D: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005553: D2 06                         if -k go     $0x6
08005555: B4 C2 01 FC                   jumpg        b.0x1FC
08005559: C3 08 00 53 69 00             call         $0x8005369,$0x0
0800555F: D2 06                         if -k go     $0x6
08005561: B4 C2 01 FC                   jumpg        b.0x1FC
08005565: FE 03                         clrk
08005567: B4 C2 01 FC                   jumpg        b.0x1FC
0800556B: 9C                            entd
0800556C: FD C0 C2 02 00                l=:          b.0x200
08005571: 1A 03 C2 01 CC                w move       $0x3,b.0x1CC
08005576: 2D 48 C4 08 00 5E 74          by comp2     b.0x20,$0x8005E74
0800557D: D4 42                         if >> go     $0x42
0800557F: 04 48                         by1 :=       b.0x20
08005581: B4 E0 08 00 5E 78             jumpg        $0x8005E78+
08005587: 19 CD A1 C2 01 C2             by move      $0xA1,b.0x1C2
0800558D: 04 CD A1                      by1 :=       $0xA1
08005590: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005596: D2 06                         if -k go     $0x6
08005598: B4 C2 02 00                   jumpg        b.0x200
0800559C: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
080055A1: C0 3F                         go           $0x3F
080055A3: 19 CD A0 C2 01 C2             by move      $0xA0,b.0x1C2
080055A9: 04 CD A0                      by1 :=       $0xA0
080055AC: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080055B2: D2 06                         if -k go     $0x6
080055B4: B4 C2 02 00                   jumpg        b.0x200
080055B8: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
080055BD: C0 23                         go           $0x23
080055BF: 19 CD 9F C2 01 C2             by move      $0x9F,b.0x1C2
080055C5: 04 CD 9F                      by1 :=       $0x9F
080055C8: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080055CE: D2 06                         if -k go     $0x6
080055D0: B4 C2 02 00                   jumpg        b.0x200
080055D4: C3 08 00 53 69 00             call         $0x8005369,$0x0
080055DA: D2 06                         if -k go     $0x6
080055DC: B4 C2 02 00                   jumpg        b.0x200
080055E0: FE 03                         clrk
080055E2: B4 C2 02 00                   jumpg        b.0x200
080055E6: 9C                            entd
080055E7: FD C0 C2 02 04                l=:          b.0x204
080055EC: 1A 05 C2 01 CC                w move       $0x5,b.0x1CC
080055F1: 2D 48 CD 4E                   by comp2     b.0x20,$0x4E
080055F5: C6 1E                         if >< go     $0x1E
080055F7: 19 CD A5 C2 01 C2             by move      $0xA5,b.0x1C2
080055FD: 04 CD A5                      by1 :=       $0xA5
08005600: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005606: D2 06                         if -k go     $0x6
08005608: B4 C2 02 04                   jumpg        b.0x204
0800560C: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
08005611: C0 16                         go           $0x16
08005613: 0C CD C1                      w1 :=        $0xC1
08005616: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800561C: D2 06                         if -k go     $0x6
0800561E: B4 C2 02 04                   jumpg        b.0x204
08005622: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005627: FE 03                         clrk
08005629: B4 C2 02 04                   jumpg        b.0x204
0800562D: 9C                            entd
0800562E: FD C0 C2 02 08                l=:          b.0x208
08005633: 1A 02 C2 01 CC                w move       $0x2,b.0x1CC
08005638: 2D 48 C4 08 00 5F 74          by comp2     b.0x20,$0x8005F74
0800563F: D4 6D                         if >> go     $0x6D
08005641: 04 48                         by1 :=       b.0x20
08005643: B4 E0 08 00 5F 78             jumpg        $0x8005F78+
08005649: 1A 03 C2 01 CC                w move       $0x3,b.0x1CC
0800564E: C0 6A                         go           $0x6A
08005650: 1A 04 C2 01 CC                w move       $0x4,b.0x1CC
08005655: C0 63                         go           $0x63
08005657: 19 CD A2 C2 01 C2             by move      $0xA2,b.0x1C2
0800565D: 04 CD A2                      by1 :=       $0xA2
08005660: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005666: D2 06                         if -k go     $0x6
08005668: B4 C2 02 08                   jumpg        b.0x208
0800566C: 1A 06 C2 01 CC                w move       $0x6,b.0x1CC
08005671: C0 47                         go           $0x47
08005673: 1A 05 C2 01 CC                w move       $0x5,b.0x1CC
08005678: C0 40                         go           $0x40
0800567A: 0C C2 01 C8                   w1 :=        b.0x1C8
0800567E: 60 01                         w1 -         $0x1
08005680: 20 C2 01 C8                   w1 =:        b.0x1C8
08005684: 44 D0                         w test       r1
08005686: CC 13                         if >= go     $0x13
08005688: 0C CD AB                      w1 :=        $0xAB
0800568B: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005691: D2 06                         if -k go     $0x6
08005693: B4 C2 02 08                   jumpg        b.0x208
08005697: C0 11                         go           $0x11
08005699: 04 CD 9E                      by1 :=       $0x9E
0800569C: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080056A2: D2 06                         if -k go     $0x6
080056A4: B4 C2 02 08                   jumpg        b.0x208
080056A8: C0 10                         go           $0x10
080056AA: C0 0E                         go           $0xE
080056AC: C3 08 00 50 C5 00             call         $0x80050C5,$0x0
080056B2: D2 06                         if -k go     $0x6
080056B4: B4 C2 02 08                   jumpg        b.0x208
080056B8: FE 03                         clrk
080056BA: B4 C2 02 08                   jumpg        b.0x208
080056BE: 9C                            entd
080056BF: FD C0 C2 02 0C                l=:          b.0x20C
080056C4: 4D C2 01 CC                   w set1       b.0x1CC
080056C8: 2D 48 C4 08 00 61 80          by comp2     b.0x20,$0x8006180
080056CF: D5 00 D7                      if >> go     $0xD7
080056D2: 04 48                         by1 :=       b.0x20
080056D4: B4 E0 08 00 61 84             jumpg        $0x8006184+
080056DA: 44 C2 01 C4                   w test       b.0x1C4
080056DE: CA 1E                         if < go      $0x1E
080056E0: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
080056E7: 4F C2 01 C4                   w incr       b.0x1C4
080056EB: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
080056F1: D2 06                         if -k go     $0x6
080056F3: B4 C2 02 0C                   jumpg        b.0x20C
080056F7: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
080056FC: 1A 4A C2 01 00                w move       b.0x28,b.0x100
08005701: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08005707: D2 06                         if -k go     $0x6
08005709: B4 C2 02 0C                   jumpg        b.0x20C
0800570D: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08005712: 44 D0                         w test       r1
08005714: C6 09                         if >< go     $0x9
08005716: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
0800571B: C0 1A                         go           $0x1A
0800571D: 44 4A                         w test       b.0x28
0800571F: C4 16                         if = go      $0x16
08005721: 0C CD BB                      w1 :=        $0xBB
08005724: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800572A: D2 06                         if -k go     $0x6
0800572C: B4 C2 02 0C                   jumpg        b.0x20C
08005730: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005735: C1 01 2A                      go           $0x12A
08005738: 44 C2 01 C4                   w test       b.0x1C4
0800573C: CA 59                         if < go      $0x59
0800573E: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
08005745: 0C C2 01 C4                   w1 :=        b.0x1C4
08005749: 54 01                         w1 +         $0x1
0800574B: 20 C2 01 C4                   w1 =:        b.0x1C4
0800574F: 18 42                         r:=          b.0x8
08005751: 20 85                         w1 =:        r.0x14
08005753: 19 48 86                      by move      b.0x20,r.0x18
08005756: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
0800575C: C3 08 00 44 62 00             call         $0x8004462,$0x0
08005762: D2 06                         if -k go     $0x6
08005764: B4 C2 02 0C                   jumpg        b.0x20C
08005768: 18 42                         r:=          b.0x8
0800576A: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005770: 44 D0                         w test       r1
08005772: C6 21                         if >< go     $0x21
08005774: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
0800577A: D2 06                         if -k go     $0x6
0800577C: B4 C2 02 0C                   jumpg        b.0x20C
08005780: 04 48                         by1 :=       b.0x20
08005782: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005788: D2 06                         if -k go     $0x6
0800578A: B4 C2 02 0C                   jumpg        b.0x20C
0800578E: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
08005793: C0 10                         go           $0x10
08005795: 04 48                         by1 :=       b.0x20
08005797: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800579D: D2 06                         if -k go     $0x6
0800579F: B4 C2 02 0C                   jumpg        b.0x20C
080057A3: C1 00 BC                      go           $0xBC
080057A6: 44 C2 01 C4                   w test       b.0x1C4
080057AA: CB 00 A2                      if < go      $0xA2
080057AD: 19 C2 01 C0 C2 01 C1          by move      b.0x1C0,b.0x1C1
080057B4: 0C C2 01 C4                   w1 :=        b.0x1C4
080057B8: 54 01                         w1 +         $0x1
080057BA: 20 C2 01 C4                   w1 =:        b.0x1C4
080057BE: 18 42                         r:=          b.0x8
080057C0: 20 85                         w1 =:        r.0x14
080057C2: 19 0D 86                      by move      $0xD,r.0x18
080057C5: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
080057CB: C3 08 00 44 62 00             call         $0x8004462,$0x0
080057D1: D2 06                         if -k go     $0x6
080057D3: B4 C2 02 0C                   jumpg        b.0x20C
080057D7: 18 42                         r:=          b.0x8
080057D9: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
080057DF: 44 D0                         w test       r1
080057E1: C6 15                         if >< go     $0x15
080057E3: C3 08 00 4E 99 00             call         $0x8004E99,$0x0
080057E9: D2 06                         if -k go     $0x6
080057EB: B4 C2 02 0C                   jumpg        b.0x20C
080057EF: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
080057F4: C0 58                         go           $0x58
080057F6: 2D C2 01 C0 CD A8             by comp2     b.0x1C0,$0xA8
080057FC: C6 18                         if >< go     $0x18
080057FE: 04 C2 01 C0                   by1 :=       b.0x1C0
08005802: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005808: D2 06                         if -k go     $0x6
0800580A: B4 C2 02 0C                   jumpg        b.0x20C
0800580E: 4A C2 01 CC                   w stz        b.0x1CC
08005812: C0 3A                         go           $0x3A
08005814: 2D C2 01 C0 CD A7             by comp2     b.0x1C0,$0xA7
0800581A: C4 0C                         if = go      $0xC
0800581C: 05 C2 01 C0                   by2 :=       b.0x1C0
08005820: 35 CE 00 AA                   w2 comp      $0xAA
08005824: C6 14                         if >< go     $0x14
08005826: 04 C2 01 C0                   by1 :=       b.0x1C0
0800582A: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005830: D2 06                         if -k go     $0x6
08005832: B4 C2 02 0C                   jumpg        b.0x20C
08005836: C0 16                         go           $0x16
08005838: 0C CD C7                      w1 :=        $0xC7
0800583B: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005841: D2 06                         if -k go     $0x6
08005843: B4 C2 02 0C                   jumpg        b.0x20C
08005847: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
0800584C: 2E C2 01 CC 01                w comp2      b.0x1CC,$0x1
08005851: C6 0E                         if >< go     $0xE
08005853: C3 08 00 56 2D 00             call         $0x800562D,$0x0
08005859: D2 06                         if -k go     $0x6
0800585B: B4 C2 02 0C                   jumpg        b.0x20C
0800585F: FE 03                         clrk
08005861: B4 C2 02 0C                   jumpg        b.0x20C
08005865: 9C                            entd
08005866: FD C0 C2 02 10                l=:          b.0x210
0800586B: 4A C2 01 CC                   w stz        b.0x1CC
0800586F: 2D 48 C4 08 00 63 88          by comp2     b.0x20,$0x8006388
08005876: D5 01 51                      if >> go     $0x151
08005879: 04 48                         by1 :=       b.0x20
0800587B: B4 E0 08 00 63 8C             jumpg        $0x800638C+
08005881: 4F C2 01 C8                   w incr       b.0x1C8
08005885: 04 CD 9D                      by1 :=       $0x9D
08005888: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800588E: D2 06                         if -k go     $0x6
08005890: B4 C2 02 10                   jumpg        b.0x210
08005894: C1 01 47                      go           $0x147
08005897: C1 01 44                      go           $0x144
0800589A: 1A 4A C2 01 00                w move       b.0x28,b.0x100
0800589F: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
080058A5: D2 06                         if -k go     $0x6
080058A7: B4 C2 02 10                   jumpg        b.0x210
080058AB: 1A C2 01 00 4A                w move       b.0x100,b.0x28
080058B0: 44 D0                         w test       r1
080058B2: C6 09                         if >< go     $0x9
080058B4: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080058B9: C0 25                         go           $0x25
080058BB: 44 4A                         w test       b.0x28
080058BD: C4 18                         if = go      $0x18
080058BF: 0C CD BB                      w1 :=        $0xBB
080058C2: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080058C8: D2 06                         if -k go     $0x6
080058CA: B4 C2 02 10                   jumpg        b.0x210
080058CE: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080058D3: C0 0B                         go           $0xB
080058D5: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
080058DA: 4D C2 01 CC                   w set1       b.0x1CC
080058DE: C1 00 FD                      go           $0xFD
080058E1: 84                            bi1 clr
080058E2: 20 C2 01 C4                   w1 =:        b.0x1C4
080058E6: 18 42                         r:=          b.0x8
080058E8: 20 85                         w1 =:        r.0x14
080058EA: 19 48 86                      by move      b.0x20,r.0x18
080058ED: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
080058F3: C3 08 00 44 62 00             call         $0x8004462,$0x0
080058F9: D2 06                         if -k go     $0x6
080058FB: B4 C2 02 10                   jumpg        b.0x210
080058FF: 18 42                         r:=          b.0x8
08005901: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005907: 44 D0                         w test       r1
08005909: C6 15                         if >< go     $0x15
0800590B: 04 48                         by1 :=       b.0x20
0800590D: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005913: D2 06                         if -k go     $0x6
08005915: B4 C2 02 10                   jumpg        b.0x210
08005919: 1A 3F C2 01 C4                w move       $0x3F,b.0x1C4
0800591E: 4D C2 01 CC                   w set1       b.0x1CC
08005922: C1 00 B9                      go           $0xB9
08005925: 0C CD CB                      w1 :=        $0xCB
08005928: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800592E: D2 06                         if -k go     $0x6
08005930: B4 C2 02 10                   jumpg        b.0x210
08005934: 04 CD A7                      by1 :=       $0xA7
08005937: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800593D: D2 06                         if -k go     $0x6
0800593F: B4 C2 02 10                   jumpg        b.0x210
08005943: 44 C2 01 C8                   w test       b.0x1C8
08005947: CE 11                         if <= go     $0x11
08005949: 0C CD AC                      w1 :=        $0xAC
0800594C: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005952: D2 06                         if -k go     $0x6
08005954: B4 C2 02 10                   jumpg        b.0x210
08005958: 85                            bi2 clr
08005959: 21 C2 01 C4                   w2 =:        b.0x1C4
0800595D: 18 42                         r:=          b.0x8
0800595F: 21 85                         w2 =:        r.0x14
08005961: 19 48 86                      by move      b.0x20,r.0x18
08005964: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
0800596A: C3 08 00 41 FF 00             call         $0x80041FF,$0x0
08005970: D2 06                         if -k go     $0x6
08005972: B4 C2 02 10                   jumpg        b.0x210
08005976: 18 42                         r:=          b.0x8
08005978: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
0800597E: 1A 0A C2 01 CC                w move       $0xA,b.0x1CC
08005983: C0 58                         go           $0x58
08005985: 0C CD CB                      w1 :=        $0xCB
08005988: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800598E: D2 06                         if -k go     $0x6
08005990: B4 C2 02 10                   jumpg        b.0x210
08005994: 04 CD A7                      by1 :=       $0xA7
08005997: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800599D: D2 06                         if -k go     $0x6
0800599F: B4 C2 02 10                   jumpg        b.0x210
080059A3: 0C 38                         w1 :=        $0x38
080059A5: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080059AB: D2 06                         if -k go     $0x6
080059AD: B4 C2 02 10                   jumpg        b.0x210
080059B1: 04 CD 9C                      by1 :=       $0x9C
080059B4: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080059BA: D2 06                         if -k go     $0x6
080059BC: B4 C2 02 10                   jumpg        b.0x210
080059C0: 1A 0C C2 01 CC                w move       $0xC,b.0x1CC
080059C5: C0 16                         go           $0x16
080059C7: 0C CD C1                      w1 :=        $0xC1
080059CA: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080059D0: D2 06                         if -k go     $0x6
080059D2: B4 C2 02 10                   jumpg        b.0x210
080059D6: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
080059DB: FE 03                         clrk
080059DD: B4 C2 02 10                   jumpg        b.0x210
080059E1: 9C                            entd
080059E2: FD C0 C2 02 14                l=:          b.0x214
080059E7: 1A 09 C2 01 CC                w move       $0x9,b.0x1CC
080059EC: 2D 48 C4 08 00 65 98          by comp2     b.0x20,$0x8006598
080059F3: D4 4F                         if >> go     $0x4F
080059F5: 04 48                         by1 :=       b.0x20
080059F7: B4 E0 08 00 65 9C             jumpg        $0x800659C+
080059FD: 0C C2 01 C4                   w1 :=        b.0x1C4
08005A01: 54 01                         w1 +         $0x1
08005A03: 20 C2 01 C4                   w1 =:        b.0x1C4
08005A07: 18 42                         r:=          b.0x8
08005A09: 20 85                         w1 =:        r.0x14
08005A0B: 19 48 86                      by move      b.0x20,r.0x18
08005A0E: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005A14: C3 08 00 44 62 00             call         $0x8004462,$0x0
08005A1A: D2 06                         if -k go     $0x6
08005A1C: B4 C2 02 14                   jumpg        b.0x214
08005A20: 18 42                         r:=          b.0x8
08005A22: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005A28: 44 D0                         w test       r1
08005A2A: C6 16                         if >< go     $0x16
08005A2C: 0C CD C1                      w1 :=        $0xC1
08005A2F: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005A35: D2 06                         if -k go     $0x6
08005A37: B4 C2 02 14                   jumpg        b.0x214
08005A3B: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005A40: C0 6A                         go           $0x6A
08005A42: 0C C2 01 C4                   w1 :=        b.0x1C4
08005A46: 54 01                         w1 +         $0x1
08005A48: 20 C2 01 C4                   w1 =:        b.0x1C4
08005A4C: 18 42                         r:=          b.0x8
08005A4E: 20 85                         w1 =:        r.0x14
08005A50: 19 0D 86                      by move      $0xD,r.0x18
08005A53: 19 C2 01 C0 C9 19             by move      b.0x1C0,r.0x19
08005A59: C3 08 00 44 62 00             call         $0x8004462,$0x0
08005A5F: D2 06                         if -k go     $0x6
08005A61: B4 C2 02 14                   jumpg        b.0x214
08005A65: 18 42                         r:=          b.0x8
08005A67: 19 C9 19 C2 01 C0             by move      r.0x19,b.0x1C0
08005A6D: 44 D0                         w test       r1
08005A6F: C6 18                         if >< go     $0x18
08005A71: 0C CD C1                      w1 :=        $0xC1
08005A74: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005A7A: D2 06                         if -k go     $0x6
08005A7C: B4 C2 02 14                   jumpg        b.0x214
08005A80: 1A 0B C2 01 CC                w move       $0xB,b.0x1CC
08005A85: C0 25                         go           $0x25
08005A87: 19 C2 01 C0 C2 01 C2          by move      b.0x1C0,b.0x1C2
08005A8E: 04 C2 01 C0                   by1 :=       b.0x1C0
08005A92: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005A98: D2 06                         if -k go     $0x6
08005A9A: B4 C2 02 14                   jumpg        b.0x214
08005A9E: C3 08 00 58 65 00             call         $0x8005865,$0x0
08005AA4: D2 06                         if -k go     $0x6
08005AA6: B4 C2 02 14                   jumpg        b.0x214
08005AAA: FE 03                         clrk
08005AAC: B4 C2 02 14                   jumpg        b.0x214
08005AB0: 9C                            entd
08005AB1: FD C0 C2 01 BC                l=:          b.0x1BC
08005AB6: 19 CD 20 C2 01 C2             by move      $0x20,b.0x1C2
08005ABC: 4A C2 01 C8                   w stz        b.0x1C8
08005AC0: 1A 46 51                      w move       b.0x18,b.0x44
08005AC3: 4A C2 01 CC                   w stz        b.0x1CC
08005AC7: 0C C2 01 CC                   w1 :=        b.0x1CC
08005ACB: B4 E0 08 00 67 14             jumpg        $0x8006714+
08005AD1: C3 08 00 58 65 00             call         $0x8005865,$0x0
08005AD7: D2 06                         if -k go     $0x6
08005AD9: B4 C2 01 BC                   jumpg        b.0x1BC
08005ADD: C1 00 A1                      go           $0xA1
08005AE0: C3 08 00 56 BE 00             call         $0x80056BE,$0x0
08005AE6: D2 06                         if -k go     $0x6
08005AE8: B4 C2 01 BC                   jumpg        b.0x1BC
08005AEC: C1 00 92                      go           $0x92
08005AEF: C3 08 00 56 2D 00             call         $0x800562D,$0x0
08005AF5: D2 06                         if -k go     $0x6
08005AF7: B4 C2 01 BC                   jumpg        b.0x1BC
08005AFB: C1 00 83                      go           $0x83
08005AFE: C3 08 00 55 6B 00             call         $0x800556B,$0x0
08005B04: D2 06                         if -k go     $0x6
08005B06: B4 C2 01 BC                   jumpg        b.0x1BC
08005B0A: C0 74                         go           $0x74
08005B0C: C3 08 00 54 F0 00             call         $0x80054F0,$0x0
08005B12: D2 06                         if -k go     $0x6
08005B14: B4 C2 01 BC                   jumpg        b.0x1BC
08005B18: C0 66                         go           $0x66
08005B1A: C3 08 00 55 E6 00             call         $0x80055E6,$0x0
08005B20: D2 06                         if -k go     $0x6
08005B22: B4 C2 01 BC                   jumpg        b.0x1BC
08005B26: C0 58                         go           $0x58
08005B28: C3 08 00 53 69 00             call         $0x8005369,$0x0
08005B2E: D2 06                         if -k go     $0x6
08005B30: B4 C2 01 BC                   jumpg        b.0x1BC
08005B34: C0 4A                         go           $0x4A
08005B36: C3 08 00 51 DE 00             call         $0x80051DE,$0x0
08005B3C: D2 06                         if -k go     $0x6
08005B3E: B4 C2 01 BC                   jumpg        b.0x1BC
08005B42: C0 3C                         go           $0x3C
08005B44: C3 08 00 50 C5 00             call         $0x80050C5,$0x0
08005B4A: D2 06                         if -k go     $0x6
08005B4C: B4 C2 01 BC                   jumpg        b.0x1BC
08005B50: C0 2E                         go           $0x2E
08005B52: C3 08 00 59 E1 00             call         $0x80059E1,$0x0
08005B58: D2 06                         if -k go     $0x6
08005B5A: B4 C2 01 BC                   jumpg        b.0x1BC
08005B5E: C0 20                         go           $0x20
08005B60: C3 08 00 4F B4 00             call         $0x8004FB4,$0x0
08005B66: D2 06                         if -k go     $0x6
08005B68: B4 C2 01 BC                   jumpg        b.0x1BC
08005B6C: C0 12                         go           $0x12
08005B6E: C3 08 00 4F 41 00             call         $0x8004F41,$0x0
08005B74: D2 06                         if -k go     $0x6
08005B76: B4 C2 01 BC                   jumpg        b.0x1BC
08005B7A: C0 04                         go           $0x4
08005B7C: C0 02                         go           $0x2
08005B7E: 2E C2 01 CC 0C                w comp2      b.0x1CC,$0xC
08005B83: C4 16                         if = go      $0x16
08005B85: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08005B8B: D2 06                         if -k go     $0x6
08005B8D: B4 C2 01 BC                   jumpg        b.0x1BC
08005B91: 2D 48 17                      by comp2     b.0x20,$0x17
08005B94: C4 05                         if = go      $0x5
08005B96: C1 FF 31                      go           $0xFFFFFFFFFFFFFF31
08005B99: FE 03                         clrk
08005B9B: B4 C2 01 BC                   jumpg        b.0x1BC
08005B9F: 9C                            entd
08005BA0: FD C0 C2 02 90                l=:          b.0x290
08005BA5: C3 08 00 35 BC 00             call         $0x80035BC,$0x0
08005BAB: D2 06                         if -k go     $0x6
08005BAD: B4 C2 02 90                   jumpg        b.0x290
08005BB1: 0D CF 08 00 2C 4C             w2 :=        $0x8002C4C
08005BB7: 20 C2 02 94                   w1 =:        b.0x294
08005BBB: 0C C2 02 94                   w1 :=        b.0x294
08005BBF: C4 0A                         if = go      $0xA
08005BC1: 1A F5 00 F4 08                w move       r2.(0x0),r1.(0x8)
08005BC6: 20 F5 00                      w1 =:        r2.(0x0)
08005BC9: 18 C4 08 00 2C 4C             r:=          $0x8002C4C
08005BCF: 1A CE 00 91 81                w move       $0x91,r.0x4
08005BD4: 04 CD 91                      by1 :=       $0x91
08005BD7: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005BDD: D2 06                         if -k go     $0x6
08005BDF: B4 C2 02 90                   jumpg        b.0x290
08005BE3: C3 08 00 5A B0 00             call         $0x8005AB0,$0x0
08005BE9: D2 06                         if -k go     $0x6
08005BEB: B4 C2 02 90                   jumpg        b.0x290
08005BEF: 19 03 C2 02 24                by move      $0x3,b.0x224
08005BF4: FE 03                         clrk
08005BF6: B4 C2 02 90                   jumpg        b.0x290
08005BFA: 9C                            entd
08005BFB: FD C0 C2 02 98                l=:          b.0x298
08005C00: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08005C06: C4 0F                         if = go      $0xF
08005C08: 18 C4 08 00 2C 4C             r:=          $0x8002C4C
08005C0E: 2E 81 CE 00 91                w comp2      r.0x4,$0x91
08005C13: C4 17                         if = go      $0x17
08005C15: 0C CD C9                      w1 :=        $0xC9
08005C18: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005C1E: D2 06                         if -k go     $0x6
08005C20: B4 C2 02 98                   jumpg        b.0x298
08005C24: 48 C2 02 24                   by stz       b.0x224
08005C28: C0 22                         go           $0x22
08005C2A: 04 CD 88                      by1 :=       $0x88
08005C2D: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005C33: D2 06                         if -k go     $0x6
08005C35: B4 C2 02 98                   jumpg        b.0x298
08005C39: C3 08 00 5A B0 00             call         $0x8005AB0,$0x0
08005C3F: D2 06                         if -k go     $0x6
08005C41: B4 C2 02 98                   jumpg        b.0x298
08005C45: 19 03 C2 02 24                by move      $0x3,b.0x224
08005C4A: FE 03                         clrk
08005C4C: B4 C2 02 98                   jumpg        b.0x298
08005C50: 9C                            entd
08005C51: FD C0 C2 02 9C                l=:          b.0x29C
08005C56: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08005C5C: C4 0F                         if = go      $0xF
08005C5E: 18 C4 08 00 2C 4C             r:=          $0x8002C4C
08005C64: 2E 81 CE 00 91                w comp2      r.0x4,$0x91
08005C69: C4 17                         if = go      $0x17
08005C6B: 0C CD CA                      w1 :=        $0xCA
08005C6E: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005C74: D2 06                         if -k go     $0x6
08005C76: B4 C2 02 9C                   jumpg        b.0x29C
08005C7A: 48 C2 02 24                   by stz       b.0x224
08005C7E: C0 1B                         go           $0x1B
08005C80: 1A CE 00 87 81                w move       $0x87,r.0x4
08005C85: 04 CD 87                      by1 :=       $0x87
08005C88: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005C8E: D2 06                         if -k go     $0x6
08005C90: B4 C2 02 9C                   jumpg        b.0x29C
08005C94: 19 03 C2 02 24                by move      $0x3,b.0x224
08005C99: FE 03                         clrk
08005C9B: B4 C2 02 9C                   jumpg        b.0x29C
08005C9F: 9C                            entd
08005CA0: FD C0 C2 02 A0                l=:          b.0x2A0
08005CA5: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08005CAB: C4 1A                         if = go      $0x1A
08005CAD: 18 C4 08 00 2C 4C             r:=          $0x8002C4C
08005CB3: 0C 81                         w1 :=        r.0x4
08005CB5: 20 C2 02 A8                   w1 =:        b.0x2A8
08005CB9: 34 CE 00 91                   w1 comp      $0x91
08005CBD: C4 1D                         if = go      $0x1D
08005CBF: 34 CE 00 87                   w1 comp      $0x87
08005CC3: C4 17                         if = go      $0x17
08005CC5: 0C CD C8                      w1 :=        $0xC8
08005CC8: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005CCE: D2 06                         if -k go     $0x6
08005CD0: B4 C2 02 A0                   jumpg        b.0x2A0
08005CD4: 48 C2 02 24                   by stz       b.0x224
08005CD8: C0 42                         go           $0x42
08005CDA: 0D C4 08 00 2C 4C             w2 :=        $0x8002C4C
08005CE0: 21 C2 02 A4                   w2 =:        b.0x2A4
08005CE4: 1A F5 08 C4 08 00 2C 4C       w move       r2.(0x8),$0x8002C4C
08005CEC: 4A F5 08                      w stz        r2.(0x8)
08005CEF: 18 42                         r:=          b.0x8
08005CF1: 21 85                         w2 =:        r.0x14
08005CF3: C3 08 00 36 01 00             call         $0x8003601,$0x0
08005CF9: D2 06                         if -k go     $0x6
08005CFB: B4 C2 02 A0                   jumpg        b.0x2A0
08005CFF: 18 42                         r:=          b.0x8
08005D01: 1A 85 C2 02 A4                w move       r.0x14,b.0x2A4
08005D06: 04 CD 8A                      by1 :=       $0x8A
08005D09: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08005D0F: D2 06                         if -k go     $0x6
08005D11: B4 C2 02 A0                   jumpg        b.0x2A0
08005D15: 19 03 C2 02 24                by move      $0x3,b.0x224
08005D1A: FE 03                         clrk
08005D1C: B4 C2 02 A0                   jumpg        b.0x2A0
08005D20: 9C                            entd
08005D21: FD C0 C2 02 AC                l=:          b.0x2AC
08005D26: 44 C4 08 00 2C 3C             w test       $0x8002C3C
08005D2C: C4 18                         if = go      $0x18
08005D2E: 0C CD BC                      w1 :=        $0xBC
08005D31: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005D37: D2 06                         if -k go     $0x6
08005D39: B4 C2 02 AC                   jumpg        b.0x2AC
08005D3D: 48 C2 02 24                   by stz       b.0x224
08005D41: C1 02 46                      go           $0x246
08005D44: 4A C2 02 B0                   w stz        b.0x2B0
08005D48: 4A C2 02 B8                   w stz        b.0x2B8
08005D4C: 2D 48 0D                      by comp2     b.0x20,$0xD
08005D4F: C5 01 F8                      if = go      $0x1F8
08005D52: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08005D58: D2 06                         if -k go     $0x6
08005D5A: B4 C2 02 AC                   jumpg        b.0x2AC
08005D5E: 2D 48 17                      by comp2     b.0x20,$0x17
08005D61: C5 01 E6                      if = go      $0x1E6
08005D64: 0D C2 02 B0                   w2 :=        b.0x2B0
08005D68: B4 E1 08 00 67 48             jumpg        $0x8006748+
08005D6E: 2D 48 CD 41                   by comp2     b.0x20,$0x41
08005D72: D8 08                         if << go     $0x8
08005D74: 2D 48 CD 5A                   by comp2     b.0x20,$0x5A
08005D78: DA 38                         if <<= go    $0x38
08005D7A: 2D 48 CD 2D                   by comp2     b.0x20,$0x2D
08005D7E: C4 32                         if = go      $0x32
08005D80: 2D 48 CD 30                   by comp2     b.0x20,$0x30
08005D84: D8 08                         if << go     $0x8
08005D86: 2D 48 CD 39                   by comp2     b.0x20,$0x39
08005D8A: DA 26                         if <<= go    $0x26
08005D8C: 2D 48 CD 28                   by comp2     b.0x20,$0x28
08005D90: C4 20                         if = go      $0x20
08005D92: 2D 48 CD 29                   by comp2     b.0x20,$0x29
08005D96: C4 1A                         if = go      $0x1A
08005D98: 2D 48 CD 3A                   by comp2     b.0x20,$0x3A
08005D9C: C4 14                         if = go      $0x14
08005D9E: 2D 48 CD 3B                   by comp2     b.0x20,$0x3B
08005DA2: C4 0E                         if = go      $0xE
08005DA4: 2D 48 CD 2E                   by comp2     b.0x20,$0x2E
08005DA8: C4 08                         if = go      $0x8
08005DAA: 2D 48 CD 22                   by comp2     b.0x20,$0x22
08005DAE: C6 46                         if >< go     $0x46
08005DB0: C3 08 00 86 91 00             call         $0x8008691,$0x0
08005DB6: D2 06                         if -k go     $0x6
08005DB8: B4 C2 02 AC                   jumpg        b.0x2AC
08005DBC: 20 C2 02 BC                   w1 =:        b.0x2BC
08005DC0: 85                            bi2 clr
08005DC1: 21 C2 02 B4                   w2 =:        b.0x2B4
08005DC5: 06 48                         by3 :=       b.0x20
08005DC7: FD 3F F4 00                   w4 laddr     r1.(0x0)
08005DCB: 57 D1                         w4 +         r2
08005DCD: 1E F7 00                      by3 =:       r4.(0x0)
08005DD0: 44 C2 02 B8                   w test       b.0x2B8
08005DD4: C6 08                         if >< go     $0x8
08005DD6: 20 C2 02 B8                   w1 =:        b.0x2B8
08005DDA: C0 08                         go           $0x8
08005DDC: 18 C2 02 C0                   r:=          b.0x2C0
08005DE0: 20 86                         w1 =:        r.0x18
08005DE2: 0C C2 02 BC                   w1 :=        b.0x2BC
08005DE6: 20 C2 02 C0                   w1 =:        b.0x2C0
08005DEA: 20 C2 02 C4                   w1 =:        b.0x2C4
08005DEE: 4D C2 02 B0                   w set1       b.0x2B0
08005DF2: C0 28                         go           $0x28
08005DF4: 2D 48 0D                      by comp2     b.0x20,$0xD
08005DF7: C6 09                         if >< go     $0x9
08005DF9: 1A 02 C2 02 B0                w move       $0x2,b.0x2B0
08005DFE: C0 1C                         go           $0x1C
08005E00: 2D 48 CD 20                   by comp2     b.0x20,$0x20
08005E04: C4 16                         if = go      $0x16
08005E06: 0C CD C1                      w1 :=        $0xC1
08005E09: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005E0F: D2 06                         if -k go     $0x6
08005E11: B4 C2 02 AC                   jumpg        b.0x2AC
08005E15: 1A 03 C2 02 B0                w move       $0x3,b.0x2B0
08005E1A: C1 01 23                      go           $0x123
08005E1D: 2D 48 CD 41                   by comp2     b.0x20,$0x41
08005E21: D8 08                         if << go     $0x8
08005E23: 2D 48 CD 5A                   by comp2     b.0x20,$0x5A
08005E27: DA 38                         if <<= go    $0x38
08005E29: 2D 48 CD 2D                   by comp2     b.0x20,$0x2D
08005E2D: C4 32                         if = go      $0x32
08005E2F: 2D 48 CD 30                   by comp2     b.0x20,$0x30
08005E33: D8 08                         if << go     $0x8
08005E35: 2D 48 CD 39                   by comp2     b.0x20,$0x39
08005E39: DA 26                         if <<= go    $0x26
08005E3B: 2D 48 CD 28                   by comp2     b.0x20,$0x28
08005E3F: C4 20                         if = go      $0x20
08005E41: 2D 48 CD 29                   by comp2     b.0x20,$0x29
08005E45: C4 1A                         if = go      $0x1A
08005E47: 2D 48 CD 3A                   by comp2     b.0x20,$0x3A
08005E4B: C4 14                         if = go      $0x14
08005E4D: 2D 48 CD 3B                   by comp2     b.0x20,$0x3B
08005E51: C4 0E                         if = go      $0xE
08005E53: 2D 48 CD 2E                   by comp2     b.0x20,$0x2E
08005E57: C4 08                         if = go      $0x8
08005E59: 2D 48 CD 22                   by comp2     b.0x20,$0x22
08005E5D: C6 3E                         if >< go     $0x3E
08005E5F: 2E C2 02 B4 13                w comp2      b.0x2B4,$0x13
08005E64: C6 1E                         if >< go     $0x1E
08005E66: C3 08 00 86 91 00             call         $0x8008691,$0x0
08005E6C: D2 06                         if -k go     $0x6
08005E6E: B4 C2 02 AC                   jumpg        b.0x2AC
08005E72: 18 C2 02 C4                   r:=          b.0x2C4
08005E76: 20 85                         w1 =:        r.0x14
08005E78: 1A 85 C2 02 C4                w move       r.0x14,b.0x2C4
08005E7D: 1A 3F C2 02 B4                w move       $0x3F,b.0x2B4
08005E82: 0D C2 02 B4                   w2 :=        b.0x2B4
08005E86: 55 01                         w2 +         $0x1
08005E88: 21 C2 02 B4                   w2 =:        b.0x2B4
08005E8C: 06 48                         by3 :=       b.0x20
08005E8E: FD 3F C6 02 C4                w4 laddr     @b.0x2C4
08005E93: 57 D1                         w4 +         r2
08005E95: 1E F7 00                      by3 =:       r4.(0x0)
08005E98: C1 00 A1                      go           $0xA1
08005E9B: 2D 48 CD 20                   by comp2     b.0x20,$0x20
08005E9F: C6 41                         if >< go     $0x41
08005EA1: 2E C2 02 B4 13                w comp2      b.0x2B4,$0x13
08005EA6: C6 1E                         if >< go     $0x1E
08005EA8: C3 08 00 86 91 00             call         $0x8008691,$0x0
08005EAE: D2 06                         if -k go     $0x6
08005EB0: B4 C2 02 AC                   jumpg        b.0x2AC
08005EB4: 18 C2 02 C4                   r:=          b.0x2C4
08005EB8: 20 85                         w1 =:        r.0x14
08005EBA: 1A 85 C2 02 C4                w move       r.0x14,b.0x2C4
08005EBF: 1A 3F C2 02 B4                w move       $0x3F,b.0x2B4
08005EC4: 0D C2 02 B4                   w2 :=        b.0x2B4
08005EC8: 55 01                         w2 +         $0x1
08005ECA: 21 C2 02 B4                   w2 =:        b.0x2B4
08005ECE: 06 0D                         by3 :=       $0xD
08005ED0: FD 3F C6 02 C4                w4 laddr     @b.0x2C4
08005ED5: 57 D1                         w4 +         r2
08005ED7: 1E F7 00                      by3 =:       r4.(0x0)
08005EDA: 4A C2 02 B0                   w stz        b.0x2B0
08005EDE: C0 5B                         go           $0x5B
08005EE0: 2D 48 0D                      by comp2     b.0x20,$0xD
08005EE3: C6 42                         if >< go     $0x42
08005EE5: 2E C2 02 B4 13                w comp2      b.0x2B4,$0x13
08005EEA: C6 1E                         if >< go     $0x1E
08005EEC: C3 08 00 86 91 00             call         $0x8008691,$0x0
08005EF2: D2 06                         if -k go     $0x6
08005EF4: B4 C2 02 AC                   jumpg        b.0x2AC
08005EF8: 18 C2 02 C4                   r:=          b.0x2C4
08005EFC: 20 85                         w1 =:        r.0x14
08005EFE: 1A 85 C2 02 C4                w move       r.0x14,b.0x2C4
08005F03: 1A 3F C2 02 B4                w move       $0x3F,b.0x2B4
08005F08: 0D C2 02 B4                   w2 :=        b.0x2B4
08005F0C: 55 01                         w2 +         $0x1
08005F0E: 21 C2 02 B4                   w2 =:        b.0x2B4
08005F12: 06 0D                         by3 :=       $0xD
08005F14: FD 3F C6 02 C4                w4 laddr     @b.0x2C4
08005F19: 57 D1                         w4 +         r2
08005F1B: 1E F7 00                      by3 =:       r4.(0x0)
08005F1E: 1A 02 C2 02 B0                w move       $0x2,b.0x2B0
08005F23: C0 16                         go           $0x16
08005F25: 0C CD C1                      w1 :=        $0xC1
08005F28: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005F2E: D2 06                         if -k go     $0x6
08005F30: B4 C2 02 AC                   jumpg        b.0x2AC
08005F34: 1A 03 C2 02 B0                w move       $0x3,b.0x2B0
08005F39: C0 04                         go           $0x4
08005F3B: C0 02                         go           $0x2
08005F3D: 2E C2 02 B0 02                w comp2      b.0x2B0,$0x2
08005F42: C4 05                         if = go      $0x5
08005F44: C1 FE 08                      go           $0xFFFFFFFFFFFFFE08
08005F47: 2E C2 02 B0 02                w comp2      b.0x2B0,$0x2
08005F4C: C6 21                         if >< go     $0x21
08005F4E: 44 C2 02 B8                   w test       b.0x2B8
08005F52: C4 19                         if = go      $0x19
08005F54: FD 3D 46                      w2 laddr     b.0x18
08005F57: 0C C2 02 B8                   w1 :=        b.0x2B8
08005F5B: 0E 14                         w3 :=        $0x14
08005F5D: FE 03                         clrk
08005F5F: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08005F65: D2 06                         if -k go     $0x6
08005F67: B4 C2 02 AC                   jumpg        b.0x2AC
08005F6B: C0 1C                         go           $0x1C
08005F6D: 18 42                         r:=          b.0x8
08005F6F: 1A C2 02 B8 85                w move       b.0x2B8,r.0x14
08005F74: C3 08 00 87 17 00             call         $0x8008717,$0x0
08005F7A: D2 06                         if -k go     $0x6
08005F7C: B4 C2 02 AC                   jumpg        b.0x2AC
08005F80: 18 42                         r:=          b.0x8
08005F82: 1A 85 C2 02 B8                w move       r.0x14,b.0x2B8
08005F87: FE 03                         clrk
08005F89: B4 C2 02 AC                   jumpg        b.0x2AC
08005F8D: 9C                            entd
08005F8E: FD C0 C2 02 C8                l=:          b.0x2C8
08005F93: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08005F99: C4 17                         if = go      $0x17
08005F9B: 0C CD C3                      w1 :=        $0xC3
08005F9E: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005FA4: D2 06                         if -k go     $0x6
08005FA6: B4 C2 02 C8                   jumpg        b.0x2C8
08005FAA: 48 C2 02 24                   by stz       b.0x224
08005FAE: C0 25                         go           $0x25
08005FB0: 2E C4 08 00 2C 40 06          w comp2      $0x8002C40,$0x6
08005FB7: C6 17                         if >< go     $0x17
08005FB9: 0C CD B0                      w1 :=        $0xB0
08005FBC: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005FC2: D2 06                         if -k go     $0x6
08005FC4: B4 C2 02 C8                   jumpg        b.0x2C8
08005FC8: 48 C2 02 24                   by stz       b.0x224
08005FCC: C0 07                         go           $0x7
08005FCE: 19 03 C2 02 24                by move      $0x3,b.0x224
08005FD3: 1A 05 C4 08 00 2C 40          w move       $0x5,$0x8002C40
08005FDA: FE 03                         clrk
08005FDC: B4 C2 02 C8                   jumpg        b.0x2C8
08005FE0: 9C                            entd
08005FE1: FD C0 C2 02 CC                l=:          b.0x2CC
08005FE6: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08005FEC: C4 17                         if = go      $0x17
08005FEE: 0C CD C3                      w1 :=        $0xC3
08005FF1: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08005FF7: D2 06                         if -k go     $0x6
08005FF9: B4 C2 02 CC                   jumpg        b.0x2CC
08005FFD: 48 C2 02 24                   by stz       b.0x224
08006001: C0 25                         go           $0x25
08006003: 2E C4 08 00 2C 40 05          w comp2      $0x8002C40,$0x5
0800600A: C6 17                         if >< go     $0x17
0800600C: 0C CD C5                      w1 :=        $0xC5
0800600F: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006015: D2 06                         if -k go     $0x6
08006017: B4 C2 02 CC                   jumpg        b.0x2CC
0800601B: 48 C2 02 24                   by stz       b.0x224
0800601F: C0 07                         go           $0x7
08006021: 19 03 C2 02 24                by move      $0x3,b.0x224
08006026: 1A 06 C4 08 00 2C 40          w move       $0x6,$0x8002C40
0800602D: FE 03                         clrk
0800602F: B4 C2 02 CC                   jumpg        b.0x2CC
08006033: 9C                            entd
08006034: FD C0 C2 02 D0                l=:          b.0x2D0
08006039: 44 C4 08 00 2C 4C             w test       $0x8002C4C
0800603F: C4 17                         if = go      $0x17
08006041: 0C CD C3                      w1 :=        $0xC3
08006044: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800604A: D2 06                         if -k go     $0x6
0800604C: B4 C2 02 D0                   jumpg        b.0x2D0
08006050: 48 C2 02 24                   by stz       b.0x224
08006054: C0 07                         go           $0x7
08006056: 19 03 C2 02 24                by move      $0x3,b.0x224
0800605B: 4D C4 08 00 2C 40             w set1       $0x8002C40
08006061: FE 03                         clrk
08006063: B4 C2 02 D0                   jumpg        b.0x2D0
08006067: 9C                            entd
08006068: FD C0 C2 02 18                l=:          b.0x218
0800606D: 4A C2 02 1C                   w stz        b.0x21C
08006071: 19 CD 20 C2 02 26             by move      $0x20,b.0x226
08006077: FC 87 C2 02 24                by set1      b.0x224
0800607C: 2D C2 02 24 01                by comp2     b.0x224,$0x1
08006081: C6 0F                         if >< go     $0xF
08006083: 1A 3F C2 02 8C                w move       $0x3F,b.0x28C
08006088: 19 02 C2 02 24                by move      $0x2,b.0x224
0800608D: C1 02 6F                      go           $0x26F
08006090: 2D C2 02 24 02                by comp2     b.0x224,$0x2
08006095: C7 02 1C                      if >< go     $0x21C
08006098: 2D 48 CD 41                   by comp2     b.0x20,$0x41
0800609C: D8 08                         if << go     $0x8
0800609E: 2D 48 CD 5D                   by comp2     b.0x20,$0x5D
080060A2: DA 08                         if <<= go    $0x8
080060A4: 2D 48 CD 2D                   by comp2     b.0x20,$0x2D
080060A8: C6 14                         if >< go     $0x14
080060AA: 0C C2 02 8C                   w1 :=        b.0x28C
080060AE: 54 01                         w1 +         $0x1
080060B0: 20 C2 02 8C                   w1 =:        b.0x28C
080060B4: 19 48 D8 02 27                by move      b.0x20,b.0x227+
080060B9: C1 01 F6                      go           $0x1F6
080060BC: 0C C2 02 8C                   w1 :=        b.0x28C
080060C0: 54 01                         w1 +         $0x1
080060C2: 20 C2 02 8C                   w1 =:        b.0x28C
080060C6: 19 0D D8 02 27                by move      $0xD,b.0x227+
080060CB: 1A D0 C2 02 DC                w move       r1,b.0x2DC
080060D0: 4A C2 02 D8                   w stz        b.0x2D8
080060D4: FD 3D C2 02 27                w2 laddr     b.0x227
080060D9: 21 C2 02 D4                   w2 =:        b.0x2D4
080060DD: 18 42                         r:=          b.0x8
080060DF: FD 20 C2 02 D4 85 0C          by bmove     b.0x2D4,r.0x14,$0xC
080060E6: C3 08 00 42 C3 00             call         $0x80042C3,$0x0
080060EC: D2 06                         if -k go     $0x6
080060EE: B4 C2 02 18                   jumpg        b.0x218
080060F2: 18 42                         r:=          b.0x8
080060F4: 19 88 C2 02 25                by move      r.0x20,b.0x225
080060F9: 44 D0                         w test       r1
080060FB: C6 1B                         if >< go     $0x1B
080060FD: 0C CD B7                      w1 :=        $0xB7
08006100: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006106: D2 06                         if -k go     $0x6
08006108: B4 C2 02 18                   jumpg        b.0x218
0800610C: 84                            bi1 clr
0800610D: FE 03                         clrk
0800610F: B4 C2 02 18                   jumpg        b.0x218
08006113: C1 01 9C                      go           $0x19C
08006116: 2D C2 02 25 C4 08 00 67 8C    by comp2     b.0x225,$0x800678C
0800611F: D5 01 54                      if >> go     $0x154
08006122: 05 C2 02 25                   by2 :=       b.0x225
08006126: B4 E1 08 00 67 90             jumpg        $0x8006790+
0800612C: C3 08 00 5B 9F 00             call         $0x8005B9F,$0x0
08006132: D2 06                         if -k go     $0x6
08006134: B4 C2 02 18                   jumpg        b.0x218
08006138: C1 01 64                      go           $0x164
0800613B: 0C 37                         w1 :=        $0x37
0800613D: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006143: D2 06                         if -k go     $0x6
08006145: B4 C2 02 18                   jumpg        b.0x218
08006149: 84                            bi1 clr
0800614A: FE 03                         clrk
0800614C: B4 C2 02 18                   jumpg        b.0x218
08006150: C1 01 4C                      go           $0x14C
08006153: C3 08 00 5B FA 00             call         $0x8005BFA,$0x0
08006159: D2 06                         if -k go     $0x6
0800615B: B4 C2 02 18                   jumpg        b.0x218
0800615F: C1 01 3D                      go           $0x13D
08006162: C3 08 00 5C 50 00             call         $0x8005C50,$0x0
08006168: D2 06                         if -k go     $0x6
0800616A: B4 C2 02 18                   jumpg        b.0x218
0800616E: C1 01 2E                      go           $0x12E
08006171: C3 08 00 5C 9F 00             call         $0x8005C9F,$0x0
08006177: D2 06                         if -k go     $0x6
08006179: B4 C2 02 18                   jumpg        b.0x218
0800617D: C1 01 1F                      go           $0x11F
08006180: 04 C2 02 25                   by1 :=       b.0x225
08006184: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
0800618A: D2 06                         if -k go     $0x6
0800618C: B4 C2 02 18                   jumpg        b.0x218
08006190: C3 08 00 5D 20 00             call         $0x8005D20,$0x0
08006196: D2 06                         if -k go     $0x6
08006198: B4 C2 02 18                   jumpg        b.0x218
0800619C: 4D 4D                         w set1       b.0x34
0800619E: C1 00 FE                      go           $0xFE
080061A1: C3 08 00 5F 8D 00             call         $0x8005F8D,$0x0
080061A7: D2 06                         if -k go     $0x6
080061A9: B4 C2 02 18                   jumpg        b.0x218
080061AD: C1 00 EF                      go           $0xEF
080061B0: C3 08 00 5F E0 00             call         $0x8005FE0,$0x0
080061B6: D2 06                         if -k go     $0x6
080061B8: B4 C2 02 18                   jumpg        b.0x218
080061BC: C1 00 E0                      go           $0xE0
080061BF: C3 08 00 60 33 00             call         $0x8006033,$0x0
080061C5: D2 06                         if -k go     $0x6
080061C7: B4 C2 02 18                   jumpg        b.0x218
080061CB: C1 00 D1                      go           $0xD1
080061CE: 04 C2 02 25                   by1 :=       b.0x225
080061D2: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080061D8: D2 06                         if -k go     $0x6
080061DA: B4 C2 02 18                   jumpg        b.0x218
080061DE: 19 03 C2 02 24                by move      $0x3,b.0x224
080061E3: C1 00 B9                      go           $0xB9
080061E6: 19 C2 02 25 C2 01 10          by move      b.0x225,b.0x110
080061ED: C3 08 00 47 B1 00             call         $0x80047B1,$0x0
080061F3: D2 06                         if -k go     $0x6
080061F5: B4 C2 02 18                   jumpg        b.0x218
080061F9: C1 00 A3                      go           $0xA3
080061FC: 2D C2 02 26 CD 20             by comp2     b.0x226,$0x20
08006202: C4 1A                         if = go      $0x1A
08006204: 0C CD BF                      w1 :=        $0xBF
08006207: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800620D: D2 06                         if -k go     $0x6
0800620F: B4 C2 02 18                   jumpg        b.0x218
08006213: 84                            bi1 clr
08006214: FE 03                         clrk
08006216: B4 C2 02 18                   jumpg        b.0x218
0800621A: C0 1F                         go           $0x1F
0800621C: C3 08 00 49 40 00             call         $0x8004940,$0x0
08006222: D2 06                         if -k go     $0x6
08006224: B4 C2 02 18                   jumpg        b.0x218
08006228: 44 D0                         w test       r1
0800622A: C6 0B                         if >< go     $0xB
0800622C: 84                            bi1 clr
0800622D: FE 03                         clrk
0800622F: B4 C2 02 18                   jumpg        b.0x218
08006233: C0 06                         go           $0x6
08006235: 4D C2 02 1C                   w set1       b.0x21C
08006239: C0 63                         go           $0x63
0800623B: 04 C2 02 25                   by1 :=       b.0x225
0800623F: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08006245: D2 06                         if -k go     $0x6
08006247: B4 C2 02 18                   jumpg        b.0x218
0800624B: C3 08 00 4A 12 00             call         $0x8004A12,$0x0
08006251: D2 06                         if -k go     $0x6
08006253: B4 C2 02 18                   jumpg        b.0x218
08006257: 4D 4D                         w set1       b.0x34
08006259: C0 43                         go           $0x43
0800625B: 0C CD B6                      w1 :=        $0xB6
0800625E: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006264: D2 06                         if -k go     $0x6
08006266: B4 C2 02 18                   jumpg        b.0x218
0800626A: 84                            bi1 clr
0800626B: FE 03                         clrk
0800626D: B4 C2 02 18                   jumpg        b.0x218
08006271: C0 2B                         go           $0x2B
08006273: 4D 85                         w set1       r.0x14
08006275: FE 79 C4 08 00 67 74 86 03    w bmove      $0x8006774,r.0x18,$0x3
0800627E: FE 79 C4 08 00 67 80 89 03    w bmove      $0x8006780,r.0x24,$0x3
08006287: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
0800628D: D2 06                         if -k go     $0x6
0800628F: B4 C2 02 18                   jumpg        b.0x218
08006293: 0C CD C6                      w1 :=        $0xC6
08006296: FE 02                         setk
08006298: B4 C2 02 18                   jumpg        b.0x218
0800629C: 2D C2 02 24 02                by comp2     b.0x224,$0x2
080062A1: C6 0E                         if >< go     $0xE
080062A3: 19 C2 02 25 C2 02 26          by move      b.0x225,b.0x226
080062AA: 19 03 C2 02 24                by move      $0x3,b.0x224
080062AF: C0 4D                         go           $0x4D
080062B1: 2D C2 02 24 03                by comp2     b.0x224,$0x3
080062B6: C6 34                         if >< go     $0x34
080062B8: 2D 48 CD 82                   by comp2     b.0x20,$0x82
080062BC: C6 0E                         if >< go     $0xE
080062BE: 1A 3F C2 02 8C                w move       $0x3F,b.0x28C
080062C3: 19 02 C2 02 24                by move      $0x2,b.0x224
080062C8: C0 20                         go           $0x20
080062CA: 2D 48 CD 20                   by comp2     b.0x20,$0x20
080062CE: C4 1A                         if = go      $0x1A
080062D0: 2D 48 0D                      by comp2     b.0x20,$0xD
080062D3: C4 15                         if = go      $0x15
080062D5: 0C 3D                         w1 :=        $0x3D
080062D7: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080062DD: D2 06                         if -k go     $0x6
080062DF: B4 C2 02 18                   jumpg        b.0x218
080062E3: 19 04 C2 02 24                by move      $0x4,b.0x224
080062E8: C0 14                         go           $0x14
080062EA: 2D C2 02 24 04                by comp2     b.0x224,$0x4
080062EF: C6 0D                         if >< go     $0xD
080062F1: 2D 48 CD 20                   by comp2     b.0x20,$0x20
080062F5: C6 07                         if >< go     $0x7
080062F7: 19 03 C2 02 24                by move      $0x3,b.0x224
080062FC: 2D 48 0D                      by comp2     b.0x20,$0xD
080062FF: C4 16                         if = go      $0x16
08006301: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08006307: D2 06                         if -k go     $0x6
08006309: B4 C2 02 18                   jumpg        b.0x218
0800630D: 2D 48 17                      by comp2     b.0x20,$0x17
08006310: C4 05                         if = go      $0x5
08006312: C1 FD 6A                      go           $0xFFFFFFFFFFFFFD6A
08006315: 0C 01                         w1 :=        $0x1
08006317: FE 03                         clrk
08006319: B4 C2 02 18                   jumpg        b.0x218
0800631D: B8 CF 00 00 02 E0             ents         $0x2E0
08006323: 84                            bi1 clr
08006324: 20 4B                         w1 =:        b.0x2C
08006326: 20 4D                         w1 =:        b.0x34
08006328: 4A 50                         w stz        b.0x40
0800632A: FC 87 4E                      by set1      b.0x38
0800632D: 4A 46                         w stz        b.0x18
0800632F: C3 08 00 45 1A 00             call         $0x800451A,$0x0
08006335: 9D                            ifkret
08006336: 2D 48 17                      by comp2     b.0x20,$0x17
08006339: C5 01 9F                      if = go      $0x19F
0800633C: 2D 4E 01                      by comp2     b.0x38,$0x1
0800633F: C7 00 CE                      if >< go     $0xCE
08006342: 2D 48 C4 08 00 6A 04          by comp2     b.0x20,$0x8006A04
08006349: D5 00 B5                      if >> go     $0xB5
0800634C: 05 48                         by2 :=       b.0x20
0800634E: B4 E1 08 00 6A 08             jumpg        $0x8006A08+
08006354: 4F C4 08 00 2C 3C             w incr       $0x8002C3C
0800635A: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08006360: C4 0E                         if = go      $0xE
08006362: 0C CD C3                      w1 :=        $0xC3
08006365: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800636B: 9D                            ifkret
0800636C: C0 0C                         go           $0xC
0800636E: 0C CD AE                      w1 :=        $0xAE
08006371: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006377: 9D                            ifkret
08006378: 48 4E                         by stz       b.0x38
0800637A: C1 00 90                      go           $0x90
0800637D: 19 04 4E                      by move      $0x4,b.0x38
08006380: 1A 4C C2 02 1C                w move       b.0x30,b.0x21C
08006385: C3 08 00 60 67 00             call         $0x8006067,$0x0
0800638B: 9D                            ifkret
0800638C: 1A C2 02 1C 4C                w move       b.0x21C,b.0x30
08006391: 44 D0                         w test       r1
08006393: C6 06                         if >< go     $0x6
08006395: 48 4E                         by stz       b.0x38
08006397: C0 1A                         go           $0x1A
08006399: 44 4D                         w test       b.0x34
0800639B: C6 0B                         if >< go     $0xB
0800639D: 04 0D                         by1 :=       $0xD
0800639F: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080063A5: 9D                            ifkret
080063A6: 44 4C                         w test       b.0x30
080063A8: C4 07                         if = go      $0x7
080063AA: 0C 03                         w1 :=        $0x3
080063AC: 80                            ret
080063AD: C0 04                         go           $0x4
080063AF: 84                            bi1 clr
080063B0: 80                            ret
080063B1: C0 59                         go           $0x59
080063B3: 1A 4A C2 01 00                w move       b.0x28,b.0x100
080063B8: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
080063BE: 9D                            ifkret
080063BF: 1A C2 01 00 4A                w move       b.0x100,b.0x28
080063C4: 44 D0                         w test       r1
080063C6: C6 06                         if >< go     $0x6
080063C8: 48 4E                         by stz       b.0x38
080063CA: C0 0B                         go           $0xB
080063CC: 44 4A                         w test       b.0x28
080063CE: C4 04                         if = go      $0x4
080063D0: 4D 4B                         w set1       b.0x2C
080063D2: 19 02 4E                      by move      $0x2,b.0x38
080063D5: C0 35                         go           $0x35
080063D7: C0 33                         go           $0x33
080063D9: 2E 50 02                      w comp2      b.0x40,$0x2
080063DC: C6 1D                         if >< go     $0x1D
080063DE: C3 08 00 86 91 00             call         $0x8008691,$0x0
080063E4: 9D                            ifkret
080063E5: 20 46                         w1 =:        b.0x18
080063E7: 05 0D                         by2 :=       $0xD
080063E9: FD 3E F4 00                   w3 laddr     r1.(0x0)
080063ED: 1D F6 00                      by2 =:       r3.(0x0)
080063F0: 0C C4 08 00 2C 40             w1 :=        $0x8002C40
080063F6: 80                            ret
080063F7: C0 05                         go           $0x5
080063F9: 0C 07                         w1 :=        $0x7
080063FB: 80                            ret
080063FC: C0 0E                         go           $0xE
080063FE: 04 48                         by1 :=       b.0x20
08006400: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
08006406: 9D                            ifkret
08006407: 19 02 4E                      by move      $0x2,b.0x38
0800640A: C1 00 C6                      go           $0xC6
0800640D: 2D 4E 02                      by comp2     b.0x38,$0x2
08006410: C7 00 C0                      if >< go     $0xC0
08006413: 2D 48 C4 08 00 6C 14          by comp2     b.0x20,$0x8006C14
0800641A: D5 00 AD                      if >> go     $0xAD
0800641D: 05 48                         by2 :=       b.0x20
0800641F: B4 E1 08 00 6C 18             jumpg        $0x8006C18+
08006425: 4F C4 08 00 2C 3C             w incr       $0x8002C3C
0800642B: 44 C4 08 00 2C 4C             w test       $0x8002C4C
08006431: C4 10                         if = go      $0x10
08006433: 0C CD C3                      w1 :=        $0xC3
08006436: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800643C: 9D                            ifkret
0800643D: 48 4E                         by stz       b.0x38
0800643F: C0 26                         go           $0x26
08006441: 44 4B                         w test       b.0x2C
08006443: C4 10                         if = go      $0x10
08006445: 0C CD BE                      w1 :=        $0xBE
08006448: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
0800644E: 9D                            ifkret
0800644F: 48 4E                         by stz       b.0x38
08006451: C0 14                         go           $0x14
08006453: C3 08 00 4C 4B 00             call         $0x8004C4B,$0x0
08006459: 9D                            ifkret
0800645A: 44 D0                         w test       r1
0800645C: C6 06                         if >< go     $0x6
0800645E: 48 4E                         by stz       b.0x38
08006460: C0 05                         go           $0x5
08006462: 0C 02                         w1 :=        $0x2
08006464: 80                            ret
08006465: C0 6B                         go           $0x6B
08006467: 0C CD C2                      w1 :=        $0xC2
0800646A: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
08006470: 9D                            ifkret
08006471: 48 4E                         by stz       b.0x38
08006473: C0 5D                         go           $0x5D
08006475: 1A 4A C2 01 00                w move       b.0x28,b.0x100
0800647A: C3 08 00 46 FA 00             call         $0x80046FA,$0x0
08006480: 9D                            ifkret
08006481: 1A C2 01 00 4A                w move       b.0x100,b.0x28
08006486: 44 D0                         w test       r1
08006488: C6 06                         if >< go     $0x6
0800648A: 48 4E                         by stz       b.0x38
0800648C: C0 08                         go           $0x8
0800648E: 44 4A                         w test       b.0x28
08006490: C4 04                         if = go      $0x4
08006492: 4D 4B                         w set1       b.0x2C
08006494: C0 3C                         go           $0x3C
08006496: 44 C4 08 00 2C 3C             w test       $0x8002C3C
0800649C: C6 19                         if >< go     $0x19
0800649E: 2E C4 08 00 2C 40 01          w comp2      $0x8002C40,$0x1
080064A5: C6 10                         if >< go     $0x10
080064A7: 0C CD BD                      w1 :=        $0xBD
080064AA: C3 08 00 45 D8 00             call         $0x80045D8,$0x0
080064B0: 9D                            ifkret
080064B1: 48 4E                         by stz       b.0x38
080064B3: C0 12                         go           $0x12
080064B5: 04 0D                         by1 :=       $0xD
080064B7: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080064BD: 9D                            ifkret
080064BE: 0C C4 08 00 2C 40             w1 :=        $0x8002C40
080064C4: 80                            ret
080064C5: C0 0B                         go           $0xB
080064C7: 04 48                         by1 :=       b.0x20
080064C9: C3 08 00 46 AF 00             call         $0x80046AF,$0x0
080064CF: 9D                            ifkret
080064D0: 2D 48 0D                      by comp2     b.0x20,$0xD
080064D3: C4 05                         if = go      $0x5
080064D5: C1 FE 5A                      go           $0xFFFFFFFFFFFFFE5A
080064D8: 18 42                         r:=          b.0x8
080064DA: 1A 46 85                      w move       b.0x18,r.0x14
080064DD: C3 08 00 87 17 00             call         $0x8008717,$0x0
080064E3: 9D                            ifkret
080064E4: 18 42                         r:=          b.0x8
080064E6: 1A 85 46                      w move       r.0x14,b.0x18
080064E9: 0C 04                         w1 :=        $0x4
080064EB: 80                            ret
080064EC: B8 CF 00 00 00 20             ents         $0x20
080064F2: 18 42                         r:=          b.0x8
080064F4: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080064FD: 0C 0C                         w1 :=        $0xC
080064FF: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
08006505: 9D                            ifkret
08006506: 20 47                         w1 =:        b.0x1C
08006508: 1A 47 46                      w move       b.0x1C,b.0x18
0800650B: 84                            bi1 clr
0800650C: 18 46                         r:=          b.0x18
0800650E: 20 81                         w1 =:        r.0x4
08006510: 4A 82                         w stz        r.0x8
08006512: 0C 46                         w1 :=        b.0x18
08006514: 80                            ret
08006515: 9C                            entd
08006516: FD C0 4F                      l=:          b.0x3C
08006519: 2E 51 01                      w comp2      b.0x44,$0x1
0800651C: C6 19                         if >< go     $0x19
0800651E: 2D 50 CD 28                   by comp2     b.0x40,$0x28
08006522: C6 07                         if >< go     $0x7
08006524: 1A 02 51                      w move       $0x2,b.0x44
08006527: C0 0B                         go           $0xB
08006529: 2D 50 CD 2E                   by comp2     b.0x40,$0x2E
0800652D: C6 05                         if >< go     $0x5
0800652F: 1A 07 51                      w move       $0x7,b.0x44
08006532: C1 01 0E                      go           $0x10E
08006535: 2E 51 02                      w comp2      b.0x44,$0x2
08006538: C6 19                         if >< go     $0x19
0800653A: 2D 50 CD 28                   by comp2     b.0x40,$0x28
0800653E: C6 07                         if >< go     $0x7
08006540: 1A 03 51                      w move       $0x3,b.0x44
08006543: C0 0B                         go           $0xB
08006545: 2D 50 CD 29                   by comp2     b.0x40,$0x29
08006549: C6 05                         if >< go     $0x5
0800654B: 1A 06 51                      w move       $0x6,b.0x44
0800654E: C1 00 F2                      go           $0xF2
08006551: 2E 51 03                      w comp2      b.0x44,$0x3
08006554: C6 19                         if >< go     $0x19
08006556: 2D 50 CD 3A                   by comp2     b.0x40,$0x3A
0800655A: C6 07                         if >< go     $0x7
0800655C: 1A 04 51                      w move       $0x4,b.0x44
0800655F: C0 0B                         go           $0xB
08006561: 2D 50 CD 29                   by comp2     b.0x40,$0x29
08006565: C6 05                         if >< go     $0x5
08006567: 1A 05 51                      w move       $0x5,b.0x44
0800656A: C1 00 D6                      go           $0xD6
0800656D: 2E 51 04                      w comp2      b.0x44,$0x4
08006570: C6 0E                         if >< go     $0xE
08006572: 2D 50 CD 29                   by comp2     b.0x40,$0x29
08006576: C6 05                         if >< go     $0x5
08006578: 1A 05 51                      w move       $0x5,b.0x44
0800657B: C1 00 C5                      go           $0xC5
0800657E: 2E 51 05                      w comp2      b.0x44,$0x5
08006581: C6 0E                         if >< go     $0xE
08006583: 2D 50 CD 29                   by comp2     b.0x40,$0x29
08006587: C6 05                         if >< go     $0x5
08006589: 1A 06 51                      w move       $0x6,b.0x44
0800658C: C1 00 B4                      go           $0xB4
0800658F: 2E 51 06                      w comp2      b.0x44,$0x6
08006592: C6 0E                         if >< go     $0xE
08006594: 2D 50 CD 2E                   by comp2     b.0x40,$0x2E
08006598: C6 05                         if >< go     $0x5
0800659A: 1A 07 51                      w move       $0x7,b.0x44
0800659D: C1 00 A3                      go           $0xA3
080065A0: 2E 51 07                      w comp2      b.0x44,$0x7
080065A3: C6 29                         if >< go     $0x29
080065A5: 2D 50 CD 28                   by comp2     b.0x40,$0x28
080065A9: C6 07                         if >< go     $0x7
080065AB: 1A 08 51                      w move       $0x8,b.0x44
080065AE: C0 1B                         go           $0x1B
080065B0: 2D 50 CD 3A                   by comp2     b.0x40,$0x3A
080065B4: C6 07                         if >< go     $0x7
080065B6: 1A 0C 51                      w move       $0xC,b.0x44
080065B9: C0 10                         go           $0x10
080065BB: 2D 50 CD 3B                   by comp2     b.0x40,$0x3B
080065BF: C6 07                         if >< go     $0x7
080065C1: 1A 0D 51                      w move       $0xD,b.0x44
080065C4: C0 05                         go           $0x5
080065C6: 1A 0B 51                      w move       $0xB,b.0x44
080065C9: C1 00 77                      go           $0x77
080065CC: 2E 51 08                      w comp2      b.0x44,$0x8
080065CF: C6 18                         if >< go     $0x18
080065D1: 2D 50 CD 3A                   by comp2     b.0x40,$0x3A
080065D5: C6 07                         if >< go     $0x7
080065D7: 1A 09 51                      w move       $0x9,b.0x44
080065DA: C0 0B                         go           $0xB
080065DC: 2D 50 CD 29                   by comp2     b.0x40,$0x29
080065E0: C6 05                         if >< go     $0x5
080065E2: 1A 0A 51                      w move       $0xA,b.0x44
080065E5: C0 5B                         go           $0x5B
080065E7: 2E 51 09                      w comp2      b.0x44,$0x9
080065EA: C6 0D                         if >< go     $0xD
080065EC: 2D 50 CD 29                   by comp2     b.0x40,$0x29
080065F0: C6 05                         if >< go     $0x5
080065F2: 1A 0A 51                      w move       $0xA,b.0x44
080065F5: C0 4B                         go           $0x4B
080065F7: 2E 51 0A                      w comp2      b.0x44,$0xA
080065FA: C6 1D                         if >< go     $0x1D
080065FC: 2D 50 CD 3A                   by comp2     b.0x40,$0x3A
08006600: C6 07                         if >< go     $0x7
08006602: 1A 0C 51                      w move       $0xC,b.0x44
08006605: C0 10                         go           $0x10
08006607: 2D 50 CD 3B                   by comp2     b.0x40,$0x3B
0800660B: C6 07                         if >< go     $0x7
0800660D: 1A 0D 51                      w move       $0xD,b.0x44
08006610: C0 05                         go           $0x5
08006612: 1A 0B 51                      w move       $0xB,b.0x44
08006615: C0 2B                         go           $0x2B
08006617: 2E 51 0B                      w comp2      b.0x44,$0xB
0800661A: C6 18                         if >< go     $0x18
0800661C: 2D 50 CD 3A                   by comp2     b.0x40,$0x3A
08006620: C6 07                         if >< go     $0x7
08006622: 1A 0C 51                      w move       $0xC,b.0x44
08006625: C0 0B                         go           $0xB
08006627: 2D 50 CD 3B                   by comp2     b.0x40,$0x3B
0800662B: C6 05                         if >< go     $0x5
0800662D: 1A 0D 51                      w move       $0xD,b.0x44
08006630: C0 10                         go           $0x10
08006632: 2E 51 0C                      w comp2      b.0x44,$0xC
08006635: C6 0B                         if >< go     $0xB
08006637: 2D 50 CD 3B                   by comp2     b.0x40,$0x3B
0800663B: C6 05                         if >< go     $0x5
0800663D: 1A 0D 51                      w move       $0xD,b.0x44
08006640: FE 03                         clrk
08006642: B4 4F                         jumpg        b.0x3C
08006644: B8 CF 00 00 00 50             ents         $0x50
0800664A: 1A 07 4C                      w move       $0x7,b.0x30
0800664D: 0C 47                         w1 :=        b.0x1C
0800664F: 20 4A                         w1 =:        b.0x28
08006651: 0D 48                         w2 :=        b.0x20
08006653: 21 52                         w2 =:        b.0x48
08006655: 34 D1                         w1 comp      r2
08006657: C8 15                         if > go      $0x15
08006659: 04 CD 2E                      by1 :=       $0x2E
0800665C: 0D 4A                         w2 :=        b.0x28
0800665E: 2D E5 18 D0                   by comp2     @b.0x18+,r1
08006662: C4 08                         if = go      $0x8
08006664: BF 4A 52 F5                   d loopi      b.0x28,b.0x48,$0xFFFFFFFFFFFFFFF5
08006668: C0 04                         go           $0x4
0800666A: 4D 4C                         w set1       b.0x30
0800666C: 85                            bi2 clr
0800666D: 04 E5 18                      by1 :=       @b.0x18+
08006670: 1C 50                         by1 =:       b.0x40
08006672: 1A 4C 51                      w move       b.0x30,b.0x44
08006675: C3 08 00 65 15 00             call         $0x8006515,$0x0
0800667B: 9D                            ifkret
0800667C: 1A 51 4C                      w move       b.0x44,b.0x30
0800667F: 1A 07 4D                      w move       $0x7,b.0x34
08006682: 1A 45 4E                      w move       b.0x14,b.0x38
08006685: 44 4E                         w test       b.0x38
08006687: C4 3E                         if = go      $0x3E
08006689: 4A 4A                         w stz        b.0x28
0800668B: FD 3D C5 38                   w2 laddr     @b.0x38
0800668F: 55 4A                         w2 +         b.0x28
08006691: 04 F5 00                      by1 :=       r2.(0x0)
08006694: 1C 49                         by1 =:       b.0x24
08006696: 30 CD 2E                      by1 comp     $0x2E
08006699: C4 0E                         if = go      $0xE
0800669B: 30 09                         by1 comp     $0x9
0800669D: C4 0A                         if = go      $0xA
0800669F: 30 0D                         by1 comp     $0xD
080066A1: C4 06                         if = go      $0x6
080066A3: BF 4A 13 E8                   d loopi      b.0x28,$0x13,$0xFFFFFFFFFFFFFFE8
080066A7: 2D 49 CD 2E                   by comp2     b.0x24,$0x2E
080066AB: C4 12                         if = go      $0x12
080066AD: 2D 49 0D                      by comp2     b.0x24,$0xD
080066B0: C4 0D                         if = go      $0xD
080066B2: 18 4E                         r:=          b.0x38
080066B4: 1A 85 4E                      w move       r.0x14,b.0x38
080066B7: 44 4E                         w test       b.0x38
080066B9: C6 D0                         if >< go     $0xFFFFFFFFFFFFFFD0
080066BB: C0 0A                         go           $0xA
080066BD: 2D 49 CD 2E                   by comp2     b.0x24,$0x2E
080066C1: C6 04                         if >< go     $0x4
080066C3: 4D 4D                         w set1       b.0x34
080066C5: FD 3C C5 14                   w1 laddr     @b.0x14
080066C9: 19 F4 00 50                   by move      r1.(0x0),b.0x40
080066CD: 1A 4D 51                      w move       b.0x34,b.0x44
080066D0: C3 08 00 65 15 00             call         $0x8006515,$0x0
080066D6: 9D                            ifkret
080066D7: 1A 51 4D                      w move       b.0x44,b.0x34
080066DA: 1A 45 4E                      w move       b.0x14,b.0x38
080066DD: 85                            bi2 clr
080066DE: 21 4A                         w2 =:        b.0x28
080066E0: 21 4B                         w2 =:        b.0x2C
080066E2: FD 3C C5 38                   w1 laddr     @b.0x38
080066E6: 54 4B                         w1 +         b.0x2C
080066E8: 2D F4 00 0D                   by comp2     r1.(0x0),$0xD
080066EC: C4 31                         if = go      $0x31
080066EE: 2E 4C 4D                      w comp2      b.0x30,b.0x34
080066F1: CE 2C                         if <= go     $0x2C
080066F3: 0D 4B                         w2 :=        b.0x2C
080066F5: 55 01                         w2 +         $0x1
080066F7: 21 4B                         w2 =:        b.0x2C
080066F9: 35 13                         w2 comp      $0x13
080066FB: CE 09                         if <= go     $0x9
080066FD: 18 4E                         r:=          b.0x38
080066FF: 1A 85 4E                      w move       r.0x14,b.0x38
08006702: 4A 4B                         w stz        b.0x2C
08006704: FD 3D C5 38                   w2 laddr     @b.0x38
08006708: 55 4B                         w2 +         b.0x2C
0800670A: 19 F5 00 50                   by move      r2.(0x0),b.0x40
0800670E: 1A 4D 51                      w move       b.0x34,b.0x44
08006711: C3 08 00 65 15 00             call         $0x8006515,$0x0
08006717: 9D                            ifkret
08006718: 1A 51 4D                      w move       b.0x44,b.0x34
0800671B: C0 C7                         go           $0xFFFFFFFFFFFFFFC7
0800671D: 05 CD 2D                      by2 :=       $0x2D
08006720: 0E 4A                         w3 :=        b.0x28
08006722: 2D E6 18 D1                   by comp2     @b.0x18+,r2
08006726: C6 29                         if >< go     $0x29
08006728: 0C 4B                         w1 :=        b.0x2C
0800672A: 54 01                         w1 +         $0x1
0800672C: 20 4B                         w1 =:        b.0x2C
0800672E: 34 13                         w1 comp      $0x13
08006730: CE 09                         if <= go     $0x9
08006732: 18 4E                         r:=          b.0x38
08006734: 1A 85 4E                      w move       r.0x14,b.0x38
08006737: 4A 4B                         w stz        b.0x2C
08006739: FD 3D C5 38                   w2 laddr     @b.0x38
0800673D: 55 4B                         w2 +         b.0x2C
0800673F: 04 F5 00                      by1 :=       r2.(0x0)
08006742: 1C 53                         by1 =:       b.0x4C
08006744: 30 CD 2D                      by1 comp     $0x2D
08006747: C4 08                         if = go      $0x8
08006749: 30 0D                         by1 comp     $0xD
0800674B: C4 04                         if = go      $0x4
0800674D: C0 DB                         go           $0xFFFFFFFFFFFFFFDB
0800674F: 04 0D                         by1 :=       $0xD
08006751: 0D 4A                         w2 :=        b.0x28
08006753: 2D E5 18 D0                   by comp2     @b.0x18+,r1
08006757: C6 07                         if >< go     $0x7
08006759: 0C 01                         w1 :=        $0x1
0800675B: 80                            ret
0800675C: C0 23                         go           $0x23
0800675E: FD 3E C5 38                   w3 laddr     @b.0x38
08006762: 56 4B                         w3 +         b.0x2C
08006764: 2D F6 00 D0                   by comp2     r3.(0x0),r1
08006768: C6 06                         if >< go     $0x6
0800676A: 84                            bi1 clr
0800676B: 80                            ret
0800676C: C0 13                         go           $0x13
0800676E: FD 3C C5 38                   w1 laddr     @b.0x38
08006772: 54 4B                         w1 +         b.0x2C
08006774: 07 F4 00                      by4 :=       r1.(0x0)
08006777: 2D E5 18 D3                   by comp2     @b.0x18+,r4
0800677B: C4 04                         if = go      $0x4
0800677D: 84                            bi1 clr
0800677E: 80                            ret
0800677F: 55 01                         w2 +         $0x1
08006781: 21 4A                         w2 =:        b.0x28
08006783: 06 CD 2D                      by3 :=       $0x2D
08006786: 2D E5 18 D2                   by comp2     @b.0x18+,r3
0800678A: C6 2C                         if >< go     $0x2C
0800678C: 0C 4B                         w1 :=        b.0x2C
0800678E: 54 01                         w1 +         $0x1
08006790: 20 4B                         w1 =:        b.0x2C
08006792: 34 13                         w1 comp      $0x13
08006794: CE 09                         if <= go     $0x9
08006796: 18 4E                         r:=          b.0x38
08006798: 1A 85 4E                      w move       r.0x14,b.0x38
0800679B: 4A 4B                         w stz        b.0x2C
0800679D: FD 3D C5 38                   w2 laddr     @b.0x38
080067A1: 55 4B                         w2 +         b.0x2C
080067A3: 04 F5 00                      by1 :=       r2.(0x0)
080067A6: 1C C1 4D                      by1 =:       b.0x4D
080067A9: 30 CD 2D                      by1 comp     $0x2D
080067AC: C4 08                         if = go      $0x8
080067AE: 30 0D                         by1 comp     $0xD
080067B0: C4 04                         if = go      $0x4
080067B2: C0 DA                         go           $0xFFFFFFFFFFFFFFDA
080067B4: C0 4F                         go           $0x4F
080067B6: 04 E5 18                      by1 :=       @b.0x18+
080067B9: 1C 50                         by1 =:       b.0x40
080067BB: 1A 4C 51                      w move       b.0x30,b.0x44
080067BE: C3 08 00 65 15 00             call         $0x8006515,$0x0
080067C4: 9D                            ifkret
080067C5: 1A 51 4C                      w move       b.0x44,b.0x30
080067C8: 0C 4B                         w1 :=        b.0x2C
080067CA: 54 01                         w1 +         $0x1
080067CC: 20 4B                         w1 =:        b.0x2C
080067CE: 34 13                         w1 comp      $0x13
080067D0: CE 09                         if <= go     $0x9
080067D2: 18 4E                         r:=          b.0x38
080067D4: 1A 85 4E                      w move       r.0x14,b.0x38
080067D7: 4A 4B                         w stz        b.0x2C
080067D9: FD 3C C5 38                   w1 laddr     @b.0x38
080067DD: 54 4B                         w1 +         b.0x2C
080067DF: 19 F4 00 50                   by move      r1.(0x0),b.0x40
080067E3: 1A 4D 51                      w move       b.0x34,b.0x44
080067E6: C3 08 00 65 15 00             call         $0x8006515,$0x0
080067EC: 9D                            ifkret
080067ED: 1A 51 4D                      w move       b.0x44,b.0x34
080067F0: FD 3D C5 38                   w2 laddr     @b.0x38
080067F4: 55 4B                         w2 +         b.0x2C
080067F6: 2D F5 00 0D                   by comp2     r2.(0x0),$0xD
080067FA: C4 09                         if = go      $0x9
080067FC: 2E 4C 4D                      w comp2      b.0x30,b.0x34
080067FF: CE 04                         if <= go     $0x4
08006801: C0 C7                         go           $0xFFFFFFFFFFFFFFC7
08006803: C1 FF 4C                      go           $0xFFFFFFFFFFFFFF4C
08006806: 0C 01                         w1 :=        $0x1
08006808: 80                            ret
08006809: B8 CF 00 00 00 28             ents         $0x28
0800680F: 18 42                         r:=          b.0x8
08006811: 1A 45 85                      w move       b.0x14,r.0x14
08006814: FD 20 46 86 0C                by bmove     b.0x18,r.0x18,$0xC
08006819: C3 08 00 88 D6 00             call         $0x80088D6,$0x0
0800681F: 9D                            ifkret
08006820: 0D 48                         w2 :=        b.0x20
08006822: 55 49                         w2 +         b.0x24
08006824: 0E 47                         w3 :=        b.0x1C
08006826: 61 D2                         w2 -         r3
08006828: 55 01                         w2 +         $0x1
0800682A: 21 49                         w2 =:        b.0x24
0800682C: 80                            ret
0800682D: B8 CF 00 00 00 2C             ents         $0x2C
08006833: 1A 46 4A                      w move       b.0x18,b.0x28
08006836: 44 4A                         w test       b.0x28
08006838: C4 33                         if = go      $0x33
0800683A: 4A 49                         w stz        b.0x24
0800683C: FD 3D C5 28                   w2 laddr     @b.0x28
08006840: 55 49                         w2 +         b.0x24
08006842: 04 F5 00                      by1 :=       r2.(0x0)
08006845: 1C 48                         by1 =:       b.0x20
08006847: 30 0D                         by1 comp     $0xD
08006849: C4 14                         if = go      $0x14
0800684B: 18 42                         r:=          b.0x8
0800684D: 1A 45 85                      w move       b.0x14,r.0x14
08006850: C3 08 00 B5 20 00             call         $0x800B520,$0x0
08006856: 9D                            ifkret
08006857: 4F 47                         w incr       b.0x1C
08006859: BF 49 13 E3                   d loopi      b.0x24,$0x13,$0xFFFFFFFFFFFFFFE3
0800685D: 2D 48 0D                      by comp2     b.0x20,$0xD
08006860: C4 0B                         if = go      $0xB
08006862: 18 4A                         r:=          b.0x28
08006864: 1A 85 4A                      w move       r.0x14,b.0x28
08006867: 44 4A                         w test       b.0x28
08006869: C6 D1                         if >< go     $0xFFFFFFFFFFFFFFD1
0800686B: 80                            ret
0800686C: B8 CF 00 00 00 30             ents         $0x30
08006872: 4A 4A                         w stz        b.0x28
08006874: 1A 46 4B                      w move       b.0x18,b.0x2C
08006877: 44 4B                         w test       b.0x2C
08006879: C4 56                         if = go      $0x56
0800687B: 4A 49                         w stz        b.0x24
0800687D: FD 3D C5 2C                   w2 laddr     @b.0x2C
08006881: 55 49                         w2 +         b.0x24
08006883: 04 F5 00                      by1 :=       r2.(0x0)
08006886: 1C 48                         by1 =:       b.0x20
08006888: 30 CD 28                      by1 comp     $0x28
0800688B: C6 06                         if >< go     $0x6
0800688D: 4F 4A                         w incr       b.0x28
0800688F: C0 09                         go           $0x9
08006891: 30 CD 29                      by1 comp     $0x29
08006894: C6 04                         if >< go     $0x4
08006896: 51 4A                         w decr       b.0x28
08006898: 30 0D                         by1 comp     $0xD
0800689A: C4 1D                         if = go      $0x1D
0800689C: 44 4A                         w test       b.0x28
0800689E: C6 07                         if >< go     $0x7
080068A0: 30 CD 3A                      by1 comp     $0x3A
080068A3: C4 14                         if = go      $0x14
080068A5: 18 42                         r:=          b.0x8
080068A7: 1A 45 85                      w move       b.0x14,r.0x14
080068AA: C3 08 00 B5 20 00             call         $0x800B520,$0x0
080068B0: 9D                            ifkret
080068B1: 4F 47                         w incr       b.0x1C
080068B3: BF 49 13 CA                   d loopi      b.0x24,$0x13,$0xFFFFFFFFFFFFFFCA
080068B7: 2D 48 0D                      by comp2     b.0x20,$0xD
080068BA: C4 15                         if = go      $0x15
080068BC: 44 4A                         w test       b.0x28
080068BE: C6 08                         if >< go     $0x8
080068C0: 2D 48 CD 3A                   by comp2     b.0x20,$0x3A
080068C4: C4 0B                         if = go      $0xB
080068C6: 18 4B                         r:=          b.0x2C
080068C8: 1A 85 4B                      w move       r.0x14,b.0x2C
080068CB: 44 4B                         w test       b.0x2C
080068CD: C6 AE                         if >< go     $0xFFFFFFFFFFFFFFAE
080068CF: 80                            ret
080068D0: B8 CF 00 00 00 5C             ents         $0x5C
080068D6: 44 C4 08 00 79 F8             w test       $0x80079F8
080068DC: C6 09                         if >< go     $0x9
080068DE: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
080068E4: 9D                            ifkret
080068E5: 1A CD 72 4B                   w move       $0x72,b.0x2C
080068E9: 4D 4C                         w set1       b.0x30
080068EB: FE 79 C4 08 00 77 10 4D 03    w bmove      $0x8007710,b.0x34,$0x3
080068F4: 1A 3F 50                      w move       $0x3F,b.0x40
080068F7: C3 08 00 B9 7C 07 4B 4C C5 34 C5 38 C5 3C 50 4A call         $0x800B97C,$0x7,b.0x2C,b.0x30,@b.0x34,@b.0x38,@b.0x3C,b.0x40,b.0x28
08006907: 9D                            ifkret
08006908: 0D 46                         w2 :=        b.0x18
0800690A: 21 49                         w2 =:        b.0x24
0800690C: 0C 47                         w1 :=        b.0x1C
0800690E: 2E 49 D0                      w comp2      b.0x24,r1
08006911: C8 10                         if > go      $0x10
08006913: 05 0D                         by2 :=       $0xD
08006915: 0E 49                         w3 :=        b.0x24
08006917: 2D E6 14 D1                   by comp2     @b.0x14+,r2
0800691B: C4 06                         if = go      $0x6
0800691D: 4F 49                         w incr       b.0x24
0800691F: C0 ED                         go           $0xFFFFFFFFFFFFFFED
08006921: 1A CD 72 4B                   w move       $0x72,b.0x2C
08006925: 4D 4C                         w set1       b.0x30
08006927: 0D 49                         w2 :=        b.0x24
08006929: 61 01                         w2 -         $0x1
0800692B: 1A D1 56                      w move       r2,b.0x58
0800692E: 4A 55                         w stz        b.0x54
08006930: 0E 45                         w3 :=        b.0x14
08006932: 22 54                         w3 =:        b.0x50
08006934: FD 20 54 51 0C                by bmove     b.0x50,b.0x44,$0xC
08006939: 1A 3F 50                      w move       $0x3F,b.0x40
0800693C: C3 08 00 B9 7C 07 4B 4C C5 44 C5 48 C5 4C 50 4A call         $0x800B97C,$0x7,b.0x2C,b.0x30,@b.0x44,@b.0x48,@b.0x4C,b.0x40,b.0x28
0800694C: 9D                            ifkret
0800694D: 1A CD 34 4B                   w move       $0x34,b.0x2C
08006951: C3 08 00 B9 7C 02 4B 48       call         $0x800B97C,$0x2,b.0x2C,b.0x20
08006959: 9D                            ifkret
0800695A: 80                            ret
0800695B: B8 CF 00 00 00 54             ents         $0x54
08006961: 1A 3F 4A                      w move       $0x3F,b.0x28
08006964: 0C 46                         w1 :=        b.0x18
08006966: 20 4B                         w1 =:        b.0x2C
08006968: 0D 47                         w2 :=        b.0x1C
0800696A: 21 4E                         w2 =:        b.0x38
0800696C: 34 D1                         w1 comp      r2
0800696E: C8 17                         if > go      $0x17
08006970: 0C 4A                         w1 :=        b.0x28
08006972: 54 01                         w1 +         $0x1
08006974: 20 4A                         w1 =:        b.0x28
08006976: 0E 4B                         w3 :=        b.0x2C
08006978: 05 E6 14                      by2 :=       @b.0x14+
0800697B: 1D E0 08 00 6E 98             by2 =:       $0x8006E98+
08006981: BF 4B 4E EF                   d loopi      b.0x2C,b.0x38,$0xFFFFFFFFFFFFFFEF
08006985: 1A 48 4D                      w move       b.0x20,b.0x34
08006988: 44 4D                         w test       b.0x34
0800698A: C4 32                         if = go      $0x32
0800698C: 4A 4B                         w stz        b.0x2C
0800698E: FD 3D C5 34                   w2 laddr     @b.0x34
08006992: 55 4B                         w2 +         b.0x2C
08006994: 04 F5 00                      by1 :=       r2.(0x0)
08006997: 1C 49                         by1 =:       b.0x24
08006999: 30 0D                         by1 comp     $0xD
0800699B: C4 18                         if = go      $0x18
0800699D: 2E 4A CD 30                   w comp2      b.0x28,$0x30
080069A1: CC 0E                         if >= go     $0xE
080069A3: 0E 4A                         w3 :=        b.0x28
080069A5: 56 01                         w3 +         $0x1
080069A7: 22 4A                         w3 =:        b.0x28
080069A9: 1C E2 08 00 6E 98             by1 =:       $0x8006E98+
080069AF: BF 4B 13 DF                   d loopi      b.0x2C,$0x13,$0xFFFFFFFFFFFFFFDF
080069B3: 18 4D                         r:=          b.0x34
080069B5: 1A 85 4D                      w move       r.0x14,b.0x34
080069B8: 44 4D                         w test       b.0x34
080069BA: C6 D2                         if >< go     $0xFFFFFFFFFFFFFFD2
080069BC: 2E 4A CD 30                   w comp2      b.0x28,$0x30
080069C0: CC 12                         if >= go     $0x12
080069C2: 0C 4A                         w1 :=        b.0x28
080069C4: 54 01                         w1 +         $0x1
080069C6: 20 4A                         w1 =:        b.0x28
080069C8: 19 CD 20 E0 08 00 6E 98       by move      $0x20,$0x8006E98+
080069D0: C0 EC                         go           $0xFFFFFFFFFFFFFFEC
080069D2: 0C CD 31                      w1 :=        $0x31
080069D5: 19 0D E0 08 00 6E 98          by move      $0xD,$0x8006E98+
080069DC: 1A CD 72 4F                   w move       $0x72,b.0x3C
080069E0: 4D 50                         w set1       b.0x40
080069E2: FE 79 C4 08 00 77 1C 51 03    w bmove      $0x800771C,b.0x44,$0x3
080069EB: 1A 3F 54                      w move       $0x3F,b.0x50
080069EE: C3 08 00 B9 7C 07 4F 50 C5 44 C5 48 C5 4C 54 4C call         $0x800B97C,$0x7,b.0x3C,b.0x40,@b.0x44,@b.0x48,@b.0x4C,b.0x50,b.0x30
080069FE: 9D                            ifkret
080069FF: 80                            ret
08006A00: B8 CF 00 00 00 34             ents         $0x34
08006A06: 44 C4 08 00 6E 28             w test       $0x8006E28
08006A0C: C5 00 CE                      if = go      $0xCE
08006A0F: C3 08 00 83 74 00             call         $0x8008374,$0x0
08006A15: 9D                            ifkret
08006A16: 18 42                         r:=          b.0x8
08006A18: 1A 85 45                      w move       r.0x14,b.0x14
08006A1B: 1A 86 46                      w move       r.0x18,b.0x18
08006A1E: 1A 87 47                      w move       r.0x1C,b.0x1C
08006A21: C3 08 00 83 96 00             call         $0x8008396,$0x0
08006A27: 9D                            ifkret
08006A28: 18 42                         r:=          b.0x8
08006A2A: 1A 85 48                      w move       r.0x14,b.0x20
08006A2D: 1A 86 49                      w move       r.0x18,b.0x24
08006A30: 1A 87 4A                      w move       r.0x1C,b.0x28
08006A33: 0D 45                         w2 :=        b.0x14
08006A35: 61 CD 32                      w2 -         $0x32
08006A38: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A3E: FC AA D1 04                   w shl        r2,$0x4
08006A42: 55 46                         w2 +         b.0x18
08006A44: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A4A: FC AA D1 05                   w shl        r2,$0x5
08006A4E: 55 47                         w2 +         b.0x1C
08006A50: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A56: FC AA D1 05                   w shl        r2,$0x5
08006A5A: 55 48                         w2 +         b.0x20
08006A5C: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A62: FC AA D1 06                   w shl        r2,$0x6
08006A66: 55 49                         w2 +         b.0x24
08006A68: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A6E: FC AA D1 06                   w shl        r2,$0x6
08006A72: 55 4A                         w2 +         b.0x28
08006A74: 21 C4 08 00 6E 2C             w2 =:        $0x8006E2C
08006A7A: 84                            bi1 clr
08006A7B: 85                            bi2 clr
08006A7C: FD 67 C4 08 00 77 38 C4 08 00 77 40       by smove     $0x8007738,$0x8007740
08006A88: 0C 0B                         w1 :=        $0xB
08006A8A: 19 CD 35 E0 08 00 6E 34       by move      $0x35,$0x8006E34+
08006A92: 0D 0D                         w2 :=        $0xD
08006A94: 19 CD 43 E1 08 00 6E 34       by move      $0x43,$0x8006E34+
08006A9C: 0E 0E                         w3 :=        $0xE
08006A9E: 19 CD 30 E2 08 00 6E 34       by move      $0x30,$0x8006E34+
08006AA6: 0F 0F                         w4 :=        $0xF
08006AA8: 19 CD 30 E3 08 00 6E 34       by move      $0x30,$0x8006E34+
08006AB0: 0C 10                         w1 :=        $0x10
08006AB2: 19 0D E0 08 00 6E 34          by move      $0xD,$0x8006E34+
08006AB9: 18 42                         r:=          b.0x8
08006ABB: FE 79 C4 08 00 77 4C 85 03    w bmove      $0x800774C,r.0x14,$0x3
08006AC4: FE 79 C4 08 00 77 58 88 03    w bmove      $0x8007758,r.0x20,$0x3
08006ACD: C3 08 00 84 18 00             call         $0x8008418,$0x0
08006AD3: 9D                            ifkret
08006AD4: 4A C4 08 00 6E 28             w stz        $0x8006E28
08006ADA: 80                            ret
08006ADB: 9C                            entd
08006ADC: FD C0 48                      l=:          b.0x20
08006ADF: FD 54 CE 00 AB C1 25          w byconv     $0xAB,b.0x25
08006AE6: 18 42                         r:=          b.0x8
08006AE8: 19 C1 25 85                   by move      b.0x25,r.0x14
08006AEC: 04 49                         by1 :=       b.0x24
08006AEE: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08006AF4: D2 04                         if -k go     $0x4
08006AF6: B4 48                         jumpg        b.0x20
08006AF8: 44 D0                         w test       r1
08006AFA: C5 00 D4                      if = go      $0xD4
08006AFD: 18 42                         r:=          b.0x8
08006AFF: 1A 45 85                      w move       b.0x14,r.0x14
08006B02: FE 79 C4 08 00 77 68 86 03    w bmove      $0x8007768,r.0x18,$0x3
08006B0B: 1A 46 89                      w move       b.0x18,r.0x24
08006B0E: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006B14: D2 04                         if -k go     $0x4
08006B16: B4 48                         jumpg        b.0x20
08006B18: 18 42                         r:=          b.0x8
08006B1A: 1A 89 46                      w move       r.0x24,b.0x18
08006B1D: 05 49                         by2 :=       b.0x24
08006B1F: 6D 0C                         w2 *         $0xC
08006B21: FE 26 E1 08 00 74 A8          by3 laddr    $0x80074A8+
08006B28: FD 20 F6 00 4B 0C             by bmove     r3.(0x0),b.0x2C,$0xC
08006B2E: 4D 4A                         w set1       b.0x28
08006B30: 04 0D                         by1 :=       $0xD
08006B32: 0D 4A                         w2 :=        b.0x28
08006B34: 2D E5 2C D0                   by comp2     @b.0x2C+,r1
08006B38: C4 06                         if = go      $0x6
08006B3A: 4F 4A                         w incr       b.0x28
08006B3C: C0 F4                         go           $0xFFFFFFFFFFFFFFF4
08006B3E: 18 42                         r:=          b.0x8
08006B40: 1A 45 85                      w move       b.0x14,r.0x14
08006B43: 61 01                         w2 -         $0x1
08006B45: 1A D1 50                      w move       r2,b.0x40
08006B48: 1A 01 4F                      w move       $0x1,b.0x3C
08006B4B: 0E 4B                         w3 :=        b.0x2C
08006B4D: 22 4E                         w3 =:        b.0x38
08006B4F: FD 20 4E 86 0C                by bmove     b.0x38,r.0x18,$0xC
08006B54: 1A 46 89                      w move       b.0x18,r.0x24
08006B57: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006B5D: D2 04                         if -k go     $0x4
08006B5F: B4 48                         jumpg        b.0x20
08006B61: 18 42                         r:=          b.0x8
08006B63: 1A 89 46                      w move       r.0x24,b.0x18
08006B66: 1A 45 85                      w move       b.0x14,r.0x14
08006B69: FE 79 C4 08 00 77 78 86 03    w bmove      $0x8007778,r.0x18,$0x3
08006B72: 1A 46 89                      w move       b.0x18,r.0x24
08006B75: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006B7B: D2 04                         if -k go     $0x4
08006B7D: B4 48                         jumpg        b.0x20
08006B7F: 18 42                         r:=          b.0x8
08006B81: 1A 89 46                      w move       r.0x24,b.0x18
08006B84: 05 C1 25                      by2 :=       b.0x25
08006B87: 6D 0C                         w2 *         $0xC
08006B89: FE 26 E1 08 00 74 3C          by3 laddr    $0x800743C+
08006B90: FD 20 F6 00 4B 0C             by bmove     r3.(0x0),b.0x2C,$0xC
08006B96: 0F 4D                         w4 :=        b.0x34
08006B98: 63 01                         w4 -         $0x1
08006B9A: 23 4A                         w4 =:        b.0x28
08006B9C: 1A 45 85                      w move       b.0x14,r.0x14
08006B9F: 1A D3 50                      w move       r4,b.0x40
08006BA2: 4A 4F                         w stz        b.0x3C
08006BA4: 0C 4B                         w1 :=        b.0x2C
08006BA6: 20 4E                         w1 =:        b.0x38
08006BA8: FD 20 4E 86 0C                by bmove     b.0x38,r.0x18,$0xC
08006BAD: 1A 46 89                      w move       b.0x18,r.0x24
08006BB0: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006BB6: D2 04                         if -k go     $0x4
08006BB8: B4 48                         jumpg        b.0x20
08006BBA: 18 42                         r:=          b.0x8
08006BBC: 1A 89 46                      w move       r.0x24,b.0x18
08006BBF: 1A 45 85                      w move       b.0x14,r.0x14
08006BC2: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006BC8: D2 04                         if -k go     $0x4
08006BCA: B4 48                         jumpg        b.0x20
08006BCC: 4D 46                         w set1       b.0x18
08006BCE: 05 C1 25                      by2 :=       b.0x25
08006BD1: 55 01                         w2 +         $0x1
08006BD3: 1D C1 25                      by2 =:       b.0x25
08006BD6: FC 1D CE 00 B6                h2 comp      $0xB6
08006BDB: CF FF 0B                      if <= go     $0xFFFFFFFFFFFFFF0B
08006BDE: FE 03                         clrk
08006BE0: B4 48                         jumpg        b.0x20
08006BE2: B8 CF 00 00 00 48             ents         $0x48
08006BE8: 19 CD 85 47                   by move      $0x85,b.0x1C
08006BEC: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006BF2: 19 CD 86 47                   by move      $0x86,b.0x1C
08006BF6: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006BFC: 19 CD 8C 47                   by move      $0x8C,b.0x1C
08006C00: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006C06: 19 CD 96 47                   by move      $0x96,b.0x1C
08006C0A: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006C10: 19 CD 98 47                   by move      $0x98,b.0x1C
08006C14: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006C1A: 19 CD 9A 47                   by move      $0x9A,b.0x1C
08006C1E: C3 08 00 6C 26 00             call         $0x8006C26,$0x0
08006C24: C0 12                         go           $0x12
08006C26: 9C                            entd
08006C27: FD C0 51                      l=:          b.0x44
08006C2A: 19 47 49                      by move      b.0x1C,b.0x24
08006C2D: C3 08 00 6A DB 00             call         $0x8006ADB,$0x0
08006C33: 9D                            ifkret
08006C34: B4 51                         jumpg        b.0x44
08006C36: 18 42                         r:=          b.0x8
08006C38: 1A 45 85                      w move       b.0x14,r.0x14
08006C3B: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006C41: 9D                            ifkret
08006C42: 4D 46                         w set1       b.0x18
08006C44: 80                            ret
08006C45: B8 CF 00 00 00 2C             ents         $0x2C
08006C4B: 18 46                         r:=          b.0x18
08006C4D: 1A 86 4A                      w move       r.0x18,b.0x28
08006C50: 44 4A                         w test       b.0x28
08006C52: C5 00 BA                      if = go      $0xBA
08006C55: 1A C5 28 49                   w move       @b.0x28,b.0x24
08006C59: 18 49                         r:=          b.0x24
08006C5B: 0C 85                         w1 :=        r.0x14
08006C5D: C3 08 00 87 41 00             call         $0x8008741,$0x0
08006C63: 9D                            ifkret
08006C64: 20 48                         w1 =:        b.0x20
08006C66: 54 05                         w1 +         $0x5
08006C68: 54 47                         w1 +         b.0x1C
08006C6A: 34 CD 4F                      w1 comp      $0x4F
08006C6D: CE 49                         if <= go     $0x49
08006C6F: 18 42                         r:=          b.0x8
08006C71: 1A 45 85                      w move       b.0x14,r.0x14
08006C74: FE 79 C4 08 00 77 88 86 03    w bmove      $0x8007788,r.0x18,$0x3
08006C7D: 1A 47 89                      w move       b.0x1C,r.0x24
08006C80: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006C86: 9D                            ifkret
08006C87: 18 42                         r:=          b.0x8
08006C89: 1A 89 47                      w move       r.0x24,b.0x1C
08006C8C: 1A 45 85                      w move       b.0x14,r.0x14
08006C8F: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006C95: 9D                            ifkret
08006C96: 4D 47                         w set1       b.0x1C
08006C98: 18 42                         r:=          b.0x8
08006C9A: 1A 45 85                      w move       b.0x14,r.0x14
08006C9D: FE 79 C4 08 00 77 A4 86 03    w bmove      $0x80077A4,r.0x18,$0x3
08006CA6: 4D 89                         w set1       r.0x24
08006CA8: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006CAE: 9D                            ifkret
08006CAF: 18 42                         r:=          b.0x8
08006CB1: 1A 89 47                      w move       r.0x24,b.0x1C
08006CB4: C0 1F                         go           $0x1F
08006CB6: 18 42                         r:=          b.0x8
08006CB8: 1A 45 85                      w move       b.0x14,r.0x14
08006CBB: FE 79 C4 08 00 77 B4 86 03    w bmove      $0x80077B4,r.0x18,$0x3
08006CC4: 1A 47 89                      w move       b.0x1C,r.0x24
08006CC7: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006CCD: 9D                            ifkret
08006CCE: 18 42                         r:=          b.0x8
08006CD0: 1A 89 47                      w move       r.0x24,b.0x1C
08006CD3: 1A 45 85                      w move       b.0x14,r.0x14
08006CD6: 18 49                         r:=          b.0x24
08006CD8: 0D 85                         w2 :=        r.0x14
08006CDA: 18 42                         r:=          b.0x8
08006CDC: 21 86                         w2 =:        r.0x18
08006CDE: 1A 47 87                      w move       b.0x1C,r.0x1C
08006CE1: C3 08 00 68 2D 00             call         $0x800682D,$0x0
08006CE7: 9D                            ifkret
08006CE8: 18 42                         r:=          b.0x8
08006CEA: 1A 87 47                      w move       r.0x1C,b.0x1C
08006CED: 1A 45 85                      w move       b.0x14,r.0x14
08006CF0: 1A 49 86                      w move       b.0x24,r.0x18
08006CF3: 1A 47 87                      w move       b.0x1C,r.0x1C
08006CF6: C3 08 00 6C 45 00             call         $0x8006C45,$0x0
08006CFC: 9D                            ifkret
08006CFD: 18 42                         r:=          b.0x8
08006CFF: 1A 87 47                      w move       r.0x1C,b.0x1C
08006D02: 18 4A                         r:=          b.0x28
08006D04: 1A 81 4A                      w move       r.0x4,b.0x28
08006D07: 44 4A                         w test       b.0x28
08006D09: C7 FF 4C                      if >< go     $0xFFFFFFFFFFFFFF4C
08006D0C: 80                            ret
08006D0D: B8 CF 00 00 00 D4             ents         $0xD4
08006D13: C1 00 A0                      go           $0xA0
08006D16: 9C                            entd
08006D17: FD C0 68                      l=:          b.0xA0
08006D1A: 20 43                         w1 =:        b.0xC
08006D1C: 44 C4 08 00 79 F8             w test       $0x80079F8
08006D22: C6 09                         if >< go     $0x9
08006D24: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08006D2A: 9D                            ifkret
08006D2B: 1A CD 72 69                   w move       $0x72,b.0xA4
08006D2F: 4D 6A                         w set1       b.0xA8
08006D31: FE 79 C4 08 00 77 C8 6B 03    w bmove      $0x80077C8,b.0xAC,$0x3
08006D3A: 1A 3F 6E                      w move       $0x3F,b.0xB8
08006D3D: C3 08 00 B9 7C 07 69 6A C5 AC C5 B0 C5 B4 6E 4C call         $0x800B97C,$0x7,b.0xA4,b.0xA8,@b.0xFFFFFFFFFFFFFFAC,@b.0xFFFFFFFFFFFFFFB0,@b.0xFFFFFFFFFFFFFFB4,b.0xB8,b.0x30
08006D4D: 9D                            ifkret
08006D4E: 18 42                         r:=          b.0x8
08006D50: 1A 45 85                      w move       b.0x14,r.0x14
08006D53: FD 3D 4F                      w2 laddr     b.0x3C
08006D56: 21 86                         w2 =:        r.0x18
08006D58: 4A 87                         w stz        r.0x1C
08006D5A: 1A CD 63 88                   w move       $0x63,r.0x20
08006D5E: FE 79 00 89 03                w bmove      $0x0,r.0x24,$0x3
08006D63: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
08006D69: 9D                            ifkret
08006D6A: 4A 4B                         w stz        b.0x2C
08006D6C: 0C 4B                         w1 :=        b.0x2C
08006D6E: 2D D4 3C 0D                   by comp2     b.0x3C+,$0xD
08006D72: C4 06                         if = go      $0x6
08006D74: 4F 4B                         w incr       b.0x2C
08006D76: C0 F6                         go           $0xFFFFFFFFFFFFFFF6
08006D78: 1A CD 72 69                   w move       $0x72,b.0xA4
08006D7C: 4D 6A                         w set1       b.0xA8
08006D7E: 60 01                         w1 -         $0x1
08006D80: 1A D0 74                      w move       r1,b.0xD0
08006D83: 4A 73                         w stz        b.0xCC
08006D85: FD 3D 4F                      w2 laddr     b.0x3C
08006D88: 21 72                         w2 =:        b.0xC8
08006D8A: FD 20 72 6F 0C                by bmove     b.0xC8,b.0xBC,$0xC
08006D8F: 1A 3F 6E                      w move       $0x3F,b.0xB8
08006D92: C3 08 00 B9 7C 07 69 6A C5 BC C5 C0 C5 C4 6E 4C call         $0x800B97C,$0x7,b.0xA4,b.0xA8,@b.0xFFFFFFFFFFFFFFBC,@b.0xFFFFFFFFFFFFFFC0,@b.0xFFFFFFFFFFFFFFC4,b.0xB8,b.0x30
08006DA2: 9D                            ifkret
08006DA3: 0C 43                         w1 :=        b.0xC
08006DA5: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08006DAB: 9D                            ifkret
08006DAC: 0C 43                         w1 :=        b.0xC
08006DAE: 81                            retk
08006DAF: FE 03                         clrk
08006DB1: B4 68                         jumpg        b.0xA0
08006DB3: C3 08 00 6A 00 00             call         $0x8006A00,$0x0
08006DB9: D2 08                         if -k go     $0x8
08006DBB: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006DC1: 4D 4A                         w set1       b.0x28
08006DC3: 18 42                         r:=          b.0x8
08006DC5: 1A 45 85                      w move       b.0x14,r.0x14
08006DC8: 4D 86                         w set1       r.0x18
08006DCA: C3 08 00 6B E2 00             call         $0x8006BE2,$0x0
08006DD0: D2 08                         if -k go     $0x8
08006DD2: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006DD8: 18 42                         r:=          b.0x8
08006DDA: 1A 86 4A                      w move       r.0x18,b.0x28
08006DDD: FD 20 47 85 0C                by bmove     b.0x1C,r.0x14,$0xC
08006DE2: C3 08 00 87 DD 00             call         $0x80087DD,$0x0
08006DE8: D2 08                         if -k go     $0x8
08006DEA: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006DF0: 18 42                         r:=          b.0x8
08006DF2: 1A 88 4D                      w move       r.0x20,b.0x34
08006DF5: 1A 45 85                      w move       b.0x14,r.0x14
08006DF8: 1A 4D 86                      w move       b.0x34,r.0x18
08006DFB: 1A 4A 87                      w move       b.0x28,r.0x1C
08006DFE: C3 08 00 68 6C 00             call         $0x800686C,$0x0
08006E04: D2 08                         if -k go     $0x8
08006E06: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006E0C: 18 42                         r:=          b.0x8
08006E0E: 1A 87 4A                      w move       r.0x1C,b.0x28
08006E11: 1A 4D 85                      w move       b.0x34,r.0x14
08006E14: C3 08 00 87 17 00             call         $0x8008717,$0x0
08006E1A: D2 08                         if -k go     $0x8
08006E1C: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006E22: 18 42                         r:=          b.0x8
08006E24: 1A 85 4D                      w move       r.0x14,b.0x34
08006E27: 1A 45 85                      w move       b.0x14,r.0x14
08006E2A: FE 79 C4 08 00 77 DC 86 03    w bmove      $0x80077DC,r.0x18,$0x3
08006E33: 1A 4A 89                      w move       b.0x28,r.0x24
08006E36: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006E3C: D2 08                         if -k go     $0x8
08006E3E: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006E44: 18 42                         r:=          b.0x8
08006E46: 1A 89 4A                      w move       r.0x24,b.0x28
08006E49: 1A 46 4E                      w move       b.0x18,b.0x38
08006E4C: 44 4E                         w test       b.0x38
08006E4E: C5 00 EB                      if = go      $0xEB
08006E51: 18 4E                         r:=          b.0x38
08006E53: 0C 85                         w1 :=        r.0x14
08006E55: C3 08 00 87 41 00             call         $0x8008741,$0x0
08006E5B: D2 08                         if -k go     $0x8
08006E5D: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006E63: 54 4A                         w1 +         b.0x28
08006E65: 54 05                         w1 +         $0x5
08006E67: 34 CD 4E                      w1 comp      $0x4E
08006E6A: CE 5E                         if <= go     $0x5E
08006E6C: 18 42                         r:=          b.0x8
08006E6E: 1A 45 85                      w move       b.0x14,r.0x14
08006E71: FE 79 C4 08 00 77 EC 86 03    w bmove      $0x80077EC,r.0x18,$0x3
08006E7A: 1A 4A 89                      w move       b.0x28,r.0x24
08006E7D: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006E83: D2 08                         if -k go     $0x8
08006E85: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006E8B: 18 42                         r:=          b.0x8
08006E8D: 1A 89 4A                      w move       r.0x24,b.0x28
08006E90: 1A 45 85                      w move       b.0x14,r.0x14
08006E93: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006E99: D2 08                         if -k go     $0x8
08006E9B: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006EA1: 4D 4A                         w set1       b.0x28
08006EA3: 18 42                         r:=          b.0x8
08006EA5: 1A 45 85                      w move       b.0x14,r.0x14
08006EA8: FE 79 C4 08 00 78 08 86 03    w bmove      $0x8007808,r.0x18,$0x3
08006EB1: 4D 89                         w set1       r.0x24
08006EB3: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006EB9: D2 08                         if -k go     $0x8
08006EBB: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006EC1: 18 42                         r:=          b.0x8
08006EC3: 1A 89 4A                      w move       r.0x24,b.0x28
08006EC6: C0 26                         go           $0x26
08006EC8: 18 42                         r:=          b.0x8
08006ECA: 1A 45 85                      w move       b.0x14,r.0x14
08006ECD: FE 79 C4 08 00 78 18 86 03    w bmove      $0x8007818,r.0x18,$0x3
08006ED6: 1A 4A 89                      w move       b.0x28,r.0x24
08006ED9: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006EDF: D2 08                         if -k go     $0x8
08006EE1: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006EE7: 18 42                         r:=          b.0x8
08006EE9: 1A 89 4A                      w move       r.0x24,b.0x28
08006EEC: 1A 45 85                      w move       b.0x14,r.0x14
08006EEF: 18 4E                         r:=          b.0x38
08006EF1: 0D 85                         w2 :=        r.0x14
08006EF3: 18 42                         r:=          b.0x8
08006EF5: 21 86                         w2 =:        r.0x18
08006EF7: 1A 4A 87                      w move       b.0x28,r.0x1C
08006EFA: C3 08 00 68 6C 00             call         $0x800686C,$0x0
08006F00: D2 08                         if -k go     $0x8
08006F02: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F08: 18 42                         r:=          b.0x8
08006F0A: 1A 87 4A                      w move       r.0x1C,b.0x28
08006F0D: 1A 45 85                      w move       b.0x14,r.0x14
08006F10: FE 79 C4 08 00 78 2C 86 03    w bmove      $0x800782C,r.0x18,$0x3
08006F19: 1A 4A 89                      w move       b.0x28,r.0x24
08006F1C: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006F22: D2 08                         if -k go     $0x8
08006F24: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F2A: 18 42                         r:=          b.0x8
08006F2C: 1A 89 4A                      w move       r.0x24,b.0x28
08006F2F: 18 4E                         r:=          b.0x38
08006F31: 1A 88 4E                      w move       r.0x20,b.0x38
08006F34: 44 4E                         w test       b.0x38
08006F36: C7 FF 1B                      if >< go     $0xFFFFFFFFFFFFFF1B
08006F39: 18 42                         r:=          b.0x8
08006F3B: 1A 45 85                      w move       b.0x14,r.0x14
08006F3E: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006F44: D2 08                         if -k go     $0x8
08006F46: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F4C: 18 42                         r:=          b.0x8
08006F4E: 1A 45 85                      w move       b.0x14,r.0x14
08006F51: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08006F57: D2 08                         if -k go     $0x8
08006F59: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F5F: 4D 4A                         w set1       b.0x28
08006F61: 1A 46 4E                      w move       b.0x18,b.0x38
08006F64: 44 4E                         w test       b.0x38
08006F66: C5 00 C7                      if = go      $0xC7
08006F69: 18 4E                         r:=          b.0x38
08006F6B: 0C 85                         w1 :=        r.0x14
08006F6D: C3 08 00 87 41 00             call         $0x8008741,$0x0
08006F73: D2 08                         if -k go     $0x8
08006F75: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F7B: 18 42                         r:=          b.0x8
08006F7D: 1A 45 85                      w move       b.0x14,r.0x14
08006F80: 18 4E                         r:=          b.0x38
08006F82: 0D 85                         w2 :=        r.0x14
08006F84: 18 42                         r:=          b.0x8
08006F86: 21 86                         w2 =:        r.0x18
08006F88: 1A 4A 87                      w move       b.0x28,r.0x1C
08006F8B: C3 08 00 68 6C 00             call         $0x800686C,$0x0
08006F91: D2 08                         if -k go     $0x8
08006F93: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006F99: 18 42                         r:=          b.0x8
08006F9B: 1A 87 4A                      w move       r.0x1C,b.0x28
08006F9E: 1A 45 85                      w move       b.0x14,r.0x14
08006FA1: FE 79 C4 08 00 78 40 86 03    w bmove      $0x8007840,r.0x18,$0x3
08006FAA: 1A 4A 89                      w move       b.0x28,r.0x24
08006FAD: C3 08 00 68 09 00             call         $0x8006809,$0x0
08006FB3: D2 08                         if -k go     $0x8
08006FB5: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006FBB: 18 42                         r:=          b.0x8
08006FBD: 1A 89 4A                      w move       r.0x24,b.0x28
08006FC0: 1A 45 85                      w move       b.0x14,r.0x14
08006FC3: 18 4E                         r:=          b.0x38
08006FC5: 0D 85                         w2 :=        r.0x14
08006FC7: 18 42                         r:=          b.0x8
08006FC9: 21 86                         w2 =:        r.0x18
08006FCB: 1A 4A 87                      w move       b.0x28,r.0x1C
08006FCE: C3 08 00 68 2D 00             call         $0x800682D,$0x0
08006FD4: D2 08                         if -k go     $0x8
08006FD6: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006FDC: 18 42                         r:=          b.0x8
08006FDE: 1A 87 4A                      w move       r.0x1C,b.0x28
08006FE1: 1A 45 85                      w move       b.0x14,r.0x14
08006FE4: 1A 4E 86                      w move       b.0x38,r.0x18
08006FE7: 1A 4A 87                      w move       b.0x28,r.0x1C
08006FEA: C3 08 00 6C 45 00             call         $0x8006C45,$0x0
08006FF0: D2 08                         if -k go     $0x8
08006FF2: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08006FF8: 18 42                         r:=          b.0x8
08006FFA: 1A 87 4A                      w move       r.0x1C,b.0x28
08006FFD: 1A 45 85                      w move       b.0x14,r.0x14
08007000: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007006: D2 08                         if -k go     $0x8
08007008: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
0800700E: 18 42                         r:=          b.0x8
08007010: 1A 45 85                      w move       b.0x14,r.0x14
08007013: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007019: D2 08                         if -k go     $0x8
0800701B: C3 08 00 6D 16 00             call         $0x8006D16,$0x0
08007021: 4D 4A                         w set1       b.0x28
08007023: 18 4E                         r:=          b.0x38
08007025: 1A 88 4E                      w move       r.0x20,b.0x38
08007028: 44 4E                         w test       b.0x38
0800702A: C7 FF 3F                      if >< go     $0xFFFFFFFFFFFFFF3F
0800702D: 80                            ret
0800702E: 9C                            entd
0800702F: FD C0 51                      l=:          b.0x44
08007032: 20 52                         w1 =:        b.0x48
08007034: 18 42                         r:=          b.0x8
08007036: 1A 4C 85                      w move       b.0x30,r.0x14
08007039: FD 3D 55                      w2 laddr     b.0x54
0800703C: 21 86                         w2 =:        r.0x18
0800703E: 4A 87                         w stz        r.0x1C
08007040: 1A CD 63 88                   w move       $0x63,r.0x20
08007044: FE 79 00 89 03                w bmove      $0x0,r.0x24,$0x3
08007049: C3 08 00 B6 CE 00             call         $0x800B6CE,$0x0
0800704F: D2 04                         if -k go     $0x4
08007051: B4 51                         jumpg        b.0x44
08007053: 4A 54                         w stz        b.0x50
08007055: 0C 54                         w1 :=        b.0x50
08007057: 2D D4 54 0D                   by comp2     b.0x54+,$0xD
0800705B: C4 06                         if = go      $0x6
0800705D: 4F 54                         w incr       b.0x50
0800705F: C0 F6                         go           $0xFFFFFFFFFFFFFFF6
08007061: 1A CD 72 6E                   w move       $0x72,b.0xB8
08007065: 4D 6F                         w set1       b.0xBC
08007067: 60 01                         w1 -         $0x1
08007069: 1A D0 75                      w move       r1,b.0xD4
0800706C: 4A 74                         w stz        b.0xD0
0800706E: FD 3D 55                      w2 laddr     b.0x54
08007071: 21 73                         w2 =:        b.0xCC
08007073: FD 20 73 70 0C                by bmove     b.0xCC,b.0xC0,$0xC
08007078: 1A 3F 76                      w move       $0x3F,b.0xD8
0800707B: C3 08 00 B9 7C 07 6E 6F C5 C0 C5 C4 C5 C8 76 53 call         $0x800B97C,$0x7,b.0xB8,b.0xBC,@b.0xFFFFFFFFFFFFFFC0,@b.0xFFFFFFFFFFFFFFC4,@b.0xFFFFFFFFFFFFFFC8,b.0xD8,b.0x4C
0800708B: D2 04                         if -k go     $0x4
0800708D: B4 51                         jumpg        b.0x44
0800708F: 1A CD 72 6E                   w move       $0x72,b.0xB8
08007093: 4D 6F                         w set1       b.0xBC
08007095: FE 79 C4 08 00 78 50 73 03    w bmove      $0x8007850,b.0xCC,$0x3
0800709E: 1A 3F 76                      w move       $0x3F,b.0xD8
080070A1: C3 08 00 B9 7C 07 6E 6F C5 CC C5 D0 C5 D4 76 53 call         $0x800B97C,$0x7,b.0xB8,b.0xBC,@b.0xFFFFFFFFFFFFFFCC,@b.0xFFFFFFFFFFFFFFD0,@b.0xFFFFFFFFFFFFFFD4,b.0xD8,b.0x4C
080070B1: D2 04                         if -k go     $0x4
080070B3: B4 51                         jumpg        b.0x44
080070B5: 18 42                         r:=          b.0x8
080070B7: 4D 85                         w set1       r.0x14
080070B9: FE 79 C4 08 00 78 60 86 03    w bmove      $0x8007860,r.0x18,$0x3
080070C2: 1A 4A 89                      w move       b.0x28,r.0x24
080070C5: C3 08 00 C6 02 00             call         $0x800C602,$0x0
080070CB: D2 04                         if -k go     $0x4
080070CD: B4 51                         jumpg        b.0x44
080070CF: 1A CD 72 6E                   w move       $0x72,b.0xB8
080070D3: 4D 6F                         w set1       b.0xBC
080070D5: FE 79 C4 08 00 78 70 77 03    w bmove      $0x8007870,b.0xDC,$0x3
080070DE: 1A 3F 76                      w move       $0x3F,b.0xD8
080070E1: C3 08 00 B9 7C 07 6E 6F C5 DC C5 E0 C5 E4 76 53 call         $0x800B97C,$0x7,b.0xB8,b.0xBC,@b.0xFFFFFFFFFFFFFFDC,@b.0xFFFFFFFFFFFFFFE0,@b.0xFFFFFFFFFFFFFFE4,b.0xD8,b.0x4C
080070F1: D2 04                         if -k go     $0x4
080070F3: B4 51                         jumpg        b.0x44
080070F5: 0C 52                         w1 :=        b.0x48
080070F7: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
080070FD: D2 04                         if -k go     $0x4
080070FF: B4 51                         jumpg        b.0x44
08007101: FE 03                         clrk
08007103: B4 51                         jumpg        b.0x44
08007105: B8 CF 00 00 00 EC             ents         $0xEC
0800710B: C0 24                         go           $0x24
0800710D: 9C                            entd
0800710E: FD C0 7A                      l=:          b.0xE8
08007111: 20 43                         w1 =:        b.0xC
08007113: 44 4C                         w test       b.0x30
08007115: C4 13                         if = go      $0x13
08007117: 18 42                         r:=          b.0x8
08007119: 1A 4C 85                      w move       b.0x30,r.0x14
0800711C: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
08007122: 9D                            ifkret
08007123: 18 42                         r:=          b.0x8
08007125: 1A 85 4C                      w move       r.0x14,b.0x30
08007128: 0C 43                         w1 :=        b.0xC
0800712A: 80                            ret
0800712B: FE 03                         clrk
0800712D: B4 7A                         jumpg        b.0xE8
0800712F: 4A 4A                         w stz        b.0x28
08007131: 85                            bi2 clr
08007132: 21 4C                         w2 =:        b.0x30
08007134: 21 4F                         w2 =:        b.0x3C
08007136: 44 C4 08 00 6E 24             w test       $0x8006E24
0800713C: C7 02 16                      if >< go     $0x216
0800713F: 4A C4 08 00 6E 30             w stz        $0x8006E30
08007145: 18 42                         r:=          b.0x8
08007147: FE 79 C4 08 00 78 7C 85 03    w bmove      $0x800787C,r.0x14,$0x3
08007150: 19 CD 52 88                   by move      $0x52,r.0x20
08007154: C3 08 00 AF 76 00             call         $0x800AF76,$0x0
0800715A: D2 08                         if -k go     $0x8
0800715C: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007162: 20 4C                         w1 =:        b.0x30
08007164: 4F 4A                         w incr       b.0x28
08007166: 18 42                         r:=          b.0x8
08007168: 1A 4C 85                      w move       b.0x30,r.0x14
0800716B: 1A 4D 86                      w move       b.0x34,r.0x18
0800716E: 1A 4A 87                      w move       b.0x28,r.0x1C
08007171: 19 49 88                      by move      b.0x24,r.0x20
08007174: C3 08 00 63 1D 00             call         $0x800631D,$0x0
0800717A: D2 08                         if -k go     $0x8
0800717C: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007182: 18 42                         r:=          b.0x8
08007184: 1A 86 4D                      w move       r.0x18,b.0x34
08007187: 1A 87 4A                      w move       r.0x1C,b.0x28
0800718A: 19 88 49                      by move      r.0x20,b.0x24
0800718D: 20 4B                         w1 =:        b.0x2C
0800718F: 44 D0                         w test       r1
08007191: C4 06                         if = go      $0x6
08007193: 34 01                         w1 comp      $0x1
08007195: C6 48                         if >< go     $0x48
08007197: 44 C4 08 00 6E 30             w test       $0x8006E30
0800719D: C4 07                         if = go      $0x7
0800719F: 34 C5 40                      w1 comp      @b.0x40
080071A2: C4 29                         if = go      $0x29
080071A4: C3 08 00 64 EC 00             call         $0x80064EC,$0x0
080071AA: D2 08                         if -k go     $0x8
080071AC: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080071B2: 20 50                         w1 =:        b.0x40
080071B4: 1A 4B F4 00                   w move       b.0x2C,r1.(0x0)
080071B8: 0D CF 08 00 6E 30             w2 :=        $0x8006E30
080071BE: 0C 50                         w1 :=        b.0x40
080071C0: 0E 08                         w3 :=        $0x8
080071C2: FE 03                         clrk
080071C4: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080071CA: 9D                            ifkret
080071CB: 18 50                         r:=          b.0x40
080071CD: FD 3D 81                      w2 laddr     r.0x4
080071D0: 0C 4D                         w1 :=        b.0x34
080071D2: 0E 18                         w3 :=        $0x18
080071D4: FE 03                         clrk
080071D6: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080071DC: 9D                            ifkret
080071DD: 44 45                         w test       b.0x14
080071DF: C4 5B                         if = go      $0x5B
080071E1: 44 C4 08 00 79 F0             w test       $0x80079F0
080071E7: C6 50                         if >< go     $0x50
080071E9: 2E 4B 05                      w comp2      b.0x2C,$0x5
080071EC: C6 25                         if >< go     $0x25
080071EE: 18 42                         r:=          b.0x8
080071F0: 1A 04 85                      w move       $0x4,r.0x14
080071F3: 1A 4D 86                      w move       b.0x34,r.0x18
080071F6: 1A 4F 87                      w move       b.0x3C,r.0x1C
080071F9: C3 08 00 AC 3B 00             call         $0x800AC3B,$0x0
080071FF: D2 08                         if -k go     $0x8
08007201: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007207: 18 42                         r:=          b.0x8
08007209: 1A 86 4D                      w move       r.0x18,b.0x34
0800720C: 1A 87 4F                      w move       r.0x1C,b.0x3C
0800720F: C0 28                         go           $0x28
08007211: 2E 4B 06                      w comp2      b.0x2C,$0x6
08007214: C6 23                         if >< go     $0x23
08007216: 18 42                         r:=          b.0x8
08007218: 1A 04 85                      w move       $0x4,r.0x14
0800721B: 1A 4F 86                      w move       b.0x3C,r.0x18
0800721E: 1A 4D 87                      w move       b.0x34,r.0x1C
08007221: C3 08 00 AC 3B 00             call         $0x800AC3B,$0x0
08007227: D2 08                         if -k go     $0x8
08007229: C3 08 00 71 0D 00             call         $0x800710D,$0x0
0800722F: 18 42                         r:=          b.0x8
08007231: 1A 86 4F                      w move       r.0x18,b.0x3C
08007234: 1A 87 4D                      w move       r.0x1C,b.0x34
08007237: C1 00 CF                      go           $0xCF
0800723A: 0D 4B                         w2 :=        b.0x2C
0800723C: B4 E1 08 00 78 88             jumpg        $0x8007888+
08007242: C1 00 C4                      go           $0xC4
08007245: C1 00 C1                      go           $0xC1
08007248: 0C CD C0                      w1 :=        $0xC0
0800724B: C3 08 00 70 2E 00             call         $0x800702E,$0x0
08007251: D2 08                         if -k go     $0x8
08007253: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007259: C1 00 AD                      go           $0xAD
0800725C: 18 42                         r:=          b.0x8
0800725E: 1A 4D 85                      w move       b.0x34,r.0x14
08007261: 1A 04 86                      w move       $0x4,r.0x18
08007264: C3 08 00 3F 37 00             call         $0x8003F37,$0x0
0800726A: D2 08                         if -k go     $0x8
0800726C: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007272: C1 00 94                      go           $0x94
08007275: 44 46                         w test       b.0x18
08007277: C5 00 8D                      if = go      $0x8D
0800727A: C3 08 00 37 30 00             call         $0x8003730,$0x0
08007280: D2 08                         if -k go     $0x8
08007282: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007288: 44 D0                         w test       r1
0800728A: C4 7A                         if = go      $0x7A
0800728C: 44 C4 08 00 79 F0             w test       $0x80079F0
08007292: C6 72                         if >< go     $0x72
08007294: 18 42                         r:=          b.0x8
08007296: 1A 4D 85                      w move       b.0x34,r.0x14
08007299: C3 08 00 87 71 00             call         $0x8008771,$0x0
0800729F: D2 08                         if -k go     $0x8
080072A1: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080072A7: 18 42                         r:=          b.0x8
080072A9: 1A 86 4E                      w move       r.0x18,b.0x38
080072AC: 1A 4E 85                      w move       b.0x38,r.0x14
080072AF: C3 08 00 A2 53 00             call         $0x800A253,$0x0
080072B5: D2 08                         if -k go     $0x8
080072B7: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080072BD: 18 42                         r:=          b.0x8
080072BF: 1A 85 4E                      w move       r.0x14,b.0x38
080072C2: 1A 47 85                      w move       b.0x1C,r.0x14
080072C5: 1A 4E 86                      w move       b.0x38,r.0x18
080072C8: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
080072CE: D2 08                         if -k go     $0x8
080072D0: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080072D6: 18 42                         r:=          b.0x8
080072D8: 1A 86 4E                      w move       r.0x18,b.0x38
080072DB: 1A 47 85                      w move       b.0x1C,r.0x14
080072DE: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
080072E4: D2 08                         if -k go     $0x8
080072E6: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080072EC: 18 42                         r:=          b.0x8
080072EE: 1A 4E 85                      w move       b.0x38,r.0x14
080072F1: C3 08 00 87 17 00             call         $0x8008717,$0x0
080072F7: D2 08                         if -k go     $0x8
080072F9: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080072FF: 18 42                         r:=          b.0x8
08007301: 1A 85 4E                      w move       r.0x14,b.0x38
08007304: C0 02                         go           $0x2
08007306: 2D 49 17                      by comp2     b.0x24,$0x17
08007309: C4 05                         if = go      $0x5
0800730B: C1 FE 59                      go           $0xFFFFFFFFFFFFFE59
0800730E: 4A 4A                         w stz        b.0x28
08007310: 18 42                         r:=          b.0x8
08007312: 1A 4C 85                      w move       b.0x30,r.0x14
08007315: C3 08 00 B1 0E 00             call         $0x800B10E,$0x0
0800731B: D2 08                         if -k go     $0x8
0800731D: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007323: 18 42                         r:=          b.0x8
08007325: 1A 85 4C                      w move       r.0x14,b.0x30
08007328: 44 C4 08 00 79 F0             w test       $0x80079F0
0800732E: C4 19                         if = go      $0x19
08007330: 0C CD 9A                      w1 :=        $0x9A
08007333: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08007339: D2 08                         if -k go     $0x8
0800733B: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007341: 0C CD 9A                      w1 :=        $0x9A
08007344: 80                            ret
08007345: C0 0A                         go           $0xA
08007347: 4D C4 08 00 6E 24             w set1       $0x8006E24
0800734D: 84                            bi1 clr
0800734E: 80                            ret
0800734F: C1 01 04                      go           $0x104
08007352: 1A C4 08 00 6E 30 50          w move       $0x8006E30,b.0x40
08007359: 44 50                         w test       b.0x40
0800735B: C5 00 D7                      if = go      $0xD7
0800735E: 44 C5 40                      w test       @b.0x40
08007361: C6 2D                         if >< go     $0x2D
08007363: 18 50                         r:=          b.0x40
08007365: 1A 81 4D                      w move       r.0x4,b.0x34
08007368: 44 4D                         w test       b.0x34
0800736A: C4 21                         if = go      $0x21
0800736C: 18 42                         r:=          b.0x8
0800736E: 1A 4D 85                      w move       b.0x34,r.0x14
08007371: 1A 04 86                      w move       $0x4,r.0x18
08007374: C3 08 00 3F 37 00             call         $0x8003F37,$0x0
0800737A: D2 08                         if -k go     $0x8
0800737C: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007382: 18 4D                         r:=          b.0x34
08007384: 1A 86 4D                      w move       r.0x18,b.0x34
08007387: 44 4D                         w test       b.0x34
08007389: C6 E3                         if >< go     $0xFFFFFFFFFFFFFFE3
0800738B: C1 00 9D                      go           $0x9D
0800738E: 44 46                         w test       b.0x18
08007390: C5 00 98                      if = go      $0x98
08007393: C3 08 00 37 30 00             call         $0x8003730,$0x0
08007399: D2 08                         if -k go     $0x8
0800739B: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080073A1: 44 D0                         w test       r1
080073A3: C5 00 85                      if = go      $0x85
080073A6: 18 50                         r:=          b.0x40
080073A8: 1A 81 4D                      w move       r.0x4,b.0x34
080073AB: 44 4D                         w test       b.0x34
080073AD: C4 7B                         if = go      $0x7B
080073AF: 18 42                         r:=          b.0x8
080073B1: 1A 4D 85                      w move       b.0x34,r.0x14
080073B4: C3 08 00 87 71 00             call         $0x8008771,$0x0
080073BA: D2 08                         if -k go     $0x8
080073BC: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080073C2: 18 42                         r:=          b.0x8
080073C4: 1A 86 4E                      w move       r.0x18,b.0x38
080073C7: 1A 4E 85                      w move       b.0x38,r.0x14
080073CA: C3 08 00 A2 53 00             call         $0x800A253,$0x0
080073D0: D2 08                         if -k go     $0x8
080073D2: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080073D8: 18 42                         r:=          b.0x8
080073DA: 1A 85 4E                      w move       r.0x14,b.0x38
080073DD: 1A 47 85                      w move       b.0x1C,r.0x14
080073E0: 1A 4E 86                      w move       b.0x38,r.0x18
080073E3: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
080073E9: D2 08                         if -k go     $0x8
080073EB: C3 08 00 71 0D 00             call         $0x800710D,$0x0
080073F1: 18 42                         r:=          b.0x8
080073F3: 1A 86 4E                      w move       r.0x18,b.0x38
080073F6: 1A 47 85                      w move       b.0x1C,r.0x14
080073F9: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
080073FF: D2 08                         if -k go     $0x8
08007401: C3 08 00 71 0D 00             call         $0x800710D,$0x0
08007407: 18 42                         r:=          b.0x8
08007409: 1A 4E 85                      w move       b.0x38,r.0x14
0800740C: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007412: D2 08                         if -k go     $0x8
08007414: C3 08 00 71 0D 00             call         $0x800710D,$0x0
0800741A: 18 42                         r:=          b.0x8
0800741C: 1A 85 4E                      w move       r.0x14,b.0x38
0800741F: 18 4D                         r:=          b.0x34
08007421: 1A 86 4D                      w move       r.0x18,b.0x34
08007424: 44 4D                         w test       b.0x34
08007426: C6 89                         if >< go     $0xFFFFFFFFFFFFFF89
08007428: 18 50                         r:=          b.0x40
0800742A: 1A 82 50                      w move       r.0x8,b.0x40
0800742D: 44 50                         w test       b.0x40
0800742F: C7 FF 2F                      if >< go     $0xFFFFFFFFFFFFFF2F
08007432: 44 C4 08 00 79 F0             w test       $0x80079F0
08007438: C4 19                         if = go      $0x19
0800743A: 0C CD 9A                      w1 :=        $0x9A
0800743D: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08007443: D2 08                         if -k go     $0x8
08007445: C3 08 00 71 0D 00             call         $0x800710D,$0x0
0800744B: 0C CD 9A                      w1 :=        $0x9A
0800744E: 80                            ret
0800744F: C0 04                         go           $0x4
08007451: 84                            bi1 clr
08007452: 80                            ret
08007453: B8 CF 00 00 00 34             ents         $0x34
08007459: 4F C4 08 00 7A 08             w incr       $0x8007A08
0800745F: 44 C4 08 00 79 F8             w test       $0x80079F8
08007465: C6 1C                         if >< go     $0x1C
08007467: 18 42                         r:=          b.0x8
08007469: FE 79 C4 08 00 78 B0 85 03    w bmove      $0x80078B0,r.0x14,$0x3
08007472: 18 46                         r:=          b.0x18
08007474: 0C 85                         w1 :=        r.0x14
08007476: 18 42                         r:=          b.0x8
08007478: 20 88                         w1 =:        r.0x20
0800747A: C3 08 00 69 5B 00             call         $0x800695B,$0x0
08007480: 9D                            ifkret
08007481: 4A 47                         w stz        b.0x1C
08007483: 18 46                         r:=          b.0x18
08007485: 1A 87 4B                      w move       r.0x1C,b.0x2C
08007488: 44 4B                         w test       b.0x2C
0800748A: C4 23                         if = go      $0x23
0800748C: FD 3D C5 2C                   w2 laddr     @b.0x2C
08007490: 04 F5 00                      by1 :=       r2.(0x0)
08007493: 1C 48                         by1 =:       b.0x20
08007495: 30 CD 83                      by1 comp     $0x83
08007498: D6 08                         if >>= go    $0x8
0800749A: 30 0D                         by1 comp     $0xD
0800749C: C4 04                         if = go      $0x4
0800749E: 4D 47                         w set1       b.0x1C
080074A0: 44 47                         w test       b.0x1C
080074A2: C6 0B                         if >< go     $0xB
080074A4: 18 4B                         r:=          b.0x2C
080074A6: 1A 86 4B                      w move       r.0x18,b.0x2C
080074A9: 44 4B                         w test       b.0x2C
080074AB: C6 E1                         if >< go     $0xFFFFFFFFFFFFFFE1
080074AD: 1A 46 C4 08 00 7A 18          w move       b.0x18,$0x8007A18
080074B4: 18 46                         r:=          b.0x18
080074B6: 1A 87 4B                      w move       r.0x1C,b.0x2C
080074B9: 44 4B                         w test       b.0x2C
080074BB: C5 00 8A                      if = go      $0x8A
080074BE: 18 42                         r:=          b.0x8
080074C0: 1A 4B 85                      w move       b.0x2C,r.0x14
080074C3: C3 08 00 87 71 00             call         $0x8008771,$0x0
080074C9: 9D                            ifkret
080074CA: 18 42                         r:=          b.0x8
080074CC: 1A 86 4C                      w move       r.0x18,b.0x30
080074CF: FD 3D C5 30                   w2 laddr     @b.0x30
080074D3: 2D F5 00 CD 83                by comp2     r2.(0x0),$0x83
080074D8: D8 11                         if << go     $0x11
080074DA: 1A 4C 85                      w move       b.0x30,r.0x14
080074DD: 1A 03 86                      w move       $0x3,r.0x18
080074E0: C3 08 00 3F 37 00             call         $0x8003F37,$0x0
080074E6: 9D                            ifkret
080074E7: C0 43                         go           $0x43
080074E9: 44 C5 18                      w test       @b.0x18
080074EC: C4 3E                         if = go      $0x3E
080074EE: 44 47                         w test       b.0x1C
080074F0: C4 3A                         if = go      $0x3A
080074F2: C3 08 00 37 30 00             call         $0x8003730,$0x0
080074F8: 9D                            ifkret
080074F9: 44 D0                         w test       r1
080074FB: C4 2F                         if = go      $0x2F
080074FD: 18 42                         r:=          b.0x8
080074FF: 1A 4C 85                      w move       b.0x30,r.0x14
08007502: C3 08 00 A2 53 00             call         $0x800A253,$0x0
08007508: 9D                            ifkret
08007509: 18 42                         r:=          b.0x8
0800750B: 1A 85 4C                      w move       r.0x14,b.0x30
0800750E: 1A 45 85                      w move       b.0x14,r.0x14
08007511: 1A 4C 86                      w move       b.0x30,r.0x18
08007514: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
0800751A: 9D                            ifkret
0800751B: 18 42                         r:=          b.0x8
0800751D: 1A 86 4C                      w move       r.0x18,b.0x30
08007520: 1A 45 85                      w move       b.0x14,r.0x14
08007523: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007529: 9D                            ifkret
0800752A: 18 42                         r:=          b.0x8
0800752C: 1A 4C 85                      w move       b.0x30,r.0x14
0800752F: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007535: 9D                            ifkret
08007536: 18 42                         r:=          b.0x8
08007538: 1A 85 4C                      w move       r.0x14,b.0x30
0800753B: 18 4B                         r:=          b.0x2C
0800753D: 1A 86 4B                      w move       r.0x18,b.0x2C
08007540: 44 4B                         w test       b.0x2C
08007542: C7 FF 7C                      if >< go     $0xFFFFFFFFFFFFFF7C
08007545: 44 47                         w test       b.0x1C
08007547: C4 06                         if = go      $0x6
08007549: 4A 49                         w stz        b.0x24
0800754B: C0 17                         go           $0x17
0800754D: 18 42                         r:=          b.0x8
0800754F: 4A 85                         w stz        r.0x14
08007551: 0C C5 18                      w1 :=        @b.0x18
08007554: 20 86                         w1 =:        r.0x18
08007556: 1A 45 87                      w move       b.0x14,r.0x1C
08007559: C3 08 00 71 05 00             call         $0x8007105,$0x0
0800755F: 9D                            ifkret
08007560: 20 49                         w1 =:        b.0x24
08007562: 4A C4 08 00 7A 18             w stz        $0x8007A18
08007568: C3 08 00 91 F3 00             call         $0x80091F3,$0x0
0800756E: 9D                            ifkret
0800756F: C3 08 00 A4 8A 00             call         $0x800A48A,$0x0
08007575: 9D                            ifkret
08007576: 44 49                         w test       b.0x24
08007578: C4 05                         if = go      $0x5
0800757A: 0C 49                         w1 :=        b.0x24
0800757C: 81                            retk
0800757D: 80                            ret
0800757E: 9C                            entd
0800757F: FD C0 4C                      l=:          b.0x30
08007582: 20 4D                         w1 =:        b.0x34
08007584: 1A CD 72 50                   w move       $0x72,b.0x40
08007588: 4D 51                         w set1       b.0x44
0800758A: FE 79 C4 08 00 78 C4 52 03    w bmove      $0x80078C4,b.0x48,$0x3
08007593: 1A 3F 55                      w move       $0x3F,b.0x54
08007596: C3 08 00 B9 7C 07 50 51 C5 48 C5 4C C5 50 55 4F call         $0x800B97C,$0x7,b.0x40,b.0x44,@b.0x48,@b.0x4C,@b.0x50,b.0x54,b.0x3C
080075A6: D2 04                         if -k go     $0x4
080075A8: B4 4C                         jumpg        b.0x30
080075AA: 4A 4E                         w stz        b.0x38
080075AC: 04 0D                         by1 :=       $0xD
080075AE: 0D 4E                         w2 :=        b.0x38
080075B0: 2D E5 18 D0                   by comp2     @b.0x18+,r1
080075B4: C4 06                         if = go      $0x6
080075B6: 4F 4E                         w incr       b.0x38
080075B8: C0 F4                         go           $0xFFFFFFFFFFFFFFF4
080075BA: 1A CD 72 50                   w move       $0x72,b.0x40
080075BE: 4D 51                         w set1       b.0x44
080075C0: 61 01                         w2 -         $0x1
080075C2: 1A D1 5B                      w move       r2,b.0x6C
080075C5: 4A 5A                         w stz        b.0x68
080075C7: 0E 46                         w3 :=        b.0x18
080075C9: 22 59                         w3 =:        b.0x64
080075CB: FD 20 59 56 0C                by bmove     b.0x64,b.0x58,$0xC
080075D0: 1A 3F 55                      w move       $0x3F,b.0x54
080075D3: C3 08 00 B9 7C 07 50 51 C5 58 C5 5C C5 60 55 4F call         $0x800B97C,$0x7,b.0x40,b.0x44,@b.0x58,@b.0x5C,@b.0x60,b.0x54,b.0x3C
080075E3: D2 04                         if -k go     $0x4
080075E5: B4 4C                         jumpg        b.0x30
080075E7: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
080075ED: D2 04                         if -k go     $0x4
080075EF: B4 4C                         jumpg        b.0x30
080075F1: 0C 4D                         w1 :=        b.0x34
080075F3: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
080075F9: D2 04                         if -k go     $0x4
080075FB: B4 4C                         jumpg        b.0x30
080075FD: FE 03                         clrk
080075FF: B4 4C                         jumpg        b.0x30
08007601: B8 CF 00 00 00 70             ents         $0x70
08007607: 04 CD 2A                      by1 :=       $0x2A
0800760A: 85                            bi2 clr
0800760B: 2D E5 18 D0                   by comp2     @b.0x18+,r1
0800760F: C6 0F                         if >< go     $0xF
08007611: 06 0D                         by3 :=       $0xD
08007613: 0F 01                         w4 :=        $0x1
08007615: 2D E7 18 D2                   by comp2     @b.0x18+,r3
08007619: C6 05                         if >< go     $0x5
0800761B: C1 00 8C                      go           $0x8C
0800761E: 06 0D                         by3 :=       $0xD
08007620: 87                            bi4 clr
08007621: 2D E7 18 D2                   by comp2     @b.0x18+,r3
08007625: C6 24                         if >< go     $0x24
08007627: 4A 4A                         w stz        b.0x28
08007629: 1A 45 4B                      w move       b.0x14,b.0x2C
0800762C: 44 4B                         w test       b.0x2C
0800762E: C4 19                         if = go      $0x19
08007630: 18 4B                         r:=          b.0x2C
08007632: 44 81                         w test       r.0x4
08007634: C4 0C                         if = go      $0xC
08007636: 44 4A                         w test       b.0x28
08007638: C6 06                         if >< go     $0x6
0800763A: 4D 4A                         w set1       b.0x28
0800763C: C0 04                         go           $0x4
0800763E: 4A 81                         w stz        r.0x4
08007640: 1A 88 4B                      w move       r.0x20,b.0x2C
08007643: 44 4B                         w test       b.0x2C
08007645: C6 EB                         if >< go     $0xFFFFFFFFFFFFFFEB
08007647: C0 60                         go           $0x60
08007649: 4A 4A                         w stz        b.0x28
0800764B: 1A 45 4B                      w move       b.0x14,b.0x2C
0800764E: 44 4B                         w test       b.0x2C
08007650: C4 47                         if = go      $0x47
08007652: 18 4B                         r:=          b.0x2C
08007654: 44 82                         w test       r.0x8
08007656: C6 38                         if >< go     $0x38
08007658: 0C 85                         w1 :=        r.0x14
0800765A: 18 42                         r:=          b.0x8
0800765C: 20 85                         w1 =:        r.0x14
0800765E: FD 20 46 86 0C                by bmove     b.0x18,r.0x18,$0xC
08007663: C3 08 00 66 44 00             call         $0x8006644,$0x0
08007669: 9D                            ifkret
0800766A: 44 D0                         w test       r1
0800766C: C4 1E                         if = go      $0x1E
0800766E: 44 4A                         w test       b.0x28
08007670: C6 0C                         if >< go     $0xC
08007672: 0C 01                         w1 :=        $0x1
08007674: 18 4B                         r:=          b.0x2C
08007676: 20 81                         w1 =:        r.0x4
08007678: 4D 4A                         w set1       b.0x28
0800767A: C0 0E                         go           $0xE
0800767C: 0C CD AF                      w1 :=        $0xAF
0800767F: C3 08 00 75 7E 00             call         $0x800757E,$0x0
08007685: 9D                            ifkret
08007686: 84                            bi1 clr
08007687: 80                            ret
08007688: C0 06                         go           $0x6
0800768A: 18 4B                         r:=          b.0x2C
0800768C: 4A 81                         w stz        r.0x4
0800768E: 18 4B                         r:=          b.0x2C
08007690: 1A 88 4B                      w move       r.0x20,b.0x2C
08007693: 44 4B                         w test       b.0x2C
08007695: C6 BD                         if >< go     $0xFFFFFFFFFFFFFFBD
08007697: 44 4A                         w test       b.0x28
08007699: C6 0E                         if >< go     $0xE
0800769B: 0C CD AD                      w1 :=        $0xAD
0800769E: C3 08 00 75 7E 00             call         $0x800757E,$0x0
080076A4: 9D                            ifkret
080076A5: 84                            bi1 clr
080076A6: 80                            ret
080076A7: 0C 01                         w1 :=        $0x1
080076A9: 80                            ret
080076AA: B8 CF 00 00 00 A4             ents         $0xA4
080076B0: C0 0D                         go           $0xD
080076B2: 9C                            entd
080076B3: FD C0 53                      l=:          b.0x4C
080076B6: 20 43                         w1 =:        b.0xC
080076B8: 80                            ret
080076B9: FE 03                         clrk
080076BB: B4 53                         jumpg        b.0x4C
080076BD: 4A 4B                         w stz        b.0x2C
080076BF: 4A 50                         w stz        b.0x40
080076C1: 04 0D                         by1 :=       $0xD
080076C3: 0D 50                         w2 :=        b.0x40
080076C5: 2D E5 18 D0                   by comp2     @b.0x18+,r1
080076C9: C4 11                         if = go      $0x11
080076CB: 06 CD 2E                      by3 :=       $0x2E
080076CE: 2D E5 18 D2                   by comp2     @b.0x18+,r3
080076D2: C6 04                         if >< go     $0x4
080076D4: 4D 4B                         w set1       b.0x2C
080076D6: 4F 50                         w incr       b.0x40
080076D8: C0 E9                         go           $0xFFFFFFFFFFFFFFE9
080076DA: 1A CE 00 BC 54                w move       $0xBC,b.0x50
080076DF: 61 01                         w2 -         $0x1
080076E1: 1A D1 5A                      w move       r2,b.0x68
080076E4: 4A 59                         w stz        b.0x64
080076E6: 0E 46                         w3 :=        b.0x18
080076E8: 22 58                         w3 =:        b.0x60
080076EA: FD 20 58 55 0C                by bmove     b.0x60,b.0x54,$0xC
080076EF: FE 79 C4 08 00 78 D0 58 03    w bmove      $0x80078D0,b.0x60,$0x3
080076F8: C3 08 00 B9 7C 0B 54 C5 54 C5 58 C5 5C C5 60 C5 64 C5 68 4C 4D 4E 4F call         $0x800B97C,$0xB,b.0x50,@b.0x54,@b.0x58,@b.0x5C,@b.0x60,@b.0x64,@b.0x68,b.0x30,b.0x34,b.0x38,b.0x3C
0800770F: D2 08                         if -k go     $0x8
08007711: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
08007717: 85                            bi2 clr
08007718: 52 50 D1                      w swap       b.0x40,r2
0800771B: 21 51                         w2 =:        b.0x44
0800771D: 44 4B                         w test       b.0x2C
0800771F: C4 11                         if = go      $0x11
08007721: 04 CD 2E                      by1 :=       $0x2E
08007724: 0D 50                         w2 :=        b.0x40
08007726: 2D E5 18 D0                   by comp2     @b.0x18+,r1
0800772A: C4 06                         if = go      $0x6
0800772C: 4F 50                         w incr       b.0x40
0800772E: C0 F3                         go           $0xFFFFFFFFFFFFFFF3
08007730: 1A CE 00 8D 54                w move       $0x8D,b.0x50
08007735: 85                            bi2 clr
08007736: FE 28 E1 08 00 6E CA          h1 laddr     $0x8006ECA+
0800773D: 20 5B                         w1 =:        b.0x6C
0800773F: 0E 50                         w3 :=        b.0x40
08007741: 62 01                         w3 -         $0x1
08007743: 1A D2 61                      w move       r3,b.0x84
08007746: 4A 60                         w stz        b.0x80
08007748: 0F 46                         w4 :=        b.0x18
0800774A: 23 5F                         w4 =:        b.0x7C
0800774C: FD 20 5F 5C 0C                by bmove     b.0x7C,b.0x70,$0xC
08007751: C3 08 00 B9 7C 09 54 C5 6C 4C 4D 4E 4B C5 70 C5 74 C5 78 call         $0x800B97C,$0x9,b.0x50,@b.0x6C,b.0x30,b.0x34,b.0x38,b.0x2C,@b.0x70,@b.0x74,@b.0x78
08007764: D2 08                         if -k go     $0x8
08007766: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
0800776C: 44 45                         w test       b.0x14
0800776E: C5 00 B1                      if = go      $0xB1
08007771: 18 CF 08 00 6E CA             r:=          $0x8006ECA
08007777: 44 8C                         w test       r.0x30
08007779: C5 00 A6                      if = go      $0xA6
0800777C: 0D 8C                         w2 :=        r.0x30
0800777E: 21 49                         w2 =:        b.0x24
08007780: 35 C4 08 00 6E 2C             w2 comp      $0x8006E2C
08007786: DB 00 99                      if <<= go    $0x99
08007789: 44 C4 08 00 79 F8             w test       $0x80079F8
0800778F: C6 10                         if >< go     $0x10
08007791: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08007797: D2 08                         if -k go     $0x8
08007799: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
0800779F: 1A CD 72 54                   w move       $0x72,b.0x50
080077A3: 4D 5B                         w set1       b.0x6C
080077A5: FE 79 C4 08 00 78 E4 5F 03    w bmove      $0x80078E4,b.0x7C,$0x3
080077AE: 1A 3F 62                      w move       $0x3F,b.0x88
080077B1: C3 08 00 B9 7C 07 54 5B C5 7C C5 80 C5 84 62 52 call         $0x800B97C,$0x7,b.0x50,b.0x6C,@b.0x7C,@b.0xFFFFFFFFFFFFFF80,@b.0xFFFFFFFFFFFFFF84,b.0x88,b.0x48
080077C1: D2 08                         if -k go     $0x8
080077C3: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
080077C9: 1A CD 72 54                   w move       $0x72,b.0x50
080077CD: 4D 5B                         w set1       b.0x6C
080077CF: 0D 51                         w2 :=        b.0x44
080077D1: 61 01                         w2 -         $0x1
080077D3: 1A D1 68                      w move       r2,b.0xA0
080077D6: 4A 67                         w stz        b.0x9C
080077D8: 0E 46                         w3 :=        b.0x18
080077DA: 22 66                         w3 =:        b.0x98
080077DC: FD 20 66 63 0C                by bmove     b.0x98,b.0x8C,$0xC
080077E1: 1A 3F 62                      w move       $0x3F,b.0x88
080077E4: C3 08 00 B9 7C 07 54 5B C5 8C C5 90 C5 94 62 52 call         $0x800B97C,$0x7,b.0x50,b.0x6C,@b.0xFFFFFFFFFFFFFF8C,@b.0xFFFFFFFFFFFFFF90,@b.0xFFFFFFFFFFFFFF94,b.0x88,b.0x48
080077F4: D2 08                         if -k go     $0x8
080077F6: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
080077FC: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08007802: D2 08                         if -k go     $0x8
08007804: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
0800780A: 0C CD AA                      w1 :=        $0xAA
0800780D: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08007813: D2 08                         if -k go     $0x8
08007815: C3 08 00 76 B2 00             call         $0x80076B2,$0x0
0800781B: 0C CD AA                      w1 :=        $0xAA
0800781E: 81                            retk
0800781F: 84                            bi1 clr
08007820: 80                            ret
08007821: 9C                            entd
08007822: FD C0 65                      l=:          b.0x94
08007825: 1A 66 6A                      w move       b.0x98,b.0xA8
08007828: 4A 69                         w stz        b.0xA4
0800782A: FD 3D C5 A8                   w2 laddr     @b.0xFFFFFFFFFFFFFFA8
0800782E: 55 69                         w2 +         b.0xA4
08007830: 04 F5 00                      by1 :=       r2.(0x0)
08007833: 1C 68                         by1 =:       b.0xA0
08007835: 30 0D                         by1 comp     $0xD
08007837: C4 2A                         if = go      $0x2A
08007839: 30 CD 28                      by1 comp     $0x28
0800783C: C4 07                         if = go      $0x7
0800783E: 30 CD 2E                      by1 comp     $0x2E
08007841: C6 08                         if >< go     $0x8
08007843: 0C 01                         w1 :=        $0x1
08007845: FE 03                         clrk
08007847: B4 65                         jumpg        b.0x94
08007849: 0C 69                         w1 :=        b.0xA4
0800784B: 54 01                         w1 +         $0x1
0800784D: 20 69                         w1 =:        b.0xA4
0800784F: 34 13                         w1 comp      $0x13
08007851: C8 07                         if > go      $0x7
08007853: 2D 68 09                      by comp2     b.0xA0,$0x9
08007856: C6 09                         if >< go     $0x9
08007858: 18 6A                         r:=          b.0xA8
0800785A: 1A 85 6A                      w move       r.0x14,b.0xA8
0800785D: 4A 69                         w stz        b.0xA4
0800785F: C0 CB                         go           $0xFFFFFFFFFFFFFFCB
08007861: 84                            bi1 clr
08007862: FE 03                         clrk
08007864: B4 65                         jumpg        b.0x94
08007866: 9C                            entd
08007867: FD C0 6B                      l=:          b.0xAC
0800786A: 4A 72                         w stz        b.0xC8
0800786C: 1A 6D 73                      w move       b.0xB4,b.0xCC
0800786F: 4A 70                         w stz        b.0xC0
08007871: FD 3D C5 CC                   w2 laddr     @b.0xFFFFFFFFFFFFFFCC
08007875: 55 70                         w2 +         b.0xC0
08007877: 04 F5 00                      by1 :=       r2.(0x0)
0800787A: 1C 6F                         by1 =:       b.0xBC
0800787C: 30 0D                         by1 comp     $0xD
0800787E: C4 56                         if = go      $0x56
08007880: 44 72                         w test       b.0xC8
08007882: C6 15                         if >< go     $0x15
08007884: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800788A: D2 04                         if -k go     $0x4
0800788C: B4 6B                         jumpg        b.0xAC
0800788E: 20 72                         w1 =:        b.0xC8
08007890: 20 74                         w1 =:        b.0xD0
08007892: 1A 3F 71                      w move       $0x3F,b.0xC4
08007895: C0 1B                         go           $0x1B
08007897: 2E 71 13                      w comp2      b.0xC4,$0x13
0800789A: C6 16                         if >< go     $0x16
0800789C: C3 08 00 86 91 00             call         $0x8008691,$0x0
080078A2: D2 04                         if -k go     $0x4
080078A4: B4 6B                         jumpg        b.0xAC
080078A6: 18 74                         r:=          b.0xD0
080078A8: 20 85                         w1 =:        r.0x14
080078AA: 1A 85 74                      w move       r.0x14,b.0xD0
080078AD: 1A 3F 71                      w move       $0x3F,b.0xC4
080078B0: 0C 71                         w1 :=        b.0xC4
080078B2: 54 01                         w1 +         $0x1
080078B4: 20 71                         w1 =:        b.0xC4
080078B6: 05 6F                         by2 :=       b.0xBC
080078B8: FD 3E C5 D0                   w3 laddr     @b.0xFFFFFFFFFFFFFFD0
080078BC: 56 D0                         w3 +         r1
080078BE: 1D F6 00                      by2 =:       r3.(0x0)
080078C1: 0F 70                         w4 :=        b.0xC0
080078C3: 57 01                         w4 +         $0x1
080078C5: 23 70                         w4 =:        b.0xC0
080078C7: 37 13                         w4 comp      $0x13
080078C9: CE 09                         if <= go     $0x9
080078CB: 18 73                         r:=          b.0xCC
080078CD: 1A 85 73                      w move       r.0x14,b.0xCC
080078D0: 4A 70                         w stz        b.0xC0
080078D2: C0 9F                         go           $0xFFFFFFFFFFFFFF9F
080078D4: 1A 6C 73                      w move       b.0xB0,b.0xCC
080078D7: 4A 70                         w stz        b.0xC0
080078D9: 44 72                         w test       b.0xC8
080078DB: C6 15                         if >< go     $0x15
080078DD: C3 08 00 86 91 00             call         $0x8008691,$0x0
080078E3: D2 04                         if -k go     $0x4
080078E5: B4 6B                         jumpg        b.0xAC
080078E7: 20 72                         w1 =:        b.0xC8
080078E9: 20 74                         w1 =:        b.0xD0
080078EB: 1A 3F 71                      w move       $0x3F,b.0xC4
080078EE: C0 1B                         go           $0x1B
080078F0: 2E 71 13                      w comp2      b.0xC4,$0x13
080078F3: C6 16                         if >< go     $0x16
080078F5: C3 08 00 86 91 00             call         $0x8008691,$0x0
080078FB: D2 04                         if -k go     $0x4
080078FD: B4 6B                         jumpg        b.0xAC
080078FF: 18 74                         r:=          b.0xD0
08007901: 20 85                         w1 =:        r.0x14
08007903: 1A 85 74                      w move       r.0x14,b.0xD0
08007906: 1A 3F 71                      w move       $0x3F,b.0xC4
08007909: 0C 71                         w1 :=        b.0xC4
0800790B: 54 01                         w1 +         $0x1
0800790D: 20 71                         w1 =:        b.0xC4
0800790F: FD 3E C5 CC                   w3 laddr     @b.0xFFFFFFFFFFFFFFCC
08007913: 56 70                         w3 +         b.0xC0
08007915: 05 F6 00                      by2 :=       r3.(0x0)
08007918: FD 3F C5 D0                   w4 laddr     @b.0xFFFFFFFFFFFFFFD0
0800791C: 57 D0                         w4 +         r1
0800791E: 1D F7 00                      by2 =:       r4.(0x0)
08007921: 31 0D                         by2 comp     $0xD
08007923: C4 15                         if = go      $0x15
08007925: 0D 70                         w2 :=        b.0xC0
08007927: 55 01                         w2 +         $0x1
08007929: 21 70                         w2 =:        b.0xC0
0800792B: 35 13                         w2 comp      $0x13
0800792D: CE 09                         if <= go     $0x9
0800792F: 18 73                         r:=          b.0xCC
08007931: 1A 85 73                      w move       r.0x14,b.0xCC
08007934: 4A 70                         w stz        b.0xC0
08007936: C0 A3                         go           $0xFFFFFFFFFFFFFFA3
08007938: 0C 72                         w1 :=        b.0xC8
0800793A: FE 03                         clrk
0800793C: B4 6B                         jumpg        b.0xAC
0800793E: B8 CF 00 00 00 D8             ents         $0xD8
08007944: 18 46                         r:=          b.0x18
08007946: 44 83                         w test       r.0xC
08007948: C4 07                         if = go      $0x7
0800794A: 84                            bi1 clr
0800794B: 80                            ret
0800794C: C1 00 F5                      go           $0xF5
0800794F: 4D 83                         w set1       r.0xC
08007951: 0D 85                         w2 :=        r.0x14
08007953: 18 42                         r:=          b.0x8
08007955: 21 85                         w2 =:        r.0x14
08007957: FD 3E 4C                      w3 laddr     b.0x30
0800795A: 22 86                         w3 =:        r.0x18
0800795C: 4A 87                         w stz        r.0x1C
0800795E: 1A CD 63 88                   w move       $0x63,r.0x20
08007962: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08007968: 9D                            ifkret
08007969: 18 42                         r:=          b.0x8
0800796B: 1A 45 85                      w move       b.0x14,r.0x14
0800796E: FD 3D 4C                      w2 laddr     b.0x30
08007971: 21 86                         w2 =:        r.0x18
08007973: 4A 87                         w stz        r.0x1C
08007975: 1A CD 63 88                   w move       $0x63,r.0x20
08007979: 18 46                         r:=          b.0x18
0800797B: 0E 84                         w3 :=        r.0x10
0800797D: 18 42                         r:=          b.0x8
0800797F: 22 89                         w3 =:        r.0x24
08007981: C3 08 00 76 AA 00             call         $0x80076AA,$0x0
08007987: 9D                            ifkret
08007988: 18 42                         r:=          b.0x8
0800798A: 0D 89                         w2 :=        r.0x24
0800798C: 18 46                         r:=          b.0x18
0800798E: 21 84                         w2 =:        r.0x10
08007990: 20 48                         w1 =:        b.0x20
08007992: 34 CD 2E                      w1 comp      $0x2E
08007995: C7 00 A9                      if >< go     $0xA9
08007998: 1A 85 66                      w move       r.0x14,b.0x98
0800799B: 21 75                         w2 =:        b.0xD4
0800799D: C3 08 00 78 21 00             call         $0x8007821,$0x0
080079A3: 9D                            ifkret
080079A4: 44 D0                         w test       r1
080079A6: C7 00 98                      if >< go     $0x98
080079A9: C3 08 00 AC 2E 00             call         $0x800AC2E,$0x0
080079AF: 9D                            ifkret
080079B0: 20 49                         w1 =:        b.0x24
080079B2: 20 4A                         w1 =:        b.0x28
080079B4: 44 D0                         w test       r1
080079B6: C5 00 88                      if = go      $0x88
080079B9: 18 46                         r:=          b.0x18
080079BB: 1A 85 6C                      w move       r.0x14,b.0xB0
080079BE: 1A 4A 6D                      w move       b.0x28,b.0xB4
080079C1: C3 08 00 78 66 00             call         $0x8007866,$0x0
080079C7: 9D                            ifkret
080079C8: 20 4B                         w1 =:        b.0x2C
080079CA: 18 42                         r:=          b.0x8
080079CC: 20 85                         w1 =:        r.0x14
080079CE: FD 3D 4C                      w2 laddr     b.0x30
080079D1: 21 86                         w2 =:        r.0x18
080079D3: 4A 87                         w stz        r.0x1C
080079D5: 1A CD 63 88                   w move       $0x63,r.0x20
080079D9: C3 08 00 88 5B 00             call         $0x800885B,$0x0
080079DF: 9D                            ifkret
080079E0: 18 42                         r:=          b.0x8
080079E2: 1A 45 85                      w move       b.0x14,r.0x14
080079E5: FD 3D 4C                      w2 laddr     b.0x30
080079E8: 21 86                         w2 =:        r.0x18
080079EA: 4A 87                         w stz        r.0x1C
080079EC: 1A CD 63 88                   w move       $0x63,r.0x20
080079F0: 18 46                         r:=          b.0x18
080079F2: 0E 84                         w3 :=        r.0x10
080079F4: 18 42                         r:=          b.0x8
080079F6: 22 89                         w3 =:        r.0x24
080079F8: C3 08 00 76 AA 00             call         $0x80076AA,$0x0
080079FE: 9D                            ifkret
080079FF: 18 42                         r:=          b.0x8
08007A01: 0D 89                         w2 :=        r.0x24
08007A03: 18 46                         r:=          b.0x18
08007A05: 21 84                         w2 =:        r.0x10
08007A07: 20 48                         w1 =:        b.0x20
08007A09: 34 CD 2E                      w1 comp      $0x2E
08007A0C: C6 15                         if >< go     $0x15
08007A0E: 18 42                         r:=          b.0x8
08007A10: 1A 4B 85                      w move       b.0x2C,r.0x14
08007A13: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007A19: 9D                            ifkret
08007A1A: 18 42                         r:=          b.0x8
08007A1C: 1A 85 4B                      w move       r.0x14,b.0x2C
08007A1F: C0 0F                         go           $0xF
08007A21: 0E 4B                         w3 :=        b.0x2C
08007A23: 52 85 D2                      w swap       r.0x14,r3
08007A26: 22 4B                         w3 =:        b.0x2C
08007A28: 18 46                         r:=          b.0x18
08007A2A: 18 85                         r:=          r.0x14
08007A2C: 22 86                         w3 =:        r.0x18
08007A2E: 2E 48 CD 2E                   w comp2      b.0x20,$0x2E
08007A32: C6 0C                         if >< go     $0xC
08007A34: 18 4A                         r:=          b.0x28
08007A36: 1A 86 4A                      w move       r.0x18,b.0x28
08007A39: 44 4A                         w test       b.0x28
08007A3B: C7 FF 7E                      if >< go     $0xFFFFFFFFFFFFFF7E
08007A3E: 0C 48                         w1 :=        b.0x20
08007A40: 80                            ret
08007A41: B8 CF 00 00 00 84             ents         $0x84
08007A47: C0 24                         go           $0x24
08007A49: 9C                            entd
08007A4A: FD C0 60                      l=:          b.0x80
08007A4D: 20 43                         w1 =:        b.0xC
08007A4F: FD 3C 47                      w1 laddr     b.0x1C
08007A52: 18 42                         r:=          b.0x8
08007A54: 20 85                         w1 =:        r.0x14
08007A56: 4A 86                         w stz        r.0x18
08007A58: 1A CD 63 87                   w move       $0x63,r.0x1C
08007A5C: 1A 43 88                      w move       b.0xC,r.0x20
08007A5F: C3 08 00 68 D0 00             call         $0x80068D0,$0x0
08007A65: 9D                            ifkret
08007A66: 80                            ret
08007A67: FE 03                         clrk
08007A69: B4 60                         jumpg        b.0x80
08007A6B: 18 45                         r:=          b.0x14
08007A6D: 0D 85                         w2 :=        r.0x14
08007A6F: 18 42                         r:=          b.0x8
08007A71: 21 85                         w2 =:        r.0x14
08007A73: FD 3E 47                      w3 laddr     b.0x1C
08007A76: 22 86                         w3 =:        r.0x18
08007A78: 4A 87                         w stz        r.0x1C
08007A7A: 1A CD 63 88                   w move       $0x63,r.0x20
08007A7E: C3 08 00 88 5B 00             call         $0x800885B,$0x0
08007A84: D2 08                         if -k go     $0x8
08007A86: C3 08 00 7A 49 00             call         $0x8007A49,$0x0
08007A8C: 18 42                         r:=          b.0x8
08007A8E: FE 79 C4 08 00 78 F4 86 03    w bmove      $0x80078F4,r.0x18,$0x3
08007A97: FD 3D 47                      w2 laddr     b.0x1C
08007A9A: 21 89                         w2 =:        r.0x24
08007A9C: 4A 8A                         w stz        r.0x28
08007A9E: 1A CD 63 8B                   w move       $0x63,r.0x2C
08007AA2: FE 79 C4 08 00 79 00 8C 03    w bmove      $0x8007900,r.0x30,$0x3
08007AAB: C3 08 00 C0 B2 00             call         $0x800C0B2,$0x0
08007AB1: D2 08                         if -k go     $0x8
08007AB3: C3 08 00 7A 49 00             call         $0x8007A49,$0x0
08007AB9: 18 42                         r:=          b.0x8
08007ABB: 1A 85 46                      w move       r.0x14,b.0x18
08007ABE: 1A 46 85                      w move       b.0x18,r.0x14
08007AC1: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
08007AC7: D2 08                         if -k go     $0x8
08007AC9: C3 08 00 7A 49 00             call         $0x8007A49,$0x0
08007ACF: 80                            ret
08007AD0: B8 CF 00 00 00 30             ents         $0x30
08007AD6: 18 45                         r:=          b.0x14
08007AD8: 44 82                         w test       r.0x8
08007ADA: C5 00 7C                      if = go      $0x7C
08007ADD: 18 42                         r:=          b.0x8
08007ADF: 4D 85                         w set1       r.0x14
08007AE1: 1A 45 86                      w move       b.0x14,r.0x18
08007AE4: C3 08 00 79 3E 00             call         $0x800793E,$0x0
08007AEA: 9D                            ifkret
08007AEB: 20 4A                         w1 =:        b.0x28
08007AED: 18 45                         r:=          b.0x14
08007AEF: 18 85                         r:=          r.0x14
08007AF1: 0D 86                         w2 :=        r.0x18
08007AF3: 18 42                         r:=          b.0x8
08007AF5: 21 85                         w2 =:        r.0x14
08007AF7: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007AFD: 9D                            ifkret
08007AFE: 18 42                         r:=          b.0x8
08007B00: 0D 85                         w2 :=        r.0x14
08007B02: 18 45                         r:=          b.0x14
08007B04: 18 85                         r:=          r.0x14
08007B06: 21 86                         w2 =:        r.0x18
08007B08: 44 4A                         w test       b.0x28
08007B0A: C4 40                         if = go      $0x40
08007B0C: 44 C4 08 00 79 F8             w test       $0x80079F8
08007B12: C6 09                         if >< go     $0x9
08007B14: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08007B1A: 9D                            ifkret
08007B1B: 18 42                         r:=          b.0x8
08007B1D: FE 79 C4 08 00 79 14 85 03    w bmove      $0x8007914,r.0x14,$0x3
08007B26: 18 45                         r:=          b.0x14
08007B28: 0D 85                         w2 :=        r.0x14
08007B2A: 18 42                         r:=          b.0x8
08007B2C: 21 88                         w2 =:        r.0x20
08007B2E: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
08007B34: 9D                            ifkret
08007B35: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08007B3B: 9D                            ifkret
08007B3C: 0C 4A                         w1 :=        b.0x28
08007B3E: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08007B44: 9D                            ifkret
08007B45: 0C 4A                         w1 :=        b.0x28
08007B47: 81                            retk
08007B48: C0 0C                         go           $0xC
08007B4A: 18 45                         r:=          b.0x14
08007B4C: 2E 84 46                      w comp2      r.0x10,b.0x18
08007B4F: D8 05                         if << go     $0x5
08007B51: 0C 01                         w1 :=        $0x1
08007B53: 80                            ret
08007B54: C0 3A                         go           $0x3A
08007B56: 4A 49                         w stz        b.0x24
08007B58: 1A 86 4B                      w move       r.0x18,b.0x2C
08007B5B: 44 4B                         w test       b.0x2C
08007B5D: C4 2E                         if = go      $0x2E
08007B5F: 0C C5 2C                      w1 :=        @b.0x2C
08007B62: 18 42                         r:=          b.0x8
08007B64: 20 85                         w1 =:        r.0x14
08007B66: 1A 46 86                      w move       b.0x18,r.0x18
08007B69: 1A 47 87                      w move       b.0x1C,r.0x1C
08007B6C: C3 08 00 7A D0 00             call         $0x8007AD0,$0x0
08007B72: 9D                            ifkret
08007B73: 44 D0                         w test       r1
08007B75: C4 0D                         if = go      $0xD
08007B77: 44 47                         w test       b.0x1C
08007B79: C4 07                         if = go      $0x7
08007B7B: 0C 01                         w1 :=        $0x1
08007B7D: 80                            ret
08007B7E: C0 04                         go           $0x4
08007B80: 4D 49                         w set1       b.0x24
08007B82: 18 4B                         r:=          b.0x2C
08007B84: 1A 81 4B                      w move       r.0x4,b.0x2C
08007B87: 44 4B                         w test       b.0x2C
08007B89: C6 D6                         if >< go     $0xFFFFFFFFFFFFFFD6
08007B8B: 0C 49                         w1 :=        b.0x24
08007B8D: 80                            ret
08007B8E: 84                            bi1 clr
08007B8F: 80                            ret
08007B90: B8 CF 00 00 00 30             ents         $0x30
08007B96: 18 47                         r:=          b.0x1C
08007B98: 44 82                         w test       r.0x8
08007B9A: C7 01 76                      if >< go     $0x176
08007B9D: 44 83                         w test       r.0xC
08007B9F: C7 01 71                      if >< go     $0x171
08007BA2: 18 42                         r:=          b.0x8
08007BA4: 1A 48 85                      w move       b.0x20,r.0x14
08007BA7: 1A 47 86                      w move       b.0x1C,r.0x18
08007BAA: C3 08 00 79 3E 00             call         $0x800793E,$0x0
08007BB0: 9D                            ifkret
08007BB1: 44 48                         w test       b.0x20
08007BB3: C4 35                         if = go      $0x35
08007BB5: 18 42                         r:=          b.0x8
08007BB7: 1A 47 85                      w move       b.0x1C,r.0x14
08007BBA: 18 47                         r:=          b.0x1C
08007BBC: 0D 84                         w2 :=        r.0x10
08007BBE: 18 42                         r:=          b.0x8
08007BC0: 21 86                         w2 =:        r.0x18
08007BC2: 53 42 1C                      w add2       b.0x8,$0x1C
08007BC5: C3 08 00 AC 2E 00             call         $0x800AC2E,$0x0
08007BCB: 9D                            ifkret
08007BCC: 53 42 24                      w add2       b.0x8,$0x24
08007BCF: 4A 4B                         w stz        b.0x2C
08007BD1: 44 D0                         w test       r1
08007BD3: C6 04                         if >< go     $0x4
08007BD5: 4D 4B                         w set1       b.0x2C
08007BD7: 0C 4B                         w1 :=        b.0x2C
08007BD9: 18 42                         r:=          b.0x8
08007BDB: 20 87                         w1 =:        r.0x1C
08007BDD: C3 08 00 7A D0 00             call         $0x8007AD0,$0x0
08007BE3: 9D                            ifkret
08007BE4: 44 D0                         w test       r1
08007BE6: C4 62                         if = go      $0x62
08007BE8: 0C 01                         w1 :=        $0x1
08007BEA: 20 C5 1C                      w1 =:        @b.0x1C
08007BED: 18 47                         r:=          b.0x1C
08007BEF: 4D 83                         w set1       r.0xC
08007BF1: 18 47                         r:=          b.0x1C
08007BF3: 18 85                         r:=          r.0x14
08007BF5: 44 86                         w test       r.0x18
08007BF7: C4 26                         if = go      $0x26
08007BF9: 85                            bi2 clr
08007BFA: 18 47                         r:=          b.0x1C
08007BFC: 18 85                         r:=          r.0x14
08007BFE: 52 86 D1                      w swap       r.0x18,r2
08007C01: 21 49                         w2 =:        b.0x24
08007C03: 18 47                         r:=          b.0x1C
08007C05: 0E 85                         w3 :=        r.0x14
08007C07: 18 42                         r:=          b.0x8
08007C09: 22 85                         w3 =:        r.0x14
08007C0B: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007C11: 9D                            ifkret
08007C12: 18 42                         r:=          b.0x8
08007C14: 0D 85                         w2 :=        r.0x14
08007C16: 18 47                         r:=          b.0x1C
08007C18: 21 85                         w2 =:        r.0x14
08007C1A: 1A 49 85                      w move       b.0x24,r.0x14
08007C1D: 18 47                         r:=          b.0x1C
08007C1F: 1A 86 4A                      w move       r.0x18,b.0x28
08007C22: 44 4A                         w test       b.0x28
08007C24: C4 22                         if = go      $0x22
08007C26: 18 42                         r:=          b.0x8
08007C28: 1A 45 85                      w move       b.0x14,r.0x14
08007C2B: 1A 46 86                      w move       b.0x18,r.0x18
08007C2E: 0C C5 28                      w1 :=        @b.0x28
08007C31: 20 87                         w1 =:        r.0x1C
08007C33: 1A 48 88                      w move       b.0x20,r.0x20
08007C36: C3 08 00 7B 90 00             call         $0x8007B90,$0x0
08007C3C: 9D                            ifkret
08007C3D: 18 4A                         r:=          b.0x28
08007C3F: 1A 81 4A                      w move       r.0x4,b.0x28
08007C42: 44 4A                         w test       b.0x28
08007C44: C6 E2                         if >< go     $0xFFFFFFFFFFFFFFE2
08007C46: C0 1D                         go           $0x1D
08007C48: 18 47                         r:=          b.0x1C
08007C4A: 18 85                         r:=          r.0x14
08007C4C: 0C 86                         w1 :=        r.0x18
08007C4E: 18 42                         r:=          b.0x8
08007C50: 20 85                         w1 =:        r.0x14
08007C52: C3 08 00 87 17 00             call         $0x8008717,$0x0
08007C58: 9D                            ifkret
08007C59: 18 42                         r:=          b.0x8
08007C5B: 0D 85                         w2 :=        r.0x14
08007C5D: 18 47                         r:=          b.0x1C
08007C5F: 18 85                         r:=          r.0x14
08007C61: 21 86                         w2 =:        r.0x18
08007C63: 18 42                         r:=          b.0x8
08007C65: 19 CD B1 85                   by move      $0xB1,r.0x14
08007C69: 04 CD 8C                      by1 :=       $0x8C
08007C6C: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007C72: 9D                            ifkret
08007C73: 44 D0                         w test       r1
08007C75: C4 44                         if = go      $0x44
08007C77: 44 C5 1C                      w test       @b.0x1C
08007C7A: C4 3F                         if = go      $0x3F
08007C7C: 18 42                         r:=          b.0x8
08007C7E: 1A 46 85                      w move       b.0x18,r.0x14
08007C81: 18 47                         r:=          b.0x1C
08007C83: 0C 85                         w1 :=        r.0x14
08007C85: 18 42                         r:=          b.0x8
08007C87: 20 86                         w1 =:        r.0x18
08007C89: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
08007C8F: 9D                            ifkret
08007C90: 18 42                         r:=          b.0x8
08007C92: 0D 86                         w2 :=        r.0x18
08007C94: 18 47                         r:=          b.0x1C
08007C96: 21 85                         w2 =:        r.0x14
08007C98: 18 42                         r:=          b.0x8
08007C9A: 1A 46 85                      w move       b.0x18,r.0x14
08007C9D: FE 79 C4 08 00 79 30 86 03    w bmove      $0x8007930,r.0x18,$0x3
08007CA6: C3 08 00 88 D6 00             call         $0x80088D6,$0x0
08007CAC: 9D                            ifkret
08007CAD: 18 42                         r:=          b.0x8
08007CAF: 1A 46 85                      w move       b.0x18,r.0x14
08007CB2: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007CB8: 9D                            ifkret
08007CB9: 18 42                         r:=          b.0x8
08007CBB: 19 CD B4 85                   by move      $0xB4,r.0x14
08007CBF: 04 CD 8C                      by1 :=       $0x8C
08007CC2: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007CC8: 9D                            ifkret
08007CC9: 44 D0                         w test       r1
08007CCB: C4 0E                         if = go      $0xE
08007CCD: 18 42                         r:=          b.0x8
08007CCF: 1A 47 85                      w move       b.0x1C,r.0x14
08007CD2: C3 08 00 7A 41 00             call         $0x8007A41,$0x0
08007CD8: 9D                            ifkret
08007CD9: 18 42                         r:=          b.0x8
08007CDB: 19 CD AF 85                   by move      $0xAF,r.0x14
08007CDF: 04 CD 8C                      by1 :=       $0x8C
08007CE2: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007CE8: 9D                            ifkret
08007CE9: 44 D0                         w test       r1
08007CEB: C6 16                         if >< go     $0x16
08007CED: 18 42                         r:=          b.0x8
08007CEF: 19 CD B5 85                   by move      $0xB5,r.0x14
08007CF3: 04 CD 8C                      by1 :=       $0x8C
08007CF6: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007CFC: 9D                            ifkret
08007CFD: 44 D0                         w test       r1
08007CFF: C4 11                         if = go      $0x11
08007D01: 18 42                         r:=          b.0x8
08007D03: 1A 45 85                      w move       b.0x14,r.0x14
08007D06: 1A 47 86                      w move       b.0x1C,r.0x18
08007D09: C3 08 00 74 53 00             call         $0x8007453,$0x0
08007D0F: 9D                            ifkret
08007D10: 80                            ret
08007D11: B8 CF 00 00 00 44             ents         $0x44
08007D17: 4D 4D                         w set1       b.0x34
08007D19: 4A C4 08 00 7A 08             w stz        $0x8007A08
08007D1F: C3 08 00 6A 00 00             call         $0x8006A00,$0x0
08007D25: 9D                            ifkret
08007D26: 18 42                         r:=          b.0x8
08007D28: 1A 46 85                      w move       b.0x18,r.0x14
08007D2B: FD 20 48 86 0C                by bmove     b.0x20,r.0x18,$0xC
08007D30: C3 08 00 76 01 00             call         $0x8007601,$0x0
08007D36: 9D                            ifkret
08007D37: 44 D0                         w test       r1
08007D39: C5 01 3E                      if = go      $0x13E
08007D3C: 18 42                         r:=          b.0x8
08007D3E: 19 CD AF 85                   by move      $0xAF,r.0x14
08007D42: 04 CD 8C                      by1 :=       $0x8C
08007D45: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007D4B: 9D                            ifkret
08007D4C: 44 D0                         w test       r1
08007D4E: C6 16                         if >< go     $0x16
08007D50: 18 42                         r:=          b.0x8
08007D52: 19 CD B5 85                   by move      $0xB5,r.0x14
08007D56: 04 CD 8C                      by1 :=       $0x8C
08007D59: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007D5F: 9D                            ifkret
08007D60: 44 D0                         w test       r1
08007D62: C4 25                         if = go      $0x25
08007D64: 18 42                         r:=          b.0x8
08007D66: 4D 85                         w set1       r.0x14
08007D68: 4A 86                         w stz        r.0x18
08007D6A: 4A 87                         w stz        r.0x1C
08007D6C: C3 08 00 71 05 00             call         $0x8007105,$0x0
08007D72: 9D                            ifkret
08007D73: 18 42                         r:=          b.0x8
08007D75: 1A 45 85                      w move       b.0x14,r.0x14
08007D78: 4D 86                         w set1       r.0x18
08007D7A: 4A 87                         w stz        r.0x1C
08007D7C: C3 08 00 AD 6E 00             call         $0x800AD6E,$0x0
08007D82: 9D                            ifkret
08007D83: 4D 4E                         w set1       b.0x38
08007D85: C0 04                         go           $0x4
08007D87: 4A 4E                         w stz        b.0x38
08007D89: 18 42                         r:=          b.0x8
08007D8B: 19 CD B4 85                   by move      $0xB4,r.0x14
08007D8F: 04 CD 8C                      by1 :=       $0x8C
08007D92: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007D98: 9D                            ifkret
08007D99: 44 D0                         w test       r1
08007D9B: C6 16                         if >< go     $0x16
08007D9D: 18 42                         r:=          b.0x8
08007D9F: 19 CD B5 85                   by move      $0xB5,r.0x14
08007DA3: 04 CD 8C                      by1 :=       $0x8C
08007DA6: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08007DAC: 9D                            ifkret
08007DAD: 44 D0                         w test       r1
08007DAF: C4 06                         if = go      $0x6
08007DB1: 4A 4F                         w stz        b.0x3C
08007DB3: C0 04                         go           $0x4
08007DB5: 4D 4F                         w set1       b.0x3C
08007DB7: 1A 46 50                      w move       b.0x18,b.0x40
08007DBA: 44 50                         w test       b.0x40
08007DBC: C4 73                         if = go      $0x73
08007DBE: 18 50                         r:=          b.0x40
08007DC0: 44 81                         w test       r.0x4
08007DC2: C4 64                         if = go      $0x64
08007DC4: 18 42                         r:=          b.0x8
08007DC6: 1A 45 85                      w move       b.0x14,r.0x14
08007DC9: 1A 4B 86                      w move       b.0x2C,r.0x18
08007DCC: 1A 50 87                      w move       b.0x40,r.0x1C
08007DCF: 1A 4F 88                      w move       b.0x3C,r.0x20
08007DD2: C3 08 00 7B 90 00             call         $0x8007B90,$0x0
08007DD8: 9D                            ifkret
08007DD9: 44 C5 40                      w test       @b.0x40
08007DDC: C4 06                         if = go      $0x6
08007DDE: 4A 4D                         w stz        b.0x34
08007DE0: C0 46                         go           $0x46
08007DE2: 18 50                         r:=          b.0x40
08007DE4: 2E 84 47                      w comp2      r.0x10,b.0x1C
08007DE7: D6 3F                         if >>= go    $0x3F
08007DE9: 18 42                         r:=          b.0x8
08007DEB: 1A 4B 85                      w move       b.0x2C,r.0x14
08007DEE: 18 50                         r:=          b.0x40
08007DF0: 0D 85                         w2 :=        r.0x14
08007DF2: 18 42                         r:=          b.0x8
08007DF4: 21 86                         w2 =:        r.0x18
08007DF6: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
08007DFC: 9D                            ifkret
08007DFD: 18 42                         r:=          b.0x8
08007DFF: 0D 86                         w2 :=        r.0x18
08007E01: 18 50                         r:=          b.0x40
08007E03: 21 85                         w2 =:        r.0x14
08007E05: 18 42                         r:=          b.0x8
08007E07: 1A 4B 85                      w move       b.0x2C,r.0x14
08007E0A: FE 79 C4 08 00 79 5C 86 03    w bmove      $0x800795C,r.0x18,$0x3
08007E13: C3 08 00 88 D6 00             call         $0x80088D6,$0x0
08007E19: 9D                            ifkret
08007E1A: 18 42                         r:=          b.0x8
08007E1C: 1A 4B 85                      w move       b.0x2C,r.0x14
08007E1F: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007E25: 9D                            ifkret
08007E26: 18 50                         r:=          b.0x40
08007E28: 1A 88 50                      w move       r.0x20,b.0x40
08007E2B: 44 50                         w test       b.0x40
08007E2D: C6 91                         if >< go     $0xFFFFFFFFFFFFFF91
08007E2F: 44 C4 08 00 79 F8             w test       $0x80079F8
08007E35: C6 09                         if >< go     $0x9
08007E37: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
08007E3D: 9D                            ifkret
08007E3E: 44 4E                         w test       b.0x38
08007E40: C4 12                         if = go      $0x12
08007E42: 18 42                         r:=          b.0x8
08007E44: 1A 45 85                      w move       b.0x14,r.0x14
08007E47: 4A 86                         w stz        r.0x18
08007E49: 4D 87                         w set1       r.0x1C
08007E4B: C3 08 00 AD 6E 00             call         $0x800AD6E,$0x0
08007E51: 9D                            ifkret
08007E52: 44 4D                         w test       b.0x34
08007E54: C4 23                         if = go      $0x23
08007E56: 18 42                         r:=          b.0x8
08007E58: 1A 4B 85                      w move       b.0x2C,r.0x14
08007E5B: FE 79 C4 08 00 79 80 86 03    w bmove      $0x8007980,r.0x18,$0x3
08007E64: C3 08 00 88 D6 00             call         $0x80088D6,$0x0
08007E6A: 9D                            ifkret
08007E6B: 18 42                         r:=          b.0x8
08007E6D: 1A 4B 85                      w move       b.0x2C,r.0x14
08007E70: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007E76: 9D                            ifkret
08007E77: 0D 4D                         w2 :=        b.0x34
08007E79: FE 11                         bi2 inv
08007E7B: 0C D1                         w1 :=        r2
08007E7D: 80                            ret
08007E7E: B8 CF 00 00 00 20             ents         $0x20
08007E84: 18 46                         r:=          b.0x18
08007E86: 44 82                         w test       r.0x8
08007E88: C4 34                         if = go      $0x34
08007E8A: 44 83                         w test       r.0xC
08007E8C: C6 2E                         if >< go     $0x2E
08007E8E: 18 42                         r:=          b.0x8
08007E90: 1A 45 85                      w move       b.0x14,r.0x14
08007E93: 18 46                         r:=          b.0x18
08007E95: 0C 85                         w1 :=        r.0x14
08007E97: 18 42                         r:=          b.0x8
08007E99: 20 86                         w1 =:        r.0x18
08007E9B: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
08007EA1: 9D                            ifkret
08007EA2: 18 42                         r:=          b.0x8
08007EA4: 0D 86                         w2 :=        r.0x18
08007EA6: 18 46                         r:=          b.0x18
08007EA8: 21 85                         w2 =:        r.0x14
08007EAA: 18 42                         r:=          b.0x8
08007EAC: 1A 45 85                      w move       b.0x14,r.0x14
08007EAF: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
08007EB5: 9D                            ifkret
08007EB6: 18 46                         r:=          b.0x18
08007EB8: 4D 83                         w set1       r.0xC
08007EBA: C0 23                         go           $0x23
08007EBC: 1A 86 47                      w move       r.0x18,b.0x1C
08007EBF: 44 47                         w test       b.0x1C
08007EC1: C4 1C                         if = go      $0x1C
08007EC3: 18 42                         r:=          b.0x8
08007EC5: 1A 45 85                      w move       b.0x14,r.0x14
08007EC8: 0C C5 1C                      w1 :=        @b.0x1C
08007ECB: 20 86                         w1 =:        r.0x18
08007ECD: C3 08 00 7E 7E 00             call         $0x8007E7E,$0x0
08007ED3: 9D                            ifkret
08007ED4: 18 47                         r:=          b.0x1C
08007ED6: 1A 81 47                      w move       r.0x4,b.0x1C
08007ED9: 44 47                         w test       b.0x1C
08007EDB: C6 E8                         if >< go     $0xFFFFFFFFFFFFFFE8
08007EDD: 80                            ret
08007EDE: B8 CF 00 00 00 2C             ents         $0x2C
08007EE4: C3 08 00 6A 00 00             call         $0x8006A00,$0x0
08007EEA: 9D                            ifkret
08007EEB: 18 42                         r:=          b.0x8
08007EED: 1A 46 85                      w move       b.0x18,r.0x14
08007EF0: FD 20 47 86 0C                by bmove     b.0x1C,r.0x18,$0xC
08007EF5: C3 08 00 76 01 00             call         $0x8007601,$0x0
08007EFB: 9D                            ifkret
08007EFC: 44 D0                         w test       r1
08007EFE: C4 27                         if = go      $0x27
08007F00: 1A 46 4A                      w move       b.0x18,b.0x28
08007F03: 44 4A                         w test       b.0x28
08007F05: C4 20                         if = go      $0x20
08007F07: 18 4A                         r:=          b.0x28
08007F09: 44 81                         w test       r.0x4
08007F0B: C4 11                         if = go      $0x11
08007F0D: 18 42                         r:=          b.0x8
08007F0F: 1A 45 85                      w move       b.0x14,r.0x14
08007F12: 1A 4A 86                      w move       b.0x28,r.0x18
08007F15: C3 08 00 7E 7E 00             call         $0x8007E7E,$0x0
08007F1B: 9D                            ifkret
08007F1C: 18 4A                         r:=          b.0x28
08007F1E: 1A 88 4A                      w move       r.0x20,b.0x28
08007F21: 44 4A                         w test       b.0x28
08007F23: C6 E4                         if >< go     $0xFFFFFFFFFFFFFFE4
08007F25: 80                            ret
08007F26: B8 CF 00 00 00 64             ents         $0x64
08007F2C: 1A 46 51                      w move       b.0x18,b.0x44
08007F2F: 4A 4C                         w stz        b.0x30
08007F31: 1A 3F 4D                      w move       $0x3F,b.0x34
08007F34: FD 3D C5 44                   w2 laddr     @b.0x44
08007F38: 55 4C                         w2 +         b.0x30
08007F3A: 04 F5 00                      by1 :=       r2.(0x0)
08007F3D: 1C 4B                         by1 =:       b.0x2C
08007F3F: 30 0D                         by1 comp     $0xD
08007F41: C4 1E                         if = go      $0x1E
08007F43: 0E 4D                         w3 :=        b.0x34
08007F45: 56 01                         w3 +         $0x1
08007F47: 22 4D                         w3 =:        b.0x34
08007F49: 1C E6 1C                      by1 =:       @b.0x1C+
08007F4C: 0F 4C                         w4 :=        b.0x30
08007F4E: 57 01                         w4 +         $0x1
08007F50: 23 4C                         w4 =:        b.0x30
08007F52: 37 13                         w4 comp      $0x13
08007F54: CE 09                         if <= go     $0x9
08007F56: 18 51                         r:=          b.0x44
08007F58: 1A 85 51                      w move       r.0x14,b.0x44
08007F5B: 4A 4C                         w stz        b.0x30
08007F5D: C0 D7                         go           $0xFFFFFFFFFFFFFFD7
08007F5F: 1A 45 51                      w move       b.0x14,b.0x44
08007F62: 4A 4C                         w stz        b.0x30
08007F64: 1A 4D 4E                      w move       b.0x34,b.0x38
08007F67: FD 3D C5 44                   w2 laddr     @b.0x44
08007F6B: 55 4C                         w2 +         b.0x30
08007F6D: 04 F5 00                      by1 :=       r2.(0x0)
08007F70: 1C 4B                         by1 =:       b.0x2C
08007F72: 30 0D                         by1 comp     $0xD
08007F74: C4 2D                         if = go      $0x2D
08007F76: 30 CD 29                      by1 comp     $0x29
08007F79: C4 07                         if = go      $0x7
08007F7B: 30 CD 2E                      by1 comp     $0x2E
08007F7E: C6 07                         if >< go     $0x7
08007F80: 1A 4E 4D                      w move       b.0x38,b.0x34
08007F83: C0 0B                         go           $0xB
08007F85: 0E 4D                         w3 :=        b.0x34
08007F87: 56 01                         w3 +         $0x1
08007F89: 22 4D                         w3 =:        b.0x34
08007F8B: 1C E6 1C                      by1 =:       @b.0x1C+
08007F8E: 0C 4C                         w1 :=        b.0x30
08007F90: 54 01                         w1 +         $0x1
08007F92: 20 4C                         w1 =:        b.0x30
08007F94: 34 13                         w1 comp      $0x13
08007F96: CE 09                         if <= go     $0x9
08007F98: 18 51                         r:=          b.0x44
08007F9A: 1A 85 51                      w move       r.0x14,b.0x44
08007F9D: 4A 4C                         w stz        b.0x30
08007F9F: C0 C8                         go           $0xFFFFFFFFFFFFFFC8
08007FA1: 0E 4D                         w3 :=        b.0x34
08007FA3: 56 01                         w3 +         $0x1
08007FA5: 22 4D                         w3 =:        b.0x34
08007FA7: 07 0D                         by4 :=       $0xD
08007FA9: 1F E6 1C                      by4 =:       @b.0x1C+
08007FAC: 4A 4A                         w stz        b.0x28
08007FAE: 18 42                         r:=          b.0x8
08007FB0: 4D 85                         w set1       r.0x14
08007FB2: 62 01                         w3 -         $0x1
08007FB4: 1A D2 54                      w move       r3,b.0x50
08007FB7: 4A 53                         w stz        b.0x4C
08007FB9: 0C 47                         w1 :=        b.0x1C
08007FBB: 20 52                         w1 =:        b.0x48
08007FBD: FD 20 52 86 0C                by bmove     b.0x48,r.0x18,$0xC
08007FC2: 4A 89                         w stz        r.0x24
08007FC4: C3 08 00 76 AA 00             call         $0x80076AA,$0x0
08007FCA: 9D                            ifkret
08007FCB: 18 42                         r:=          b.0x8
08007FCD: 1A 89 4A                      w move       r.0x24,b.0x28
08007FD0: 20 4F                         w1 =:        b.0x3C
08007FD2: 34 CD 2E                      w1 comp      $0x2E
08007FD5: C6 2D                         if >< go     $0x2D
08007FD7: 4A 50                         w stz        b.0x40
08007FD9: 1A CE 00 91 55                w move       $0x91,b.0x54
08007FDE: 0D 4D                         w2 :=        b.0x34
08007FE0: 61 01                         w2 -         $0x1
08007FE2: 1A D1 58                      w move       r2,b.0x60
08007FE5: 4A 57                         w stz        b.0x5C
08007FE7: 0E 47                         w3 :=        b.0x1C
08007FE9: 22 56                         w3 =:        b.0x58
08007FEB: FD 20 56 52 0C                by bmove     b.0x58,b.0x48,$0xC
08007FF0: C3 08 00 B9 7C 06 55 C5 48 C5 4C C5 50 50 50 call         $0x800B97C,$0x6,b.0x54,@b.0x48,@b.0x4C,@b.0x50,b.0x40,b.0x40
08007FFF: 9D                            ifkret
08008000: C0 07                         go           $0x7
08008002: 44 D0                         w test       r1
08008004: C4 03                         if = go      $0x3
08008006: 81                            retk
08008007: 80                            ret
08008008: B8 CF 00 00 01 10             ents         $0x110
0800800E: 84                            bi1 clr
0800800F: 20 47                         w1 =:        b.0x1C
08008011: 20 48                         w1 =:        b.0x20
08008013: 0D 3F                         w2 :=        $0x3F
08008015: 21 49                         w2 =:        b.0x24
08008017: 21 4A                         w2 =:        b.0x28
08008019: 18 42                         r:=          b.0x8
0800801B: 1A 45 85                      w move       b.0x14,r.0x14
0800801E: FD 3E 4F                      w3 laddr     b.0x3C
08008021: 22 86                         w3 =:        r.0x18
08008023: 4A 87                         w stz        r.0x1C
08008025: 1A CD 63 88                   w move       $0x63,r.0x20
08008029: C3 08 00 88 5B 00             call         $0x800885B,$0x0
0800802F: 9D                            ifkret
08008030: C0 69                         go           $0x69
08008032: 9C                            entd
08008033: FD C0 C2 01 04                l=:          b.0x104
08008038: 20 43                         w1 =:        b.0xC
0800803A: 2E 49 3F                      w comp2      b.0x24,$0x3F
0800803D: C4 0E                         if = go      $0xE
0800803F: 18 42                         r:=          b.0x8
08008041: 1A 49 85                      w move       b.0x24,r.0x14
08008044: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
0800804A: 9D                            ifkret
0800804B: 2E 4A 3F                      w comp2      b.0x28,$0x3F
0800804E: C4 0E                         if = go      $0xE
08008050: 18 42                         r:=          b.0x8
08008052: 1A 4A 85                      w move       b.0x28,r.0x14
08008055: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
0800805B: 9D                            ifkret
0800805C: 44 47                         w test       b.0x1C
0800805E: C4 1B                         if = go      $0x1B
08008060: FD 3D 4F                      w2 laddr     b.0x3C
08008063: 18 42                         r:=          b.0x8
08008065: 21 85                         w2 =:        r.0x14
08008067: 4A 86                         w stz        r.0x18
08008069: 1A CD 63 87                   w move       $0x63,r.0x1C
0800806D: 1A 43 88                      w move       b.0xC,r.0x20
08008070: C3 08 00 68 D0 00             call         $0x80068D0,$0x0
08008076: 9D                            ifkret
08008077: C0 19                         go           $0x19
08008079: FD 3D 68                      w2 laddr     b.0xA0
0800807C: 18 42                         r:=          b.0x8
0800807E: 21 85                         w2 =:        r.0x14
08008080: 4A 86                         w stz        r.0x18
08008082: 1A CD 63 87                   w move       $0x63,r.0x1C
08008086: 1A 43 88                      w move       b.0xC,r.0x20
08008089: C3 08 00 68 D0 00             call         $0x80068D0,$0x0
0800808F: 9D                            ifkret
08008090: 0C 43                         w1 :=        b.0xC
08008092: 81                            retk
08008093: FE 03                         clrk
08008095: B4 C2 01 04                   jumpg        b.0x104
08008099: 4D 47                         w set1       b.0x1C
0800809B: 18 42                         r:=          b.0x8
0800809D: FE 79 C4 08 00 79 90 86 03    w bmove      $0x8007990,r.0x18,$0x3
080080A6: FD 3D 4F                      w2 laddr     b.0x3C
080080A9: 21 89                         w2 =:        r.0x24
080080AB: 4A 8A                         w stz        r.0x28
080080AD: 1A CD 63 8B                   w move       $0x63,r.0x2C
080080B1: FE 79 C4 08 00 79 9C 8C 03    w bmove      $0x800799C,r.0x30,$0x3
080080BA: C3 08 00 C0 B2 00             call         $0x800C0B2,$0x0
080080C0: D2 08                         if -k go     $0x8
080080C2: C3 08 00 80 32 00             call         $0x8008032,$0x0
080080C8: 18 42                         r:=          b.0x8
080080CA: 1A 85 49                      w move       r.0x14,b.0x24
080080CD: 1A 49 85                      w move       b.0x24,r.0x14
080080D0: 0C CE 08 00                   w1 :=        $0x800
080080D4: C3 08 00 C0 99 00             call         $0x800C099,$0x0
080080DA: D2 08                         if -k go     $0x8
080080DC: C3 08 00 80 32 00             call         $0x8008032,$0x0
080080E2: 1A CD 21 C2 01 08             w move       $0x21,b.0x108
080080E8: 86                            bi3 clr
080080E9: FE 29 E2 08 00 6E CA          h2 laddr     $0x8006ECA+
080080F0: 21 C2 01 0C                   w2 =:        b.0x10C
080080F4: C3 08 00 B9 7C 03 C2 01 08 49 C6 01 0C          call         $0x800B97C,$0x3,b.0x108,b.0x24,@b.0x10C
08008101: D2 08                         if -k go     $0x8
08008103: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008109: 18 CF 08 00 6E CA             r:=          $0x8006ECA
0800810F: 0D 8C                         w2 :=        r.0x30
08008111: 21 4B                         w2 =:        b.0x2C
08008113: 44 D1                         w test       r2
08008115: C6 04                         if >< go     $0x4
08008117: 4A 4B                         w stz        b.0x2C
08008119: 1A 8D 4C                      w move       r.0x34,b.0x30
0800811C: 4A 47                         w stz        b.0x1C
0800811E: 18 42                         r:=          b.0x8
08008120: 1A 45 85                      w move       b.0x14,r.0x14
08008123: 1A 46 86                      w move       b.0x18,r.0x18
08008126: FD 3D 68                      w2 laddr     b.0xA0
08008129: 21 87                         w2 =:        r.0x1C
0800812B: 4A 88                         w stz        r.0x20
0800812D: 1A CD 63 89                   w move       $0x63,r.0x24
08008131: 1A 4E 8A                      w move       b.0x38,r.0x28
08008134: C3 08 00 7F 26 00             call         $0x8007F26,$0x0
0800813A: D2 08                         if -k go     $0x8
0800813C: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008142: 18 42                         r:=          b.0x8
08008144: 1A 8A 4E                      w move       r.0x28,b.0x38
08008147: 19 CD B5 85                   by move      $0xB5,r.0x14
0800814B: 04 CD 8C                      by1 :=       $0x8C
0800814E: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08008154: D2 08                         if -k go     $0x8
08008156: C3 08 00 80 32 00             call         $0x8008032,$0x0
0800815C: 44 D0                         w test       r1
0800815E: C6 08                         if >< go     $0x8
08008160: 2E 4E 4B                      w comp2      b.0x38,b.0x2C
08008163: D7 01 07                      if >>= go    $0x107
08008166: 4D 48                         w set1       b.0x20
08008168: 18 42                         r:=          b.0x8
0800816A: FE 79 C4 08 00 79 AC 86 03    w bmove      $0x80079AC,r.0x18,$0x3
08008173: FD 3C 68                      w1 laddr     b.0xA0
08008176: 20 89                         w1 =:        r.0x24
08008178: 4A 8A                         w stz        r.0x28
0800817A: 1A CD 63 8B                   w move       $0x63,r.0x2C
0800817E: FE 79 C4 08 00 79 B8 8C 03    w bmove      $0x80079B8,r.0x30,$0x3
08008187: C3 08 00 C0 B2 00             call         $0x800C0B2,$0x0
0800818D: D2 08                         if -k go     $0x8
0800818F: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008195: 18 42                         r:=          b.0x8
08008197: 1A 85 4A                      w move       r.0x14,b.0x28
0800819A: 1A 4A 85                      w move       b.0x28,r.0x14
0800819D: 0C CE 08 00                   w1 :=        $0x800
080081A1: C3 08 00 C0 99 00             call         $0x800C099,$0x0
080081A7: D2 08                         if -k go     $0x8
080081A9: C3 08 00 80 32 00             call         $0x8008032,$0x0
080081AF: 4A 48                         w stz        b.0x20
080081B1: 44 C4 08 00 79 F8             w test       $0x80079F8
080081B7: C6 1E                         if >< go     $0x1E
080081B9: 18 42                         r:=          b.0x8
080081BB: FE 79 C4 08 00 79 CC 85 03    w bmove      $0x80079CC,r.0x14,$0x3
080081C4: 1A 45 88                      w move       b.0x14,r.0x20
080081C7: C3 08 00 69 5B 00             call         $0x800695B,$0x0
080081CD: D2 08                         if -k go     $0x8
080081CF: C3 08 00 80 32 00             call         $0x8008032,$0x0
080081D5: 4D 4D                         w set1       b.0x34
080081D7: 2E 4D 4C                      w comp2      b.0x34,b.0x30
080081DA: D4 55                         if >> go     $0x55
080081DC: 4D 47                         w set1       b.0x1C
080081DE: 18 42                         r:=          b.0x8
080081E0: 1A 49 85                      w move       b.0x24,r.0x14
080081E3: 0C 4D                         w1 :=        b.0x34
080081E5: 60 01                         w1 -         $0x1
080081E7: 20 86                         w1 =:        r.0x18
080081E9: FE 79 C4 08 00 79 D8 87 03    w bmove      $0x80079D8,r.0x1C,$0x3
080081F2: C3 08 00 C3 2D 00             call         $0x800C32D,$0x0
080081F8: D2 08                         if -k go     $0x8
080081FA: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008200: 4A 47                         w stz        b.0x1C
08008202: 4D 48                         w set1       b.0x20
08008204: 18 42                         r:=          b.0x8
08008206: 1A 4A 85                      w move       b.0x28,r.0x14
08008209: 0D 4D                         w2 :=        b.0x34
0800820B: 61 01                         w2 -         $0x1
0800820D: 21 86                         w2 =:        r.0x18
0800820F: FE 79 C4 08 00 79 E4 87 03    w bmove      $0x80079E4,r.0x1C,$0x3
08008218: C3 08 00 C3 81 00             call         $0x800C381,$0x0
0800821E: D2 08                         if -k go     $0x8
08008220: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008226: 4A 48                         w stz        b.0x20
08008228: 4F 4D                         w incr       b.0x34
0800822A: 2E 4D 4C                      w comp2      b.0x34,b.0x30
0800822D: DA AF                         if <<= go    $0xFFFFFFFFFFFFFFAF
0800822F: 18 42                         r:=          b.0x8
08008231: 1A 49 85                      w move       b.0x24,r.0x14
08008234: C3 08 00 C3 1A 00             call         $0x800C31A,$0x0
0800823A: D2 08                         if -k go     $0x8
0800823C: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008242: 60 01                         w1 -         $0x1
08008244: 18 42                         r:=          b.0x8
08008246: 1A 4A 85                      w move       b.0x28,r.0x14
08008249: C3 08 00 C3 05 00             call         $0x800C305,$0x0
0800824F: D2 08                         if -k go     $0x8
08008251: C3 08 00 80 32 00             call         $0x8008032,$0x0
08008257: 18 42                         r:=          b.0x8
08008259: 1A 4A 85                      w move       b.0x28,r.0x14
0800825C: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
08008262: D2 08                         if -k go     $0x8
08008264: C3 08 00 80 32 00             call         $0x8008032,$0x0
0800826A: 18 42                         r:=          b.0x8
0800826C: 1A 49 85                      w move       b.0x24,r.0x14
0800826F: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
08008275: D2 08                         if -k go     $0x8
08008277: C3 08 00 80 32 00             call         $0x8008032,$0x0
0800827D: 80                            ret
0800827E: B8 CF 00 00 00 20             ents         $0x20
08008284: 18 45                         r:=          b.0x14
08008286: 44 82                         w test       r.0x8
08008288: C4 1C                         if = go      $0x1C
0800828A: 44 83                         w test       r.0xC
0800828C: C6 16                         if >< go     $0x16
0800828E: 0C 85                         w1 :=        r.0x14
08008290: 18 42                         r:=          b.0x8
08008292: 20 85                         w1 =:        r.0x14
08008294: 1A 46 86                      w move       b.0x18,r.0x18
08008297: C3 08 00 80 08 00             call         $0x8008008,$0x0
0800829D: 9D                            ifkret
0800829E: 18 45                         r:=          b.0x14
080082A0: 4D 83                         w set1       r.0xC
080082A2: C0 23                         go           $0x23
080082A4: 1A 86 47                      w move       r.0x18,b.0x1C
080082A7: 44 47                         w test       b.0x1C
080082A9: C4 1C                         if = go      $0x1C
080082AB: 0C C5 1C                      w1 :=        @b.0x1C
080082AE: 18 42                         r:=          b.0x8
080082B0: 20 85                         w1 =:        r.0x14
080082B2: 1A 46 86                      w move       b.0x18,r.0x18
080082B5: C3 08 00 82 7E 00             call         $0x800827E,$0x0
080082BB: 9D                            ifkret
080082BC: 18 47                         r:=          b.0x1C
080082BE: 1A 81 47                      w move       r.0x4,b.0x1C
080082C1: 44 47                         w test       b.0x1C
080082C3: C6 E8                         if >< go     $0xFFFFFFFFFFFFFFE8
080082C5: 80                            ret
080082C6: B8 CF 00 00 00 38             ents         $0x38
080082CC: C3 08 00 6A 00 00             call         $0x8006A00,$0x0
080082D2: 9D                            ifkret
080082D3: 18 42                         r:=          b.0x8
080082D5: 1A 45 85                      w move       b.0x14,r.0x14
080082D8: FD 20 46 86 0C                by bmove     b.0x18,r.0x18,$0xC
080082DD: C3 08 00 76 01 00             call         $0x8007601,$0x0
080082E3: 9D                            ifkret
080082E4: 44 D0                         w test       r1
080082E6: C4 5A                         if = go      $0x5A
080082E8: 18 42                         r:=          b.0x8
080082EA: FD 20 49 85 0C                by bmove     b.0x24,r.0x14,$0xC
080082EF: C3 08 00 87 DD 00             call         $0x80087DD,$0x0
080082F5: 9D                            ifkret
080082F6: 18 42                         r:=          b.0x8
080082F8: 1A 88 4D                      w move       r.0x20,b.0x34
080082FB: 1A 45 4C                      w move       b.0x14,b.0x30
080082FE: 44 4C                         w test       b.0x30
08008300: C4 20                         if = go      $0x20
08008302: 18 4C                         r:=          b.0x30
08008304: 44 81                         w test       r.0x4
08008306: C4 11                         if = go      $0x11
08008308: 18 42                         r:=          b.0x8
0800830A: 1A 4C 85                      w move       b.0x30,r.0x14
0800830D: 1A 4D 86                      w move       b.0x34,r.0x18
08008310: C3 08 00 82 7E 00             call         $0x800827E,$0x0
08008316: 9D                            ifkret
08008317: 18 4C                         r:=          b.0x30
08008319: 1A 88 4C                      w move       r.0x20,b.0x30
0800831C: 44 4C                         w test       b.0x30
0800831E: C6 E4                         if >< go     $0xFFFFFFFFFFFFFFE4
08008320: 44 C4 08 00 79 F8             w test       $0x80079F8
08008326: C6 09                         if >< go     $0x9
08008328: C3 08 00 8F 5C 00             call         $0x8008F5C,$0x0
0800832E: 9D                            ifkret
0800832F: 18 42                         r:=          b.0x8
08008331: 1A 4D 85                      w move       b.0x34,r.0x14
08008334: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800833A: 9D                            ifkret
0800833B: 18 42                         r:=          b.0x8
0800833D: 1A 85 4D                      w move       r.0x14,b.0x34
08008340: 80                            ret
08008341: B8 CF 00 00 00 1C             ents         $0x1C
08008347: 1A CD 4B 45                   w move       $0x4B,b.0x14
0800834B: 85                            bi2 clr
0800834C: FD 3C E1 08 00 7A 84          w1 laddr     $0x8007A84+
08008353: 20 46                         w1 =:        b.0x18
08008355: C3 08 00 B9 7C 02 45 C5 18    call         $0x800B97C,$0x2,b.0x14,@b.0x18
0800835E: 9D                            ifkret
0800835F: 0E 06                         w3 :=        $0x6
08008361: 0D E2 08 00 7A 84             w2 :=        $0x8007A84+
08008367: 61 CE 07 6C                   w2 -         $0x76C
0800836B: 0F 06                         w4 :=        $0x6
0800836D: 21 E3 08 00 7A 84             w2 =:        $0x8007A84+
08008373: 80                            ret
08008374: B8 CF 00 00 00 20             ents         $0x20
0800837A: 0C 06                         w1 :=        $0x6
0800837C: 1A E0 08 00 7A 84 45          w move       $0x8007A84+,b.0x14
08008383: 0D 05                         w2 :=        $0x5
08008385: 1A E1 08 00 7A 84 46          w move       $0x8007A84+,b.0x18
0800838C: 0E 04                         w3 :=        $0x4
0800838E: 1A E2 08 00 7A 84 47          w move       $0x8007A84+,b.0x1C
08008395: 80                            ret
08008396: B8 CF 00 00 00 20             ents         $0x20
0800839C: 0C 03                         w1 :=        $0x3
0800839E: 1A E0 08 00 7A 84 45          w move       $0x8007A84+,b.0x14
080083A5: 0D 02                         w2 :=        $0x2
080083A7: 1A E1 08 00 7A 84 46          w move       $0x8007A84+,b.0x18
080083AE: 0E 01                         w3 :=        $0x1
080083B0: 1A E2 08 00 7A 84 47          w move       $0x8007A84+,b.0x1C
080083B7: 80                            ret
080083B8: B8 CF 00 00 00 18             ents         $0x18
080083BE: 20 45                         w1 =:        b.0x14
080083C0: B4 E0 08 00 83 00             jumpg        $0x8008300+
080083C6: 0C 01                         w1 :=        $0x1
080083C8: 80                            ret
080083C9: C0 20                         go           $0x20
080083CB: 0C 02                         w1 :=        $0x2
080083CD: 80                            ret
080083CE: C0 1B                         go           $0x1B
080083D0: 0C 03                         w1 :=        $0x3
080083D2: 80                            ret
080083D3: C0 16                         go           $0x16
080083D5: 0C 04                         w1 :=        $0x4
080083D7: 80                            ret
080083D8: C0 11                         go           $0x11
080083DA: 0C 05                         w1 :=        $0x5
080083DC: 80                            ret
080083DD: C0 0C                         go           $0xC
080083DF: 0C 06                         w1 :=        $0x6
080083E1: 80                            ret
080083E2: C0 07                         go           $0x7
080083E4: 0C 07                         w1 :=        $0x7
080083E6: 80                            ret
080083E7: C0 02                         go           $0x2
080083E9: B8 CF 00 00 00 28             ents         $0x28
080083EF: 0C 46                         w1 :=        b.0x18
080083F1: 20 48                         w1 =:        b.0x20
080083F3: 0D 47                         w2 :=        b.0x1C
080083F5: 21 49                         w2 =:        b.0x24
080083F7: 34 D1                         w1 comp      r2
080083F9: C8 17                         if > go      $0x17
080083FB: 04 CD 20                      by1 :=       $0x20
080083FE: 0D 48                         w2 :=        b.0x20
08008400: 2D E5 14 D0                   by comp2     @b.0x14+,r1
08008404: C6 08                         if >< go     $0x8
08008406: 06 0D                         by3 :=       $0xD
08008408: 1E E5 14                      by3 =:       @b.0x14+
0800840B: 80                            ret
0800840C: BF 48 49 EF                   d loopi      b.0x20,b.0x24,$0xFFFFFFFFFFFFFFEF
08008410: 0C 47                         w1 :=        b.0x1C
08008412: 05 0D                         by2 :=       $0xD
08008414: 1D E4 14                      by2 =:       @b.0x14+
08008417: 80                            ret
08008418: B8 CF 00 00 00 C8             ents         $0xC8
0800841E: 4A 4B                         w stz        b.0x2C
08008420: 4D 4F                         w set1       b.0x3C
08008422: 1A 3F 50                      w move       $0x3F,b.0x40
08008425: 0C 46                         w1 :=        b.0x18
08008427: 20 4D                         w1 =:        b.0x34
08008429: 0D 47                         w2 :=        b.0x1C
0800842B: 21 6A                         w2 =:        b.0xA8
0800842D: 34 D1                         w1 comp      r2
0800842F: C9 01 22                      if > go      $0x122
08008432: 0D 4D                         w2 :=        b.0x34
08008434: 04 E5 14                      by1 :=       @b.0x14+
08008437: 1C 4C                         by1 =:       b.0x30
08008439: 30 CD 41                      by1 comp     $0x41
0800843C: D8 07                         if << go     $0x7
0800843E: 30 CD 5A                      by1 comp     $0x5A
08008441: DA 30                         if <<= go    $0x30
08008443: 30 CD 2D                      by1 comp     $0x2D
08008446: C4 2B                         if = go      $0x2B
08008448: 30 CD 30                      by1 comp     $0x30
0800844B: D8 07                         if << go     $0x7
0800844D: 30 CD 39                      by1 comp     $0x39
08008450: DA 21                         if <<= go    $0x21
08008452: 30 CD 28                      by1 comp     $0x28
08008455: C4 1C                         if = go      $0x1C
08008457: 30 CD 29                      by1 comp     $0x29
0800845A: C4 17                         if = go      $0x17
0800845C: 30 CD 3A                      by1 comp     $0x3A
0800845F: C4 12                         if = go      $0x12
08008461: 30 CD 3B                      by1 comp     $0x3B
08008464: C4 0D                         if = go      $0xD
08008466: 30 CD 2E                      by1 comp     $0x2E
08008469: C4 08                         if = go      $0x8
0800846B: 30 CD 22                      by1 comp     $0x22
0800846E: C7 00 E3                      if >< go     $0xE3
08008471: 2E 4F 01                      w comp2      b.0x3C,$0x1
08008474: C6 6E                         if >< go     $0x6E
08008476: 30 CD 22                      by1 comp     $0x22
08008479: C6 0A                         if >< go     $0xA
0800847B: 44 4B                         w test       b.0x2C
0800847D: C6 06                         if >< go     $0x6
0800847F: 4D 4B                         w set1       b.0x2C
08008481: C0 55                         go           $0x55
08008483: 30 CD 28                      by1 comp     $0x28
08008486: C6 07                         if >< go     $0x7
08008488: 1A 02 4F                      w move       $0x2,b.0x3C
0800848B: C0 4B                         go           $0x4B
0800848D: 30 CD 3A                      by1 comp     $0x3A
08008490: C6 07                         if >< go     $0x7
08008492: 1A 03 4F                      w move       $0x3,b.0x3C
08008495: C0 41                         go           $0x41
08008497: 30 CD 3B                      by1 comp     $0x3B
0800849A: C4 07                         if = go      $0x7
0800849C: 30 CD 22                      by1 comp     $0x22
0800849F: C6 37                         if >< go     $0x37
080084A1: 0E 50                         w3 :=        b.0x40
080084A3: 56 01                         w3 +         $0x1
080084A5: 22 50                         w3 =:        b.0x40
080084A7: 19 CD 3A D6 44                by move      $0x3A,b.0x44+
080084AC: 0F 49                         w4 :=        b.0x24
080084AE: 23 4E                         w4 =:        b.0x38
080084B0: 0C 4A                         w1 :=        b.0x28
080084B2: 20 6B                         w1 =:        b.0xAC
080084B4: 37 D0                         w4 comp      r1
080084B6: C8 1D                         if > go      $0x1D
080084B8: 04 CD 20                      by1 :=       $0x20
080084BB: 0D 4E                         w2 :=        b.0x38
080084BD: 2D E5 20 D0                   by comp2     @b.0x20+,r1
080084C1: C4 12                         if = go      $0x12
080084C3: 0E 50                         w3 :=        b.0x40
080084C5: 56 01                         w3 +         $0x1
080084C7: 22 50                         w3 =:        b.0x40
080084C9: 07 E5 20                      by4 :=       @b.0x20+
080084CC: 1F D6 44                      by4 =:       b.0x44+
080084CF: BF 4E 6B E9                   d loopi      b.0x38,b.0xAC,$0xFFFFFFFFFFFFFFE9
080084D3: 1A 04 4F                      w move       $0x4,b.0x3C
080084D6: 0C 50                         w1 :=        b.0x40
080084D8: 54 01                         w1 +         $0x1
080084DA: 20 50                         w1 =:        b.0x40
080084DC: 19 4C D4 44                   by move      b.0x30,b.0x44+
080084E0: C0 6C                         go           $0x6C
080084E2: 2E 4F 02                      w comp2      b.0x3C,$0x2
080084E5: C6 14                         if >< go     $0x14
080084E7: 30 CD 29                      by1 comp     $0x29
080084EA: C6 04                         if >< go     $0x4
080084EC: 4D 4F                         w set1       b.0x3C
080084EE: 0E 50                         w3 :=        b.0x40
080084F0: 56 01                         w3 +         $0x1
080084F2: 22 50                         w3 =:        b.0x40
080084F4: 1C D6 44                      by1 =:       b.0x44+
080084F7: C0 55                         go           $0x55
080084F9: 2E 4F 03                      w comp2      b.0x3C,$0x3
080084FC: C6 42                         if >< go     $0x42
080084FE: 30 CD 3B                      by1 comp     $0x3B
08008501: C4 07                         if = go      $0x7
08008503: 30 CD 22                      by1 comp     $0x22
08008506: C6 29                         if >< go     $0x29
08008508: 0E 49                         w3 :=        b.0x24
0800850A: 22 4E                         w3 =:        b.0x38
0800850C: 0F 4A                         w4 :=        b.0x28
0800850E: 23 6C                         w4 =:        b.0xB0
08008510: 36 D3                         w3 comp      r4
08008512: C8 1D                         if > go      $0x1D
08008514: 04 CD 20                      by1 :=       $0x20
08008517: 0D 4E                         w2 :=        b.0x38
08008519: 2D E5 20 D0                   by comp2     @b.0x20+,r1
0800851D: C4 12                         if = go      $0x12
0800851F: 0E 50                         w3 :=        b.0x40
08008521: 56 01                         w3 +         $0x1
08008523: 22 50                         w3 =:        b.0x40
08008525: 07 E5 20                      by4 :=       @b.0x20+
08008528: 1F D6 44                      by4 =:       b.0x44+
0800852B: BF 4E 6C E9                   d loopi      b.0x38,b.0xB0,$0xFFFFFFFFFFFFFFE9
0800852F: 1A 04 4F                      w move       $0x4,b.0x3C
08008532: 0C 50                         w1 :=        b.0x40
08008534: 54 01                         w1 +         $0x1
08008536: 20 50                         w1 =:        b.0x40
08008538: 19 4C D4 44                   by move      b.0x30,b.0x44+
0800853C: C0 10                         go           $0x10
0800853E: 2E 4F 04                      w comp2      b.0x3C,$0x4
08008541: C6 0B                         if >< go     $0xB
08008543: 0E 50                         w3 :=        b.0x40
08008545: 56 01                         w3 +         $0x1
08008547: 22 50                         w3 =:        b.0x40
08008549: 1C D6 44                      by1 =:       b.0x44+
0800854C: E1 4D 6A FE                   loopi        b.0x34,b.0xA8,$0xFFFFFFFFFFFFFFFE
08008550: E6 2E                         w3 and       $0x2E
08008552: 4F 01                         w incr       $0x1
08008554: C4 07                         if = go      $0x7
08008556: 2E 4F 03                      w comp2      b.0x3C,$0x3
08008559: C6 39                         if >< go     $0x39
0800855B: 2E 4F 01                      w comp2      b.0x3C,$0x1
0800855E: C6 0D                         if >< go     $0xD
08008560: 0C 50                         w1 :=        b.0x40
08008562: 54 01                         w1 +         $0x1
08008564: 20 50                         w1 =:        b.0x40
08008566: 19 CD 3A D4 44                by move      $0x3A,b.0x44+
0800856B: 0C 49                         w1 :=        b.0x24
0800856D: 20 4E                         w1 =:        b.0x38
0800856F: 0D 4A                         w2 :=        b.0x28
08008571: 21 6D                         w2 =:        b.0xB4
08008573: 34 D1                         w1 comp      r2
08008575: C8 1D                         if > go      $0x1D
08008577: 04 CD 20                      by1 :=       $0x20
0800857A: 0D 4E                         w2 :=        b.0x38
0800857C: 2D E5 20 D0                   by comp2     @b.0x20+,r1
08008580: C4 12                         if = go      $0x12
08008582: 0E 50                         w3 :=        b.0x40
08008584: 56 01                         w3 +         $0x1
08008586: 22 50                         w3 =:        b.0x40
08008588: 07 E5 20                      by4 :=       @b.0x20+
0800858B: 1F D6 44                      by4 =:       b.0x44+
0800858E: BF 4E 6D E9                   d loopi      b.0x38,b.0xB4,$0xFFFFFFFFFFFFFFE9
08008592: 0C 50                         w1 :=        b.0x40
08008594: 54 01                         w1 +         $0x1
08008596: 20 50                         w1 =:        b.0x40
08008598: 19 0D D4 44                   by move      $0xD,b.0x44+
0800859C: FD 3D 51                      w2 laddr     b.0x44
0800859F: 21 6F                         w2 =:        b.0xBC
080085A1: 1A CD 64 6E                   w move       $0x64,b.0xB8
080085A5: FC 6E 47 46 70                w sub3       b.0x1C,b.0x18,b.0xC0
080085AA: FC 69 46 45 71                w add3       b.0x18,b.0x14,b.0xC4
080085AF: 4F 70                         w incr       b.0xC0
080085B1: CA 08                         if < go      $0x8
080085B3: 84                            bi1 clr
080085B4: 85                            bi2 clr
080085B5: FD 67 6E 70                   by smove     b.0xB8,b.0xC0
080085B9: 80                            ret
080085BA: B8 CF 00 00 00 2C             ents         $0x2C
080085C0: 0C 46                         w1 :=        b.0x18
080085C2: 20 48                         w1 =:        b.0x20
080085C4: 0D 47                         w2 :=        b.0x1C
080085C6: 21 49                         w2 =:        b.0x24
080085C8: 34 D1                         w1 comp      r2
080085CA: C8 28                         if > go      $0x28
080085CC: 0D 48                         w2 :=        b.0x20
080085CE: 04 E5 14                      by1 :=       @b.0x14+
080085D1: 1C 4A                         by1 =:       b.0x28
080085D3: 30 CD 61                      by1 comp     $0x61
080085D6: D8 13                         if << go     $0x13
080085D8: 30 CD 7D                      by1 comp     $0x7D
080085DB: D4 0E                         if >> go     $0xE
080085DD: FC 3C CD 61                   by1 -        $0x61
080085E1: FC 34 CD 41                   by1 +        $0x41
080085E5: C0 06                         go           $0x6
080085E7: C0 04                         go           $0x4
080085E9: C0 02                         go           $0x2
080085EB: 1C E5 14                      by1 =:       @b.0x14+
080085EE: BF 48 49 DE                   d loopi      b.0x20,b.0x24,$0xFFFFFFFFFFFFFFDE
080085F2: 80                            ret
080085F3: B8 CF 00 00 00 34             ents         $0x34
080085F9: 0C 49                         w1 :=        b.0x24
080085FB: 20 4B                         w1 =:        b.0x2C
080085FD: 0D 46                         w2 :=        b.0x18
080085FF: 21 4C                         w2 =:        b.0x30
08008601: 0D 4C                         w2 :=        b.0x30
08008603: 04 E5 14                      by1 :=       @b.0x14+
08008606: 0E 4B                         w3 :=        b.0x2C
08008608: 2D E6 20 D0                   by comp2     @b.0x20+,r1
0800860C: C4 04                         if = go      $0x4
0800860E: 84                            bi1 clr
0800860F: 80                            ret
08008610: 07 0D                         by4 :=       $0xD
08008612: 2D E6 20 D3                   by comp2     @b.0x20+,r4
08008616: C4 08                         if = go      $0x8
08008618: 4F 4B                         w incr       b.0x2C
0800861A: 4F 4C                         w incr       b.0x30
0800861C: C0 E5                         go           $0xFFFFFFFFFFFFFFE5
0800861E: 0C 01                         w1 :=        $0x1
08008620: 80                            ret
08008621: B8 CF 00 00 00 38             ents         $0x38
08008627: 0C 49                         w1 :=        b.0x24
08008629: 20 4B                         w1 =:        b.0x2C
0800862B: 0D 46                         w2 :=        b.0x18
0800862D: 21 4C                         w2 =:        b.0x30
0800862F: 0D 4B                         w2 :=        b.0x2C
08008631: 04 E5 20                      by1 :=       @b.0x20+
08008634: 1C 4D                         by1 =:       b.0x34
08008636: 30 CD 20                      by1 comp     $0x20
08008639: C4 0B                         if = go      $0xB
0800863B: 30 CD 2C                      by1 comp     $0x2C
0800863E: C4 06                         if = go      $0x6
08008640: 30 0D                         by1 comp     $0xD
08008642: C6 07                         if >< go     $0x7
08008644: 0C 01                         w1 :=        $0x1
08008646: 80                            ret
08008647: C0 41                         go           $0x41
08008649: 06 0D                         by3 :=       $0xD
0800864B: 0F 4C                         w4 :=        b.0x30
0800864D: 2D E7 14 D2                   by comp2     @b.0x14+,r3
08008651: C6 06                         if >< go     $0x6
08008653: 84                            bi1 clr
08008654: 80                            ret
08008655: C0 33                         go           $0x33
08008657: 04 CD 2D                      by1 :=       $0x2D
0800865A: 2D E5 20 D0                   by comp2     @b.0x20+,r1
0800865E: C6 1F                         if >< go     $0x1F
08008660: 04 CD 2D                      by1 :=       $0x2D
08008663: 0D 4C                         w2 :=        b.0x30
08008665: 2D E5 14 D0                   by comp2     @b.0x14+,r1
08008669: C4 12                         if = go      $0x12
0800866B: 55 01                         w2 +         $0x1
0800866D: 21 4C                         w2 =:        b.0x30
0800866F: 06 0D                         by3 :=       $0xD
08008671: 2D E5 14 D2                   by comp2     @b.0x14+,r3
08008675: C6 04                         if >< go     $0x4
08008677: 84                            bi1 clr
08008678: 80                            ret
08008679: C0 E7                         go           $0xFFFFFFFFFFFFFFE7
0800867B: C0 0D                         go           $0xD
0800867D: 04 E7 14                      by1 :=       @b.0x14+
08008680: 2D E5 20 D0                   by comp2     @b.0x20+,r1
08008684: C4 04                         if = go      $0x4
08008686: 84                            bi1 clr
08008687: 80                            ret
08008688: 4F 4B                         w incr       b.0x2C
0800868A: 4F 4C                         w incr       b.0x30
0800868C: C0 A3                         go           $0xFFFFFFFFFFFFFFA3
0800868E: 0C 01                         w1 :=        $0x1
08008690: 80                            ret
08008691: B8 CF 00 00 00 2C             ents         $0x2C
08008697: 44 C4 08 00 7A 80             w test       $0x8007A80
0800869D: C6 1D                         if >< go     $0x1D
0800869F: 18 42                         r:=          b.0x8
080086A1: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080086AA: 0C 1C                         w1 :=        $0x1C
080086AC: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080086B2: 9D                            ifkret
080086B3: 20 48                         w1 =:        b.0x20
080086B5: 1A 48 46                      w move       b.0x20,b.0x18
080086B8: C0 44                         go           $0x44
080086BA: 18 C4 08 00 7A 80             r:=          $0x8007A80
080086C0: 44 86                         w test       r.0x18
080086C2: C6 14                         if >< go     $0x14
080086C4: 0C C4 08 00 7A 80             w1 :=        $0x8007A80
080086CA: 20 46                         w1 =:        b.0x18
080086CC: 1A F4 14 C4 08 00 7A 80       w move       r1.(0x14),$0x8007A80
080086D4: C0 28                         go           $0x28
080086D6: 0C C4 08 00 7A 80             w1 :=        $0x8007A80
080086DC: 20 46                         w1 =:        b.0x18
080086DE: 0D F4 18                      w2 :=        r1.(0x18)
080086E1: 21 C4 08 00 7A 80             w2 =:        $0x8007A80
080086E7: 21 47                         w2 =:        b.0x1C
080086E9: 18 47                         r:=          b.0x1C
080086EB: 44 85                         w test       r.0x14
080086ED: C4 07                         if = go      $0x7
080086EF: 1A 85 47                      w move       r.0x14,b.0x1C
080086F2: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
080086F4: 18 46                         r:=          b.0x18
080086F6: 0C 85                         w1 :=        r.0x14
080086F8: 18 47                         r:=          b.0x1C
080086FA: 20 85                         w1 =:        r.0x14
080086FC: 85                            bi2 clr
080086FD: 18 46                         r:=          b.0x18
080086FF: 21 85                         w2 =:        r.0x14
08008701: 4A 86                         w stz        r.0x18
08008703: FD 3D C5 18                   w2 laddr     @b.0x18
08008707: 21 4A                         w2 =:        b.0x28
08008709: 85                            bi2 clr
0800870A: 1A 14 49                      w move       $0x14,b.0x24
0800870D: CA 07                         if < go      $0x7
0800870F: 06 0D                         by3 :=       $0xD
08008711: FD 82 49                      by3 sfill    b.0x24
08008714: 0C 46                         w1 :=        b.0x18
08008716: 80                            ret
08008717: B8 CF 00 00 00 1C             ents         $0x1C
0800871D: 44 45                         w test       b.0x14
0800871F: C4 21                         if = go      $0x21
08008721: 1A 45 46                      w move       b.0x14,b.0x18
08008724: 18 46                         r:=          b.0x18
08008726: 44 85                         w test       r.0x14
08008728: C4 07                         if = go      $0x7
0800872A: 1A 85 46                      w move       r.0x14,b.0x18
0800872D: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
0800872F: 1A C4 08 00 7A 80 85          w move       $0x8007A80,r.0x14
08008736: 84                            bi1 clr
08008737: 52 45 D0                      w swap       b.0x14,r1
0800873A: 20 C4 08 00 7A 80             w1 =:        $0x8007A80
08008740: 80                            ret
08008741: B8 CF 00 00 00 24             ents         $0x24
08008747: 20 45                         w1 =:        b.0x14
08008749: 4A 47                         w stz        b.0x1C
0800874B: 20 48                         w1 =:        b.0x20
0800874D: 44 D0                         w test       r1
0800874F: C4 1F                         if = go      $0x1F
08008751: 4A 46                         w stz        b.0x18
08008753: FD 3C C5 20                   w1 laddr     @b.0x20
08008757: 54 46                         w1 +         b.0x18
08008759: 2D F4 00 0D                   by comp2     r1.(0x0),$0xD
0800875D: C4 08                         if = go      $0x8
0800875F: 4F 47                         w incr       b.0x1C
08008761: BF 46 13 F2                   d loopi      b.0x18,$0x13,$0xFFFFFFFFFFFFFFF2
08008765: 18 48                         r:=          b.0x20
08008767: 1A 85 48                      w move       r.0x14,b.0x20
0800876A: 44 48                         w test       b.0x20
0800876C: C6 E5                         if >< go     $0xFFFFFFFFFFFFFFE5
0800876E: 0C 47                         w1 :=        b.0x1C
08008770: 80                            ret
08008771: B8 CF 00 00 00 34             ents         $0x34
08008777: 4A 46                         w stz        b.0x18
08008779: 1A 45 47                      w move       b.0x14,b.0x1C
0800877C: 44 47                         w test       b.0x1C
0800877E: C4 5E                         if = go      $0x5E
08008780: C3 08 00 86 91 00             call         $0x8008691,$0x0
08008786: 9D                            ifkret
08008787: 20 48                         w1 =:        b.0x20
08008789: FD 3D C5 1C                   w2 laddr     @b.0x1C
0800878D: 21 4A                         w2 =:        b.0x28
0800878F: 1A 14 49                      w move       $0x14,b.0x24
08008792: FD 3E F4 00                   w3 laddr     r1.(0x0)
08008796: 22 4C                         w3 =:        b.0x30
08008798: 1A 14 4B                      w move       $0x14,b.0x2C
0800879B: CA 08                         if < go      $0x8
0800879D: 84                            bi1 clr
0800879E: 85                            bi2 clr
0800879F: FD 67 49 4B                   by smove     b.0x24,b.0x2C
080087A3: 2E 47 45                      w comp2      b.0x1C,b.0x14
080087A6: C4 1D                         if = go      $0x1D
080087A8: 18 47                         r:=          b.0x1C
080087AA: 44 86                         w test       r.0x18
080087AC: C4 17                         if = go      $0x17
080087AE: 0C 86                         w1 :=        r.0x18
080087B0: 18 42                         r:=          b.0x8
080087B2: 20 85                         w1 =:        r.0x14
080087B4: C3 08 00 87 71 00             call         $0x8008771,$0x0
080087BA: 9D                            ifkret
080087BB: 18 42                         r:=          b.0x8
080087BD: 0D 86                         w2 :=        r.0x18
080087BF: 18 48                         r:=          b.0x20
080087C1: 21 86                         w2 =:        r.0x18
080087C3: FD 3D 46                      w2 laddr     b.0x18
080087C6: 0C 48                         w1 :=        b.0x20
080087C8: 0E 14                         w3 :=        $0x14
080087CA: FE 03                         clrk
080087CC: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080087D2: 9D                            ifkret
080087D3: 18 47                         r:=          b.0x1C
080087D5: 1A 85 47                      w move       r.0x14,b.0x1C
080087D8: 44 47                         w test       b.0x1C
080087DA: C6 A6                         if >< go     $0xFFFFFFFFFFFFFFA6
080087DC: 80                            ret
080087DD: B8 CF 00 00 00 34             ents         $0x34
080087E3: C3 08 00 86 91 00             call         $0x8008691,$0x0
080087E9: 9D                            ifkret
080087EA: 20 48                         w1 =:        b.0x20
080087EC: 20 4B                         w1 =:        b.0x2C
080087EE: 1A 3F 4A                      w move       $0x3F,b.0x28
080087F1: 0D 46                         w2 :=        b.0x18
080087F3: 21 49                         w2 =:        b.0x24
080087F5: 0E 47                         w3 :=        b.0x1C
080087F7: 22 4C                         w3 =:        b.0x30
080087F9: 35 D2                         w2 comp      r3
080087FB: C8 3A                         if > go      $0x3A
080087FD: 04 0D                         by1 :=       $0xD
080087FF: 0D 49                         w2 :=        b.0x24
08008801: 2D E5 14 D0                   by comp2     @b.0x14+,r1
08008805: C4 30                         if = go      $0x30
08008807: 2E 4A 13                      w comp2      b.0x28,$0x13
0800880A: C6 13                         if >< go     $0x13
0800880C: C3 08 00 86 91 00             call         $0x8008691,$0x0
08008812: 9D                            ifkret
08008813: 18 4B                         r:=          b.0x2C
08008815: 20 85                         w1 =:        r.0x14
08008817: 1A 3F 4A                      w move       $0x3F,b.0x28
0800881A: 1A 85 4B                      w move       r.0x14,b.0x2C
0800881D: 0C 4A                         w1 :=        b.0x28
0800881F: 54 01                         w1 +         $0x1
08008821: 20 4A                         w1 =:        b.0x28
08008823: 0E 49                         w3 :=        b.0x24
08008825: 05 E6 14                      by2 :=       @b.0x14+
08008828: FD 3F C5 2C                   w4 laddr     @b.0x2C
0800882C: 57 D0                         w4 +         r1
0800882E: 1D F7 00                      by2 =:       r4.(0x0)
08008831: BF 49 4C CC                   d loopi      b.0x24,b.0x30,$0xFFFFFFFFFFFFFFCC
08008835: 2E 4A 13                      w comp2      b.0x28,$0x13
08008838: C6 13                         if >< go     $0x13
0800883A: C3 08 00 86 91 00             call         $0x8008691,$0x0
08008840: 9D                            ifkret
08008841: 18 4B                         r:=          b.0x2C
08008843: 20 85                         w1 =:        r.0x14
08008845: 1A 3F 4A                      w move       $0x3F,b.0x28
08008848: 1A 85 4B                      w move       r.0x14,b.0x2C
0800884B: 0D 4A                         w2 :=        b.0x28
0800884D: 55 01                         w2 +         $0x1
0800884F: 06 0D                         by3 :=       $0xD
08008851: FD 3F C5 2C                   w4 laddr     @b.0x2C
08008855: 57 D1                         w4 +         r2
08008857: 1E F7 00                      by3 =:       r4.(0x0)
0800885A: 80                            ret
0800885B: B8 CF 00 00 00 34             ents         $0x34
08008861: 1A 45 4C                      w move       b.0x14,b.0x30
08008864: 4A 4A                         w stz        b.0x28
08008866: 0C 47                         w1 :=        b.0x1C
08008868: 60 01                         w1 -         $0x1
0800886A: 20 4B                         w1 =:        b.0x2C
0800886C: FD 3D C5 30                   w2 laddr     @b.0x30
08008870: 55 4A                         w2 +         b.0x28
08008872: 04 F5 00                      by1 :=       r2.(0x0)
08008875: 1C 49                         by1 =:       b.0x24
08008877: 30 0D                         by1 comp     $0xD
08008879: C4 30                         if = go      $0x30
0800887B: 0E 48                         w3 :=        b.0x20
0800887D: 62 01                         w3 -         $0x1
0800887F: 2E 4B D2                      w comp2      b.0x2C,r3
08008882: CC 0F                         if >= go     $0xF
08008884: 30 09                         by1 comp     $0x9
08008886: C4 0B                         if = go      $0xB
08008888: 0F 4B                         w4 :=        b.0x2C
0800888A: 57 01                         w4 +         $0x1
0800888C: 23 4B                         w4 =:        b.0x2C
0800888E: 1C E7 18                      by1 =:       @b.0x18+
08008891: 0C 4A                         w1 :=        b.0x28
08008893: 54 01                         w1 +         $0x1
08008895: 20 4A                         w1 =:        b.0x28
08008897: 34 13                         w1 comp      $0x13
08008899: C8 07                         if > go      $0x7
0800889B: 2D 49 09                      by comp2     b.0x24,$0x9
0800889E: C6 09                         if >< go     $0x9
080088A0: 18 4C                         r:=          b.0x30
080088A2: 1A 85 4C                      w move       r.0x14,b.0x30
080088A5: 4A 4A                         w stz        b.0x28
080088A7: C0 C5                         go           $0xFFFFFFFFFFFFFFC5
080088A9: 0F 4B                         w4 :=        b.0x2C
080088AB: 57 01                         w4 +         $0x1
080088AD: 04 0D                         by1 :=       $0xD
080088AF: 1C E7 18                      by1 =:       @b.0x18+
080088B2: 80                            ret
080088B3: B8 CF 00 00 00 18             ents         $0x18
080088B9: 18 42                         r:=          b.0x8
080088BB: 1A 45 85                      w move       b.0x14,r.0x14
080088BE: 04 0D                         by1 :=       $0xD
080088C0: C3 08 00 B5 20 00             call         $0x800B520,$0x0
080088C6: 9D                            ifkret
080088C7: 18 42                         r:=          b.0x8
080088C9: 1A 45 85                      w move       b.0x14,r.0x14
080088CC: 04 0A                         by1 :=       $0xA
080088CE: C3 08 00 B5 20 00             call         $0x800B520,$0x0
080088D4: 9D                            ifkret
080088D5: 80                            ret
080088D6: B8 CF 00 00 00 2C             ents         $0x2C
080088DC: 0C 47                         w1 :=        b.0x1C
080088DE: 20 49                         w1 =:        b.0x24
080088E0: 0D 48                         w2 :=        b.0x20
080088E2: 21 4A                         w2 =:        b.0x28
080088E4: 34 D1                         w1 comp      r2
080088E6: C8 17                         if > go      $0x17
080088E8: 18 42                         r:=          b.0x8
080088EA: 1A 45 85                      w move       b.0x14,r.0x14
080088ED: 0D 49                         w2 :=        b.0x24
080088EF: 04 E5 18                      by1 :=       @b.0x18+
080088F2: C3 08 00 B5 20 00             call         $0x800B520,$0x0
080088F8: 9D                            ifkret
080088F9: BF 49 4A EF                   d loopi      b.0x24,b.0x28,$0xFFFFFFFFFFFFFFEF
080088FD: 80                            ret
080088FE: B8 CF 00 00 00 40             ents         $0x40
08008904: 84                            bi1 clr
08008905: 20 47                         w1 =:        b.0x1C
08008907: 20 48                         w1 =:        b.0x20
08008909: 1A 46 4B                      w move       b.0x18,b.0x2C
0800890C: 85                            bi2 clr
0800890D: 21 4E                         w2 =:        b.0x38
0800890F: 21 4F                         w2 =:        b.0x3C
08008911: 44 4B                         w test       b.0x2C
08008913: C5 00 A1                      if = go      $0xA1
08008916: 4A 4A                         w stz        b.0x28
08008918: FD 3D C5 2C                   w2 laddr     @b.0x2C
0800891C: 55 4A                         w2 +         b.0x28
0800891E: 04 F5 00                      by1 :=       r2.(0x0)
08008921: 1C 49                         by1 =:       b.0x24
08008923: 30 09                         by1 comp     $0x9
08008925: C4 16                         if = go      $0x16
08008927: 30 0D                         by1 comp     $0xD
08008929: C4 12                         if = go      $0x12
0800892B: 18 42                         r:=          b.0x8
0800892D: 1A 45 85                      w move       b.0x14,r.0x14
08008930: C3 08 00 B5 20 00             call         $0x800B520,$0x0
08008936: 9D                            ifkret
08008937: BF 4A 13 E1                   d loopi      b.0x28,$0x13,$0xFFFFFFFFFFFFFFE1
0800893B: 18 4B                         r:=          b.0x2C
0800893D: 44 86                         w test       r.0x18
0800893F: C4 0B                         if = go      $0xB
08008941: 1A 4F 4E                      w move       b.0x3C,b.0x38
08008944: 0C 01                         w1 :=        $0x1
08008946: 20 47                         w1 =:        b.0x1C
08008948: 20 48                         w1 =:        b.0x20
0800894A: 2D 49 09                      by comp2     b.0x24,$0x9
0800894D: C6 57                         if >< go     $0x57
0800894F: 44 48                         w test       b.0x20
08008951: C4 53                         if = go      $0x53
08008953: 44 4E                         w test       b.0x38
08008955: C6 07                         if >< go     $0x7
08008957: 1A 46 4F                      w move       b.0x18,b.0x3C
0800895A: C0 07                         go           $0x7
0800895C: 18 4E                         r:=          b.0x38
0800895E: 1A 85 4F                      w move       r.0x14,b.0x3C
08008961: 84                            bi1 clr
08008962: 18 4F                         r:=          b.0x3C
08008964: 52 86 D0                      w swap       r.0x18,r1
08008967: 20 4C                         w1 =:        b.0x30
08008969: 20 4D                         w1 =:        b.0x34
0800896B: 18 4D                         r:=          b.0x34
0800896D: 44 85                         w test       r.0x14
0800896F: C4 07                         if = go      $0x7
08008971: 1A 85 4D                      w move       r.0x14,b.0x34
08008974: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
08008976: 84                            bi1 clr
08008977: 18 4B                         r:=          b.0x2C
08008979: 52 85 D0                      w swap       r.0x14,r1
0800897C: 18 4D                         r:=          b.0x34
0800897E: 20 85                         w1 =:        r.0x14
08008980: 44 4E                         w test       b.0x38
08008982: C6 07                         if >< go     $0x7
08008984: 1A 4C 46                      w move       b.0x30,b.0x18
08008987: C0 07                         go           $0x7
08008989: 18 4E                         r:=          b.0x38
0800898B: 1A 4C 85                      w move       b.0x30,r.0x14
0800898E: 18 42                         r:=          b.0x8
08008990: 1A 4F 85                      w move       b.0x3C,r.0x14
08008993: C3 08 00 87 17 00             call         $0x8008717,$0x0
08008999: 9D                            ifkret
0800899A: 18 42                         r:=          b.0x8
0800899C: 1A 85 4F                      w move       r.0x14,b.0x3C
0800899F: 1A 4D 4B                      w move       b.0x34,b.0x2C
080089A2: 4A 48                         w stz        b.0x20
080089A4: 0D 4B                         w2 :=        b.0x2C
080089A6: 21 4F                         w2 =:        b.0x3C
080089A8: 1A F5 14 4B                   w move       r2.(0x14),b.0x2C
080089AC: 2D 49 0D                      by comp2     b.0x24,$0xD
080089AF: C4 05                         if = go      $0x5
080089B1: C1 FF 60                      go           $0xFFFFFFFFFFFFFF60
080089B4: 44 47                         w test       b.0x1C
080089B6: C4 11                         if = go      $0x11
080089B8: 18 42                         r:=          b.0x8
080089BA: 1A 45 85                      w move       b.0x14,r.0x14
080089BD: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
080089C3: 9D                            ifkret
080089C4: C1 FF 40                      go           $0xFFFFFFFFFFFFFF40
080089C7: 80                            ret
080089C8: B8 CF 00 00 00 30             ents         $0x30
080089CE: 1A CD 72 46                   w move       $0x72,b.0x18
080089D2: 4D 47                         w set1       b.0x1C
080089D4: FE 79 C4 08 00 83 20 48 03    w bmove      $0x8008320,b.0x20,$0x3
080089DD: 1A 3F 4B                      w move       $0x3F,b.0x2C
080089E0: C3 08 00 B9 7C 07 46 47 C5 20 C5 24 C5 28 4B 45 call         $0x800B97C,$0x7,b.0x18,b.0x1C,@b.0x20,@b.0x24,@b.0x28,b.0x2C,b.0x14
080089F0: 9D                            ifkret
080089F1: 80                            ret
080089F2: B8 CF 00 00 00 4C             ents         $0x4C
080089F8: 1A 45 49                      w move       b.0x14,b.0x24
080089FB: 44 49                         w test       b.0x24
080089FD: C4 5C                         if = go      $0x5C
080089FF: 4A 47                         w stz        b.0x1C
08008A01: 2E 47 14                      w comp2      b.0x1C,$0x14
08008A04: CC 19                         if >= go     $0x19
08008A06: FD 3D C5 24                   w2 laddr     @b.0x24
08008A0A: 55 47                         w2 +         b.0x1C
08008A0C: 04 F5 00                      by1 :=       r2.(0x0)
08008A0F: 1C 46                         by1 =:       b.0x18
08008A11: 30 09                         by1 comp     $0x9
08008A13: C4 0A                         if = go      $0xA
08008A15: 30 0D                         by1 comp     $0xD
08008A17: C4 06                         if = go      $0x6
08008A19: 4F 47                         w incr       b.0x1C
08008A1B: C0 E6                         go           $0xFFFFFFFFFFFFFFE6
08008A1D: 1A CD 72 4A                   w move       $0x72,b.0x28
08008A21: 4D 4B                         w set1       b.0x2C
08008A23: 0C 47                         w1 :=        b.0x1C
08008A25: 60 01                         w1 -         $0x1
08008A27: 1A D0 51                      w move       r1,b.0x44
08008A2A: 4A 50                         w stz        b.0x40
08008A2C: FD 3E C5 24                   w3 laddr     @b.0x24
08008A30: 22 4F                         w3 =:        b.0x3C
08008A32: FD 20 4F 4C 0C                by bmove     b.0x3C,b.0x30,$0xC
08008A37: 1A 3F 52                      w move       $0x3F,b.0x48
08008A3A: C3 08 00 B9 7C 07 4A 4B C5 30 C5 34 C5 38 52 48 call         $0x800B97C,$0x7,b.0x28,b.0x2C,@b.0x30,@b.0x34,@b.0x38,b.0x48,b.0x20
08008A4A: 9D                            ifkret
08008A4B: 2D 46 0D                      by comp2     b.0x18,$0xD
08008A4E: C4 0B                         if = go      $0xB
08008A50: 18 49                         r:=          b.0x24
08008A52: 1A 85 49                      w move       r.0x14,b.0x24
08008A55: 44 49                         w test       b.0x24
08008A57: C6 A8                         if >< go     $0xFFFFFFFFFFFFFFA8
08008A59: 80                            ret
08008A5A: B8 CF 00 00 00 40             ents         $0x40
08008A60: 1A CD 72 4A                   w move       $0x72,b.0x28
08008A64: 4D 4B                         w set1       b.0x2C
08008A66: FD 20 45 4C 0C                by bmove     b.0x14,b.0x30,$0xC
08008A6B: 1A 3F 4F                      w move       $0x3F,b.0x3C
08008A6E: C3 08 00 B9 7C 07 4A 4B C5 30 C5 34 C5 38 4F 49 call         $0x800B97C,$0x7,b.0x28,b.0x2C,@b.0x30,@b.0x34,@b.0x38,b.0x3C,b.0x24
08008A7E: 9D                            ifkret
08008A7F: 18 42                         r:=          b.0x8
08008A81: 1A 48 85                      w move       b.0x20,r.0x14
08008A84: C3 08 00 89 F2 00             call         $0x80089F2,$0x0
08008A8A: 9D                            ifkret
08008A8B: 80                            ret
08008A8C: 9C                            entd
08008A8D: FD C0 46                      l=:          b.0x18
08008A90: 2E 47 C4 08 00 7A 0C          w comp2      b.0x1C,$0x8007A0C
08008A97: CE 22                         if <= go     $0x22
08008A99: 18 42                         r:=          b.0x8
08008A9B: 1A CE 01 9A 85                w move       $0x19A,r.0x14
08008AA0: 0C CE 0F C0                   w1 :=        $0xFC0
08008AA4: 54 47                         w1 +         b.0x1C
08008AA6: 20 86                         w1 =:        r.0x18
08008AA8: C3 08 00 B7 8E 00             call         $0x800B78E,$0x0
08008AAE: D2 04                         if -k go     $0x4
08008AB0: B4 46                         jumpg        b.0x18
08008AB2: 1A 47 C4 08 00 7A 0C          w move       b.0x1C,$0x8007A0C
08008AB9: FE 03                         clrk
08008ABB: B4 46                         jumpg        b.0x18
08008ABD: 9C                            entd
08008ABE: FD C0 48                      l=:          b.0x20
08008AC1: 1A CD 72 51                   w move       $0x72,b.0x44
08008AC5: 4D 52                         w set1       b.0x48
08008AC7: FD 20 49 53 0C                by bmove     b.0x24,b.0x4C,$0xC
08008ACC: 1A 3F 56                      w move       $0x3F,b.0x58
08008ACF: C3 08 00 B9 7C 07 51 52 C5 4C C5 50 C5 54 56 4D call         $0x800B97C,$0x7,b.0x44,b.0x48,@b.0x4C,@b.0x50,@b.0x54,b.0x58,b.0x34
08008ADF: D2 04                         if -k go     $0x4
08008AE1: B4 48                         jumpg        b.0x20
08008AE3: 2E 4C 3E                      w comp2      b.0x30,$0x3E
08008AE6: C6 0E                         if >< go     $0xE
08008AE8: FE 79 C4 08 00 83 54 4E 03    w bmove      $0x8008354,b.0x38,$0x3
08008AF1: C1 03 6E                      go           $0x36E
08008AF4: 2E 4C 3D                      w comp2      b.0x30,$0x3D
08008AF7: C6 0E                         if >< go     $0xE
08008AF9: FE 79 C4 08 00 83 8C 4E 03    w bmove      $0x800838C,b.0x38,$0x3
08008B02: C1 03 5D                      go           $0x35D
08008B05: 2E 4C 3C                      w comp2      b.0x30,$0x3C
08008B08: C6 0E                         if >< go     $0xE
08008B0A: FE 79 C4 08 00 83 CC 4E 03    w bmove      $0x80083CC,b.0x38,$0x3
08008B13: C1 03 4C                      go           $0x34C
08008B16: 2E 4C 3B                      w comp2      b.0x30,$0x3B
08008B19: C6 0E                         if >< go     $0xE
08008B1B: FE 79 C4 08 00 83 EC 4E 03    w bmove      $0x80083EC,b.0x38,$0x3
08008B24: C1 03 3B                      go           $0x33B
08008B27: 2E 4C 3A                      w comp2      b.0x30,$0x3A
08008B2A: C6 0E                         if >< go     $0xE
08008B2C: FE 79 C4 08 00 84 28 4E 03    w bmove      $0x8008428,b.0x38,$0x3
08008B35: C1 03 2A                      go           $0x32A
08008B38: 2E 4C 39                      w comp2      b.0x30,$0x39
08008B3B: C6 0E                         if >< go     $0xE
08008B3D: FE 79 C4 08 00 84 54 4E 03    w bmove      $0x8008454,b.0x38,$0x3
08008B46: C1 03 19                      go           $0x319
08008B49: 2E 4C 38                      w comp2      b.0x30,$0x38
08008B4C: C6 0E                         if >< go     $0xE
08008B4E: FE 79 C4 08 00 84 98 4E 03    w bmove      $0x8008498,b.0x38,$0x3
08008B57: C1 03 08                      go           $0x308
08008B5A: 2E 4C 37                      w comp2      b.0x30,$0x37
08008B5D: C6 0E                         if >< go     $0xE
08008B5F: FE 79 C4 08 00 84 DC 4E 03    w bmove      $0x80084DC,b.0x38,$0x3
08008B68: C1 02 F7                      go           $0x2F7
08008B6B: 2E 4C 36                      w comp2      b.0x30,$0x36
08008B6E: C6 0E                         if >< go     $0xE
08008B70: FE 79 C4 08 00 85 1C 4E 03    w bmove      $0x800851C,b.0x38,$0x3
08008B79: C1 02 E6                      go           $0x2E6
08008B7C: 44 4C                         w test       b.0x30
08008B7E: C6 0E                         if >< go     $0xE
08008B80: FE 79 C4 08 00 85 38 4E 03    w bmove      $0x8008538,b.0x38,$0x3
08008B89: C1 02 D6                      go           $0x2D6
08008B8C: 2E 4C CD CC                   w comp2      b.0x30,$0xCC
08008B90: C6 0E                         if >< go     $0xE
08008B92: FE 79 C4 08 00 85 6C 4E 03    w bmove      $0x800856C,b.0x38,$0x3
08008B9B: C1 02 C4                      go           $0x2C4
08008B9E: 2E 4C CD CB                   w comp2      b.0x30,$0xCB
08008BA2: C6 0E                         if >< go     $0xE
08008BA4: FE 79 C4 08 00 85 A0 4E 03    w bmove      $0x80085A0,b.0x38,$0x3
08008BAD: C1 02 B2                      go           $0x2B2
08008BB0: 2E 4C CD CA                   w comp2      b.0x30,$0xCA
08008BB4: C6 0E                         if >< go     $0xE
08008BB6: FE 79 C4 08 00 85 D8 4E 03    w bmove      $0x80085D8,b.0x38,$0x3
08008BBF: C1 02 A0                      go           $0x2A0
08008BC2: 2E 4C CD C9                   w comp2      b.0x30,$0xC9
08008BC6: C6 0E                         if >< go     $0xE
08008BC8: FE 79 C4 08 00 86 10 4E 03    w bmove      $0x8008610,b.0x38,$0x3
08008BD1: C1 02 8E                      go           $0x28E
08008BD4: 2E 4C CD C8                   w comp2      b.0x30,$0xC8
08008BD8: C6 0E                         if >< go     $0xE
08008BDA: FE 79 C4 08 00 86 48 4E 03    w bmove      $0x8008648,b.0x38,$0x3
08008BE3: C1 02 7C                      go           $0x27C
08008BE6: 2E 4C CD C7                   w comp2      b.0x30,$0xC7
08008BEA: C6 0E                         if >< go     $0xE
08008BEC: FE 79 C4 08 00 86 7C 4E 03    w bmove      $0x800867C,b.0x38,$0x3
08008BF5: C1 02 6A                      go           $0x26A
08008BF8: 2E 4C CD C6                   w comp2      b.0x30,$0xC6
08008BFC: C6 0E                         if >< go     $0xE
08008BFE: FE 79 C4 08 00 86 B4 4E 03    w bmove      $0x80086B4,b.0x38,$0x3
08008C07: C1 02 58                      go           $0x258
08008C0A: 2E 4C CD C5                   w comp2      b.0x30,$0xC5
08008C0E: C6 0E                         if >< go     $0xE
08008C10: FE 79 C4 08 00 86 E0 4E 03    w bmove      $0x80086E0,b.0x38,$0x3
08008C19: C1 02 46                      go           $0x246
08008C1C: 2E 4C CD C4                   w comp2      b.0x30,$0xC4
08008C20: C6 0E                         if >< go     $0xE
08008C22: FE 79 C4 08 00 87 24 4E 03    w bmove      $0x8008724,b.0x38,$0x3
08008C2B: C1 02 34                      go           $0x234
08008C2E: 2E 4C CD C3                   w comp2      b.0x30,$0xC3
08008C32: C6 0E                         if >< go     $0xE
08008C34: FE 79 C4 08 00 87 50 4E 03    w bmove      $0x8008750,b.0x38,$0x3
08008C3D: C1 02 22                      go           $0x222
08008C40: 2E 4C CD C2                   w comp2      b.0x30,$0xC2
08008C44: C6 0E                         if >< go     $0xE
08008C46: FE 79 C4 08 00 87 90 4E 03    w bmove      $0x8008790,b.0x38,$0x3
08008C4F: C1 02 10                      go           $0x210
08008C52: 2E 4C CD C1                   w comp2      b.0x30,$0xC1
08008C56: C6 0E                         if >< go     $0xE
08008C58: FE 79 C4 08 00 87 BC 4E 03    w bmove      $0x80087BC,b.0x38,$0x3
08008C61: C1 01 FE                      go           $0x1FE
08008C64: 2E 4C CD C0                   w comp2      b.0x30,$0xC0
08008C68: C6 0E                         if >< go     $0xE
08008C6A: FE 79 C4 08 00 88 00 4E 03    w bmove      $0x8008800,b.0x38,$0x3
08008C73: C1 01 EC                      go           $0x1EC
08008C76: 2E 4C CD BF                   w comp2      b.0x30,$0xBF
08008C7A: C6 0E                         if >< go     $0xE
08008C7C: FE 79 C4 08 00 88 3C 4E 03    w bmove      $0x800883C,b.0x38,$0x3
08008C85: C1 01 DA                      go           $0x1DA
08008C88: 2E 4C CD BE                   w comp2      b.0x30,$0xBE
08008C8C: C6 0E                         if >< go     $0xE
08008C8E: FE 79 C4 08 00 88 60 4E 03    w bmove      $0x8008860,b.0x38,$0x3
08008C97: C1 01 C8                      go           $0x1C8
08008C9A: 2E 4C CD BD                   w comp2      b.0x30,$0xBD
08008C9E: C6 0E                         if >< go     $0xE
08008CA0: FE 79 C4 08 00 88 94 4E 03    w bmove      $0x8008894,b.0x38,$0x3
08008CA9: C1 01 B6                      go           $0x1B6
08008CAC: 2E 4C CD BC                   w comp2      b.0x30,$0xBC
08008CB0: C6 0E                         if >< go     $0xE
08008CB2: FE 79 C4 08 00 88 C8 4E 03    w bmove      $0x80088C8,b.0x38,$0x3
08008CBB: C1 01 A4                      go           $0x1A4
08008CBE: 2E 4C CD BB                   w comp2      b.0x30,$0xBB
08008CC2: C6 0E                         if >< go     $0xE
08008CC4: FE 79 C4 08 00 89 10 4E 03    w bmove      $0x8008910,b.0x38,$0x3
08008CCD: C1 01 92                      go           $0x192
08008CD0: 2E 4C CD BA                   w comp2      b.0x30,$0xBA
08008CD4: C6 0E                         if >< go     $0xE
08008CD6: FE 79 C4 08 00 89 2C 4E 03    w bmove      $0x800892C,b.0x38,$0x3
08008CDF: C1 01 80                      go           $0x180
08008CE2: 2E 4C CD B9                   w comp2      b.0x30,$0xB9
08008CE6: C6 0E                         if >< go     $0xE
08008CE8: FE 79 C4 08 00 89 5C 4E 03    w bmove      $0x800895C,b.0x38,$0x3
08008CF1: C1 01 6E                      go           $0x16E
08008CF4: 2E 4C CD B8                   w comp2      b.0x30,$0xB8
08008CF8: C6 0E                         if >< go     $0xE
08008CFA: FE 79 C4 08 00 89 98 4E 03    w bmove      $0x8008998,b.0x38,$0x3
08008D03: C1 01 5C                      go           $0x15C
08008D06: 2E 4C CD B7                   w comp2      b.0x30,$0xB7
08008D0A: C6 0E                         if >< go     $0xE
08008D0C: FE 79 C4 08 00 89 CC 4E 03    w bmove      $0x80089CC,b.0x38,$0x3
08008D15: C1 01 4A                      go           $0x14A
08008D18: 2E 4C CD B6                   w comp2      b.0x30,$0xB6
08008D1C: C6 0E                         if >< go     $0xE
08008D1E: FE 79 C4 08 00 8A 00 4E 03    w bmove      $0x8008A00,b.0x38,$0x3
08008D27: C1 01 38                      go           $0x138
08008D2A: 2E 4C CD B5                   w comp2      b.0x30,$0xB5
08008D2E: C6 0E                         if >< go     $0xE
08008D30: FE 79 C4 08 00 8A 44 4E 03    w bmove      $0x8008A44,b.0x38,$0x3
08008D39: C1 01 26                      go           $0x126
08008D3C: 2E 4C CD B4                   w comp2      b.0x30,$0xB4
08008D40: C6 0E                         if >< go     $0xE
08008D42: FE 79 C4 08 00 8A 6C 4E 03    w bmove      $0x8008A6C,b.0x38,$0x3
08008D4B: C1 01 14                      go           $0x114
08008D4E: 2E 4C CD B3                   w comp2      b.0x30,$0xB3
08008D52: C6 0E                         if >< go     $0xE
08008D54: FE 79 C4 08 00 8A 9C 4E 03    w bmove      $0x8008A9C,b.0x38,$0x3
08008D5D: C1 01 02                      go           $0x102
08008D60: 2E 4C CD B2                   w comp2      b.0x30,$0xB2
08008D64: C6 0E                         if >< go     $0xE
08008D66: FE 79 C4 08 00 8A D8 4E 03    w bmove      $0x8008AD8,b.0x38,$0x3
08008D6F: C1 00 F0                      go           $0xF0
08008D72: 2E 4C CD B1                   w comp2      b.0x30,$0xB1
08008D76: C6 0E                         if >< go     $0xE
08008D78: FE 79 C4 08 00 8B 18 4E 03    w bmove      $0x8008B18,b.0x38,$0x3
08008D81: C1 00 DE                      go           $0xDE
08008D84: 2E 4C CD B0                   w comp2      b.0x30,$0xB0
08008D88: C6 0E                         if >< go     $0xE
08008D8A: FE 79 C4 08 00 8B 44 4E 03    w bmove      $0x8008B44,b.0x38,$0x3
08008D93: C1 00 CC                      go           $0xCC
08008D96: 2E 4C CD AF                   w comp2      b.0x30,$0xAF
08008D9A: C6 0E                         if >< go     $0xE
08008D9C: FE 79 C4 08 00 8B 64 4E 03    w bmove      $0x8008B64,b.0x38,$0x3
08008DA5: C1 00 BA                      go           $0xBA
08008DA8: 2E 4C CD AE                   w comp2      b.0x30,$0xAE
08008DAC: C6 0E                         if >< go     $0xE
08008DAE: FE 79 C4 08 00 8B 84 4E 03    w bmove      $0x8008B84,b.0x38,$0x3
08008DB7: C1 00 A8                      go           $0xA8
08008DBA: 2E 4C CD AD                   w comp2      b.0x30,$0xAD
08008DBE: C6 0E                         if >< go     $0xE
08008DC0: FE 79 C4 08 00 8B A0 4E 03    w bmove      $0x8008BA0,b.0x38,$0x3
08008DC9: C1 00 96                      go           $0x96
08008DCC: 2E 4C CD AC                   w comp2      b.0x30,$0xAC
08008DD0: C6 0E                         if >< go     $0xE
08008DD2: FE 79 C4 08 00 8B C8 4E 03    w bmove      $0x8008BC8,b.0x38,$0x3
08008DDB: C1 00 84                      go           $0x84
08008DDE: 2E 4C CD AB                   w comp2      b.0x30,$0xAB
08008DE2: C6 0D                         if >< go     $0xD
08008DE4: FE 79 C4 08 00 8B F0 4E 03    w bmove      $0x8008BF0,b.0x38,$0x3
08008DED: C0 72                         go           $0x72
08008DEF: 2E 4C CD AA                   w comp2      b.0x30,$0xAA
08008DF3: C6 0D                         if >< go     $0xD
08008DF5: FE 79 C4 08 00 8C 28 4E 03    w bmove      $0x8008C28,b.0x38,$0x3
08008DFE: C0 61                         go           $0x61
08008E00: 2E 4C CD 9A                   w comp2      b.0x30,$0x9A
08008E04: C6 0D                         if >< go     $0xD
08008E06: FE 79 C4 08 00 8C 54 4E 03    w bmove      $0x8008C54,b.0x38,$0x3
08008E0F: C0 50                         go           $0x50
08008E11: 1A CD 72 51                   w move       $0x72,b.0x44
08008E15: 4D 52                         w set1       b.0x48
08008E17: FE 79 C4 08 00 8C 74 57 03    w bmove      $0x8008C74,b.0x5C,$0x3
08008E20: 1A 3F 56                      w move       $0x3F,b.0x58
08008E23: C3 08 00 B9 7C 07 51 52 C5 5C C5 60 C5 64 56 4D call         $0x800B97C,$0x7,b.0x44,b.0x48,@b.0x5C,@b.0x60,@b.0x64,b.0x58,b.0x34
08008E33: D2 04                         if -k go     $0x4
08008E35: B4 48                         jumpg        b.0x20
08008E37: 18 42                         r:=          b.0x8
08008E39: 4D 85                         w set1       r.0x14
08008E3B: FE 79 C4 08 00 8C 84 86 03    w bmove      $0x8008C84,r.0x18,$0x3
08008E44: 1A 4C 89                      w move       b.0x30,r.0x24
08008E47: C3 08 00 C6 02 00             call         $0x800C602,$0x0
08008E4D: D2 04                         if -k go     $0x4
08008E4F: B4 48                         jumpg        b.0x20
08008E51: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08008E57: D2 04                         if -k go     $0x4
08008E59: B4 48                         jumpg        b.0x20
08008E5B: FE 03                         clrk
08008E5D: B4 48                         jumpg        b.0x20
08008E5F: 1A CD 72 51                   w move       $0x72,b.0x44
08008E63: 4D 52                         w set1       b.0x48
08008E65: FD 20 4E 5A 0C                by bmove     b.0x38,b.0x68,$0xC
08008E6A: 1A 3F 56                      w move       $0x3F,b.0x58
08008E6D: C3 08 00 B9 7C 07 51 52 C5 68 C5 6C C5 70 56 4D call         $0x800B97C,$0x7,b.0x44,b.0x48,@b.0x68,@b.0x6C,@b.0x70,b.0x58,b.0x34
08008E7D: D2 04                         if -k go     $0x4
08008E7F: B4 48                         jumpg        b.0x20
08008E81: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08008E87: D2 04                         if -k go     $0x4
08008E89: B4 48                         jumpg        b.0x20
08008E8B: FE 03                         clrk
08008E8D: B4 48                         jumpg        b.0x20
08008E8F: B8 CF 00 00 00 78             ents         $0x78
08008E95: 20 45                         w1 =:        b.0x14
08008E97: 34 CD C6                      w1 comp      $0xC6
08008E9A: C6 2C                         if >< go     $0x2C
08008E9C: 1A CD 3F 47                   w move       $0x3F,b.0x1C
08008EA0: C3 08 00 8A 8C 00             call         $0x8008A8C,$0x0
08008EA6: 9D                            ifkret
08008EA7: FE 79 C4 08 00 8C 98 49 03    w bmove      $0x8008C98,b.0x24,$0x3
08008EB0: 1A 45 4C                      w move       b.0x14,b.0x30
08008EB3: C3 08 00 8A BD 00             call         $0x8008ABD,$0x0
08008EB9: 9D                            ifkret
08008EBA: 4F C4 08 00 79 F0             w incr       $0x80079F0
08008EC0: 0C 01                         w1 :=        $0x1
08008EC2: 80                            ret
08008EC3: C1 00 99                      go           $0x99
08008EC6: 34 CD 9B                      w1 comp      $0x9B
08008EC9: C8 19                         if > go      $0x19
08008ECB: FE 79 C4 08 00 8C B4 49 03    w bmove      $0x8008CB4,b.0x24,$0x3
08008ED4: 20 4C                         w1 =:        b.0x30
08008ED6: C3 08 00 8A BD 00             call         $0x8008ABD,$0x0
08008EDC: 9D                            ifkret
08008EDD: 84                            bi1 clr
08008EDE: 80                            ret
08008EDF: C1 00 7D                      go           $0x7D
08008EE2: 34 CD CD                      w1 comp      $0xCD
08008EE5: C8 2B                         if > go      $0x2B
08008EE7: 1A CD 38 47                   w move       $0x38,b.0x1C
08008EEB: C3 08 00 8A 8C 00             call         $0x8008A8C,$0x0
08008EF1: 9D                            ifkret
08008EF2: FE 79 C4 08 00 8C C8 49 03    w bmove      $0x8008CC8,b.0x24,$0x3
08008EFB: 1A 45 4C                      w move       b.0x14,b.0x30
08008EFE: C3 08 00 8A BD 00             call         $0x8008ABD,$0x0
08008F04: 9D                            ifkret
08008F05: 4F C4 08 00 79 F0             w incr       $0x80079F0
08008F0B: 0C 01                         w1 :=        $0x1
08008F0D: 80                            ret
08008F0E: C0 4E                         go           $0x4E
08008F10: 34 3F                         w1 comp      $0x3F
08008F12: C8 29                         if > go      $0x29
08008F14: 1A 10 47                      w move       $0x10,b.0x1C
08008F17: C3 08 00 8A 8C 00             call         $0x8008A8C,$0x0
08008F1D: 9D                            ifkret
08008F1E: FE 79 C4 08 00 8C E0 49 03    w bmove      $0x8008CE0,b.0x24,$0x3
08008F27: 1A 45 4C                      w move       b.0x14,b.0x30
08008F2A: C3 08 00 8A BD 00             call         $0x8008ABD,$0x0
08008F30: 9D                            ifkret
08008F31: 4F C4 08 00 79 F4             w incr       $0x80079F4
08008F37: 84                            bi1 clr
08008F38: 80                            ret
08008F39: C0 23                         go           $0x23
08008F3B: 1A CD 30 47                   w move       $0x30,b.0x1C
08008F3F: C3 08 00 8A 8C 00             call         $0x8008A8C,$0x0
08008F45: 9D                            ifkret
08008F46: 1A CD 34 5D                   w move       $0x34,b.0x74
08008F4A: C3 08 00 B9 7C 02 5D 45       call         $0x800B97C,$0x2,b.0x74,b.0x14
08008F52: 9D                            ifkret
08008F53: 4F C4 08 00 79 F0             w incr       $0x80079F0
08008F59: 0C 01                         w1 :=        $0x1
08008F5B: 80                            ret
08008F5C: B8 CF 00 00 00 64             ents         $0x64
08008F62: FD 20 CD 20 46 CD 32          by bmove     $0x20,b.0x18,$0x32
08008F69: 0C CD 31                      w1 :=        $0x31
08008F6C: 19 0D D4 18                   by move      $0xD,b.0x18+
08008F70: 1A CD 72 53                   w move       $0x72,b.0x4C
08008F74: 4D 54                         w set1       b.0x50
08008F76: FD 3D 46                      w2 laddr     b.0x18
08008F79: 21 55                         w2 =:        b.0x54
08008F7B: 4A 56                         w stz        b.0x58
08008F7D: 1A CD 31 57                   w move       $0x31,b.0x5C
08008F81: 1A 3F 58                      w move       $0x3F,b.0x60
08008F84: C3 08 00 B9 7C 07 53 54 C5 54 C5 58 C5 5C 58 45 call         $0x800B97C,$0x7,b.0x4C,b.0x50,@b.0x54,@b.0x58,@b.0x5C,b.0x60,b.0x14
08008F94: 9D                            ifkret
08008F95: 80                            ret
08008F96: 9C                            entd
08008F97: FD C0 45                      l=:          b.0x14
08008F9A: 04 46                         by1 :=       b.0x18
08008F9C: 6C CD 48                      w1 *         $0x48
08008F9F: 0D 06                         w2 :=        $0x6
08008FA1: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
08008FA8: 21 80                         w2 =:        r.0x0
08008FAA: 06 46                         by3 :=       b.0x18
08008FAC: 6E CD 48                      w3 *         $0x48
08008FAF: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
08008FB6: FE 27 82                      by4 laddr    r.0x8
08008FB9: FE 79 C4 08 00 8C EC F7 00 08 w bmove      $0x8008CEC,r4.(0x0),$0x8
08008FC3: 04 46                         by1 :=       b.0x18
08008FC5: 6C CD 48                      w1 *         $0x48
08008FC8: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
08008FCF: FE 25 82                      by2 laddr    r.0x8
08008FD2: 21 47                         w2 =:        b.0x1C
08008FD4: 06 C1 19                      by3 :=       b.0x19
08008FD7: FC 86 E6 1C                   bi set1      @b.0x1C+
08008FDB: 07 46                         by4 :=       b.0x18
08008FDD: 6F CD 48                      w4 *         $0x48
08008FE0: 0C 07                         w1 :=        $0x7
08008FE2: FC 5A E3 08 00 57 68          by rladdr    $0x8005768+
08008FE9: 20 81                         w1 =:        r.0x4
08008FEB: FE 03                         clrk
08008FED: B4 45                         jumpg        b.0x14
08008FEF: B8 CF 00 00 00 20             ents         $0x20
08008FF5: 19 CD 85 46                   by move      $0x85,b.0x18
08008FF9: 19 CD B2 C1 19                by move      $0xB2,b.0x19
08008FFE: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009004: 9D                            ifkret
08009005: 19 CD 86 46                   by move      $0x86,b.0x18
08009009: 19 CD B2 C1 19                by move      $0xB2,b.0x19
0800900E: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009014: 9D                            ifkret
08009015: 19 CD 8C 46                   by move      $0x8C,b.0x18
08009019: 19 CD AF C1 19                by move      $0xAF,b.0x19
0800901E: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009024: 9D                            ifkret
08009025: 19 CD 96 46                   by move      $0x96,b.0x18
08009029: 19 CD AC C1 19                by move      $0xAC,b.0x19
0800902E: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009034: 9D                            ifkret
08009035: 19 CD 98 46                   by move      $0x98,b.0x18
08009039: 19 CD B3 C1 19                by move      $0xB3,b.0x19
0800903E: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009044: 9D                            ifkret
08009045: 19 CD 9A 46                   by move      $0x9A,b.0x18
08009049: 19 CD B0 C1 19                by move      $0xB0,b.0x19
0800904E: C3 08 00 8F 96 00             call         $0x8008F96,$0x0
08009054: 9D                            ifkret
08009055: 80                            ret
08009056: B8 CF 00 00 00 1C             ents         $0x1C
0800905C: 1C 46                         by1 =:       b.0x18
0800905E: 34 CE 00 85                   w1 comp      $0x85
08009062: C4 0E                         if = go      $0xE
08009064: 2D 46 CD 86                   by comp2     b.0x18,$0x86
08009068: C4 08                         if = go      $0x8
0800906A: 2D 46 CD 98                   by comp2     b.0x18,$0x98
0800906E: C6 12                         if >< go     $0x12
08009070: 2D 45 CD B2                   by comp2     b.0x14,$0xB2
08009074: C4 0A                         if = go      $0xA
08009076: 2D 45 CD B3                   by comp2     b.0x14,$0xB3
0800907A: C4 04                         if = go      $0x4
0800907C: 84                            bi1 clr
0800907D: 80                            ret
0800907E: C0 66                         go           $0x66
08009080: 2D 46 CD 8C                   by comp2     b.0x18,$0x8C
08009084: C6 24                         if >< go     $0x24
08009086: 2D 45 CD AF                   by comp2     b.0x14,$0xAF
0800908A: C4 1C                         if = go      $0x1C
0800908C: 2D 45 CD B1                   by comp2     b.0x14,$0xB1
08009090: C4 16                         if = go      $0x16
08009092: 2D 45 CD B5                   by comp2     b.0x14,$0xB5
08009096: C4 10                         if = go      $0x10
08009098: 2D 45 CD B4                   by comp2     b.0x14,$0xB4
0800909C: C4 0A                         if = go      $0xA
0800909E: 2D 45 CD B2                   by comp2     b.0x14,$0xB2
080090A2: C4 04                         if = go      $0x4
080090A4: 84                            bi1 clr
080090A5: 80                            ret
080090A6: C0 3E                         go           $0x3E
080090A8: 2D 46 CD 96                   by comp2     b.0x18,$0x96
080090AC: C6 1A                         if >< go     $0x1A
080090AE: 05 45                         by2 :=       b.0x14
080090B0: 35 CE 00 AB                   w2 comp      $0xAB
080090B4: C4 10                         if = go      $0x10
080090B6: 2D 45 CD AC                   by comp2     b.0x14,$0xAC
080090BA: C4 0A                         if = go      $0xA
080090BC: 2D 45 CD AD                   by comp2     b.0x14,$0xAD
080090C0: C4 04                         if = go      $0x4
080090C2: 84                            bi1 clr
080090C3: 80                            ret
080090C4: C0 20                         go           $0x20
080090C6: 05 46                         by2 :=       b.0x18
080090C8: 35 CE 00 9A                   w2 comp      $0x9A
080090CC: C6 18                         if >< go     $0x18
080090CE: 2D 45 CD AE                   by comp2     b.0x14,$0xAE
080090D2: C4 12                         if = go      $0x12
080090D4: 2D 45 CD B0                   by comp2     b.0x14,$0xB0
080090D8: C4 0C                         if = go      $0xC
080090DA: 05 45                         by2 :=       b.0x14
080090DC: 35 CE 00 B6                   w2 comp      $0xB6
080090E0: C4 04                         if = go      $0x4
080090E2: 84                            bi1 clr
080090E3: 80                            ret
080090E4: 0C 01                         w1 :=        $0x1
080090E6: 80                            ret
080090E7: B8 CF 00 00 00 24             ents         $0x24
080090ED: 1C 47                         by1 =:       b.0x1C
080090EF: 0C 46                         w1 :=        b.0x18
080090F1: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
080090F7: 9D                            ifkret
080090F8: 05 47                         by2 :=       b.0x1C
080090FA: 6D CD 48                      w2 *         $0x48
080090FD: 20 48                         w1 =:        b.0x20
080090FF: FC 5A E1 08 00 57 68          by rladdr    $0x8005768+
08009106: 0C 80                         w1 :=        r.0x0
08009108: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800910E: 9D                            ifkret
0800910F: 2E 48 D0                      w comp2      b.0x20,r1
08009112: C9 00 BB                      if > go      $0xBB
08009115: 2E 46 03                      w comp2      b.0x18,$0x3
08009118: C6 69                         if >< go     $0x69
0800911A: 05 47                         by2 :=       b.0x1C
0800911C: 6D CD 48                      w2 *         $0x48
0800911F: FC 5A E1 08 00 57 68          by rladdr    $0x8005768+
08009126: 2E 80 03                      w comp2      r.0x0,$0x3
08009129: C4 58                         if = go      $0x58
0800912B: 06 47                         by3 :=       b.0x1C
0800912D: 6E CD 48                      w3 *         $0x48
08009130: 07 47                         by4 :=       b.0x1C
08009132: 6F CD 48                      w4 *         $0x48
08009135: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
0800913C: 0C 80                         w1 :=        r.0x0
0800913E: FC 5A E3 08 00 57 68          by rladdr    $0x8005768+
08009145: 20 81                         w1 =:        r.0x4
08009147: 05 47                         by2 :=       b.0x1C
08009149: 6D CD 48                      w2 *         $0x48
0800914C: 06 47                         by3 :=       b.0x1C
0800914E: 6E CD 48                      w3 *         $0x48
08009151: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
08009158: FE 24 8A                      by1 laddr    r.0x28
0800915B: FC 5A E1 08 00 57 68          by rladdr    $0x8005768+
08009162: FE 79 82 F4 00 08             w bmove      r.0x8,r1.(0x0),$0x8
08009168: 07 47                         by4 :=       b.0x1C
0800916A: 6F CD 48                      w4 *         $0x48
0800916D: FC 5A E3 08 00 57 68          by rladdr    $0x8005768+
08009174: FE 24 82                      by1 laddr    r.0x8
08009177: FE 79 C4 08 00 8D 0C F4 00 08 w bmove      $0x8008D0C,r1.(0x0),$0x8
08009181: 06 47                         by3 :=       b.0x1C
08009183: 6E CD 48                      w3 *         $0x48
08009186: 0D 46                         w2 :=        b.0x18
08009188: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
0800918F: 21 80                         w2 =:        r.0x0
08009191: 2D 47 CD 8C                   by comp2     b.0x1C,$0x8C
08009195: C6 08                         if >< go     $0x8
08009197: 2D 45 CD B2                   by comp2     b.0x14,$0xB2
0800919B: C4 1B                         if = go      $0x1B
0800919D: 04 47                         by1 :=       b.0x1C
0800919F: 6C CD 48                      w1 *         $0x48
080091A2: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
080091A9: FE 27 82                      by4 laddr    r.0x8
080091AC: FE 79 C4 08 00 8D 2C F7 00 08 w bmove      $0x8008D2C,r4.(0x0),$0x8
080091B6: 05 47                         by2 :=       b.0x1C
080091B8: 6D CD 48                      w2 *         $0x48
080091BB: FC 5A E1 08 00 57 68          by rladdr    $0x8005768+
080091C2: FE 26 82                      by3 laddr    r.0x8
080091C5: 22 48                         w3 =:        b.0x20
080091C7: 04 45                         by1 :=       b.0x14
080091C9: FC 86 E4 20                   bi set1      @b.0x20+
080091CD: 80                            ret
080091CE: B8 CF 00 00 00 24             ents         $0x24
080091D4: 1C 46                         by1 =:       b.0x18
080091D6: 6C CD 48                      w1 *         $0x48
080091D9: 4A 47                         w stz        b.0x1C
080091DB: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
080091E2: FE 25 82                      by2 laddr    r.0x8
080091E5: 21 48                         w2 =:        b.0x20
080091E7: 06 45                         by3 :=       b.0x14
080091E9: 41 E6 20                      bi test      @b.0x20+
080091EC: C4 04                         if = go      $0x4
080091EE: 4D 47                         w set1       b.0x1C
080091F0: 0C 47                         w1 :=        b.0x1C
080091F2: 80                            ret
080091F3: B8 CF 00 00 00 1C             ents         $0x1C
080091F9: FD 54 CE 00 85 45             w byconv     $0x85,b.0x14
080091FF: C3 08 00 92 3B 00             call         $0x800923B,$0x0
08009205: 19 CD 86 45                   by move      $0x86,b.0x14
08009209: C3 08 00 92 3B 00             call         $0x800923B,$0x0
0800920F: 19 CD 8C 45                   by move      $0x8C,b.0x14
08009213: C3 08 00 92 3B 00             call         $0x800923B,$0x0
08009219: 19 CD 96 45                   by move      $0x96,b.0x14
0800921D: C3 08 00 92 3B 00             call         $0x800923B,$0x0
08009223: 19 CD 98 45                   by move      $0x98,b.0x14
08009227: C3 08 00 92 3B 00             call         $0x800923B,$0x0
0800922D: FD 54 CE 00 9A 45             w byconv     $0x9A,b.0x14
08009233: C3 08 00 92 3B 00             call         $0x800923B,$0x0
08009239: C0 6A                         go           $0x6A
0800923B: 9C                            entd
0800923C: FD C0 46                      l=:          b.0x18
0800923F: 04 45                         by1 :=       b.0x14
08009241: 6C CD 48                      w1 *         $0x48
08009244: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
0800924B: 2E 80 03                      w comp2      r.0x0,$0x3
0800924E: C6 53                         if >< go     $0x53
08009250: 05 45                         by2 :=       b.0x14
08009252: 6D CD 48                      w2 *         $0x48
08009255: 06 45                         by3 :=       b.0x14
08009257: 6E CD 48                      w3 *         $0x48
0800925A: FC 5A E1 08 00 57 68          by rladdr    $0x8005768+
08009261: 0F 81                         w4 :=        r.0x4
08009263: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
0800926A: 23 80                         w4 =:        r.0x0
0800926C: 37 07                         w4 comp      $0x7
0800926E: C4 23                         if = go      $0x23
08009270: 07 45                         by4 :=       b.0x14
08009272: 6F CD 48                      w4 *         $0x48
08009275: 04 45                         by1 :=       b.0x14
08009277: 6C CD 48                      w1 *         $0x48
0800927A: FC 5A E0 08 00 57 68          by rladdr    $0x8005768+
08009281: FE 25 82                      by2 laddr    r.0x8
08009284: FC 5A E3 08 00 57 68          by rladdr    $0x8005768+
0800928B: FE 79 8A F5 00 08             w bmove      r.0x28,r2.(0x0),$0x8
08009291: 06 45                         by3 :=       b.0x14
08009293: 6E CD 48                      w3 *         $0x48
08009296: 0C 07                         w1 :=        $0x7
08009298: FC 5A E2 08 00 57 68          by rladdr    $0x8005768+
0800929F: 20 81                         w1 =:        r.0x4
080092A1: B4 46                         jumpg        b.0x18
080092A3: 80                            ret
080092A4: 9C                            entd
080092A5: FD C0 46                      l=:          b.0x18
080092A8: FD 54 CE 00 AB C1 1D          w byconv     $0xAB,b.0x1D
080092AF: 18 42                         r:=          b.0x8
080092B1: 19 C1 1D 85                   by move      b.0x1D,r.0x14
080092B5: 04 47                         by1 :=       b.0x1C
080092B7: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
080092BD: D2 04                         if -k go     $0x4
080092BF: B4 46                         jumpg        b.0x18
080092C1: 44 D0                         w test       r1
080092C3: C5 00 EB                      if = go      $0xEB
080092C6: 1A 3F 49                      w move       $0x3F,b.0x24
080092C9: 4D 48                         w set1       b.0x20
080092CB: 04 47                         by1 :=       b.0x1C
080092CD: 6C 0C                         w1 *         $0xC
080092CF: FC 5A E0 08 00 74 A8          by rladdr    $0x80074A8+
080092D6: 0D 82                         w2 :=        r.0x8
080092D8: 21 63                         w2 =:        b.0x8C
080092DA: 2E 48 D1                      w comp2      b.0x20,r2
080092DD: C8 36                         if > go      $0x36
080092DF: 04 47                         by1 :=       b.0x1C
080092E1: 6C 0C                         w1 *         $0xC
080092E3: 05 0D                         by2 :=       $0xD
080092E5: FD 20 E0 08 00 74 A8 64 04    by bmove     $0x80074A8+,b.0x90,$0x4
080092EE: 0E 48                         w3 :=        b.0x20
080092F0: 2D E6 90 D1                   by comp2     @b.0xFFFFFFFFFFFFFF90+,r2
080092F4: C4 1F                         if = go      $0x1F
080092F6: 07 47                         by4 :=       b.0x1C
080092F8: 6F 0C                         w4 *         $0xC
080092FA: 0C 49                         w1 :=        b.0x24
080092FC: 54 01                         w1 +         $0x1
080092FE: 20 49                         w1 =:        b.0x24
08009300: FD 20 E3 08 00 74 A8 64 04    by bmove     $0x80074A8+,b.0x90,$0x4
08009309: 05 E6 90                      by2 :=       @b.0xFFFFFFFFFFFFFF90+
0800930C: 1D D4 28                      by2 =:       b.0x28+
0800930F: BF 48 63 D0                   d loopi      b.0x20,b.0x8C,$0xFFFFFFFFFFFFFFD0
08009313: 0C 49                         w1 :=        b.0x24
08009315: 54 01                         w1 +         $0x1
08009317: 20 49                         w1 =:        b.0x24
08009319: 19 CD 20 D4 28                by move      $0x20,b.0x28+
0800931E: 05 C1 1D                      by2 :=       b.0x1D
08009321: 6D 0C                         w2 *         $0xC
08009323: FC 5A E1 08 00 74 3C          by rladdr    $0x800743C+
0800932A: 0E 81                         w3 :=        r.0x4
0800932C: 22 48                         w3 =:        b.0x20
0800932E: 07 C1 1D                      by4 :=       b.0x1D
08009331: 6F 0C                         w4 *         $0xC
08009333: FC 5A E3 08 00 74 3C          by rladdr    $0x800743C+
0800933A: 0C 82                         w1 :=        r.0x8
0800933C: 20 64                         w1 =:        b.0x90
0800933E: 36 D0                         w3 comp      r1
08009340: C8 38                         if > go      $0x38
08009342: 04 C1 1D                      by1 :=       b.0x1D
08009345: 6C 0C                         w1 *         $0xC
08009347: 05 0D                         by2 :=       $0xD
08009349: FD 20 E0 08 00 74 3C 65 04    by bmove     $0x800743C+,b.0x94,$0x4
08009352: 0E 48                         w3 :=        b.0x20
08009354: 2D E6 94 D1                   by comp2     @b.0xFFFFFFFFFFFFFF94+,r2
08009358: C4 20                         if = go      $0x20
0800935A: 07 C1 1D                      by4 :=       b.0x1D
0800935D: 6F 0C                         w4 *         $0xC
0800935F: 0C 49                         w1 :=        b.0x24
08009361: 54 01                         w1 +         $0x1
08009363: 20 49                         w1 =:        b.0x24
08009365: FD 20 E3 08 00 74 3C 65 04    by bmove     $0x800743C+,b.0x94,$0x4
0800936E: 05 E6 94                      by2 :=       @b.0xFFFFFFFFFFFFFF94+
08009371: 1D D4 28                      by2 =:       b.0x28+
08009374: BF 48 64 CE                   d loopi      b.0x20,b.0x90,$0xFFFFFFFFFFFFFFCE
08009378: 1A CD 72 65                   w move       $0x72,b.0x94
0800937C: 4D 66                         w set1       b.0x98
0800937E: 1A 49 6C                      w move       b.0x24,b.0xB0
08009381: 4A 6B                         w stz        b.0xAC
08009383: FD 3C 4A                      w1 laddr     b.0x28
08009386: 20 6A                         w1 =:        b.0xA8
08009388: FD 20 6A 67 0C                by bmove     b.0xA8,b.0x9C,$0xC
0800938D: 1A 3F 6D                      w move       $0x3F,b.0xB4
08009390: C3 08 00 B9 7C 07 65 66 C5 9C C5 A0 C5 A4 6D 45 call         $0x800B97C,$0x7,b.0x94,b.0x98,@b.0xFFFFFFFFFFFFFF9C,@b.0xFFFFFFFFFFFFFFA0,@b.0xFFFFFFFFFFFFFFA4,b.0xB4,b.0x14
080093A0: D2 04                         if -k go     $0x4
080093A2: B4 46                         jumpg        b.0x18
080093A4: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
080093AA: D2 04                         if -k go     $0x4
080093AC: B4 46                         jumpg        b.0x18
080093AE: 05 C1 1D                      by2 :=       b.0x1D
080093B1: 55 01                         w2 +         $0x1
080093B3: 1D C1 1D                      by2 =:       b.0x1D
080093B6: FC 1D CE 00 B6                h2 comp      $0xB6
080093BB: CF FE F4                      if <= go     $0xFFFFFFFFFFFFFEF4
080093BE: FE 03                         clrk
080093C0: B4 46                         jumpg        b.0x18
080093C2: B8 CF 00 00 00 DC             ents         $0xDC
080093C8: 1A CD 72 6E                   w move       $0x72,b.0xB8
080093CC: 4D 6F                         w set1       b.0xBC
080093CE: FE 79 C4 08 00 8D 54 70 03    w bmove      $0x8008D54,b.0xC0,$0x3
080093D7: 1A 3F 73                      w move       $0x3F,b.0xCC
080093DA: C3 08 00 B9 7C 07 6E 6F C5 C0 C5 C4 C5 C8 73 45 call         $0x800B97C,$0x7,b.0xB8,b.0xBC,@b.0xFFFFFFFFFFFFFFC0,@b.0xFFFFFFFFFFFFFFC4,@b.0xFFFFFFFFFFFFFFC8,b.0xCC,b.0x14
080093EA: 9D                            ifkret
080093EB: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
080093F1: 9D                            ifkret
080093F2: 1A CD 72 6E                   w move       $0x72,b.0xB8
080093F6: 4D 6F                         w set1       b.0xBC
080093F8: FE 79 C4 08 00 8D 68 74 03    w bmove      $0x8008D68,b.0xD0,$0x3
08009401: 1A 3F 73                      w move       $0x3F,b.0xCC
08009404: C3 08 00 B9 7C 07 6E 6F C5 D0 C5 D4 C5 D8 73 45 call         $0x800B97C,$0x7,b.0xB8,b.0xBC,@b.0xFFFFFFFFFFFFFFD0,@b.0xFFFFFFFFFFFFFFD4,@b.0xFFFFFFFFFFFFFFD8,b.0xCC,b.0x14
08009414: 9D                            ifkret
08009415: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800941B: 9D                            ifkret
0800941C: 19 CD 85 47                   by move      $0x85,b.0x1C
08009420: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
08009426: 9D                            ifkret
08009427: 19 CD 86 47                   by move      $0x86,b.0x1C
0800942B: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
08009431: 9D                            ifkret
08009432: 19 CD 8C 47                   by move      $0x8C,b.0x1C
08009436: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
0800943C: 9D                            ifkret
0800943D: 19 CD 96 47                   by move      $0x96,b.0x1C
08009441: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
08009447: 9D                            ifkret
08009448: 19 CD 98 47                   by move      $0x98,b.0x1C
0800944C: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
08009452: 9D                            ifkret
08009453: 19 CD 9A 47                   by move      $0x9A,b.0x1C
08009457: C3 08 00 92 A4 00             call         $0x80092A4,$0x0
0800945D: 9D                            ifkret
0800945E: 80                            ret
0800945F: B8 CF 00 00 00 20             ents         $0x20
08009465: 18 42                         r:=          b.0x8
08009467: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
08009470: 0C 0C                         w1 :=        $0xC
08009472: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
08009478: 9D                            ifkret
08009479: 20 47                         w1 =:        b.0x1C
0800947B: 1A 47 46                      w move       b.0x1C,b.0x18
0800947E: 4A C5 18                      w stz        @b.0x18
08009481: 18 46                         r:=          b.0x18
08009483: 4A 81                         w stz        r.0x4
08009485: 4A 82                         w stz        r.0x8
08009487: 0C 46                         w1 :=        b.0x18
08009489: 80                            ret
0800948A: B8 CF 00 00 00 28             ents         $0x28
08009490: 18 42                         r:=          b.0x8
08009492: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
0800949B: 0C CE 02 28                   w1 :=        $0x228
0800949F: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080094A5: 9D                            ifkret
080094A6: 20 47                         w1 =:        b.0x1C
080094A8: 1A 47 46                      w move       b.0x1C,b.0x18
080094AB: FD 3C C5 18                   w1 laddr     @b.0x18
080094AF: 20 49                         w1 =:        b.0x24
080094B1: 85                            bi2 clr
080094B2: 1A CE 00 89 48                w move       $0x89,b.0x20
080094B7: CA 06                         if < go      $0x6
080094B9: 86                            bi3 clr
080094BA: FD 8A 48                      w3 sfill     b.0x20
080094BD: 18 46                         r:=          b.0x18
080094BF: 4A CA 02 24                   w stz        r.0x224
080094C3: 0C 46                         w1 :=        b.0x18
080094C5: 80                            ret
080094C6: B8 CF 00 00 00 20             ents         $0x20
080094CC: 18 42                         r:=          b.0x8
080094CE: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
080094D7: 0C 1C                         w1 :=        $0x1C
080094D9: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
080094DF: 9D                            ifkret
080094E0: 20 47                         w1 =:        b.0x1C
080094E2: 1A 47 46                      w move       b.0x1C,b.0x18
080094E5: 18 46                         r:=          b.0x18
080094E7: 1A 07 C5 18                   w move       $0x7,@b.0x18
080094EB: 1A 07 81                      w move       $0x7,r.0x4
080094EE: 4A 82                         w stz        r.0x8
080094F0: 4A 83                         w stz        r.0xC
080094F2: 4A 84                         w stz        r.0x10
080094F4: 4A 86                         w stz        r.0x18
080094F6: 44 C4 08 00 7A 1C             w test       $0x8007A1C
080094FC: C6 0B                         if >< go     $0xB
080094FE: 1A 46 C4 08 00 7A 1C          w move       b.0x18,$0x8007A1C
08009505: C0 0B                         go           $0xB
08009507: 18 C4 08 00 7A 20             r:=          $0x8007A20
0800950D: 1A 46 86                      w move       b.0x18,r.0x18
08009510: 1A 46 C4 08 00 7A 20          w move       b.0x18,$0x8007A20
08009517: 0C 46                         w1 :=        b.0x18
08009519: 80                            ret
0800951A: B8 CF 00 00 00 30             ents         $0x30
08009520: 1A 45 4B                      w move       b.0x14,b.0x2C
08009523: 84                            bi1 clr
08009524: 20 48                         w1 =:        b.0x20
08009526: 20 4A                         w1 =:        b.0x28
08009528: 20 49                         w1 =:        b.0x24
0800952A: FD 3D C5 2C                   w2 laddr     @b.0x2C
0800952E: 55 48                         w2 +         b.0x20
08009530: 04 F5 00                      by1 :=       r2.(0x0)
08009533: 1C 47                         by1 =:       b.0x1C
08009535: 30 0D                         by1 comp     $0xD
08009537: C4 32                         if = go      $0x32
08009539: 30 09                         by1 comp     $0x9
0800953B: C4 16                         if = go      $0x16
0800953D: FC 3C CD 20                   by1 -        $0x20
08009541: FD 4B D0 D2                   by wconv     r1,r3
08009545: 0F 49                         w4 :=        b.0x24
08009547: 57 01                         w4 +         $0x1
08009549: 23 49                         w4 =:        b.0x24
0800954B: 6F D2                         w4 *         r3
0800954D: 57 4A                         w4 +         b.0x28
0800954F: 23 4A                         w4 =:        b.0x28
08009551: 0C 48                         w1 :=        b.0x20
08009553: 54 01                         w1 +         $0x1
08009555: 20 48                         w1 =:        b.0x20
08009557: 34 13                         w1 comp      $0x13
08009559: C8 07                         if > go      $0x7
0800955B: 2D 47 09                      by comp2     b.0x1C,$0x9
0800955E: C6 09                         if >< go     $0x9
08009560: 18 4B                         r:=          b.0x2C
08009562: 1A 85 4B                      w move       r.0x14,b.0x2C
08009565: 4A 48                         w stz        b.0x20
08009567: C0 C3                         go           $0xFFFFFFFFFFFFFFC3
08009569: 0C 4A                         w1 :=        b.0x28
0800956B: 80                            ret
0800956C: B8 CF 00 00 00 14             ents         $0x14
08009572: 44 C4 08 00 7A 10             w test       $0x8007A10
08009578: C6 18                         if >< go     $0x18
0800957A: 1A CE 00 89 C4 08 00 7A 10    w move       $0x89,$0x8007A10
08009583: C3 08 00 94 8A 00             call         $0x800948A,$0x0
08009589: 9D                            ifkret
0800958A: 20 C4 08 00 7A 68             w1 =:        $0x8007A68
08009590: 80                            ret
08009591: B8 CF 00 00 00 5C             ents         $0x5C
08009597: 18 42                         r:=          b.0x8
08009599: 1A 45 85                      w move       b.0x14,r.0x14
0800959C: C3 08 00 95 1A 00             call         $0x800951A,$0x0
080095A2: 9D                            ifkret
080095A3: 20 47                         w1 =:        b.0x1C
080095A5: 0D C4 08 00 7A 68             w2 :=        $0x8007A68
080095AB: 21 4E                         w2 =:        b.0x38
080095AD: 44 D1                         w test       r2
080095AF: C5 00 F7                      if = go      $0xF7
080095B2: FC 7E D0 C4 08 00 7A 10 D3    w3 div4      r1,$0x8007A10,r4
080095BB: 22 4B                         w3 =:        b.0x2C
080095BD: 2E 4B CE 00 88                w comp2      b.0x2C,$0x88
080095C2: CE 10                         if <= go     $0x10
080095C4: 18 4E                         r:=          b.0x38
080095C6: 1A CA 02 24 4E                w move       r.0x224,b.0x38
080095CB: E0 4B CE 00 89                w sub2       b.0x2C,$0x89
080095D0: C0 ED                         go           $0xFFFFFFFFFFFFFFED
080095D2: FD 3C C5 38                   w1 laddr     @b.0x38
080095D6: 20 52                         w1 =:        b.0x48
080095D8: 0D 4B                         w2 :=        b.0x2C
080095DA: 1A E5 48 4F                   w move       @b.0x48+,b.0x3C
080095DE: 44 4F                         w test       b.0x3C
080095E0: C5 00 C6                      if = go      $0xC6
080095E3: 1A 45 50                      w move       b.0x14,b.0x40
080095E6: 18 4F                         r:=          b.0x3C
080095E8: 18 81                         r:=          r.0x4
080095EA: 1A 83 51                      w move       r.0xC,b.0x44
080095ED: 44 46                         w test       b.0x18
080095EF: C4 49                         if = go      $0x49
080095F1: FD 3C C5 40                   w1 laddr     @b.0x40
080095F5: 20 54                         w1 =:        b.0x50
080095F7: 1A 14 53                      w move       $0x14,b.0x4C
080095FA: FD 3D C5 44                   w2 laddr     @b.0x44
080095FE: 21 56                         w2 =:        b.0x58
08009600: 1A 14 55                      w move       $0x14,b.0x54
08009603: 84                            bi1 clr
08009604: 85                            bi2 clr
08009605: FD BE 53 55 00                by scopa     b.0x4C,b.0x54,$0x0
0800960A: C6 2C                         if >< go     $0x2C
0800960C: 18 50                         r:=          b.0x40
0800960E: 0C 85                         w1 :=        r.0x14
08009610: 20 50                         w1 =:        b.0x40
08009612: 44 D0                         w test       r1
08009614: C6 13                         if >< go     $0x13
08009616: 18 51                         r:=          b.0x44
08009618: 0D 85                         w2 :=        r.0x14
0800961A: 21 51                         w2 =:        b.0x44
0800961C: 44 D1                         w test       r2
0800961E: C6 07                         if >< go     $0x7
08009620: 18 4F                         r:=          b.0x3C
08009622: 0C 81                         w1 :=        r.0x4
08009624: 80                            ret
08009625: C0 07                         go           $0x7
08009627: 18 51                         r:=          b.0x44
08009629: 1A 85 51                      w move       r.0x14,b.0x44
0800962C: 44 50                         w test       b.0x40
0800962E: C4 08                         if = go      $0x8
08009630: 44 51                         w test       b.0x44
08009632: C4 04                         if = go      $0x4
08009634: C0 BD                         go           $0xFFFFFFFFFFFFFFBD
08009636: C0 66                         go           $0x66
08009638: 84                            bi1 clr
08009639: 20 4C                         w1 =:        b.0x30
0800963B: 20 4D                         w1 =:        b.0x34
0800963D: FD 3D C5 40                   w2 laddr     @b.0x40
08009641: 55 4C                         w2 +         b.0x30
08009643: 04 F5 00                      by1 :=       r2.(0x0)
08009646: 1C 4A                         by1 =:       b.0x28
08009648: 30 09                         by1 comp     $0x9
0800964A: C6 0B                         if >< go     $0xB
0800964C: 18 50                         r:=          b.0x40
0800964E: 1A 85 50                      w move       r.0x14,b.0x40
08009651: 4A 4C                         w stz        b.0x30
08009653: C0 EA                         go           $0xFFFFFFFFFFFFFFEA
08009655: 4A 52                         w stz        b.0x48
08009657: FD 3E C5 44                   w3 laddr     @b.0x44
0800965B: 56 4D                         w3 +         b.0x34
0800965D: 30 F6 00                      by1 comp     r3.(0x0)
08009660: C6 04                         if >< go     $0x4
08009662: 4D 52                         w set1       b.0x48
08009664: 1A 52 49                      w move       b.0x48,b.0x24
08009667: 44 49                         w test       b.0x24
08009669: C4 2A                         if = go      $0x2A
0800966B: 30 0D                         by1 comp     $0xD
0800966D: C4 26                         if = go      $0x26
0800966F: 0F 4C                         w4 :=        b.0x30
08009671: 57 01                         w4 +         $0x1
08009673: 23 4C                         w4 =:        b.0x30
08009675: 37 13                         w4 comp      $0x13
08009677: CE 09                         if <= go     $0x9
08009679: 18 50                         r:=          b.0x40
0800967B: 1A 85 50                      w move       r.0x14,b.0x40
0800967E: 4A 4C                         w stz        b.0x30
08009680: 0F 4D                         w4 :=        b.0x34
08009682: 57 01                         w4 +         $0x1
08009684: 23 4D                         w4 =:        b.0x34
08009686: 37 13                         w4 comp      $0x13
08009688: CE 09                         if <= go     $0x9
0800968A: 18 51                         r:=          b.0x44
0800968C: 1A 85 51                      w move       r.0x14,b.0x44
0800968F: 4A 4D                         w stz        b.0x34
08009691: C0 AC                         go           $0xFFFFFFFFFFFFFFAC
08009693: 44 49                         w test       b.0x24
08009695: C4 07                         if = go      $0x7
08009697: 18 4F                         r:=          b.0x3C
08009699: 0C 81                         w1 :=        r.0x4
0800969B: 80                            ret
0800969C: 18 4F                         r:=          b.0x3C
0800969E: 1A 82 4F                      w move       r.0x8,b.0x3C
080096A1: 44 4F                         w test       b.0x3C
080096A3: C7 FF 40                      if >< go     $0xFFFFFFFFFFFFFF40
080096A6: 84                            bi1 clr
080096A7: 80                            ret
080096A8: B8 CF 00 00 00 30             ents         $0x30
080096AE: 0C C4 08 00 7A 14             w1 :=        $0x8007A14
080096B4: 54 01                         w1 +         $0x1
080096B6: 20 C4 08 00 7A 14             w1 =:        $0x8007A14
080096BC: 34 C4 08 00 7A 10             w1 comp      $0x8007A10
080096C2: CE 09                         if <= go     $0x9
080096C4: C3 08 00 95 6C 00             call         $0x800956C,$0x0
080096CA: 9D                            ifkret
080096CB: 1A C4 08 00 7A 68 49          w move       $0x8007A68,b.0x24
080096D2: FC 7C 46 C4 08 00 7A 10 D1    w1 div4      b.0x18,$0x8007A10,r2
080096DB: 20 48                         w1 =:        b.0x20
080096DD: 2E 48 CE 00 88                w comp2      b.0x20,$0x88
080096E2: CE 10                         if <= go     $0x10
080096E4: 18 49                         r:=          b.0x24
080096E6: 1A CA 02 24 49                w move       r.0x224,b.0x24
080096EB: E0 48 CE 00 89                w sub2       b.0x20,$0x89
080096F0: C0 ED                         go           $0xFFFFFFFFFFFFFFED
080096F2: C3 08 00 94 5F 00             call         $0x800945F,$0x0
080096F8: 9D                            ifkret
080096F9: 20 4A                         w1 =:        b.0x28
080096FB: 1A 46 F4 00                   w move       b.0x18,r1.(0x0)
080096FF: FD 3D C5 24                   w2 laddr     @b.0x24
08009703: 21 4B                         w2 =:        b.0x2C
08009705: 0E 48                         w3 :=        b.0x20
08009707: FD 3D E6 2C                   w2 laddr     @b.0x2C+
0800970B: 0C 4A                         w1 :=        b.0x28
0800970D: 0E 08                         w3 :=        $0x8
0800970F: FE 03                         clrk
08009711: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08009717: 9D                            ifkret
08009718: C3 08 00 94 C6 00             call         $0x80094C6,$0x0
0800971E: 9D                            ifkret
0800971F: 18 4A                         r:=          b.0x28
08009721: 20 81                         w1 =:        r.0x4
08009723: 0D 45                         w2 :=        b.0x14
08009725: 18 4A                         r:=          b.0x28
08009727: 18 81                         r:=          r.0x4
08009729: 21 83                         w2 =:        r.0xC
0800972B: 18 4A                         r:=          b.0x28
0800972D: 0C 81                         w1 :=        r.0x4
0800972F: 80                            ret
08009730: B8 CF 00 00 00 34             ents         $0x34
08009736: 1A 45 4B                      w move       b.0x14,b.0x2C
08009739: 4A 48                         w stz        b.0x20
0800973B: 4A 46                         w stz        b.0x18
0800973D: 4A 4A                         w stz        b.0x28
0800973F: FD 3C C5 2C                   w1 laddr     @b.0x2C
08009743: 54 48                         w1 +         b.0x20
08009745: 19 F4 00 47                   by move      r1.(0x0),b.0x1C
08009749: 0D 4A                         w2 :=        b.0x28
0800974B: B4 E1 08 00 8D 74             jumpg        $0x8008D74+
08009751: 2D 47 CD 3A                   by comp2     b.0x1C,$0x3A
08009755: C4 08                         if = go      $0x8
08009757: 2D 47 CD 3B                   by comp2     b.0x1C,$0x3B
0800975B: C6 07                         if >< go     $0x7
0800975D: 19 0D 47                      by move      $0xD,b.0x1C
08009760: C0 0A                         go           $0xA
08009762: 2D 47 CD 28                   by comp2     b.0x1C,$0x28
08009766: C6 04                         if >< go     $0x4
08009768: 4D 4A                         w set1       b.0x28
0800976A: C0 0C                         go           $0xC
0800976C: 2D 47 CD 29                   by comp2     b.0x1C,$0x29
08009770: C6 04                         if >< go     $0x4
08009772: 4A 4A                         w stz        b.0x28
08009774: C0 02                         go           $0x2
08009776: 44 46                         w test       b.0x18
08009778: C4 07                         if = go      $0x7
0800977A: 2E 49 13                      w comp2      b.0x24,$0x13
0800977D: C6 1E                         if >< go     $0x1E
0800977F: C3 08 00 86 91 00             call         $0x8008691,$0x0
08009785: 9D                            ifkret
08009786: 20 4C                         w1 =:        b.0x30
08009788: 1A 3F 49                      w move       $0x3F,b.0x24
0800978B: FD 3D 46                      w2 laddr     b.0x18
0800978E: 0C 4C                         w1 :=        b.0x30
08009790: 0E 14                         w3 :=        $0x14
08009792: FE 03                         clrk
08009794: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
0800979A: 9D                            ifkret
0800979B: 0C 49                         w1 :=        b.0x24
0800979D: 54 01                         w1 +         $0x1
0800979F: 20 49                         w1 =:        b.0x24
080097A1: 05 47                         by2 :=       b.0x1C
080097A3: FD 3E C5 30                   w3 laddr     @b.0x30
080097A7: 56 D0                         w3 +         r1
080097A9: 1D F6 00                      by2 =:       r3.(0x0)
080097AC: 31 0D                         by2 comp     $0xD
080097AE: C4 16                         if = go      $0x16
080097B0: 0F 48                         w4 :=        b.0x20
080097B2: 57 01                         w4 +         $0x1
080097B4: 23 48                         w4 =:        b.0x20
080097B6: 37 13                         w4 comp      $0x13
080097B8: CE 09                         if <= go     $0x9
080097BA: 18 4B                         r:=          b.0x2C
080097BC: 1A 85 4B                      w move       r.0x14,b.0x2C
080097BF: 4A 48                         w stz        b.0x20
080097C1: C1 FF 7E                      go           $0xFFFFFFFFFFFFFF7E
080097C4: 80                            ret
080097C5: B8 CF 00 00 00 34             ents         $0x34
080097CB: 1A 45 4B                      w move       b.0x14,b.0x2C
080097CE: 4A 49                         w stz        b.0x24
080097D0: 4A 46                         w stz        b.0x18
080097D2: 4A 47                         w stz        b.0x1C
080097D4: FD 3D C5 2C                   w2 laddr     @b.0x2C
080097D8: 55 49                         w2 +         b.0x24
080097DA: 04 F5 00                      by1 :=       r2.(0x0)
080097DD: 1C 48                         by1 =:       b.0x20
080097DF: 30 CD 29                      by1 comp     $0x29
080097E2: C4 07                         if = go      $0x7
080097E4: 30 CD 2E                      by1 comp     $0x2E
080097E7: C6 17                         if >< go     $0x17
080097E9: 18 42                         r:=          b.0x8
080097EB: 1A 46 85                      w move       b.0x18,r.0x14
080097EE: C3 08 00 87 17 00             call         $0x8008717,$0x0
080097F4: 9D                            ifkret
080097F5: 18 42                         r:=          b.0x8
080097F7: 1A 85 46                      w move       r.0x14,b.0x18
080097FA: 4A 47                         w stz        b.0x1C
080097FC: C0 52                         go           $0x52
080097FE: 30 CD 28                      by1 comp     $0x28
08009801: C6 06                         if >< go     $0x6
08009803: 4D 47                         w set1       b.0x1C
08009805: C0 49                         go           $0x49
08009807: 44 47                         w test       b.0x1C
08009809: C6 45                         if >< go     $0x45
0800980B: 30 CD 3A                      by1 comp     $0x3A
0800980E: C4 07                         if = go      $0x7
08009810: 30 CD 3B                      by1 comp     $0x3B
08009813: C6 05                         if >< go     $0x5
08009815: 19 0D 48                      by move      $0xD,b.0x20
08009818: 44 46                         w test       b.0x18
0800981A: C4 07                         if = go      $0x7
0800981C: 2E 4A 13                      w comp2      b.0x28,$0x13
0800981F: C6 1E                         if >< go     $0x1E
08009821: C3 08 00 86 91 00             call         $0x8008691,$0x0
08009827: 9D                            ifkret
08009828: 20 4C                         w1 =:        b.0x30
0800982A: 1A 3F 4A                      w move       $0x3F,b.0x28
0800982D: FD 3D 46                      w2 laddr     b.0x18
08009830: 0C 4C                         w1 :=        b.0x30
08009832: 0E 14                         w3 :=        $0x14
08009834: FE 03                         clrk
08009836: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
0800983C: 9D                            ifkret
0800983D: 0C 4A                         w1 :=        b.0x28
0800983F: 54 01                         w1 +         $0x1
08009841: 20 4A                         w1 =:        b.0x28
08009843: 05 48                         by2 :=       b.0x20
08009845: FD 3E C5 30                   w3 laddr     @b.0x30
08009849: 56 D0                         w3 +         r1
0800984B: 1D F6 00                      by2 =:       r3.(0x0)
0800984E: 2D 48 0D                      by comp2     b.0x20,$0xD
08009851: C4 16                         if = go      $0x16
08009853: 0C 49                         w1 :=        b.0x24
08009855: 54 01                         w1 +         $0x1
08009857: 20 49                         w1 =:        b.0x24
08009859: 34 13                         w1 comp      $0x13
0800985B: CE 09                         if <= go     $0x9
0800985D: 18 4B                         r:=          b.0x2C
0800985F: 1A 85 4B                      w move       r.0x14,b.0x2C
08009862: 4A 49                         w stz        b.0x24
08009864: C1 FF 70                      go           $0xFFFFFFFFFFFFFF70
08009867: 80                            ret
08009868: B8 CF 00 00 00 34             ents         $0x34
0800986E: 1A 45 4B                      w move       b.0x14,b.0x2C
08009871: 4A 48                         w stz        b.0x20
08009873: 4A 46                         w stz        b.0x18
08009875: 4A 4A                         w stz        b.0x28
08009877: FD 3C C5 2C                   w1 laddr     @b.0x2C
0800987B: 54 48                         w1 +         b.0x20
0800987D: 19 F4 00 47                   by move      r1.(0x0),b.0x1C
08009881: 0D 4A                         w2 :=        b.0x28
08009883: B4 E1 08 00 8D 7C             jumpg        $0x8008D7C+
08009889: 2D 47 CD 28                   by comp2     b.0x1C,$0x28
0800988D: C6 06                         if >< go     $0x6
0800988F: 4D 4A                         w set1       b.0x28
08009891: C0 14                         go           $0x14
08009893: 2D 47 CD 3A                   by comp2     b.0x1C,$0x3A
08009897: C6 07                         if >< go     $0x7
08009899: 1A 02 4A                      w move       $0x2,b.0x28
0800989C: C0 09                         go           $0x9
0800989E: 2D 47 CD 3B                   by comp2     b.0x1C,$0x3B
080098A2: C6 03                         if >< go     $0x3
080098A4: 80                            ret
080098A5: C0 4D                         go           $0x4D
080098A7: 2D 47 CD 29                   by comp2     b.0x1C,$0x29
080098AB: C6 04                         if >< go     $0x4
080098AD: 4A 4A                         w stz        b.0x28
080098AF: C0 43                         go           $0x43
080098B1: 2D 47 CD 3B                   by comp2     b.0x1C,$0x3B
080098B5: C6 05                         if >< go     $0x5
080098B7: 19 0D 47                      by move      $0xD,b.0x1C
080098BA: 44 46                         w test       b.0x18
080098BC: C4 07                         if = go      $0x7
080098BE: 2E 49 13                      w comp2      b.0x24,$0x13
080098C1: C6 1E                         if >< go     $0x1E
080098C3: C3 08 00 86 91 00             call         $0x8008691,$0x0
080098C9: 9D                            ifkret
080098CA: 20 4C                         w1 =:        b.0x30
080098CC: 1A 3F 49                      w move       $0x3F,b.0x24
080098CF: FD 3D 46                      w2 laddr     b.0x18
080098D2: 0C 4C                         w1 :=        b.0x30
080098D4: 0E 14                         w3 :=        $0x14
080098D6: FE 03                         clrk
080098D8: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
080098DE: 9D                            ifkret
080098DF: 0C 49                         w1 :=        b.0x24
080098E1: 54 01                         w1 +         $0x1
080098E3: 20 49                         w1 =:        b.0x24
080098E5: 05 47                         by2 :=       b.0x1C
080098E7: FD 3E C5 30                   w3 laddr     @b.0x30
080098EB: 56 D0                         w3 +         r1
080098ED: 1D F6 00                      by2 =:       r3.(0x0)
080098F0: C0 02                         go           $0x2
080098F2: 2D 47 0D                      by comp2     b.0x1C,$0xD
080098F5: C4 16                         if = go      $0x16
080098F7: 0C 48                         w1 :=        b.0x20
080098F9: 54 01                         w1 +         $0x1
080098FB: 20 48                         w1 =:        b.0x20
080098FD: 34 13                         w1 comp      $0x13
080098FF: CE 09                         if <= go     $0x9
08009901: 18 4B                         r:=          b.0x2C
08009903: 1A 85 4B                      w move       r.0x14,b.0x2C
08009906: 4A 48                         w stz        b.0x20
08009908: C1 FF 6F                      go           $0xFFFFFFFFFFFFFF6F
0800990B: 80                            ret
0800990C: 9C                            entd
0800990D: FD C0 48                      l=:          b.0x20
08009910: 44 C4 08 00 7A 18             w test       $0x8007A18
08009916: C6 4D                         if >< go     $0x4D
08009918: 44 C4 08 00 79 F8             w test       $0x80079F8
0800991E: C6 0C                         if >< go     $0xC
08009920: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08009926: D2 04                         if -k go     $0x4
08009928: B4 48                         jumpg        b.0x20
0800992A: 18 42                         r:=          b.0x8
0800992C: FE 79 C4 08 00 8D 90 85 03    w bmove      $0x8008D90,r.0x14,$0x3
08009935: 1A 45 88                      w move       b.0x14,r.0x20
08009938: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
0800993E: D2 04                         if -k go     $0x4
08009940: B4 48                         jumpg        b.0x20
08009942: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
08009948: D2 04                         if -k go     $0x4
0800994A: B4 48                         jumpg        b.0x20
0800994C: 0C CD B9                      w1 :=        $0xB9
0800994F: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
08009955: D2 04                         if -k go     $0x4
08009957: B4 48                         jumpg        b.0x20
08009959: 0C CD B9                      w1 :=        $0xB9
0800995C: FE 02                         setk
0800995E: B4 48                         jumpg        b.0x20
08009960: C1 01 B5                      go           $0x1B5
08009963: 18 42                         r:=          b.0x8
08009965: 1A 45 85                      w move       b.0x14,r.0x14
08009968: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800996E: D2 04                         if -k go     $0x4
08009970: B4 48                         jumpg        b.0x20
08009972: 18 42                         r:=          b.0x8
08009974: 1A 85 45                      w move       r.0x14,b.0x14
08009977: 18 47                         r:=          b.0x1C
08009979: 0D 82                         w2 :=        r.0x8
0800997B: 21 4C                         w2 =:        b.0x30
0800997D: B4 E1 08 00 8D CC             jumpg        $0x8008DCC+
08009983: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009989: 1A 86 4B                      w move       r.0x18,b.0x2C
0800998C: 44 4B                         w test       b.0x2C
0800998E: C4 34                         if = go      $0x34
08009990: 18 C5 2C                      r:=          @b.0x2C
08009993: 0C 85                         w1 :=        r.0x14
08009995: 18 42                         r:=          b.0x8
08009997: 20 85                         w1 =:        r.0x14
08009999: C3 08 00 97 30 00             call         $0x8009730,$0x0
0800999F: D2 04                         if -k go     $0x4
080099A1: B4 48                         jumpg        b.0x20
080099A3: 18 42                         r:=          b.0x8
080099A5: 1A 86 49                      w move       r.0x18,b.0x24
080099A8: 44 45                         w test       b.0x14
080099AA: C6 07                         if >< go     $0x7
080099AC: 1A 49 45                      w move       b.0x24,b.0x14
080099AF: C0 07                         go           $0x7
080099B1: 18 4A                         r:=          b.0x28
080099B3: 1A 49 86                      w move       b.0x24,r.0x18
080099B6: 1A 49 4A                      w move       b.0x24,b.0x28
080099B9: 18 4B                         r:=          b.0x2C
080099BB: 1A 81 4B                      w move       r.0x4,b.0x2C
080099BE: 44 4B                         w test       b.0x2C
080099C0: C6 D0                         if >< go     $0xFFFFFFFFFFFFFFD0
080099C2: C1 01 53                      go           $0x153
080099C5: 18 C4 08 00 7A 18             r:=          $0x8007A18
080099CB: 0C 86                         w1 :=        r.0x18
080099CD: 20 4B                         w1 =:        b.0x2C
080099CF: 44 D0                         w test       r1
080099D1: C4 1A                         if = go      $0x1A
080099D3: 18 F4 00                      r:=          r1.(0x0)
080099D6: 0D 85                         w2 :=        r.0x14
080099D8: 18 42                         r:=          b.0x8
080099DA: 21 85                         w2 =:        r.0x14
080099DC: C3 08 00 97 30 00             call         $0x8009730,$0x0
080099E2: D2 04                         if -k go     $0x4
080099E4: B4 48                         jumpg        b.0x20
080099E6: 18 42                         r:=          b.0x8
080099E8: 1A 86 45                      w move       r.0x18,b.0x14
080099EB: C1 01 2A                      go           $0x12A
080099EE: 18 C4 08 00 7A 18             r:=          $0x8007A18
080099F4: 1A 86 4B                      w move       r.0x18,b.0x2C
080099F7: 44 4B                         w test       b.0x2C
080099F9: C4 34                         if = go      $0x34
080099FB: 18 C5 2C                      r:=          @b.0x2C
080099FE: 0C 85                         w1 :=        r.0x14
08009A00: 18 42                         r:=          b.0x8
08009A02: 20 85                         w1 =:        r.0x14
08009A04: C3 08 00 98 68 00             call         $0x8009868,$0x0
08009A0A: D2 04                         if -k go     $0x4
08009A0C: B4 48                         jumpg        b.0x20
08009A0E: 18 42                         r:=          b.0x8
08009A10: 1A 86 49                      w move       r.0x18,b.0x24
08009A13: 44 45                         w test       b.0x14
08009A15: C6 07                         if >< go     $0x7
08009A17: 1A 49 45                      w move       b.0x24,b.0x14
08009A1A: C0 07                         go           $0x7
08009A1C: 18 4A                         r:=          b.0x28
08009A1E: 1A 49 86                      w move       b.0x24,r.0x18
08009A21: 1A 49 4A                      w move       b.0x24,b.0x28
08009A24: 18 4B                         r:=          b.0x2C
08009A26: 1A 81 4B                      w move       r.0x4,b.0x2C
08009A29: 44 4B                         w test       b.0x2C
08009A2B: C6 D0                         if >< go     $0xFFFFFFFFFFFFFFD0
08009A2D: C1 00 E8                      go           $0xE8
08009A30: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009A36: 0C 86                         w1 :=        r.0x18
08009A38: 20 4B                         w1 =:        b.0x2C
08009A3A: 44 D0                         w test       r1
08009A3C: C4 1A                         if = go      $0x1A
08009A3E: 18 F4 00                      r:=          r1.(0x0)
08009A41: 0D 85                         w2 :=        r.0x14
08009A43: 18 42                         r:=          b.0x8
08009A45: 21 85                         w2 =:        r.0x14
08009A47: C3 08 00 98 68 00             call         $0x8009868,$0x0
08009A4D: D2 04                         if -k go     $0x4
08009A4F: B4 48                         jumpg        b.0x20
08009A51: 18 42                         r:=          b.0x8
08009A53: 1A 86 45                      w move       r.0x18,b.0x14
08009A56: C1 00 BF                      go           $0xBF
08009A59: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009A5F: 0C 85                         w1 :=        r.0x14
08009A61: 18 42                         r:=          b.0x8
08009A63: 20 85                         w1 =:        r.0x14
08009A65: C3 08 00 97 30 00             call         $0x8009730,$0x0
08009A6B: D2 04                         if -k go     $0x4
08009A6D: B4 48                         jumpg        b.0x20
08009A6F: 18 42                         r:=          b.0x8
08009A71: 1A 86 45                      w move       r.0x18,b.0x14
08009A74: C1 00 A1                      go           $0xA1
08009A77: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009A7D: 0C 85                         w1 :=        r.0x14
08009A7F: 18 42                         r:=          b.0x8
08009A81: 20 85                         w1 =:        r.0x14
08009A83: 1A 45 86                      w move       b.0x14,r.0x18
08009A86: C3 08 00 97 C5 00             call         $0x80097C5,$0x0
08009A8C: D2 04                         if -k go     $0x4
08009A8E: B4 48                         jumpg        b.0x20
08009A90: 18 42                         r:=          b.0x8
08009A92: 1A 86 45                      w move       r.0x18,b.0x14
08009A95: C1 00 80                      go           $0x80
08009A98: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009A9E: 0C 85                         w1 :=        r.0x14
08009AA0: 18 42                         r:=          b.0x8
08009AA2: 20 85                         w1 =:        r.0x14
08009AA4: C3 08 00 98 68 00             call         $0x8009868,$0x0
08009AAA: D2 04                         if -k go     $0x4
08009AAC: B4 48                         jumpg        b.0x20
08009AAE: 18 42                         r:=          b.0x8
08009AB0: 1A 86 45                      w move       r.0x18,b.0x14
08009AB3: C0 62                         go           $0x62
08009AB5: C3 08 00 86 91 00             call         $0x8008691,$0x0
08009ABB: D2 04                         if -k go     $0x4
08009ABD: B4 48                         jumpg        b.0x20
08009ABF: 20 45                         w1 =:        b.0x14
08009AC1: 18 C4 08 00 7A 18             r:=          $0x8007A18
08009AC7: 44 80                         w test       r.0x0
08009AC9: C4 0E                         if = go      $0xE
08009ACB: 05 CD AA                      by2 :=       $0xAA
08009ACE: FD 3E F4 00                   w3 laddr     r1.(0x0)
08009AD2: 1D F6 00                      by2 =:       r3.(0x0)
08009AD5: C0 0C                         go           $0xC
08009AD7: 05 CD A7                      by2 :=       $0xA7
08009ADA: FD 3F F4 00                   w4 laddr     r1.(0x0)
08009ADE: 1D F7 00                      by2 =:       r4.(0x0)
08009AE1: 05 0D                         by2 :=       $0xD
08009AE3: FD 3E F4 00                   w3 laddr     r1.(0x0)
08009AE7: 56 01                         w3 +         $0x1
08009AE9: 1D F6 00                      by2 =:       r3.(0x0)
08009AEC: C0 29                         go           $0x29
08009AEE: 18 42                         r:=          b.0x8
08009AF0: 4D 85                         w set1       r.0x14
08009AF2: FE 79 C4 08 00 8D B4 86 03    w bmove      $0x8008DB4,r.0x18,$0x3
08009AFB: FE 79 C4 08 00 8D C0 89 03    w bmove      $0x8008DC0,r.0x24,$0x3
08009B04: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
08009B0A: D2 04                         if -k go     $0x4
08009B0C: B4 48                         jumpg        b.0x20
08009B0E: 0C CD C6                      w1 :=        $0xC6
08009B11: FE 02                         setk
08009B13: B4 48                         jumpg        b.0x20
08009B15: FE 03                         clrk
08009B17: B4 48                         jumpg        b.0x20
08009B19: 9C                            entd
08009B1A: FD C0 54                      l=:          b.0x50
08009B1D: 0C 55                         w1 :=        b.0x54
08009B1F: 78 CE 03 E8                   w1 /         $0x3E8
08009B23: 54 CD 30                      w1 +         $0x30
08009B26: 85                            bi2 clr
08009B27: 1C E5 58                      by1 =:       @b.0x58+
08009B2A: 0E 55                         w3 :=        b.0x54
08009B2C: 7A CD 64                      w3 /         $0x64
08009B2F: FC 7F D2 0A D0                w4 div4      r3,$0xA,r1
08009B34: 57 CD 30                      w4 +         $0x30
08009B37: 0D 01                         w2 :=        $0x1
08009B39: 1F E5 58                      by4 =:       @b.0x58+
08009B3C: 0E 55                         w3 :=        b.0x54
08009B3E: 7A 0A                         w3 /         $0xA
08009B40: FC 7C D2 0A D3                w1 div4      r3,$0xA,r4
08009B45: 54 CD 30                      w1 +         $0x30
08009B48: 0D 02                         w2 :=        $0x2
08009B4A: 1C E5 58                      by1 =:       @b.0x58+
08009B4D: FC 7E 55 0A D3                w3 div4      b.0x54,$0xA,r4
08009B52: 56 CD 30                      w3 +         $0x30
08009B55: 0C 03                         w1 :=        $0x3
08009B57: 1E E4 58                      by3 =:       @b.0x58+
08009B5A: 05 0D                         by2 :=       $0xD
08009B5C: 0F 04                         w4 :=        $0x4
08009B5E: 1D E7 58                      by2 =:       @b.0x58+
08009B61: FE 03                         clrk
08009B63: B4 54                         jumpg        b.0x50
08009B65: 9C                            entd
08009B66: FD C0 59                      l=:          b.0x64
08009B69: 0C 5A                         w1 :=        b.0x68
08009B6B: 78 0A                         w1 /         $0xA
08009B6D: 54 CD 30                      w1 +         $0x30
08009B70: 85                            bi2 clr
08009B71: 1C E5 78                      by1 =:       @b.0x78+
08009B74: FC 7E 5A 0A D3                w3 div4      b.0x68,$0xA,r4
08009B79: 56 CD 30                      w3 +         $0x30
08009B7C: 0C 01                         w1 :=        $0x1
08009B7E: 1E E4 78                      by3 =:       @b.0x78+
08009B81: 05 5D                         by2 :=       b.0x74
08009B83: 0F 02                         w4 :=        $0x2
08009B85: 1D E7 78                      by2 =:       @b.0x78+
08009B88: 0E 5B                         w3 :=        b.0x6C
08009B8A: 7A 0A                         w3 /         $0xA
08009B8C: 56 CD 30                      w3 +         $0x30
08009B8F: 0C 03                         w1 :=        $0x3
08009B91: 1E E4 78                      by3 =:       @b.0x78+
08009B94: FC 7D 5B 0A D3                w2 div4      b.0x6C,$0xA,r4
08009B99: 55 CD 30                      w2 +         $0x30
08009B9C: 0E 04                         w3 :=        $0x4
08009B9E: 1D E6 78                      by2 =:       @b.0x78+
08009BA1: 04 5D                         by1 :=       b.0x74
08009BA3: 0F 05                         w4 :=        $0x5
08009BA5: 1C E7 78                      by1 =:       @b.0x78+
08009BA8: 0D 5C                         w2 :=        b.0x70
08009BAA: 79 0A                         w2 /         $0xA
08009BAC: 55 CD 30                      w2 +         $0x30
08009BAF: 0E 06                         w3 :=        $0x6
08009BB1: 1D E6 78                      by2 =:       @b.0x78+
08009BB4: FC 7C 5C 0A D3                w1 div4      b.0x70,$0xA,r4
08009BB9: 54 CD 30                      w1 +         $0x30
08009BBC: 0D 07                         w2 :=        $0x7
08009BBE: 1C E5 78                      by1 =:       @b.0x78+
08009BC1: 06 0D                         by3 :=       $0xD
08009BC3: 0F 08                         w4 :=        $0x8
08009BC5: 1E E7 78                      by3 =:       @b.0x78+
08009BC8: FE 03                         clrk
08009BCA: B4 59                         jumpg        b.0x64
08009BCC: 9C                            entd
08009BCD: FD C0 4D                      l=:          b.0x34
08009BD0: 18 45                         r:=          b.0x14
08009BD2: 0C 85                         w1 :=        r.0x14
08009BD4: 18 42                         r:=          b.0x8
08009BD6: 20 85                         w1 =:        r.0x14
08009BD8: C3 08 00 87 17 00             call         $0x8008717,$0x0
08009BDE: D2 04                         if -k go     $0x4
08009BE0: B4 4D                         jumpg        b.0x34
08009BE2: 18 42                         r:=          b.0x8
08009BE4: 0D 85                         w2 :=        r.0x14
08009BE6: 18 45                         r:=          b.0x14
08009BE8: 21 85                         w2 =:        r.0x14
08009BEA: 18 47                         r:=          b.0x1C
08009BEC: 0E 82                         w3 :=        r.0x8
08009BEE: 22 61                         w3 =:        b.0x84
08009BF0: B4 E2 08 00 8E 84             jumpg        $0x8008E84+
08009BF6: C3 08 00 83 74 00             call         $0x8008374,$0x0
08009BFC: D2 04                         if -k go     $0x4
08009BFE: B4 4D                         jumpg        b.0x34
08009C00: 18 42                         r:=          b.0x8
08009C02: 1A 85 4E                      w move       r.0x14,b.0x38
08009C05: 1A 86 4F                      w move       r.0x18,b.0x3C
08009C08: 1A 87 50                      w move       r.0x1C,b.0x40
08009C0B: 1A 4E 5A                      w move       b.0x38,b.0x68
08009C0E: 1A 4F 5B                      w move       b.0x3C,b.0x6C
08009C11: 1A 50 5C                      w move       b.0x40,b.0x70
08009C14: 19 CD 2D 5D                   by move      $0x2D,b.0x74
08009C18: FD 3D C5 14                   w2 laddr     @b.0x14
08009C1C: 21 5E                         w2 =:        b.0x78
08009C1E: 4A 5F                         w stz        b.0x7C
08009C20: 1A 13 60                      w move       $0x13,b.0x80
08009C23: C3 08 00 9B 65 00             call         $0x8009B65,$0x0
08009C29: D2 04                         if -k go     $0x4
08009C2B: B4 4D                         jumpg        b.0x34
08009C2D: C1 03 73                      go           $0x373
08009C30: 18 42                         r:=          b.0x8
08009C32: 19 CD B3 85                   by move      $0xB3,r.0x14
08009C36: 04 CD 86                      by1 :=       $0x86
08009C39: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009C3F: D2 04                         if -k go     $0x4
08009C41: B4 4D                         jumpg        b.0x34
08009C43: 44 D0                         w test       r1
08009C45: C4 2C                         if = go      $0x2C
08009C47: 0C CE 08 64                   w1 :=        $0x864
08009C4B: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009C52: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009C57: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009C5C: 4F 62                         w incr       b.0x88
08009C5E: FD 3D C5 14                   w2 laddr     @b.0x14
08009C62: 21 65                         w2 =:        b.0x94
08009C64: 1A 14 64                      w move       $0x14,b.0x90
08009C67: CA 08                         if < go      $0x8
08009C69: 84                            bi1 clr
08009C6A: 85                            bi2 clr
08009C6B: FD 67 62 64                   by smove     b.0x88,b.0x90
08009C6F: C0 2A                         go           $0x2A
08009C71: 0C CE 08 58                   w1 :=        $0x858
08009C75: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009C7C: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009C81: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009C86: 4F 62                         w incr       b.0x88
08009C88: FD 3D C5 14                   w2 laddr     @b.0x14
08009C8C: 21 65                         w2 =:        b.0x94
08009C8E: 1A 14 64                      w move       $0x14,b.0x90
08009C91: CA 08                         if < go      $0x8
08009C93: 84                            bi1 clr
08009C94: 85                            bi2 clr
08009C95: FD 67 62 64                   by smove     b.0x88,b.0x90
08009C99: C1 03 07                      go           $0x307
08009C9C: 1A C4 08 00 7A 08 55          w move       $0x8007A08,b.0x54
08009CA3: FD 3C C5 14                   w1 laddr     @b.0x14
08009CA7: 20 56                         w1 =:        b.0x58
08009CA9: 4A 57                         w stz        b.0x5C
08009CAB: 1A 13 58                      w move       $0x13,b.0x60
08009CAE: C3 08 00 9B 19 00             call         $0x8009B19,$0x0
08009CB4: D2 04                         if -k go     $0x4
08009CB6: B4 4D                         jumpg        b.0x34
08009CB8: C1 02 E8                      go           $0x2E8
08009CBB: 18 42                         r:=          b.0x8
08009CBD: 19 CD AB 85                   by move      $0xAB,r.0x14
08009CC1: 04 CD 96                      by1 :=       $0x96
08009CC4: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009CCA: D2 04                         if -k go     $0x4
08009CCC: B4 4D                         jumpg        b.0x34
08009CCE: 44 D0                         w test       r1
08009CD0: C4 2C                         if = go      $0x2C
08009CD2: 0C CE 08 04                   w1 :=        $0x804
08009CD6: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009CDD: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009CE2: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009CE7: 4F 62                         w incr       b.0x88
08009CE9: FD 3D C5 14                   w2 laddr     @b.0x14
08009CED: 21 65                         w2 =:        b.0x94
08009CEF: 1A 14 64                      w move       $0x14,b.0x90
08009CF2: CA 08                         if < go      $0x8
08009CF4: 84                            bi1 clr
08009CF5: 85                            bi2 clr
08009CF6: FD 67 62 64                   by smove     b.0x88,b.0x90
08009CFA: C0 6B                         go           $0x6B
08009CFC: 18 42                         r:=          b.0x8
08009CFE: 19 CD AC 85                   by move      $0xAC,r.0x14
08009D02: 04 CD 96                      by1 :=       $0x96
08009D05: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009D0B: D2 04                         if -k go     $0x4
08009D0D: B4 4D                         jumpg        b.0x34
08009D0F: 44 D0                         w test       r1
08009D11: C4 2C                         if = go      $0x2C
08009D13: 0C CE 08 10                   w1 :=        $0x810
08009D17: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009D1E: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009D23: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009D28: 4F 62                         w incr       b.0x88
08009D2A: FD 3D C5 14                   w2 laddr     @b.0x14
08009D2E: 21 65                         w2 =:        b.0x94
08009D30: 1A 14 64                      w move       $0x14,b.0x90
08009D33: CA 08                         if < go      $0x8
08009D35: 84                            bi1 clr
08009D36: 85                            bi2 clr
08009D37: FD 67 62 64                   by smove     b.0x88,b.0x90
08009D3B: C0 2A                         go           $0x2A
08009D3D: 0C CE 08 1C                   w1 :=        $0x81C
08009D41: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009D48: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009D4D: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009D52: 4F 62                         w incr       b.0x88
08009D54: FD 3D C5 14                   w2 laddr     @b.0x14
08009D58: 21 65                         w2 =:        b.0x94
08009D5A: 1A 14 64                      w move       $0x14,b.0x90
08009D5D: CA 08                         if < go      $0x8
08009D5F: 84                            bi1 clr
08009D60: 85                            bi2 clr
08009D61: FD 67 62 64                   by smove     b.0x88,b.0x90
08009D65: C1 02 3B                      go           $0x23B
08009D68: 18 42                         r:=          b.0x8
08009D6A: 19 CD AB 85                   by move      $0xAB,r.0x14
08009D6E: 04 CD 96                      by1 :=       $0x96
08009D71: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009D77: D2 04                         if -k go     $0x4
08009D79: B4 4D                         jumpg        b.0x34
08009D7B: 44 D0                         w test       r1
08009D7D: C4 19                         if = go      $0x19
08009D7F: FD 3C C5 14                   w1 laddr     @b.0x14
08009D83: 20 63                         w1 =:        b.0x8C
08009D85: 1A 14 62                      w move       $0x14,b.0x88
08009D88: CA 0C                         if < go      $0xC
08009D8A: 84                            bi1 clr
08009D8B: 85                            bi2 clr
08009D8C: FD 67 C4 08 00 8E 18 62       by smove     $0x8008E18,b.0x88
08009D94: C0 17                         go           $0x17
08009D96: FD 3C C5 14                   w1 laddr     @b.0x14
08009D9A: 20 63                         w1 =:        b.0x8C
08009D9C: 1A 14 62                      w move       $0x14,b.0x88
08009D9F: CA 0C                         if < go      $0xC
08009DA1: 84                            bi1 clr
08009DA2: 85                            bi2 clr
08009DA3: FD 67 C4 08 00 8E 20 62       by smove     $0x8008E20,b.0x88
08009DAB: C1 01 F5                      go           $0x1F5
08009DAE: 18 42                         r:=          b.0x8
08009DB0: 19 CD AB 85                   by move      $0xAB,r.0x14
08009DB4: 04 CD 96                      by1 :=       $0x96
08009DB7: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009DBD: D2 04                         if -k go     $0x4
08009DBF: B4 4D                         jumpg        b.0x34
08009DC1: 44 D0                         w test       r1
08009DC3: C4 19                         if = go      $0x19
08009DC5: FD 3C C5 14                   w1 laddr     @b.0x14
08009DC9: 20 63                         w1 =:        b.0x8C
08009DCB: 1A 14 62                      w move       $0x14,b.0x88
08009DCE: CA 0C                         if < go      $0xC
08009DD0: 84                            bi1 clr
08009DD1: 85                            bi2 clr
08009DD2: FD 67 C4 08 00 8E 28 62       by smove     $0x8008E28,b.0x88
08009DDA: C0 17                         go           $0x17
08009DDC: FD 3C C5 14                   w1 laddr     @b.0x14
08009DE0: 20 63                         w1 =:        b.0x8C
08009DE2: 1A 14 62                      w move       $0x14,b.0x88
08009DE5: CA 0C                         if < go      $0xC
08009DE7: 84                            bi1 clr
08009DE8: 85                            bi2 clr
08009DE9: FD 67 C4 08 00 8E 30 62       by smove     $0x8008E30,b.0x88
08009DF1: C1 01 AF                      go           $0x1AF
08009DF4: 18 42                         r:=          b.0x8
08009DF6: 19 CD B0 85                   by move      $0xB0,r.0x14
08009DFA: 04 CD 9A                      by1 :=       $0x9A
08009DFD: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009E03: D2 04                         if -k go     $0x4
08009E05: B4 4D                         jumpg        b.0x34
08009E07: 44 D0                         w test       r1
08009E09: C4 19                         if = go      $0x19
08009E0B: FD 3C C5 14                   w1 laddr     @b.0x14
08009E0F: 20 63                         w1 =:        b.0x8C
08009E11: 1A 14 62                      w move       $0x14,b.0x88
08009E14: CA 0C                         if < go      $0xC
08009E16: 84                            bi1 clr
08009E17: 85                            bi2 clr
08009E18: FD 67 C4 08 00 8E 38 62       by smove     $0x8008E38,b.0x88
08009E20: C0 45                         go           $0x45
08009E22: 18 42                         r:=          b.0x8
08009E24: 19 CD AE 85                   by move      $0xAE,r.0x14
08009E28: 04 CD 9A                      by1 :=       $0x9A
08009E2B: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009E31: D2 04                         if -k go     $0x4
08009E33: B4 4D                         jumpg        b.0x34
08009E35: 44 D0                         w test       r1
08009E37: C4 19                         if = go      $0x19
08009E39: FD 3C C5 14                   w1 laddr     @b.0x14
08009E3D: 20 63                         w1 =:        b.0x8C
08009E3F: 1A 14 62                      w move       $0x14,b.0x88
08009E42: CA 0C                         if < go      $0xC
08009E44: 84                            bi1 clr
08009E45: 85                            bi2 clr
08009E46: FD 67 C4 08 00 8E 40 62       by smove     $0x8008E40,b.0x88
08009E4E: C0 17                         go           $0x17
08009E50: FD 3C C5 14                   w1 laddr     @b.0x14
08009E54: 20 63                         w1 =:        b.0x8C
08009E56: 1A 14 62                      w move       $0x14,b.0x88
08009E59: CA 0C                         if < go      $0xC
08009E5B: 84                            bi1 clr
08009E5C: 85                            bi2 clr
08009E5D: FD 67 C4 08 00 8E 48 62       by smove     $0x8008E48,b.0x88
08009E65: C1 01 3B                      go           $0x13B
08009E68: 18 42                         r:=          b.0x8
08009E6A: 19 CD B3 85                   by move      $0xB3,r.0x14
08009E6E: 04 CD 98                      by1 :=       $0x98
08009E71: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009E77: D2 04                         if -k go     $0x4
08009E79: B4 4D                         jumpg        b.0x34
08009E7B: 44 D0                         w test       r1
08009E7D: C4 2C                         if = go      $0x2C
08009E7F: 0C CE 08 64                   w1 :=        $0x864
08009E83: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009E8A: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009E8F: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009E94: 4F 62                         w incr       b.0x88
08009E96: FD 3D C5 14                   w2 laddr     @b.0x14
08009E9A: 21 65                         w2 =:        b.0x94
08009E9C: 1A 14 64                      w move       $0x14,b.0x90
08009E9F: CA 08                         if < go      $0x8
08009EA1: 84                            bi1 clr
08009EA2: 85                            bi2 clr
08009EA3: FD 67 62 64                   by smove     b.0x88,b.0x90
08009EA7: C0 2A                         go           $0x2A
08009EA9: 0C CE 08 58                   w1 :=        $0x858
08009EAD: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009EB4: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009EB9: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009EBE: 4F 62                         w incr       b.0x88
08009EC0: FD 3D C5 14                   w2 laddr     @b.0x14
08009EC4: 21 65                         w2 =:        b.0x94
08009EC6: 1A 14 64                      w move       $0x14,b.0x90
08009EC9: CA 08                         if < go      $0x8
08009ECB: 84                            bi1 clr
08009ECC: 85                            bi2 clr
08009ECD: FD 67 62 64                   by smove     b.0x88,b.0x90
08009ED1: C1 00 CF                      go           $0xCF
08009ED4: C3 08 00 83 96 00             call         $0x8008396,$0x0
08009EDA: D2 04                         if -k go     $0x4
08009EDC: B4 4D                         jumpg        b.0x34
08009EDE: 18 42                         r:=          b.0x8
08009EE0: 1A 85 51                      w move       r.0x14,b.0x44
08009EE3: 1A 86 52                      w move       r.0x18,b.0x48
08009EE6: 1A 87 53                      w move       r.0x1C,b.0x4C
08009EE9: 1A 51 5A                      w move       b.0x44,b.0x68
08009EEC: 1A 52 5B                      w move       b.0x48,b.0x6C
08009EEF: 1A 53 5C                      w move       b.0x4C,b.0x70
08009EF2: 19 CD 2E 5D                   by move      $0x2E,b.0x74
08009EF6: FD 3D C5 14                   w2 laddr     @b.0x14
08009EFA: 21 5E                         w2 =:        b.0x78
08009EFC: 4A 5F                         w stz        b.0x7C
08009EFE: 1A 13 60                      w move       $0x13,b.0x80
08009F01: C3 08 00 9B 65 00             call         $0x8009B65,$0x0
08009F07: D2 04                         if -k go     $0x4
08009F09: B4 4D                         jumpg        b.0x34
08009F0B: C1 00 95                      go           $0x95
08009F0E: 18 42                         r:=          b.0x8
08009F10: 19 CD B3 85                   by move      $0xB3,r.0x14
08009F14: 04 CD 85                      by1 :=       $0x85
08009F17: C3 08 00 91 CE 00             call         $0x80091CE,$0x0
08009F1D: D2 04                         if -k go     $0x4
08009F1F: B4 4D                         jumpg        b.0x34
08009F21: 44 D0                         w test       r1
08009F23: C4 2C                         if = go      $0x2C
08009F25: 0C CE 08 64                   w1 :=        $0x864
08009F29: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009F30: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009F35: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009F3A: 4F 62                         w incr       b.0x88
08009F3C: FD 3D C5 14                   w2 laddr     @b.0x14
08009F40: 21 65                         w2 =:        b.0x94
08009F42: 1A 14 64                      w move       $0x14,b.0x90
08009F45: CA 08                         if < go      $0x8
08009F47: 84                            bi1 clr
08009F48: 85                            bi2 clr
08009F49: FD 67 62 64                   by smove     b.0x88,b.0x90
08009F4D: C0 2A                         go           $0x2A
08009F4F: 0C CE 08 58                   w1 :=        $0x858
08009F53: FC 5A E0 08 00 74 3C          by rladdr    $0x800743C+
08009F5A: FC 6E 82 81 62                w sub3       r.0x8,r.0x4,b.0x88
08009F5F: FC 69 81 80 63                w add3       r.0x4,r.0x0,b.0x8C
08009F64: 4F 62                         w incr       b.0x88
08009F66: FD 3D C5 14                   w2 laddr     @b.0x14
08009F6A: 21 65                         w2 =:        b.0x94
08009F6C: 1A 14 64                      w move       $0x14,b.0x90
08009F6F: CA 08                         if < go      $0x8
08009F71: 84                            bi1 clr
08009F72: 85                            bi2 clr
08009F73: FD 67 62 64                   by smove     b.0x88,b.0x90
08009F77: C0 29                         go           $0x29
08009F79: 18 42                         r:=          b.0x8
08009F7B: 4D 85                         w set1       r.0x14
08009F7D: FE 79 C4 08 00 8E 6C 86 03    w bmove      $0x8008E6C,r.0x18,$0x3
08009F86: FE 79 C4 08 00 8E 78 89 03    w bmove      $0x8008E78,r.0x24,$0x3
08009F8F: C3 08 00 C4 AD 00             call         $0x800C4AD,$0x0
08009F95: D2 04                         if -k go     $0x4
08009F97: B4 4D                         jumpg        b.0x34
08009F99: 0C CD C6                      w1 :=        $0xC6
08009F9C: FE 02                         setk
08009F9E: B4 4D                         jumpg        b.0x34
08009FA0: FE 03                         clrk
08009FA2: B4 4D                         jumpg        b.0x34
08009FA4: 9C                            entd
08009FA5: FD C0 66                      l=:          b.0x98
08009FA8: 18 47                         r:=          b.0x1C
08009FAA: 44 84                         w test       r.0x10
08009FAC: C6 19                         if >< go     $0x19
08009FAE: 18 42                         r:=          b.0x8
08009FB0: 1A 45 85                      w move       b.0x14,r.0x14
08009FB3: C3 08 00 87 17 00             call         $0x8008717,$0x0
08009FB9: D2 04                         if -k go     $0x4
08009FBB: B4 66                         jumpg        b.0x98
08009FBD: 18 42                         r:=          b.0x8
08009FBF: 1A 85 45                      w move       r.0x14,b.0x14
08009FC2: C1 00 81                      go           $0x81
08009FC5: 85                            bi2 clr
08009FC6: 52 45 D1                      w swap       b.0x14,r2
08009FC9: 21 67                         w2 =:        b.0x9C
08009FCB: 1A 84 68                      w move       r.0x10,b.0xA0
08009FCE: 44 68                         w test       b.0xA0
08009FD0: C4 59                         if = go      $0x59
08009FD2: 44 67                         w test       b.0x9C
08009FD4: C6 23                         if >< go     $0x23
08009FD6: C3 08 00 86 91 00             call         $0x8008691,$0x0
08009FDC: D2 04                         if -k go     $0x4
08009FDE: B4 66                         jumpg        b.0x98
08009FE0: 20 67                         w1 =:        b.0x9C
08009FE2: FD 3D 45                      w2 laddr     b.0x14
08009FE5: 0C 67                         w1 :=        b.0x9C
08009FE7: 0E 14                         w3 :=        $0x14
08009FE9: FE 03                         clrk
08009FEB: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
08009FF1: D2 04                         if -k go     $0x4
08009FF3: B4 66                         jumpg        b.0x98
08009FF5: C0 09                         go           $0x9
08009FF7: 44 45                         w test       b.0x14
08009FF9: C6 05                         if >< go     $0x5
08009FFB: 1A 67 45                      w move       b.0x9C,b.0x14
08009FFE: FD 3C C5 A0                   w1 laddr     @b.0xFFFFFFFFFFFFFFA0
0800A002: 20 6B                         w1 =:        b.0xAC
0800A004: 1A 14 6A                      w move       $0x14,b.0xA8
0800A007: FD 3D C5 9C                   w2 laddr     @b.0xFFFFFFFFFFFFFF9C
0800A00B: 21 6D                         w2 =:        b.0xB4
0800A00D: 1A 14 6C                      w move       $0x14,b.0xB0
0800A010: CA 08                         if < go      $0x8
0800A012: 84                            bi1 clr
0800A013: 85                            bi2 clr
0800A014: FD 67 6A 6C                   by smove     b.0xA8,b.0xB0
0800A018: 0C 67                         w1 :=        b.0x9C
0800A01A: 20 69                         w1 =:        b.0xA4
0800A01C: 1A F4 14 67                   w move       r1.(0x14),b.0x9C
0800A020: 18 68                         r:=          b.0xA0
0800A022: 1A 85 68                      w move       r.0x14,b.0xA0
0800A025: 44 68                         w test       b.0xA0
0800A027: C6 AB                         if >< go     $0xFFFFFFFFFFFFFFAB
0800A029: 18 69                         r:=          b.0xA4
0800A02B: 0C 85                         w1 :=        r.0x14
0800A02D: 18 42                         r:=          b.0x8
0800A02F: 20 85                         w1 =:        r.0x14
0800A031: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A037: D2 04                         if -k go     $0x4
0800A039: B4 66                         jumpg        b.0x98
0800A03B: 18 42                         r:=          b.0x8
0800A03D: 0D 85                         w2 :=        r.0x14
0800A03F: 18 69                         r:=          b.0xA4
0800A041: 21 85                         w2 =:        r.0x14
0800A043: FE 03                         clrk
0800A045: B4 66                         jumpg        b.0x98
0800A047: B8 CF 00 00 00 BC             ents         $0xBC
0800A04D: 18 42                         r:=          b.0x8
0800A04F: 1A 45 85                      w move       b.0x14,r.0x14
0800A052: 4A 86                         w stz        r.0x18
0800A054: C3 08 00 95 91 00             call         $0x8009591,$0x0
0800A05A: 9D                            ifkret
0800A05B: 18 42                         r:=          b.0x8
0800A05D: 1A 87 46                      w move       r.0x1C,b.0x18
0800A060: 20 47                         w1 =:        b.0x1C
0800A062: 44 D0                         w test       r1
0800A064: C4 08                         if = go      $0x8
0800A066: 2E F4 00 07                   w comp2      r1.(0x0),$0x7
0800A06A: C6 3D                         if >< go     $0x3D
0800A06C: 44 C4 08 00 79 F8             w test       $0x80079F8
0800A072: C6 09                         if >< go     $0x9
0800A074: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A07A: 9D                            ifkret
0800A07B: 18 42                         r:=          b.0x8
0800A07D: FE 79 C4 08 00 8E D8 85 03    w bmove      $0x8008ED8,r.0x14,$0x3
0800A086: 1A 45 88                      w move       b.0x14,r.0x20
0800A089: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
0800A08F: 9D                            ifkret
0800A090: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A096: 9D                            ifkret
0800A097: 0C CD BA                      w1 :=        $0xBA
0800A09A: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
0800A0A0: 9D                            ifkret
0800A0A1: 0C CD BA                      w1 :=        $0xBA
0800A0A4: 81                            retk
0800A0A5: C0 45                         go           $0x45
0800A0A7: 44 F4 00                      w test       r1.(0x0)
0800A0AA: C6 39                         if >< go     $0x39
0800A0AC: 0D F4 08                      w2 :=        r1.(0x8)
0800A0AF: 21 6E                         w2 =:        b.0xB8
0800A0B1: 35 04                         w2 comp      $0x4
0800A0B3: C4 1E                         if = go      $0x1E
0800A0B5: 35 06                         w2 comp      $0x6
0800A0B7: C4 1A                         if = go      $0x1A
0800A0B9: 35 03                         w2 comp      $0x3
0800A0BB: C4 16                         if = go      $0x16
0800A0BD: 35 05                         w2 comp      $0x5
0800A0BF: C4 12                         if = go      $0x12
0800A0C1: 35 0D                         w2 comp      $0xD
0800A0C3: C4 0E                         if = go      $0xE
0800A0C5: 35 0F                         w2 comp      $0xF
0800A0C7: C4 0A                         if = go      $0xA
0800A0C9: 35 10                         w2 comp      $0x10
0800A0CB: C4 06                         if = go      $0x6
0800A0CD: 35 11                         w2 comp      $0x11
0800A0CF: C6 0B                         if >< go     $0xB
0800A0D1: C3 08 00 99 0C 00             call         $0x800990C,$0x0
0800A0D7: 9D                            ifkret
0800A0D8: C0 09                         go           $0x9
0800A0DA: C3 08 00 9B CC 00             call         $0x8009BCC,$0x0
0800A0E0: 9D                            ifkret
0800A0E1: C0 09                         go           $0x9
0800A0E3: C3 08 00 9F A4 00             call         $0x8009FA4,$0x0
0800A0E9: 9D                            ifkret
0800A0EA: 80                            ret
0800A0EB: 9C                            entd
0800A0EC: FD C0 50                      l=:          b.0x40
0800A0EF: 1C 51                         by1 =:       b.0x44
0800A0F1: 44 4B                         w test       b.0x2C
0800A0F3: C4 07                         if = go      $0x7
0800A0F5: 2E 49 13                      w comp2      b.0x24,$0x13
0800A0F8: C6 20                         if >< go     $0x20
0800A0FA: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A100: D2 04                         if -k go     $0x4
0800A102: B4 50                         jumpg        b.0x40
0800A104: 20 4C                         w1 =:        b.0x30
0800A106: 44 4B                         w test       b.0x2C
0800A108: C6 06                         if >< go     $0x6
0800A10A: 20 4B                         w1 =:        b.0x2C
0800A10C: C0 06                         go           $0x6
0800A10E: 18 4D                         r:=          b.0x34
0800A110: 20 85                         w1 =:        r.0x14
0800A112: 1A 4C 4D                      w move       b.0x30,b.0x34
0800A115: 1A 3F 49                      w move       $0x3F,b.0x24
0800A118: 0C 49                         w1 :=        b.0x24
0800A11A: 54 01                         w1 +         $0x1
0800A11C: 20 49                         w1 =:        b.0x24
0800A11E: 05 51                         by2 :=       b.0x44
0800A120: FD 3E C5 34                   w3 laddr     @b.0x34
0800A124: 56 D0                         w3 +         r1
0800A126: 1D F6 00                      by2 =:       r3.(0x0)
0800A129: FE 03                         clrk
0800A12B: B4 50                         jumpg        b.0x40
0800A12D: 9C                            entd
0800A12E: FD C0 52                      l=:          b.0x48
0800A131: 84                            bi1 clr
0800A132: 20 58                         w1 =:        b.0x60
0800A134: 20 55                         w1 =:        b.0x54
0800A136: FD 3D C5 4C                   w2 laddr     @b.0x4C
0800A13A: 55 54                         w2 +         b.0x50
0800A13C: 04 F5 00                      by1 :=       r2.(0x0)
0800A13F: 1C 47                         by1 =:       b.0x1C
0800A141: 30 CD 80                      by1 comp     $0x80
0800A144: C6 4D                         if >< go     $0x4D
0800A146: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A14C: D2 04                         if -k go     $0x4
0800A14E: B4 52                         jumpg        b.0x48
0800A150: 20 59                         w1 =:        b.0x64
0800A152: 44 58                         w test       b.0x60
0800A154: C4 31                         if = go      $0x31
0800A156: 2E 57 13                      w comp2      b.0x5C,$0x13
0800A159: C6 16                         if >< go     $0x16
0800A15B: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A161: D2 04                         if -k go     $0x4
0800A163: B4 52                         jumpg        b.0x48
0800A165: 18 5A                         r:=          b.0x68
0800A167: 20 85                         w1 =:        r.0x14
0800A169: 1A 85 5A                      w move       r.0x14,b.0x68
0800A16C: 1A 3F 57                      w move       $0x3F,b.0x5C
0800A16F: 0C 57                         w1 :=        b.0x5C
0800A171: 54 01                         w1 +         $0x1
0800A173: 20 57                         w1 =:        b.0x5C
0800A175: 05 09                         by2 :=       $0x9
0800A177: FD 3E C5 68                   w3 laddr     @b.0x68
0800A17B: 56 D0                         w3 +         r1
0800A17D: 1D F6 00                      by2 =:       r3.(0x0)
0800A180: 18 59                         r:=          b.0x64
0800A182: 1A 58 86                      w move       b.0x60,r.0x18
0800A185: 0C 59                         w1 :=        b.0x64
0800A187: 20 58                         w1 =:        b.0x60
0800A189: 20 5A                         w1 =:        b.0x68
0800A18B: 1A 3F 57                      w move       $0x3F,b.0x5C
0800A18E: C1 00 AD                      go           $0xAD
0800A191: 2E 57 13                      w comp2      b.0x5C,$0x13
0800A194: C6 16                         if >< go     $0x16
0800A196: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A19C: D2 04                         if -k go     $0x4
0800A19E: B4 52                         jumpg        b.0x48
0800A1A0: 18 5A                         r:=          b.0x68
0800A1A2: 20 85                         w1 =:        r.0x14
0800A1A4: 1A 85 5A                      w move       r.0x14,b.0x68
0800A1A7: 1A 3F 57                      w move       $0x3F,b.0x5C
0800A1AA: 2D 47 CD 3B                   by comp2     b.0x1C,$0x3B
0800A1AE: C7 00 7C                      if >< go     $0x7C
0800A1B1: 0C 57                         w1 :=        b.0x5C
0800A1B3: 54 01                         w1 +         $0x1
0800A1B5: 05 0D                         by2 :=       $0xD
0800A1B7: FD 3E C5 68                   w3 laddr     @b.0x68
0800A1BB: 56 D0                         w3 +         r1
0800A1BD: 1D F6 00                      by2 =:       r3.(0x0)
0800A1C0: 0F 58                         w4 :=        b.0x60
0800A1C2: 23 59                         w4 =:        b.0x64
0800A1C4: 1A F7 18 58                   w move       r4.(0x18),b.0x60
0800A1C8: 4A F7 18                      w stz        r4.(0x18)
0800A1CB: 18 42                         r:=          b.0x8
0800A1CD: 23 85                         w4 =:        r.0x14
0800A1CF: C3 08 00 A0 47 00             call         $0x800A047,$0x0
0800A1D5: D2 04                         if -k go     $0x4
0800A1D7: B4 52                         jumpg        b.0x48
0800A1D9: 18 42                         r:=          b.0x8
0800A1DB: 1A 85 59                      w move       r.0x14,b.0x64
0800A1DE: 44 58                         w test       b.0x60
0800A1E0: C6 0B                         if >< go     $0xB
0800A1E2: 1A 59 55                      w move       b.0x64,b.0x54
0800A1E5: FE 03                         clrk
0800A1E7: B4 52                         jumpg        b.0x48
0800A1E9: C0 3F                         go           $0x3F
0800A1EB: 1A 58 5A                      w move       b.0x60,b.0x68
0800A1EE: 18 5A                         r:=          b.0x68
0800A1F0: 44 85                         w test       r.0x14
0800A1F2: C4 07                         if = go      $0x7
0800A1F4: 1A 85 5A                      w move       r.0x14,b.0x68
0800A1F7: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
0800A1F9: 0C 59                         w1 :=        b.0x64
0800A1FB: 20 85                         w1 =:        r.0x14
0800A1FD: 44 D0                         w test       r1
0800A1FF: C6 07                         if >< go     $0x7
0800A201: 19 09 56                      by move      $0x9,b.0x58
0800A204: C0 10                         go           $0x10
0800A206: 18 5A                         r:=          b.0x68
0800A208: 44 85                         w test       r.0x14
0800A20A: C4 07                         if = go      $0x7
0800A20C: 1A 85 5A                      w move       r.0x14,b.0x68
0800A20F: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
0800A211: 19 0D 56                      by move      $0xD,b.0x58
0800A214: 4A 57                         w stz        b.0x5C
0800A216: FD 3C C5 68                   w1 laddr     @b.0x68
0800A21A: 54 57                         w1 +         b.0x5C
0800A21C: 2D F4 00 56                   by comp2     r1.(0x0),b.0x58
0800A220: C4 06                         if = go      $0x6
0800A222: 4F 57                         w incr       b.0x5C
0800A224: C0 F2                         go           $0xFFFFFFFFFFFFFFF2
0800A226: 51 57                         w decr       b.0x5C
0800A228: C0 13                         go           $0x13
0800A22A: 0D 57                         w2 :=        b.0x5C
0800A22C: 55 01                         w2 +         $0x1
0800A22E: 21 57                         w2 =:        b.0x5C
0800A230: 06 47                         by3 :=       b.0x1C
0800A232: FD 3F C5 68                   w4 laddr     @b.0x68
0800A236: 57 D1                         w4 +         r2
0800A238: 1E F7 00                      by3 =:       r4.(0x0)
0800A23B: 0C 54                         w1 :=        b.0x50
0800A23D: 54 01                         w1 +         $0x1
0800A23F: 20 54                         w1 =:        b.0x50
0800A241: 34 13                         w1 comp      $0x13
0800A243: CE 09                         if <= go     $0x9
0800A245: 18 53                         r:=          b.0x4C
0800A247: 1A 85 53                      w move       r.0x14,b.0x4C
0800A24A: 4A 54                         w stz        b.0x50
0800A24C: C1 FE EA                      go           $0xFFFFFFFFFFFFFEEA
0800A24F: FE 03                         clrk
0800A251: B4 52                         jumpg        b.0x48
0800A253: B8 CF 00 00 00 7C             ents         $0x7C
0800A259: 4A 46                         w stz        b.0x18
0800A25B: 4A 48                         w stz        b.0x20
0800A25D: 4A 4B                         w stz        b.0x2C
0800A25F: 1A 45 4A                      w move       b.0x14,b.0x28
0800A262: FD 3D C5 28                   w2 laddr     @b.0x28
0800A266: 55 48                         w2 +         b.0x20
0800A268: 04 F5 00                      by1 :=       r2.(0x0)
0800A26B: 1C 47                         by1 =:       b.0x1C
0800A26D: 30 CD 80                      by1 comp     $0x80
0800A270: C7 00 C8                      if >< go     $0xC8
0800A273: 44 46                         w test       b.0x18
0800A275: C6 53                         if >< go     $0x53
0800A277: 1A 45 4F                      w move       b.0x14,b.0x3C
0800A27A: 44 4F                         w test       b.0x3C
0800A27C: C4 44                         if = go      $0x44
0800A27E: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A284: 9D                            ifkret
0800A285: 20 4C                         w1 =:        b.0x30
0800A287: FD 3D C5 3C                   w2 laddr     @b.0x3C
0800A28B: 21 5C                         w2 =:        b.0x70
0800A28D: 1A 14 5B                      w move       $0x14,b.0x6C
0800A290: FD 3E F4 00                   w3 laddr     r1.(0x0)
0800A294: 22 5E                         w3 =:        b.0x78
0800A296: 1A 14 5D                      w move       $0x14,b.0x74
0800A299: CA 08                         if < go      $0x8
0800A29B: 84                            bi1 clr
0800A29C: 85                            bi2 clr
0800A29D: FD 67 5B 5D                   by smove     b.0x6C,b.0x74
0800A2A1: 44 4B                         w test       b.0x2C
0800A2A3: C6 07                         if >< go     $0x7
0800A2A5: 1A 4C 4B                      w move       b.0x30,b.0x2C
0800A2A8: C0 07                         go           $0x7
0800A2AA: 18 4D                         r:=          b.0x34
0800A2AC: 1A 4C 85                      w move       b.0x30,r.0x14
0800A2AF: 1A 4C 4D                      w move       b.0x30,b.0x34
0800A2B2: 2E 4F 4A                      w comp2      b.0x3C,b.0x28
0800A2B5: C4 0B                         if = go      $0xB
0800A2B7: 18 4F                         r:=          b.0x3C
0800A2B9: 1A 85 4F                      w move       r.0x14,b.0x3C
0800A2BC: 44 4F                         w test       b.0x3C
0800A2BE: C6 C0                         if >< go     $0xFFFFFFFFFFFFFFC0
0800A2C0: 0C 48                         w1 :=        b.0x20
0800A2C2: 60 01                         w1 -         $0x1
0800A2C4: 20 49                         w1 =:        b.0x24
0800A2C6: 4D 46                         w set1       b.0x18
0800A2C8: 1A 4A 53                      w move       b.0x28,b.0x4C
0800A2CB: 1A 48 54                      w move       b.0x20,b.0x50
0800A2CE: 1A 4E 55                      w move       b.0x38,b.0x54
0800A2D1: C3 08 00 A1 2D 00             call         $0x800A12D,$0x0
0800A2D7: 9D                            ifkret
0800A2D8: 1A 53 4A                      w move       b.0x4C,b.0x28
0800A2DB: 1A 54 48                      w move       b.0x50,b.0x20
0800A2DE: 1A 55 4E                      w move       b.0x54,b.0x38
0800A2E1: 44 4E                         w test       b.0x38
0800A2E3: C4 53                         if = go      $0x53
0800A2E5: 04 09                         by1 :=       $0x9
0800A2E7: C3 08 00 A0 EB 00             call         $0x800A0EB,$0x0
0800A2ED: 9D                            ifkret
0800A2EE: 18 4D                         r:=          b.0x34
0800A2F0: 1A 4E 85                      w move       b.0x38,r.0x14
0800A2F3: 4A 4D                         w stz        b.0x34
0800A2F5: 1A 4E 4C                      w move       b.0x38,b.0x30
0800A2F8: 18 4C                         r:=          b.0x30
0800A2FA: 44 85                         w test       r.0x14
0800A2FC: C4 07                         if = go      $0x7
0800A2FE: 1A 85 4C                      w move       r.0x14,b.0x30
0800A301: C0 F7                         go           $0xFFFFFFFFFFFFFFF7
0800A303: 44 4D                         w test       b.0x34
0800A305: C6 05                         if >< go     $0x5
0800A307: 1A 4C 4D                      w move       b.0x30,b.0x34
0800A30A: 4A 49                         w stz        b.0x24
0800A30C: FD 3C C5 30                   w1 laddr     @b.0x30
0800A310: 54 49                         w1 +         b.0x24
0800A312: 2D F4 00 0D                   by comp2     r1.(0x0),$0xD
0800A316: C4 06                         if = go      $0x6
0800A318: 4F 49                         w incr       b.0x24
0800A31A: C0 F2                         go           $0xFFFFFFFFFFFFFFF2
0800A31C: 05 09                         by2 :=       $0x9
0800A31E: FD 3E C5 30                   w3 laddr     @b.0x30
0800A322: 56 49                         w3 +         b.0x24
0800A324: 1D F6 00                      by2 =:       r3.(0x0)
0800A327: 18 4E                         r:=          b.0x38
0800A329: 0F 86                         w4 :=        r.0x18
0800A32B: 23 4E                         w4 =:        b.0x38
0800A32D: 44 D3                         w test       r4
0800A32F: C4 04                         if = go      $0x4
0800A331: C0 C4                         go           $0xFFFFFFFFFFFFFFC4
0800A333: 1A 13 49                      w move       $0x13,b.0x24
0800A336: C0 0D                         go           $0xD
0800A338: 44 46                         w test       b.0x18
0800A33A: C4 09                         if = go      $0x9
0800A33C: C3 08 00 A0 EB 00             call         $0x800A0EB,$0x0
0800A342: 9D                            ifkret
0800A343: 2D 47 0D                      by comp2     b.0x1C,$0xD
0800A346: C4 16                         if = go      $0x16
0800A348: 0C 48                         w1 :=        b.0x20
0800A34A: 54 01                         w1 +         $0x1
0800A34C: 20 48                         w1 =:        b.0x20
0800A34E: 34 13                         w1 comp      $0x13
0800A350: CE 09                         if <= go     $0x9
0800A352: 18 4A                         r:=          b.0x28
0800A354: 1A 85 4A                      w move       r.0x14,b.0x28
0800A357: 4A 48                         w stz        b.0x20
0800A359: C1 FF 09                      go           $0xFFFFFFFFFFFFFF09
0800A35C: 44 46                         w test       b.0x18
0800A35E: C4 16                         if = go      $0x16
0800A360: 18 42                         r:=          b.0x8
0800A362: 1A 45 85                      w move       b.0x14,r.0x14
0800A365: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A36B: 9D                            ifkret
0800A36C: 18 42                         r:=          b.0x8
0800A36E: 1A 85 45                      w move       r.0x14,b.0x14
0800A371: 1A 4B 45                      w move       b.0x2C,b.0x14
0800A374: 80                            ret
0800A375: B8 CF 00 00 00 30             ents         $0x30
0800A37B: 18 42                         r:=          b.0x8
0800A37D: 1A 45 85                      w move       b.0x14,r.0x14
0800A380: 4D 86                         w set1       r.0x18
0800A382: C3 08 00 95 91 00             call         $0x8009591,$0x0
0800A388: 9D                            ifkret
0800A389: 18 42                         r:=          b.0x8
0800A38B: 1A 87 49                      w move       r.0x1C,b.0x24
0800A38E: 20 4A                         w1 =:        b.0x28
0800A390: 44 D0                         w test       r1
0800A392: C6 14                         if >< go     $0x14
0800A394: 1A 45 85                      w move       b.0x14,r.0x14
0800A397: 1A 49 86                      w move       b.0x24,r.0x18
0800A39A: C3 08 00 96 A8 00             call         $0x80096A8,$0x0
0800A3A0: 9D                            ifkret
0800A3A1: 20 4A                         w1 =:        b.0x28
0800A3A3: C1 00 8F                      go           $0x8F
0800A3A6: 0C 47                         w1 :=        b.0x1C
0800A3A8: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800A3AE: 9D                            ifkret
0800A3AF: 20 4B                         w1 =:        b.0x2C
0800A3B1: 0C C5 28                      w1 :=        @b.0x28
0800A3B4: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800A3BA: 9D                            ifkret
0800A3BB: 2E 4B D0                      w comp2      b.0x2C,r1
0800A3BE: CE 63                         if <= go     $0x63
0800A3C0: 44 C5 28                      w test       @b.0x28
0800A3C3: C6 3B                         if >< go     $0x3B
0800A3C5: 44 C4 08 00 79 F8             w test       $0x80079F8
0800A3CB: C6 09                         if >< go     $0x9
0800A3CD: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A3D3: 9D                            ifkret
0800A3D4: 18 42                         r:=          b.0x8
0800A3D6: FE 79 C4 08 00 8E EC 85 03    w bmove      $0x8008EEC,r.0x14,$0x3
0800A3DF: 1A 45 88                      w move       b.0x14,r.0x20
0800A3E2: C3 08 00 8A 5A 00             call         $0x8008A5A,$0x0
0800A3E8: 9D                            ifkret
0800A3E9: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A3EF: 9D                            ifkret
0800A3F0: 0C CD B5                      w1 :=        $0xB5
0800A3F3: C3 08 00 8E 8F 00             call         $0x8008E8F,$0x0
0800A3F9: 9D                            ifkret
0800A3FA: 0C CD B5                      w1 :=        $0xB5
0800A3FD: 81                            retk
0800A3FE: 18 42                         r:=          b.0x8
0800A400: 1A 45 85                      w move       b.0x14,r.0x14
0800A403: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A409: 9D                            ifkret
0800A40A: 18 42                         r:=          b.0x8
0800A40C: 1A 85 45                      w move       r.0x14,b.0x14
0800A40F: 1A 46 85                      w move       b.0x18,r.0x14
0800A412: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A418: 9D                            ifkret
0800A419: 18 42                         r:=          b.0x8
0800A41B: 1A 85 46                      w move       r.0x14,b.0x18
0800A41E: 80                            ret
0800A41F: C0 13                         go           $0x13
0800A421: 18 42                         r:=          b.0x8
0800A423: 1A 45 85                      w move       b.0x14,r.0x14
0800A426: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A42C: 9D                            ifkret
0800A42D: 18 42                         r:=          b.0x8
0800A42F: 1A 85 45                      w move       r.0x14,b.0x14
0800A432: 2E 47 03                      w comp2      b.0x1C,$0x3
0800A435: C6 17                         if >< go     $0x17
0800A437: 2E C5 28 03                   w comp2      @b.0x28,$0x3
0800A43B: C4 11                         if = go      $0x11
0800A43D: 0D C5 28                      w2 :=        @b.0x28
0800A440: 18 4A                         r:=          b.0x28
0800A442: 21 81                         w2 =:        r.0x4
0800A444: 0E 84                         w3 :=        r.0x10
0800A446: 22 85                         w3 =:        r.0x14
0800A448: 4D 48                         w set1       b.0x20
0800A44A: C0 04                         go           $0x4
0800A44C: 4A 48                         w stz        b.0x20
0800A44E: 18 4A                         r:=          b.0x28
0800A450: 1A 47 C5 28                   w move       b.0x1C,@b.0x28
0800A454: 44 46                         w test       b.0x18
0800A456: C4 13                         if = go      $0x13
0800A458: 18 42                         r:=          b.0x8
0800A45A: 1A 46 85                      w move       b.0x18,r.0x14
0800A45D: C3 08 00 A2 53 00             call         $0x800A253,$0x0
0800A463: 9D                            ifkret
0800A464: 18 42                         r:=          b.0x8
0800A466: 1A 85 46                      w move       r.0x14,b.0x18
0800A469: 44 48                         w test       b.0x20
0800A46B: C6 19                         if >< go     $0x19
0800A46D: 18 4A                         r:=          b.0x28
0800A46F: 0D 84                         w2 :=        r.0x10
0800A471: 18 42                         r:=          b.0x8
0800A473: 21 85                         w2 =:        r.0x14
0800A475: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800A47B: 9D                            ifkret
0800A47C: 18 42                         r:=          b.0x8
0800A47E: 0D 85                         w2 :=        r.0x14
0800A480: 18 4A                         r:=          b.0x28
0800A482: 21 84                         w2 =:        r.0x10
0800A484: 18 4A                         r:=          b.0x28
0800A486: 1A 46 84                      w move       b.0x18,r.0x10
0800A489: 80                            ret
0800A48A: B8 CF 00 00 00 18             ents         $0x18
0800A490: 1A C4 08 00 7A 1C 45          w move       $0x8007A1C,b.0x14
0800A497: 44 45                         w test       b.0x14
0800A499: C4 23                         if = go      $0x23
0800A49B: 2E C5 14 03                   w comp2      @b.0x14,$0x3
0800A49F: C6 14                         if >< go     $0x14
0800A4A1: 18 45                         r:=          b.0x14
0800A4A3: 0C 81                         w1 :=        r.0x4
0800A4A5: 20 C5 14                      w1 =:        @b.0x14
0800A4A8: 34 07                         w1 comp      $0x7
0800A4AA: C4 06                         if = go      $0x6
0800A4AC: 0C 85                         w1 :=        r.0x14
0800A4AE: 20 84                         w1 =:        r.0x10
0800A4B0: 1A 07 81                      w move       $0x7,r.0x4
0800A4B3: 18 45                         r:=          b.0x14
0800A4B5: 1A 86 45                      w move       r.0x18,b.0x14
0800A4B8: 44 45                         w test       b.0x14
0800A4BA: C6 E1                         if >< go     $0xFFFFFFFFFFFFFFE1
0800A4BC: 80                            ret
0800A4BD: B8 CF 00 00 00 2C             ents         $0x2C
0800A4C3: 84                            bi1 clr
0800A4C4: 20 C4 08 00 7A 10             w1 =:        $0x8007A10
0800A4CA: 20 C4 08 00 7A 14             w1 =:        $0x8007A14
0800A4D0: 85                            bi2 clr
0800A4D1: 21 C4 08 00 7A 68             w2 =:        $0x8007A68
0800A4D7: 21 C4 08 00 7A 1C             w2 =:        $0x8007A1C
0800A4DD: 4D 45                         w set1       b.0x14
0800A4DF: C3 08 00 86 91 00             call         $0x8008691,$0x0
0800A4E5: 9D                            ifkret
0800A4E6: 20 47                         w1 =:        b.0x1C
0800A4E8: 0D 45                         w2 :=        b.0x14
0800A4EA: B4 E1 08 00 8F D8             jumpg        $0x8008FD8+
0800A4F0: C1 01 AE                      go           $0x1AE
0800A4F3: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A4F7: 20 4A                         w1 =:        b.0x28
0800A4F9: 1A 14 49                      w move       $0x14,b.0x24
0800A4FC: CA 0C                         if < go      $0xC
0800A4FE: 84                            bi1 clr
0800A4FF: 85                            bi2 clr
0800A500: FD 67 C4 08 00 8F 00 49       by smove     $0x8008F00,b.0x24
0800A508: C1 01 96                      go           $0x196
0800A50B: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A50F: 20 4A                         w1 =:        b.0x28
0800A511: 1A 14 49                      w move       $0x14,b.0x24
0800A514: CA 0C                         if < go      $0xC
0800A516: 84                            bi1 clr
0800A517: 85                            bi2 clr
0800A518: FD 67 C4 08 00 8F 0C 49       by smove     $0x8008F0C,b.0x24
0800A520: C1 01 7E                      go           $0x17E
0800A523: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A527: 20 4A                         w1 =:        b.0x28
0800A529: 1A 14 49                      w move       $0x14,b.0x24
0800A52C: CA 0C                         if < go      $0xC
0800A52E: 84                            bi1 clr
0800A52F: 85                            bi2 clr
0800A530: FD 67 C4 08 00 8F 18 49       by smove     $0x8008F18,b.0x24
0800A538: C1 01 66                      go           $0x166
0800A53B: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A53F: 20 4A                         w1 =:        b.0x28
0800A541: 1A 14 49                      w move       $0x14,b.0x24
0800A544: CA 0C                         if < go      $0xC
0800A546: 84                            bi1 clr
0800A547: 85                            bi2 clr
0800A548: FD 67 C4 08 00 8F 24 49       by smove     $0x8008F24,b.0x24
0800A550: C1 01 4E                      go           $0x14E
0800A553: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A557: 20 4A                         w1 =:        b.0x28
0800A559: 1A 14 49                      w move       $0x14,b.0x24
0800A55C: CA 0C                         if < go      $0xC
0800A55E: 84                            bi1 clr
0800A55F: 85                            bi2 clr
0800A560: FD 67 C4 08 00 8F 30 49       by smove     $0x8008F30,b.0x24
0800A568: C1 01 36                      go           $0x136
0800A56B: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A56F: 20 4A                         w1 =:        b.0x28
0800A571: 1A 14 49                      w move       $0x14,b.0x24
0800A574: CA 0C                         if < go      $0xC
0800A576: 84                            bi1 clr
0800A577: 85                            bi2 clr
0800A578: FD 67 C4 08 00 8F 3C 49       by smove     $0x8008F3C,b.0x24
0800A580: C1 01 1E                      go           $0x11E
0800A583: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A587: 20 4A                         w1 =:        b.0x28
0800A589: 1A 14 49                      w move       $0x14,b.0x24
0800A58C: CA 0C                         if < go      $0xC
0800A58E: 84                            bi1 clr
0800A58F: 85                            bi2 clr
0800A590: FD 67 C4 08 00 8F 48 49       by smove     $0x8008F48,b.0x24
0800A598: C1 01 06                      go           $0x106
0800A59B: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A59F: 20 4A                         w1 =:        b.0x28
0800A5A1: 1A 14 49                      w move       $0x14,b.0x24
0800A5A4: CA 0C                         if < go      $0xC
0800A5A6: 84                            bi1 clr
0800A5A7: 85                            bi2 clr
0800A5A8: FD 67 C4 08 00 8F 54 49       by smove     $0x8008F54,b.0x24
0800A5B0: C1 00 EE                      go           $0xEE
0800A5B3: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A5B7: 20 4A                         w1 =:        b.0x28
0800A5B9: 1A 14 49                      w move       $0x14,b.0x24
0800A5BC: CA 0C                         if < go      $0xC
0800A5BE: 84                            bi1 clr
0800A5BF: 85                            bi2 clr
0800A5C0: FD 67 C4 08 00 8F 60 49       by smove     $0x8008F60,b.0x24
0800A5C8: C1 00 D6                      go           $0xD6
0800A5CB: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A5CF: 20 4A                         w1 =:        b.0x28
0800A5D1: 1A 14 49                      w move       $0x14,b.0x24
0800A5D4: CA 0C                         if < go      $0xC
0800A5D6: 84                            bi1 clr
0800A5D7: 85                            bi2 clr
0800A5D8: FD 67 C4 08 00 8F 6C 49       by smove     $0x8008F6C,b.0x24
0800A5E0: C1 00 BE                      go           $0xBE
0800A5E3: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A5E7: 20 4A                         w1 =:        b.0x28
0800A5E9: 1A 14 49                      w move       $0x14,b.0x24
0800A5EC: CA 0C                         if < go      $0xC
0800A5EE: 84                            bi1 clr
0800A5EF: 85                            bi2 clr
0800A5F0: FD 67 C4 08 00 8F 78 49       by smove     $0x8008F78,b.0x24
0800A5F8: C1 00 A6                      go           $0xA6
0800A5FB: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A5FF: 20 4A                         w1 =:        b.0x28
0800A601: 1A 14 49                      w move       $0x14,b.0x24
0800A604: CA 0C                         if < go      $0xC
0800A606: 84                            bi1 clr
0800A607: 85                            bi2 clr
0800A608: FD 67 C4 08 00 8F 84 49       by smove     $0x8008F84,b.0x24
0800A610: C1 00 8E                      go           $0x8E
0800A613: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A617: 20 4A                         w1 =:        b.0x28
0800A619: 1A 14 49                      w move       $0x14,b.0x24
0800A61C: CA 0C                         if < go      $0xC
0800A61E: 84                            bi1 clr
0800A61F: 85                            bi2 clr
0800A620: FD 67 C4 08 00 8F 90 49       by smove     $0x8008F90,b.0x24
0800A628: C1 00 76                      go           $0x76
0800A62B: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A62F: 20 4A                         w1 =:        b.0x28
0800A631: 1A 14 49                      w move       $0x14,b.0x24
0800A634: CA 0C                         if < go      $0xC
0800A636: 84                            bi1 clr
0800A637: 85                            bi2 clr
0800A638: FD 67 C4 08 00 8F A0 49       by smove     $0x8008FA0,b.0x24
0800A640: C0 5E                         go           $0x5E
0800A642: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A646: 20 4A                         w1 =:        b.0x28
0800A648: 1A 14 49                      w move       $0x14,b.0x24
0800A64B: CA 0C                         if < go      $0xC
0800A64D: 84                            bi1 clr
0800A64E: 85                            bi2 clr
0800A64F: FD 67 C4 08 00 8F AC 49       by smove     $0x8008FAC,b.0x24
0800A657: C0 47                         go           $0x47
0800A659: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A65D: 20 4A                         w1 =:        b.0x28
0800A65F: 1A 14 49                      w move       $0x14,b.0x24
0800A662: CA 0C                         if < go      $0xC
0800A664: 84                            bi1 clr
0800A665: 85                            bi2 clr
0800A666: FD 67 C4 08 00 8F B8 49       by smove     $0x8008FB8,b.0x24
0800A66E: C0 30                         go           $0x30
0800A670: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A674: 20 4A                         w1 =:        b.0x28
0800A676: 1A 14 49                      w move       $0x14,b.0x24
0800A679: CA 0C                         if < go      $0xC
0800A67B: 84                            bi1 clr
0800A67C: 85                            bi2 clr
0800A67D: FD 67 C4 08 00 8F C4 49       by smove     $0x8008FC4,b.0x24
0800A685: C0 19                         go           $0x19
0800A687: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A68B: 20 4A                         w1 =:        b.0x28
0800A68D: 1A 14 49                      w move       $0x14,b.0x24
0800A690: CA 0C                         if < go      $0xC
0800A692: 84                            bi1 clr
0800A693: 85                            bi2 clr
0800A694: FD 67 C4 08 00 8F D0 49       by smove     $0x8008FD0,b.0x24
0800A69C: C0 02                         go           $0x2
0800A69E: FD 3C C5 1C                   w1 laddr     @b.0x1C
0800A6A2: 18 42                         r:=          b.0x8
0800A6A4: 20 85                         w1 =:        r.0x14
0800A6A6: 4A 86                         w stz        r.0x18
0800A6A8: 1A 13 87                      w move       $0x13,r.0x1C
0800A6AB: C3 08 00 83 E9 00             call         $0x80083E9,$0x0
0800A6B1: 9D                            ifkret
0800A6B2: 18 42                         r:=          b.0x8
0800A6B4: 1A 47 85                      w move       b.0x1C,r.0x14
0800A6B7: C3 08 00 95 1A 00             call         $0x800951A,$0x0
0800A6BD: 9D                            ifkret
0800A6BE: 20 46                         w1 =:        b.0x18
0800A6C0: 18 42                         r:=          b.0x8
0800A6C2: 1A 47 85                      w move       b.0x1C,r.0x14
0800A6C5: 20 86                         w1 =:        r.0x18
0800A6C7: C3 08 00 96 A8 00             call         $0x80096A8,$0x0
0800A6CD: 9D                            ifkret
0800A6CE: 20 48                         w1 =:        b.0x20
0800A6D0: 4A F4 00                      w stz        r1.(0x0)
0800A6D3: 1A 45 F4 08                   w move       b.0x14,r1.(0x8)
0800A6D7: 0D 45                         w2 :=        b.0x14
0800A6D9: 20 E1 08 00 7A 1C             w1 =:        $0x8007A1C+
0800A6DF: 4F 45                         w incr       b.0x14
0800A6E1: 2E 45 12                      w comp2      b.0x14,$0x12
0800A6E4: DB FD FB                      if <<= go    $0xFFFFFFFFFFFFFDFB
0800A6E7: 80                            ret
0800A6E8: B8 CF 00 00 00 4C             ents         $0x4C
0800A6EE: 1A CD 72 47                   w move       $0x72,b.0x1C
0800A6F2: 4D 48                         w set1       b.0x20
0800A6F4: FE 79 C4 08 00 90 2C 49 03    w bmove      $0x800902C,b.0x24,$0x3
0800A6FD: 1A 3F 4C                      w move       $0x3F,b.0x30
0800A700: C3 08 00 B9 7C 07 47 48 C5 24 C5 28 C5 2C 4C 45 call         $0x800B97C,$0x7,b.0x1C,b.0x20,@b.0x24,@b.0x28,@b.0x2C,b.0x30,b.0x14
0800A710: 9D                            ifkret
0800A711: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A717: 9D                            ifkret
0800A718: 1A CD 72 47                   w move       $0x72,b.0x1C
0800A71C: 4D 48                         w set1       b.0x20
0800A71E: FE 79 C4 08 00 90 40 4D 03    w bmove      $0x8009040,b.0x34,$0x3
0800A727: 1A 3F 4C                      w move       $0x3F,b.0x30
0800A72A: C3 08 00 B9 7C 07 47 48 C5 34 C5 38 C5 3C 4C 45 call         $0x800B97C,$0x7,b.0x1C,b.0x20,@b.0x34,@b.0x38,@b.0x3C,b.0x30,b.0x14
0800A73A: 9D                            ifkret
0800A73B: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A741: 9D                            ifkret
0800A742: 1A C4 08 00 7A 1C 46          w move       $0x8007A1C,b.0x18
0800A749: 44 46                         w test       b.0x18
0800A74B: C4 57                         if = go      $0x57
0800A74D: 18 46                         r:=          b.0x18
0800A74F: 44 82                         w test       r.0x8
0800A751: C6 48                         if >< go     $0x48
0800A753: 0C 83                         w1 :=        r.0xC
0800A755: 18 42                         r:=          b.0x8
0800A757: 20 85                         w1 =:        r.0x14
0800A759: C3 08 00 89 F2 00             call         $0x80089F2,$0x0
0800A75F: 9D                            ifkret
0800A760: 1A CD 72 47                   w move       $0x72,b.0x1C
0800A764: 4D 48                         w set1       b.0x20
0800A766: FE 79 C4 08 00 90 50 50 03    w bmove      $0x8009050,b.0x40,$0x3
0800A76F: 1A 3F 4C                      w move       $0x3F,b.0x30
0800A772: C3 08 00 B9 7C 07 47 48 C5 40 C5 44 C5 48 4C 45 call         $0x800B97C,$0x7,b.0x1C,b.0x20,@b.0x40,@b.0x44,@b.0x48,b.0x30,b.0x14
0800A782: 9D                            ifkret
0800A783: 18 46                         r:=          b.0x18
0800A785: 0D 84                         w2 :=        r.0x10
0800A787: 18 42                         r:=          b.0x8
0800A789: 21 85                         w2 =:        r.0x14
0800A78B: C3 08 00 89 F2 00             call         $0x80089F2,$0x0
0800A791: 9D                            ifkret
0800A792: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800A798: 9D                            ifkret
0800A799: 18 46                         r:=          b.0x18
0800A79B: 1A 86 46                      w move       r.0x18,b.0x18
0800A79E: 44 46                         w test       b.0x18
0800A7A0: C6 AD                         if >< go     $0xFFFFFFFFFFFFFFAD
0800A7A2: 80                            ret
0800A7A3: B8 CF 00 00 00 28             ents         $0x28
0800A7A9: 18 42                         r:=          b.0x8
0800A7AB: 1A CE 01 9A 85                w move       $0x19A,r.0x14
0800A7B0: 4A 86                         w stz        r.0x18
0800A7B2: C3 08 00 B7 8E 00             call         $0x800B78E,$0x0
0800A7B8: 9D                            ifkret
0800A7B9: 4A C4 08 00 7A 0C             w stz        $0x8007A0C
0800A7BF: 1A CD 63 46                   w move       $0x63,b.0x18
0800A7C3: C3 08 00 B9 7C 05 46 C4 08 00 79 F8 C4 08 00 79 FC C4 08 00 7A 00 C4 08 00 7A 04 call         $0x800B97C,$0x5,b.0x18,$0x80079F8,$0x80079FC,$0x8007A00,$0x8007A04
0800A7DE: 9D                            ifkret
0800A7DF: 0D CE 06 24                   w2 :=        $0x624
0800A7E3: FD 20 C4 08 00 90 7C E1 08 00 74 A8 0C          by bmove     $0x800907C,$0x80074A8+,$0xC
0800A7F0: 0E CE 06 30                   w3 :=        $0x630
0800A7F4: FD 20 C4 08 00 90 C4 E2 08 00 74 A8 0C          by bmove     $0x80090C4,$0x80074A8+,$0xC
0800A801: 0F CE 06 3C                   w4 :=        $0x63C
0800A805: FD 20 C4 08 00 90 EC E3 08 00 74 A8 0C          by bmove     $0x80090EC,$0x80074A8+,$0xC
0800A812: 0C CE 06 48                   w1 :=        $0x648
0800A816: FD 20 C4 08 00 91 0C E0 08 00 74 A8 0C          by bmove     $0x800910C,$0x80074A8+,$0xC
0800A823: 0D CE 06 54                   w2 :=        $0x654
0800A827: FD 20 C4 08 00 91 20 E1 08 00 74 A8 0C          by bmove     $0x8009120,$0x80074A8+,$0xC
0800A834: 0E CE 06 60                   w3 :=        $0x660
0800A838: FD 20 C4 08 00 91 34 E2 08 00 74 A8 0C          by bmove     $0x8009134,$0x80074A8+,$0xC
0800A845: 0F CE 06 6C                   w4 :=        $0x66C
0800A849: FD 20 C4 08 00 91 4C E3 08 00 74 A8 0C          by bmove     $0x800914C,$0x80074A8+,$0xC
0800A856: 0C CE 06 78                   w1 :=        $0x678
0800A85A: FD 20 C4 08 00 91 60 E0 08 00 74 A8 0C          by bmove     $0x8009160,$0x80074A8+,$0xC
0800A867: 0D CE 06 84                   w2 :=        $0x684
0800A86B: FD 20 C4 08 00 91 78 E1 08 00 74 A8 0C          by bmove     $0x8009178,$0x80074A8+,$0xC
0800A878: 0E CE 06 90                   w3 :=        $0x690
0800A87C: FD 20 C4 08 00 91 BC E2 08 00 74 A8 0C          by bmove     $0x80091BC,$0x80074A8+,$0xC
0800A889: 0F CE 06 9C                   w4 :=        $0x69C
0800A88D: FD 20 C4 08 00 91 D0 E3 08 00 74 A8 0C          by bmove     $0x80091D0,$0x80074A8+,$0xC
0800A89A: 0C CE 06 A8                   w1 :=        $0x6A8
0800A89E: FD 20 C4 08 00 92 28 E0 08 00 74 A8 0C          by bmove     $0x8009228,$0x80074A8+,$0xC
0800A8AB: 0D CE 06 B4                   w2 :=        $0x6B4
0800A8AF: FD 20 C4 08 00 92 3C E1 08 00 74 A8 0C          by bmove     $0x800923C,$0x80074A8+,$0xC
0800A8BC: 0E CE 06 C0                   w3 :=        $0x6C0
0800A8C0: FD 20 C4 08 00 92 50 E2 08 00 74 A8 0C          by bmove     $0x8009250,$0x80074A8+,$0xC
0800A8CD: 0F CE 06 CC                   w4 :=        $0x6CC
0800A8D1: FD 20 C4 08 00 92 60 E3 08 00 74 A8 0C          by bmove     $0x8009260,$0x80074A8+,$0xC
0800A8DE: 0C CE 06 D8                   w1 :=        $0x6D8
0800A8E2: FD 20 C4 08 00 92 78 E0 08 00 74 A8 0C          by bmove     $0x8009278,$0x80074A8+,$0xC
0800A8EF: 0D CE 06 E4                   w2 :=        $0x6E4
0800A8F3: FD 20 C4 08 00 92 C0 E1 08 00 74 A8 0C          by bmove     $0x80092C0,$0x80074A8+,$0xC
0800A900: 0E CE 06 F0                   w3 :=        $0x6F0
0800A904: FD 20 C4 08 00 92 DC E2 08 00 74 A8 0C          by bmove     $0x80092DC,$0x80074A8+,$0xC
0800A911: 0F CE 06 FC                   w4 :=        $0x6FC
0800A915: FD 20 C4 08 00 93 1C E3 08 00 74 A8 0C          by bmove     $0x800931C,$0x80074A8+,$0xC
0800A922: 0C CE 07 08                   w1 :=        $0x708
0800A926: FD 20 C4 08 00 93 40 E0 08 00 74 A8 0C          by bmove     $0x8009340,$0x80074A8+,$0xC
0800A933: 0D CE 07 14                   w2 :=        $0x714
0800A937: FD 20 C4 08 00 93 6C E1 08 00 74 A8 0C          by bmove     $0x800936C,$0x80074A8+,$0xC
0800A944: 0E CE 07 20                   w3 :=        $0x720
0800A948: FD 20 C4 08 00 93 90 E2 08 00 74 A8 0C          by bmove     $0x8009390,$0x80074A8+,$0xC
0800A955: 0F CE 07 2C                   w4 :=        $0x72C
0800A959: FD 20 C4 08 00 93 B8 E3 08 00 74 A8 0C          by bmove     $0x80093B8,$0x80074A8+,$0xC
0800A966: 0C CE 07 38                   w1 :=        $0x738
0800A96A: FD 20 C4 08 00 93 F0 E0 08 00 74 A8 0C          by bmove     $0x80093F0,$0x80074A8+,$0xC
0800A977: 0D CE 07 44                   w2 :=        $0x744
0800A97B: FD 20 C4 08 00 94 04 E1 08 00 74 A8 0C          by bmove     $0x8009404,$0x80074A8+,$0xC
0800A988: 0E CE 07 50                   w3 :=        $0x750
0800A98C: FD 20 C4 08 00 94 18 E2 08 00 74 A8 0C          by bmove     $0x8009418,$0x80074A8+,$0xC
0800A999: 1A CE 00 83 45                w move       $0x83,b.0x14
0800A99E: 0C 45                         w1 :=        b.0x14
0800A9A0: 6C 0C                         w1 *         $0xC
0800A9A2: 0D 45                         w2 :=        b.0x14
0800A9A4: 6D 0C                         w2 *         $0xC
0800A9A6: FC 5A E1 08 00 74 A8          by rladdr    $0x80074A8+
0800A9AD: 0E 82                         w3 :=        r.0x8
0800A9AF: 1A D2 49                      w move       r3,b.0x24
0800A9B2: 1A 01 48                      w move       $0x1,b.0x20
0800A9B5: FD 20 E0 08 00 74 A8 47 04    by bmove     $0x80074A8+,b.0x1C,$0x4
0800A9BE: 18 42                         r:=          b.0x8
0800A9C0: FD 20 47 85 0C                by bmove     b.0x1C,r.0x14,$0xC
0800A9C5: C3 08 00 83 E9 00             call         $0x80083E9,$0x0
0800A9CB: 9D                            ifkret
0800A9CC: BF 45 CE 00 9C D2             d loopi      b.0x14,$0x9C,$0xFFFFFFFFFFFFFFD2
0800A9D2: 0C CE 07 C8                   w1 :=        $0x7C8
0800A9D6: FD 20 C4 08 00 94 28 E0 08 00 74 3C 0C          by bmove     $0x8009428,$0x800743C+,$0xC
0800A9E3: 0D CE 07 D4                   w2 :=        $0x7D4
0800A9E7: FD 20 C4 08 00 94 3C E1 08 00 74 3C 0C          by bmove     $0x800943C,$0x800743C+,$0xC
0800A9F4: 0E CE 07 E0                   w3 :=        $0x7E0
0800A9F8: FD 20 C4 08 00 94 4C E2 08 00 74 3C 0C          by bmove     $0x800944C,$0x800743C+,$0xC
0800AA05: 0F CE 07 EC                   w4 :=        $0x7EC
0800AA09: FD 20 C4 08 00 94 5C E3 08 00 74 3C 0C          by bmove     $0x800945C,$0x800743C+,$0xC
0800AA16: 0C CE 07 F8                   w1 :=        $0x7F8
0800AA1A: FD 20 C4 08 00 94 70 E0 08 00 74 3C 0C          by bmove     $0x8009470,$0x800743C+,$0xC
0800AA27: 1A CE 00 A6 45                w move       $0xA6,b.0x14
0800AA2C: 0C 45                         w1 :=        b.0x14
0800AA2E: 6C 0C                         w1 *         $0xC
0800AA30: 18 42                         r:=          b.0x8
0800AA32: FD 20 E0 08 00 74 3C 85 0C    by bmove     $0x800743C+,r.0x14,$0xC
0800AA3B: C3 08 00 83 E9 00             call         $0x80083E9,$0x0
0800AA41: 9D                            ifkret
0800AA42: BF 45 CE 00 AA EA             d loopi      b.0x14,$0xAA,$0xFFFFFFFFFFFFFFEA
0800AA48: 0C CE 08 04                   w1 :=        $0x804
0800AA4C: FD 20 C4 08 00 94 80 E0 08 00 74 3C 0C          by bmove     $0x8009480,$0x800743C+,$0xC
0800AA59: 0D CE 08 10                   w2 :=        $0x810
0800AA5D: FD 20 C4 08 00 94 90 E1 08 00 74 3C 0C          by bmove     $0x8009490,$0x800743C+,$0xC
0800AA6A: 0E CE 08 1C                   w3 :=        $0x81C
0800AA6E: FD 20 C4 08 00 94 A4 E2 08 00 74 3C 0C          by bmove     $0x80094A4,$0x800743C+,$0xC
0800AA7B: 0F CE 08 28                   w4 :=        $0x828
0800AA7F: FD 20 C4 08 00 94 B8 E3 08 00 74 3C 0C          by bmove     $0x80094B8,$0x800743C+,$0xC
0800AA8C: 0C CE 08 34                   w1 :=        $0x834
0800AA90: FD 20 C4 08 00 94 CC E0 08 00 74 3C 0C          by bmove     $0x80094CC,$0x800743C+,$0xC
0800AA9D: 0D CE 08 40                   w2 :=        $0x840
0800AAA1: FD 20 C4 08 00 94 E0 E1 08 00 74 3C 0C          by bmove     $0x80094E0,$0x800743C+,$0xC
0800AAAE: 0E CE 08 4C                   w3 :=        $0x84C
0800AAB2: FD 20 C4 08 00 94 F4 E2 08 00 74 3C 0C          by bmove     $0x80094F4,$0x800743C+,$0xC
0800AABF: 0F CE 08 58                   w4 :=        $0x858
0800AAC3: FD 20 C4 08 00 95 04 E3 08 00 74 3C 0C          by bmove     $0x8009504,$0x800743C+,$0xC
0800AAD0: 0C CE 08 64                   w1 :=        $0x864
0800AAD4: FD 20 C4 08 00 95 14 E0 08 00 74 3C 0C          by bmove     $0x8009514,$0x800743C+,$0xC
0800AAE1: 0D CE 08 70                   w2 :=        $0x870
0800AAE5: FD 20 C4 08 00 95 28 E1 08 00 74 3C 0C          by bmove     $0x8009528,$0x800743C+,$0xC
0800AAF2: 0E CE 08 7C                   w3 :=        $0x87C
0800AAF6: FD 20 C4 08 00 95 44 E2 08 00 74 3C 0C          by bmove     $0x8009544,$0x800743C+,$0xC
0800AB03: 0F CE 08 88                   w4 :=        $0x888
0800AB07: FD 20 C4 08 00 95 58 E3 08 00 74 3C 0C          by bmove     $0x8009558,$0x800743C+,$0xC
0800AB14: 1A CE 00 AB 45                w move       $0xAB,b.0x14
0800AB19: 0C 45                         w1 :=        b.0x14
0800AB1B: 6C 0C                         w1 *         $0xC
0800AB1D: 18 42                         r:=          b.0x8
0800AB1F: FD 20 E0 08 00 74 3C 85 0C    by bmove     $0x800743C+,r.0x14,$0xC
0800AB28: C3 08 00 83 E9 00             call         $0x80083E9,$0x0
0800AB2E: 9D                            ifkret
0800AB2F: BF 45 CE 00 B6 EA             d loopi      b.0x14,$0xB6,$0xFFFFFFFFFFFFFFEA
0800AB35: 80                            ret
0800AB36: B8 CF 00 00 00 B4             ents         $0xB4
0800AB3C: FD 54 CE 00 83 45             w byconv     $0x83,b.0x14
0800AB42: 04 45                         by1 :=       b.0x14
0800AB44: 6C 0C                         w1 *         $0xC
0800AB46: FD 20 E0 08 00 74 A8 63 04    by bmove     $0x80074A8+,b.0x8C,$0x4
0800AB4F: 86                            bi3 clr
0800AB50: 05 E6 8C                      by2 :=       @b.0xFFFFFFFFFFFFFF8C+
0800AB53: 1D 62                         by2 =:       b.0x88
0800AB55: 31 CD 20                      by2 comp     $0x20
0800AB58: C4 08                         if = go      $0x8
0800AB5A: 31 CD 2B                      by2 comp     $0x2B
0800AB5D: C7 00 96                      if >< go     $0x96
0800AB60: 4A 47                         w stz        b.0x1C
0800AB62: 07 45                         by4 :=       b.0x14
0800AB64: 6F 0C                         w4 *         $0xC
0800AB66: FC 5A E3 08 00 74 A8          by rladdr    $0x80074A8+
0800AB6D: 0C 81                         w1 :=        r.0x4
0800AB6F: 20 48                         w1 =:        b.0x20
0800AB71: 06 45                         by3 :=       b.0x14
0800AB73: 6E 0C                         w3 *         $0xC
0800AB75: FC 5A E2 08 00 74 A8          by rladdr    $0x80074A8+
0800AB7C: 0D 82                         w2 :=        r.0x8
0800AB7E: 21 63                         w2 =:        b.0x8C
0800AB80: 34 D1                         w1 comp      r2
0800AB82: C8 3D                         if > go      $0x3D
0800AB84: 04 45                         by1 :=       b.0x14
0800AB86: 6C 0C                         w1 *         $0xC
0800AB88: 05 0D                         by2 :=       $0xD
0800AB8A: FD 20 E0 08 00 74 A8 64 04    by bmove     $0x80074A8+,b.0x90,$0x4
0800AB93: 0E 48                         w3 :=        b.0x20
0800AB95: 2D E6 90 D1                   by comp2     @b.0xFFFFFFFFFFFFFF90+,r2
0800AB99: C6 0B                         if >< go     $0xB
0800AB9B: 0F 47                         w4 :=        b.0x1C
0800AB9D: 19 CD 20 D7 24                by move      $0x20,b.0x24+
0800ABA2: C0 17                         go           $0x17
0800ABA4: 07 45                         by4 :=       b.0x14
0800ABA6: 6F 0C                         w4 *         $0xC
0800ABA8: FD 20 E3 08 00 74 A8 64 04    by bmove     $0x80074A8+,b.0x90,$0x4
0800ABB1: 05 E6 90                      by2 :=       @b.0xFFFFFFFFFFFFFF90+
0800ABB4: 0C 47                         w1 :=        b.0x1C
0800ABB6: 1D D4 24                      by2 =:       b.0x24+
0800ABB9: 4F 47                         w incr       b.0x1C
0800ABBB: BF 48 63 C9                   d loopi      b.0x20,b.0x8C,$0xFFFFFFFFFFFFFFC9
0800ABBF: 1A CD 72 64                   w move       $0x72,b.0x90
0800ABC3: 4D 65                         w set1       b.0x94
0800ABC5: 0C 47                         w1 :=        b.0x1C
0800ABC7: 60 01                         w1 -         $0x1
0800ABC9: 1A D0 6B                      w move       r1,b.0xAC
0800ABCC: 4A 6A                         w stz        b.0xA8
0800ABCE: FD 3D 49                      w2 laddr     b.0x24
0800ABD1: 21 69                         w2 =:        b.0xA4
0800ABD3: FD 20 69 66 0C                by bmove     b.0xA4,b.0x98,$0xC
0800ABD8: 1A 3F 6C                      w move       $0x3F,b.0xB0
0800ABDB: C3 08 00 B9 7C 07 64 65 C5 98 C5 9C C5 A0 6C 46 call         $0x800B97C,$0x7,b.0x90,b.0x94,@b.0xFFFFFFFFFFFFFF98,@b.0xFFFFFFFFFFFFFF9C,@b.0xFFFFFFFFFFFFFFA0,b.0xB0,b.0x18
0800ABEB: 9D                            ifkret
0800ABEC: C3 08 00 89 C8 00             call         $0x80089C8,$0x0
0800ABF2: 9D                            ifkret
0800ABF3: 05 45                         by2 :=       b.0x14
0800ABF5: 55 01                         w2 +         $0x1
0800ABF7: 1D 45                         by2 =:       b.0x14
0800ABF9: FC 1D CE 00 9C                h2 comp      $0x9C
0800ABFE: CF FF 44                      if <= go     $0xFFFFFFFFFFFFFF44
0800AC01: 80                            ret
0800AC02: B8 CF 00 00 00 18             ents         $0x18
0800AC08: 44 C4 08 00 7A 6C             w test       $0x8007A6C
0800AC0E: C6 0E                         if >< go     $0xE
0800AC10: 84                            bi1 clr
0800AC11: 52 45 D0                      w swap       b.0x14,r1
0800AC14: 20 C4 08 00 7A 6C             w1 =:        $0x8007A6C
0800AC1A: C0 13                         go           $0x13
0800AC1C: 18 42                         r:=          b.0x8
0800AC1E: 1A 45 85                      w move       b.0x14,r.0x14
0800AC21: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800AC27: 9D                            ifkret
0800AC28: 18 42                         r:=          b.0x8
0800AC2A: 1A 85 45                      w move       r.0x14,b.0x14
0800AC2D: 80                            ret
0800AC2E: B8 CF 00 00 00 18             ents         $0x18
0800AC34: 0C C4 08 00 7A 6C             w1 :=        $0x8007A6C
0800AC3A: 80                            ret
0800AC3B: B8 CF 00 00 00 24             ents         $0x24
0800AC41: 44 46                         w test       b.0x18
0800AC43: C4 62                         if = go      $0x62
0800AC45: 44 C4 08 00 7A 74             w test       $0x8007A74
0800AC4B: C6 15                         if >< go     $0x15
0800AC4D: 1A 45 C4 08 00 7A 70          w move       b.0x14,$0x8007A70
0800AC54: 84                            bi1 clr
0800AC55: 52 46 D0                      w swap       b.0x18,r1
0800AC58: 20 C4 08 00 7A 74             w1 =:        $0x8007A74
0800AC5E: C0 47                         go           $0x47
0800AC60: 0C 45                         w1 :=        b.0x14
0800AC62: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800AC68: 9D                            ifkret
0800AC69: 20 48                         w1 =:        b.0x20
0800AC6B: 0C C4 08 00 7A 70             w1 :=        $0x8007A70
0800AC71: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800AC77: 9D                            ifkret
0800AC78: 2E 48 D0                      w comp2      b.0x20,r1
0800AC7B: CE 15                         if <= go     $0x15
0800AC7D: 18 42                         r:=          b.0x8
0800AC7F: 1A 46 85                      w move       b.0x18,r.0x14
0800AC82: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800AC88: 9D                            ifkret
0800AC89: 18 42                         r:=          b.0x8
0800AC8B: 1A 85 46                      w move       r.0x14,b.0x18
0800AC8E: C0 17                         go           $0x17
0800AC90: 0D CF 08 00 7A 74             w2 :=        $0x8007A74
0800AC96: 0C 46                         w1 :=        b.0x18
0800AC98: 0E 18                         w3 :=        $0x18
0800AC9A: FE 03                         clrk
0800AC9C: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
0800ACA2: 9D                            ifkret
0800ACA3: 4A 46                         w stz        b.0x18
0800ACA5: 44 47                         w test       b.0x1C
0800ACA7: C4 62                         if = go      $0x62
0800ACA9: 44 C4 08 00 7A 7C             w test       $0x8007A7C
0800ACAF: C6 15                         if >< go     $0x15
0800ACB1: 1A 45 C4 08 00 7A 78          w move       b.0x14,$0x8007A78
0800ACB8: 84                            bi1 clr
0800ACB9: 52 47 D0                      w swap       b.0x1C,r1
0800ACBC: 20 C4 08 00 7A 7C             w1 =:        $0x8007A7C
0800ACC2: C0 47                         go           $0x47
0800ACC4: 0C 45                         w1 :=        b.0x14
0800ACC6: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800ACCC: 9D                            ifkret
0800ACCD: 20 48                         w1 =:        b.0x20
0800ACCF: 0C C4 08 00 7A 78             w1 :=        $0x8007A78
0800ACD5: C3 08 00 83 B8 00             call         $0x80083B8,$0x0
0800ACDB: 9D                            ifkret
0800ACDC: 2E 48 D0                      w comp2      b.0x20,r1
0800ACDF: CE 15                         if <= go     $0x15
0800ACE1: 18 42                         r:=          b.0x8
0800ACE3: 1A 47 85                      w move       b.0x1C,r.0x14
0800ACE6: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800ACEC: 9D                            ifkret
0800ACED: 18 42                         r:=          b.0x8
0800ACEF: 1A 85 47                      w move       r.0x14,b.0x1C
0800ACF2: C0 17                         go           $0x17
0800ACF4: 0D CF 08 00 7A 7C             w2 :=        $0x8007A7C
0800ACFA: 0C 47                         w1 :=        b.0x1C
0800ACFC: 0E 18                         w3 :=        $0x18
0800ACFE: FE 03                         clrk
0800AD00: C3 08 00 CA 0B 00             call         $0x800CA0B,$0x0
0800AD06: 9D                            ifkret
0800AD07: 4A 47                         w stz        b.0x1C
0800AD09: 80                            ret
0800AD0A: 9C                            entd
0800AD0B: FD C0 4A                      l=:          b.0x28
0800AD0E: 18 42                         r:=          b.0x8
0800AD10: 1A 48 85                      w move       b.0x20,r.0x14
0800AD13: C3 08 00 87 71 00             call         $0x8008771,$0x0
0800AD19: D2 04                         if -k go     $0x4
0800AD1B: B4 4A                         jumpg        b.0x28
0800AD1D: 18 42                         r:=          b.0x8
0800AD1F: 1A 86 49                      w move       r.0x18,b.0x24
0800AD22: 1A 49 85                      w move       b.0x24,r.0x14
0800AD25: C3 08 00 A2 53 00             call         $0x800A253,$0x0
0800AD2B: D2 04                         if -k go     $0x4
0800AD2D: B4 4A                         jumpg        b.0x28
0800AD2F: 18 42                         r:=          b.0x8
0800AD31: 1A 85 49                      w move       r.0x14,b.0x24
0800AD34: 1A 45 85                      w move       b.0x14,r.0x14
0800AD37: 1A 49 86                      w move       b.0x24,r.0x18
0800AD3A: C3 08 00 88 FE 00             call         $0x80088FE,$0x0
0800AD40: D2 04                         if -k go     $0x4
0800AD42: B4 4A                         jumpg        b.0x28
0800AD44: 18 42                         r:=          b.0x8
0800AD46: 1A 86 49                      w move       r.0x18,b.0x24
0800AD49: 1A 45 85                      w move       b.0x14,r.0x14
0800AD4C: C3 08 00 88 B3 00             call         $0x80088B3,$0x0
0800AD52: D2 04                         if -k go     $0x4
0800AD54: B4 4A                         jumpg        b.0x28
0800AD56: 18 42                         r:=          b.0x8
0800AD58: 1A 49 85                      w move       b.0x24,r.0x14
0800AD5B: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800AD61: D2 04                         if -k go     $0x4
0800AD63: B4 4A                         jumpg        b.0x28
0800AD65: 18 42                         r:=          b.0x8
0800AD67: 1A 85 49                      w move       r.0x14,b.0x24
0800AD6A: FE 03                         clrk
0800AD6C: B4 4A                         jumpg        b.0x28
0800AD6E: B8 CF 00 00 00 2C             ents         $0x2C
0800AD74: 44 46                         w test       b.0x18
0800AD76: C4 1D                         if = go      $0x1D
0800AD78: 1A C4 08 00 7A 74 48          w move       $0x8007A74,b.0x20
0800AD7F: 44 48                         w test       b.0x20
0800AD81: C4 12                         if = go      $0x12
0800AD83: C3 08 00 AD 0A 00             call         $0x800AD0A,$0x0
0800AD89: 9D                            ifkret
0800AD8A: 18 48                         r:=          b.0x20
0800AD8C: 1A 86 48                      w move       r.0x18,b.0x20
0800AD8F: 44 48                         w test       b.0x20
0800AD91: C6 F2                         if >< go     $0xFFFFFFFFFFFFFFF2
0800AD93: 44 47                         w test       b.0x1C
0800AD95: C4 1D                         if = go      $0x1D
0800AD97: 1A C4 08 00 7A 7C 48          w move       $0x8007A7C,b.0x20
0800AD9E: 44 48                         w test       b.0x20
0800ADA0: C4 12                         if = go      $0x12
0800ADA2: C3 08 00 AD 0A 00             call         $0x800AD0A,$0x0
0800ADA8: 9D                            ifkret
0800ADA9: 18 48                         r:=          b.0x20
0800ADAB: 1A 86 48                      w move       r.0x18,b.0x20
0800ADAE: 44 48                         w test       b.0x20
0800ADB0: C6 F2                         if >< go     $0xFFFFFFFFFFFFFFF2
0800ADB2: 80                            ret
0800ADB3: B8 CF 00 00 00 14             ents         $0x14
0800ADB9: 18 42                         r:=          b.0x8
0800ADBB: 1A C4 08 00 7A 74 85          w move       $0x8007A74,r.0x14
0800ADC2: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800ADC8: 9D                            ifkret
0800ADC9: 18 42                         r:=          b.0x8
0800ADCB: 1A 85 C4 08 00 7A 74          w move       r.0x14,$0x8007A74
0800ADD2: 1A C4 08 00 7A 7C 85          w move       $0x8007A7C,r.0x14
0800ADD9: C3 08 00 87 17 00             call         $0x8008717,$0x0
0800ADDF: 9D                            ifkret
0800ADE0: 18 42                         r:=          b.0x8
0800ADE2: 1A 85 C4 08 00 7A 7C          w move       r.0x14,$0x8007A7C
0800ADE9: 80                            ret
0800ADEA: B8 CF 00 00 00 3C             ents         $0x3C
0800ADF0: 20 48                         w1 =:        b.0x20
0800ADF2: 20 49                         w1 =:        b.0x24
0800ADF4: 85                            bi2 clr
0800ADF5: FD 3D E5 14                   w2 laddr     @b.0x14+
0800ADF9: 21 4D                         w2 =:        b.0x34
0800ADFB: 44 F5 00                      w test       r2.(0x0)
0800ADFE: C6 16                         if >< go     $0x16
0800AE00: FD 3E F5 08                   w3 laddr     r2.(0x8)
0800AE04: 22 F5 00                      w3 =:        r2.(0x0)
0800AE07: 0F CF 00 01 86 9F             w4 :=        $0x1869F
0800AE0D: FD 3F E7 14                   w4 laddr     @b.0x14+
0800AE11: 23 F5 04                      w4 =:        r2.(0x4)
0800AE14: 54 04                         w1 +         $0x4
0800AE16: 60 01                         w1 -         $0x1
0800AE18: 78 04                         w1 /         $0x4
0800AE1A: 20 4A                         w1 =:        b.0x28
0800AE1C: 1A F5 00 4C                   w move       r2.(0x0),b.0x30
0800AE20: FD 3E C5 30                   w3 laddr     @b.0x30
0800AE24: 22 4E                         w3 =:        b.0x38
0800AE26: FD 3E E4 38                   w3 laddr     @b.0x38+
0800AE2A: 22 4B                         w3 =:        b.0x2C
0800AE2C: 36 F5 04                      w3 comp      r2.(0x4)
0800AE2F: DA 06                         if <<= go    $0x6
0800AE31: 84                            bi1 clr
0800AE32: 81                            retk
0800AE33: C0 05                         go           $0x5
0800AE35: 22 F5 00                      w3 =:        r2.(0x0)
0800AE38: FD 3C C5 30                   w1 laddr     @b.0x30
0800AE3C: 20 4E                         w1 =:        b.0x38
0800AE3E: 86                            bi3 clr
0800AE3F: FD 3C E6 38                   w1 laddr     @b.0x38+
0800AE43: 80                            ret
0800AE44: B8 CF 00 00 00 18             ents         $0x18
0800AE4A: 20 45                         w1 =:        b.0x14
0800AE4C: 80                            ret
0800AE4D: B8 CF 00 00 00 24             ents         $0x24
0800AE53: 1A CF 00 06 1A 80 45          w move       $0x61A80,b.0x14
0800AE5A: 1A CE 01 12 47                w move       $0x112,b.0x1C
0800AE5F: 4A 48                         w stz        b.0x20
0800AE61: C3 08 00 B9 7C 04 47 45 48 46 call         $0x800B97C,$0x4,b.0x1C,b.0x14,b.0x20,b.0x18
0800AE6B: 9D                            ifkret
0800AE6C: 0D 46                         w2 :=        b.0x18
0800AE6E: FC AD D1 1B                   w sha        r2,$0x1B
0800AE72: 86                            bi3 clr
0800AE73: 21 E2 08 00 95 64             w2 =:        $0x8009564+
0800AE79: 0F 01                         w4 :=        $0x1
0800AE7B: 4A E3 08 00 95 64             w stz        $0x8009564+
0800AE81: 0C 02                         w1 :=        $0x2
0800AE83: 4D E0 08 00 95 64             w set1       $0x8009564+
0800AE89: 1A C4 08 00 95 64 47          w move       $0x8009564,b.0x1C
0800AE90: 85                            bi2 clr
0800AE91: 4A E5 1C                      w stz        @b.0x1C+
0800AE94: 80                            ret
0800AE95: B8 CF 00 00 00 20             ents         $0x20
0800AE9B: 44 C4 08 00 95 78             w test       $0x8009578
0800AEA1: C6 1E                         if >< go     $0x1E
0800AEA3: 18 42                         r:=          b.0x8
0800AEA5: FD 20 C4 08 00 95 64 85 0C    by bmove     $0x8009564,r.0x14,$0xC
0800AEAE: 0C CD 5C                      w1 :=        $0x5C
0800AEB1: C3 08 00 AD EA 00             call         $0x800ADEA,$0x0
0800AEB7: 9D                            ifkret
0800AEB8: 20 47                         w1 =:        b.0x1C
0800AEBA: 1A 47 46                      w move       b.0x1C,b.0x18
0800AEBD: C0 12                         go           $0x12
0800AEBF: 0C C4 08 00 95 78             w1 :=        $0x8009578
0800AEC5: 20 46                         w1 =:        b.0x18
0800AEC7: 1A F4 18 C4 08 00 95 78       w move       r1.(0x18),$0x8009578
0800AECF: 84                            bi1 clr
0800AED0: 18 46                         r:=          b.0x18
0800AED2: 20 82                         w1 =:        r.0x8
0800AED4: 20 83                         w1 =:        r.0xC
0800AED6: 4A 84                         w stz        r.0x10
0800AED8: 0C 46                         w1 :=        b.0x18
0800AEDA: 80                            ret
0800AEDB: B8 CF 00 00 00 18             ents         $0x18
0800AEE1: 18 45                         r:=          b.0x14
0800AEE3: 1A C4 08 00 95 78 86          w move       $0x8009578,r.0x18
0800AEEA: 84                            bi1 clr
0800AEEB: 52 45 D0                      w swap       b.0x14,r1
0800AEEE: 20 C4 08 00 95 78             w1 =:        $0x8009578
0800AEF4: 80                            ret
0800AEF5: B8 CF 00 00 00 30             ents         $0x30
0800AEFB: 1A 3F 4B                      w move       $0x3F,b.0x2C
0800AEFE: FD 4B 02 4A                   by wconv     $0x2,b.0x28
0800AF02: 18 45                         r:=          b.0x14
0800AF04: FD 3D 87                      w2 laddr     r.0x1C
0800AF07: 55 4A                         w2 +         b.0x28
0800AF09: 04 F5 00                      by1 :=       r2.(0x0)
0800AF0C: 1C 49                         by1 =:       b.0x24
0800AF0E: 30 CD 27                      by1 comp     $0x27
0800AF11: C4 0F                         if = go      $0xF
0800AF13: 0E 4B                         w3 :=        b.0x2C
0800AF15: 56 01                         w3 +         $0x1
0800AF17: 22 4B                         w3 =:        b.0x2C
0800AF19: 1C E6 18                      by1 =:       @b.0x18+
0800AF1C: BF 4A 11 E6                   d loopi      b.0x28,$0x11,$0xFFFFFFFFFFFFFFE6
0800AF20: 18 45                         r:=          b.0x14
0800AF22: FD 3C 87                      w1 laddr     r.0x1C
0800AF25: 54 12                         w1 +         $0x12
0800AF27: 2D F4 00 CD 27                by comp2     r1.(0x0),$0x27
0800AF2C: C4 30                         if = go      $0x30
0800AF2E: 0D 4B                         w2 :=        b.0x2C
0800AF30: 55 01                         w2 +         $0x1
0800AF32: 21 4B                         w2 =:        b.0x2C
0800AF34: 06 CD 3A                      by3 :=       $0x3A
0800AF37: 1E E5 18                      by3 =:       @b.0x18+
0800AF3A: FD 4B 12 4A                   by wconv     $0x12,b.0x28
0800AF3E: 18 45                         r:=          b.0x14
0800AF40: FD 3D 87                      w2 laddr     r.0x1C
0800AF43: 55 4A                         w2 +         b.0x28
0800AF45: 04 F5 00                      by1 :=       r2.(0x0)
0800AF48: 1C 49                         by1 =:       b.0x24
0800AF4A: 30 CD 27                      by1 comp     $0x27
0800AF4D: C4 0F                         if = go      $0xF
0800AF4F: 0E 4B                         w3 :=        b.0x2C
0800AF51: 56 01                         w3 +         $0x1
0800AF53: 22 4B                         w3 =:        b.0x2C
0800AF55: 1C E6 18                      by1 =:       @b.0x18+
0800AF58: BF 4A 15 E6                   d loopi      b.0x28,$0x15,$0xFFFFFFFFFFFFFFE6
0800AF5C: 0C 4B                         w1 :=        b.0x2C
0800AF5E: 54 01                         w1 +         $0x1
0800AF60: 20 4B                         w1 =:        b.0x2C
0800AF62: 05 0D                         by2 :=       $0xD
0800AF64: 1D E4 18                      by2 =:       @b.0x18+
0800AF67: 80                            ret
0800AF68: B8 CF 00 00 00 18             ents         $0x18
0800AF6E: 1A 45 C4 08 00 95 7C          w move       b.0x14,$0x800957C
0800AF75: 80                            ret
0800AF76: B8 CF 00 00 00 60             ents         $0x60
0800AF7C: C0 28                         go           $0x28
0800AF7E: 9C                            entd
0800AF7F: FD C0 4E                      l=:          b.0x38
0800AF82: 20 43                         w1 =:        b.0xC
0800AF84: 44 C4 08 00 95 7C             w test       $0x800957C
0800AF8A: C4 13                         if = go      $0x13
0800AF8C: 18 42                         r:=          b.0x8
0800AF8E: FD 20 45 85 0C                by bmove     b.0x14,r.0x14,$0xC
0800AF93: 20 88                         w1 =:        r.0x20
0800AF95: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800AF9C: 9D                            ifkret
0800AF9D: 0C 43                         w1 :=        b.0xC
0800AF9F: 81                            retk
0800AFA0: FE 03                         clrk
0800AFA2: B4 4E                         jumpg        b.0x38
0800AFA4: C3 08 00 AE 95 00             call         $0x800AE95,$0x0
0800AFAA: D2 08                         if -k go     $0x8
0800AFAC: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800AFB2: 20 4D                         w1 =:        b.0x34
0800AFB4: 19 48 F4 04                   by move      b.0x20,r1.(0x4)
0800AFB8: 2D 48 CD 52                   by comp2     b.0x20,$0x52
0800AFBC: C6 35                         if >< go     $0x35
0800AFBE: 18 42                         r:=          b.0x8
0800AFC0: FE 79 C4 08 00 A6 68 86 03    w bmove      $0x800A668,r.0x18,$0x3
0800AFC9: FD 20 45 89 0C                by bmove     b.0x14,r.0x24,$0xC
0800AFCE: FE 79 C4 08 00 A6 74 8C 03    w bmove      $0x800A674,r.0x30,$0x3
0800AFD7: C3 08 00 C0 B2 00             call         $0x800C0B2,$0x0
0800AFDD: D2 08                         if -k go     $0x8
0800AFDF: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800AFE5: 18 42                         r:=          b.0x8
0800AFE7: 0D 85                         w2 :=        r.0x14
0800AFE9: 18 4D                         r:=          b.0x34
0800AFEB: 21 82                         w2 =:        r.0x8
0800AFED: 4A 4A                         w stz        b.0x28
0800AFEF: C0 54                         go           $0x54
0800AFF1: 2D 48 CD 57                   by comp2     b.0x20,$0x57
0800AFF5: C6 35                         if >< go     $0x35
0800AFF7: 18 42                         r:=          b.0x8
0800AFF9: FE 79 C4 08 00 A6 84 86 03    w bmove      $0x800A684,r.0x18,$0x3
0800B002: FD 20 45 89 0C                by bmove     b.0x14,r.0x24,$0xC
0800B007: FE 79 C4 08 00 A6 90 8C 03    w bmove      $0x800A690,r.0x30,$0x3
0800B010: C3 08 00 C0 B2 00             call         $0x800C0B2,$0x0
0800B016: D2 08                         if -k go     $0x8
0800B018: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B01E: 18 42                         r:=          b.0x8
0800B020: 0D 85                         w2 :=        r.0x14
0800B022: 18 4D                         r:=          b.0x34
0800B024: 21 82                         w2 =:        r.0x8
0800B026: 4D 4A                         w set1       b.0x28
0800B028: C0 1B                         go           $0x1B
0800B02A: 18 42                         r:=          b.0x8
0800B02C: 20 85                         w1 =:        r.0x14
0800B02E: C3 08 00 AE DB 00             call         $0x800AEDB,$0x0
0800B034: D2 08                         if -k go     $0x8
0800B036: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B03C: 18 42                         r:=          b.0x8
0800B03E: 1A 85 4D                      w move       r.0x14,b.0x34
0800B041: 84                            bi1 clr
0800B042: 80                            ret
0800B043: 1A CE 00 B3 4F                w move       $0xB3,b.0x3C
0800B048: 18 4D                         r:=          b.0x34
0800B04A: FD 3D 82                      w2 laddr     r.0x8
0800B04D: 21 50                         w2 =:        b.0x40
0800B04F: C3 08 00 B9 7C 05 4F C5 40 4A 4B 4C       call         $0x800B97C,$0x5,b.0x3C,@b.0x40,b.0x28,b.0x2C,b.0x30
0800B05B: D2 08                         if -k go     $0x8
0800B05D: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B063: 0D 4C                         w2 :=        b.0x30
0800B065: E5 01                         w2 and       $0x1
0800B067: 35 01                         w2 comp      $0x1
0800B069: C6 35                         if >< go     $0x35
0800B06B: 4A C5 34                      w stz        @b.0x34
0800B06E: 0D 02                         w2 :=        $0x2
0800B070: 18 4D                         r:=          b.0x34
0800B072: FD 3F 87                      w4 laddr     r.0x1C
0800B075: 23 4F                         w4 =:        b.0x3C
0800B077: FE 26 E5 3C                   by3 laddr    @b.0x3C+
0800B07B: 22 52                         w3 =:        b.0x48
0800B07D: 0C 0A                         w1 :=        $0xA
0800B07F: 54 3F                         w1 +         $0x3F
0800B081: 20 51                         w1 =:        b.0x44
0800B083: CA 0C                         if < go      $0xC
0800B085: 84                            bi1 clr
0800B086: 85                            bi2 clr
0800B087: FD 67 C4 08 00 A6 A8 51       by smove     $0x800A6A8,b.0x44
0800B08F: 04 CD 27                      by1 :=       $0x27
0800B092: 18 4D                         r:=          b.0x34
0800B094: FD 3D 87                      w2 laddr     r.0x1C
0800B097: 55 12                         w2 +         $0x12
0800B099: 1C F5 00                      by1 =:       r2.(0x0)
0800B09C: C0 49                         go           $0x49
0800B09E: 4D C5 34                      w set1       @b.0x34
0800B0A1: 18 4D                         r:=          b.0x34
0800B0A3: 0C 82                         w1 :=        r.0x8
0800B0A5: 18 42                         r:=          b.0x8
0800B0A7: 20 85                         w1 =:        r.0x14
0800B0A9: 0C CE 08 00                   w1 :=        $0x800
0800B0AD: C3 08 00 C0 99 00             call         $0x800C099,$0x0
0800B0B3: D2 08                         if -k go     $0x8
0800B0B5: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B0BB: 1A CD 21 4F                   w move       $0x21,b.0x3C
0800B0BF: 18 4D                         r:=          b.0x34
0800B0C1: FD 3D 82                      w2 laddr     r.0x8
0800B0C4: 21 50                         w2 =:        b.0x40
0800B0C6: FD 3F 87                      w4 laddr     r.0x1C
0800B0C9: 23 57                         w4 =:        b.0x5C
0800B0CB: 84                            bi1 clr
0800B0CC: FE 2A E4 5C                   h3 laddr     @b.0x5C+
0800B0D0: 22 56                         w3 =:        b.0x58
0800B0D2: C3 08 00 B9 7C 03 4F C5 40 C5 58    call         $0x800B97C,$0x3,b.0x3C,@b.0x40,@b.0x58
0800B0DD: D2 08                         if -k go     $0x8
0800B0DF: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B0E5: 2D 48 CD 57                   by comp2     b.0x20,$0x57
0800B0E9: C6 08                         if >< go     $0x8
0800B0EB: 18 4D                         r:=          b.0x34
0800B0ED: 4A 85                         w stz        r.0x14
0800B0EF: C0 1C                         go           $0x1C
0800B0F1: 18 4D                         r:=          b.0x34
0800B0F3: 0C 82                         w1 :=        r.0x8
0800B0F5: 18 42                         r:=          b.0x8
0800B0F7: 20 85                         w1 =:        r.0x14
0800B0F9: C3 08 00 C3 1A 00             call         $0x800C31A,$0x0
0800B0FF: D2 08                         if -k go     $0x8
0800B101: C3 08 00 AF 7E 00             call         $0x800AF7E,$0x0
0800B107: 18 4D                         r:=          b.0x34
0800B109: 20 85                         w1 =:        r.0x14
0800B10B: 0C 4D                         w1 :=        b.0x34
0800B10D: 80                            ret
0800B10E: B8 CF 00 00 00 5C             ents         $0x5C
0800B114: C0 5C                         go           $0x5C
0800B116: 9C                            entd
0800B117: FD C0 4D                      l=:          b.0x34
0800B11A: 20 43                         w1 =:        b.0xC
0800B11C: 44 C4 08 00 95 7C             w test       $0x800957C
0800B122: C4 47                         if = go      $0x47
0800B124: 44 C5 14                      w test       @b.0x14
0800B127: C6 15                         if >< go     $0x15
0800B129: 18 42                         r:=          b.0x8
0800B12B: FE 79 00 85 03                w bmove      $0x0,r.0x14,$0x3
0800B130: 20 88                         w1 =:        r.0x20
0800B132: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B139: 9D                            ifkret
0800B13A: C0 2F                         go           $0x2F
0800B13C: 18 42                         r:=          b.0x8
0800B13E: 1A 45 85                      w move       b.0x14,r.0x14
0800B141: FD 3D 47                      w2 laddr     b.0x1C
0800B144: 21 86                         w2 =:        r.0x18
0800B146: 4A 87                         w stz        r.0x1C
0800B148: 1A 15 88                      w move       $0x15,r.0x20
0800B14B: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B151: 9D                            ifkret
0800B152: FD 3D 47                      w2 laddr     b.0x1C
0800B155: 18 42                         r:=          b.0x8
0800B157: 21 85                         w2 =:        r.0x14
0800B159: 4A 86                         w stz        r.0x18
0800B15B: 1A 15 87                      w move       $0x15,r.0x1C
0800B15E: 1A 43 88                      w move       b.0xC,r.0x20
0800B161: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B168: 9D                            ifkret
0800B169: 0C 43                         w1 :=        b.0xC
0800B16B: 81                            retk
0800B16C: FE 03                         clrk
0800B16E: B4 4D                         jumpg        b.0x34
0800B170: 18 45                         r:=          b.0x14
0800B172: 44 82                         w test       r.0x8
0800B174: C5 00 ED                      if = go      $0xED
0800B177: 44 C5 14                      w test       @b.0x14
0800B17A: C6 5A                         if >< go     $0x5A
0800B17C: FD 50 C4 08 00 95 70 D1       h wconv      $0x8009570,r2
0800B184: 44 D1                         w test       r2
0800B186: CE 4C                         if <= go     $0x4C
0800B188: 1A CD 72 4E                   w move       $0x72,b.0x38
0800B18C: FD 3D 82                      w2 laddr     r.0x8
0800B18F: 21 4F                         w2 =:        b.0x3C
0800B191: 0A C4 08 00 95 70             h3 :=        $0x8009570
0800B197: FC 42 01                      h3 -         $0x1
0800B19A: FD 50 D2 D3                   h wconv      r3,r4
0800B19E: 1A D3 55                      w move       r4,b.0x54
0800B1A1: 4A 54                         w stz        b.0x50
0800B1A3: 0C CF 08 00 95 80             w1 :=        $0x8009580
0800B1A9: 20 53                         w1 =:        b.0x4C
0800B1AB: FD 20 53 50 0C                by bmove     b.0x4C,b.0x40,$0xC
0800B1B0: 1A 3F 56                      w move       $0x3F,b.0x58
0800B1B3: C3 08 00 B9 7C 07 4E C5 3C C5 40 C5 44 C5 48 56 46 call         $0x800B97C,$0x7,b.0x38,@b.0x3C,@b.0x40,@b.0x44,@b.0x48,b.0x58,b.0x18
0800B1C4: D2 08                         if -k go     $0x8
0800B1C6: C3 08 00 B1 16 00             call         $0x800B116,$0x0
0800B1CC: 49 C4 08 00 95 70             h stz        $0x8009570
0800B1D2: C0 79                         go           $0x79
0800B1D4: 2D 81 CD 52                   by comp2     r.0x4,$0x52
0800B1D8: C6 17                         if >< go     $0x17
0800B1DA: FD 50 C4 08 00 95 72 D1       h wconv      $0x8009572,r2
0800B1E2: 2E 82 D1                      w comp2      r.0x8,r2
0800B1E5: C6 08                         if >< go     $0x8
0800B1E7: 49 C4 08 00 95 72             h stz        $0x8009572
0800B1ED: C0 5E                         go           $0x5E
0800B1EF: FD 50 C4 08 00 95 74 D2       h wconv      $0x8009574,r3
0800B1F7: 2E 82 D2                      w comp2      r.0x8,r3
0800B1FA: C6 35                         if >< go     $0x35
0800B1FC: FD 50 C4 08 00 95 74 D3       h wconv      $0x8009574,r4
0800B204: 18 42                         r:=          b.0x8
0800B206: 23 85                         w4 =:        r.0x14
0800B208: FD 50 C4 08 00 95 76 D0       h wconv      $0x8009576,r1
0800B210: 20 86                         w1 =:        r.0x18
0800B212: FE 79 C4 08 00 A6 B0 87 03    w bmove      $0x800A6B0,r.0x1C,$0x3
0800B21B: C3 08 00 C3 81 00             call         $0x800C381,$0x0
0800B221: D2 08                         if -k go     $0x8
0800B223: C3 08 00 B1 16 00             call         $0x800B116,$0x0
0800B229: 49 C4 08 00 95 74             h stz        $0x8009574
0800B22F: 18 45                         r:=          b.0x14
0800B231: 0D 85                         w2 :=        r.0x14
0800B233: 61 01                         w2 -         $0x1
0800B235: 0E 82                         w3 :=        r.0x8
0800B237: 18 42                         r:=          b.0x8
0800B239: 22 85                         w3 =:        r.0x14
0800B23B: 0C D1                         w1 :=        r2
0800B23D: C3 08 00 C3 05 00             call         $0x800C305,$0x0
0800B243: D2 08                         if -k go     $0x8
0800B245: C3 08 00 B1 16 00             call         $0x800B116,$0x0
0800B24B: 18 45                         r:=          b.0x14
0800B24D: 0D 82                         w2 :=        r.0x8
0800B24F: 18 42                         r:=          b.0x8
0800B251: 21 85                         w2 =:        r.0x14
0800B253: C3 08 00 C2 F2 00             call         $0x800C2F2,$0x0
0800B259: D2 08                         if -k go     $0x8
0800B25B: C3 08 00 B1 16 00             call         $0x800B116,$0x0
0800B261: 18 42                         r:=          b.0x8
0800B263: 1A 45 85                      w move       b.0x14,r.0x14
0800B266: C3 08 00 AE DB 00             call         $0x800AEDB,$0x0
0800B26C: D2 08                         if -k go     $0x8
0800B26E: C3 08 00 B1 16 00             call         $0x800B116,$0x0
0800B274: 18 42                         r:=          b.0x8
0800B276: 1A 85 45                      w move       r.0x14,b.0x14
0800B279: 80                            ret
0800B27A: B8 CF 00 00 00 34             ents         $0x34
0800B280: C0 5C                         go           $0x5C
0800B282: 9C                            entd
0800B283: FD C0 4C                      l=:          b.0x30
0800B286: 20 43                         w1 =:        b.0xC
0800B288: 44 C4 08 00 95 7C             w test       $0x800957C
0800B28E: C4 47                         if = go      $0x47
0800B290: 44 C5 14                      w test       @b.0x14
0800B293: C6 15                         if >< go     $0x15
0800B295: 18 42                         r:=          b.0x8
0800B297: FE 79 00 85 03                w bmove      $0x0,r.0x14,$0x3
0800B29C: 20 88                         w1 =:        r.0x20
0800B29E: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B2A5: 9D                            ifkret
0800B2A6: C0 2F                         go           $0x2F
0800B2A8: 18 42                         r:=          b.0x8
0800B2AA: 1A 45 85                      w move       b.0x14,r.0x14
0800B2AD: FD 3D 46                      w2 laddr     b.0x18
0800B2B0: 21 86                         w2 =:        r.0x18
0800B2B2: 4A 87                         w stz        r.0x1C
0800B2B4: 1A 15 88                      w move       $0x15,r.0x20
0800B2B7: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B2BD: 9D                            ifkret
0800B2BE: FD 3D 46                      w2 laddr     b.0x18
0800B2C1: 18 42                         r:=          b.0x8
0800B2C3: 21 85                         w2 =:        r.0x14
0800B2C5: 4A 86                         w stz        r.0x18
0800B2C7: 1A 15 87                      w move       $0x15,r.0x1C
0800B2CA: 1A 43 88                      w move       b.0xC,r.0x20
0800B2CD: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B2D4: 9D                            ifkret
0800B2D5: 0C 43                         w1 :=        b.0xC
0800B2D7: 81                            retk
0800B2D8: FE 03                         clrk
0800B2DA: B4 4C                         jumpg        b.0x30
0800B2DC: 44 C5 14                      w test       @b.0x14
0800B2DF: C4 5A                         if = go      $0x5A
0800B2E1: 18 45                         r:=          b.0x14
0800B2E3: 2D 81 CD 52                   by comp2     r.0x4,$0x52
0800B2E7: C6 35                         if >< go     $0x35
0800B2E9: 44 83                         w test       r.0xC
0800B2EB: CE 17                         if <= go     $0x17
0800B2ED: FD 50 C4 08 00 95 72 D1       h wconv      $0x8009572,r2
0800B2F5: 2E 82 D1                      w comp2      r.0x8,r2
0800B2F8: C6 08                         if >< go     $0x8
0800B2FA: 49 C4 08 00 95 72             h stz        $0x8009572
0800B300: 4A 83                         w stz        r.0xC
0800B302: 0E 82                         w3 :=        r.0x8
0800B304: 18 42                         r:=          b.0x8
0800B306: 22 85                         w3 =:        r.0x14
0800B308: C3 08 00 C3 1A 00             call         $0x800C31A,$0x0
0800B30E: D2 08                         if -k go     $0x8
0800B310: C3 08 00 B2 82 00             call         $0x800B282,$0x0
0800B316: 18 45                         r:=          b.0x14
0800B318: 20 85                         w1 =:        r.0x14
0800B31A: C0 1D                         go           $0x1D
0800B31C: 44 83                         w test       r.0xC
0800B31E: CE 17                         if <= go     $0x17
0800B320: FD 50 C4 08 00 95 74 D1       h wconv      $0x8009574,r2
0800B328: 2E 82 D1                      w comp2      r.0x8,r2
0800B32B: C6 08                         if >< go     $0x8
0800B32D: 49 C4 08 00 95 74             h stz        $0x8009574
0800B333: 4A 83                         w stz        r.0xC
0800B335: 4A 85                         w stz        r.0x14
0800B337: 4A 84                         w stz        r.0x10
0800B339: 80                            ret
0800B33A: B8 CF 00 00 00 3C             ents         $0x3C
0800B340: 20 46                         w1 =:        b.0x18
0800B342: C0 5C                         go           $0x5C
0800B344: 9C                            entd
0800B345: FD C0 4E                      l=:          b.0x38
0800B348: 20 43                         w1 =:        b.0xC
0800B34A: 44 C4 08 00 95 7C             w test       $0x800957C
0800B350: C4 47                         if = go      $0x47
0800B352: 44 C5 14                      w test       @b.0x14
0800B355: C6 15                         if >< go     $0x15
0800B357: 18 42                         r:=          b.0x8
0800B359: FE 79 00 85 03                w bmove      $0x0,r.0x14,$0x3
0800B35E: 20 88                         w1 =:        r.0x20
0800B360: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B367: 9D                            ifkret
0800B368: C0 2F                         go           $0x2F
0800B36A: 18 42                         r:=          b.0x8
0800B36C: 1A 45 85                      w move       b.0x14,r.0x14
0800B36F: FD 3D 48                      w2 laddr     b.0x20
0800B372: 21 86                         w2 =:        r.0x18
0800B374: 4A 87                         w stz        r.0x1C
0800B376: 1A 15 88                      w move       $0x15,r.0x20
0800B379: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B37F: 9D                            ifkret
0800B380: FD 3D 48                      w2 laddr     b.0x20
0800B383: 18 42                         r:=          b.0x8
0800B385: 21 85                         w2 =:        r.0x14
0800B387: 4A 86                         w stz        r.0x18
0800B389: 1A 15 87                      w move       $0x15,r.0x1C
0800B38C: 1A 43 88                      w move       b.0xC,r.0x20
0800B38F: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B396: 9D                            ifkret
0800B397: 0C 43                         w1 :=        b.0xC
0800B399: 81                            retk
0800B39A: FE 03                         clrk
0800B39C: B4 4E                         jumpg        b.0x38
0800B39E: 44 C5 14                      w test       @b.0x14
0800B3A1: C5 00 9D                      if = go      $0x9D
0800B3A4: 0C 46                         w1 :=        b.0x18
0800B3A6: 78 CE 08 00                   w1 /         $0x800
0800B3AA: 20 47                         w1 =:        b.0x1C
0800B3AC: 18 45                         r:=          b.0x14
0800B3AE: 2D 81 CD 52                   by comp2     r.0x4,$0x52
0800B3B2: C6 36                         if >< go     $0x36
0800B3B4: 2E 83 D0                      w comp2      r.0xC,r1
0800B3B7: C4 15                         if = go      $0x15
0800B3B9: FD 50 C4 08 00 95 72 D1       h wconv      $0x8009572,r2
0800B3C1: 2E 82 D1                      w comp2      r.0x8,r2
0800B3C4: C6 08                         if >< go     $0x8
0800B3C6: 49 C4 08 00 95 72             h stz        $0x8009572
0800B3CC: 0E 82                         w3 :=        r.0x8
0800B3CE: 18 42                         r:=          b.0x8
0800B3D0: 22 85                         w3 =:        r.0x14
0800B3D2: C3 08 00 C3 1A 00             call         $0x800C31A,$0x0
0800B3D8: D2 08                         if -k go     $0x8
0800B3DA: C3 08 00 B3 44 00             call         $0x800B344,$0x0
0800B3E0: 60 46                         w1 -         b.0x18
0800B3E2: 18 45                         r:=          b.0x14
0800B3E4: 20 85                         w1 =:        r.0x14
0800B3E6: C0 4C                         go           $0x4C
0800B3E8: 2E 83 D0                      w comp2      r.0xC,r1
0800B3EB: C4 42                         if = go      $0x42
0800B3ED: FD 50 C4 08 00 95 74 D1       h wconv      $0x8009574,r2
0800B3F5: 2E 82 D1                      w comp2      r.0x8,r2
0800B3F8: C6 35                         if >< go     $0x35
0800B3FA: FD 50 C4 08 00 95 74 D2       h wconv      $0x8009574,r3
0800B402: 18 42                         r:=          b.0x8
0800B404: 22 85                         w3 =:        r.0x14
0800B406: FD 50 C4 08 00 95 76 D3       h wconv      $0x8009576,r4
0800B40E: 23 86                         w4 =:        r.0x18
0800B410: FE 79 C4 08 00 A6 BC 87 03    w bmove      $0x800A6BC,r.0x1C,$0x3
0800B419: C3 08 00 C3 81 00             call         $0x800C381,$0x0
0800B41F: D2 08                         if -k go     $0x8
0800B421: C3 08 00 B3 44 00             call         $0x800B344,$0x0
0800B427: 49 C4 08 00 95 74             h stz        $0x8009574
0800B42D: 18 45                         r:=          b.0x14
0800B42F: 1A 46 85                      w move       b.0x18,r.0x14
0800B432: 1A 47 83                      w move       b.0x1C,r.0xC
0800B435: FC 7C 46 CE 08 00 D1          w1 div4      b.0x18,$0x800,r2
0800B43C: 20 84                         w1 =:        r.0x10
0800B43E: 80                            ret
0800B43F: B8 CF 00 00 00 34             ents         $0x34
0800B445: C0 5E                         go           $0x5E
0800B447: 9C                            entd
0800B448: FD C0 4C                      l=:          b.0x30
0800B44B: 20 43                         w1 =:        b.0xC
0800B44D: 44 C4 08 00 95 7C             w test       $0x800957C
0800B453: C4 49                         if = go      $0x49
0800B455: 44 C5 14                      w test       @b.0x14
0800B458: C6 15                         if >< go     $0x15
0800B45A: 18 42                         r:=          b.0x8
0800B45C: FE 79 00 85 03                w bmove      $0x0,r.0x14,$0x3
0800B461: 20 88                         w1 =:        r.0x20
0800B463: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B46A: 9D                            ifkret
0800B46B: C0 31                         go           $0x31
0800B46D: 18 42                         r:=          b.0x8
0800B46F: 1A 45 85                      w move       b.0x14,r.0x14
0800B472: FD 3D C1 1A                   w2 laddr     b.0x1A
0800B476: 21 86                         w2 =:        r.0x18
0800B478: 4A 87                         w stz        r.0x1C
0800B47A: 1A 15 88                      w move       $0x15,r.0x20
0800B47D: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B483: 9D                            ifkret
0800B484: FD 3D C1 1A                   w2 laddr     b.0x1A
0800B488: 18 42                         r:=          b.0x8
0800B48A: 21 85                         w2 =:        r.0x14
0800B48C: 4A 86                         w stz        r.0x18
0800B48E: 1A 15 87                      w move       $0x15,r.0x1C
0800B491: 1A 43 88                      w move       b.0xC,r.0x20
0800B494: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B49B: 9D                            ifkret
0800B49C: 0C 43                         w1 :=        b.0xC
0800B49E: 81                            retk
0800B49F: FE 03                         clrk
0800B4A1: B4 4C                         jumpg        b.0x30
0800B4A3: 18 45                         r:=          b.0x14
0800B4A5: 44 85                         w test       r.0x14
0800B4A7: C6 07                         if >< go     $0x7
0800B4A9: 04 17                         by1 :=       $0x17
0800B4AB: 80                            ret
0800B4AC: C0 74                         go           $0x74
0800B4AE: 2E 84 CE 08 00                w comp2      r.0x10,$0x800
0800B4B3: C6 10                         if >< go     $0x10
0800B4B5: 0D 83                         w2 :=        r.0xC
0800B4B7: 55 01                         w2 +         $0x1
0800B4B9: 21 83                         w2 =:        r.0xC
0800B4BB: 4A 84                         w stz        r.0x10
0800B4BD: 49 C4 08 00 95 72             h stz        $0x8009572
0800B4C3: FD 50 C4 08 00 95 72 D2       h wconv      $0x8009572,r3
0800B4CB: 2E 82 D2                      w comp2      r.0x8,r3
0800B4CE: C4 32                         if = go      $0x32
0800B4D0: 0F 82                         w4 :=        r.0x8
0800B4D2: FC 13 C4 08 00 95 72          h4 =:        $0x8009572
0800B4D9: FD 50 D3 D0                   h wconv      r4,r1
0800B4DD: 18 42                         r:=          b.0x8
0800B4DF: 20 85                         w1 =:        r.0x14
0800B4E1: 18 45                         r:=          b.0x14
0800B4E3: 0D 83                         w2 :=        r.0xC
0800B4E5: 18 42                         r:=          b.0x8
0800B4E7: 21 86                         w2 =:        r.0x18
0800B4E9: FE 79 C4 08 00 A6 C8 87 03    w bmove      $0x800A6C8,r.0x1C,$0x3
0800B4F2: C3 08 00 C3 2D 00             call         $0x800C32D,$0x0
0800B4F8: D2 08                         if -k go     $0x8
0800B4FA: C3 08 00 B4 47 00             call         $0x800B447,$0x0
0800B500: 18 45                         r:=          b.0x14
0800B502: 0E 84                         w3 :=        r.0x10
0800B504: 05 E2 08 00 95 E4             by2 :=       $0x80095E4+
0800B50A: FC 91 CD 7F                   by2 and      $0x7F
0800B50E: 1D C1 19                      by2 =:       b.0x19
0800B511: 0F 84                         w4 :=        r.0x10
0800B513: 57 01                         w4 +         $0x1
0800B515: 23 84                         w4 =:        r.0x10
0800B517: 0C 85                         w1 :=        r.0x14
0800B519: 60 01                         w1 -         $0x1
0800B51B: 20 85                         w1 =:        r.0x14
0800B51D: 04 D1                         by1 :=       r2
0800B51F: 80                            ret
0800B520: B8 CF 00 00 00 60             ents         $0x60
0800B526: 1C 46                         by1 =:       b.0x18
0800B528: C0 5C                         go           $0x5C
0800B52A: 9C                            entd
0800B52B: FD C0 4E                      l=:          b.0x38
0800B52E: 20 43                         w1 =:        b.0xC
0800B530: 44 C4 08 00 95 7C             w test       $0x800957C
0800B536: C4 47                         if = go      $0x47
0800B538: 44 C5 14                      w test       @b.0x14
0800B53B: C6 15                         if >< go     $0x15
0800B53D: 18 42                         r:=          b.0x8
0800B53F: FE 79 00 85 03                w bmove      $0x0,r.0x14,$0x3
0800B544: 20 88                         w1 =:        r.0x20
0800B546: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B54D: 9D                            ifkret
0800B54E: C0 2F                         go           $0x2F
0800B550: 18 42                         r:=          b.0x8
0800B552: 1A 45 85                      w move       b.0x14,r.0x14
0800B555: FD 3D 48                      w2 laddr     b.0x20
0800B558: 21 86                         w2 =:        r.0x18
0800B55A: 4A 87                         w stz        r.0x1C
0800B55C: 1A 15 88                      w move       $0x15,r.0x20
0800B55F: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B565: 9D                            ifkret
0800B566: FD 3D 48                      w2 laddr     b.0x20
0800B569: 18 42                         r:=          b.0x8
0800B56B: 21 85                         w2 =:        r.0x14
0800B56D: 4A 86                         w stz        r.0x18
0800B56F: 1A 15 87                      w move       $0x15,r.0x1C
0800B572: 1A 43 88                      w move       b.0xC,r.0x20
0800B575: B5 C4 08 00 95 7C 00          callg        $0x800957C,$0x0
0800B57C: 9D                            ifkret
0800B57D: 0C 43                         w1 :=        b.0xC
0800B57F: 81                            retk
0800B580: FE 03                         clrk
0800B582: B4 4E                         jumpg        b.0x38
0800B584: 44 C5 14                      w test       @b.0x14
0800B587: C6 78                         if >< go     $0x78
0800B589: FD 50 C4 08 00 95 70 D0       h wconv      $0x8009570,r1
0800B591: 19 46 E0 08 00 95 80          by move      b.0x18,$0x8009580+
0800B598: 4E C4 08 00 95 70             h incr       $0x8009570
0800B59E: 2D 46 0A                      by comp2     b.0x18,$0xA
0800B5A1: C4 0F                         if = go      $0xF
0800B5A3: FD 50 C4 08 00 95 70 D1       h wconv      $0x8009570,r2
0800B5AB: 35 CD 63                      w2 comp      $0x63
0800B5AE: CE 4E                         if <= go     $0x4E
0800B5B0: 1A CD 72 4F                   w move       $0x72,b.0x3C
0800B5B4: 18 45                         r:=          b.0x14
0800B5B6: FD 3D 82                      w2 laddr     r.0x8
0800B5B9: 21 50                         w2 =:        b.0x40
0800B5BB: 0A C4 08 00 95 70             h3 :=        $0x8009570
0800B5C1: FC 42 01                      h3 -         $0x1
0800B5C4: FD 50 D2 D3                   h wconv      r3,r4
0800B5C8: 1A D3 56                      w move       r4,b.0x58
0800B5CB: 4A 55                         w stz        b.0x54
0800B5CD: 0C CF 08 00 95 80             w1 :=        $0x8009580
0800B5D3: 20 54                         w1 =:        b.0x50
0800B5D5: FD 20 54 51 0C                by bmove     b.0x50,b.0x44,$0xC
0800B5DA: 1A 3F 57                      w move       $0x3F,b.0x5C
0800B5DD: C3 08 00 B9 7C 07 4F C5 40 C5 44 C5 48 C5 4C 57 47 call         $0x800B97C,$0x7,b.0x3C,@b.0x40,@b.0x44,@b.0x48,@b.0x4C,b.0x5C,b.0x1C
0800B5EE: D2 08                         if -k go     $0x8
0800B5F0: C3 08 00 B5 2A 00             call         $0x800B52A,$0x0
0800B5F6: 49 C4 08 00 95 70             h stz        $0x8009570
0800B5FC: C1 00 D1                      go           $0xD1
0800B5FF: FD 50 C4 08 00 95 74 D1       h wconv      $0x8009574,r2
0800B607: 18 45                         r:=          b.0x14
0800B609: 2E 82 D1                      w comp2      r.0x8,r2
0800B60C: C4 0E                         if = go      $0xE
0800B60E: FD 50 C4 08 00 95 74 D2       h wconv      $0x8009574,r3
0800B616: 44 D2                         w test       r3
0800B618: C6 09                         if >< go     $0x9
0800B61A: 2E 84 CE 08 00                w comp2      r.0x10,$0x800
0800B61F: C6 35                         if >< go     $0x35
0800B621: FD 50 C4 08 00 95 74 D2       h wconv      $0x8009574,r3
0800B629: 18 42                         r:=          b.0x8
0800B62B: 22 85                         w3 =:        r.0x14
0800B62D: FD 50 C4 08 00 95 76 D3       h wconv      $0x8009576,r4
0800B635: 23 86                         w4 =:        r.0x18
0800B637: FE 79 C4 08 00 A6 D4 87 03    w bmove      $0x800A6D4,r.0x1C,$0x3
0800B640: C3 08 00 C3 81 00             call         $0x800C381,$0x0
0800B646: D2 08                         if -k go     $0x8
0800B648: C3 08 00 B5 2A 00             call         $0x800B52A,$0x0
0800B64E: 49 C4 08 00 95 74             h stz        $0x8009574
0800B654: 18 45                         r:=          b.0x14
0800B656: 2E 84 CE 08 00                w comp2      r.0x10,$0x800
0800B65B: C6 0C                         if >< go     $0xC
0800B65D: 0D 83                         w2 :=        r.0xC
0800B65F: 55 01                         w2 +         $0x1
0800B661: 21 83                         w2 =:        r.0xC
0800B663: 4A 84                         w stz        r.0x10
0800B665: C0 38                         go           $0x38
0800B667: FD 50 C4 08 00 95 74 D2       h wconv      $0x8009574,r3
0800B66F: 2E 82 D2                      w comp2      r.0x8,r3
0800B672: C4 2B                         if = go      $0x2B
0800B674: 44 85                         w test       r.0x14
0800B676: CE 27                         if <= go     $0x27
0800B678: 0F 82                         w4 :=        r.0x8
0800B67A: 18 42                         r:=          b.0x8
0800B67C: 23 85                         w4 =:        r.0x14
0800B67E: 18 45                         r:=          b.0x14
0800B680: 0C 83                         w1 :=        r.0xC
0800B682: 18 42                         r:=          b.0x8
0800B684: 20 86                         w1 =:        r.0x18
0800B686: FE 79 C4 08 00 A6 E0 87 03    w bmove      $0x800A6E0,r.0x1C,$0x3
0800B68F: C3 08 00 C3 2D 00             call         $0x800C32D,$0x0
0800B695: D2 08                         if -k go     $0x8
0800B697: C3 08 00 B5 2A 00             call         $0x800B52A,$0x0
0800B69D: 18 45                         r:=          b.0x14
0800B69F: 0D 82                         w2 :=        r.0x8
0800B6A1: FC 11 C4 08 00 95 74          h2 =:        $0x8009574
0800B6A8: 0E 83                         w3 :=        r.0xC
0800B6AA: FC 12 C4 08 00 95 76          h3 =:        $0x8009576
0800B6B1: 07 46                         by4 :=       b.0x18
0800B6B3: 04 E3 08 00 A5 E4             by1 :=       $0x800A5E4+
0800B6B9: 0D 84                         w2 :=        r.0x10
0800B6BB: 1C E1 08 00 9D E4             by1 =:       $0x8009DE4+
0800B6C1: 0E 84                         w3 :=        r.0x10
0800B6C3: 56 01                         w3 +         $0x1
0800B6C5: 22 84                         w3 =:        r.0x10
0800B6C7: 0F 85                         w4 :=        r.0x14
0800B6C9: 57 01                         w4 +         $0x1
0800B6CB: 23 85                         w4 =:        r.0x14
0800B6CD: 80                            ret
0800B6CE: B8 CF 00 00 00 40             ents         $0x40
0800B6D4: 44 46                         w test       b.0x18
0800B6D6: C4 13                         if = go      $0x13
0800B6D8: 18 42                         r:=          b.0x8
0800B6DA: 1A 45 85                      w move       b.0x14,r.0x14
0800B6DD: FD 20 46 86 0C                by bmove     b.0x18,r.0x18,$0xC
0800B6E2: C3 08 00 AE F5 00             call         $0x800AEF5,$0x0
0800B6E8: 9D                            ifkret
0800B6E9: 44 49                         w test       b.0x24
0800B6EB: C4 28                         if = go      $0x28
0800B6ED: 44 C5 14                      w test       @b.0x14
0800B6F0: C4 23                         if = go      $0x23
0800B6F2: 18 45                         r:=          b.0x14
0800B6F4: FD 3D 87                      w2 laddr     r.0x1C
0800B6F7: 21 4D                         w2 =:        b.0x34
0800B6F9: 1A CD 20 4C                   w move       $0x20,b.0x30
0800B6FD: FC 6E 4B 4A 4E                w sub3       b.0x2C,b.0x28,b.0x38
0800B702: 0E 4A                         w3 :=        b.0x28
0800B704: AA 02 49                      w3 mulad     $0x2,b.0x24
0800B707: 22 4F                         w3 =:        b.0x3C
0800B709: 4F 4E                         w incr       b.0x38
0800B70B: CA 08                         if < go      $0x8
0800B70D: 84                            bi1 clr
0800B70E: 85                            bi2 clr
0800B70F: FD 68 4C 4E                   h smove      b.0x30,b.0x38
0800B713: 80                            ret
0800B714: B8 CF 00 00 00 78             ents         $0x78
0800B71A: 44 C4 08 00 A6 EC             w test       $0x800A6EC
0800B720: C6 67                         if >< go     $0x67
0800B722: 1A CD 63 5C                   w move       $0x63,b.0x70
0800B726: C3 08 00 B9 7C 05 5C 46 47 48 49    call         $0x800B97C,$0x5,b.0x70,b.0x18,b.0x1C,b.0x20,b.0x24
0800B731: 9D                            ifkret
0800B732: 1A CE 00 B3 5C                w move       $0xB3,b.0x70
0800B737: 4D 5D                         w set1       b.0x74
0800B739: C3 08 00 B9 7C 05 5C 48 5D 4A 4B    call         $0x800B97C,$0x5,b.0x70,b.0x20,b.0x74,b.0x28,b.0x2C
0800B744: 9D                            ifkret
0800B745: 2E 4A 01                      w comp2      b.0x28,$0x1
0800B748: C4 07                         if = go      $0x7
0800B74A: 2E 4A 02                      w comp2      b.0x28,$0x2
0800B74D: C6 0B                         if >< go     $0xB
0800B74F: 1A 48 C4 08 00 A6 EC          w move       b.0x20,$0x800A6EC
0800B756: C0 31                         go           $0x31
0800B758: 2E 46 01                      w comp2      b.0x18,$0x1
0800B75B: C4 07                         if = go      $0x7
0800B75D: 2E 46 02                      w comp2      b.0x18,$0x2
0800B760: C6 27                         if >< go     $0x27
0800B762: 1A CD 21 5C                   w move       $0x21,b.0x70
0800B766: 86                            bi3 clr
0800B767: FE 29 D6 30                   h2 laddr     b.0x30+
0800B76B: 21 5D                         w2 =:        b.0x74
0800B76D: C3 08 00 B9 7C 03 5C 48 C5 74 call         $0x800B97C,$0x3,b.0x70,b.0x20,@b.0x74
0800B777: 9D                            ifkret
0800B778: 86                            bi3 clr
0800B779: 09 D6 30                      h2 :=        b.0x30+
0800B77C: FC 95 CE 07 FF                h2 and       $0x7FF
0800B781: 21 C4 08 00 A6 EC             w2 =:        $0x800A6EC
0800B787: 0C C4 08 00 A6 EC             w1 :=        $0x800A6EC
0800B78D: 80                            ret
0800B78E: B8 CF 00 00 00 54             ents         $0x54
0800B794: 18 42                         r:=          b.0x8
0800B796: 4D 85                         w set1       r.0x14
0800B798: 4A 86                         w stz        r.0x18
0800B79A: FD 3C 49                      w1 laddr     b.0x24
0800B79D: 20 87                         w1 =:        r.0x1C
0800B79F: 4A 88                         w stz        r.0x20
0800B7A1: 1A 17 89                      w move       $0x17,r.0x24
0800B7A4: C3 08 00 B7 DF 00             call         $0x800B7DF,$0x0
0800B7AA: 9D                            ifkret
0800B7AB: 20 48                         w1 =:        b.0x20
0800B7AD: 44 D0                         w test       r1
0800B7AF: C4 03                         if = go      $0x3
0800B7B1: 80                            ret
0800B7B2: FD 55 45 D0                   w hconv      b.0x14,r1
0800B7B6: 0D 04                         w2 :=        $0x4
0800B7B8: FC 10 D5 24                   h1 =:        b.0x24+
0800B7BC: FD 55 46 D2                   w hconv      b.0x18,r3
0800B7C0: 0F 05                         w4 :=        $0x5
0800B7C2: FC 12 D7 24                   h3 =:        b.0x24+
0800B7C6: 18 42                         r:=          b.0x8
0800B7C8: 1A 02 85                      w move       $0x2,r.0x14
0800B7CB: 4A 86                         w stz        r.0x18
0800B7CD: FD 3C 49                      w1 laddr     b.0x24
0800B7D0: 20 87                         w1 =:        r.0x1C
0800B7D2: 4A 88                         w stz        r.0x20
0800B7D4: 1A 17 89                      w move       $0x17,r.0x24
0800B7D7: C3 08 00 B7 DF 00             call         $0x800B7DF,$0x0
0800B7DD: 9D                            ifkret
0800B7DE: 80                            ret
0800B7DF: B8 CF 00 00 00 44             ents         $0x44
0800B7E5: 18 42                         r:=          b.0x8
0800B7E7: 1A CE 00 D1 85                w move       $0xD1,r.0x14
0800B7EC: C3 08 00 B8 36 00             call         $0x800B836,$0x0
0800B7F2: 9D                            ifkret
0800B7F3: 44 D0                         w test       r1
0800B7F5: C6 1B                         if >< go     $0x1B
0800B7F7: FD 55 00 D0                   w hconv      $0x0,r1
0800B7FB: FC 6E 49 48 4F                w sub3       b.0x24,b.0x20,b.0x3C
0800B800: 0D 48                         w2 :=        b.0x20
0800B802: A9 02 47                      w2 mulad     $0x2,b.0x1C
0800B805: 21 50                         w2 =:        b.0x40
0800B807: 4F 4F                         w incr       b.0x3C
0800B809: 85                            bi2 clr
0800B80A: FD 84 4F                      h1 sfill     b.0x3C
0800B80D: 0C 00                         w1 :=        $0x0
0800B80F: 80                            ret
0800B810: 4A 4C                         w stz        b.0x30
0800B812: 0D 48                         w2 :=        b.0x20
0800B814: FE 2A E5 1C                   h3 laddr     @b.0x1C+
0800B818: 22 4E                         w3 =:        b.0x38
0800B81A: 0F 49                         w4 :=        b.0x24
0800B81C: 63 48                         w4 -         b.0x20
0800B81E: 57 01                         w4 +         $0x1
0800B820: 6F 02                         w4 *         $0x2
0800B822: 23 4D                         w4 =:        b.0x34
0800B824: C3 F8 00 00 D1 03 45 46 4D    call         $0xFFFFFFFFF80000D1,$0x3,b.0x14,b.0x18,b.0x34 ; MON 321B UEADM
0800B82D: D2 06                         if -k go     $0x6
0800B82F: 20 4C                         w1 =:        b.0x30
0800B831: FE 03                         clrk
0800B833: 0C 4C                         w1 :=        b.0x30
0800B835: 80                            ret
0800B836: B8 CF 00 00 00 50             ents         $0x50
0800B83C: C3 08 00 B8 60 00             call         $0x800B860,$0x0
0800B842: 9D                            ifkret
0800B843: 30 CD 49                      by1 comp     $0x49
0800B846: D6 04                         if >>= go    $0x4
0800B848: C0 15                         go           $0x15
0800B84A: C3 F8 00 00 CA 02 45 47       call         $0xFFFFFFFFF80000CA,$0x2,b.0x14,b.0x1C ; MON 312B MOINF
0800B852: D0 0B                         if k go      $0xB
0800B854: 44 47                         w test       b.0x1C
0800B856: C6 04                         if >< go     $0x4
0800B858: C0 05                         go           $0x5
0800B85A: 0C 01                         w1 :=        $0x1
0800B85C: 80                            ret
0800B85D: 0C 00                         w1 :=        $0x0
0800B85F: 80                            ret
0800B860: B8 CF 00 00 00 4C             ents         $0x4C
0800B866: 4A 46                         w stz        b.0x18
0800B868: C3 F8 00 00 B2 02 46 47       call         $0xFFFFFFFFF80000B2,$0x2,b.0x18,b.0x1C ; MON 262B CPUST
0800B870: D0 0D                         if k go      $0xD
0800B872: 0D 04                         w2 :=        $0x4
0800B874: 08 D5 1C                      h1 :=        b.0x1C+
0800B877: FC 94 CE 00 FF                h1 and       $0xFF
0800B87C: 80                            ret
0800B87D: 04 CD 48                      by1 :=       $0x48
0800B880: 80                            ret
0800B881: 9C                            entd
0800B882: FD C0 74                      l=:          b.0xD0
0800B885: FE 24 6E                      by1 laddr    b.0xB8
0800B888: FD 20 C5 A8 F4 00 0C          by bmove     @b.0xFFFFFFFFFFFFFFA8,r1.(0x0),$0xC
0800B88F: 0D 0C                         w2 :=        $0xC
0800B891: FE 25 E5 A8                   by2 laddr    @b.0xFFFFFFFFFFFFFFA8+
0800B895: 21 6A                         w2 =:        b.0xA8
0800B897: 18 6B                         r:=          b.0xAC
0800B899: FD 3E 81                      w3 laddr     r.0x4
0800B89C: 22 80                         w3 =:        r.0x0
0800B89E: 0F 70                         w4 :=        b.0xC0
0800B8A0: 0C 6F                         w1 :=        b.0xBC
0800B8A2: 63 D0                         w4 -         r1
0800B8A4: 57 02                         w4 +         $0x2
0800B8A6: 1A 00 76                      w move       $0x0,b.0xD8
0800B8A9: 1A D3 77                      w move       r4,b.0xDC
0800B8AC: 0C 77                         w1 :=        b.0xDC
0800B8AE: 60 76                         w1 -         b.0xD8
0800B8B0: 54 01                         w1 +         $0x1
0800B8B2: C3 08 00 C0 82 00             call         $0x800C082,$0x0
0800B8B8: D2 04                         if -k go     $0x4
0800B8BA: B4 74                         jumpg        b.0xD0
0800B8BC: 60 76                         w1 -         b.0xD8
0800B8BE: 20 75                         w1 =:        b.0xD4
0800B8C0: FE 79 75 71 03                w bmove      b.0xD4,b.0xC4,$0x3
0800B8C5: 4A 6D                         w stz        b.0xB4
0800B8C7: 0C 6F                         w1 :=        b.0xBC
0800B8C9: 20 6C                         w1 =:        b.0xB0
0800B8CB: 0D 70                         w2 :=        b.0xC0
0800B8CD: 21 78                         w2 =:        b.0xE0
0800B8CF: 34 D1                         w1 comp      r2
0800B8D1: C8 12                         if > go      $0x12
0800B8D3: 0D 6C                         w2 :=        b.0xB0
0800B8D5: 04 E5 B8                      by1 :=       @b.0xFFFFFFFFFFFFFFB8+
0800B8D8: 0E 6D                         w3 :=        b.0xB4
0800B8DA: 1C E6 C4                      by1 =:       @b.0xFFFFFFFFFFFFFFC4+
0800B8DD: 4F 6D                         w incr       b.0xB4
0800B8DF: BF 6C 78 F4                   d loopi      b.0xB0,b.0xE0,$0xFFFFFFFFFFFFFFF4
0800B8E3: 0C 6D                         w1 :=        b.0xB4
0800B8E5: 60 01                         w1 -         $0x1
0800B8E7: 20 6C                         w1 =:        b.0xB0
0800B8E9: 44 D0                         w test       r1
0800B8EB: CA 12                         if < go      $0x12
0800B8ED: 04 CD 20                      by1 :=       $0x20
0800B8F0: 0D 6C                         w2 :=        b.0xB0
0800B8F2: 2D E5 C4 D0                   by comp2     @b.0xFFFFFFFFFFFFFFC4+,r1
0800B8F6: C6 07                         if >< go     $0x7
0800B8F8: FD 25 6C 00 F5                d loopd      b.0xB0,$0x0,$0xFFFFFFFFFFFFFFF5
0800B8FD: 0C 6C                         w1 :=        b.0xB0
0800B8FF: 54 01                         w1 +         $0x1
0800B901: 20 6D                         w1 =:        b.0xB4
0800B903: 05 CD 27                      by2 :=       $0x27
0800B906: 1D E4 C4                      by2 =:       @b.0xFFFFFFFFFFFFFFC4+
0800B909: 54 01                         w1 +         $0x1
0800B90B: 18 6B                         r:=          b.0xAC
0800B90D: 20 81                         w1 =:        r.0x4
0800B90F: 86                            bi3 clr
0800B910: FE 26 E6 C4                   by3 laddr    @b.0xFFFFFFFFFFFFFFC4+
0800B914: 22 82                         w3 =:        r.0x8
0800B916: 0F 0C                         w4 :=        $0xC
0800B918: FE 27 E7 AC                   by4 laddr    @b.0xFFFFFFFFFFFFFFAC+
0800B91C: 23 6B                         w4 =:        b.0xAC
0800B91E: FE 03                         clrk
0800B920: B4 74                         jumpg        b.0xD0
0800B922: 9C                            entd
0800B923: FD C0 79                      l=:          b.0xE4
0800B926: FE 24 6E                      by1 laddr    b.0xB8
0800B929: FD 20 C5 A8 F4 00 0C          by bmove     @b.0xFFFFFFFFFFFFFFA8,r1.(0x0),$0xC
0800B930: 0D 0C                         w2 :=        $0xC
0800B932: FE 25 E5 A8                   by2 laddr    @b.0xFFFFFFFFFFFFFFA8+
0800B936: 21 6A                         w2 =:        b.0xA8
0800B938: 18 6B                         r:=          b.0xAC
0800B93A: FD 3E 81                      w3 laddr     r.0x4
0800B93D: 22 80                         w3 =:        r.0x0
0800B93F: 0F 6F                         w4 :=        b.0xBC
0800B941: 23 6D                         w4 =:        b.0xB4
0800B943: 0C 70                         w1 :=        b.0xC0
0800B945: 60 D3                         w1 -         r4
0800B947: 54 01                         w1 +         $0x1
0800B949: 20 81                         w1 =:        r.0x4
0800B94B: FE 25 E7 B8                   by2 laddr    @b.0xFFFFFFFFFFFFFFB8+
0800B94F: 21 82                         w2 =:        r.0x8
0800B951: 0E 0C                         w3 :=        $0xC
0800B953: FE 26 E6 AC                   by3 laddr    @b.0xFFFFFFFFFFFFFFAC+
0800B957: 22 6B                         w3 =:        b.0xAC
0800B959: FE 03                         clrk
0800B95B: B4 79                         jumpg        b.0xE4
0800B95D: 9C                            entd
0800B95E: FD C0 7A                      l=:          b.0xE8
0800B961: 0C C5 A8                      w1 :=        @b.0xFFFFFFFFFFFFFFA8
0800B964: 18 6B                         r:=          b.0xAC
0800B966: 20 80                         w1 =:        r.0x0
0800B968: 0D 04                         w2 :=        $0x4
0800B96A: FE 25 E5 A8                   by2 laddr    @b.0xFFFFFFFFFFFFFFA8+
0800B96E: 21 6A                         w2 =:        b.0xA8
0800B970: 0E 0C                         w3 :=        $0xC
0800B972: FE 26 E6 AC                   by3 laddr    @b.0xFFFFFFFFFFFFFFAC+
0800B976: 22 6B                         w3 =:        b.0xAC
0800B978: FE 03                         clrk
0800B97A: B4 7A                         jumpg        b.0xE8
0800B97C: B8 CF 00 00 01 0C             ents         $0x10C
0800B982: FE 24 46                      by1 laddr    b.0x18
0800B985: 20 6A                         w1 =:        b.0xA8
0800B987: FE 25 58                      by2 laddr    b.0x60
0800B98A: 21 6B                         w2 =:        b.0xAC
0800B98C: 0F C5 14                      w4 :=        @b.0x14
0800B98F: 06 E3 08 00 A6 F0             by3 :=       $0x800A6F0+
0800B995: 22 7B                         w3 =:        b.0xEC
0800B997: B4 E2 08 00 A8 40             jumpg        $0x800A840+
0800B99D: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9A3: 9D                            ifkret
0800B9A4: C1 03 90                      go           $0x390
0800B9A7: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9AD: 9D                            ifkret
0800B9AE: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9B4: 9D                            ifkret
0800B9B5: C1 03 7F                      go           $0x37F
0800B9B8: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9BE: 9D                            ifkret
0800B9BF: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9C5: 9D                            ifkret
0800B9C6: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9CC: 9D                            ifkret
0800B9CD: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9D3: 9D                            ifkret
0800B9D4: C1 03 60                      go           $0x360
0800B9D7: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800B9DD: 9D                            ifkret
0800B9DE: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9E4: 9D                            ifkret
0800B9E5: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800B9EB: 9D                            ifkret
0800B9EC: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800B9F2: 9D                            ifkret
0800B9F3: C1 03 41                      go           $0x341
0800B9F6: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800B9FC: 9D                            ifkret
0800B9FD: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA03: 9D                            ifkret
0800BA04: C1 03 30                      go           $0x330
0800BA07: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA0D: 9D                            ifkret
0800BA0E: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA14: 9D                            ifkret
0800BA15: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA1B: 9D                            ifkret
0800BA1C: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA22: 9D                            ifkret
0800BA23: C1 03 11                      go           $0x311
0800BA26: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA2C: 9D                            ifkret
0800BA2D: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA33: 9D                            ifkret
0800BA34: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA3A: 9D                            ifkret
0800BA3B: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA41: 9D                            ifkret
0800BA42: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA48: 9D                            ifkret
0800BA49: C1 02 EB                      go           $0x2EB
0800BA4C: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA52: 9D                            ifkret
0800BA53: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA59: 9D                            ifkret
0800BA5A: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA60: 9D                            ifkret
0800BA61: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA67: 9D                            ifkret
0800BA68: C1 02 CC                      go           $0x2CC
0800BA6B: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BA71: 9D                            ifkret
0800BA72: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA78: 9D                            ifkret
0800BA79: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA7F: 9D                            ifkret
0800BA80: C1 02 B4                      go           $0x2B4
0800BA83: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA89: 9D                            ifkret
0800BA8A: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BA90: 9D                            ifkret
0800BA91: C1 02 A3                      go           $0x2A3
0800BA94: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BA9A: 9D                            ifkret
0800BA9B: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAA1: 9D                            ifkret
0800BAA2: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BAA8: 9D                            ifkret
0800BAA9: C1 02 8B                      go           $0x28B
0800BAAC: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAB2: 9D                            ifkret
0800BAB3: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAB9: 9D                            ifkret
0800BABA: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BAC0: 9D                            ifkret
0800BAC1: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAC7: 9D                            ifkret
0800BAC8: C1 02 6C                      go           $0x26C
0800BACB: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BAD1: 9D                            ifkret
0800BAD2: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BAD8: 9D                            ifkret
0800BAD9: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BADF: 9D                            ifkret
0800BAE0: C1 02 54                      go           $0x254
0800BAE3: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAE9: 9D                            ifkret
0800BAEA: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BAF0: 9D                            ifkret
0800BAF1: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BAF7: 9D                            ifkret
0800BAF8: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BAFE: 9D                            ifkret
0800BAFF: C1 02 35                      go           $0x235
0800BB02: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB08: 9D                            ifkret
0800BB09: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BB0F: 9D                            ifkret
0800BB10: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB16: 9D                            ifkret
0800BB17: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB1D: 9D                            ifkret
0800BB1E: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB24: 9D                            ifkret
0800BB25: C1 02 0F                      go           $0x20F
0800BB28: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB2E: 9D                            ifkret
0800BB2F: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BB35: 9D                            ifkret
0800BB36: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB3C: 9D                            ifkret
0800BB3D: C1 01 F7                      go           $0x1F7
0800BB40: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BB46: 9D                            ifkret
0800BB47: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB4D: 9D                            ifkret
0800BB4E: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB54: 9D                            ifkret
0800BB55: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB5B: 9D                            ifkret
0800BB5C: 18 61                         r:=          b.0x84
0800BB5E: 0D 80                         w2 :=        r.0x0
0800BB60: 44 D1                         w test       r2
0800BB62: C4 09                         if = go      $0x9
0800BB64: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BB6A: 9D                            ifkret
0800BB6B: C1 01 C9                      go           $0x1C9
0800BB6E: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BB74: 9D                            ifkret
0800BB75: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB7B: 9D                            ifkret
0800BB7C: C1 01 B8                      go           $0x1B8
0800BB7F: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BB85: 9D                            ifkret
0800BB86: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BB8C: 9D                            ifkret
0800BB8D: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB93: 9D                            ifkret
0800BB94: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BB9A: 9D                            ifkret
0800BB9B: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBA1: 9D                            ifkret
0800BBA2: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBA8: 9D                            ifkret
0800BBA9: C1 01 8B                      go           $0x18B
0800BBAC: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBB2: 9D                            ifkret
0800BBB3: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BBB9: 9D                            ifkret
0800BBBA: 0D 5D                         w2 :=        b.0x74
0800BBBC: 21 5B                         w2 =:        b.0x6C
0800BBBE: C1 01 76                      go           $0x176
0800BBC1: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBC7: 9D                            ifkret
0800BBC8: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BBCE: 9D                            ifkret
0800BBCF: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBD5: 9D                            ifkret
0800BBD6: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BBDC: 9D                            ifkret
0800BBDD: 18 5E                         r:=          b.0x78
0800BBDF: 0D 80                         w2 :=        r.0x0
0800BBE1: 21 6C                         w2 =:        b.0xB0
0800BBE3: 44 D1                         w test       r2
0800BBE5: CA 09                         if < go      $0x9
0800BBE7: 2E 5C D1                      w comp2      b.0x70,r2
0800BBEA: CE 04                         if <= go     $0x4
0800BBEC: 21 5C                         w2 =:        b.0x70
0800BBEE: C1 01 46                      go           $0x146
0800BBF1: 1A CF F8 00 00 A9 6C          w move       $0xF80000A9,b.0xB0
0800BBF8: B5 6C 07 C5 18 C5 1C C5 20 C5 24 C5 28 C5 2C C5 30 callg        b.0xB0,$0x7,@b.0x18,@b.0x1C,@b.0x20,@b.0x24,@b.0x28,@b.0x2C,@b.0x30
0800BC09: D3 01 5D                      if -k go     $0x15D
0800BC0C: 20 43                         w1 =:        b.0xC
0800BC0E: C1 01 58                      go           $0x158
0800BC11: C1 01 23                      go           $0x123
0800BC14: 1A CF F8 00 01 43 6C          w move       $0xF8000143,b.0xB0
0800BC1B: B5 6C 0E C5 18 C5 1C C5 20 C5 24 C5 28 C5 2C C5 30 C5 34 C5 38 C5 3C C5 40 C5 44 C5 48 C5 4C callg        b.0xB0,$0xE,@b.0x18,@b.0x1C,@b.0x20,@b.0x24,@b.0x28,@b.0x2C,@b.0x30,@b.0x34,@b.0x38,@b.0x3C,@b.0x40,@b.0x44,@b.0x48,@b.0x4C
0800BC3A: D3 01 2C                      if -k go     $0x12C
0800BC3D: 20 43                         w1 =:        b.0xC
0800BC3F: C1 01 27                      go           $0x127
0800BC42: C1 00 F2                      go           $0xF2
0800BC45: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC4B: 9D                            ifkret
0800BC4C: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC52: 9D                            ifkret
0800BC53: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC59: 9D                            ifkret
0800BC5A: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC60: 9D                            ifkret
0800BC61: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC67: 9D                            ifkret
0800BC68: 18 64                         r:=          b.0x90
0800BC6A: 0D 80                         w2 :=        r.0x0
0800BC6C: 44 D1                         w test       r2
0800BC6E: C4 09                         if = go      $0x9
0800BC70: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BC76: 9D                            ifkret
0800BC77: C1 00 BD                      go           $0xBD
0800BC7A: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC80: 9D                            ifkret
0800BC81: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC87: 9D                            ifkret
0800BC88: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC8E: 9D                            ifkret
0800BC8F: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BC95: 9D                            ifkret
0800BC96: 18 61                         r:=          b.0x84
0800BC98: 0D 80                         w2 :=        r.0x0
0800BC9A: 44 D1                         w test       r2
0800BC9C: C4 09                         if = go      $0x9
0800BC9E: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BCA4: 9D                            ifkret
0800BCA5: C1 00 8F                      go           $0x8F
0800BCA8: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCAE: 9D                            ifkret
0800BCAF: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCB5: 9D                            ifkret
0800BCB6: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCBC: 9D                            ifkret
0800BCBD: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BCC3: 9D                            ifkret
0800BCC4: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCCA: 9D                            ifkret
0800BCCB: 18 64                         r:=          b.0x90
0800BCCD: 0D 80                         w2 :=        r.0x0
0800BCCF: 44 D1                         w test       r2
0800BCD1: C4 09                         if = go      $0x9
0800BCD3: C3 08 00 B8 81 00             call         $0x800B881,$0x0
0800BCD9: 9D                            ifkret
0800BCDA: C0 5A                         go           $0x5A
0800BCDC: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCE2: 9D                            ifkret
0800BCE3: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCE9: 9D                            ifkret
0800BCEA: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCF0: 9D                            ifkret
0800BCF1: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCF7: 9D                            ifkret
0800BCF8: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BCFE: 9D                            ifkret
0800BCFF: C3 08 00 B9 22 00             call         $0x800B922,$0x0
0800BD05: 9D                            ifkret
0800BD06: C0 2E                         go           $0x2E
0800BD08: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD0E: 9D                            ifkret
0800BD0F: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD15: 9D                            ifkret
0800BD16: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD1C: 9D                            ifkret
0800BD1D: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD23: 9D                            ifkret
0800BD24: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD2A: 9D                            ifkret
0800BD2B: C3 08 00 B9 5D 00             call         $0x800B95D,$0x0
0800BD31: 9D                            ifkret
0800BD32: C0 02                         go           $0x2
0800BD34: 1A 45 7C                      w move       b.0x14,b.0xF0
0800BD37: 1A 58 7D                      w move       b.0x60,b.0xF4
0800BD3A: 1A 5B 7E                      w move       b.0x6C,b.0xF8
0800BD3D: 1A 5E 7F                      w move       b.0x78,b.0xFC
0800BD40: 1A 61 C2 01 00                w move       b.0x84,b.0x100
0800BD45: 1A 64 C2 01 04                w move       b.0x90,b.0x104
0800BD4A: 1A 67 C2 01 08                w move       b.0x9C,b.0x108
0800BD4F: C3 08 00 BE 20 07 C5 F0 C5 F4 C5 F8 C5 FC C6 01 00 C6 01 04 C6 01 08 call         $0x800BE20,$0x7,@b.0xFFFFFFFFFFFFFFF0,@b.0xFFFFFFFFFFFFFFF4,@b.0xFFFFFFFFFFFFFFF8,@b.0xFFFFFFFFFFFFFFFC,@b.0x100,@b.0x104,@b.0x108
0800BD66: 44 43                         w test       b.0xC
0800BD68: C4 05                         if = go      $0x5
0800BD6A: 0C 43                         w1 :=        b.0xC
0800BD6C: 81                            retk
0800BD6D: 80                            ret
0800BD6E: B8 CF 00 00 00 5C             ents         $0x5C
0800BD74: 1A 45 48                      w move       b.0x14,b.0x20
0800BD77: 08 C5 20                      h1 :=        @b.0x20
0800BD7A: FC 10 49                      h1 =:        b.0x24
0800BD7D: 53 48 02                      w add2       b.0x20,$0x2
0800BD80: 09 C5 20                      h2 :=        @b.0x20
0800BD83: FC 11 C1 26                   h2 =:        b.0x26
0800BD87: 53 48 02                      w add2       b.0x20,$0x2
0800BD8A: 0A C5 20                      h3 :=        @b.0x20
0800BD8D: FC 12 4A                      h3 =:        b.0x28
0800BD90: 4A 48                         w stz        b.0x20
0800BD92: 4A 46                         w stz        b.0x18
0800BD94: 08 49                         h1 :=        b.0x24
0800BD96: FC AC D0 3C                   h sha        r1,$0x3C
0800BD9A: FC 94 CD 3F                   h1 and       $0x3F
0800BD9E: FD 50 D0 D1                   h wconv      r1,r2
0800BDA2: 21 47                         w2 =:        b.0x1C
0800BDA4: 44 D1                         w test       r2
0800BDA6: C4 16                         if = go      $0x16
0800BDA8: E5 CD 20                      w2 and       $0x20
0800BDAB: 44 D1                         w test       r2
0800BDAD: C6 06                         if >< go     $0x6
0800BDAF: 53 47 CD 40                   w add2       b.0x1C,$0x40
0800BDB3: 0D 47                         w2 :=        b.0x1C
0800BDB5: 0E 48                         w3 :=        b.0x20
0800BDB7: 1D D6 2A                      by2 =:       b.0x2A+
0800BDBA: 4F 48                         w incr       b.0x20
0800BDBC: 09 49                         h2 :=        b.0x24
0800BDBE: FC AC D1 06                   h sha        r2,$0x6
0800BDC2: 0A C1 26                      h3 :=        b.0x26
0800BDC5: FC AC D2 36                   h sha        r3,$0x36
0800BDC9: FC 96 CD 3F                   h3 and       $0x3F
0800BDCD: FC 3A D1                      h3 +         r2
0800BDD0: FC 12 49                      h3 =:        b.0x24
0800BDD3: 0B C1 26                      h4 :=        b.0x26
0800BDD6: FC AC D3 06                   h sha        r4,$0x6
0800BDDA: 08 4A                         h1 :=        b.0x28
0800BDDC: FC AC D0 36                   h sha        r1,$0x36
0800BDE0: FC 94 CD 3F                   h1 and       $0x3F
0800BDE4: FC 38 D3                      h1 +         r4
0800BDE7: FC 10 C1 26                   h1 =:        b.0x26
0800BDEB: 09 4A                         h2 :=        b.0x28
0800BDED: FC AC D1 06                   h sha        r2,$0x6
0800BDF1: FC 11 4A                      h2 =:        b.0x28
0800BDF4: BF 46 06 A0                   d loopi      b.0x18,$0x6,$0xFFFFFFFFFFFFFFA0
0800BDF8: 0C 48                         w1 :=        b.0x20
0800BDFA: 19 CD 27 D4 2A                by move      $0x27,b.0x2A+
0800BDFF: 85                            bi2 clr
0800BE00: FE 26 D5 2A                   by3 laddr    b.0x2A+
0800BE04: 22 4E                         w3 =:        b.0x38
0800BE06: 54 01                         w1 +         $0x1
0800BE08: 20 4D                         w1 =:        b.0x34
0800BE0A: 87                            bi4 clr
0800BE0B: FE 25 E7 14                   by2 laddr    @b.0x14+
0800BE0F: 21 51                         w2 =:        b.0x44
0800BE11: 0E 48                         w3 :=        b.0x20
0800BE13: 56 01                         w3 +         $0x1
0800BE15: 22 50                         w3 =:        b.0x40
0800BE17: CA 08                         if < go      $0x8
0800BE19: 84                            bi1 clr
0800BE1A: 85                            bi2 clr
0800BE1B: FD 67 4D 50                   by smove     b.0x34,b.0x40
0800BE1F: 80                            ret
0800BE20: B8 CF 00 00 00 54             ents         $0x54
0800BE26: 0C C5 14                      w1 :=        @b.0x14
0800BE29: 34 CE 01 4C                   w1 comp      $0x14C
0800BE2D: CE 08                         if <= go     $0x8
0800BE2F: 1A 3E 51                      w move       $0x3E,b.0x44
0800BE32: C1 02 46                      go           $0x246
0800BE35: 0C C5 14                      w1 :=        @b.0x14
0800BE38: 54 CF F8 00 00 00             w1 +         $0xF8000000
0800BE3E: 20 50                         w1 =:        b.0x40
0800BE40: 0D C5 14                      w2 :=        @b.0x14
0800BE43: 6D 01                         w2 *         $0x1
0800BE45: FE 26 E1 08 00 A8 B0          by3 laddr    $0x800A8B0+
0800BE4C: 22 4D                         w3 =:        b.0x34
0800BE4E: 4A 51                         w stz        b.0x44
0800BE50: 4A 4E                         w stz        b.0x38
0800BE52: FD EB F6 00 1C 04             w4 getbf     r3.(0x0),$0x1C,$0x4
0800BE58: 23 52                         w4 =:        b.0x48
0800BE5A: B4 E3 08 00 AA 00             jumpg        $0x800AA00+
0800BE60: B5 50 00                      callg        b.0x40,$0x0
0800BE63: 20 51                         w1 =:        b.0x44
0800BE65: D1 02 13                      if k go      $0x213
0800BE68: C1 01 EF                      go           $0x1EF
0800BE6B: 4D 4E                         w set1       b.0x38
0800BE6D: B5 50 01 C5 18                callg        b.0x40,$0x1,@b.0x18
0800BE72: 20 51                         w1 =:        b.0x44
0800BE74: D1 02 04                      if k go      $0x204
0800BE77: C1 01 E0                      go           $0x1E0
0800BE7A: 1A 02 4E                      w move       $0x2,b.0x38
0800BE7D: B5 50 02 C5 18 C5 1C          callg        b.0x40,$0x2,@b.0x18,@b.0x1C
0800BE84: 20 51                         w1 =:        b.0x44
0800BE86: D1 01 F2                      if k go      $0x1F2
0800BE89: C1 01 CE                      go           $0x1CE
0800BE8C: 1A 03 4E                      w move       $0x3,b.0x38
0800BE8F: B5 50 03 C5 18 C5 1C C5 20    callg        b.0x40,$0x3,@b.0x18,@b.0x1C,@b.0x20
0800BE98: 20 51                         w1 =:        b.0x44
0800BE9A: D1 01 DE                      if k go      $0x1DE
0800BE9D: C1 01 BA                      go           $0x1BA
0800BEA0: 1A 04 4E                      w move       $0x4,b.0x38
0800BEA3: B5 50 04 C5 18 C5 1C C5 20 C5 24    callg        b.0x40,$0x4,@b.0x18,@b.0x1C,@b.0x20,@b.0x24
0800BEAE: 20 51                         w1 =:        b.0x44
0800BEB0: D1 01 C8                      if k go      $0x1C8
0800BEB3: C1 01 A4                      go           $0x1A4
0800BEB6: 1A 05 4E                      w move       $0x5,b.0x38
0800BEB9: B5 50 05 C5 18 C5 1C C5 20 C5 24 C5 28          callg        b.0x40,$0x5,@b.0x18,@b.0x1C,@b.0x20,@b.0x24,@b.0x28
0800BEC6: 20 51                         w1 =:        b.0x44
0800BEC8: D1 01 B0                      if k go      $0x1B0
0800BECB: C1 01 8C                      go           $0x18C
0800BECE: 1A 06 4E                      w move       $0x6,b.0x38
0800BED1: B5 50 06 C5 18 C5 1C C5 20 C5 24 C5 28 C5 2C callg        b.0x40,$0x6,@b.0x18,@b.0x1C,@b.0x20,@b.0x24,@b.0x28,@b.0x2C
0800BEE0: 20 51                         w1 =:        b.0x44
0800BEE2: D1 01 96                      if k go      $0x196
0800BEE5: C1 01 72                      go           $0x172
0800BEE8: 1A 3E 51                      w move       $0x3E,b.0x44
0800BEEB: C1 01 8D                      go           $0x18D
0800BEEE: C1 01 69                      go           $0x169
0800BEF1: 4A 4E                         w stz        b.0x38
0800BEF3: B5 50 02 C5 18 C5 1C          callg        b.0x40,$0x2,@b.0x18,@b.0x1C
0800BEFA: 20 51                         w1 =:        b.0x44
0800BEFC: D1 01 7C                      if k go      $0x17C
0800BEFF: C1 01 58                      go           $0x158
0800BF02: 1A 03 4E                      w move       $0x3,b.0x38
0800BF05: B5 50 02 C5 18 C5 1C          callg        b.0x40,$0x2,@b.0x18,@b.0x1C
0800BF0C: 20 51                         w1 =:        b.0x44
0800BF0E: D1 01 6A                      if k go      $0x16A
0800BF11: C1 01 46                      go           $0x146
0800BF14: 4D 4E                         w set1       b.0x38
0800BF16: B5 50 02 C5 18 C5 20          callg        b.0x40,$0x2,@b.0x18,@b.0x20
0800BF1D: 20 51                         w1 =:        b.0x44
0800BF1F: D1 01 59                      if k go      $0x159
0800BF22: C1 01 35                      go           $0x135
0800BF25: 1A 05 4E                      w move       $0x5,b.0x38
0800BF28: B5 50 05 C5 1C C5 20 C5 18 C5 24 C5 28          callg        b.0x40,$0x5,@b.0x1C,@b.0x20,@b.0x18,@b.0x24,@b.0x28
0800BF35: 20 51                         w1 =:        b.0x44
0800BF37: D1 01 41                      if k go      $0x141
0800BF3A: C1 01 1D                      go           $0x11D
0800BF3D: 0C C5 14                      w1 :=        @b.0x14
0800BF40: 34 CE 00 D7                   w1 comp      $0xD7
0800BF44: C6 41                         if >< go     $0x41
0800BF46: 1A 03 4E                      w move       $0x3,b.0x38
0800BF49: 0C C5 18                      w1 :=        @b.0x18
0800BF4C: 34 02                         w1 comp      $0x2
0800BF4E: C6 12                         if >< go     $0x12
0800BF50: B5 50 03 C5 18 C5 1C C5 24    callg        b.0x40,$0x3,@b.0x18,@b.0x1C,@b.0x24
0800BF59: 20 51                         w1 =:        b.0x44
0800BF5B: D1 01 1D                      if k go      $0x11D
0800BF5E: C0 25                         go           $0x25
0800BF60: 0C C5 18                      w1 :=        @b.0x18
0800BF63: 34 03                         w1 comp      $0x3
0800BF65: C6 12                         if >< go     $0x12
0800BF67: B5 50 03 C5 18 C5 1C C5 20    callg        b.0x40,$0x3,@b.0x18,@b.0x1C,@b.0x20
0800BF70: 20 51                         w1 =:        b.0x44
0800BF72: D1 01 06                      if k go      $0x106
0800BF75: C0 0E                         go           $0xE
0800BF77: B5 50 02 C5 18 C5 1C          callg        b.0x40,$0x2,@b.0x18,@b.0x1C
0800BF7E: 20 51                         w1 =:        b.0x44
0800BF80: D1 00 F8                      if k go      $0xF8
0800BF83: C0 2A                         go           $0x2A
0800BF85: 0C C5 14                      w1 :=        @b.0x14
0800BF88: 34 CD 6A                      w1 comp      $0x6A
0800BF8B: C6 22                         if >< go     $0x22
0800BF8D: 1A 02 4E                      w move       $0x2,b.0x38
0800BF90: 18 47                         r:=          b.0x1C
0800BF92: 1A 81 47                      w move       r.0x4,b.0x1C
0800BF95: B5 50 02 C5 18 C5 1C          callg        b.0x40,$0x2,@b.0x18,@b.0x1C
0800BF9C: 20 51                         w1 =:        b.0x44
0800BF9E: D1 00 DA                      if k go      $0xDA
0800BFA1: 18 42                         r:=          b.0x8
0800BFA3: 1A 47 85                      w move       b.0x1C,r.0x14
0800BFA6: C3 08 00 BD 6E 00             call         $0x800BD6E,$0x0
0800BFAC: 9D                            ifkret
0800BFAD: C1 00 AA                      go           $0xAA
0800BFB0: 0C C5 24                      w1 :=        @b.0x24
0800BFB3: 44 D0                         w test       r1
0800BFB5: C4 1F                         if = go      $0x1F
0800BFB7: 0C C5 1C                      w1 :=        @b.0x1C
0800BFBA: E4 CD 7F                      w1 and       $0x7F
0800BFBD: 54 CE 00 80                   w1 +         $0x80
0800BFC1: 20 4F                         w1 =:        b.0x3C
0800BFC3: B5 50 04 C5 18 4F C5 20 C5 28 callg        b.0x40,$0x4,@b.0x18,b.0x3C,@b.0x20,@b.0x28
0800BFCD: 20 51                         w1 =:        b.0x44
0800BFCF: D1 00 A9                      if k go      $0xA9
0800BFD2: C0 10                         go           $0x10
0800BFD4: B5 50 03 C5 18 C5 1C C5 20    callg        b.0x40,$0x3,@b.0x18,@b.0x1C,@b.0x20
0800BFDD: 20 51                         w1 =:        b.0x44
0800BFDF: D1 00 99                      if k go      $0x99
0800BFE2: C0 75                         go           $0x75
0800BFE4: 0C C5 14                      w1 :=        @b.0x14
0800BFE7: 34 CE 00 8F                   w1 comp      $0x8F
0800BFEB: C6 36                         if >< go     $0x36
0800BFED: B5 50 05 C5 18 C5 1C C5 20 C5 24 C5 2C          callg        b.0x40,$0x5,@b.0x18,@b.0x1C,@b.0x20,@b.0x24,@b.0x2C
0800BFFA: 20 51                         w1 =:        b.0x44
0800BFFC: D1 00 7C                      if k go      $0x7C
0800BFFF: 4A C5 28                      w stz        @b.0x28
0800C002: 4A 54                         w stz        b.0x50
0800C004: FD D0 C5 1C 07                w1 getbi     @b.0x1C,$0x7
0800C009: C4 04                         if = go      $0x4
0800C00B: 4D 54                         w set1       b.0x50
0800C00D: 0D 54                         w2 :=        b.0x50
0800C00F: 21 53                         w2 =:        b.0x4C
0800C011: FD DD C5 28 00                w2 putbi     @b.0x28,$0x0
0800C016: 0E C5 1C                      w3 :=        @b.0x1C
0800C019: E6 CD 7F                      w3 and       $0x7F
0800C01C: 22 C5 1C                      w3 =:        @b.0x1C
0800C01F: C0 36                         go           $0x36
0800C021: 0F C5 28                      w4 :=        @b.0x28
0800C024: 44 D3                         w test       r4
0800C026: C4 20                         if = go      $0x20
0800C028: 0F C5 1C                      w4 :=        @b.0x1C
0800C02B: E7 CD 7F                      w4 and       $0x7F
0800C02E: 57 CE 00 80                   w4 +         $0x80
0800C032: 23 4F                         w4 =:        b.0x3C
0800C034: B5 50 05 C5 18 4F C5 20 C5 24 C5 2C       callg        b.0x40,$0x5,@b.0x18,b.0x3C,@b.0x20,@b.0x24,@b.0x2C
0800C040: 20 51                         w1 =:        b.0x44
0800C042: D0 36                         if k go      $0x36
0800C044: C0 11                         go           $0x11
0800C046: B5 50 04 C5 18 C5 1C C5 20 C5 24    callg        b.0x40,$0x4,@b.0x18,@b.0x1C,@b.0x20,@b.0x24
0800C051: 20 51                         w1 =:        b.0x44
0800C053: D0 25                         if k go      $0x25
0800C055: C0 02                         go           $0x2
0800C057: 18 4D                         r:=          b.0x34
0800C059: 04 80                         by1 :=       r.0x0
0800C05B: FC 90 08                      by1 and      $0x8
0800C05E: C4 16                         if = go      $0x16
0800C060: FD 3D 46                      w2 laddr     b.0x18
0800C063: 21 4C                         w2 =:        b.0x30
0800C065: 0E 4E                         w3 :=        b.0x38
0800C067: 6E 04                         w3 *         $0x4
0800C069: 56 4C                         w3 +         b.0x30
0800C06B: 22 4C                         w3 =:        b.0x30
0800C06D: 0F 51                         w4 :=        b.0x44
0800C06F: 18 C5 30                      r:=          @b.0x30
0800C072: 23 80                         w4 =:        r.0x0
0800C074: 4A 43                         w stz        b.0xC
0800C076: C0 05                         go           $0x5
0800C078: 1A 51 43                      w move       b.0x44,b.0xC
0800C07B: 18 40                         r:=          b.0x0
0800C07D: 0C 43                         w1 :=        b.0xC
0800C07F: 20 83                         w1 =:        r.0xC
0800C081: 80                            ret
0800C082: 9C                            entd
0800C083: FE 03                         clrk
0800C085: 54 03                         w1 +         $0x3
0800C087: E4 3C                         w1 and       $0x3C
0800C089: 54 42                         w1 +         b.0x8
0800C08B: FD C9 D1                      tos=:        r2
0800C08E: 34 D1                         w1 comp      r2
0800C090: CC 06                         if >= go     $0x6
0800C092: 52 D0 42                      w swap       r1,b.0x8
0800C095: 82                            retd
0800C096: FE 02                         setk
0800C098: 82                            retd
0800C099: B8 CF 00 00 00 1C             ents         $0x1C
0800C09F: 20 46                         w1 =:        b.0x18
0800C0A1: FC AD D0 3F                   w sha        r1,$0x3F
0800C0A5: 18 42                         r:=          b.0x8
0800C0A7: 1A 45 85                      w move       b.0x14,r.0x14
0800C0AA: C3 08 00 CB 01 00             call         $0x800CB01,$0x0
0800C0B0: 9D                            ifkret
0800C0B1: 80                            ret
0800C0B2: B8 CF 00 00 01 18             ents         $0x118
0800C0B8: C0 0D                         go           $0xD
0800C0BA: 9C                            entd
0800C0BB: FD C0 7D                      l=:          b.0xF4
0800C0BE: 20 43                         w1 =:        b.0xC
0800C0C0: 81                            retk
0800C0C1: FE 03                         clrk
0800C0C3: B4 7D                         jumpg        b.0xF4
0800C0C5: 4A 55                         w stz        b.0x54
0800C0C7: 0D 4E                         w2 :=        b.0x38
0800C0C9: 35 3F                         w2 comp      $0x3F
0800C0CB: C6 1B                         if >< go     $0x1B
0800C0CD: FD 3D 56                      w2 laddr     b.0x58
0800C0D0: 21 7F                         w2 =:        b.0xFC
0800C0D2: 1A 05 7E                      w move       $0x5,b.0xF8
0800C0D5: CA 0C                         if < go      $0xC
0800C0D7: 84                            bi1 clr
0800C0D8: 85                            bi2 clr
0800C0D9: FD 67 C4 08 00 AA C0 7E       by smove     $0x800AAC0,b.0xF8
0800C0E1: 1A 04 55                      w move       $0x4,b.0x54
0800C0E4: C0 5A                         go           $0x5A
0800C0E6: 0C 4D                         w1 :=        b.0x34
0800C0E8: 20 51                         w1 =:        b.0x44
0800C0EA: 0D 4E                         w2 :=        b.0x38
0800C0EC: 21 C2 01 00                   w2 =:        b.0x100
0800C0F0: 34 D1                         w1 comp      r2
0800C0F2: C8 4C                         if > go      $0x4C
0800C0F4: 0D 51                         w2 :=        b.0x44
0800C0F6: 04 E5 30                      by1 :=       @b.0x30+
0800C0F9: FC 90 0F                      by1 and      $0xF
0800C0FC: 20 54                         w1 =:        b.0x50
0800C0FE: 06 E5 30                      by3 :=       @b.0x30+
0800C101: FC A8 D2 3C                   by shl       r3,$0x3C
0800C105: 22 53                         w3 =:        b.0x4C
0800C107: FD D3 E2 08 00 AA 8C D0       w4 getbi     $0x800AA8C+,r1
0800C10F: C6 04                         if >< go     $0x4
0800C111: C0 2D                         go           $0x2D
0800C113: 07 E5 30                      by4 :=       @b.0x30+
0800C116: 0C 55                         w1 :=        b.0x54
0800C118: 1F D4 58                      by4 =:       b.0x58+
0800C11B: 05 D4 58                      by2 :=       b.0x58+
0800C11E: FC 91 CD 60                   by2 and      $0x60
0800C122: 31 CD 60                      by2 comp     $0x60
0800C125: C6 0C                         if >< go     $0xC
0800C127: 05 D4 58                      by2 :=       b.0x58+
0800C12A: FC 3D CD 20                   by2 -        $0x20
0800C12E: 1D D4 58                      by2 =:       b.0x58+
0800C131: 4F 55                         w incr       b.0x54
0800C133: 2E 55 04                      w comp2      b.0x54,$0x4
0800C136: CC 08                         if >= go     $0x8
0800C138: BF 51 C2 01 00 BC             d loopi      b.0x44,b.0x100,$0xFFFFFFFFFFFFFFBC
0800C13E: 0C 55                         w1 :=        b.0x54
0800C140: 19 CD 27 D4 58                by move      $0x27,b.0x58+
0800C145: 4A 55                         w stz        b.0x54
0800C147: FD 3C 50                      w1 laddr     b.0x40
0800C14A: 20 7F                         w1 =:        b.0xFC
0800C14C: 1A 02 7E                      w move       $0x2,b.0xF8
0800C14F: CA 09                         if < go      $0x9
0800C151: 85                            bi2 clr
0800C152: 06 CD 20                      by3 :=       $0x20
0800C155: FD 82 7E                      by3 sfill    b.0xF8
0800C158: 0C 47                         w1 :=        b.0x1C
0800C15A: 20 51                         w1 =:        b.0x44
0800C15C: 0D 48                         w2 :=        b.0x20
0800C15E: 21 C2 01 04                   w2 =:        b.0x104
0800C162: 34 D1                         w1 comp      r2
0800C164: C8 48                         if > go      $0x48
0800C166: 04 CD 20                      by1 :=       $0x20
0800C169: 0D 51                         w2 :=        b.0x44
0800C16B: 2D E5 18 D0                   by comp2     @b.0x18+,r1
0800C16F: C4 32                         if = go      $0x32
0800C171: 06 E5 18                      by3 :=       @b.0x18+
0800C174: FD 3F 50                      w4 laddr     b.0x40
0800C177: 23 C2 01 08                   w4 =:        b.0x108
0800C17B: 0C 55                         w1 :=        b.0x54
0800C17D: 1E E8 01 08                   by3 =:       @b.0x108+
0800C181: 05 E4 18                      by2 :=       @b.0x18+
0800C184: FC 91 CD 60                   by2 and      $0x60
0800C188: 31 CD 60                      by2 comp     $0x60
0800C18B: C6 14                         if >< go     $0x14
0800C18D: 05 E4 18                      by2 :=       @b.0x18+
0800C190: FC 3D CD 20                   by2 -        $0x20
0800C194: FD 3E 50                      w3 laddr     b.0x40
0800C197: 22 C2 01 08                   w3 =:        b.0x108
0800C19B: 1D E8 01 08                   by2 =:       @b.0x108+
0800C19F: 4F 55                         w incr       b.0x54
0800C1A1: 2E 55 02                      w comp2      b.0x54,$0x2
0800C1A4: CC 08                         if >= go     $0x8
0800C1A6: BF 51 C2 01 04 C0             d loopi      b.0x44,b.0x104,$0xFFFFFFFFFFFFFFC0
0800C1AC: 4A 55                         w stz        b.0x54
0800C1AE: 0C 4A                         w1 :=        b.0x28
0800C1B0: 20 51                         w1 =:        b.0x44
0800C1B2: 0D 4B                         w2 :=        b.0x2C
0800C1B4: 21 C2 01 08                   w2 =:        b.0x108
0800C1B8: 34 D1                         w1 comp      r2
0800C1BA: C8 4E                         if > go      $0x4E
0800C1BC: 0D 51                         w2 :=        b.0x44
0800C1BE: 04 E5 24                      by1 :=       @b.0x24+
0800C1C1: FC 90 0F                      by1 and      $0xF
0800C1C4: 20 54                         w1 =:        b.0x50
0800C1C6: 06 E5 24                      by3 :=       @b.0x24+
0800C1C9: FC A8 D2 3C                   by shl       r3,$0x3C
0800C1CD: 22 53                         w3 =:        b.0x4C
0800C1CF: FD D3 E2 08 00 AA 8C D0       w4 getbi     $0x800AA8C+,r1
0800C1D7: C6 04                         if >< go     $0x4
0800C1D9: C0 2F                         go           $0x2F
0800C1DB: 07 E5 24                      by4 :=       @b.0x24+
0800C1DE: 0C 55                         w1 :=        b.0x54
0800C1E0: 1F D4 5D                      by4 =:       b.0x5D+
0800C1E3: 06 D4 5D                      by3 :=       b.0x5D+
0800C1E6: FC 92 CD 60                   by3 and      $0x60
0800C1EA: 32 CD 60                      by3 comp     $0x60
0800C1ED: C6 0C                         if >< go     $0xC
0800C1EF: 06 D4 5D                      by3 :=       b.0x5D+
0800C1F2: FC 3E CD 20                   by3 -        $0x20
0800C1F6: 1E D4 5D                      by3 =:       b.0x5D+
0800C1F9: 4F 55                         w incr       b.0x54
0800C1FB: 2E 55 CE 00 95                w comp2      b.0x54,$0x95
0800C200: CC 08                         if >= go     $0x8
0800C202: BF 51 C2 01 08 BA             d loopi      b.0x44,b.0x108,$0xFFFFFFFFFFFFFFBA
0800C208: 0C 55                         w1 :=        b.0x54
0800C20A: 19 CD 27 D4 5D                by move      $0x27,b.0x5D+
0800C20F: 85                            bi2 clr
0800C210: FE 25 E1 08 00 AA 3C          by2 laddr    $0x800AA3C+
0800C217: 21 4F                         w2 =:        b.0x3C
0800C219: 0E CD 48                      w3 :=        $0x48
0800C21C: FE 26 E2 08 00 AA 3C          by3 laddr    $0x800AA3C+
0800C223: 22 C2 01 0C                   w3 =:        b.0x10C
0800C227: 35 D2                         w2 comp      r3
0800C229: D4 1D                         if >> go     $0x1D
0800C22B: 08 50                         h1 :=        b.0x40
0800C22D: 18 4F                         r:=          b.0x3C
0800C22F: FC 16 80 D0                   h comp2      r.0x0,r1
0800C233: C4 0E                         if = go      $0xE
0800C235: 53 4F 08                      w add2       b.0x3C,$0x8
0800C238: 2E 4F C2 01 0C                w comp2      b.0x3C,b.0x10C
0800C23D: DA EE                         if <<= go    $0xFFFFFFFFFFFFFFEE
0800C23F: C0 07                         go           $0x7
0800C241: 1A 81 52                      w move       r.0x4,b.0x48
0800C244: C0 05                         go           $0x5
0800C246: 1A 3F 52                      w move       $0x3F,b.0x48
0800C249: 2E 52 3F                      w comp2      b.0x48,$0x3F
0800C24C: C7 00 79                      if >< go     $0x79
0800C24F: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
0800C254: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
0800C259: 4F 7E                         w incr       b.0xF8
0800C25B: 84                            bi1 clr
0800C25C: 85                            bi2 clr
0800C25D: FD BE 7E C4 08 00 AA C8 00    by scopa     b.0xF8,$0x800AAC8,$0x0
0800C266: C6 07                         if >< go     $0x7
0800C268: 1A 0B 52                      w move       $0xB,b.0x48
0800C26B: C0 5A                         go           $0x5A
0800C26D: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
0800C272: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
0800C277: 4F 7E                         w incr       b.0xF8
0800C279: 84                            bi1 clr
0800C27A: 85                            bi2 clr
0800C27B: FD BE 7E C4 08 00 AA D0 00    by scopa     b.0xF8,$0x800AAD0,$0x0
0800C284: C6 07                         if >< go     $0x7
0800C286: 1A 0A 52                      w move       $0xA,b.0x48
0800C289: C0 3C                         go           $0x3C
0800C28B: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
0800C290: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
0800C295: 4F 7E                         w incr       b.0xF8
0800C297: 84                            bi1 clr
0800C298: 85                            bi2 clr
0800C299: FD BE 7E C4 08 00 AA D8 00    by scopa     b.0xF8,$0x800AAD8,$0x0
0800C2A2: C6 07                         if >< go     $0x7
0800C2A4: 1A 13 52                      w move       $0x13,b.0x48
0800C2A7: C0 1E                         go           $0x1E
0800C2A9: FC 6E 48 47 7E                w sub3       b.0x20,b.0x1C,b.0xF8
0800C2AE: FC 69 47 46 7F                w add3       b.0x1C,b.0x18,b.0xFC
0800C2B3: 4F 7E                         w incr       b.0xF8
0800C2B5: 84                            bi1 clr
0800C2B6: 85                            bi2 clr
0800C2B7: FD BE 7E C4 08 00 AA E0 00    by scopa     b.0xF8,$0x800AAE0,$0x0
0800C2C0: C6 05                         if >< go     $0x5
0800C2C2: 1A 12 52                      w move       $0x12,b.0x48
0800C2C5: FD 3C C1 5D                   w1 laddr     b.0x5D
0800C2C9: 18 42                         r:=          b.0x8
0800C2CB: 20 85                         w1 =:        r.0x14
0800C2CD: 4A 86                         w stz        r.0x18
0800C2CF: 1A CE 00 95 87                w move       $0x95,r.0x1C
0800C2D4: FD 3D 56                      w2 laddr     b.0x58
0800C2D7: 21 88                         w2 =:        r.0x20
0800C2D9: 4A 89                         w stz        r.0x24
0800C2DB: 1A 04 8A                      w move       $0x4,r.0x28
0800C2DE: 1A 52 8B                      w move       b.0x48,r.0x2C
0800C2E1: C3 08 00 CA 62 00             call         $0x800CA62,$0x0
0800C2E7: D2 08                         if -k go     $0x8
0800C2E9: C3 08 00 C0 BA 00             call         $0x800C0BA,$0x0
0800C2EF: 20 45                         w1 =:        b.0x14
0800C2F1: 80                            ret
0800C2F2: B8 CF 00 00 00 18             ents         $0x18
0800C2F8: 18 42                         r:=          b.0x8
0800C2FA: 1A 45 85                      w move       b.0x14,r.0x14
0800C2FD: C3 08 00 CA 53 00             call         $0x800CA53,$0x0
0800C303: 9D                            ifkret
0800C304: 80                            ret
0800C305: B8 CF 00 00 00 1C             ents         $0x1C
0800C30B: 20 46                         w1 =:        b.0x18
0800C30D: 18 42                         r:=          b.0x8
0800C30F: 1A 45 85                      w move       b.0x14,r.0x14
0800C312: C3 08 00 CA ED 00             call         $0x800CAED,$0x0
0800C318: 9D                            ifkret
0800C319: 80                            ret
0800C31A: B8 CF 00 00 00 1C             ents         $0x1C
0800C320: 18 42                         r:=          b.0x8
0800C322: 1A 45 85                      w move       b.0x14,r.0x14
0800C325: C3 08 00 CA D8 00             call         $0x800CAD8,$0x0
0800C32B: 9D                            ifkret
0800C32C: 80                            ret
0800C32D: B8 CF 00 00 00 44             ents         $0x44
0800C333: 0C 49                         w1 :=        b.0x24
0800C335: 0D 48                         w2 :=        b.0x20
0800C337: 60 D1                         w1 -         r2
0800C339: 54 01                         w1 +         $0x1
0800C33B: 54 01                         w1 +         $0x1
0800C33D: FC AD D0 3F                   w sha        r1,$0x3F
0800C341: 20 4B                         w1 =:        b.0x2C
0800C343: 0E 48                         w3 :=        b.0x20
0800C345: 7A 04                         w3 /         $0x4
0800C347: 22 4C                         w3 =:        b.0x30
0800C349: 0F 49                         w4 :=        b.0x24
0800C34B: 7B 04                         w4 /         $0x4
0800C34D: 23 4D                         w4 =:        b.0x34
0800C34F: 18 42                         r:=          b.0x8
0800C351: 1A 45 85                      w move       b.0x14,r.0x14
0800C354: 1A C4 08 00 AA E8 86          w move       $0x800AAE8,r.0x18
0800C35B: 1A D3 50                      w move       r4,b.0x40
0800C35E: 1A D2 4F                      w move       r3,b.0x3C
0800C361: 0D 47                         w2 :=        b.0x1C
0800C363: 21 4E                         w2 =:        b.0x38
0800C365: FD 20 4E 87 0C                by bmove     b.0x38,r.0x1C,$0xC
0800C36A: 1A 46 8A                      w move       b.0x18,r.0x28
0800C36D: 20 8B                         w1 =:        r.0x2C
0800C36F: C3 08 00 CB 1D 00             call         $0x800CB1D,$0x0
0800C375: 9D                            ifkret
0800C376: 0D 49                         w2 :=        b.0x24
0800C378: 0E 48                         w3 :=        b.0x20
0800C37A: 61 D2                         w2 -         r3
0800C37C: 55 01                         w2 +         $0x1
0800C37E: 0C D1                         w1 :=        r2
0800C380: 80                            ret
0800C381: B8 CF 00 00 00 44             ents         $0x44
0800C387: 0C 49                         w1 :=        b.0x24
0800C389: 0D 48                         w2 :=        b.0x20
0800C38B: 60 D1                         w1 -         r2
0800C38D: 54 01                         w1 +         $0x1
0800C38F: 54 01                         w1 +         $0x1
0800C391: FC AD D0 3F                   w sha        r1,$0x3F
0800C395: 20 4B                         w1 =:        b.0x2C
0800C397: 0E 48                         w3 :=        b.0x20
0800C399: 7A 04                         w3 /         $0x4
0800C39B: 22 4C                         w3 =:        b.0x30
0800C39D: 0F 49                         w4 :=        b.0x24
0800C39F: 7B 04                         w4 /         $0x4
0800C3A1: 23 4D                         w4 =:        b.0x34
0800C3A3: 18 42                         r:=          b.0x8
0800C3A5: 1A 45 85                      w move       b.0x14,r.0x14
0800C3A8: 1A C4 08 00 AA EC 86          w move       $0x800AAEC,r.0x18
0800C3AF: 1A D3 50                      w move       r4,b.0x40
0800C3B2: 1A D2 4F                      w move       r3,b.0x3C
0800C3B5: 0D 47                         w2 :=        b.0x1C
0800C3B7: 21 4E                         w2 =:        b.0x38
0800C3B9: FD 20 4E 87 0C                by bmove     b.0x38,r.0x1C,$0xC
0800C3BE: 1A 46 8A                      w move       b.0x18,r.0x28
0800C3C1: 20 8B                         w1 =:        r.0x2C
0800C3C3: C3 08 00 CB 41 00             call         $0x800CB41,$0x0
0800C3C9: 9D                            ifkret
0800C3CA: 0D 49                         w2 :=        b.0x24
0800C3CC: 0E 48                         w3 :=        b.0x20
0800C3CE: 61 D2                         w2 -         r3
0800C3D0: 55 01                         w2 +         $0x1
0800C3D2: 0C D1                         w1 :=        r2
0800C3D4: 80                            ret
0800C3D5: B8 CF 00 00 00 54             ents         $0x54
0800C3DB: 0C 47                         w1 :=        b.0x1C
0800C3DD: 20 51                         w1 =:        b.0x44
0800C3DF: 0D 4A                         w2 :=        b.0x28
0800C3E1: 21 53                         w2 =:        b.0x4C
0800C3E3: 54 01                         w1 +         $0x1
0800C3E5: 20 4F                         w1 =:        b.0x3C
0800C3E7: 86                            bi3 clr
0800C3E8: 22 4E                         w3 =:        b.0x38
0800C3EA: 22 52                         w3 =:        b.0x48
0800C3EC: 18 42                         r:=          b.0x8
0800C3EE: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
0800C3F3: 20 88                         w1 =:        r.0x20
0800C3F5: C3 08 00 C9 9A 00             call         $0x800C99A,$0x0
0800C3FB: 9D                            ifkret
0800C3FC: 18 42                         r:=          b.0x8
0800C3FE: 1A 88 4F                      w move       r.0x20,b.0x3C
0800C401: 20 50                         w1 =:        b.0x40
0800C403: 0E 51                         w3 :=        b.0x44
0800C405: 05 E6 18                      by2 :=       @b.0x18+
0800C408: FC 91 CD DF                   by2 and      $0xDF
0800C40C: 31 CD 41                      by2 comp     $0x41
0800C40F: C4 0A                         if = go      $0xA
0800C411: 0D 4B                         w2 :=        b.0x2C
0800C413: 61 53                         w2 -         b.0x4C
0800C415: 55 01                         w2 +         $0x1
0800C417: 21 50                         w2 =:        b.0x40
0800C419: 44 50                         w test       b.0x40
0800C41B: C6 17                         if >< go     $0x17
0800C41D: 0C 4B                         w1 :=        b.0x2C
0800C41F: 60 53                         w1 -         b.0x4C
0800C421: 54 01                         w1 +         $0x1
0800C423: 20 50                         w1 =:        b.0x40
0800C425: 34 CE 00 87                   w1 comp      $0x87
0800C429: CE 07                         if <= go     $0x7
0800C42B: 1A CE 00 88 50                w move       $0x88,b.0x40
0800C430: 4D 52                         w set1       b.0x48
0800C432: 4A 51                         w stz        b.0x44
0800C434: 4A 4F                         w stz        b.0x3C
0800C436: 0C 50                         w1 :=        b.0x40
0800C438: 60 01                         w1 -         $0x1
0800C43A: 20 54                         w1 =:        b.0x50
0800C43C: 2E 4F D0                      w comp2      b.0x3C,r1
0800C43F: C8 65                         if > go      $0x65
0800C441: C3 08 00 C9 EA 02 45 4D       call         $0x800C9EA,$0x2,b.0x14,b.0x34
0800C449: 9D                            ifkret
0800C44A: 4F 4E                         w incr       b.0x38
0800C44C: 2E 4E 01                      w comp2      b.0x38,$0x1
0800C44F: C6 0B                         if >< go     $0xB
0800C451: 2E 4D 0A                      w comp2      b.0x34,$0xA
0800C454: C6 06                         if >< go     $0x6
0800C456: 4D 51                         w set1       b.0x44
0800C458: C0 E9                         go           $0xFFFFFFFFFFFFFFE9
0800C45A: 2E 4E 01                      w comp2      b.0x38,$0x1
0800C45D: C6 0F                         if >< go     $0xF
0800C45F: 44 52                         w test       b.0x48
0800C461: C6 0B                         if >< go     $0xB
0800C463: 2E 4D 0D                      w comp2      b.0x34,$0xD
0800C466: C6 06                         if >< go     $0x6
0800C468: 51 4E                         w decr       b.0x38
0800C46A: C0 D7                         go           $0xFFFFFFFFFFFFFFD7
0800C46C: 2E 4D 0D                      w comp2      b.0x34,$0xD
0800C46F: C4 11                         if = go      $0x11
0800C471: 0D 4D                         w2 :=        b.0x34
0800C473: 0E 53                         w3 :=        b.0x4C
0800C475: 1D E6 24                      by2 =:       @b.0x24+
0800C478: 4F 53                         w incr       b.0x4C
0800C47A: BF 4F 54 C7                   d loopi      b.0x3C,b.0x50,$0xFFFFFFFFFFFFFFC7
0800C47E: C0 26                         go           $0x26
0800C480: 2E 4E 50                      w comp2      b.0x38,b.0x40
0800C483: CC 1B                         if >= go     $0x1B
0800C485: 44 52                         w test       b.0x48
0800C487: C6 17                         if >< go     $0x17
0800C489: 4F 4F                         w incr       b.0x3C
0800C48B: 2E 4F 50                      w comp2      b.0x3C,b.0x40
0800C48E: C8 10                         if > go      $0x10
0800C490: 04 CD 20                      by1 :=       $0x20
0800C493: 0D 53                         w2 :=        b.0x4C
0800C495: 1C E5 24                      by1 =:       @b.0x24+
0800C498: 4F 53                         w incr       b.0x4C
0800C49A: BF 4F 50 F6                   d loopi      b.0x3C,b.0x40,$0xFFFFFFFFFFFFFFF6
0800C49E: 44 4E                         w test       b.0x38
0800C4A0: CE 04                         if <= go     $0x4
0800C4A2: 51 4E                         w decr       b.0x38
0800C4A4: 44 51                         w test       b.0x44
0800C4A6: C4 04                         if = go      $0x4
0800C4A8: 51 4E                         w decr       b.0x38
0800C4AA: 0C 4E                         w1 :=        b.0x38
0800C4AC: 80                            ret
0800C4AD: B8 CF 00 00 00 7C             ents         $0x7C
0800C4B3: 0C 4B                         w1 :=        b.0x2C
0800C4B5: 0D 4A                         w2 :=        b.0x28
0800C4B7: 21 4E                         w2 =:        b.0x38
0800C4B9: 60 D1                         w1 -         r2
0800C4BB: 54 01                         w1 +         $0x1
0800C4BD: 20 53                         w1 =:        b.0x4C
0800C4BF: 20 51                         w1 =:        b.0x44
0800C4C1: 0E 48                         w3 :=        b.0x20
0800C4C3: 22 4F                         w3 =:        b.0x3C
0800C4C5: 0F 47                         w4 :=        b.0x1C
0800C4C7: 23 50                         w4 =:        b.0x40
0800C4C9: 62 D3                         w3 -         r4
0800C4CB: 22 4D                         w3 =:        b.0x34
0800C4CD: 57 01                         w4 +         $0x1
0800C4CF: 23 56                         w4 =:        b.0x58
0800C4D1: 4A 54                         w stz        b.0x50
0800C4D3: 4D 58                         w set1       b.0x60
0800C4D5: 1A CD 20 52                   w move       $0x20,b.0x48
0800C4D9: 4A 5A                         w stz        b.0x68
0800C4DB: 2E 4F 3F                      w comp2      b.0x3C,$0x3F
0800C4DE: C6 04                         if >< go     $0x4
0800C4E0: 4D 5A                         w set1       b.0x68
0800C4E2: 0C 50                         w1 :=        b.0x40
0800C4E4: 05 E4 18                      by2 :=       @b.0x18+
0800C4E7: FC 91 CD DF                   by2 and      $0xDF
0800C4EB: 4A 5B                         w stz        b.0x6C
0800C4ED: 31 CD 41                      by2 comp     $0x41
0800C4F0: C4 04                         if = go      $0x4
0800C4F2: 4D 5B                         w set1       b.0x6C
0800C4F4: 0D 5A                         w2 :=        b.0x68
0800C4F6: A1 5B                         w2 or        b.0x6C
0800C4F8: 4A 5C                         w stz        b.0x70
0800C4FA: 44 D2                         w test       r3
0800C4FC: C6 04                         if >< go     $0x4
0800C4FE: 4D 5C                         w set1       b.0x70
0800C500: A1 5C                         w2 or        b.0x70
0800C502: 4A 5D                         w stz        b.0x74
0800C504: 36 01                         w3 comp      $0x1
0800C506: C6 04                         if >< go     $0x4
0800C508: 4D 5D                         w set1       b.0x74
0800C50A: 06 E7 18                      by3 :=       @b.0x18+
0800C50D: FC 92 CD DF                   by3 and      $0xDF
0800C511: 4A 5E                         w stz        b.0x78
0800C513: 32 CD 4C                      by3 comp     $0x4C
0800C516: C6 04                         if >< go     $0x4
0800C518: 4D 5E                         w set1       b.0x78
0800C51A: 0E 5D                         w3 :=        b.0x74
0800C51C: E6 5E                         w3 and       b.0x78
0800C51E: A2 D1                         w3 or        r2
0800C520: 22 59                         w3 =:        b.0x64
0800C522: 44 D2                         w test       r3
0800C524: C6 4F                         if >< go     $0x4F
0800C526: 04 E7 18                      by1 :=       @b.0x18+
0800C529: FC 90 CD DF                   by1 and      $0xDF
0800C52D: 30 CD 4C                      by1 comp     $0x4C
0800C530: C6 06                         if >< go     $0x6
0800C532: 4A 58                         w stz        b.0x60
0800C534: 4F 56                         w incr       b.0x58
0800C536: 18 42                         r:=          b.0x8
0800C538: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
0800C53D: 1A 56 88                      w move       b.0x58,r.0x20
0800C540: C3 08 00 C9 9A 00             call         $0x800C99A,$0x0
0800C546: 9D                            ifkret
0800C547: 18 42                         r:=          b.0x8
0800C549: 1A 88 56                      w move       r.0x20,b.0x58
0800C54C: 20 51                         w1 =:        b.0x44
0800C54E: 44 58                         w test       b.0x60
0800C550: C4 23                         if = go      $0x23
0800C552: 34 53                         w1 comp      b.0x4C
0800C554: CE 1F                         if <= go     $0x1F
0800C556: 20 85                         w1 =:        r.0x14
0800C558: 1A 52 86                      w move       b.0x48,r.0x18
0800C55B: 1A 45 87                      w move       b.0x14,r.0x1C
0800C55E: 0C 53                         w1 :=        b.0x4C
0800C560: C3 08 00 C8 F8 00             call         $0x800C8F8,$0x0
0800C566: 9D                            ifkret
0800C567: 54 54                         w1 +         b.0x50
0800C569: 20 54                         w1 =:        b.0x50
0800C56B: 1A 53 51                      w move       b.0x4C,b.0x44
0800C56E: 44 D0                         w test       r1
0800C570: CC 03                         if >= go     $0x3
0800C572: 80                            ret
0800C573: 44 58                         w test       b.0x60
0800C575: C6 0C                         if >< go     $0xC
0800C577: 2E 51 53                      w comp2      b.0x44,b.0x4C
0800C57A: CE 07                         if <= go     $0x7
0800C57C: 1A 53 57                      w move       b.0x4C,b.0x5C
0800C57F: C0 05                         go           $0x5
0800C581: 1A 51 57                      w move       b.0x44,b.0x5C
0800C584: 4A 56                         w stz        b.0x58
0800C586: 2E 56 57                      w comp2      b.0x58,b.0x5C
0800C589: CC 55                         if >= go     $0x55
0800C58B: 04 CD 24                      by1 :=       $0x24
0800C58E: 0D 4E                         w2 :=        b.0x38
0800C590: 2D E5 24 D0                   by comp2     @b.0x24+,r1
0800C594: C6 34                         if >< go     $0x34
0800C596: 55 01                         w2 +         $0x1
0800C598: 0E 4B                         w3 :=        b.0x2C
0800C59A: 35 D2                         w2 comp      r3
0800C59C: C8 19                         if > go      $0x19
0800C59E: 0D 4E                         w2 :=        b.0x38
0800C5A0: 55 01                         w2 +         $0x1
0800C5A2: 07 CD 24                      by4 :=       $0x24
0800C5A5: 2D E5 24 D3                   by comp2     @b.0x24+,r4
0800C5A9: C6 0C                         if >< go     $0xC
0800C5AB: 4F 4E                         w incr       b.0x38
0800C5AD: 1A CD 24 55                   w move       $0x24,b.0x54
0800C5B1: 4F 56                         w incr       b.0x58
0800C5B3: C0 13                         go           $0x13
0800C5B5: 1A 0D 55                      w move       $0xD,b.0x54
0800C5B8: C3 08 00 C9 D2 02 45 55       call         $0x800C9D2,$0x2,b.0x14,b.0x54
0800C5C0: 9D                            ifkret
0800C5C1: 4F 54                         w incr       b.0x50
0800C5C3: 1A 0A 55                      w move       $0xA,b.0x54
0800C5C6: C0 07                         go           $0x7
0800C5C8: 06 E5 24                      by3 :=       @b.0x24+
0800C5CB: 22 55                         w3 =:        b.0x54
0800C5CD: C3 08 00 C9 D2 02 45 55       call         $0x800C9D2,$0x2,b.0x14,b.0x54
0800C5D5: 9D                            ifkret
0800C5D6: 4F 54                         w incr       b.0x50
0800C5D8: 4F 4E                         w incr       b.0x38
0800C5DA: 4F 56                         w incr       b.0x58
0800C5DC: C0 AA                         go           $0xFFFFFFFFFFFFFFAA
0800C5DE: 44 58                         w test       b.0x60
0800C5E0: C6 1F                         if >< go     $0x1F
0800C5E2: 2E 53 51                      w comp2      b.0x4C,b.0x44
0800C5E5: C4 1A                         if = go      $0x1A
0800C5E7: 18 42                         r:=          b.0x8
0800C5E9: 1A 51 85                      w move       b.0x44,r.0x14
0800C5EC: 1A 52 86                      w move       b.0x48,r.0x18
0800C5EF: 1A 45 87                      w move       b.0x14,r.0x1C
0800C5F2: 0C 53                         w1 :=        b.0x4C
0800C5F4: C3 08 00 C8 F8 00             call         $0x800C8F8,$0x0
0800C5FA: 9D                            ifkret
0800C5FB: 54 54                         w1 +         b.0x50
0800C5FD: 20 54                         w1 =:        b.0x50
0800C5FF: 0C 54                         w1 :=        b.0x50
0800C601: 80                            ret
0800C602: B8 CF 00 00 00 5C             ents         $0x5C
0800C608: 0C 49                         w1 :=        b.0x24
0800C60A: 20 54                         w1 =:        b.0x50
0800C60C: 20 55                         w1 =:        b.0x54
0800C60E: 85                            bi2 clr
0800C60F: 21 4E                         w2 =:        b.0x38
0800C611: 21 51                         w2 =:        b.0x44
0800C613: 0E 47                         w3 :=        b.0x1C
0800C615: 22 4B                         w3 =:        b.0x2C
0800C617: 56 01                         w3 +         $0x1
0800C619: 22 4C                         w3 =:        b.0x30
0800C61B: 18 42                         r:=          b.0x8
0800C61D: FD 20 46 85 0C                by bmove     b.0x18,r.0x14,$0xC
0800C622: 22 88                         w3 =:        r.0x20
0800C624: C3 08 00 C9 9A 00             call         $0x800C99A,$0x0
0800C62A: 9D                            ifkret
0800C62B: 18 42                         r:=          b.0x8
0800C62D: 1A 88 4C                      w move       r.0x20,b.0x30
0800C630: 20 4D                         w1 =:        b.0x34
0800C632: 0E 4B                         w3 :=        b.0x2C
0800C634: 05 E6 18                      by2 :=       @b.0x18+
0800C637: FC 91 CD DF                   by2 and      $0xDF
0800C63B: 31 CD 42                      by2 comp     $0x42
0800C63E: C6 05                         if >< go     $0x5
0800C640: C1 02 40                      go           $0x240
0800C643: FD D1 49 1F                   w2 getbi     b.0x24,$0x1F
0800C647: C4 54                         if = go      $0x54
0800C649: 0F 49                         w4 :=        b.0x24
0800C64B: E7 CF 7F FF FF FF             w4 and       $0x7FFFFFFF
0800C651: 44 D3                         w test       r4
0800C653: C6 48                         if >< go     $0x48
0800C655: 1A 0B 51                      w move       $0xB,b.0x44
0800C658: 07 E6 18                      by4 :=       @b.0x18+
0800C65B: FC 93 CD DF                   by4 and      $0xDF
0800C65F: 33 CD 42                      by4 comp     $0x42
0800C662: C6 1D                         if >< go     $0x1D
0800C664: 4A 4C                         w stz        b.0x30
0800C666: 0D 4C                         w2 :=        b.0x30
0800C668: 04 E1 08 00 AB 23             by1 :=       $0x800AB23+
0800C66E: 20 56                         w1 =:        b.0x58
0800C670: C3 08 00 C9 D2 02 45 56       call         $0x800C9D2,$0x2,b.0x14,b.0x58
0800C678: 9D                            ifkret
0800C679: BF 4C 0A ED                   d loopi      b.0x30,$0xA,$0xFFFFFFFFFFFFFFED
0800C67D: C0 1B                         go           $0x1B
0800C67F: 4A 4C                         w stz        b.0x30
0800C681: 0D 4C                         w2 :=        b.0x30
0800C683: 04 E1 08 00 AB 18             by1 :=       $0x800AB18+
0800C689: 20 56                         w1 =:        b.0x58
0800C68B: C3 08 00 C9 D2 02 45 56       call         $0x800C9D2,$0x2,b.0x14,b.0x58
0800C693: 9D                            ifkret
0800C694: BF 4C 0A ED                   d loopi      b.0x30,$0xA,$0xFFFFFFFFFFFFFFED
0800C698: C1 02 5D                      go           $0x25D
0800C69B: 1A 54 53                      w move       b.0x50,b.0x4C
0800C69E: 1A CD 20 4F                   w move       $0x20,b.0x3C
0800C6A2: 05 E6 18                      by2 :=       @b.0x18+
0800C6A5: FC 91 CD DF                   by2 and      $0xDF
0800C6A9: 31 CD 4F                      by2 comp     $0x4F
0800C6AC: C4 0D                         if = go      $0xD
0800C6AE: 05 E6 18                      by2 :=       @b.0x18+
0800C6B1: FC 91 3F                      by2 and      $0x3F
0800C6B4: 31 CD 5A                      by2 comp     $0x5A
0800C6B7: C6 05                         if >< go     $0x5
0800C6B9: C1 00 F4                      go           $0xF4
0800C6BC: FD D1 54 1F                   w2 getbi     b.0x50,$0x1F
0800C6C0: C4 0F                         if = go      $0xF
0800C6C2: 0F 54                         w4 :=        b.0x50
0800C6C4: 93                            w4 neg
0800C6C5: 23 54                         w4 =:        b.0x50
0800C6C7: 23 55                         w4 =:        b.0x54
0800C6C9: 23 53                         w4 =:        b.0x4C
0800C6CB: 1A CD 2D 4F                   w move       $0x2D,b.0x3C
0800C6CF: 0F 48                         w4 :=        b.0x20
0800C6D1: 37 3F                         w4 comp      $0x3F
0800C6D3: C4 0E                         if = go      $0xE
0800C6D5: 07 E6 18                      by4 :=       @b.0x18+
0800C6D8: FC 93 CD DF                   by4 and      $0xDF
0800C6DC: 33 CD 49                      by4 comp     $0x49
0800C6DF: C4 04                         if = go      $0x4
0800C6E1: 4A 4D                         w stz        b.0x34
0800C6E3: 4A 4C                         w stz        b.0x30
0800C6E5: 0C 53                         w1 :=        b.0x4C
0800C6E7: 0D 4C                         w2 :=        b.0x30
0800C6E9: 78 E1 08 00 AA F0             w1 /         $0x800AAF0+
0800C6EF: 20 50                         w1 =:        b.0x40
0800C6F1: 44 D0                         w test       r1
0800C6F3: C8 07                         if > go      $0x7
0800C6F5: 2E 53 55                      w comp2      b.0x4C,b.0x54
0800C6F8: C4 11                         if = go      $0x11
0800C6FA: 4F 4E                         w incr       b.0x38
0800C6FC: 0E E1 08 00 AA F0             w3 :=        $0x800AAF0+
0800C702: FC 7F 53 D2 D0                w4 div4      b.0x4C,r3,r1
0800C707: 23 53                         w4 =:        b.0x4C
0800C709: BF 4C 09 DC                   d loopi      b.0x30,$0x9,$0xFFFFFFFFFFFFFFDC
0800C70D: 44 4D                         w test       b.0x34
0800C70F: C6 1E                         if >< go     $0x1E
0800C711: 44 4E                         w test       b.0x38
0800C713: C6 07                         if >< go     $0x7
0800C715: 4D 4E                         w set1       b.0x38
0800C717: 1A 3F 4B                      w move       $0x3F,b.0x2C
0800C71A: 2E 4F CD 2D                   w comp2      b.0x3C,$0x2D
0800C71E: C6 0A                         if >< go     $0xA
0800C720: 0C 4E                         w1 :=        b.0x38
0800C722: 54 01                         w1 +         $0x1
0800C724: 20 4D                         w1 =:        b.0x34
0800C726: C0 07                         go           $0x7
0800C728: 1A 4E 4D                      w move       b.0x38,b.0x34
0800C72B: C0 2A                         go           $0x2A
0800C72D: 44 4E                         w test       b.0x38
0800C72F: C6 07                         if >< go     $0x7
0800C731: 4D 4E                         w set1       b.0x38
0800C733: 1A 3F 4B                      w move       $0x3F,b.0x2C
0800C736: 18 42                         r:=          b.0x8
0800C738: 1A 4D 85                      w move       b.0x34,r.0x14
0800C73B: 1A 4F 86                      w move       b.0x3C,r.0x18
0800C73E: 1A 45 87                      w move       b.0x14,r.0x1C
0800C741: 0C 4E                         w1 :=        b.0x38
0800C743: C3 08 00 C8 F8 00             call         $0x800C8F8,$0x0
0800C749: 9D                            ifkret
0800C74A: 54 51                         w1 +         b.0x44
0800C74C: 20 51                         w1 =:        b.0x44
0800C74E: 44 D0                         w test       r1
0800C750: CC 05                         if >= go     $0x5
0800C752: C1 01 A3                      go           $0x1A3
0800C755: 4A 4C                         w stz        b.0x30
0800C757: 0C 54                         w1 :=        b.0x50
0800C759: 0D 4C                         w2 :=        b.0x30
0800C75B: 78 E1 08 00 AA F0             w1 /         $0x800AAF0+
0800C761: 20 50                         w1 =:        b.0x40
0800C763: 44 D0                         w test       r1
0800C765: C8 07                         if > go      $0x7
0800C767: 2E 54 55                      w comp2      b.0x50,b.0x54
0800C76A: C4 28                         if = go      $0x28
0800C76C: 54 CD 30                      w1 +         $0x30
0800C76F: 20 52                         w1 =:        b.0x48
0800C771: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C779: 9D                            ifkret
0800C77A: 4F 51                         w incr       b.0x44
0800C77C: 0D 54                         w2 :=        b.0x50
0800C77E: 0E 4C                         w3 :=        b.0x30
0800C780: 79 E2 08 00 AA F0             w2 /         $0x800AAF0+
0800C786: 6D E2 08 00 AA F0             w2 *         $0x800AAF0+
0800C78C: 0F 54                         w4 :=        b.0x50
0800C78E: 63 D1                         w4 -         r2
0800C790: 23 54                         w4 =:        b.0x50
0800C792: BF 4C 09 C5                   d loopi      b.0x30,$0x9,$0xFFFFFFFFFFFFFFC5
0800C796: 2E 4B 3F                      w comp2      b.0x2C,$0x3F
0800C799: C6 11                         if >< go     $0x11
0800C79B: 1A CD 30 52                   w move       $0x30,b.0x48
0800C79F: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C7A7: 9D                            ifkret
0800C7A8: 4F 51                         w incr       b.0x44
0800C7AA: C1 01 4B                      go           $0x14B
0800C7AD: 4A 4C                         w stz        b.0x30
0800C7AF: 0C 1F                         w1 :=        $0x1F
0800C7B1: 60 4C                         w1 -         b.0x30
0800C7B3: 20 4E                         w1 =:        b.0x38
0800C7B5: FD D1 53 1F                   w2 getbi     b.0x4C,$0x1F
0800C7B9: C6 0E                         if >< go     $0xE
0800C7BB: 0E 53                         w3 :=        b.0x4C
0800C7BD: FC AD D2 01                   w sha        r3,$0x1
0800C7C1: 22 53                         w3 =:        b.0x4C
0800C7C3: BF 4C 1F EC                   d loopi      b.0x30,$0x1F,$0xFFFFFFFFFFFFFFEC
0800C7C7: 0C 4E                         w1 :=        b.0x38
0800C7C9: 78 03                         w1 /         $0x3
0800C7CB: 54 01                         w1 +         $0x1
0800C7CD: 20 4E                         w1 =:        b.0x38
0800C7CF: 44 4D                         w test       b.0x34
0800C7D1: C6 04                         if >< go     $0x4
0800C7D3: 20 4D                         w1 =:        b.0x34
0800C7D5: 44 4E                         w test       b.0x38
0800C7D7: C6 04                         if >< go     $0x4
0800C7D9: 4D 4E                         w set1       b.0x38
0800C7DB: 04 CD 5A                      by1 :=       $0x5A
0800C7DE: 0D 4B                         w2 :=        b.0x2C
0800C7E0: 2D E5 18 D0                   by comp2     @b.0x18+,r1
0800C7E4: C6 2A                         if >< go     $0x2A
0800C7E6: 0E 4D                         w3 :=        b.0x34
0800C7E8: 62 4E                         w3 -         b.0x38
0800C7EA: 22 50                         w3 =:        b.0x40
0800C7EC: 44 D2                         w test       r3
0800C7EE: CC 07                         if >= go     $0x7
0800C7F0: 1A 4D 4E                      w move       b.0x34,b.0x38
0800C7F3: 4A 50                         w stz        b.0x40
0800C7F5: 44 50                         w test       b.0x40
0800C7F7: C4 15                         if = go      $0x15
0800C7F9: 1A CD 30 52                   w move       $0x30,b.0x48
0800C7FD: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C805: 9D                            ifkret
0800C806: 4F 51                         w incr       b.0x44
0800C808: 51 50                         w decr       b.0x40
0800C80A: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
0800C80C: C0 21                         go           $0x21
0800C80E: 18 42                         r:=          b.0x8
0800C810: 1A 4D 85                      w move       b.0x34,r.0x14
0800C813: 1A 4F 86                      w move       b.0x3C,r.0x18
0800C816: 1A 45 87                      w move       b.0x14,r.0x1C
0800C819: 0C 4E                         w1 :=        b.0x38
0800C81B: C3 08 00 C8 F8 00             call         $0x800C8F8,$0x0
0800C821: 9D                            ifkret
0800C822: 54 51                         w1 +         b.0x44
0800C824: 20 51                         w1 =:        b.0x44
0800C826: 44 D0                         w test       r1
0800C828: CC 05                         if >= go     $0x5
0800C82A: C1 00 CB                      go           $0xCB
0800C82D: 2E 4E 0B                      w comp2      b.0x38,$0xB
0800C830: C6 1D                         if >< go     $0x1D
0800C832: 0C 54                         w1 :=        b.0x50
0800C834: FC AD D0 22                   w sha        r1,$0x22
0800C838: E4 03                         w1 and       $0x3
0800C83A: 54 CD 30                      w1 +         $0x30
0800C83D: 20 52                         w1 =:        b.0x48
0800C83F: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C847: 9D                            ifkret
0800C848: 4F 51                         w incr       b.0x44
0800C84A: 1A 0A 4E                      w move       $0xA,b.0x38
0800C84D: 0D 4E                         w2 :=        b.0x38
0800C84F: 61 01                         w2 -         $0x1
0800C851: 6D 03                         w2 *         $0x3
0800C853: 21 50                         w2 =:        b.0x40
0800C855: 4D 4C                         w set1       b.0x30
0800C857: 2E 4C 4E                      w comp2      b.0x30,b.0x38
0800C85A: C8 24                         if > go      $0x24
0800C85C: 0C 50                         w1 :=        b.0x40
0800C85E: 90                            w1 neg
0800C85F: 0D 54                         w2 :=        b.0x50
0800C861: FC AD D1 D0                   w sha        r2,r1
0800C865: E5 07                         w2 and       $0x7
0800C867: 55 CD 30                      w2 +         $0x30
0800C86A: 21 52                         w2 =:        b.0x48
0800C86C: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C874: 9D                            ifkret
0800C875: 4F 51                         w incr       b.0x44
0800C877: E0 50 03                      w sub2       b.0x40,$0x3
0800C87A: BF 4C 4E E2                   d loopi      b.0x30,b.0x38,$0xFFFFFFFFFFFFFFE2
0800C87E: C0 77                         go           $0x77
0800C880: 44 4D                         w test       b.0x34
0800C882: C6 04                         if >< go     $0x4
0800C884: 4D 4D                         w set1       b.0x34
0800C886: 2E 4D 04                      w comp2      b.0x34,$0x4
0800C889: CE 05                         if <= go     $0x5
0800C88B: 1A 04 4D                      w move       $0x4,b.0x34
0800C88E: 2E 4D 03                      w comp2      b.0x34,$0x3
0800C891: CE 19                         if <= go     $0x19
0800C893: 0C 54                         w1 :=        b.0x50
0800C895: FC AD D0 28                   w sha        r1,$0x28
0800C899: E4 CE 00 FF                   w1 and       $0xFF
0800C89D: 20 52                         w1 =:        b.0x48
0800C89F: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C8A7: 9D                            ifkret
0800C8A8: 4F 51                         w incr       b.0x44
0800C8AA: 2E 4D 02                      w comp2      b.0x34,$0x2
0800C8AD: CE 19                         if <= go     $0x19
0800C8AF: 0D 54                         w2 :=        b.0x50
0800C8B1: FC AD D1 30                   w sha        r2,$0x30
0800C8B5: E5 CE 00 FF                   w2 and       $0xFF
0800C8B9: 21 52                         w2 =:        b.0x48
0800C8BB: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C8C3: 9D                            ifkret
0800C8C4: 4F 51                         w incr       b.0x44
0800C8C6: 2E 4D 01                      w comp2      b.0x34,$0x1
0800C8C9: CE 19                         if <= go     $0x19
0800C8CB: 0D 54                         w2 :=        b.0x50
0800C8CD: FC AD D1 38                   w sha        r2,$0x38
0800C8D1: E5 CE 00 FF                   w2 and       $0xFF
0800C8D5: 21 52                         w2 =:        b.0x48
0800C8D7: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C8DF: 9D                            ifkret
0800C8E0: 4F 51                         w incr       b.0x44
0800C8E2: 0D 54                         w2 :=        b.0x50
0800C8E4: E5 CE 00 FF                   w2 and       $0xFF
0800C8E8: 21 52                         w2 =:        b.0x48
0800C8EA: C3 08 00 C9 D2 02 45 52       call         $0x800C9D2,$0x2,b.0x14,b.0x48
0800C8F2: 9D                            ifkret
0800C8F3: 4F 51                         w incr       b.0x44
0800C8F5: 0C 51                         w1 :=        b.0x44
0800C8F7: 80                            ret
0800C8F8: B8 CF 00 00 00 34             ents         $0x34
0800C8FE: 20 48                         w1 =:        b.0x20
0800C900: 20 49                         w1 =:        b.0x24
0800C902: 4A 4C                         w stz        b.0x30
0800C904: 2E 46 CD 20                   w comp2      b.0x18,$0x20
0800C908: C6 04                         if >< go     $0x4
0800C90A: 51 49                         w decr       b.0x24
0800C90C: 0C 49                         w1 :=        b.0x24
0800C90E: 54 01                         w1 +         $0x1
0800C910: 34 45                         w1 comp      b.0x14
0800C912: CE 17                         if <= go     $0x17
0800C914: 18 42                         r:=          b.0x8
0800C916: 1A 47 85                      w move       b.0x1C,r.0x14
0800C919: 0C 45                         w1 :=        b.0x14
0800C91B: C3 08 00 C9 5F 00             call         $0x800C95F,$0x0
0800C921: 9D                            ifkret
0800C922: 20 4C                         w1 =:        b.0x30
0800C924: 90                            w1 neg
0800C925: 20 4C                         w1 =:        b.0x30
0800C927: C0 35                         go           $0x35
0800C929: 0C 45                         w1 :=        b.0x14
0800C92B: 60 49                         w1 -         b.0x24
0800C92D: 60 01                         w1 -         $0x1
0800C92F: 20 4A                         w1 =:        b.0x28
0800C931: 44 4A                         w test       b.0x28
0800C933: C4 15                         if = go      $0x15
0800C935: 1A CD 20 4B                   w move       $0x20,b.0x2C
0800C939: C3 08 00 C9 D2 02 47 4B       call         $0x800C9D2,$0x2,b.0x1C,b.0x2C
0800C941: 9D                            ifkret
0800C942: 4F 4C                         w incr       b.0x30
0800C944: 51 4A                         w decr       b.0x28
0800C946: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
0800C948: 2E 46 CD 20                   w comp2      b.0x18,$0x20
0800C94C: C4 10                         if = go      $0x10
0800C94E: 1A 46 4B                      w move       b.0x18,b.0x2C
0800C951: C3 08 00 C9 D2 02 47 4B       call         $0x800C9D2,$0x2,b.0x1C,b.0x2C
0800C959: 9D                            ifkret
0800C95A: 4F 4C                         w incr       b.0x30
0800C95C: 0C 4C                         w1 :=        b.0x30
0800C95E: 80                            ret
0800C95F: B8 CF 00 00 00 28             ents         $0x28
0800C965: 20 46                         w1 =:        b.0x18
0800C967: 60 01                         w1 -         $0x1
0800C969: 20 47                         w1 =:        b.0x1C
0800C96B: 4A 48                         w stz        b.0x20
0800C96D: 44 D0                         w test       r1
0800C96F: CE 11                         if <= go     $0x11
0800C971: 1A CD 20 49                   w move       $0x20,b.0x24
0800C975: C3 08 00 C9 D2 02 45 49       call         $0x800C9D2,$0x2,b.0x14,b.0x24
0800C97D: 9D                            ifkret
0800C97E: 4F 48                         w incr       b.0x20
0800C980: 44 47                         w test       b.0x1C
0800C982: C4 15                         if = go      $0x15
0800C984: 1A CD 2A 49                   w move       $0x2A,b.0x24
0800C988: C3 08 00 C9 D2 02 45 49       call         $0x800C9D2,$0x2,b.0x14,b.0x24
0800C990: 9D                            ifkret
0800C991: 4F 48                         w incr       b.0x20
0800C993: 51 47                         w decr       b.0x1C
0800C995: C0 EB                         go           $0xFFFFFFFFFFFFFFEB
0800C997: 0C 48                         w1 :=        b.0x20
0800C999: 80                            ret
0800C99A: B8 CF 00 00 00 3C             ents         $0x3C
0800C9A0: 4A 4A                         w stz        b.0x28
0800C9A2: 0C 47                         w1 :=        b.0x1C
0800C9A4: 20 4E                         w1 =:        b.0x38
0800C9A6: 2E 48 D0                      w comp2      b.0x20,r1
0800C9A9: C8 26                         if > go      $0x26
0800C9AB: 0D 48                         w2 :=        b.0x20
0800C9AD: 04 E5 14                      by1 :=       @b.0x14+
0800C9B0: FC 90 CD 7F                   by1 and      $0x7F
0800C9B4: 1C 4D                         by1 =:       b.0x34
0800C9B6: 30 CD 30                      by1 comp     $0x30
0800C9B9: D8 16                         if << go     $0x16
0800C9BB: 30 CD 39                      by1 comp     $0x39
0800C9BE: D4 11                         if >> go     $0x11
0800C9C0: 0E 4A                         w3 :=        b.0x28
0800C9C2: 6E 0A                         w3 *         $0xA
0800C9C4: FC 90 0F                      by1 and      $0xF
0800C9C7: 54 D2                         w1 +         r3
0800C9C9: 20 4A                         w1 =:        b.0x28
0800C9CB: BF 48 4E E0                   d loopi      b.0x20,b.0x38,$0xFFFFFFFFFFFFFFE0
0800C9CF: 0C 4A                         w1 :=        b.0x28
0800C9D1: 80                            ret
0800C9D2: B8 CF 00 00 00 1C             ents         $0x1C
0800C9D8: 0C C5 14                      w1 :=        @b.0x14
0800C9DB: 18 42                         r:=          b.0x8
0800C9DD: 20 85                         w1 =:        r.0x14
0800C9DF: 0C C5 18                      w1 :=        @b.0x18
0800C9E2: C3 08 00 CA 3F 00             call         $0x800CA3F,$0x0
0800C9E8: 9D                            ifkret
0800C9E9: 80                            ret
0800C9EA: B8 CF 00 00 00 1C             ents         $0x1C
0800C9F0: 0C C5 14                      w1 :=        @b.0x14
0800C9F3: 18 42                         r:=          b.0x8
0800C9F5: 20 85                         w1 =:        r.0x14
0800C9F7: C3 08 00 CA 2A 00             call         $0x800CA2A,$0x0
0800C9FD: 9D                            ifkret
0800C9FE: 20 C5 18                      w1 =:        @b.0x18
0800CA01: 0D C5 18                      w2 :=        @b.0x18
0800CA04: E5 CD 7F                      w2 and       $0x7F
0800CA07: 21 C5 18                      w2 =:        @b.0x18
0800CA0A: 80                            ret
0800CA0B: 9C                            entd
0800CA0C: 44 D1                         w test       r2
0800CA0E: C4 19                         if = go      $0x19
0800CA10: 44 F5 00                      w test       r2.(0x0)
0800CA13: C4 11                         if = go      $0x11
0800CA15: 0D F5 00                      w2 :=        r2.(0x0)
0800CA18: FD 6D F5 00 D2 CF 7F FF FF FF w2 chain     r2.(0x0),r3,$0x7FFFFFFF
0800CA22: 55 D2                         w2 +         r3
0800CA24: 20 F5 00                      w1 =:        r2.(0x0)
0800CA27: FE 03                         clrk
0800CA29: 82                            retd
0800CA2A: B8 CF 00 00 00 20             ents         $0x20
0800CA30: C3 F8 00 00 01 02 45 47       call         $0xFFFFFFFFF8000001,$0x2,b.0x14,b.0x1C ; MON 1B INBT
0800CA38: 9D                            ifkret
0800CA39: 0C 47                         w1 :=        b.0x1C
0800CA3B: 80                            ret
0800CA3C: 04 47                         by1 :=       b.0x1C
0800CA3E: 80                            ret
0800CA3F: B8 CF 00 00 00 20             ents         $0x20
0800CA45: 20 46                         w1 =:        b.0x18
0800CA47: 20 47                         w1 =:        b.0x1C
0800CA49: C3 F8 00 00 02 02 45 47       call         $0xFFFFFFFFF8000002,$0x2,b.0x14,b.0x1C ; MON 2B OUTBT
0800CA51: 9D                            ifkret
0800CA52: 80                            ret
0800CA53: B8 CF 00 00 00 18             ents         $0x18
0800CA59: C3 F8 00 00 23 01 45          call         $0xFFFFFFFFF8000023,$0x1,b.0x14 ; MON 43B CLOSE
0800CA60: 9D                            ifkret
0800CA61: 80                            ret
0800CA62: B8 CF 00 00 00 F8             ents         $0xF8
0800CA68: 4A 7A                         w stz        b.0xE8
0800CA6A: 0C 46                         w1 :=        b.0x18
0800CA6C: 20 79                         w1 =:        b.0xE4
0800CA6E: 0D 47                         w2 :=        b.0x1C
0800CA70: 21 7C                         w2 =:        b.0xF0
0800CA72: 34 D1                         w1 comp      r2
0800CA74: C8 12                         if > go      $0x12
0800CA76: 0D 79                         w2 :=        b.0xE4
0800CA78: 04 E5 14                      by1 :=       @b.0x14+
0800CA7B: 0E 7A                         w3 :=        b.0xE8
0800CA7D: 1C D6 44                      by1 =:       b.0x44+
0800CA80: 4F 7A                         w incr       b.0xE8
0800CA82: BF 79 7C F4                   d loopi      b.0xE4,b.0xF0,$0xFFFFFFFFFFFFFFF4
0800CA86: 0C 7A                         w1 :=        b.0xE8
0800CA88: 19 CD 27 D4 44                by move      $0x27,b.0x44+
0800CA8D: 54 01                         w1 +         $0x1
0800CA8F: 20 4D                         w1 =:        b.0x34
0800CA91: 4A 7A                         w stz        b.0xE8
0800CA93: 0D 49                         w2 :=        b.0x24
0800CA95: 21 79                         w2 =:        b.0xE4
0800CA97: 0E 4A                         w3 :=        b.0x28
0800CA99: 22 7D                         w3 =:        b.0xF4
0800CA9B: 35 D2                         w2 comp      r3
0800CA9D: C8 12                         if > go      $0x12
0800CA9F: 0D 79                         w2 :=        b.0xE4
0800CAA1: 04 E5 20                      by1 :=       @b.0x20+
0800CAA4: 0E 7A                         w3 :=        b.0xE8
0800CAA6: 1C D6 DB                      by1 =:       b.0xFFFFFFFFFFFFFFDB+
0800CAA9: 4F 7A                         w incr       b.0xE8
0800CAAB: BF 79 7D F4                   d loopi      b.0xE4,b.0xF4,$0xFFFFFFFFFFFFFFF4
0800CAAF: 0C 7A                         w1 :=        b.0xE8
0800CAB1: 19 CD 27 D4 DB                by move      $0x27,b.0xFFFFFFFFFFFFFFDB+
0800CAB6: 54 01                         w1 +         $0x1
0800CAB8: 20 4F                         w1 =:        b.0x3C
0800CABA: 85                            bi2 clr
0800CABB: FE 25 D5 44                   by2 laddr    b.0x44+
0800CABF: 21 4E                         w2 =:        b.0x38
0800CAC1: 86                            bi3 clr
0800CAC2: FE 26 D6 DB                   by3 laddr    b.0xFFFFFFFFFFFFFFDB+
0800CAC6: 22 50                         w3 =:        b.0x40
0800CAC8: 4A 7B                         w stz        b.0xEC
0800CACA: C3 F8 00 00 28 04 7B 4B 4D 4F call         $0xFFFFFFFFF8000028,$0x4,b.0xEC,b.0x2C,b.0x34,b.0x3C ; MON 50B OPEN
0800CAD4: 9D                            ifkret
0800CAD5: 0C 7B                         w1 :=        b.0xEC
0800CAD7: 80                            ret
0800CAD8: B8 CF 00 00 00 20             ents         $0x20
0800CADE: C3 F8 00 00 32 02 45 47       call         $0xFFFFFFFFF8000032,$0x2,b.0x14,b.0x1C ; MON 62B RMAX
0800CAE6: 9D                            ifkret
0800CAE7: 0C 47                         w1 :=        b.0x1C
0800CAE9: 80                            ret
0800CAEA: 0C 47                         w1 :=        b.0x1C
0800CAEC: 80                            ret
0800CAED: B8 CF 00 00 00 20             ents         $0x20
0800CAF3: 20 46                         w1 =:        b.0x18
0800CAF5: 20 47                         w1 =:        b.0x1C
0800CAF7: C3 F8 00 00 3B 02 45 47       call         $0xFFFFFFFFF800003B,$0x2,b.0x14,b.0x1C ; MON 73B SMAX
0800CAFF: 9D                            ifkret
0800CB00: 80                            ret
0800CB01: B8 CF 00 00 00 20             ents         $0x20
0800CB07: 20 46                         w1 =:        b.0x18
0800CB09: 20 47                         w1 =:        b.0x1C
0800CB0B: 0C 47                         w1 :=        b.0x1C
0800CB0D: FC AD D0 01                   w sha        r1,$0x1
0800CB11: 20 47                         w1 =:        b.0x1C
0800CB13: C3 F8 00 00 3E 02 45 47       call         $0xFFFFFFFFF800003E,$0x2,b.0x14,b.0x1C ; MON 76B SETBS
0800CB1B: 9D                            ifkret
0800CB1C: 80                            ret
0800CB1D: B8 CF 00 00 00 38             ents         $0x38
0800CB23: 0C 48                         w1 :=        b.0x20
0800CB25: FD 3D E4 1C                   w2 laddr     @b.0x1C+
0800CB29: 21 4C                         w2 =:        b.0x30
0800CB2B: 0E 4B                         w3 :=        b.0x2C
0800CB2D: FC AD D2 01                   w sha        r3,$0x1
0800CB31: 22 4D                         w3 =:        b.0x34
0800CB33: C3 F8 00 00 4F 05 45 46 C5 30 4A 4D       call         $0xFFFFFFFFF800004F,$0x5,b.0x14,b.0x18,@b.0x30,b.0x28,b.0x34 ; MON 117B RFILE
0800CB3F: 9D                            ifkret
0800CB40: 80                            ret
0800CB41: B8 CF 00 00 00 38             ents         $0x38
0800CB47: 0C 48                         w1 :=        b.0x20
0800CB49: FD 3D E4 1C                   w2 laddr     @b.0x1C+
0800CB4D: 21 4C                         w2 =:        b.0x30
0800CB4F: 0E 4B                         w3 :=        b.0x2C
0800CB51: FC AD D2 01                   w sha        r3,$0x1
0800CB55: 22 4D                         w3 =:        b.0x34
0800CB57: C3 F8 00 00 50 05 45 46 C5 30 4A 4D       call         $0xFFFFFFFFF8000050,$0x5,b.0x14,b.0x18,@b.0x30,b.0x28,b.0x34 ; MON 120B WFILE
0800CB63: 9D                            ifkret
0800CB64: 80                            ret
0800CB65: 00                    ??? ; opcode 0x0000
