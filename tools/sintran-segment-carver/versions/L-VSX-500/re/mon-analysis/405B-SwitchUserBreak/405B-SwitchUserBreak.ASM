; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 47665 (0xBA31) length 59 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a single contiguous ND-500 region (routing proven).
; ================================================================
; MON 405B (USTRK) - 405B-SwitchUserBreak
; ND-500 System Monitor handler - real SINTRAN L machine code
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), as carved from the native segment
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*405B = 0x026A
;                  vector value (16-bit)     = 0xBA31  (points here directly)
; Entry (file off): 0xBA31   End (next slot 406B): 0xBA6C   Length: 59 bytes
;   NOTE: the 0x60 vector value 0xBA31 points AT this offset directly - no +5
;         correction is needed (unlike 410B). BUT the stream does NOT decode as
;         a clean ND-500 subroutine: the first op is 'w1 comp' (not an ENT*),
;         and byte 0xBA4C is an undecodable opcode (0x00F2) - both signal that
;         the true instruction alignment inside this window is NOT proven.
;         The RAW BYTES are ground truth; the mnemonics below are a best-effort
;         aligned reading only, and are UNVERIFIED past the first few bytes.
; Tool           : nd500-dis -a -noansi -s ba31
; ================================================================

0000BA31: 34 BA                         w1 comp      r.0xE8
0000BA33: 1F C9 00                      by4 =:       r.0x0
0000BA36: 29 34                         d2 =:        $0x34
0000BA38: 59 C9 24                      f2 +         r.0x24
0000BA3B: 54 A8                         w1 +         r.0xA0
0000BA3D: 0B F2 29                      h4 :=        DESC(r3) $0x29
0000BA40: C0 35                         go           $0x35
0000BA42: A8 08 49                      w1 mulad     $0x8,b.0x24
0000BA45: 04 B3                         by1 :=       r.0xCC
0000BA47: 06 59                         by3 :=       b.0x64
0000BA49: C9 4C F1                      if > go      $0x4CF1
0000BA4C: F2                    ??? ; opcode 0x00F2  (undecodable - misalignment marker)
0000BA4D: 04 BA                         by1 :=       r.0xE8
0000BA4F: 10 C9 00                      f1 :=        r.0x0
0000BA52: BA 10 49                      entsn        $0x10,b.0x24
0000BA55: 04 B2                         by1 :=       r.0xC8
0000BA57: 04 49                         by1 :=       b.0x24
0000BA59: C1 09 05                      go           $0x905
0000BA5C: AA 0C 01                      w3 mulad     $0xC,$0x1
0000BA5F: 0F 49                         w4 :=        b.0x24
0000BA61: C1 C0 05                      go           $0xFFFFFFFFFFFFC005
0000BA64: AA 09 AA                      w3 mulad     $0x9,r.0xA8
; --- next 0x60 vector slot (406B) begins at 0xBA6C ---
