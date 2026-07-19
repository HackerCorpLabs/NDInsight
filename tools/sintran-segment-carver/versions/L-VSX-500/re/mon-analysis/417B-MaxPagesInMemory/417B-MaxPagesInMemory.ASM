; ================================================================
; MON 417B (MXPISG) - MaxPagesInMemory
; ND-500 System Monitor handler - REAL SINTRAN L bytes
; ================================================================
; This is an ND-500 (32-bit) call. It is NOT dispatched through the
; ND-100 GOTAB; it is routed by the SINTRAN in-memory ND-500 System
; Monitor S3SM5 via its 0x60 MON vector table.
;
; REGION: 417B handler entry (fix-family shared body, mid-block entry)
;   Segment      : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
;                  canonical bin: ../../../segments/030-S3SM5.bin
;                  runtime load base: word 40000B
;   Code type    : ND-500 (32-bit), as carved
;   Dispatch     : 0x60 MON vector table; slot file off = 0x60 + 2*417B = 0x027e
;                  vector value (16-bit BE) at 0x027e = 0xbdf6  (handler entry)
;                  next vector (420B) at 0x0280       = 0xbe0f  (window close)
;   Entry (file) : 0xbdf6   End (next entry): 0xbe0f   Length: 25 bytes
;   Tool         : nd500-dis -a -noansi -s bdf6 ../../../segments/030-S3SM5.bin
;
; NOTE: 0xbdf6 is a MID-BLOCK entry into the packed 400B..421B fix-family
;       handler group. nd500-dis therefore starts inside an instruction
;       stream whose true alignment is fixed by the caller, not by this
;       entry point. The leading bytes decode as bare opcodes (F2, 01).
;       The raw BYTES are ground truth; the decoded MNEMONICS are LOW
;       confidence and the last line reads past 0xbe0f into the 420B body.
; ================================================================

0000BDF6: F2                    ??? ; opcode 0x00F2      <- vector entry point (417B)
0000BDF7: 01                    ??? ; opcode 0x0001
0000BDF8: BA 04 C9 00                   entsn        $0x4,r.0x0    ; looks like frame/enter
0000BDFC: AA 03 6B                      w3 mulad     $0x3,b.0xAC
0000BDFF: AD 6C A6 6A                   w2 lind      b.0xB0,r.0x98,b.0xA8  ; load descriptor field
0000BE03: DA 2C                         if <<= go    $0x2C         ; conditional (range check?)
0000BE05: 40 B3 04                      d comp2      r.0xCC,$0x4   ; compare against literal 4
0000BE08: F2                    ??? ; opcode 0x00F2
0000BE09: 08 C7 0E A8 03 F1             h1 :=        @b.0xEA803F1  ; spills past 0xbe0f into 420B
