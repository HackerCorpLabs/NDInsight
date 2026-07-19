; ================================================================
; GENERATED single-region slice of ../../../segments/030-S3SM5.bin
;   (canonical; whole-segment disassembly in ../../segments-ref/030-S3SM5/)
;   at byte-offset 43165 (0xA89D) length 17 bytes.
;   Regenerate from canonical - do NOT hand-edit.
;   This body is a single contiguous ND-500 region.
; ================================================================
; MON 266B (octal) ND500MagTape (500MT) - real SINTRAN L machine code
; ND-500 System Monitor handler carved from the S3SM5 segment image.
; ================================================================
; Source segment : 030-S3SM5.bin (SINTRAN in-memory ND-500 SEG image, VSX-500 / L)
; Code type      : ND-500 (32-bit), as carved
; Dispatch       : 0x60 MON vector table, slot = octal MON number
;                  vector slot file offset = 0x60 + 2*decimal(266B) = 0x60 + 2*182 = 0x01CC
;                  vector value (16-bit)  = 0xA89D  (handler file offset)
; Entry (file off): 0xA89D   End (next entry 270B): 0xA8AE   Length: 17 bytes
;   This slot sits inside the contiguous 260B-277B block of real handler offsets
;   in the S3SM5 vector table, so the ROUTING is byte-verified. The region is
;   SHORT (17 bytes to the next non-zero slot 270B=0xA8AE) - consistent with a
;   small dispatch/branch stub into a shared magtape body rather than a full
;   routine.
;   COHERENCE: the linear decode from the raw vector offset is only PARTLY
;   coherent, so 0xA89D is not a proven instruction boundary. The RAW BYTES are
;   ground truth; the mnemonics below are unreliable where marked.
; Tool           : nd500-dis -a -noansi -s a89d
; Addresses are hex file byte offsets into 030-S3SM5.bin.
; ================================================================

0000A89D: 39 AA                         f2 comp      r.0xA8
0000A89F: 03                            noop
0000A8A0: 6B F1 82                      d4 -         DESC(r2) r.0x8
0000A8A3: 48 6A                         by stz       b.0xA8
0000A8A5: D9 2C 40                      if << go     $0x2C40        ; UNVERIFIED target (misalignment)
0000A8A8: C0 05                         go           $0x5
0000A8AA: AA 05 BA                      w3 mulad     $0x5,r.0xE8    ; last full op inside the 17-byte window
; (0xA8AD onward belongs to the 270B handler at 0xA8AE)
