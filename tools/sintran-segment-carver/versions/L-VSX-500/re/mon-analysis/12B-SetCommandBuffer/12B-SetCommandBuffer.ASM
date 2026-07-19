; ============================================================================
; MON 12B (octal) SetCommandBuffer (SETCM) - real SINTRAN L disassembly
; Transfers a string (up to 32 characters) into the command buffer - the buffer
; that holds the last command typed at the terminal (readable via logical device
; number 0). The parameter is fetched through the ALTERNATIVE page table.
;
; Dispatch reality (3-layer; see README Code location):
;   GOTAB[12B] = 000000  -> FALL-THROUGH (no direct GOTAB handler word); the
;   level-14 handler is reached via the resident MFELL/CALLPROC path, which is
;   NOT present in any carved segment (uncarved bridge).
;
;   There is NO ND-100 code worker for this call in the carved segments. The
;   manual short name SETCM resolves ONLY in L07 N500-SYMBOLS (SETCM=106214B) -
;   that is the ND-500-side companion, not the ND-100 body. The ND-100 command
;   buffer itself is the DATA area CBUF=170207B (L07 SYMBOL-2-LIST), which in
;   this L image is zero-filled. So no executable body can be read here; the
;   named regions are data (CBUF) or the ND-500 companion (SETCM).
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binary.
; ============================================================================

; ---- CBUF command-buffer DATA area (025-S3IRPIT, load 32000B) ----
; CBUF = 170207B (SYMBOL-2-LIST). This is the DATA destination of MON 12 - the
; command buffer, not code. In this L image it is zero (uninitialised at carve
; time). Shown as data; there is no worker to disassemble.
170207  000000  	.WORD 000000		; CBUF: command-buffer data area (zero in this L image)
;
; No entry-stub region exists: GOTAB[12] = 000000 is a resident fall-through,
; not a 025-S3IRPIT stub, so there is nothing to disassemble on the dispatch head.
; SETCM=106214B is the ND-500 companion (N500-SYMBOLS), outside the ND-100 body
; carved here (see README caveats).
