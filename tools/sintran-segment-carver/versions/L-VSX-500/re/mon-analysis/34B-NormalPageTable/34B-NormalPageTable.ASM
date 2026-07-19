; ============================================================================
; MON 034B (octal) NormalPageTable (ALTOF) - real SINTRAN L disassembly
; Two regions (the dispatch is a fall-through; see README Code location):
;   A. GOTAB[034] level-14 dispatch word @071267B in resident SINTRAN-DATA_commoncode (load 0)
;      = 000000 -> FALL-THROUGH: NO F16xx code stub; the level-14 default path
;        (resident MFELL / CALLPROC) selects the worker. That bridge is uncarved,
;        so there is no stub to list here - only the data word.
;   B. ALTOF page-table-reset worker  ALTOF @004116B in resident SINTRAN-DATA_commoncode (load 0)
;      (documented short-name ALTOFF; real code that rewrites the paging-control
;       register so the alternative page table equals the normal page table)
;      (the A->B link is the uncarved resident MFELL/CALLPROC - not in these bytes)
; Addresses and values are octal. Disassembled from the canonical segment bytes.
; ============================================================================

; ---- Region A: GOTAB[034] dispatch word (resident SINTRAN-DATA_commoncode) ----
; GOTAB base 071233B + 034 = 071267B. Byte offset 58734 dec. Raw = 00 00.
071267  000000  	; GOTAB[034] = 000000  (FALL-THROUGH; no direct handler word)

; ---- Region B: ALTOF page-table-reset worker (resident SINTRAN-DATA_commoncode) ----
; ALTOF @004116B. Byte offset 4252 dec. Body 004116B..004133B (14 words); it shares
; the constant pool 004134B..004140B with ALTON (MON 033B) directly above it.
004116  150401  	IOF			; interrupts off (privileged)
004117  004354  	STA -24			; save A to scratch 004073B
004120  014355  	STX -23			; save X to scratch 004075B
004121  170410  	SAA 10			; A := 10B  (level selector for the PGC read)
004122  150014  	TRA PGC			; A = PGC : read the paging-control for that level
004123  070014  	AND 14			; A &= mem[004137B] = 074000B  (keep frame, drop PT-select)
004124  074014  	ORA 14			; A |= mem[004140B] = 001616B  (normal-table select bits)
004125  150103  	TRR PCR			; PCR = A : alternative page table := normal page table
004126  055007  	LDX I 7			; X = mem[ mem[004135B] ] = mem[004007B]  (PCR-image ptr)
004127  006017  	STA ,X 17		; mem[X+17] = A : store updated PCR image
004130  044343  	LDA -35			; restore A from 004073B
004131  054344  	LDX -34			; restore X from 004075B
004132  150402  	ION			; interrupts on
004133  146142  	EXIT			; P = L  (return)
; ---- Shared constant pool 004134B..004140B (DATA; used as P-relative masks) ----
;   004134=003600 004135=004007(PCR-image ptr) 004136=074177 004137=074000 004140=001616
004134  003600  	; DATA mask 003600B  (used by ALTON, MON 033B)
004135  004007  	; DATA ptr  004007B  (PCR-image table pointer)
004136  074177  	; DATA mask 074177B  (used by ALTON, MON 033B)
004137  074000  	; DATA mask 074000B
004140  001616  	; DATA const 001616B (normal-table select bits)
; ---- end ----
