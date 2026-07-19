;=============================================================================
; MON 263B (octal) GetDeviceType - real SINTRAN L disassembly
;
; MON 263B is a FALL-THROUGH call: GOTAB[263] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   GDEVT get-device-type worker body   GDEVT @107104B  in segment 025-S3IRPIT (load 32000B)
;      Window 107104B..107130B = 21 words, bounded strictly to the next symbol
;      X21CL=107131B.
;
; GDEVT=107104B is a SYMBOL-2-LIST symbol; 025-S3IRPIT is the SYMBOL-2-LIST code
; overlay (its companion MON-32 worker MSG is real code here, garbage in the
; 026-S3IMPIT overlay), so 025-S3IRPIT holds the GDEVT bytes. The named worker is
; attached by symbol name (fall-through dispatch; see README Honest caveats).
;
; Several branches (107110/107114 -> X21CL=107131B, 107130 -> 107132B) leave this
; window into the ADJACENT X21CL routine - shared level-14 tail code, NOT part of
; the GetDeviceType body and NOT swallowed here (uncarved link; README caveats).
; Addresses and values are octal.
;=============================================================================

; ---- GDEVT get-device-type worker (025-S3IRPIT, load 32000B) ----
107104  135161  	JPL I 161		; -> 107265   GDEVT entry: JPL via link @107265 (resident worker)
107105  170404  	SAA 4		; SAA 4
107106  150307  	MST PIE		; MST PIE: enable interrupt-enable bits (A=4)
107107  045157  	LDA I 157		; A = device descriptor word (indirect)
107110  131021  	JAZ 21		; -> 107131   JAZ 21 -> 107131: if 0, exit to shared X21CL tail (adjacent routine; uncarved link)
107111  044411  	LDA ,B 11		; A = B+11 field (device flag)
107112  171001  	SAT 1		; SAT 1
107113  140065  	SKP IF DA EQL ST		; skip if A == 1 (device present / type match)
107114  124015  	JMP 15		; -> 107131   JMP 15 -> 107131: exit to shared X21CL tail (adjacent routine; uncarved link)
107115  045365  	LDA I -13		; A = value (indirect -13)
107116  131403  	JAF 3		; -> 107121   JAF 3 -> 107121: branch if A != 0
107117  045362  	LDA I -16		; A = value (indirect -16)
107120  124010  	JMP 10		; -> 107130   JMP 10 -> 107130: to type-store tail
107121  055362  	LDX I -16		; X = value (indirect -16)
107122  044412  	LDA ,B 12		; A = B+12 field
107123  131403  	JAF 3		; -> 107126   JAF 3 -> 107126: branch if A != 0
107124  046026  	LDA ,X 26		; A = ,X 26 (device-type attribute)
107125  124003  	JMP 3		; -> 107130   JMP 3 -> 107130: to type-store tail
107126  056012  	LDX ,X 12		; X = ,X 12 (follow chain)
107127  046023  	LDA ,X 23		; A = ,X 23 (device-type attribute)
107130  124002  	JMP 2		; -> 107132   JMP 2 -> 107132: into shared X21CL tail (adjacent routine; uncarved link)
