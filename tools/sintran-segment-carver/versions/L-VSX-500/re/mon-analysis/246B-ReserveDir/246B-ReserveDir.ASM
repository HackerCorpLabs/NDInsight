;=============================================================================
; MON 246B (octal) ReserveDir (RESDI) - real SINTRAN L disassembly
;
; Reserves a directory for special use. The directory must be entered; all files in
; it must be closed; other users cannot open files on a reserved directory. Release
; it with ReleaseDir (MON 247B). User RT and user SYSTEM.
;
; MON 246B is a FALL-THROUGH call: GOTAB[246] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   RESDI/RELDI reserve-directory worker  RESDI @107401B in segment 006-S3FS (load 26000B)
;      Window 107401B..107433B = 27 words, bounded strictly to the next symbol
;      500RD=107434B. Words 107401B..107426B are code; 107427B is a one-word ROP
;      NOOP pad; 107430B..107433B are a pointer table (link cells, DATA).
;   (the fall-through -> RESDI bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; This is a SHARED reserve/release body with two entries selected by the SSK skip
; flag: RESDI=107401B clears SSK (reserve, MON 246B); the sibling RELDI=107403B sets
; SSK (release, MON 247B). Both join the common body at 107404B and 107413B
; (BSKP ONE SSK) selects the reserve vs release resident worker call. RESDI/RELDI are
; FILSYS-SYMBOLS symbols in the file-system segment 006-S3FS.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- RESDI/RELDI reserve/release-directory worker (006-S3FS, load 26000B) ----
107401  174020  	BSET ZRO SSK	; RESDI entry (ReserveDir): clear skip flag (SSK = 0)
107402  124002  	JMP 2		; -> 107404
107403  174220  	BSET ONE SSK	; RELDI entry (ReleaseDir, MON 247B): set skip flag (SSK = 1)
107404  021023  	STD I 23	; store A/D pair via [link cell] - save incoming
107405  146145  	RADD CLD SL DA	; A = L  (copy return link)
107406  146131  	RADD CLD SB DD	; D = B  (copy frame pointer)
107407  170006  	SAB 6		; B = 6
107410  135020  	JPL I 20	; -> [107430]  call resident prologue worker (find directory)
107411  054405  	LDX ,B 5	; X = mem[B+5]  (directory datafield address)
107412  056031  	LDX ,X 31	; X = mem[X+31]
107413  175220  	BSKP ONE SSK	; skip next if SSK set (release path)
107414  124004  	JMP 4		; -> 107420  (reserve path)
107415  135014  	JPL I 14	; -> [107431]  call resident worker (release directory)
107416  124007  	JMP 7		; -> 107425  (error tail)
107417  124003  	JMP 3		; -> 107422  (success)
107420  135012  	JPL I 12	; -> [107432]  call resident worker (reserve directory)
107421  124004  	JMP 4		; -> 107425  (error tail)
107422  040404  	MIN ,B 4	; mem[B+4]++ ; skip next if it wraps to 0 (success advance)
107423  170772  	SAA -6		; A = -6   (error code)
107424  125007  	JMP I 7		; -> [107433]  indirect return
107425  004402  	STA ,B 2	; mem[B+2] = A  (error tail: store error code)
107426  124375  	JMP -3		; -> 107423
107427  147417  	ROP NOOP	; one-word pad (closes the code region)
; ---- link cells (DATA, not code) 107430..107433 ----
107430  003752  	STZ I ,B ,X -26	; pointer word (read by JPL I 20 at 107410)
107431  131260  	JAZ -120	; pointer word (read by JPL I 14 at 107415)
107432  131256  	JAZ -122	; pointer word (read by JPL I 12 at 107420)
107433  003776  	STZ I ,B ,X -2	; pointer word (read by JMP I 7 at 107424)
; 107434 = 500RD boundary (next symbol); not part of this worker.
