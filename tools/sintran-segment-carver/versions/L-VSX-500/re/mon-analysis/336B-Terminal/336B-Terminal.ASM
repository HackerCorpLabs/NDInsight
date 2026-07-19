;=============================================================================
; MON 336B (octal) Terminal (IOMTY) - real SINTRAN L disassembly
;
; The I/O multifunction monitor call: changes the attributes of terminal and
; terminal-access-device (TAD) I/O, and configures NET/One interfaces and SCSI
; disks. It takes a varying number of input/output parameters (per function code)
; passed in an array.
;
; MON 336B is a FALL-THROUGH call: GOTAB[336] = 000000 (byte-proven), so there is
; NO per-call level-14 entry stub. Dispatch drops into the resident MFELL/CALLPROC
; second-level path (uncarved) which reaches the worker below. One labelled region:
;   IOMTY I/O-multifunction worker body   IOMTY @51745B in resident SINTRAN-DATA_commoncode (load 0)
;      Window 51745B..51762B = 14 words, bounded strictly to the next symbol
;      MBFDI=51763B. All 14 words are code: an alternating sequence of resident-
;      worker calls (JPL I) and descriptor loads (LDD ,B ...). The JPL I link
;      cells (052021/052023) lie past this window in the following region.
;   (the fall-through -> IOMTY bridge is the uncarved resident CALLPROC - not in
;    these bytes; see README Honest caveats)
;
; IOMTY is a SYMBOL-1-LIST symbol in the resident commoncode image.
; Addresses and values are octal. Bytes are from the real SINTRAN L segment binaries.
;=============================================================================

; ---- IOMTY I/O-multifunction worker (SINTRAN-DATA_commoncode, load 0) ----
051745  135054  	JPL I 54		; -> [052021]  call resident I/O worker
051746  024663  	LDD ,B -115		; A = mem[B-115]; D = mem[B-114]  (param descriptor)
051747  135052  	JPL I 52		; -> [052021]  call resident I/O worker
051750  024653  	LDD ,B -125		; A = mem[B-125]; D = mem[B-124]  (param descriptor)
051751  135052  	JPL I 52		; -> [052023]  call resident I/O worker
051752  024645  	LDD ,B -133		; A = mem[B-133]; D = mem[B-132]  (param descriptor)
051753  135050  	JPL I 50		; -> [052023]  call resident I/O worker
051754  024675  	LDD ,B -103		; A = mem[B-103]; D = mem[B-102]  (param descriptor)
051755  135046  	JPL I 46		; -> [052023]  call resident I/O worker
051756  024635  	LDD ,B -143		; A = mem[B-143]; D = mem[B-142]  (param descriptor)
051757  135044  	JPL I 44		; -> [052023]  call resident I/O worker
051760  024661  	LDD ,B -117		; A = mem[B-117]; D = mem[B-116]  (param descriptor)
051761  135042  	JPL I 42		; -> [052023]  call resident I/O worker
051762  024651  	LDD ,B -127		; A = mem[B-127]; D = mem[B-126]  (param descriptor)
; 51763 = MBFDI boundary (next symbol); not part of this worker.
