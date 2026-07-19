; ============================================================================
; MON 121B (octal) AwaitFileTransfer - real SINTRAN L disassembly
;
; One labelled region only:
;   F1657 GOTAB stub fragment        F1657 @122013B  in segment 025-S3IRPIT (load 32000B)
;      GOTAB[121] = 122013B (byte-proven, non-zero) points here. F1657 is part of
;      the F16xx resident GOTAB-stub cluster (siblings F1656=117B ReadFromFile,
;      F1660=123B ReleaseResource). It is real bytes but a FRAGMENT bounded by the
;      next symbol BDPUT=122017B (4 words), and it holds no static pointer to a
;      transfer-wait worker - so the stub -> worker hop crosses the uncarved
;      resident CALLPROC.
;
; The AwaitFileTransfer worker (manual short name WAITF) is NOT present in any
; carved segment: no WAITF symbol exists in 006-S3FS (FILSYS-SYMBOLS) or in
; 025-S3IRPIT (SYMBOL-2-LIST), and it is not reachable from these bytes. The
; worker body is therefore NOT byte-recoverable here; only the documented
; behaviour is modelled in the .pseudo.c (flagged UNVERIFIED). Addresses octal.
; ============================================================================

; ---- F1657 GOTAB stub fragment (025-S3IRPIT, load 32000B) ----
; The four words below sit in the F16xx GOTAB-stub cluster; at this alignment
; nd100-dis renders them as FDV forms. They are resident dispatch-cluster bytes
; (pointer / fragment), not a followable path to the WAITF worker.
122013  116610  	FDV ,X ,B -170
122014  116036  	FDV ,X 36
122015  115720  	FDV I ,B -60
122016  116001  	FDV ,X 1
