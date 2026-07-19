/* ===========================================================================
 *  DEFINF   ->  MON 60 subfunction DEFINF = 142B (0x62 = 98 dec)
 *  Purpose: (redefine default infant file).  Dispatch 5NOPAR.
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Six call sites; each marshals ONE parameter (slot 6) = a pooled word.
 *  Group A (ptr 003072) : inside command interpreter ENTER-routine 002662.
 *  Group B (ptr 030404) : inside ENTER-routine 030302 (framesize 000004).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_DEFINF(void);   /* thunk 146745 SAA 142 (all six sites)        */
extern word *p;                   /* gateway slots, p[6]=param1 */

/* ---- Group A : interpreter 002662 --------------------------------------- */
void definf_A_002723(void) {      /* also the local error/retry handler @002715 */
    p[6] = pooled_003071;          /* 002720 LDA 151 ; 002722 STA ,X 6           */
    if (MON60_DEFINF() == ERROR)   /* 002723 -> thunk 146745                     */
        return LEAVE_value();      /* 002724 -> ptr 003073 = 177327              */
    /* 002725 success continues */
}
void definf_A_003014(void) {
    p[6] = pooled_003106;          /* 003011 LDA 75 ; 003013 STA ,X 6            */
    if (MON60_DEFINF() == ERROR)   /* 003014 -> thunk 146745                     */
        goto handler_002715;       /* 003015 JPL -100 -> 002715                  */
    /* 003016 success continues */
}
void definf_A_003034(void) {
    p[6] = pooled_003071;          /* 003031 LDA 40 ; 003033 STA ,X 6            */
    if (MON60_DEFINF() == ERROR)   /* 003034 -> thunk 146745                     */
        goto handler_002715;       /* 003035 JPL -120 -> 002715                  */
    /* 003036 success continues */
}

/* ---- Group B : routine 030302 ------------------------------------------- */
void definf_B_030314(void) {
    p[6] = pooled_030403;          /* 030311 LDA 72 ; 030313 STA ,X 6           */
    if (MON60_DEFINF() == ERROR)   /* 030314 -> thunk 146745                     */
        return LEAVE_value();      /* 030315 -> ptr 030405 = 177327              */
    /* 030316 success continues */
}
void definf_B_030370(void) {
    p[6] = pooled_030417;          /* 030365 LDA 32 ; 030367 STA ,X 6           */
    if (MON60_DEFINF() == ERROR)   /* 030370 -> thunk 146745                     */
        goto handler_030306;       /* 030371 JPL -63 -> 030306                   */
    /* 030372 success -> 030420 */
}
void definf_B_030377(void) {
    p[6] = pooled_030403;          /* 030374 LDA 7 ; 030376 STA ,X 6            */
    if (MON60_DEFINF() == ERROR)   /* 030377 -> thunk 146745                     */
        goto handler_030306;       /* 030400 JPL -72 -> 030306                   */
    /* 030401 success -> 030421 */
}

/* PROVEN: all six sites store exactly ONE MON60 param (slot 6) = a pooled word
 *   loaded P-relative (EAs 003071/003106/003071/030403/030417/030403).
 * INFERRED: the pooled words are default-infant-file name/parameter constants;
 *   their contents were not traced.  Handler 5NOPAR = generic forward path. */
