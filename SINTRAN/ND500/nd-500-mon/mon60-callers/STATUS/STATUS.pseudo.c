/* ===========================================================================
 *  STATUS   ->  MON 60 subfunction RSTAT = 41B (0x21 = 33 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : STANDALONE ENTER-routine at 127551 (framesize 000010).
 *  NPL purpose of 41B: READ ND-500 INTERFACE STATUS. yaml: RSTAT
 *  "Read communication status", params <status> <MAR>.
 *  Return convention (PROVEN): callsite+1 = ERROR (LEAVE-value), callsite+2 =
 *  SUCCESS (fall-through). Routine ends with LEAVE-SKIP (177335) at 127732.
 * ===========================================================================
 */

extern int   MON60_RSTAT(void);    /* JPL I ->127734; ptr=thunk 146461; SAA 41 */
extern void  put_text(word idx);   /* helper 000052: emit a message-table string */
extern void  put_val(word v);      /* helper 122441 / 122405: emit a number/value */
extern void  leave_val(int err);   /* 177327: error return, error code in A */
extern word *p;                    /* gateway outgoing param slots */

void cmd_status(void)
{
    word status[2];   /* @B-171 : bits 16:31 = ND-500, bits 0:15 = ND-100 (per yaml) */
    word mar;         /* @B-167 : MAR (memory address register)                      */
    word extra;       /* @B-165 : third returned word (identity UNKNOWN, see below)  */

    /* --- marshal three output addresses, then read status (127554..127566) - */
    p[6]  = (word)&status[0];       /* 127554 RADD SB DA;127555 AAA -171;127557 STA ,X 6 */
    p[7]  = (word)&mar;             /* 127560 RADD SB DA;127561 AAA -167;127562 STA ,X 7 */
    p[10] = (word)&extra;           /* 127563 RADD SB DA;127564 AAA -165;127565 STA ,X 10 */

    if (MON60_RSTAT() == ERROR)     /* 127566 JPL I 146 -> thunk 146461 (RSTAT 41B) */
        leave_val(/*errcode*/);     /* 127567 callsite+1 = ERROR -> 177327          */
    /* callsite+2 = SUCCESS falls through to formatting at 127570                 */

    /* --- format the ND-100 half of the status word, bit by bit -------------- */
    put_text(0144);                 /* 127570-127575 heading text (LDT 144 -> helper 000052) */
    put_val(status_lo(status));     /* 127577-127602 emit the raw ND-100 status value       */

    for (i = 0; i < 011; i++) {     /* 127604-127634 loop, SAA 5 base, SAT 11 limit          */
        /* test bit i of the ND-100 status via SHA/AND/EXR; if set, print the    */
        /* corresponding message-table string (127616-127625 -> helper 000052).  */
    }

    /* --- format the ND-500 half of the status word (second loop) ------------ */
    put_text(0106);                 /* 127635-127642 second heading                          */
    put_val(status_hi(status));     /* 127644-127650                                         */
    for (j = 0; j < 017; j++) {     /* 127651-127701 loop, SAA 12 base, SAT 17 limit         */
        /* same bit-decode-and-print for the ND-500 status bits.                 */
    }

    /* --- emit MAR and the third word --------------------------------------- */
    put_text(046);                  /* 127702-127710 */
    put_val(mar);                   /* 127711-127715 (helper 122405) */
    put_text(034);                  /* 127716-127724 */
    put_val(extra);                 /* 127725-127731 */

    return /*SKIP*/;                /* 127732 JPL I 23 -> 177335 LEAVE-SKIP (success) */
}
