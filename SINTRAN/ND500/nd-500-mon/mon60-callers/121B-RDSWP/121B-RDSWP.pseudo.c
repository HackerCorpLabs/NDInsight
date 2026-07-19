/* ===========================================================================
 *  RDSWP   ->  MON 60 subfunction RDSWP = 121B (0x51 = 81 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Three call sites in three different ENTER-routines; all marshal FOUR
 *  parameters (slots 6,7,10,11).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_RDSWP(void);    /* thunk 146665 SAA 121 (all three sites)      */
extern word *p;                   /* gateway outgoing slots p[6..11] */

/* ---- call site 1 @073152 (routine 073115, framesize 000336) --------------- */
void rdswp_site1(void)
{
    p[ 6] = pooled_073314;           /* 073137 LDA 155 ; 073141 STA ,X 6         */
    p[ 7] = pooled_073315;           /* 073142 LDA 153 ; 073143 STA ,X 7         */
    p[010] = (word)&local_B122;      /* 073144..073146 : &local(B-122)           */
    p[011] = (word)&local_B130;      /* 073147..073151 : &local(B-130)           */
    if (MON60_RDSWP() == ERROR)      /* 073152 -> thunk 146665                    */
        return LEAVE_value();        /* 073153 -> ptr 073313 = 177327             */
    /* 073154 success: continues in-line */
}

/* ---- call site 2 @074310 (routine 074267, framesize 000007) --------------- */
void rdswp_site2(void)
{
    p[ 6] = (word)&local_B172;       /* 074274..074277                            */
    p[ 7] = (word)&local_B170;       /* 074300..074302                            */
    p[010] = local_B166;             /* 074303..074304 : value                    */
    p[011] = (word)&local_B165;      /* 074305..074307                            */
    if (MON60_RDSWP() == ERROR)      /* 074310 -> thunk 146665                    */
        return LEAVE_value();        /* 074311 -> ptr 074333 = 177327             */
    /* 074312 success -> 074327 */
}

/* ---- call site 3 @107515 (routine 103722, framesize 000605) --------------- */
void rdswp_site3(void)
{
    p[ 6] = pooled_107677;           /* 107504 LDA 173 ; 107506 STA ,X 6          */
    p[ 7] = pooled_107700;           /* 107507 LDA 171 ; 107510 STA ,X 7          */
    p[010] = local_B144;             /* 107511..107512 : value                    */
    p[011] = pooled_107701;          /* 107513 LDA 166 ; 107514 STA ,X 11         */
    if (MON60_RDSWP() == ERROR)      /* 107515 -> thunk 146665                    */
        goto err_107361;             /* 107516 JMP I ,B -135 (frame-relative)     */
    /* 107517 success: continues in-line */
}

/* PROVEN: all three sites store exactly FOUR MON60 params (slots 6,7,10,11).
 *   Mix of pooled constants and pointers to frame locals.
 * INFERRED: per SUBFUNCTION-TABLE.md, RDSWP 121B = "READ FROM SWAPPERS DATA
 *   MEMORY (LOGICAL ADDRS)" (handler 5NOPAR).  The four params are, by the
 *   read shape, plausibly {logical address, length/count, source descriptor,
 *   destination buffer}; the exact field roles were NOT traced. */
