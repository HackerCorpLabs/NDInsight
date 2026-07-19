/* ===========================================================================
 *  RFLAG   ->  MON 60 subfunction 100B = 0x40 = 64 dec
 * ---------------------------------------------------------------------------
 *  Purpose : READ FLAGS FROM ND-500 DATA SEGMENT   (server handler RRFLAG)
 *  Call site 005264, inside the main command interpreter routine 002662
 *  (framesize 000331).
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return polarity (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int   MON60_RFLAG(void);   /* JPL I 165 @005264 -> ptr 005451 = thunk 146616 (SAA 100) */
extern void  cmd_error_002673(void);
extern word *p;

void cmd_read_flags(void)
{
    dword arg1;                    /* B-125 - second parameter (role INFERRED) */
    dword flags = CONST_174;       /* B-127 - preloaded default; server writes flags here */
                                   /*   005253 LDD 174 ; 005254 STD ,B -127 */
    p[6] = (word)&arg1;            /* 005255..005260  &(B-125) */
    p[7] = (word)&flags;           /* 005261..005263  &(B-127) */

    if (MON60_RFLAG() == ERROR)    /* 005264 JPL I 165 -> thunk 146616 (RFLAG 100B) */
        cmd_error_002673();        /* 005265 callsite+1 = ERROR -> 002673 */
    /* 005266 callsite+2 = SUCCESS: LDD ,B -127  (read the returned flags) */
}
