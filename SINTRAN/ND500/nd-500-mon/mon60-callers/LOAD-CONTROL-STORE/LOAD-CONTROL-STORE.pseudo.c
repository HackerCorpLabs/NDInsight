/* ===========================================================================
 *  LOAD-CONTROL-STORE   ->  MON 60 subfunction LDCS = 37B (0x1F = 31 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : command case 006064..006116, INSIDE the command interpreter
 *            ENTER-routine that begins at 002662 (framesize 000331).
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): the caller does  LDX ,B -176  (X := stack top =
 *  base of the gateway frame) then  STx ,X 6/7/10 = MON 60 parameter 1/2/3.
 *  Return convention (PROVEN, prog.md sec 5.4): callsite+1 = ERROR (direct),
 *  callsite+2 = SUCCESS (skip).
 * ===========================================================================
 */

extern dword get_num_param(int which);   /* helper @002003 (own ENTER routine);   */
                                          /*   evaluates a command-line numeric arg */
                                          /*   -> returns 32-bit value in D. ROLE INFERRED */
extern int   MON60_LDCS(void);            /* JPL I ->006146; ptr=thunk 146445;      */
                                          /*   SAA 37 -> gateway 146244 -> MON 60 146256 */
extern void  cmd_error_002673(void);      /* internal error reporter (PROVEN target, role INFERRED) */
extern void  cmd_loop_010613(void);       /* return to the command loop (PROVEN target) */

/* The gateway's outgoing parameter slots, indexed as ,X <offset> from stack top */
extern word *p;                           /* p[6]=param1, p[7]=param2, p[10]=param3 */

void cmd_load_control_store(void)
{
    dword csaddr;          /* frame local @B+0105 - the <CS addr> value           */
    dword count;           /* frame local @B+0107 - the <no of words> value       */
    /* fpname : the <file name> descriptor already sits in the F-image @B-0113    */

    /* --- evaluate operand 1 -> CS address (006064..006073) ------------------ */
    csaddr = get_num_param(1);       /* 006064 SAA 1 ; 006065 JPL I 47 ; 006067 STD ,B 105 */
    p[6]   = (word)&csaddr;          /* 006070 RADD SB DA ;006071 AAA 105 ;006073 STA ,X 6 */

    /* --- evaluate operand 2 -> word count (006074..006111) ------------------ */
    count  = get_num_param(2);       /* 006077 SAA 2 ; 006100 JPL I 34 ; 006102 STD ,B 107 */
    p[7]   = (word)&count;           /* 006106 RADD SB DA ;006107 AAA 107 ;006111 STA ,X 7 */

    /* --- operand 3 -> file name (passed inline, F register = 3 words) -------- */
    p[10]  = fpname;                 /* 006112 LDF ,B -113 ; 006113 STF ,X 10              */

    /* --- issue the ND-500 call ---------------------------------------------- */
    if (MON60_LDCS() == ERROR)       /* 006114 JPL I 32 -> thunk 146445 (LDCS 37B)         */
        cmd_error_002673();          /* 006115 callsite+1 = ERROR  -> ptr 006130 = 002673  */
    cmd_loop_010613();               /* 006116 callsite+2 = SUCCESS -> ptr 005731 = 010613  */

    /* NOTE: NO file I/O is performed on the ND-100 side.  The only MON call in
     * this case is the LDCS MON 60 itself.  SINTRAN's ICSLOAD handler (server
     * side) copies the passed file name and reads the file into control store.
     * (Confirmed: no OPEN/RFILE/ALTON appears between 006064 and 006116.)      */
}
