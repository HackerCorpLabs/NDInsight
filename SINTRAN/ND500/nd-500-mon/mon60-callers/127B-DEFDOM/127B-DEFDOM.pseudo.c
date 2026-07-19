/* ===========================================================================
 *  DEFDOM   ->  MON 60 subfunction DEFDOM = 127B (0x57 = 87 dec)
 *  Purpose: DEFINE STANDARD DOMAIN.  Server handler IDFSYDOM.
 *  Operator command DEFINE-STANDARD-DOMAIN (INDEX.md sec 2.1).
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : ENTER-routine 045463 (framesize 003116).  Call site 046056.
 *  Source  : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int  MON60_DEFDOM(void);   /* 046056 JPL I ->046105; thunk 146707 SAA 127 */
extern void err_045511(void);     /* error handler (PROVEN target)               */
extern word *p;                   /* gateway slots, p[6]=param1 */

void define_standard_domain(void)
{
    /* ... routine 045463 builds the domain definition in its frame first ... */
    p[6] = local_B_plus_24;          /* 046053 LDA ,B 24 ; 046055 STA ,X 6        */
    if (MON60_DEFDOM() == ERROR)     /* 046056 -> thunk 146707 (DEFDOM 127B)      */
        err_045511();                /* 046057 callsite+1 -> ptr 046100 = 045511   */
    /* 046060 callsite+2 SUCCESS -> 046070 */
}

/* PROVEN: one MON60 parameter (slot 6) = local(B+24), stored immediately before
 *   the call.  No slots 7/10 are stored adjacent.
 * INFERRED: local(B+24) is the domain-definition operand (name/descriptor or a
 *   pointer to the domain-definition block assembled earlier in this large
 *   routine).  The exact contents were not traced.  Server handler IDFSYDOM
 *   defines the standard domain on the SINTRAN side. */
