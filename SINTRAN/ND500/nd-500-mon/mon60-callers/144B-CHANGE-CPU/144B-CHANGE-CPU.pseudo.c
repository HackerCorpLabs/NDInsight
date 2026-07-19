/* ===========================================================================
 *  144B CHANGE CPU  ->  MON 60 subfunction 144B (0x64 = 100 dec)
 *  Purpose: CHANGE CPU (ICHACPU) ; server handler ICHACPU.
 *  Program : nd-500-mon-j04.prog (ND-100 side). Thunk 146750 = SAA 144.
 *  Every fact below is traceable to an OCTAL address in
 *  SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  Slot convention (PROVEN): X := ,B-176 (stack top); STx ,X 6/7/10/11 = MON60 param.
 *  Return (PROVEN, prog.md 5.4): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_144(void);   /* JPL I -> thunk 146750 (SAA 144) -> gateway 146244 */
extern word *p;              /* gateway param slots: p[6]=param1, p[7]=param2, ... */

/* --- call site 010345, in ENTER-routine 002662 (framesize 000331) --------------- */
void site_010345(void)
{
    /* param1 (,X 6) = &(B-127), where B-127 receives the value returned in D by the numeric-arg evaluator 002003 (010336 JPL I -37 -> ptr 010277=002003). Store at 010344. Value=CPU number: INFERRED. */
    if (MON60_144() == ERROR)   /* 010345 */
        goto error;            /* 010346 -> ptr 010256 = 007500 (leaf error handler, role INFERRED) */
    /* success: 010347 -> ptr 010513 = 010613 (command loop, PROVEN) */
    return;
error:
    handle_error();
}

/* --- call site 011231, in ENTER-routine 011043 (framesize 000236) --------------- */
void site_011231(void)
{
    /* param1 (,X 6) = &(B-152); B-152 := D copied from B-160 (011223 LDD ,B-160 / 011224 STD ,B-152). Store at 011230. Guarded by 011221 SKP IF DA EQL ST. */
    if (MON60_144() == ERROR)   /* 011231 */
        goto error;            /* 011232 -> 011114 (leaf handler, role INFERRED) */
    /* success: 011233 -> 011377 (continues in routine) */
    return;
error:
    handle_error();
}

