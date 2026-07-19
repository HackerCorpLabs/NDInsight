/* ===========================================================================
 *  006B-PLACE   ->  MON 60 subfunction PLACE = 6B (0x06 = 6 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Caller  : standalone ENTER-routine 041730 (framesize 000000).
 *  Purpose (NPL, authoritative): LOAD (PLACE), ONE SEGMENT.  Handler: ISEGLOAD.
 *  Two call sites: 042230 and 042535 (identical parameter layout).
 *  Return (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 * ===========================================================================
 */

extern int MON60_PLACE(void);   /* thunk 146335 (SAA 6) -> gateway 146244        */
extern word *p;                 /* p[6..12] = MON 60 params 1..5                 */

/* both sites store the same five slots into the gateway frame: */
static void place_one_segment(int site)         /* routine @041730 */
{
    p[6]  = local_B162;          /* 042210-042212 / 042515-042517 - param1 (value) */
    p[7]  = (word)&local_B127;   /* param2 */
    p[10] = (word)&local_B155;   /* param3 */
    p[11] = (word)&local_B157;   /* param4 */
    p[12] = f_register_B135;     /* param5 - F register (3-word), SAA 11/SWAP     */

    if (MON60_PLACE() == ERROR)  /* 042230 JPL I 74 / 042535 JPL I 34 -> thunk 146335 */
        goto err;                /* callsite+1 = ERROR -> local block 042134/042416 */
    /* callsite+2 = SUCCESS */
}
