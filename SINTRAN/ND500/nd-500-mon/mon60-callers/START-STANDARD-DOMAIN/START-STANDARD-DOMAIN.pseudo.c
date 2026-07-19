/* ===========================================================================
 *  START-STANDARD-DOMAIN   ->  MON 60 subfunction 130B (0x58 = 88 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : STANDALONE ENTER-routine at 043011 (framesize 000717),
 *            spanning 043011..045462.
 *  NPL purpose of 130B (authoritative): START STANDARD DOMAIN (handler ISFSYDOM).
 *  yaml name for 130B: PLADOM "Place standard domain".
 *  This routine STARTS the standard domain and then brackets a PLACE operation
 *  (start-place / ... / end-place). Only the MON 60 interactions are modelled;
 *  the large formatting/placement body is summarised.
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  OPERATOR-COMMAND NAME UNVERIFIED (bank-2 command table not consulted); folder
 *  named by authoritative subfunction PURPOSE 130B.
 * ===========================================================================
 */

extern int  MON60_130B(void);      /* JPL I ->043223; ptr=thunk 146712; SAA 130 START-STD-DOMAIN */
extern int  MON60_SRESPL(void);    /* JPL I ->043674; ptr=thunk 146737; SAA 140 Start-residual-place */
extern int  MON60_SPLAC(void);     /* JPL I ->043676; ptr=thunk 146530; SAA 55  Start-place */
extern int  MON60_EPLAC(void);     /* JPL I ->044117; ptr=thunk 146533; SAA 56  End-place */
extern void leave_val(int err);    /* 177327 error return (reached via 043217) */
extern word *p;

void cmd_start_standard_domain(void)
{
    word name;         /* @B-162 : first word of the domain-name descriptor (from F @B-171) */
    word flag33;       /* value read from global [33]                                        */

    /* --- START STANDARD DOMAIN (130B) at 043171 ---------------------------- */
    /* 043162 LDF ,B -171 ; 043163 STF ,B -162 ; 043164 LDA ,B -162            */
    p[6] = name;                    /* 043166 STA ,X 6  = param1 (<name>)        */
    p[7] = flag33;                  /* 043167 LDA 33 ; 043170 STA ,X 7 = param2  */
    if (MON60_130B() == ERROR)      /* 043171 JPL I 32 -> thunk 146712 (130B)    */
        goto err;                   /* 043172 callsite+1 = ERROR -> 043071       */
    /* 043173 success continues ... (domain now started) */

    /* ... large intervening body: builds the place descriptor, formats output,
     * decides SRESPL vs SPLAC on local flag @B-163 ... (043174..043546) ...   */

    /* --- place bracket: START-PLACE, selected by flag @B-163 --------------- */
    if (start_residual /* B-163 != 0 */) {
        if (MON60_SRESPL() == ERROR) /* 043547 JPL I 125 -> thunk 146737 (140B) */
            goto err;                /* 043550 -> 043321 */
    } else {
        if (MON60_SPLAC() == ERROR)  /* 043552 JPL I 124 -> thunk 146530 (55B)  */
            goto err;                /* 043553 -> 043321 */
    }

    /* ... place one or more segments, format progress ... (043554..044061) ... */

    /* --- END-PLACE (56B) at 044062 ----------------------------------------- */
    if (MON60_EPLAC() == ERROR)     /* 044062 JPL I 35 -> thunk 146533 (56B)     */
        goto err;                   /* 044063 -> 043321 */
    /* 044064 continues ... eventually LEAVE-SKIP (043442) on success           */
    return /*SKIP*/;

err:
    leave_val(/*errcode*/);         /* -> 177327 via pointer 043217              */
}

/* MON 60 subfunctions issued by this handler (all PROVEN by thunk resolution):
 *   130B START-STANDARD-DOMAIN @043171
 *   140B SRESPL (start residual place) @043547
 *   055B SPLAC  (start place)          @043552
 *   056B EPLAC  (end place)            @044062                                 */
