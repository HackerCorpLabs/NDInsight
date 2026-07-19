/* ===========================================================================
 *  LIST-SYSTEM-PARAMETERS  ->  MON 60 subfunction RSYSP = 103B (0x43 = 67 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : STANDALONE ENTER-routine at 073115 (framesize 000336),
 *            spanning 073115..073411.
 *  NPL purpose of 103B (authoritative): READ SYSTEM VARIABLES (handler IRSYSP).
 *  yaml: RSYSP "Read system parameters", param <parameter array>.
 *  This handler issues FOUR distinct MON 60 subfunctions.
 *  Return convention (PROVEN): callsite+1 = ERROR (LEAVE-value), callsite+2 =
 *  SUCCESS. Routine ends via LEAVE-SKIP (177335, reached through 073330/073405).
 * ===========================================================================
 */

extern int  MON60_RSYSP(void);     /* JPL I ->073312; ptr=thunk 146624; SAA 103 READ-SYS-VARS */
extern int  MON60_RDSWP(void);     /* JPL I ->073316; ptr=thunk 146665; SAA 121 Read-from-swapper */
extern int  MON60_WSYSP(void);     /* JPL I ->073402; ptr=thunk 146627; SAA 104 Write-sys-params */
extern int  MON60_TOSWP(void);     /* JPL I ->073404; ptr=thunk 146610; SAA 76  Send-msg-to-swapper */
extern void put_val(word v);       /* helpers 054045/000067/054430/030060 : emit values */
extern void leave_val(int err);    /* 177327 error return */
extern word *p;

void cmd_list_system_parameters(void)
{
    word sysarr[..];   /* @B-157.. : system-variable array (also used as RDSWP data area) */

    /* --- read the ND-500 system variables (103B) at 073132 ----------------- */
    p[6] = (word)&sysarr;          /* 073125 AAA -167 ; 073127 AAA 10 -> &(B-157) ; 073131 STA ,X 6 */
    if (MON60_RSYSP() == ERROR)    /* 073132 JPL I 160 -> thunk 146624 (RSYSP 103B) */
        leave_val();               /* 073133 -> 073313 = LEAVE(val) */

    /* --- read a block from the swapper (121B) at 073152 -------------------- */
    p[6]  = system_word[0155];     /* 073137 LDA 155 ; 073141 STA ,X 6  = <no. of bytes>   */
    p[7]  = system_word[0153];     /* 073142 LDA 153 ; 073143 STA ,X 7  = <ND-500 address> */
    p[10] = (word)&buf_B122;       /* 073144 AAA -122 ; 073146 STA ,X 10 = <data area>     */
    p[11] = (word)&nread_B130;     /* 073147 AAA -130 ; 073151 STA ,X 11 = <bytes read>    */
    if (MON60_RDSWP() == ERROR)    /* 073152 JPL I 144 -> thunk 146665 (RDSWP 121B) */
        leave_val();               /* 073153 -> 073313 */

    /* ... format and print the collected values via helpers ...
     * 073204 (054045), 073207/073212 (000067), 073226 (054430), 073246 (030060),
     * 073261 (167224), 073374 (001726) - NONE are MON calls. (073154..073263)  */

    /* --- internal jump-table dispatch selects the follow-up action --------- */
    /* 073263 LDX ,B -170 ; 073264 LDX I ,X 43 ; 073265 JMP ,X 0  (computed goto) */
    switch (selector /* @B-170, indexes table via [X+43] */) {

    case WRITE:   /* WRITE system parameters (104B), branch entered at 073347 */
        p[6] = (word)&sysarr;      /* 073347 AAA -167 ; 073353 STA ,X 6 */
        if (MON60_WSYSP() == ERROR)/* 073354 JPL I 26 -> thunk 146627 (WSYSP 104B) */
            leave_val();           /* 073355 -> 073313 */
        break;

    case SEND:    /* SEND message to swapper (76B), branch entered at 073356 */
        p[6] = (word)&sysarr;      /* 073356 AAA -167 ; 073361 STA ,X 6 = <record> */
        if (MON60_TOSWP() == ERROR)/* 073362 JPL I 22 -> thunk 146610 (TOSWP 76B) */
            leave_val();           /* 073363 -> 073313 */
        return /*SKIP*/;           /* 073364 -> 073330 = LEAVE-SKIP (success) */
    }
    /* other cases return through the routine's common exits */
}

/* MON 60 subfunctions issued (all PROVEN by thunk resolution):
 *   103B RSYSP  Read system variables   @073132
 *   121B RDSWP  Read from swapper        @073152
 *   104B WSYSP  Write system parameters  @073354
 *   076B TOSWP  Send message to swapper  @073362                              */
