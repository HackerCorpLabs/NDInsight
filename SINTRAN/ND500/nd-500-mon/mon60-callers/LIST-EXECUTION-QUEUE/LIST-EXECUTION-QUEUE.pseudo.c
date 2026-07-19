/* ===========================================================================
 *  LIST-EXECUTION-QUEUE  ->  MON 60 subfunction LSTEXQ = 133B (0x5B = 91 dec)
 * ---------------------------------------------------------------------------
 *  Program : nd-500-mon-j04.prog  (ND-500/5000 MONITOR J04, ND-100 side)
 *  Handler : STANDALONE ENTER-routine at 111430 (framesize 000407),
 *            spanning 111430..111603.
 *  NPL purpose of 133B (authoritative): LIST ND-500 EX-QUEUE (handler ILI5EXQ).
 *  yaml: LSTEXQ "List execution queue", params (none).
 *  Return convention (PROVEN): callsite+1 = ERROR (LEAVE-value), callsite+2 =
 *  SUCCESS. Routine ends via LEAVE-SKIP (177335) at 111564/111603.
 * ===========================================================================
 */

extern int  MON60_LSTEXQ(void);    /* JPL I ->111570; ptr=thunk 146723; SAA 133 LIST-EX-QUEUE */
extern void put_text(word idx);    /* helpers 016507/054452 : emit strings */
extern void put_val(word v);       /* emit numeric field */
extern void hold(void);            /* MON 104 SuspendProgram (pause between screens) */
extern void leave_val(int err);    /* 177327 error return */
extern word *p;

void cmd_list_execution_queue(void)
{
    word qbuf[..];     /* work buffer @B-165 : receives the queue snapshot   */

    do {
        /* --- request the execution-queue snapshot (133B) at 111445 -------- */
        /* 111433 AAA -165 -> &qbuf saved @B-171 ; 111442 LDT ,B-171 ;
         * 111444 STF ,X 6 : param 1 = 3-word descriptor (&qbuf + count)      */
        p[6] = (word)&qbuf;
        if (MON60_LSTEXQ() == ERROR)   /* 111445 JPL I 123 -> thunk 146723 (LSTEXQ 133B) */
            leave_val();               /* 111446 callsite+1 = ERROR -> 111571 = LEAVE(val) */
        /* 111447 success continues */

        put_text(0121);                /* 111447-111454 column heading (helper 016507) */

        /* --- walk the returned queue entries ----------------------------- */
        for (i = 0; qbuf[i] != -1; i++) {   /* 111456-111554 loop; SAT -1 = end sentinel */
            /* print each entry's fields via the output helpers
             * (111470/111477/111506/111524/111530/111542 -> put_text/put_val) */
        }

        /* --- pause between screenfuls ------------------------------------ */
        if (more_to_show /* B-172 != -1, tested 111555-111560 */) {
            hold();                    /* 111561 LDA I 21 ; 111562 MON 104 (HOLD) */
            /* 111563 JMP -123 -> 111440 : loop and re-request the queue      */
        }
    } while (more_to_show);

    return /*SKIP*/;                   /* 111564 JPL I 17 -> 111603 = LEAVE-SKIP (success) */
}

/* MON calls issued (PROVEN):
 *   MON 60 LSTEXQ (133B) @111445  - request the ND-500 execution queue
 *   MON 104 (HOLD)        @111562  - suspend/pause between screenfuls          */
