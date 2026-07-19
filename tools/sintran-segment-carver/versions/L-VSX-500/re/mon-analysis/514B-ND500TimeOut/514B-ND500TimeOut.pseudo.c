/* ==========================================================================
 * MON 514B - ND500TimeOut (M5TMOUT / M5TMO)  pseudo-C model for an emulator author.
 *
 * Source: real SINTRAN L bytes, 026-S3IMPIT.bin (S3MPIT), load base 32000B.
 *   M5TMO = 140563B  (entry)
 *   NNT11 = 140600B  (internal restart-reason continuation tag)
 *   NNC16 = 140712B, NNC17 = 140763B, NNC18 = 141005B (internal continuation tags)
 * ND-500 level-12 call: dispatched via the level-12 GOSW table (uncarved), slot 14
 * octal of the 5CMNO-L12MIN GOSW (M5TMOUT), NOT the ND-100 GOTAB. The handler itself
 * is ND-100 code and its control flow is byte-verified against 514B-ND500TimeOut.ASM.
 *
 * Suspends the calling ND-500 program for a given time; it is placed in a time queue
 * inside the ND-500 (not the ND-100). Parameters are passed in the ND-500 MESSAGE
 * BUFFER (indexed by 5MBBANK + field displacement), not in ND-100 A/T/X user
 * registers; the A/T/X usage below is level-12 driver-internal.
 *
 * VERDICTS: control flow, the indirect worker pointers, the EC174 error constant, and
 * the exit routes are byte-verified. The exact time-unit arithmetic, the message-field
 * displacements, and the parameter/return semantics are INFERRED from the code
 * structure, the resolved worker names, and the NPL M5TMOUT source (a DIFFERENT
 * revision than L - its addresses are ~200B lower) plus the reference manual - marked
 * inline. All addresses in comments are octal. No unicode below.
 * ========================================================================== */

/* Indirect worker pointers the body jumps through (resolved to L07 symbols). */
#define P_SLOCK   0023706   /* -> SLOCK     : take the ND-500 driver lock            */
#define P_SUNLO   0024041   /* -> SUNLOCK   : release the ND-500 driver lock         */
#define P_XTER5   0145372   /* -> XTER500   : terminate/clear ND-500 timer (INFERRED)*/
#define P_SPITM   0023320   /* -> SPITMQ    : set restart reason into queue (INFERRED)*/
#define P_IFM50   0022704   /* -> IFM500XQ  : fix proc into ND-500 time queue (INFERRED)*/
#define P_WN5ST   0023670   /* -> WN5STATUS : write ND-500 process status            */
#define P_EMONI   0023021   /* -> EMONICO   : restart ND-500 proc with error code    */
#define P_XACTR   0145466   /* -> XACTRDY   : reactivate ND-500 executor queue       */
#define P_NXTMS   0135067   /* -> NXTMSG    : handle next ND-500 message (exit loop)  */

#define EC174     0174      /* illegal parameter error code (VERIFIED byte SAA 174)  */

/* mem[] / phys[] and the T/X PHYSICAL transfers.
 * mem[] is the ND-100 word address space of the resident level-12 driver.
 * The handler reaches the ND-500 message buffer with the T/X physical transfer
 * instructions (LDATX / STATX / LDDTX / STDTX here). These form a 24-bit PHYSICAL
 * address that BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte), X the word offset; disp3 (the 3-bit field, bits 3-5)
 * is 0 in every transfer here - the code adjusts X with AAX instead. T is loaded
 * with the ND-500 message-buffer bank 5MBBANK, so these are reads/writes of the
 * ND-500 message buffer at physical bank:offset: LDATX A=phys[EL]; STATX
 * phys[EL]=A; LDDTX A=phys[EL],D=phys[EL+1]; STDTX phys[EL]=A,phys[EL+1]=D.
 * phys[] is that 24-bit physical space; msg[] below is the same buffer named for
 * the ND-500 field it carries. */
#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

/* --------------------------------------------------------------------------
 * M5TMO (140563B) - entry. ENTRY (from the NPL source, a different revision, so
 * INFERRED here): X = current message, B = ND-500 CPU datafield.
 * -------------------------------------------------------------------------- */
int mon_514B_ND500TimeOut(void)
{
    int reason;                            /* restart cause returned to the caller  */

    call((( P_SLOCK )));                    /* 140563B JPL I -> SLOCK (VERIFIED ptr) */

    /* 140565B..140571B: read the message flag word 5MSFL with a physical transfer
     * (LDT I 146 loads bank 5MBBANK into T, AAX -1 sets X, 140567B LDATX:
     * A = phys[ELADDR(T, X)], MMU bypassed) and test the repeat bit (55REP,
     * BSKP ONE 170 = bit 15). */
    if (/* repeat bit 55REP set - INFERRED field */ 0) {
        /* 140572B..140575B: clear the repeat bit, release the lock, reason = -1. */
        clear_bit_55REP();                 /* BSET ZRO 170 DA clears bit 15; 140573B */
                                           /* STATX writes phys[ELADDR(T,X)] = A back */
        call((( P_SUNLO )));               /* 140574B JPL I -> SUNLOCK               */
        reason = -1;                       /* 140575B SAA -1 : "scheduled for repeat"*/

        /* 140576B (falls into) NNT11 (140600B): restart-reason continuation.
         * Store reason, terminate the timer, push the reason, reactivate, exit. */
    n5tmf:
        store_restart_reason(reason);      /* 140576B STA 154 (msg field, INFERRED)  */
        call((( P_XTER5 )));               /* 140600B JPL I -> XTER500 (NNT11 tag)   */
        call((( P_SPITM )));               /* 140603B JPL I -> SPITMQ (reason)       */
        call((( P_XACTR )));               /* 140604B JPL I -> XACTRDY               */
        goto_addr((( P_NXTMS )));          /* 140605B JMP I -> NXTMSG (exit)         */
    }

    call((( P_SUNLO )));                    /* 140606B JPL I -> SUNLOCK               */

    /* 140607B..140627B: read the number-of-time-units parameter (5ADP1) from the
     * ND-500 message buffer with a physical double transfer (LDT I 123, AAX 100,
     * 140612B LDDTX: A = phys[ELADDR(T, X)]; D = phys[ELADDR(T, X) + 1]). */
    int num_time_units = msg_double_INFERRED(/*5ADP1 via phys[EL]/phys[EL+1]*/);

    /* 140613B..140620B: reject an illegal count (A non-zero high word / out of range). */
    if (/* illegal count - INFERRED test (NPL: A<>0) */ 0) {
    lilp1:
        {   int err = EC174;               /* 140615B SAA 174 (VERIFIED byte)        */
            call((( P_EMONI )));           /* 140616B JPL I -> EMONICO (with err)    */
            call((( P_XACTR )));           /* 140617B JPL I -> XACTRDY               */
            goto_addr((( P_NXTMS )));      /* 140620B JMP I -> NXTMSG (exit)         */
        }
    }

    /* 140621B..140622B: a zero count means "restart immediately, clears restart
     * flag" - jump back to the reason=0 restart path (NPL: IF D=0 GO N5TMF). */
    if (num_time_units == 0) { reason = 0; goto n5tmf_zero; }

    /* 140623B..140667B: read the time-unit selector (via 5ADP2), validate it is
     * 1..4 (basic / seconds / minutes / hours) else go to the EC174 path (lilp1),
     * then convert the requested delay to basic time units (the SAT/SHA/RADD ladder
     * at 140633B..140667B; exact arithmetic INFERRED). */
    int time_unit  = msg_INFERRED(/*5ADP2*/);
    if (time_unit < 1 || time_unit > 4) goto lilp1;
    long basic_units = convert_to_basic_units_INFERRED(num_time_units, time_unit);

    /* 140670B..141021B: enqueue the process on the ND-500 time queue.
     *   140670B SLOCK        - re-take the driver lock
     *   140672B IFM500XQ     - fix the process into the ND-500 time queue
     *   140674B WN5STATUS    - write the ND-500 process status (queued/timed)
     *   NNC16=140712B / NNC17=140763B / NNC18=141005B: byte-address <-> word-address
     *     conversions while linking the entry into the sorted time queue; these read
     *     and write the message buffer with the physical transfers LDDTX/STDTX
     *     (A=phys[EL],D=phys[EL+1] / phys[EL]=A,phys[EL+1]=D) and LDATX/STATX
     *     (A=phys[EL] / phys[EL]=A) - exact field layout INFERRED.
     *   141020B SUNLOCK      - release the lock
     *   141021B JMP I -> NXTMSG - exit (the ND-500 proc stays suspended until the
     *                            queued restart time). */
    call((( P_SLOCK )));                    /* 140670B                                */
    call((( P_IFM50 )));                    /* 140672B fix into time queue            */
    call((( P_WN5ST )));                    /* 140674B write ND-500 status            */
    insert_into_time_queue_INFERRED(basic_units);
    call((( P_SUNLO )));                    /* 141020B                                */
    goto_addr((( P_NXTMS )));               /* 141021B JMP I -> NXTMSG (exit)         */

n5tmf_zero:
    reason = 0;
    goto n5tmf_use_reason;                  /* re-enter the NNT11 restart-reason block */
n5tmf_use_reason:
    /* (models NPL "GO N5TMF" with reason 0; same body as the n5tmf label above)   */
    return reason;

    /* The value the caller collects (ReturnStatus: 0=time elapsed, 1=interrupt,
     * -1=scheduled for repeat) is delivered via the ND-500 message buffer when the
     * process is finally restarted, not by an in-line return here (INFERRED). */
}

/* Notes for the emulator:
 *  - Every exit is EMONICO(err)/XACTRDY (error restart) or NXTMSG (normal); there is
 *    no ND-100-style skip-return in this window (VERIFIED).
 *  - The nine indirect worker pointers all resolve 1:1 to the M5TMOUT worker set
 *    (SLOCK, SUNLOCK, XTER500, SPITMQ, IFM500XQ, WN5STATUS, EMONICO, XACTRDY, NXTMSG)
 *    - this is the byte-level identity proof for the handler.
 *  - Reference-manual / NPL contract (INFERRED against these L bytes): parameters are
 *    (1) NoOfTimeUnits, (2) TimeUnit (1=1/50s, 2=s, 3=min, 4=hour); output =
 *    ReturnStatus. Only the SAA 174 (EC174 illegal-parameter) error is byte-proven.
 *  - The data pools at 140732B..140752B and 141022B..141026B are POINTER/PARAMETER
 *    words the disassembler renders as instructions - disregard those as code. The
 *    140731B JMP jumps OVER the first pool.
 */
