/* ==========================================================================
 * MON 503B - InputString (NINST / XNINS)  pseudo-C model for an emulator author.
 *
 * Source: real SINTRAN L bytes, 026-S3IMPIT.bin (S3MPIT), load base 32000B.
 *   NINST = 141272B  (entry)
 *   XNINS = 141277B  (extended/worker input-string routine)
 *   MO600 = 141565B  (internal level-12 restart/continuation tag, PART 2)
 * ND-500 level-12 call: dispatched via the level-12 GOSW table (uncarved), slot 3
 * of the 5CMNO-L12MIN GOSW (NINSTR), NOT the ND-100 GOTAB. The handler itself is
 * ND-100 code and its control flow is byte-verified against 503B-InputString.ASM.
 *
 * Reads a line of input into the calling ND-500 process and returns the number of
 * bytes actually read. Parameters are passed in the ND-500 MESSAGE BUFFER (indexed
 * by 5MBBANK + field displacement), not in ND-100 A/T/X user registers; the A/T/X
 * usage below is level-12 driver-internal.
 *
 * VERDICTS: control flow, the indirect worker pointers, the EC174 error constant,
 * and the three exit routes are byte-verified. The exact byte-count / buffer-copy
 * arithmetic and the message-field layout are INFERRED from the code structure,
 * the resolved worker names, and the reference manual (a different revision than L)
 * - marked inline. All addresses in comments are octal. No unicode below.
 * ========================================================================== */

/* Indirect worker pointers the body jumps through (resolved to L07 symbols). */
#define P_EMONI   0023021   /* -> EMONICO : restart ND-500 proc with error code   */
#define P_XACTR   0145466   /* -> XACTRDY : reactivate ND-500 executor queue      */
#define P_NXTMS   0135067   /* -> NXTMSG  : handle next ND-500 message (exit loop) */
#define P_5GTDF   0147440   /* -> 5GTDF   : get terminal datafield                 */
#define P_NORMM   0137167   /* -> NORMMC  : non-terminal MON fallback             */
#define P_SET12   0027212   /* -> SET12   : level-12 setup helper (INFERRED role)  */
#define P_GCPUD   0023624   /* -> GCPUDF  : get ND-500 CPU datafield               */
#define P_WN5ST   0023670   /* -> WN5STATUS : write ND-500 process status          */

#define EC174     0174      /* illegal parameter / byte-count error code (VERIFIED byte) */

/* mem[] / phys[] and the T/X PHYSICAL transfers.
 * mem[] is the ND-100 word address space of the resident level-12 driver.
 * The handler reaches the ND-500 message buffer with the T/X physical transfer
 * instructions (LDDTX/STDTX/STZTX here). These form a 24-bit PHYSICAL address
 * that BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte), X the word offset; disp3 (the 3-bit field, bits 3-5)
 * is 0 in every transfer here - the code adjusts X with AAX instead. When T is
 * loaded with the ND-500 message-buffer bank 5MBBANK these are reads/writes of the
 * ND-500 message buffer at physical bank:offset; where the code sets T=0 the
 * access is to physical bank 0. phys[] is that 24-bit physical space. */
#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

/* --------------------------------------------------------------------------
 * NINST (141272B) - entry. XNINS (141277B) is the second entry into the same
 * body (the extended worker); a caller may arrive at either.
 * -------------------------------------------------------------------------- */
int mon_503B_InputString(void)
{
    int bytes_read;                       /* the value MON 503B returns          */

    /* 141272B..141276B: load the message-field base/index and fetch a control
     * word (indirect through ptr words 141420B=004654, 141421B=011260). */
    /* 141277B (XNINS): JAF - test the fetched field; branch selects the path. */
    if (/* field not acceptable - INFERRED test */ 0) {
        /* 141303B..141307B: error path.
         * SAA 174 loads the error code, then restart the process and dispatch. */
        int err = EC174;                  /* SAA 174 (VERIFIED byte)             */
        call((( P_EMONI )));              /* JPL I -> EMONICO (with err)         */
        call((( P_XACTR )));              /* JPL I -> XACTRDY                    */
        goto_addr((( P_NXTMS )));         /* JMP I -> NXTMSG (exit)              */
    }

    /* 141310B..141313B: is the source a terminal? get its datafield. */
    /* STA into msg field 136B, then: */
    if (/* not a terminal - INFERRED */ 0) {
        goto_addr((( P_NORMM )));         /* 141313B JMP I -> NORMMC (fallback)  */
    }
    call((( P_5GTDF )));                   /* 141312B JPL I -> 5GTDF (datafield)  */

    /* 141314B..141341B: test a terminal-datafield status bit (LDA,X 3 / BSKP ONE).
     * On the not-connected / error branch (141331B..141341B) build an error code
     * and leave via EMONICO/XACTRDY, else fall through to the read setup. */
    if (/* line not connected - INFERRED (NPL error 316) */ 0) {
        call((( P_EMONI )));              /* 141337B JPL I -> EMONICO            */
        call((( P_XACTR )));              /* 141340B JPL I -> XACTRDY            */
        /* 141341B JMP I -> internal continuation (141434B)                     */
    }
    call((( P_SET12 )));                   /* 141325B JPL I -> SET12 (setup)      */

    /* 141342B..141417B: input-transfer setup - part 1.
     * Compute/scale field indices (AND/ADD/SHT/SHA), index the terminal input
     * datafield, and prepare the transfer descriptor. The exact byte-count field
     * (max bytes to read = the caller's buffer size) and the destination-array
     * pointer are read from the message buffer here (INFERRED layout).
     * Block ends at 141417B JMP -> 141450B, jumping OVER the pointer/parameter
     * pool at 141420B..141447B (data, not code). */
    int max_bytes = msg_field_INFERRED();  /* caller-supplied max input length    */
    int dest_ptr  = msg_field_INFERRED();  /* caller-supplied destination array   */

    /* 141450B..141564B: input-transfer processing - part 2a.
     * A SAT-driven decision ladder (SAT -1 / SAT 11 / SAT 7 / SAT 10, each with
     * SKP IF DA EQL/MLST ST) classifies the current character/state and updates
     * the running result in msg field -11B (STA,B -11). Semantics INFERRED. */
    bytes_read = 0;
    /* ... character/state classification loop; accumulate bytes_read ... */

    /* 141565B (MO600) .. 141612B: PART 2 continuation - the finish/echo/error tags
     * PT2OK=141567B, PT2AO=141570B, ERRFP=141574B, PT2YE=141576B, PT2ER=141600B.
     * This block finalises the returned byte count (STD into a msg field),
     * writes process status, and selects the exit. Reached by the body's own
     * direct jumps 141544B->141605B and 141562B->141571B (VERIFIED closure). */
    call((( P_WN5ST )));                   /* 141515B/part-2 JPL I -> WN5STATUS    */
    /* 141600B/141601B JPL I -> EMONICO / XACTRDY on the error tags             */

    /* Final exit: JMP I -> NXTMSG (via ptr pool word 141623B=135067).
     * The number of bytes actually read has been written to the message buffer
     * for the ND-500 caller to collect (field/mask layout INFERRED). */
    return bytes_read;                     /* value delivered via the msg buffer  */
}

/* Notes for the emulator:
 *  - Every exit is EMONICO(err)/XACTRDY (error restart) or NXTMSG (normal); there
 *    is no ND-100-style skip-return in this window (VERIFIED).
 *  - Reference-manual contract (mark as inferred against these L bytes): params
 *    are (1) open/LDN file number, (2) max bytes to read = buffer size,
 *    (3) destination array; output = returned byte count. Errors include
 *    end-of-file on nowait input and line-not-connected - those live in the
 *    NPL DVINST/NINSTR body (a different revision) and are only structurally
 *    matched here (the SAA 174 / EMONICO error path is the byte-proven one).
 *  - The pointer/parameter pools at 141420B..141447B and 141613B..141623B are
 *    DATA; the disassembler renders them as instructions - disregard those.
 */
