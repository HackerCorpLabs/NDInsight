/* ============================================================================
 * MON 305B  GetSIBASMessage (MSIBB)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Two distinct code paths are involved; keep them straight:
 *
 *  (1) GOTAB[305]=111752B (BYTE-PROVEN by prove-mon.py) dispatches to DIA0, the
 *      head of the DIA device/DIA-block body. That body decodes as RT-level
 *      driver code (it issues MON 131/64/116/134 and uses IRW/MST PID), which is
 *      INCONSISTENT with a level-14 handler for a MON# this high - so whether
 *      GOTAB[base]+305 is truly the level-14 target here is UNVERIFIED. Modelled
 *      below as dispatch_DIA0(). This is the SAME body the MON 304B DSI0 stub
 *      jumps into.
 *
 *  (2) MSIBB=104221B is the clean SIBAS-server message handler (the semantic
 *      "GetSIBASMessage"). It is the sibling of MAPSI (MON 304B SendSIBASMessage)
 *      inside one shared SIBAS body. It is real SINTRAN L bytes but is
 *      UNREACHABLE from GOTAB[305] and from every GOTAB slot 0..377; it is
 *      reached only via the uncarved resident CALLPROC second-level dispatch.
 *      Modelled below as mon_get_sibas_message().
 *
 * Control flow is BYTE-VERIFIED from 305B-GetSIBASMessage.ASM; the semantic
 * labels (which ,B offset is the device word / status word, the buffer-chain
 * walk) are INFERRED from the NPL SUBR MAPSIB,MSIBB,TISIBB structure - treat as
 * a model, not gospel. Every opcode is grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md.
 * Addresses in comments are octal.
 * ============================================================================ */

/* --- Path (1): the byte-proven GOTAB[305] target (DIA0 body) --------------- */
/* Real bytes, RT-level driver semantics UNVERIFIED. Provided so an emulator
 * that follows GOTAB literally lands somewhere sane. Same body as MON 304B. */
void dispatch_DIA0(mon_regs *r)          /* 111752 DIA0 */
{
    int st = r->B[033];                  /* 111771 LDT ,B 33  - status/flags word   */

    ei();                                 /* 112016 ION  */
    abstr_data_transfer(r);               /* 112021 MON 131 (ABSTR) - block xfer     */
    di();                                 /* 112023 IOF  */

    if (/* error path 112077.. */ 0) {    /* 112035 JMP 42 -> 112077 on DD<=0        */
        warning_message();                /* 112106 MON 64  (ERMSG)  */
        unfix_segment(013);               /* 112111 MON 116 (UNFIX)  */
        exit_rt_program();                /* 112112 MON 134 (RTEXT)  */
    }
}

/* --- Path (2): the semantic SIBAS-server handler (MSIBB) -------------------- */
/* Sibling of MAPSI (MON 304B). Parameter slots per the NPL contract (INFERRED
 * mapping to ,B offsets):
 *   dev word (,B 11), HOINT (,B -1), status/result (,B 12).
 * Per ND100-INSTRUCTION-SEMANTICS.md:
 *   RADD CLD SX DB = B = X (COPY idiom, dest cleared then +source);
 *   SHA ZIN SHR 6  = logical right shift A by 6 (zero fill);
 *   SKP IF DA UEQ ST = skip next if A != T (unsigned not-equal);
 *   LDX ,X 1 / BSKP ONE 170 DA = follow chain pointer, test bit 15 of A. */
int mon_get_sibas_message(mon_regs *r)   /* 104221 MSIBB */
{
    r->B_reg = r->X;                     /* 104221 RADD CLD SX DB */
    set_level(4);                        /* 104222 SAA 4 ; 104223 MCL PIE */

    int dw = r->B[011] >> 6;             /* 104224 LDA ,B 11 ; 104225 SHA ZIN SHR 6 */
    if (dw != 0) {                        /* 104226 JAZ 17 -> 104245 (skip block)    */
        int a = dw | 057;                 /* 104227 ORA 57 */
        if (a == r->B[-1])                /* 104231 SKP IF DA UEQ ST ; 104230 LDT ,B -1 */
            goto scan;                    /* 104232 JMP 13 -> 104245 */
        r->B[-1] = a;                     /* 104233 STA ,B -1  (HOINT)               */
        /* 104234-104244: mask ,B 11 by 052, store descriptor via STD I ,X 45       */
        store_sibas_descriptor(r);
    }

scan:                                     /* 104245 */
    /* 104245-104262: mask ,B 11 by 041, load buffer head (LDX I ,X 41), IOF, then
     * walk the message-buffer chain (LDX ,X 1); for a non-empty link test bit 15
     * (BSKP ONE 170 DA) and clear/mark it (BSET ZRO 170 DA / STA ,X 1).           */
    int *buf = sibas_buffer_head(r->B[011] & 041);
    di();
    walk_message_chain(buf);

    /* 104263-104276: branch on the caller status word (,B 12); on the "message
     * present" path stage the reply record (LDX I 25 / mark bit 15 / STA ,X 1)
     * and set the two status slots (,B -4/-5); else set A=1 and clear ,B -5.     */
    if (r->B[012] != 0)                  /* 104264 JAF 11 -> 104275 */
        deliver_reply_record(r);
    else
        r->B[-5] = 0;                     /* 104276 STZ ,B -5 */

    /* 104277-104302: build the result descriptor (X = D), store at ,X 4,
     * bump the message counter (MIN ,B 7).                                       */
    stage_result(r);                      /* 104300 STA ,X 4 ; 104301 MIN ,B 7      */
    return sibas_return(r);               /* 104303 JMP I 10 -> 104313 (return)     */
}

/* Callers:
 *   MON 305B GetSIBASMessage: semantically mon_get_sibas_message(r).
 *   NOTE: a literal GOTAB[305] follow lands in dispatch_DIA0(r) instead - the
 *   MSIBB<->MON305 link crosses the uncarved CALLPROC bridge (see README).
 */
