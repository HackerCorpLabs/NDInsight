/* ============================================================================
 * MON 304B  SendSIBASMessage (MAPSI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Two distinct code paths are involved; keep them straight:
 *
 *  (1) GOTAB[304]=112065B (BYTE-PROVEN by prove-mon.py) dispatches to DSI0/XBUSA
 *      in the DSI/DIA block. DSI0 is a 2-word stub that jumps into the DIA0
 *      body (111771B). That body decodes as RT-level driver code (it issues
 *      MON 131/64/116/134), which is INCONSISTENT with a level-14 handler for
 *      a MON# this high - so whether GOTAB[base]+304 is truly the level-14
 *      target here is UNVERIFIED. Modelled below as dispatch_DSI0().
 *
 *  (2) MAPSI=103675B is the clean SIBAS message handler (the semantic
 *      "SendSIBASMessage"). It is real SINTRAN L bytes but is UNREACHABLE from
 *      GOTAB[304] and from every GOTAB slot 0..377; it is reached only via the
 *      uncarved resident CALLPROC second-level dispatch. Modelled below as
 *      mon_send_sibas_message().
 *
 * Control flow is BYTE-VERIFIED from 304B-SendSIBASMessage.ASM; the semantic
 * labels (which word is ZTREG/ZXREG/ZAREG, *MOVAP/*MOVPA) are INFERRED from the
 * NPL SUBR MAPSIB,MSIBB,TISIBB structure - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* --- Path (1): the byte-proven GOTAB[304] target (DSI0 -> DIA0 body) -------- */
/* Real bytes, RT-level driver semantics UNVERIFIED. Provided so an emulator
 * that follows GOTAB literally lands somewhere sane. */
void dispatch_DSI0(mon_regs *r)          /* 112065 DSI0/XBUSA */
{
    /* 112065 ION ; 112066 JMP -75 -> 111771 (into DIA0 body) */
    int st = r->B[033];                  /* 111771 LDT ,B 33  - status/flags word */

    ei();                                 /* 112016 ION  */
    abstr_data_transfer(r);               /* 112021 MON 131 (ABSTR) - block xfer  */
    di();                                 /* 112023 IOF  */

    if (/* error path 112077.. */ 0) {    /* 112035 JMP 42 -> 112077 on DD<=0     */
        warning_message();                /* 112106 MON 64  (ERMSG)  */
        unfix_segment(013);               /* 112111 MON 116 (UNFIX)  */
        exit_rt_program();                /* 112112 MON 134 (RTEXT)  */
    }
}

/* --- Path (2): the semantic SIBAS handler (MAPSI/MSIBB/TISIB) --------------- */
/* Parameter slots per the NPL contract (INFERRED mapping to ,B offsets):
 *   ZTREG = SIBAS device number   (,B 11)
 *   ZXREG = address of message    (length in word S1, max 177*4)
 *   ZAREG = result: 0 OK; -1/-2/-3 errors; 174 bad size   (,B 12) */
int mon_send_sibas_message(mon_regs *r)  /* 103675 MAPSI */
{
    set_level(4);                        /* 103676 SAA 4 (MLEV) ; 103677 MCL PIE */

    int dev = r->B[011];                 /* 103706 LDA ,B 11  = ZTREG            */
    if (dev > MXSIB)                      /* 103710 SKP IF DT MGRE SA (MXSIB=150) */
        return set_result(r, -1);         /* 103711 JMP I -> 104060 error exit    */

    /* 103712-103763: locate SIBAS device block, validate/allocate buffer.      */
    sibas_reactivate(dev);                /* schedule the SIBAS server           */

    /* 104037-104052: copy request message into RT-common (*MOVAP-style MOVEW). */
    move_msg_to_rtcommon(r->X /*ZXREG*/, dev);   /* 104051 MOVEW */

    /* Block until the SIBAS server answers (5WAIT), then copy the answer back  */
    /* into the caller buffer (*MOVPA-style MOVEW at 104170).                   */
    wait_for_sibas_reply(dev);
    move_reply_to_caller(dev, r->X);      /* 104170 MOVEW */

    /* Result codes staged at 104207-104216 before STA ,B 12 (ZAREG):
     *   174 = bad message size ; -1/-2/-3 = SIBAS errors ; 0 = OK.            */
    return set_result(r, 0);              /* 104216 STA ,B 12 ; 104220 return    */
}

/* Callers:
 *   MON 304B SendSIBASMessage: semantically mon_send_sibas_message(r).
 *   NOTE: a literal GOTAB[304] follow lands in dispatch_DSI0(r) instead - the
 *   MAPSI<->MON304 link crosses the uncarved CALLPROC bridge (see README).
 */
