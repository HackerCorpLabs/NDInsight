/* ============================================================================
 * MON 501B  StopProcess (NSTOP)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 level-12 message call.
 *
 * Derived from the real SINTRAN L bytes (see 501B-StopProcess.ASM). The control
 * flow, the SLOCK/SUNLO lock pairing, and the six JPL I pointer-word targets are
 * BYTE-VERIFIED. The semantic labels (which status bit means STOPPED vs
 * clear-and-restart, what the counted loops scan) are INFERRED - treat as a
 * model, not gospel. Addresses in comments are octal.
 *
 * Byte-proven pointer words (140544..140562):
 *   XACTR=145466  OKMON=023025  SLOCK=023706  SUNLO=024041  WN5ST=023670
 *
 * T/X PHYSICAL transfers and the message buffer.
 * The handler reaches the ND-500 MESSAGE BUFFER / process table with the T/X
 * physical transfer instructions (LDATX/STATX/LDDTX/STDTX). These form a 24-bit
 * PHYSICAL address that BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte, loaded via the LDT I pointer words), X the word
 * offset; disp3 (bits 3-5) is 0 in every transfer here - the code adjusts X with
 * AAX. phys[] is that 24-bit physical space. The status word acted on below is
 * READ and WRITTEN with LDATX/STATX at phys[ELADDR(T,X)], not an ND-100 register.
 * ============================================================================ */

#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */
#define BIT15         0100000   /* 0170>>3 = 15 : the status bit BSKP/BSET address     */

/* Handler body carved at 140511B in the resident PIT (026-S3IMPIT, load 32000B). */
void mon_nstop(void)
{
    int status;

    slock();                               /* 140511 JPL I 43 -> SLOCK (023706)     */

    /* 140513-140515: T := status-word bank (LDT I 21); X-=1 (AAX -1);
     * LDATX reads this process' status word from the message buffer.               */
    status = phys[ELADDR(T, X)];           /* 140515 LDATX: A = phys[EL]            */

    /* 140516 BSKP ONE 170 DA -> skip if status bit15 == 1 (bit 0170>>3 = 15).
     * Bit SET  -> skip the 140517 jump, fall into the clear-and-restart arm.
     * Bit CLEAR-> take 140517 JMP 10 -> 140527, the report-only arm.               */
    if (status & BIT15) {                  /* 140516 skip-if-set : bit-SET arm       */
        /* --- 140520..140526: clear the bit, store it back, reactivate ---------- */
        status &= ~BIT15;                  /* 140520 BSET ZRO 170 DA                */
        phys[ELADDR(T, X)] = status;       /* 140521 STATX: store status back       */
        /* 140522 AAX 1 */
        sunlo();                           /* 140523 JPL I 34 -> SUNLO              */
        okmon();                           /* 140524 JPL I 26 -> OKMON              */
        xactr();                           /* 140525 JPL I 17 -> XACTR (reschedule) */
        /* 140526 JMP 5 -> 140533 return                                           */
    } else {
        /* --- 140527..140532: report status, no clear -------------------------- */
        sunlo();                           /* 140527 JPL I 30 -> SUNLO              */
        wn5st(/*mode=*/013);               /* 140530 SAA 13 / 140531 LDX I 11 /     */
                                           /* 140532 JPL I 30 -> WN5ST (023670)     */
    }
    /* 140533 JMP I 12 -> NXTMSG : indirect exit to the level-12 message loop.      */
    return_to_message_loop();

    /* ------------------------------------------------------------------------- *
     * NOT MODELLED IN DETAIL (INFERRED) - the further code in this window:
     *   140563..140731 : a second SLOCK-guarded block (entered as an internal
     *     JPL/JMP target, not fall-through from 140533). It runs counted scans
     *     over a resident ND-500 process/message table using LDDTX stride
     *     (AAX 2) with a SAT -1 sentinel (140612/140625/140706/140720 LDDTX).
     *   140753..141026 : a further CODE block reached by 140731 JMP 22 -> 140753.
     *     It performs message-buffer T/X transfers - phys[ELADDR(T,X)] reads and
     *     writes via LDDTX/STDTX/LDATX/STATX (140755/140757/140772/140776/141001/
     *     141011/141015/141017). This region is EXECUTABLE, not data.
     * The pointer/constant POOLS the JPL I above dereference are the words at
     * 140534..140562 and 140732..140752 (data, rendered as instructions by the
     * disassembler). The table these scans walk is not named by any symbol in
     * the window, so its identity and exact purpose are UNVERIFIED.
     * ------------------------------------------------------------------------- */
}

/* Dispatch (asserted, NOT byte-proven): ND-500 MON 501B arrives via the level-12
 * 5CMNO / GOSW message-code table, not the ND-100 MON GOTAB. See README caveats. */
