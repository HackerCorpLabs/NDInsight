/* ============================================================================
 * MON 500B  StartProcess (STAPROC)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  ND-500 level-12 call.
 * STAPR and SWITP (SwitchProcess) share this SAME entry word (140356B);
 * NSTOP (stop / next-process) is a second entry at 140511B in the same body.
 *
 * Derived from the real SINTRAN L bytes (see 500B-StartProcess.ASM). The CONTROL
 * FLOW (bounds check, index, magic compare, state fork, worker dispatch,
 * skip-return funnel) is BYTE-VERIFIED. The SEMANTIC labels (what each shared
 * worker does, the meaning of mode selectors 13/16, the exact status and error
 * codes) are INFERRED - the worker bodies reached by JPL I are OUTSIDE this
 * carved window. Treat this as a model, not gospel. Addresses are octal.
 *
 * T/X PHYSICAL transfers and the message buffer.
 * The handler reaches the ND-500 MESSAGE BUFFER with the T/X physical transfer
 * instructions (here LDDTX/LDATX/LDXTX/STATX). These form a 24-bit PHYSICAL
 * address that BYPASSES the MMU - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * T is the BANK (high byte, here the ND-500 message-buffer bank loaded via the
 * LDT I pointer words), X the word offset; disp3 (bits 3-5) is 0 in every
 * transfer here - the code adjusts X with AAX instead. phys[] is that 24-bit
 * physical space. Consequences that matter for the model:
 *   - The process number and its descriptor words are READ FROM the message
 *     buffer (LDDTX at 140360, LDATX at 140376, LDXTX at 140400), NOT passed in
 *     ND-100 A/T/X user registers; A/T/X below are level-12 driver-internal.
 *   - RADD CLD S<r> D<r> is a register COPY (dest cleared, source added to 0),
 *     e.g. 140372 RADD CLD SA DX = X := A.
 * ============================================================================ */

#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

int mon_start_process(void)   /* level-12 handler; operands come from phys[] msg buffer */
{
    int pnum;                          /* process number, read from the message buffer  */

    /* 140356-140360: T := message-buffer bank (LDT I 156); X := field index (AAX 100);
     * LDDTX reads the process-number doubleword from the ND-500 message buffer.      */
    pnum = phys[ELADDR(T, X)];         /* 140360 LDDTX: A = phys[EL]; D = phys[EL+1]  */

    /* 140361-140366: two-sided bounds test on pnum against a low/high limit pair.
     *   140361 LDT I 154 (lower); 140362 SKP IF DT LST SA -> skip if lower <  pnum   *
     *   140364 LDT I 152 (upper); 140365 SKP IF DT MGRE SA -> skip if upper >= pnum  *
     * Out of range on either side falls into the reject/return block at 140405.      */
    if (!(lower < pnum && pnum <= upper))          /* INFERRED bound meaning          */
        return proc_reject();                       /* 140366 JMP 17 -> 140405        */

    /* 140367-140400: turn the validated number into a descriptor address
     *   (SUB I 146 normalise, MPY 147 * descriptor-size, ADD 147), copy it to X
     *   (140372 RADD CLD SA DX = X := A), then follow the descriptor:               *
     *   140373 LDX ,X 7; 140374 LDT I 140 (bank); 140375 AAX -3;                     *
     *   140376 LDATX -> A = phys[EL]; 140377 AAX 147; 140400 LDXTX -> X = phys[EL].  */
    /* desc value(s) fetched from the message buffer via LDATX/LDXTX above.           */

    /* 140401-140402: magic / reservation compare (SKP IF DA EQL SD -> skip if A==D).
     * Not equal is the "illegal process" rejection (EILPROC-style).                  */
    if (/* fetched A */ 0 != /* fetched D */ 1)     /* 140402 JMP 3 -> 140405          */
        return proc_reject();

    /* 140403-140404: 140403 LDA ,X 1 loads the state/status field; 140404 JAF 6
     * jumps to 140412 if it is non-zero, else falls into the reject block.           */
    if (/* state */ 0 == 0)                         /* JAF 6 not taken -> 140405       */
        return proc_reject();

    /* --- 140412-140455: active-process main path -------------------------------- */
    /*   140412 LDA ,X 4; 140413 RADD CLD SA DD (D := A); 140414 LDX ,X 7;            *
     *   140415 JPL I 131 (unconditional worker call);                               *
     *   140416 BSKP ONE 60 DD -> skip if D bit6 == 1 (bit 060>>3 = 6). Bit set ->    *
     *   continue at 140420; bit clear -> 140417 JMP 47 -> 140466 alternate path.     */
    if (descriptor_bit6(/* D */ 0)) {               /* 140416 fork on D bit6           */
        sched_worker(ptr131);                       /* 140415 JPL I 131               */
        /* 140420-140425: mode selector (SAT 13 / SKP IF DA UEQ ST, SAT 16 /
         * SKP IF DA EQL ST). A == 13 or A == 16 -> 140426; else -> 140466.           */
        if (mode == 013 || mode == 016) {           /* meaning INFERRED               */
            /* 140426 RADD CLD SA DD (D := A); 140427/140430 JPL I 120 workers;       *
             * 140431 SWAP SB DA (full exchange A<->B); then a SAT-13 compare ladder  *
             * (140433-140455) dispatching JPL I 110/107/106/101, SAA 1, JPL I 102x3. */
            start_workers(/*start_flag=*/1);        /* 140452 SAA 1 before JPL I 102   */
        }
    } else {
        /* --- 140466-140515: alternate / NSTOP tail path ------------------------- */
        /*   140466 JPL I 66; 140470 LDT I 44 (bank); 140471 AAX -1;                  *
         *   140472 LDATX -> A = phys[EL]; 140473 BSET ONE 170 DA -> A |= bit15       *
         *   (bit 0170>>3 = 15); 140474 STATX -> phys[EL] = A (set status bit15);     *
         *   140475 JPL I 62. NSTOP (140511) is the stop/next-process entry that      *
         *   shares this body; 140513-140515 LDT I 21 / AAX -1 / LDATX read another   *
         *   message-buffer word.                                                     */
        phys[ELADDR(T, X)] |= 0100000;              /* 140472/140473/140474 set bit15  */
        alt_or_stop_workers();                      /* JPL I 66/62/44/35/43            */
    }

    /* Return funnel (140405-140411 and 140507-140510): multi-way (normal / skip /
     * error) return through the shared workers at ptr134 / ptr35, which set status
     * and perform the level-12 exit (JPL I / JMP I to the message loop).            */
    return proc_return();
}

/* Callers (same body, different entry / behaviour):
 *   MON 500B StartProcess  : enter STAPR = 140356B
 *   MON 502B SwitchProcess : enter SWITP = 140356B  (same word)
 *   stop / next-process    : enter NSTOP = 140511B
 *
 * NOT MODELLED (outside this carve): the exact status/error codes, the real
 * semantics of mode selectors 13/16, and the bodies of every JPL I worker
 * (14054xB..14056xB). Resolving StartProcess vs SwitchProcess vs Stop precisely
 * requires carving those workers next.
 *
 * Notes for the emulator:
 *  - Every operand is read/written through phys[ELADDR(T,X)] (LDDTX/LDATX/LDXTX/
 *    STATX), i.e. the 24-bit PHYSICAL message buffer, MMU bypassed (reference
 *    ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5).
 *  - The pointer/parameter pool at 14054xB..14056xB is DATA reached only as the
 *    JPL I / JMP I indirect targets; the disassembler renders it as instructions.
 */
