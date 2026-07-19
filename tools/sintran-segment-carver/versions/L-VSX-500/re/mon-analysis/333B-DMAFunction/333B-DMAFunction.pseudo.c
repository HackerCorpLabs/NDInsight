/* ============================================================================
 * MON 333B  DMAFunction (UDMA)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  DIA11 entry stub 025-S3IRPIT @112561B ;
 * UDMA worker body 025-S3IRPIT @110770B..111120B.
 *
 * Derived from the real disassembly (see 333B-DMAFunction.ASM). The DIA11
 * register-block save + TRR PCR context switch, and the UDMA function-code
 * decoder (which matches FuncCode against 0,1,2,3,7,20,21,24,54,55,56,57,...
 * and sets control-word bits), are BYTE-VERIFIED. Which device control bit
 * corresponds to which DMA operation, and the caller-side parameter list, are
 * INFERRED from the DMAFunction manual (ND-860228.2 EN, page 160).
 *
 * NOTE ON DISPATCH: GOTAB[333] = 112561B routes to the DIA11 stub. The stub
 * saves the caller register block, then UDMA runs after a page-context switch
 * (TRR PCR) whose second-level bridge (resident CALLPROC) is in an uncarved
 * overlay. So "DIA11 reaches UDMA" is INFERRED from the symbol + the function
 * codes that match the manual, not a followed pointer. Register roles X (saved
 * register block) and B (per-call DMA datafield) are inferred from the access
 * pattern. Addresses in comments are octal.
 * ============================================================================ */

/* 112561-112615 (DIA11): DMA-device level-14 entry stub. */
void mon_dmafunction_entry(mon_regs *r, reg_block *X, dma_field *B)
{
    interrupts_off();                  /* 112561 IOF                             */
    set_page_control_register(r->A);   /* 112562 TRR PCR   context switch        */
    X = (reg_block *)mem[0112661];     /* 112563 LDX I 76  worker block pointer   */
    X->slot17 = r->A;                  /* 112564 STA ,X 17 save caller A          */
    interrupts_on();                   /* 112565 ION                             */
    /* 112606-112613 stage param (,B 20) and MON number into the block, switch
     * context again, then UDMA runs after the resident bridge (uncarved). */
    mon_dmafunction_worker(X, B);
}

/* 110770-111120 (UDMA): DMA function-code decoder -> device control word D. */
int mon_dmafunction_worker(reg_block *X, dma_field *B)
{
    int func;
    unsigned dctl;                     /* built in register D */

    dma_setup(X, B);                   /* 110770-110772 JPL I 131/130 setup       */
    if (mem[deref(0111123)] /*A*/ == 0)/* 110773 SKP IF DA UEQ 0 ; 110774 JMP I   */
        return dma_error(X, B);

    set_interrupt_enable(4);           /* 111003-111004 SAA 4 ; MST PIE           */
    /* 111011 LRB / 111013 SRB : copy an 8-word register block (P,X,T,A,D,L,STS,B)
     * to/from the alt page table at X - stages the DMA descriptor. */
    stage_dma_descriptor(X);

    dctl = 0;                          /* 111016-111017 SAA 0 ; RADD CLD SA DD    */
    func = B->func;                    /* 111020 LDA ,B 20  FuncCode              */

    /* 111021-111120: match FuncCode and set the corresponding control bits in D.
     * Function codes are the manual's DMA functions (page 160):
     * (bit number = the nd100-dis printed field >> 3):
     *   0             receive DMA            -> D bit0, bit2           (111022-111023)
     *   1             send DMA               -> D bit1                 (111030)
     *   2,3           receive/send no-wait   -> D bit3                 (111040)
     *   20,24         read status / clear    -> D bit2, bit4           (111050-111051)
     *   54,56         PIO device (even)      -> D bit5                 (111061)
     *   55,57         PIO device (odd)       -> D bit6                 (111071)
     *   7,62          test mode              -> D bit2                 (111101)
     *   21,64,65,70   -> indirect worker via [111130]                 (111120)
     * The exact meaning of each control bit is INFERRED; the FuncCode literals
     * and the bit-set instructions are byte-verified. */
    if (func == 0)               { dctl |= (1<<0) | (1<<2); }
    else if (func == 001)        { dctl |= (1<<1); }
    else if (func == 002 || func == 003) { dctl |= (1<<3); }
    else if (func == 020 || func == 024) { dctl |= (1<<2) | (1<<4); }
    else if (func == 054 || func == 056) { dctl |= (1<<5); }
    else if (func == 055 || func == 057) { dctl |= (1<<6); }
    else if (func == 007 || func == 062) { dctl |= (1<<2); }
    else if (func == 021 || func == 064 || func == 065 || func == 070)
        return dma_indirect_worker(X, B, mem[0111130]); /* 111120 JMP I 10        */
    /* else: unrecognised FuncCode -> merge with no bits set (111117)             */

    /* 111131 (shared tail): store the built control word to the caller field. */
    B->control = dctl;                 /* 111131 RADD CLD SD DA (A=D) ; 111132 STA ,B 35 */
    return B->control;
}

/* Caller (from the manual, ND-860228.2 EN page 160):
 *   MON 333B DMAFunction: T = logical device number of a DMA channel;
 *   A = address of parameter list (FuncCode, DataAddress, InPara, OutPara);
 *   OutPara / ErrCode returned. The exact register staging into ,B slots is
 *   set by the caller-side wrapper, upstream of this carve.
 */
