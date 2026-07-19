/* ==========================================================================
 * MON 510B - CallSwapper (SWMC)  pseudo-C model for an emulator author.
 *
 * Source: real SINTRAN L bytes, 026-S3IMPIT.bin (S3MPIT), entry 142153B (SWMC),
 * load base 32000B.  ND-500 level-12 call: dispatched via the level-12 GOSW table
 * (uncarved), NOT the ND-100 GOTAB. The handler itself is ND-100 code and its
 * control flow is byte-verified against 510B-CallSwapper.ASM.
 *
 * Every instruction below is translated per
 *   ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md
 * Control flow (142153B..142166B) is byte-verified. The trap sub-code meaning is
 * INFERRED - marked inline. All addresses in comments are octal. No unicode below.
 * ========================================================================== */

/* Inline data words the body indirects/indexes through (142167B..142172B).
 * nd100-dis renders these four words as instructions; the body uses them as data. */
#define PTR_BANK  0004654   /* 142167B: pointer word; LDT I 11 loads T = mem[this] */
#define MASK377   0000377   /* 142170B: 8-bit mask; USED by the AND at 142161B     */
#define P_5ACTS   0145162   /* 142171B: -> 5ACTSWAPPER (resident swapper)          */
#define P_NXTMS   0135067   /* 142172B: -> NXTMSG (next-message dispatch loop)     */

/* mem[] is the ND-100 word address space of the resident level-12 driver.
 * A, D, T, X model the ND-100 CPU registers; the call arrives on level 12 with X
 * holding the message context index.
 *
 * The LDATX / STATX instructions are T/X-indexed PHYSICAL transfers: they form a
 * 24-bit PHYSICAL address that BYPASSES the MMU / page tables - grounded in
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md section 5:
 *     EL = ((T & 0377) << 16) | ((X + disp3) & 0177777)
 * with disp3 = (opcode >> 3) & 7 = 0 in both transfers here. T is the BANK (high
 * byte), X the word offset. T is loaded from mem[004654B] - the ND-500 message
 * buffer bank - so LDATX/STATX read/write the message buffer at physical
 * bank:offset. phys[] is that 24-bit physical space. */
#define ELADDR(t, x)  ( (((t) & 0377) << 16) | ((x) & 0177777) )   /* 24-bit physical */

void mon_510B_SWMC(void)
{
    int A, D, T, X;              /* ND-100 CPU registers (X = entry msg index)     */

    /* 142153B..142155B: build the swapper trap seed (high byte) in D. */
    A = 027;                     /* SAA 27  : A = 027 (SWMC trap sub-code, meaning INFERRED) */
    A = A << 010;                /* SHA ZIN 10 : A <<= 8, zero fill  => A = 013400  */
    D = A;                       /* RADD CLD SA DD (COPY SA DD) : D = A = 013400     */

    /* 142156B: T := message-buffer bank (indirect through ptr word 142167B). */
    T = mem[ PTR_BANK ];         /* LDT I 11 : T = mem[ mem[142167B] ] = mem[004654B] */

    /* 142157B..142163B: read one PHYSICAL message word, keep its low 8 bits,
     * merge the seed, and write the composed trap code back. */
    X = X + 016;                 /* AAX 16  : index into the message buffer         */
    A = phys[ ELADDR(T, X) ];    /* LDATX   : PHYSICAL read (MMU-bypass)            */
    A = A & mem[0142170];        /* AND 7   : P-relative; mem[142170B]=0377 => A &= 0377 (low 8 bits) */
    A = A + D;                   /* RADD SD DA : A = A + D  => 013400 | (field & 0377) */
    phys[ ELADDR(T, X) ] = A;    /* STATX   : PHYSICAL write of the composed trap code */
    X = X - 016;                 /* AAX -16 : restore X                             */

    /* 142165B: activate the resident swapper as a subroutine. */
    call((( P_5ACTS )));         /* JPL I 4 -> 5ACTSWAPPER = 145162B (L := 142166B)  */

    /* 142166B: tail-jump into the next-message dispatch loop (no return). */
    goto_addr((( P_NXTMS )));    /* JMP I 4 -> NXTMSG = 135067B                     */
}

/* Notes for the emulator:
 *  - The composed trap code is (027 << 8) | (message_word & 0377): an 8-bit field
 *    ORed under a fixed high seed. This matches the README "MSM510<<8 | low 8 bits"
 *    description; the earlier "keep low 3 bits" note was wrong (the AND masks with
 *    mem[142170B]=0377, i.e. 8 bits). The 027 seed is a VERIFIED byte; its exact
 *    role as the swapper sub-code is INFERRED (note MSWMC=014B does not equal 027).
 *  - No skip-return / error code is set in this window: exit is an unconditional
 *    tail-jump to NXTMSG (VERIFIED).
 *  - The message-buffer bank (mem[004654B]) and the +16 word index are read from the
 *    bytes; their precise field layout is INFERRED, not independently proven.
 *  - 142167B..142252B are DATA (pointer word + mask + two worker pointers, then an
 *    unlabelled table); nd100-dis renders them as instructions - disregard those.
 */
