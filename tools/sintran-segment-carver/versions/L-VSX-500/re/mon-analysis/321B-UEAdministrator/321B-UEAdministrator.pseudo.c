/* ============================================================================
 * MON 321B - UEADM / UEAdministrator - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 321B-UEAdministrator.ASM.
 * Dispatch chain and the worker's control flow (selector range-check, physical
 * user-table read, computed jump table) are VERIFIED from bytes; the meaning of
 * each of the 8 sub-functions is INFERRED from structure.
 *
 * Dispatch: MON 321B -> ENT14 072167B -> GOTAB[321B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[321B] @006141B = 065453B = UEADM (worker below).
 * All constants octal.
 * ============================================================================
 */

/* Frame slots (B-relative), from the carved bytes: */
#define F_SELECTOR (-0170)   /* ,B -170 : the sub-function selector (1..8)     */
#define F_PARAMBASE(-0166)   /* ,B -166 : caller parameter block base (X:=B)   */
#define F_LINK     (-0167)   /* ,B -167 : saved return link (L)                */
#define F_TSAVE    (-0200)   /* ,B -200 : staged operand / caller T            */

#define ERR_ILLEGAL_PARAM 0124   /* stored into param[12] when selector out of range */

void UEADM(word *param)                     /* entry 065453B, param base in B */
{
    frame[F_PARAMBASE] = (word)param;       /* 065456B: STX ,B -166 */
    frame[F_LINK]      = saved_L;           /* 065460B */

    int sel = param[012];                   /* 065461B: LDA ,X 12  (sub-function selector) */

    /* 065462B-065467B: range check sel in [1..8]. */
    if (sel < 1 || sel > 8) {               /* out of range */
        param[012] = ERR_ILLEGAL_PARAM;     /* 065470B-065471B */
        return_error();                     /* 065472B: JMP I 123 */
    }
    frame[F_SELECTOR] = sel;                /* 065474B */

    /* 065475B-065507B: stage operands and call the user-table helper. */
    frame[F_TSAVE] = param[044];            /* 065477B-065500B */
    user_table_helper();                    /* 065507B: JPL I 111  (-> @065620B) */

    if (sel == 010)                         /* 065511B-065514B: selector 8 -> early return */
        return_ok();                        /* JMP I 105 */

    /* 065515B-065524B: pull two bit-fields out of param[17]. */
    int field_hi = (param[017] >> 7)  & 0103;   /* 065515B-065520B */
    int field_lo = (param[017] >> 13) & 0077;   /* 065521B-065524B (>>11 decimal) */
    frame[-0173] = field_hi;
    frame[-0172] = field_lo;

    /* 065525B-065534B: index a per-user table and PHYSICALLY read an entry. */
    unsigned idx = frame[F_TSAVE] * 074;    /* 065527B: MPY 74  (entry size 074) */
    word entry = phys_read(user_table_base + idx);  /* 065532B-065533B: LDT I 73 / LDATX */

    /* 065535B-065577B: compare/update the entry against the extracted fields. */
    /* ... (per-sub-function detail; INFERRED) ... */

    /* 065600B-065612B: computed jump table - P := P + selector - one arm per
     * sub-function, via the pointer table at 065615B-065640B. */
    switch (sel) {                          /* 065601B: RADD SA DP */
        /* each arm performs its sub-function then falls to the common tail. */
        default: break;                     /* INFERRED: 8 arms, semantics not byte-proven */
    }

    /* 065641B: common tail - reload saved registers and return via F_LINK. */
    return_ok();
}
