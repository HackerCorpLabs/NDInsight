/* ============================================================================
 * MON 14B  ClearOutBuffer (COBUF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Shares one body with MON 13B ClearInBuffer (CIBUF).
 *
 * Derived from the real disassembly (see 014B-ClearOutBuffer.ASM). Control flow
 * is BYTE-VERIFIED; the semantic labels (which datafield cell is the output-
 * buffer pointer, the drain routine at 044222B) are INFERRED from the code
 * structure - treat as a model, not gospel. Addresses in comments are octal.
 *
 * NOTE: the MON 14B -> COBUF dispatch link is NOT byte-proven; GOTAB[14]=000000
 * is a fall-through, so no stub address points at 044125B. See README "Honest
 * caveats". This models the worker body only, whose attribution rests on the
 * COBUF symbol name + behaviour.
 * ============================================================================ */

/* Entry 044125 = COBUF (output ring, MON 14B) - one word into the shared body.
 * Entry 044120 = CIBUF (input ring, MON 13B).
 * The per-iteration entry is loaded by LDD ,X 0 (044115): A = word0 = mem[tab],
 * D = word1 = mem[tab+1]  (LDD low word -> A, high word -> D; ref sec 2.2). The
 * selector/sentinel/bit tests read A (word0); the accumulated value derives from
 * D (word1). Bare LDT/AND with a bare displacement are P-relative mem[P+disp]
 * (ref sec 1), NOT literals; several land past the carved window (UNVERIFIED). */
void mon_clear_buffer(mon_regs *r)
{
    /* 044113-044117: prologue - base into device/buffer table, atomic read */
    word *tab = deref(r->B - 0200);        /* 044113 LDX ,B -200 : X = mem[B-0200] */

    for (;;) {                             /* 044150/044151: AAX 2; JMP -35 loop  */
        dbl entry = atomic_load2(tab);     /* 044114-044116: POF; LDD ,X 0; PON   */
        word T = 0177777;                  /* 044117 SAT -1 : T = -1              */

        if (A_of(entry) == T)              /* 044120 SKP IF DA UEQ ST: compares A  */
            break;                         /* 044121 JMP 31 -> 044152 (tail)      */

        if (A_of(entry) & (1 << 14)) {     /* 044122 BSKP ONE 14 DA (bit 160>>3)  */
            /* output-side clear (COBUF path, MON 14B entry @044125) */
            word a = D_of(entry) & MASK_044220; /* 044124 A=D ; 044125 AND mem[044220B] */
            T = MEM_044221;                     /* 044126 LDT 73 = mem[044221B]        */
            if ((unsigned)a < (unsigned)T) {    /* 044127 SKP IF DA MLST ST: body if a <u T */
                r->cnt_54 += a;             /* 044131-044132: ADD/STA ,B -54       */
                if (carry_set())            /* 044133 BSKP ONE 6 STS (C, from the add) */
                    drain_buffer(0044222);  /* 044135 JPL I 65 -> ptr@044222 (indir) */
            }
        } else if (A_of(entry) & (1 << 13)) { /* 044137 BSKP ONE 13 DA (bit 150>>3) */
            /* input-side clear (CIBUF path, MON 13B) */
            word a = D_of(entry) & MASK_044220; /* 044141 A=D ; 044142 AND mem[044220B] */
            r->cnt_16 += a;                 /* 044143-044144: ADD/STA ,B -16       */
            if (carry_set())                /* 044145 BSKP ONE 6 STS (C, from the add) */
                drain_buffer(0044222);      /* 044147 JPL I 53 -> ptr@044222 (indir) */
        }
        tab += 2;                          /* 044150 AAX 2 - next 2-word entry     */
    }

    /* 044152-044156: tail - LDA 51/LDX 51/LDA 44 read P-relative words at
     * 044223B/044224B/044221B (all OUTSIDE the carved 044113..044156 window -
     * UNVERIFIED), then STA ,X 14 / LDT ,X 15 stage the exit. */
    exit_store_status(r);   /* targets out of carve - UNVERIFIED */
}

/* MASK_044220 = mem[044220B] (operand of AND 73 @044125 and AND 56 @044142, both
 * P-relative to the SAME word); MEM_044221 = mem[044221B] (operand of LDT 73
 * @044126). Both lie just past the carved worker window, so their values are
 * UNVERIFIED (out of carve) - named here, not invented. carry_set() = STS bit 6
 * (C), left by the preceding ADD. */

/* Callers (via the uncarved resident CALLPROC fall-through, GOTAB[n]=0):
 *   MON 14B ClearOutBuffer: enter at COBUF (044125B), output ring.
 *   MON 13B ClearInBuffer:  enter at CIBUF (044120B), input ring.
 * The shared per-buffer drain routine at 044222B is an indirect JPL I target,
 * outside the carved window - NOT modelled here. */
