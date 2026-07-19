/* ============================================================================
 * MON 16B  GetTerminalType (MGTTY)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 16B-GetTerminalType.ASM). MGTTY is the
 * GET entry into a large shared terminal-service module; its sibling MSTTY (MON
 * 17B) falls through into the SAME body after presetting a mode global. Control
 * flow and register moves are BYTE-VERIFIED; the field/global semantics (which
 * cell is the terminal type, the exact meaning of the mode global at 000072 and
 * the SSPTM bit) are INFERRED - the resident datafield and cell 000072 lie
 * outside the carved window. Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md. Addresses octal.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = logical device number  (the terminal; 1 = own terminal in background)
 *   A = terminal type          (OUTPUT: appendix H type number)
 * Error return: A = error number. */

/* mode_global = mem[000072], reached indirectly through link cell 044775.
 * MSTTY (17B) presets it to 20B for SET; the GET path leaves it at its
 * get-default value. The shared body reads it to locate the terminal datafield
 * and to drive the resident terminal primitive.  (semantics INFERRED) */

int mon_getterminaltype(mon_regs *r)
{
    /* --- 044630 MGTTY get-entry: mark get-mode discriminator nonzero --- */
    int discriminator = mem[044777];      /* 044630/044632: LDA 147 (mode const) */
    mem_indirect_store(045000, discriminator); /* 044631: STA I 147               */
    r->Bframe[0164] = discriminator;      /* 044633: STA ,B 164 (set/get flag)   */
    /* 044634 JMP 3 -> 044637 (the 044635-044636 flag-clear prologue is the
     * STMT1 sibling's entry, not reached from MGTTY) */

    /* --- 044637 locate the terminal datafield (page-map protect bracketed) --- */
    prot_off();                           /* 044637: BSET ZRO SSPTM              */
    int *df = terminal_datafield(mem[000072]); /* 044640/044641: LDX I 135 / LDX I ,X 141 */
    save(-31, df);                        /* 044642: STX -31                     */
    r->A = df[1];                         /* 044643: LDA ,X 1                    */
    prot_on();                            /* 044644: BSET ONE SSPTM              */
    r->A = term_helper(df);               /* 044645: JPL I 136 -> [045003]=042146 */
    r->X = r->A;                          /* 044646: RADD CLD SX DA              */

    prot_off();                           /* 044647 */
    df = load(-37);                       /* 044650: LDX -37                     */
    r->T = df[0];                         /* 044651: LDT ,X 0                    */
    prot_on();                            /* 044652 */
    if (r->A == 0 || r->A == r->T)        /* 044653 JAZ / 044654 SKP UEQ / 044655 */
        resident_routine();               /* 044656: JPL I 120 -> [044776]=000215 */

    r->Bframe[-050] = r->A;               /* 044657: STA ,B -50                  */
    r->X = r->A;                          /* 044660: RADD CLD SA DX              */

    /* --- 044661 set/get fork: GET skips the datafield WRITE --- */
    if (r->Bframe[0164] != 0) {           /* 044661/044662: LDA ,B 164 / JAZ 4   */
        /* SET path (used by MON 17B) - not taken on the GET path */
        df[027] |= 1;                     /* 044663-044665: LDA/BSET ONE 0/STA   */
    }

    /* --- 044666.. read back the terminal datafield words --- */
    prot_off();                           /* 044666 */
    df = load(-56);                       /* 044667: LDX -56                     */
    save(-57, df[3]);                     /* 044670/044671: LDA ,X 3 / STA -57   */
    r->A = df[2];                         /* 044672: LDA ,X 2                    */
    prot_on();                            /* 044673 */
    r->Bframe[0103] = r->A;               /* 044674: STA ,B 103                  */
    prot_off();                           /* 044675 */
    r->A = df[1];                         /* 044676: LDA ,X 1                    */
    prot_on();                            /* 044677 */

    /* --- 044705 T1P04: compute the terminal-type index (32-bit divide) --- */
    r->D = mem[045006];                   /* 044705/044706: LDA 101 / D = A      */
    r->A = 0;                             /* 044707: RADD CLD 0 DA               */
    /* 044710 RDIV ST: A = (A:D)/T ; D = remainder  (per semantics 3.7)         */
    rdiv_ST(r);
    if (r->D == 0)                        /* 044711 SKP DD UEQ 0 / 044712 / 044713 */
        resident_routine();               /* 044713: JPL I 63 -> [044776]=000215 */

    store_type_index(r->A);               /* 044714/044715: SAX 0 / STA I ,X 72  */
    r->A *= mem[045010];                  /* 044717: MPY 71                      */
    r->A += r->Bframe[0164];              /* 044720: ADD ,B 164                  */
    /* 044721.. scatter the computed fields into the terminal datafield          */

    /* --- 044751.. dispatch a shared terminal-message helper --- */
    if (terminal_needs_kick(r)) {         /* 044751-044757: LDA/LDT/SKP forks    */
        r->A = 0; r->D = 0;               /* 044762/044763                       */
        do {
            term_message_helper();        /* 044764: JPL I -152 -> [044612]      */
            resident_routine();           /* 044765: JPL I 11 -> [044776]=000215 */
        } while (1);                      /* 044766: JMP -2 (in-body wait loop)  */
    }
    /* 044767-044773 finalize; 044774 JMP 22 -> 045016 (PL010) continues into the
     * shared-module second phase, which is PAST this carved window. */
    goto shared_module_continuation;      /* 044774: JMP 22 -> 045016 (UNVERIFIED tail) */
}

/* Byte-verified anchors:
 *   MGTTY entry 044630, get/set discriminator B+164 (044633/044661),
 *   SSPTM protect-bracketed datafield access (044637..044677),
 *   RDIV type-index compute at T1P04 044705-044710, MPY 044717,
 *   term-message helper JPL I -152 -> [044612] (044764) with wait loop 044766.
 *
 * The MON 16B -> MGTTY link is NOT byte-proven: GOTAB[16] = 000000 (a
 * fall-through, no per-call vector); dispatch is routed by the resident
 * MFELL/CALLPROC, which lives in an uncarved overlay. Attribution rests on the
 * MGTTY symbol name and its adjacency + shared body with MSTTY (MON 17B). The
 * 045016 (PL010) continuation and cell 000072 / SSPTM semantics are UNVERIFIED. */
