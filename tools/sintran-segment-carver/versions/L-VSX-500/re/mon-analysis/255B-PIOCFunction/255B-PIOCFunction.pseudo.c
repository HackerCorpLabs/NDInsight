/* ============================================================================
 * MON 255B  PIOCFunction  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Parallel-I/O-Controller (PIOC) function call.
 *
 * Derived from the real disassembly (see 255B-PIOCFunction.ASM). The modelled
 * body is the carved worker PIOCM (114120B) in segment 025-S3IRPIT. Control
 * flow, the function-code range checks, the cross-bank jump-table dispatch, and
 * the two documented error codes (-24B, -32B) are BYTE-VERIFIED. The names of
 * the caller descriptor fields (X+10 / X+11 / B+12) are INFERRED from the access
 * pattern - treat as a model, not gospel. Addresses in comments are octal.
 *
 * IMPORTANT (see README "Honest caveats"): GOTAB[255B] = 000000, a level-14
 * fall-through, so there is no stored dispatch address to follow. The
 * MON 255 -> PIOCM link crosses the uncarved resident CALLPROC / MFELL bridge,
 * which is in no carved segment. So this models the identified worker, not a
 * statically proven path. Identity rests on the symbol name PIOCM and on the
 * two status codes matching the manual (-24B = illegal function code,
 * -32B = illegal LDN).
 *
 * Every instruction is translated against the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md :
 *   RADD CLD Ss Dd = COPY (d := s); RADD Ss Dd = (d := d + s);
 *   SAA/SAT n = signext8 immediate; AAA n = A += signext8(n);
 *   SKP IF DA EQL/UEQ/GRE ST = signed compare A vs T (skip on true);
 *   SKP IF DT MLST SA = unsigned compare (T < A) then skip;
 *   LDA I ,X d = A := mem[X + mem[P+d]]; STX I d / STA I d = store through mem[P+d];
 *   BSET ZRO/ONE SSPTM = clear/set the page-table (bank-select) STS bit;
 *   MST PIE = PIE |= A; IOF = interrupts off; JAF = branch if A != 0.
 * ============================================================================ */

/* PIOC function codes (caller passes FuncNo in T per the manual; by the time
 * PIOCM runs, the marshalled parameters live in the caller block reached via X):
 *   0 Reserve slot   1 Release slot   2 Send to PIOC   3 Read from PIOC
 *   4 Load segment   5 Unload segments 6 Start PIOC    7 Stop PIOC
 * Status codes returned (octal): 1 ok, -24 illegal function code,
 *   -32 illegal LDN, ... (full list in the parameter YAML).                    */

int mon_255_piocm(mon_regs *r)              /* PIOCM @114120B */
{
    save_cell_X = r->X;                     /* 114120 STX I 76  -> resident 007253 */
    save_cell_B = r->B;                     /* 114121-114122 A:=B; STA I 75 -> 007254 */

    /* 114123-114127: a function/slot word of -1 is a fast-path escape to the
     * sibling routine EXEL (114242B).                                          */
    if (mem[r->X + 011] == -1)              /* 114123 LDA ,X 11; 114124 SAT -1; 114125 EQL */
        goto EXEL;                          /* 114127 JMP I 71 -> ptr 114242 (EXEL)  */

    /* 114130-114131: hand a second parameter word to a resident helper.        */
    r->A = mem[r->X + 010];                 /* 114130 LDA ,X 10                      */
    resident_010376(r);                     /* 114131 JPL I 70 -> ptr 010376         */

    /* 114132-114136: validate the helper result; a bad result returns the
     * documented "illegal LDN" status through the common PIRET epilogue.        */
    if (r->A == 0) r->A = r->D;             /* 114132 JAF; 114133 RADD CLD SD DA     */
    if (r->A == 0) {                        /* 114134 JAF (still zero)               */
        r->A = -032;                        /* 114135 SAA -32  (status = illegal LDN) */
        goto PIRET;                         /* 114136 JMP I 64 -> ptr 114207 (PIRET) */
    }

    /* 114137-114146: a second gate - a device/state word at B+12 must equal
     * 173B, else illegal LDN.                                                   */
    r->B = r->A;                            /* 114137 RADD CLD SA DB                 */
    mem[P_local(-021)] = r->A;              /* 114140 STA -21                        */
    if (mem[r->B + 012] != 0173) {          /* 114141 LDA ,B 12; 114142 SAT 173; UEQ */
        r->A = -032;                        /* 114145 SAA -32                        */
        goto PIRET;                         /* 114146 JMP I 55 -> ptr 114207 (PIRET) */
    }

    /* 114147-114154: split on the function code magnitude.                     */
    r->A = mem[r->X + 011];                 /* 114147 LDA ,X 11 (function code)      */
    if (r->A >= 020) {                      /* 114150 SAT 20; 114151 GRE             */
        goto range_hi;                      /* 114153 JMP -> 114175                  */
    }
    resident_000374(r);                     /* 114154 JPL I 50 -> ptr 000374         */

    /* 114155-114167: low-range setup. Enable the PIOC interrupt bits, re-read
     * the function code through a fresh block base, and bound it to 10B; an
     * out-of-range code returns "illegal function code".                       */
    r->A = 4;  PIE |= r->A;                 /* 114155 SAA 4; 114156 MST PIE          */
    r->A = mem[P_local(-040)];              /* 114157 LDA -40                        */
    r->B = r->A;                            /* 114160 RADD CLD SA DB                 */
    r->X = mem[/*@114161 indirect*/ 035];   /* 114161 LDX I 35 (block base)          */
    r->A = mem[r->X + 011];                 /* 114162 LDA ,X 11 (function code)      */
    if (!( (unsigned)010 < (unsigned)r->A )) {  /* 114163 SAT 10; 114164 MLST (T <u A) */
        r->A = -024;                        /* 114166 SAA -24 (illegal function code) */
        goto PIRET;                         /* 114167 JMP I 36 -> ptr 114207 (PIRET) */
    }

    /* 114170-114174: cross-bank jump-table dispatch. Toggle to the base bank,
     * read the sub-function handler address from the table at base 114106B
     * indexed by the (scaled) function code, restore the bank, and jump to it.  */
    select_base_bank();                     /* 114170 BSET ZRO SSPTM (*1BANK)        */
    r->X = r->A;                            /* 114171 RADD CLD SA DX                 */
    handler = mem[r->X + mem[/*114106*/]];  /* 114172 LDA I ,X 34 (table @114106B)   */
    restore_alt_bank();                     /* 114173 BSET ONE SSPTM (*2BANK)        */
    goto *handler;                          /* 114174 RADD CLD SA DP  (P := A)       */

range_hi:                                   /* 114175: entered when func >= 20B      */
    r->A -= 020;                            /* 114175 AAA -20                        */
    if (!(2 < (int)r->A)) {                 /* 114176 SAT 2; 114177 LST (signed T<A)  */
        r->A = -024;                        /* 114201 SAA -24 (illegal function code) */
        goto PIRET;                         /* 114202 JMP I 25 -> ptr 114207 (PIRET) */
    }
    /* 114203-114206: a computed relative jump (P := P + A) selects one of three
     * resident sub-workers (ptr cells 114230/114231/114232 = 115532/115527/115642). */
    P += r->A;                              /* 114203 RADD SA DP                     */
    /* one of: */ resident_115532(r); resident_115527(r); resident_115642(r);

PIRET:                                      /* 114207: common return / error exit    */
    r->X = mem[/*@114207 indirect*/ 007];   /* 114207 LDX I 7 (return block base)     */
    r->B = r->X;                            /* 114210 RADD CLD SX DB                 */
    r->X = mem[r->B + 1];                   /* 114211 LDX ,B 1                       */
    mem[r->B + 011] = r->A;                 /* 114212 STA ,B 11 (Status -> caller)   */
    interrupts_off();                       /* 114213 IOF                            */
    resident_010610(r);                     /* 114214 JPL I 17 -> ptr 010610         */
    resident_010341(r);                     /* 114215 JPL I 17 -> ptr 010341         */
    return r->A;                            /* status word (caller Status parameter)  */

EXEL:                                       /* 114242B - sibling routine (outside carve) */
    return exel(r);
}

/* Resident pointer targets embedded in the link-cell table (outside the carved
 * 114120B..114241B window, NOT resolved to names here):
 *   010376, 000374, 115532, 115527, 115642, 010610, 010341
 * plus the two save cells 007253 (X) / 007254 (B) and the jump-table base 114106B.
 */
