/* ============================================================================
 * MON 56B  SetUserParam (PASET)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Sets the 5 user parameters of a background program (termination-handling info).
 * GetUserParam (MON 57B) reads them back. Background programs only.
 *
 * Derived from the real disassembly (see 56B-SetUserParam.ASM), the MPASE worker at
 * 102363B in segment 025-S3IRPIT (a SYMBOL-2-LIST symbol). This is a SHARED body: the
 * SSK skip flag selects set (MPASE, SSK=0, this call) vs get (MPAGE, SSK=1, MON 57B).
 * The two directions differ only in which side of the 5-word MOVUS copy carries the
 * USPAR system array. Control flow is BYTE-VERIFIED; the field meanings match the
 * SINTRAN monitor-call source shape (MPASET/MPAGET). Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[56B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved) which reaches MPASE. So the
 *   MON 56 -> MPASE link is NOT byte-followable statically; identity rests on the
 *   symbol NAME (MPASE = set entry of the set/get user-parameter body) - see README.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  JPL I = indirect call;  SAA n = A:=n;
 *   MST PIE = mask-set PIE;  BSKP ONE SSK = skip if skip-flag set;  RADD CLD SA DD =
 *   register copy (D=A);  LDA/LDT ,B n = frame loads;  SAX n = X:=n;  JMP I = indirect
 *   return.
 * ============================================================================ */

/* Manual register contract (from the MON 56 description, INFERRED - not byte-proven):
 *   PASET(APAR): a 5-word array of user parameters (16-bit on ND-100). The B-frame
 *   word B+20 (D0) is the array pointer; USPAR is the system-side parameter array. */

#define USPAR 0145445                              /* 102410: constant used by both dirs */

int mon_56B_SetUserParam(mon_regs *r, int ssk_entry) /* ssk_entry: 0 = MPASE (set), 1 = MPAGE (get) */
{
    int SSK = ssk_entry;                           /* 102363 BSET ZRO / 102365 BSET ONE */
    word array = resident_get_array_addr();        /* 102366: JPL I 21 (GETP0)          */
    r->A = 4;                                       /* 102367: SAA 4                    */
    mask_set_PIE();                                /* 102370: MST PIE                  */

    word src, dst_ptr;
    if (SSK) {                                      /* 102371: BSKP ONE SSK (GET path)   */
        r->D = USPAR;                              /* 102373-102374: D = "USPAR"        */
        r->T = r->B[020];                          /* 102375: T = D0 (user array)       */
    } else {                                        /* SET path (this call)              */
        r->D = r->B[020];                          /* 102377-102400: D = D0             */
        r->T = USPAR;                              /* 102401: T = "USPAR"               */
    }
    r->X = 5;                                       /* 102402: SAX 5 (5 words)           */
    r->A = r->B[017];                              /* 102403: A = OLDPAGE               */
    movus_copy(r->D, r->T, r->X, r->A);            /* 102404: JPL I 5 (MOVUS user<->sys) */
    r->B[012] = 0;                                  /* 102405: STZ ,B 12 (A = 0 return)  */
    return indirect_return_102412(r);              /* 102406: JMP I 4 -> [102412]       */
}

/* Byte-verified anchors:
 *   MPASE/MPAGE shared entry 102363/102365 with the SSK discriminator, the prologue
 *   (JPL I 21 -> [102407]), the MST PIE monitor entry, the set/get fork (BSKP ONE SSK
 *   at 102371 choosing which side holds USPAR), the MOVUS 5-word copy (SAX 5 /
 *   JPL I 5 -> [102411]) and the JMP I 4 -> [102412] return.
 * NOT proven: the fall-through MON 56 -> MPASE bridge (uncarved MFELL/CALLPROC);
 *   the exact user-parameter array layout at B+20 (INFERRED from the manual); the
 *   JPL I / JMP I link cells (102407..102412) are a pointer table (DATA, one word of
 *   which is the USPAR=145445 constant) whose runtime targets are not resolved here. */
