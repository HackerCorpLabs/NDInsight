/* ============================================================================
 * MON 57B  GetUserParam (PAGEI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Gets the 5 user parameters of a background program (why the last program
 * terminated). SetUserParam (MON 56B) sets them. Background programs only.
 *
 * Derived from the real disassembly (see 57B-GetUserParam.ASM), the MPAGE worker at
 * 102365B in segment 025-S3IRPIT (a SYMBOL-2-LIST symbol). This is a SHARED body: the
 * SSK skip flag selects set (MPASE, SSK=0, MON 56B) vs get (MPAGE, SSK=1, this call).
 * The two directions differ only in which side of the 5-word MOVUS copy carries the
 * USPAR system array. Control flow is BYTE-VERIFIED; the field meanings match the
 * SINTRAN monitor-call source shape (MPASET/MPAGET). Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[57B] = 121246B -> a shared entry-stub block F1636 in 025-S3IRPIT (byte-proven
 *   value). The stub's own branches leave its window and it does not itself reach MPAGE;
 *   the real transfer is the resident CALLPROC (uncarved). So the MON 57 -> MPAGE link
 *   is NOT byte-followable statically; identity rests on the symbol NAME (MPAGE = get
 *   entry of the set/get user-parameter body) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ZRO/ONE SSK = clear/set skip flag;  JPL I = indirect call;  SAA n = A:=n;
 *   MST PIE = mask-set PIE;  BSKP ONE SSK = skip if skip-flag set;  RADD CLD SA DD =
 *   register copy (D=A);  LDA/LDT ,B n = frame loads;  SAX n = X:=n;  JMP I = indirect
 *   return.
 * ============================================================================ */

/* Manual register contract (from the MON 57 description, INFERRED - not byte-proven):
 *   PAGEI(Buff): a 5-word buffer returned. Buff[0]=dir/user index, [1]=terminal LDN,
 *   [2]=error number (-1 if ESCAPE), [3..4]=user-defined. */

#define USPAR 0145445                              /* 102410: constant used by both dirs */

int mon_57B_GetUserParam(mon_regs *r, int ssk_entry) /* ssk_entry: 0 = MPASE (set), 1 = MPAGE (get, this call) */
{
    int SSK = ssk_entry;                           /* 102363 BSET ZRO / 102365 BSET ONE */
    word array = resident_get_array_addr();        /* 102366: JPL I 21 (GETP0)          */
    r->A = 4;                                       /* 102367: SAA 4                    */
    mask_set_PIE();                                /* 102370: MST PIE                  */

    if (SSK) {                                      /* 102371: BSKP ONE SSK (GET, this call) */
        r->D = USPAR;                              /* 102373-102374: D = "USPAR"        */
        r->T = r->B[020];                          /* 102375: T = user array D0         */
    } else {                                        /* SET path                          */
        r->D = r->B[020];                          /* 102377-102400: D = D0             */
        r->T = USPAR;                              /* 102401: T = "USPAR"               */
    }
    r->X = 5;                                       /* 102402: SAX 5 (5 words)           */
    r->A = r->B[017];                              /* 102403: A = OLDPAGE               */
    movus_copy(r->D, r->T, r->X, r->A);            /* 102404: JPL I 5 (MOVUS sys->user)  */
    r->B[012] = 0;                                  /* 102405: STZ ,B 12 (A = 0 return)  */
    return indirect_return_102412(r);              /* 102406: JMP I 4 -> [102412]       */
}

/* Byte-verified anchors:
 *   MPASE/MPAGE shared entry 102363/102365 with the SSK discriminator, the prologue
 *   (JPL I 21 -> [102407]), the MST PIE monitor entry, the set/get fork (BSKP ONE SSK
 *   at 102371 choosing which side holds USPAR), the MOVUS 5-word copy (SAX 5 /
 *   JPL I 5 -> [102411]) and the JMP I 4 -> [102412] return.
 * NOT proven: the GOTAB[57]=121246 stub -> MPAGE bridge (uncarved CALLPROC); the exact
 *   buffer layout Buff[0..4] (INFERRED from the manual); the JPL I / JMP I link cells
 *   (102407..102412) are a pointer table (DATA, one word of which is the USPAR=145445
 *   constant) whose runtime targets are not resolved here. */
