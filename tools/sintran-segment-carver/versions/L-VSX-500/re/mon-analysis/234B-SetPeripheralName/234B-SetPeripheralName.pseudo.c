/* ============================================================================
 * MON 234B  SetPeripheralName (SPEFI)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Defines a peripheral file: connects a file name to the logical device number of a
 * peripheral (e.g. a printer). The file name should exist in advance with no file type.
 *
 * Derived from the real disassembly (see 234B-SetPeripheralName.ASM), the SPEFI worker
 * at 106055B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). SPEFI is the first of a
 * THREE-entry idiom (SPEFI/MRNFI/MDLFI) encoding an initial function selector in the
 * SSM/SSK skip flags; the common body derives a function code (0..3) into B+123 and
 * dispatches by it to resident workers. Control flow (the entry idiom, the function-code
 * derivation, the four-way dispatch and the success/error tails) is BYTE-VERIFIED; the
 * per-code worker semantics are INFERRED from the call structure. Addresses are octal.
 *
 * Dispatch reality:
 *   GOTAB[234B] = 066176B -> a 3-word entry stub F1724 in 025-S3IRPIT (byte-proven
 *   value). The stub sits in a shared stub block and does not itself reach SPEFI; the
 *   real transfer is the resident CALLPROC (uncarved). So the MON 234 -> SPEFI link is
 *   NOT byte-followable statically; identity rests on the symbol NAME (SPEFI = Set
 *   PEripheral FIle) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   BSET ONE/ZRO SSM/SSK = set/clear skip flag;  RADD CLD Ss Dd = register copy;
 *   RADD SB DA/DX = add B;  SAA/SAT/SAB/SAX n = set argument;  BSKP ONE = skip if set;
 *   SKP IF DA EQL ST = skip if A==T;  JAF d = jump if A != 0;  MIN ,B n = increment
 *   mem, skip on wrap;  JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MON 234 description, INFERRED - not byte-proven):
 *   SetPeripheralName(FileName, DeviceNumber):
 *     FileName      : peripheral file-name string (appendix G)
 *     DeviceNumber  : logical device number (appendix B)
 *   Error number returned in A. */

int mon_234B_SetPeripheralName(mon_regs *r, int ssm_entry, int ssk_entry) /* SPEFI: SSM=1,SSK=0 */
{
    int SSM = ssm_entry, SSK = ssk_entry;          /* 106055-106064: three-entry idiom  */
    save_ad_pair(r->A, r->D);                       /* 106065: STD I 111                */
    r->A = r->L;                                    /* 106066: RADD CLD SL DA            */
    r->D = r->B;                                    /* 106067: RADD CLD SB DD            */
    r->B = 0125;                                    /* 106070: SAB 125                  */
    resident_prologue_worker();                     /* 106071: JPL I 106 -> [106177]     */

    int func;                                       /* 106072-106106: derive function code */
    if (SSM)        func = SSK ? 3 : 2;             /* SPEFI path (this call: func = 2)  */
    else            func = SSK ? 1 : 0;             /* MRNFI / MDLFI paths               */
    r->B[0123] = func;                              /* 106107: STA ,B 123               */

    r->A = mem[pc_rel(106110,070)] + r->B;          /* 106110-106111                    */
    r->T = mem[pc_rel(106112,067)];                 /* 106112: LDT 67                   */
    r->X = r->B[0];                                 /* 106113: LDX ,B 0                 */
    if (resident_parse_name_failed())               /* 106114 JPL I 66 / 106115 JMP -> tail */
        goto error_tail;

    if (r->B[0123] != 1) {                           /* 106116-106121: code == 1 branch   */
        r->A = mem[pc_rel(106122,061)] + r->B;      /* 106122-106123                    */
        r->T = 026; r->X = r->B[2];                 /* 106124-106125                    */
        if (resident_worker_106202_failed())        /* 106126 JPL I 54 / 106127 JMP -> tail */
            goto error_tail;
    }

    r->X = mem[pc_rel(106130,050)] + r->B;          /* 106130-106131                    */
    func = r->B[0123];                              /* 106132: LDA ,B 123               */
    if (func == 0) {                                 /* 106133: JAF 4 (skip if != 0)      */
        if (resident_worker_106204_failed())        /* 106134 JPL I 50 / 106135 JMP -> tail */
            goto error_tail;
        goto success;                               /* 106136: JMP -> 106171             */
    }
    if (func == 1) {                                 /* 106137-106142                    */
        r->A = mem[pc_rel(106143,040)] + r->B;      /* 106143-106144                    */
        if (resident_worker_106205_failed()) goto error_tail; /* 106145-106146          */
        goto success;                               /* 106147                            */
    }
    if (func == 2) {                                 /* 106150-106153 (this call)         */
        r->A = r->B[2];                             /* 106154: LDA ,B 2                 */
        if (resident_worker_106206_failed()) goto error_tail; /* 106155-106156          */
        goto success;                               /* 106157                            */
    }
    if (func == 3) {                                 /* 106160-106163                    */
        if (resident_worker_106207_failed()) goto error_tail; /* 106164-106165          */
        goto success;                               /* 106166                            */
    }
    if (resident_worker_106210_failed())            /* 106167 JPL I 21 / 106170 JMP -> tail */
        goto error_tail;

success:
    if (++mem_at_B4() == 0) {                        /* 106171: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-0125);                       /* 106172: SAA -125 (error code)     */
        return indirect_return_106211(r);           /* 106173: JMP I 16 -> [106211]      */
    }
error_tail:
    r->B[2] = r->A;                                 /* 106174: STA ,B 2                 */
    r->A = (word)(-0125);                           /* 106175 JMP -3 -> 106172           */
    return indirect_return_106211(r);
}

/* Byte-verified anchors:
 *   the three-entry SSM/SSK idiom (SPEFI/MRNFI/MDLFI at 106055/106060/106063), the
 *   function-code derivation into B+123 (106072-106107), the parse-name call
 *   (JPL I 66 -> [106202]), the four-way function dispatch (106116-106170 comparing
 *   B+123 against 1/2/3 and calling resident workers via [106204..106210]), the success
 *   advance (MIN ,B 4 at 106171) and the SAA -125 / JMP I 16 -> [106211] return.
 * NOT proven: the GOTAB[234]=066176 stub -> SPEFI bridge (uncarved CALLPROC); the exact
 *   meaning of each resident worker and the B-frame offsets (INFERRED); the JPL I /
 *   JMP I link cells (106177..106211) are a pointer table (DATA) whose runtime targets
 *   are not resolved here. */
