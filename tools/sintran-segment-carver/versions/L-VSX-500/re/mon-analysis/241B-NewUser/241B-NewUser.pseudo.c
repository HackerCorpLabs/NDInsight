/* ============================================================================
 * MON 241B  NewUser (SUSCN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Switches the user name you are logged in under; the program continues under the
 * new user name. Restore the old name with OldUser (MON 242B). User RT and user
 * SYSTEM only.
 *
 * Derived from the real disassembly (see 241B-NewUser.ASM), the SUSCN worker at
 * 106377B in segment 006-S3FS (a FILSYS-SYMBOLS symbol). Control flow (the name
 * validation, the user-RT test, the save/restore of the old name words, the
 * install call and the two error tails) is BYTE-VERIFIED. The register/field
 * meanings (name / password / project-password addresses, the returned status)
 * are INFERRED from the SINTRAN III Monitor Calls manual MAC example and the code
 * shape - treat as a model. Addresses in comments are octal.
 *
 * Dispatch reality:
 *   GOTAB[241B] = 000000 -> FALL-THROUGH (no per-call stub). Dispatch drops into the
 *   resident MFELL/CALLPROC second-level path (uncarved) which reaches SUSCN. So the
 *   MON 241 -> SUSCN link is NOT byte-followable statically; identity rests on the
 *   symbol NAME (SUSCN, the NewUser/switch-user twin of RUSCN) - see README caveats.
 *
 * Every instruction below is grounded in the canonical ND-100 instruction semantics
 * reference (../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md):
 *   RADD CLD Ss Dd = register copy;  RADD SB DA/DX = add B;  SHA ZIN 10 = logical
 *   left shift 8;  SAB/SAT/SAA n = set arg;  SKP IF DA EQL ST = skip if A==T;
 *   JAF d = jump if flag false;  MIN ,B n = increment mem, skip on wrap;
 *   JPL I / JMP I = indirect call/return.
 * ============================================================================ */

/* Manual register contract (from the MAC example, INFERRED - not byte-proven):
 *   MON 241 NewUser - MAC:
 *     LDT (PROJP / LDX (USER / LDA (PASSW / MON 241 / JMP ERROR / STA STAT
 *   X = address of the new user-name string
 *   A = user password coded as an integer (contents of the password location)
 *   T = address of the project-password string
 *   On return A = status (public users 0, user SYSTEM 1, user RT 2). */

int mon_241B_NewUser(mon_regs *r)               /* in: UserName, UserPassword, ProjectPassword */
{
    /* 106377-106403: save the incoming A/D pair, stage L/B, call the resident
     * prologue worker (JPL I 126 -> [106531]).                                   */
    save_ad_pair(r->A, r->D);                  /* 106377: STD I 131                */
    r->A = r->L;                               /* 106400: RADD CLD SL DA (copy)     */
    r->D = r->B;                               /* 106401: RADD CLD SB DD (copy)     */
    r->B = 032;                                /* 106402: SAB 32                   */
    resident_prologue_worker();                /* 106403: JPL I 126 -> [106531]     */

    if (resident_worker_106534_failed())       /* 106404 JPL I 130 / 106405 JMP -> tail */
        goto error_tail;
    r->X = r->B[0];                            /* 106406: LDX ,B 0                 */
    r->A = mem[pc_rel(106407, 0123)] + r->B;   /* 106407 LDA 123 / 106410 RADD SB DA */
    r->T = 044;                                /* 106411: SAT 44                   */
    if (resident_worker_106535_failed())       /* 106412 JPL I 123 / 106413 JMP -> tail */
        goto error_tail;                       /*   (validate user name)            */
    r->D = r->A; r->X = r->A;                   /* 106414-106415: copies             */
    r->A = mem[pc_rel(106416, 0120)];          /* 106416: LDA 120                  */
    r->T = 044;                                /* 106417: SAT 44                   */
    if (resident_worker_106537_failed())       /* 106420 JPL I 117 / 106421 JMP -> tail */
        goto error_tail;

    if (mem_ind(106540) != (word)(-1)) {        /* 106422-106425: SAT -1 / SKP IF EQL */
        /* pack + store the coded name/password (106426-106435)                    */
        mem_ind(106540) = (mem_ind(106541) << 8) + mem_ind(106542); /* SHA ZIN 10; ADD */
        /* propagate name words (106432-106435, indirect copies)                   */
    }
    if ((mem_ind(106547) & SELECT_BIT_40) == 0) { /* 106436-106437 BSKP ONE 40 DA    */
        if (!flag_false(r->B[2]))              /* 106441 LDA ,B 2 / 106442 JAF       */
            { r->A = 025; goto error_tail; }   /* 106443 SAA 25 -> tail             */
    }
    if (mem_ind(106547) != 1) {                 /* 106445-106450: user-RT test (T=1)  */
        /* not user RT: extra validation via resident workers                      */
        r->X = mem[pc_rel(106451, 061)] + r->B;/* 106451 LDX 61 / 106452 RADD SB DX  */
        if (resident_worker_106550_failed())   /* 106453 JPL I 75 / 106454 JMP -> tail(526) */
            goto error_tail2;
        r->A = r->T;                           /* 106455: RADD CLD ST DA            */
        if (resident_worker_106551_failed())   /* 106456 JPL I 73 / 106457 JMP -> tail(526) */
            goto error_tail2;
        if (r->A != r->T)                       /* 106460: SKP IF DA EQL ST          */
            { r->A = 025; goto error_tail2; }  /* 106462 SAA 25 -> tail(526)        */
    }
    /* save the current (old) name words before switching (106464-106467)          */
    r->B[030] = mem_ind(106541);               /* 106464-106465                    */
    r->B[031] = mem_ind(106542);               /* 106466-106467                    */
    resident_worker_106552();                  /* 106470: JPL I 62                 */
    if (resident_worker_106553_failed())       /* 106471 JPL I 62 / 106472 JMP -> tail(523) */
        goto error_tail3;
    r->X = mem[pc_rel(106473, 037)] + r->B;    /* 106473 LDX 37 / 106474 RADD SB DX  */
    r->A = r->B[2];                            /* 106475: LDA ,B 2                 */
    if (resident_worker_106554_failed())       /* 106476 JPL I 56 / 106477 JMP -> restore(512) */
        goto error_restore;                    /*   (install the new user)          */
    r->B[2] = r->A;                            /* 106500: returned status 0/1/2     */
    /* commit the new name (106501-106504, indirect stores)                        */
    if (++mem_at_B4() == 0) {                   /* 106505: MIN ,B 4 (skip on wrap)   */
        r->A = (word)(-032);                   /* 106506: SAA -32 (error code)      */
        return indirect_return_106557(r);      /* 106507: JMP I 50 -> [106557]      */
    }
error_tail:
    r->B[2] = r->A;                            /* 106510: STA ,B 2                 */
    r->A = (word)(-032);                       /* 106511 JMP -3 -> 106506           */
    return indirect_return_106557(r);

error_restore:
    r->B[2] = r->A;                            /* 106512: STA ,B 2                 */
    mem_ind(106541) = r->B[030];               /* 106513-106514: restore old name 0 */
    mem_ind(106542) = r->B[031];               /* 106515-106516: restore old name 1 */
    resident_worker_106560();                  /* 106517: JPL I 41                 */
    resident_worker_106561();                  /* 106520: JPL I 41                 */
    r->A = (word)(-032); return indirect_return_106557(r); /* 106521-106522 -> 106506 */

error_tail3:
    r->B[2] = r->A;                            /* 106523: STA ,B 2                 */
    resident_worker_106560();                  /* 106524: JPL I 34                 */
    r->A = (word)(-032); return indirect_return_106557(r); /* 106525 -> 106506       */

error_tail2:
    r->B[2] = r->A;                            /* 106526: STA ,B 2                 */
    r->A = (word)(-032); return indirect_return_106557(r); /* 106527 -> 106506       */
}

/* Byte-verified anchors:
 *   SUSCN entry 106377 (006-S3FS), the name-validate calls (JPL I -> [106535/
 *   106537]), the user-RT test (SKP IF DA EQL ST with T=1 at 106445-106450), the
 *   old-name save (106464-106467) and restore-on-failure (106512-106516), the
 *   install call (JPL I 56 -> [106554]) and the four error tails converging on the
 *   SAA -32 return (106506, JMP I 50 -> [106557]).
 * NOT proven: the fall-through MON 241 -> SUSCN bridge (uncarved MFELL/CALLPROC);
 *   the semantic label of each B-frame word and the exact packing of name/password
 *   (INFERRED from the manual); the JPL I / JMP I link cells (106531..106561) are a
 *   pointer table (DATA) whose runtime targets are not resolved here. */
