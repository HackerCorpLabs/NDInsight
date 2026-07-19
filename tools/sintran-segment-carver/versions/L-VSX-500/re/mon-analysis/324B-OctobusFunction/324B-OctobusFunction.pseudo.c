/* ============================================================================
 * MON 324B  OctobusFunction (OCTIO)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  DSI8 Octobus handler 025-S3IRPIT @112674B..112722B.
 *
 * Derived from the real disassembly (see 324B-OctobusFunction.ASM). The DSI8
 * handler control flow - argument validation, the device-number fold
 * (AND 103 / ADD 103), and the indirect dispatch through pointer words
 * [113003]..[113013] - is BYTE-VERIFIED. The concrete Octobus operations
 * (0=kick, 1=wait-for-kick, 5=read status, 6=who-am-I) live in the Octobus
 * device driver reached through those pointer words, which is NOT in any
 * carved segment; those routines are modelled as opaque calls (documented
 * behaviour from the OctobusFunction manual, ND-860228.2 EN).
 *
 * NOTE ON DISPATCH: GOTAB[324] = 112674B routes directly to DSI8 (real code).
 * DSI8 does the argument handling inline, then JPL I / JMP I through pointer
 * words to the Octobus device workers - which are past the carve. Register
 * roles X (result/param pointer) and B (per-call Octobus datafield) are
 * inferred from the access pattern. Addresses in comments are octal.
 * ============================================================================ */

/* 112674-112722 (DSI8): Octobus level-14 handler. */
int mon_octobus_function(mon_regs *r, octo_field *B)
{
    void *X;
    int func, dev, result;

    X = (void *)B->slot12;             /* 112674 LDX ,B 12                        */
    octo_setup(X, B);                  /* 112675 JPL I 106 -> [113003]            */
    func = ((int *)X)[1];              /* 112676 LDA ,X 1  function/param         */
    B->slot21 = octo_prep(func);       /* 112677 JPL I 105 ; 112700 STA ,B 21     */

    dev = B->slot17;                   /* 112701 LDA ,B 17  device number          */
    dev = (dev & mem[0113005]) + mem[0113006]; /* 112702 AND 103 ; 112703 ADD 103 : fold device no */
    B->slot23 = dev;                   /* 112704 STA ,B 23  folded device selector */

    result = octo_operation(X, B);     /* 112705 JPL I 102 -> [113007] : kick / wait / status / who-am-i */
    if (result /*D*/ == 0)             /* 112706 SKP IF DD UEQ 0 ; 112707 JMP I 101 */
        return octo_return_noresult(B);/* -> [113010]                             */

    X = (void *)result;                /* 112710 RADD CLD SD DX : X = D           */
    if (((int *)X)[1] == mem[deref(0113011)]) { /* 112711-112713 LDA ,X 1 ; LDT I 77 ; SKP */
        B->status = 5;                 /* 112715 SAA 5                            */
        return octo_status_return(B);  /* 112716 JMP I 74 -> [113012]             */
    }
    if ((((int *)X)[3] & (1<<11)) != 0)/* 112717 LDA ,X 3 ; 112720 BSKP ZRO 130 DA */
        return octo_return(B);         /* 112721 JMP I 72 -> [113013]             */
    return octo_return(B);
}

/* Octobus device operations (documented, ND-860228.2 EN; NOT byte-proven here -
 * the driver routines sit past the [113003]..[113013] pointer words, uncarved):
 *   func 0 = kick               (returns destination station in Parameter)
 *   func 1 = wait for kick
 *   func 5 = read Octobus status (returns status value)
 *   func 6 = who am I
 * Caller: FunctionCode, DeviceNo (logical), Parameter (function-dependent, io).
 * Performs functions on an old Octobus (earlier than version 3).
 */
