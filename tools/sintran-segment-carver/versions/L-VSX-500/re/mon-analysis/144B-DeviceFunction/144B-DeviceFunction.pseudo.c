/* ============================================================================
 * MON 144B  DeviceFunction (MAGTP)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Worker body 006-S3FS @026354B..027036B.
 *
 * Derived from the real disassembly (see 144B-DeviceFunction.ASM). Control flow,
 * the function-code range check, the AND-66 mask, the two MOVEW/STATX transfer
 * blocks and the error-status store are BYTE-VERIFIED. The semantic labels
 * (which device sub-entry does what, what each error code means, the caller-side
 * parameter convention) are INFERRED - treat as a model, not gospel.
 *
 * NOTE ON DISPATCH: MON 144B has GOTAB[144] = 000000 (a fall-through). This body
 * is reached only AFTER the resident CALLPROC second-level dispatch, which lives
 * in an uncarved overlay. So "MON 144B calls this body" is INFERRED from the
 * MAGTP symbol, not a followed pointer. Register roles X (per-call work area)
 * and B (device/data-field block) are inferred from the access pattern; the glue
 * that sets X/B is upstream of this carve. Addresses in comments are octal.
 * ============================================================================ */

/* r->func is the device function code the caller placed in the parameter list;
 * the handler reads it from device-block slot 20 (,B 20). */
int mon_device_function(mon_regs *r, work_area *X, dev_block *B)
{
    int func, code;

    /* 026354-026445: per-device sub-entry setup (500RF/500WF/XRFIL/XWFIL).
     * Shuffles caller params through work-area slots 17/25/27/31 and calls
     * shared file-system workers via pointer words @026442/026443/026444/026445
     * (targets live in uncarved resident code - not resolvable here). */
    setup_device_variant(X, B);            /* JPL I 63/61/26 ; JMP I 50/12 */

    /* 026446-026467 (XMRW): function-code range check to [100B..177B]. */
    func = B->slot20;                      /* 026446 LDA ,B 20            */
    if (func < 0100 || func > 0177) {      /* 026447-026454 SAT/SKP/JMP   */
        /* 026470-026477: out of range -> default function 71B, then mask
         * the device-state word (,B 25) with 66B and re-validate. */
        B->slot30 = 0121252;               /* 026470 LDT 71 = mem[026561B]=0121252 (P-rel, NOT literal) ; 026471 STT ,B 30 */
        if (((B->slot25 & 0176777) - 077) > 0) /* 026473 LDA ,B 25 ; 026474 AND 66 = &mem[026562B]=0176777 ; 026475 AAA -77 ; 026476 SKP IF 0 GRE SA -> reject when >0 */
            goto reject;                    /* 026477 JMP -> 026554        */
        code = sub_dispatch(X, B);         /* 026500-026553 worker/queue  */
    } else {
        /* 026455-026465: in range -> derive handler code into slot 30. */
        code = derive_code(B);             /* 026455-026465 STA ,B 30     */
        B->slot30 = code;
        /* 026466-026467 JMP -> 026607: fall into the transfer-setup block. */
    }

    /* 026607-026735: validate the request, choose an error code on failure
     * (132B / 133B / 174B loaded via SAA before the error tail), and prepare
     * the block-transfer parameters. */
    if (!request_ok(X, B))
        goto reject;

    /* 026737-027035: build a MOVEW descriptor and do the block move, then a
     * STATX device-status transfer, then call finalisers via pointer words
     * @027052/027053/027054/027055 (targets outside this carve). Two nearly
     * identical MOVEW blocks: 026741-026750 (A) and 027022-027031 (B). */
    move_block_and_status(X, B);           /* MOVEW ; STATX ; JPL I finaliser */
    return OK;

reject:
    B->slot12 = 0201;                      /* 026554 LDA 14 = mem[026570B]=0201 (P-rel, NOT literal) ; 026555 STA ,B 12 */
    return ERROR;                          /* error/status word to caller       */
}

/* Caller (INFERRED, not byte-proven from this carve):
 *   MON 144B DeviceFunction: A = address of parameter list
 *     (Func, Buff, DevNo, Param1, Param2); A out = status.
 */
