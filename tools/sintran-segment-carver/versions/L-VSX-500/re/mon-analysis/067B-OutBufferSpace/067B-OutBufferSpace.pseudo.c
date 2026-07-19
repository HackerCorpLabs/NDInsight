/* ============================================================================
 * MON 67B  OutBufferSpace (OSIZE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the number of bytes still FREE in a device's
 * output buffer before the caller would have to wait (bytes free, NOT bytes used).
 * Not available for internal devices.
 *
 * Two-part model:
 *   (A) The dispatched code is F1642 @121372B (overlay 025-S3IRPIT): a monitor
 *       entry/exit wrapper. BYTE-VERIFIED as the GOTAB[67] target.
 *   (B) The semantic worker is OSIZE @044231B (resident commoncode): a per-device
 *       buffer scan. Its bytes are real, but the F1642 -> OSIZE hop crosses the
 *       uncarved resident CALLPROC and is NOT byte-proven (see README caveats).
 *
 * Control flow of each region is BYTE-VERIFIED from the disassembly
 * (see 067B-OutBufferSpace.ASM); register-level semantics are INFERRED - treat
 * as a model, not gospel. Addresses in comments are octal.
 * ============================================================================ */

/* (A) Level-14 entry stub F1642 @121372B. Sets up regs, enables interrupts,
 * and re-enters the monitor; the real transfer to the worker is a runtime
 * pointer (word @121415 = 116036B) that a static decode cannot resolve. */
int mon_67B_OutBufferSpace(mon_regs *r)      /* A in = logical device number */
{
    /* 121372-121377: stage saved registers (DX/DB/DL) from the caller frame  */
    /* 121400 MST PIE: enable interrupts                                       */
    /* 121401 EXIT  : return into the monitor; worker runs via CALLPROC        */
    return osize_worker(r);                   /* UNVERIFIED bridge -> (B) */
}

/* (B) OSIZE worker @044231B: scan the device table, and for the caller's device
 * compute how many bytes remain free in its output ring buffer. */
int osize_worker(mon_regs *r)                 /* A out = bytes free in out-buffer */
{
    int idx = 0;                              /* 044240: X = loop index          */

    for (;;) {                                /* 044240-044342: per-device scan  */
        devent = devtab[idx];                 /* 044241: LDD I ,X 104 (2-word)   */
        /* 044242 SAT -1 (T = 0177777); 044243 SKP IF DA UEQ ST skips when A != T,
         * so the JMP at 044244 fires (loop exit) only when A == -1 = sentinel.    */
        if (devent == /*sentinel*/ 0177777)   /* 044243: SKP IF DA UEQ ST        */
            break;                            /* 044244: JMP -> data/return      */
        if (devent == 0)                      /* 044245: JAZ (low word A == 0)   */
            goto advance;                     /*   skip empty slot               */

        status = ioxt_read(devent);           /* 044250-044252: IOXT status read */

        if (device_class(status) == CLASS_A) {          /* 044255 JAZ -> 044304 */
            /* 044256-044303: mask fields, read the buffer descriptor with
             * interrupts OFF (POF .. LDD ,X 0 .. PON), compare head vs tail,
             * and zero the pair when the buffer is empty. */
            disable_interrupts();             /* 044267 POF */
            desc = buf_descriptor(devent);    /* 044270 LDD ,X 0 */
            enable_interrupts();              /* 044271 PON */
            if (buffer_empty(desc))
                buf_descriptor(devent) = 0;   /* 044300-044301 STZ ,X 0/1 */
        } else {                              /* 044304-044330: alternate path   */
            /* bit tests (BSKP ONE 60/100 DD) select a masked/shifted value;
             * 044320 JPL I 27 is an indirect call via pointer word @044347. */
            free_bytes = compute_free(desc);  /* SHA/AND/SAA field arithmetic    */
        }

        /* 044331-044336: assemble the free-byte count (RADD/SWAP SD DA) and
         * store it back through the device slot (STD I ,X 15). */
        store_result(devent, free_bytes);

    advance:
        idx += 2;                             /* 044337-044341: double-word stride */
    }

    return r->A;                              /* free bytes returned in A (INFERRED) */
}

/* Caveats for the emulator author:
 *   - The A->B bridge (F1642 EXIT -> OSIZE) is the resident CALLPROC re-entry,
 *     which lives in an UNCARVED overlay; a live PC trace is needed to confirm
 *     that MON 67B actually lands on OSIZE=044231.
 *   - The exact ZAREG/ZXREG/ZTREG save-area mapping and the A-in/A-out register
 *     contract are manual-derived, not byte-proven from this carve.
 *   - 044343-044352 in region B is a data/pointer block; the tail 044353-044427
 *     may be a sibling routine sharing the closure window (UNVERIFIED).
 */
