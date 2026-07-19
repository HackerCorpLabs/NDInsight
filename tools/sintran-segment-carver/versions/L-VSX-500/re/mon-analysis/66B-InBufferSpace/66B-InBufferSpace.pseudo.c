/* ============================================================================
 * MON 66B  InBufferSpace (ISIZE)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the number of bytes currently held in a
 * device's INPUT buffer (all input monitor calls read from this buffer).
 * Not meaningful for internal devices.  A in = logical device number
 * (1 = own terminal); result returned in W1 / A.
 *
 * Dispatch reality:
 *   GOTAB[66B] = 000000 -> FALL-THROUGH.  No direct GOTAB handler word; the
 *   level-14 handler is reached through the resident MFELL/CALLPROC path,
 *   which is NOT in any carved segment (uncarved bridge).  The candidate
 *   worker is ISIZE @044203B (resident commoncode).  Its bytes are real, but
 *   the MON 66 -> ISIZE hop is NOT byte-proven (see README caveats).
 *
 * ISIZE is the INPUT twin of OSIZE (MON 67B OutBufferSpace): after a small
 * private prologue it JMPs into the SAME per-device scan loop that OSIZE runs
 * (044234-044342).  Control flow is BYTE-VERIFIED from the disassembly
 * (see 66B-InBufferSpace.ASM); register-level semantics are INFERRED.
 * Addresses in comments are octal.
 * ============================================================================ */

/* ISIZE worker @044203B.  Entry prologue (044203-044214) sets up working
 * locals then JMP 20 -> 044234 into the shared device-scan loop. */
int mon_66B_InBufferSpace(mon_regs *r)        /* A in = logical device number   */
{
    /* 044203-044213: ISIZE prologue - stage regs / working locals            */
    /* 044205 JPL I 24 : indirect via runtime pointer [044231] (unresolvable)  */
    /* 044214 JMP 20   : enter the shared scan loop at 044234                  */
    int idx = 0;                              /* 044240: X = loop index         */

    for (;;) {                                /* 044234-044342: per-device scan */
        devent = devtab[idx];                 /* 044241: LDD I ,X 104 (2-word)  */
        if (devent == END_SENTINEL)           /* 044243: SKP IF DA UEQ ST       */
            break;                            /* 044244: JMP -> data/return     */
        if (devent == 0)                      /* 044245: JAZ                    */
            goto advance;                     /*   skip empty slot              */

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
        } else {                              /* 044304-044330: alternate path  */
            /* bit tests (BSKP ONE 60/100 DD) select a masked/shifted value;
             * 044320 JPL I 27 is an indirect call via pointer word @044347. */
            bytes = compute_count(desc);      /* SHA/AND/SAA field arithmetic   */
        }

        /* 044331-044336: assemble the byte count (RADD/SWAP SD DA) and store
         * it back through the device slot (STD I ,X 15). */
        store_result(devent, bytes);

    advance:
        idx += 2;                             /* 044337-044341: double-word stride */
    }

    return r->A;                              /* bytes-in-buffer returned in A (INFERRED) */
}

/* Caveats for the emulator author:
 *   - GOTAB[66B]=0: the MON 66 -> ISIZE bridge is the resident MFELL/CALLPROC
 *     path in an UNCARVED overlay; a live PC trace is needed to confirm that
 *     MON 66B actually lands on ISIZE=044203.
 *   - ISIZE and OSIZE (MON 67B) share the scan loop at 044234-044342; ISIZE
 *     enters at 044234 (via 044214 JMP 20), OSIZE at 044231.  The loop reads
 *     the SAME device buffer descriptors; whether the returned quantity is
 *     "bytes used" (input) vs "bytes free" (output) is decided by the private
 *     prologues / field selection, which is INFERRED, not byte-isolated here.
 *   - The A-in device-number / A-out count register contract is manual-derived
 *     (66B_InBufferSpace.yaml), not byte-proven from this carve.
 *   - 044215-044230 and 044343-044352 are data/pointer cells, not code.
 */
