/* ============================================================================
 * MON 62B  GetBytesInFile (RMAX)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 62B-GetBytesInFile.ASM). Control flow
 * and the success/error skip-return split are BYTE-VERIFIED; the semantic labels
 * (what RMAXB computes, the register roles) are INFERRED from the call structure,
 * the FILSYS symbol table and the manual - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Entry 103767 = RMAX. Returns the number of data bytes in an open file as a
 * 32-bit (INTEGER4 / double-word) value. Success is a SKIP return; the normal
 * (no-skip) return is the error path (matches the MAC pattern MON 62 / JMP ERROR
 * / STD BYTES). */
int mon_get_bytes_in_file(mon_regs *r)
{
    save_params(r);                        /* 103767 STD I 34: stash caller D       */
    frame_setup();                         /* 103770-103772: SAB 6, build B frame   */

    prologue_worker();                     /* 103773 JPL I -> 003752 (resident)     */
    r->local_fno = r->T;                   /* 103774 STT I 31: stash file number    */

    /* RMAXB does the real work: it computes the byte count for the open file.
     * It returns SKIP on success (D = INTEGER4 byte count) and NO-SKIP on error
     * (A = error code). */
    if (RMAXB(r->local_fno)) {             /* 103775 JPL I -> 072016 (RMAXB)        */
        /* success (skip return -> 103777) */
        r->B[2] = r->D;                    /* 103777 STD ,B 2: byte count (double)  */
        r->B[4] += 1;                      /* 104000 MIN ,B 4: set caller OK/skip    */
        return 0;                          /* 104002 JMP I -> 003776 (resident ret) */
    } else {
        /* error (no-skip -> 103776 JMP 5 -> 104003) */
        r->B[2] = r->A;                    /* 104003 STA ,B 2: error code           */
        return r->A;                       /* 104004/104001 -> 003776 (resident ret)*/
    }
}

/* Caller (MAC):
 *   LDT FILNO   ; T = file number (from an earlier OpenFile)
 *   MON 62      ; GetBytesInFile
 *   JMP ERROR   ; no-skip = error (A = error code)
 *   STD BYTES   ; skip     = success (D = INTEGER4 byte count, a double word)
 *
 * The count is only meaningful for sequentially accessed files.
 */
