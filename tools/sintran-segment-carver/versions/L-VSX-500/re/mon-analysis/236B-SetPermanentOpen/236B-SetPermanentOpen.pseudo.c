/* ============================================================================
 * MON 236B  SetPermanentOpen (SETPO)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 236B-SetPermanentOpen.ASM). Control
 * flow and the permanent-open bit set are BYTE-VERIFIED; the semantic labels
 * (open-file-table layout, error numbers) are INFERRED from the SINTRAN III
 * Monitor Calls manual - treat as a model, not gospel. Addresses octal.
 *
 * The file must already be open. SetPermanentOpen marks that open so the file is
 * NOT closed by CloseFile(-1) and NOT closed when the program terminates; only a
 * close with the explicit file number (or -2) will close it. Only mass-storage
 * files can be set permanently open. Nothing is opened here - no FOPEN call.
 * (The manual short name is SPERD; the byte-level worker symbol is SETPO.)
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = file number returned from an earlier open (the ONLY input)
 * Error return: A = error number. */

int mon_setpermanentopen(mon_regs *r)
{
    save_params(r);                        /* 072465: STD I 120 - stash caller D   */
    resident_prologue();                   /* 072471: JPL I 115 -> 003752          */

    filno = r->frame[2];                   /* 072472: LDT ,B 2 - caller file number*/
    if (!(filno <= 0100 ||                 /* 072473-072500: range-check the file  */
          filno <= indirect(0111)))        /*   number; out of range -> error path */
        goto error_range;                  /*   -> 072556                          */

    /* 072501-072555: scan the open-file table (base @107, stride 2 words) for the
     * entry whose key matches filno; skip entries whose flag bits are set.        */
    for (x = table_base; ; x += 2) {       /* 072551: AAX 2                        */
        if (entry_matches(x, filno))       /* 072520-072521: SKP IF DA EQL ST      */
            goto found;
        if (x == table_end) goto not_found;/* 072553: SKP IF DX EQL ST             */
    }

error_range:                               /* 072556                               */
    rc = classify_error(r->frame[2]);      /* 072557: JPL I 37 -> 072616           */
    goto store_status;

found:                                     /* 072560                               */
    if (r->D == 0) { rc = 0132; goto store_status; } /* 072561-072564: err 132     */
    x = file_entry_addr(r);                /* 072567: RADD CLD SA DX               */
    if ((entry[x+3] & (1 << 14)) == 0) {   /* 072571: BSKP ZRO 160 DA (bit 14)     */
        rc = 0133; goto store_status;      /* 072573: SAA 133 - err 133            */
    }
    entry[x+7] |= (1 << 15);               /* 072575-072577: LDA ,X 7; BSET ONE    */
                                           /*   bit15; STA ,X 7 - mark permanent   */
    r->frame[4] += 1;                      /* 072600: MIN ,B 4 - success flag      */
    goto resident_ret;                     /* 072601 SAA -7 / 072602 JMP I -> 003776*/

not_found:                                 /* falls through the scan               */
store_status:                              /* 072603                               */
    r->status = rc;                        /* 072603: STA ,B 2 -> caller           */
resident_ret:
    resident_return();                     /* 072602/072617: JMP I -> 003776       */
    return rc;                             /* error number in A on failure          */
}

/* Byte-verified anchors:
 *   SETPO entry 072465 (STD I 120, SAB 7), prologue JPL I 115 -> 003752,
 *   file number LDT ,B 2, table scan (AAX 2 / SKP IF DA EQL ST),
 *   permanent-open bit set LDA ,X 7 / BSET ONE bit15 DA / STA ,X 7 (072575-077),
 *   success MIN ,B 4, resident return JMP I -> 003776 (link cell 072617).
 * NOTE: no FOPEN call - the file is already open; this only sets a flag bit. */
