/* ============================================================================
 * MON 245B  GetNameEntry (GNAEN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * GNAEN is a flag-setting entry into a body SHARED with two siblings:
 *   GDIEN @107111 (MON 244B GetDirEntry,  SSK=0 SSM=0)
 *   WDIEN @107106 (MON 311B WriteDirEntry, SSM=1)
 * GNAEN @107114 sets SSK=1 (name entry, not directory) and SSM=0 (get, not
 * write), then joins the common body at 107116. Control flow and the name-path
 * call to GNAMA are BYTE-VERIFIED; the field semantics are INFERRED from the
 * SINTRAN III Monitor Calls manual. Addresses octal.
 *
 * Every instruction is translated per the canonical
 * ../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md:
 *   - "BSET ONE SSK" / "BSET ZRO SSM" set/clear the skip flags
 *   - "BSKP ONE SSx" tests a skip flag; bare "LDA disp" = mem[P+disp]
 *   - "MIN ,B 4" bumps the caller return-link word => skip-return = success
 * Region A (the level-14 stub) does not exist for this call - GOTAB[245] = 0.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = name index of the device (LDT NAMIX)
 *   X = address of the 14-word (28-byte) buffer receiving the name entry
 * Error return: A = error number. */

int mon_245B_GNAEN(mon_regs *r)     /* GNAEN @107114B (006-S3FS), shared body to RESDI=107401 */
{
    int name_flag, write_flag, rc;

    /* 107114-107115: GNAEN sets the shared-body mode flags */
    name_flag  = 1;                    /* 107114 BSET ONE SSK - name entry           */
    write_flag = 0;                    /* 107115 BSET ZRO SSM - get, not write       */

    save_params(r);                    /* 107116 STD I 112 - stash caller D          */
    resident_prologue();               /* 107122 JPL I 107 -> 003752                 */
    frame[0104] = write_flag;          /* 107123-107130: latch SSM -> B+104          */
    frame[0105] = name_flag;           /* 107131-107136: latch SSK -> B+105          */

    /* name path: name_flag == 1 -> get name address (device name entry) */
    if (name_flag)                     /* 107211-107212: branch on name flag         */
        get_name_address(r);           /* 107217 JPL I 27 -> GNAMA 030235            */
    else
        get_directory_address(r);      /* 107213 JPL I 31 -> GDIRA 030225            */

    if (address_out_of_range())        /* 107224 SKP IF 0 GRE SA                     */
        { rc = 0174; goto store_status; } /* 107226 SAA 174 -> exit                  */

    /* get path (write_flag == 0): read + format the name entry into the buffer */
    format_name_entry(r);              /* 107301-107356: SUCPB / GNFLA / resident    */
    rc = 0;                            /* success */
    { int t = frame[4] + 1; frame[4] = t; } /* 107360 MIN ,B 4 : skip-return ok      */
    goto resident_return;

store_status:
    r->status = rc;                    /* 107364 STA ,B 2 -> caller                  */
resident_return:
    resident_return_teardown();        /* 107361/107362 JMP I -> 003776              */
    return rc;                         /* A = error number on error return           */
}

/* Byte-verified anchors:
 *   GNAEN entry 107114 (BSET ONE SSK / BSET ZRO SSM), shared body 107116,
 *   frame SAB 106, prologue JPL I 107 -> 003752,
 *   GNAMA call JPL I 27 -> 030235 (link cell 107246, the name path),
 *   GDIRA sibling call JPL I 31 -> 030225 (link cell 107244),
 *   error literal SAA 174 (107226), status store STA ,B 2 (107364),
 *   resident return JMP I 16 -> 003776.
 * INFERRED: the name-index-in / name-entry-out contract, the error-174 meaning,
 *   and which shared-body path each sibling takes (see header). */
