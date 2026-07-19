/* ============================================================================
 * MON 244B  GetDirEntry (GDIEN)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 244B-GetDirEntry.ASM). Control flow and
 * the calls to the directory-address primitives GDIRA / GNAMA are BYTE-VERIFIED;
 * the semantic labels (which field is directory index / entry buffer / flag) are
 * INFERRED from the SINTRAN III Monitor Calls manual - treat as a model, not
 * gospel. Addresses in comments are octal.
 *
 * GDIEN shares its body with two sibling entries that set different skip flags:
 *   WDIEN @107106 (SSM=1) - Write Dir Entry
 *   GNAEN @107114 (SSK=1) - Get Name Entry
 * GDIEN @107111 sets SSK=0 (directory entry, not name) and SSM=0 (get, not write).
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   T = directory index          (set bit 15 in X if remote system id supplied)
 *   X = address of buffer to receive the 24-word (42-byte) directory entry
 *   D = remote system identification (used only if bit 15 in X is set)
 * Normal return: A = 1 or 5 -> disk has spare-track allocation; A = 3 -> none.
 * Error return:  A = error number. */

int mon_getdirentry(mon_regs *r)
{
    int name_flag, write_flag;

    /* 107111-107113: GDIEN entry sets the shared-body mode flags */
    name_flag  = 0;                        /* 107111: BSET ZRO SSK - directory entry */
    write_flag = 0;                        /* 107112: BSET ZRO SSM - get, not write  */

    save_params(r);                        /* 107116: STD I 112 - stash caller D      */
    resident_prologue();                   /* 107122: JPL I 107 -> 003752             */
    frame[0104] = write_flag;              /* 107123-107130: latch SSM -> B+104       */
    frame[0105] = name_flag;               /* 107131-107136: latch SSK -> B+105       */

    /* GDIEN (name_flag==0, write_flag==0) takes the get-entry path at 107211 */
    if (name_flag == 0 && write_flag == 0) {   /* 107137-107144 */
        get_directory_address(r);          /* 107213: JPL I 31 -> GDIRA 030225        */
    } else {
        get_name_address(r);               /* 107217: JPL I 27 -> GNAMA 030235        */
    }

    if (address_out_of_range())            /* 107224: SKP IF 0 GRE SA                  */
        { rc = 0174; goto store_status; }  /* 107226: SAA 174 -> exit                  */

    if (write_flag) {                      /* 107250-107251: branch on write flag      */
        /* WDIEN path: format + write the entry (107252-107357) */
        write_directory_entry(r);          /* 107356: JPL I 21 -> WDIRE 047716         */
    } else {
        /* GDIEN path: read + format the directory entry into the caller buffer */
        format_dir_entry(r);               /* 107301-107356: SUCPB/GNFLA/resident      */
    }
    rc = OK;
    goto success;

store_status:
    r->status = rc;                        /* 107364: STA ,B 2 -> caller               */
success:
    resident_return();                     /* 107361/107362: JMP I -> 003776           */
    return rc;                             /* A = spare-track flag on normal return     */
}

/* Byte-verified anchors:
 *   GDIEN entry 107111 (BSET ZRO SSK / BSET ZRO SSM), shared body 107116,
 *   frame SAB 106, prologue JPL I 107 -> 003752,
 *   GDIRA call JPL I 31 -> 030225, GNAMA call JPL I 27 -> 030235,
 *   WDIRE call JPL I 21 -> 047716, status store STA ,B 2 (107364),
 *   resident return -> 003776. */
