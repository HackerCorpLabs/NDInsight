/* ============================================================================
 * MON 075B  GetStartByte (REABT)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  "Read byte pointer": returns, as an INTEGER4, the
 * next byte position that would be accessed in an open sequential mass-storage
 * file.
 *
 * Derived from the real disassembly (see 075B-GetStartByte.ASM). Control flow
 * (the flag fork at 104054 and the two result-staging paths) is BYTE-VERIFIED;
 * the semantic labels (which field is the byte position, which worker does what)
 * are INFERRED from the call structure - treat as a model, not gospel.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Entry 104005 = REABT worker body. Result is an INTEGER4 staged as two 16-bit
 * words into locals B+123 (hi) and B+125 (lo) before the common exit at 104202. */
int mon_get_start_byte(mon_regs *r)
{
    save_params(r);                        /* 104005: STD I 16 - stash caller A:D pair */
    fs_resolve(r);                         /* 104011/104013: JPL I workers        */
                                           /*   (003752, 072014) - locate open    */
                                           /*   file descriptor                   */

    /* 104044-104052: compute a validity/open flag into local B+127.
     * 104045 BSKP ONE SSK picks 1 (SAA 1) vs 0 (STZ). */
    int flag = file_is_valid(r) ? 1 : 0;   /* stored at B+127                      */

    if (flag != 0) {                       /* 104053 LDA ,B 127 / 104054 JAF -> 104137:
                                            * JAF jumps when A != 0, i.e. flag != 0     */
        /* ---- byte-pointer read-out path (104137-104201) ---- */
        rec = fs_lookup(r);                /* 104141 JPL I 73 (-> 104234)         */
        /* 104150/104154 two masked BSKP tests select a sub-path; each loads a
         * field off the open-file record (LDX I 56; LDA ,X 3/7; STA ,X 24/25). */
        pos = read_byte_position(rec);     /* INFERRED: the current byte position */
        r->result_hi = HIGH16(pos);        /* 104200: STT ,B 123                  */
        r->result_lo = LOW16(pos);         /* 104201: STD ,B 125 (A:D pair)       */
    } else {                               /* flag == 0: fall through 104055 LDA ,B 1 /
                                            * 104056 JAP -> 104132 (jump if A >= 0)     */
        /* ---- error / already-staged path (104132-104136) ---- */
        r->result_hi = r->stagedT;         /* 104134: STT ,B 123                  */
        r->result_lo = r->stagedD;         /* 104135: STD ,B 125                  */
    }

    return fs_return(r);                   /* 104202: JPL I 42 (-> 104244) common  */
                                           /*   exit into shared monitor-return    */
}

/* Caller:
 *   MON 075B GetStartByte:  mon_get_start_byte(r);
 *
 * NOTE: the exact input register (file number) and the numeric error code are
 * NOT recoverable from the carved window alone - the caller-side MON wrapper and
 * the shared return/error dispatch live just past this slice. INFERRED.
 */
