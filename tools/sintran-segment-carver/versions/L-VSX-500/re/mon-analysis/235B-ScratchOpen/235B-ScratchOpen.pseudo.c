/* ============================================================================
 * MON 235B  ScratchOpen (SCROP)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * Derived from the real disassembly (see 235B-ScratchOpen.ASM). Control flow, the
 * flag-fork mechanism, and the call to the FOPEN file-open primitive are
 * BYTE-VERIFIED; the semantic labels (which field is name / type / access) are
 * INFERRED from the SINTRAN III Monitor Calls manual and the field copies seen -
 * treat as a model, not gospel. Addresses in comments are octal.
 *
 * ScratchOpen is one of FOUR entries into a single shared OPEN body at 103041.
 * Each entry is a 3-word prelude that sets two STS scratch flags (M = SSM,
 * K = SSK) then jumps to the common body, which reads the two flags back to pick
 * the access mode. SCROP sets M=1,K=0. The file is opened as a scratch file;
 * a maximum of 64 pages is kept when the file is closed (see SET-CLOSED-FILE-SIZE).
 * It is closed like any other opened file.
 * ============================================================================ */

/* Manual register contract (from the MAC example, inferred - not byte-proven):
 *   X = address of file-name string
 *   A = address of default file-type string ; on return A = open file number
 *   T = access code (0..9: seq/random, read/write/append, contiguous, ...)
 * Error return: A = error number. */

int mon_scratchopen(mon_regs *r)
{
    /* --- Region A: SCROP flag-fork prelude (103031-103033) ------------------ */
    STS.M = 1;                             /* 103031: BSET ONE SSM (A |= 1<<M)     */
    STS.K = 0;                             /* 103032: BSET ZRO SSK (A &= ~(1<<K))  */
    goto open_body;                        /* 103033: JMP -> 103041                */

    /* --- Region B: shared OPEN body (103041-103347) ------------------------- */
open_body:
    save_params(r);                        /* 103041: STD I 173 - stash caller D   */
    resident_prologue();                   /* 103045: JPL I 170 -> 003752          */

    /* 103046-103062: read the two fork flags back, select the access mode.
     * SCROP arrives with M=1,K=0 -> mode = 2 (the scratch-open mode).            */
    if (STS.M) {                           /* 103046: BSKP ONE SSM                 */
        mode = STS.K ? -1 : 2;             /* 103052 SAA -1 / 103054 SAA 2 (Scratch) */
    } else {
        mode = STS.K ? 1 : 0;              /* 103060 SAA 1 / 103062 A=0            */
    }
    r->frame[0147] = mode;                 /* 103063: STA ,B 147                   */

    /* 103064-103071: copy 3 caller pointer words (name/type descriptors) into
     * the frame (indirect loads I 150/151/152 -> B+144/145/146).                 */
    stash_descriptors(r);

    if (r->frame[0] == 0) goto contiguous; /* 103072 LDX ,B 0 / 103073 JXZ -> 103143 */

    /* 103074-103202: locate / validate the open-file-table slot for this mode,
     * scanning entries and computing the record layout (SHA ZIN, AAX/AAT loops).
     * Parse/validation failures branch to store_status (103313).                 */
    if (locate_slot(r, mode) != OK)        /* JPL I workers 103245/103246/...      */
        goto store_status;

contiguous:                                /* 103143 path (frame[0]==0)            */
    setup_transfer(r);                     /* 103143-103202 resident JPL workers   */

finalize:                                  /* 103267: JMP 65 lands here            */
    /* 103267-103312: build FOPEN arguments and perform the open. */
    if (mode < 0)                          /* 103275 LDA ,B 147 / 103276 JAP       */
        r->T |= (1 << 15);                 /* 103277: BSET ONE 170 DT (bit 15)     */
    prep_open(r);                          /* 103300-103302: JPL I 35 -> 103337    */
    if (r->D == 0)                         /* 103303: SKP IF DD EQL 0              */
        rc = fopen_primitive(r);           /* 103310: JPL I 31 -> FOPEN 067432     */
    else
        rc = alt_open_worker(r);           /* 103305: JPL I 33 -> 103340           */
    r->frame[4] += 1;                      /* 103312: MIN ,B 4 - success flag      */

store_status:                              /* 103313                               */
    r->status = rc;                        /* 103313: STA ,B 2 -> caller           */
    restore_descriptors(r);                /* 103314-103321: STA I 23/24/25        */
resident_ret:
    resident_return();                     /* 103323: JMP I 22 -> 003776           */
    return rc;                             /* A = open file number on success       */
}

/* Byte-verified anchors:
 *   SCROP prelude 103031 (M=1,K=0), shared entry 103041 (STD I 173, SAB 150),
 *   prologue JPL I 170 -> 003752, flag readback BSKP ONE SSM/SSK -> mode 147,
 *   FOPEN call JPL I 31 -> 067432 (link cell 103341), status store STA ,B 2
 *   (103313), resident return JMP I 22 -> 003776 (link cell 103345). */
