/* ============================================================================
 * MON 121B  AwaitFileTransfer (WAITF)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.
 *
 * IMPORTANT - NOT BYTE-RECOVERABLE.  The AwaitFileTransfer worker (manual short
 * name WAITF) is NOT present in any carved segment: there is no WAITF symbol in
 * 006-S3FS (FILSYS-SYMBOLS) or in 025-S3IRPIT (SYMBOL-2-LIST), and the GOTAB
 * stub F1657 @122013B (see 121B-AwaitFileTransfer.ASM) holds no static pointer
 * to it.  The body below is therefore modelled ONLY from the documented behaviour
 * in the SINTRAN III Monitor Calls manual, not from real SINTRAN L bytes.  Every
 * line is flagged UNVERIFIED - do not treat it as byte-proven.
 *
 * Documented contract (manual + 121B_AwaitFileTransfer.yaml):
 *   in : file number (from OpenFile), wait flag
 *   out: status word:  0 = transfer finished
 *                     -1 = transfer not finished
 *                    > 0 = standard error code (appendix A)
 * The wait flag: 0 => the program is put in the I/O wait state until the transfer
 * completes; any other value => return immediately with the current state.
 * ND-500 programs never wait.
 * ============================================================================ */

int mon_121B_WAITF(int file_number, int wait_flag)   /* returns the status word */
{
    /* UNVERIFIED: locate the open-file entry for file_number and read the state
     * of its outstanding no-wait ReadFromFile / WriteToFile transfer. */
    int transfer_done   = check_file_transfer_state(file_number);  /* UNVERIFIED */
    int transfer_error  = last_transfer_error(file_number);        /* UNVERIFIED */

    if (transfer_error > 0)
        return transfer_error;          /* UNVERIFIED: > 0 = standard error code */

    if (transfer_done)
        return 0;                       /* UNVERIFIED: 0 = transfer finished     */

    /* transfer still in progress */
    if (wait_flag == 0) {
        /* UNVERIFIED: wait flag 0 => block in the I/O wait state until the
         * transfer completes, then report success (ND-500 callers never wait) */
        wait_for_io_completion(file_number);   /* UNVERIFIED */
        return 0;
    }
    return -1;                          /* UNVERIFIED: -1 = transfer not finished */
}

/* No byte-verified anchors exist for this call: the worker is uncarved.  The only
 * byte-proven fact is GOTAB[121B] = 122013B (the F1657 resident stub fragment),
 * which does not reach WAITF in any carved segment.  See README Honest caveats. */
