/* ============================================================================
 * MON 311B - WriteDirEntry (WDIEN) - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Derived from the carved bytes in 311B-WriteDirEntry.ASM.
 * The SSM/SSK mode latch, the shared prologue and the two flag forks are VERIFIED
 * from bytes; the directory-entry copy in the shared body is INFERRED (manual +
 * the WDIEN/GDIEN/GNAEN family structure).
 *
 * Dispatch: MON 311B -> ENT14 072167B -> GOTAB[311B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[311B] @006131B = 107106B = WDIEN (worker below).
 * WDIEN, GDIEN and GNAEN are three entries into ONE body, distinguished only by
 * the SSM/SSK status bits latched at entry.  All constants octal.
 * ============================================================================
 */

/* Entry mode bits (latched at each entry, tested at 107123B / 107131B): */
/*   WDIEN (MON 311B): SSM=1, SSK=0  -> write, index lookup                */
/*   GDIEN (MON 244B): SSM=0, SSK=0  -> read,  index lookup                */
/*   GNAEN (MON 245B): SSM=0, SSK=1  -> read,  name  lookup                */

void WDIEN(int dir_index, word *entry_buf)   /* entry 107106B; SSM=1, SSK=0 */
{
    int write_mode  = 1;    /* 107106B: BSET ONE SSM */
    int name_lookup = 0;    /* 107107B: BSET ZRO SSK */

    /* 107116B-107122B: shared prologue - save regs, call directory helper. */
    directory_helper();     /* 107122B: JPL I 107 -> 107231 */

    /* 107123B-107137B: turn the latched mode bits into frame flags. */
    frame[0104] = write_mode  ? 1 : 0;   /* BSKP ONE SSM -> ,B 104 */
    frame[0105] = name_lookup ? 1 : 0;   /* BSKP ONE SSK -> ,B 105 */

    /* Shared body (from 107137B) validates the SYSTEM-user + reserved-directory
     * preconditions and, on the write path (frame[0104]==1), copies the 48-byte
     * directory entry from entry_buf into directory entry dir_index.
     * INFERRED - the copy body is not carved in this folder. */
    write_directory_entry(dir_index, entry_buf);   /* INFERRED */

    return_ok();
}
