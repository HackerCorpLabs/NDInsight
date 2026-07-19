/* ============================================================================
 * MON 12B  SetCommandBuffer (SETCM)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Transfers a string (up to 32 characters) into the
 * command buffer - the buffer holding the last command typed at the terminal
 * (readable by reading logical device number 0).  Useful to erase sensitive
 * information (e.g. password parameters) from the command buffer.
 *
 * Dispatch reality:
 *   GOTAB[12B] = 000000 -> FALL-THROUGH (BYTE-VERIFIED).  There is no direct
 *   GOTAB handler word, so the level-14 handler is reached through the resident
 *   MFELL/CALLPROC path - NOT present in any carved segment (uncarved bridge).
 *   There is NO ND-100 code worker in the carved segments: SETCM=106214B is the
 *   ND-500 companion (N500-SYMBOLS), and CBUF=170207B (SYMBOL-2-LIST) is the
 *   command-buffer DATA area (zero-filled in this L image).  The model below is
 *   the DOCUMENTED behaviour only, NOT carved code.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Manual parameter contract (from 12B_SetCommandBuffer.yaml; INFERRED, MAC form):
 *   A -> Command : string (up to 32 characters) to place in the command buffer
 *   The parameter is fetched through the ALTERNATIVE page table. */

int mon_12B_SetCommandBuffer(mon_regs *r)   /* A -> Command string (<= 32 chars) */
{
    /* Documented model (NOT carved: no ND-100 worker body in these bytes).
     * The command buffer is the resident data area CBUF (170207B).  The string
     * is copied from the caller through the alternative page table into CBUF,
     * truncated/padded to the 32-character buffer. */
    for (int i = 0; i < 32; i++)
        command_buffer[i] = alt_page_read(param.Command + i);   /* alt page table */
    return 0;
}

/* Caveats for the emulator author:
 *   - GOTAB[12B]=000000 (fall-through) is BYTE-VERIFIED; there is no entry stub
 *     to model.  Dispatch enters the resident MFELL/CALLPROC (UNCARVED).
 *   - No ND-100 worker exists in the carved segments; the loop above is the
 *     manual's behaviour only.  SETCM=106214B is the ND-500 companion.
 *   - CBUF=170207B is the command-buffer DATA area (zero-filled here), not code.
 *   - The "alternative page table" fetch is per the manual; the exact copy loop
 *     lives past the uncarved bridge and is not byte-derived.
 *   - A live PC trace (break on a real MON 12, single-step the fall-through) is
 *     needed to confirm the real worker.
 */
