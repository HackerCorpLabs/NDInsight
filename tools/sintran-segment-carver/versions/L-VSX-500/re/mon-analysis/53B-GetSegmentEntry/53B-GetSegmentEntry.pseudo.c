/* ============================================================================
 * MON 53B  GetSegmentEntry (RSEGM)  -  pseudo-C model for an emulator
 * SINTRAN III VSX/500 L.  Returns the 5-word segment-table entry for an ND-100
 * segment number (segment 0 = RT common).
 *
 * Dispatch reality:
 *   GOTAB[53B] = 121174B -> F1634 (BYTE-VERIFIED).  F1634's first instruction is
 *   an indirect JMP through link cell 121211B (= 010341B) into the RESIDENT
 *   monitor - NOT present in any carved segment (uncarved bridge).  The actual
 *   segment-entry read (the RSEGM behaviour) is performed there.  The manual
 *   short name RSEGM resolves to RSEGM=000021B (a data constant / table index,
 *   NOT a code address), so there is no separate worker body to model from
 *   bytes.  The model below is of the DOCUMENTED behaviour; only the F1634 entry
 *   thunk is byte-derived.
 * Addresses in comments are octal.
 * ============================================================================ */

/* Manual parameter contract (from 53B_GetSegmentEntry.yaml; INFERRED, MAC form):
 *   A -> parameter list: { SegmentNumber, Buffer }
 *   SegmentNumber : IN  segment number (0 = RT common)
 *   Buffer        : OUT 5-word (10-byte) segment-table entry
 *   Error return  : standard error code (MAC: JMP ERROR after MON 53) */

int mon_53B_GetSegmentEntry(mon_regs *r)   /* A -> {SegmentNumber, Buffer} */
{
    /* --- Byte-verified entry thunk (F1634 @121174B, 025-S3IRPIT) --- */
    /* 121174 JMP I 15 -> [121211] = 010341 : the ONLY instruction on the MON 53   */
    /* dispatch path - an unconditional indirect jump into the RESIDENT routine    */
    /* 010341B (UNCARVED).  Everything below models what that resident routine does */
    /* per the manual; it is NOT byte-derived.                                     */
    resident_segment_entry_worker();   /* 121174: JMP I -> 010341 (UNVERIFIED body) */

    /* Documented behaviour of the resident worker (from the manual, INFERRED): */
    int seg = param.SegmentNumber;                 /* IN */
    segtab_entry *e = &segment_table[seg];         /* locate 5-word segment entry */
    for (int i = 0; i < 5; i++)                     /* copy 5 words (10 bytes) */
        param.Buffer[i] = e->word[i];
    return 0;                                       /* + standard error code */
}

/* Byte-verified anchors (F1634 @121174B, 025-S3IRPIT, GOTAB[53] target):
 *   121174 JMP I 15 -> cell 121211 (= 010341, RESIDENT, uncarved)
 *   121175 LDT ,B 21    (T = mem[B+21])       shared F163x-family code
 *   121176 RADD CLD ST DB (B = T)             shared F163x-family code
 *   121177 STA ,B 12    (mem[B+12] = A)        shared F163x-family code
 *   121200 JMP I 11 -> cell 121211 (= 010341, RESIDENT, uncarved)
 *   121201..121214 = link-cell / pointer table (DATA, not code).
 *
 * Caveats for the emulator author:
 *   - GOTAB[53B]=121174B is BYTE-VERIFIED; F1634's thunk (JMP I -> 010341) is the
 *     only byte-proven step.  The segment-entry read itself is in the RESIDENT
 *     routine 010341B, which is UNCARVED - the loop above is the manual's model,
 *     NOT carved code.
 *   - RSEGM=000021B (L07 SYMBOL-1-LIST) is a DATA constant, not a worker address;
 *     there is no ND-100 RSEGM code region to carve.
 *   - A live PC trace (break at 121174B on a real MON 53, single-step the JMP I
 *     into 010341B) is needed to confirm the real worker.
 */
