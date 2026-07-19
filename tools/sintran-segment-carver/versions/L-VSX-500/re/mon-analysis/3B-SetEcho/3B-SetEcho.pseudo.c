/* ============================================================================
 * MON 3B - ECHOM - pseudo-C model for an emulator author.
 *
 * SINTRAN III VSX/500 L07.  Worker home: 026-S3IMPIT (monitor-PIT overlay),
 * symbol ECHOM=044540B.  The block-move control flow and the SSPTM page-table
 * switch are VERIFIED from bytes; the SEMANTIC PURPOSE is INFERRED - the carved
 * body is a page-crossing block copy, which does not obviously match the old
 * "SetEcho" gloss (see README caveats).
 *
 * Dispatch: MON 3B -> ENT14 072167B -> GOTAB[3B]=MFELL -> level switch to
 *           CALLP 032201B -> MCTAB[3B] @005623B = 044540B = ECHOM (below).
 * Constants octal.
 * ============================================================================
 */

void ECHOM(word *frame)                 /* entry 044540B */
{
    /* 044540B-044545B: block move #1 - from descriptor at frame[52]. */
    movew(dst_from(frame[052]), /*count*/ 0);

    /* 044546B-044561B: block move #2 - offset descriptor (AAT 21, SAA 32). */
    movew(dst2_from(frame[052], frame[053]), /*count*/ 032);

    call_helper_1(frame[062]);          /* 044563B: JPL I 30 -> 044613B */

    if (frame[064] != 0) {              /* 044564B-044565B: JAZ skip */
        /* 044566B-044576B: block move #3 across the ALTERNATE page table. */
        set_page_table(ALT);            /* 044602B: BSET ONE SSPTM */
        call_helper_2();                /* 044603B: JPL I 11 -> 044614B */
        set_page_table(NORMAL);         /* 044605B: BSET ZRO SSPTM */
    }

    /* 044606B-044610B: return path. */
    frame[-0115] += 1;                  /* MIN -115 */
    return_via(frame[-0117]);           /* 044610B: JMP I -117 -> 044471B */
}
