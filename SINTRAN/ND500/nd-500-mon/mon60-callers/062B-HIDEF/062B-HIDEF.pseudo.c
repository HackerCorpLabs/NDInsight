/* 062B-HIDEF -> MON 60 HIDEF = 62B (0x32 = 50 dec). Purpose: DEFINE HISTOGRAM (IDEFHIST).
 * Call site 040133 inside routine 040050 (framesize 000010). OCTAL, BANK 1, base 0. */
extern int  MON60_HIDEF(void);        /* JPL I ->040146; thunk 146544; SAA 62 */
extern word *p;
int define_histogram(void)
{
    p[6]  = &frame_B172;              /* 040122..040125 STA ,X 6  = &(B-172) */
    p[7]  = const_LDA15;              /* 040126 LDA 15; 040127 STA ,X 7 */
    p[10] = &frame_B166;              /* 040130..040132 STA ,X 10 = &(B-166) */
    if (MON60_HIDEF() == ERROR)       /* 040133 JPL I 13 -> thunk 146544 (62B) */
        return LEAVE_error(A);        /* 040134 callsite+1 -> 177327 LEAVE-with-value */
    return LEAVE_skip();              /* 040135 callsite+2 -> 177335 LEAVE-SKIP */
}
