/* 070B-GPRTE -> MON 60 GPRTE = 70B (0x38 = 56 dec). Purpose: READ A PROCESS TABLE
 * ENTRY FROM THE SYS.MON, handler 5NOPAR. Three call sites: 073472, 074030, 110410.
 * OCTAL, BANK 1, base 0. */
extern int  MON60_GPRTE(void);        /* JPL I -> thunk 146566; SAA 70 */
extern void err_110374(void);
extern word *p;
int gprte_site1(void)                 /* 073463..073474, routine 073412 */
{
    p[6] = &frame_B166;               /* 073466 STA ,X 6 */
    p[7] = &frame_B75;                /* 073471 STA ,X 7 */
    if (MON60_GPRTE() == ERROR)       /* 073472 JPL I 40 -> thunk 146566 (70B) */
        return LEAVE_error(A);        /* 073473 -> 177327 */
    /* 073474 success */
}
int gprte_site2(void)                 /* 074021..074032, routine 074013 */
{
    p[6] = &frame_B172;               /* 074024 STA ,X 6 */
    p[7] = &frame_B170;               /* 074027 STA ,X 7 */
    if (MON60_GPRTE() == ERROR)       /* 074030 JPL I 51 -> thunk 146566 (70B) */
        return LEAVE_error(A);        /* 074031 -> 177327 */
    /* 074032 success */
}
int gprte_site3(void)                 /* 110402..110412, routine 110365 */
{
    p[6] = const_LDA162;              /* 110402 LDA 162; 110404 STA ,X 6 */
    p[7] = &frame_B166;               /* 110407 STA ,X 7 */
    if (MON60_GPRTE() == ERROR)       /* 110410 JPL I 155 -> thunk 146566 (70B) */
        err_110374();                 /* 110411 JPL -15 -> 110374 (local handler) */
    /* 110412 success */
}
