/* 051B-RIFRG -> MON 60 RIFRG = 51B (0x29 = 41 dec). Purpose: READ INTERFACE
 * (COMMUNICATION), IODATUT REGISTER; handler 5NOPAR. Call site 130136 inside
 * standalone ENTER-routine 127551 (framesize 000010). OCTAL, BANK 1, base 0. */
extern int  MON60_RIFRG(void);        /* JPL I ->130173; thunk 146514; SAA 51 */
extern word *p;
int read_interface_register(void)
{
    p[6] = &frame_B167;               /* 130132..130135 STA ,X 6 = &(B-167) */
    if (MON60_RIFRG() == ERROR)       /* 130136 JPL I 35 -> thunk 146514 (51B) */
        goto abort_B_141;             /* 130137 JMP I ,B -141 : dynamic B-relative error exit */
    /* 130140 success: fall through to next operation */
    return frame_B167;
abort_B_141:
    ;                                 /* runtime target = B-141 (saved abort vector) */
}
