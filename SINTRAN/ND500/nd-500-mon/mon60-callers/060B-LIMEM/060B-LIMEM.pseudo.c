/* 060B-LIMEM -> MON 60 LIMEM = 60B (0x30 = 48 dec). Purpose: LIST MEMORY CONFIGURATION, handler 5NOPAR.
 * Call site 135532 inside routine 135502 (framesize 000060). OCTAL, BANK 1, base 0. */
extern int  MON60_LIMEM(void);        /* JPL I ->135676; thunk 146541; SAA 60 */
extern word *p;
int list_memory_configuration(void)
{
    frame_B127 = -1;                  /* 135524 SAA -1; 135525 STA ,B -127 */
    p[6] = &frame_B164;               /* 135526..135531 STA ,X 6 = &(B-164) */
    if (MON60_LIMEM() == ERROR)       /* 135532 JPL I 144 -> thunk 146541 (60B) */
        return LEAVE_error(A);        /* 135533 callsite+1 -> 177327 LEAVE-with-value */
    /* 135534 callsite+2 = SUCCESS: process the returned config list at B-164 */
}
