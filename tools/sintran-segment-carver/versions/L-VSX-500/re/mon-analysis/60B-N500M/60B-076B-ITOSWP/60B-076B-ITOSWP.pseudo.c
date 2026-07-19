/* MON 60B 076B ITOSWP - copy a swapper message from user. From 5P-P2-MON60.NPL (.npl). */
int i_toswp(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    /* first 7 words are a header set elsewhere; copy the remaining (55MESSIZE-7) words as bytes */
    frusmove(cpu, buf->swpmsg, p->p1_ptr + 7, (SWMSG_WORDS - 7) << 1);
    return mon60_common_path(cpu, 076, buf);
}
