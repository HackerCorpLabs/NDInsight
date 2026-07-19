/* MON 60B 024B/157B IWCNTS - write control-store words from user. From 5P-P2-MON60.NPL (.npl). */
int i_wcnts(CPU *cpu, Mon60Params *p)
{
    Mon60Buffer *buf = mon60_buffer(cpu);
    if (p->d22_cswords > 02000)                 /* 2000B CS-words max */
        return mon60_error(cpu, EGENERR);
    frusmove(cpu, buf->cs_words, p->p3_ptr, p->d22_cswords << 1); /* words -> bytes */
    return mon60_common_path(cpu, p->function, buf);
}
