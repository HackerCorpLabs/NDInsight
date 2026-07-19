/* MON 60B 064B ISTOHIST - stop histogram sampling. From 5P-P2-MON60.NPL (.npl). */
int i_stohist(CPU *cpu, Mon60Params *p)
{
    if (RTREF != cpu->hist_rtp) return mon60_error(cpu, EHNRESERVED);
    if (cpu->hist_flag == 1) rlhilog(cpu);   /* dequeue histogram message */
    cpu->hist_rtp = 0;
    return mon60_ok(cpu);
}
