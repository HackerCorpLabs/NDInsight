/* MON 60B 066B IRELHIST - stop and release the histogram facility. From 5P-P2-MON60.NPL (.npl). */
int i_relhist(CPU *cpu, Mon60Params *p)
{
    if (RTREF != cpu->hist_rtp) return mon60_error(cpu, EHNRESERVED);
    if (cpu->hist_flag != 0) rlhilog(cpu);
    cpu->hist_rtp = 0;
    return mon60_ok(cpu);
}
