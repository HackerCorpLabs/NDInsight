/* MON 60B 065B IREAHIST - read histogram data back to user. From 5P-P2-MON60.NPL (.npl). */
int i_reahist(CPU *cpu, Mon60Params *p)
{
    if (RTREF != cpu->hist_rtp) return mon60_error(cpu, EHNRESERVED);
    movus(cpu, p->p1_ptr, cpu->hist_data, 0200);          /* 200B, 2 words/channel */
    stds0(cpu, p->p1_ptr + 0200, cpu->hist_outside);      /* outside-range count */
    return mon60_ok(cpu);
}
