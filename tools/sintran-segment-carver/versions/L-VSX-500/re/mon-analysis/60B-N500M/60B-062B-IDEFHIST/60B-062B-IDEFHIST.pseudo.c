/* MON 60B 062B IDEFHIST - define a PC-sampling histogram. From 5P-P2-MON60.NPL (.npl). */
int i_defhist(CPU *cpu, Mon60Params *p)
{
    if (cpu->hist_rtp != 0 && p->a != RTREF) return mon60_error(cpu, EHIUSED);
    cpu->hist_rtp = RTREF; cpu->hist_flag = 0;
    uint32_t chans = p->d3_channels;
    if (chans == 0 || chans > 0100) return mon60_error(cpu, EILPAR);   /* 100B max */
    cpu->hist_channels = chans;
    if (p->d2_interval == 0) return mon60_error(cpu, EILPAR);
    cpu->hist_interval = p->d2_interval;
    cpu->hist_start = p->d1_start;
    memset(cpu->hist_data, 0, HIST_DATA_WORDS * 2);   /* clear 5HIDATA..5HIOUTSIDE */
    return mon60_ok(cpu);
}
