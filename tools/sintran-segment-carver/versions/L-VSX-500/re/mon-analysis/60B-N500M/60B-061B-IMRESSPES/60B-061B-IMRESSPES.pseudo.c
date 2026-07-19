/* MON 60B 061B IMRESSPES - reserve memory/CPU for the ND-500 test-monitor. From 5P-P2-MON60.NPL (.npl). */
int i_mresspes(CPU *cpu, Mon60Params *p)
{
    if (p->d11 == -1 && p->d12 != -1) {                 /* a specific CPU is named */
        if ((uint32_t)(p->a_cpu - 1) > (uint32_t)(NCPU - 1)) return mon60_error(cpu, EILPAR);
        Cpudf *df = &cpu->cpudf[p->a_cpu];              /* A*5CPUDFSZ+S5CPUDF */
        cpu->ccpudf = df;
        if (df->spref != 0 && p->a_cpu != RTREF) return mon60_error(cpu, ESPRES);
    }
    if (!tuson(cpu)) return mon60_error(cpu, EGENERR);  /* fail if in use by others */
    return mon60_common_path(cpu, 061, mon60_buffer(cpu));
}
