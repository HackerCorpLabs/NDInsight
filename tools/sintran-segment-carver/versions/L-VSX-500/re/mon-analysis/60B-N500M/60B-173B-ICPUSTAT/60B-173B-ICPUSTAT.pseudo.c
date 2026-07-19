/* MON 60B 173B ICPUSTAT - set CPU status by patching the 5PIT page tables. From 5P-P2-MON60.NPL (.npl). */
static void patch_5pit(CPU *cpu, int pit, uint16_t d42) { /* set 5NOTP or 5EXCL bit in the PIT entry */
    uint16_t e = get1l(cpu, pit);
    e = (d42_page_avail(d42) == 0) ? (e | BIT_5NOTP) : (e | BIT_5EXCL);
    put1l(cpu, pit, e);
    dalton(cpu, A5PIT); mon_2wseg(cpu, pit); altoff(cpu);
}
int i_cpustat(CPU *cpu, Mon60Params *p)
{
    if (p->d11 != 0 || (uint32_t)(p->d12 - 1) > (uint32_t)(NCPU - 1) || p->d41 != 0 || p->d42 > 3)
        return mon60_error(cpu, EILPAR);
    if (p->d22 != 0) patch_5pit(cpu, PIT_5IDPIT, p->d42);
    if (p->d32 != 0) patch_5pit(cpu, PIT_5SDPIT, p->d42);
    return mon60_ok(cpu);
}
