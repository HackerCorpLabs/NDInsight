/* MON 60B 154B IDBUGSW - debug swapper on/off. From 5P-P2-MON60.NPL (.npl). */
int i_dbugsw(CPU *cpu, Mon60Params *p)
{
    if (p->d12 == 1) { if (!xtuson(cpu)) return mon60_error(cpu, EGENERR); } /* on: fail if in use */
    return mon60_common_path(cpu, 0154, mon60_buffer(cpu));
}
