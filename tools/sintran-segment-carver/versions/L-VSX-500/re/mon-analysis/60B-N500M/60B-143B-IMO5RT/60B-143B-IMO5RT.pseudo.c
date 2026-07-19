/* MON 60B 143B IMO5RT - activate a program in the ND-500 or ND-100. From 5P-P2-MON60.NPL (.npl). */
int i_mo5rt(CPU *cpu, Mon60Params *p)
{
    Rtdesc *rt = (p->d1 == 0) ? rt_of(cpu, RTREF) : rt_of(cpu, p->d1);
    if (!goodrt(cpu, rt)) return mon60_error(cpu, EILPAR);
    irq_off(cpu);
    Procdesc *pd = fsema(cpu, p->d1);
    if (pd != NULL) pr_activate(cpu, pd);          /* activate the ND-500 proc */
    return iim5rt(cpu);                            /* execute the RT MON (EXR SD) */
}
