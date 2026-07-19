/* MON 60B 135B IABLOG - logoff process and abort the owning RT-program. From 5P-P2-MON60.NPL (.npl). */
int i_ablog(CPU *cpu, Mon60Params *p)
{
    if (p->d11 != 0) return mon60_error(cpu, EILPAR);
    Rtdesc *rt = (p->d12 == 0) ? rt_of(cpu, RTREF) : rt_of(cpu, p->d12);
    if (rt->status & BACKGR) return mon60_error(cpu, EILPAR);   /* only RT-programs */
    if (!goodrt(cpu, rt)) return mon60_error(cpu, EILPAR);
    irq_off(cpu);
    Procdesc *pd = fsema(cpu, p->d1);                           /* the ND-500 proc it reserved */
    if (pd == NULL) return goto_5abprog(cpu);
    pd->pstat |= (SYSABORT | SOFFLOG);
    return inprabort(cpu, pd);                                  /* shared abort tail (117B) */
}
