/* MON 60B 117B/122B IPRABORT/ILOGOFF - abort / logoff an ND-500 process. From 5P-P2-MON60.NPL (.npl).
 * COMPLEX: 5MPM message buffer + level-driven abort. */
int i_prabort(CPU *cpu, Mon60Params *p)
{
    if (p->d11 != 0 || p->d12 <= SWPROC || p->a > MX5PROCS) return mon60_error(cpu, EILPAR);
    Procdesc *pd = procdesc_of(cpu, p->a);
    if (pd->rtres == 0) return mon60_ok(cpu);              /* not reserved -> nothing to do */
    bool caller = (pd->rtres == RTREF);
    if (p->function == PRSTOP) {                           /* ABORT */
        if (caller) { p->function = ABREL; return mon60_common_path(cpu, ABREL, mon60_buffer(cpu)); }
        pd->pstat = (pd->pstat | SYSABORT) & ~SOFFLOGG;
    } else {                                               /* LOGOFF */
        if (caller) { pd->pstat = (pd->pstat | SOFFLOGG) & ~SYSABORT;
                      p->function = XN5REL; return mon60_common_path(cpu, XN5REL, mon60_buffer(cpu)); }
        pd->pstat = (pd->pstat | SOFFLOGG) & ~SYSABORT;
    }
    /* INPRABORT shared tail: mark in-break + drive abort on the process level */
    slock(cpu);
    pd->messbuf.flags |= (IBRK | ESCSET);
    sunlock(cpu);
    trigger_abort(cpu, pd);                                /* IRW MLEVB, SYSABORT */
    return mon60_ok(cpu);
}
