/* MON 60B 102B IFORGET - stop ND-500 system: abort all active procs, release buffers.
 * From 5P-P2-MON60.NPL (.npl). COMPLEX: exec-queue + 5MPM message buffers. */
int i_forget(CPU *cpu, Mon60Params *p)
{
    irq_off(cpu);
    cpu->nspref = RTREF;                        /* reserve system for special use */
    cpu->sysinitflag |= B5STOP;                 /* STOP-ND-500 mode */
    rstartall(cpu);                             /* remove all procs from exec-queue */
    irq_on(cpu);
    for (int n = SWPROC + 1; n <= MX5PROCS; n++) {
        Procdesc *pd = procdesc_of(cpu, n);
        if (pd->rtres == 0 || pd->rtres == RTREF) continue; /* skip free / caller-owned */
        irq_off(cpu);
        if (pd->status & BACKGROUND) pd->status |= ESCF;   /* escape shadow process */
        pd->pstat |= SYSABORT;
        slock(cpu);
        pd->messbuf.flags |= (IBRK | ESCSET);
        sunlock(cpu);
        trigger_abort(cpu, pd);                            /* IRW MLEVB, SYSABORT */
        irq_on(cpu);
    }
    return mon60_common_path(cpu, 102, mon60_buffer(cpu));
}
