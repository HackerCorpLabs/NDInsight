/* MON 60B 063B ISTAHIST - start histogram sampling (insert HIMESS into the ND-500 exec queue).
 * From 5P-P2-MON60.NPL (.npl). COMPLEX: touches 5MPM message buffer + exec-queue. */
int i_stahist(CPU *cpu, Mon60Params *p)
{
    if (RTREF != cpu->hist_rtp) return mon60_error(cpu, EHNRESERVED);
    if (cpu->hist_flag != 0) return mon60_ok(cpu);      /* already started */
    uint16_t procno = caller_procno(cpu);               /* RDIV(5PRDESCR-S500S, 5PRDSIZE) */
    /* build & queue the histogram message under lock */
    irq_off(cpu);
    himf_dequeue(cpu);                                  /* ensure not already queued */
    slock(cpu);
    Msgbuf *mb = &cpu->procdesc[procno].messbuf;
    mb->flags |= CPUBOUND;                              /* 5CPUBOUND in 5MSFL */
    cpu->himess.procno = procno;                        /* WANTP field */
    ito500xq(cpu, gcpudf(cpu));                         /* insert HIMESS into ND-500 exec queue */
    sunlock(cpu);
    cpu->hist_flag = 1;                                 /* started */
    irq_on(cpu);
    return mon60_ok(cpu);
}
