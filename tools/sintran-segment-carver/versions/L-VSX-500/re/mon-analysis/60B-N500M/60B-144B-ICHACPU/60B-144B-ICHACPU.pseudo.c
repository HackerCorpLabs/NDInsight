/* MON 60B 144B ICHACPU - change the ND-500 CPU the caller's process is bound to. From 5P-P2-MON60.NPL (.npl). */
int i_chacpu(CPU *cpu, Mon60Params *p)
{
    if ((uint32_t)(p->d12 - 1) > (uint32_t)(NCPU - 1)) return mon60_error(cpu, EILPAR);
    Cpudf *tgt = &cpu->cpudf[p->a];
    if (!(tgt->cpuavailable & ALIVE)) return mon60_error(cpu, ENOCPU);
    if (cpu->background && cpu->passtype == 0 && (p->a & EXCLUDE)) return mon60_error(cpu, ENAUTHORISED);
    if (tgt->spref != 0 && p->a != RTREF) return mon60_error(cpu, ESPRES);
    Cpudf *old = gcpudf(cpu);
    if (old->spref == RTREF && old != tgt) relcpu(cpu);        /* release old special-use CPU */
    Cpudf *ncpu = &cpu->cpudf[p->d12 - 1];
    Msgbuf *mb = &pr_descr(cpu)->messbuf;
    irq_off(cpu);
    mb->cpun = ncpu->cpuno;                                    /* new CPU-DF in message */
    mb->flags |= CPUBOUND;                                     /* CPU-bound process */
    irq_on(cpu);
    return mon60_ok(cpu);
}
