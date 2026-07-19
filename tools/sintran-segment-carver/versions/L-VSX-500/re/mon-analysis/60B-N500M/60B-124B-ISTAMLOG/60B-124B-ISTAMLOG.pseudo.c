/* MON 60B 124B ISTAMLOG - start monitor-call logging. From 5P-P2-MON60.NPL (.npl). */
int i_stamlog(CPU *cpu, Mon60Params *p)
{
    if (cpu->mlog != RTREF && p->a != 0) return mon60_error(cpu, ELOGINUSE);
    if (p->d1 != 0) {                                     /* log all processes */
        if (cpu->background && cpu->passtype != 2) return mon60_error(cpu, ENAUTHORISED);
        cpu->mlogproc = -1;
    } else {
        cpu->mlogproc = caller_procno(cpu);              /* own process */
    }
    cpu->mlog = RTREF;
    memset(mon60_logbuf(cpu), 0, MON60_BUF_WORDS * 2);   /* repurpose first MON60 buffer */
    return mon60_ok(cpu);
}
