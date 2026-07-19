/* MON 60B 043B ISRES - reserve ND-500 CPU/system for special use. From 5P-P2-MON60.NPL (.npl). */
int i_sres(CPU *cpu, Mon60Params *p)
{
    if (p->d11 == 0 && p->d12 == 0) {                 /* reserve CPU only */
        if (ccpudf(cpu)->spref == RTREF) return mon60_ok(cpu);
        return tuson(cpu) ? mon60_ok(cpu) : mon60_error(cpu, EGENERR);
    } else {                                          /* reserve whole system */
        if (RTREF == cpu->nspref) return mon60_ok(cpu);
        cpu->nspref = p->a_cpu;
        return xtuson(cpu) ? mon60_ok(cpu) : mon60_error(cpu, EGENERR);
    }
}
