/* MON 60B 100B/101B RRFLAG/WWFLAG - read/write ND-500 process flag word. From 5P-P2-MON60.NPL (.npl). */
int i_flags(CPU *cpu, Mon60Params *p)
{
    Procdesc *pd;
    if (p->d12 == -1) pd = pr_descr(cpu);                 /* own process */
    else {
        if (p->a <= SWPROC || p->a > MX5PROCS) return mon60_error(cpu, EILPAR);
        pd = procdesc_of(cpu, p->a);
        if (pd->rtres == 0) return mon60_error(cpu, EILPAR);
    }
    map_nd500_dataseg(cpu, pd);                           /* M1MEXY */
    if (p->function == 5RFLAG) {                          /* read */
        uint32_t flags = read_flagword(cpu, FF500);
        restore_segments(cpu);
        stds0(cpu, p->p2_ptr, flags);                     /* copy to user */
    } else {                                              /* write */
        if (pd != pr_descr(cpu) && cpu->background && cpu->passtype == 0)
            return mon60_error(cpu, ENAUTHORISED);
        write_flagword(cpu, FT500, p->d2);
        restore_segments(cpu);
    }
    return mon60_ok(cpu);
}
