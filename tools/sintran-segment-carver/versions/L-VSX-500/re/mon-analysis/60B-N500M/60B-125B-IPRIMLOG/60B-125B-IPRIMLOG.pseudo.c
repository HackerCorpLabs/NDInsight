/* MON 60B 125B IPRIMLOG - read moncall-log data to user. From 5P-P2-MON60.NPL (.npl). */
int i_primlog(CPU *cpu, Mon60Params *p)
{
    if (cpu->mlog != RTREF) return mon60_error(cpu, ELOGNRESERVED);
    xsupd_window(cpu, MB_BANK, mon60_logbuf_addr(cpu));   /* 5FBUM60 */
    tousmove(cpu, p->p1_ptr, mon60_logbuf(cpu), 03000);   /* 3000B bytes */
    return mon60_ok(cpu);
}
