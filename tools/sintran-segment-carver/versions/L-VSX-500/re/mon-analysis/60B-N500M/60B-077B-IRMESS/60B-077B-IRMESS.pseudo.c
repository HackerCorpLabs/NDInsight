/* MON 60B 077B IRMESS - read a process message buffer back to user. From 5P-P2-MON60.NPL (.npl).
 * Touches the 5MPM message buffer. */
int i_rmess(CPU *cpu, Mon60Params *p)
{
    uint32_t msgbuf;
    if (p->d11 == 0 && p->d12 >= SWPROC) {
        if (p->a == -1) msgbuf = pr_descr(cpu)->messbuff;      /* own process */
        else {
            if (p->a > MX5PROCS) return mon60_error(cpu, EILPAR);
            msgbuf = procdesc_of(cpu, p->a)->messbuff;
        }
    } else return mon60_error(cpu, EILFUNC);
    xsupd_window(cpu, MB_BANK, msgbuf);                        /* map the message */
    tousmove(cpu, p->p2_ptr, msgbuf, (MSGSIZE - MSNEGSIZE - 3) << 1);
    return mon60_ok(cpu);
}
