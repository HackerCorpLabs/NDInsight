/* MON 60B 055B ISPLACE - start-place: clear 55REP bit in the process message-buffer flags.
 * From 5P-P2-MON60.NPL (.npl). Touches the 5MPM message buffer. */
int i_splace(CPU *cpu, Mon60Params *p)
{
    uint32_t mb = pr_descr(cpu)->messbuff;               /* X:=5PRDESCR.MESSBUFF */
    irq_off(cpu);
    uint16_t fl = read_bank(cpu, MB_BANK, mb + OFF_5MSFL);/* *AAX 5MSFL; LDATX */
    fl &= ~BIT_55REP;                                    /* A BZERO 55REP */
    write_bank(cpu, MB_BANK, mb + OFF_5MSFL, fl);        /* *STATX */
    irq_on(cpu);
    return mon60_common_path(cpu, 055, mon60_buffer(cpu));
}
