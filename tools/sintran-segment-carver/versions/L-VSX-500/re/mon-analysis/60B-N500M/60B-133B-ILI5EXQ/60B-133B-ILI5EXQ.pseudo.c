/* MON 60B 133B/150B ILI5EXQ/ILI5TQU - list ND-500 exec-queue (K=0) / time-queue (K=1).
 * From 5P-P2-MON60.NPL (.npl). COMPLEX: walks the 5MPM queue in IOF. */
int i_li5f(CPU *cpu, Mon60Params *p, int K /* 0=exec, 1=time */)
{
    irq_off(cpu);                                         /* must run in IOF */
    map_mon60_buffer_window(cpu);
    uint16_t *out = logbuf(cpu);
    uint32_t node;
    if (K) { *out++ = cpu->atime; node = queue_head(cpu, X5BTI); }   /* time-queue */
    else   { *out++ = current_active_proc(cpu); node = queue_head(cpu, X5BEX); } /* exec-queue */
    for (; node != (uint32_t)-1; node = queue_next(cpu, node)) {
        if (node == DUMMESS) continue;
        int procno = msg_field(cpu, node, SENDE);
        if (procno == -1) continue;                       /* histogram/watchdog */
        *out++ = procno;
        if (K) *out++ = msg_field(cpu, node, D5TIM);      /* start-time */
        else   *out++ = msg_field(cpu, node, 5PRIO);      /* priority */
    }
    *out++ = (uint16_t)-1;                                /* end marker */
    irq_on(cpu);
    tousmove(cpu, p->p1_ptr, logbuf(cpu), (out - logbuf(cpu)) * 2);
    return mon60_ok(cpu);
}
