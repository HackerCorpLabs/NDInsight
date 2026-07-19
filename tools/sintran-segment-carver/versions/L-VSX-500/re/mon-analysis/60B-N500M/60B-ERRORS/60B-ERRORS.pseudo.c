/* MON 60B error entry points + ERET. From 5P-P2-MON60.NPL (.npl).
 * Each error label loads a code and returns via ERET (direct return = error). */
enum Mon60Error {
    ENIMPLEMENT,    /* not implemented           (EENIMPLEMENT) */
    EFUNRTP,        /* not legal for RT-program   (EEFUNRTP)     */
    ENOPROC,        /* no ND-500 process          (EENPROC)      */
    ENOPCOMMU,      /* no ND-500 communication    (EENPCOMMU)    */
    ENAUTHORISED,   /* not authorised (not SYSTEM)(EENAUTHORIZED)*/
    ESPRES,         /* reserved for special use   (EESPRES)      */
    EC174,          /* illegal parameter          (EEILPAR)      */
    EILFUNC,        /* illegal function code      (ILLFUNC)      */
};
int mon60_error(CPU *cpu, enum Mon60Error code)
{
    cpu->zareg = code;                 /* save error code (ERET) */
    /* N5REL first-access special case -> ok */
    if (cpu->function == N5REL && !(cpu->sysinitflag & BFIRSTACCESS)) return mon60_ok(cpu);
    return mon60_direct_return(cpu);   /* direct return = error (RET5) */
}
