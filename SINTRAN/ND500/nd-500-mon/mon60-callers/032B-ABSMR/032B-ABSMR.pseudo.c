/* ===========================================================================
 *  ABSMR  ->  MON 60 subfunction 032B (0x1A = 26 dec) PHYSICAL DATA MEMORY READ
 * ---------------------------------------------------------------------------
 *  Source : SINTRAN/ND500/nd-500-mon/nd-500-mon-j04.prog.asm
 *  FOUR call sites, all resolving to thunk 146426 (SAA 32).
 *  Return convention (PROVEN): callsite+1 = ERROR, callsite+2 = SUCCESS.
 *  yaml params: <no. of bytes> <ND-500 addr.> <data area> <bytes returned>.
 * ===========================================================================
 */

extern int MON60_ABSMR(void);
extern word *p;

/* Site A @ 012721 (routine 012700): data area is a single word -> ,X 10;
 *                                    bytes-returned at ,X 11.               */
int absmr_A(void)
{
    p[6]  = nbytes;                    /* 012710 = value (LDA 54 P-rel) */
    p[7]  = &nd500_addr;               /* 012713 = &(B-165) */
    p[10] = data_area;                 /* 012715 = (B-170) value */
    p[11] = &bytes_returned;           /* 012720 = &(B-163) */
    if (MON60_ABSMR() == ERROR) return LEAVE_value();  /* 012722 -> 012764 */
    /* 012723 SUCCESS continues */
}

/* Sites B/C/D: data area is a 3-word F descriptor -> ,X 10;
 *              bytes-returned at ,X 13 (11/12 taken by the F).             */
int absmr_B(void)   /* @ 022616 (routine 022310) */
{
    p[6]  = &nbytes;                   /* 022604 = &(B-160) */
    p[7]  = nd500_addr;                /* 022606 = value (LDA -67 P-rel) */
    p[10] = data_area_desc;            /* 022612 STF = 3-word F */
    p[13] = &bytes_returned;           /* 022615 = &(B-170) */
    if (MON60_ABSMR() == ERROR) goto err_022623;       /* 022617 */
    /* 022620 JMP -> 022621 SUCCESS */
}

int absmr_C(void)   /* @ 056364 (routine 056042) */
{
    p[6]  = nbytes;                    /* 056346 = value (LDA -77 P-rel) */
    p[7]  = &nd500_addr;               /* 056351 = &(B-163) */
    p[10] = data_area_desc;            /* 056360 STF = 3-word F */
    p[13] = &bytes_returned;           /* 056363 = &(B-165) */
    if (MON60_ABSMR() == ERROR) goto err_056240;       /* 056365 */
    goto ok_056452;                    /* 056366 */
}

int absmr_D(void)   /* @ 131163 (routine 130475) */
{
    p[6]  = &nbytes;                   /* 131151 = &(B-66) */
    p[7]  = nd500_addr;                /* 131153 = value (LDA -37 P-rel) */
    p[10] = data_area_desc;            /* 131157 STF = 3-word F */
    p[13] = &bytes_returned;           /* 131162 = &(B-110) */
    if (MON60_ABSMR() == ERROR) goto handler_131107;   /* 131164 */
    /* 131165 SUCCESS continues */
}

/* PROVEN slot difference: site A places <data area> as a single word at ,X 10 and
 * <bytes returned> at ,X 11; sites B/C/D place a 3-word F descriptor at ,X 10 and
 * <bytes returned> at ,X 13.  Read from the bytes, not assumed. */
