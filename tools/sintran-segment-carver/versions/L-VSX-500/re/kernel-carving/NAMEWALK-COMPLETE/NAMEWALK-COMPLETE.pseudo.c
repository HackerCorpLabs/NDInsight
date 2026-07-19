/* =========================================================================
 * NAMEWALK-COMPLETE.pseudo.c
 * Readable pseudo-C of the SINTRAN III L07 directory NAME-WALK machinery
 * that @ENTER-DIRECTORY (ENDIR) uses to resolve a directory name to an
 * on-disk directory-entry index.  Segment 006-S3FS (FILSYS), base 26000B.
 * Reconstructed from byte-verified disassembly (see NAMEWALK-COMPLETE.ASM).
 * All addresses OCTAL with trailing B.  This is LOGIC, not literal codegen.
 *
 * KEY FACT: this entire family issues NO device transfer.  It walks two
 * in-core per-directory tables:
 *    - DIRECTORY table : 2-word entries, accessor GDIRT/PDIRT (+ default GDDRT)
 *    - NAME      table : 2-word entries, accessor GNAMT/PNAMT (+ default GDNMT)
 * The block-0 read that the SCSI mount is missing is NOT here (see
 * ../COLDE-CONNECT/ : it is CHDSI->RXDIR->RCBLO->driver, and the fork lives in
 * the SCSI disk driver 065-S3SIPIT).
 * ========================================================================= */

/* ---- table entry shapes (VERIFIED: bodies copy exactly 2 words) ---------- */
typedef struct { word w0, w1; } dir_entry;   /* directory-table entry (2 wds) */
typedef struct { word w0, w1; } name_entry;   /* name-table entry      (2 wds) */

/* ---- resident helpers (out of this segment) ------------------------------ */
extern void enter_setup(void);   /* resident 003752B - ubiquitous prologue    */
extern word GTTCH(ptr spec);     /* 030070B - get one char from the spec      */
extern void helper_004735(void); /* resident, COLDE-only; role OPEN           */


/* =========================================================================
 * PART A - the 2-word table accessors (4-way BSET split, ONE shared body)
 * Same idiom as RDISK/WDISK and COLDE/DCOLD/XCOLD: tiny stubs set two mode
 * flags then fall into a shared body.
 *   SSM (1st BSET) : DEFAULT(1) vs USER(0) table   [default/user = INFERRED]
 *   SSK (2nd BSET) : PUT/write(1) vs GET/read(0)   [VERIFIED]
 * ========================================================================= */

/* directory-table stubs -> dir_table_body (050134B) */
int GDDRT(int idx, word *p){ return dir_table_body(idx,p,/*def*/1,/*wr*/0);} /*050121*/
int GDIRT(int idx, word *p){ return dir_table_body(idx,p,/*def*/0,/*wr*/0);} /*050124 READ*/
int PDDRT(int idx, word *p){ return dir_table_body(idx,p,/*def*/1,/*wr*/1);} /*050127*/
int PDIRT(int idx, word *p){ return dir_table_body(idx,p,/*def*/0,/*wr*/1);} /*050132*/

int dir_table_body(int idx, word *p, int def, int wr)          /* 050134B */
{
    enter_setup();                                 /* 050140B                */
    dir_entry *e = table_base(def) + (idx+1)*W;     /* 050141..050152B: addr  */
                                                    /* = (idx+1)*W + base     */
    if (e > upper_bound) return err_status;         /* 050154B range check    */
    if (e < lower_bound) return 174;                /* 050163B  status 174B   */

    if (!wr) {                     /* GET: table -> params                    */
        p[0] = e->w0;              /* 050167B                                 */
        if (second_word) p[1] = e->w1;  /* 050173B                            */
    } else {                       /* PUT: params -> table                    */
        e->w0 = p[0];              /* 050176B                                 */
        if (second_word) e->w1 = p[1]; /* 050202B                            */
    }
    return OK;                                       /* 050205B -10 (ok)      */
}

/* name-table stubs -> name_table_body (050233B), structurally identical */
int GDNMT(int idx, word *p){ return name_table_body(idx,p,1,0);} /* 050220 */
int GNAMT(int idx, word *p){ return name_table_body(idx,p,0,0);} /* 050223 READ */
int PDNMT(int idx, word *p){ return name_table_body(idx,p,1,1);} /* 050226 */
int PNAMT(int idx, word *p){ return name_table_body(idx,p,0,1);} /* 050231 */
/* name_table_body @050233B: same 2-word copy shape as dir_table_body.        */


/* =========================================================================
 * PART D - GDIRA / GNAMA : leaf entry-address calculators
 * BYTE-IDENTICAL 6-word leaf routines (no stack frame).  Both compute the
 * address of table entry #index as (index+1)*W and hand it back in X.
 * The SAME calculator serves the directory table (GDIRA) and the name table
 * (GNAMA) - the two tables share stride W.
 * (VERIFIED: (index+1)*W shape and byte-identity.  OPEN: source of W.)
 * ========================================================================= */
void *GDIRA(void *base, int index){ return base_reg(base), (index+1)*W; } /*030225*/
void *GNAMA(void *base, int index){ return base_reg(base), (index+1)*W; } /*030235*/


/* =========================================================================
 * PART E - CLPAR : parse/classify the leading parameter of the spec
 * NOTE: prior carve labelled this "clear parameter block".  The BYTES show a
 * PARSER: it reads two characters via GTTCH and matches them against ASCII
 * letters {A,B,D,F,I,L,O,P,S,U,X}.  It classifies a 1-2 letter keyword/type
 * code at the head of the spec.  (VERIFIED it calls GTTCH + compares ASCII;
 * the exact accepted grammar is OPEN.)
 * ========================================================================= */
int CLPAR(spec_t *s)                                           /* 044777B */
{
    enter_setup();                                 /* 045003B                */
    char c1 = GTTCH(s);            /* 045007B  first char                     */
    char c2 = GTTCH(s);            /* 045013B  second char                    */

    /* Decision tree over ASCII letters (exact grammar OPEN).  Recognises a   */
    /* small set of leading type/keyword codes; on a recognised token it      */
    /* jumps to the accepted-token handler [045123]=040730, otherwise it      */
    /* rejects at 045125B.  Letters tested (octal): P=120 S=123 A=101 U=125   */
    /* F=106 I=111 D=104 B=102 L=114 O=117 X=130.                             */
    if (recognised(c1, c2))        /* 045015..045076B                         */
        return classify_token();   /* 045077B -> handler                      */
    return reject;                 /* 045125B                                 */
}


/* =========================================================================
 * PART F - GNAMI : build/search the name index
 * Packs an ASCII name (up to 7 chars) into name-table word form, then probes
 * the NAME table (reading slots via GNAMT) with a 3-way outcome: free slot /
 * exact match / grow.  Returns the resolved name-table slot in the params.
 * NO device I/O.  (Structure VERIFIED; per-field packing details INFERRED.)
 * ========================================================================= */
int GNAMI(name_t *name, int *out_slot)                          /* 047536B */
{
    enter_setup();                                 /* 047542B                */
    word buf = name_buffer_base;                    /* 047543B  local22       */
    int  cursor = 0;                                /* 047545B  local21       */

    for (int i = 0; i < 7; i++) {   /* 047550..047567B  pack chars           */
        word ch = GNAMT(cursor + i, ...);   /* 047560B  fetch name word       */
        packed[i] = ch;                     /* 047563B                        */
    }

    /* probe the name table for the packed name */
    switch (probe_name_slot(buf, cursor)) {         /* 047574B                */
      case FREE:                                     /* 047575 -> 047600B      */
        cursor++; buf += 16;                         /* advance to next slot   */
        if (buf == table_end) goto retry;            /* 047606B table full     */
        *out_slot = resolved; return OK;             /* 047621B                */
      case MATCH:                                     /* 047576 -> 047627B      */
        cursor++; goto free_path;                     /* 047627B                */
      case GROW:                                      /* 047577 -> 047633B      */
        *out_slot = cursor; return OK;                /* 047633B                */
    }
retry: ;                                              /* 047547B loop back      */
}


/* =========================================================================
 * PART C - GDIRE : GET DIRECTORY ENTRY BY NAME  (THE NAME-MATCHER)
 * Resolves an object NAME to a directory-entry index by walking a hash chain
 * over the NAME table (GNAMT) and DIRECTORY table (GDIRT).  This is the core
 * name->entry resolver.  NO device I/O.
 *   returns entry index in param1, status/type in param2;
 *   status 33B  == "entry not found" (chain exhausted).
 * ========================================================================= */
int GDIRE(word target_name, int *out_index, int *out_status)   /* 131732B */
{
    enter_setup();                                  /* 131736B                */
    word chain_head = GNAMT(head_slot, ...);        /* 131740B read name head  */
    int  cursor = 0;                                /* 131743B                */
    word hash   = seed;                             /* 131744B  local7        */

    while (hash != CHAIN_END) {          /* 131746..131751B  chain not empty  */

        dir_entry e;
        e.w0 = GDIRT(cursor, ...);       /* 131754B  read entry word0         */
        if (e.w0 == 0) goto advance;     /* 131756B  empty slot -> advance    */
        e.w1 = GDIRT(cursor, ...);       /* 131761B  read entry word1         */

        word key = target_name;          /* 131763B                          */
        if (key & BIT6) key &= ~BIT6;    /* 131764..131766B strip flag bit    */

        /* 131767..131773B: fold e's hash bits with param1                     */
        if (key == fold(e, param1)) {    /* 131774B  primary compare          */
            /* 131776..132002B secondary masked compare                        */
            if (masked_eq(e, param2)) {  /* 132002B                           */
                /* --- FULL MATCH : resolve + classify the entry --- */
                dir_entry m = GDIRT(cursor, ...);      /* 132017B             */
                *out_status = classify_type(m);        /* 132021..132034B      */
                *out_index  = cursor;                  /* 132036B  param1      */
                return OK;                             /* 132040B              */
            }
        }
advance:                                  /* 132004B                          */
        cursor++;                          /* 132004B  ++cursor                */
        hash += 30;                        /* 132006B  next bucket stride 30B  */
    }

    *out_status = 33;                      /* 132011B  status 33B not found    */
    return NOT_FOUND;                      /* 132013B                          */
}


/* =========================================================================
 * PART B - GNEXM : scan directory table for next matching entry
 * Linear scan of the directory table (entry stride 55B) via GDIRT, comparing
 * an unsigned key until a match is found; status 43B on "not found".
 * (Not on COLDE's primary path but part of the same accessor family.)
 * ========================================================================= */
int GNEXM(int start, word key, int *out_index)                  /* 050025B */
{
    enter_setup();                                  /* 050031B                */
    for (int i = start; ; i++) {                     /* 050033..050074B        */
        dir_entry *e = base + (i+1)*55;              /* 050036B                */
        if (unsigned_eq(e->key, key)) { *out_index = i; return OK; }  /*050042*/
    }
    return 43; /* 050103B not found */
}


/* =========================================================================
 * HOW A DIRECTORY NAME IS PARSED AND MATCHED (the whole walk)
 *
 *  ENDIR (140176B)                         [../COLDE-CONNECT/, ../ENTER-DIRECTORY/]
 *    -> CLPAR  044777B   parse/classify the spec's leading letter(s)  [GTTCH]
 *    -> COLDE  132072B   cold-enter: drive the name walk (NO device I/O)
 *         -> CLPAR                classify parameter
 *         -> GDIRT / GNAMT        read directory- & name-table entries
 *         -> GNAMI  047536B       pack the ASCII name + probe name table
 *         -> GNAMA  030235B       name-entry address arithmetic
 *         -> helper_004735        (resident; OPEN)
 *    -> GDIRE  131732B   resolve NAME -> directory-entry index (hash-chain walk)
 *         -> GNAMT  050223B       walk the NAME-table hash chain
 *         -> GDIRT  050124B       read candidate DIRECTORY entries + confirm
 *    (only AFTER the name is resolved does ENDIR reach CHDSI->RXDIR->RCBLO,
 *     the single block-0 device read - carved in ../COLDE-CONNECT/.)
 *
 *  So: CLPAR tokenises the spec, GNAMI turns the ASCII object name into the
 *  packed name-table form, and GDIRE hash-walks the name table (GNAMT) and
 *  cross-checks the directory table (GDIRT) until the matching entry index is
 *  found (or status 33B = not found).  GDIRA/GNAMA are the shared leaf address
 *  calculators; the GDIRT/GNAMT (+ default/put siblings) are the shared 2-word
 *  entry accessors underneath everything.
 * ========================================================================= */
