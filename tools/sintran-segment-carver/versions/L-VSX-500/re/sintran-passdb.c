/* sintran-passdb.c
 *
 * Precomputed reverse lookup for the SINTRAN III (L / L07) 16-bit password fold.
 *
 *     acc = 0
 *     for each char c (a-z uppercased; digits/symbols unchanged):
 *         acc = ( ROL16(acc,3) + c ) & 0xFFFF
 *     stored_word = acc
 *
 * The fold output is only 16 bits, so there are exactly 65536 possible values.
 * A wordlist (e.g. rockyou.txt, ~14M entries) is folded into a direct-indexed
 * table of 65536 slots: slot[value] = one wordlist password producing that value
 * (first one seen that fits, i.e. the most common when the list is frequency
 * ordered). Lookup is O(1): index the table by the value. Since login only
 * compares the 16-bit fold, ANY password with the matching value logs in.
 *
 * Build (Linux):  gcc -O2 -o passdb sintran-passdb.c
 *
 * Usage:
 *   passdb build  <wordlist.txt> <db-file>     # build the table
 *   passdb lookup <db-file> <value>            # value: decimal, or octal if it
 *                                              #        ends in b/B
 *     -> prints the password and exit(1) if found
 *     -> prints "password not found" and exit(0) if not
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <fcntl.h>
#include <unistd.h>
#include <sys/mman.h>
#include <sys/stat.h>

#define NSLOTS   65536
#define MAXPW    63                       /* max stored password length        */
#define SLOTSZ   (1 + MAXPW)              /* 64 bytes: [len][pw...]             */
#define HDRSZ    32
#define DATA_OFF HDRSZ

static const char MAGIC[8] = { 'S','3','P','W','D','B','0','1' };

static inline uint16_t rol3(uint16_t x) { return (uint16_t)((x << 3) | (x >> 13)); }

static uint16_t fold(const char *s, size_t len)
{
    uint16_t a = 0;
    size_t i;
    for (i = 0; i < len; i++) {
        unsigned char c = (unsigned char)s[i];
        /* toupper letters only - VERIFIED by test vectors (ford == FORD). The char
         * is added UNMASKED, exactly as the disassembled fold loop does (RADD SA DT
         * adds the full A register). An online spec claims a 7-bit mask; our
         * disassembly does NOT show one, so it is intentionally not applied. */
        if (c >= 'a' && c <= 'z') c -= 0x20;
        a = (uint16_t)(rol3(a) + c);
    }
    return a;
}

/* word-likeness tier: 2 = all letters, 1 = some letters, 0 = no letters.
 * Used to prefer readable passwords over numeric/symbol collisions. */
static int wordlike_tier(const char *s, size_t len)
{
    size_t i, letters = 0;
    for (i = 0; i < len; i++) {
        unsigned char c = (unsigned char)s[i];
        if ((c >= 'A' && c <= 'Z') || (c >= 'a' && c <= 'z')) letters++;
    }
    if (letters == len) return 2;
    if (letters > 0)     return 1;
    return 0;
}

/* ---- header (fixed 32 bytes; no padding) --------------------------------- */
struct hdr {                  /* exactly HDRSZ (32) bytes: 8 + 6*4 */
    char     magic[8];
    uint32_t nslots;
    uint32_t slotsz;
    uint32_t filled;
    uint32_t version;
    uint32_t reserved0;
    uint32_t reserved1;
};

/* ========================================================================= */
static int do_build(const char *wordlist, const char *dbfile)
{
    int fd = open(wordlist, O_RDONLY);
    struct stat st;
    char *data;
    size_t i, n, filled = 0;
    uint8_t *table;               /* NSLOTS * SLOTSZ, slot[v] = [len][pw]      */
    FILE *out;
    struct hdr h;

    if (fd < 0) { perror(wordlist); return 1; }
    if (fstat(fd, &st) != 0) { perror("fstat"); close(fd); return 1; }
    n = (size_t)st.st_size;

    data = (n ? mmap(NULL, n, PROT_READ, MAP_PRIVATE, fd, 0) : NULL);
    if (n && data == MAP_FAILED) { perror("mmap"); close(fd); return 1; }

    table = (uint8_t *)calloc(NSLOTS, SLOTSZ);   /* all len bytes = 0 (empty)   */
    if (!table) { fprintf(stderr, "out of memory\n"); return 1; }

    fprintf(stderr, "building from %s (%zu bytes)...\n", wordlist, n);

    i = 0;
    while (i < n) {
        size_t start = i, len;
        while (i < n && data[i] != '\n') i++;
        len = i - start;
        if (len && data[start + len - 1] == '\r') len--;   /* strip CRLF */
        i++;                                               /* skip '\n' */

        if (len == 0 || len > MAXPW) continue;
        uint16_t v = fold(data + start, len);
        uint8_t *slot = table + (size_t)v * SLOTSZ;

        /* Keep the "best" password per value: prefer more word-like (more letters),
         * then shortest, then first-seen (most common). */
        if (slot[0] == 0) {
            slot[0] = (uint8_t)len;
            memcpy(slot + 1, data + start, len);
            filled++;
        } else {
            int old_len  = slot[0];
            int new_tier = wordlike_tier(data + start, len);
            int old_tier = wordlike_tier((char *)slot + 1, (size_t)old_len);
            if (new_tier > old_tier ||
                (new_tier == old_tier && (int)len < old_len)) {
                slot[0] = (uint8_t)len;
                memcpy(slot + 1, data + start, len);
            }
        }
    }

    if (n) munmap(data, n);
    close(fd);

    out = fopen(dbfile, "wb");
    if (!out) { perror(dbfile); free(table); return 1; }

    memset(&h, 0, sizeof h);
    memcpy(h.magic, MAGIC, 8);
    h.nslots = NSLOTS; h.slotsz = SLOTSZ; h.filled = (uint32_t)filled; h.version = 1;

    if (fwrite(&h, HDRSZ, 1, out) != 1 ||
        fwrite(table, SLOTSZ, NSLOTS, out) != NSLOTS) {
        perror("write"); fclose(out); free(table); return 1;
    }
    fclose(out);
    free(table);

    fprintf(stderr, "wrote %s: %zu/%d values covered (%.1f%%), %d-byte db\n",
            dbfile, filled, NSLOTS, 100.0 * filled / NSLOTS,
            HDRSZ + SLOTSZ * NSLOTS);
    return 0;
}

/* ========================================================================= */
static int do_lookup(const char *dbfile, const char *valstr)
{
    int fd, base = 10;
    struct hdr h;
    char sval[64], *end;
    unsigned long v;
    uint8_t slot[SLOTSZ];
    size_t sl;

    /* parse value: octal if it ends in b/B, else decimal; mask to 16 bits */
    sl = strlen(valstr);
    if (sl >= sizeof sval) sl = sizeof sval - 1;
    memcpy(sval, valstr, sl); sval[sl] = 0;
    while (sl && isspace((unsigned char)sval[sl - 1])) sval[--sl] = 0;
    if (sl && (sval[sl - 1] == 'b' || sval[sl - 1] == 'B')) { base = 8; sval[--sl] = 0; }
    v = strtoul(sval, &end, base);
    if (end == sval) { fprintf(stderr, "'%s' is not a %s number\n",
                               sval, base == 8 ? "octal" : "decimal"); return 2; }
    v &= 0xFFFF;

    fd = open(dbfile, O_RDONLY);
    if (fd < 0) { perror(dbfile); return 2; }
    if (pread(fd, &h, HDRSZ, 0) != HDRSZ || memcmp(h.magic, MAGIC, 8) != 0 ||
        h.slotsz != SLOTSZ) {
        fprintf(stderr, "%s: not a valid password db\n", dbfile);
        close(fd); return 2;
    }

    if (pread(fd, slot, SLOTSZ, (off_t)DATA_OFF + (off_t)v * SLOTSZ) != SLOTSZ) {
        perror("pread"); close(fd); return 2;
    }
    close(fd);

    if (slot[0] > 0 && slot[0] <= MAXPW) {
        fwrite(slot + 1, 1, slot[0], stdout);
        fputc('\n', stdout);
        return 1;                 /* FOUND -> exit 1 with the password */
    }
    printf("password not found\n");
    return 0;                     /* NOT FOUND -> exit 0 */
}

/* ========================================================================= */
static int usage(const char *me)
{
    printf(
"sintran-passdb - reverse lookup for the SINTRAN III 16-bit password fold\n"
"\n"
"The SINTRAN III (L/L07) password is a single 16-bit word:\n"
"    acc = 0;  for each char (uppercased): acc = (ROL16(acc,3) + char) & 0xFFFF\n"
"(reverse-engineered from the S3CP LOGIN disassembly - see PASSWORD-ALGORITHM.md).\n"
"Because the output is only 16 bits there are just 65536 possible values, so a\n"
"wordlist is folded once into a 65536-slot table (value -> a password producing it),\n"
"and any lookup is a single indexed read. Login only checks the 16-bit word, so ANY\n"
"password with the same value authenticates.\n"
"\n"
"USAGE:\n"
"  %s build  <wordlist.txt> <db-file>\n"
"      Fold every line of the wordlist (e.g. rockyou.txt) into <db-file>.\n"
"      Per value it keeps the most word-like entry (most letters), then the\n"
"      shortest, then the first seen - so you get 'FORD' rather than a numeric\n"
"      collision like '032291' when a readable one exists. Any stored password is\n"
"      still just a COLLISION (it folds to the value and logs in), not necessarily\n"
"      the original. DB is ~4 MB regardless of wordlist size.\n"
"\n"
"  %s lookup <db-file> <value>\n"
"      <value> is DECIMAL, or OCTAL if it ends in 'b' or 'B' (e.g. 41620 or 121224b).\n"
"      It is masked to 16 bits.\n"
"      FOUND     -> prints the password to stdout, exits with code 1\n"
"      NOT FOUND -> prints 'password not found',   exits with code 0\n"
"\n"
"EXAMPLES:\n"
"  %s build rockyou.txt rockyou.db\n"
"  %s lookup rockyou.db 41620      # decimal\n"
"  %s lookup rockyou.db 121224b    # same value in octal\n",
        me, me, me, me, me);
    return 2;
}

int main(int argc, char **argv)
{
    if (argc >= 2 && strcmp(argv[1], "build") == 0 && argc == 4)
        return do_build(argv[2], argv[3]);
    if (argc >= 2 && strcmp(argv[1], "lookup") == 0 && argc == 4)
        return do_lookup(argv[2], argv[3]);
    return usage(argv[0]);
}
