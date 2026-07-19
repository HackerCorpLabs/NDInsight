/* sintran-passcrack.c
 *
 * Brute-force a SINTRAN III (L / L07) 16-bit password word back to a plaintext.
 * The stored password is a single 16-bit fold of the typed string:
 *
 *     acc = 0
 *     for each char c (a-z uppercased; digits/symbols unchanged):
 *         acc = ( ROL16(acc,3) + c ) & 0xFFFF   ; ROL16 = rotate-left 3 bits
 *     stored_word = acc
 *
 * See PASSWORD-ALGORITHM.md in this folder for the full write-up and the
 * verified derivation from the disassembly of S3CP LOGIN.
 *
 * Build (Linux):  gcc -O2 -o passcrack sintran-passcrack.c
 * Run:            ./passcrack
 *
 * NOTE: the fold is 16-bit, so many strings collide onto the same value. The
 * tool reports the FIRST match in search order, which is not necessarily the
 * original password.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdint.h>
#include <ctype.h>
#include <signal.h>
#include <unistd.h>

/* Base alphabet: A-Z then 0-9. Lowercase is redundant (it uppercases to A-Z). */
static const char BASE[]     = "ABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789";
/* Special characters (printable ASCII punctuation), incl. { [ ] } \ | */
static const char SPECIALS[] = "!\"#$%&'()*+,-./:;<=>?@[\\]^_`{|}~";

#define MAXLEN 31

static inline uint16_t rol3(uint16_t x) { return (uint16_t)((x << 3) | (x >> 13)); }

/* ---- Unicode progress bar (eighth-blocks for smooth fill) ---------------- */
static const char *EIGHTHS[8] = {
    " ", "▏", "▎", "▍", "▌", "▋", "▊", "▉"
};
#define FULLBLK "█"     /* U+2588 full block   */
#define VBAR    "│"     /* U+2502 box vertical */

static void show_cursor(void) { fputs("\033[?25h", stdout); fflush(stdout); }
static void on_sigint(int s)  { (void)s; show_cursor(); fputc('\n', stdout); _exit(130); }

static void draw_bar(double frac, unsigned long long done, unsigned long long total)
{
    const int CELLS = 40;
    int i, full, rem;
    double filled;
    if (frac < 0) frac = 0;
    if (frac > 1) frac = 1;
    filled = frac * CELLS;
    full   = (int)filled;
    rem    = (int)((filled - full) * 8.0);   /* 0..7 eighths of a cell */
    if (full > CELLS) { full = CELLS; rem = 0; }

    fputs("\r\033[2K" VBAR, stdout);          /* CR, clear line, left border  */
    for (i = 0; i < CELLS; i++) {
        if      (i <  full) fputs(FULLBLK,    stdout);
        else if (i == full) fputs(EIGHTHS[rem], stdout);
        else                fputc(' ',        stdout);
    }
    printf(VBAR " %6.2f%%  %llu/%llu", frac * 100.0, done, total);
    fflush(stdout);
}

/* ---- read a trimmed line ------------------------------------------------- */
static int read_line(char *buf, size_t n)
{
    size_t len;
    if (!fgets(buf, (int)n, stdin)) return 0;
    len = strlen(buf);
    while (len && isspace((unsigned char)buf[len - 1])) buf[--len] = 0;
    return 1;
}

int main(void)
{
    char line[256], *end;
    char charset[128];
    int  nchars, maxlen, base, i, L, pos, found_len = -1;
    unsigned long tv;
    uint16_t target;
    unsigned long long total, pw, done = 0;
    int idx[MAXLEN];
    uint16_t pref[MAXLEN + 1];

    signal(SIGINT, on_sigint);

    /* --- target value ----------------------------------------------------- */
    printf("Target password value (decimal, or octal with trailing b/B): ");
    fflush(stdout);
    if (!read_line(line, sizeof line) || line[0] == 0) {
        fprintf(stderr, "no value given\n");
        return 1;
    }
    base = 10;
    {
        size_t len = strlen(line);
        if (len && (line[len - 1] == 'b' || line[len - 1] == 'B')) {
            base = 8;
            line[--len] = 0;
            while (len && isspace((unsigned char)line[len - 1])) line[--len] = 0;
        }
    }
    tv = strtoul(line, &end, base);
    if (end == line) { fprintf(stderr, "'%s' is not a %s number\n",
                               line, base == 8 ? "octal" : "decimal"); return 1; }
    if (tv > 0xFFFFUL)
        fprintf(stderr, "warning: fold is 16-bit; masking %lu to %u\n",
                tv, (unsigned)(tv & 0xFFFF));
    target = (uint16_t)tv;

    /* --- max length (default 10) ----------------------------------------- */
    printf("Max password length [10]: ");
    fflush(stdout);
    maxlen = 10;
    if (read_line(line, sizeof line) && line[0] != 0) {
        maxlen = atoi(line);
        if (maxlen < 1) maxlen = 1;
    }
    if (maxlen > MAXLEN) {
        fprintf(stderr, "capping length to %d\n", MAXLEN);
        maxlen = MAXLEN;
    }

    /* --- include special characters? (default no) ------------------------ */
    printf("Include special characters (e.g. { [ ] } \\ | ) ? [y/N]: ");
    fflush(stdout);
    strcpy(charset, BASE);
    if (read_line(line, sizeof line) && (line[0] == 'y' || line[0] == 'Y'))
        strcat(charset, SPECIALS);
    nchars = (int)strlen(charset);

    printf("Searching alphabet of %d chars, length 1..%d, for value %u (0%ob decimal shown)\n",
           nchars, maxlen, (unsigned)target, (unsigned)target);

    /* total combinations, for the progress bar */
    total = 0; pw = 1;
    for (L = 1; L <= maxlen; L++) { pw *= (unsigned)nchars; total += pw; }

    fputs("\033[?25l", stdout);   /* hide cursor while searching */

    for (L = 1; L <= maxlen && found_len < 0; L++) {
        for (i = 0; i < L; i++) idx[i] = 0;
        pref[0] = 0;
        for (i = 0; i < L; i++)
            pref[i + 1] = (uint16_t)(rol3(pref[i]) + (unsigned char)charset[idx[i]]);

        for (;;) {
            if (pref[L] == target) { found_len = L; break; }
            if (((++done) & 0xFFFFF) == 0)
                draw_bar((double)done / (double)total, done, total);

            /* odometer: increment from the right, recompute the fold suffix */
            pos = L - 1;
            while (pos >= 0) {
                if (++idx[pos] < nchars) {
                    for (i = pos; i < L; i++)
                        pref[i + 1] = (uint16_t)(rol3(pref[i]) + (unsigned char)charset[idx[i]]);
                    break;
                }
                idx[pos] = 0;
                pos--;
            }
            if (pos < 0) break;   /* this length exhausted */
        }
    }

    draw_bar(found_len >= 0 ? (double)done / (double)total : 1.0, done, total);
    fputc('\n', stdout);
    show_cursor();

    if (found_len >= 0) {
        char pwd[MAXLEN + 1];
        for (i = 0; i < found_len; i++) pwd[i] = charset[idx[i]];
        pwd[found_len] = 0;
        printf("MATCH: \"%s\"  (value %u = 0%ob octal = 0x%04X)\n",
               pwd, (unsigned)target, (unsigned)target, (unsigned)target);
        printf("(first match in search order; the fold is 16-bit so other strings collide)\n");
        return 0;
    }

    printf("No password over that alphabet up to length %d produces value %u.\n",
           maxlen, (unsigned)target);
    return 2;
}
