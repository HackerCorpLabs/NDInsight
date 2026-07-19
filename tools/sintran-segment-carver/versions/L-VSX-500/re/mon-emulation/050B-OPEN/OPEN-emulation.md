# MON 50B OpenFile (OPEN) - emulation notes

Behaviour-focused model of the SINTRAN III L-VSX-500 OPEN handler for an emulator
author. Grounded in the byte-verified disassembly `050B-OPEN.ASM` (OPENF entry
`123525B` in `006-S3FS.bin`) plus the official parameter/error tables. Anything
not byte-traced is marked UNVERIFIED.

## Registers / parameters

MON 50B is called with a 4-parameter list (SINTRAN passes them via the standard
monitor-call parameter block; the ND-100 MAC form loads A/T/X directly):

```
  param 1 : file number        INT  (in/out; the caller's file-number cell)
  param 2 : access code        INT  (in)   e.g. R, W, RW, RX, WX, D, ...
  param 3 : file name          STR  (in)   pointer to a packed 2-char/word name
  param 4 : default file type  STR  (in)   e.g. "SYMB", "DATA", "BRF"
```

Return: on success A = file number (>= 0), skip/normal return. On failure A = a
file-system error code (see table) and the error return is taken; the program
normally follows MON 50 with MON 65 (ErrorMessage) which aborts on a non-zero code.

## Control flow (verified against 050B-OPEN.ASM)

```
OPENF:
    enter monitor frame                      ; 123531B call 003752B (resident)
    CLPAR(file name)                         ; 123536B call 044777B
        on parse error -> return err         ; 123537B -> 123612B
    CLPAR(default type)                      ; 123541B call 044777B
        on parse error -> return err         ; 123542B -> 123612B
    ; --- validate access code ---
    for entry in ACCESS_TABLE[0..8]:         ; 123545B..123554B  (9 entries, SAT 11)
        if access_code == entry: goto ok
    A := 104B ; return err                    ; 123555B "No such access code"
ok: ...                                        ; 123557B call 010500B (resident)
    A := FOPEN(name, access, type)            ; 123565B call 067432B
        on FOPEN error -> return err          ; 123566B -> 123614B
    store file number into param 1            ; 123574B..123606B
    return A = file number                    ; 123611B -> 003776B (resident exit)
```

`CLPAR` (`044777B`) parses a string parameter, matching two-letter mnemonics
(the disassembly checks ASCII 'R' 'W' 'S' 'A' 'F' 'I' 'D' 'B' 'L' 'O' 'X'); it is
used for the default-type / access spelling. `FOPEN` (`067432B`) performs the
directory lookup by name and allocates an open-file-table slot; its own error
constants seen in code include `SAA 122B` (too many files).

## Q1 - empty / all-zero name

There is **no default-file, init-file, or scratch fallback** in OPEN. Scratch and
direct opens are separate calls (235B ScratchOpen -> OPENS; 220B DirectOpen ->
DOPEN). An empty name terminates at the first NUL, the directory search matches
nothing, and OPEN returns an error (non-zero A). Best-supported code:
**056B "No such file name"** (HIGH confidence it is an error; MEDIUM that the code
is exactly 056B vs a CLPAR parameter error such as 021B "Illegal character in
parameter" / 044B "Too long parameter").

Emulator rule: **OPEN(name="") -> error, never success.** Do not synthesise a
default/scratch file.

## Relevant file-system error codes (octal; from ND-60.050.06 Users Guide)

| Code | Meaning                       | Where |
|------|-------------------------------|-------|
| 000  | Illegal monitor call          | internal-error class |
| 003  | End of File                   | RFILE next read past EOF |
| 021  | Illegal character in parameter| name/type parse |
| 044  | Too long parameter            | name/type parse |
| 056  | No such file name             | directory search (empty/unknown name) |
| 057  | Ambiguous file name           | directory search |
| 074  | No such file version          | directory search |
| 104  | No such access code           | OPENF access-code table (VERIFIED at 123555B) |
| 105  | File already opened           | FOPEN |
| 107  | Attempt to open too many files| FOPEN |
| 121  | Too many mass-storage files   | FOPEN |
| 122  | Attempt to open too many files| FOPEN (SAA 122B seen in code) |

## Pseudocode

```
function OPEN(fileno_cell, access_code, name_str, type_str) -> A:
    name = parse_string(name_str)          # CLPAR; may raise 021/044
    type = parse_string(type_str)          # CLPAR
    if access_code not in ACCESS_TABLE:     # 9 valid codes
        return err(0o104)                   # No such access code

    if name is empty:                       # all-zero / zero-length
        return err(0o056)                   # No such file name  (no fallback)

    ent = directory_lookup(name, type, access_code)
    if ent is None:
        return err(0o056)                   # No such file name
    if no_free_open_slot():
        return err(0o122)                   # Attempt to open too many files
    if already_open_conflict(ent, access_code):
        return err(0o105)                   # File already opened
    slot = allocate_open_file_entry(ent, access_code)
    store fileno_cell = slot.file_number
    return slot.file_number                 # success (A >= 0)
```

## C sketch (emulator side)

```c
/* Return value: >=0 file number on success, negative -errcode on failure.
   The caller mirrors it into A and (for the file-number cell) param 1. */
int sintran_open(uint16_t *fileno_cell, int access_code,
                 const uint8_t *name, size_t name_len,
                 const char *deftype)
{
    /* 1. access code must be one of the 9 valid codes (OPENF table @123545B) */
    if (!access_code_valid(access_code))
        return -0104;                 /* octal 104: No such access code */

    /* 2. empty / all-zero name has NO special meaning in MON 50B.
          Names are NUL/apostrophe terminated; an all-zero buffer is empty.   */
    size_t n = 0;
    while (n < name_len && name[n] != 0 && name[n] != '\'' )
        n++;
    if (n == 0)
        return -0056;                 /* octal 056: No such file name        */
                                      /* (NOT a scratch/default open)         */

    /* 3. directory lookup by (name, deftype) */
    dir_entry_t *e = fs_dir_lookup(name, n, deftype);
    if (e == NULL)
        return -0056;                 /* No such file name                   */

    /* 4. allocate an open-file-table slot (FOPEN @067432B) */
    int slot = oft_alloc(e, access_code);
    if (slot == OFT_FULL)   return -0122; /* Attempt to open too many files  */
    if (slot == OFT_INUSE)  return -0105; /* File already opened             */

    *fileno_cell = (uint16_t)oft_file_number(slot);
    return oft_file_number(slot);
}
```

## Not verified / open items

- Exact error for a zero-length name (056B vs a CLPAR parameter code) - the
  name-analysis / directory-search that emits 056B lives in resident routines
  (`003752B`, and code reached from FOPEN) whose L-binary addresses are offset
  from the L07 resident symbol table, so they were not byte-traced here.
- The 9 concrete access-code values in the OPENF table were not decoded
  (they are compared as data at `123550B`-relative offsets); the emulator can use
  the documented access mnemonics (R, W, RW, RX, WX, WA, D, ...) instead.
