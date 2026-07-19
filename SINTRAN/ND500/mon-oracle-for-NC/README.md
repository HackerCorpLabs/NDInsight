# MON-Call Oracle for the NC-Compiler Emulator

**Full path:** `SINTRAN/ND500/mon-oracle-for-NC/README.md`

Real SINTRAN III VSX/500 **L07** monitor-call behaviour, extracted from carved L07 bytes +
symbols, to break emulator-vs-emulator ties for the NC (Norsk Data cross-target) compiler. Every
claim in the files is tagged **VERIFIED** (from bytes) or **INFERRED**.

## Files
| File | Calls | Headline answer |
|------|-------|-----------------|
| `SINTRAN/ND500/mon-oracle-for-NC/262B-CPUST.md` | 262B CPUST (GetSystemInfo) | **There is no ND-500 value for bytes 2-3.** CPUST reports the ND-100-family front-end CPU (the ND-500 is a coprocessor with no OS). Emulator should return bytes 2-3 from its emulated ND-100/110 host CPU, not a fabricated ND-500 value. No 24-byte template exists in the carve. (Note 2026-07-13: the "uncarved CALLPROC bridge" this doc referred to does not exist — see `SINTRAN/CARVING-HANDOFF.md` section 3a. The CPUST worker is `MCTAB[262B] = CPUST = 063022B` in `006-S3FS`, and it IS carved; the buffer-fill body can now be disassembled.) |
| `SINTRAN/ND500/mon-oracle-for-NC/312B-MOINF_317B-UECOM.md` | 312B MOINF, 317B UECOM | **CORRECTED 2026-07-13** (the earlier entry values were wrong — see the doc's correction notice). **312B** = capability probe: skip-return convention, returns the **`MCTAB[N]`** entry-or-0 (`MCTAB @ 005620B`; carved: **312B -> 032600B**, **317B -> 050701B**). It is NOT the `GOTAB[N]` word — that is `MFELL` for 224 of the 256 calls. Both emulator fakes (0x4C, 0xF8000000+num) are wrong. **317B** = executes a SINTRAN command line as-if-typed, synchronously, prints-on-error, does NOT terminate caller; `define-cat-copy` must actually be performed (persistent FS mutation), not stubbed. |
| `SINTRAN/ND500/mon-oracle-for-NC/tier2-123B-RELES_54B-MDLFI_503B-DVINST.md` | 123B, 54B, 503B | **RELES** releasing a never-reserved device = **silent success** (not err 5). **MDLFI** empty name → **124** (=174B, Illegal param), nonexistent well-formed name → **46** (No such file). **DVINST** break/count = **14** (break char is read/stored/counted, MaxNo inclusive). |
| `SINTRAN/ND500/mon-oracle-for-NC/tier3-422B-GSWSP_256B_41B_50B.md` | 422B, 256B, 41B, 50B | **GSWSP** auto(0) → lowest free logical seg 0..31, size rounded to 2048-byte pages. **256B** full name `dir:user:name:type;version` apostrophe-terminated. **41B ROBJE** 64-byte object entry (#pages @32B). **50B OPEN** file# in A, error in A. |

## Caveats (read before using)
- Version = **L07**. If the target emulator emulates a *different* SINTRAN version, redo naming
  against that version's symbols (`SINTRAN/NPL-SOURCE/SYMBOLS/{K03,L07,M06}/`) — addresses differ.
- **DVINST 503B** (14 vs 12) rests on L07 NPL source + the sibling carved `/mnt/d/ND/t/re/mon-analysis/511B-DVIO`, not a
  dedicated carved DVINST handler — corroborated, not pure-byte-verified.
- **MOINF 312B**: the `MOINF@32600B` symbol is a stale/version-mismatched label; semantics grounded
  in the carved GOTAB table + manual instead.
- Source carves + symbols: `/mnt/d/ND/t/` and `SINTRAN/NPL-SOURCE/`. See
  `SINTRAN/CARVING-HANDOFF.md`.
