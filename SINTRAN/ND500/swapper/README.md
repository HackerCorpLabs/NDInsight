# ND-500/5000 Swapper (SWAPPER-K01) - Binaries and RE Analysis

**The ND-500 swapper binary (SWAPPER-K01), its disassembly, the resident monitor symbol table, and the reverse-engineering analysis.**

---

## Files

| File | Contents |
|------|----------|
| [SWAPPER-K01-ANALYSIS.md](SWAPPER-K01-ANALYSIS.md) | The reverse-engineering analysis (2026-07-08) - start here |
| `SWAPPER-K01.PSEG` | ND-500 program segment (I-space machine code, 38161 bytes) |
| `SWAPPER-K01.DSEG` | ND-500 data segment (D-space, 218117 bytes) |
| `SWAPPER-K01.PSEG.asm` | ND-500 disassembly of the PSEG (12046 lines) |
| `N500-SYMBOLS.SYMB` | Resident ND-500 monitor symbol table (7157 symbols) |

---

## Related

- [../ND500-SWAPPER-LOADING-MECHANISM.md](../ND500-SWAPPER-LOADING-MECHANISM.md) - how SINTRAN loads the swapper (INZ500, MSINIT, 5SWRT)
- [../ND500-SWAPPER-ANALYSIS.md](../ND500-SWAPPER-ANALYSIS.md) - swapper FIFO/queue mechanics from the ND-100 side

---

**Parent:** [../README.md](../README.md)
