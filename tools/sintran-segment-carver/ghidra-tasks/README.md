# Ghidra RE tasks — SINTRAN III (L07) carved segments

Each `TASK-*.md` here is a **self-contained job** a Ghidra LLM (Windows, with the
Ghidra MCP tools + the `nd100-ghidra` skill) can pick up on its own. Paths are
Windows (`E:` = the WSL `/mnt/e` mount). Addresses are **hex** (Ghidra) with the
octal in parentheses (SINTRAN symbols/docs are octal).

## Shared setup (every task does this first)

The carved `.bin` files are **big-endian, native ND-100 word order — load as-is,
no byte-swapping.** ND-100 is **word-addressed** (the addressable unit is a
16-bit word).

1. **Import** the task's `.bin`:
   `File > Import File…` → **Raw Binary** → Language = **ND-100 big-endian 16-bit**
   → **Base Address** = the task's hex base → import.
2. **Disassemble** from the base (`D`).
3. **Apply labels**: `Window > Script Manager > ImportSymbolsScript` → select the
   task's `*.ghidra-symbols.txt` (format `NAME 0xADDR`, word addresses).
4. **Verify the load** with the task's landmark before doing RE — if the landmark
   instruction is at the stated address, base/endianness/word-addressing are correct.

Common files:
- carved segments: `tools/sintran-segment-carver/versions/L-VSX-500/segments/`
- symbol files:    `tools/sintran-segment-carver/versions/L-VSX-500/re/*.ghidra-symbols.txt`
- source symbols:  `SINTRAN/NPL-SOURCE/SYMBOLS/L07/`
- disk (for cross-checks via `ndtool` in WSL): `~/repos/nd100x/SMD0.IMG`

## Reading ND-100 disassembly
- Word-addressed, big-endian, one instruction per word. `JPL`=direct call,
  `JPL I`=indirect (read the literal at the target to get the routine address).
  Jump tables interleave with code (a run of `JPL I` is usually a dispatch table).
- `MON nnn` = octal 161000+nnn; low 8 bits = monitor-call number.
- Radix: symbols/docs octal, Ghidra hex (e.g. `RUSPW` = octal 053217 = `0x568F`).

## Deliverable convention
For each task, append your findings to the task's own results file under
`versions\L-VSX-500\re\` (e.g. `TASK-02-results.md`), citing addresses in both
hex and octal, and mark VERIFIED vs UNCERTAIN. Do not mention any AI assistant.

## Task index
| Task | Target | Base | Goal |
|------|--------|------|------|
| [TASK-01](TASK-01-password-login.md) | `006-S3FS.bin` + `003-S3CP.bin` | 0x2C00 / 0x3000 | The 16-bit password fold + login check |
| [TASK-02](TASK-02-nd500-system-monitor.md) | `030-S3SM5.bin` | 0x4000 | Map the ND-500 System Monitor + extended MON calls |
| [TASK-03](TASK-03-mon-dispatch-handlers.md) | `116-S3SERWD.bin` | 0x600 | Per-handler RE of the GOTAB MON table |
| [TASK-04](TASK-04-filesystem-s3fs.md) | `006-S3FS.bin` | 0x2C00 | File-system directory/allocation/open-close |
| [TASK-05](TASK-05-undocumented-mon-calls.md) | resident handlers + `030-S3SM5.bin` | live DAP / 0x4000 | Verify undocumented MON calls (ND-100 legacy + ND-500). **Needs live DAP for the ND-100 handlers — see the task's §D** |
