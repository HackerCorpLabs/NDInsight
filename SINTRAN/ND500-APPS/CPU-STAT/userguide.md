# CPU-STAT - print CPU type, microcode and system identity

## Overview

CPU-STAT is a small Norsk Data diagnostic that prints the identity of the host
computer: CPU number and type, instruction set, microcode version, system type,
operating system, version/revision, and the generation date. It takes no
arguments and runs to a clean exit. [verified]

The program is written in ND Pascal; the recovered source calls a single system
routine `GetSystemInfo(0, sysrec)` and then formats the returned record.
[from disasm]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `CPU-STAT.DOM` - the runnable ND-500 domain (self-contained). [verified]

No PSEG/DSEG/HELP/INIT files ship - the DOM is all that is needed. [verified]
(The `analysis/` folder additionally holds a recovered Pascal source
`cpu-stat.pasc`, used here to document the exact output fields.) [from disasm]

## Requirements

- Just the `.DOM` file to run - no external libraries or segments. [verified]
- Install: copy `files/CPU-STAT.DOM` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
CPU-STAT
```

Scripted (non-interactive) drive, from `~/repos/nd500x`:

```
printf 'LOGIN GUEST\nCPU-STAT\nEXIT\n' | ./build/bin/nd500x --monitor \
    --user GUEST --sintran-root ~/ND500USERS
```

## Commands and options

None. CPU-STAT takes no arguments and has no interactive prompt: it prints the
report and terminates. [verified]

Output fields printed (label : value, with a parenthetical decode): [from disasm]

- `CPU number` - the system/CPU number (`sysno`).
- `CPU type` - numeric code plus decode, for example
  `4 = ND-110 48-bit floating`. Decodes: 0/1 Nord-10 48/32-bit,
  2/3 ND-100 48/32-bit, 4/5 ND-110 48/32-bit, 6/7 ND-120 48/32-bit,
  8/9 ND-130 48/32-bit (8/9 marked uncertain in the source). [from disasm]
- `Instruction set` - numeric code plus decode (0 Standard ND-100, 1 /CE,
  2 /100 CX, 3 /110 PCX, 4 /120 PCX, 8 /120 CX, 9 /110 CX print 3095,
  10 /110 CX print 3090). [from disasm]
- `Micro prog vers.` - microcode version. [from disasm]
- `System type` - for example `5800`. [verified]
- `Operating system` - numeric code plus decode (0 VS, 1 VSE, 2 VSE-500,
  3 RTP, 4 VSX, 5 VSX-500). [from disasm]
- `Version` - operating-system version character. [from disasm]
- `Revision` - revision number, printed with a trailing `b` (octal). [from disasm]
- `Generated` - generation date: day, month name, year, hour:minute. [from disasm]

## Verified behaviour in nd500x

Verified 2026-07-31 in the `nd500x` C emulator: [verified]

- `CPU-STAT` printed all fields above (for example `CPU type 4 = ND-110 48-bit
  floating`, `System type 5800`, `Operating system 5 = Sintran III VSX-500`).
- The program ran clean to `MON 0B LEAVE` (normal program termination).

## Known issues / status

- No known issues. Runs clean to exit. [verified]

## References

- Shared conventions: [../README.md](../README.md)
- Recovered Pascal source: [analysis/cpu-stat.pasc](analysis/cpu-stat.pasc)
- Disassembly: [analysis/cpu-stat.asm](analysis/cpu-stat.asm)
- Runnable domain: [files/CPU-STAT.DOM](files/CPU-STAT.DOM)
