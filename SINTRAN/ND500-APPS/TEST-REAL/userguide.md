# TEST-REAL - real-number (floating-point) ASCII parse test

## Overview

TEST-REAL is a small ND Pascal test program that exercises the real-number
(floating-point) ASCII-to-real conversion routine `getreal`. The recovered
source (`analysis/test-real.pasc`) shows the whole program: [from disasm]

```
program test_real (input, output);
$include getreal:pasc
var st : packed array[1..100] of char; r : real;
begin
   while true do begin
      writeln;
      write ('Give real as ASCII string: ');
      readln (st);
      getreal (st, r);
      writeln ('Real value : ', r);
   end;
end.
```

So it is an INTERACTIVE loop, not a batch of self-tests: it repeatedly prompts
`Give real as ASCII string:`, reads a line, converts it with `getreal`, and
prints `Real value : <r>`. [from disasm] The loop is `while true` - it does not
exit on its own; you break out at the SINTRAN level. [from disasm]

Note: the disassembly (`test-real.asm`) shows timing/clock monitor calls
(`MON 11B TIME`, `MON 114B TUSED`, `MON 422B GSWSP`); those come from the Pascal
runtime / `getreal` include, not from the visible program body. [from disasm]

For shared install/run conventions see [../README.md](../README.md).

## Files (in files/)

- `TEST-REAL.DOM` - the runnable ND-500 domain. [verified] Large (~6.3 MB)
  because the ND Pascal runtime is statically linked in. [verified] (file size)
  Self-contained (no PSEG/DSEG/HELP/INIT ships). [verified]

## Requirements

- Just the `.DOM` file to run. [verified]
- Install: copy `files/TEST-REAL.DOM` into the sintran-root. See
  [../README.md](../README.md).

## How to run

Interactive: at the SINTRAN `@` prompt type the bare name (the `@` is the
prompt, do not type it):

```
TEST-REAL
```

then type real numbers as text at each `Give real as ASCII string:` prompt.

Scripted drive from `~/repos/nd500x`, feeding a few test values:

```
printf 'LOGIN GUEST\nTEST-REAL\n3.14159\n1E10\n-0.5\n' | ./build/bin/nd500x \
    --monitor --user GUEST --sintran-root ~/ND500USERS
```

Because the program loops forever, a scripted run will keep asking after the
last value; the emulator stops when stdin reaches end-of-file. [from disasm]
How exactly the loop terminates in nd500x (EOF, escape, or break) is NOT
verified. [UNVERIFIED]

## Commands and options

None. TEST-REAL has no command language and no options - it only reads a real
number per line and echoes the parsed value. [from disasm]

## Verified behaviour in nd500x

- Loads. [verified] (load-sweep 2026-07-31)
- The parse/echo loop and the exact conversion output have NOT been captured in
  nd500x. [verified]

## Known issues / status

- Interactive infinite loop; needs a stdin source and a way to terminate.
  [from disasm]
- The `getreal:pasc` include is not shipped separately - it is linked into the
  DOM. [from disasm]

## References

- Shared conventions: [../README.md](../README.md)
- Recovered Pascal source: [analysis/test-real.pasc](analysis/test-real.pasc)
- NRF (relocatable) object: [analysis/test-real.nrf](analysis/test-real.nrf)
- Disassembly: [analysis/test-real.asm](analysis/test-real.asm)
- Runnable domain: [files/TEST-REAL.DOM](files/TEST-REAL.DOM)
