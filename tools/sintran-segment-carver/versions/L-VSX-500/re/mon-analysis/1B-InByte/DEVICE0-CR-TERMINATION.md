# MON 1B on device 0 - the command line is CR-terminated (read side + emulator)

Companion to [`README.md`](README.md), which carves the MON 1B dispatch and worker. That
README's honest caveat says *"the wait/no-wait and logical-device-0 behaviours come from the
manual."* This note closes the **logical-device-0** half of that gap with byte + runtime
evidence, from two independent directions.

**One-line answer:** a program reading logical **device 0** (the SINTRAN command buffer) via
MON 1B gets the invocation command line **terminated by CR (`015B` / `0x0D`)** - never `47B`,
never raw End-Of-File. A program launched with no arguments still reads a **lone CR**.

All addresses octal unless prefixed `0x`. Full paths are absolute so this is readable cold.

---

## Two sentinels, two layers (do not conflate them)

| Value | Layer | Who sees it |
|---|---|---|
| `47B` (`0x27`, `'`) | command-processor **source** string terminator | the command PROCESSOR, on the write/prepare side. **A device-0 reader never sees it.** |
| `015B` (`0x0D`, CR) | the **device-0 line** terminator | the READER, via MON 1B |
| `-1` / `177777B` | the level-4 device **"no byte available"** sentinel | the INBT worker internally (see [README](README.md)) |

These are three different things. Conflating the first two nearly caused a wrong experiment
(parking the command byte-pointer on `47B` and breaking in the level-4 read loop watches the
wrong sentinel).

---

## WRITE side - VERIFIED (command-processor bytes, L07)

The command processor reads the source line and, at the `47B` source marker, **substitutes
CR** and resets the command byte pointer (`CPNT`):

```
050773: SAA 15        % A := 015B (CR)
        ... SBYT      % store that byte into the command buffer
        ... reset CPNT
```

So `47B` is an internal SOURCE marker; what lands in the command buffer for a MON 1B reader
ends in **CR**. (Trace captured while watching `CPNT = 144033B`; the command buffer machinery
is `CPNT` byte pointer + `CSTRIN = 144035B` string - NOT `CBUF = 170207B`, which is a
boot-time scratch pointer, `INTEGER CBUF % ADDRESS OF CURRENT I/O BUFFER`,
`/mnt/e/Dev/Ronny/NDInsight/SINTRAN/NPL-SOURCE/NPL/PH-P2-START-BASE.NPL:24`.)

## READ side - VERIFIED (runtime, on the ND-500 emulator)

The ND Linker `linker-b01.dom` is a real program that reads device 0 past its line, so it
answers the read side directly - no ND-100 guest required. Under the `nd500x` emulator, with
device 0 delivering CR:

- its first device-0 read (MON 1B INBT @ `PC = 0xB004E759`) returns **`0x0D`**;
- it then prints its banner + startup dialogue and **never reads device 0 again**.

So a device-0 read yields **args + CR**, and a reader consumes that and stops.

## Emulator consequence - VERIFIED (measured)

Returning raw EOF at end-of-line instead of the CR makes that same linker **busy-spin**:
**20,088 consecutive MON 1B INBT** at `PC = 0xB004E759` within 600k instructions, no other
activity. Delivering the CR lets it complete its line and proceed. Fixed in `nd500x` at
`/home/ronny/repos/nd500x/src/libmon/mon_file_table.c` (`mon_read_command_buffer_char`):
return the stored bytes, then exactly one CR if the line did not already end in one, then
end-of-line.

---

## Still UNPROVEN (do not guess)

What a device-0 read returns **past** the CR (a read after end-of-line). The linker never
does it, so there is no runtime evidence, and the deciding code is the level-4 byte-input
worker whose device-0 branch is not isolated (see [README](README.md) - `M1 = 071633B` is a
two-word stub `M1: "INBT"; GO IOB14`; the behaviour lives in the level-4 worker `INBT`, live-
proven as resident code at `032471B` but its device-0 path not yet traced). The `nd500x`
handler currently treats a past-CR read as a process suspend, which is defensible but not
proven.

---

## Cross-references

- Dispatch + worker carve: [`README.md`](README.md)
- Manual contract (device 0 = command buffer, "parameters following the program name", break
  and echo both 1): `/mnt/e/Dev/Ronny/NDInsight/Developer/MON/calls/1B_InByte.yaml`
- MON 12B SETCM ("the command buffer contains the last command input from the terminal"):
  `/mnt/e/Dev/Ronny/NDInsight/Developer/MON/calls/12B_SetCommandBuffer.yaml`
- NPL answers (device-0 / command buffer / RSIO / DESCF):
  `/mnt/e/Dev/Ronny/NDInsight/SINTRAN/ND500/NPL-ANSWERS-DEVICE0-CMDBUF-RSIO-DESCF.md`
- Emulator fix + read-side trace: `/home/ronny/repos/nd500x` commit `19f7354`
