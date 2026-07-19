# 03 - Configuration: Telling SINTRAN Which Printers Exist

This document answers "how do you configure for printing" and "how do you
choose what printers you have." There are three layers: **system generation**
(is a spooling program built in at all), the **peripheral file / device binding**
(what the printer is called and which device number it uses), and the
**spooling-file versions** (how much concurrency and buffering the printer has).

Sources: `../../Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md`
section 3.8; `../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md`
(`SET-PERIPHERAL-FILE`, `SET-SPOOLING-DEVICE-NUMBER`, `SINTRAN-SERVICE-PROGRAM`).

---

## 1. Layer 1: system generation - is spooling available?

Output spooling for a given peripheral only works "if the actual SINTRAN III
system is generated with an optional spooling program for the peripheral in
question" (Users Guide 3.8).

That is: whether a printer *can* be spooled at all is decided when the SINTRAN
system is built (generated). If no spooling program was generated for that
device class, `@START-SPOOLING` will simply report that no spooling program
exists for the peripheral (Reference Manual, `START-SPOOLING` rules). This is a
build-time choice, not something you toggle at runtime.

Generation also fixes, via SINTRAN Configuration (SINGEN) menus (System
Supervisor manual):

- **Number of printers with spooling** - how many spooling programs (SPRT1,
  SPRT2, ...) exist.
- **Spooling queue size in pages** (a "VARIOUS" parameter; e.g. 4 pages).
- **Define printer type** - for each line printer, the interface type:
  `0` = do not use, `1` = DMA (Fujitsu), `2` = Parallel (CDC/DP), `3` = Serial.
  This is only for line printers with a *special* interface; printers on a
  terminal line do not use it (see
  [09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md)).
  The byte-level effect of this choice is documented in
  `../Devices/LINE-PRINTER-CONFIG-INSPECTION.md`.

**Takeaway:** "what printers you have" starts at system generation, which
determines which spooling programs exist and each line printer's interface type.

---

## 2. Layer 2: the peripheral file and its device number

A printer is a **peripheral file** bound to a **logical device number**
(see [01-OVERVIEW-AND-CONCEPTS.md](01-OVERVIEW-AND-CONCEPTS.md) section 1).

### 2.1 `@SET-PERIPHERAL-FILE` - bind a name to a device

```
@SET-PERIPHERAL-FILE <file name>,<device name>
```

- Permitted only for **user SYSTEM**.
- Associates a file-system name with a peripheral device (logical device
  number). The device must be properly configured and the file must exist /
  be accessible.

Example (Commands Reference):

```
@SET-PERIPHERAL-FILE "LINE-PRINTER",5
```

creates the file `LINE-PRINTER` and associates it with logical device number 5.
After this, opening and writing `LINE-PRINTER` reaches that device.

The standard names to use (`LINE-PRINTER`, `PRINTER`, `VERSATEC-n`, ...) and the
hardware device numbers behind them are in
[02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md).

### 2.2 The quote-the-whole-spec rule

When you name a new file in these commands, the quotes must wrap the **entire**
file specification (including any `(USER)` prefix), for example
`"(SYSTEM)LINE-PRINTER"`. This is a general SINTRAN new-file rule, not specific
to printing, but it bites here because you are creating peripheral-file objects.

---

## 3. Layer 3: spooling-file versions - concurrency and buffering

Once the real peripheral file exists, you decide how much spooling capacity the
printer has by **creating extra versions** of the same file. Recall the rule
(Users Guide 3.8):

- The version connected to a device number = the **peripheral file** (hardware).
- Every version **not** connected to a device number = a **spooling file**
  (disk buffer).

The Users Guide example creates **ten versions** of `LINE-PRINTER`: version 1 is
the device (device number 5), the other nine are spooling files. More spooling
files means:

- more users can "open the printer" simultaneously (each gets a free spooling
  file version), and
- more jobs can be buffered on disk ahead of the printer.

**ASSUMPTION:** the extra versions are created with the normal file-creation
mechanism (`@CREATE-FILE` with additional versions) as for any versioned
SINTRAN file. The Users Guide states the versions exist and their meaning; it
does not, in the excerpt reviewed, spell out the exact create command, so the
create step is flagged as interpretation.

---

## 4. Binding a spooling index to a device (service program)

There is a lower-level configuration command that connects an internal
**spooling index** to a **logical device number**:

```
*SET-SPOOLING-DEVICE-NUMBER <spooling index>,<logical device number> (<memory?>) (<image?>) (<save-area?>)
```

Notes (System Supervisor manual; the Commands Reference shows a shorter
memory/save-area form of the same command):

- The leading **`*`** means this is entered inside the **SINTRAN Service
  Program**, not at the normal `@` command prompt. You enter that program with
  `@SINTRAN-SERVICE-PROGRAM` (and leave it when done).
- `<spooling index>` is the octal index of the spooling device in the system;
  its range is fixed at **system generation** time. **Special-interface** line
  printers (the DMA/parallel/serial types) must take the **lowest** indexes,
  sorted ascending; printers on a terminal line follow after in any order
  (System Supervisor note).
- `<logical device number>` is the octal logical device number of the printer
  to use for spooling.
- `<memory?>`, `<image?>`, `<save-area?>` (Y/N) choose whether the change is
  written to running memory, the disk image, and/or the save-area, so the
  binding can be made permanent across restarts. Answer all three `Y` to make it
  stick.

This is the command that ties the generated spooling machinery (identified by
index) to a concrete printer device number. Using it on a terminal-line printer
also spares you the separate `*REMOVE-FROM-BACKGROUND-TABLE` step (see
[09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md](09-PRINTING-VIA-TERMINAL-AND-RETROTERM.md)).

**PARTIALLY VERIFIED:** the System Supervisor manual confirms the spooling index
binds a spooler to a logical device and that special-interface printers take the
lowest indexes. The precise internal data structure the index addresses (and how
it relates to the peripheral-file "versions" in section 3) is being resolved by
the carving pass - see [07a-CARVED-INTERNALS-FINDINGS.md](07a-CARVED-INTERNALS-FINDINGS.md).

---

## 5. The `@` prompt vs the `*` prompt

You will see two prompts in printing/spooling work:

- `@...` - normal SINTRAN III command mode (day-to-day operator and user
  commands: `START-SPOOLING`, `APPEND-SPOOLING-FILE`, etc.).
- `*...` - the **SINTRAN Service Program**, a maintenance/configuration monitor
  entered with `@SINTRAN-SERVICE-PROGRAM`. Configuration commands like
  `SET-SPOOLING-DEVICE-NUMBER` and `INSERT-IN-IOX-TABLE` live here.

Getting the prompt wrong is a common cause of "command not found."

---

## 6. Configuration checklist

To make a working spooled printer, in order:

1. Ensure SINTRAN was **generated** with a spooling program for that device
   class (build-time; section 1).
2. As user SYSTEM, create/bind the **peripheral file** to the printer's device
   number with `@SET-PERIPHERAL-FILE` (section 2).
3. Create the desired number of **extra versions** so there are spooling files
   for buffering/concurrency (section 3).
4. If needed, bind the **spooling index** to the logical device number with
   `*SET-SPOOLING-DEVICE-NUMBER` in the Service Program, saving to memory and/or
   save area (section 4).
5. Start the spooler with `@START-SPOOLING <peripheral file name>`
   (see [04-OPERATOR-COMMANDS.md](04-OPERATOR-COMMANDS.md)).

Now users can print to it (see
[05-USER-COMMANDS.md](05-USER-COMMANDS.md)).
