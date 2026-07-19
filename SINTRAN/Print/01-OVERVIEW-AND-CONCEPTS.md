# 01 - Overview and Concepts

This document explains the ideas behind SINTRAN III printing and spooling. Once
you understand the peripheral-file model and the spooling-file / queue model
here, the configuration, operator, and user commands in the other documents
follow naturally.

Sources: `../../Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md`
section 3.8 "The Spooling System"; `../../Reference-Manuals/ND-60.128.5 EN
SINTRAN III Reference Manual.md`.

---

## 1. What is a "printer" to SINTRAN?

SINTRAN III does not print to hardware directly from user programs. Instead,
every peripheral - including every printer - is represented by a **peripheral
file**: an ordinary file-system name that is tied to a **logical device
number**. When a program opens the peripheral file and writes to it, SINTRAN's
device driver turns those writes into I/O on the corresponding hardware
controller.

Standard peripheral file names for print devices (from the SINTRAN III Users
Guide / Commands Reference, Appendix D "Standard Peripheral File Names"):

| Peripheral file name | Device | Typical hardware device number (octal) |
|----------------------|--------|----------------------------------------|
| `LINE-PRINTER`       | Line printer | 430, 431, ... |
| `PRINTER`            | Matrix printer (and terminals used as printers) | 414, 415, 417 |
| `LINE-PRINTER-1`, `LINE-PRINTER-2`, ... | Versatec printer/plotter (when it is the only line printer) | 603-606 |
| `VERSATEC-1`, `VERSATEC-2`, ... | Versatec printer/plotter (when another line printer exists) | 603-606 |
| `TERMINAL`           | A terminal - can also act as `PRINTER` | 300 and up |

The physical side of these devices is covered in
[02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md).

**Key point:** a printer is just a file name bound to a device number. This is
why the same commands and the same file-system model apply to printers, disks,
tapes, and terminals.

---

## 2. Two ways to print: direct vs spooled

### 2.1 Direct (no spooling)

You can `COPY-FILE` (or have a program write) straight to the peripheral file.
While that copy runs, the physical printer is **reserved** to that user, and
the user program runs at the speed of the printer. If the printer is a 200 line
per minute line printer, the program is stuck for as long as printing takes,
and nobody else can use that printer. For a single user with a fast printer
this is fine; on a multi-user timesharing system it is not.

### 2.2 Spooled (the normal case)

**Spooling** (Simultaneous Peripheral Operation On-Line) decouples the program
from the printer:

1. The user's output is copied quickly to a **disk file** (a *spooling file*).
2. That file is placed on a **spooling queue** for the target printer.
3. A background **spooling program** later reads the queue and empties each
   file onto the physical printer, one at a time.

The user program finishes as soon as the fast disk copy is done and continues
with other work; the slow printing happens in the background. This is the whole
reason spooling exists.

> Users Guide 3.8: "When the file is closed, the file is linked to a spooling
> queue for the peripheral and eventually emptied on the peripheral."

---

## 3. Peripheral files vs spooling files: the "versions" mechanism

This is the mechanism that makes SINTRAN spooling work, and it is unusual, so
it is worth stating precisely.

A peripheral file can be **created in more versions than there are physical
devices**. The rule (Users Guide 3.8):

- A version that **is** connected to a device number is a **peripheral file**
  (it maps to real hardware).
- A version that is **not** connected to a device number is a **spooling
  file** (it is just disk space that looks like the printer).

Example from the Users Guide: `LINE-PRINTER` is created with ten versions.
Version 1 is the real peripheral file (device number 5, a line printer). The
other nine versions are spooling files - disk buffers.

When spooling is running and a user **opens the peripheral** (`LINE-PRINTER`),
SINTRAN does **not** give them the hardware. It gives them **the first free
spooling file** of that printer. The user writes into that disk file. When they
**close** it, SINTRAN links it onto the spooling queue, and the spooling
program eventually empties it to the physical device.

Because there are multiple spooling-file versions, **several users can "open
the printer" at the same time** (or one user can open it several times) - each
gets a different spooling file. This is how many users share one slow printer
without blocking each other.

---

## 4. The spooling queue

The **spooling queue** is the ordered list of files waiting to be printed on a
given peripheral. Entries reach the queue in two ways:

1. Automatically, when a user closes a spooling file they wrote into (see
   above).
2. Explicitly, with `@APPEND-SPOOLING-FILE`, which lets a user queue any file
   (not just a spooling file) and ask for a number of copies.

You inspect the queue with `@LIST-SPOOLING-QUEUE`, reorder it with
`@MOVE-SPOOLING-QUEUE-ENTRY`, and remove entries with
`@REMOVE-FROM-SPOOLING-QUEUE` or `@DELETE-SPOOLING-FILE`. These are covered in
[05-USER-COMMANDS.md](05-USER-COMMANDS.md).

Note: the file currently being printed is **not** shown in the queue - printing
of it has already started (Users Guide 3.8 example).

---

## 5. The spooling page pool

Spooling files consume disk space, and that space belongs to **user SYSTEM**.
To stop spooling from eating all of SYSTEM's disk, SINTRAN caps the number of
pages the spooling files may use. The default limit is **500 pages**.

- `@SPOOLING-PAGES-LEFT` reports how many pages remain.
- `@GIVE-SPOOLING-PAGES <n>` raises the limit by `n`.
- `@TAKE-SPOOLING-PAGES <n>` lowers it by `n`.

If the pool is exhausted (limit reached, or user SYSTEM has no free pages), any
user program currently writing to a spooling file is put into a **wait state**.
The spooling program then prints one of the queued spooling files, returns its
pages to the free pool, and wakes the waiting programs (Users Guide 3.8). So
the system self-throttles rather than failing outright.

(1 page = 1024 words = 1KW = 2KB, per the project memory convention.)

---

## 6. Life of a print job (end to end)

Putting it together, here is what happens when spooling is running and a user
prints a file to `LINE-PRINTER`:

```
   User program / COPY-FILE
        |
        |  opens "LINE-PRINTER"
        v
   +--------------------------+
   | First FREE spooling file |   (a disk file, one of the LINE-PRINTER versions)
   +--------------------------+
        |
        |  user writes data (fast, disk speed) then CLOSES the file
        v
   +--------------------------+
   |     Spooling queue       |   (ordered list for this printer)
   +--------------------------+
        |
        |  background spooling program picks the next entry
        v
   +--------------------------+
   |  Physical printer (dev.  |   printed at printer speed;
   |  no. via the driver)     |   pages returned to the pool afterwards
   +--------------------------+
```

Meanwhile the user program has long since moved on. Multiple users feed the
queue concurrently through different spooling-file versions.

---

## 7. Where to go next

- To understand the actual hardware doing the printing (serial vs parallel vs
  network): [02-HARDWARE-DEVICES.md](02-HARDWARE-DEVICES.md).
- To set up which printers exist:
  [03-CONFIGURATION.md](03-CONFIGURATION.md).
- To run the spooler as an operator:
  [04-OPERATOR-COMMANDS.md](04-OPERATOR-COMMANDS.md).
- To print as an everyday user:
  [05-USER-COMMANDS.md](05-USER-COMMANDS.md).
