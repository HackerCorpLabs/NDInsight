# 05 - User Commands: Printing as an Everyday User

These are the commands a normal (public) timesharing user uses to get something
printed and to manage their own queue entries. Operator/SYSTEM commands for
running the spooler are in [04-OPERATOR-COMMANDS.md](04-OPERATOR-COMMANDS.md).

Sources: `../../Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md`
section 3.8.2; `../../Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference
Manual.md`; `../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md`.

---

## 1. The simplest way: copy a file to the printer

If spooling is running for the printer, you just copy your file onto the
peripheral file. SINTRAN transparently gives you a **spooling file** (not the
hardware), and queues it when you close it.

```
@COPY-FILE LINE-PRINTER,USER-FILE-ONE
```

From the Users Guide 3.8: "The file USER-FILE-ONE is copied onto a spooling file
version of the file LINE-PRINTER. The spooling file is linked to the spooling
queue when the COPY-FILE command is finished. The file is emptied while the user
continues with other commands."

You do not wait for the printer - the copy is to disk, and the background
spooler prints it later. Because there are several spooling-file versions,
several users (or you, several times) can do this at once.

---

## 2. Queue any file, with copies: `@APPEND-SPOOLING-FILE`

`COPY-FILE` copies your data into a fresh spooling file. If instead you want to
queue an **existing** file directly and choose how many copies, use:

```
@APPEND-SPOOLING-FILE <peripheral file name>,<file name>,<no. of copies>,<text>[,<printing message?>]
```

- `<peripheral file name>` - the printer's peripheral file (e.g. `LINE-PRINTER`).
- `<file name>` - the file to print.
- `<no. of copies>` - how many copies (blank = 1).
- `<text>` - a message associated with the entry.
- `<printing message?>` - `YES` = the spooler waits for a `@START-PRINT` after
  printing `<text>` (e.g. so an operator can mount special paper); this
  overrides `@DEFINE-SPOOLING-CONDITIONS`. `NO` (default) = the text is only
  printed on the error device if the spooling conditions request it.

Examples (Reference Manual / Users Guide):

```
@APPEND-SPOOLING-FILE LINE-PRINTER,FILE-ONE,2
@APPEND-SPOOLING-FILE LINE-PRINTER,FILE-TWO,1
@APPEND-SPOOLING-FILE LINE-PRINTER,F-1,,'MOUNT FORM-1',YES
```

The last one queues one copy of `F-1`, writes "MOUNT FORM-1" on the error device
before printing, and then waits for the operator's `@START-PRINT`.

The monitor calls behind this are `APSPF` (MON 240 octal, append file to
spooling queue) and `SPCLO` (MON 40 octal, close spooling file) - see
[07-INTERNALS-AND-MON-CALLS.md](07-INTERNALS-AND-MON-CALLS.md).

---

## 3. Looking at the queue: `@LIST-SPOOLING-QUEUE`

```
@LIST-SPOOLING-QUEUE <peripheral file name>[,<output file>]
```

Lists the entries waiting on the printer's queue (optionally to a file rather
than the terminal). Note: the file **currently being printed** is not listed,
because its printing has already started (Users Guide 3.8 example).

---

## 4. Editing your queue

### `@MOVE-SPOOLING-QUEUE-ENTRY`

```
@MOVE-SPOOLING-QUEUE-ENTRY <peripheral file>,<file name>,<insert or append>,<before/after file name>
```

Reorders the queue - move an entry to before/after another entry (e.g. to push a
short job ahead).

### `@REMOVE-FROM-SPOOLING-QUEUE`

```
@REMOVE-FROM-SPOOLING-QUEUE <peripheral file>,<file name>
```

Removes a specific entry from the queue without deleting the underlying file.

### `@DELETE-SPOOLING-FILE`

```
@DELETE-SPOOLING-FILE <peripheral file name>,<file name>
```

Removes a file from the queue **and** empties it. If it is a spooling file, its
pages are released back to the free spooling-page pool and the file is marked
unused. If it is not a spooling file, this behaves like
`@REMOVE-FROM-SPOOLING-QUEUE` (Reference Manual).

Example:
```
@DELETE-SPOOLING-FILE LINE-PRINTER,LINE-PRINTER::10
```

---

## 5. The spooling page pool (user view)

Users share the spooling page pool described in
[01-OVERVIEW-AND-CONCEPTS.md](01-OVERVIEW-AND-CONCEPTS.md) section 5. As a user
you can check `@SPOOLING-PAGES-LEFT`. Raising/lowering the limit
(`@GIVE-SPOOLING-PAGES` / `@TAKE-SPOOLING-PAGES`) is an operator/SYSTEM concern
(see doc 04). If the pool is exhausted, your program simply waits until the
spooler frees pages - it will not fail.

---

## 6. Quick user reference

| Command | Purpose |
|---------|---------|
| `@COPY-FILE <printer>,<file>` | Simplest print: copy into a spooling file, auto-queued on close. |
| `@APPEND-SPOOLING-FILE` | Queue an existing file; choose copies; optional operator message/wait. |
| `@LIST-SPOOLING-QUEUE` | See what is waiting (not the file already printing). |
| `@MOVE-SPOOLING-QUEUE-ENTRY` | Reorder the queue. |
| `@REMOVE-FROM-SPOOLING-QUEUE` | Drop an entry, keep the file. |
| `@DELETE-SPOOLING-FILE` | Drop an entry and empty/free the file. |
| `@SPOOLING-PAGES-LEFT` | Check free spooling pages. |

To print to a printer on **another machine**, see
[06-REMOTE-AND-NETWORK-PRINTING.md](06-REMOTE-AND-NETWORK-PRINTING.md).
