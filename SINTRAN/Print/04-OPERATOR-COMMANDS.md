# 04 - Operator Commands (User SYSTEM)

These are the commands used by the operator / user SYSTEM to **run** the
spooler: starting and stopping it, defining how it behaves, managing forms and
the page pool, and controlling a print-out live. Everyday user commands for
submitting jobs are in [05-USER-COMMANDS.md](05-USER-COMMANDS.md).

Sources: `../../Reference-Manuals/ND-60.128.5 EN SINTRAN III Reference
Manual.md`; `../../Reference-Manuals/ND-60.050.06 SINTRAN III Users Guide.md`
section 3.8.1; `../../Reference-Manuals/SINTRAN-COMMANDS-REFERENCE.md`.

---

## 1. Starting and stopping the spooler

### `@START-SPOOLING <peripheral file name>`

Starts the spooling program for the named peripheral. The physical device is
then **reserved by the spooling program** and can no longer be used directly;
the spooler prints every file on that device's spooling queue until stopped
(Users Guide 3.8.1.1).

- If more than one version of the file is a peripheral file, the spooling
  programs for **all** peripheral versions are started; include a version number
  to start one specific device.
- Error if the name is not a peripheral, or if no spooling program was generated
  for it (see [03-CONFIGURATION.md](03-CONFIGURATION.md) section 1).

```
@START-SPOOLING LINE-PRINTER
```

### `@STOP-SPOOLING <peripheral file name>`

Stops the spooling program and **releases** the peripheral (it can then be
accessed directly again). Any file currently printing is **finished first**; the
queue is left intact and files may still be appended. Printing resumes from the
head of the queue when `@START-SPOOLING` is issued again (Users Guide 3.8.1.2).

---

## 2. Defining how the spooler behaves

### `@DEFINE-SPOOLING-CONDITIONS`

```
@DEFINE-SPOOLING-CONDITIONS <peripheral file name>,
    <printing name of spooling files?>,
    <stop and wait for START-PRINT between files?>,
    <no. of lines per page>
```

Sets per-device behaviour (Reference Manual):

- **printing name of spooling files?** - whether the file's name/banner is
  printed.
- **stop between files?** - `YES` = an automatic `@STOP-PRINT` between every
  file (so the operator can, e.g., change paper), used together with
  `@DEFINE-SPOOLING-FILE-MESSAGE`. Can be YES only if the printing-name option
  is YES.
- **no. of lines per page** - the page length the spooler assumes; this also
  governs how `@BACKSPACE-PRINT` / `@FORWARD-SPACE-PRINT` count pages.

Example: `@DEFINE-SPOOLING-CONDITIONS LINE-PRINTER,NO,NO,,` - files are printed
back-to-back without interruption.

### `@DEFINE-SPOOLING-FILE-MESSAGE`

```
@DEFINE-SPOOLING-FILE-MESSAGE <text>,<print message independent of conditions?>
```

Defines a text written on the **error device** whenever a user's file is printed
on the spooling device (Reference Manual). With the second parameter `YES` the
text is printed unconditionally; `NO` (default) prints it only when
`@DEFINE-SPOOLING-CONDITIONS` requested it. Useful for "MOUNT FORM-1"-style
operator prompts.

---

## 3. Forms and headers

### `@SET-SPOOLING-FORM <peripheral file name>,<form identification>`

Sets the form type/parameters in use on the spooling device (e.g. standard,
wide, special stationery).

### `@LIST-SPOOLING-FORM <peripheral file name>`

Lists the current spooling-form information for the device.

### `@INSERT-SPOOLING-HEADER` / `@REMOVE-SPOOLING-HEADER`

Insert or remove a header (banner) that the spooler prints. (Commands Reference
lists both; use them to turn separator/identification headers on or off.)

---

## 4. Managing the spooling page pool

The spooler's disk buffers draw from user SYSTEM's pages, capped by default at
**500 pages** (see [01-OVERVIEW-AND-CONCEPTS.md](01-OVERVIEW-AND-CONCEPTS.md)
section 5).

| Command | Effect |
|---------|--------|
| `@SPOOLING-PAGES-LEFT` | Reports the number of free spooling pages (e.g. `500 SPOOLING PAGES LEFT`). |
| `@GIVE-SPOOLING-PAGES <n>` | Raise the limit by `n` pages. |
| `@TAKE-SPOOLING-PAGES <n>` | Lower the limit by `n` pages. |

Keep at least as many unused SYSTEM pages as there are spooling pages left, or
programs writing spooling files will block until pages free up.

---

## 5. Controlling a print-out live

Once a file is printing, the operator can steer that print-out. All of these
act on the **currently printing** file on the named device.

### `@ABORT-PRINT <peripheral file name>`

Abort the current print-out and let the spooler continue with the **next** file
in the queue. Only effective when the spooler is started and a file is actually
printing (Reference Manual).

### `@STOP-PRINT <peripheral file name>`

Pause the current print-out (the counterpart that `@START-PRINT`,
`@BACKSPACE-PRINT`, `@FORWARD-SPACE-PRINT` build on).

### `@START-PRINT <peripheral file name>`

Resume a print-out that was stopped (e.g. after an operator paper change, or
after a `<printing message?>` wait requested by `@APPEND-SPOOLING-FILE`).

### `@BACKSPACE-PRINT <peripheral file name>,<no. of pages>,<no. of lines>`

After `@STOP-PRINT`, re-print the specified number of pages/lines and then
continue to the end of the file. Valid only while spooling is started and
printing has been stopped. The page size comes from
`@DEFINE-SPOOLING-CONDITIONS`.

Example: back up two pages and resume:
```
@STOP-PRINT LINE-PRINTER
@BACKSPACE-PRINT LINE-PRINTER,2,0
@START-PRINT LINE-PRINTER
```

### `@FORWARD-SPACE-PRINT <peripheral file name>,<no. of pages>,<no. of lines>`

The opposite of backspace: after `@STOP-PRINT`, **skip** the specified pages/
lines and resume printing further into the file.

---

## 6. Quick operator reference

| Command | Purpose |
|---------|---------|
| `@START-SPOOLING` | Reserve device, begin printing the queue. |
| `@STOP-SPOOLING` | Finish current file, release device, keep queue. |
| `@DEFINE-SPOOLING-CONDITIONS` | Lines/page, name printing, stop-between-files. |
| `@DEFINE-SPOOLING-FILE-MESSAGE` | Operator message on the error device per file. |
| `@SET-SPOOLING-FORM` / `@LIST-SPOOLING-FORM` | Manage stationery/forms. |
| `@INSERT-/@REMOVE-SPOOLING-HEADER` | Banner header on/off. |
| `@SPOOLING-PAGES-LEFT` / `@GIVE-` / `@TAKE-SPOOLING-PAGES` | Manage the disk page pool. |
| `@STOP-PRINT` / `@START-PRINT` | Pause / resume current print-out. |
| `@BACKSPACE-PRINT` / `@FORWARD-SPACE-PRINT` | Reprint / skip pages after a stop. |
| `@ABORT-PRINT` | Drop current file, go to next in queue. |
