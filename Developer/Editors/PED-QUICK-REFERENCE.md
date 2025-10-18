# PED Quick Reference Card

## Introduction
PED (Program EDitor) is Norsk Data’s advanced full-screen and line-based text editor, designed for program and data file editing under SINTRAN III. It provides cursor-controlled editing with support for multiple windows, regions, and comprehensive text manipulation commands.

PED supports two modes:
- **Page Mode:** Full-screen editing with cursor control (recommended for video terminals such as NOTIS and FACIT 4420).
- **Line Mode:** Command-line editing (QED-compatible) for teletype terminals.

To start PED:
```
@PED [filename]
```

To exit PED safely, always save your work using `W` or `U` before typing `E` or `EXIT`.

---

## File Handling Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `R` | **Read file** | `R SOAP-MAIN` | Reads an existing file into PED’s work area. |
| `R+` | **Read full file (beyond EOF)** | `R+ SOAP-MAIN` | Reads file including all physical pages, useful for damaged files. |
| `U` | **Update file (exclusive access)** | `U SOURCE` | Opens file for editing, locking it from others. |
| `A` | **Append to file** | `A SOURCE` | Appends the buffer to an existing file. |
| `W` | **Write file** | `W "NEWFILE"` | Writes work area to file. Creates file if quoted. |
| `W AREA:` | **Write region or range** | `W SOURCE AREA:10:100` | Writes selected lines only. |
| `E` / `EXIT` | **Exit PED** | `E` | Leaves PED, prompting to save unsaved work. |
| `@` | **Execute SINTRAN command** | `@DIR` | Executes SINTRAN command without leaving PED. |
| `PED @` | **Recover buffer** | `PED @` | Recovers unsaved buffer from scratch file. |

---

## Region Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| Region syntax | `region.firstline.firstcol:secondline.secondcol` | `MAIN.10.1:20.80` | Defines an editable text area. |
| `X` | **Display all regions** | `X` | Lists currently defined regions. |
| `M` | **Move to region** | `M "SUBR"` | Switches to region SUBR. |
| `ALL` | **Special area** | `ALL` | Refers to the entire current region. |

---

## Editing Commands (HOME Commands)

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `D` | **Delete area** | `D MAIN.10:20` | Deletes lines 10–20. |
| `C` | **Copy area** | `C FIELD.1:20` | Copies specified text to cursor position. |
| `M` | **Move area** | `M SOAP.1:50` | Moves area to cursor position. |
| `I` | **Insert area** | `I` | Opens insert mode at cursor. |
| `S` | **Substitute** | `S OLD/NEW/` | Replaces text within region. |
| `#` | **Enter Line Mode** | `#` | Switch to line-based editing (QED-compatible). |

---

## Parameter and Mode Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `T` | **Set tab stops** | `T F` | Sets FORTRAN-style tabs. |
| `B` | **Set borders** | `B 5.72` | Sets left border 5, right border 72. |
| `!` | **Change terminal type** | `! 53` | Sets terminal type (e.g., NOTIS = 53). |
| `EXP` | **Expand mode toggle** | `EXP` | Toggles expand mode. |
| `INS` | **Insert mode toggle** | `INS` | Toggles insert mode. |
| `FUNC 4` | **Alternative delete mode** | `FUNC 4` | Enables alternate DEL behavior. |
| `1`, `2` | **Display menus** | `1` | Displays PED menu 1 or 2. |

---

## Navigation Keys

| Key / Function | Description |
|----------------|-------------|
| ↑ / ↓ | Move cursor one line up/down |
| ← / → | Move cursor one column left/right |
| HOME | Move to command line (PED:) |
| RETURN | Move cursor to next line |
| SCROLL-UP / DOWN | Scroll text window |
| SHIFT + SCROLL-UP | Scroll left |
| SHIFT + SCROLL-DOWN | Scroll right |
| FUNC N / 0 | Move between windows or regions |

---

## Marking and Area Operations

| Key / Command | Purpose | Example |
|----------------|----------|----------|
| `MARK` / `FUNC <V>` | Mark top-left/bottom-right corners |  |
| `FIELD` / `FUNC F` | Mark current line |  |
| `DELETE` / `FUNC D` | Delete marked area |  |
| `COPY` / `FUNC C` | Copy marked area |  |
| `MOVE` / `FUNC M` | Move marked area |  |
| `FUNC I` | Replace with marked area |  |
| `FUNC Q` | Re-mark last area |  |
| `FUNC U` | Convert marked area to upper case |  |
| `FUNC L` | Convert marked area to lower case |  |

---

## Utility Commands

| Command | Purpose | Example |
|----------|----------|----------|
| `V` | **Evaluate numeric expression** | `V 12+3*4` | Displays computed value. |
| `<V>` | **Show cursor position** | `<V>` | Displays line/column number. |
| `FUNC a` | **Refresh screen** | `FUNC a` | Re-draws screen display. |
| `FUNC ?` | **Help** | `FUNC ?` | Displays context-sensitive help. |
| `FUNC LF` | **Set LF/CRLF** | `FUNC LF` | Toggles line endings. |

---

## Quick Tips

- Always **save** your work with `W` or `U` before `E`.
- **HOME** position (`PED:`) is where all commands are typed.
- **CTRL+L** or **RETURN** confirms parameter input.
- **FUNC keys** perform editing and navigation tasks.
- **EXP** LED indicates expand mode; **INS** LED indicates insert mode.
- PED supports up to **40 regions** and **255 columns per line**.

---

## Example Session
```
@PED SOAP-MAIN
PED: R SOAP-IO "IO"        ; Read file SOAP-IO into region IO
PED: W "SOAP-MAIN"         ; Write all changes
PED: E                     ; Exit PED safely
```

---

## Common Function Key Summary

| Key | Action |
|------|--------|
| F1 | Help |
| F2 | Menu |
| F3 | Select |
| F4 | Execute |
| F5 | Split line |
| SHIFT+F5 | Join line |
| F6 | Lowercase area |
| SHIFT+F6 | Uppercase area |
| FUNC ( | Set left border |
| FUNC ) | Set right border |
| FUNC TAB | Set tab stop |
| FUNC BTAB | Set secondary tab stop |

---

### Reference
Based on *ND-60.121.04 PED User’s Guide (June 1983)* by Norsk Data A.S.

