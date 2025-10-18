# QED Quick Reference Card

## Introduction
QED (Quick EDitor) is a powerful and compact text editor for Norsk Data systems (e.g., under SINTRAN III). It is designed for editing source files such as MAC, NORD PL, FORTRAN, or BASIC. QED works interactively from the terminal and stores text in an in-memory buffer which can be written to or read from disk or tape.

A QED session begins by starting the program (usually via `@QED`) and ends with the `EX` command. Commands are entered at the `*` prompt. Lines can be edited, listed, deleted, or modified using concise one-letter commands.

Typical session:
```
@QED
QED 4.0
*R SOURCE
212 WORDS READ
*L1,5
*E3,L
*W SOURCE
212 WORDS WRITTEN
*EX
@
```

---

## All QED Commands (Summary)

| Function | Syntax | Description |
|-----------|---------|--------------|
| **APPEND** | `*A LINE {,D} {(LINE-1,LINE-2)}` | Add new lines after a specific line or move a range of lines. |
| **INSERT** | `*I LINE {,D} {(LINE-1,LINE-2)}` | Insert lines before a given line or move text. |
| **CHANGE** | `*C LINE-1 {,LINE-2}` | Replace one or more lines with new content. |
| **DELETE** | `*D LINE-1 {,LINE-2}` | Delete one or more lines. |
| **EDIT** | `*E LINE {,L}` | Edit a specific line (optional listing). |
| **LIST** | `*L {LINE-1} {,LINE-2}` or `*L LINE-1 {(LINE-2,LINE-3)}` | List lines or search for text. |
| **NEXT** | `*N number` | List next *n* lines. |
| **PREVIOUS** | `*P number` | List previous *n* lines. |
| **VALUE** | `*V {LINE}` | Show the line number of a specified line. |
| **READ** | `*R FILE {,LINE} {,(LINE-1,LINE-2)}` | Read lines from a file into the buffer. |
| **WRITE** | `*W FILE {,LINE-1,LINE-2} {,A}` | Write buffer (or range) to a file. Optional append. |
| **SUBSTITUTE** | `*S:options/new/old/ {LINE-1,LINE-2}` | Replace occurrences of text strings. |
| **EXIT** | `*EX` | Exit QED and return to SINTRAN. |
| **MODE** | `*M operand(value), ...` | Display or set QED mode parameters. |
| **TABS** | `*T number,number,...` | Display or define tab stops. |
| **EXAMINE** | `*X` | Display the file name from the last READ command. |
| **MOVE CURRENT** | `* ±number` | Move the current line pointer up or down. |
| **COLUMN GUIDE** | `*G` | Display a 72-column ruler. |

---

## File Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `*R FILE` | **Read file** | `*R TEST` | Reads file TEST:SYMB into buffer. |
| `*R FILE,LINE` | **Read after specific line** | `*R DATA,10` | Appends file DATA after line 10. |
| `*R FILE,(LINE1,LINE2)` | **Read specific range** | `*R LARGE,(1000,1200)` | Reads lines 1000–1200 from LARGE. |
| `*W FILE` | **Write buffer to file** | `*W DATA` | Writes full buffer to file DATA. |
| `*W FILE,LINE1,LINE2` | **Write specific range** | `*W DATA,1,200` | Writes lines 1–200 to file DATA. |
| `*W FILE,,,A` | **Append** | `*W LOG,,,A` | Appends buffer to existing file. |
| `*X` | **Show current file name** | `*X` | Displays the active file name. |

### Notes
- Filenames may be quoted to create new files, e.g. `*W"MYNEWFILE",1,100`.
- Always use `*W` before exiting QED to avoid losing edits.

---

## Listing Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `*L` | **List current line** | `*L` | Lists current line. |
| `*L1,20` | **List range** | `*L1,20` | Lists lines 1–20. |
| `*L/TEXT/` | **Search and list** | `*L/TEXT/` | Lists first line containing TEXT. |
| `*N number` | **Next** | `*N 10` | Lists next 10 lines. |
| `*P number` | **Previous** | `*P 5` | Lists 5 lines before current line. |

### Notes
- If the search fails, QED prints `STRING NOT FOUND`.
- The last line listed becomes the current line (`.`).

---

## Editing Commands

| Command | Purpose | Example | Description |
|----------|----------|----------|--------------|
| `*A` | **Append** | `*A` | Add new lines after current line. End with (CTRL)L. |
| `*I` | **Insert** | `*I10` | Insert new lines before line 10. End with (CTRL)L. |
| `*C` | **Change** | `*C3,5` | Replace lines 3–5 with new text. End with (CTRL)L. |
| `*D` | **Delete** | `*D1,10` | Delete lines 1–10. |
| `*E` | **Edit line** | `*E25,L` | Edit line 25 (lists it first). |

### Common Edit Control Keys

| Key | Action |
|------|--------|
| (CTRL)C | Copy one character |
| (CTRL)D | Copy rest of line, end edit |
| (CTRL)E | Insert characters (toggle insert mode) |
| (CTRL)A | Backspace one character |
| (CTRL)S | Skip one character |
| (CTRL)G | Delete next word |
| (CTRL)T | Show alignment of old/new line |
| (CTRL)Y | Copy rest of old line and continue editing |
| (CTRL)L | End append/insert/change |

---

## Substitute Command

| Command | Purpose | Example |
|----------|----------|----------|
| `*S:new/old/` | Replace all occurrences | `*S:WORLD/HELLO/` |
| `*S:L/new/old/` | Replace and list changes | `*S:L/HELLO/HI/` |
| `*S:W/new/old/` | Confirm each change | `*S:W/HELLO/HI/` |

---

## Miscellaneous Commands

| Command | Purpose | Example |
|----------|----------|----------|
| `*T` | Show or set tab stops | `*T` or `*T8,16,24` |
| `*M` | Show or set mode parameters | `*M TO(0)` |
| `*G` | Display column guide | `*G` |
| `*V LINE` | Show line number | `*V/END/` |
| `* number` | Move current line pointer | `* -5` |
| `*EX` | Exit QED | `*EX` |

---

## Quick Tips

- **Start QED:** `@QED`
- **Prompt:** `*` means ready for next command.
- **Terminate input:** (CTRL)L ends APPEND/INSERT/CHANGE input.
- **Search delimiters:** `/` or `;` can be used, but avoid reserved characters like `:` or `*`.
- **Line references:**
  - `.` → current line
  - `$` → last line
  - `+n` or `-n` → relative offsets

---

## Example Session
```
@QED TEST
*L1,10            ; List first ten lines
*A$               ; Append at end of file
NEW LINE ONE
NEW LINE TWO
(CTRL)L
*C/OLD/NEW/       ; Replace OLD with NEW in first match
*W TEST           ; Write changes to TEST
*EX               ; Exit to SINTRAN
@
```

