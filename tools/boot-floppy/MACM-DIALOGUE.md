# MACM — the interactive operator dialogue during SINTRAN system generation

**Binary analysed: `D:\ND\BPUN\MACM-1718L.BPUN`** — the *standalone* copy, which is
the one loaded in Ghidra. It is **not** byte-identical to the copy on the L
distribution floppy (standalone base 0o77120 / 19,273 words vs the floppy's
0o76203 / 19,738 words). Every address in this document is a **word address in
Ghidra's `ram:` space** for the standalone build. Ghidra's hexdump byte offset is
always exactly `2 × word address`, which is how data addresses below were derived.

> An earlier note claimed the standalone build "lacks the MSTYP strings". **That is
> wrong.** [VERIFIED] All of `PLEASE DEFINE THE DISC TYPE (MSTYP) !`,
> `ENTER MSTYP: '`, `REMOVABLE OR FIXED (R/F): '`, `)REDEF`, `)HENT`, `22!` and
> `10,0$` are present in `D:\ND\BPUN\MACM-1718L.BPUN`. Ghidra's *auto-analysis*
> had simply not defined them as strings, so `list_strings` did not report them.
> They were found with `search_bytes` / `get_hexdump` (7-bit ASCII, high bit clear —
> the 8-bit form `cd d3 d4 d9 d0` for "MSTYP" returns **0 matches**).

Marking convention used throughout: **[VERIFIED]** = quoted bytes or disassembly at a
named address. **[INFERRED]** = reasoning shown. **COULD NOT DETERMINE** = stated plainly.

Companion tool: `E:\Dev\Ronny\NDInsight\tools\boot-floppy\tools\nd_sixbit_decode.py`
(decodes the ND 30-bit packed 6-bit symbol names; validated against 12 anchors).

---

## 1. Command dispatch table

**[VERIFIED] MACM's command/symbol table is a flat array of 3-word entries:**

```
    word 0 : packed name, bits 29..16 (top 2 bits are entry flags)
    word 1 : packed name, bits 15..0
    word 2 : value — for a command, the handler's word address
```

Proof by three independent anchors, all of which land on the same `mod 3` lattice
(`0x872A`, `0x8760`, `0x8763` are all ≡ 0x872A mod 3):

| address | words | decoded name | value | pre-existing Ghidra function at that value |
|---|---|---|---|---|
| `ram:872a` | `1214 4146 9257` | `REDEF` | `ram:9257` | (was unnamed) |
| `ram:8760` | `0020 5394 9913` | `HENT`  | `ram:9913` | `cmd_HENT_restore_core_image` ✔ |
| `ram:8763` | `001c a14d 990b` | `GJEM`  | `ram:990b` | `cmd_GJEM_save_core_image` ✔ |
| `ram:8766` | `0001 2090 98ce` | `RBP`   | `ram:98ce` | `cmd_RBP_reset_breakpoints` ✔ |
| `ram:8703` | `00e4 53c6 8917` | `9EOF`  | `ram:8917` | — |

Three of the five values are functions a **previous** analyst named independently.
That is what makes the table format certain rather than plausible.

**[VERIFIED] `)REDEF` is a real command-table entry**, name `REDEF`
(`0x12144146`) at `ram:872a`, handler `ram:9257`. Disassembly at `ram:925c`:
`LDA *0x92ac ; COPY SA,DB ; JPL 0x92ae ; JPL 0x92af ; SAT 1 ; SAA 0 ; …`

**[VERIFIED] `)HENT` is a real command-table entry**, name `HENT` (`0x00205394`)
at `ram:8760`, handler `ram:9913` = the already-identified
`cmd_HENT_restore_core_image`.

**COULD NOT DETERMINE — the full table extents.** The lattice continues in both
directions well past `ram:8600` (it is MAC's whole permanent symbol table:
opcodes, pseudo-ops and commands share it), and I did not dump all of it. The
window `ram:86e0`–`ram:87b6` was dumped and is available in the Ghidra program.

**COULD NOT DETERMINE — the 11 three-word slots at `ram:872d`–`ram:874d`.**
These sit immediately after `REDEF` on the same lattice and their *value* words
(`872f, 8732, 8735, 8738, 873b, 873e, 8741, 8744, 8747, 874a, 874d`) are the
destinations of the `)9BYTT` parameter copy (§6.5). But their name words do
**not** decode to sensible 6-bit symbols:
`(0000,0415) (aa3d,033d) (0000,0030) (0000,0000) (0000,0008) (0000,0011)
(0000,0348) (0000,0d04) (0000,0010) (0000,0d4d) (0000,3909)`.
I am not going to invent an interpretation. Raw words recorded here for whoever
closes it.

---

## 2. Every prompt and its accepted input

### 2.1 String inventory (operator-facing, MSTYP dialogue)

All [VERIFIED] by hexdump. ND text here is **7-bit ASCII**, terminated by an
apostrophe `'` (0x27); a NUL byte pads to the word boundary.

| address | text |
|---|---|
| `ram:934a` | `\r\nGIVE DISK TYPE AS ONE OF THE FOLLOWING OCTAL NUMBERS:\r\n\r\n 0: DISC-14MB … 24: SCSI\r\n\r\nDISK TYPE: '` |
| `ram:9701` | `\r\nINITIALIZED FOR: '` |
| `ram:970b` | `REMOVABLE\r\n'` |
| `ram:9711` | `FIXED\r\n'` |
| `ram:9817` | `\r\nPLEASE DEFINE THE DISC TYPE (MSTYP) !\r\n\r\nMSTYP  SINTRAN DEVICE NAME\r\n\r\n'` |
| `ram:983c` | `REMOVABLE OR FIXED (R/F): '` |
| `ram:984b` | `\r\nENTER MSTYP: '` |
| `ram:9853` | `\r\n\r\n'` (blank lines) |
| `ram:9856` | `\r\nREMEMBER THE MACM COMMANDS:\r\n\r\n)REDEF => REDEFINE DISC TYPE\r\n)HENT  => GET SINTRAN FROM SAVE-AREA\r\n22!    => START SINTRAN\r\n10,0$  => LOAD SINTRAN FROM DISKETTE\r\n\r\nTYPE ANY MACM COMMAND:\r\n\r\n'` |
| `ram:9597..95aa` | 20-word pointer table to the 20 `MSTYP  SINTRAN DEVICE NAME` lines (`ram:95ab` … `ram:96fc`) |

Other operator-facing strings elsewhere in the image (from `list_strings`, all
[VERIFIED]): `ILL. ADDRESS`, `WHAT? `, `RANGE EXCEEDED`, `ILL. INSTRUCTION`,
`ILL. MNEMONIC`, `ALREADY DEFINED`, `POSSIBLE FAULT`, `TABLE FULL`, `ERROR`,
`ILL. BRF UNIT INITIATION` (`ram:8431`–`ram:84e5`); `UDEF ENTRY`, `)FILL MISSING`,
`ILL. EXPRESSION`, `OPTION MISSING`, `SYMBOL NOT DEFINED`, `EXT DEFINED`,
`EXT IN ADDRESS ARITHMETIC`, `\r\n% **** ERROR AT: `;
`REGISTERS:  P X T A D L S B`, `LEVEL `, `PAGE TABLES:` (`ram:998d`);
`UNDEF. CORELOAD NUMBER`, `UNDEF CORELOAD NO:`, `UNDEF SYMBOLS`;
`WRITE `, `READ `, `ERROR`, `STATUS= ` (`ram:9fb7`–`ram:9fc2`);
`DISASSEMBLER ERROR`, ` CHECKSUM ERROR`, `\r\nREAD ERROR`, `\r\nFEED ERROR`.

### 2.2 Prompt: `DISK TYPE: ` — routine `disktype_menu_prompt_and_parse` @ `ram:94ad`

**[VERIFIED]** disassembly:

```
ram:94ad: 58 19   LDX *0x94c6         ; X := 0x934A  ("GIVE DISK TYPE …")
ram:94ae: ba 19   JPL I *0x94c7       ; call 0x915C  (print string at X)
ram:94af: ba 19   JPL I *0x94c8       ; call 0x9CC3  (read an octal number)
ram:94b0: a8 fd   JMP *0x94ad         ; <- error return: re-prompt
ram:94b1: b1 fc   JAN *0x94ad         ; negative value -> re-prompt
ram:94b2: f3 15   SAX 0x15            ; X := 21
ram:94b3: c6 3d   SKP DA,LST,SX       ; skip if A < 21
ram:94b4: a8 f9   JMP *0x94ad         ; out of range -> re-prompt
ram:94b5: cc 6f   COPY SA, DX
ram:94b6: cc 2f   RADD SA, DX         ; X := 2*answer
ram:94b7: 2e 12   LDD ,X I *0x94c9    ; A,D := table[0x9483 + 2*answer]
ram:94b8: 09 87   STA -0x79,B         ; M[0x8342] := A   (MSTYP)
ram:94b9: cc 4d   COPY SD, DA
ram:94ba: 09 80   STA -0x80,B         ; M[0x833B] := D   (SINTRAN disc-type code)
ram:94bb: a8 08   JMP *0x94c3
```

* **Input accepted:** an **octal** number, read by the routine at `ram:9CC3`.
  `ram:94af` is a *skip return* call: the error path returns to `94b0` (re-prompt),
  the success path to `94b1`. [INFERRED from the `JMP *0x94ad` at the return
  address followed by validation code — the standard ND-100 skip-return idiom.]
* **Valid range: 0 to 0o24 inclusive** [VERIFIED] — `SAX 0x15` (21 decimal) then
  `SKP DA,LST,SX`, i.e. `answer < 21`. Plus `JAN` rejects negatives.
* **Bad input:** silently re-prompts from the top, reprinting the whole menu.
  There is no error message on this path. [VERIFIED — every rejection path is
  `JMP *0x94ad`.]
* **COULD NOT DETERMINE — the terminator character.** It is consumed inside the
  number reader at `ram:9CC3`, which I did not disassemble.

### 2.3 Prompt: `ENTER MSTYP: ` — routine `mstyp_menu_prompt_and_parse` @ `ram:94cc`

**[VERIFIED]** `ram:94cc`–`ram:94f7` is a straight-line sequence of 21
`LDX *lit ; JPL I *0x952b` pairs that print, in order:
`ram:9853` (blank lines), `ram:9817` (the `PLEASE DEFINE THE DISC TYPE (MSTYP) !`
header), then the **19** name lines pointed to by `ram:952d`…`ram:953f`, then
`ram:984b` (`ENTER MSTYP: `).

**[VERIFIED] The menu prints 19 lines, not 20.** The display list at
`ram:952d`–`ram:953f` is `95ab, 95b2, 95bb, 95cc, 95e6, 960a, 9624, 962e, 963f,
9648, 9659, 966f, 9678, 968b, 969e, 96b1, 96cd, 96e9, 96fc` — it **skips
`ram:95b0`**, which is the string `?\r\n'`, the MSTYP 1 entry. The 20-entry
*lookup* table at `ram:9597` does include it.

Validation, **[VERIFIED]**:

```
ram:94f8: ba 49   JPL I *0x9541       ; call 0x9CC3 (read octal number)
ram:94f9: a8 d3   JMP *0x94cc         ; <- error return: re-prompt
ram:94fa: b1 d2   JAN *0x94cc         ; negative -> re-prompt
ram:94fb: f2 14   SAX 0x14            ; X := 20
ram:94fc: c6 35   SKP DA,LST,SX       ; skip if A < 20
ram:94fd: a8 cf   JMP *0x94cc         ; >= 0o24 -> re-prompt
ram:94fe: f2 01   SAX 0x01            ; X := 1
ram:94ff: c4 35   SKP …,SX            ; compare A with 1
ram:9500: a8 cc   JMP *0x94cc         ; MSTYP == 1 -> re-prompt
ram:9501: 09 87   STA -0x79,B         ; M[0x8342] := MSTYP
```

* **Input accepted:** an **octal** number, 0 to 0o23, **excluding 1**.
* **MSTYP 1 is explicitly rejected** [VERIFIED — the `SAX 1` / `SKP` / `JMP
  re-prompt` at `ram:94fe`–`ram:9500`], which is exactly the entry whose name
  string is `?` and whose parameter record (`ram:9734`) is all zeros.
* **Storage: `ram:8342`** [VERIFIED — `STA -0x79,B` with B = `0x83bb`, confirmed
  by `LDX *0x9589` at `ram:9549` loading the literal `0x8342` directly].
* Bad input re-prompts with the full menu; no error message.

### 2.4 Prompt: `REMOVABLE OR FIXED (R/F): `

**[VERIFIED]** disassembly at `ram:9510`:

```
ram:9510: 58 35   LDX *0x9545         ; X := 0x983C ("REMOVABLE OR FIXED (R/F): ")
ram:9511: ba 1a   JPL I *0x952b       ; print
ram:9512: bb 46   JPL I 0x46,B        ; read one character -> A
ram:9513: f5 ae   AAA -0x52           ; A - 'R'
ram:9514: b2 04   JAZ *0x9518         ; 'R' -> A == 0  (removable)
ram:9515: f5 0c   AAA 0x0c            ; net A - 'F'
ram:9516: b3 fa   JAF *0x9510         ; not 'F' -> re-prompt
ram:9517: f1 01   SAA 0x1             ; 'F' -> A := 1  (fixed)
ram:9518: 0a 2b   STA I *0x9543       ; M[0x98B8] := 0 or 1
ram:9519: bb 46   JPL I 0x46,B        ; read (consume terminator)
```

* **Input accepted: exactly one character, `R` or `F`, upper case only.**
  [VERIFIED — `AAA -0x52` is `-'R'`; `-0x52 + 0x0c = -0x46` is `-'F'`. Lower case
  `r` = 0x72 / `f` = 0x66 would not match either test.]
* **Anything else re-prompts** (`JAF *0x9510`), with no error message. [VERIFIED]
* The answer is stored as an **index 0/1** at `ram:98b8`, used both to pick the
  library mark and to decide whether to set the FIXED bit (§6.4).
* This question is asked **only for MSTYP 2 and MSTYP 6** — see §6.3.

### 2.5 Prompt: `DISK TYPE:` / `ENTER MSTYP:` selection logic

**[VERIFIED]** at `ram:94bf`:

```
ram:94bf: cc 65   COPY SL, DA         ; A := return link
ram:94c0: 0a 0a   STA I *0x94ca       ; M[0x98B9] := return link
ram:94c1: 49 87   LDA -0x79,B         ; A := M[0x8342]  (MSTYP)
ram:94c2: b2 eb   JAZ *0x94ad         ; MSTYP == 0 -> ask "GIVE DISK TYPE…"
ram:94c3: fa 90   BSKP 0x2, 0, *0x94c5
ram:94c4: aa 07   JMP I *0x94cb       ; -> 0x9502 (MSTYP already known, skip menu)
ram:94c5: a8 07   JMP *0x94cc         ; -> ask "ENTER MSTYP:"
```

So the two prompts are alternatives selected by a flag and by whether MSTYP is
already non-zero. The return address is parked in `ram:98b9` and jumped back to at
`ram:9587` (`LDA I *0x9596 ; COPY SA, DP`). [VERIFIED]

---

## 3. `10,0$` decoded

### 3.1 Where the string lives

**[VERIFIED]** `10,0$  => LOAD SINTRAN FROM DISKETTE` occurs at byte offset
`0x1312A` (mid-word, word `ram:9895`), inside the single help string that starts
at `ram:9856`:

```
130ac  0d 0a 52 45 4d 45 4d 42 45 52 20 54 48 45 20 4d 41 43 4d 20 43 4f 4d 4d 41 4e 44 53 3a
       "\r\nREMEMBER THE MACM COMMANDS:"
130ce  ")REDEF => REDEFINE DISC TYPE"
130ea  ")HENT  => GET SINTRAN FROM SAVE-AREA"
13111  "22!    => START SINTRAN"
1312a  "10,0$  => LOAD SINTRAN FROM DISKETTE"
13152  "TYPE ANY MACM COMMAND:"
```

### 3.2 What it is

**[VERIFIED] `10,0$` is NOT a MACM command.**

The evidence is a clean contrast within the very same help text:

* `)REDEF` **does** have a command-table entry — `ram:872a`, handler `ram:9257`.
* `)HENT` **does** have a command-table entry — `ram:8760`, handler `ram:9913`.
* `10,0$` and `22!` have **no** command-table entry. The command table stores
  names as 6-bit packed symbols; `,`, `$` and `!` are outside the symbol
  character set MACM's scanner builds names from, and no table value decodes to
  anything resembling either string.

**[INFERRED]** `10,0$` is therefore a command typed at the **ND-100 console
(MOPC / the microprogram operator communication)**, not at MACM's `)` prompt.
MACM prints it purely as a crib sheet for the operator, alongside `22!`, so the
operator knows what to type after leaving MACM. Reasoning: MACM contains the
string but contains no parser for it; the two entries in the list that *are*
MACM commands are both marked with the `)` prefix and both exist in the table,
and the two that are not marked with `)` are both absent from the table.

### 3.3 What the two numbers are

**COULD NOT DETERMINE from the MACM binary.**

I did **not** find any code in `MACM-1718L.BPUN` that parses `10,0$`, so I cannot
verify from this binary what `10` and `0` mean, nor which character is the
executor. Specifically:

* I could not confirm whether `$` is a load-executor and `!` a start-executor —
  MACM implements neither.
* I could not confirm whether `10` is a device number, a load-address, or an
  ALD (Automatic Load Descriptor) value.
* Naming the diskette device number reached by `10,0$` requires the MOPC
  microcode or the console-emulator, neither of which is this binary.

I will not guess. The honest state is: **`10,0$` is a console command whose
semantics are outside `MACM-1718L.BPUN`.** To close it, disassemble the ND-100
MOPC microcode or the panel/console handler, not MACM.

### 3.4 Relation to `)9READ`, `)HENT`, `)9GET` and the BPUN loader

* **[VERIFIED]** `)HENT` = `cmd_HENT_restore_core_image` at `ram:9913` — pulls the
  SINTRAN core image back from MACM's own save area. Table entry `ram:8760`.
* **[VERIFIED, from the pre-existing Ghidra naming]** `cmd_9READ_load_binary_tape`
  at `ram:b474`, `cmd_9SAVE_coreimage_to_disc` at `ram:a019`,
  `cmd_9GET_disc_to_coreimage` at `ram:a01c`, `cmd_GJEM_save_core_image` at
  `ram:990b` (independently re-confirmed here via its table entry `ram:8763`).
* **[INFERRED]** `10,0$` is the *bootstrap* step that gets MACM-plus-SINTRAN into
  memory in the first place, i.e. it happens *before* MACM's own commands are
  available; `)9READ` / `)HENT` / `)9GET` are what MACM offers *once it is
  running*. This ordering follows from the help text itself, which lists
  `10,0$` as "LOAD SINTRAN FROM DISKETTE" and `22!` as "START SINTRAN" — i.e.
  load, then start — and from `)HENT` being described as "GET SINTRAN FROM
  SAVE-AREA", a different source.

**No IOX instruction reachable from any `10,0$` handler was traced, because no
such handler exists in this binary.**

---

## 4. `22!` — MOPC vs MACM

**[VERIFIED]** the literal `22!    => START SINTRAN` is at byte `0x13111`
(word `ram:9888`), inside the same help string at `ram:9856`.

**[VERIFIED] `22!` is not a MACM command** — same argument as §3.2: no entry in
the 3-word command table, whose format and contents are proven by `REDEF`,
`HENT`, `GJEM`, `RBP` and `9EOF`.

**[INFERRED, consistent with the earlier agent's finding]** `22!` is a MOPC
console command meaning "start the program at address 22 octal", handled by the
ND-100 microcode/console monitor. I can corroborate the *shape* of the claim
from MACM (it is printed as a reminder and never parsed) but I cannot verify the
`!` semantics from this binary, because MACM does not implement `!`.

### Summary of the four items MACM lists

| item | MACM command? | evidence |
|---|---|---|
| `)REDEF` | **YES** | table entry `ram:872a` → handler `ram:9257` [VERIFIED] |
| `)HENT`  | **YES** | table entry `ram:8760` → handler `ram:9913` [VERIFIED] |
| `22!`    | **NO** — console/MOPC | string only, no table entry [VERIFIED negative]; MOPC meaning [INFERRED] |
| `10,0$`  | **NO** — console/MOPC | string only, no table entry [VERIFIED negative]; meaning COULD NOT DETERMINE |

---

## 5. The full generation dialogue, in order

Step status is marked per line. Steps 1–2 are outside this binary.

| # | who | what appears / what is typed | status |
|---|---|---|---|
| 1 | operator | `10,0$` at the ND-100 console — loads MACM+SINTRAN from the diskette | [INFERRED] from the help text; no code in MACM |
| 2 | operator | `22!` at the console — starts the loaded program | [INFERRED] likewise |
| 3 | MACM | if `M[0x8342]` (MSTYP) is 0 → prints `GIVE DISK TYPE AS ONE OF THE FOLLOWING OCTAL NUMBERS:` and the 21-line list `0: DISC-14MB` … `24: SCSI`, then `DISK TYPE: ` | **[VERIFIED]** `ram:94c2`, `ram:94ad`, string `ram:934a` |
| 4 | operator | an octal number `0`–`24` | **[VERIFIED]** range check `ram:94b2`–`94b4` |
| 4a | MACM | out of range or negative → silently reprints the whole menu | **[VERIFIED]** `JMP *0x94ad` |
| 5 | MACM | translates it via the table at `ram:9483` into MSTYP (`ram:8342`) and a disc-type code (`ram:833b`) | **[VERIFIED]** `ram:94b7`–`94ba` |
| 3′ | MACM | *alternative path*: prints `PLEASE DEFINE THE DISC TYPE (MSTYP) !`, the header `MSTYP  SINTRAN DEVICE NAME`, the 19 name lines, then `ENTER MSTYP: ` | **[VERIFIED]** `ram:94cc`–`94f7` |
| 4′ | operator | an octal MSTYP, `0`–`23`, **not** `1` | **[VERIFIED]** `ram:94fa`–`9500` |
| 6 | MACM | looks up `mstyp_record_ptr_table[MSTYP]` and copies 9 geometry words into the `)9BYTT` block at `ram:8343`–`ram:834b` | **[VERIFIED]** `ram:9503`–`950b` |
| 7 | MACM | if record word 0 == 1 (only MSTYP 2 and 6): prints `REMOVABLE OR FIXED (R/F): ` | **[VERIFIED]** `ram:950e`–`9511` |
| 8 | operator | one character, `R` or `F` | **[VERIFIED]** `ram:9513`–`9517` |
| 8a | MACM | any other character → reprints the question | **[VERIFIED]** `JAF *0x9510` |
| 9 | MACM | if `F`: sets bit 15 (0o100000) in `ram:8347, 8348, 834a, 834b`; and if `M[0x833b]==0o20`, rewrites it to 7 | **[VERIFIED]** `ram:951c`–`9528` |
| 10 | MACM | installs the library mark: fetches the packed 30-bit symbol via `record[10 + R/F index]` and writes it, with value −1, into the symbol slot at `0x5389`/`0x538b` | **[VERIFIED]** `ram:9561`–`9569` |
| 11 | MACM | prints blank lines, `INITIALIZED FOR: `, blank lines, then the chosen `MSTYP  SINTRAN DEVICE NAME` line (skipping its leading number word) | **[VERIFIED]** `ram:956a`–`9575` |
| 12 | MACM | for MSTYP 2 or 6 only, prints `REMOVABLE` or `FIXED` based on the sign of `M[0x8348]` | **[VERIFIED]** `ram:9576`–`9583` |
| 13 | MACM | prints `REMEMBER THE MACM COMMANDS:` … `TYPE ANY MACM COMMAND:` and returns to the command loop via `ram:98b9` | **[VERIFIED]** `ram:9584`–`9587` |
| 14 | operator | `)REDEF` to go round again, `)HENT` / `)9GET` / `)9READ` to bring in the image, `)9SAVE` / `)GJEM` to write it out | **[VERIFIED]** table entries; exact ordering of a real session **[INFERRED]** |

**Note on step 11's exact rendering:** the pointer sequence printed is
`ram:9853` ("\r\n\r\n"), `ram:9701` ("\r\nINITIALIZED FOR: "), `ram:9853` again,
then the device-name line. The pointers are [VERIFIED]; the resulting screen
layout is [INFERRED] from them.

---

## 6. MSTYP — the tables and the complete mapping

This is the section the whole exercise was for. **Every row below is [VERIFIED]**
from data in the image plus the code that indexes it, and the mapping is
cross-checked 21/21 against the two independent name tables (§6.6).

### 6.1 The `DISK TYPE` menu (21 entries, octal 0–24) — string `ram:934a`

```
 0: DISC-14MB                     11: DISC-70MB
 1: DISC-21MB                     12: DISC-74MB   (DISC-36MB-C (BUTTERFLY))
 2: DISC-23MB                     13: DISC-75MB
 3: DISC-28MB                     14: DISC-140MB  (DISC-2-70MB)
 4: DISC-30MB   (DISC-60MB/DISC-90MB)   15: DISC-2-75MB
 5: DISC-33MB                     16: DISC-288MB-R (DISC-225MB-R/DISC-3-75MB/DISC-4-70MB-R)
 6: DISC-38MB                     17: DISC-288MB-F (DISC-4-70MB-F)
 7: DISC-45MB                     20: DISC-450MB-F (DISC-2-225MB-F/DISC-6-70MB-F)
10: DISC-66MB                     21: DISC-288MB-E (DISC-4-70MB-E)
                                  22: DISC-450MB-N (DISC-2-225MB-N/DISC-6-70MB-N)
                                  23: DISC-288MB-N (DISC-4-70MB-N)
                                  24: SCSI
```

### 6.2 The translation table `ram:9483` — DISK TYPE → MSTYP

21 entries × 2 words. Read at `ram:94b7` with `X = 2 × answer`.
`LDD` loads **A from the first word, D from the second**; A → MSTYP (`ram:8342`),
D → disc-type code (`ram:833b`).

| DISK TYPE (oct) | menu name | words | → MSTYP (oct) | → `ram:833b` |
|---|---|---|---|---|
| 0 | DISC-14MB | `0008 0008` | 10 | 8 |
| 1 | DISC-21MB | `0008 0009` | 10 | 9 |
| 2 | DISC-23MB | `000a 000a` | 12 | 10 |
| 3 | DISC-28MB | `000c 000c` | 14 | 12 |
| 4 | DISC-30MB | `0006 0010` | 6 | 16 |
| 5 | DISC-33MB | `0003 0002` | 3 | 2 |
| 6 | DISC-38MB | `0004 0011` | 4 | 17 |
| 7 | DISC-45MB | `0009 000b` | 11 | 11 |
| 10 | DISC-66MB | `0003 0003` | 3 | 3 |
| 11 | DISC-70MB | `0004 0012` | 4 | 18 |
| 12 | DISC-74MB | `000b 000d` | 13 | 13 |
| 13 | DISC-75MB | `0004 0013` | 4 | 19 |
| 14 | DISC-140MB | `000d 0014` | 15 | 20 |
| 15 | DISC-2-75MB | `0007 0016` | 7 | 22 |
| 16 | DISC-288MB-R | `0005 0017` | 5 | 23 |
| 17 | DISC-288MB-F | `000e 0019` | 16 | 25 |
| 20 | DISC-450MB-F | `0010 001a` | 20 | 26 |
| 21 | DISC-288MB-E | `000f 000f` | 17 | 15 |
| 22 | DISC-450MB-N | `0011 001c` | 21 | 28 |
| 23 | DISC-288MB-N | `0012 001d` | 22 | 29 |
| 24 | **SCSI** | `0013 001e` | **23** | 30 |

**COULD NOT DETERMINE** what `ram:833b` means beyond "a per-disc-type code that
the FIXED answer remaps from 0o20 to 7" (`ram:9524`–`9528`). Note that the only
entry with `833b == 16` is DISK TYPE 4 / MSTYP 6, which is exactly one of the two
MSTYPs that ask R/F — a satisfying internal consistency, but the field's purpose
is still open.

### 6.3 The MSTYP record table

**Pointer table `ram:9715`, 20 words, indexed directly by MSTYP.**
[VERIFIED] `ram:9503`–`9505`: `LDA -0x79,B ; LDX *0x9542 (=0x9715) ; RADD SA,DX ;
LDX 0,X`.

**Record layout** (11 words, or 12 when there is an R/F variant):

| word | meaning | evidence |
|---|---|---|
| 0 | 1 ⇒ ask REMOVABLE/FIXED; 0 ⇒ don't | `ram:950e` `LDA 0,X` / `ram:950f` `JAZ *0x9546` [VERIFIED] |
| 1 | **SINTRAN device number** | copied to `ram:8343` by `LDF 1,X ; STF -0x78,B` [VERIFIED] |
| 2–3 | `0o30000`, `0o30000` (constant in every record) | [VERIFIED] |
| 4 | `0o100` (constant in every record) | [VERIFIED] |
| 5–6 | geometry pair; bit 15 set on FIXED | copied to `ram:8347/8348` [VERIFIED] |
| 7 | `0o175777` (constant in every record) | copied to `ram:8349` [VERIFIED] |
| 8–9 | geometry pair; bit 15 set on FIXED | copied to `ram:834a/834b` [VERIFIED] |
| 10 | pointer to packed 30-bit library-mark symbol (removable / default) | `ram:9561`–`9566` [VERIFIED] |
| 11 | same, FIXED variant — present only when word 0 == 1 | selected by `ram:98b8` = 1 [VERIFIED] |

**COULD NOT DETERMINE** the individual meaning of words 2–9 (geometry: almost
certainly sectors/track, words/sector, cylinder counts and similar). Only three
distinct parameter sets exist:

| set | used by | w2..w9 (octal) |
|---|---|---|
| DRUM | MSTYP 0 | `30000 30000 100 4000 4000 175777 2000 40` |
| Winchester-A | MSTYP 2 | `30000 30000 100 2000 2000 175777 1000 10` |
| standard | MSTYP 3 – 23 | `30000 30000 100 400 400 175777 200 2` |

### 6.4 The library marks

**[VERIFIED]** Eight 2-word packed symbols at `ram:9807`–`ram:9816`, decoded with
`nd_sixbit_decode.py`:

| address | words | symbol |
|---|---|---|
| `ram:9807` | `0011 254d` | `DRUM` |
| `ram:9809` | `0000 0000` | (none — MSTYP 1's placeholder) |
| `ram:980b` | `1214 d3d6` | `REMOV` |
| `ram:980d` | `0625 8144` | `FIXED` |
| `ram:980f` | `0213 2e38` | `BD288` |
| `ram:9811` | `0210 6258` | `BDFIX` |
| `ram:9813` | `17e0 9383` | `W8INC` |
| `ram:9815` | `130c 14c9` | `SCASI` |

All six marks the brief asked about are accounted for, plus `DRUM` and the null
entry. **[VERIFIED]** installation code:

```
ram:9561: 5a 29   LDX I *0x958a       ; X := M[0x98B7]  (record base address)
ram:9562: 4a 29   LDA I *0x958b       ; A := M[0x98B8]  (0 = removable, 1 = fixed)
ram:9563: cc 2f   RADD SA, DX         ; X := base + R/F index
ram:9564: f7 0a   AAX 0xa             ; X := base + index + 10
ram:9565: 5c 00   LDX 0x0,X           ; X := record[10 + index]  = mark pointer
ram:9566: 2c 00   LDD 0x0,X           ; A,D := the packed 30-bit name
ram:9567: 22 25   STD I *0x958c       ; store name at 0x5389/0x538A
ram:9568: f1 ff   SAA -0x1
ram:9569: 0a 24   STA I *0x958d       ; store value -1 at 0x538B  (mark = TRUE)
```

The value `−1` is what makes it a *set* library mark. Exactly **one** mark is
installed per generation run.

**[VERIFIED] the FIXED flag** (`ram:951c`–`9523`): when the answer is `F`, bit 15
(0o100000) is set in **four** words — `ram:8347`, `ram:8348`, `ram:834a`,
`ram:834b` — by two `LDD / BSET 0xf,DA / BSET 0xf,DD / STD` pairs. The earlier
"FR=100000 marks fixed disc" note is therefore correct but incomplete: it is four
parameter words, not one, and it is *in addition to* selecting the `FIXED` /
`BDFIX` mark. `ram:8348` is the word later tested (`ram:957c` `LDA I *0x9592 ;
JAN`) to decide whether to print `FIXED` or `REMOVABLE`.

### 6.5 Where the parameters land

**[VERIFIED]** with B = `0x83bb` (derived from `LDX *0x9589` at `ram:9549`
loading the literal `0x8342`, which is `B-0x79`):

| address | source | note |
|---|---|---|
| `ram:833b` | disc-type table word 1 | `0o20` → 7 when FIXED |
| `ram:8342` | **MSTYP** | the operator's answer |
| `ram:8343` | record w1 | **device number** |
| `ram:8344`–`8346` | record w2, w3, w4 | |
| `ram:8347`, `8348` | record w5, w6 | +bit15 when FIXED |
| `ram:8349` | record w7 | |
| `ram:834a`, `834b` | record w8, w9 | +bit15 when FIXED |

**On the earlier claim that the `)9BYTT` parameter storage is at `ram:834c`:
REFUTED as stated.** [VERIFIED] The block written by the MSTYP dialogue is
`ram:8342`–`ram:834b` (ten consecutive words), plus `ram:833b`. `ram:834c` is one
word *past* the end of it. The routine at `ram:9546`–`ram:955f` then copies those
same ten words plus `ram:833b` out to eleven stride-3 slots starting at
`ram:872f` (B = `0x872d`), i.e. into the value fields of eleven consecutive
symbol-table entries — which is how the SINTRAN source gets to reference them.

### 6.6 THE MSTYP MAPPING — the deliverable

| MSTYP (oct) | record @ | SINTRAN device name (`ram:9597[i]`) | device no. (oct) | library mark(s) | R/F asked? |
|---|---|---|---|---|---|
| 0 | `ram:9729` | ` 0 DRUM` | **0o540** | `DRUM` | no |
| 1 | `ram:9734` | `?` (rejected on input) | 0 | none | no |
| 2 | `ram:973f` | ` 2 DISC-10MB-1` | **0o500** | `REMOV` / `FIXED` | **YES** |
| 3 | `ram:974b` | ` 3 DISC-33MB-1, DISC-66MB-1` | 0o1540 | `BD288` | no |
| 4 | `ram:9756` | ` 4 DISC-38MB-1, DISC-70MB-1, DISC-75MB-1` | 0o1540 | `BD288` | no |
| 5 | `ram:9761` | ` 5 DISC-288MB-1-R, DISC-3-75MB-1, DISC-225MB-1-R` | 0o1540 | `BD288` | no |
| 6 | `ram:976c` | ` 6 DISC-30MB-1, DISC-60MB-1, DISC-90MB-1` | 0o1540 | `BD288` / `BDFIX` | **YES** |
| 7 | `ram:9778` | ` 7 DISC-2-75MB-1` | 0o1540 | `BD288` | no |
| 10 | `ram:9783` | `10 DISC-21MB-1, DISC-14MB-1` | **0o500** | `W8INC` | no |
| 11 | `ram:978e` | `11 DISC-45MB-1` | **0o500** | `W8INC` | no |
| 12 | `ram:9799` | `12 DISC-23MB-1  DISC-16MB-1` | **0o500** | `W8INC` | no |
| 13 | `ram:97a4` | `13 DISC-74MB-1  DISC-36-C (BUTTERFLY)` | **0o500** | `W8INC` | no |
| 14 | `ram:97af` | `14 DISC-28MB-1` | **0o500** | `W8INC` | no |
| 15 | `ram:97ba` | `15 DISC-140MB-1-F, DISC-2-70MB-1-F` | 0o1540 | `BD288` | no |
| 16 | `ram:97c5` | `16 DISC-288MB-1-F, DISC-4-70MB-1-F` | 0o1540 | `BD288` | no |
| 17 | `ram:97d0` | `17 DISC-288MB-1-E, DISC-4-70MB-1-E` | 0o1540 | `BD288` | no |
| 20 | `ram:97db` | `20 DISC-450MB-1-F, DISC-2-225MB-1-F, DISC-6-70MB-1-F` | 0o1540 | `BD288` | no |
| 21 | `ram:97e6` | `21 DISC-450MB-1-N, DISC-2-225MB-1-N, DISC-6-70MB-1-N` | 0o1540 | `BD288` | no |
| 22 | `ram:97f1` | `22 DISC-288MB-1-N, DISC-4-70MB-1-N` | 0o1540 | `BD288` | no |
| 23 | `ram:97fc` | `23 SCSI` | **0o144300** | `SCASI` | no |

Cross-check: every one of the 21 rows of §6.2 lands on an MSTYP whose device-name
line in this table lists the disc the operator picked. All 21 agree. For example
DISK TYPE 5 (`DISC-33MB`) → MSTYP 3, whose line reads `DISC-33MB-1, DISC-66MB-1`;
DISK TYPE 10 (`DISC-66MB`) → also MSTYP 3. DISK TYPE 24 (`SCSI`) → MSTYP 0o23,
device `0o144300`, mark `SCASI`. That 21-for-21 agreement between two tables that
are consumed by different code paths is what makes this mapping certain.

### 6.7 The SCSI device number 0o144300 as a bit pattern

**[VERIFIED]** the raw word: `ram:97fd` = `c8c0` = 0o144300 = `1100 1000 1100 0000`.
It occurs **exactly once** in the whole image (`search_bytes c8 c0` → 1 match).

**[VERIFIED, structural]** 0o144300 **cannot be an IOX device address.** The
ND-100 `IOX` instruction carries an 11-bit device field (max 0o3777); 0o144300
needs 16 bits. A device number this large is only reachable through `IOXT`,
which takes the number in the T register.

**COULD NOT DETERMINE** any further decomposition of 0o144300 into sub-fields.
I found no code in MACM that manipulates it — it is a constant that MACM merely
hands to the SINTRAN assembly as a symbol value. One observation worth recording
without over-reading it: bit 15 (0o100000) is already set in 0o144300, and bit 15
is also the FIXED flag applied to other parameter words — but MSTYP 0o23 has
record word 0 == 0, so the FIXED path never runs for SCSI and the two never
interact. Whether the coincidence is meaningful: COULD NOT DETERMINE.

---

## 7. Does MACM speak SCSI?

**Short answer: NO. `MACM-1718L.BPUN` (standalone `D:\ND\BPUN` build) is not
SCSI-aware. It performs blind block I/O to the SMD disc controller and trusts the
MSTYP the operator typed.**

### 7.1 No IOXT anywhere — the decisive negative

**[VERIFIED]** `search_bytes d3 0d` (the ND-100 `IOXT` opcode, 0o150415 = `0xD30D`)
over the entire program returns **0 matches**.

Combined with §6.7 — 0o144300 needs 16 bits and `IOX` only carries 11 — this is
conclusive: **MACM cannot address device 0o144300 at all.** It has no instruction
in it that is capable of doing so.

*(Caveat, stated for honesty: the numeric value of the `IOXT` opcode, 0o150415, is
ND-100 ISA knowledge I brought to the search, not something I re-derived from
this binary. The search itself and its zero result are verified.)*

### 7.2 What MACM actually drives: the SMD controller, device group 0o1540

**[VERIFIED]** disassembly at `ram:a46c` — Ghidra's own ND-100 device tables label
the registers:

```
ram:a46c: 08 14   STA *0xa480
ram:a46d: f1 00   SAA 0x0
ram:a46e: f2 12   SAT 0x12
ram:a46f: c3 b0   RDIV ST
ram:a470: d9 08   SHA 0x8
ram:a471: cc 0d   RADD SD, DA
ram:a472: eb 63   IOX 0x0363          ; SMD1_LOAD_BLOCK_ADDR   (= IOX 0o1543)
ram:a473: f1 00   SAA 0x0
ram:a474: f8 fd   BSET 0xf, DA
ram:a475: eb 65   IOX 0x0365          ; SMD1_LOAD_CONTROL      (= IOX 0o1545)
ram:a476: 48 0a   LDA *0xa480
ram:a477: eb 63   IOX 0x0363          ; SMD1_LOAD_BLOCK_ADDR
ram:a478: f4 02   AAB 0x2
ram:a479: 50 08   LDT *0xa481
ram:a47a: f1 04   SAA 0x4
ram:a47b: fc dd   BSTA 0xb, DA
ram:a47c: eb 65   IOX 0x0365          ; SMD1_LOAD_CONTROL
ram:a47d: cc 62   EXIT
```

This is a plain "load block address, load control word, go" sequence — a **dumb
block driver**, no command descriptor, no protocol phase handling.

**[VERIFIED]** occurrence counts across the image:
* `eb 65` (`IOX 0o1545`, SMD control) — 20+ sites, including `ram:a0a2, a340,
  a475, a47c, a485, a49b, a4f3, a4fa, a505, a51c, a78c, a7eb, a931, a9c8, a9cd,
  aa16, aa1b, ac74, ac7e, ac81`.
* `eb 63` (`IOX 0o1543`, SMD block address) — present, e.g. `ram:a472, a477`.
* `e9 40` (`IOX 0o500`, the Winchester device) — 4 sites: `ram:af09, af0b, c2d9,
  c2db`.

So `)9SAVE` / `)9GET` and the core-image engine at `ram:9d27` / `ram:9d4b` reach
the disc through **IOX to device group 0o1540 (SMD) and 0o500 (Winchester)** —
the same two device numbers that appear as record word 1 for MSTYP 3–23 and
MSTYP 2/10–14 respectively. **[INFERRED]** that is not a coincidence: MACM can
only save to the disc types it can itself drive, and SCSI is not one of them.

### 7.3 No SCSI Command Descriptor Blocks

**[VERIFIED negative]** Searching for the CDB opcode constants in the packed
high-byte form the SINTRAN kernel uses:
* `25 00` (READ CAPACITY(10), 0o022400) — 11 byte matches, **all at odd
  half-word offsets** (`ram:7f6d.1, 8175.1, 9b2b.1, 9c27.1, be4b.1, be85.1,
  bf47.1, c70a.1`) except three, none of which sit in a CDB-building context.
* `12 00` (INQUIRY) — 18 byte matches, again overwhelmingly at `.1` half-word
  offsets, i.e. they are not aligned 16-bit constants at all.

No aligned INQUIRY / READ CAPACITY / READ(6) / READ(10) / WRITE(6) / WRITE(10) /
TEST UNIT READY / REQUEST SENSE / MODE SENSE constant was found in a
CDB-construction context. **NOT FOUND.**

### 7.4 No disk name, vendor ID or product ID

**[VERIFIED]** Every device name MACM ever prints comes from its own **static**
string table at `ram:95ab`–`ram:96fc`, selected by the MSTYP the operator typed
(`ram:9570`–`9575`: `LDA I *0x9589 ; ADD *0x9591 ; COPY SA,DX ; LDX 0,X ; AAX 1 ;
JPL I *0x958f`). There is no code that reads an ASCII vendor/product field off a
device, and no comparison of any such string.

**[VERIFIED negative]** The complete operator-facing string inventory (§2.1)
contains no `WRONG DISC`, `ILLEGAL DISC`, `DISC NOT READY`, `NOT SCSI`, or any
vendor name. The only I/O diagnostics are the generic
`WRITE ` / `READ ` / `ERROR` / `STATUS= ` group at `ram:9fb7`–`ram:9fc2`,
`\r\nREAD ERROR` at `ram:b6c7`, `\r\nFEED ERROR` at `ram:b6ce`, and
` CHECKSUM ERROR` at `ram:b4ac`.

### 7.5 No validation of the disc against MSTYP — the heart of the question

**[VERIFIED]** The MSTYP dialogue (`ram:94ad`–`ram:9587`, disassembled in full
above) contains **not one I/O instruction**. It is: print, read a number,
range-check the number against a constant, index two tables, copy words into a
parameter block, install a symbol, print the name. Nothing is read from any drive
and nothing is compared against anything the hardware says.

**MACM does not verify the operator's answer against the actual disc. It cannot.
If the operator types the wrong MSTYP, MACM will happily generate a SINTRAN for
the wrong disc.**

### 7.6 No sense data, no SCSI status handling

**[VERIFIED]** The error reporting path prints a raw hardware status word —
`READ `/`WRITE ` + `ERROR` + `STATUS= ` at `ram:9fb7`–`ram:9fc2`. That is an SMD
controller status register, printed verbatim. There is no REQUEST SENSE, no sense
key decode, no additional-sense-code table.

### 7.7 Conclusion, and where SCSI awareness actually lives

**[VERIFIED]** MACM handles device 0o144300 purely as a **number**: it is a
constant in the MSTYP record for MSTYP 0o23, copied into `ram:8343` and from
there into a symbol value that the SINTRAN source assembles into the kernel.
MACM never issues a single I/O to it.

**[INFERRED]** SCSI support therefore arrives only with the generated SINTRAN
kernel itself — which, per the cross-referenced carving work, *does* issue
INQUIRY, READ CAPACITY(10), reads the vendor control record from the last block,
validates block size, and has `2210 / 144300B` in `MDISCS`. MACM's job is to bake
the right device number and the right library mark into that kernel; the
protocol-aware driver is the kernel's, not MACM's. Reasoning: MACM contains
neither IOXT nor any CDB construction, so no SCSI transaction can originate in
it, yet the constant 0o144300 and the mark `SCASI` are both present as
assembly-time data destined for the kernel.

---

## 8. Open questions

1. **`10,0$` semantics.** Not resolvable from MACM — the binary contains the
   string and no parser. Needs the ND-100 MOPC microcode or the console handler.
   Same for the `!` executor in `22!`.
2. **The eleven 3-word slots at `ram:872d`–`ram:874d`.** Their value words are
   verified as the `)9BYTT` parameter destinations, but the name words do not
   decode as 6-bit symbols. Layout unknown. Raw words in §1.
3. **Record words 2–9** — the disc geometry parameters. Only three distinct
   parameter sets exist (§6.3); which word is sectors/track vs words/sector vs
   cylinders is undetermined. Comparing against a known SINTRAN `)9BYTT` command
   line would close this quickly.
4. **`ram:833b`** — the second value from the DISK TYPE table. Purpose unknown
   beyond the `0o20 → 7` FIXED remap.
5. **The full command-table extent and entry list.** Format proven, five entries
   decoded; the table was not dumped end to end. `nd_sixbit_decode.py` makes this
   mechanical whenever it is wanted.
6. **The 0o144300 bit pattern.** No decomposition evidence in MACM.
7. **The floppy build.** Everything here is the standalone
   `D:\ND\BPUN\MACM-1718L.BPUN`. The L-floppy copy (0o76203 / 19,738 words) is
   465 words larger and was **not** examined; the MSTYP tables may have moved or
   grown. Extract read-only with
   `ndtool -x -o <dir> D:\ND\S\VSXL1.IMG` (no `-p` — it strips bit 7 and corrupts
   binaries) if that comparison is wanted.

---

## Ghidra annotations added by this analysis

In `MACM-1718L.BPUN`:

Functions created — `disktype_menu_prompt_and_parse` (`ram:94ad`),
`mstyp_menu_prompt_and_parse` (`ram:94cc`),
`mstyp_install_libmark_and_report` (`ram:9546`).

Data labelled — `sintran_disktype_to_mstyp_table` (`ram:9483`),
`mstyp_name_ptr_table` (`ram:9597`), `mstyp_record_ptr_table` (`ram:9715`),
`var_mstyp_record_addr` (`ram:98b7`), `var_mstyp_fixed_index` (`ram:98b8`),
`var_mstyp_return_link` (`ram:98b9`), `mark_DRUM` (`ram:9807`),
`mark_REMOV` (`ram:980b`), `mark_FIXED` (`ram:980d`), `mark_BD288` (`ram:980f`),
`mark_BDFIX` (`ram:9811`), `mark_W8INC` (`ram:9813`), `mark_SCASI` (`ram:9815`).

Comments — plate comments on `ram:9483` and `ram:9715` documenting both table
layouts; a pre-comment on `ram:951c` documenting the FIXED bit-15 path.

Nothing was renamed that is not positively identified above. Nothing under
`D:\ND\` was modified.
