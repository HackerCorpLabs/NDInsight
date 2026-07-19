# S3SM5 - ND-500 System Monitor: Labeled Routine Map

Segment file: [`../segments/030-S3SM5.bin`](../segments/030-S3SM5.bin) (98304 bytes / 0x18000)
Disassembler: `nd500-dis` (RAW mode, big-endian, no byte-swap)
Symbol source: [`030-S3SM5.ghidra-symbols.txt`](030-S3SM5.ghidra-symbols.txt),
[`../../../../../SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT`](../../../../../SINTRAN/NPL-SOURCE/SYMBOLS/L07/N500-SYMBOLS.SYMB.TXT)
ND-100 servicing side: [`../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL`](../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL)

Version stamp (segment header): `88. 8.17` revision `L00` (VSX-500 / L release).

Convention in this document: **MON call numbers and symbol values are octal** (matching the
NPL/symbol tables); **file offsets are hex** (matching the disassembler).

---

## 1. Segment structure map

The segment is loaded at **runtime word address 40000B = 0x4000**. The Ghidra symbol
values therefore relate to file offsets as:

```
runtime_word = file_word + 0x4000            (file_byte = 2 * (runtime_word - 0x4000))
```

VERIFIED: symbol `VERSI 0x4005` -> file word 5 -> file byte 0x0A, which holds the ASCII
version string `88. 8.17`; `REVIS 0x400a` -> file byte 0x14 holds `L00`.

| Region | File offset (hex) | Contents |
|--------|-------------------|----------|
| Header / control block | `0x0000 - 0x005F` | 48 words. Word0 `0x2C00`, version string `88. 8.17'` at `0x0A`, revision `L00'` at `0x14`, capability/domain flag words. Maps to Ghidra symbols `FSCAP 0x4000 .. VERSI/REVIS`. |
| **MON dispatch vector table** | `0x0060 - 0x03FF` | 464 x 16-bit big-endian words. Slot index = **octal MON call number**. Non-zero values >= 0x8000 are handler entry offsets; `0x0000` = no handler; a large group share one value (`0x9533`) = the shared "illegal MON call" handler. See section 2. |
| Message / error-string pool | `0x0400 - 0x0F3D` | `'`-terminated (0x27) ASCII operator/error text referenced by the MON handlers (e.g. `Open File Table is full`, `Illegal monitor call number` @ `0x0663`, `Priviliged monitor call` @ `0x0C35`, `ND-500(0) power fail has occured`). |
| Segment/process name pool | `0x0F32 - 0x0F5F` | `ORIGPROCESS' SWAPPER' PROCESS-0' TERMINAL-' DATCL'`. Additional segment-class names (`RESIDENT-SEGMENT 0x604A`, `SCRATCH-SEGMENT 0x605C`, `PROCESS-SEGMENT 0x606C`, `EXTRA-DATA-SEGMENT 0x60AC`). |
| Padding / uninitialised | `0x0F60 - 0x800E` | Sparse data / mostly zero between the pre-code pools and the code region. |
| **Code region** | `0x800F - 0x17467` | ND-500 code with inline `'`-terminated ASCII strings interspersed (data-before/after-code PLANC layout). First byte `0x800F` = `ret` (used as a null-handler stub target). |
| Command name + jump-stub table | `0xA57C - 0xA73F` | 29 command names (`DATCL' COPY' SCHEDULE' ... SET-TEMPORARY-FILE'`) immediately followed by a parallel 29-entry 16-bit stub-pointer array at `0xA716`. See section 3. |
| Zero pad | `0x17468 - 0x17FFF` | Zero fill to segment size. Last non-zero byte at `0x17467`. |

---

## 2. MON dispatch vector table (slot = octal MON number)

The table at file `0x60` is indexed by octal MON call number: `file_byte = 0x60 + 2*MONnumber`.

**VERIFIED** that slot index == octal MON number, by two independent signals:

1. Slots **410B/411B** hold `0xBAE1`/`0xBB38`, sitting inside a contiguous **400B-421B**
   block of segment/process handlers - exactly the numeric range and order expected for
   `fixseg/unfix/.../wsegn/mxpisg`.
2. Dozens of *unimplemented* MON numbers (e.g. 106B-172B) all point to the **same** value
   `0x9533` - the classic "one shared illegal-call handler" fingerprint of a numeric
   dispatch table. `0x0000` slots = calls not routed to this segment at all.

### 2.1 Non-zero slots (octal MON -> handler file offset)

```
6=0x8bae   7=0x8bb5   10=0x8bf1  11=0x8c23  12=0x8c52  17=0x8c88  20=0x8c8c  21=0x8cb5
22=0x8cc1  24=0x8ccd  25=0x8cdd  27=0x8cf0  30=0x8d52  31=0x8cf0  32=0x8d52  34=0x8d8d
35=0x8d9f  36=0x8dc4  37=0x8dc8  40=0x8dfc  43=0x8e38  44=0x8e5b  45=0x8e79  46=0x8ebd
47=0x8ef3  51=0x8f15  52=0x8f22  53=0x8f68  56=0x9036  57=0x90b5  60=0x90b8  61=0x90cd
62=0x90f5  63=0x9120  64=0x9140  65=0x9162  66=0x9185  70=0x920f  71=0x922b  72=0x9272
73=0x9272  74=0x92ac  75=0x92ac  76=0x93ed  77=0x9449  100=0x9453 101=0x945d 102=0x9480
103=0x94a8 104=0x94c7 105=0x9503 106..172 mostly=0x9533 (shared illegal-MON handler)
113=0x954c 121=0x958e 122=0x95a8 125=0x95ac 126=0x95b5 127=0x98dd 130/132=0x9577
131/133=0x955a 134..136=0x957f 137=0x9937 142=0x9964 143=0x996b 150=0x997c 151=0x99af
152=0x99c7 154=0x9e82 155=0x9eb8 156=0x9eee 157=0x9f02 160=0x9f30 161=0x9f54 162=0x9f69
163=0x9f88 167=0x9fb1 170=0xa01d 171=0xa0d5 173=0xa0e1 174=0xa126 207=0xa17b 221=0xa209
222=0xa241 223=0xa292 224=0xa2d8 225=0xa31b 226=0x9036 227=0xa36c 235=0xa39d 236=0xa3bd
237=0xa3d4 240=0xa3ff 241=0xa446 242=0xa46d 243=0x9036 244=0xa49b 245=0xa4a7 246=0xa534
247=0xa5a0 250=0xa5ea 251=0xa5ef 252=0xa61f 253=0xa66a 254=0xa688 255=0xa693 256=0xa69e
257=0xa6e7 260=0xa75d 261=0xa773 262=0xa79f 264=0xa7ad 265=0xa825 266=0xa89d 270=0xa8ae
271=0xa8cc 275=0xa8e9 276=0xa906 277=0xa908 300=0xa96d 301=0xa987 302=0xa9e6 303=0xaa40
304=0xaa67 305=0xaa72 306=0xaa78 307=0xaa83 310=0xaa89 311=0xaa8e 312=0xaa93 314=0xaaca
315=0x8e5b 316=0x8cf0 317=0xaad9 320=0xaaf4 321=0xab29 322=0xab3b 324=0xabe3 325=0xabea
326=0xac1e 327=0xac49 330=0xacab 332=0xacd7 333=0xad0f 335=0xad21 336=0xae97 337=0xaeb3
340=0xaefb 341=0xaf61 342=0xb04d 344=0xb0cc 346=0xb1e9 351=0xb23d 355=0xb280 356=0xb2c1
361=0xb2d7 362=0xb2da 363=0xb2f4 364=0xb2fc 365=0xb301 366=0xb346 367=0xb35f 370=0xb3c4
371=0xb3d2 372=0xb454 373=0xb494 374=0xb610 375=0xb6a1 376=0xb6a1 377=0xb6c8
400=0xb716 401=0xb780 402=0xb798 403=0xb836 404=0xb885 405=0xba31 406=0xba6c 407=0xba9d
410=0xbae1 411=0xbb38 412=0x98dd 413=0xbb73 414=0xbb9e 415=0xbc20 416=0xbd70 417=0xbdf6
420=0xbe0f 421=0xbfcf 446=0xc1f0 447=0x8044 514=0xc000 515=0x800f
646/647/667=0xffff 662=0xff02 717=0xff00   (>=646B: table tail / sentinels, not code)
```

The low block (6B-53B) handlers reference the file-operation error strings
(`File number out of range`, `File number already used`, `Illegal access code`), i.e. these
are the ND-500 monitor's **file MON call** handlers. `0x9533` is the shared reject handler;
`0x9036` (reused by 56B/226B/243B) is a shared common entry.

---

## 3. Interactive command table (@HELP command set)

> **These are SINTRAN interactive commands, NOT MON calls.** The names below are the
> verbs the ND-500 Monitor's command interpreter accepts at its `@`/`N500:` prompt - the
> set you see listed by `@HELP` (CREATE-FILE, OPEN-FILE, LIST-FILES, COPY, SCHEDULE, HOLD,
> OPERATOR, ...). They are an entirely separate mechanism from the **numeric MON-call
> dispatch** in section 2 (the `0x60` vector table indexed by octal MON number). A command
> name here is a typed operator command, not a monitor-call name, and must not be read as
> one - e.g. the command `OPEN-FILE` is the interactive verb, whereas the MON-call that
> opens a file is a numbered call in the section-2 table. Do not equate the two lists.

S3SM5 holds this command set as a **name-driven command interpreter**: a table of command
names with a parallel stub-pointer array sitting adjacent in the code region, so
**slot i -> name[i] -> stub[i]** 1:1.

**VERIFIED** by file layout: the 29-word pointer array at `0xA716` directly follows the
last name (`SET-TEMPORARY-FILE'`) at `0xA702`, in the same order.

| # | Name (file offset) | Stub offset | # | Name | Stub |
|---|--------------------|-------------|---|------|------|
| 0 | DATCL `0xA57C` | `0x92be` | 15 | FILE-STATISTICS | `0x9319` |
| 1 | COPY | `0x92c1` | 16 | OPEN-FILE | `0x9321` |
| 2 | SCHEDULE | `0x92c4` | 17 | CONNECT-FILE | `0x9326` |
| 3 | HOLD | `0x92c9` | 18 | CLOSE-FILE | `0x932d` |
| 4 | TERMINAL-MODE | `0x92cc` | 19 | LIST-OPEN-FILES | `0x9333` |
| 5 | OPERATOR | `0x92d3` | 20 | SET-BLOCK-SIZE | `0x933b` |
| 6 | WAIT-FOR-OPERATOR | `0x92d8` | 21 | SET-PERMANENT-OPEN | `0x9343` |
| 7 | SET-TERMINAL-TYPE | `0x92e1` | 22 | SET-BYTE-POINTER | `0x934d` |
| 8 | GET-TERMINAL-TYPE | `0x92ea` | 23 | SET-BLOCK-POINTER | `0x9356` |
| 9 | CREATE-FILE | `0x92f3` | 24 | SCRATCH-OPEN | `0x935f` |
| 10 | EXPAND-FILE | `0x92f9` | 25 | COPY-FILE | `0x9366` |
| 11 | DELETE-FILE | `0x92ff` | 26 | APPEND-SPOOLING-FILE | `0x936b` |
| 12 | RENAME-FILE | `0x9305` | 27 | DELETE-SPOOLING-FILE | `0x9376` |
| 13 | SET-FILE-ACCESS | `0x930b` | 28 | SET-TEMPORARY-FILE `0xA702` | `0x9381` |
| 14 | LIST-FILES | `0x9313` | (end) | 0xFFFF sentinel @ `0xA750` | |

The stub targets `0x92be..0x9381` are tightly packed (3-9 bytes apart) - a **jump/trampoline
table** into the larger command-handler bodies. INFERRED: each stub loads a function code and
branches into the shared request builder that, for the file commands, packages the
corresponding open-file / file-maintenance operation to the ND-100 file system (S3FS). Again:
these are command-interpreter entry points, not entries of the numeric MON dispatch table.

---

## 4. MON-relevant routines (the user's request)

Two disjoint servicing paths exist, and this is the key finding:

- **400B-series (segment/process fix) -> serviced INSIDE S3SM5.** In
  [`MP-P2-N500.NPL`](../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL) `MCHANDEL` (line
  ~1286 / addr 137006B), any MON call that is not 347B and not in 500B-523B falls through to
  `NORMMC` = *"MONITOR CALL SHOULD BE HANDLED BY THE SYSTEM MONITOR"* - i.e. forwarded to
  S3SM5, which dispatches it through the section-2 vector table.
- **500B-523B-series -> serviced ON THE ND-100 SIDE, never reaching S3SM5.** `MCHANDEL`:
  `IF A >= L12MIN(500B) AND A <= L12MAX(523B) THEN ... 5CMNO-L12MIN GOSW`. This is why the
  section-2 vector slots for 500B/501B/505B/510B-513B are `0x0000` or stubs
  (515B -> `0x800F` = `ret`; 501B -> `0x2400`, not a code offset).

### 4.1 GOSW table for 500B-523B (ND-100 level-12 servicing)

VERIFIED from [`MP-P2-N500.NPL`](../../../../../SINTRAN/NPL-SOURCE/NPL/MP-P2-N500.NPL) lines
1385-1389 (`5CMNO-L12MIN GOSW ...`). The GOSW order (base 500B) is:

| MON (octal) | User label | ND-100 handler (MP-P2-N500) | NPL location |
|-------------|------------|------------------------------|--------------|
| 500 | startpr | `STAPROC` | GOSW @1385 |
| 501 | stoppr | `NSTOPROC` | GOSW; stop/restart logic ~1547-1584 |
| 502 | (switch) | `SWITPROC` | GOSW |
| 503 | (in-str) | `NINSTR` | GOSW |
| 504 | (out-str) | `NOUTSTR` | GOSW |
| 505 | gerrcod | `GERRC` | GOSW |
| 506 | | `5SIBMO` | GOSW |
| 507 | (set prio) | `SPRIO` | GOSW |
| 510 | | `SWMC` | GOSW |
| 511 | DVIO | `DVIO` | SUBR @1688; A=511 test @1770; msg build 1811-1900 |
| 512 | XMSG | `A5XMSG` | SUBR @2062/2076; X5MASK/X5MAX @2071 |
| 513 | XMSG(B) | `B5XMSG` | SUBR @2062/2077 |
| 514 | | `M5TMOUT` | GOSW |
| 515 | 5MTRANS | `5MTRANS` | SUBR @2440 |
| 516-523 | (patch) | `M516..M523` -> `GO NORMMC` | @1451-1463 (patch stubs, fall to system monitor) |

INFERRED: `A5XMSG`/`B5XMSG` are the two halves of the XMSG interface (allocate xtblock via
`MON 2XMSG` / define wakeup, then dispatch on `A /\ X5MASK GOSW` over the LF* sub-functions
`LFDUM/LFDCT/LFREA/LFWRI/...`). The level-14 arrival side is `N5XXC: A GOSW` (line 393).

### 4.2 400B-series routines resolved to S3SM5 entries

| MON (octal) | User label | S3SM5 entry (hex) | ND-100 companion |
|-------------|------------|-------------------|-------------------|
| 410 | fixseg  (fix segment) | `0xBAE1` | `NORMMC` -> system monitor (MP-P2-N500 @1371) |
| 411 | unfix   (unfix segment) | `0xBB38` | `NORMMC` -> system monitor |
| 416 | wsegn   (write segment) | `0xBD70` | `NORMMC` -> system monitor |
| 417 | mxpisg  (max phys/segment) | `0xBDF6` | `NORMMC` -> system monitor |
| 400-407 / 412-421 | (neighbours) | `0xB716..0xBFCF` | same NORMMC path |

The surrounding code pool (`0xB700-0xC000`) is dense with the fix/transfer error strings the
handlers emit, e.g. `Byte address not modulo sector size in direct transfer'` @ `0xBAAD`
(immediately preceding the 410B entry), and the memory-fix errors from the pre-code pool
(`Max global fix`, `Impossible to do fix contiguous because of already System fixed pages`,
`Trying to fix pages shared with a Sintran-III segment`). This confirms the 400B block is the
**memory/segment fixing** group, consistent with `fixseg/unfix/wsegn/mxpisg`.

The friend's ND-100 handler names (`UNFIX`, `GPRNA`, `SPRNA`, `STOPP`, `STOPR`, ...) belong to
the ND-100-side monitor, not to S3SM5. For 425B/426B/427B (sprname/gprnum/gprname) and 500B
(startpr) the S3SM5 vector slot is `0x0000` (serviced elsewhere - process-name calls are
handled ND-100-side, not packaged by this segment).

### 4.3 Alignment with the MON-call documentation (`Developer/MON/calls/` YAMLs)

Each numeric MON slot that has a handler is cross-checked below against the documented
monitor call in [`../../../../../Developer/MON/calls/`](../../../../../Developer/MON/calls/)
(files named `<octal>B_<Name>.yaml`). "Handler" = S3SM5 code offset (section 2) for the
400B/low blocks, or the ND-100 `GOSW` entry (section 4.1) for the 500B block.

**500B-515B (serviced ND-100-side via GOSW):** the GOSW order matches the YAML names
**exactly** - strong VERIFICATION of the section-4.1 table.

| MON (oct) | Handler | YAML file / documented name | Finding |
|-----------|---------|-----------------------------|---------|
| 500 | ND-100 GOSW `STAPROC` | `500B_STARTPROCESS.yaml` / StartProcess | VERIFIED - GOSW slot 0 = start process by number. |
| 501 | ND-100 GOSW `NSTOPROC` | `501B_StopProcess.yaml` / StopProcess | VERIFIED - GOSW slot 1; NPL stop/restart logic @1547-1584. |
| 502 | ND-100 GOSW `SWITPROC` | `502B_SwitchProcess.yaml` / SwitchProcess | VERIFIED - name/position agree. |
| 503 | ND-100 GOSW `NINSTR` | `503B_InputString.yaml` / InputString | VERIFIED - `NINSTR` = N-500 input string. |
| 504 | ND-100 GOSW `NOUTSTR` | `504B_OutputString.yaml` / OutputString | VERIFIED - `NOUTSTR` = N-500 output string. |
| 505 | ND-100 GOSW `GERRC` | `505B_GetTrapReason.yaml` / GetTrapReason | VERIFIED - reads swapper error code (`gerrcod`). |
| 506 | ND-100 GOSW `5SIBMO` | **NO YAML** | Handler exists, **undocumented**. `5SIBMO` = SINTRAN buffer-mode/monitor sub-call (purpose unconfirmed). |
| 507 | ND-100 GOSW `SPRIO` | `507B_SetProcessPriority.yaml` / SetProcessPriority | VERIFIED - set process priority. |
| 510 | ND-100 GOSW `SWMC` | **NO YAML** | Handler exists, **undocumented** driver-level call. |
| 511 | ND-100 GOSW `DVIO` | **NO YAML** | Handler exists, **undocumented** (DVIO device-I/O; NPL SUBR @1688, `A=511` test @1770). |
| 512 | ND-100 GOSW `A5XMSG` | **NO YAML** | Handler exists, **undocumented** (XMSG interface half A; NPL @2062/2076). |
| 513 | ND-100 GOSW `B5XMSG` | **NO YAML** | Handler exists, **undocumented** (XMSG interface half B; NPL @2062/2077). |
| 514 | ND-100 GOSW `M5TMOUT` | `514B_ND500TimeOut.yaml` / ND500TimeOut | VERIFIED - `M5TMOUT` = ND-500 time-out/suspend. |
| 515 | ND-100 GOSW `5MTRANS` | **NO YAML** | Handler exists, **undocumented** (`5MTRANS` memory transfer; NPL SUBR @2440). |

**410B-421B (serviced inside S3SM5):**

| MON (oct) | S3SM5 handler | YAML file / documented name | Finding |
|-----------|---------------|-----------------------------|---------|
| 405 | `0xBA31` | `405B_SwitchUserBreak.yaml` / SwitchUserBreak | Slot present; body not decoded (INFERRED). |
| 406 | `0xBA6C` | `406B_AccessRTCommon.yaml` / AccessRTCommon | Slot present; body not decoded (INFERRED). |
| 407 | `0xBA9D` | **NO YAML** | Handler exists, **undocumented**. Immediately precedes the `direct transfer` error strings @ `0xBAAD` - likely a direct-transfer call. |
| 410 | `0xBAE1` | `410B_FIXINMEMORY.yaml` / FixInMemory (FIXMEM) | VERIFIED name+region: fixes a data segment in physical memory; surrounded by `Max global fix` / `fix contiguous` / `System fixed pages` strings. Param contract (FixType/FirstAddr/Length/ND100Addr) not confirmable from disassembly (see UNCERTAIN note). |
| 411 | `0xBB38` | `411B_MemoryUnfix.yaml` / MemoryUnfix (UNFIXM) | VERIFIED - unfix companion, adjacent to 410B as documented (`see_also`). |
| 412 | `0x98DD` | `412B_FileAsSegment.yaml` / FileAsSegment | Slot shares `0x98DD` with 127B - a common connect-file-as-segment entry (INFERRED). |
| 413 | `0xBB73` | `413B_FileNotAsSegment.yaml` / FileNotAsSegment | Slot present; body not decoded (INFERRED). |
| 414 | `0xBB9E` | `414B_BCNAFCAMAC.yaml` / BCNAF (CAMAC) | Slot present; body not decoded (INFERRED). |
| 415 | `0xBC20` | `415B_BCNAF1CAMAC.yaml` / BCNAF1 (CAMAC) | Slot present; body not decoded (INFERRED). |
| 416 | `0xBD70` | `416B_SaveND500Segment.yaml` / SaveND500Segment (WSEGN) | VERIFIED - writes modified segment pages back to disk. |
| 417 | `0xBDF6` | `417B_MaxPagesInMemory.yaml` / MaxPagesInMemory (MXPISG) | VERIFIED - limits pages a segment keeps in memory. |
| 420 | `0xBE0F` | `420B_GetUserRegisters.yaml` / GetUserRegisters | Slot present; body not decoded (INFERRED). |
| 421 | `0xBFCF` | `421B_GetActiveSegment.yaml` / GetActiveSegment | Slot present; body not decoded (INFERRED). |

**Low block 6B-53B (serviced inside S3SM5) - selected, alignment INFERRED:** these S3SM5
handlers cover the standard SINTRAN file/terminal/RT calls an ND-500 program may issue. The
handler strings (`File number out of range`, `File number already used`, `Illegal access
code`) match the file-oriented calls (6B/7B/10B), but the full slot-to-operation legend was
not disassembly-verified, so treat the pairings as INFERRED.

| MON (oct) | S3SM5 handler | YAML file / documented name |
|-----------|---------------|-----------------------------|
| 6  | `0x8BAE` | `6B_WRITESCRATCHFILE.yaml` / WriteScratchFile |
| 7  | `0x8BB5` | `7B_ReadBlock.yaml` / ReadBlock |
| 10 | `0x8BF1` | `10B_WriteBlock.yaml` / WriteBlock |
| 11 | `0x8C23` | `11B_GetBasicTime.yaml` / GetBasicTime |
| 12 | `0x8C52` | `12B_SetCommandBuffer.yaml` / SetCommandBuffer |
| 17 | `0x8C88` | `17B_SetTerminalType.yaml` / SetTerminalType |
| 20 | `0x8C8C` | **NO YAML** (undocumented) |
| 21 | `0x8CB5` | `21B_InUpTo8Bytes.yaml` / InUpTo8Bytes |
| 22 | `0x8CC1` | `22B_OutUpTo8Bytes.yaml` / OutUpTo8Bytes |
| 24 | `0x8CCD` | `24B_Out8Bytes.yaml` / Out8Bytes |
| 25 | `0x8CDD` | **NO YAML** (undocumented) |
| 27 | `0x8CF0` | `27B_GetRTDescr.yaml` / GetRTDescr |
| 30 | `0x8D52` | `30B_GetOwnRTAddress.yaml` / GetOwnRTAddress |
| 31 | `0x8CF0` | `31B_IOInstruction.yaml` / IOInstruction |
| 32 | `0x8D52` | `32B_OutMessage.yaml` / OutMessage |
| 34-40 | `0x8D8D..0x8DFC` | `34B_NormalPageTable` `35B_OutNumber` `36B_NoWaitSwitch` `37B_ReadADChannel` `40B_CloseSpoolingFile` |
| 43 | `0x8E38` | `43B_CloseFile.yaml` / CloseFile |
| 44 | `0x8E5B` | `44B_GetUserEntry.yaml` / GetUserEntry |
| 45-51 | `0x8E79..0x8F15` | **NO YAML** for 45B/46B/47B/51B (undocumented) |
| 52 | `0x8F22` | `52B_TerminalMode.yaml` / TerminalMode |
| 53 | `0x8F68` | `53B_GetSegmentEntry.yaml` / GetSegmentEntry |

**Undocumented handlers (present in S3SM5 or GOSW, no YAML) - the write-up targets:**
`20B`, `25B`, `45B`, `46B`, `47B`, `51B`, `407B`, `506B`, `510B`, **`511B (DVIO)`**,
**`512B (XMSG-A5XMSG)`**, **`513B (XMSG-B5XMSG)`**, **`515B (5MTRANS)`**. The user's four
priority undocumented calls (510/511/512/513/515) are confirmed to have live handlers in the
ND-100 GOSW dispatch yet **no MON-call YAML**.

**UNCERTAIN (contract not confirmed by disassembly):** for every 400B/low-block slot marked
INFERRED above, the documented YAML *parameter list* could not be cross-checked against the
handler body because `nd500-dis` does not frame these routines reliably (section 6). The
name/number alignment is trustworthy; the argument contracts are taken from the YAMLs as-is
and are **not** independently verified against S3SM5 code. 410B in particular
(FixType/FirstAddr/Length/ND100Addr) is documented but unverified here.

---

## 5. Cross-reference summary (who packages vs. who services)

```
ND-500 program issues MON n
        |
        v
  ND-100 driver kernel N500  ->  DECOMESS  ->  MCHANDEL   [MP-P2-N500.NPL @1286]
        |
        +-- n = 347B         --> 5SERVER            (nucleus call)
        +-- 500B <= n <= 523B --> GOSW STAPROC..M523 (serviced on ND-100 level 12)   <-- DVIO,XMSG,5MTRANS,startpr,stoppr,gerrcod
        +-- otherwise         --> NORMMC = "handled by the SYSTEM MONITOR" = S3SM5
                                       |
                                       v
                                 S3SM5 numeric vector table [file 0x60, slot=MONoctal]
                                       |
                                       +-- fixseg 410B -> 0xBAE1
                                       +-- unfix  411B -> 0xBB38
                                       +-- wsegn  416B -> 0xBD70
                                       +-- mxpisg 417B -> 0xBDF6
                                       +-- file ops 6B-53B, ... , illegal -> 0x9533
```

So: **S3SM5 PACKAGES/handles the 400B-series (segment fixing) and the file/terminal command
set; MP-P2-N500 SERVICES the 500B-series (process control, DVIO, XMSG, 5MTRANS).**

---

## 6. What is still unresolved

1. **Instruction-level decode is not trustworthy.** `nd500-dis` frames these routines
   inconsistently (frequent `???` opcodes, operands that are clearly ASCII from adjacent
   inline strings). The ND-500 variable-length encoding plus PLANC data-before/after-code
   layout means the byte at a vector target is often mid-way through a string or descriptor.
   The *dispatch mapping* (slot -> offset, name -> stub) is solid; the *body semantics* of
   individual 400B routines are only inferred from their neighbouring strings, not from a
   clean instruction trace. A corrected disassembler (or Ghidra with a proper ND-500 SLEIGH
   spec) is needed to confirm each handler's exact behaviour.
2. **Exact entry framing.** Several vector values (e.g. 410B=`0xBAE1`) land a few bytes inside
   the tail of the preceding `'`-terminated string. This is consistent either with
   data-before-code (entry points at a leading descriptor) or with a small fixed base
   adjustment; I could not prove which without a reliable decoder. Two candidate bases were
   tested (raw file-byte vs. runtime-word `2*(v-0x4000)`); the command-stub table is
   unambiguously **file-byte** (runtime-word interpretation lands on the name text), so the
   MON table is assumed file-byte too, but this is not byte-proven for every slot.
3. **Low-MON handler identities (6B-53B).** These are clearly the ND-500 monitor's file MON
   handlers (matching the `File number...`/`Illegal access code` strings), but the precise
   MON-number-to-operation legend for this segment's *internal* numbering was not
   cross-checked against an authoritative ND-500 monitor-call list.
4. **425B/426B/427B and 500B (sprname/gprnum/gprname/startpr).** Slot is `0x0000` in S3SM5.
   `startpr` is in the 500B GOSW as `STAPROC`; the process-name calls (`sprname/gprname`) were
   not located in either the S3SM5 vector table or the MP-P2-N500 GOSW - servicing point
   unconfirmed (likely another ND-100 monitor module).
5. **N500-SYMBOLS coverage.** The Ghidra symbol file resolves the header/data labels
   (`FSCAP`, `VERSI`, `REVIS`, fix-error message symbols) but does not carry entry-point names
   for the numbered MON handlers, so routine names above are structural/inferred, not symbolic.
