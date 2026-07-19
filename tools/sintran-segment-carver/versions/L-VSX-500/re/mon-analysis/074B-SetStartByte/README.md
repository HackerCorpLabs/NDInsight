# MON 074B (octal) - SetStartByte (SETBY)

Sets the next byte to be read or written in an already-opened mass-storage file (byte-pointer
positioning) so the next INBT/OUTBT or block transfer starts at a caller-supplied byte number.
The documented MON mnemonic is **SETBT**; the internal file-system worker symbol is **SETBY**
(`103720B` in `FILSYS-SYMBOLS`).

**Status:** dispatch head byte-proven (`GOTAB[074B] = 000000` = fall-through, matches a live read);
the `SETBY` worker body is real SINTRAN L bytes; the exact `MON 074 -> worker` link crosses an
uncarved kernel bridge (see [Honest caveats](#honest-caveats)). All addresses/values are **octal**.

- **Full disassembly:** [`074B-SetStartByte.ASM`](074B-SetStartByte.ASM) - the actual code, both regions (GOTAB dispatch word + SETBY worker).
- **Bytes live once in** the canonical segment layer: [`../../segments-ref/`](../../segments-ref/).

---

## Dispatch path

```mermaid
flowchart LR
    A["User program<br/>MON 074B"] --> B["ENT14 level-14<br/>T = MON number"]
    B --> C["GOTAB[074B] = 000000<br/>(byte + live proven)"]
    C -.uncarved MFELL/CALLPROC.-> D["SETBY worker<br/>006-S3FS :103720B"]
    D --> E["save 32-bit byte<br/>+ two JPL I FS workers"]
    class A blue
    class B,C teal
    class D,E green
    classDef blue fill:#E3F2FD,stroke:#0D47A1,color:#0D47A1
    classDef teal fill:#E0F7FA,stroke:#00838F,color:#00838F
    classDef green fill:#E8F5E9,stroke:#2E7D32,color:#2E7D32
```

`GOTAB[074B] = 000000` carries **no** handler address - it is a fall-through entry. The dashed hop
(`C -> D`) is the resident level-14 default path (`MFELL` / `CALLPROC` second-level dispatch), which
is **not present in any carved segment**, so it is the one link that cannot be followed statically.

---

## Code location (dispatch path)

Every row is a real region you can open. Byte offset = `(addr - loadbase)` in octal words x 2.

| Role | Segment (full disasm) | Addr range (octal) | Byte offset | Symbol | Verdict |
|------|------------------------|--------------------|-------------|--------|---------|
| GOTAB[074] dispatch word | [commoncode.asm](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.asm) - [.hex](../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex) | `071327B` (1 word) | 58798 | `GOTAB+074` = `000000` | **VERIFIED** (fall-through; no direct handler) |
| resident MFELL/CALLPROC bridge | - (uncarved) | - | - | `MFELL` / `CALLPROC` | **UNVERIFIED** (not carved; needs live trace) |
| SETBY worker body | [006-S3FS.asm](../../segments-ref/006-S3FS/006-S3FS.asm) - [.hex](../../segments-ref/006-S3FS/006-S3FS.hex) | `103720B-104202B` (179w closure) | 47008 | `SETBY` (folder name "SETBT" is the MON mnemonic; N500 `SETBT=112200B` is a different segment) | real SINTRAN L bytes; link **MISATTRIBUTED** |

**Verify by hand:** `grep '^71327 ' ../../segments-ref/SINTRAN-DATA_commoncode/SINTRAN-DATA_commoncode.hex`
-> `71327  000000 ... 58798`; then `dd if=../../../resident/SINTRAN-DATA_commoncode.bin bs=1 skip=58798 count=2 | od -An -tx1`
-> `00 00` (the fall-through word). For the worker:
`grep '^103720 ' ../../segments-ref/006-S3FS/006-S3FS.hex` -> byte offset `47008`; then
`dd if=../../../segments/006-S3FS.bin bs=1 skip=47008 count=8 | od -An -tx1` -> `22 43 cc 65 cc 59 f0 06`
(= octal `021103 146145 146131 170006`, the `SETBY` entry `STD I 103 / RADD / RADD / SAB 6`).

---

## Instruction walkthrough

Full listing: [`074B-SetStartByte.ASM`](074B-SetStartByte.ASM). `SETBY` and its four siblings
(`SETBC`, `SBSIZ`, `RMAX`, `REABT`) are near-identical skeletons that share one small pointer table;
annotating `SETBY` explains all of them.

**SETBY entry (103720B-103734B)** - the routine proper is 11 words (`SETBC=103735B` follows):
```
103720  021103   STD I 103      ; save 32-bit start-byte arg (D=A:D) via ptr@104024 = 147417
103721  146145   RADD CLD SL DA ; register setup
103722  146131   RADD CLD SB DD ; register setup
103723  170006   SAB 6          ; B := 6  (argument-block index)
103724  135100   JPL I 100      ; -> 104024 (ptr 003752B) : resident worker 3752B (UNCARVED)
103725  011100   STT I 100      ; store returned T via ptr@104025 = 147505
103726  135101   JPL I 101      ; -> 104027 (ptr 072622B) : resident worker 72622B (UNCARVED)
103727  124004   JMP 4          ; -> 103733 (normal path)
103730  040404   MIN ,B 4       ; (error/return tail)
103731  170772   SAA -6         ; A := -6  (error/skip code)
103732  125076   JMP I 76       ; -> 104030 (ptr 003776B) : resident common exit (UNCARVED)
103733  004402   STA ,B 2       ; store result A into arg block +2
103734  124375   JMP -3         ; -> 103731 (tail loop to SAA -6 / exit)
```

**Shared pointer table (104023B-104034B) - DATA, not code.** `nd100-dis` prints these words as bogus
instructions (it disassembles data as code); read them as raw words:
`104024=003752  104027=072622  104030=003776  104031=072072  104032=072351  104033=072016  104034=072014`.
The low targets (`3752B`, `3776B`) and the high targets (`72xxxB`) are **resident file-system routines
outside this carve** - they are not present in `006-S3FS.bin` at these addresses, so their exact
semantics cannot be proven from the carved bytes.

**Siblings + MROBJ/DROBJ.** `SETBC`/`SBSIZ`/`RMAX`/`REABT` repeat the identical skeleton with different
table displacements selecting other entries of the same pointer table. From `104035B` a larger routine
(`MROBJ`/`DROBJ`, with its own pointer table and a real `MON 152` / skip tests) runs; it is reachable by
direct branch from the sibling block, which is why the 179-word closure includes it - it is **not** the
MON 074B entry path and is documented only to justify the carve window.

What *is* provable from these bytes: `SETBY` (1) saves the 32-bit byte argument (`STD I`), (2) sets
`B=6`, (3) calls two shared workers, (4) on the error path loads `A := -6` and takes the common indirect
exit at `3776B`.

---

## Parameter / register contract

| Reg / field | Dir | Meaning | Verdict |
|-------------|-----|---------|---------|
| File number (INTEGER) | in | open-file handle whose byte pointer is set | VERIFIED (documented SETBT contract; consistent with `SAB 6` arg block) |
| Start byte (INTEGER4, 32-bit) | in | byte number for next read/write, numbered from 0 | VERIFIED as 32-bit (`STD I 103` saves full A:D pair) |
| `D` (A:D) | in | carries the 32-bit start-byte value at entry | VERIFIED (`STD I` at 103720B) |
| `T` | in/out | returned by worker 3752B, restowed via `STT I 100` | stored VERIFIED; meaning inferred (worker uncarved) |
| `B` | internal | set to `6` as the argument-block base index | VERIFIED (`SAB 6`) |
| `A` | out | error/return code; error tail loads `A := -6` (`SAA -6`) | value VERIFIED; exact error meaning inferred |
| skip return | out | normal-vs-error split at 103727B (`JMP 4`) vs error tail -> `JMP I 76` (3776B) | structure VERIFIED; skip semantics inferred (exit is resident) |

The *inputs* (file number + 32-bit byte) match the documented SETBT call and are consistent with the
code. The *outputs / error codes* depend on the resident workers (`3752B`, `72622B`, `3776B`) that are
**not** in this carve, so their precise values remain **inferred / UNVERIFIED**.

---

## Pseudo-code (for an emulator)

See **[`074B-SetStartByte.pseudo.c`](074B-SetStartByte.pseudo.c)** - a pseudo-C model of the handler
for emulator authors. The `SETBY` control flow (arg save, `B=6`, two `JPL I` worker calls, error tail)
is byte-verified; the file-system worker semantics are inferred from the call structure. Every
instruction is translated per the canonical
[`../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md`](../../instruction-semantics/ND100-INSTRUCTION-SEMANTICS.md).

---

## Honest caveats

**What is byte-proven:** `GOTAB[074B] = 000000` (level-14 dispatch, matches a live read of the running
system) = MON 074B is a fall-through call, **not** a resident GOTAB jump-table handler and **not** an
F16xx stub. The `SETBY` entry bytes at `103720B` are real code and match the disassembly and the L07
symbol table (`SETBY=103720B`).

**What is NOT proven:** the link from `GOTAB[074]=000000` to the `SETBY` worker in `006-S3FS`. A `000000`
GOTAB word gives no target address; the transfer runs through the resident `MFELL`/`CALLPROC`
second-level dispatch, which is in an **uncarved overlay**, so no byte-level edge connects entry 074 to
`103720B`. The `MON 074 -> SETBY` attribution therefore rests on the documented mnemonic + the matching
byte-pointer behaviour, not a followed pointer - hence **MISATTRIBUTED** in the strict sense.

**Reconciling the two names.** The folder is named "SETBT". `SETBT` is correct as the *documented MON
mnemonic*; as an *internal FILSYS symbol* the correct name is `SETBY=103720B`. A separate `SETBT=112200B`
exists only in `N500-SYMBOLS` (a different ND-500 segment) - do not conflate them. Both facts are
reported; neither by itself confirms which worker MON 074B actually enters.

Confirming the link needs a live trace: set MON 074B from a user program, break at level 14, confirm
`GOTAB[074]=000000` takes the CALLPROC default path, single-step the second-level dispatch, and confirm
P lands on `SETBY=103720B` (or reveals the true worker).

Method: [../../../../../EXTRACTING-RESIDENT-CODE.md](../../../../../EXTRACTING-RESIDENT-CODE.md) - dispatch reality:
[../../TASK-05-mismatches.md](../../TASK-05-mismatches.md) - master map: [../../MON-CALL-INDEX.md](../../MON-CALL-INDEX.md).
