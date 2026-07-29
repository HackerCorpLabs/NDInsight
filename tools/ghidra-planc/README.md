# ghidra-planc - decompiling ND PLANC-MC (MC68000) code in Ghidra

Tooling to make Ghidra's decompiler produce sane C from firmware compiled by Norsk Data's
**PLANC-MC** compiler for the MC68000 - for example the ENCOS Ethernet II controller firmware
(`Installation\Communication\Ethernet\x\stripped\encos-ser-all-banks-68k.bin`).

Out of the box Ghidra gets this code badly wrong, because PLANC-MC uses two conventions the stock
68000 compiler spec does not know about. Both are documented by ND and both are verified in the
binary.

---

## The two problems

### 1. The skip return

A PLANC routine that terminates **normally** returns to **RETLINK + 2**, not RETLINK. The two bytes
immediately after every call are the **error path**, and are not executed on success.

> "If the routine terminates normally (not ERRETURN), this address is incremented by two (bytes)
> when returning (also called **skip return**)."
> - `Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md` line 3432

> "an ERRETURN exit will return according to LINK (in stack upper part) while **normal return jumps
> back to LINK + 2**."
> - `Reference-Manuals\ND-60.117.5 EN PLANC Reference Manual.md` line 12390

So the compiled shape is:

```
    bsr.w   SomeRoutine
    jmp     (A5)          ; 4E D5 - 2-byte ERROR slot, NOT executed on success
    <normal return lands here>
```

The epilogue that performs it:

```
    movea.l (SP)+,A6      ; 2C 5F   restore caller frame pointer
    movea.l (SP)+,A2      ; 24 5F   pop the RETURN ADDRESS into A2
    jmp     (0x2,A2)      ; 4E EA 00 02   -> retaddr + 2
```

Verified in the ENCOS image: **400+** of these epilogues, and **zero** routines that return to +0
except the runtime unwind routine `#XRET` itself.

`A5` permanently holds the runtime error vector - `#XRET` (propagate the error up one frame) or
`#ERET` (dispatch to an `ON ROUTINEERROR` handler). In the ENCOS image A5 is loaded in exactly five
places: the three process roots, plus `#XRET` and `#ERET` re-arming it.

**Consequence in Ghidra:** it falls through into the error slot, so the real next instruction is
never disassembled, and it decompiles the epilogue as an indirect *call* -
`(**(code **)(in_stack_00000000 + 2))()` with "Could not recover jumptable" warnings - instead of a
return.

Note: **leaf runtime routines do not do this.** `#IMU`, `#IDV`, `#APPD`, `#REMV` and friends are
hand-written, take register arguments, and end in a plain `rts`. Calls to those must NOT be skipped.
The script distinguishes them automatically.

### 2. The calling convention

PLANC-MC does not pass arguments on A7. The leading argument arrives in **D0** (scalars <= 32 bits)
or **A0** (everything else, as a pointer); further arguments are written by the caller into the
callee's frame from offset `24B` (octal) = 0x14 upward. Results come back in **D0** (integers,
booleans, enums, pointers-as-values) or **A0** (REAL8, POINTER, ARRAY/RECORD/SET addresses).

Frames are **bump-allocated in a separate arena, not on A7, and are never popped**. Only the
(saved A6, return address) pair lives on the machine stack. A caller can therefore still read
secondary results out of a callee's frame after the call returns - a real PLANC-MC idiom, and how
multi-value returns work.

---

## Files

| File | What it does |
|---|---|
| `PlancFixFlow.java` | **Fixes problem 1.** Ghidra script. Flow only - it renames nothing and sets no types. |
| `PlancUndoFixFlow.java` | Reverts `PlancFixFlow`. |
| `planc-68000.cspec-snippet.xml` | **Addresses problem 2.** A `__planc` prototype model to paste into the 68000 compiler spec. |

## Installing and running the script

1. In Ghidra: **Window > Script Manager**, then the "Manage Script Directories" button (top right).
2. Add this folder: `E:\Dev\Ronny\NDInsight\tools\ghidra-planc`
3. Refresh the script list. The scripts appear under category **ND.PLANC**.
4. Open the program, then run **PlancFixFlow.java**.

Set `DRY_RUN = true` at the top of the script to see the report without changing anything. Run it
that way first.

### What it does, in four passes

1. Finds every function whose body contains the `4E EA 00 02` epilogue - these are the skip-return
   callees.
2. For every call whose target is one of those, overrides the fallthrough to skip the 2-byte error
   slot.
3. Marks every `4E EA 00 02` as a `RETURN` via `FlowOverride`.
4. Disassembles the code that was unreachable until now.

### Safety

- **Conservative.** A fallthrough is only overridden when the callee is *proven* to use the +2
  epilogue AND the bytes being skipped look like an error slot (`4E D5`, or a short `BSR` `0x61xx`).
  Anything else is reported, not changed.
- **Reversible.** Run `PlancUndoFixFlow.java`, or in the GUI clear the fallthrough override
  (right-click > Fallthrough > Auto) and set Flow Override back to None.
- **Idempotent.** Re-running changes nothing further.

Read the warnings it prints. A call into a skip-return function whose following bytes are *not* an
error slot is either a mis-identified callee or something genuinely unusual - worth a look either way.

## Installing the calling convention

`planc-68000.cspec-snippet.xml` contains a `<prototype name="__planc">` block. Paste it into the
`<prototype>` list of your Ghidra 68000 compiler spec:

```
<GHIDRA_INSTALL>\Ghidra\Processors\68000\data\languages\68000.cspec
```

Back the file up first, restart Ghidra, then set it per function via
**right-click > Edit Function > Calling Convention > __planc**, or with the
`set_function_prototype` MCP tool.

**Honest limits.** The convention gets the *return value*, the *register preservation* (A5 and A6
unaffected) and the *leading argument* right. It cannot express "second and subsequent arguments
live in the callee's frame at 0x14 upward", because Ghidra's prototype model assumes a
stack-pointer-relative argument area and PLANC frames are not on A7. Those arguments stay manual.

---

## Reading the decompiler output

Even after both fixes, two idioms remain. They are not errors - learn to read them:

| You see | It means |
|---|---|
| `piVar1[n]` where `piVar1 = (int *)*unaff_A6` | PLANC frame slot at byte offset `n*4` from A6. `[5]` = `(0x14,A6)` = first parameter or first local. |
| `piVar1[2] = (int)&stack0x...` | the prologue parking SP at frame offset `0x8` |
| `*piVar1 = (int)(piVar1 + 7)` | the prologue publishing its own next-free cursor at frame offset 0; the `7` encodes the frame size (7*4 = 0x1C) |
| `jmp (A5)` reached in the listing | error unwind to `#XRET`/`#ERET`, not a jump table and not a coroutine yield |

### Frame layout (A6-relative, from ND-820026.1 Figure 8, cross-checked against the binary)

| Offset | Field | Notes |
|---|---|---|
| `+0x00` | `STP` | next-free cursor; the prologue's `movea.l (A6),A6` follows it |
| `+0x04` | reserved / frame limit | overflow guard written by the frame allocator |
| `+0x08` | `SMAX` / saved SP | read by `#XRET` and by hand-written interrupt epilogues |
| `+0x0C` | `SYST` | PLANC runtime use |
| `+0x10` | | (see caveat below) |
| `+0x14` | `ERRCODE` / first param | see caveat |
| `+0x14` onward | parameters then locals | packed by declared size |
| `@A7+0` | `PREV` | previous A6 |
| `@A7+4` | `RETLINK` | return address; normal return goes to +2 |

**Caveat, unresolved:** the two ND manuals disagree about slots `0B` and `4B`. `ND-820026.1`
(valid from compiler version H) says `STP` then `Unused`; `ND-60.117.5` (version G era) says
`PREVB` then `STP`. Slots `10B`/`14B`/`20B`/`24B` agree in both. The ENCOS binary matches the
ND-820026 reading. ND-820026.1:5792 documents a deliberate calling-sequence change between
versions G and H (`OPTION 2`), which is the likely cause.

---

## Sources

- `Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md` - Figure 8 (ordinary
  frame), Figure 9 (NATIVE frame), the skip-return statement at 3432, `#XRET`/A5 at 3436, SPECIAL /
  NATIVE / exception-handler rules at 5804 and 5846. **The citation of record** - the `-1c-`
  revision lost the `<-- A6` / `<-- A7` figure annotations.
- `Reference-Manuals\ND-60.117.5 EN PLANC Reference Manual.md` - appendix 0.4 "Interfacing with
  PLANC on the MC68000": frame table, out-value registers, the LINK+2 rule.
- `Reference-Manuals\ND-20034-1-EN ND-Specific Programming & Advanced PLANC.md` - section 4.4 stacks,
  4.5 parameter transfer (arrays as {virtual origo, lower, upper} descriptors, records as pointers).
  Section 4.5.1 is **ND-500 only** - do not apply it to MC68000.
- `Installation\Communication\Ethernet\x\stripped\docs\ENCOS-FIRMWARE-SYMBOL-TABLE-2026-07-26.md` -
  the vendor symbol table, including the PLANC runtime routines `#XRET`, `#ERET`, `#APPD`, `#REMV`,
  `#IMU`, `#IDV`.

**Do not** apply the ND-500 or ND-100 PLANC conventions to MC68000 code. All three differ. ND-500
signals errors with a status bit (`IF K RET` after the call), ND-100 uses a skip return like the
68000 but via `5LEAV`/`5ERET`, and only the MC68000 uses the A5/`#XRET` vector.
