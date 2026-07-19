# MON 60B subfunction carve - COMPLETE (2026-07-15)

**All MON 60B / N500M subfunctions carved: 47 folders**, each with `README.md` + `.pseudo.c` +
`.npl` (verbatim source from `5P-P2-MON60.NPL`). `.ASM` per folder is PENDING the bank-2 5IFUNC
address (see `60B-5IFUNC-dispatch-table.md`); `037B-ICSLOAD` additionally carries the located
`050-S3I5PIT` dispatcher-head `.ASM`.

## What "complete" means here
- The **dispatch chain** (MON 60B -> ENT14 -> GOTAB=MFELL -> CALLP -> MCTAB=N500M=030416B) is
  byte-verified in L07.
- The **worker overlay** (5PIT = `050-S3I5PIT`) is confirmed via the L release-doc PIT layout.
- The **5IFUNC map** (code -> handler, 128 entries) is 3-way cross-verified (L07 bytes + caller decode
  + manual). See `60B-5IFUNC-dispatch-table.md`.
- Every distinct **handler body** is documented from the authoritative worker source
  (`5P-P2-MON60.NPL`) with an emulator `.pseudo.c` and the **verbatim NPL** excerpt.
- The **5NOPAR common path** and the **error handlers** are carved as their own folders.

## Folders (47)
Param-copy / file: `006B-ISEGLOAD`, `007B-ICOPF` (007/046/047/067/071/130/131), `013B-ICONNFI`,
`037B-ICSLOAD`, `127B-IDFSYDOM` (127/161), `134B-IPLDEB`, `160B-IN5SEGLOAD`.
Memory/registers/CS: `004B-MEMWRITE` (004/005/033), `011B-IWRGS` (011/056), `024B-IWCNTS` (024/157),
`110B-IWPHSG`.
Reserve/release/place: `043B-ISRES`, `044B-ISREL`, `055B-ISPLACE`, `061B-IMRESSPES`, `123B-IMRELSPES`.
Histogram: `062B-IDEFHIST`, `063B-ISTAHIST`, `064B-ISTOHIST`, `065B-IREAHIST`, `066B-IRELHIST`.
Process/user/msg: `074B-ISPRNM` (074/136), `075B-ITSTUSER`, `076B-ITOSWP`, `077B-IRMESS`,
`100B-FLAGS` (100/101), `102B-IFORGET`, `117B-IPRABORT` (117/122), `135B-IABLOG`, `143B-IMO5RT`,
`144B-ICHACPU`.
System/log/domain/queue: `103B-IRSYSP`, `104B-IWSYSP`, `111B-ISTAPRLOG`, `112B-ISTOLOG`,
`113B-IPRILOG`, `114B-IRELLOG`, `115B-ISTLAPR`, `124B-ISTAMLOG`, `125B-IPRIMLOG`, `126B-ISTOMLOG`,
`133B-ILI5EXQ` (133/150), `145B-ISSTDOM`, `154B-IDBUGSW`, `173B-ICPUSTAT`.
Common/errors: `5NOPAR-COMMON` (hand-off to the ND-500 system monitor), `ERRORS`.

## Server side - DONE (2026-07-15)
**`FPT2ENTRY` / the ND-500 SYSTEM MONITOR** (the `5NOPAR` hand-off target - the "more than MON 60"
code that builds the 5MPM message and drives the ND-500) is now carved:
[`../../ND500-SYSTEM-MONITOR/`](../../ND500-SYSTEM-MONITOR/README.md). It contains the `FUNCS`
dispatch table (twin of `5IFUNC`), the 3022 IOX driver + register map, the control-store gate,
the 5MPM message + `ACT50` activation, the level-12 return path, and ALL ~60 FUNCS operation bodies.

## Remaining follow-ups (NOT part of "all 60B carved")
1. **Locate the bank-2 5IFUNC table** in L07 -> add a byte-verified `.ASM` per handler.
2. Reconcile with the other session's `nd-500-mon:prog` (caller) + swapper decode when it lands.
