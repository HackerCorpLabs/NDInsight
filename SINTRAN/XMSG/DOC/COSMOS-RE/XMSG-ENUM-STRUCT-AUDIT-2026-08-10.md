# XMSG / Ethernet enum + structure audit - two C# codebases vs the ND symbol files

**Date:** 2026-08-10
**Author:** static reading only (no machine booted, no Ghidra)
**Question:** does the C# mirror ND's authoritative XMSG symbol files, once and without drift?

## Sources read (all VERIFIED by direct read this session)

Authoritative ND source (values are OCTAL only when the file is in `@OCT`; `XMSG-VALUES-M.SYMB`
switches to `@DEC` at the top, so its numbers are DECIMAL - the file says so on line 11:
`% =*=*=*= Warning: This file is now in DECIMAL to keep PLANC happy! =*=*=*=`):

- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-VALUES-M.SYMB` - functions, option bits, driver
  functions, message types, user errors, XROUT services + sub-services, XROUT errors, XMFIDO
  statuses, crash codes.
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-PL-VALUES-M.INCL` - the PLANC include (the JSON's real source).
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\xmsg-constants.json` - the project's machine-readable copy.
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\XMSG-POFTABS-L03.SYMB`,
  `...\XMSG-SYSTABS-L03.SYMB`, `...\XMSG-SYS-DEF-L.SYMB` - kernel table (structure) definitions.

C# codebase 1 (NDInsight, generated + curated):
`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\Xmsg.Protocol\Enums\*.cs`,
generator `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\tools\gen-xmsg-enums.py`.

C# codebase 2 (RetroCore, hand-maintained):
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Sintran\MON_200_XMSG.cs`,
`...\NDBUS\Xmsg\*.cs`, `...\NDBUS\EthernetII\*.cs`.

---

## 1. DRIFT TABLE - RetroCore vs the authoritative source

Comparison base: `XMSG-VALUES-M.SYMB` / `xmsg-constants.json` / the NDInsight `Xmsg.Protocol` enums
(these three agree with each other except where noted in section 6).

### Function codes (RetroCore `XmsgFunction`, MON_200_XMSG.cs:34-201)

VERIFIED: every function code `XFDUM=0` .. `XFGSM=47` matches the ND source name, value AND comment,
one-for-one. RetroCore additionally defines `X5FUN=48` (MON_200_XMSG.cs:200), the ND end-marker
`SYMBOL X5FUN=48 % == END MARKER ==`. The NDInsight `XmsgFunction` OMITS `X5FUN` (its last member is
`XFGSM=47`, XmsgFunction.cs). This is the only function-code difference between the two C# copies, and
it is harmless (48 is a fence-post, not a callable function).

**No wrong values, no missing callable functions in either C# function enum.**

### T-reg OPTION BITS (RetroCore `XmsgOptions` [Flags], MON_200_XMSG.cs:207-269)

VERIFIED: every active option `SYMBOL` in `XMSG-VALUES-M.SYMB:70-99` is present, as a proper
`[Flags]` member with `1 << bitnumber` matching the .SYMB bit number, in BOTH codebases:

| Symbol | .SYMB bit | RetroCore | NDInsight `XmsgOption` |
|--------|-----------|-----------|------------------------|
| XFWTF | 15 | `1<<15` OK | `1<<15` OK |
| XFWAK | 14 | `1<<14` OK | `1<<14` OK |
| XFPON | 13 | `1<<13` OK | `1<<13` OK |
| XFHIP | 13 | `1<<13` OK | `1<<13` OK |
| XFRRO | 13 | `1<<13` OK | `1<<13` OK |
| XFEXC | 13 | `1<<13` OK | `1<<13` OK |
| XFRES | 12 | `1<<12` OK | `1<<12` OK |
| XFRMR | 12 | `1<<12` OK | `1<<12` OK |
| XFBNC | 12 | `1<<12` OK | `1<<12` OK |
| XFFWD | 11 | `1<<11` OK | `1<<11` OK |
| XFROU | 10 | `1<<10` OK | `1<<10` OK |
| XFRDI | 10 | `1<<10` OK | `1<<10` OK |
| XFSEC |  9 | `1<<9`  OK | `1<<9`  OK |
| XFTCM |  8 | `1<<8`  OK | `1<<8`  OK |
| XFSYS |  7 | `1<<7`  OK | `1<<7`  OK |

**No option-bit drift. Both are correct [Flags] enums with 1<<n and the ND comments.** The commented-out
pseudo-symbols in the .SYMB (`XFPRM=13`, `XFOPS=12`, `XFWOK=13`, `XFUSG=13`) are obsolete/not-implemented
and are correctly absent from both.

### XROUT services (RetroCore `XroutService`, MON_200_XMSG.cs:470-570)

**DRIFT - RetroCore is MISSING members that exist in the ND source and in NDInsight:**

| ND symbol | value | in .SYMB / NDInsight | in RetroCore |
|-----------|-------|----------------------|--------------|
| XSRME | 70 | present (NDInsight, from the L Communication Guide) | **MISSING** |
| XSCMG | 72 | present (NDInsight, from the L Communication Guide) | **MISSING** |
| XSDMC (alias of 74) | 74 | present | **MISSING** (only XSDSY=74 defined) |
| XSGMC (alias of 75) | 75 | present | **MISSING** (only XSGSY=75 defined) |
| XSMAX (=XSGSG) | 96 | present | **MISSING** |

Note: `XSRME=70` and `XSCMG=72` are NOT in `XMSG-VALUES-M.SYMB` itself (which jumps 69->71->73);
NDInsight added them from `ND-60.134.2 EN SINTRAN III Communication Guide` (documented inline in
XroutService.cs). So RetroCore matches the raw M.SYMB but lags NDInsight's enriched set.
The aliases and `XSMAX` are cosmetic. **All shared XROUT service VALUES that both define agree.**

### XROUT errors (RetroCore `XroutError`, MON_200_XMSG.cs:652-751)

**DRIFT - one missing member:**

| ND symbol | value | source | RetroCore |
|-----------|-------|--------|-----------|
| XRILX | 55 | present in `XMSG-PL-VALUES-L.INCL` + NDInsight (XroutError.cs) | **MISSING** (last is XRIRR=54) |

`XRXXX=16960`, `XRSOK=0` .. `XRIRR=54` all match name+value in both. `XRILX=55` is an L-version
addition; the M .SYMB stops at `XRIRR=54`, so RetroCore matches raw M but lags NDInsight.

### Everything else - VERIFIED matching, no drift

- Driver functions `XDINF=1`..`XDGER=6` (RetroCore `DrxmsgFunction`) - match.
- Message types `XMTNO=1`..`XMTPS=6` (RetroCore `XmsgXFRCVReturnType`) - match.
- User errors `XKXXX=16896`, `XENOT=-1`..`XECRA=-63` (RetroCore `XmsgErrorCode`) - match, all 44 codes.
- XSSCI sub-services `XSDAR=1`..`XSGAR=7` (RetroCore `XroutSubService`) - match.
- XSGAT sub-services `XSGXV=1`..`XSGLO=5` (RetroCore `XsgatSubService`) - match.
- XSDAT sub-services `XSDFR=1`..`XSSRT=6` (RetroCore `XsdatSubService`) - match.
- XMFIDO statuses `XEFOK=0`, `X412B`base .. `XEFRU` (RetroCore `XmfidoStatus`) - match.
- Crash codes `XXEIE=1`..`XXTBM=57` (RetroCore `XmsgCrashCode`) - match, INCLUDING the `XXN33=44`
  quirk (see section 6).

---

## 2. FLAGS coverage

**Every option bit in the ND source is present as a `[Flags]` member with the ND comment in BOTH
codebases.** See the table in section 1. There is NO option bit that exists in the ND source but is
missing from either C# enum. Both enums are complete on flags.

Style compliance: both use `1 << n`. NDInsight `XmsgOption` is `ushort` and carries a 3-part
`/// <summary>` + `/// <remarks>` with the mask and the "bit N of the MON 200B T-register high byte"
note. RetroCore `XmsgOptions` carries the ND meaning per member but no per-member on-wire mask remark
(cosmetic gap only).

Two extra flag helpers exist ONLY in NDInsight and have NO RetroCore counterpart:

- `XmsgSendOptions` (byte) - the T-reg high byte as it rides in the wire "role" octet, with the
  hard-won XFHIP-vs-XFRRO bit-5 disambiguation (`IsHighPriority` / `IsRemoteXrout`). This is a
  wire-decode helper, not a raw ND symbol; RetroCore does not decode the wire role byte this way.
- `XmsgFrameFlags` (byte) - the sub-header frame-flags byte, partially decoded from captures.

These are not "missing" from RetroCore in a drift sense - RetroCore drives the kernel through the
MON 200 mailbox and never parses the on-wire role/flags octets, so it has no need for them yet.

---

## 3. STRUCTURES - what POFTABS / SYSTABS define, and what C# has

`XMSG-POFTABS-L03.SYMB` (718 lines) and `XMSG-SYSTABS-L03.SYMB` (62 lines) are PLANC `DISP`
(displacement) record layouts for XMSG's INTERNAL KERNEL RAM tables - not wire formats. VERIFIED
structures:

**POFTABS** (tables common to kernel code + the XMSG-COMMAND program):
- `5S3BA` - SINTRAN-variables basefield (pointers XMSGA, error/fatal pointers, PIT descriptors).
- `5BASE` - the XMSG basefield proper (message descriptor area; `XM5BS`..`XM5BE` bracket the message
  descriptor; note "the whole basefield upwards from XCRAR gets zeroed at start").
- `5PLEN` - **PORT element** (the structure of one port).
- `5MLEN` - **MESSAGE table element** (`XMTHD` must be the first word of a message descriptor).
- `XS5LN` - **SYSTEM-INFO and ROUTING element** (per remote system).
- `5LLEN` - **LINK table** element (XL-block).
- `5DLEN` - **DATAGRAM / XD-block** element (frame-buffer descriptors).
- function-descriptor block (one per XMSG function) + a bit-translation table.
- `XPASW` version/password word; `XVERS`/`XREVI` version+revision; `CMCRM` config-mask bit.

**SYSTABS** (tables common to RESIDENT + kernel) - the file is partly OCR-garbled but the readable
part defines the **XT-BLOCK** (`5XLEN`, the per-task descriptor): chaining pointers, status word,
CT-address, port chain head, message size, paging status, register save area (PA/ZA/MA...),
user-buffer address, system-bank address, answer/return counts, multicall pointer, memory-allowance
words (MMH/MML/MMX), bit-map words (BM0..BM3). `XMSG-SYS-DEF-L.SYMB` (50 lines) holds system-definition
constants.

**Are any represented as C# today?** NO - not as the kernel table layouts. What EXISTS in C# are wire
and API model types, which are a different thing:
- NDInsight `Xmsg.Protocol\Wire\` - `SintranHeader`, `XmsgSubHeader`, `XmsgFrame` (on-wire, not kernel RAM).
- NDInsight `Xmsg.Api\Model\` - `XmsgMagicNumber`, `XmsgPortNumber`, `XmsgPortStatus`,
  `XmsgMessageStatus`, `XmsgMessageBuffer`, `XmsgLinkInformation` (API-shaped, not the ND table layout).
- RetroCore `NDBUS\Xmsg\MboxhTransport.cs` - hardcodes mailbox element offsets `NXMSG/NXFNC/NXPAR/
  NXXTB/NXLB/NXPNU` and param offsets `P_FUNC/P_A/P_D/P_X/P_UADDR` as `private const uint` (these are
  the MBOXH mailbox layout, again not the POFTABS/SYSTABS kernel tables).

**MISSING and would be useful as shared helpers:** none of the POFTABS/SYSTABS kernel tables are
needed for wire/ethernet work - they are internals of a real XMSG kernel. They become useful ONLY if
someone emulates the XMSG kernel's own memory (port table, message table, XT-block) rather than
speaking to it. If that day comes, `PortElement (5PLEN)`, `MessageElement (5MLEN)`,
`SystemRoutingElement (XS5LN)`, `LinkTableElement (5LLEN)`, `DatagramElement (5DLEN)` and `XtBlock
(5XLEN)` would be the shared struct set to carve. **Recommendation: do NOT build them speculatively**
(RULE #0 / "value scans are noise") - the SYSTABS file is OCR-damaged and would need a clean re-read
first. Mark this as a future item, not a gap.

---

## 4. DUPLICATION MAP

Within RetroCore there is NO duplication: `NDBUS\Xmsg\XmsgClient.cs` and `IXmsgTransport.cs`
explicitly `using Emulated.HW.ND.CPU.ND100.Sintran;` and REUSE the MON_200_XMSG.cs enums
(comments at XmsgClient.cs:18-21 and IXmsgTransport.cs:27 say "reused, not duplicated"). Good.

The duplication is **BETWEEN the two codebases** - the same ND constant family is defined twice, once
generated (NDInsight) and once hand-typed (RetroCore). Twelve families are duplicated:

| ND concept | NDInsight (authoritative, generated) | RetroCore hand copy | Values agree? |
|------------|--------------------------------------|---------------------|---------------|
| Function codes | `XmsgFunction` (gen) | `XmsgFunction` | YES (RC adds X5FUN=48) |
| T-reg option bits | `XmsgOption` (gen) | `XmsgOptions` | YES (name differs by 's') |
| Driver functions | `XmsgDriverFunction` (gen) | `DrxmsgFunction` | YES |
| Message types | `XmsgMessageType` (gen) | `XmsgXFRCVReturnType` | YES |
| User errors | `XmsgError` (gen) | `XmsgErrorCode` | YES |
| XROUT services | `XroutService` (gen) | `XroutService` | RC missing XSRME/XSCMG/aliases/XSMAX |
| XROUT errors | `XroutError` (gen) | `XroutError` | RC missing XRILX=55 |
| XSSCI sub-services | `XroutSetCrashInfoSubservice` (gen) | `XroutSubService` | YES |
| XSGAT sub-services | `XroutGetAttributeSubservice` (gen) | `XsgatSubService` | YES |
| XSDAT sub-services | `XroutDefineAttributeSubservice` (gen) | `XsdatSubService` | YES |
| XMFIDO statuses | `XmfidoStatus` (gen) | `XmfidoStatus` | YES |
| Crash codes | `XmsgCrashCode` (gen) | `XmsgCrashCode` | YES |

**Authoritative side:** NDInsight, because those files are regenerated from `xmsg-constants.json`
(each carries a `// <auto-generated>` header and "Do not edit by hand"). **Drift-risk side:**
RetroCore's `MON_200_XMSG.cs`, which is hand-typed. It is already 5 members behind on two enums,
which is exactly the drift this audit predicts will keep happening.

NDInsight-only, no RetroCore twin (not duplication, just wider scope): `XmsgSendOptions`,
`XmsgFrameFlags`, `XmcsmService`, `XmsgOptionConversion`, `XroutConnectionType`, plus wire types.

---

## 5. CONSOLIDATION PLAN - one source of truth

Goal: RetroCore stops hand-maintaining a second copy and consumes the SAME generated enums, without
breaking the generator's "regenerate from JSON" model and without a NuGet/project-reference coupling
between two separate repositories.

**Obstacles (real, must be handled):**
1. Different namespaces - `NDInsight.Sintran.Xmsg` vs `Emulated.HW.ND.CPU.ND100.Sintran`.
2. Different member names - `XmsgOptions`/`XmsgOption`, `DrxmsgFunction`/`XmsgDriverFunction`,
   `XmsgXFRCVReturnType`/`XmsgMessageType`, `XmsgErrorCode`/`XmsgError`, and the three sub-service
   enums renamed. RetroCore code refers to these names all over MON_200_XMSG.cs and NDBUS.
3. RetroCore includes `X5FUN=48` (end marker) that NDInsight omits.
4. The two repos are separate git repos; a hard project reference across `E:\Dev\Ronny\NDInsight`
   and `E:\Dev\Repos\Ronny\RetroCore` is fragile.
5. House rules: no LINQ, `[Flags]` uses `1<<n`, each member needs a 3-line `/// <summary>`, no
   FluentAssertions - the current generator already honours all of these, so the fix must stay inside it.

**Recommended approach: teach the generator to emit a second file set, shared as source (not binary).**
The JSON stays the single source of truth; the generator gains a second output profile so BOTH repos
get identical, generated code. This keeps RULE #0 (one verified source) and the regenerate-from-JSON
model intact.

Step by step:

1. **Freeze the ND source of truth as `xmsg-constants.json`.** First add the currently-missing symbols
   to it so the generated output is a superset that satisfies RetroCore too: `XSRME=70`, `XSCMG=72`,
   `XSMAX=96`, the `XSDMC`/`XSGMC` aliases, `XRILX=55`, and the `X5FUN=48` end marker (mark each with
   its `source` tag - the file already supports `"source":"L"` provenance and warns regenerators to
   preserve them). No RetroCore code changes yet.

2. **Add a `--profile retrocore` mode to `gen-xmsg-enums.py`.** Same JSON in, but: emit into the
   RetroCore namespace, and apply a small name-map table (`XmsgOption->XmsgOptions`,
   `XmsgDriverFunction->DrxmsgFunction`, `XmsgMessageType->XmsgXFRCVReturnType`,
   `XmsgError->XmsgErrorCode`, `XroutSetCrashInfoSubservice->XroutSubService`,
   `XroutGetAttributeSubservice->XsgatSubService`, `XroutDefineAttributeSubservice->XsdatSubService`).
   The name-map lives in the generator, so the JSON is untouched and the NDInsight profile is
   unaffected. Keep the exact same `[Flags]` + `1<<n` + `/// <summary>` emission the generator already
   does (verified in gen-xmsg-enums.py:104-138), so house rules are met for free.

3. **Point the generator's RetroCore output at the RetroCore path** (a config/CLI arg giving the
   `E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Sintran\` directory), writing e.g.
   `MON_200_XMSG.Enums.g.cs`. Delete the hand-typed enum block from `MON_200_XMSG.cs` and leave only
   the dispatch/behaviour code there. The generated file carries the same `// <auto-generated> Do not
   edit by hand` header, so nobody re-types it.

4. **Build RetroCore.** Because the generated names equal the old ones (via the name-map) and the new
   file adds the previously-missing members, existing RetroCore call sites compile unchanged. Fix the
   handful of spots that assumed `X5FUN` was absent, if any (there are none today - RC already has it).

5. **Add a lockstep test in each repo.** NDInsight already has `EnumValueTests` / `WireByteEnumTests`;
   add a matching RetroCore test that asserts each generated value equals the JSON. That turns any
   future JSON edit into a red test on both sides instead of silent drift.

6. **Document the flow** in the XMSG README: JSON is the source, `gen-xmsg-enums.py` emits both
   profiles, neither `.cs` is hand-edited. One command regenerates both repos.

**Cheaper fallback if cross-repo generation is unwanted:** keep two files but add a CI/pre-commit check
(a tiny script) that diffs the RetroCore enum values against `xmsg-constants.json` and fails on any
mismatch or missing member. This does not remove the duplication but it removes the DRIFT, which is the
actual harm. It is the smallest possible fix and could ship today.

---

## 6. Note on a genuine ND-internal source discrepancy (not a C# bug)

Crash code 44: `XMSG-VALUES-M.SYMB` says `SYMBOL XXN76=44 % ... seg 76`, but `XMSG-PL-VALUES-M.INCL`
(line 370, the generator's actual source) says `CONSTANT XXN33=44 % ... seg 33`. BOTH C# codebases use
`XXN33=44`, i.e. both faithfully follow the `.INCL`. This is a disagreement WITHIN ND's own M-version
files (`.SYMB` vs `.INCL`), not a mirroring defect. Flagging it because "VERIFIED must be falsifiable":
if a future carve of the running kernel resolves which label is real, update the JSON accordingly.
