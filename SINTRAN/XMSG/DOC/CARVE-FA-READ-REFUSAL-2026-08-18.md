# The FA read driver ignores a refusal it is already told about

**Measured on D100, 2026-08-18.** Two runs, one variable changed: the file exists, or it does not.

## The short version

A `--pull` of a file that does not exist **hung**. The reason is not that the machine stays silent
and not that the refusal is undecodable. D100 refuses correctly, on the very first step, with an
error number we already have documented and verified.

**`FaReadDriver` never looks at selector 1.** So it treats a refusal as an ordinary reply, climbs
the rest of the ladder against a file that was never opened, collects three more refusals, and then
waits for a data block that is never coming.

## The measurement

Same ladder both times, up to and including `ReadFile`:

| step | file exists | file missing |
|---|---|---|
| `SendConnectLetter` | sent | sent |
| `ReserveFileEntry` | sent | sent |
| `OpenFile` | sent | sent |
| `SetBlockSize` | sent | sent |
| `SiiiSpecial` | sent | sent |
| `ReadFile` | sent **53 times**, then `CloseFile` | sent **once**, then nothing for ever |

Every refused reply, in order, from the failing run:

```
OpenFile      07F0 0002 81 00 9169 92 0005 92 0002  F2 0001  A2 002E  F2 00FF 00
SetBlockSize  07F0 0002 82 00 9169 92 0007 92 0003  F2 0001  A2 4104  F2 00FF 00
SiiiSpecial   07F0 0002 83 00 9169 92 000C 92 0004  F2 0001  A2 4104  F2 00FF 00
ReadFile      07F0 0002 84 00 9169 92 0008 92 0005  F2 0001  A2 4104  F2 00FF 00
```

and the successful `ReadFile` reply for comparison - **no selector 1 at all**:

```
ReadFile      07F0 0002 84 00 914C 92 0008 92 0005           F2 00FF 8C
```

## What each part is, and how confident

The body is a QFORM selector/value stream, which we already implement - `0xF2` is a selector with a
two-byte field number, `0xA2` a typed two-byte integer (`Xmsg.Protocol/Qform/QformTagByte.cs`).

 - **`F2 0001` marks a refusal, and success omits selector 1 entirely.** Already documented and
   verified in `FaServerStatus`, from the 2026-08-04 captures. This carve is an independent
   confirmation of it from the other direction.
 - **`A2 002E` = 46 = "NO SUCH FILE NAME".** VERIFIED - it matches the number `FaServerStatus`
   already records for exactly this case, from `capture-open-error.txt`.
 - **`81 00`, `82 00`, `83 00`, `84 00`** step with the ladder, so this counts the operation.
   INFERRED from the ordering; not confirmed against a specification.
 - **`9169`** is CONSTANT across all four replies within a run and DIFFERS between runs
   (`4C`, `69`, `86`, `A3`). So it identifies the session, not the message. INFERRED.
 - **`A2 4104` is STILL NOT EXPLAINED, but four explanations are now ELIMINATED.** What IS measured
   is that it appears only on operations issued *after* a refused `OpenFile` - that is, against a
   file that was never opened - and that it is **byte-identical for two different missing filenames**
   (`NOSUCH:LIST` and `ZZTOP:DATA`), so it is a fixed code and not derived from the name. A
   plausible reading is "the file is not open", and that remains a guess.

   **What it is NOT** (2026-08-18, each with its source):

   | Ruled out | Why |
   |---|---|
   | a TAD-converted XMSG error | The CNVERR rule is `SINTRAN code = 41000B OR (-XMSG code)` (NPL source, `POF:625-627`), which can only produce values **at or above 41000B = 16896**. 16644 is below the base. |
   | an XROUT error | `XRXXX = 041100B` = **16960** (`NPL-SOURCE/SYMBOLS/L07/SYMBOL-1-LIST.SYMB.TXT`), and the library form is base+n - `XMXRUNN=16962`, `XMXRDDF=16963`. Also above 16644. |
   | a SINTRAN file-system error number | Every FA status measured or inferred is a small SINTRAN error - 0, 46, 48, 97, 129, 197, 211 (`DOC/protocols/fa-qform.json`). 16644 is outside that space, and appears nowhere in the Reference-Manuals. |
   | a third COSMOS error base | There are only two in the shipped definitions: `41000B` (XMSG) and `41100B` (XROUT). Both are above 16644. Grepped from the compiled `XMP-B02:DEFS` in `listings/CHATSV.LIST.txt`. |

   **The one structural fact worth having:** 16644 is `040404B`, which is `0x4000 | 0x0100 | 0x0004`
   - bits 14, 8 and 2. **Three bits, not an ordinal.** That fits a status *word* rather than an
   error *number*, and it fits the measurement that the value never varies with the filename.

   **The binary does not settle it either.** `cos-fa-serv-e04.prog` contains the 16-bit pattern
   five times (`ram:169b`, `1839`, `5163`, `5260`, `545c`), but every one is in a region Ghidra has
   not analysed as code and **none has a single cross-reference**, so no code path is confirmed.
   The next place to look is the reply builders `fa_build_status_reply @ ram:0a41` and
   `fa_status_to_reply_message @ ram:2403`; both decompile into indirect calls through runtime
   thunks, so they need real work rather than a glance.

 - **The operation ladder names are now confirmed from the vendor binary**, not inferred from our
   own naming. `cos-fa-serv-e04.prog` carries them as one string table (`BANK2::8731` onwards):
   `File-entry-disconnect`, `Reserve-file-entry`, `Release-file-entry`, `Change-file-entry-id`,
   `Open-file`, `Close-file`, `Set-block-size`, `Read-file`, `Write-file`, `Create-file`,
   `Delete-file`, `SIII-special`, `Device-function`.

## The controls that make the above worth anything

 - **Two different missing names** produced byte-identical payloads apart from the session byte. So
   nothing in the refusal is name-derived.
 - **The same name twice** produced identical payloads apart from the session byte. So it is stable,
   not noise.
 - **The successful run carries no selector 1 anywhere** - `grep -c "F20001" good.log` is 0 across
   the whole transfer. So selector 1 is not something a healthy read ever contains, and testing for
   it cannot produce a false failure on a good transfer.

## What to change

`FaReadDriver` should read selector 1 out of every reply and, when present, set `Failure` with the
SINTRAN error number. Two things follow:

 - a missing file fails **immediately, on the OpenFile reply**, with "NO SUCH FILE NAME (46)"
   instead of climbing the ladder and hanging;
 - `--transfer-timeout` goes back to being what it was meant to be - a net for the unforeseen -
   rather than the only thing that ends a perfectly well-explained refusal.

The timeout stays. It is what catches the cases nobody has carved yet.

## Reproducing it

```powershell
$r = 'SRC\Xmsg.Live.Runner\bin\Debug\net9.0\Xmsg.Live.Runner.exe'
& $r --self 19999 --originate-from-seed --transfer-timeout 40 `
     --pull 'NOSUCH:LIST' --pull-from 100 --pull-to out.LIST *> bad.log
& $r --self 19999 --originate-from-seed --transfer-timeout 90 `
     --pull 'CHATSV:LIST' --pull-from 100 --pull-to good.LIST *> good.log
```

Then compare `Trailing bytes:` lines carrying `F20001`. The failing run has four; the good run has
none.
