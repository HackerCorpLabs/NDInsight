# The *XFTRA request, one input at a time (2026-07-28)

The `*XFTRA` letter was first captured on the L pack, where the COSMOS File User is blocked by the
revision-F gate and only the transfer client runs. This is the same command re-driven on the
**SINTRAN K** image, where the whole COSMOS file machinery is live, and driven **six times varying
exactly one input per run** so the letter can be read field by field instead of guessed at.

Companion to
[XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md)
(the first capture) and
[XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md](XMSG-FA-SERVER-REQUEST-CAPTURED-2026-07-28.md)
(the other file server, which does everything differently).

**Method.** RetroCore MON 200 trace, harness test `Boot_ProbeFileTransfer_OnK`. All six transfers
fail - D101 and D102 have no peer - but the request leaving the machine is the deliverable.

---

## 1. The baseline [VERIFIED]

`TRANSFER-FILE`, To `D102(SYSTEM)."XMSG-COPY:BATC"`, From `(SYSTEM)SINTRAN:DATA`:

```
XFWRI  NBYTES=62  displacement 0
  01 41 00 3A                        serial 0x01, service 0x41 = 65 = XSLET, length 58
  FF 06 "*XFTRA"                     p1   server name
  FE 04 "D102"                       p2   remote system name
  F4 06 "SYSTEM"                     p12  remote user name
  0D 02 0000                         p13  integer 0
  F8 10 "\"XMSG-COPY:BATC\""         p8   destination file spec, quotes included
  F7 04 "SYMB"                       p9   constant - see below
  0A 02 0400                         p10  integer 1024
  0B 02 0002                         p11  integer 2
```

**This is byte-for-byte the request captured on L.** The tagged-parameter form is therefore the
protocol itself and not an artefact of a half-installed product on the L pack - a question the
single earlier capture could not settle.

## 2. What each variation moved [VERIFIED]

| Run | Changed input | Effect on the request |
|---|---|---|
| baseline | - | as above, length 0x3A |
| vary source file | From `(SYSTEM)RONNY:TXT` | **no change whatsoever** |
| vary destination | To `..."OTHER-COPY:SYMB"` | p8 becomes `F8 11 "\"OTHER-COPY:SYMB\"" 00`, length 0x3C |
| vary remote user | To `D102(RT)...` | p12 becomes `F4 02 "RT"`, length 0x36 |
| vary remote system | To `D101(SYSTEM)...` | p2 becomes `FE 04 "D101"` |
| repeat of baseline | nothing | **byte-identical to the baseline** |

Four facts fall straight out of that table.

**The source file is not in the request.** `SINTRAN:DATA` and `RONNY:TXT` produce identical bytes.
The opening letter names only where the data is going; the sender knows what it is sending and the
receiver does not need to be told. Anything reading this letter to learn the source will find
nothing, because nothing is there.

**Parameter 2 is the remote system name and parameter 12 is the remote user name,** each proven by
moving alone. Parameter 13 is the password, sent as integer 0 when empty.

**Strings are word-aligned with a pad byte.** The 17-character destination spec is declared
`F8 11` and followed by one `00`, and the message length grows by **two**, from 0x3A to 0x3C, not
by one. Every string seen until now happened to be even-length, so this is the first proof of the
padding rule.

**There is no per-request counter in this letter.** The repeat of the baseline is identical to the
baseline, including the serial byte. That is worth stating plainly because the *other* file server
behaves the opposite way: a trailer byte in the `*FA-SERVER` request stepped 06 -> 08 -> 0A across
three requests. Two servers in one product, two conventions, again.

## 3. What parameter 9 is not [VERIFIED negative]

`F7 04 "SYMB"` was constant in all six runs and in the original L capture. It survived a
destination typed `:BATC` and a destination typed `:SYMB`, and a source typed `:DATA` and a source
typed `:TXT`. **So it is not the file type of either file**, which was the obvious reading.

Meaning UNKNOWN. Likewise `p10 = 1024` and `p11 = 2` never moved. Candidates worth a later run: a
transfer mode, a default type for a destination created without one, or a block size and count -
but nothing here decides between them.

## 4. Bonus: XSDRN observed [VERIFIED]

`DEF-REMOTE,,D101 101` in the XMSG command program emits:

```
01 49 00 0A  FF 04 "D101"  02 02 0065
```

Service 0x49 = 73 = `XSDRN`, define remote name, with the name as string parameter 1 and the system
number 101 as integer parameter 2. This corroborates the service number carved from ENNS0 in
[XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md](XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md)
with live traffic.

## 5. The refusal [VERIFIED]

Every run came back as before: the whole body returned unchanged with the service byte overwritten,
`01 0C 00 3A ...`, `0x0C` = 12 = `XRNRO`, no access to remote system.

## 6. Trap for the next run

The first attempt at this experiment lost **four of its five requests** and nothing warned that it
had. `TRANSFER-FILE` opens the SOURCE first, and a missing source fails locally with a SINTRAN file
system error before anything reaches XMSG - so the console shows an error either way, and only the
MON 200 trace reveals that no request was ever built. The L pack's `XMSG-STARTEX:BATC` does not
exist on K. **Pick sources from a live `LIST-FILES` on the pack under test.**

---

## Provenance

Image `D:\BIGDISK0-K.IMG` (SINTRAN III VSX/500 K, XMSG L03, COSMOS Basic E04, File Transfer E02).
Test `Boot_ProbeFileTransfer_OnK` in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`.
Console transcript `cosmos-file-transfer-k-console.txt` and Device log
`ethii-controller-log.XFTRA-K-2026-07-28.txt` under the RetroCore scratch directory.
