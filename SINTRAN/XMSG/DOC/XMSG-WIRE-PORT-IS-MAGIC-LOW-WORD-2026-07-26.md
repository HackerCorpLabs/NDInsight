# Are the wire port fields XMDPT / XMSPT the magic-number LOW WORD?

Date: 2026-07-26
Question: is `XMDPT` (sub-header abs. offsets 20-21) / `XMSPT` (24-25) exactly
`(port number << 7) | random`, i.e. the low word of MAGNO as carved in
`XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md`?

## VERDICT: CONFIRMED

Two independent, non-circular ties, plus a corpus-wide consistency check. One
residual gap is named at the end.

---

## Evidence 1 - the TAD port-assign carries a full 32-bit MAGNO, and both halves
land verbatim in the wire fields  [VERIFIED]

The TAD `0x07` (7CORS) port-assign trailer is documented as
`07 05 00 00 <node16> <port16>`
(`E:\Dev\Ronny\NDInsight\SINTRAN\TAD\TAD-Message-Formats.md` line 158, line 1389).

That is byte-for-byte the carved MAGNO layout: high word = system number,
low word = the port word. Eight port-assign frames exist in the corpus
(`E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\pcap-decode-report.txt`, raw lines
1018, 1532, 2493, 9054, 9440, 9811, 10856, 11741).

Worked example (report line 2493):

```
2113 000E 0064 0066 0130 0400 D8E4      SINTRAN hdr: dst sys 0x0064=100, src sys 0x0066=102
2100 8640 0064 02AB 0066 0156           sub-hdr: XMDSY=100 XMDPT=0x02AB(683)
                                                 XMSSY=102 XMSPT=0x0156(342, *TADADM)
0400 0000 0018 00
07 05 00 00  0066 04C2                  <- payload magic: system 0x0066=102, port word 0x04C2
1F 03 4C00 0000 0B02 0304 1502 0108 FF00
```

The very next data frames from 102 (report lines 2069, 2080, 2105, 2123, 2141,
2159, 2177 ...) carry `... 0066 04C2 0064 02AB ...` - i.e. **XMSSY = 0x0066 and
XMSPT = 0x04C2, the two halves of the assigned magic used verbatim as the wire
system/port pair.**

All eight assignments behave the same way, and the payload high word always equals
the ASSIGNER's own system number (not the peer's):

| report line | assigner | payload magic | port word | later seen as wire port |
|---|---|---|---|---|
| 1018, 1532 | 102 | `0066 0313` | 0x0313 = 787 | yes |
| 2493 | 102 | `0066 04C2` | 0x04C2 = 1218 | yes |
| 9054, 9811 | 102 | `0066 0313` | 787 | yes |
| 9440 | 102 | `0066 0308` | 0x0308 = 776 | yes |
| 10856 | **100** | `0064 031E` | 0x031E = 798 | yes |
| 11741 | 102 | `0066 0341` | 0x0341 = 833 | yes |

VERIFIED: the wire (XMxSY, XMxPT) pair IS a MAGNO split into its A/D words.

## Evidence 2 - `>> 7` yields the kernel PORT NUMBER, confirmed from a non-wire
source  [VERIFIED]

Live COSMOS `list-servers` on node 100 reports `*TADADM` at **logical port 2** and
`*XM-FIDO` at **logical port 4**
(`XMSG-PROTOCOL.md` lines 850-880; `TAD-Message-Formats.md` line 1382).

`*TADADM`'s observed wire ports are 342 (`0x0156`) and 358 (`0x0166`).
`342 >> 7 = 2`, `358 >> 7 = 2`. The registry number was obtained from an operator
command, not derived from the wire, so this is **not circular**: it independently
fixes the shift at 7 and identifies the high 9 bits as the kernel port number.
The two values differ only in the low 7 bits (86 vs 102) across two boots - exactly
the "random part changes on re-registration" behaviour of `RNMAG`.

## Evidence 3 - corpus-wide decomposition is plausible everywhere  [VERIFIED]

All `endpoints:` lines of `pcap-decode-report.txt` (14458 lines) decomposed with
the 9/7 split. 27 distinct port words, 26 non-zero:

```
   port   hex     port#  random  frames
      0  0x0000     0      0      166   (fixed protocol sink - not a minted magic)
    342  0x0156     2     86       61
    358  0x0166     2    102        5
    567  0x0237     4     55        2
    569  0x0239     4     57        6
    581  0x0245     4     69      114
    636  0x027C     4    124       30
    648  0x0288     5      8       94
    650  0x028A     5     10        6
    657  0x0291     5     17       35
    664  0x0298     5     24       36
    677  0x02A5     5     37        6
    683  0x02AB     5     43      115
    705  0x02C1     5     65      264
    722  0x02D2     5     82       90
    738  0x02E2     5     98        6
    739  0x02E3     5     99       37
    740  0x02E4     5    100       88
    776  0x0308     6      8       31
    787  0x0313     6     19      150
    798  0x031E     6     30       29
    833  0x0341     6     65       53
    845  0x034D     6     77        6
   1049  0x0419     8     25       24
   1175  0x0497     9     23        2
   1218  0x04C2     9     66       44
   1222  0x04C6     9     70        6
```

- Port numbers observed: **2, 4, 5, 6, 8, 9** only. Every one is small and
  plausible; the 9-bit field is nowhere near saturated. **No implausible
  decomposition exists in the corpus** - nothing falsifies the split.
- Port 2 matches `*TADADM`'s registry number (Evidence 2).
- The random field spans 8..124 with no clustering.
- The only `low7 == 0` value is port word 0, which is the fixed XROUT sink and is
  explicitly NOT a created magic number (`MFM2P` tests port==0 as a special case at
  kernel 126777).

## The low7 == 0 / == 127 test - result, and why it is only weak support

**Counts: low7 == 0 occurs in exactly 1 of 27 distinct port words (the port-0 sink);
low7 == 127 occurs in 0 of 27. Among the 26 real minted magics, neither 0 nor 127
ever occurs.**

Two caveats, stated plainly:

1. **The premise "0 and 127 are rejected and redrawn" is NOT supported by the
   carved kernel listing.** `ZRAND` at 131152 ends with `SAT 127` + `RAND ST DA`,
   which masks to 0..127 with no rejection loop, and the carved doc itself says
   "masked to `0..127`"
   (`XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md` lines 28, 103-117). No
   redraw code is shown. So the task's 1..126 assumption is UNVERIFIED and should
   not be relied on.
2. Even if the redraw existed, the test is statistically weak at this corpus size:
   under a uniform 0..127 draw, P(26 samples avoid both 0 and 127) = (126/128)^26
   = 0.66. Observing no 0 and no 127 is therefore **consistent with, but not
   evidence for**, the hypothesis.

The verdict does NOT rest on this test. It rests on Evidence 1 and 2.

## What is still not proven  [INFERRED]

The corpus contains no `XSGMG` / `XSGNM` / `XFM2P` exchange, so no frame ever
states "wire port 0x04C2 is port number 9" in words. Evidence 2 supplies exactly
that for ONE value (`*TADADM` = 2) from the operator-command side; the general rule
for session ports is INFERRED-strong by extension.

**Capture that would close it completely:** an `XSGMG` (get-magic) or `XSGNM`
reply, or any XROUT letter carrying a 4-byte magic parameter for a port whose
kernel port number is independently known - e.g. run `list-servers` on the node
while capturing a session to a server other than `*TADADM` (`*FA-SERVER` logical
port 11, `*XFTRA` 8) and check that its wire port `>> 7` equals 11 / 8. The corpus
already shows port numbers 8 and 9 in use, so `*XFTRA` (8) is the cheapest target.

## Recommended edit to XMSG-PROTOCOL.md section 7.1

The current text (`XMSG-PROTOCOL.md` line 852) calls the field
`(logical slot << 7) | low7` and treats low7 as opaque. That is correct but
under-committed. It can now be restated as:

> The wire port field is the LOW WORD of the 32-bit XMSG magic number
> (`XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md`): the high 9 bits are the
> kernel's 1-based port number, the low 7 bits are the magic's `RNMAG` random
> part. `XMDSY:XMDPT` and `XMSSY:XMSPT` are literally MAGNO A:D pairs - which is
> why the TAD port-assign can ship a session endpoint as one 32-bit value.

Section 18.1/18.3 item **U4** ("Session-port low-7, derivation NOT FOUND") is
resolved as to WHAT the field is (the `RNMAG` random part); HOW `RNMAG` is seeded
per port block remains open, but `ZRAND` (kernel 131152) is now the named producer.

---

## Files cited

- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\XMSG-MAGIC-NUMBER-LAYOUT-CARVED-2026-07-26.md` (lines 12-30, 60-100, 103-140, 174-181)
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\DOC\XMSG-PROTOCOL.md` (lines 788-912, 1580-1610, 1331-1340, 1387)
- `E:\Dev\Ronny\NDInsight\SINTRAN\TAD\TAD-Message-Formats.md` (lines 157-158, 1382-1395)
- `E:\Dev\Ronny\NDInsight\SINTRAN\XMSG\SRC\pcap-decode-report.txt` (raw lines 1018, 1532, 2069-2178, 2493, 9054, 9440, 9811, 10856, 11741)
- `E:\Dev\Ronny\X25Emulator\pcap\*.pcapng` (source captures)
