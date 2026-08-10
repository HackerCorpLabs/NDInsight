# XMSG generation variables — ND's own names, meanings and defaults

> **CONFIRMED ON THE LIVE MACHINE, same day.** `LIST-GENERATION-VARIABLES` was run on D100
> (XMSG kernel version 1987.02.08, Release L, product 210373L). **Every value below matched**, and
> the running system reported one variable the file does not carry:
>
> ```
> X3FSZ, Maximum frame size in words (input)................:   312
> ```
>
> That is the INPUT counterpart to `X4FSO` (output), both 312 **words**. See the `X4FSO` section
> below — the pair settles the LAPB question outright.
>
> The live run also confirms `X4LTO` as the real name, not the manual's OCR'd `X4TMO`, and
> `LIST-UTILIZATION` independently confirms the table limits: task 80, port 128, message 256, name
> 768, system 512, link 4, message buffer 25 pages.

**Date:** 2026-08-07
**Sources, both official ND:**
- `SINTRAN/XMSG/XMSG-SYS-DEF-L.SYMB` — the file the XMSG installation program reads. Holds the
  variable names WITH their default values. It is in ND's 7-bit-with-parity encoding; strip the
  high bit of every byte to read it.
- `Installation/Installation-Description/ND-210373L-EN.md` section 5.16 — the version-L program
  description, which gives the MEANINGS as printed by `LIST-GENERATION-VARIABLES`.

Neither source alone is enough: the manual names 14 variables without values, the file carries 23
with values but terse labels. Together they are complete.

---

## The table

| Variable | Meaning | Default |
|---|---|---:|
| `X4TSK` | Maximum number of task descriptors | 80 |
| `X5PRT` | Maximum number of ports | 128 |
| `X4NAM` | Maximum number of named systems/ports | 768 |
| `X4NLW` | Maximum length of name in words | 16 |
| `X4MES` | Maximum number of message elements | 256 |
| `X4MMX` | **Maximum message size in bytes** | **2500** |
| `X5MTS` | Maximum buffer space owned by a task, in bytes | 12500 |
| `X4BPG` | Message buffer space in pages | 25 |
| `X4MCB` | Maximum number of calls in a multicall function | 5 |
| `X4SIR` | Maximum number of systems accessible | 512 |
| `X5LNK` | Maximum number of links | 4 |
| `X4LTO` | Default HDLC/Megalink timeout in XTUs | 10 |
| `X5TO1` | Timeout in XTUs when receiving datagrams | 150 |
| `X5TO2` | Timeout in XTUs when transmitting datagrams | 200 |
| `X4FSO` | **Maximum frame size in WORDS (output)** | **312** |
| `X4ACK` | Maximum number of network acknowledgement frames | 15 |
| `X4NBF` | Default number of receive frames per link | 5 |
| `X4TMS` | Number of transmit buffers per network server | 2 |
| `X4IRM` | Default maximum number of SABMs when starting a link | 10 |
| `X4RPM` | Maximum number of repeats before the link is stopped | 5 |
| `X4MXH` | **Maximum number of hops allowed** | **20** |
| `X5NGT` | Gateway timeout in XTUs when sending to a net server | 50 |
| `X5TRB` | Number of trace buffers | 2 |

**One discrepancy between the two sources.** The manual's section 5.16 table calls the HDLC/Megalink
timeout `X4TMO`; the file calls it `X4LTO`. The FILE is the primary artifact — it is what the
installation program actually reads — so `X4LTO` is used above. The manual is an OCR of a 1988
scan, and `T`/`L` is a plausible misread, but it has not been checked against the paper.

**None of these are in `xmsg-constants.json`.** That file is generated from `XMSG-PL-VALUES-M.INCL`,
which carries the protocol constants; these are SYSTEM GENERATION variables from a different file
and were absent from this repository until now.

---

## Why three of them matter to this project

### `X4FSO = 312` — the LAPB 312 was WORDS all along

`LapbLayer.MaxInformationLength` was raised from 312 to 622 on 2026-08-05 because 452 recorded
frames exceeded 312 and would have been answered with an FRMR. That left a deliberate, documented
disagreement with the ND LAPB spec, which states 312 with no unit.

`X4FSO` is "maximum frame size in **words** (output)" — and 312 words is **624 bytes**. Under that
reading there was never a conflict: the largest frame ever recorded is 622 bytes, inside 624.

**The WORDS reading is now confirmed twice over.** The live machine reports a second variable the
file does not carry:

```
X3FSZ, Maximum frame size in words (input)................:   312
X4FSO, Maximum frame size in words (output)...............:   312
```

Two independent frame-size limits, both stated in WORDS, both 312, one per direction. A bare "312"
in the LAPB spec is a word count — there is no longer any reading in which it is a byte count.

**One question remains, and it is now the only one:** `X4FSO`/`X3FSZ` say "frame" while the LAPB
requirement bounds the INFORMATION FIELD. If they bound the whole frame, the info limit is 624 less
address, control and FCS — about 620, which is under the 622 observed. So either they bound the info
field, or the LAPB number is a different limit that happens to share the value. Settling that needs
the ND LAPB spec read against these, not another capture.

### `X4MXH = 20` — a relay hop limit exists, and we do not implement one

`DatagramRelay` forwards without any hop count. ND allow 20. Nothing in the captures shows where a
hop count lives in the datagram — the relay rule we verified changes only word 0 and word 6, and
carries no counter — so this is NOT implemented, and inventing a field would be fabrication. What
the variable establishes is that a limit exists and that a real network is expected to enforce one
somewhere. Worth knowing before assuming a relay may forward indefinitely.

### `X4MMX = 2500` — the message size ceiling

Maximum message size in bytes. Our fragmentation splits at 594 body bytes per fragment with a
28-byte first-fragment head; a 2500-byte message is well within what two fragments can carry, so
this does not contradict anything, but it is the first hard number we have for the ceiling.

---

## Other useful numbers

- `X5PRT = 128` ports maximum, while `XFGSM` reports on only the first **16**. That is not a
  contradiction — ND say the snapshot covers 16 — but it does mean a task holding more than 16
  ports cannot see all of them in one call.
- `X4IRM = 10` SABMs when starting a link and `X4RPM = 5` repeats before the link is stopped are
  LAPB retry limits. Our `LapbLayer` has its own; these are ND's defaults to compare against.
- `X5LNK = 4` links maximum. Relevant to the multi-link relay node: four is the ceiling ND ship.
