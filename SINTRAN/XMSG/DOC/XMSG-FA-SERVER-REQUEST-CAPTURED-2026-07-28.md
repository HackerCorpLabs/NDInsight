# The *FA-SERVER remote-file-access request, captured (2026-07-28)

Second COSMOS file server decoded, hours after the first. Where
[XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md](XMSG-XFTRA-FILE-TRANSFER-REQUEST-CAPTURED-2026-07-28.md)
covers file TRANSFER, this is remote file ACCESS - the client half that makes ordinary SINTRAN
commands work on another machine's files.

**Why it needed a new image.** SINTRAN gates the COSMOS File User at revision F and only E media
exists here, so this can never run on the L or M packs. It was captured on a purpose-built
**SINTRAN K** image (K's documented minimum is revision D), where `LIST-FILES d100(system).` lists
files successfully through the File User path.

**Method.** RetroCore MON 200 trace, harness test `Boot_ProbeRemoteFileAccess_OnK`. The image
starts XMSG, TADADM and the COSMOS products from its boot batch job, so the test only logs in and
uses remote file syntax.

---

## 1. The request [VERIFIED]

`LIST-FILES d102(system).` produces two writes:

```
XFWRI  NBYTES=30  displacement 0
  1B 41 00 12                     serial 0x1B, service 0x41 = 65 = XSLET, length 18
  FF 0A "*FA-SERVER"              string parameter 1 - the server name
  FE 04 "D102"                    string parameter 2 - the system name
  07 E2 00 00 00 06 64 00         8 bytes BEYOND the declared length

XFWRI  NBYTES=4   displacement = -1 (append)
  A2 00 FF 92
```

Then `XFSND` to port 0 (XROUT) from port 8, and XROUT's own `XFSND` carrying `0x00660000` - system
102 in the high half - as it tries to route.

## 2. Two things this shows that XFTRA did not

**The application data is RAW, not parameters.** The XROUT length field says 18, which covers only
the two documented XSLET fields. Everything after that is opaque payload for the receiving task.
`*XFTRA` did the opposite - it packed its whole transfer specification into tagged XROUT parameters
8 to 13. So "the remainder of the message can contain data for the receiving task" (appendix B
section 3.4) is honoured two different ways by two servers in the same product.

**The message is built in two writes,** the second with displacement -1 to APPEND. This is the
first append-form `XFWRI` seen in any capture. A reader that assumes one write per message will
truncate this request.

## 3. The varying field [VERIFIED as varying, meaning INFERRED]

Three requests in one session:

| Command | trailer bytes |
|---|---|
| `LIST-FILES d102(system).` | `07 E2 00 00 00 06 64 00` |
| `FILE-STATISTICS d102(system).SINTRAN:DATA` | `07 E2 00 00 00 08 64 00` |
| (the retry of the above) | `07 E2 00 00 00 0A 64 00` |

Only the sixth byte moves, stepping 06 -> 08 -> 0A. Because the RETRY also advanced it, it behaves
like a **request counter incrementing by 2**, not a function code. [INFERRED]

`64 00` is plausibly our own system number (100 = 0x64) [INFERRED]. `07 E2` and the appended
`A2 00 FF 92` were constant across all three [VERIFIED constant; meaning UNKNOWN].

Nothing here is settled until a request that differs in a known way is compared - a different
remote user, a different file, or a system number other than 100.

## 4. The reply, and a correction [VERIFIED]

XROUT could not route to 102 and returned:

```
1B 0C 00 12  ...all 34 bytes of the original, unchanged...
```

Service byte `0x41` overwritten with `0x0C` = **12 = XRNRO**, "no access to remote system" - the
same rule and the same status as the file-transfer attempt.

**This corrects an inference made earlier today.** From the console alone it looked as though file
access got further than file transfer, because file transfer failed instantly with an XROUT status
while file access sat for a minute and then said *"NO ANSWER FROM REMOTE SYSTEM; FILE-ACCESS
CONNECTION ABORTED"*. The trace shows both got the identical XROUT refusal. The difference is
entirely client-side: the File User retries and waits about 56 seconds before giving up, and reports
its own message rather than the XROUT status. **A console message is not evidence about the wire.**

## 5. What is still unknown

Everything past the opening letter, for both servers. No peer answered, so nothing came back but the
refusal. The two routes to more:

1. **Answer it.** We now know both opening requests well enough to stand up a C# responder on node
   102 and see what the real client does with a reply. This is the higher-information direction.
2. **Two machines.** Two RetroCore instances over HDLC, both with COSMOS started - faithful, and
   more setup.

The undecoded trailer bytes are the reason to prefer route 1: varying one input at a time against a
live client is how they get named.

---

## Provenance

Image `D:\BIGDISK0-K.IMG` (SINTRAN III VSX/500 K, XMSG L03, COSMOS Basic E04). Console transcript
and Device log under the RetroCore scratch directory; test
`Boot_ProbeRemoteFileAccess_OnK` in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`.
