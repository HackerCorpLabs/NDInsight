# The *XFTRA file-transfer request, captured (2026-07-28)

First working traffic ever captured from a COSMOS file server. Until now `*XFTRA` was a name in
the registry whose purpose our own notes marked as *inferred from the name*.

**Method.** RetroCore MON 200 trace, harness test `Boot_Login_StartCosmos_ProbeFileTransfer`. Boot
SINTRAN L, start XMSG and the COSMOS products, run `@TRANSFER-FILE`, and read the `XFWRI` buffers
out of the Device log. No second machine and no pcap needed - the request is built locally, and
that is where it is legible.

---

## 1. The request [VERIFIED]

Typed at the client:

```
@TRANSFER-FILE
F-T:SET-DEFAULT-REMOTE-SYSTEM   -> D102 / SYSTEM / (no password)
F-T:TRANSFER-FILE
To?   D102(SYSTEM)."XMSG-COPY:BATC"
From? (SYSTEM)XMSG-STARTEX:BATC
```

What went to XROUT, 62 bytes:

```
01 41 00 3A                       serial 01, service 0x41 = 65 = XSLET, length 58
FF 06 "*XFTRA"                    string parameter 1  - the SERVER name
FE 04 "D102"                      string parameter 2  - the SYSTEM name
F4 06 "SYSTEM"                    string parameter 12 - remote USER name
0D 02 00 00                       integer parameter 13 = 0
F8 10 "\"XMSG-COPY:BATC\""        string parameter 8  - destination FILE NAME
F7 04 "SYMB"                      string parameter 9  - default file TYPE
0A 02 04 00                       integer parameter 10 = 1024
0B 02 00 02                       integer parameter 11 = 2
```

Parameter tags follow the rule we already had: an integer parameter `n` is tagged `n`, a string
parameter `n` is tagged `256 - n`. So `FF`=1, `FE`=2, `F8`=8, `F7`=9, `F4`=12.

**Parameters are NOT in numerical order on the wire** - the order here is 1, 2, 12, 13, 8, 9, 10,
11. They are tagged, so order carries no meaning. Any parser that assumes ascending order will
break on real traffic.

## 2. What this tells us [VERIFIED / INFERRED]

- **The letter carries the whole transfer specification.** Appendix B section 3.4 says of `XSLET`:
  *"The remainder of the message can contain data for the receiving task (user name, password,
  ....)"*. This is that sentence decoded. `*XFTRA` needs no separate session-setup exchange to
  learn what to copy - it is all in the first letter. [VERIFIED]
- **Parameters 1 and 2 are the documented XSLET fields** (server name, system name). Parameters 8
  to 13 are the application's own, and are documented nowhere. [VERIFIED that they are present;
  their meanings below are INFERRED]
- **The destination file name keeps its quotes** - `"XMSG-COPY:BATC"` is sent with the `0x22`
  characters intact, so the *remote* SINTRAN is what interprets them as "create this file". The
  client does not strip them. [VERIFIED]
- **The file type travels separately** as parameter 9 (`SYMB`), even though the name already
  contains `:BATC`. Presumably the default to apply if the name carries no type. [INFERRED]
- **Parameters 10 and 11 are almost certainly the transfer conditions.** `DEFINE-TRANSFER-CONDITIONS`
  takes *"No of buffers, Size in bytes, Secure messages"*, and here parameter 10 = 1024 (a buffer
  size in bytes) and parameter 11 = 2 (a buffer count). Parameter 13 = 0 may be the secure-messages
  flag. Version E02 refuses the `DEFINE-TRANSFER-CONDITIONS` command, so these are its built-in
  defaults. [INFERRED - to confirm, capture on a version that accepts the command and change them]

## 3. The reply, and a rule confirmed on a real body [VERIFIED]

XROUT could not route to system 102 - `D102` is a defined name with no link - and answered:

```
01 0C 00 3A  ...the same 58 bytes of body...
```

The service byte `0x41` has been overwritten with `0x0C` = **12 = XRNRO**, "no access to remote
system", and the **entire original body is returned unchanged**. The console showed:

```
*** Error in accessing: D102(SYSTEM)."XMSG-COPY:BATC"
XMSG Routing/Naming error: No access to remote system
```

We already modelled "XROUT overwrites the service byte with a status" from header-only error
replies. This is the first time it has been seen on a message with a real body, and it confirms the
body survives intact - the sender can match the returned letter to what it sent.

## 4. What is still unknown

The letter is only the opening move. Everything after it - how `*XFTRA` on the receiving system
answers, how the file's blocks are actually carried, the acknowledgement rule, and what the
`*FA-*` file-access family does - needs a **peer that answers**. This capture failed at routing, so
nothing came back but the error.

Next step, from [PLAN-FILE-SERVER-CAPTURE-2026-07-28.md](PLAN-FILE-SERVER-CAPTURE-2026-07-28.md):
bring our own C# node up as node 102 on the HDLC link and let it receive. Even a conversation that
stalls after three frames gives us the answer shape. With the request format now known, we can also
*send* a plausible letter and watch a real `*XFTRA` respond - the more informative direction.

## 5. Also captured in the same run

`SET-DEFAULT-REMOTE-SYSTEM` triggers an `XSGNI` (69) registry walk before the transfer - the client
resolves the system name through the name table first, and only then posts the letter.

---

## Library

`XroutRequests.SendLetter` builds parameters 1 and 2. The file-transfer parameters (8-13) are
modelled in `Xmsg.Protocol.Tests\XroutBufferFormTests.cs` against these exact bytes, including the
out-of-order tags and the returned-with-status reply.
