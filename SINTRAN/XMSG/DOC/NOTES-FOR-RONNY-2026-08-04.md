# Notes for Ronny - 2026-08-04

Where the COSMOS file server got to, what is proven, and the one thing I could not close.

---

## The headline

**The FA protocol layer is done and live-verified.** A real SINTRAN III machine (D100) now drives
our C# file server through a complete conversation over HDLC: connect, reserve, a two-entry
directory walk, and a clean teardown, with no rejections anywhere.

**It still does not print a listing.** D100 completes the conversation, then reconnects and tries
again. The remaining fault is almost certainly in the CONTENT of the 64-byte directory record we
synthesise - see section 4.

---

## 1. What was wrong, and is now fixed

Six real defects, all found by comparing against the D100/D102 captures in
`DOC\captures\FA-READ-WRITE-2026-08-04\`. Each one only became visible after the previous was
fixed, because each was blocking the conversation earlier.

| # | Defect | How it was proved |
| --- | --- | --- |
| 1 | A request is answered by a **ShortAck**; the reply is the NEXT exchange | The pattern repeats unbroken through a 100-file listing |
| 2 | **XMCSM is the frame's own body length**, not the request's | Every Data frame in all four captures |
| 3 | The **server holds the directory-walk position**; the cursor means "start over / next", not an index | `A2 FFFF` appears once, `A2 0000` 102 times, different file each time |
| 4 | **`0x078x` means "finished"** and wants a Close, not a refusal | The captured teardown |
| 5 | **Flags 1 is PER DIRECTION**, not one shared number echoed by the answer | 222 matches, 0 mismatches when predicting each side from its own count |
| 6 | An **FA body is word aligned** - pad an odd length with one zero byte | 480 Data frames, every body length even, none odd |

Defect 5 is the one worth knowing about. The old model - one number per exchange, shared, echoed
by the answer - reproduces every capture we hold, but only because the conversation strictly
alternates, so two independent counters advance in lockstep and look like one. They come apart only
when the two sides drift, which no capture ever does. It cost most of the day.

It also explains `XmsgNode.ResyncAcceptDown`, the "step the accept down one per XENSE" recovery
someone added earlier: echoing OVERSHOOTS when the peer is ahead, and stepping down walks back to
our real count. That hack was a symptom of this bug. **It should be deleted once the new model has
run for a while** - I have left it in place for now rather than remove a safety net on the same day.

---

## 2. Something D100 told us, which is a nice confirmation

I had marked `0x0781` as "observed live, meaning UNKNOWN - possibly ended-early against the
captured `0x0782`", and refused to state it as fact.

After the word-alignment fix, **D100 started sending `0x0782` instead of `0x0781`** on the same
conversation. So the low bit does distinguish an abnormal ending from a normal one, and our
listing is now ending normally. That was the machine confirming the guess, not us.

---

## 3. Two harness bugs that cost hours and had nothing to do with the protocol

Both made the ND look broken when the fault was on our side. Both are fixed and written up in
`tools\README.md`.

1. **`ndterm`'s login raced.** It used fixed delays. D100 was busy just after an XMSG restart, its
   banner arrived late, the user name went out before the `ENTER` prompt existed, and every step
   after that was one out of phase. The session never left the login loop and 13 X-C commands each
   burned a 120-second timeout: **28 minutes, no configuration applied.** It now waits for the
   prompts, and copes with a line that is already logged in.

2. **The runner blocked on its own stdout.** Started hidden with no reader, the console buffer
   fills and every write blocks about a second - so a connect confirm that should take 200 ms took
   23 seconds and D100 gave up. Always start it with `-RedirectStandardOutput`.

---

## 4. THE ONE THING I COULD NOT CLOSE - and where I would look next

D100 asks for exactly two entries (we serve two files), gets both, and then finishes the
conversation **without sending `ReleaseFileEntry`** and **without ever asking for a third entry**.
It then opens a second connection and starts again. A real `LIST-FILES` uses exactly ONE connect,
so the retry means D100 is not satisfied.

Our reply is now byte-shaped like the real server's everywhere I can check - 98 bytes, same
envelope, same QFORM prefix `8C 4B A2 0000 A2 0000 A2 0001`. The differences left are all INSIDE
the 64-byte record, which we synthesise ourselves:

```
            ours                              real
rec[00]  80 00 "HELLO'"                    90 00 "SINTRAN'"     <- both values occur, NOT a defect
rec[18]  "TXT'"                            "DATA"
rec[27]  00                                07
rec[29]  08                                20
rec[32]  mostly zero                       timestamps c1 fa aa 75 ..., sizes
```

**This is the honest gap: nobody has ever watched a real client read a record we built.** The test
file says so in its own remarks. The record layout came from `RetroFS`'s NDFS `ObjectEntry`, which
is right for a file ON DISC; whether the file server sends exactly that, and which fields it
requires, has not been established.

**What I would do next, in order:**

1. Decode `rec[24..31]` and `rec[32..63]` properly against a real NDFS directory. The two-byte
   values at 26-27 and 28-29 differ between ours and the real one and are cheap to identify.
2. Serve exactly ONE file and see whether D100 asks for a second. That separates "the record is
   bad" from "the walk-end signalling is bad", which is the fork I could not resolve today.
3. If the record turns out to be the problem, consider replaying a REAL captured 64-byte record
   verbatim (with the name changed) as a diagnostic - if D100 accepts that, the fault is provably
   in our record builder and nowhere else.

Step 2 is the cheapest and I would start there.

---

## 5. The other thing you should know

**The Ethernet path has a bug of its own**, separate from all of the above. Your HDLC A/B
suggestion is what proved it: with the identical upper stack, HDLC reaches the directory walk while
Ethernet is torn down (`kind 0x60`) right after the connect confirm and never reaches a single
file-access request. So D19999-over-Ethernet is blocked in `NdLinkLayer` / `EthernetLink` or the
emulated card, NOT in XMSG or FA. Nothing in this document's six fixes will help it.

That was a good call - it split one confusing problem into two clear ones.

---

## 6. Your "stop-link" hint - what it actually found

You suggested cycling the link. There is **no `STOP-LINK` command** (X-C answered
`** System name STOP-LINK is not known **`), but trying it exposed the real fault:

```
X-C:  XMSG Kernel error: XMSG is either not generated, not loaded or not started
      *- XMSG error code: -45
```

**XMSG itself was down.** I had been staring at the HDLC link while the kernel underneath it was
gone. `STOP-X` then reported `OK: XMSG terminated`, so it was half-up rather than absent - exactly
the tell already written up in `tools\README.md`, which I failed to check.

After `STOP-X` / `START-X`, X-C came back healthy and `START-LINK,1362,,,-1,,` returned `Ok`. Our
LAPB link went Active in **44 ms**.

Two facts about these commands, since they are easy to get wrong:

 - `LIST-LINK` takes no link number - it prompts `XROUT system?`. My `STOP-LINK,1362` line was
   swallowed answering that prompt, which is why it looked like a rejected command.
 - After `START-X` the remote names are gone and must be redefined (`DEF-REMOTE,,D103 103` etc.)
   before anything can reach us.

**Everything protocol-side is now verified**, including the two that were outstanding:

 - the **restart announce** - the first inbound Data frame arrived at Flags 1 `0x0000`, so D100 did
   reset its sequence when told. The link also stayed Active through it, which confirms the
   loop-tick deferral fix.
 - the **per-direction Flags 1 model**, re-confirmed on the same clean run.

### What D100 actually does now

It does not hang. It runs the whole conversation, then **retries it three times** - connection
numbers `0x0042`, `0x0043`, `0x0044` - and gives up silently. That is what a record it cannot use
looks like, and it narrows section 4's question rather than answering it.

### Two more record fields, measured offline

Found without using the machine at all, by comparing every 16-bit field across all 49 real
directory records to see which are constant and which vary:

 - **bytes 22-25** - the entry ORDINAL, counting 0, 1, 2, 3. `ObjectEntry.ToBytes` never writes
   there, so every record we sent carried zero.
 - **bytes 60-63** (`FilePointer`) - `0x000078DA` on **all 49 records, for 49 different files**. A
   block pointer that never changes is not a block pointer, so its meaning is UNKNOWN, but a
   constant that every record shares is exactly what a reader checks. Ours sent zero.

Both are committed (`a85ad3e`) and **not yet live-verified** - see below.

### ENNS0 can take XMSG down with it - and it is not needed for the HDLC path

Late in the evening a restart failed in a new way: `LIST-FILES D103(sys).` answered
**`UNKNOWN REMOTE SYSTEM NAME`**. The X-C transcript shows why:

```
DEF-REMOTE,,D103 103        Ok
START-NET-SERVER,ENNS0,,,N  *- XMSG error code: -45: XMSG is ... not started
DEF-NETWORK-CONN ...        -45
START-LINK,1362,,,-1,,      -45          <- so the HDLC link was never started
ENABLE-ROUTE-THROUGH        -45
```

`START-NET-SERVER` killed XMSG, and every command after it failed the same way. **It is
intermittent - one restart in eleven today**, so it is not the explanation for everything that went
wrong, but it is worth knowing that a machine which looks thoroughly broken may only have lost
XMSG at that one line.

Two changes, both Ronny's, now in `restart-xmsg-cosmos.ps1`:

 1. **ENNS0 is opt-in** (`-WithEthernet`, off by default). It is the ETHERNET network server; a
    system reached over HDLC does not need it, so the D103 path should not pay for an intermittent
    crash it has no use for.
 2. **`ABORT ENNS0` first, then wait 10 seconds.** `ABORT` is a SINTRAN command at the `@` prompt,
    NOT an X-C command - it belongs beside the COSMOS mode file's own `@ABORT FSART`. If a previous
    `START-NET-SERVER` took XMSG down with it, the RT program is left hung and starting it again
    just repeats the crash. The abort is not complete when the prompt returns, hence the wait.

Skipping ENNS0 gave a completely clean configuration run: `DEF-REMOTE,,D103 103` `Ok`,
`START-LINK` `Ok`, `ENABLE-ROUTE-THROUGH` `Ok`, no `-45` anywhere.

### The record diff, finished - only TWO fields are still unexplained

With both replies now 98 bytes, a byte-for-byte diff of ours against a real one gives 33 differing
bytes. Naming every one of them against `ObjectEntry`:

| bytes | ours | real | verdict |
| --- | --- | --- | --- |
| body 07 | `81` | `bb` | responder session token - the SERVER picks it, varies across recordings. Not a defect. |
| body 30 | `80` | `90` | record byte 0. BOTH values occur in the capture on different files. Not a defect. |
| body 32-51 | HELLO/TXT | SINTRAN/DATA | content |
| body 65 | `01` | `00` | record 35 `ObjectIndex` - **was a defect**, fixed: it is the 0-based walk ordinal, not the file number |
| body 69-81 | zeros | dates | see the date gap below |
| body 85 | `01` | `3f` | record 55 `PagesInFile` - ours is 1 page, the real file is 63. Content. |
| body 87-89 | 19 | 122880 | record 57-59 `BytesInFile`. Content. |
| **body 57, 59** | `0000`, `0008` | `0007`, `0020` | **record bytes 26-29. STILL UNEXPLAINED.** |

So everything is now either fixed, or provably content, or the server's own choice - **except record
bytes 26-29**, two words that `ObjectEntry` does not model at all (the same blind spot as bytes
22-25, which did turn out to matter). Real records carry `0x0007` and `0x0020` there. That is the
next thing to chase.

**A real gap found on the way:** the packed ND date only spans **1950-2013**, so a file with a
present-day Windows timestamp encodes as `0` and every file we serve shows **no date**. It is not
the blocker - the capture has real records carrying `0000` in those fields too - but it is a
visible hole. Left documented rather than papered over with an invented date; it belongs with the
sidecar design.

### Where I stopped, and what I need

D100's file access is crashed again: the last run produced **zero inbound frames**, and the
terminal said `FILE-ACCESS NOT RUNNING OR CRASHED`, even though the restart reported
`Server 1 started. No of FACs attached: 30` twice. The fault is inside D100 before it reaches the
network at all.

I have now restarted XMSG on D100 about ten times today. **I have not touched the RetroCore
emulator process and will not without you saying so** - whether it needs a proper restart is your
call.

**The one thing to do next**, once the machine is healthy: run `LIST-FILES D103(sys).` and see
whether the two new record fields change D100's behaviour. Everything else is in place.

---

## 7. Nothing else needs a decision from you

Everything above is either done or is ordinary work I can carry on with. The only judgement call I
made without asking was leaving `ResyncAcceptDown` in place (section 1); say the word and I will
remove it.

Documents: this file, plus
`DOC\FA-EXCHANGE-MODEL-AND-DIRECTORY-WALK-2026-08-04.md` (the protocol rules) and
`DOC\FA-READ-WRITE-WIRE-PROTOCOL-2026-08-04.md` (what a request and reply contain).
