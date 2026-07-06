# GOD-LLM question: TAD terminal output length limit and multi-frame chunking

Date: 2026-07-06
Context: node 102 (our C# host) sending terminal output to node 100 (asker/terminal) over a
connect-to TAD session. Everything below is from a live capture against machine 100
(xmsg-runner.log, 2026-07-06 12:13). Secure-ACK of session data is already correct (fixed).

## The symptom

Short command replies work. A long reply (`stat`, ~460 bytes, split across 2 frames) is
DISPLAYED in full on 100's screen and then 100 crashes:

```
SYSTEM MALFUNCTION:  (See supervisor / RFIRUT : 1)
TAD protocol error: Illegal element length
SYSTEM MALFUNCTION:  (See supervisor / TERMREAD : 8)
Network inaccessible, xmsg error:
XMSG crash : Illegal port address in the creation of magic number
-- DISCONNECTED FROM: D102 --
```

## The measured boundary (verified from the wire)

WORKS - `help` reply, one frame, XMLEN 146, a single BDAT of 137 bytes:

```
[TX] ... 00000092 01 89 0D0A2020312E2054696D65...   (XMLEN=0x92=146, BDAT 0x01 count=0x89=137)
```

CRASHES - `stat` reply, two frames:

```
frame 1: ... 000000 F2 01 F0 0D0A2D2D2D2054414420...  (XMLEN=0xF2=242, BDAT 0x01 count=0xF0=240, NO RFI)
frame 2: ... 000000 E6 01 DE 0A5465726D696E616C...1302000A 0200
         (XMLEN=0xE6=230, BDAT 0x01 count=0xDE=222, then SYCN 000A, then RFI 0200)
```

So: a 137-byte terminal-data BDAT element is accepted; a 240-byte one is rejected with
"Illegal element length" (routine RFIRUT). The cap is somewhere in the half-open range
(137, 240]. As a stopgap we chunk at 128 (proven-safe), but we want the real rule.

## Questions

1. What is the EXACT maximum length that RFIRUT/TERMREAD on machine 100 accepts for a single
   inbound terminal-data element? Is the limit on:
   - the BDAT byte count (opcode 0x01 count field), or
   - the whole XMSG frame / XMLEN (header+subheader+trailer), or
   - something else (a TAD input-buffer size, e.g. a specific octal constant)?
   If it is a known SINTRAN/TAD constant, what is its name and value?

2. How does the REAL host (the genuine d102 / SINTRAN TAD) send terminal output longer than
   that limit? Specifically:
   - Does it split into multiple frames, each a BDAT under the cap? If so, what cap?
   - Or does it send several smaller BDAT messages CHAINED inside one frame (opcode/count
     repeated) up to a frame limit, rather than one large BDAT?
   - Must EACH output frame carry its own RFI (input credit), or only the final one? Our
     current split puts SYCN+RFI only on the last frame (frame 2 above) and leaves frame 1 a
     bare BDAT. Is a bare continuation frame with no RFI legal?

3. Is the "Illegal port address in the creation of magic number" (TERMREAD:8) a downstream
   effect of the RFIRUT element-length fault (corrupted parser state), or an independent
   fault we must also address?

4. Is there any word-alignment / even-length requirement on a terminal-data BDAT element that
   is a continuation (not the last in the reply)?

## UPDATE 2026-07-06 (second and third live runs) - chunk size is NOT the cause

Chunking the reply into 128-byte BDAT frames did NOT help - `stat` still crashed with the same
"Illegal element length". Decisive findings from the runs:

- A 137-byte `help` reply (single frame) is accepted. `stat` (~460 bytes) crashes REGARDLESS of
  whether it is sent as one 460-byte element or as four <=128-byte frames. So the limit is on the
  TOTAL terminal-data bytes 100 accumulates for one reply BEFORE the RFI, not on any single BDAT
  element or frame. 100 buffers all BDAT data until the RFI, then RFIRUT rejects the accumulated
  element once it exceeds ~137..255 bytes.
- The crash cascades: when `stat` crashes 100's XMSG, 100's connect-to program AUTO-RECONNECTS
  (a fresh XSLET letter, Flags1=0x0000). Our server opens a NEW session with a NEW wire port each
  time (0x0211 -> 0x0212 -> 0x0213) and leaks the old one, so 100 ends up addressing a stale port
  and its magic-number creation fails ("Illegal port address..."). The user sees one continuous
  terminal (login, commands, then a burst of bare "#" prompts) until it finally disconnects.
- We send ZERO secure ACKs the whole session, yet login/Time/Date/Echo/help all work - so a
  responder secure-ACK is apparently NOT required for terminal traffic.

Interim workaround shipped: `stat` was made a COMPACT single-frame reply (<=128 bytes) ending in
"\r\n# ", exactly like the Time/help replies - so it no longer crashes and the prompt returns
without an Enter. The rich multi-line report is on hold pending the pagination answer.

## The real question now

How does the genuine SINTRAN TAD host stream terminal OUTPUT longer than one element (say a
500-byte `LIST-... ` or directory listing) to an asker without tripping RFIRUT? Options we need
disambiguated:
 - Send N separate replies, each a complete BDAT chain ending in its own RFI (i.e. flush per
   block)? If so, does each block re-assert SYCN, and is there a max block size?
 - Or one datagram whose BDAT data is <= some fixed max (what value?), and the client scrolls?
 - Is there a TAD "more"/pagination or a flush opcode we are missing?
 - Separately: on a re-connect (fresh XSLET, same asker system+port), should the host REUSE the
   existing session's wire port and reset its Flags1, or allocate a new port? What closes the old
   session so 100 stops polling the stale port?

## What we will do with the answer

Restore the full rich `stat` (and future `who` / `list route`) output using the correct
long-output mechanism, and make re-connect reuse/reset the session cleanly.
