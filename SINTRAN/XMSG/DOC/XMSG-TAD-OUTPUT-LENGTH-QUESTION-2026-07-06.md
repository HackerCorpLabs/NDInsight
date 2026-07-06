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

## What we will do with the answer

Set the chunk size to the real cap and, if required, change the framing (per-frame RFI, or
chain-within-one-frame) so long command output (`stat`, `who`, `list route`, `help`) streams
to 100 without tripping RFIRUT.
