# XSGIN name lookup captured, XSGMG still open (2026-07-27)

`XSGIN` (82) "get information about name" is captured in all three of its forms. `XSGMG`
(71) "get magic number from name" is **not**, and this run establishes why: nothing in the
XMSG command program issues it.

**Method.** New probe test `Boot_Login_StartXmsg_ProbeNameLookupServices` in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`,
reading the `MON 200` Device trace as before. It drives the XMSG command program's
name/system lookups and lets the trace say which XROUT service each one emits.

---

## 1. `XSGIN` on a SYSTEM name [VERIFIED]

`X-C:GET-SYSTEM-NAME-OR-NUMBER,D102,,`

```
request  01 52 00 06  FF 04 44 31 30 32          service 0x52 = 82 = XSGIN
                      |     "D102"
                      string parameter 1

reply    01 00 00 04  02 02 00 66                status 0 = XRSOK
                      |     |
                      |     system number = 102
                      integer parameter 2
```

Parameter 1 is **absent** from the reply. That is the manual's rule working: appendix B
section 3.11 makes the port number an *optional* OUT parameter 1, returned only when the
name is a port name. `D102` is a system name, so only parameter 2 comes back.

## 2. `XSGIN` on a PORT name [VERIFIED]

`X-C:GET-SYSTEM-NAME-OR-NUMBER,*TADADM,,`

```
request  01 52 00 0A  FF 07 "*TADADM" 00
reply    01 00 00 08  01 02 00 04  02 02 00 64
                      |     |      |     |
                      |     |      |     system number = 100
                      |     |      integer parameter 2
                      |     port number = 4
                      integer parameter 1
```

Both outputs, and `*TADADM` did sit on port 4 of system 100 in that boot - the registry
listing agrees. This is the unprivileged way to resolve a name: it yields a port number and
a system number, never a magic number, which is exactly the split
`XMSG-SERVER-NAMES-AND-LETTERS.md` section 2 describes.

## 3. `XSGIN` on an unknown name [VERIFIED]

`X-C:DEABBREVIATE-SYSTEM-NAME,D10,,` - an abbreviation, not a defined name:

```
request  01 52 00 06  FF 03 44 31 30 00     "D10", padded
reply    01 02 00 00                        service byte overwritten with status 2
```

No parameters at all, and the console printed "System name D10 is not known". So the
error reply is header-only, and the status lands where the service byte was - the same rule
`XroutReply` already models.

## 4. Which command uses which service [VERIFIED]

| Command | Service emitted |
|---|---|
| `GET-SYSTEM-NAME-OR-NUMBER,<name>,,` | `XSGIN` (82) |
| `GET-SYSTEM-NAME-OR-NUMBER,<number>,,` | `XSGNI` (69) walk - a lookup by number is a table walk |
| `DEABBREVIATE-SYSTEM-NAME,<name>,,` | `XSGIN` (82) |
| `LIST-NAMES,,,` | `XSGNI` (69) walk |
| `ASK-ROUTE` | not a command on this build ("Command not recognised") |

## 5. A trailing pad byte is the caller's choice [VERIFIED]

Two writers on the same machine disagree about an odd-length string that is the LAST
parameter, and XROUT accepts both:

| Writer | Bytes | Length field |
|---|---|---|
| XMSG-COMMAND (`XSGIN`) | `FF 07 "*TADADM" 00` | 10 - padded to a word |
| TADADM itself (`XSNAM`) | `FF 07 "*TADADM"` | 8 - no pad |

Our builders emit no trailing pad, matching TADADM. Padding *between* parameters is not
optional and we do emit it. `XroutRequestTests` pins both behaviours so neither gets
"fixed" into the other by accident.

This also explains the oddity flagged in
[XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md](XMSG-XSCRS-CONNECTION-PORTS-CAPTURED-2026-07-27.md)
section 6 only **partly**: TADADM's length field of 8 still does not match its own 9-byte
body. **UNRESOLVED.**

## 6. Why `XSGMG` did not appear [VERIFIED as far as it goes]

`XSGMG` is privileged, so the probe tried `SET-PRIVILEGED` and repeated the lookups. The
traffic was identical - still `XSGIN`.

**CORRECTION (same day):** that first probe did NOT actually raise privilege.
`SET-PRIVILEGED` answered "** Command not recognised **", which the run did not check. The
command exists only in ADVANCED mode - see section 8. The conclusion happens to survive,
because the commands that emit `XSGIN` are the same either way, but the reasoning was wrong
and is corrected here rather than quietly.

Nothing the XMSG command program exposes as an ordinary command resolves a name to a magic
number.

That is consistent with what `XSGMG` is for: handing one task another task's addressable
identity, which is the network server's job when routing between systems, not an operator
command. The remaining ways to capture it:

1. **The network-server path.** ENNS0 uses `getMagic`/`XSGMG` for inter-node resolution
   (`ENNS0-XROUT-GETMAGIC-FINDINGS-2026-07-07.md`). Blocked on the Ethernet II bring-up.
2. **The raw request builder.** The command program has `Open-Port`, `Get-Message-Space`,
   `Clear-Buffer`, `Append-Integer`, `Append-String`, `Buffer-Ready`, `Route-Message`,
   `Receive-Message` and `Decode-Buffer` - enough to hand-build ANY XROUT request,
   including service 71, and decode the reply. Their argument syntax is not documented in
   `XMSG-COMMAND-REFERENCE.md` and was not probed. **This is the cheapest remaining route
   and is untried.**

## 7. The raw request builder EXISTS, behind advanced mode [VERIFIED]

The route to `XSGMG` is real and now half-opened. `XMSG-COMMAND-REFERENCE.md` lists the
builder commands, but on this build every one of them - and `SET-PRIVILEGED` itself -
answers "** Command not recognised **" at the ordinary prompt. They are not absent, they
are **gated**:

```
X-C:SET-ADVANCED-MODE

X-C(Adv):SET-PRIVILEGED
*- WARNING: You can now bypass system protection mechanisms -*

X-C(Adv):OPEN-PORT
New Port no: 5

X-C(Adv):GET-MESSAGE-SPACE
No. of bytes?
Current message: 161605
```

So the sequence is: `SET-ADVANCED-MODE`, then `SET-PRIVILEGED`, then the builder commands.
Without privilege, `OPEN-PORT` in advanced mode answers
"*- XMSG error code: -27: Privileged function called without privilege" - which is itself a
clean confirmation of what the privilege gate does.

Note the program's own `?` is NOT an inventory: it lists only "new and modified commands
for product no. 210373M" (two of them). The reference document's list is broader than what
the ordinary prompt accepts, and advanced mode is the reason.

### The prompts, all of them [VERIFIED]

Learned by typing each command bare. Documented nowhere else:

| Command | Prompts |
|---|---|
| `OPEN-PORT` | none - answers "New Port no: N" |
| `GET-MESSAGE-SPACE` | "No. of bytes?" - answers "Current message: &lt;octal address&gt;" |
| `CLEAR-BUFFER` | none |
| `FILL-OUTPUT-BUFFER` | "Automatic?" then "No. of bytes?" - a test-data generator, NOT a way to attach the buffer |
| `APPEND-STRING` | "Parameter no?" then "Text?" |
| `APPEND-INTEGER` | "Parameter no?" then "Integer value?" |
| `BUFFER-READY` | "Ref. no?" (serial) then "Service no?" |
| `ROUTE-MESSAGE` | "From port no?" then "Message address?" |
| `RECEIVE-MESSAGE` | "Port no?" then "Wait?" |
| `DECODE-BUFFER` | "Input buffer?" |
| `LIST-BUFFER` | none - prints both buffers with addresses and lengths |

### The request assembles correctly

```
X-C(Adv):APPEND-STRING
Parameter no? 1
Text? *TADADM
X-C(Adv):BUFFER-READY
Ref. no? 1
Service no? 71
X-C(Adv):LIST-BUFFER
Output buffer address: 17060, length (bytes): 14, max: 511.
```

14 bytes is exactly right for `01 47 00 0A FF 07 "*TADADM" 00` - serial, service 71, length
10, string parameter 1 padded to a word.

### STILL STUCK: the buffer never reaches the message

`ROUTE-MESSAGE` does send - the trace shows
`XFSND [Receiving port: 0x00000000 Sending port: 5]`, a route to XROUT from our own port -
and XROUT answers. But it answers with an error, because the message is empty:

| "Message address?" answered with | Result |
|---|---|
| the MESSAGE address (`161605`, from GET-MESSAGE-SPACE) | request sent, XROUT replies `00 08 00 00` = status **8 = XRMTL**, "too short message" |
| the OUTPUT BUFFER address (`17060`, from LIST-BUFFER) | rejected before sending: "*- XMSG error code: -6: Illegal message buffer pointer" |
| blank | same as the message address - empty message, status 8 |

So `BUFFER-READY` assembles the buffer but does not write it into the message, and the
command that does is still unidentified. Untried candidates: `SET-CURRENT-MESSAGE` before
routing, and `SEND-MESSAGE` instead of `ROUTE-MESSAGE`.

**This is one step from done.** Everything else works: the gates, the assembly, the route to
XROUT, and a live reply path.

### What the failures already confirmed

Not nothing - two XROUT statuses are now observed rather than read from a symbol file:

- **8 = XRMTL** "too short message or resulting message too long", from routing an empty
  message.
- **2 = XRUNN** "unknown name", from the `XSGIN` lookup of "D10" in section 3 - the console
  text "System name D10 is not known" matches the symbol exactly.

And **XMSG -6** "Illegal message buffer pointer" is confirmed as the kernel's own reply to a
bad `XFSND` pointer.

## 8. BLOCKER: the boot harness crashes intermittently

Three consecutive runs ended with the host process dying - no exception, no log line, the
process simply gone (the signature of a stack overflow, which cannot be caught). It died at
a DIFFERENT point each time:

| Run | Last thing on the console |
|---|---|
| 1 | `CLEAR-BUFFER` in advanced mode |
| 2 | `START-TADADM` |
| 3 | `SET-ADVANCED-MODE` |

Earlier runs of the same harness completed the full sequence twice, and the registry probe
crashed once then passed on retry, so this is intermittent rather than caused by any one
command. **Do not read run 1 as "CLEAR-BUFFER is fatal"** - that was my first inference and
runs 2 and 3 refute it.

**Tested and ruled out: the MON 200 decoder change.** The obvious suspect was the odd-byte
fix made the same day, so it was reverted and the registry test re-run: it passed. Then the
fix was restored and the same test re-run: it passed again. Four subsequent probe runs also
passed with the fix in place. The change is not the cause, and the crashes cluster in time
rather than around any command or any code state - which points at something environmental.
No dump was produced (`--blame-crash` attached but collected nothing) and the Windows
Application log has no matching event, which is what a stack overflow looks like: the
process dies without a catchable exception.

This is emulator-side and belongs with the existing Ethernet-harness instability, not with
XMSG. It stopped blocking after a while - six later runs completed - but it will waste time
again.

## 9. Bonus: an XSLET with parameter 10 [OBSERVED, NOT EXPLAINED]

`LIST-CONNECTIONS` answered with system `D100` produced:

```
00 41 00 0E  FF 08 "*XM-FIDO"  0A 02 00 66
                               |     |
                               |     102
                               integer parameter 10
```

Service 0x41 = 65 = `XSLET`, a letter to `*XM-FIDO` carrying parameter **10**. Appendix B
section 3.4 documents parameters 1, 2 and 4 for XSLET and says nothing about a parameter
10. **UNRESOLVED** - recorded here because it is the only XSLET we have from a task's own
buffer rather than off the wire.

---

## Decoder fixes made for this capture

Both in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Sintran\MON_200_XMSG.cs`:

- The `XFWRI` dump printed `NBYTES / 2` words, silently dropping the final byte of an
  ODD-length buffer - which is why `*TADADM` read as `*TADAD` in the 07-26 capture. It now
  prints the trailing byte as the high half of the last word. **Verified in this run**:
  the same registration now ends `[0x4D]`.
- A dead local applied the message displacement to the user-buffer address. `X` is a
  displacement within the MESSAGE, not the buffer, so the reads were right and only the
  variable was misleading; it is gone, and a `-1` displacement now prints as `append`.

## Files

- Console transcript:
  `C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Repos-Ronny-RetroCore-Emulated-HW-ND-CPU-NDBUS\37a0478f-30f0-4e59-ab6b-17b6944f56c9\scratchpad\xrout-name-lookup-console.txt`
- Device log: `...\scratchpad\ethii-controller-log.txt`
- The XSCRS run's log, kept: `...\scratchpad\ethii-controller-log.XSCRS-2026-07-27.txt`
