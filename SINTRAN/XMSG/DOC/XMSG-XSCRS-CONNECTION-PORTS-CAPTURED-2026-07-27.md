# XSCRS connection ports captured from guest memory (2026-07-27)

The last unobserved XROUT registration service. This closes the gap left open by
`XMSG-XROUT-BUFFER-FORM-CAPTURED-2026-07-26.md`, and it corrects an assumption we were
carrying about how the "Free SPs" count gets its value.

**Method.** Same as the 07-26 capture: RetroCore traces `MON 200` at Device level and its
`XFWRI` decoder dumps the user buffer. Booting BIGDISK0-L, starting XMSG, then the COSMOS
products - `RT XFTRAD`, `RTON COSPO`, `COS-FA-SERV-E04:MODE` and the FSA
`START-SERVER 1` - puts every registration inside the capture window. Reproduce with
`Boot_Login_StartXmsg_StartCosmos_ListServers` in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.Tests\ND100\Nd100SintranEthernetIIBootHarnessTests.cs`.

The harness needed NO change - the earlier note that "the trace window did not cover their
startup" was wrong. The 07-26 run simply did not reach the products.

Words are printed as read from memory; ND is big-endian, so `[0x5350]` is bytes `53 50`.

---

## 1. `XSCRS` - creating a connection port [VERIFIED]

Four registrations, in the order they happened:

```
XFWRI  NBYTES=12   53 50 00 08  FF 06 2A 58 46 54 52 41
                   |  |  |      |  |  "*XFTRA"
                   |  |  |      string parameter 1, 6 bytes
                   |  |  length of remainder = 8
                   |  service = 80 = XSCRS
                   serial 0x53

XFWRI  NBYTES=18   53 50 00 0E  FF 07 "*FA-FSA" 00  02 02 00 00
XFWRI  NBYTES=20   53 50 00 10  FF 09 "*FA-FSA-I" 00  02 02 00 00
XFWRI  NBYTES=20   53 50 00 10  FF 0A "*FA-SERVER"    02 02 00 00
```

Each is followed by `XFSND` to XROUT from the port being named, exactly as `XSNAM` is.

Note the pad byte after an odd-length name: `"*FA-FSA"` is 7 characters, so its block is 9
bytes and a `00` even-aligns the integer block that follows. `"*FA-SERVER"` is 10
characters and needs none.

## 2. Two things the manual allows and the real servers use [VERIFIED]

Appendix B section 3.2 lists three parameters - name, max connections, uniqueness flag.
Nothing on this machine sends all three:

| Server | Parameter 1 | Parameter 2 | Parameter 3 |
|---|---|---|---|
| `*XFTRA` | name | ABSENT | ABSENT |
| `*FA-FSA` | name | 0 | ABSENT |
| `*FA-FSA-I` | name | 0 | ABSENT |
| `*FA-SERVER` | name | 0 | ABSENT |

So parameters 2 and 3 are genuinely optional, and `*XFTRA` shows that an absent count
behaves the same as an explicit zero.

## 3. The free-SP count is built by XSNSP, not by XSCRS [VERIFIED - CORRECTS AN ASSUMPTION]

This is the substantive finding. We had assumed - reasonably, from the manual's "sets a
counter ... to the value specified in parameter 2" - that a server declares its capacity
once, at registration. It does not. Every captured server registers with ZERO and then
issues one `XSNSP` of exactly +1 per service point:

```
XFWRI  NBYTES=8    54 51 00 04  01 02 00 01
                   |  |  |      |  |  |
                   |  |  |      |  |  value = +1
                   |  |  |      integer parameter 1, 2 bytes
                   |  |  length 4
                   |  service = 81 = XSNSP
                   serial
```

Counted in the trace, and then confirmed by the operator listing:

| Server | XSNSP +1 calls | `list-serv` Free SPs |
|---|---|---|
| `*XFTRA` | 1 | 1 |
| `*FA-FSA` | 2 | 2 |
| `*FA-SERVER` | 30 | 30 |

The `START-SERVER 1` command reported "No of FACs attached: 30", and thirty consecutive
`XSNSP` buffers follow it in the log. The counter is therefore a running total the server
maintains, which is consistent with how it is spent: XROUT decrements it per forwarded
letter and the server increments it again when a session ends.

## 4. The registry reply distinguishes the two port kinds [VERIFIED]

Walking the name table with `XSGNI` answers with a third parameter for connection ports
and none for plain named ports:

```
01 00 00 16  01 04 00 64 05 CC  FE 0A "*FA-SERVER"  03 02 00 1E
                                                    |  |  |
                                                    |  |  free connections = 30
                                                    integer parameter 3

01 00 00 10  01 04 00 64 01 F8  FE 08 "*XM-FIDO"
                                                    (no parameter 3 - XSNAM port)
```

That is how a caller tells a connection port from a named port without asking.

## 5. The registry, and the magic numbers behind it

```
System   Port  Free SPs   Name
   100     0             D100.
   100     3             *XM-FIDO.
   100     4             *TADADM.
   100     5       1     *XFTRA.
   100     6             *COSPO.
   100     7       2     *FA-FSA.
   100    11      30     *FA-SERVER.
```

Every magic number in the walk decomposes with the carved layout, and every random part
falls in the 1..126 range `ZRAND` can mint:

| Name | Magic | Port word | Port | Random |
|---|---|---|---|---|
| `*XM-FIDO` | `0x006401F8` | 504 | 3 | 120 |
| `*TADADM` | `0x0064025B` | 603 | 4 | 91 |
| `*XFTRA` | `0x006402D4` | 724 | 5 | 84 |
| `*COSPO` | `0x0064037D` | 893 | 6 | 125 |
| `*FA-FSA` | `0x006403F0` | 1008 | 7 | 112 |
| `*FA-SERVER` | `0x006405CC` | 1484 | 11 | 76 |

All six randoms lie within the first THIRTEEN steps of a single `ZRAND` orbit seeded at
120 - at orbit indices 0, 7, 4, 5, 8 and 12 respectively. Six values landing in so short a
window is not chance, so this is independent confirmation of the generator from a source
with no wire involved.

**UNRESOLVED:** the orbit indices are not in port order (port 4 drew index 7, port 5 drew
index 4). Something else consumes draws between registrations, or ports are not handed out
in draw order. Not investigated.

## 6. `*TADADM` uses XSNAM, not XSCRS [VERIFIED]

Worth stating because it is easy to assume the terminal-access server pools connections:

```
XFWRI  NBYTES=13   00 42 00 08  FF 07 "*TADADM"
```

Service `0x42` = `XSNAM`. It has no free-SP count, and the registry listing confirms the
column is blank for it.

**UNRESOLVED:** the length field says 8 while the body is 9 bytes (`FF 07` plus seven
characters). `*XM-FIDO` is self-consistent (length 10, body 10) and so is every `XSCRS`
buffer above, so this is specific to what TADADM writes, not to the format. XROUT accepts
it and the name registers correctly. Not investigated.

Note also that the logger truncates odd-length buffers: it prints `NBYTES / 2` words, so
the final `M` of the name is missing from the raw dump. Same for the `XFWRI` displacement,
which the decoder folds into the address for the trace text but not for the read - both are
cosmetic issues in
`E:\Dev\Repos\Ronny\RetroCore\Emulated.HW\ND\CPU\ND100\Sintran\MON_200_XMSG.cs`, harmless
here because every buffer of interest had displacement 0.

---

## What this changes in the library

- `XroutRequests.CreateConnectionPort` gained two overloads - name only, and name plus
  count - because the three-parameter form is not what anything actually sends.
- `XroutBufferFormTests` and `XroutRequestTests` parse these captured bytes and prove our
  builders reproduce them byte for byte, pad byte included.
- `XMSG-SERVER-NAMES-AND-LETTERS.md` no longer has to describe `XSCRS` from the manual
  alone.

## Files

- Console transcript:
  `C:\Users\ronny\AppData\Local\Temp\claude\E--Dev-Repos-Ronny-RetroCore-Emulated-HW-ND-CPU-NDBUS\37a0478f-30f0-4e59-ab6b-17b6944f56c9\scratchpad\xrout-registry-console.txt`
- Device log with the MON 200 trace:
  `...\scratchpad\ethii-controller-log.txt`
