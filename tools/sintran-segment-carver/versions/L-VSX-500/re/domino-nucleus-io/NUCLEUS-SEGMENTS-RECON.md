# NUCLEUS server segments 104-107 - RECON pass (L07)

Recon only (entry points, structure, strings, linkage to the kernel primitives).
Full analysis is a later job; target list at the end. Companion:
`NUCLEUS-PRIMITIVES-CARVE.md`.

## 1. Inventory [V]

| Segment | Name | Meta description | Load base | Pages | Notes |
|---|---|---|---|---|---|
| 104 | S3SNKSE | Save of NUCLEUS server | 030000B | 52 | byte-identical to 105 (`cmp`) |
| 105 | S3INKSE | Image of NUCLEUS server | 030000B | 52 | segment number = symbol `5NKSE=105` |
| 106 | S3SNKNA | Save of NUCLEUS name server | 0 | 64 | byte-identical to 107 (`cmp`) |
| 107 | S3INKNA | Image of NUCLEUS name server | 0 | 64 | segment number = symbol `5NKNA=107` |

## 2. 105-S3INKSE - the NUCLEUS server program [V observations]

- Nonzero pages 0-17 and 20-25 (VA 030000-053777 + 070000-101777 approx; per-page scan
  in the carve session). First words: `030000 176000 112773 033253 ...` - looks like a
  header (start addr, top, ?, pointer to version string).
- Version string at word 033253: `C02 September 26, 1988`.
- Diagnostic strings: `*** NUCLEUS: Error multibyte send, stat=` (w 035710),
  `*** Nucleus FATAL Error` (w 040062, 041774, 042013), `doNuc: unknown func=`
  (w 040101), `NKNAME` (w 042031).
- `doNuc: unknown func=` implies the server main dispatcher is near VA 0401xx; the
  three FATAL sites near 0400xx-0420xx are its error paths. [I]
- SYMBOL-2-LIST has the server-side segment symbols: `NKSER=014513`, `NKNAM=014541`,
  `NKMBU=041152`, `MSNKS=001237/LSNKS=000064`, `MSNKN=001323/LSNKN=000100`,
  `MINKS=003130`, `MINKN=003214` (mailbox/name descriptors for the two servers) [I -
  not yet pinned to bytes].
- Kernel linkage: the server is reached via SERVE (MON 347B) / 5SERV / NCALL mailbox +
  datafield block at resident VA 1251xx (DKICK pointers at 125142/125143); the server
  reads its mailbox with RNMSG (kernel-side helper at 045432 using cells 007276/007277)
  and enters/leaves the segment via LNKSE 035056 / segment number 105. [V pointers,
  I flow]

## 3. 107-S3INKNA - the NUCLEUS name server [V observations]

- Sparse: nonzero pages 0 and 13-18 only (VA 0-1777 and 032000-045777). First words:
  `000000 176000 045332 033363` - same header shape as 105, code high.
- Version string at w 033363: `C02 September 22, 1988`.
- String `serviceport` at w 034443 - the well-known NUCLEUS name-server port name.
- PLANC runtime error strings: `NO ROUTINEERROR HANDLER, ERRETURN=` (w 044660),
  `- STACK OVERFLOW AT` (w 045221) -> the name server is compiled PLANC, unlike the
  hand-NPL kernel. [I from strings]

## 4. Follow-up target list

1. Disassemble 105 pages 0-3 (03xxxx): server main loop, mailbox poll (RNMSG/GNMSG
   protocol), `doNuc` dispatcher; recover the server function-code table (the fn<=13B
   bound seen in SERVE/5SERV).
2. Recover the master-block fields the primitives never touch: hash array (+10/+12),
   net-address table (+26), buffer-area start/end, freelist handling (FREELINK/USER
   quota) - all server-side.
3. NCALL mailbox record layout (state word values 4/5, +1..4 caller id, +10 length)
   against the server's reader.
4. 107 name-server: name record format, "serviceport" bootstrap, interaction with
   XMSG/COSMOS naming (XROUT?).
5. Pin SYMBOL-2-LIST NKSER/NKNAM/NKMBU/MSNKS/MSNKN to segment bytes.
6. Cross-version: K06/M06 counterparts of segments 104-107.
