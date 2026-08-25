# Extraction trust report

Extracted from `c3_2024_1.img` (May 2024 read of the **c3** Micropolis 1325)

Corroborated against:
- `c3-k-bd.img`
- `BD.IMG`
- `WD0-M.IMG`
- `c3-recovered.img`

A page is 2048 bytes. Verdicts:

| Verdict | Meaning |
|---|---|
| CONFIRMED | another image of this pack holds identical bytes |
| UNCORROBORATED | data is present, nothing else could vouch for it |
| CONTRADICTED | another image holds DIFFERENT bytes here |
| LOST | zero here, but another image of the pack HAS data - imaging lost it |
| BLANK | zero here and zero in every witness - blank on the disk |
| ZERO? | zero, and no witness could say whether that is damage |
| SPARSE | block pointer is 0 - a real hole, by design, not damage |

| File | bytes | pages | conf | uncorr | contra | LOST | blank | zero? | sparse |
|---|--:|--:|--:|--:|--:|--:|--:|--:|--:|
| `SYSTEM/AIP-CONFIG:SYMB` | 681 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-HOSTS:SYMB` | 633 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-NETWORKS:SYMB` | 680 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-PROTOCOL:SYMB` | 785 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-SERVICES:SYMB` | 3881 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/HENT-MODE-C3:MODE` | 8559 | 5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/IN-TCP-IP-XX-D02:INST` | 559 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/IN-TCP-IP-XX-D02:LOGG` | 16599 | 9 | 9 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/LOAD-MODE-C3:MODE` | 8362 | 5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/COMP-TCCOM:MODE` | 126 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/LOAD-TCCOM:MODE` | 457 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/LOAD-TCSHAR:MODE` | 309 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/TCCOM:BRF` | 3404 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/TCCOM:SYMB` | 8741 | 5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/TCNESH:BRF` | 76 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/TCNESH:SYMB` | 333 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-COMM/UDP-TCCOM:SYMB` | 7729 | 4 | 4 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/DEFINE-FTPRT-D02:MODE` | 607 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/DEFINE-TCPP-D02:MODE` | 605 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/FTP-CLIEN-D01:PROG` | 178176 | 87 | 52 | 0 | 0 | 0 | 10 | 0 | 25 |
| `TCP-IP/FTP-SERVER-C07:PROG` | 299008 | 146 | 58 | 0 | 0 | 0 | 18 | 0 | 70 |
| `TCP-IP/FTPRT-D02:PROG` | 276480 | 135 | 25 | 0 | 0 | 0 | 1 | 0 | 109 |
| `TCP-IP/NK-100-1BANK-C03:BRF` | 12876 | 7 | 7 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/NK-100-2BANK-C03:BRF` | 12942 | 7 | 7 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/NK-ERRCODE-C03:DEFS` | 4399 | 3 | 3 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/NK-LIB-100-1:BRF` | 9322 | 5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/NK-LIB-100-2:BRF` | 9388 | 5 | 5 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/NK-LIBRARY-C03:IMPT` | 1116 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/PO-PWRFAIL-D02:PROG` | 276480 | 135 | 22 | 0 | 0 | 0 | 1 | 0 | 112 |
| `TCP-IP/PO-STOP-D02:PROG` | 264192 | 129 | 5 | 0 | 0 | 0 | 0 | 0 | 124 |
| `TCP-IP/RSH-CLIEN-D01:PROG` | 147456 | 72 | 37 | 0 | 0 | 0 | 1 | 0 | 34 |
| `TCP-IP/SKP-C00:DEFS` | 17260 | 9 | 9 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SKP-C00:IMPT` | 7759 | 4 | 4 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SKP-C00:INTL` | 7149 | 4 | 4 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB-NRE-1B-B01:BRF` | 98140 | 48 | 48 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB-NRE-2B-B01:BRF` | 99220 | 49 | 49 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB-REE-1B-B01:BRF` | 36975 | 19 | 19 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB-REE-2B-B01:BRF` | 37740 | 19 | 19 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB:DEFS` | 27886 | 14 | 14 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/SLIB:IMPT` | 5311 | 3 | 3 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-IP-LO-C07:LIST` | 4790 | 3 | 3 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-IP-LO-D02:LIST` | 4848 | 3 | 3 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-IP-LO-D02:MODE` | 2307 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-SER-B0-D02:BPUN` | 131205 | 65 | 65 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-SER-B1-D02:BPUN` | 131205 | 65 | 32 | 0 | 0 | 0 | 33 | 0 | 0 |
| `TCP-IP/TCP-SER-B2-D02:BPUN` | 131205 | 65 | 1 | 0 | 0 | 0 | 64 | 0 | 0 |
| `TCP-IP/TCP-SER-B3-D02:BPUN` | 131205 | 65 | 10 | 0 | 0 | 0 | 55 | 0 | 0 |
| `TCP-IP/TCP-START-C07:LIST` | 930 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-START-D02:LIST` | 684 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-START-D02:MODE` | 581 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-STOP-D02:MODE` | 574 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCPIP-MONITOR:PROG` | 206848 | 101 | 64 | 0 | 0 | 0 | 20 | 0 | 17 |
| `TCP-IP/TCPP-D02:PROG` | 299008 | 146 | 54 | 0 | 0 | 0 | 2 | 0 | 90 |
| `TCP-IP/TELNET-CLIEN-D01:PROG` | 169984 | 83 | 41 | 0 | 0 | 0 | 12 | 0 | 30 |

**Totals:** CONFIRMED 719, UNCORROBORATED 0, CONTRADICTED 0, LOST 0, BLANK 217, ZERO? 0, SPARSE 611

## Files with no lost or contradicted page (54)

- `SYSTEM/AIP-CONFIG:SYMB`
- `SYSTEM/AIP-HOSTS:SYMB`
- `SYSTEM/AIP-NETWORKS:SYMB`
- `SYSTEM/AIP-PROTOCOL:SYMB`
- `SYSTEM/AIP-SERVICES:SYMB`
- `SYSTEM/HENT-MODE-C3:MODE`
- `SYSTEM/IN-TCP-IP-XX-D02:INST`
- `SYSTEM/IN-TCP-IP-XX-D02:LOGG`
- `SYSTEM/LOAD-MODE-C3:MODE`
- `TCP-COMM/COMP-TCCOM:MODE`
- `TCP-COMM/LOAD-TCCOM:MODE`
- `TCP-COMM/LOAD-TCSHAR:MODE`
- `TCP-COMM/TCCOM:BRF`
- `TCP-COMM/TCCOM:SYMB`
- `TCP-COMM/TCNESH:BRF`
- `TCP-COMM/TCNESH:SYMB`
- `TCP-COMM/UDP-TCCOM:SYMB`
- `TCP-IP/DEFINE-FTPRT-D02:MODE`
- `TCP-IP/DEFINE-TCPP-D02:MODE`
- `TCP-IP/FTP-CLIEN-D01:PROG`
- `TCP-IP/FTP-SERVER-C07:PROG`
- `TCP-IP/FTPRT-D02:PROG`
- `TCP-IP/NK-100-1BANK-C03:BRF`
- `TCP-IP/NK-100-2BANK-C03:BRF`
- `TCP-IP/NK-ERRCODE-C03:DEFS`
- `TCP-IP/NK-LIB-100-1:BRF`
- `TCP-IP/NK-LIB-100-2:BRF`
- `TCP-IP/NK-LIBRARY-C03:IMPT`
- `TCP-IP/PO-PWRFAIL-D02:PROG`
- `TCP-IP/PO-STOP-D02:PROG`
- `TCP-IP/RSH-CLIEN-D01:PROG`
- `TCP-IP/SKP-C00:DEFS`
- `TCP-IP/SKP-C00:IMPT`
- `TCP-IP/SKP-C00:INTL`
- `TCP-IP/SLIB-NRE-1B-B01:BRF`
- `TCP-IP/SLIB-NRE-2B-B01:BRF`
- `TCP-IP/SLIB-REE-1B-B01:BRF`
- `TCP-IP/SLIB-REE-2B-B01:BRF`
- `TCP-IP/SLIB:DEFS`
- `TCP-IP/SLIB:IMPT`
- `TCP-IP/TCP-IP-LO-C07:LIST`
- `TCP-IP/TCP-IP-LO-D02:LIST`
- `TCP-IP/TCP-IP-LO-D02:MODE`
- `TCP-IP/TCP-SER-B0-D02:BPUN`
- `TCP-IP/TCP-SER-B1-D02:BPUN`
- `TCP-IP/TCP-SER-B2-D02:BPUN`
- `TCP-IP/TCP-SER-B3-D02:BPUN`
- `TCP-IP/TCP-START-C07:LIST`
- `TCP-IP/TCP-START-D02:LIST`
- `TCP-IP/TCP-START-D02:MODE`
- `TCP-IP/TCP-STOP-D02:MODE`
- `TCP-IP/TCPIP-MONITOR:PROG`
- `TCP-IP/TCPP-D02:PROG`
- `TCP-IP/TELNET-CLIEN-D01:PROG`
