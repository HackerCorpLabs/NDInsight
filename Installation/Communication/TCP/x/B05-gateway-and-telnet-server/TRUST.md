# Extraction trust report

Extracted from `tingo_micropolis_1325.img` (the **Tingo** Micropolis 1325)

Corroborated against:
- `micropolis_tingo`

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
| `SYSTEM/AIP-HOSTS:SYMB` | 780 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-NETWORKS:SYMB` | 680 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-PROTOCOL:SYMB` | 785 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/AIP-SERVICES:SYMB` | 3350 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/FTP-SERVER-B05:PROG` | 380928 | 186 | 100 | 0 | 0 | 0 | 19 | 0 | 67 |
| `SYSTEM/OUT-HENT:SYMB` | 24128 | 12 | 12 | 0 | 0 | 0 | 0 | 0 | 0 |
| `SYSTEM/TCP-IP-LO-1-B05:LIST` | 3496 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/FTP-CLIENT-B05:PROG` | 372736 | 182 | 101 | 0 | 0 | 0 | 4 | 0 | 77 |
| `TCP-IP/FTPRT-B05:PROG` | 268288 | 131 | 19 | 0 | 0 | 0 | 1 | 0 | 111 |
| `TCP-IP/PO-PWRFAIL-B05:PROG` | 30720 | 15 | 14 | 0 | 0 | 0 | 1 | 0 | 0 |
| `TCP-IP/PO-STOP-B05:PROG` | 4096 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-ERROR-1-B05:BRF` | 421260 | 206 | 206 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-IP-LO-1-B05:MODE` | 2096 | 2 | 2 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-SER-B0-B05:BPUN` | 131205 | 65 | 65 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TCP-SER-B1-B05:BPUN` | 131205 | 65 | 17 | 0 | 0 | 0 | 48 | 0 | 0 |
| `TCP-IP/TCP-SER-B2-B05:BPUN` | 131205 | 65 | 3 | 0 | 0 | 0 | 62 | 0 | 0 |
| `TCP-IP/TCP-SER-B3-B05:BPUN` | 131205 | 65 | 12 | 0 | 0 | 0 | 53 | 0 | 0 |
| `TCP-IP/TCP-START-B05:MODE` | 318 | 1 | 1 | 0 | 0 | 0 | 0 | 0 | 0 |
| `TCP-IP/TELNET-CLIEN-B05:PROG` | 387072 | 189 | 99 | 0 | 0 | 0 | 10 | 0 | 80 |
| `TCP-IP/TELNET-SERV-B05:PROG` | 356352 | 174 | 80 | 0 | 0 | 0 | 52 | 0 | 42 |

**Totals:** CONFIRMED 740, UNCORROBORATED 0, CONTRADICTED 0, LOST 0, BLANK 250, ZERO? 0, SPARSE 377

## Files with no lost or contradicted page (20)

- `SYSTEM/AIP-HOSTS:SYMB`
- `SYSTEM/AIP-NETWORKS:SYMB`
- `SYSTEM/AIP-PROTOCOL:SYMB`
- `SYSTEM/AIP-SERVICES:SYMB`
- `SYSTEM/FTP-SERVER-B05:PROG`
- `SYSTEM/OUT-HENT:SYMB`
- `SYSTEM/TCP-IP-LO-1-B05:LIST`
- `TCP-IP/FTP-CLIENT-B05:PROG`
- `TCP-IP/FTPRT-B05:PROG`
- `TCP-IP/PO-PWRFAIL-B05:PROG`
- `TCP-IP/PO-STOP-B05:PROG`
- `TCP-IP/TCP-ERROR-1-B05:BRF`
- `TCP-IP/TCP-IP-LO-1-B05:MODE`
- `TCP-IP/TCP-SER-B0-B05:BPUN`
- `TCP-IP/TCP-SER-B1-B05:BPUN`
- `TCP-IP/TCP-SER-B2-B05:BPUN`
- `TCP-IP/TCP-SER-B3-B05:BPUN`
- `TCP-IP/TCP-START-B05:MODE`
- `TCP-IP/TELNET-CLIEN-B05:PROG`
- `TCP-IP/TELNET-SERV-B05:PROG`
