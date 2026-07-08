# COSMOS X.21 Option - Installation Notes

**Installation tips for the COSMOS X.21 Option (ND-10403).**

---

## Files

| File | Contents |
|------|----------|
| `ND-xxxxx1-T1-NO.pdf` | One-page internal Norsk Data memo in Norwegian (from OPH, dated 04.03.87): "Noen tips vedroerende installasjon av COSMOS X.21 Option". Key points from the memo: X.21 runs on the HDLC-Link output; switch 13A all OFF; cable 325383 X-DCE X21 N100; HDLC must be 1 in the S3 config; do not use START-LINK in `(UT)XMSG-COM`; run `X21NS-IN-Dxx:PROG` fully, then edit only `X21NS-START-Dxx:MODE` (DEF-NET-LOCAL-ENDPOINT / DEF-NET-REMOTE-ENDPOINT examples given); XMSG patch level matters (level 06 as of 4/3-87, patch level 01 required for X.21 to work). |

The file name `ND-xxxxx1-T1-NO` is the archive's placeholder naming - the memo itself
carries no ND document number.

---

**Parent:** [../README.md](../README.md)
