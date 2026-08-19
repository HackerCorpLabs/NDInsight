# Sample corpus: `DESCRIPTION-FILE:DESC` and `:LINK` from 13 vendor floppies

These are the real files the DESC and `:LINK` findings were checked against, kept here so the
evidence outlives the disk images and the session that extracted them. Nothing in
`DESCRIPTION-FILE-FORMAT.md` or `LINK-FILE-FORMAT.md` has to be taken on trust - re-run the
checks against these.

**Provenance.** Extracted 2026-08-17 from SINTRAN floppy disk images with
`ndtool -x -F 'FLOPPY-USER/*' -o <dir> <image>.img`. Every directory below is named after the
image it came from. The DESC file is `DESCRIPTION-FILE.DESC` in each; `:LINK` files are stored
alongside it, named after their segment. `:PSEG` and `:DSEG` files are **not** copied here -
they run to megabytes and only their sizes matter, which are recorded below.

Twelve distinct products across seven years, from unrelated release lines.

| Directory | Floppy directory | Domain name in DESC | Segment | `.pseg` | `.dseg` | `:LINK` bytes |
|---|---|---|---|---|---|---|
| `ND-disk-00458` | ND-10190D-PART1 | FORTRAN-500-D | FORTRAN-500 | 152832 | 477085 | 12545 |
| `8_nd_f0b_10177h00-1s_fe` | 10177H00-1S | COBOL-500-H00 | COBOL-500-H00 | 172735 | 343997 | 11361 |
| `ND-disk-00042` | 210319H02 | LINKAGE-LOAD-H02 | LINKAGE-LOAD-H02 | 123989 | 2184977 | **0** |
| `ND-disk-00096` | 211159A01-XX-01D | LED-FORTRAN-A01 | LED-FORTRAN-A01 | 267382 | 1017597 | 13057 |
| `ND-disk-00093` | 210190K02-XX-01D | FORTRAN-500-K02 | FORTRAN-500-K02 | 309413 | 548865 | 18561 |
| `ND-disk-00092` | 210177K01-XX-01D | COBOL-85-K01 | COBOL-85-K01 | 265213 | 129253 | 17089 |
| `ND-disk-00095` | 211160B03-XX-01D | LED-B03 | LED-B03 | 223695 | 394525 | **0** |
| `ND-disk-00172` | 210814L05-XX-01D | HYPHEN-TEST-L03 | HYPHEN-TEST-L03 | 153595 | 454545 | 28737 |
| `ND-disk-00177` | 210874L05-XX-02D | SL202-FO-L27 | SL202-FO-L27 | 252164 | 288783 | 344641 |
| `ND-disk-00022` | 211078A01-EN-02D | OEM-STATU-A01 | OEM-STATU-A01 | 191144 | 677897 | 30337 |
| `ND-disk-00215` | 211066D10-SW-02D | RG-SERVICE-D | RG-SERVICE-D10 | 120342 | 77825 | 24353 |
| `ND-disk-00216` | 210528D10-SW-01D | NOTIS-RG-SW-D | NOTIS-RG-SW-D10 | 442113 | 247809 | 43937 |
| `ND-disk-00217` | 210528D10-SW-02D | RG-START-SW-D | RG-START-SW-D10 | 231578 | 114689 | 30209 |

Every floppy also carries a `SCRATCH-DOMAIN` / `SCRATCH-SEG-01` pair, whose `.pseg` is 5 bytes
and `.dseg` 1029 bytes everywhere except the 1982 floppy, where both are 5.

## Things this table already shows

- **The domain name and the segment file name are not the same string.** `RG-SERVICE-D` is the
  domain; `RG-SERVICE-D10` is the segment. Anyone matching one against the other will get
  nothing. `LINKAGE-LOAD-H02` is exactly 16 characters and therefore has no `0x27` terminator,
  which is why the terminator convention went unnoticed for so long.
- **`:LINK` is optional.** The linker and the symbolic debugger both ship without one.
- **DESC is 22528 bytes in all thirteen**, with domain entries at bytes 256 and 312 and segment
  chains at `0x4000` and `0x40C0`, across seven years. The layout did not drift.

## Reproducing the checks

Size rule, `PLB + PSIZE + 1 = .pseg` and `DLB + DSIZE + 1 = .dseg` - 48 comparisons, no
mismatches:

```
nd500-dump <dir>/DESCRIPTION-FILE.DESC
```

and compare against the `.pseg`/`.dseg` columns above. `nd500-dump` is built from the
`pcc-nd500` repository, `src/nd500-dump/`.

**Take segment file sizes from the image directory (`ndtool -t <image>.img`), not from
extracted copies.** A file with a non-zero byte count but zero allocated pages extracts as
empty. That happens on the 1982 floppy and it looks exactly like a format anomaly. It is not
one.

The browser viewer in `../viewer/` parses these same files and is kept in agreement with
`nd500-dump` - both read domain entries by index at `56*index + 256*(index div 32 + 1)` and
follow the segment chain.

---

**See:** [../DESCRIPTION-FILE-FORMAT.md](../DESCRIPTION-FILE-FORMAT.md),
[../LINK-FILE-FORMAT.md](../LINK-FILE-FORMAT.md)
