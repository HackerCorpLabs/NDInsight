# QUDF.ABPA2 producer carve - is DSTBL scaled? (2026-07-23)

Question: does SINTRAN scale the BDIO media address (`DSTBL` = `ABPA2` = ABSTrans
parameter 2) anywhere between the ABSTrans caller and the DIOC - i.e. is `DSTBL`
a 2KB-page index or a converted 512-byte sector / cylinder-surface address?

Answer: **NO scaling on the DOMINO path. `DSTBL` is the caller's media address
copied verbatim, and the DOMINO path deliberately SKIPS the geometry conversion
(`TOSECT`) that the SMD disk path performs.** [V]

Overlay: 017-S3SMPIT = 026-S3IMPIT, base 032000B (0x3400).
Symbols: `ABPA2 = 000017` [SYMBOL-1-LIST], `GAPFU = 000744` / `GAPFD = 034006`
(literal pool of BDMTR, `BDIO-DOMINO-DRIVER-CARVE.md` section 7).

## The path

```
ABSTrans caller (file system) --param list-->  BDMTR 073454B (MTRANS for DOMINO)
   -> GAPFU 000744B  (T=0 / swapping path)   copy param list -> QUDF
   -> GAPFD 034006B  (normal path)           copy param list -> QUDF
   -> level 12 -> STRBDIO/MBUILD             copy QUDF.ABPA2 -> DOMDF.DSTBL
   -> DIOC
```

## GAPFD 034006B - the normal param copier  [V]

```
034006  146137  RADD CLD SB DX   ; X := QUDF (que element)
034007  146153  RADD CLD SA DB   ; B := A = caller parameter list
034010  051400  LDT I ,B 0       ; T := param[0]           (ABFUN)
034011  025401  LDD I ,B 1       ; AD := param[1] double    (MEMA1 memory addr)
034012  032014  STF ,X 14        ; QUDF.ABFUN(14):=T, MEMAD(15-16):=MEMA1
034013  025402  LDD I ,B 2       ; AD := param[2] double    (ABP21 = MEDIA ADDR)
034014  022017  STD ,X 17        ; QUDF.ABPA2(17-20) := AD  <-- VERBATIM, no shift/mul
034015  025403  LDD I ,B 3       ; AD := param[3] double    (ABP31 = page count)
034016  022021  STD ,X 21        ; QUDF.ABP31(21-22) := AD  <-- verbatim
034017  044403  LDA ,B 3         ; A := param low word (count)
034020  146151  RADD CLD SA DD   ; D := A
034021  170401  SAA 1            ; A := 1
034022  022023  STD ,X 23        ; QUDF.ABA31(23-24) := (1, count)
034023  146173  RADD CLD SX DB
034024  146142  EXIT
```

## GAPFU 000744B - the swapping param copier (resident commoncode, base 0)  [V]

Same verbatim media-address copy (bank-switched with SSPTM around each store):

```
000762  025402  LDD I ,B 2       ; AD := param[2] double (ABP21 media addr)
000764  022017  STD ,X 17        ; QUDF.ABPA2 := AD       <-- VERBATIM
000766  025403  LDD I ,B 3       ; AD := param[3] (ABP31)
000770  022017? STD ,X 21        ; QUDF.ABP31 := AD (022021)
```
(GAPFU then does extra >>10 / >>6 arithmetic at 000773-001005 to build the
physical ABA31 for the swapper's own use - it does NOT touch the ABPA2 copy.)

## dd / xxd reproduction (ground rule)  [V]

Big-endian binaries, word `A` byte offset = `(A - base_word) * 2`.

| binary | word (octal) | expected | xxd read |
|---|---|---|---|
| SINTRAN-DATA_commoncode.bin (base 0) | 000762 | 025402 `LDD I,B 2` | 025402 |
| SINTRAN-DATA_commoncode.bin (base 0) | 000764 | 022017 `STD ,X 17` | 022017 |
| 017-S3SMPIT.bin (base 032000B) | 034013 | 025402 `LDD I,B 2` | 025402 |
| 017-S3SMPIT.bin (base 032000B) | 034014 | 022017 `STD ,X 17` | 022017 |

## No TOSECT on the DOMINO path  [V]

`BDMTR 073454B` (the DOMINO MTRANS entry, carved in `BDIO-DOMINO-DRIVER-CARVE.md`
section 7) calls `GETFREE`, `BRESERVE`, `GAPFU`/`GAPFD`, `WDATA` - and **never
`TOSECT`**. The SMD disk path DOES call `TOSECT` "to convert disk address to
cylinder and surface" (`ND-820023-1-EN`, "Monitor Level", line ~5012). So the
DOMINO path hands the DIOC a LOGICAL media address; the DIOC firmware does its
own physical mapping. This is why `DSTBL` is not a cylinder/surface/sector value.

## Conclusion

- [V] `DSTBL` = the ABSTrans caller's media address (`ABP21`), copied verbatim
  through GAPFU/GAPFD (this carve) and MBUILD (`BDIO-DOMINO-DRIVER-CARVE.md`) -
  SINTRAN applies ZERO scaling on the DOMINO path.
- [V] The DOMINO path skips the SMD `TOSECT` geometry conversion.
- [V, manual] The DOMINO SCSI device is uniformly 2-Kbyte pages/blocks.
- => `DSTBL` is a **logical 2KB-page/block index**; disk byte offset =
  `DSTBL * 2048`, transfer length = `DNRPG * 2048` bytes. The one hop not
  byte-carved is the file-system caller's own computation of `ABP21`, but with
  no driver scaling and a page-native device it cannot be anything but a page
  index. Upgraded from [I] to [V-driver-path + strong-I-unit].

Remaining fully-[V] closure (optional): carve the file-system page-transfer
routine that issues the ABSTrans call for a DOMINO file and read the `ABP21` it
forms. Not required for the DIOC emulation.
