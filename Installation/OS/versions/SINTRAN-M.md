# SINTRAN III Version M — Install Notes

> Status: SCAFFOLD. Verified facts only. Source: SINTRAN-M distribution archive.

## Verified facts
- Two init variants present: `Admin\System initialisation.txt` and `Admin\System initialisation-WD0.txt`.
- `Admin\HENT-MODE.txt` — warm-boot script (RT-LOADER, READ-BIN DMAC, INIT-BACKGROUND,
  CHANGE-DATAFIELD terminal speeds, BATCH, MAIL, SET-AVAILABLE).
- `Admin\PATCH-LOG.txt` present.
- `System initialisation.txt` does NOT declare PAPERTAPE peripheral files (differs from K).
- M06 symbol set includes `N5000-SYMBOLS` and `XMSG-SYMBOL-LIST`; `XMSG-STARTEX.TXT` present.
- Three VSX floppies: `VSXM1/2/3.TXT`.

## Source files
- `Admin\System initialisation.txt`, `...-WD0.txt`, `HENT-MODE.txt`, `PATCH-LOG.txt`, `SINTRAN-COMMANDS-M.TXT`
- `FILE-INFO\BIGDISK0-M.TXT`, `VSXM1/2/3.TXT`, `ND-PATCH-SIN-M.TXT`

## TODO (validate via doc / command / manual exercise)
- Difference between `System initialisation.txt` and the `-WD0` variant (WD = Winchester disk?).
- M bootstrap transcript and floppy file-system IDs.
