# SINTRAN III Version K — Install Notes

> Status: SCAFFOLD. Verified facts only. Source: SINTRAN-K distribution archive.

## Verified facts
- Golden disk dump: `FILE-INFO\BIGDISK0-K.txt` (PACK-ONE, 38400 pages, 75 MB).
- Distribution floppy `VSXK1` directory name (file-system ID): `N-220046K03--01D`
  (`FILE-INFO\VSXK1.txt`); contains `MACM-1718L:BPUN` and `SINTRAN:DATA`.
- Init transcript: `Admin\System initialisation.txt` (declares PAPERTAPE reader/punch peripheral files).
- Users on disk: SYSTEM, FLOPPY-USER, UTILITY, BPUN-FILES, SCRATCH, RT, GAMES (`BIGDISK0-K.txt`).

## Source files
- `Admin\System initialisation.txt`, `Admin\SINTRAN-COMMANDS.TXT`, `Admin\SINTRAN-COMMANDS-L.TXT`
- `FILE-INFO\BIGDISK0-K.txt`, `VSXK1.txt`, `VSXK2.txt`, `ND-PATCH-SIN-K.txt`

## TODO (validate via doc / command / manual exercise)
- K bootstrap transcript (floppy load messages).
- Which MACM build is used (VSXK1 contains `MACM-1718L:BPUN` — confirm it is the one loaded).
