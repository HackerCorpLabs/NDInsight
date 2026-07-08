# SINTRAN III Version H — Install Notes

> Status: SCAFFOLD. Verified facts only. Source: SINTRAN-H distribution archive (`Admin/`).

## Verified facts
- MACM banner: `MACM-1718-K`, "INITIALIZED FOR: DISC-75MB AND DISC-38MB" (`install-log.txt`).
- Two distribution floppies: SINTRAN-DISKETTE-I (`N-10-102-I.img`) and DISKETTE-II (`N-10-102-II.img`) (`install-log.txt`).
- Load via `10,0$`; start via `22!`; later cold start via `)HENT` (`install-log.txt`).
- Floppy I dumps file-system part 1 & 2 + spooling; floppy II dumps RT-loader,
  SINTRAN-SERVICE-PROGRAM/MAIL/NORD-NET, "PAGING-OFF" area, SINTRAN; then `)GJEM`/`)9BYTT` (`install-log.txt`).

## Source files
- `Admin\install-log.txt`, `Admin\System initialisation.txt`, `Admin\SINTRAN-COMMANDS.TXT`.

## TODO (validate via doc / command / manual exercise)
- File-system init transcript specific to H.
- Floppy file-system IDs (directory names) for the two diskettes.
