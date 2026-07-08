# 06 — Startup & Runtime / Terminal Configuration

> Status: SCAFFOLD. Verified sources: `SINTRAN-M\Admin\HENT-MODE.txt`,
> `SINTRAN-L\Admin\START-SINTRAN-MULTIUSER.TXT`.

## Verified content present in source (not yet narrated)

`HENT-MODE.txt` (SINTRAN-M) contains, verbatim:
- `@SET-UNAVAILABLE` ... warm-booting banner
- `@IN-CO ENT-DIR,,D-75-1 0`
- `@RTENTER` / `@RT-LOADER` / `READ-BIN DMAC 7` / `EXIT`
- `@INIT-BACKGROUND`
- `@CHA-BACK-SEG-SI 1 128` and `@CHA-BACK-SEG-SI 38 128`
- `@SIN` ... `@CHANGE-DATAFIELD 38D I Y Y N` with `TSPEED/0` (terminal 38, 9600 BD per source comment)
- `@CHANGE-DATAFIELD 17B I Y Y N` with `TSPEED/21` (remote-micro, 4800 BD per source comment)
- `@BATCH`, `@MAIL` / `@RUN`, `@SET-AVAILABLE`

## TODO
- Narrate the cold-start (`START-SINTRAN-MULTIUSER.TXT`, L) vs warm-boot (`HENT-MODE`, M) flows.
- Transcribe `START-SINTRAN-MULTIUSER.TXT` (not yet read).
- Per-version differences in startup.
