# Handoff 2026-09-02 - the manuals round

Everything below is committed as `744b2b4c` on `xmsg-chat`, except the seven untracked debug
pulls in `listings/` (RTLOG, HCOPY, CHATLG2) which are noise and can be deleted.

## What was done, and how it was proved

| Thing | Where | Proof |
|---|---|---|
| user manual | `DOC/manuals/CHAT-USER-MANUAL.md` | screens captured on D100 (FJELL) and D102 (VIDDA) with two people talking across the trunk, `/who`, `/help`, `/map`, `/list`, `/tell` both ways, windows, `/leave` |
| admin manual | `DOC/manuals/CHAT-ADMIN-MANUAL.md` | `CHAT-MON` `?`, `STATUS`, `LIST-TRUNKS`, `LIST-MEMBER` captured on D100 |
| build and deploy manual | `DOC/manuals/CHAT-BUILD-AND-DEPLOY-MANUAL.md` | the loop was run end to end twice today (client, then client+server) |
| spec updated | `DOC/CHAT-APP-SPECIFICATION.md` | `/w` `/window` `/close` added; "windows designed, not built" replaced |
| install + source floppies | `tools/make-floppies.ps1`, `dist/NDCHAT-INSTALL-2026-09-02.img`, `dist/NDCHAT-SOURCE-2026-09-02.img`, `dist/NDCHAT-2026-09-02.manifest.txt` | binaries pulled off D100 through the daemon (byte counts match FILE-STATISTICS, top bits intact); every file extracted back from the image and compared byte for byte |
| `DBGUI=0` | `SINTRAN-CHAT/CHAT.PLNC` | title row reads `TESTER@FJELL`, no numbers |
| `putNumber` five digits | `SINTRAN-CHAT/CHATSV.PLNC` | `STATUS` used to print `empty o45` for 6345 |
| build stamps bumped | both sources | `STATUS` reads `build S02-1340`, `LIST-MEMBER` reads `B02-1340` |
| deployed | all three machines | server RT-loaded on segment 211 on D100/D102/D103, trunks all `up`, NDCHAT re-dumped on D102, a line said each way FJELL-VIDDA |

## Where the machines are now

All three run server `S02-1340` on segment 211 and client `B02-1340`. Every trunk `up`. No
client is joined (all exited cleanly). No sync daemon is running - the one for this session
(PID 60052) was killed by hand at ~14:31 while idle; it sent no DISC, so the first daemon started
next may sit on "the link has not learned the peer yet" until D100 addresses us
(`@COPY-FILE D19999(SYSTEM).WAKE:TXT,CHAT:CNFG` on D100 - the copy fails and it still works).

History on D100's LOBBY started empty after the reload - histNext is not persisted across an
RT-load (the recovery scan was reverted on 2026-09-02 because CONTINUOUS unwritten blocks are
garbage). Not a regression; a known limit, now in the plan.

## Not done, on purpose

- **The install floppy has not been mounted on a machine.** `attach fd0 <img>` / `mount floppy0
  0 <img>` live in RetroCore's console window, which I do not drive, and the DAP `evaluate`
  cannot run console commands (connecting also takes the CPU and leaves it paused on
  disconnect). Ronny chose to skip it for now. Admin manual 3.1 says UNVERIFIED; the SINTRAN
  side (`ENTER-DIRECTORY`, `COPY-FILE`) is written from convention, not measured.
- `CHATRT:MODE` on the floppy has never been run as a MODE file; its step list is
  `rt-load.ps1`'s, which is proved.

## Found while photographing - all in `DOC/CHAT-PLAN.md`, items 1-9

1. After `/nick` the client's corner and the "sent" half of a private window keep the login
   name. Other end sees the right name.
2. Header shows `LOBBY@trunk` for a plain LOBBY.
3. A topic set on FJELL did not reach VIDDA.
4. `/leave` on a screen terminal shows nothing.
5. `/list` counts local seats only.
6. `putNumber` - fixed and deployed.
7. Stamps hand-set, never bumped - bumped; a check that cannot be forgotten is still wanted.
8. `planc-lint.py` treats all files on its command line as one link set (CHAT + CHATSV draw a
   false `inBuf` collision). Lint per link set.
9. `nd-deploy.ps1` stages into `sync-out`, the daemon watches `sync-relay`; its fallback stops
   every RetroCore. Do not use it.

## Traps met today, for the next person

- `FILE-STATISTICS CHAT:PLNC` lists every `CHAT*` file and `;1` does not narrow it - the one
  you want scrolls off; read the scrollback.
- Asking the daemon to pull a file a MODE file has just deleted is refused with SINTRAN error
  110 and reads like a transport fault.
- `ndtool -x -p` strips the parity bit from binaries too - extract text and binaries separately.
- Feeding the linter both programs at once is not a link set.
