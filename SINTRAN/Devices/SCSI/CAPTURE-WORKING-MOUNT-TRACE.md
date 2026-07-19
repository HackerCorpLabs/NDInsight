# Capture a Working-Mount Trace (to find the DISC-SCSI-1 dispatch gate)

**Full path:** `SINTRAN/Devices/SCSI/CAPTURE-WORKING-MOUNT-TRACE.md`

## Why
Verified from the failing SCSI trace: `@ENTER-DIRECTORY,,DISC-SCSI-1,0` performs a full,
**successful** disc identify (INQUIRY → READ CAPACITY → last-block read, all `SS_GOOD`), but the
directory-mount worker **`ENDIR` (140176B) executes 0 times** and no `READ_6 lba=0` is ever issued.
SINTRAN skips the mount at the resident enter-directory dispatch (`140xxx`) via a **skip arm** that
leaves no trace. To see the guard, we diff against a trace where the mount **succeeds** (the arm
that calls `ENDIR` IS taken). See `scsi-open-last-block-read.md` §5d.

## Requirements (both traces)
- **Same SINTRAN version: K** (the running system). L addresses will not line up.
- **Same trace format:** `Level = Opcodes`, destination File — the format that prints
  `X B A D T L` registers on every opcode line.
- Log target: `C:\Users\ronny\AppData\Local\trace\file-trace.txt` (append mode — rename between runs).

## Steps
0. **Preserve the failing run:** rename `file-trace.txt` → `file-trace-scsi-FAIL.txt`.
1. **Pick a directory that will actually mount** (trace OFF): `@LIST-DIRECTORIES-ENTERED` and the
   device list; choose a non-SCSI directory unit that can be freshly entered (Winchester
   `DISC-75MB-1`, a floppy, or a not-yet-entered unit). If it is already entered as MAIN+DEFAULT,
   ENTER will no-op — instead enter a present-but-unentered unit, or `@RELEASE-DIRECTORY` a
   **non-default** entered directory and re-enter it. The traced ENTER **must** read+enter the
   directory (confirm via a later `@dir`); a no-op re-entry has no `ENDIR` arm and is useless.
2. **Enable the opcode+register trace** immediately before the command.
3. `@ENTER-DIRECTORY <dir-name>,<working-device>,<unit>` — let it complete; confirm it entered.
4. **Trace OFF**, rename `file-trace.txt` → `file-trace-smd-OK.txt`.
5. Provide both `file-trace-scsi-FAIL.txt` and `file-trace-smd-OK.txt`.

## Analysis plan (once both traces exist)
1. Confirm the OK trace is a real mount: `ENDIR`/mount worker executes >0 and a `READ_6 lba=0`
   appears.
2. Diff the resident dispatch (`140xxx`) OK-vs-FAIL: OK takes the `ENDIR`-calling arm, FAIL takes
   the skip arm; the last common instruction before divergence is the **device-kind / config
   test**.
3. Read the compared value from the OK trace registers at that branch → the exact condition
   `DISC-SCSI-1` must satisfy (device kind / configuration) to pass the gate and mount.

## Notes
- The mount-worker symbol used here is `ENDIR = 140176B`; if the OK trace shows the directory read
  being built from a different resident PC, that PC is the true worker and the diff anchors there.
- Keep both files; do not let the append-mode log mix the two runs.
