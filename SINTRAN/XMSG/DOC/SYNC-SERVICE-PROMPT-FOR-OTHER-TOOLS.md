# Getting your files onto the ND-100 — a prompt to hand to another tool

Copy everything below the line into your other assistant's context. It is written to be read
cold, by something that knows nothing about this project.

---

## How to put a file on the ND-100 machines (D100 / D102 / D103)

There is a **sync service** that carries files from a Windows folder onto a live SINTRAN III
machine over XMSG/COSMOS. You do not talk to the machine yourself. You drop a file in a folder
and it appears in the right user's directory on the ND-100.

### Where to put the file

```
<sync-root>\<SINTRAN-USER>\<FILENAME>
```

For example:

```
sync-root\SYSTEM\CHATSV.PLNC     appears as  (SYSTEM)CHATSV:PLNC     on the machine
sync-root\UTILITY\XSTART.MODE    appears as  (UTILITY)XSTART:MODE
```

**One folder per SINTRAN user, exactly one level deep.** A SINTRAN user directory is flat — it
has no subfolders — so a Windows folder can only correspond to a user. Anything you put deeper
than one level is skipped and reported, never flattened, because two files with the same name in
different subfolders would map to the same SINTRAN file and one would destroy the other.

If the user folder you need does not exist, create it. The name is the SINTRAN user name.

### Four rules about the file itself

1. **The colon becomes a dot.** SINTRAN names files `NAME:TYPE`. Windows will not allow a colon,
   so write `CHATSV.PLNC` and it arrives as `CHATSV:PLNC`.

2. **Line endings must be CRLF.** A text file with Unix LF endings arrives on the machine as one
   enormous line and SINTRAN rejects every line of it with `TOO LONG STRING`. Convert before you
   save:

   ```python
   data = open(path, 'rb').read().replace(b'\r\n', b'\n').replace(b'\n', b'\r\n')
   ```

   You do NOT need to do anything about parity or high bits. Plain 7-bit ASCII with CRLF is
   correct.

3. **Keep the name short: 11 characters or fewer, including the type.** `CHATSV.PLNC` is 11 and
   is fine. `CHATMON.PLNC` is 12 and will be refused before anything leaves the machine. This is
   a limit of the current transfer code, not of SINTRAN — a file the compiler creates on the
   machine itself can have a long name. If you need a long name on the machine, use a short one
   here and ask for it to be renamed on the machine afterwards.

4. **Write the file completely, then leave it alone.** The service waits for a file to stop
   changing for a few seconds before sending it. If you are generating a file, write it to a
   temporary name and rename it into place, so a half-written file is never picked up.

### How to know it worked

Watch the service's output. A successful transfer says:

```
[sync] create D100(SYSTEM)."CHATSV:PLNC" on D100 <- sync-root\SYSTEM\CHATSV.PLNC done, 235487 byte(s)
```

The important part is **`done, N byte(s)`** and the byte count matching your file.

**A byte count alone does not prove the file is usable.** A transfer once reported the exactly
correct number of bytes for a file that was useless, because the line endings were wrong. If the
machine then behaves oddly, suspect the format before suspecting the machine.

### When it does not work

```
[sync] FAILED ... node 100 did not answer any of 4 connect letters
```

**This almost always means two copies of the service are running.** They share the machine's
single message counter and one of them ends up permanently behind, at which point the machine
acknowledges its frames but answers nothing. It looks exactly like a broken machine and is not.

**Do not start a second copy of the service.** Check first:

```powershell
Get-CimInstance Win32_Process -Filter "Name='Xmsg.Live.Runner.exe'" | Select-Object ProcessId, CommandLine
```

If one is running, use its folder. If none is running and you need one started, ask — starting it
needs a node number that nothing else is using.

### What this service cannot do

- **It cannot fetch a file OFF the machine** by having the machine send it. That direction is not
  finished and hangs the machine's terminal. Ask a person if you need a file pulled back.
- **It cannot create SINTRAN users**, set access rights, or run anything on the machine. It moves
  files into existing user directories and nothing more.
- **It cannot write into a user that has a password** unless that password has been configured
  for it. If you need a user that is not `SYSTEM`, `UTILITY` or `RT`, ask first.

### If you are asked to compile or run something

Putting the source on the machine is only the first step. Compiling it, loading it and proving it
runs is a separate procedure with its own traps — most notably that the PLANC compiler accepts an
undeclared name silently, so a truncated source compiles perfectly and behaves wrongly at run
time. **Do not attempt the build yourself.** Deliver the file and say it is in place.
