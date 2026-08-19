# Compiler listings fetched off the machine

`CHAT.LIST` and `CHAT.LIST.txt` are pulled from D100 after a build and are **regenerated**, not
edited. `.txt` is the same file with bit 7 stripped, because SINTRAN text can carry even parity.

## Why they are fetched at all

The compiler prints its diagnostics as it goes. On a thousand-line source they scroll off a
24-line terminal long before the summary appears, and the `0 DIAGNOSTICS` still on screen at the
end belongs to the SECOND pass - the loader - so it sits happily underneath a COMPILE that had
three errors. A build was read as clean twice that way.

The listing is the only honest record. Fetch it and grep it:

```powershell
.\tools\planc-build.ps1 -PullOnly
```

which fails the run on any `*** ERROR`.

## What it caught the first time it was used

```
899   (239)/SENDJOIN  *** ERROR - ILLEGAL SYNTAX "SHOWSTATUS"
```

PLANC is single pass: `showStatus` was declared BELOW `sendJoin`, which calls it, and the
compiler reports the CALL rather than the missing declaration - so the message names a routine
that exists and looks nonsensical until you know the rule. Same listing also caught a local
`offset` colliding with the module's global `offSet`, because PLANC ignores case.
