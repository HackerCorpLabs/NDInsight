# MON-call index - SINTRAN III VSX/500 L07 (FULL working tree)

Master table. Regenerated from the **carved `MCTAB`**, which is ground truth for what this
build implements and where each worker lives.

---

## CORRECTION (2026-07-13) - the previous dispatch model was WRONG

The previous revision listed a `GOTAB[N]` value per call and a `partial` / `misattributed`
verdict derived from it. Both were artefacts of a broken model. What the bytes actually say
(full derivation: [`mon-analysis/317B-ExecuteCommand/README.md`](mon-analysis/317B-ExecuteCommand/README.md)):

```
MON N -> ENT14 072167B      level-14 entry (017-S3SMPIT / 026-S3IMPIT, NOT commoncode)
      -> X := MEM[MGOTA+N]  MGOTA = 071233B
      -> JMP ,X (072260B)   a DIRECT JUMP - no call, no "CALLPROC bridge"
      -> GOTAB[N] : 32/256 slots = resident fast handlers (incl. MON 200B XMSG)
                   224/256 slots = MFELL 072114B
      -> MFELL   : IRW 20 DP := CALLP 032201B ; MST PID/PIE  = a program-LEVEL switch
      -> MCTAB[N]  (MCTAB / 9MCTA = 005620B, segment 044-S3IDPIT)
      -> worker
```

- **`MCTAB` @ `005620B` is the real monitor-call table.** The `Worker` column below is the
  carved `MCTAB[N]` word - byte-verified, not a symbol-name guess.
- **`GOTAB` is NOT the monitor-call table**; it is the level-14 fast-path table only. It is
  `MFELL` for 224 of 256 calls, so it cannot identify a call.
- **Commoncode's `071233B` is NOT the GOTAB** (slot 0 = `000000`, slot 1 = `120303B`; the real
  values are `MFELL 072114B` and `M1 071633B`). Every `112xxx`/`120xxx`/`121xxx` "GOTAB entry"
  in the old table was unrelated bytes from a different overlay.
- **`misattributed` is retired.** It only ever described artefacts of the wrong model.

**Status vocabulary:**
- `worker VERIFIED` - the worker address is the carved `MCTAB[N]` word. The folder's *Dispatch*
  section still shows the old fictional model and must be rewritten.
- `WORKER CHANGED` - `MCTAB` disagrees with the folder's old worker. The folder is **wrong**.
- `NO FOLDER` - implemented in L07, not yet analysed.
- `byte-verified` - folder fully rebuilt on the corrected model.


## ND-100 calls (from carved MCTAB)

| MON | Name | MCTAB slot | Worker (carved MCTAB[N]) | Home segment | Folder | Status |
|-----|------|-----------|--------------------------|--------------|--------|--------|
| 000B | ExitFromProgram | `005620B` | **`PRTEX=032673`** | (resident / 003-S3CP) | [0B-ExitFromProgram/](mon-analysis/0B-ExitFromProgram/) | **WORKER CHANGED** (folder said `LEAVE=144142 (SYMBOL-1; NOT CARVED / zero-filled`) |
| 001B | InByte | `005621B` | **`YFGET=026576`** | 006-S3FS | [1B-InByte/](mon-analysis/1B-InByte/) | **WORKER CHANGED** (folder said `F1607 stub=120303 (SYMBOL-2, 025); INBT=032471 (`) |
| 002B | OutByte | `005622B` | **`YFPUT=026600`** | 006-S3FS | [2B-OutByte/](mon-analysis/2B-OutByte/) | **WORKER CHANGED** (folder said `OUTBT=032355 (SYMBOL-1, real code; shared driver`) |
| 003B | SetEcho | `005623B` | **`ECHOM=044540`** | (resident / 003-S3CP) | [3B-SetEcho/](mon-analysis/3B-SetEcho/) | worker VERIFIED; dispatch needs rewrite |
| 004B | SetBreak | `005624B` | **`BRKM=044425`** | (resident / 003-S3CP) | [4B-SetBreak/](mon-analysis/4B-SetBreak/) | worker VERIFIED; dispatch needs rewrite |
| 005B | ReadScratchFile | `005625B` | **`RDISK=102021`** | 006-S3FS | [005B-ReadScratchFile/](mon-analysis/005B-ReadScratchFile/) | worker VERIFIED; dispatch needs rewrite |
| 006B | WriteScratchFile | `005626B` | **`WDISK=102023`** | 006-S3FS | [006B-WriteScratchFile/](mon-analysis/006B-WriteScratchFile/) | worker VERIFIED; dispatch needs rewrite |
| 007B | ReadBlock | `005627B` | **`XRPAG=026572`** | 006-S3FS | [7B-ReadBlock/](mon-analysis/7B-ReadBlock/) | **WORKER CHANGED** (folder said `F1612 stub=120402 (SYMBOL-2, 025); RPAGE=101707 `) |
| 010B | WriteBlock | `005630B` | **`XWPAG=026574`** | 006-S3FS | [10B-WriteBlock/](mon-analysis/10B-WriteBlock/) | **WORKER CHANGED** (folder said `WPAGE=101711 (FILSYS; write entry of the shared `) |
| 011B | GetBasicTime | `005631B` | **`TIME=040747`** | (resident / 003-S3CP) | [11B-GetBasicTime/](mon-analysis/11B-GetBasicTime/) | worker VERIFIED; dispatch needs rewrite |
| 012B | SetCommandBuffer | `005632B` | **`SETOL=050666`** | (resident / 003-S3CP) | [12B-SetCommandBuffer/](mon-analysis/12B-SetCommandBuffer/) | **WORKER CHANGED** (folder said `no ND-100 worker; CBUF=170207 (SYMBOL-2, data bu`) |
| 013B | ClearInBuffer | `005633B` | **`CIBUF=044120`** | (resident / 003-S3CP) | [013B-ClearInBuffer/](mon-analysis/013B-ClearInBuffer/) | worker VERIFIED; dispatch needs rewrite |
| 014B | ClearOutBuffer | `005634B` | **`COBUF=044125`** | (resident / 003-S3CP) | [014B-ClearOutBuffer/](mon-analysis/014B-ClearOutBuffer/) | worker VERIFIED; dispatch needs rewrite |
| 015B | (undocumented) | `005635B` | **`SETUP=103417`** | 006-S3FS | [015B-Undocumented/](mon-analysis/015B-Undocumented/) | **WORKER CHANGED** (folder said `F1615=120501 (SYMBOL-2)`) |
| 016B | GetTerminalType | `005636B` | **`MGTTY=044630`** | (resident / 003-S3CP) | [16B-GetTerminalType/](mon-analysis/16B-GetTerminalType/) | worker VERIFIED; dispatch needs rewrite |
| 017B | SetTerminalType | `005637B` | **`MSTTY=044626`** | (resident / 003-S3CP) | [17B-SetTerminalType/](mon-analysis/17B-SetTerminalType/) | worker VERIFIED; dispatch needs rewrite |
| 020B | - | `005640B` | **`WCI=103463`** | 006-S3FS | - | **NO FOLDER** |
| 021B | - | `005641B` | **`M8INB=033734`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 022B | - | `005642B` | **`M8OUT=033557`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 023B | - | `005643B` | **`B8INB=033736`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 024B | - | `005644B` | **`B8OUT=033561`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 025B | - | `005645B` | **`SETW=103602`** | 006-S3FS | - | **NO FOLDER** |
| 026B | - | `005646B` | **`LSTC=045112`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 027B | - | `005647B` | **`RDSC=037404`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 030B | GetOwnRTAddress | `005650B` | **`GTRT=072036`** | (resident / 025-S3IRPIT) | [30B-GetOwnRTAddress/](mon-analysis/30B-GetOwnRTAddress/) | **WORKER CHANGED** (folder said `no ND-100 worker/named region; GETRT=106704 (N50`) |
| 031B | - | `005651B` | **`3BACK=045037`** | 004-S3RTL | - | **NO FOLDER** |
| 032B | OutMessage | `005652B` | **`MSG=102453`** | (resident / 025-S3IRPIT) | [32B-OutMessage/](mon-analysis/32B-OutMessage/) | worker VERIFIED; dispatch needs rewrite |
| 033B | AltPageTable | `005653B` | **`MALTN=037246`** | (resident / 003-S3CP) | [33B-AltPageTable/](mon-analysis/33B-AltPageTable/) | **WORKER CHANGED** (folder said `F1624 stub=120724 (SYMBOL-2, 025); ALTON=004076 `) |
| 034B | NormalPageTable | `005654B` | **`MALTF=037313`** | (resident / 003-S3CP) | [34B-NormalPageTable/](mon-analysis/34B-NormalPageTable/) | **WORKER CHANGED** (folder said `ALTOF=004116 (SYMBOL-1, real code; TRA PGC/TRR P`) |
| 035B | - | `005655B` | **`IOUT=051377`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 036B | - | `005656B` | **`NOWAI=044743`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 040B | - | `005660B` | **`SPCLO=067572`** | 006-S3FS | - | **NO FOLDER** |
| 041B | ReadObjectEntry | `005661B` | **`MROBJ=104035`** | 006-S3FS | [41B-ReadObjectEntry/](mon-analysis/41B-ReadObjectEntry/) | **WORKER CHANGED** (folder said `F1627=121023 (vector); ROBJE=55566 (FILSYS)`) |
| 042B | (undocumented) | `005662B` | **`OLDOP=103037`** | 006-S3FS | [042B-Undocumented/](mon-analysis/042B-Undocumented/) | worker VERIFIED; dispatch needs rewrite |
| 043B | CloseFile | `005663B` | **`CLOFI=103355`** | 006-S3FS | [43B-CloseFile/](mon-analysis/43B-CloseFile/) | **WORKER CHANGED** (folder said `CLOSF=123741 (FILSYS; calls FCLOS=67612)`) |
| 044B | GetUserEntry | `005664B` | **`MRUSE=105010`** | 006-S3FS | [44B-GetUserEntry/](mon-analysis/44B-GetUserEntry/) | **WORKER CHANGED** (folder said `GUSEN=055111 (FILSYS; two-entry SSK idiom w/ NGU`) |
| 045B | DefineBreakpoint | `005665B` | **`BDBRK=002235`** | (resident / 003-S3CP) | [045B-DefineBreakpoint/](mon-analysis/045B-DefineBreakpoint/) | **WORKER CHANGED** (folder said `F1631=121075; folder=DEBUGGER=105075`) |
| 046B | - | `005666B` | **`BGBRK=002245`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 047B | - | `005667B` | **`BSBRK=002274`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 050B | OpenFile | `005670B` | **`OPFIL=103034`** | 006-S3FS | [50B-OpenFile/](mon-analysis/50B-OpenFile/) | **WORKER CHANGED** (folder said `OPENF=123525 (FILSYS; calls FOPEN=67432)`) |
| 051B | DMACBreakpoint | `005671B` | **`GBRKD=014263`** | (resident / 003-S3CP) | [051B-DMACBreakpoint/](mon-analysis/051B-DMACBreakpoint/) | **WORKER CHANGED** (folder said `F1633=121147 (SYMBOL-2)`) |
| 052B | - | `005672B` | **`MTERM=046535`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 053B | GetSegmentEntry | `005673B` | **`MRSEG=040232`** | (resident / 003-S3CP) | [53B-GetSegmentEntry/](mon-analysis/53B-GetSegmentEntry/) | **WORKER CHANGED** (folder said `F1634 entry thunk=121174 (SYMBOL-2, 025; JMP I -`) |
| 054B | DeleteFile | `005674B` | **`MDLFI=106063`** | 006-S3FS | [54B-DeleteFile/](mon-analysis/54B-DeleteFile/) | worker VERIFIED; dispatch needs rewrite |
| 055B | - | `005675B` | **`RSPQE=106212`** | 006-S3FS | - | **NO FOLDER** |
| 056B | SetUserParam | `005676B` | **`MPASE=102363`** | (resident / 025-S3IRPIT) | [56B-SetUserParam/](mon-analysis/56B-SetUserParam/) | worker VERIFIED; dispatch needs rewrite |
| 057B | GetUserParam | `005677B` | **`MPAGE=102365`** | (resident / 025-S3IRPIT) | [57B-GetUserParam/](mon-analysis/57B-GetUserParam/) | worker VERIFIED; dispatch needs rewrite |
| 060B | N500M | `005700B` | **`N500M=030416`** | worker 050-S3I5PIT; sysmon 030-S3SM5 | [60B-N500M/](mon-analysis/60B-N500M/) | **FULLY CARVED** (47 subfn folders + [ND-500 system monitor](ND500-SYSTEM-MONITOR/README.md)) |
| 061B | - | `005701B` | **`FIXC5=076540`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 062B | GetBytesInFile | `005702B` | **`RMAX=103767`** | 006-S3FS | [62B-GetBytesInFile/](mon-analysis/62B-GetBytesInFile/) | worker VERIFIED; dispatch needs rewrite |
| 063B | - | `005703B` | **`ATMUL=033740`** | 006-S3FS | - | **NO FOLDER** |
| 064B | WarningMessage | `005704B` | **`ERMSG=016714`** | (resident / 003-S3CP) | [64B-WarningMessage/](mon-analysis/64B-WarningMessage/) | worker VERIFIED; dispatch needs rewrite |
| 065B | ErrorMessage | `005705B` | **`QERMS=016716`** | (resident / 003-S3CP) | [65B-ErrorMessage/](mon-analysis/65B-ErrorMessage/) | worker VERIFIED; dispatch needs rewrite |
| 066B | InBufferSpace | `005706B` | **`ISIZE=044203`** | (resident / 003-S3CP) | [66B-InBufferSpace/](mon-analysis/66B-InBufferSpace/) | worker VERIFIED; dispatch needs rewrite |
| 067B | OutBufferSpace | `005707B` | **`OSIZE=044231`** | (resident / 003-S3CP) | [067B-OutBufferSpace/](mon-analysis/067B-OutBufferSpace/) | worker VERIFIED; dispatch needs rewrite |
| 070B | - | `005710B` | **`COMSB=050673`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 071B | DisableEscape | `005711B` | **`MCDES=047020`** | (resident / 003-S3CP) | [71B-DisableEscape/](mon-analysis/71B-DisableEscape/) | **WORKER CHANGED** (folder said `F1643 stub=121417 (SYMBOL-2, 025; ZERO / runtime`) |
| 072B | EnableEscape | `005712B` | **`MCEES=047022`** | (resident / 003-S3CP) | [72B-EnableEscape/](mon-analysis/72B-EnableEscape/) | **WORKER CHANGED** (folder said `EESCF=112123 (N500-SYMBOLS only, ND-500 side; no`) |
| 073B | SetMaxBytes | `005713B` | **`SMAX=103706`** | 006-S3FS | [73B-SetMaxBytes/](mon-analysis/73B-SetMaxBytes/) | worker VERIFIED; dispatch needs rewrite |
| 074B | SetStartByte | `005714B` | **`SETBY=103720`** | 006-S3FS | [074B-SetStartByte/](mon-analysis/074B-SetStartByte/) | worker VERIFIED; dispatch needs rewrite |
| 075B | GetStartByte | `005715B` | **`REABT=104005`** | 006-S3FS | [075B-GetStartByte/](mon-analysis/075B-GetStartByte/) | worker VERIFIED; dispatch needs rewrite |
| 076B | SetBlockSize | `005716B` | **`SBSIZ=103752`** | 006-S3FS | [76B-SetBlockSize/](mon-analysis/76B-SetBlockSize/) | worker VERIFIED; dispatch needs rewrite |
| 077B | SetStartBlock | `005717B` | **`SETBC=103735`** | 006-S3FS | [77B-SetStartBlock/](mon-analysis/77B-SetStartBlock/) | **WORKER CHANGED** (folder said `F1646 stub=121516 (SYMBOL-2, 025); SETBL=030164 `) |
| 100B | - | `005720B` | **`7DGEY=036620`** | 004-S3RTL | - | **NO FOLDER** |
| 101B | - | `005721B` | **`SET=040470`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 102B | - | `005722B` | **`LUSTX=040527`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 103B | - | `005723B` | **`INTV=040444`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 104B | SuspendProgram | `005724B` | **`HOLD=040645`** | (resident / 003-S3CP) | [104B-SuspendProgram/](mon-analysis/104B-SuspendProgram/) | worker VERIFIED; dispatch needs rewrite |
| 105B | - | `005725B` | **`ABORT=036721`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 106B | - | `005726B` | **`CONCT=046373`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 107B | - | `005727B` | **`DSCNT=046470`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 110B | - | `005730B` | **`PRIOR=036634`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 111B | - | `005731B` | **`UPDAT=041147`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 112B | - | `005732B` | **`CLADJ=041037`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 113B | GetCurrentTime | `005733B` | **`CLOCK=040756`** | (resident / 003-S3CP) | [113B-GetCurrentTime/](mon-analysis/113B-GetCurrentTime/) | worker VERIFIED; dispatch needs rewrite |
| 114B | GetTimeUsed | `005734B` | **`TUSED=041303`** | (resident / 003-S3CP) | [114B-GetTimeUsed/](mon-analysis/114B-GetTimeUsed/) | worker VERIFIED; dispatch needs rewrite |
| 115B | - | `005735B` | **`MOFIX=067130`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 116B | - | `005736B` | **`MUNFI=067341`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 117B | ReadFromFile | `005737B` | **`XRFIL=026405`** | 006-S3FS | [117B-ReadFromFile/](mon-analysis/117B-ReadFromFile/) | **WORKER CHANGED** (folder said `F1656=121766; RFILE=102130 (FILSYS)`) |
| 120B | WriteToFile | `005740B` | **`XWFIL=026407`** | 006-S3FS | [120B-WriteToFile/](mon-analysis/120B-WriteToFile/) | **WORKER CHANGED** (folder said `WFILE=102132 (FILSYS)`) |
| 121B | AwaitFileTransfer | `005741B` | **`ABL1=043717`** | 004-S3RTL | [121B-AwaitFileTransfer/](mon-analysis/121B-AwaitFileTransfer/) | **WORKER CHANGED** (folder said `F1657 stub=122013 (SYMBOL-2, 025); WAITF worker `) |
| 122B | ReserveResource | `005742B` | **`RESRV=037103`** | (resident / 003-S3CP) | [122B-ReserveResource/](mon-analysis/122B-ReserveResource/) | worker VERIFIED; dispatch needs rewrite |
| 123B | ReleaseResource | `005743B` | **`RELES=037156`** | (resident / 003-S3CP) | [123B-ReleaseResource/](mon-analysis/123B-ReleaseResource/) | worker VERIFIED; dispatch needs rewrite |
| 124B | - | `005744B` | **`PRSRV=037076`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 125B | - | `005745B` | **`PRLS=037147`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 126B | - | `005746B` | **`DSET=040477`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 127B | - | `005747B` | **`M4CRT=040611`** | 030-S3SM5 | - | **NO FOLDER** |
| 130B | - | `005750B` | **`DINTV=040453`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 131B | - | `005751B` | **`X163T=034026`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 132B | - | `005752B` | **`MCALL=066355`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 133B | - | `005753B` | **`MEXIT=066410`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 134B | - | `005754B` | **`PRTEX=032673`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 135B | - | `005755B` | **`RTWT=032650`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 136B | - | `005756B` | **`RTON=072045`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 137B | - | `005757B` | **`RTOFF=072052`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 140B | - | `005760B` | **`WHERE=037213`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 141B | - | `005761B` | **`IOSET=044021`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 142B | ToErrorDevice | `005762B` | **`ERRMO=071776`** | (resident / 025-S3IRPIT) | [142B-ToErrorDevice/](mon-analysis/142B-ToErrorDevice/) | **WORKER CHANGED** (folder said `ERMON=114574 (N500-SYMBOLS only, ND-500 side; no`) |
| 143B | ExecutionInfo | `005763B` | **`RSIO=051430`** | (resident / 003-S3CP) | [143B-ExecutionInfo/](mon-analysis/143B-ExecutionInfo/) | worker VERIFIED; dispatch needs rewrite |
| 144B | DeviceFunction | `005764B` | **`MAGTP=026354`** | 006-S3FS | [144B-DeviceFunction/](mon-analysis/144B-DeviceFunction/) | worker VERIFIED; dispatch needs rewrite |
| 146B | - | `005766B` | **`IPRIV=113027`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 147B | - | `005767B` | **`CAMAC=113064`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 150B | - | `005770B` | **`GL=113134`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 151B | - | `005771B` | **`GRTDA=041426`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 152B | GetRTName | `005772B` | **`WMSBA=041745`** | (resident / 003-S3CP) | [152B-GetRTName/](mon-analysis/152B-GetRTName/) | worker VERIFIED; dispatch needs rewrite |
| 153B | - | `005773B` | **`IOXN=113154`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 154B | - | `005774B` | **`ASSIG=113166`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 156B | - | `005776B` | **`BCNAF=113241`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 157B | SegmentToPageTable | `005777B` | **`ENTSG=067764`** | (resident / 025-S3IRPIT) | [157B-SegmentToPageTable/](mon-analysis/157B-SegmentToPageTable/) | worker VERIFIED; dispatch needs rewrite |
| 160B | - | `006000B` | **`FIXC=076342`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 161B | - | `006001B` | **`3INST=112430`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 162B | OutString | `006002B` | **`3OUTS=112671`** | (resident / 025-S3IRPIT) | [162B-OutString/](mon-analysis/162B-OutString/) | **WORKER CHANGED** (folder said `OUTST=41013 (FILSYS; loops GETCH=30062/SOUTB=310`) |
| 164B | - | `006004B` | **`WSEG=071537`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 167B | - | `006007B` | **`REENT=067623`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 170B | UserDef0 (US0) | `006010B` | **`US0X=113321`** | (resident / 025-S3IRPIT) | [170B-UserDef0/](mon-analysis/170B-UserDef0/) | worker VERIFIED; dispatch needs rewrite |
| 171B | UserDef1 (US1) | `006011B` | **`TPFR1=113452`** | (resident / 025-S3IRPIT) | [171B-UserDef1/](mon-analysis/171B-UserDef1/) | worker VERIFIED; dispatch needs rewrite |
| 172B | UserDef2 (US2) | `006012B` | **`US2X=113610`** | (resident / 025-S3IRPIT) | [172B-UserDef2/](mon-analysis/172B-UserDef2/) | worker VERIFIED; dispatch needs rewrite |
| 173B | UserDef3 (US3) | `006013B` | **`US3X=113704`** | (resident / 025-S3IRPIT) | [173B-UserDef3/](mon-analysis/173B-UserDef3/) | worker VERIFIED; dispatch needs rewrite |
| 174B | UserDef4 (US4) | `006014B` | **`US4X=113770`** | (resident / 025-S3IRPIT) | [174B-UserDef4/](mon-analysis/174B-UserDef4/) | worker VERIFIED; dispatch needs rewrite |
| 201B | HDLCfunction | `006021B` | **`XTLX=102566`** | (resident / 025-S3IRPIT) | [201B-HDLCfunction/](mon-analysis/201B-HDLCfunction/) | **WORKER CHANGED** (folder said `HDLC=103112 (SYMBOL-2, 025-S3IRPIT code overlay,`) |
| 204B | - | `006024B` | **`BRPNT=104554`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 205B | - | `006025B` | **`DEBUG=105075`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 206B | - | `006026B` | **`EDTRM=102265`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 207B | - | `006027B` | **`RERRP=102413`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 213B | GetDirUserIndexes | `006033B` | **`MUIDI=105012`** | 006-S3FS | [213B-GetDirUserIndexes/](mon-analysis/213B-GetDirUserIndexes/) | worker VERIFIED; dispatch needs rewrite |
| 214B | GetUserName | `006034B` | **`GUSNA=105301`** | 006-S3FS | [214B-GetUserName/](mon-analysis/214B-GetUserName/) | worker VERIFIED; dispatch needs rewrite |
| 215B | GetObjectEntry | `006035B` | **`DROBJ=104037`** | 006-S3FS | [215B-GetObjectEntry/](mon-analysis/215B-GetObjectEntry/) | worker VERIFIED; dispatch needs rewrite |
| 216B | SetObjectEntry | `006036B` | **`DWOBJ=104410`** | 006-S3FS | [216B-SetObjectEntry/](mon-analysis/216B-SetObjectEntry/) | worker VERIFIED; dispatch needs rewrite |
| 217B | GetAllFileIndexes | `006037B` | **`GUIOI=105432`** | 006-S3FS | [217B-GetAllFileIndexes/](mon-analysis/217B-GetAllFileIndexes/) | worker VERIFIED; dispatch needs rewrite |
| 220B | DirectOpen | `006040B` | **`DOPEN=103026`** | 006-S3FS | [220B-DirectOpen/](mon-analysis/220B-DirectOpen/) | worker VERIFIED; dispatch needs rewrite |
| 221B | CreateFile | `006041B` | **`CRALF=105562`** | 006-S3FS | [221B-CreateFile/](mon-analysis/221B-CreateFile/) | **WORKER CHANGED** (folder said `CRFIL=115425 (FILSYS)`) |
| 222B | - | `006042B` | **`RINDX=051453`** | 006-S3FS | - | **NO FOLDER** |
| 227B | - | `006047B` | **`LAOB3=047070`** | 030-S3SM5 | - | **NO FOLDER** |
| 230B | - | `006050B` | **`MGDAE=047072`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 231B | ExpandFile | `006051B` | **`EXPFI=105555`** | 006-S3FS | [231B-ExpandFile/](mon-analysis/231B-ExpandFile/) | worker VERIFIED; dispatch needs rewrite |
| 232B | RenameFile | `006052B` | **`MRNFI=106060`** | 006-S3FS | [232B-RenameFile/](mon-analysis/232B-RenameFile/) | worker VERIFIED; dispatch needs rewrite |
| 233B | SetTemporaryFile | `006053B` | **`STEFI=106052`** | 006-S3FS | [233B-SetTemporaryFile/](mon-analysis/233B-SetTemporaryFile/) | **WORKER CHANGED** (folder said `SETTF=106043 (FILSYS; presets mode 4, joins MDLF`) |
| 234B | SetPeripheralName | `006054B` | **`SPEFI=106055`** | 006-S3FS | [234B-SetPeripheralName/](mon-analysis/234B-SetPeripheralName/) | worker VERIFIED; dispatch needs rewrite |
| 235B | ScratchOpen | `006055B` | **`SCROP=103031`** | 006-S3FS | [235B-ScratchOpen/](mon-analysis/235B-ScratchOpen/) | worker VERIFIED; dispatch needs rewrite |
| 236B | SetPermanentOpen | `006056B` | **`SPERM=103353`** | 006-S3FS | [236B-SetPermanentOpen/](mon-analysis/236B-SetPermanentOpen/) | **WORKER CHANGED** (folder said `F1725 stub=066202 (SYMBOL-2, 025); SETPO=72465 (`) |
| 237B | SetFileAccess | `006057B` | **`SFACC=105552`** | 006-S3FS | [237B-SetFileAccess/](mon-analysis/237B-SetFileAccess/) | worker VERIFIED; dispatch needs rewrite |
| 240B | - | `006060B` | **`APSPF=106307`** | 006-S3FS | - | **NO FOLDER** |
| 241B | NewUser | `006061B` | **`SUSCN=106377`** | 006-S3FS | [241B-NewUser/](mon-analysis/241B-NewUser/) | worker VERIFIED; dispatch needs rewrite |
| 242B | OldUser | `006062B` | **`RUSCN=106562`** | 006-S3FS | [242B-OldUser/](mon-analysis/242B-OldUser/) | worker VERIFIED; dispatch needs rewrite |
| 243B | GetDirNameIndex | `006063B` | **`FDINA=106734`** | 006-S3FS | [243B-GetDirNameIndex/](mon-analysis/243B-GetDirNameIndex/) | worker VERIFIED; dispatch needs rewrite |
| 244B | GetDirEntry | `006064B` | **`GDIEN=107111`** | 006-S3FS | [244B-GetDirEntry/](mon-analysis/244B-GetDirEntry/) | worker VERIFIED; dispatch needs rewrite |
| 245B | GetNameEntry | `006065B` | **`GNAEN=107114`** | 006-S3FS | [245B-GetNameEntry/](mon-analysis/245B-GetNameEntry/) | worker VERIFIED; dispatch needs rewrite |
| 246B | ReserveDir | `006066B` | **`RESDI=107401`** | 006-S3FS | [246B-ReserveDir/](mon-analysis/246B-ReserveDir/) | worker VERIFIED; dispatch needs rewrite |
| 247B | ReleaseDir | `006067B` | **`RELDI=107403`** | 006-S3FS | [247B-ReleaseDir/](mon-analysis/247B-ReleaseDir/) | worker VERIFIED; dispatch needs rewrite |
| 250B | GetDefaultDir | `006070B` | **`FDFDI=106732`** | 006-S3FS | [250B-GetDefaultDir/](mon-analysis/250B-GetDefaultDir/) | worker VERIFIED; dispatch needs rewrite |
| 251B | CopyPage | `006071B` | **`COPAG=110050`** | 006-S3FS | [251B-CopyPage/](mon-analysis/251B-CopyPage/) | worker VERIFIED; dispatch needs rewrite |
| 252B | BackupClose | `006072B` | **`BCLOS=103350`** | 006-S3FS | [252B-BackupClose/](mon-analysis/252B-BackupClose/) | worker VERIFIED; dispatch needs rewrite |
| 253B | NewFileVersion | `006073B` | **`CRALN=105560`** | 006-S3FS | [253B-NewFileVersion/](mon-analysis/253B-NewFileVersion/) | worker VERIFIED; dispatch needs rewrite |
| 254B | GetErrorDevice | `006074B` | **`GERDV=102525`** | (resident / 025-S3IRPIT) | [254B-GetErrorDevice/](mon-analysis/254B-GetErrorDevice/) | worker VERIFIED; dispatch needs rewrite |
| 255B | PIOCFunction | `006075B` | **`PIOCM=114120`** | (resident / 025-S3IRPIT) | [255B-PIOCFunction/](mon-analysis/255B-PIOCFunction/) | worker VERIFIED; dispatch needs rewrite |
| 256B | FullFileName | `006076B` | **`DEABF=111015`** | 006-S3FS | [256B-FullFileName/](mon-analysis/256B-FullFileName/) | worker VERIFIED; dispatch needs rewrite |
| 257B | OpenFileInfo | `006077B` | **`FOPFN=111212`** | 006-S3FS | [257B-OpenFileInfo/](mon-analysis/257B-OpenFileInfo/) | **WORKER CHANGED** (folder said `FOPEN=67432 (FILSYS; calls FCON=67002)`) |
| 260B | UserControl | `006100B` | **`USCNT=047120`** | (resident / 003-S3CP) | [260B-UserControl/](mon-analysis/260B-UserControl/) | worker VERIFIED; dispatch needs rewrite |
| 261B | - | `006101B` | **`SYCNT=047125`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 262B | GetSystemInfo | `006102B` | **`CPUST=063022`** | 006-S3FS | [262B-GetSystemInfo/](mon-analysis/262B-GetSystemInfo/) | worker VERIFIED; dispatch needs rewrite |
| 263B | GetDeviceType | `006103B` | **`GDEVT=107104`** | (resident / 025-S3IRPIT) | [263B-GetDeviceType/](mon-analysis/263B-GetDeviceType/) | worker VERIFIED; dispatch needs rewrite |
| 264B | - | `006104B` | **`500RF=026375`** | 006-S3FS | [264B-ND500ReadFile/](mon-analysis/264B-ND500ReadFile/) | worker VERIFIED; dispatch needs rewrite |
| 265B | - | `006105B` | **`500WF=026401`** | 006-S3FS | [265B-ND500WriteFile/](mon-analysis/265B-ND500WriteFile/) | worker VERIFIED; dispatch needs rewrite |
| 266B | - | `006106B` | **`500MT=026351`** | 006-S3FS | [266B-ND500MagTape/](mon-analysis/266B-ND500MagTape/) | worker VERIFIED; dispatch needs rewrite |
| 267B | - | `006107B` | **`TMOUT=040713`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 270B | ReadDiskPage | `006110B` | **`RDPAG=107447`** | 006-S3FS | [270B-ReadDiskPage/](mon-analysis/270B-ReadDiskPage/) | worker VERIFIED; dispatch needs rewrite |
| 271B | WriteDiskPage | `006111B` | **`WDPAG=107451`** | 006-S3FS | [271B-WriteDiskPage/](mon-analysis/271B-WriteDiskPage/) | worker VERIFIED; dispatch needs rewrite |
| 272B | DeletePage | `006112B` | **`DELPG=110472`** | 006-S3FS | [272B-DeletePage/](mon-analysis/272B-DeletePage/) | worker VERIFIED; dispatch needs rewrite |
| 273B | GetFileName | `006113B` | **`MGFIL=111013`** | 006-S3FS | [273B-GetFileName/](mon-analysis/273B-GetFileName/) | worker VERIFIED; dispatch needs rewrite |
| 274B | GetFileIndexes | `006114B` | **`FOBJN=111210`** | 006-S3FS | [274B-GetFileIndexes/](mon-analysis/274B-GetFileIndexes/) | worker VERIFIED; dispatch needs rewrite |
| 275B | SetTerminalName | `006115B` | **`SETTF=106043`** | 006-S3FS | [275B-SetTerminalName/](mon-analysis/275B-SetTerminalName/) | **WORKER CHANGED** (folder said `STRFI worker NOT LOCATED in any carved segment (`) |
| 276B | - | `006116B` | **`ELOFU=110420`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 277B | - | `006117B` | **`DLOFU=110422`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 300B | - | `006120B` | **`EUSEL=110471`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 301B | - | `006121B` | **`DUSEL=110473`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 302B | - | `006122B` | **`ELON=110302`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 303B | - | `006123B` | **`ELOFF=110362`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 304B | SendSIBASMessage | `006124B` | **`MAPSI=103675`** | (resident / 025-S3IRPIT) | [304B-SendSIBASMessage/](mon-analysis/304B-SendSIBASMessage/) | worker VERIFIED; dispatch needs rewrite |
| 305B | GetSIBASMessage | `006125B` | **`MSIBB=104221`** | (resident / 025-S3IRPIT) | [305B-GetSIBASMessage/](mon-analysis/305B-GetSIBASMessage/) | worker VERIFIED; dispatch needs rewrite |
| 306B | - | `006126B` | **`GTMOD=046716`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 307B | - | `006127B` | **`TNOWA=044746`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 311B | WriteDirEntry | `006131B` | **`WDIEN=107106`** | 006-S3FS | [311B-WriteDirEntry/](mon-analysis/311B-WriteDirEntry/) | worker VERIFIED; dispatch needs rewrite |
| 312B | CheckMonCall | `006132B` | **`MOINF=032600`** | (resident / 003-S3CP) | [312B-CheckMonCall/](mon-analysis/312B-CheckMonCall/) | worker VERIFIED; dispatch needs rewrite |
| 313B | InBufferState | `006133B` | **`IBRSI=110543`** | (resident / 025-S3IRPIT) | [313B-InBufferState/](mon-analysis/313B-InBufferState/) | worker VERIFIED; dispatch needs rewrite |
| 314B | DefaultRemoteSystem | `006134B` | **`SDRUS=111501`** | 006-S3FS | [314B-DefaultRemoteSystem/](mon-analysis/314B-DefaultRemoteSystem/) | **WORKER CHANGED** (folder said `GOTAB->DSI4=112326 (device stub); no ND-100 work`) |
| 315B | - | `006135B` | **`GETSY=064224`** | 030-S3SM5 | - | **NO FOLDER** |
| 316B | SetRemoteAccess | `006136B` | **`SLRMO=027530`** | 006-S3FS | [316B-SetRemoteAccess/](mon-analysis/316B-SetRemoteAccess/) | **WORKER CHANGED** (folder said `GOTAB->DSI5=112355 (device stub); no ND-100 work`) |
| 317B | ExecuteCommand | `006137B` | **`UECOM=050701`** | (resident / 003-S3CP) | [317B-ExecuteCommand/](mon-analysis/317B-ExecuteCommand/) | **byte-verified** |
| 320B | UELogin | `006140B` | **`UELOG=050726`** | (resident / 003-S3CP) | [320B-UELogin/](mon-analysis/320B-UELogin/) | worker VERIFIED; dispatch needs rewrite |
| 321B | UEAdministrator | `006141B` | **`UEADM=065453`** | (resident / 003-S3CP) | [321B-UEAdministrator/](mon-analysis/321B-UEAdministrator/) | worker VERIFIED; dispatch needs rewrite |
| 322B | GetSegmentNo | `006142B` | **`GSGNO=041424`** | (resident / 003-S3CP) | [322B-GetSegmentNo/](mon-analysis/322B-GetSegmentNo/) | worker VERIFIED; dispatch needs rewrite |
| 323B | - | `006143B` | **`SPLRE=071066`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 325B | - | `006145B` | **`MBECH=051476`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 326B | - | `006146B` | **`MLOGI=106465`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 327B | FileSystemFunction | `006147B` | **`MFFSC=111563`** | 006-S3FS | [327B-FileSystemFunction/](mon-analysis/327B-FileSystemFunction/) | **WORKER CHANGED** (folder said `GTYPR=113312; GOTAB->112503`) |
| 330B | - | `006150B` | **`TRTER=122606`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 331B | DiskMirroring | `006151B` | **`MSYSU=132567`** | (resident / 025-S3IRPIT) | [331B-DiskMirroring/](mon-analysis/331B-DiskMirroring/) | worker VERIFIED; dispatch needs rewrite |
| 332B | - | `006152B` | **`TREPP=106715`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 333B | DMAFunction | `006153B` | **`UDMA=110770`** | (resident / 025-S3IRPIT) | [333B-DMAFunction/](mon-analysis/333B-DMAFunction/) | worker VERIFIED; dispatch needs rewrite |
| 334B | GetErrorMessage | `006154B` | **`GETXM=107273`** | (resident / 025-S3IRPIT) | [334B-GetErrorMessage/](mon-analysis/334B-GetErrorMessage/) | worker VERIFIED; dispatch needs rewrite |
| 335B | - | `006155B` | **`DOPEN=110066`** | 030-S3SM5 | - | **NO FOLDER** |
| 336B | Terminal | `006156B` | **`IOMTY=051745`** | (resident / 003-S3CP) | [336B-Terminal/](mon-analysis/336B-Terminal/) | worker VERIFIED; dispatch needs rewrite |
| 337B | - | `006157B` | **`SPCHG=067043`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 340B | - | `006160B` | **`RSREC=037745`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 341B | - | `006161B` | **`SGMTY=066463`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 342B | - | `006162B` | **`MNADP=123534`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 343B | - | `006163B` | **`MNCFG=125205`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 344B | - | `006164B` | **`MOPER=116417`** | (resident / 025-S3IRPIT) | - | **NO FOLDER** |
| 345B | MTAFunction | `006165B` | **`MTSTA=064636`** | (resident / 025-S3IRPIT) | [345B-MTAFunction/](mon-analysis/345B-MTAFunction/) | worker VERIFIED; dispatch needs rewrite |
| 347B | - | `006167B` | **`MGDAE=047072`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 350B | - | `006170B` | **`RWLSG=047205`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 373B | - | `006213B` | **`BFY4I=047337`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 374B | - | `006214B` | **`500RD=107434`** | 006-S3FS | - | **NO FOLDER** |
| 375B | - | `006215B` | **`500WD=107436`** | 006-S3FS | - | **NO FOLDER** |
| 376B | - | `006216B` | **`5INB=032340`** | (resident / 003-S3CP) | - | **NO FOLDER** |
| 377B | - | `006217B` | **`5OUTB=032347`** | (resident / 003-S3CP) | - | **NO FOLDER** |

## ND-500 calls (unchanged - never affected by the GOTAB bug)

ND-500 calls do not go through GOTAB/MCTAB; they dispatch through their own tables and were
always byte-verified.

| MON | Name | Worker/entry | Folder |
|-----|------|--------------|--------|
| 405B | - | (ND-500 own table) | [405B-SwitchUserBreak/](mon-analysis/405B-SwitchUserBreak/) |
| 410B | - | (ND-500 own table) | [410B-FixInMemory/](mon-analysis/410B-FixInMemory/) |
| 411B | - | (ND-500 own table) | [411B-MemoryUnfix/](mon-analysis/411B-MemoryUnfix/) |
| 412B | - | (ND-500 own table) | [412B-FileAsSegment/](mon-analysis/412B-FileAsSegment/) |
| 413B | - | (ND-500 own table) | [413B-FileNotAsSegment/](mon-analysis/413B-FileNotAsSegment/) |
| 416B | - | (ND-500 own table) | [416B-SaveND500Segment/](mon-analysis/416B-SaveND500Segment/) |
| 417B | - | (ND-500 own table) | [417B-MaxPagesInMemory/](mon-analysis/417B-MaxPagesInMemory/) |
| 420B | - | (ND-500 own table) | [420B-GetUserRegisters/](mon-analysis/420B-GetUserRegisters/) |
| 422B | - | (ND-500 own table) | [422B-GetScratchSegment/](mon-analysis/422B-GetScratchSegment/) |
| 423B | - | (ND-500 own table) | [423B-CopyCapability/](mon-analysis/423B-CopyCapability/) |
| 425B | - | (ND-500 own table) | [425B-SetProcessName/](mon-analysis/425B-SetProcessName/) |
| 426B | - | (ND-500 own table) | [426B-GetProcessNo/](mon-analysis/426B-GetProcessNo/) |
| 427B | - | (ND-500 own table) | [427B-GetOwnProcessInfo/](mon-analysis/427B-GetOwnProcessInfo/) |
| 432B | - | (ND-500 own table) | [432B-SIBASFunction/](mon-analysis/432B-SIBASFunction/) |
| 436B | - | (ND-500 own table) | [436B-SetND500Param/](mon-analysis/436B-SetND500Param/) |
| 437B | - | (ND-500 own table) | [437B-GetND500Param/](mon-analysis/437B-GetND500Param/) |
| 500B | - | (ND-500 own table) | [500B-StartProcess/](mon-analysis/500B-StartProcess/) |
| 501B | - | (ND-500 own table) | [501B-StopProcess/](mon-analysis/501B-StopProcess/) |
| 503B | - | (ND-500 own table) | [503B-InputString/](mon-analysis/503B-InputString/) |
| 504B | - | (ND-500 own table) | [504B-OutputString/](mon-analysis/504B-OutputString/) |
| 505B | - | (ND-500 own table) | [505B-GetTrapReason/](mon-analysis/505B-GetTrapReason/) |
| 506B | - | (ND-500 own table) | [506B-AnswerSIBAS/](mon-analysis/506B-AnswerSIBAS/) |
| 510B | - | (ND-500 own table) | [510B-CallSwapper/](mon-analysis/510B-CallSwapper/) |
| 511B | - | (ND-500 own table) | [511B-DVIO/](mon-analysis/511B-DVIO/) |
| 512B | - | (ND-500 own table) | [512B-XMSGCallA/](mon-analysis/512B-XMSGCallA/) |
| 513B | - | (ND-500 own table) | [513B-XMSGCallB/](mon-analysis/513B-XMSGCallB/) |
| 514B | - | (ND-500 own table) | [514B-ND500TimeOut/](mon-analysis/514B-ND500TimeOut/) |
| 515B | - | (ND-500 own table) | [515B-MultipleDataTransfer/](mon-analysis/515B-MultipleDataTransfer/) |

---

## Coverage

- MON calls implemented in L07 (non-zero `MCTAB` slots): **216**
- Of those, have a folder: **122** ; **no folder yet: 94**
- Folders fully rebuilt on the corrected model: **1** (317B)

**Folders for MON numbers that are NOT in MCTAB** (i.e. `MCTAB[N] = 0`, so not implemented in
this build): `155B, 175B, 176B, 177B, 324B`. These ND-100 folders may be documenting nothing -
they need auditing. (`200B` is legitimately absent from MCTAB: it is a GOTAB *fast-path* call.)

