# SINTRAN III Monitor Calls Reference

**Complete documentation for all 230 monitor calls**

*Extracted from: SINTRAN III Monitor Calls (ND-860228.2 EN)*

---

## Table of Contents

- [Index by Octal Number](#index-by-octal-number)
- [Index by Call Name](#index-by-call-name)
- [Index by Short Name](#index-by-short-name)
- [Monitor Call Documentation](#monitor-call-documentation)

---

## Index by Octal Number

| Octal | Name | Short Name | Description |
|-------|------|------------|-------------|
| [0B](#0b-exitfromprogram-leave) | [ExitFromProgram](#0b-exitfromprogram-leave) | LEAVE | Terminates the program. Returns to SINTRAN III. Batch jobs c... |
| [1B](#1b-inbyte-inbt) | [InByte](#1b-inbyte-inbt) | INBT | Reads one byte from a character device, e.g. a terminal or a... |
| [2B](#2b-outbyte-outbt) | [OutByte](#2b-outbyte-outbt) | OUTBT | Writes one byte to a character device, e.g. a terminal or an... |
| [3B](#3b-setecho-echom) | [SetEcho](#3b-setecho-echom) | ECHOM | When you press a key on the terminal, a character is normall... |
| [4B](#4b-setbreak-brkm) | [SetBreak](#4b-setbreak-brkm) | BRKM | Sets the break characters for a terminal. Normally, a progra... |
| [5B](#5b-readscratchfile-rdisk) | [ReadScratchFile](#5b-readscratchfile-rdisk) | RDISK | Reads randomly from the scratch file. One block is transferr... |
| [6B](#6b-writescratchfile-wdisk) | [WriteScratchFile](#6b-writescratchfile-wdisk) | WDISK | Writes randomly to the scratch file. One block is transferre... |
| [7B](#7b-readblock-rpage) | [ReadBlock](#7b-readblock-rpage) | RPAGE | Reads randomly from a file. You read one block at a time. Th... |
| [10B](#10b-writeblock-wpage) | [WriteBlock](#10b-writeblock-wpage) | WPAGE | Writes randomly to a file. You write one block at a time. Th... |
| [11B](#11b-getbasictime-time) | [GetBasicTime](#11b-getbasictime-time) | TIME | **Time**... |
| [12B](#12b-setcommandbuffer-setcm) | [SetCommandBuffer](#12b-setcommandbuffer-setcm) | SETCM | Transfers a string to the command buffer. The command buffer... |
| [13B](#13b-clearinbuffer-cibuf) | [ClearInBuffer](#13b-clearinbuffer-cibuf) | CIBUF | Clears a device input buffer. Input from character devices, ... |
| [14B](#14b-clearoutbuffer-cobuf) | [ClearOutBuffer](#14b-clearoutbuffer-cobuf) | COBUF | Clears a device output buffer. Output to character devices, ... |
| [16B](#16b-getterminaltype-mgtty) | [GetTerminalType](#16b-getterminaltype-mgtty) | MGTTY | Gets the terminal type. The terminal type tells SINTRAN III ... |
| [17B](#17b-setterminaltype-mstty) | [SetTerminalType](#17b-setterminaltype-mstty) | MSTTY | Sets the type of a terminal. The terminal type tells SINTRAN... |
| [21B](#21b-inupto8bytes-m8inb) | [InUpTo8Bytes](#21b-inupto8bytes-m8inb) | M8INB | See also In8Bytes, InByte, InString, In4x2Bytes, and Out8Byt... |
| [22B](#22b-outupto8bytes-m8out) | [OutUpTo8Bytes](#22b-outupto8bytes-m8out) | M8OUT | Writes up to 8 characters to a device, e.g. a terminal or an... |
| [23B](#23b-in8bytes-b8inb) | [In8Bytes](#23b-in8bytes-b8inb) | B8INB | Reads 8 bytes from a device. The input is fast, but the moni... |
| [24B](#24b-out8bytes-b8out) | [Out8Bytes](#24b-out8bytes-b8out) | B8OUT | Writes 8 bytes to a character device, e.g. a terminal. All 8... |
| [26B](#26b-getlastbyte-lastc) | [GetLastByte](#26b-getlastbyte-lastc) | LASTC | Gets the last character typed on a terminal. The monitor cal... |
| [27B](#27b-getrtdescr-rtdsc) | [GetRTDescr](#27b-getrtdescr-rtdsc) | RTDSC | Reads an RT description. The RT description contains various... |
| [30B](#30b-getownrtaddress-getrt) | [GetOwnRTAddress](#30b-getownrtaddress-getrt) | GETRT | Gets the address of the calling program's RT description. Ba... |
| [31B](#31b-ioinstruction-exiox) | [IOInstruction](#31b-ioinstruction-exiox) | EXIOX | Executes an IOX machine instruction. The IOX instruction han... |
| [32B](#32b-outmessage-msg) | [OutMessage](#32b-outmessage-msg) | MSG | Writes a message to the user's terminal. This is convenient ... |
| [33B](#33b-altpagetable-alton) | [AltPageTable](#33b-altpagetable-alton) | ALTON | Switches page table. Each page table allows you to access 12... |
| [34B](#34b-normalpagetable-altoff) | [NormalPageTable](#34b-normalpagetable-altoff) | ALTOFF | **ALTOFF** Sets the alternative page table equal to the norm... |
| [35B](#35b-outnumber-iout) | [OutNumber](#35b-outnumber-iout) | IOUT | Writes a number to the user's terminal. The number can be ou... |
| [36B](#36b-nowaitswitch-nowt) | [NoWaitSwitch](#36b-nowaitswitch-nowt) | NOWT | Switches No Wait on and off. No Wait is useful for input fro... |
| [37B](#37b-readadchannel-airdw) | [ReadADChannel](#37b-readadchannel-airdw) | AIRDW | Reads an analog to digital channel.... |
| [40B](#40b-closespoolingfile-spclo) | [CloseSpoolingFile](#40b-closespoolingfile-spclo) | SPCLO | Appends an opened file to a spooling queue. You specify a te... |
| [41B](#41b-readobjectentry-robje) | [ReadObjectEntry](#41b-readobjectentry-robje) | ROBJE | Gets information about an opened file. An object entry descr... |
| [43B](#43b-closefile-close) | [CloseFile](#43b-closefile-close) | CLOSE | Closes one or more files. Files must be opened before they a... |
| [44B](#44b-getuserentry-ruser) | [GetUserEntry](#44b-getuserentry-ruser) | RUSER | Gets information about a user. The user entry in the directo... |
| [50B](#50b-openfile-open) | [OpenFile](#50b-openfile-open) | OPEN | Opens a file. You cannot access a file before you open it. S... |
| [52B](#52b-terminalmode-termo) | [TerminalMode](#52b-terminalmode-termo) | TERMO | Selects various terminal functions. You may stop output on f... |
| [53B](#53b-getsegmententry-rsegm) | [GetSegmentEntry](#53b-getsegmententry-rsegm) | RSEGM | Gets information about a segment in the ND-100. The monitor ... |
| [54B](#54b-deletefile-mdlfi) | [DeleteFile](#54b-deletefile-mdlfi) | MDLFI | Deletes a file. The pages of the file are released.... |
| [55B](#55b-getspoolingentry-rsqpe) | [GetSpoolingEntry](#55b-getspoolingentry-rsqpe) | RSQPE | Gets the next spooling queue entry, that is, the next file t... |
| [56B](#56b-setuserparam-paset) | [SetUserParam](#56b-setuserparam-paset) | PASET | Sets information about a background program. Use GetUserPara... |
| [57B](#57b-getuserparam-pagei) | [GetUserParam](#57b-getuserparam-pagei) | PAGEI | Gets information about why the last program terminated. Ther... |
| [61B](#61b-memoryallocation-fixcs) | [MemoryAllocation](#61b-memoryallocation-fixcs) | FIXCS | Fixes or unfixes ND-100 segments to be used by the ND-500 Mo... |
| [62B](#62b-getbytesinfile-rmax) | [GetBytesInFile](#62b-getbytesinfile-rmax) | RMAX | Gets the number of bytes in a file. Only the bytes containin... |
| [63B](#63b-in4x2bytes-b41nw) | [In4x2Bytes](#63b-in4x2bytes-b41nw) | B41NW | Reads 8 bytes from a word-oriented or character-oriented dev... |
| [64B](#64b-warningmessage-ermsg) | [WarningMessage](#64b-warningmessage-ermsg) | ERMSG | Outputs a file system error message. Appendix A shows the me... |
| [65B](#65b-errormessage-qerms) | [ErrorMessage](#65b-errormessage-qerms) | QERMS | Displays a file system error message. Appendix A shows the m... |
| [66B](#66b-inbufferspace-isize) | [InBufferSpace](#66b-inbufferspace-isize) | ISIZE | Gets the current number of bytes in the input buffer. Termin... |
| [67B](#67b-outbufferspace-osize) | [OutBufferSpace](#67b-outbufferspace-osize) | OSIZE | Gets the number of free bytes in the output buffer (number o... |
| [70B](#70b-callcommand-commnd) | [CallCommand](#70b-callcommand-commnd) | COMMND | Executes a SINTRAN III command from a program. The program t... |
| [71B](#71b-disableescape-descf) | [DisableEscape](#71b-disableescape-descf) | DESCF | The ESCAPE key on the terminal normally terminates a program... |
| [72B](#72b-enableescape-eescf) | [EnableEscape](#72b-enableescape-eescf) | EESCF | Enables the ESCAPE key on the terminal. The ESCAPE key norma... |
| [73B](#73b-setmaxbytes-smax) | [SetMaxBytes](#73b-setmaxbytes-smax) | SMAX | Sets the value of the maximum byte pointer in an opened file... |
| [74B](#74b-setstartbyte-setbt) | [SetStartByte](#74b-setstartbyte-setbt) | SETBT | Sets the next byte to be read or written in an opened mass-s... |
| [75B](#75b-getstartbyte-reabt) | [GetStartByte](#75b-getstartbyte-reabt) | REABT | Gets the number of the next byte to access in a file. The by... |
| [76B](#76b-setblocksize-setbs) | [SetBlockSize](#76b-setblocksize-setbs) | SETBS | Sets the block size of an opened file. Monitor calls which r... |
| [77B](#77b-setstartblock-setbl) | [SetStartBlock](#77b-setstartblock-setbl) | SETBL | Sets the next block to be read or written in an opened file.... |
| [100B](#100b-startrtprogram-rt) | [StartRTProgram](#100b-startrtprogram-rt) | RT | Starts an RT program. The program is moved to the execution ... |
| [101B](#101b-delaystart-set) | [DelayStart](#101b-delaystart-set) | SET | Starts an RT program after a specified time. The RT program ... |
| [102B](#102b-startuptime-abset) | [StartupTime](#102b-startuptime-abset) | ABSET | Starts an RT program at a specified time of the day. The RT ... |
| [103B](#103b-startupinterval-intv) | [StartupInterval](#103b-startupinterval-intv) | INTV | Prepares an RT program for periodic execution. The interval ... |
| [104B](#104b-suspendprogram-hold) | [SuspendProgram](#104b-suspendprogram-hold) | HOLD | Suspends the execution of your program for a given time. The... |
| [105B](#105b-stoprtprogram-abort) | [StopRTProgram](#105b-stoprtprogram-abort) | ABORT | Stops an RT program. It is removed from the time or executio... |
| [106B](#106b-startoninterrupt-conct) | [StartOnInterrupt](#106b-startoninterrupt-conct) | CONCT | StartOnInterrupt connects an RT program to interrupts from a... |
| [107B](#107b-nointerruptstart-dscnt) | [NoInterruptStart](#107b-nointerruptstart-dscnt) | DSCNT | StartOnInterrupt connects an RT program to interrupts from a... |
| [110B](#110b-setrtpriority-prior) | [SetRTPriority](#110b-setrtpriority-prior) | PRIOR | Sets the priority of an RT program. RT programs may be given... |
| [111B](#111b-setclock-updat) | [SetClock](#111b-setclock-updat) | UPDAT | Gives new values to the computer's clock and calendar. If th... |
| [112B](#112b-adjustclock-cladj) | [AdjustClock](#112b-adjustclock-cladj) | CLADJ | Sets the computer's clock (i.e. the current system time) for... |
| [113B](#113b-getcurrenttime-clock) | [GetCurrentTime](#113b-getcurrenttime-clock) | CLOCK | Gets the current system time and date.... |
| [114B](#114b-gettimeused-tused) | [GetTimeUsed](#114b-gettimeused-tused) | TUSED | Gets the time you have used the CPU since you logged in. In ... |
| [115B](#115b-fixscattered-fix) | [FixScattered](#115b-fixscattered-fix) | FIX | Place a segment in physical memory. Its pages will no longer... |
| [116B](#116b-unfixsegment-unfix) | [UnfixSegment](#116b-unfixsegment-unfix) | UNFIX | Releases a fixed segment and removes it from the Page Index ... |
| [117B](#117b-readfromfile-rfile) | [ReadFromFile](#117b-readfromfile-rfile) | RFILE | Reads any number of bytes from a file. The read operation mu... |
| [120B](#120b-writetofile-wfile) | [WriteToFile](#120b-writetofile-wfile) | WFILE | Writes any number of bytes to a file. The read operation mus... |
| [121B](#121b-awaitfiletransfer-waitf) | [AwaitFileTransfer](#121b-awaitfiletransfer-waitf) | WAITF | Checks that a data transfer to or from a mass-storage file i... |
| [122B](#122b-reserveresource-resrv) | [ReserveResource](#122b-reserveresource-resrv) | RESRV | Reserves a device or file for your program only. You release... |
| [123B](#123b-releaseresource-reles) | [ReleaseResource](#123b-releaseresource-reles) | RELES | Releases a reserved device or file. The resource can then be... |
| [124B](#124b-forcereserve-prsrv) | [ForceReserve](#124b-forcereserve-prsrv) | PRSRV | Reserves a device for an RT program other than that which is... |
| [125B](#125b-forcerelease-prlrs) | [ForceRelease](#125b-forcerelease-prlrs) | PRLRS | Releases a device reserved by an RT program other than that ... |
| [126B](#126b-exactdelaystart-dset) | [ExactDelayStart](#126b-exactdelaystart-dset) | DSET | Sets an RT program to start after a given period. It is then... |
| [127B](#127b-exactstartup-dabst) | [ExactStartup](#127b-exactstartup-dabst) | DABST | Starts an RT program at a specific time. The time is given i... |
| [130B](#130b-exactinterval-dintv) | [ExactInterval](#130b-exactinterval-dintv) | DINTV | Prepares an RT program for periodic execution. The interval ... |
| [131B](#131b-datatransfer-abstr) | [DataTransfer](#131b-datatransfer-abstr) | ABSTR | Transfers data between physical memory and a mass-storage de... |
| [132B](#132b-jumptosegment-mcall) | [JumpToSegment](#132b-jumptosegment-mcall) | MCALL | Calls a routine on another segment in the ND-100. You can di... |
| [133B](#133b-exitfromsegment-mexit) | [ExitFromSegment](#133b-exitfromsegment-mexit) | MEXIT | Exchanges one or both current segments. Commonly used to ret... |
| [134B](#134b-exitrtprogram-rtext) | [ExitRTProgram](#134b-exitrtprogram-rtext) | RTEXT | Terminates the calling RT or background program. Releases al... |
| [135B](#135b-waitforrestart-rtwt) | [WAITFORRESTART](#135b-waitforrestart-rtwt) | RTWT | Sets the RT program in a waiting state. It is restarted by S... |
| [136B](#136b-enablertstart-rton) | [EnableRTStart](#136b-enablertstart-rton) | RTON | RTON RT programs cannot be started after DisableRTStart has ... |
| [137B](#137b-disablertstart-rtoff) | [DisableRTStart](#137b-disablertstart-rtoff) | RTOFF | Disables start of RT programs. No RT program can be started ... |
| [140B](#140b-reservationinfo-whdev) | [ReservationInfo](#140b-reservationinfo-whdev) | WHDEV | Checks that a device is not reserved. If it is reserved, you... |
| [141B](#141b-devicecontrol-ioset) | [DeviceControl](#141b-devicecontrol-ioset) | IOSET | Sets control information for a character device, e.g. a term... |
| [142B](#142b-toerrordevice-ermon) | [ToErrorDevice](#142b-toerrordevice-ermon) | ERMON | Outputs a user-defined, real-time error. The error message i... |
| [143B](#143b-executioninfo-rsio) | [ExecutionInfo](#143b-executioninfo-rsio) | RSIO | Gets information about the execution of the calling program.... |
| [144B](#144b-devicefunction-magtp) | [DeviceFunction](#144b-devicefunction-magtp) | MAGTP | Performs various operations on floppy disks, magnetic tapes,... |
| [146B](#146b-privinstruction-ipriv) | [PrivInstruction](#146b-privinstruction-ipriv) | IPRIV | Executes a privileged machine instruction on the ND-100. Pri... |
| [147B](#147b-camacfunction-camac) | [CAMACFunction](#147b-camacfunction-camac) | CAMAC | Operates the CAMAC, i.e. executes the NAF register. CAMAC is... |
| [150B](#150b-camacglregister-gl) | [CAMACGLRegister](#150b-camacglregister-gl) | GL | Read the CAMAC GL (Graded LAM -"look at me") register or the... |
| [151B](#151b-getrtaddress-grtda) | [GetRTAddress](#151b-getrtaddress-grtda) | GRTDA | Gets the address of an RT description. You specify the name ... |
| [152B](#152b-getrtname-grtna) | [GetRTName](#152b-getrtname-grtna) | GRTNA | Gets the name of an RT program. You specify the RT descripti... |
| [153B](#153b-camacioinstruction-ioxin) | [CAMACIOInstruction](#153b-camacioinstruction-ioxin) | IOXIN | Executes a single IOX instruction for CAMAC. See under CAMAC... |
| [154B](#154b-assigncamaclam-assig) | [AssignCAMACLAM](#154b-assigncamaclam-assig) | ASSIG | Assigns a graded LAM in the CAMAC identification table to a ... |
| [155B](#155b-graphicfunction-graph) | [GRAPHICFUNCTION](#155b-graphicfunction-graph) | GRAPH | Executes various functions on a graphic peripheral, such as ... |
| [157B](#157b-segmenttopagetable-entsg) | [SegmentToPageTable](#157b-segmenttopagetable-entsg) | ENTSG | Enters a routine as a direct task or as a device driver, and... |
| [160B](#160b-fixcontiguous-fixc) | [FixContiguous](#160b-fixcontiguous-fixc) | FIXC | Places a segment in physical memory. Its pages will no longe... |
| [161B](#161b-instring-instr) | [INSTRING](#161b-instring-instr) | INSTR | Reads a string of characters from a peripheral device, e.g. ... |
| [162B](#162b-outstring-outst) | [OutString](#162b-outstring-outst) | OUTST | Writes a string of characters to a peripheral file, e.g., a ... |
| [164B](#164b-savesegment-wseg) | [SaveSegment](#164b-savesegment-wseg) | WSEG | Saves a segment in the ND-100. All pages in physical memory ... |
| [165B](#165b-getinregisters-diw) | [GETINREGISTERS](#165b-getinregisters-diw) | DIW | Reads the device interface registers.... |
| [167B](#167b-attachsegment-reent) | [AttachSegment](#167b-attachsegment-reent) | REENT | Attaches a reentrant segment to your two current segments. T... |
| [170B](#170b-userdef0-us0) | [UserDef0](#170b-userdef0-us0) | US0 | User-defined monitor call. You can implement up to 8 monitor... |
| [171B](#171b-userdef1-us1) | [UserDef1](#171b-userdef1-us1) | US1 | User-defined monitor call. You can implement up to 8 monitor... |
| [172B](#172b-userdef2-us2) | [UserDef2](#172b-userdef2-us2) | US2 | User-defined monitor call. You can implement up to 8 monitor... |
| [173B](#173b-userdef3-us3) | [UserDef3](#173b-userdef3-us3) | US3 | User-defined monitor call. You can implement up to 8 monitor... |
| [174B](#174b-userdef4-us4) | [UserDef4](#174b-userdef4-us4) | US4 | User-defined monitor call. You can implement up to 8 monitor... |
| [175B](#175b-userdef5-us5) | [UserDef5](#175b-userdef5-us5) | US5 | User-defined monitor call. You can implement up to 8 monitor... |
| [176B](#176b-userdef6-us6) | [UserDef6](#176b-userdef6-us6) | US6 | User-defined monitor call. You can implement up to 8 monitor... |
| [177B](#177b-userdef7-us7) | [UserDef7](#177b-userdef7-us7) | US7 | User-defined monitor call. You can implement up to 8 monitor... |
| [200B](#200b-xmsgfunction-xmsg) | [XMSGFunction](#200b-xmsgfunction-xmsg) | XMSG | Performs various data communication functions. All types of ... |
| [201B](#201b-hdlcfunction-mhdlc) | [HDLCfunction](#201b-hdlcfunction-mhdlc) | MHDLC | Performs various HDLC functions. A HDLC is a high-level data... |
| [206B](#206b-terminationhandling-edtmp) | [TerminationHandling](#206b-terminationhandling-edtmp) | EDTMP | Switches termination handling on and off.... |
| [207B](#207b-geterrorinfo-rerrp) | [GetErrorInfo](#207b-geterrorinfo-rerrp) | RERRP | Gets information about the last real-time error. The monitor... |
| [212B](#212b-reentrantsegment-sreen) | [ReentrantSegment](#212b-reentrantsegment-sreen) | SREEN | Connects a reentrant segment to your two current segments. A... |
| [213B](#213b-getdiruserindexes-muidi) | [GetDirUserIndexes](#213b-getdiruserindexes-muidi) | MUIDI | Gets a directory index and a user index. You have to specify... |
| [214B](#214b-getusername-gusna) | [GetUserName](#214b-getusername-gusna) | GUSNA | Gets the name of a user. The user may be on a remote compute... |
| [215B](#215b-getobjectentry-drobj) | [GetObjectEntry](#215b-getobjectentry-drobj) | DROBJ | Gets information about a file. An object entry describes eac... |
| [216B](#216b-setobjectentry-dwobj) | [SetObjectEntry](#216b-setobjectentry-dwobj) | DWOBJ | Changes the description of a file. An object entry describes... |
| [217B](#217b-getallfileindexes-guioi) | [GetAllFileIndexes](#217b-getallfileindexes-guioi) | GUIOI | Gets the directory index, the user index, and the object ind... |
| [220B](#220b-directopen-dopen) | [DirectOpen](#220b-directopen-dopen) | DOPEN | Opens a file. Files must be opened before they can be access... |
| [221B](#221b-createfile-cralf) | [CreateFile](#221b-createfile-cralf) | CRALF | Creates a file. The file may be indexed, contiguous, or allo... |
| [222B](#222b-getaddressarea-gbsiz) | [GetAddressArea](#222b-getaddressarea-gbsiz) | GBSIZ | Gets the size of your address area. Your address area may co... |
| [227B](#227b-setesclocalchars-msdae) | [SetEscLocalChars](#227b-setesclocalchars-msdae) | MSDAE | You can terminate most programs with the ESCAPE key. A LOCAL... |
| [230B](#230b-getesclocalchars-mgdae) | [GEtEscLocalChars](#230b-getesclocalchars-mgdae) | MGDAE | Gets ESCAPE and LOCAL characters. You can terminate most pro... |
| [231B](#231b-expandfile-expfi) | [ExpandFile](#231b-expandfile-expfi) | EXPFI | Expands the file size. You use this monitor call to increase... |
| [232B](#232b-renamefile-mrnfi) | [RenameFile](#232b-renamefile-mrnfi) | MRNFI | See also @RENAME-FILE.... |
| [233B](#233b-settemporaryfile-stefi) | [SetTemporaryFile](#233b-settemporaryfile-stefi) | STEFI | Defines a file to store information temporarily. The file ca... |
| [234B](#234b-setperipheralname-spefi) | [SetPeripheralName](#234b-setperipheralname-spefi) | SPEFI | Defines a peripheral file, e.g. a printer. You connect a fil... |
| [235B](#235b-scratchopen-scrop) | [ScratchOpen](#235b-scratchopen-scrop) | SCROP | Opens a file as a scratch file. A maximum of 64 pages of the... |
| [236B](#236b-setpermanentopen-sperd) | [SetPermanentOpen](#236b-setpermanentopen-sperd) | SPERD | Sets a file permanently open. The file is not closed by Clos... |
| [237B](#237b-setfileaccess-sfacc) | [SetFileAccess](#237b-setfileaccess-sfacc) | SFACC | Sets the access protection for a file. You should specify th... |
| [240B](#240b-appendspooling-apspe) | [AppendSpooling](#240b-appendspooling-apspe) | APSPE | Prints a file. The printer has a queue of files waiting to b... |
| [241B](#241b-newuser-suscn) | [NewUser](#241b-newuser-suscn) | SUSCN | Switches the user name you are logged in under. The command ... |
| [242B](#242b-olduser-ruscn) | [OldUser](#242b-olduser-ruscn) | RUSCN | Switches back to the user name you were logged in under befo... |
| [243B](#243b-getdirnameindex-fdina) | [GetDirNameIndex](#243b-getdirnameindex-fdina) | FDINA | Gets directory index and name index. The name index identifi... |
| [244B](#244b-getdirentry-gdien) | [GetDirEntry](#244b-getdirentry-gdien) | GDIEN | Gets information about a directory. The directory entry is r... |
| [245B](#245b-getnameentry-gnaen) | [GetNameEntry](#245b-getnameentry-gnaen) | GNAEN | Gets information about devices, e.g. disks and floppy disks.... |
| [246B](#246b-reservedir-redir) | [ReserveDir](#246b-reservedir-redir) | REDIR | Reserves a directory for special use. The directory must be ... |
| [247B](#247b-releasedir-rldir) | [ReleaseDir](#247b-releasedir-rldir) | RLDIR | Releases a directory. The directory must have been reserved ... |
| [250B](#250b-getdefaultdir-fdfdi) | [GetDefaultDir](#250b-getdefaultdir-fdfdi) | FDFDI | Gets the user’s default directory. The directory index and t... |
| [251B](#251b-copypage-copag) | [CopyPage](#251b-copypage-copag) | COPAG | Copies file pages between two opened files. One of the files... |
| [252B](#252b-backupclose-bclos) | [BackupClose](#252b-backupclose-bclos) | BCLOS | Closes a file. The version number and the last date accessed... |
| [253B](#253b-newfileversion-craln) | [NewFileVersion](#253b-newfileversion-craln) | CRALN | Creates new versions of a file. You may create new versions ... |
| [254B](#254b-geterrordevice-gerdv) | [GetErrorDevice](#254b-geterrordevice-gerdv) | GERDV | Gets the logical device number of the error device. The erro... |
| [255B](#255b-pioccfunction-piocm) | [PIOCCFunction](#255b-pioccfunction-piocm) | PIOCM | PIOCC (Programmed Input/Output Control Channel) function mon... |
| [256B](#256b-fullfilename-deabf) | [FullFileName](#256b-fullfilename-deabf) | DEABF | Returns a complete file name from an abbreviated one. The di... |
| [257B](#257b-openfileinfo-fopen) | [OpenFileInfo](#257b-openfileinfo-fopen) | FOPEN | Monitor call 257B - OpenFileInfo... |
| [262B](#262b-getsysteminfo-cpust) | [GetSystemInfo](#262b-getsysteminfo-cpust) | CPUST | Gets various system information. The system number, the CPU ... |
| [263B](#263b-getdevicetype-gdevt) | [GetDeviceType](#263b-getdevicetype-gdevt) | GDEVT | Gets the device type, e.g. terminal, floppy disk, mass-stora... |
| [267B](#267b-timeout-tmout) | [TimeOut](#267b-timeout-tmout) | TMOUT | Suspends the execution of your program for a given time. The... |
| [270B](#270b-readdiskpage-rdpag) | [ReadDiskPage](#270b-readdiskpage-rdpag) | RDPAG | Reads one or more directory pages. Any page can be read.... |
| [271B](#271b-writediskpage-wdpag) | [WriteDiskPage](#271b-writediskpage-wdpag) | WDPAG | Writes to one or more pages in a directory. Any page can be ... |
| [272B](#272b-deletepage-delpg) | [DeletePage](#272b-deletepage-delpg) | DELPG | Deletes pages from a file. Pages between two page numbers ar... |
| [273B](#273b-getfilename-mgfil) | [GetFileName](#273b-getfilename-mgfil) | MGFIL | Gets the name of a file. You specify the directory index, th... |
| [274B](#274b-getfileindexes-fobjn) | [GetFileIndexes](#274b-getfileindexes-fobjn) | FOBJN | Gets the directory index, the user index, and the object ind... |
| [275B](#275b-setterminalname-strfi) | [SetTerminalName](#275b-setterminalname-strfi) | STRFI | Defines the file name to be used for terminals. This is norm... |
| [276B](#276b-enablelocal-elofu) | [EnableLocal](#276b-enablelocal-elofu) | ELOFU | You may log in on remote computers through the COSMOS data n... |
| [277B](#277b-disablelocal-dlofu) | [DISABLELOCAL](#277b-disablelocal-dlofu) | DLOFU | You may log in on remote computers through the COSMOS data n... |
| [300B](#300b-setescapehandling-eusel) | [SetEscapeHandling](#300b-setescapehandling-eusel) | EUSEL | Enables user-defined escape handling. When the ESCAPE key is... |
| [301B](#301b-stopescapehandling-dusel) | [StopEscapeHandling](#301b-stopescapehandling-dusel) | DUSEL | Disables user-defined escape handling. The ESCAPE key termin... |
| [302B](#302b-onesclocalfunction-elon) | [ONESCLOCALFUNCTION](#302b-onesclocalfunction-elon) | ELON | Enables delayed escape and local functions for your terminal... |
| [303B](#303b-offesclocalfunction-eloff) | [OffEscLocalFunction](#303b-offesclocalfunction-eloff) | ELOFF | Delays the escape and local functions for your terminal. The... |
| [306B](#306b-getterminalmode-gtmod) | [GetTerminalMode](#306b-getterminalmode-gtmod) | GTMOD | Gets the terminal mode. The terminal mode tells how the term... |
| [307B](#307b-terminalnowait-tnowai) | [TerminalNoWait](#307b-terminalnowait-tnowai) | TNOWAI | Switches No Wait on and off. No Wait is useful for input fro... |
| [310B](#310b-in8andflag-tbin8) | [In8AndFlag](#310b-in8andflag-tbin8) | TBIN8 | Reads 8 bytes from a device, e.g., a terminal. The monitor c... |
| [311B](#311b-writedirentry-wdien) | [WriteDirEntry](#311b-writedirentry-wdien) | WDIEN | Changes the information about a directory. The complete cont... |
| [312B](#312b-checkmoncall-moinf) | [CheckMonCall](#312b-checkmoncall-moinf) | MOINF | Some monitor calls are optional or only available in later v... |
| [313B](#313b-inbufferstate-ibrisz) | [InBufferState](#313b-inbufferstate-ibrisz) | IBRISZ | Gets information about an input buffer. The current number o... |
| [314B](#314b-defaultremotesystem-srusi) | [DefaultRemoteSystem](#314b-defaultremotesystem-srusi) | SRUSI | Sets default values for COSMOS remote file access. You can s... |
| [315B](#315b-lamufunction-mlamu) | [LAMUFunction](#315b-lamufunction-mlamu) | MLAMU | Performs various functions on the LAMU system. A LAMU is a l... |
| [316B](#316b-setremoteaccess-srlmo) | [SetRemoteAccess](#316b-setremoteaccess-srlmo) | SRLMO | Switches remote file access on and off. The COSMOS network a... |
| [317B](#317b-executecommand-uecom) | [ExecuteCommand](#317b-executecommand-uecom) | UECOM | Executes a SINTRAN III command. Specify the command name and... |
| [322B](#322b-getsegmentno-gsgno) | [GetSegmentNo](#322b-getsegmentno-gsgno) | GSGNO | Gets the number of a segment in the ND-100. You specify the ... |
| [323B](#323b-segmentoverlay-splre) | [SegmentOverlay](#323b-segmentoverlay-splre) | SPLRE | Used to build multisegment programs in the ND-100. It is mai... |
| [324B](#324b-octobusfunction-octio) | [OctobusFunction](#324b-octobusfunction-octio) | OCTIO | Performs various functions on an old Octobus (earlier than v... |
| [325B](#325b-batchmodeecho-mbech) | [BATCHMODEECHO](#325b-batchmodeecho-mbech) | MBECH | Controls echo of input and output if the program is executed... |
| [326B](#326b-loginstart-mlogi) | [LogInStart](#326b-loginstart-mlogi) | MLOGI | Allowed in RT programs (ring ≥ 1), and in all programs run b... |
| [327B](#327b-filesystemfunction-fsmty) | [FileSystemFunction](#327b-filesystemfunction-fsmty) | FSMTY | Multifunction monitor call to make sure that an uncontrolled... |
| [330B](#330b-terminalstatus-terst) | [TerminalStatus](#330b-terminalstatus-terst) | TERST | Gets information about a terminal. The user logged in, the t... |
| [332B](#332b-terminallineinfo-trepp) | [TerminalLineInfo](#332b-terminallineinfo-trepp) | TREPP | Gets information about a terminal line. You may also enable ... |
| [333B](#333b-dmafunction-udma) | [DMAFunction](#333b-dmafunction-udma) | UDMA | Monitor call 333B - DMAFunction... |
| [334B](#334b-geterrormessage-getxm) | [GetErrorMessage](#334b-geterrormessage-getxm) | GETXM | Gets a SINTRAN III error message text. Appendix A shows the ... |
| [335B](#335b-transferdata-exabs) | [TRANSFERDATA](#335b-transferdata-exabs) | EXABS | Transfers data between physical memory and a mass-storage de... |
| [336B](#336b-terminal-iomty) | [TERMINAL](#336b-terminal-iomty) | IOMTY | This I/O multifunction monitor call is used to change the at... |
| [337B](#337b-changesegment-spchg) | [ChangeSegment](#337b-changesegment-spchg) | SPCHG | Changes the segment and the page table your program uses. Th... |
| [340B](#340b-readsystemrecord-rsrec) | [READSYSTEMRECORD](#340b-readsystemrecord-rsrec) | RSREC | Used to read the system record into a buffer.... |
| [341B](#341b-segmentfunction-sgmty) | [SegmentFunction](#341b-segmentfunction-sgmty) | SGMTY | This is a multifunction monitor call used to change the acti... |
| [400B](#400b-errorreturn-macroe) | [ErrorReturn](#400b-errorreturn-macroe) | MACROE | Terminates the program and sets an error code. The error cod... |
| [401B](#401b-disassemble-diass) | [DisAssemble](#401b-disassemble-diass) | DIASS | Disassembles one machine instruction on the ND-500. Output i... |
| [402B](#402b-getinputflags-rflag) | [GetInputFlags](#402b-getinputflags-rflag) | RFLAG | ND-100 and ND-500 programs may communicate through two 32-bi... |
| [403B](#403b-setoutputflags-wflag) | [SetOutputFlags](#403b-setoutputflags-wflag) | WFLAG | ND-100 and ND-500 programs may communicate through two 32-bi... |
| [404B](#404b-fixioarea-iofix) | [FixIOArea](#404b-fixioarea-iofix) | IOFIX | Fixes an address area in a domain in physical memory. The me... |
| [405B](#405b-switchuserbreak-ustrk) | [SwitchUserBreak](#405b-switchuserbreak-ustrk) | USTRK | Switches user-defined escape handling on and off. The user-d... |
| [406B](#406b-accessrtcommon-rwrtc) | [AccessRTCommon](#406b-accessrtcommon-rwrtc) | RWRTC | Reads from or writes to RT common from an ND-500 program. RT... |
| [410B](#410b-fixinmemory-fixmem) | [FixInMemory](#410b-fixinmemory-fixmem) | FIXMEM | Fixes a logical segment of your domain in physical memory. Y... |
| [411B](#411b-memoryunfix-unfixm) | [MemoryUnfix](#411b-memoryunfix-unfixm) | UNFIXM | Releases a fixed segment in your domain from physical memory... |
| [412B](#412b-fileassegment-fscnt) | [FileAsSegment](#412b-fileassegment-fscnt) | FSCNT | Connects a file as a segment to your domain. You can then ac... |
| [413B](#413b-filenotassegment-fscdnt) | [FileNotAsSegment](#413b-filenotassegment-fscdnt) | FSCDNT | Disconnects a file as a segment in your domain. FileAsSegmen... |
| [414B](#414b-bcnafcamac-bcnaf) | [BCNAFCAMAC](#414b-bcnafcamac-bcnaf) | BCNAF | Special CAMAC function on the ND-500. (Same as mon 156 TRACB... |
| [415B](#415b-bcnaf1camac-bcnaf1) | [BCNAF1CAMAC](#415b-bcnaf1camac-bcnaf1) | BCNAF1 | Special CAMAC monitor call for the ND-500. (Same as mon 176 ... |
| [416B](#416b-savend500segment-wsegn) | [SaveND500Segment](#416b-savend500segment-wsegn) | WSEGN | Writes all modified pages of a segment back to the disk.... |
| [417B](#417b-maxpagesinmemory-mxpisg) | [MaxPagesInMemory](#417b-maxpagesinmemory-mxpisg) | MXPISG | Sets the maximum number of pages a segment may have in physi... |
| [420B](#420b-getuserregisters-grblk) | [GetUserRegisters](#420b-getuserregisters-grblk) | GRBLK | SwitchUserBreak allows you to save the registers when you te... |
| [421B](#421b-getactivesegment-gasgm) | [GetActiveSegment](#421b-getactivesegment-gasgm) | GASGM | Gets the name of the segments in your domain. A 2048 byte bu... |
| [422B](#422b-getscratchsegment-gswsp) | [GetScratchSegment](#422b-getscratchsegment-gswsp) | GSWSP | Connects an empty data segment to the user's domain and reserves space for it on the swap file. The segment is assigned the default name "SCRATCH-SEGMENT:DSEG". |
| [423B](#423b-copycapability-capcop) | [CopyCapability](#423b-copycapability-capcop) | CAPCOP | Copies a capability for a segment. The segment itself is als... |
| [424B](#424b-clearcapability-capcle) | [ClearCapability](#424b-clearcapability-capcle) | CAPCLE | Clears a capability. A capability describes each logical seg... |
| [425B](#425b-setprocessname-sprnam) | [SetProcessName](#425b-setprocessname-sprnam) | SPRNAM | Defines a new name for your process.... |
| [426B](#426b-getprocessno-gprnam) | [GetProcessNo](#426b-getprocessno-gprnam) | GPRNAM | Gets the number of a process in the ND-500. You specify the ... |
| [427B](#427b-getownprocessinfo-gprnme) | [GetOwnProcessInfo](#427b-getownprocessinfo-gprnme) | GPRNME | Gets the name and number of your own process in the ND-500. ... |
| [430B](#430b-translateaddress-adrioo) | [TranslateAddress](#430b-translateaddress-adrioo) | ADRIOO | The call formats are marked from a-i. All formats give statu... |
| [431B](#431b-awaittransfer-mwaitf) | [AwaitTransfer](#431b-awaittransfer-mwaitf) | MWAITF | Checks that a data transfer to or from a mass-storage file i... |
| [435B](#435b-forcetrap-prt) | [ForceTrap](#435b-forcetrap-prt) | PRT | Forces a programmed trap to occur in another ND-500 process.... |
| [436B](#436b-setnd500param-5paset) | [SetND500Param](#436b-setnd500param-5paset) | 5PASET | Sets information about an ND-500 program. Use GetND500Param ... |
| [437B](#437b-getnd500param-5paget) | [GetND500Param](#437b-getnd500param-5paget) | 5PAGET | Gets information about why the last ND-500 program terminate... |
| [440B](#440b-attach500segment-atsgm) | [Attach500Segment](#440b-attach500segment-atsgm) | ATSGM | Maps a logical ND-500 data segment onto shared ND-100/ND-500... |
| [500B](#500b-startprocess-startp) | [StartProcess](#500b-startprocess-startp) | STARTP | Starts a process in the ND-500. You identify the process wit... |
| [501B](#501b-stopprocess-stoppr) | [StopProcess](#501b-stopprocess-stoppr) | STOPPR | Sets the current process in a wait state. StartProcess resta... |
| [502B](#502b-switchprocess-switchp) | [SwitchProcess](#502b-switchprocess-switchp) | SWITCHP | Sets the current process in a wait state. Restarts another p... |
| [503B](#503b-inputstring-dvinst) | [InputString](#503b-inputstring-dvinst) | DVINST | Reads a string from a device, e.g., a terminal or an opened ... |
| [504B](#504b-outputstring-dvouts) | [OutputString](#504b-outputstring-dvouts) | DVOUTS | Writes a string to a device, e.g. a terminal or an opened fi... |
| [505B](#505b-gettrapreason-gerrcod) | [GetTrapReason](#505b-gettrapreason-gerrcod) | GERRCOD | Gets the error code from the swapper process. This is only r... |
| [507B](#507b-setprocesspriority-sprio) | [SetProcessPriority](#507b-setprocesspriority-sprio) | SPRIO | Sets the priority for a process in the ND-500. The prioritie... |
| [514B](#514b-nd500timeout-5tmout) | [ND500TimeOut](#514b-nd500timeout-5tmout) | 5TMOUT | Suspends the execution of an ND-500 program for a given time... |

---

## Index by Call Name

| Name | Octal | Short Name | Description |
|------|-------|------------|-------------|
| [AccessRTCommon](#406b-accessrtcommon-rwrtc) | [406B](#406b-accessrtcommon-rwrtc) | RWRTC | Reads from or writes to RT common from an ND-500 program. RT... |
| [AdjustClock](#112b-adjustclock-cladj) | [112B](#112b-adjustclock-cladj) | CLADJ | Sets the computer's clock (i.e. the current system time) for... |
| [AltPageTable](#33b-altpagetable-alton) | [33B](#33b-altpagetable-alton) | ALTON | Switches page table. Each page table allows you to access 12... |
| [AppendSpooling](#240b-appendspooling-apspe) | [240B](#240b-appendspooling-apspe) | APSPE | Prints a file. The printer has a queue of files waiting to b... |
| [AssignCAMACLAM](#154b-assigncamaclam-assig) | [154B](#154b-assigncamaclam-assig) | ASSIG | Assigns a graded LAM in the CAMAC identification table to a ... |
| [Attach500Segment](#440b-attach500segment-atsgm) | [440B](#440b-attach500segment-atsgm) | ATSGM | Maps a logical ND-500 data segment onto shared ND-100/ND-500... |
| [AttachSegment](#167b-attachsegment-reent) | [167B](#167b-attachsegment-reent) | REENT | Attaches a reentrant segment to your two current segments. T... |
| [AwaitFileTransfer](#121b-awaitfiletransfer-waitf) | [121B](#121b-awaitfiletransfer-waitf) | WAITF | Checks that a data transfer to or from a mass-storage file i... |
| [AwaitTransfer](#431b-awaittransfer-mwaitf) | [431B](#431b-awaittransfer-mwaitf) | MWAITF | Checks that a data transfer to or from a mass-storage file i... |
| [BackupClose](#252b-backupclose-bclos) | [252B](#252b-backupclose-bclos) | BCLOS | Closes a file. The version number and the last date accessed... |
| [BATCHMODEECHO](#325b-batchmodeecho-mbech) | [325B](#325b-batchmodeecho-mbech) | MBECH | Controls echo of input and output if the program is executed... |
| [BCNAF1CAMAC](#415b-bcnaf1camac-bcnaf1) | [415B](#415b-bcnaf1camac-bcnaf1) | BCNAF1 | Special CAMAC monitor call for the ND-500. (Same as mon 176 ... |
| [BCNAFCAMAC](#414b-bcnafcamac-bcnaf) | [414B](#414b-bcnafcamac-bcnaf) | BCNAF | Special CAMAC function on the ND-500. (Same as mon 156 TRACB... |
| [CallCommand](#70b-callcommand-commnd) | [70B](#70b-callcommand-commnd) | COMMND | Executes a SINTRAN III command from a program. The program t... |
| [CAMACFunction](#147b-camacfunction-camac) | [147B](#147b-camacfunction-camac) | CAMAC | Operates the CAMAC, i.e. executes the NAF register. CAMAC is... |
| [CAMACGLRegister](#150b-camacglregister-gl) | [150B](#150b-camacglregister-gl) | GL | Read the CAMAC GL (Graded LAM -"look at me") register or the... |
| [CAMACIOInstruction](#153b-camacioinstruction-ioxin) | [153B](#153b-camacioinstruction-ioxin) | IOXIN | Executes a single IOX instruction for CAMAC. See under CAMAC... |
| [ChangeSegment](#337b-changesegment-spchg) | [337B](#337b-changesegment-spchg) | SPCHG | Changes the segment and the page table your program uses. Th... |
| [CheckMonCall](#312b-checkmoncall-moinf) | [312B](#312b-checkmoncall-moinf) | MOINF | Some monitor calls are optional or only available in later v... |
| [ClearCapability](#424b-clearcapability-capcle) | [424B](#424b-clearcapability-capcle) | CAPCLE | Clears a capability. A capability describes each logical seg... |
| [ClearInBuffer](#13b-clearinbuffer-cibuf) | [13B](#13b-clearinbuffer-cibuf) | CIBUF | Clears a device input buffer. Input from character devices, ... |
| [ClearOutBuffer](#14b-clearoutbuffer-cobuf) | [14B](#14b-clearoutbuffer-cobuf) | COBUF | Clears a device output buffer. Output to character devices, ... |
| [CloseFile](#43b-closefile-close) | [43B](#43b-closefile-close) | CLOSE | Closes one or more files. Files must be opened before they a... |
| [CloseSpoolingFile](#40b-closespoolingfile-spclo) | [40B](#40b-closespoolingfile-spclo) | SPCLO | Appends an opened file to a spooling queue. You specify a te... |
| [CopyCapability](#423b-copycapability-capcop) | [423B](#423b-copycapability-capcop) | CAPCOP | Copies a capability for a segment. The segment itself is als... |
| [CopyPage](#251b-copypage-copag) | [251B](#251b-copypage-copag) | COPAG | Copies file pages between two opened files. One of the files... |
| [CreateFile](#221b-createfile-cralf) | [221B](#221b-createfile-cralf) | CRALF | Creates a file. The file may be indexed, contiguous, or allo... |
| [DataTransfer](#131b-datatransfer-abstr) | [131B](#131b-datatransfer-abstr) | ABSTR | Transfers data between physical memory and a mass-storage de... |
| [DefaultRemoteSystem](#314b-defaultremotesystem-srusi) | [314B](#314b-defaultremotesystem-srusi) | SRUSI | Sets default values for COSMOS remote file access. You can s... |
| [DelayStart](#101b-delaystart-set) | [101B](#101b-delaystart-set) | SET | Starts an RT program after a specified time. The RT program ... |
| [DeleteFile](#54b-deletefile-mdlfi) | [54B](#54b-deletefile-mdlfi) | MDLFI | Deletes a file. The pages of the file are released.... |
| [DeletePage](#272b-deletepage-delpg) | [272B](#272b-deletepage-delpg) | DELPG | Deletes pages from a file. Pages between two page numbers ar... |
| [DeviceControl](#141b-devicecontrol-ioset) | [141B](#141b-devicecontrol-ioset) | IOSET | Sets control information for a character device, e.g. a term... |
| [DeviceFunction](#144b-devicefunction-magtp) | [144B](#144b-devicefunction-magtp) | MAGTP | Performs various operations on floppy disks, magnetic tapes,... |
| [DirectOpen](#220b-directopen-dopen) | [220B](#220b-directopen-dopen) | DOPEN | Opens a file. Files must be opened before they can be access... |
| [DisableEscape](#71b-disableescape-descf) | [71B](#71b-disableescape-descf) | DESCF | The ESCAPE key on the terminal normally terminates a program... |
| [DISABLELOCAL](#277b-disablelocal-dlofu) | [277B](#277b-disablelocal-dlofu) | DLOFU | You may log in on remote computers through the COSMOS data n... |
| [DisableRTStart](#137b-disablertstart-rtoff) | [137B](#137b-disablertstart-rtoff) | RTOFF | Disables start of RT programs. No RT program can be started ... |
| [DisAssemble](#401b-disassemble-diass) | [401B](#401b-disassemble-diass) | DIASS | Disassembles one machine instruction on the ND-500. Output i... |
| [DMAFunction](#333b-dmafunction-udma) | [333B](#333b-dmafunction-udma) | UDMA | Monitor call 333B - DMAFunction... |
| [EnableEscape](#72b-enableescape-eescf) | [72B](#72b-enableescape-eescf) | EESCF | Enables the ESCAPE key on the terminal. The ESCAPE key norma... |
| [EnableLocal](#276b-enablelocal-elofu) | [276B](#276b-enablelocal-elofu) | ELOFU | You may log in on remote computers through the COSMOS data n... |
| [EnableRTStart](#136b-enablertstart-rton) | [136B](#136b-enablertstart-rton) | RTON | RTON RT programs cannot be started after DisableRTStart has ... |
| [ErrorMessage](#65b-errormessage-qerms) | [65B](#65b-errormessage-qerms) | QERMS | Displays a file system error message. Appendix A shows the m... |
| [ErrorReturn](#400b-errorreturn-macroe) | [400B](#400b-errorreturn-macroe) | MACROE | Terminates the program and sets an error code. The error cod... |
| [ExactDelayStart](#126b-exactdelaystart-dset) | [126B](#126b-exactdelaystart-dset) | DSET | Sets an RT program to start after a given period. It is then... |
| [ExactInterval](#130b-exactinterval-dintv) | [130B](#130b-exactinterval-dintv) | DINTV | Prepares an RT program for periodic execution. The interval ... |
| [ExactStartup](#127b-exactstartup-dabst) | [127B](#127b-exactstartup-dabst) | DABST | Starts an RT program at a specific time. The time is given i... |
| [ExecuteCommand](#317b-executecommand-uecom) | [317B](#317b-executecommand-uecom) | UECOM | Executes a SINTRAN III command. Specify the command name and... |
| [ExecutionInfo](#143b-executioninfo-rsio) | [143B](#143b-executioninfo-rsio) | RSIO | Gets information about the execution of the calling program.... |
| [ExitFromProgram](#0b-exitfromprogram-leave) | [0B](#0b-exitfromprogram-leave) | LEAVE | Terminates the program. Returns to SINTRAN III. Batch jobs c... |
| [ExitFromSegment](#133b-exitfromsegment-mexit) | [133B](#133b-exitfromsegment-mexit) | MEXIT | Exchanges one or both current segments. Commonly used to ret... |
| [ExitRTProgram](#134b-exitrtprogram-rtext) | [134B](#134b-exitrtprogram-rtext) | RTEXT | Terminates the calling RT or background program. Releases al... |
| [ExpandFile](#231b-expandfile-expfi) | [231B](#231b-expandfile-expfi) | EXPFI | Expands the file size. You use this monitor call to increase... |
| [FileAsSegment](#412b-fileassegment-fscnt) | [412B](#412b-fileassegment-fscnt) | FSCNT | Connects a file as a segment to your domain. You can then ac... |
| [FileNotAsSegment](#413b-filenotassegment-fscdnt) | [413B](#413b-filenotassegment-fscdnt) | FSCDNT | Disconnects a file as a segment in your domain. FileAsSegmen... |
| [FileSystemFunction](#327b-filesystemfunction-fsmty) | [327B](#327b-filesystemfunction-fsmty) | FSMTY | Multifunction monitor call to make sure that an uncontrolled... |
| [FixContiguous](#160b-fixcontiguous-fixc) | [160B](#160b-fixcontiguous-fixc) | FIXC | Places a segment in physical memory. Its pages will no longe... |
| [FixInMemory](#410b-fixinmemory-fixmem) | [410B](#410b-fixinmemory-fixmem) | FIXMEM | Fixes a logical segment of your domain in physical memory. Y... |
| [FixIOArea](#404b-fixioarea-iofix) | [404B](#404b-fixioarea-iofix) | IOFIX | Fixes an address area in a domain in physical memory. The me... |
| [FixScattered](#115b-fixscattered-fix) | [115B](#115b-fixscattered-fix) | FIX | Place a segment in physical memory. Its pages will no longer... |
| [ForceRelease](#125b-forcerelease-prlrs) | [125B](#125b-forcerelease-prlrs) | PRLRS | Releases a device reserved by an RT program other than that ... |
| [ForceReserve](#124b-forcereserve-prsrv) | [124B](#124b-forcereserve-prsrv) | PRSRV | Reserves a device for an RT program other than that which is... |
| [ForceTrap](#435b-forcetrap-prt) | [435B](#435b-forcetrap-prt) | PRT | Forces a programmed trap to occur in another ND-500 process.... |
| [FullFileName](#256b-fullfilename-deabf) | [256B](#256b-fullfilename-deabf) | DEABF | Returns a complete file name from an abbreviated one. The di... |
| [GetActiveSegment](#421b-getactivesegment-gasgm) | [421B](#421b-getactivesegment-gasgm) | GASGM | Gets the name of the segments in your domain. A 2048 byte bu... |
| [GetAddressArea](#222b-getaddressarea-gbsiz) | [222B](#222b-getaddressarea-gbsiz) | GBSIZ | Gets the size of your address area. Your address area may co... |
| [GetAllFileIndexes](#217b-getallfileindexes-guioi) | [217B](#217b-getallfileindexes-guioi) | GUIOI | Gets the directory index, the user index, and the object ind... |
| [GetBasicTime](#11b-getbasictime-time) | [11B](#11b-getbasictime-time) | TIME | **Time**... |
| [GetBytesInFile](#62b-getbytesinfile-rmax) | [62B](#62b-getbytesinfile-rmax) | RMAX | Gets the number of bytes in a file. Only the bytes containin... |
| [GetCurrentTime](#113b-getcurrenttime-clock) | [113B](#113b-getcurrenttime-clock) | CLOCK | Gets the current system time and date.... |
| [GetDefaultDir](#250b-getdefaultdir-fdfdi) | [250B](#250b-getdefaultdir-fdfdi) | FDFDI | Gets the user’s default directory. The directory index and t... |
| [GetDeviceType](#263b-getdevicetype-gdevt) | [263B](#263b-getdevicetype-gdevt) | GDEVT | Gets the device type, e.g. terminal, floppy disk, mass-stora... |
| [GetDirEntry](#244b-getdirentry-gdien) | [244B](#244b-getdirentry-gdien) | GDIEN | Gets information about a directory. The directory entry is r... |
| [GetDirNameIndex](#243b-getdirnameindex-fdina) | [243B](#243b-getdirnameindex-fdina) | FDINA | Gets directory index and name index. The name index identifi... |
| [GetDirUserIndexes](#213b-getdiruserindexes-muidi) | [213B](#213b-getdiruserindexes-muidi) | MUIDI | Gets a directory index and a user index. You have to specify... |
| [GetErrorDevice](#254b-geterrordevice-gerdv) | [254B](#254b-geterrordevice-gerdv) | GERDV | Gets the logical device number of the error device. The erro... |
| [GetErrorInfo](#207b-geterrorinfo-rerrp) | [207B](#207b-geterrorinfo-rerrp) | RERRP | Gets information about the last real-time error. The monitor... |
| [GetErrorMessage](#334b-geterrormessage-getxm) | [334B](#334b-geterrormessage-getxm) | GETXM | Gets a SINTRAN III error message text. Appendix A shows the ... |
| [GEtEscLocalChars](#230b-getesclocalchars-mgdae) | [230B](#230b-getesclocalchars-mgdae) | MGDAE | Gets ESCAPE and LOCAL characters. You can terminate most pro... |
| [GetFileIndexes](#274b-getfileindexes-fobjn) | [274B](#274b-getfileindexes-fobjn) | FOBJN | Gets the directory index, the user index, and the object ind... |
| [GetFileName](#273b-getfilename-mgfil) | [273B](#273b-getfilename-mgfil) | MGFIL | Gets the name of a file. You specify the directory index, th... |
| [GetInputFlags](#402b-getinputflags-rflag) | [402B](#402b-getinputflags-rflag) | RFLAG | ND-100 and ND-500 programs may communicate through two 32-bi... |
| [GETINREGISTERS](#165b-getinregisters-diw) | [165B](#165b-getinregisters-diw) | DIW | Reads the device interface registers.... |
| [GetLastByte](#26b-getlastbyte-lastc) | [26B](#26b-getlastbyte-lastc) | LASTC | Gets the last character typed on a terminal. The monitor cal... |
| [GetNameEntry](#245b-getnameentry-gnaen) | [245B](#245b-getnameentry-gnaen) | GNAEN | Gets information about devices, e.g. disks and floppy disks.... |
| [GetND500Param](#437b-getnd500param-5paget) | [437B](#437b-getnd500param-5paget) | 5PAGET | Gets information about why the last ND-500 program terminate... |
| [GetObjectEntry](#215b-getobjectentry-drobj) | [215B](#215b-getobjectentry-drobj) | DROBJ | Gets information about a file. An object entry describes eac... |
| [GetOwnProcessInfo](#427b-getownprocessinfo-gprnme) | [427B](#427b-getownprocessinfo-gprnme) | GPRNME | Gets the name and number of your own process in the ND-500. ... |
| [GetOwnRTAddress](#30b-getownrtaddress-getrt) | [30B](#30b-getownrtaddress-getrt) | GETRT | Gets the address of the calling program's RT description. Ba... |
| [GetProcessNo](#426b-getprocessno-gprnam) | [426B](#426b-getprocessno-gprnam) | GPRNAM | Gets the number of a process in the ND-500. You specify the ... |
| [GetRTAddress](#151b-getrtaddress-grtda) | [151B](#151b-getrtaddress-grtda) | GRTDA | Gets the address of an RT description. You specify the name ... |
| [GetRTDescr](#27b-getrtdescr-rtdsc) | [27B](#27b-getrtdescr-rtdsc) | RTDSC | Reads an RT description. The RT description contains various... |
| [GetRTName](#152b-getrtname-grtna) | [152B](#152b-getrtname-grtna) | GRTNA | Gets the name of an RT program. You specify the RT descripti... |
| [GetScratchSegment](#422b-getscratchsegment-gswsp) | [422B](#422b-getscratchsegment-gswsp) | GSWSP | Connects an empty data segment to the user's domain and reserves space for it on the swap file. The segment is assigned the default name "SCRATCH-SEGMENT:DSEG". |
| [GetSegmentEntry](#53b-getsegmententry-rsegm) | [53B](#53b-getsegmententry-rsegm) | RSEGM | Gets information about a segment in the ND-100. The monitor ... |
| [GetSegmentNo](#322b-getsegmentno-gsgno) | [322B](#322b-getsegmentno-gsgno) | GSGNO | Gets the number of a segment in the ND-100. You specify the ... |
| [GetSpoolingEntry](#55b-getspoolingentry-rsqpe) | [55B](#55b-getspoolingentry-rsqpe) | RSQPE | Gets the next spooling queue entry, that is, the next file t... |
| [GetStartByte](#75b-getstartbyte-reabt) | [75B](#75b-getstartbyte-reabt) | REABT | Gets the number of the next byte to access in a file. The by... |
| [GetSystemInfo](#262b-getsysteminfo-cpust) | [262B](#262b-getsysteminfo-cpust) | CPUST | Gets various system information. The system number, the CPU ... |
| [GetTerminalMode](#306b-getterminalmode-gtmod) | [306B](#306b-getterminalmode-gtmod) | GTMOD | Gets the terminal mode. The terminal mode tells how the term... |
| [GetTerminalType](#16b-getterminaltype-mgtty) | [16B](#16b-getterminaltype-mgtty) | MGTTY | Gets the terminal type. The terminal type tells SINTRAN III ... |
| [GetTimeUsed](#114b-gettimeused-tused) | [114B](#114b-gettimeused-tused) | TUSED | Gets the time you have used the CPU since you logged in. In ... |
| [GetTrapReason](#505b-gettrapreason-gerrcod) | [505B](#505b-gettrapreason-gerrcod) | GERRCOD | Gets the error code from the swapper process. This is only r... |
| [GetUserEntry](#44b-getuserentry-ruser) | [44B](#44b-getuserentry-ruser) | RUSER | Gets information about a user. The user entry in the directo... |
| [GetUserName](#214b-getusername-gusna) | [214B](#214b-getusername-gusna) | GUSNA | Gets the name of a user. The user may be on a remote compute... |
| [GetUserParam](#57b-getuserparam-pagei) | [57B](#57b-getuserparam-pagei) | PAGEI | Gets information about why the last program terminated. Ther... |
| [GetUserRegisters](#420b-getuserregisters-grblk) | [420B](#420b-getuserregisters-grblk) | GRBLK | SwitchUserBreak allows you to save the registers when you te... |
| [GRAPHICFUNCTION](#155b-graphicfunction-graph) | [155B](#155b-graphicfunction-graph) | GRAPH | Executes various functions on a graphic peripheral, such as ... |
| [HDLCfunction](#201b-hdlcfunction-mhdlc) | [201B](#201b-hdlcfunction-mhdlc) | MHDLC | Performs various HDLC functions. A HDLC is a high-level data... |
| [In4x2Bytes](#63b-in4x2bytes-b41nw) | [63B](#63b-in4x2bytes-b41nw) | B41NW | Reads 8 bytes from a word-oriented or character-oriented dev... |
| [In8AndFlag](#310b-in8andflag-tbin8) | [310B](#310b-in8andflag-tbin8) | TBIN8 | Reads 8 bytes from a device, e.g., a terminal. The monitor c... |
| [In8Bytes](#23b-in8bytes-b8inb) | [23B](#23b-in8bytes-b8inb) | B8INB | Reads 8 bytes from a device. The input is fast, but the moni... |
| [InBufferSpace](#66b-inbufferspace-isize) | [66B](#66b-inbufferspace-isize) | ISIZE | Gets the current number of bytes in the input buffer. Termin... |
| [InBufferState](#313b-inbufferstate-ibrisz) | [313B](#313b-inbufferstate-ibrisz) | IBRISZ | Gets information about an input buffer. The current number o... |
| [InByte](#1b-inbyte-inbt) | [1B](#1b-inbyte-inbt) | INBT | Reads one byte from a character device, e.g. a terminal or a... |
| [InputString](#503b-inputstring-dvinst) | [503B](#503b-inputstring-dvinst) | DVINST | Reads a string from a device, e.g., a terminal or an opened ... |
| [INSTRING](#161b-instring-instr) | [161B](#161b-instring-instr) | INSTR | Reads a string of characters from a peripheral device, e.g. ... |
| [InUpTo8Bytes](#21b-inupto8bytes-m8inb) | [21B](#21b-inupto8bytes-m8inb) | M8INB | See also In8Bytes, InByte, InString, In4x2Bytes, and Out8Byt... |
| [IOInstruction](#31b-ioinstruction-exiox) | [31B](#31b-ioinstruction-exiox) | EXIOX | Executes an IOX machine instruction. The IOX instruction han... |
| [JumpToSegment](#132b-jumptosegment-mcall) | [132B](#132b-jumptosegment-mcall) | MCALL | Calls a routine on another segment in the ND-100. You can di... |
| [LAMUFunction](#315b-lamufunction-mlamu) | [315B](#315b-lamufunction-mlamu) | MLAMU | Performs various functions on the LAMU system. A LAMU is a l... |
| [LogInStart](#326b-loginstart-mlogi) | [326B](#326b-loginstart-mlogi) | MLOGI | Allowed in RT programs (ring ≥ 1), and in all programs run b... |
| [MaxPagesInMemory](#417b-maxpagesinmemory-mxpisg) | [417B](#417b-maxpagesinmemory-mxpisg) | MXPISG | Sets the maximum number of pages a segment may have in physi... |
| [MemoryAllocation](#61b-memoryallocation-fixcs) | [61B](#61b-memoryallocation-fixcs) | FIXCS | Fixes or unfixes ND-100 segments to be used by the ND-500 Mo... |
| [MemoryUnfix](#411b-memoryunfix-unfixm) | [411B](#411b-memoryunfix-unfixm) | UNFIXM | Releases a fixed segment in your domain from physical memory... |
| [ND500TimeOut](#514b-nd500timeout-5tmout) | [514B](#514b-nd500timeout-5tmout) | 5TMOUT | Suspends the execution of an ND-500 program for a given time... |
| [NewFileVersion](#253b-newfileversion-craln) | [253B](#253b-newfileversion-craln) | CRALN | Creates new versions of a file. You may create new versions ... |
| [NewUser](#241b-newuser-suscn) | [241B](#241b-newuser-suscn) | SUSCN | Switches the user name you are logged in under. The command ... |
| [NoInterruptStart](#107b-nointerruptstart-dscnt) | [107B](#107b-nointerruptstart-dscnt) | DSCNT | StartOnInterrupt connects an RT program to interrupts from a... |
| [NormalPageTable](#34b-normalpagetable-altoff) | [34B](#34b-normalpagetable-altoff) | ALTOFF | **ALTOFF** Sets the alternative page table equal to the norm... |
| [NoWaitSwitch](#36b-nowaitswitch-nowt) | [36B](#36b-nowaitswitch-nowt) | NOWT | Switches No Wait on and off. No Wait is useful for input fro... |
| [OctobusFunction](#324b-octobusfunction-octio) | [324B](#324b-octobusfunction-octio) | OCTIO | Performs various functions on an old Octobus (earlier than v... |
| [OffEscLocalFunction](#303b-offesclocalfunction-eloff) | [303B](#303b-offesclocalfunction-eloff) | ELOFF | Delays the escape and local functions for your terminal. The... |
| [OldUser](#242b-olduser-ruscn) | [242B](#242b-olduser-ruscn) | RUSCN | Switches back to the user name you were logged in under befo... |
| [ONESCLOCALFUNCTION](#302b-onesclocalfunction-elon) | [302B](#302b-onesclocalfunction-elon) | ELON | Enables delayed escape and local functions for your terminal... |
| [OpenFile](#50b-openfile-open) | [50B](#50b-openfile-open) | OPEN | Opens a file. You cannot access a file before you open it. S... |
| [OpenFileInfo](#257b-openfileinfo-fopen) | [257B](#257b-openfileinfo-fopen) | FOPEN | Monitor call 257B - OpenFileInfo... |
| [Out8Bytes](#24b-out8bytes-b8out) | [24B](#24b-out8bytes-b8out) | B8OUT | Writes 8 bytes to a character device, e.g. a terminal. All 8... |
| [OutBufferSpace](#67b-outbufferspace-osize) | [67B](#67b-outbufferspace-osize) | OSIZE | Gets the number of free bytes in the output buffer (number o... |
| [OutByte](#2b-outbyte-outbt) | [2B](#2b-outbyte-outbt) | OUTBT | Writes one byte to a character device, e.g. a terminal or an... |
| [OutMessage](#32b-outmessage-msg) | [32B](#32b-outmessage-msg) | MSG | Writes a message to the user's terminal. This is convenient ... |
| [OutNumber](#35b-outnumber-iout) | [35B](#35b-outnumber-iout) | IOUT | Writes a number to the user's terminal. The number can be ou... |
| [OutputString](#504b-outputstring-dvouts) | [504B](#504b-outputstring-dvouts) | DVOUTS | Writes a string to a device, e.g. a terminal or an opened fi... |
| [OutString](#162b-outstring-outst) | [162B](#162b-outstring-outst) | OUTST | Writes a string of characters to a peripheral file, e.g., a ... |
| [OutUpTo8Bytes](#22b-outupto8bytes-m8out) | [22B](#22b-outupto8bytes-m8out) | M8OUT | Writes up to 8 characters to a device, e.g. a terminal or an... |
| [PIOCCFunction](#255b-pioccfunction-piocm) | [255B](#255b-pioccfunction-piocm) | PIOCM | PIOCC (Programmed Input/Output Control Channel) function mon... |
| [PrivInstruction](#146b-privinstruction-ipriv) | [146B](#146b-privinstruction-ipriv) | IPRIV | Executes a privileged machine instruction on the ND-100. Pri... |
| [ReadADChannel](#37b-readadchannel-airdw) | [37B](#37b-readadchannel-airdw) | AIRDW | Reads an analog to digital channel.... |
| [ReadBlock](#7b-readblock-rpage) | [7B](#7b-readblock-rpage) | RPAGE | Reads randomly from a file. You read one block at a time. Th... |
| [ReadDiskPage](#270b-readdiskpage-rdpag) | [270B](#270b-readdiskpage-rdpag) | RDPAG | Reads one or more directory pages. Any page can be read.... |
| [ReadFromFile](#117b-readfromfile-rfile) | [117B](#117b-readfromfile-rfile) | RFILE | Reads any number of bytes from a file. The read operation mu... |
| [ReadObjectEntry](#41b-readobjectentry-robje) | [41B](#41b-readobjectentry-robje) | ROBJE | Gets information about an opened file. An object entry descr... |
| [ReadScratchFile](#5b-readscratchfile-rdisk) | [5B](#5b-readscratchfile-rdisk) | RDISK | Reads randomly from the scratch file. One block is transferr... |
| [READSYSTEMRECORD](#340b-readsystemrecord-rsrec) | [340B](#340b-readsystemrecord-rsrec) | RSREC | Used to read the system record into a buffer.... |
| [ReentrantSegment](#212b-reentrantsegment-sreen) | [212B](#212b-reentrantsegment-sreen) | SREEN | Connects a reentrant segment to your two current segments. A... |
| [ReleaseDir](#247b-releasedir-rldir) | [247B](#247b-releasedir-rldir) | RLDIR | Releases a directory. The directory must have been reserved ... |
| [ReleaseResource](#123b-releaseresource-reles) | [123B](#123b-releaseresource-reles) | RELES | Releases a reserved device or file. The resource can then be... |
| [RenameFile](#232b-renamefile-mrnfi) | [232B](#232b-renamefile-mrnfi) | MRNFI | See also @RENAME-FILE.... |
| [ReservationInfo](#140b-reservationinfo-whdev) | [140B](#140b-reservationinfo-whdev) | WHDEV | Checks that a device is not reserved. If it is reserved, you... |
| [ReserveDir](#246b-reservedir-redir) | [246B](#246b-reservedir-redir) | REDIR | Reserves a directory for special use. The directory must be ... |
| [ReserveResource](#122b-reserveresource-resrv) | [122B](#122b-reserveresource-resrv) | RESRV | Reserves a device or file for your program only. You release... |
| [SaveND500Segment](#416b-savend500segment-wsegn) | [416B](#416b-savend500segment-wsegn) | WSEGN | Writes all modified pages of a segment back to the disk.... |
| [SaveSegment](#164b-savesegment-wseg) | [164B](#164b-savesegment-wseg) | WSEG | Saves a segment in the ND-100. All pages in physical memory ... |
| [ScratchOpen](#235b-scratchopen-scrop) | [235B](#235b-scratchopen-scrop) | SCROP | Opens a file as a scratch file. A maximum of 64 pages of the... |
| [SegmentFunction](#341b-segmentfunction-sgmty) | [341B](#341b-segmentfunction-sgmty) | SGMTY | This is a multifunction monitor call used to change the acti... |
| [SegmentOverlay](#323b-segmentoverlay-splre) | [323B](#323b-segmentoverlay-splre) | SPLRE | Used to build multisegment programs in the ND-100. It is mai... |
| [SegmentToPageTable](#157b-segmenttopagetable-entsg) | [157B](#157b-segmenttopagetable-entsg) | ENTSG | Enters a routine as a direct task or as a device driver, and... |
| [SetBlockSize](#76b-setblocksize-setbs) | [76B](#76b-setblocksize-setbs) | SETBS | Sets the block size of an opened file. Monitor calls which r... |
| [SetBreak](#4b-setbreak-brkm) | [4B](#4b-setbreak-brkm) | BRKM | Sets the break characters for a terminal. Normally, a progra... |
| [SetClock](#111b-setclock-updat) | [111B](#111b-setclock-updat) | UPDAT | Gives new values to the computer's clock and calendar. If th... |
| [SetCommandBuffer](#12b-setcommandbuffer-setcm) | [12B](#12b-setcommandbuffer-setcm) | SETCM | Transfers a string to the command buffer. The command buffer... |
| [SetEcho](#3b-setecho-echom) | [3B](#3b-setecho-echom) | ECHOM | When you press a key on the terminal, a character is normall... |
| [SetEscapeHandling](#300b-setescapehandling-eusel) | [300B](#300b-setescapehandling-eusel) | EUSEL | Enables user-defined escape handling. When the ESCAPE key is... |
| [SetEscLocalChars](#227b-setesclocalchars-msdae) | [227B](#227b-setesclocalchars-msdae) | MSDAE | You can terminate most programs with the ESCAPE key. A LOCAL... |
| [SetFileAccess](#237b-setfileaccess-sfacc) | [237B](#237b-setfileaccess-sfacc) | SFACC | Sets the access protection for a file. You should specify th... |
| [SetMaxBytes](#73b-setmaxbytes-smax) | [73B](#73b-setmaxbytes-smax) | SMAX | Sets the value of the maximum byte pointer in an opened file... |
| [SetND500Param](#436b-setnd500param-5paset) | [436B](#436b-setnd500param-5paset) | 5PASET | Sets information about an ND-500 program. Use GetND500Param ... |
| [SetObjectEntry](#216b-setobjectentry-dwobj) | [216B](#216b-setobjectentry-dwobj) | DWOBJ | Changes the description of a file. An object entry describes... |
| [SetOutputFlags](#403b-setoutputflags-wflag) | [403B](#403b-setoutputflags-wflag) | WFLAG | ND-100 and ND-500 programs may communicate through two 32-bi... |
| [SetPeripheralName](#234b-setperipheralname-spefi) | [234B](#234b-setperipheralname-spefi) | SPEFI | Defines a peripheral file, e.g. a printer. You connect a fil... |
| [SetPermanentOpen](#236b-setpermanentopen-sperd) | [236B](#236b-setpermanentopen-sperd) | SPERD | Sets a file permanently open. The file is not closed by Clos... |
| [SetProcessName](#425b-setprocessname-sprnam) | [425B](#425b-setprocessname-sprnam) | SPRNAM | Defines a new name for your process.... |
| [SetProcessPriority](#507b-setprocesspriority-sprio) | [507B](#507b-setprocesspriority-sprio) | SPRIO | Sets the priority for a process in the ND-500. The prioritie... |
| [SetRemoteAccess](#316b-setremoteaccess-srlmo) | [316B](#316b-setremoteaccess-srlmo) | SRLMO | Switches remote file access on and off. The COSMOS network a... |
| [SetRTPriority](#110b-setrtpriority-prior) | [110B](#110b-setrtpriority-prior) | PRIOR | Sets the priority of an RT program. RT programs may be given... |
| [SetStartBlock](#77b-setstartblock-setbl) | [77B](#77b-setstartblock-setbl) | SETBL | Sets the next block to be read or written in an opened file.... |
| [SetStartByte](#74b-setstartbyte-setbt) | [74B](#74b-setstartbyte-setbt) | SETBT | Sets the next byte to be read or written in an opened mass-s... |
| [SetTemporaryFile](#233b-settemporaryfile-stefi) | [233B](#233b-settemporaryfile-stefi) | STEFI | Defines a file to store information temporarily. The file ca... |
| [SetTerminalName](#275b-setterminalname-strfi) | [275B](#275b-setterminalname-strfi) | STRFI | Defines the file name to be used for terminals. This is norm... |
| [SetTerminalType](#17b-setterminaltype-mstty) | [17B](#17b-setterminaltype-mstty) | MSTTY | Sets the type of a terminal. The terminal type tells SINTRAN... |
| [SetUserParam](#56b-setuserparam-paset) | [56B](#56b-setuserparam-paset) | PASET | Sets information about a background program. Use GetUserPara... |
| [StartOnInterrupt](#106b-startoninterrupt-conct) | [106B](#106b-startoninterrupt-conct) | CONCT | StartOnInterrupt connects an RT program to interrupts from a... |
| [StartProcess](#500b-startprocess-startp) | [500B](#500b-startprocess-startp) | STARTP | Starts a process in the ND-500. You identify the process wit... |
| [StartRTProgram](#100b-startrtprogram-rt) | [100B](#100b-startrtprogram-rt) | RT | Starts an RT program. The program is moved to the execution ... |
| [StartupInterval](#103b-startupinterval-intv) | [103B](#103b-startupinterval-intv) | INTV | Prepares an RT program for periodic execution. The interval ... |
| [StartupTime](#102b-startuptime-abset) | [102B](#102b-startuptime-abset) | ABSET | Starts an RT program at a specified time of the day. The RT ... |
| [StopEscapeHandling](#301b-stopescapehandling-dusel) | [301B](#301b-stopescapehandling-dusel) | DUSEL | Disables user-defined escape handling. The ESCAPE key termin... |
| [StopProcess](#501b-stopprocess-stoppr) | [501B](#501b-stopprocess-stoppr) | STOPPR | Sets the current process in a wait state. StartProcess resta... |
| [StopRTProgram](#105b-stoprtprogram-abort) | [105B](#105b-stoprtprogram-abort) | ABORT | Stops an RT program. It is removed from the time or executio... |
| [SuspendProgram](#104b-suspendprogram-hold) | [104B](#104b-suspendprogram-hold) | HOLD | Suspends the execution of your program for a given time. The... |
| [SwitchProcess](#502b-switchprocess-switchp) | [502B](#502b-switchprocess-switchp) | SWITCHP | Sets the current process in a wait state. Restarts another p... |
| [SwitchUserBreak](#405b-switchuserbreak-ustrk) | [405B](#405b-switchuserbreak-ustrk) | USTRK | Switches user-defined escape handling on and off. The user-d... |
| [TERMINAL](#336b-terminal-iomty) | [336B](#336b-terminal-iomty) | IOMTY | This I/O multifunction monitor call is used to change the at... |
| [TerminalLineInfo](#332b-terminallineinfo-trepp) | [332B](#332b-terminallineinfo-trepp) | TREPP | Gets information about a terminal line. You may also enable ... |
| [TerminalMode](#52b-terminalmode-termo) | [52B](#52b-terminalmode-termo) | TERMO | Selects various terminal functions. You may stop output on f... |
| [TerminalNoWait](#307b-terminalnowait-tnowai) | [307B](#307b-terminalnowait-tnowai) | TNOWAI | Switches No Wait on and off. No Wait is useful for input fro... |
| [TerminalStatus](#330b-terminalstatus-terst) | [330B](#330b-terminalstatus-terst) | TERST | Gets information about a terminal. The user logged in, the t... |
| [TerminationHandling](#206b-terminationhandling-edtmp) | [206B](#206b-terminationhandling-edtmp) | EDTMP | Switches termination handling on and off.... |
| [TimeOut](#267b-timeout-tmout) | [267B](#267b-timeout-tmout) | TMOUT | Suspends the execution of your program for a given time. The... |
| [ToErrorDevice](#142b-toerrordevice-ermon) | [142B](#142b-toerrordevice-ermon) | ERMON | Outputs a user-defined, real-time error. The error message i... |
| [TRANSFERDATA](#335b-transferdata-exabs) | [335B](#335b-transferdata-exabs) | EXABS | Transfers data between physical memory and a mass-storage de... |
| [TranslateAddress](#430b-translateaddress-adrioo) | [430B](#430b-translateaddress-adrioo) | ADRIOO | The call formats are marked from a-i. All formats give statu... |
| [UnfixSegment](#116b-unfixsegment-unfix) | [116B](#116b-unfixsegment-unfix) | UNFIX | Releases a fixed segment and removes it from the Page Index ... |
| [UserDef0](#170b-userdef0-us0) | [170B](#170b-userdef0-us0) | US0 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef1](#171b-userdef1-us1) | [171B](#171b-userdef1-us1) | US1 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef2](#172b-userdef2-us2) | [172B](#172b-userdef2-us2) | US2 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef3](#173b-userdef3-us3) | [173B](#173b-userdef3-us3) | US3 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef4](#174b-userdef4-us4) | [174B](#174b-userdef4-us4) | US4 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef5](#175b-userdef5-us5) | [175B](#175b-userdef5-us5) | US5 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef6](#176b-userdef6-us6) | [176B](#176b-userdef6-us6) | US6 | User-defined monitor call. You can implement up to 8 monitor... |
| [UserDef7](#177b-userdef7-us7) | [177B](#177b-userdef7-us7) | US7 | User-defined monitor call. You can implement up to 8 monitor... |
| [WAITFORRESTART](#135b-waitforrestart-rtwt) | [135B](#135b-waitforrestart-rtwt) | RTWT | Sets the RT program in a waiting state. It is restarted by S... |
| [WarningMessage](#64b-warningmessage-ermsg) | [64B](#64b-warningmessage-ermsg) | ERMSG | Outputs a file system error message. Appendix A shows the me... |
| [WriteBlock](#10b-writeblock-wpage) | [10B](#10b-writeblock-wpage) | WPAGE | Writes randomly to a file. You write one block at a time. Th... |
| [WriteDirEntry](#311b-writedirentry-wdien) | [311B](#311b-writedirentry-wdien) | WDIEN | Changes the information about a directory. The complete cont... |
| [WriteDiskPage](#271b-writediskpage-wdpag) | [271B](#271b-writediskpage-wdpag) | WDPAG | Writes to one or more pages in a directory. Any page can be ... |
| [WriteScratchFile](#6b-writescratchfile-wdisk) | [6B](#6b-writescratchfile-wdisk) | WDISK | Writes randomly to the scratch file. One block is transferre... |
| [WriteToFile](#120b-writetofile-wfile) | [120B](#120b-writetofile-wfile) | WFILE | Writes any number of bytes to a file. The read operation mus... |
| [XMSGFunction](#200b-xmsgfunction-xmsg) | [200B](#200b-xmsgfunction-xmsg) | XMSG | Performs various data communication functions. All types of ... |

---

## Index by Short Name

| Short Name | Name | Octal | Description |
|------------|------|-------|-------------|
| **5PAGET** | [GetND500Param](#437b-getnd500param-5paget) | [437B](#437b-getnd500param-5paget) | Gets information about why the last ND-500 program terminate... |
| **5PASET** | [SetND500Param](#436b-setnd500param-5paset) | [436B](#436b-setnd500param-5paset) | Sets information about an ND-500 program. Use GetND500Param ... |
| **5TMOUT** | [ND500TimeOut](#514b-nd500timeout-5tmout) | [514B](#514b-nd500timeout-5tmout) | Suspends the execution of an ND-500 program for a given time... |
| **ABORT** | [StopRTProgram](#105b-stoprtprogram-abort) | [105B](#105b-stoprtprogram-abort) | Stops an RT program. It is removed from the time or executio... |
| **ABSET** | [StartupTime](#102b-startuptime-abset) | [102B](#102b-startuptime-abset) | Starts an RT program at a specified time of the day. The RT ... |
| **ABSTR** | [DataTransfer](#131b-datatransfer-abstr) | [131B](#131b-datatransfer-abstr) | Transfers data between physical memory and a mass-storage de... |
| **ADRIOO** | [TranslateAddress](#430b-translateaddress-adrioo) | [430B](#430b-translateaddress-adrioo) | The call formats are marked from a-i. All formats give statu... |
| **AIRDW** | [ReadADChannel](#37b-readadchannel-airdw) | [37B](#37b-readadchannel-airdw) | Reads an analog to digital channel.... |
| **ALTOFF** | [NormalPageTable](#34b-normalpagetable-altoff) | [34B](#34b-normalpagetable-altoff) | **ALTOFF** Sets the alternative page table equal to the norm... |
| **ALTON** | [AltPageTable](#33b-altpagetable-alton) | [33B](#33b-altpagetable-alton) | Switches page table. Each page table allows you to access 12... |
| **APSPE** | [AppendSpooling](#240b-appendspooling-apspe) | [240B](#240b-appendspooling-apspe) | Prints a file. The printer has a queue of files waiting to b... |
| **ASSIG** | [AssignCAMACLAM](#154b-assigncamaclam-assig) | [154B](#154b-assigncamaclam-assig) | Assigns a graded LAM in the CAMAC identification table to a ... |
| **ATSGM** | [Attach500Segment](#440b-attach500segment-atsgm) | [440B](#440b-attach500segment-atsgm) | Maps a logical ND-500 data segment onto shared ND-100/ND-500... |
| **B41NW** | [In4x2Bytes](#63b-in4x2bytes-b41nw) | [63B](#63b-in4x2bytes-b41nw) | Reads 8 bytes from a word-oriented or character-oriented dev... |
| **B8INB** | [In8Bytes](#23b-in8bytes-b8inb) | [23B](#23b-in8bytes-b8inb) | Reads 8 bytes from a device. The input is fast, but the moni... |
| **B8OUT** | [Out8Bytes](#24b-out8bytes-b8out) | [24B](#24b-out8bytes-b8out) | Writes 8 bytes to a character device, e.g. a terminal. All 8... |
| **BCLOS** | [BackupClose](#252b-backupclose-bclos) | [252B](#252b-backupclose-bclos) | Closes a file. The version number and the last date accessed... |
| **BCNAF** | [BCNAFCAMAC](#414b-bcnafcamac-bcnaf) | [414B](#414b-bcnafcamac-bcnaf) | Special CAMAC function on the ND-500. (Same as mon 156 TRACB... |
| **BCNAF1** | [BCNAF1CAMAC](#415b-bcnaf1camac-bcnaf1) | [415B](#415b-bcnaf1camac-bcnaf1) | Special CAMAC monitor call for the ND-500. (Same as mon 176 ... |
| **BRKM** | [SetBreak](#4b-setbreak-brkm) | [4B](#4b-setbreak-brkm) | Sets the break characters for a terminal. Normally, a progra... |
| **CAMAC** | [CAMACFunction](#147b-camacfunction-camac) | [147B](#147b-camacfunction-camac) | Operates the CAMAC, i.e. executes the NAF register. CAMAC is... |
| **CAPCLE** | [ClearCapability](#424b-clearcapability-capcle) | [424B](#424b-clearcapability-capcle) | Clears a capability. A capability describes each logical seg... |
| **CAPCOP** | [CopyCapability](#423b-copycapability-capcop) | [423B](#423b-copycapability-capcop) | Copies a capability for a segment. The segment itself is als... |
| **CIBUF** | [ClearInBuffer](#13b-clearinbuffer-cibuf) | [13B](#13b-clearinbuffer-cibuf) | Clears a device input buffer. Input from character devices, ... |
| **CLADJ** | [AdjustClock](#112b-adjustclock-cladj) | [112B](#112b-adjustclock-cladj) | Sets the computer's clock (i.e. the current system time) for... |
| **CLOCK** | [GetCurrentTime](#113b-getcurrenttime-clock) | [113B](#113b-getcurrenttime-clock) | Gets the current system time and date.... |
| **CLOSE** | [CloseFile](#43b-closefile-close) | [43B](#43b-closefile-close) | Closes one or more files. Files must be opened before they a... |
| **COBUF** | [ClearOutBuffer](#14b-clearoutbuffer-cobuf) | [14B](#14b-clearoutbuffer-cobuf) | Clears a device output buffer. Output to character devices, ... |
| **COMMND** | [CallCommand](#70b-callcommand-commnd) | [70B](#70b-callcommand-commnd) | Executes a SINTRAN III command from a program. The program t... |
| **CONCT** | [StartOnInterrupt](#106b-startoninterrupt-conct) | [106B](#106b-startoninterrupt-conct) | StartOnInterrupt connects an RT program to interrupts from a... |
| **COPAG** | [CopyPage](#251b-copypage-copag) | [251B](#251b-copypage-copag) | Copies file pages between two opened files. One of the files... |
| **CPUST** | [GetSystemInfo](#262b-getsysteminfo-cpust) | [262B](#262b-getsysteminfo-cpust) | Gets various system information. The system number, the CPU ... |
| **CRALF** | [CreateFile](#221b-createfile-cralf) | [221B](#221b-createfile-cralf) | Creates a file. The file may be indexed, contiguous, or allo... |
| **CRALN** | [NewFileVersion](#253b-newfileversion-craln) | [253B](#253b-newfileversion-craln) | Creates new versions of a file. You may create new versions ... |
| **DABST** | [ExactStartup](#127b-exactstartup-dabst) | [127B](#127b-exactstartup-dabst) | Starts an RT program at a specific time. The time is given i... |
| **DEABF** | [FullFileName](#256b-fullfilename-deabf) | [256B](#256b-fullfilename-deabf) | Returns a complete file name from an abbreviated one. The di... |
| **DELPG** | [DeletePage](#272b-deletepage-delpg) | [272B](#272b-deletepage-delpg) | Deletes pages from a file. Pages between two page numbers ar... |
| **DESCF** | [DisableEscape](#71b-disableescape-descf) | [71B](#71b-disableescape-descf) | The ESCAPE key on the terminal normally terminates a program... |
| **DIASS** | [DisAssemble](#401b-disassemble-diass) | [401B](#401b-disassemble-diass) | Disassembles one machine instruction on the ND-500. Output i... |
| **DINTV** | [ExactInterval](#130b-exactinterval-dintv) | [130B](#130b-exactinterval-dintv) | Prepares an RT program for periodic execution. The interval ... |
| **DIW** | [GETINREGISTERS](#165b-getinregisters-diw) | [165B](#165b-getinregisters-diw) | Reads the device interface registers.... |
| **DLOFU** | [DISABLELOCAL](#277b-disablelocal-dlofu) | [277B](#277b-disablelocal-dlofu) | You may log in on remote computers through the COSMOS data n... |
| **DOPEN** | [DirectOpen](#220b-directopen-dopen) | [220B](#220b-directopen-dopen) | Opens a file. Files must be opened before they can be access... |
| **DROBJ** | [GetObjectEntry](#215b-getobjectentry-drobj) | [215B](#215b-getobjectentry-drobj) | Gets information about a file. An object entry describes eac... |
| **DSCNT** | [NoInterruptStart](#107b-nointerruptstart-dscnt) | [107B](#107b-nointerruptstart-dscnt) | StartOnInterrupt connects an RT program to interrupts from a... |
| **DSET** | [ExactDelayStart](#126b-exactdelaystart-dset) | [126B](#126b-exactdelaystart-dset) | Sets an RT program to start after a given period. It is then... |
| **DUSEL** | [StopEscapeHandling](#301b-stopescapehandling-dusel) | [301B](#301b-stopescapehandling-dusel) | Disables user-defined escape handling. The ESCAPE key termin... |
| **DVINST** | [InputString](#503b-inputstring-dvinst) | [503B](#503b-inputstring-dvinst) | Reads a string from a device, e.g., a terminal or an opened ... |
| **DVOUTS** | [OutputString](#504b-outputstring-dvouts) | [504B](#504b-outputstring-dvouts) | Writes a string to a device, e.g. a terminal or an opened fi... |
| **DWOBJ** | [SetObjectEntry](#216b-setobjectentry-dwobj) | [216B](#216b-setobjectentry-dwobj) | Changes the description of a file. An object entry describes... |
| **ECHOM** | [SetEcho](#3b-setecho-echom) | [3B](#3b-setecho-echom) | When you press a key on the terminal, a character is normall... |
| **EDTMP** | [TerminationHandling](#206b-terminationhandling-edtmp) | [206B](#206b-terminationhandling-edtmp) | Switches termination handling on and off.... |
| **EESCF** | [EnableEscape](#72b-enableescape-eescf) | [72B](#72b-enableescape-eescf) | Enables the ESCAPE key on the terminal. The ESCAPE key norma... |
| **ELOFF** | [OffEscLocalFunction](#303b-offesclocalfunction-eloff) | [303B](#303b-offesclocalfunction-eloff) | Delays the escape and local functions for your terminal. The... |
| **ELOFU** | [EnableLocal](#276b-enablelocal-elofu) | [276B](#276b-enablelocal-elofu) | You may log in on remote computers through the COSMOS data n... |
| **ELON** | [ONESCLOCALFUNCTION](#302b-onesclocalfunction-elon) | [302B](#302b-onesclocalfunction-elon) | Enables delayed escape and local functions for your terminal... |
| **ENTSG** | [SegmentToPageTable](#157b-segmenttopagetable-entsg) | [157B](#157b-segmenttopagetable-entsg) | Enters a routine as a direct task or as a device driver, and... |
| **ERMON** | [ToErrorDevice](#142b-toerrordevice-ermon) | [142B](#142b-toerrordevice-ermon) | Outputs a user-defined, real-time error. The error message i... |
| **ERMSG** | [WarningMessage](#64b-warningmessage-ermsg) | [64B](#64b-warningmessage-ermsg) | Outputs a file system error message. Appendix A shows the me... |
| **EUSEL** | [SetEscapeHandling](#300b-setescapehandling-eusel) | [300B](#300b-setescapehandling-eusel) | Enables user-defined escape handling. When the ESCAPE key is... |
| **EXABS** | [TRANSFERDATA](#335b-transferdata-exabs) | [335B](#335b-transferdata-exabs) | Transfers data between physical memory and a mass-storage de... |
| **EXIOX** | [IOInstruction](#31b-ioinstruction-exiox) | [31B](#31b-ioinstruction-exiox) | Executes an IOX machine instruction. The IOX instruction han... |
| **EXPFI** | [ExpandFile](#231b-expandfile-expfi) | [231B](#231b-expandfile-expfi) | Expands the file size. You use this monitor call to increase... |
| **FDFDI** | [GetDefaultDir](#250b-getdefaultdir-fdfdi) | [250B](#250b-getdefaultdir-fdfdi) | Gets the user’s default directory. The directory index and t... |
| **FDINA** | [GetDirNameIndex](#243b-getdirnameindex-fdina) | [243B](#243b-getdirnameindex-fdina) | Gets directory index and name index. The name index identifi... |
| **FIX** | [FixScattered](#115b-fixscattered-fix) | [115B](#115b-fixscattered-fix) | Place a segment in physical memory. Its pages will no longer... |
| **FIXC** | [FixContiguous](#160b-fixcontiguous-fixc) | [160B](#160b-fixcontiguous-fixc) | Places a segment in physical memory. Its pages will no longe... |
| **FIXCS** | [MemoryAllocation](#61b-memoryallocation-fixcs) | [61B](#61b-memoryallocation-fixcs) | Fixes or unfixes ND-100 segments to be used by the ND-500 Mo... |
| **FIXMEM** | [FixInMemory](#410b-fixinmemory-fixmem) | [410B](#410b-fixinmemory-fixmem) | Fixes a logical segment of your domain in physical memory. Y... |
| **FOBJN** | [GetFileIndexes](#274b-getfileindexes-fobjn) | [274B](#274b-getfileindexes-fobjn) | Gets the directory index, the user index, and the object ind... |
| **FOPEN** | [OpenFileInfo](#257b-openfileinfo-fopen) | [257B](#257b-openfileinfo-fopen) | Monitor call 257B - OpenFileInfo... |
| **FSCDNT** | [FileNotAsSegment](#413b-filenotassegment-fscdnt) | [413B](#413b-filenotassegment-fscdnt) | Disconnects a file as a segment in your domain. FileAsSegmen... |
| **FSCNT** | [FileAsSegment](#412b-fileassegment-fscnt) | [412B](#412b-fileassegment-fscnt) | Connects a file as a segment to your domain. You can then ac... |
| **FSMTY** | [FileSystemFunction](#327b-filesystemfunction-fsmty) | [327B](#327b-filesystemfunction-fsmty) | Multifunction monitor call to make sure that an uncontrolled... |
| **GASGM** | [GetActiveSegment](#421b-getactivesegment-gasgm) | [421B](#421b-getactivesegment-gasgm) | Gets the name of the segments in your domain. A 2048 byte bu... |
| **GBSIZ** | [GetAddressArea](#222b-getaddressarea-gbsiz) | [222B](#222b-getaddressarea-gbsiz) | Gets the size of your address area. Your address area may co... |
| **GDEVT** | [GetDeviceType](#263b-getdevicetype-gdevt) | [263B](#263b-getdevicetype-gdevt) | Gets the device type, e.g. terminal, floppy disk, mass-stora... |
| **GDIEN** | [GetDirEntry](#244b-getdirentry-gdien) | [244B](#244b-getdirentry-gdien) | Gets information about a directory. The directory entry is r... |
| **GERDV** | [GetErrorDevice](#254b-geterrordevice-gerdv) | [254B](#254b-geterrordevice-gerdv) | Gets the logical device number of the error device. The erro... |
| **GERRCOD** | [GetTrapReason](#505b-gettrapreason-gerrcod) | [505B](#505b-gettrapreason-gerrcod) | Gets the error code from the swapper process. This is only r... |
| **GETRT** | [GetOwnRTAddress](#30b-getownrtaddress-getrt) | [30B](#30b-getownrtaddress-getrt) | Gets the address of the calling program's RT description. Ba... |
| **GETXM** | [GetErrorMessage](#334b-geterrormessage-getxm) | [334B](#334b-geterrormessage-getxm) | Gets a SINTRAN III error message text. Appendix A shows the ... |
| **GL** | [CAMACGLRegister](#150b-camacglregister-gl) | [150B](#150b-camacglregister-gl) | Read the CAMAC GL (Graded LAM -"look at me") register or the... |
| **GNAEN** | [GetNameEntry](#245b-getnameentry-gnaen) | [245B](#245b-getnameentry-gnaen) | Gets information about devices, e.g. disks and floppy disks.... |
| **GPRNAM** | [GetProcessNo](#426b-getprocessno-gprnam) | [426B](#426b-getprocessno-gprnam) | Gets the number of a process in the ND-500. You specify the ... |
| **GPRNME** | [GetOwnProcessInfo](#427b-getownprocessinfo-gprnme) | [427B](#427b-getownprocessinfo-gprnme) | Gets the name and number of your own process in the ND-500. ... |
| **GRAPH** | [GRAPHICFUNCTION](#155b-graphicfunction-graph) | [155B](#155b-graphicfunction-graph) | Executes various functions on a graphic peripheral, such as ... |
| **GRBLK** | [GetUserRegisters](#420b-getuserregisters-grblk) | [420B](#420b-getuserregisters-grblk) | SwitchUserBreak allows you to save the registers when you te... |
| **GRTDA** | [GetRTAddress](#151b-getrtaddress-grtda) | [151B](#151b-getrtaddress-grtda) | Gets the address of an RT description. You specify the name ... |
| **GRTNA** | [GetRTName](#152b-getrtname-grtna) | [152B](#152b-getrtname-grtna) | Gets the name of an RT program. You specify the RT descripti... |
| **GSGNO** | [GetSegmentNo](#322b-getsegmentno-gsgno) | [322B](#322b-getsegmentno-gsgno) | Gets the number of a segment in the ND-100. You specify the ... |
| **GSWSP** | [GetScratchSegment](#422b-getscratchsegment-gswsp) | [422B](#422b-getscratchsegment-gswsp) | Connects an empty data segment to the user's domain and reserves space for it on the swap file. The segment is assigned the default name "SCRATCH-SEGMENT:DSEG". |
| **GTMOD** | [GetTerminalMode](#306b-getterminalmode-gtmod) | [306B](#306b-getterminalmode-gtmod) | Gets the terminal mode. The terminal mode tells how the term... |
| **GUIOI** | [GetAllFileIndexes](#217b-getallfileindexes-guioi) | [217B](#217b-getallfileindexes-guioi) | Gets the directory index, the user index, and the object ind... |
| **GUSNA** | [GetUserName](#214b-getusername-gusna) | [214B](#214b-getusername-gusna) | Gets the name of a user. The user may be on a remote compute... |
| **HOLD** | [SuspendProgram](#104b-suspendprogram-hold) | [104B](#104b-suspendprogram-hold) | Suspends the execution of your program for a given time. The... |
| **IBRISZ** | [InBufferState](#313b-inbufferstate-ibrisz) | [313B](#313b-inbufferstate-ibrisz) | Gets information about an input buffer. The current number o... |
| **INBT** | [InByte](#1b-inbyte-inbt) | [1B](#1b-inbyte-inbt) | Reads one byte from a character device, e.g. a terminal or a... |
| **INSTR** | [INSTRING](#161b-instring-instr) | [161B](#161b-instring-instr) | Reads a string of characters from a peripheral device, e.g. ... |
| **INTV** | [StartupInterval](#103b-startupinterval-intv) | [103B](#103b-startupinterval-intv) | Prepares an RT program for periodic execution. The interval ... |
| **IOFIX** | [FixIOArea](#404b-fixioarea-iofix) | [404B](#404b-fixioarea-iofix) | Fixes an address area in a domain in physical memory. The me... |
| **IOMTY** | [TERMINAL](#336b-terminal-iomty) | [336B](#336b-terminal-iomty) | This I/O multifunction monitor call is used to change the at... |
| **IOSET** | [DeviceControl](#141b-devicecontrol-ioset) | [141B](#141b-devicecontrol-ioset) | Sets control information for a character device, e.g. a term... |
| **IOUT** | [OutNumber](#35b-outnumber-iout) | [35B](#35b-outnumber-iout) | Writes a number to the user's terminal. The number can be ou... |
| **IOXIN** | [CAMACIOInstruction](#153b-camacioinstruction-ioxin) | [153B](#153b-camacioinstruction-ioxin) | Executes a single IOX instruction for CAMAC. See under CAMAC... |
| **IPRIV** | [PrivInstruction](#146b-privinstruction-ipriv) | [146B](#146b-privinstruction-ipriv) | Executes a privileged machine instruction on the ND-100. Pri... |
| **ISIZE** | [InBufferSpace](#66b-inbufferspace-isize) | [66B](#66b-inbufferspace-isize) | Gets the current number of bytes in the input buffer. Termin... |
| **LASTC** | [GetLastByte](#26b-getlastbyte-lastc) | [26B](#26b-getlastbyte-lastc) | Gets the last character typed on a terminal. The monitor cal... |
| **LEAVE** | [ExitFromProgram](#0b-exitfromprogram-leave) | [0B](#0b-exitfromprogram-leave) | Terminates the program. Returns to SINTRAN III. Batch jobs c... |
| **M8INB** | [InUpTo8Bytes](#21b-inupto8bytes-m8inb) | [21B](#21b-inupto8bytes-m8inb) | See also In8Bytes, InByte, InString, In4x2Bytes, and Out8Byt... |
| **M8OUT** | [OutUpTo8Bytes](#22b-outupto8bytes-m8out) | [22B](#22b-outupto8bytes-m8out) | Writes up to 8 characters to a device, e.g. a terminal or an... |
| **MACROE** | [ErrorReturn](#400b-errorreturn-macroe) | [400B](#400b-errorreturn-macroe) | Terminates the program and sets an error code. The error cod... |
| **MAGTP** | [DeviceFunction](#144b-devicefunction-magtp) | [144B](#144b-devicefunction-magtp) | Performs various operations on floppy disks, magnetic tapes,... |
| **MBECH** | [BATCHMODEECHO](#325b-batchmodeecho-mbech) | [325B](#325b-batchmodeecho-mbech) | Controls echo of input and output if the program is executed... |
| **MCALL** | [JumpToSegment](#132b-jumptosegment-mcall) | [132B](#132b-jumptosegment-mcall) | Calls a routine on another segment in the ND-100. You can di... |
| **MDLFI** | [DeleteFile](#54b-deletefile-mdlfi) | [54B](#54b-deletefile-mdlfi) | Deletes a file. The pages of the file are released.... |
| **MEXIT** | [ExitFromSegment](#133b-exitfromsegment-mexit) | [133B](#133b-exitfromsegment-mexit) | Exchanges one or both current segments. Commonly used to ret... |
| **MGDAE** | [GEtEscLocalChars](#230b-getesclocalchars-mgdae) | [230B](#230b-getesclocalchars-mgdae) | Gets ESCAPE and LOCAL characters. You can terminate most pro... |
| **MGFIL** | [GetFileName](#273b-getfilename-mgfil) | [273B](#273b-getfilename-mgfil) | Gets the name of a file. You specify the directory index, th... |
| **MGTTY** | [GetTerminalType](#16b-getterminaltype-mgtty) | [16B](#16b-getterminaltype-mgtty) | Gets the terminal type. The terminal type tells SINTRAN III ... |
| **MHDLC** | [HDLCfunction](#201b-hdlcfunction-mhdlc) | [201B](#201b-hdlcfunction-mhdlc) | Performs various HDLC functions. A HDLC is a high-level data... |
| **MLAMU** | [LAMUFunction](#315b-lamufunction-mlamu) | [315B](#315b-lamufunction-mlamu) | Performs various functions on the LAMU system. A LAMU is a l... |
| **MLOGI** | [LogInStart](#326b-loginstart-mlogi) | [326B](#326b-loginstart-mlogi) | Allowed in RT programs (ring ≥ 1), and in all programs run b... |
| **MOINF** | [CheckMonCall](#312b-checkmoncall-moinf) | [312B](#312b-checkmoncall-moinf) | Some monitor calls are optional or only available in later v... |
| **MRNFI** | [RenameFile](#232b-renamefile-mrnfi) | [232B](#232b-renamefile-mrnfi) | See also @RENAME-FILE.... |
| **MSDAE** | [SetEscLocalChars](#227b-setesclocalchars-msdae) | [227B](#227b-setesclocalchars-msdae) | You can terminate most programs with the ESCAPE key. A LOCAL... |
| **MSG** | [OutMessage](#32b-outmessage-msg) | [32B](#32b-outmessage-msg) | Writes a message to the user's terminal. This is convenient ... |
| **MSTTY** | [SetTerminalType](#17b-setterminaltype-mstty) | [17B](#17b-setterminaltype-mstty) | Sets the type of a terminal. The terminal type tells SINTRAN... |
| **MUIDI** | [GetDirUserIndexes](#213b-getdiruserindexes-muidi) | [213B](#213b-getdiruserindexes-muidi) | Gets a directory index and a user index. You have to specify... |
| **MWAITF** | [AwaitTransfer](#431b-awaittransfer-mwaitf) | [431B](#431b-awaittransfer-mwaitf) | Checks that a data transfer to or from a mass-storage file i... |
| **MXPISG** | [MaxPagesInMemory](#417b-maxpagesinmemory-mxpisg) | [417B](#417b-maxpagesinmemory-mxpisg) | Sets the maximum number of pages a segment may have in physi... |
| **NOWT** | [NoWaitSwitch](#36b-nowaitswitch-nowt) | [36B](#36b-nowaitswitch-nowt) | Switches No Wait on and off. No Wait is useful for input fro... |
| **OCTIO** | [OctobusFunction](#324b-octobusfunction-octio) | [324B](#324b-octobusfunction-octio) | Performs various functions on an old Octobus (earlier than v... |
| **OPEN** | [OpenFile](#50b-openfile-open) | [50B](#50b-openfile-open) | Opens a file. You cannot access a file before you open it. S... |
| **OSIZE** | [OutBufferSpace](#67b-outbufferspace-osize) | [67B](#67b-outbufferspace-osize) | Gets the number of free bytes in the output buffer (number o... |
| **OUTBT** | [OutByte](#2b-outbyte-outbt) | [2B](#2b-outbyte-outbt) | Writes one byte to a character device, e.g. a terminal or an... |
| **OUTST** | [OutString](#162b-outstring-outst) | [162B](#162b-outstring-outst) | Writes a string of characters to a peripheral file, e.g., a ... |
| **PAGEI** | [GetUserParam](#57b-getuserparam-pagei) | [57B](#57b-getuserparam-pagei) | Gets information about why the last program terminated. Ther... |
| **PASET** | [SetUserParam](#56b-setuserparam-paset) | [56B](#56b-setuserparam-paset) | Sets information about a background program. Use GetUserPara... |
| **PIOCM** | [PIOCCFunction](#255b-pioccfunction-piocm) | [255B](#255b-pioccfunction-piocm) | PIOCC (Programmed Input/Output Control Channel) function mon... |
| **PRIOR** | [SetRTPriority](#110b-setrtpriority-prior) | [110B](#110b-setrtpriority-prior) | Sets the priority of an RT program. RT programs may be given... |
| **PRLRS** | [ForceRelease](#125b-forcerelease-prlrs) | [125B](#125b-forcerelease-prlrs) | Releases a device reserved by an RT program other than that ... |
| **PRSRV** | [ForceReserve](#124b-forcereserve-prsrv) | [124B](#124b-forcereserve-prsrv) | Reserves a device for an RT program other than that which is... |
| **PRT** | [ForceTrap](#435b-forcetrap-prt) | [435B](#435b-forcetrap-prt) | Forces a programmed trap to occur in another ND-500 process.... |
| **QERMS** | [ErrorMessage](#65b-errormessage-qerms) | [65B](#65b-errormessage-qerms) | Displays a file system error message. Appendix A shows the m... |
| **RDISK** | [ReadScratchFile](#5b-readscratchfile-rdisk) | [5B](#5b-readscratchfile-rdisk) | Reads randomly from the scratch file. One block is transferr... |
| **RDPAG** | [ReadDiskPage](#270b-readdiskpage-rdpag) | [270B](#270b-readdiskpage-rdpag) | Reads one or more directory pages. Any page can be read.... |
| **REABT** | [GetStartByte](#75b-getstartbyte-reabt) | [75B](#75b-getstartbyte-reabt) | Gets the number of the next byte to access in a file. The by... |
| **REDIR** | [ReserveDir](#246b-reservedir-redir) | [246B](#246b-reservedir-redir) | Reserves a directory for special use. The directory must be ... |
| **REENT** | [AttachSegment](#167b-attachsegment-reent) | [167B](#167b-attachsegment-reent) | Attaches a reentrant segment to your two current segments. T... |
| **RELES** | [ReleaseResource](#123b-releaseresource-reles) | [123B](#123b-releaseresource-reles) | Releases a reserved device or file. The resource can then be... |
| **RERRP** | [GetErrorInfo](#207b-geterrorinfo-rerrp) | [207B](#207b-geterrorinfo-rerrp) | Gets information about the last real-time error. The monitor... |
| **RESRV** | [ReserveResource](#122b-reserveresource-resrv) | [122B](#122b-reserveresource-resrv) | Reserves a device or file for your program only. You release... |
| **RFILE** | [ReadFromFile](#117b-readfromfile-rfile) | [117B](#117b-readfromfile-rfile) | Reads any number of bytes from a file. The read operation mu... |
| **RFLAG** | [GetInputFlags](#402b-getinputflags-rflag) | [402B](#402b-getinputflags-rflag) | ND-100 and ND-500 programs may communicate through two 32-bi... |
| **RLDIR** | [ReleaseDir](#247b-releasedir-rldir) | [247B](#247b-releasedir-rldir) | Releases a directory. The directory must have been reserved ... |
| **RMAX** | [GetBytesInFile](#62b-getbytesinfile-rmax) | [62B](#62b-getbytesinfile-rmax) | Gets the number of bytes in a file. Only the bytes containin... |
| **ROBJE** | [ReadObjectEntry](#41b-readobjectentry-robje) | [41B](#41b-readobjectentry-robje) | Gets information about an opened file. An object entry descr... |
| **RPAGE** | [ReadBlock](#7b-readblock-rpage) | [7B](#7b-readblock-rpage) | Reads randomly from a file. You read one block at a time. Th... |
| **RSEGM** | [GetSegmentEntry](#53b-getsegmententry-rsegm) | [53B](#53b-getsegmententry-rsegm) | Gets information about a segment in the ND-100. The monitor ... |
| **RSIO** | [ExecutionInfo](#143b-executioninfo-rsio) | [143B](#143b-executioninfo-rsio) | Gets information about the execution of the calling program.... |
| **RSQPE** | [GetSpoolingEntry](#55b-getspoolingentry-rsqpe) | [55B](#55b-getspoolingentry-rsqpe) | Gets the next spooling queue entry, that is, the next file t... |
| **RSREC** | [READSYSTEMRECORD](#340b-readsystemrecord-rsrec) | [340B](#340b-readsystemrecord-rsrec) | Used to read the system record into a buffer.... |
| **RT** | [StartRTProgram](#100b-startrtprogram-rt) | [100B](#100b-startrtprogram-rt) | Starts an RT program. The program is moved to the execution ... |
| **RTDSC** | [GetRTDescr](#27b-getrtdescr-rtdsc) | [27B](#27b-getrtdescr-rtdsc) | Reads an RT description. The RT description contains various... |
| **RTEXT** | [ExitRTProgram](#134b-exitrtprogram-rtext) | [134B](#134b-exitrtprogram-rtext) | Terminates the calling RT or background program. Releases al... |
| **RTOFF** | [DisableRTStart](#137b-disablertstart-rtoff) | [137B](#137b-disablertstart-rtoff) | Disables start of RT programs. No RT program can be started ... |
| **RTON** | [EnableRTStart](#136b-enablertstart-rton) | [136B](#136b-enablertstart-rton) | RTON RT programs cannot be started after DisableRTStart has ... |
| **RTWT** | [WAITFORRESTART](#135b-waitforrestart-rtwt) | [135B](#135b-waitforrestart-rtwt) | Sets the RT program in a waiting state. It is restarted by S... |
| **RUSCN** | [OldUser](#242b-olduser-ruscn) | [242B](#242b-olduser-ruscn) | Switches back to the user name you were logged in under befo... |
| **RUSER** | [GetUserEntry](#44b-getuserentry-ruser) | [44B](#44b-getuserentry-ruser) | Gets information about a user. The user entry in the directo... |
| **RWRTC** | [AccessRTCommon](#406b-accessrtcommon-rwrtc) | [406B](#406b-accessrtcommon-rwrtc) | Reads from or writes to RT common from an ND-500 program. RT... |
| **SCROP** | [ScratchOpen](#235b-scratchopen-scrop) | [235B](#235b-scratchopen-scrop) | Opens a file as a scratch file. A maximum of 64 pages of the... |
| **SET** | [DelayStart](#101b-delaystart-set) | [101B](#101b-delaystart-set) | Starts an RT program after a specified time. The RT program ... |
| **SETBL** | [SetStartBlock](#77b-setstartblock-setbl) | [77B](#77b-setstartblock-setbl) | Sets the next block to be read or written in an opened file.... |
| **SETBS** | [SetBlockSize](#76b-setblocksize-setbs) | [76B](#76b-setblocksize-setbs) | Sets the block size of an opened file. Monitor calls which r... |
| **SETBT** | [SetStartByte](#74b-setstartbyte-setbt) | [74B](#74b-setstartbyte-setbt) | Sets the next byte to be read or written in an opened mass-s... |
| **SETCM** | [SetCommandBuffer](#12b-setcommandbuffer-setcm) | [12B](#12b-setcommandbuffer-setcm) | Transfers a string to the command buffer. The command buffer... |
| **SFACC** | [SetFileAccess](#237b-setfileaccess-sfacc) | [237B](#237b-setfileaccess-sfacc) | Sets the access protection for a file. You should specify th... |
| **SGMTY** | [SegmentFunction](#341b-segmentfunction-sgmty) | [341B](#341b-segmentfunction-sgmty) | This is a multifunction monitor call used to change the acti... |
| **SMAX** | [SetMaxBytes](#73b-setmaxbytes-smax) | [73B](#73b-setmaxbytes-smax) | Sets the value of the maximum byte pointer in an opened file... |
| **SPCHG** | [ChangeSegment](#337b-changesegment-spchg) | [337B](#337b-changesegment-spchg) | Changes the segment and the page table your program uses. Th... |
| **SPCLO** | [CloseSpoolingFile](#40b-closespoolingfile-spclo) | [40B](#40b-closespoolingfile-spclo) | Appends an opened file to a spooling queue. You specify a te... |
| **SPEFI** | [SetPeripheralName](#234b-setperipheralname-spefi) | [234B](#234b-setperipheralname-spefi) | Defines a peripheral file, e.g. a printer. You connect a fil... |
| **SPERD** | [SetPermanentOpen](#236b-setpermanentopen-sperd) | [236B](#236b-setpermanentopen-sperd) | Sets a file permanently open. The file is not closed by Clos... |
| **SPLRE** | [SegmentOverlay](#323b-segmentoverlay-splre) | [323B](#323b-segmentoverlay-splre) | Used to build multisegment programs in the ND-100. It is mai... |
| **SPRIO** | [SetProcessPriority](#507b-setprocesspriority-sprio) | [507B](#507b-setprocesspriority-sprio) | Sets the priority for a process in the ND-500. The prioritie... |
| **SPRNAM** | [SetProcessName](#425b-setprocessname-sprnam) | [425B](#425b-setprocessname-sprnam) | Defines a new name for your process.... |
| **SREEN** | [ReentrantSegment](#212b-reentrantsegment-sreen) | [212B](#212b-reentrantsegment-sreen) | Connects a reentrant segment to your two current segments. A... |
| **SRLMO** | [SetRemoteAccess](#316b-setremoteaccess-srlmo) | [316B](#316b-setremoteaccess-srlmo) | Switches remote file access on and off. The COSMOS network a... |
| **SRUSI** | [DefaultRemoteSystem](#314b-defaultremotesystem-srusi) | [314B](#314b-defaultremotesystem-srusi) | Sets default values for COSMOS remote file access. You can s... |
| **STARTP** | [StartProcess](#500b-startprocess-startp) | [500B](#500b-startprocess-startp) | Starts a process in the ND-500. You identify the process wit... |
| **STEFI** | [SetTemporaryFile](#233b-settemporaryfile-stefi) | [233B](#233b-settemporaryfile-stefi) | Defines a file to store information temporarily. The file ca... |
| **STOPPR** | [StopProcess](#501b-stopprocess-stoppr) | [501B](#501b-stopprocess-stoppr) | Sets the current process in a wait state. StartProcess resta... |
| **STRFI** | [SetTerminalName](#275b-setterminalname-strfi) | [275B](#275b-setterminalname-strfi) | Defines the file name to be used for terminals. This is norm... |
| **SUSCN** | [NewUser](#241b-newuser-suscn) | [241B](#241b-newuser-suscn) | Switches the user name you are logged in under. The command ... |
| **SWITCHP** | [SwitchProcess](#502b-switchprocess-switchp) | [502B](#502b-switchprocess-switchp) | Sets the current process in a wait state. Restarts another p... |
| **TBIN8** | [In8AndFlag](#310b-in8andflag-tbin8) | [310B](#310b-in8andflag-tbin8) | Reads 8 bytes from a device, e.g., a terminal. The monitor c... |
| **TERMO** | [TerminalMode](#52b-terminalmode-termo) | [52B](#52b-terminalmode-termo) | Selects various terminal functions. You may stop output on f... |
| **TERST** | [TerminalStatus](#330b-terminalstatus-terst) | [330B](#330b-terminalstatus-terst) | Gets information about a terminal. The user logged in, the t... |
| **TIME** | [GetBasicTime](#11b-getbasictime-time) | [11B](#11b-getbasictime-time) | **Time**... |
| **TMOUT** | [TimeOut](#267b-timeout-tmout) | [267B](#267b-timeout-tmout) | Suspends the execution of your program for a given time. The... |
| **TNOWAI** | [TerminalNoWait](#307b-terminalnowait-tnowai) | [307B](#307b-terminalnowait-tnowai) | Switches No Wait on and off. No Wait is useful for input fro... |
| **TREPP** | [TerminalLineInfo](#332b-terminallineinfo-trepp) | [332B](#332b-terminallineinfo-trepp) | Gets information about a terminal line. You may also enable ... |
| **TUSED** | [GetTimeUsed](#114b-gettimeused-tused) | [114B](#114b-gettimeused-tused) | Gets the time you have used the CPU since you logged in. In ... |
| **UDMA** | [DMAFunction](#333b-dmafunction-udma) | [333B](#333b-dmafunction-udma) | Monitor call 333B - DMAFunction... |
| **UECOM** | [ExecuteCommand](#317b-executecommand-uecom) | [317B](#317b-executecommand-uecom) | Executes a SINTRAN III command. Specify the command name and... |
| **UNFIX** | [UnfixSegment](#116b-unfixsegment-unfix) | [116B](#116b-unfixsegment-unfix) | Releases a fixed segment and removes it from the Page Index ... |
| **UNFIXM** | [MemoryUnfix](#411b-memoryunfix-unfixm) | [411B](#411b-memoryunfix-unfixm) | Releases a fixed segment in your domain from physical memory... |
| **UPDAT** | [SetClock](#111b-setclock-updat) | [111B](#111b-setclock-updat) | Gives new values to the computer's clock and calendar. If th... |
| **US0** | [UserDef0](#170b-userdef0-us0) | [170B](#170b-userdef0-us0) | User-defined monitor call. You can implement up to 8 monitor... |
| **US1** | [UserDef1](#171b-userdef1-us1) | [171B](#171b-userdef1-us1) | User-defined monitor call. You can implement up to 8 monitor... |
| **US2** | [UserDef2](#172b-userdef2-us2) | [172B](#172b-userdef2-us2) | User-defined monitor call. You can implement up to 8 monitor... |
| **US3** | [UserDef3](#173b-userdef3-us3) | [173B](#173b-userdef3-us3) | User-defined monitor call. You can implement up to 8 monitor... |
| **US4** | [UserDef4](#174b-userdef4-us4) | [174B](#174b-userdef4-us4) | User-defined monitor call. You can implement up to 8 monitor... |
| **US5** | [UserDef5](#175b-userdef5-us5) | [175B](#175b-userdef5-us5) | User-defined monitor call. You can implement up to 8 monitor... |
| **US6** | [UserDef6](#176b-userdef6-us6) | [176B](#176b-userdef6-us6) | User-defined monitor call. You can implement up to 8 monitor... |
| **US7** | [UserDef7](#177b-userdef7-us7) | [177B](#177b-userdef7-us7) | User-defined monitor call. You can implement up to 8 monitor... |
| **USTRK** | [SwitchUserBreak](#405b-switchuserbreak-ustrk) | [405B](#405b-switchuserbreak-ustrk) | Switches user-defined escape handling on and off. The user-d... |
| **WAITF** | [AwaitFileTransfer](#121b-awaitfiletransfer-waitf) | [121B](#121b-awaitfiletransfer-waitf) | Checks that a data transfer to or from a mass-storage file i... |
| **WDIEN** | [WriteDirEntry](#311b-writedirentry-wdien) | [311B](#311b-writedirentry-wdien) | Changes the information about a directory. The complete cont... |
| **WDISK** | [WriteScratchFile](#6b-writescratchfile-wdisk) | [6B](#6b-writescratchfile-wdisk) | Writes randomly to the scratch file. One block is transferre... |
| **WDPAG** | [WriteDiskPage](#271b-writediskpage-wdpag) | [271B](#271b-writediskpage-wdpag) | Writes to one or more pages in a directory. Any page can be ... |
| **WFILE** | [WriteToFile](#120b-writetofile-wfile) | [120B](#120b-writetofile-wfile) | Writes any number of bytes to a file. The read operation mus... |
| **WFLAG** | [SetOutputFlags](#403b-setoutputflags-wflag) | [403B](#403b-setoutputflags-wflag) | ND-100 and ND-500 programs may communicate through two 32-bi... |
| **WHDEV** | [ReservationInfo](#140b-reservationinfo-whdev) | [140B](#140b-reservationinfo-whdev) | Checks that a device is not reserved. If it is reserved, you... |
| **WPAGE** | [WriteBlock](#10b-writeblock-wpage) | [10B](#10b-writeblock-wpage) | Writes randomly to a file. You write one block at a time. Th... |
| **WSEG** | [SaveSegment](#164b-savesegment-wseg) | [164B](#164b-savesegment-wseg) | Saves a segment in the ND-100. All pages in physical memory ... |
| **WSEGN** | [SaveND500Segment](#416b-savend500segment-wsegn) | [416B](#416b-savend500segment-wsegn) | Writes all modified pages of a segment back to the disk.... |
| **XMSG** | [XMSGFunction](#200b-xmsgfunction-xmsg) | [200B](#200b-xmsgfunction-xmsg) | Performs various data communication functions. All types of ... |

---

## Monitor Call Documentation

### 0B - ExitFromProgram (LEAVE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Terminates the program. Returns to SINTRAN III. Batch jobs continues with the next command.

- Background programs close all files not set permanently open. RT programs do not close any files.
- RT programs release all reserved devices.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Language` | Code | I |  |
| `PASCAL` | StopProgram; [Note routine name.] | I |  |
| `COBOL` | MONITOR-CALL "ExitFromProgram". | I |  |
| `FORTRAN` | Monitor_Call('ExitFromProgram') | I |  |

#### See Also

[ExitRTProgram](#134b-exitrtprogram-rtext)

#### Examples

<details>
<summary><strong>PLANC</strong></summary>

```planc
Monitor_Call('ExitFromProgram')
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ExitFromProgram : EQU 37B9 + 0B
...
CALLG ExitFromProgram, 0
IF K GO Error %Possible if wrong number of parameters.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON 0 %Monitor call ExitFromProgram.

| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 185*

[↑ Back to Top](#table-of-contents)

---

### 1B - InByte (INBT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads one byte from a character device, e.g. a terminal or an opened file. If the device is a word-oriented device, one word is read. This monitor call can be used on most input devices.

- Bit 7 is a parity bit if terminal or file input. IOMultiFunction may change this.
- The program waits if there is no bytes in the input buffer of the device. You can change this with NoWaitSwitch or TerminalNoWait.
- The pointer to the next byte is incremented when you read from a mass-storage file.
- Input from card readers are converted to ASCII characters. Use DeviceControl to read the 12-bit card columns.
- Background programs may read from logical device number 0. This is the SINTRAN III command buffer. You may read parameters following the program name this way. Break and echo are both set to 1. Normal SINTRAN III command editing is available. All letters are converted to uppercase. You may control this with IOMultiFunction.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `⟶` | Logical device number. See appendix B. Use 1 for your own terminal. | I |  |
| `←` | The read byte. | I |  |
| `←` | Standard Error Code. See appendix A. | I |  |

#### See Also

[In8AndFlag](#310b-in8andflag-tbin8), InUpTo88Bytes, [In8Bytes](#23b-in8bytes-b8inb), InString, [InputString](#503b-inputstring-dvinst), [In4x2Bytes](#63b-in4x2bytes-b41nw), and OutByte

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, ReturnValue : INTEGER2;
...
InByte(DeviceNumber, ReturnValue);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 ReturnValue COMP.
01 ErrCode COMP.
...
MONITOR-CALL "InByte" USING DeviceNumber, ReturnValue.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, ReturnValue
...
Monitor Call('InByte', DeviceNumber, ReturnValue)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER: DeviceNumber, ReturnValue
..
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ..
ENDON
Monitor_Call('InByte', DeviceNumber, ReturnValue)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber: W BLOCK 1
ReturnValue: W BLOCK 1
ErrCode: W BLOCK 1
InByte: EQU 37B9 + 1B

CALLG InByte, 2, DeviceNumber, ReturnValue
IF' K GO ERROR

ERROR W1.. ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO %Logical device number.
MON 1    %Monitor call InByte.
JMP ERROR %Error return from monitor call.
STA BYTE  %Normal return, store byte read.

ERROR.    %Error number in register A.

DEVNO.
BYTE. 0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 319*

[↑ Back to Top](#table-of-contents)

---

### 2B - OutByte (OUTBT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes one byte to a character device, e.g. a terminal or an opened file. If the device is a word-oriented device, one word is written.

- The program waits if the output buffer of the device is full. You can change this with NoWaitSwitch or TerminalNoWait.
- The pointer to the next byte is incremented when you write to a mass-storage file.
- Output from card readers are converted to ASCII characters. Use DeviceControl to write the 12-bit card columns.
- You are advised to use the faster OutputString on the ND-500.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. Use 1 for your own terminal. |
| `Param2` | UNKNOWN | I | The byte to write. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[OutUpTo8Bytes](#22b-outupto8bytes-m8out), [Out8Bytes](#24b-out8bytes-b8out), [OutString](#162b-outstring-outst), [OutputString](#504b-outputstring-dvouts), [OutMessage](#32b-outmessage-msg), [OutNumber](#35b-outnumber-iout), and InByte

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, OutputValue : INTEGER2;
...
OutByte(DeviceNumber, OutputValue);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 OutputValue COMP.
01 ErrCode COMP.
...
MONITOR-CALL "OutByte" USING DeviceNumber, OutputValue.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, OutputValue
...
Monitor_Call('OutByte', DeviceNumber, OutputValue)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, OutputValue
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_call('OutByte', DeviceNumber, OutputValue)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
OutputValue  : W BLOCK 1
ErrCode      : W BLOCK 1
OutByte      : EQU 37B9 + 2B
...
    CALLG OutByte, 2, DeviceNumber, OutputValue
    IF K GO ERROR
...
ERROR : W1 =: ErrCode       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO        %Logical device number.
LDA BYTE         %Byte to be written.
MON 2            %Monitor call OutByte.
JMP ERROR        %Error return from monitor call.
...              %Normal return.
ERROR,           %Error number in A register.

DEVNO,   ...
BYTE,   'A'



| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 377*

[↑ Back to Top](#table-of-contents)

---

### 3B - SetEcho (ECHOM)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

When you press a key on the terminal, a character is normally displayed. This is called echo. You modify a terminal's echo with this monitor call.

- If 8-bit I/O is set (TerminalFunction, function number 112), you will get echo always if bit 7 in the echo table is set.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The terminal's logical device number. See appendix B. Only needed for RT program |
| `Param2` | UNKNOWN | I | Echo strategy. Less than 0: No echo, e.g. for password fields. |
| `Param3` | UNKNOWN | I | 0: Echo on all characters. |
| `Param4` | UNKNOWN | I | 1: Echo on all characters except control characters. |
| `Param5` | UNKNOWN | I | 2: Special echo used by MAC. |
| `Param6` | UNKNOWN | I | 3-6: System defined echo. |
| `Param7` | UNKNOWN | I | 7: User-defined echo. See the next parameter. |
| `Param8` | UNKNOWN | I | 8: Last user-defined echo table. (Only meaningful with echo strategy 7.) |
| `Param9` | UNKNOWN | I | User-defined echo table. Ignored if the echo is different from 7. Use 128 bits t |

#### See Also

[SetBreak](#4b-setbreak-brkm)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, EchoStrategy : INTEGER2;
Table : RECORD...END;
...
SetEcho(DeviceNumber, EchoStrategy, Table);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 EchoStrategy COMP.
01 Table.
  02 array COMP OCCURS 8 TIMES.
  ...
MONITOR-CALL "SetEcho" USING DeviceNumber, EchoStrategy, Table.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, EchoStrategy
INTEGER Table(8)
...
Monitor_Call('SetEcho', DeviceNumber, EchoStrategy, Table(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, EchoStrategy
INTEGER2ARRAY{0:7}
...
Monitor_Call('SetEcho', DeviceNumber, EchoStrategy, Table(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
EchoStrategy : W BLOCK 1
Table : H BLOCK 8
SetEcho : EQU 37B9 + 3B
...
CALLG SetEcho, 3, DeviceNumber, EchoStrategy, Table
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO  %Logical device number.
LDA STRAT  %Echo strategy.
LDX (TABLE %Eight word large bit map.
MON 3      %Monitor call SetEcho.
...
DEVNO, ...
STRAT, ...
TABLE, ...
*+10/ %Make a buffer of 8 words.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 441*

[↑ Back to Top](#table-of-contents)

---

### 4B - SetBreak (BRKM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the break characters for a terminal. Normally, a program waits for input. When a break character is typed, the program restarts. For example, most subsystems restart when you press the RETURN-key after a command. The subsystems have defined the RETURN-key as a break character.

- SINTRAN III has some predefined break tables.
- You may define your own break table. This is a 128-bit array where each bit represents an ASCII character. Use 1 for the characters you want as break characters. The ability to define your own break tables is optional in older versions of SINTRAN III.
- If 8-bit I/O is set (TerminalFunction, function number 112), you will get break always if bit 7 in the break table is set.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | **Logical device number.** See appendix B. Only used by RT programs. Your own te |
| `Param2` | UNKNOWN | I | **Break strategy.** Negative values gives break on no characters. |
| `0` | Break on all characters. | I |  |
| `1` | Break on control characters, i.e. ASCII values less than 32 and DEL. This is the default. | I |  |
| `2` | Special break table used by the MAC assembler. | I |  |
| `3-6` | System defined break strategy. | I |  |
| `7` | User-defined break table. See the next parameter. | I |  |
| `8` | Last user-defined break table. (Only meaningful with strategy 7.) | I |  |
| `9` | Change the maximum numbers of characters before break only. | I |  |
| `Param10` | UNKNOWN | I | **User-defined break table, i.e. 128 bits which represent the ASCII characters.  |
| `Param11` | UNKNOWN | I | **Maximum number of characters before break.** Dummy with break strategies 0, 1, |

#### See Also

[SetEcho](#3b-setecho-echom)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, BreakStrategy, NoOfChar : INTEGER2;
Table : RECORD...END;
...
SetBreak(DeviceNo, BreakStrategy, Table, NoOfChar);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 BreakStrategy COMP.
01 NoOfChar COMP.
01 Table.
   02 array COMP OCCURS 8 TIMES.
...
MONITOR-CALL "SetBreak" USING DeviceNo, BreakStrategy, Table, NoOfChar.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, Strategy, NoOfChar
INTEGER Table(8)
...
Monitor_Call('SetBreak', DeviceNo, Strategy, Table(1), NoOfChar)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, Strategy, NoOfChar
INTEGER2ARRAY(0:7)
...
Monitor_Call('SetBreak', DeviceNo, Strategy, Table(0), NoOfChar)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
BreakStrategy : W BLOCK 1
NoOfChar : W BLOCK 1
Table : H BLOCK 8
SetBreak : EQU 37B9 + 4B
...
CALLG SetBreak, 4, DeviceNo, BreakStrategy, Table, NoOfChar
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO %Logical device number.
LDA NOCHR %Number of characters input before break,
COPY SA DD % only if STRAT greater or equal to 3.
LDX (TABLE %Address of 8-word large bit map.
LDA STRAT %Break strategy.
MON 4 %Monitor call SetBreak.
...
DEVNO, ...
STRAT, 1 %Break only on control characters.
TABLE, ...
*+10/ %Make an 8-word large buffer.
NOCHR, ...

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 435*

[↑ Back to Top](#table-of-contents)

---

### 5B - ReadScratchFile (RDISK)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads randomly from the scratch file. One block is transferred. There is one scratch file connected to each terminal. It is opened for random read and write access when you log in. Its file number is 100B.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Block number` | Block number to start the reading. | I |  |
| `Array` | Array for the transferred data. | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[ReadBlock](#7b-readblock-rpage), ReadFromFile. ReadFromFile is the most efficient monitor call

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
BlockNumber : INTEGER2;
DataDestination : RECORD...END;
...
ReadScratchFile(BlockNumber, DataDestination);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 BlockNumber COMP.
01 DataDestination.
   02 array COMP OCCURS 256 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "ReadScratchFile" USING BlockNumber, DataDestination.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER BlockNumber
INTEGER DataDestination(256)
...
Monitor_Call('ReadScratchFile', BlockNumber, DataDestination(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : BlockNumber
BYTES : DataDestination(0:511)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('ReadScratchFile', BlockNumber, DataDestination(0))
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Description                                  |
|-------------|----------------------------------------------|
| LDT BLKNO   | %Block number to be read.                    |
| LDX (BUFF   | %Address of buffer to receive block read.    |
| MON 5       | %Monitor call ReadScratchFile.               |
| JMP ERROR   | %Error return from monitor call.             |
| ...         | %Normal return.                              |
| ERROR, ...  | %Error number in register A.                 |
| BLKNO, ...  |                                              |
| BUFF, 0     | `*+400/` %Make a buffer of 256 words, 1 block. |


ND-100 | All users | Background programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 403*

[↑ Back to Top](#table-of-contents)

---

### 6B - WriteScratchFile (WDISK)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Writes randomly to the scratch file. One block is transferred. There is one scratch file connected to each terminal. It is opened for random read and write access when you log in. Its file number is 100B.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➔` | Block number to start the writing from. | I |  |
| `➔` | The data to be transferred. | I |  |
| `⬅` | Standard Error Code. See appendix A. | I |  |

#### See Also

[WriteBlock](#10b-writeblock-wpage), [WriteToFile](#120b-writetofile-wfile), and ReadScratchFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
BlockNumber : INTEGER2;
Buffer : ARRAY [0..15] OF RECORD...END;
...
WriteScratchFile(BlockNumber, Buffer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 BlockNumber COMP.
01 Buffer.
   02 array COMP OCCURS 256 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "WriteScratchFile" USING BlockNumber, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER BlockNumber
INTEGER Buffer(256)
...
Monitor Call('WriteScratchFile', BlockNumber, Buffer(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : BlockNumber
BYTES : Buffer(0:511)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('WriteScratchFile', BlockNumber, Buffer(0))
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT  BLKNO   %Block number to be written into.
LDX  (BUFF   %Address of buffer containing data to be written.
MON  6       %Monitor call WriteScratchFile.
JMP  ERROR   %Error return from monitor call.
...          %Normal return.
ERROR, ...   %Error number in register A.
...
BLKNO, ...
BUFF, 0
*+400/       %Make a buffer of 256 words, 1 block.


| ND-100 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 567*

[↑ Back to Top](#table-of-contents)

---

### 7B - ReadBlock (RPAGE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads randomly from a file. You read one block at a time. The file must be opened for random read access.

- The standard block size is 512 bytes. You can change this with `SetBlockSize`. The first block is number 0.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 10B - WriteBlock (WPAGE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Writes randomly to a file. You write one block at a time. The file must be opened for random write access.

- The standard block size is 512 bytes. You can change this with SetBlockSize. The first block is number 0.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 11B - GetBasicTime (TIME)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

**Time**

Gets the current internal time. The internal time is specified in basic time units. There are 50 basic time units in a second.

- The internal time is set to 0 each time SINTRAN III is started.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Internal time in basic time units. |

#### See Also

[GetCurrentTime](#113b-getcurrenttime-clock), [AdjustClock](#112b-adjustclock-cladj), and SetClock

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
BasicTime : LONGINT;
...
GetBasicTime(BasicTime);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 BasicTime COMP PIC S9(10).
...
MONITOR-CALL "GetBasicTime" USING BasicTime.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER*4 BasicTime
...
Monitor_Call('GetBasicTime', BasicTime)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER4 : BasicTime
...
Monitor_Call('GetBasicTime', BasicTime)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
BasicTime : W BLOCK 1
GetBasicTime : EQU 37B9 + 11B
...
CALLG GetBasicTime, 0
W1 =: BasicTime  %Result is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| MON | 11   | %Monitor call GetTime.                                |
|-----|------|-------------------------------------------------------|
| STD | TIME | %Store time returned in the A and D register.         |
| ... |      |                                                       |
| TIME, 0 | | %Time in basic time units as a double word.            |
| 0      |    | %                                                   |


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 223*

[↑ Back to Top](#table-of-contents)

---

### 12B - SetCommandBuffer (SETCM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Transfers a string to the command buffer. The command buffer contains the last command input from the terminal. You may read the command buffer by reading from logical device number 0. See InByte.

- The command @TERMINAL-STATISTICS lists the command buffer.
- You may apply the SINTRAN III command editing characters to the command buffer when the program has terminated.
- The parameter is fetched through the alternative page table.
- You may use this monitor call to erase sensitive information in the command buffer, e.g., password parameters.

#### See Also

`ExecuteCommand`, `CallCommand`

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Command : PACKED ARRAY [0..31] OF CHAR;
    ...
    SetCommandBuffer(Command);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Command PIC X(32).
    ...
    MONITOR-CALL "SetCommandBuffer" USING Command.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER Command*32
    ...
    Monitor_Call('SetCommandBuffer', Command(1:32))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Command(0:31)
...
Monitor_Call('SetCommandBuffer', Command)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Command : STRINGDATA 'CLOSE-FILE 102'
SetCommandBuffer : EQU 37B9 + 12B
...
CALLG SetCommandBuffer, 1, Command
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Operand | Description                                           |
|-------------|---------|-------------------------------------------------------|
| LDA         | (CMND)  | %Address of string with command.                      |
| MON         | 12      | %Monitor call SetCommandBuffer.                       |
| ...         |         |                                                       |
| CMND,       | 'CLOSE-FILE 102' | %Transfer 'CLOSE-FILE 102' to the command buffer. |

| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 439*

[↑ Back to Top](#table-of-contents)

---

### 13B - ClearInBuffer (CIBUF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Clears a device input buffer. Input from character devices, e.g. terminals, are temporarily stored in this buffer.

- You can use logical device number 1 for your own terminal in background programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

@CLEAR-DEVICE, ClearOutBuffer, and DeviceControl

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
...
ClearInBuffer(DeviceNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ClearInBuffer" USING DeviceNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
...
Monitor_Call('ClearInBuffer', DeviceNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('ClearInBuffer', DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
ErrCode : W BLOCK 1
ClearInBuffer : EQU 37B9 + 13B
...
CALLG ClearInBuffer, 1, DeviceNumber
IF K GO ERROR
...
ERROR : W1 =: ErrCode            %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT   DEVNO       %Load register T with logical device number.
MON   13          %Monitor call ClearInBuffer.
JMP   ERROR       %Error return from monitor call.
...               %Normal return.
ERROR, ...        %Error number in register A.
...
DEVNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 113*

[↑ Back to Top](#table-of-contents)

---

### 14B - ClearOutBuffer (COBUF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Clears a device output buffer. Output to character devices, e.g. terminals, are temporarily stored in this buffer.

- You can use logical device number 1 for your own terminal in background programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number. See appendix B.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

@CLEAR-DEVICE, [ClearInBuffer](#15b-clearinbuffer-cibuf), and DeviceControl

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
...
ClearOutBuffer (DeviceNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ClearOutBuffer" USING DeviceNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
...
Monitor Call ('ClearOutBuffer', DeviceNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
...
ON ROUTINEERROR DO
    IF ErrCode <> 0 THEN ...
ENDON
Monitor_Call('ClearOutBuffer', DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
ErrCode : W BLOCK 1
ClearOutBuffer : EQU 37B9 + 14B
...
CALLG ClearOutBuffer, 1, DeviceNumber
    IF K GO ERROR
...
ERROR : W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO      %Load register T with logical device number.
MON 14         %Monitor call ClearOutBuffer.
JMP ERROR      %Error return from monitor call.
...            %Normal return.
ERROR, ...     %Error number in register A.
...
DEVNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 16B - GetTerminalType (MGTTY)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the terminal type. The terminal type tells SINTRAN III how to handle a particular terminal. A wrong terminal type normally distorts the screen. The function-keys cannot be used.

- Appendix H lists the terminal types.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The logical device number of the terminal. You may use 1 for your own terminal i |
| `Param2` | UNKNOWN | I | The terminal type. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SetTerminalType](#17b-setterminaltype-mstty), and @GET-TERMINAL-TYPE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, TerminalType : INTEGER2;
...
GetTerminalType(DeviceNumber, TerminalType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 TerminalType COMP.
01 ErrCode COMP.
...
MONITOR-CALL "GetTerminalType" USING DeviceNumber, TerminalType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, TerminalType
...
Monitor_Call('GetTerminalType', DeviceNumber, TerminalType)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, TerminalType
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetTerminalType', DeviceNumber, TerminalType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
TerminalType : W BLOCK 1
ErrCode : W BLOCK 1
GetTerminalType : EQU 37B9 + 16B
...
CALLG GetTerminalType, 2, DeviceNumber, TerminalType
IF K GO ERROR
...
ERROR : W1 =: ErrCode   %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO    %Logical device number, must be a terminal.
MON 16       %Monitor call GetTerminalType.
JMP ERROR    %Error return from monitor call.
STA TYPE     %Normal return, store terminal type number.
...
ERROR, ...   %Error number in register A.
...
DEVNO, ...
TYPE, 0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 291*

[↑ Back to Top](#table-of-contents)

---

### 17B - SetTerminalType (MSTTY)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the type of a terminal. The terminal type tells SINTRAN III how to handle a particular terminal. A wrong terminal type normally distorts the screen. The function keys cannot be used.

- Appendix H lists the terminal types.
- Public background users may only set the terminal type for their own terminal. A background program must be run from user SYSTEM or RT to set the terminal type for another terminal.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The logical device number of the terminal. You may use 1 for your own terminal i |
| `Param2` | UNKNOWN | I | The terminal type. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetTerminalType](#16b-getterminaltype-mgtty), @SET-TERMINAL-TYPE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, TerminalType : INTEGER2;
...
SetTerminalType(DeviceNumber, TerminalType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 TerminalType COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SetTerminalType" USING DeviceNumber, TerminalType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, TerminalType
...
Monitor Call('SetTerminalType', DeviceNumber, TerminalType)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, TerminalType
...
ON ROUTINEERROR DO
    IF ErrCode <> 0 THEN ...
ENDON

Monitor_Call('SetTerminalType', DeviceNumber, TerminalType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
TerminalType : W BLOCK 1
ErrCode : W BLOCK 1
SetTerminalType : EQU 37B9 + 17B
...
CALLG SetTerminalType, 2, DeviceNumber, TerminalType
IF K GO ERROR
...

ERROR : W1 =: ErrCode             %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO        %Logical device number.
LDA TYPE         %Terminal type number.
MON 17           %Monitor call SetTerminalType.
JMP ERROR        %Error return from monitor call.
...              %Normal return.
ERROR, ...       %Error number in register A.
...

DEVNO, ...
TYPE, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 477*

[↑ Back to Top](#table-of-contents)

---

### 21B - InUpTo8Bytes (M8INB)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

See also In8Bytes, InByte, InString, In4x2Bytes, and Out8Bytes.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number. See appendix B.` |  | I |  |
| `Number of bytes read. If you use the monitor call on a word-oriented device, it returns the number of words read.` |  | I |  |
| `The 8 bytes.` |  | I |  |
| `Standard Error Code. See appendix A.` |  | I |  |

#### See Also

[In8Bytes](#23b-in8bytes-b8inb), [InByte](#1b-inbyte-inbt), InString, [In4x2Bytes](#63b-in4x2bytes-b41nw), and Out8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes : INTEGER2;
InData : RECORD...END;
...
InUpTo8Bytes(DeviceNumber, NoOfBytes, InData);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber  COMP.
01 NoOfBytes     COMP.
01 InData        PIC X(8).
01 ErrCode       COMP.
...
MONITOR-CALL "InUpTo8Bytes" USING DeviceNumber, NoOfBytes, InData.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER  DeviceNumber, NoOfBytes
CHARACTER InData*8
...
Monitor_Call('InUpTo8Bytes', DeviceNumber, NoOfBytes, InData(1:8))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
BYTES : InData(0:7)
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('InUpTo8Bytes', DeviceNumber, NoOfBytes, InData)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
InData : STRING 8
ErrCode : W BLOCK 1
InUpTo8Bytes : EQU 37B9 + 21B
...
&nbsp;&nbsp;&nbsp;&nbsp;CALLG InUpTo8Bytes, 3, DeviceNumber, NoOfBytes, InData
&nbsp;&nbsp;&nbsp;&nbsp;IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp;&nbsp;&nbsp;&nbsp;%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO &nbsp;&nbsp;&nbsp;&nbsp;%Logical device number.
MON 21 &nbsp;&nbsp;&nbsp;&nbsp;%Monitor call InUpTo8Bytes.
JMP ERROR &nbsp;&nbsp;&nbsp;&nbsp;%Error return from monitor call.
STD BYTES &nbsp;&nbsp;&nbsp;&nbsp;%Normal return, store first 4 bytes read.
COPY SL DA
STA BYTES+2 &nbsp;&nbsp;&nbsp;&nbsp;%Store next 2 bytes read.
STX BYTES+3 &nbsp;&nbsp;&nbsp;&nbsp;%Store last 2 bytes read.
STT COUNT &nbsp;&nbsp;&nbsp;&nbsp;%Store number of bytes (or words) read.
...
ERROR, ... &nbsp;&nbsp;&nbsp;&nbsp;%Error number in register A.
...
DEVNO, ...
COUNT, 0
BYTES, 0
0
0
0


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 325*

[↑ Back to Top](#table-of-contents)

---

### 22B - OutUpTo8Bytes (M8OUT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Writes up to 8 characters to a device, e.g. a terminal or an internal device.

- You can only use this monitor call for terminals, TADs, internal devices, and synchronous modems.
- The writing terminates when a character with value 0 is found. The 0 byte is not output.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. You can use 1 for your own terminal. File |
| `Param2` | UNKNOWN | I | The 8 characters to be written. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[OutMessage](#32b-outmessage-msg), [Out8Bytes](#24b-out8bytes-b8out), [OutputString](#504b-outputstring-dvouts), [OutString](#162b-outstring-outst), [OutNumber](#35b-outnumber-iout), [OutByte](#2b-outbyte-outbt), and InUpTo8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo : INTEGER2;
OutData : RECORD...END;
...
OutUpTo8Bytes(DeviceNo, OutData);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 OutData PIC X(8).
01 ErrCode COMP.
...
MONITOR-CALL "OutUpTo8Bytes" USING DeviceNo, OutData.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo
CHARACTER OutData*8
...
Monitor_Call('OutUpTo8Bytes', DeviceNo, OutData(1:8))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo
    BYTES : OutData(0:7)
    ...
    ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
    ENDON
    Monitor_Call('OutUpTo8Bytes', DeviceNo, OutData)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
    OutData : STRINGDATA 8
    ErrCode : W BLOCK 1
    OutUpTo8Bytes : EQU 37B9 + 22B
    ...
    CALLG OutUpTo8Bytes, 2, DeviceNo, OutData
    IF K GO ERROR
    ...
    ERROR : W1 =: ErrCode               %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT  DEVNO           %Logical device address.
    LDD  BYTES           %First 4 bytes to be written.
    LDX  BYTES+2         %Next 2 bytes to be written.
    COPY SX DL
    LDX  BYTES+3         %Last 2 bytes to be written.
    MON  22              %Monitor call OutUpTo8Bytes.
    JMP  ERROR           %Error return from monitor call.
    ...
                          %Normal return.
    ERROR, ...           %Error number in register A.
    ...
    DEVNO, ...
    BYTES, 'HELLO!!!'


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 387*

[↑ Back to Top](#table-of-contents)

---

### 23B - In8Bytes (B8INB)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads 8 bytes from a device. The input is fast, but the monitor call does not apply the defined echo and break setting.

- Do not use this monitor call for terminals and TAD's when echo and break should be applied.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number. See appendix B.` |  | I |  |
| `Number of bytes read. If the monitor call is used on a word-oriented device, the number of words is returned.` |  | I |  |
| `The string of bytes read.` |  | I |  |
| `Standard Error Code. See appendix A.` |  | I |  |

#### See Also

[In8AndFlag](#310b-in8andflag-tbin8), [InUpTo8Bytes](#21b-inupto8bytes-m8inb), [InByte](#1b-inbyte-inbt), InString, [InputString](#503b-inputstring-dvinst), [In4x2Bytes](#63b-in4x2bytes-b41nw), and Out8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes: INTEGER2;
DataRead: PACKED ARRAY [0..7] OF CHAR;
...
In8Bytes(DeviceNumber, NoOfBytes, DataRead);
IF ErrCode <> O THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 NoOfBytes COMP.
01 DataRead PIC X(8).
01 ErrCode COMP.
...
MONITOR-CALL "In8Bytes" USING DeviceNumber, NoOfBytes, DataRead.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoOfBytes
CHARACTER DataRead*8
...
Monitor_Call('In8Bytes', DeviceNumber, NoOfBytes, DataRead(1:8))
IF (ErrCode .NE. O) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
BYTES : DataRead(0:7)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('In8Bytes', DeviceNumber, NoOfBytes, DataRead)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
DataRead : STRING 8
ErrCode : W BLOCK 1
In8Bytes : EQU 37B9 + 23B
...
CALLG In8Bytes, 3, DeviceNumber, NoOfBytes, DataRead
IF K GO ERROR
...
ERROR : W1 =: ErrCode


%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO         %Logical device number.
MON 23            %Monitor call In8Bytes.
JMP ERROR         %Error return from monitor call.
STD BYTES         %Normal return, store first 4 bytes read.
COPY SL DA
STA BYTES+2       %Store next 2 bytes read.
STX BYTES+3       %Store last 2 bytes read.
STT COUNT         %Store number of bytes (or words) read.
...
ERROR, ...        %Error number in register A.
...
DEVNO, ...
COUNT, 0
BYTES, 0
0
0
0


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 313*

[↑ Back to Top](#table-of-contents)

---

### 24B - Out8Bytes (B8OUT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes 8 bytes to a character device, e.g. a terminal. All 8 bytes are output. OutUpTo8Bytes stops if a byte is 0.

- On the ND-500, you are advised to use the faster OutputString.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number` | See appendix B. | I |  |
| `The string of bytes to be written` |  | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[OutUpTo8Bytes](#22b-outupto8bytes-m8out), [OutByte](#2b-outbyte-outbt), [OutString](#162b-outstring-outst), [OutputString](#504b-outputstring-dvouts), [OutMessage](#32b-outmessage-msg), [OutNumber](#35b-outnumber-iout), and In8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
OutData : RECORD...END;
...
Out8Bytes(DeviceNumber, OutData);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 OutData PIC X(8).
01 ErrCode COMP.
...
MONITOR-CALL "Out8Bytes" USING DeviceNumber, OutData.
CALL "CBerror" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
CHARACTER OutData*8
...
Monitor_Call('Out8Bytes', DeviceNumber, OutData(1:8))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
BYTES : OutData(0:7)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('Out8Bytes', DeviceNumber, OutData)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
OutData : STRINGDATA 'HELLO!!!'
ErrCode : W BLOCK 1
Out8Bytes : EQU 37B9 + 24B
...
  CALLG Out8Bytes, 2, DeviceNumber, OutData
  IF K GO ERROR
...
ERROR : W1 =: ErrCode          %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO          %Logical device address.
LDD BYTES          %First 4 bytes to be written.
LDX BYTES+2        %Next 2 bytes to be written.
COPY SX DL
LDX BYTES+3        %Last 2 bytes to be written.
MON 24             %Monitor call Out8Bytes.
JMP ERROR          %Error return from monitor call.
...
ERROR, ...         %Error number in register A.
...
DEVNO, ...
BYTES, 'HELLO!!!'



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 373*

[↑ Back to Top](#table-of-contents)

---

### 26B - GetLastByte (LASTC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the last character typed on a terminal. The monitor call can be used to terminate long output sequences, reading from files, etc. The program never enters the I/O wait state because of this monitor call.

- Only user SYSTEM can execute this monitor call on a non-reserved terminal.
- This monitor call always returns -1 if an error is encountered, not the standard SINTRAN error codes.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number of a terminal. |
| `Param2` | UNKNOWN | I | The last character typed on the terminal. |

#### See Also

[InByte](#1b-inbyte-inbt)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, LastCharTyped : INTEGER2;
...
GetLastByte(DeviceNumber, LastCharTyped);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 LastCharTyped COMP.
...
MONITOR-CALL "GetLastByte" USING DeviceNumber, LastCharTyped.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, LastCharTyped
...
Monitor_Call('GetLastByte', DeviceNumber, LastCharTyped)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, LastCharTyped
...
Monitor_Call('GetLastByte', DeviceNumber, LastCharTyped)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
LastCharTyped : W BLOCK 1
GetLastByte : EQU 37B9 + 26B
...
    CALLG GetLastByte, 1, DeviceNumber
    W1 := LastCharTyped %Result is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 26 %Monitor call GetLastByte.
JAN ERROR %Error in parameters return -1.
STA CHAR %No errors, store byte returned.
...
ERROR, ... %Handle the error.
...
CHAR, O %Last typed character right adjusted.
PAR, DEVNO %Logical device number of a terminal.
...
DEVNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 255*

[↑ Back to Top](#table-of-contents)

---

### 27B - GetRTDescr (RTDSC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads an RT description. The RT description contains various information about an RT program. You specify the RT program address. See the SINTRAN III Real Time Guide (ND-860133) for further details.

- Use GetRTAddress if you only know the name of the RT program.
- This monitor call is only available to background programs in SINTRAN III VSX, version K.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | RT description address. Use 0 for the calling RT program. |
| `Param2` | UNKNOWN | I | A 52 byte RT description. |
| `Param3` | UNKNOWN | I | Number of devices connected to the RT program through StartOnInterrupt. Wrong RT |

#### See Also

[GetOwnRTAddress](#30b-getownrtaddress-getrt), @LIST-RT-DESCRIPTION

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTPogram, NoOfConnDev : INTEGER2;
RTDescriptor : ARRAY [0..1] OF RECORD...END;
...
GetRTDescr(RTProgram, RTDescriptor, NoOfConnDev);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01  RTProgram COMP.
01  NoOfConnDev COMP.
01  RTDescriptor.
   02  array COMP OCCURS 26 TIMES.
...
MONITOR-CALL "GetRTDescr" USING RTProgram, RTDescriptor, NoOfConnDev.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER  RTProgram, NoOfConnDev
INTEGER  RTDescriptor(26)
...
Monitor_Call('GetRTDescr', RTProgram, RTDescriptor(1), NoOfConnDev)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, NoOfConnDev
BYTES : RTDescriptor(0:51)
...
Monitor_Call('GetRTDescr', RTProgram, RTDescriptor(0), NoOfConnDev)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
NoOfConnDev : W BLOCK 1
RTDescriptor : H BLOCK 32B
GetRTDescr : EQU 37B9 + 27B
...
CALLG GetRTDescr, 2, RTProgram, RTDescriptor
IF K GO Error
W1 =: NoOfConnDev %Result is returned in W1 register.
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 27 %Monitor call GetRTDescr.
JAN ERROR %Handle error if register A is negative.
STA NODEV %Store number of connected devices.
...
ERROR, ... %Handle this error, register A = -1.
...
NODEV, 0
PAR, RTPRO %Address of RT description.
BUFF %Buffer receiving RT description.

...
RTPRO, ...
BUFF, 0
*+32B/ %Make a 52 byte buffer.


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
|-------------------|-------------------------|-------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 271*

[↑ Back to Top](#table-of-contents)

---

### 30B - GetOwnRTAddress (GETRT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the address of the calling program's RT description. Background programs get the RT description address of the RT program which controls the terminal.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The RT description address. |

#### See Also

GetRTAddress, [GetRTDescr](#27b-getrtdescr-rtdsc), [GetRTName](#152b-getrtname-grtna), and @LIST-RT-PROGRAMS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTDescrAddress : INTEGER2;
...
GetOwnRTAddress(RTDescrAddress);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTDescrAddress COMP.
...
MONITOR-CALL "GetOwnRTAddress" USING RTDescrAddress.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTDescrAddress
...
Monitor_Call('GetOwnRTAddress', RTDescrAddress)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTDescrAddress
...
Monitor_Call('GetOwnRTAddress', RTDescrAddress)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTDescrAddress : W BLOCK 1
GetOwnRTAddress : EQU 37B9 + 30B
...
CALLG GetOwnRTAddress, 0
W1 =: RTDescrAddress %Result is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON 30 %Monitor call GetOwnRTAddress.
STA RTPRO %Store address of RT description.
...
RTPRO, 0

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 265*

[↑ Back to Top](#table-of-contents)

---

### 31B - IOInstruction (EXIOX)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Executes an IOX machine instruction. The IOX instruction handles the device registers. The IOX instruction must be inserted in the IOX table by the SINTRAN-SERVICE-PROGRAM command INSERT-IN-IOX-TABLE first.

- SINTRAN III must know the device register addresses.
- This monitor call may be used in debugging of device interfaces.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Register contents before execution. |
| `Param2` | UNKNOWN | I | Device register address. |
| `Param3` | UNKNOWN | I | Register contents after execution. |

#### See Also

[PrivInstruction](#146b-privinstruction-ipriv), [CAMACIOInstruction](#153b-camacioinstruction-ioxin), @EXECUTE-IOX

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RegContents, DevRegAddr, ContentsAfter : INTEGER2;
...
IOInstruction(RegContents, DevRegAddr, ContentsAfter);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RegContent COMP.
01 DevRegAddr COMP.
01 ContentAfter COMP.
...
MONITOR-CALL "IOInstruction" USING RegContent, DevRegAddr, ContentAfter.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RegContents, DevRegAddr, ContentAfter
...
Monitor_Call('IOInstruction', RegContents, DevRegAddr, ContentAfter)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RegContents, DevRegAddr, ContentsAfter
...
Monitor_Call('IOInstruction', RegContents, DevRegAddr, ContentsAfter)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RegContents : W BLOCK 1
DevRegAddr : W BLOCK 1
ContentsAfter : W BLOCK 1
IOInstruction : EQU 3789 + 31B
...
CALLG IOInstruction, 2, RegContents, DevRegAddr
IF K GO error
W1 =: ContentsAfter %Result is returned in W1 register.
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 31 %Monitor call IOInstruction.
STA STAT %Store status returned.
...
STAT, 0

| PAR, REG | %Register contents. |
|----------|---------------------|
| DEV      | %Device register address.  |

REG, ...
DEV, ...


| ND-100 and ND-500 | User RT and user SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 327*

[↑ Back to Top](#table-of-contents)

---

### 32B - OutMessage (MSG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes a message to the user's terminal. This is convenient for error messages in background programs.

#### See Also

[OutUpTo8Bytes](#22b-outupto8bytes-m8out), [Out8Bytes](#24b-out8bytes-b8out), [OutString](#162b-outstring-outst), [OutputString](#504b-outputstring-dvouts), [OutNumber](#35b-outnumber-iout), and OutByte

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Message : PACKED ARRAY [0..79] OF CHAR;
...
OutMessage(Message);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Message PIC X(100).
...
MONITOR-CALL "OutMessage" USING Message.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER Message*80
...
Monitor_Call('OutMessage', Message(1:80))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Message(0:79)
...
Monitor_Call('OutMessage', Message)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Message                | STRINGDATA 'This is a test...' | %Message to be sent.        |
|------------------------|--------------------------------|-----------------------------|
| OutMessage             | EQU 37B9 + 32B                 |                             |
| CALLG OutMessage, 1, Message |                          |                             |
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX  (TEXT     %Address string to be sent to user's terminal.
MON  32        %Monitor call OutMessage.
...
TEXT, 'THIS IS A TEXT'  %String to be written.


| ND-100 and ND-500 | All users | Background programs |
|-------------------|-----------|---------------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 379*

[↑ Back to Top](#table-of-contents)

---

### 33B - AltPageTable (ALTON)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Switches page table. Each page table allows you to access 128 Kbyte memory. SINTRAN III has 4 page tables. SINTRAN III VSX, version K, has 16 page tables. They are numbered 0-15. RT programs may use any page table. Background programs will get page table 2. The parameter is ignored.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Number of the page table to use. |

#### See Also

[NormalPageTable](#34b-normalpagetable-altoff)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
PageTableNumber : INTEGER2;
...
AltPageTable(PageTableNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 PageTableNumber COMP.
...
MONITOR-CALL "AltPageTable" USING PageTableNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER PageTableNumber
...
Monitor_Call('AltPageTable', PageTableNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : PageTableNumber
...
Monitor_Call('AltPageTable', PageTableNumber)
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Parameters  | Description                            |
|-------------|-------------|----------------------------------------|
| LDA         | (PAR        | %Load register A with address of parameter list. |
| MON         | 33          | %Monitor call AltPageTable.            |
| ...         |             |                                        |
| PAR,        | PAGE        |                                        |
| ...         |             |                                        |
| PAGE,       | ...         | %Number of the alternative page table. |

| ND-100 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 77*

[↑ Back to Top](#table-of-contents)

---

### 34B - NormalPageTable (ALTOFF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

**ALTOFF** Sets the alternative page table equal to the normal page table. All memory addresses are mapped through the normal page table after this monitor call.

- Use AltPageTable to set an alternative page table.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 35B - OutNumber (IOUT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes a number to the user's terminal. The number can be output as an octal or a decimal value.

- The number may be in the range -32768 to 32767.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Octal or decimal output. Use 8 for octal and 10 for decimal. |
| `Param2` | UNKNOWN | I | The number to be written. |

#### See Also

[OutMessage](#32b-outmessage-msg), [OutUpTo8Bytes](#22b-outupto8bytes-m8out), [Out8Bytes](#24b-out8bytes-b8out), [OutString](#162b-outstring-outst), [OutputString](#504b-outputstring-dvouts), and OutByte

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Format, Number : INTEGER2;
    ...
    OutNumber(Format, Number);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Format COMP.
    01 Number COMP.
    ...
    MONITOR-CALL "OutNumber" USING Format, Number.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Format, Number
    ...
    Monitor_Call('OutNumber', Format, Number)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Format, Number
...
Monitor_Call('OutNumber', Format, Number)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Format : W BLOCK 1 %12B=decimal, 10B=octal, 20B=hexadecimal, 2=bitpattern.
Number : W BLOCK 1
OutNumber : EQU 37B9 + 35B


CALLG OutNumber, 2, Format, Number
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Operand | Comment                          |
|-------------|---------|----------------------------------|
| LDT         | FORM    | %Format of number to be printed. |
| LDA         | NUM     | %Number to be printed.           |
| MON         | 35      | %Monitor call OutNumber.         |
| ...         |         |                                  |
| FORM,       | 12      | %Interpret NUM as a decimal digit.|
| NUM,        | ...     |                                  |


| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 381*

[↑ Back to Top](#table-of-contents)

---

### 36B - NoWaitSwitch (NOWT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Switches No Wait on and off. No Wait is useful for input from, and output to several devices simultaneously. In No Wait, the program does not wait for input or output to complete. Monitor calls like InByte return error code 3 instead.

- **SuspendProgram** or **WaitForRestart** may passivate the program afterwards. The program restarts when input or output to the device is completed.
- For performance reasons, use **TerminalNoWait** (TNOWAI, mon 307) rather than NoWaitSwitch.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number of a character device. See appendix B.` | UNKNOWN | I |  |
| `Input or output flag. Use 0 for input and 1 for output.` | UNKNOWN | I |  |
| `No Wait flag. Use 0 to switch No Wait off, and any other number to switch it on.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

[InByte](#1b-inbyte-inbt), [OutByte](#2b-outbyte-outbt), and TerminalNoWait

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, IOFlag, WaitFlag : INTEGER2;
...
NoWaitSwitch(DeviceNumber, IOFlag, WaitFlag);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 IOFlag COMP.
01 WaitFlag COMP.
01 ErrCode COMP.
...
MONITOR-CALL "NoWaitSwitch" USING DeviceNumber, IOFlag, WaitFlag.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, IOFlag, WaitFlag
...
Monitor_Call('NoWaitSwitch', DeviceNumber, IOFlag, WaitFlag)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, IOFlag, Waitflag
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('NoWaitSwitch', DeviceNumber, IOFlag, Waitflag)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
IOFlag : W BLOCK 1
WaitFlag : W BLOCK 1
ErrCode : W BLOCK 1
NoWaitSwitch : EQU 37B9 + 36B
...
CALLG NoWaitSwitch, 3, DeviceNumber, IOFlag, WaitFlag
IF K GO ERROR
...
ERROR : W1 =: ErrCode                      %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA   (PAR                %Load register A with address of parameter list.
MON   36                  %Monitor call NoWaitSwitch.
JAF   ERROR               %Handle error if register A is non-zero.
...
ERROR, ...                %Handle this error.
...
PAR,  DEVNO               %Logical device number.
IOF                       %Input/output flag.
WAITFL                    %No Wait flag.
...
DEVNO, ...
IOF, ...
WAITFL, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 359*

[↑ Back to Top](#table-of-contents)

---

### 37B - ReadADChannel (AIRDW)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Reads an analog to digital channel.

### PARAMETERS

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See `OpenFile`. |
| `Param2` | UNKNOWN | I | Block number. |
| `Param3` | UNKNOWN | I | Array for data returned. |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, BlockNo : INTEGER2;
DataDestination : ARRAY [0..15] OF RECORD...END;
...
ReadBlock(FileNumber, BlockNo, DataDestination);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 BlockNo COMP.
01 DataDestination.
   02 array COMP OCCURS 256 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "ReadBlock" USING FileNumber, BlockNo, DataDestination.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, BlockNo
INTEGER DataDestination(256)
...
Monitor Call('ReadBlock', FileNumber, BlockNo, DataDestination(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NoOfChannels : W BLOCK 1
Channel : W BLOCK 512
Buffer : W BLOCK 512
ReturnValue : W BLOCK 1
ReadADChannel : EQU 37B9 + 37B

...
CALLG ReadADChannel, 4, NoOfChannels, Channel, Buffer, ReturnValue
IF K GO Error
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON 37 %Monitor call ReadADChannel.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 393*

[↑ Back to Top](#table-of-contents)

---

### 40B - CloseSpoolingFile (SPCLO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Appends an opened file to a spooling queue. You specify a text to be printed on the error device when the file is to be printed.

- If the file is not a spooling file, a normal close is performed.
- Does not work for remote files. For these, a normal close is performed.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File Number` | File number given when the file was opened. | I |  |
| `Text` | Text to be output on the error device. | I |  |
| `NoOfCopies` | Number of print copies. | I |  |
| `PrintFlag` | Print flag. If 0, the text is only output if required by @DEFINE-SPOOLING-CONDITIONS. If not 0, the file is printed unconditionally. A stop print condition occurs before printing. | I |  |
| `ErrCode` | Standard Error Code. See appendix A. | I |  |

#### See Also

[CloseFile](#43b-closefile-close), [AppendSpooling](#240b-appendspooling-apspe), @DEFINE-SPOOLING-CONDITIONS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, NoOfCopies, PrintFlag : INTEGER2;
UserText : PACKED ARRAY [0..79] OF CHAR;
...
CloseSpoolingFile(FileNo, UserText, NoOfCopies, PrintFlag);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 NoOfCopies COMP.
01 PrintFlag COMP.
01 UserText PIC X(100).
01 ErrCode COMP.
...
MONITOR-CALL "CloseSpoolingFile" USING FileNo, UserText,
                                  NoOfCopies, PrintFlag.

CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, NoOfCopies, PrintFlag
CHARACTER UserText*80
...
Monitor_Call('CloseSpoolingFile', FileNo, UserText(1:80),
             NoOfCopies, PrintFlag)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo, NoOfCopies, PrintFlag
BYTES : UserText(0:79)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('CloseSpoolingFile', FileNo, UserText, NoOfCopies, PrintFlag)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
NoOfCopies : W BLOCK 1
PrintFlag : W BLOCK 1
UserText : STRINGDATA 'Printing file...'
ErrCode : W BLOCK 1
CloseSpoolingFile : EQU 37B9 + 40B
...
  CALLG CloseSpoolingFile. 4, FileNo, UserText, NoOfCopies, PrintFlag
  IF K GO ERROR
  ...
ERROR : W1 =: ErrCode                  %ErrorCode in W1 register.




LDT FILNO                 %File number returned from earlier open.
LDA NOCOP                 %Number of copies.
COPY SA D0
LDA FLAG                  %Condition flag.
LDX (TEXT                 %Address of text to be sent to error device.
MON 40                    %Monitor call CloseSpoolingFile.
JMP ERROR                 %Error return from monitor call.
...                       %Normal return.
ERROR, ...                %Error number in register A.
...
FILNO, ...
TEXT, 'GUMMED LABELS'     %Message when the file is to be printed.
NOCOP, ...
FLAG, 0                   %Text is only written if required by
                          % @DEFINE-SPOOLING-CONDITIONS.



| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 119*

[↑ Back to Top](#table-of-contents)

---

### 41B - ReadObjectEntry (ROBJE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about an opened file. An object entry describes each file. It contains the file name, the access rights, the date last opened for read and write, the size, and more. See the file system description in the SINTRAN III System Supervisor (ND-830003). You specify the file number.

- There is one object entry for each version of a file.
- The device number location in the object entry contains the logical device number and the unit number where the mass-storage file resides. The logical device number is placed in bit 11-0. The unit number in bit 15-12. The location contains the logical device number for peripheral files.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | ➞ The file number. See Openfile. |
| `Param2` | UNKNOWN | I | ➞ The 64 byte object entry. See appendix C. |
| `Param3` | UNKNOWN | I | ➞ Standard Error Code. See appendix A. |

#### See Also

[GetObjectEntry](#215b-getobjectentry-drobj), [SetObjectEntry](#216b-setobjectentry-dwobj)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
Buff : ARRAY [0..1] OF RECORD...END;
...
ReadObjectEntry(FileNumber, Buff);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 Buff.
02 array COMP OCCURS 32 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "ReadObjectEntry" USING FileNumber, Buff.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER Buff(32)
...
Monitor_Call('ReadObjectEntry', FileNumber, Buff(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
BYTES : Buff(0:63)
...
ON ROUTINEERROR DO
IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('ReadObjectEntry', FileNumber, Buff(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
Buff : H BLOCK 40B
ErrCode : W BLOCK 1
ReadObjectEntry : EQU 37B9 + 41B
...
CALLG ReadObjectEntry, 2, FileNumber, Buff
IF K GO ERROR
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO %File number returned from earlier open.
LDA {BUFF %Address of buffer to receive object entry.
MON 41 %Monitor call ReadObjectEntry.
JMP ERROR %Error return from monitor call.
... %Normal return.
ERROR, ... %Error number in register A.
...
FILNO, ...
BUFF, 0
*+40/ %Make a 32 words large buffer.

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 401*

[↑ Back to Top](#table-of-contents)

---

### 43B - CloseFile (CLOSE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Closes one or more files. Files must be opened before they are accessed. Afterwards they should be closed.

- **CloseFile** also resets peripheral files. This is similar to DeviceControl with control flag -1.
- For non-RT programs, files are closed when your program terminates.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number returned when the file was opened. Use -1 to close all your files wh |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[OpenFile](#50b-openfile-open), [CloseSpoolingFile](#40b-closespoolingfile-spclo), BackupClose, and @CLOSE-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
...
CloseFile(FileNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "CloseFile" USING FileNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
...
Monitor_Call('CloseFile', FileNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
...
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('Closefile', FileNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
ErrCode : W BLOCK 1
CloseFile : EQU 37B9 + 43B
...
CALLG CloseFile, 1, FileNumber
IF K GO ERROR
...
ERROR : W1 =: ErrCode          %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT  FILNO                 %Load register T with file number.
MON  43                   %Monitor call Closefile.
JMP  ERROR                %Error return from monitor call.
...                        %Normal return.
ERROR, ...                 %Error number in register A.
...
FILNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 117*

[↑ Back to Top](#table-of-contents)

---

### 44B - GetUserEntry (RUSER)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about a user. The user entry in the directory is returned. It contains the user name, default file accesses, the pages in use, the password, the table of friends, and more.

- Only user RT and user SYSTEM may read the user entries of other users.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `User name` | It may include a directory name, e.g. PACK-ONE:P-HANSEN. | I |  |
| `User entry` | The 64-byte user entry. See appendix C. | I |  |
| `Error Code` | Standard Error Code. See appendix A. | I |  |

#### See Also

[GetUserName](#214b-getusername-gusna), @DUMP-USER-ENTRY

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
UserName : PACKED ARRAY [0..63] OF CHAR;
Buff : ARRAY [0..1] OF RECORD...END;
...
ReadUserEntry(UserName, Buff);  [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 UserName PIC X(64).
01 Buff.
02 array COMP OCCURS 32 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetUserEntry" USING UserName, Buff.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER UserName*64
INTEGER Buff(32)
...
Monitor Call('GetUserEntry', UserName(1:64), Buff(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : UserName(0:63)
BYTES : Buff(0:63)


**ON ROUTINEERROR DO**


IF ErrCode > 0 THEN ...



ENDON
Monitor_Call('GetUserEntry', UserName, Buff(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
UserName : STRINGDATA 'A-HANSEN' %Get entry of user A-HANSEN.
Buff : H BLOCK 40B
ErrCode : W BLOCK 1
GetUserEntry : EQU 37B9 + 44B
...
CALLG GetUserEntry, 2, UserName, Buff
IF K GO ERROR



ERROR : W1 := ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (BUFF  %Address for receiving user entry.
LDX (USER  %Address of string containing user name.
MON 44  %Monitor call GetUserEntry.
JMP ERROR  %Error return from monitor call.
... %Normal return.
ERROR, ... %Error number in register A.
USER, 'A-HANSEN' %Obtain user entry for A-HANSEN.
BUFF, 0
*+40/ %Make a buffer of 32 words.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 297*

[↑ Back to Top](#table-of-contents)

---

### 50B - OpenFile (OPEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Opens a file. You cannot access a file before you open it. Specify what kind of access you want, e.g. sequential write or random read.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `0` | Sequential write. | I |  |
| `1` | Sequential read. | I |  |
| `2` | Random read or write. | I |  |
| `3` | Random read only. | I |  |
| `4` | Sequential read or write. | I |  |
| `5` | Sequential write append. | I |  |
| `6` | Random read or write common on contiguous files. | I |  |
| `7` | Random read common on contiguous files. | I |  |
| `8` | Random read or write on contiguous files. Direct transfer for ReadFromFile, WriteToFile and DeviceFunction in RT programs. | I |  |
| `9` | Random read, write append for WriteToFile. | I |  |

#### See Also

ScratchOpen, [SetPermanentOpen](#236b-setpermanentopen-sperd), [DirectOpen](#220b-directopen-dopen), and @OPEN-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, AccessCode : INTEGER;
FileName : PACKED ARRAY [0..63] OF CHAR;
FileType : PACKED ARRAY [0..3] OF CHAR;
...
OpenFile(FileNo, AccessCode, FileName, FileType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 AccessCode COMP.
01 FileName PIC X(64).
01 FileType PIC X(4).
01 ErrCode COMP.
MOVE 0 TO FileNo.
MONITOR-CALL "OpenFile" USING FileNo, AccessCode, FileName, FileType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, AccessCode
CHARACTER FileName*64, FileType*4
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
plaintext
INTEGER : FileNo, AccessCode
BYTES : FileName(0:63), FileType(0:3)
...
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ...
ENDON

0 =: FileNo
Monitor_Call('OpenFile', FileNo, AccessCode, FileName, FileType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Variable   | Type      | Description                                                |
|------------|-----------|------------------------------------------------------------|
| FileNo     | W BLOCK 1 | %Returned ND-500 open file number if 0 on input.           |
| S3No       | W BLOCK 1 | %SINTRAN III open file number as optional parameter.       |
| AccessCode | W BLOCK 1 |                                                           |
| FileName   | STRINGDATA| 'EXAMPLE'                                                  |
| FileType   | STRINGDATA| 'SYMB'                                                     |
| ErrCode    | W BLOCK 1 |                                                           |
| OpenFile   | EQU 37B9 + 50B |                                                      |

plaintext
...
CALLG OpenFile, 4, FileNo, AccessCode, FileName, FileType
IF K GO ERROR
...
ERROR = W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
plaintext
LDX (FILE %If 0, the name is read from the terminal.
LDA (TYPE %Address of default file type string.
LDT ACCES %Access code.
MON 50 %Monitor call OpenFile.
JMP ERROR %Error return from monitor call.
STA FILNO %Normal return, store the file number returned.
...
ERROR, ... %Error number in register A.
FILNO, 0
ACCES, ...
FILE, 'EXAMPLE' %Open EXAMPLE:SYMB
TYPE, 'SYMB' %



ND-100 and ND-500 | All users | All programs
---|---|---
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 369*

[↑ Back to Top](#table-of-contents)

---

### 52B - TerminalMode (TERMO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Selects various terminal functions. You may stop output on full page. Input may be converted to uppercase letters. A delay after carriage return can be set. You may also set automatic logout if the line between the terminal and the computer is broken.

- Terminal mode can also be set on TADs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The logical device number of the terminal. See appendix B. Use 1 for your own te |
| `Param2` | UNKNOWN | I | The terminal mode. The numbers below are used. |
| `Terminal mode` | Capital letters? | Delay after return? | Stop on full page? |
| `0` | No | No | No |
| `1` | Yes | No | No |
| `2` | No | Yes | No |
| `3` | Yes | Yes | No |
| `4` | No | No | Yes |
| `5` | Yes | No | Yes |
| `6` | No | Yes | Yes |
| `7` | Yes | Yes | Yes |
| `8` | No | No | No |
| `9` | Yes | No | No |
| `10` | No | Yes | No |
| `11` | Yes | Yes | No |
| `12` | No | No | Yes |
| `13` | Yes | No | Yes |
| `14` | No | Yes | Yes |
| `15` | Yes | Yes | Yes |

#### See Also

[GetTerminalMode](#306b-getterminalmode-gtmod), @TERMINAL-MODE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, Mode : INTEGER2;
...
TermMode(DeviceNumber, Mode);   \[Note routine name.\]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP. 01 Mode COMP.
01 ErrCode COMP.
MONITOR-CALL "TerminaIMode" USING DeviceNumber, Mode.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, Mode
...
Monitor_Call('TerminalMode', DeviceNumber, Mode)
If (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, Mode

...

ON ROUTINEERROR DO

&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...

ENDON

Monitor_Call('TerminalMode', DeviceNumber, Mode)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
Mode : W BLOCK 1
ErrCode : W BLOCK 1
TerminalMode : EQU 37B9 + 52B
...
CALLG TerminalMode, 2, DeviceNumber, Mode
IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%Load register A with address of parameter list.
MON 52 &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%Monitor call TerminalMode.
JAF ERROR &nbsp;%Handle error if register A is non-zero.
...
ERROR, ... &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%Error number in register A.
...
PAR, DEVNO &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%Logical device number.
MODE &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%Communication mode.
...
DEVNO, ...
MODE, ...


| ND-100 and ND-500 | All users        | Background programs |
|-------------------|------------------|---------------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 533*

[↑ Back to Top](#table-of-contents)

---

### 53B - GetSegmentEntry (RSEGM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about a segment in the ND-100. The monitor call returns the segment entry. You specify the segment number. See the SINTRAN III Real Time Guide (ND-860133) for further information.

- Use GetSegmentNo to get a segment number from a segment name.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Segment number` | Use 0 for RT common. Only the returned segment link, the first physical page, and the flag are relevant to RT common. | I |  |

#### See Also

@LIST-SEGMENT, the SINTRAN-SERVICE-PROGRAM command DUMP-SEGMENT-TABLE-ENTRY

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
Buffer : RECORD...END;
...
ReadSegmentEntry(SegmentNumber, Buffer); [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
01 Buffer.
   02 array COMP OCCURS 5 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetSegmentEntry" USING SegmentNumber, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
INTEGER Buffer(5)
...
Monitor_Call1('GetSegmentEntry', SegmentNumber, Buffer(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber
BYTES : Buffer(0:9)
ON ROUTINEERROR DO
IF ErrCode > 0 THEN ...
ENDON
...
Monitor_Call('GetSegmentEntry', SegmentNumber, Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNumber : W BLOCK 1
Buffer : W BLOCK 5
GetSegmentEntry : EQU 37B9 + 53B
...
CALLG GetSegmentEntry, 2, SegmentNumber, Buffer
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 53 %Monitor call GetSegmentEntry.
JMP ERROR %Handle error.
... %Normal return.
ERROR, ...
...

PAR, SEGNO %Segment number.
BUFF %Buffer for receiving a segment entry.
...

SEGNO, ...
BUFF, 0
*+6/ %Make a 6 word buffer.
...

| ND-100 and ND-500 | User RT and user SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 277*

[↑ Back to Top](#table-of-contents)

---

### 54B - DeleteFile (MDLFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Deletes a file. The pages of the file are released.

- You must have directory access to the file in order to delete it. RT programs can delete a file if user RT has directory access to it.
- Include a version number in the file name to delete specific versions of a file. Otherwise, all versions are deleted.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File name` |  | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

@DELETE-FILE, @DELETE-USERS-FILES, and CreateFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
...
DeleteFile(FileName);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "DeleteFile" USING FileName.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER FileName*64
...
Monitor_Call('DeleteFile', FileName(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : FileName(0:63)
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('DeleteFile', FileName)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRINGDATA 'EXAMPLE:SYMB'
ErrCode : W BLOCK 1
DeleteFile : EQU 37B9 + 54B

...
CALLG DeleteFile, 1, FileName
IF K GO ERROR
...
ERROR : W1 =: ErrCode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX   (FILE        %Load register X with address of file name.
MON   54           %Monitor call DeleteFile.
JMP   ERROR        %Error return from monitor call.
...                %Normal return.
ERROR, ...         %Error number in register A.
...
FILE, 'EXAMPLE:SYMB' %Delete file EXAMPLE:SYMB.


| ND-100 and ND-500 | All users | All programs |

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 137*

[↑ Back to Top](#table-of-contents)

---

### 55B - GetSpoolingEntry (RSQPE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the next spooling queue entry, that is, the next file to be printed. The entry is removed from the spooling queue.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | **Logical device number** of the printer. See appendix B. |
| `Param2` | UNKNOWN | I | **The 272-byte spooling entry.** |
| `Param3` | UNKNOWN | I | Byte 0:1 Number of printed copies. |
| `Param4` | UNKNOWN | I | 2:3 If ASCII apostrophe, the file is printed independently of spooling condition |
| `Param5` | UNKNOWN | I | 4:97 File name of spooling file. |
| `Param6` | UNKNOWN | I | 98:255 Message to be output on the error device before printing. |
| `Param7` | UNKNOWN | I | **Standard Error Code.** See appendix A. |

#### See Also

[AppendSpooling](#240b-appendspooling-apspe), [CloseSpoolingFile](#40b-closespoolingfile-spclo), and @LIST-SPOOLING-QUEUE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SpoolDevNumber : INTEGER2;
Buffer : ARRAY [0..8] OF RECORD...END;
...
GetSpoolingEntry(SpoolDevNumber, Buffer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SpoolDevNumber COMP.
01 Buffer.
   02 array COMP OCCURS 136 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetSpoolingEntry" USING SpoolDevNumber, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SpoolDevNumber
INTEGER Buffer(136)
...
Monitor Call('GetSpoolingEntry', SpoolDevNumber, Buffer(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SpoolDevNumber
BYTES : Buffer(0:271)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetSpoolingEntry', SpoolDevNumber, Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SpoolDevNumber : W BLOCK 1
Buffer : BY BLOCK 1000B
ErrCode : W BLOCK 1
GetSpoolingEntry : EQU 37B9 + 55B
...
CALLG GetSpoolingEntry, 2, SpoolDevNumber, Buffer
IF K GO ERROR
...
ERROR : W1 =: ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX  (BUFF       %Address of buffer for receiving queue entry.
LDT  SPOOL       %Logical number of spooling device.
MON  55          %Monitor call GetSpoolingEntry.
JMP  ERROR       %Error return from monitor call.
...              %Normal return.
ERROR, ...       %Error number in register A.
...
SPOOL, 5         %Obtain spooling entry of line printer no. 1.
BUFF, 0
*+128/           %Make a buffer of 256 bytes.


| ND-100 and ND-500   | All users | All programs |
|---------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 281*

[↑ Back to Top](#table-of-contents)

---

### 56B - SetUserParam (PASET)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets information about a background program. Use GetUserParam to read the 5 parameters when a program is terminated.

- SINTRAN III sets some of the parameter values if you give the command @ENABLE-TERMINATION-HANDLING first.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 57B - GetUserParam (PAGEI)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about why the last program terminated. There are 5 parameters for each background user. These can be set by SINTRAN III or your background program.

- Use SetUserParam to set the parameter values.
- SINTRAN III sets some of the parameter values if you give the command @ENABLE-TERMINATION-HANDLING first.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `1` | The last byte contains the user index. The byte in front of it contains the directory index. | I |  |
| `2` | Logical device number of the terminal. | I |  |
| `3` | Fatal error or the monitor call ErrorMessage returns the error number. If escape was pressed, -1 is returned. | I |  |
| `4` | User defined. | I |  |
| `5` | User defined. | I |  |

#### See Also

[TerminationHandling](#206b-terminationhandling-edtmp), [GetND500Param](#437b-getnd500param-5paget), @DEFINE-TERMINATION-HANDLING, @DISABLE-TERMINATION-HANDLING, and @SET-USER-PARAMETERS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buff : RECORD...END;
...
GetUserParam(Buff);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buff.
   02 array COMP OCCURS 5 TIMES.
   ...
   MONITOR-CALL "GetUserParam" USING Buff.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buff(5)
...
Monitor_Call('GetUserParam', Buff(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Buff(0:9)
...
Monitor_Call('GetUserParam', Buff(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buff : H BLOCK 5
GetUserParam : EQU 37B9 + 57B

CALLG GetUserParam, 1, Buff
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 57 %Monitor call GetUserParam.


PAR BUFF
%Buffer of 5 words to receive user parameters.

BUFF |
---|
0 | %Left byte: dir.index. Right byte: user index.
0 | %Logical device number, terminal number.
0 | %-1 if escape, otherwise error number.
0 | %User defined.
0 | %User defined.


ND-100 and ND-500 | All users | Background programs
---|---|---
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 301*

[↑ Back to Top](#table-of-contents)

---

### 61B - MemoryAllocation (FIXCS)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Fixes or unfixes ND-100 segments to be used by the ND-500 Monitor. You may also reserve a contiguous area in physical memory. Various other memory functions are reserved for the ND-500 Monitor.

- This monitor call is not normally used by ordinary programs.
- You may use this monitor call to reserve space for DMA buffers.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 62B - GetBytesInFile (RMAX)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the number of bytes in a file. Only the bytes containing data are counted.

- The file must be open.
- The number of bytes are only relevant to sequentially accessed files.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number. See OpenFile.` | UNKNOWN | I |  |
| `Number of bytes in the file.` | UNKNOWN | O |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[SetMaxBytes](#73b-setmaxbytes-smax), GetBytesInFile, and @FILE-STATISTICS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
NoOfBytes : LONGINT;
...
GetBytesInFile(FileNumber, NoOfBytes);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 NoOfBytes COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "GetBytesInFile" USING FileNumber, NoOfBytes.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER*4 NoOfBytes
...
Monitor Call('GetBytesInFile', FileNumber, NoOfBytes)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER FileNumber
INTEGER4: NoOfBytes
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetBytesInFile', FileNumber, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber: W BLOCK 1
NoofBytes: W BLOCK 1
ErrCode: W BLOCK 1
GetBytesInFile: EQU 37B9 + 62B
...
CALLG GetBytesInFile, 2, FileNumber, NoOfBytes
    IF K GO ERROR
...
ERROR: W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO      %File number returned from earlier open.
MON 62         %Monitor call GetBytesInFile.
JMP ERROR      %Error return from monitor call.
STD BYTES      %Normal return, store the number of bytes obtained.
...
ERROR,         %Error number in register A.
...
FILNO,
...
BYTES, O       %A double word
0              %


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 63B - In4x2Bytes (B41NW)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Reads 8 bytes from a word-oriented or character-oriented device, e.g. internal devices.

- Do not use this monitor call for terminals.
- This monitor call was mainly used for SIBAS communication via ND-NET. It is now seldom used.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number.` | See appendix B. | I |  |
| `Number of bytes read.` |  | I |  |
| `The string of bytes read.` |  | I |  |
| `Standard Error Code.` | See appendix A. | I |  |

#### See Also

[In8Bytes](#23b-in8bytes-b8inb), [InUpTo8Bytes](#21b-inupto8bytes-m8inb), [In8AndFlag](#310b-in8andflag-tbin8), InString, [InputString](#503b-inputstring-dvinst), [InByte](#1b-inbyte-inbt), and Out8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes : INTEGER2;
DataRead : PACKED ARRAY [0..7] OF CHAR;
...
In4x2Bytes(DeviceNumber, NoOfBytes, DataRead);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber  COMP.
01 NoOfBytes     COMP.
01 DataRead      PIC X(8).
01 ErrCode       COMP.
...
MONITOR-CALL "In4x2Bytes" USING DeviceNumber, NoOfBytes, DataRead.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoOfBytes
CHARACTER DataRead*8
...
Monitor_Call('In4x2Bytes', DeviceNumber, NoOfBytes, DataRead(1:8))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
BYTES : DataRead(0:7)
...

ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON

Monitor_Call(’In4x2Bytes’, DeviceNumber, NoOfBytes, DataRead)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
DataRead : STRING 8
ErrCode : W BLOCK 1
In4x2Bytes : EQU 37B9 + 63B
...

CALLG In4x2Bytes, 2, DeviceNumber, DataRead
  IF K GO ERROR
  W1 =: NoOfBytes
...

ERROR : W1 =: ErrCode       % ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO            % Logical device number.
MON 63               % Monitor call In4x2Bytes.
JMP ERROR            % Error return from monitor call.
STD BYTES            % Normal return, store first 4 bytes read.
COPY SL DA
STA BYTES+2          % Store next 2 bytes read.
STX BYTES+3          % Store last 2 bytes read.
STT COUNT            % Store number of bytes read.
...

ERROR, ...           % Error number in register A.
...

DEVNO, ...
COUNT, 0
BYTES, 0
0
0
0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 309*

[↑ Back to Top](#table-of-contents)

---

### 64B - WarningMessage (ERMSG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Outputs a file system error message. Appendix A shows the messages connected to each error code. The error code is input. The program continues.

- The error message is output to the terminal. In batch jobs, mode jobs, and RT programs it is output to the error device. The error device is normally the console.
- Error code 0 is illegal.

#### See Also

[GetErrorMessage](#334b-geterrormessage-getxm), ErrorMessage. ErrorMessage writes out the error message, terminates the program

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrCode : INTEGER2;
...
WarningMessage(ErrCode);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrCode COMP.
...
MONITOR-CALL "WarningMessage" USING ErrCode.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrCode
...
Monitor_Call('WarningMessage', ErrCode)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrCode
...
Monitor_Call('WarningMessage', ErrCode)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrCode : W BLOCK 1
WarningMessage : EQU 37B9 + 64B
...
CALLG WarningMessage, 1, ErrCode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Command | Parameter | Description                                  |
|---------|-----------|----------------------------------------------|
| LDA     | ERRNO     | %Error number of error message to be printed.|
| MON     | 64        | %Monitor call WarningMessage.                |
|         |           |                                              |

...
ERRNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 559*

[↑ Back to Top](#table-of-contents)

---

### 65B - ErrorMessage (QERMS)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Displays a file system error message. Appendix A shows the messages connected to each error number. The error number is input. The program terminates.

- The error message is displayed on the terminal. RT programs write it to the error device. The error device is normally the console.
- Do not input error number 0.

#### See Also

[GetErrorMessage](#334b-geterrormessage-getxm), WarningMessage. WarningMessage writes out the error message without terminating the program

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrNumber : INTEGER2;
...
ErrorMessage(ErrNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrNumber COMP.
...
MONITOR-CALL "ErrorMessage" USING ErrNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrNumber
...
Monitor_Call('ErrorMessage', ErrNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrNumber
...
Monitor_Call('ErrorMessage', ErrNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrNumber : W BLOCK 1
ErrorMessage : EQU 37B9 + 65B
...
CALLG ErrorMessage, 1, ErrNumber
IF K GO Error
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA ERRNO     %Error number of message to be printed.
MON 65        %Monitor call ErrorMessage.
...
ERRNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 171*

[↑ Back to Top](#table-of-contents)

---

### 66B - InBufferSpace (ISIZE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the current number of bytes in the input buffer. Terminals and other character devices place input in a buffer. All input monitor calls read from this buffer.

- Use ExecutionInfo to get the logical device number for terminals. You can specify 1 for your own terminal.
- The buffer size depends on the device.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Number of bytes in the buffer. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[InBufferState](#313b-inbufferstate-ibrisz), [ClearInBuffer](#13b-clearinbuffer-cibuf), [OutBufferSpace](#67b-outbufferspace-osize)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes : INTEGER2;
...
BytesInBuffer(DeviceNumber, NoOfBytes);   [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 NoOfBytes COMP.
01 ErrCode COMP.
...
MONITOR-CALL "InBufferSpace" USING DeviceNumber, NoOfBytes.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoOfBytes
...
Monitor Call1('InBufferSpace', DeviceNumber, NoOfBytes)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
...
ON ROUTINEERROR DO
&emsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('InBufferSpace', DeviceNumber, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
ErrCode : W BLOCK 1
InBufferSpace : EQU 3789 + 66B
...
CALLG InBufferSpace, i, DeviceNumber
IF K GO ERROR
W1 =: NoOfBytes %Result is returned in W1 register.
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA DEVNO %Logical device number.
MON 66 %Monitor call InBufferSpace.
JMP ERROR %Error return from monitor call.
STA COUNT %Normal return, store number of bytes in inbuffer.
...
ERROR, ... %Error number in register A.
...
DEVNO, ...
COUNT, 0

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 315*

[↑ Back to Top](#table-of-contents)

---

### 67B - OutBufferSpace (OSIZE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the number of free bytes in the output buffer (number of bytes which can be written before the program must wait). Terminals and other character devices place output in a buffer. Monitor calls like OutByte writes to this buffer.

- Use ExecutionInfo to get the logical device number for terminals. You can specify 1 for your own terminal.
- This monitor call is not available for internal devices. Use InBufferSpace and subtract this size from the inbuffer size.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Number of bytes which can be written before the program must wait. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

ClearOutBuffer, [InBufferSpace](#66b-inbufferspace-isize)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes : INTEGER2;
...
OutBufferSpace(DeviceNumber, NoOfBytes);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber  COMP.
01 NoOfBytes     COMP.
01 ErrCode       COMP.
...
MONITOR-CALL "OutBufferSpace" USING DeviceNumber, NoOfBytes.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoOfBytes
...
Monitor_Cal1('OutBufferSpace', DeviceNumber, NoOfBytes)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('OutBufferSpace', DeviceNumber, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
ErrCode : W BLOCK 1
OutBufferSpace : EQU 37B9 + 67B
    ...
    CALLG OutBufferSpace, 1, DeviceNumber
    IF K GO ERROR
    W1 =: NoOfBytes           %Result is returned in W1 register.
    ...
ERROR : W1 =: ErrCode        %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO                %Logical device number.
    MON 67                   %Monitor call OutBufferSpace.
    JMP ERROR                %Error return from monitor call.
    STA COUNT                %Normal return, store number of bytes.
ERROR,
    ...                      %Error number in register A.
DEVNO, ...
COUNT, 0                     %Number of bytes free space in outbuffer.


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 375*

[↑ Back to Top](#table-of-contents)

---

### 70B - CallCommand (COMMND)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Executes a SINTRAN III command from a program. The program terminates if an error occurs in the command.

- You are advised to use the newer monitor call, ExecuteCommand (UECOM), instead of CallCommand.
- Some commands may destroy your program. Use care with commands which affect your program's memory area.
- Note that the program may terminate if an error occurs. Use ExecuteCommand (UECOM) to avoid this problem.
- For commands with output, but without an output file as a parameter, e.g. @WHO, output is displayed automatically on the terminal of the user executing the program.
- Use SuspendProgram to wait a specified time interval between two CallCommands which depend on each other, e.g. CreateFile and OpenFile.
- The following commands are allowed from ND-500 programs: @DATCL, @COPY, @COPY-FILE, @SCHEDULE, @HOLD, @TERMINAL-MODE, @OPERATOR, @WAIT-FOR-OPERATOR, @SET-TERMINAL-TYPE, @GET-TERMINAL-TYPE, @CREATE-FILE, @EXPAND-FILE, @DELETE-FILE, @RENAME-FILE, @LIST-FILE, @FILE-STATISTICS, @OPEN-FILE, @CONNECT-FILE, @SET-FILE-ACCESS, @CLOSE-FILE, @LIST-OPEN-FILE, @SET-BLOCK-SIZE, @SET-PERMANENT-OPEN, @SET-BYTE-POINTER, @SET-BLOCK-POINTER, @APPEND-SPOOLING-FILE, @DELETE-SPOOLING-FILE, @SET-TEMPORARY-FILE, and @SCRATCH-OPEN.

#### See Also

[ExecuteCommand](#317b-executecommand-uecom), SetCommandBuffer. ExecuteCommand does not terminate the program if an error occurs

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Command : PACKED ARRAY [0..79] OF CHAR;
...
CallCommand(Command);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Command PIC X(100).
...
MONITOR-CALL "CallCommand" USING Command.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER Command*80
...
Monitor_Call('Call')Command', Command(1:80))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Command(0:79)
...
Monitor_Call('CallCommand', Command)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Command : STRINGDATA 'CLOSE-FILE 102'''
ErrCode : W BLOCK 1
CallCommand : EQU 3789 + 70B
...
CALLG CallCommand, 1, Command
IF K GO Error
...
Error, W1 =: ErrCode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (CMND    %Address of string with command to be executed.
MON 70        %Monitor call CallCommand.
...
CMND, 'CLOSE-FILE 102'  %Execute @CLOSE-FILE 102


| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 99*

[↑ Back to Top](#table-of-contents)

---

### 71B - DisableEscape (DESCF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

The ESCAPE key on the terminal normally terminates a program. This is called user break. This monitor call disables the escape function.

- The escape function is enabled again by EnableEscape.
- The escape function is enabled when you log out.
- When escape function is disabled, the escape character is treated as any other character.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `The terminal's logical device number.` | UNKNOWN | I | This parameter is ignored for background programs. Your own terminal is always selected. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[EnableEscape](#72b-enableescape-eescf), [SetEscapeHandling](#300b-setescapehandling-eusel), and @DISABLE-ESCAPE-FUNCTION

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
...
EscapeDisable(DeviceNumber); [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "DisableEscape" USING DeviceNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
...
Monitor_Call('DisableEscape', DeviceNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
...
Monitor_Call('DisableEscape', DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
DisableEscape : EQU 37B9 + 71B
...
CALLG DisableEscape, 1, DeviceNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO      %Logical device number.
MON 71         %Monitor call DisableEscape.
...
DEVNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 72B - EnableEscape (EESCF)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Enables the ESCAPE key on the terminal. The ESCAPE key normally terminates a program. This is called user break. You can disable this key with DisableEscape. To enable it again you should use EnableEscape.

- The escape function is enabled when you log out.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The terminal's logical device number. This parameter is ignored for background p |

#### See Also

DisableEscape, [SetEscapeHandling](#300b-setescapehandling-eusel), and @ENABLE-ESCAPE-FUNCTION

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
...
EscapeEnable(DeviceNumber); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
...
MONITOR-CALL "EnableEscape" USING DeviceNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
...
Monitor_Call('EnableEscape', DeviceNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
...
Monitor_Call('EnableEscape', DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
EnableEscape : EQU 37B9 + 72B
...
CALLG EnableEscape, 1, DeviceNumber
IF K GO Error
...
Error,
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDT | DEVNO | %Logical device number. |
| MON | 72    | %Monitor call EnableEscape. |
| ... |       |                             |
| DEVNO, ...  |                             |

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 165*

[↑ Back to Top](#table-of-contents)

---

### 73B - SetMaxBytes (SMAX)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the value of the maximum byte pointer in an opened file (i.e. the number of bytes minus 1). The specified number of bytes are stored when the file is closed. The error code 3 is returned if you later try to read beyond this size. Error code 3 means end of file.

- The file must be opened for write.
- This monitor call is only relevant for sequential access.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number. See OpenFile.` | UNKNOWN | I |  |
| `Maximum file size in bytes.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

GetBytesInfile, [SetStartByte](#74b-setstartbyte-setbt)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
MaxBytePointer : LONGINT;
...
SetMaxBytes(FileNumber, MaxBytePointer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 MaxBytePointer COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "SetMaxBytes" USING FileNumber, MaxBytePointer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER*4 MaxBytePointer
...
Monitor_Call('SetMaxBytes', FileNumber, MaxBytePointer)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
INTEGER4 : MaxBytePointer
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetMaxBytes', FileNumber, MaxBytePointer)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
MaxBytePointer : W BLOCK 1
ErrCode : W BLOCK 1
SetMaxBytes : EQU 37B9 + 73B
...
CALLG SetMaxBytes, 2, FileNumber, MaxBytePointer
    IF K GO ERROR
...
ERROR : W1 =: ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT  FILNO      %File number returned from earlier open.
LDD  POINT      %Maximum byte pointer.
MON  73         %Monitor call SetMaxBytes.
JMP  ERROR      %Error return from monitor call.
...             %Normal return.
ERROR,          %Error number in register A.
...
FILNO,
...
POINT,          %A double word.
...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 449*

[↑ Back to Top](#table-of-contents)

---

### 74B - SetStartByte (SETBT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the next byte to be read or written in an opened mass-storage file.

- The bytes in a file are numbered upwards from 0.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See OpenFile. |
| `Param2` | UNKNOWN | I | Start byte in the file, i.e. number of next byte to be read or written. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetStartByte](#75b-getstartbyte-reabt), [SetStartBlock](#77b-setstartblock-setbl), [SetMaxBytes](#73b-setmaxbytes-smax), and @SET-BYTE-POINTER

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
BytePointer : LONGINT;
...
SetBytePointer(FileNumber, BytePointer); [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 BytePointer COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "SetStartByte" USING FileNumber, BytePointer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER*4 BytePointer
...
Monitor_Call('SetStartByte', FileNumber, BytePointer)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
INTEGER4 : BytePointer
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetStartByte', FileNumber, BytePointer)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
BytePointer : W BLOCK 1
ErrCode : W BLOCK 1
SetStartByte : EQU 37B9 + 74B
...
CALLG SetStartByte, 2, FileNumber, BytePointer
IF K GO ERROR
...
ERROR : W1 =: ErrCode           %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT    FILNO        %File number returned from earlier open.
LOD    POINT        %Byte pointer.
MON    74           %Monitor call SetStartByte.
JMP    ERROR        %Error return from monitor call.
                    ... %Normal return.
ERROR, ...          %Error number in register A.
...
FILNO, ...
POINT, ...          %A double word.
                    ... %



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 471*

[↑ Back to Top](#table-of-contents)

---

### 75B - GetStartByte (REABT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the number of the next byte to access in a file. The bytes in a file are numbered from 0.

- The file must be opened for sequential access.
- You cannot use this monitor call for peripheral files.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞ File number.` | See OpenFile. | I |  |
| `↔ The number of the next byte to access.` | UNKNOWN | I |  |
| `← Standard Error Code.` | See appendix A. | I |  |

#### See Also

[SetStartByte](#74b-setstartbyte-setbt), [SetStartBlock](#77b-setstartblock-setbl)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
BytePointer : LONGINT;
...
GetStartByte(FileNumber, BytePointer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 BytePointer COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "GetStartByte" USING FileNumber, BytePointer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER*4 BytePointer
...
Monitor Call1('GetStartByte', FileNumber, BytePointer)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
INTEGER4 : BytePointer
...
ON ROUTINEERROR DO
   IF ErrCode >< 0 THEN ...
ENDON

Monitor_Call('GetStartByte', FileNumber, BytePointer)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
BytePointer : W BLOCK 1
ErrCode : W BLOCK 1
GetStartByte : EQU 37B9 + 75B
...
CALLG GetStartByte, 2, FileNumber, BytePointer
IF K GO ERROR
...
ERROR : W1 =: ErrCode          %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO        %File number returned from earlier open.
MON 75           %Monitor call GetStartByte.
JMP ERROR        %Error return from monitor call.
STD POINT        %Normal return, store byte pointer.
...
ERROR, ...       %Error number in register A.
...
FILNO, ...
POINT, 0         %A double word.
0                %



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 283*

[↑ Back to Top](#table-of-contents)

---

### 76B - SetBlockSize (SETBS)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the block size of an opened file. Monitor calls which read randomly from, or write randomly to a file, operate on blocks. See ReadFromFile and WriteToFile.

- The standard block size is 512 bytes. This block size is set when the file is opened.
- The block size is reset when the file is closed.
- Factors of 2048 bytes are the most efficient block sizes.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `⮞` | File number. See OpenFile. | I |  |
| `⮞` | Block size in bytes. It must be an even number. | I |  |
| `⮜` | Standard Error Code. See appendix A. | I |  |

#### See Also

[SetStartBlock](#77b-setstartblock-setbl), [ReadFromFile](#117b-readfromfile-rfile), [WriteToFile](#120b-writetofile-wfile), and @SET-BLOCK-SIZE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
BlockSize : LONGINT;
...
SetBlockSize(FileNumber, BlockSize);
If ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 BlockSize COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "SetBlockSize" USING FileNumber, BlockSize.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
INTEGER*4 BlockSize
...
Monitor_Call('SetBlockSize', FileNumber, BlockSize)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
INTEGER : BlockSize
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetBlockSize', FileNumber, BlockSize)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
BlockSize : W BLOCK 1
ErrCode : W BLOCK 1
SetBlockSize : EQU 37B9 + 76B
...
CALLG SetBlockSize, 2, FileNumber, BlockSize
IF K GO ERROR
...
ERROR : W1 =: ErrCode       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO       %File number returned from earlier open.
LDA SIZE        %Block size in words.
MON 76          %Monitor call SetBlockSize.
JMP ERROR       %Error return from monitor call.
...             %Normal return.
ERROR, ...      %Error number in register A.

FILNO, ...
SIZE, ...       %New block size.


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 433*

[↑ Back to Top](#table-of-contents)

---

### 77B - SetStartBlock (SETBL)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the next block to be read or written in an opened file. You may access the first bytes in the block with the monitor calls to read and write bytes.

- The standard block size is 512 bytes. This block size is set when the file is opened. You can change this with SetBlockSize.
- The first block of a file is number 0.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number` | See OpenFile. | I |  |
| `Block number` | Next block to be read or written. | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[SetStartByte](#74b-setstartbyte-setbt), @SET-BLOCK-POINTER. The monitor call is identical to SetStartByte with the block number multiplied by the block size as parameter

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, BlockNumber : INTEGER2;
...
SetStartBlock(FileNumber, BlockNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 BlockNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SetStartBlock" USING FileNumber, BlockNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, BlockNumber
...
Monitor Call('SetStartBlock', FileNumber, BlockNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, BlockNumber
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetStartBlock', FileNumber, BlockNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
BlockNumber : W BLOCK 1
ErrCode : W BLOCK 1
SetStartBlock : EQU 3789 + 77B
...
CALLG SetStartBlock, 2, FileNumber, BlockNumber
IF K GO ERROR
...
ERROR : W1 =: ErrCode      %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT   FILNO    %File number returned from earlier open.
LDA   BLKNO    %Block number.
MON   77       %Monitor call SetStartBlock.
JMP   ERROR    %Error return from monitor call.
...            %Normal return.
ERROR, ...     %Error number in register A.
...
FILNO, ...
BLKNO, ...


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 469*

[↑ Back to Top](#table-of-contents)

---

### 100B - StartRTProgram (RT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Starts an RT program. The program is moved to the execution queue. It is executed according to its priority.

- The program may be in the execution queue. Then it restarts as soon as it terminates.
- You can terminate RT programs with StopRTProgram.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of RT description. Use 0 for your own RT description address. |

#### See Also

[StartupTime](#102b-startuptime-abset), StartupInterval, [DelayStart](#101b-delaystart-set), and @RT

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram : INTEGER2;
...
StartRTProgram(RTProgram);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
...
MONITOR-CALL "StartRTProgram" USING RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram
...
Monitor_Call('StartRTProgram', RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram
...
Monitor_Call('StartRTProgram', RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
StartRTProgram : EQU 37B9 + 100B
...
CALLG StartRTProgram, 1, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
|        |            |                                                  |
|--------|------------|--------------------------------------------------|
| LDA    | (PAR       | %Load register A with address of parameter list. |
| MON    | 100        | %Monitor call StartRTProgram.                    |
| ...    |            |                                                  |
| PAR,   | RTPRO      | %Address of RT description.                      |
| ...    |            |                                                  |
| RTPRO, |            |                                                  |


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
|-------------------|-------------------------|-------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 485*

[↑ Back to Top](#table-of-contents)

---

### 101B - DelayStart (SET)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Starts an RT program after a specified time. The RT program is put in the time queue. It is moved to the execution queue after the specified period.

- RT programs already in the time queue are reinserted according to the new specifications.
- AdjustClock and @CLADJ do not affect the specified period.
- A period less than or equal to 0 moves the RT program to the execution queue the next time the basic time unit counter is incremented.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. Use 0 for the calling program. |
| `Param2` | UNKNOWN | I | The number of time units to stay in the time queue. |
| `Param3` | UNKNOWN | I | The type of time units. 1 = basic time units, i.e. 1/50th of a second, 2 = secon |

#### See Also

@SET, [StartupTime](#102b-startuptime-abset)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, TimeUnits, UnitType : INTEGER2;
...
DelayStart(RTProgram, TimeUnits, UnitType);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 TimeUnits COMP.
01 UnitType COMP.
...
MONITOR-CALL "DelayStart" USING RTProgram, TimeUnits, UnitType.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, TimeUnits, UnitType
...
Monitor_Call('DelayStart', RTProgram, TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, TimeUnits, UnitType
...
Monitor_Call('DelayStart', RTProgram, TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
TimeUnits : W BLOCK 1
UnitType : W BLOCK 1
DelayStart : EQU 37B9 + 101B
...
CALLG DelayStart, 3, RTProgram, TimeUnits, UnitType
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (PAR   %Load register A with address of parameter list.
MON  101    %Monitor call DelayStart.
...

PAR,
RTPRO
TIME
BASE
...

RTPRO, ...  %Address of RT description.
TIME,  ...  %Number of time units.
BASE,  ...  %Base time units.


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 135*

[↑ Back to Top](#table-of-contents)

---

### 102B - StartupTime (ABSET)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Starts an RT program at a specified time of the day. The RT program is then put in the time queue. It is moved to the execution queue at the specified time.

- The time of the day specified may have already passed. In this case, the program starts the next day.
- RT programs already in the time queue are reinserted according to the new specifications.
- AdjustClock, and @CLADJ affect the system's clock. Whenever the system time is changed, RT programs start according to the new time.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. Use 0 for the calling program. |
| `Param2` | UNKNOWN | I | Seconds. |
| `Param3` | UNKNOWN | I | Minutes. |
| `Param4` | UNKNOWN | I | Hours. |

#### See Also

[ExactStartup](#127b-exactstartup-dabst), DelayStartup, [AdjustClock](#112b-adjustclock-cladj), @ABSET

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, Seconds, Minutes, Hours: INTEGER2;
...
StartTime(RTProgram, Seconds, Minutes, Hours); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 Seconds COMP.
01 Minutes COMP.
01 Hours COMP.
...
MONITOR-CALL "StartupTime" USING RTProgram, Seconds, Minutes, Hours.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, Seconds, Minutes, Hours
...
Monitor_Call('StartupTime', RTProgram, Seconds, Minutes, Hours)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, Seconds, Minutes, Hours
...
Monitor_Call('StartupTime', RTProgram, Seconds, Minutes, Hours)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
Seconds : W BLOCK 1
Minutes : W BLOCK 1
Hours : W BLOCK 1
StartupTime : EQU 37B9 + 102B
...
CALLG StartupTime, 4, RTProgram, Seconds, Minutes, Hours
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Operation | Parameters     | Description                                      |
|-----------|----------------|--------------------------------------------------|
| LDA       | (PAR           | %Load register A with address of parameter list. |
| MON       | 102            | %Monitor call StartupTime.                       |

...
PAR, RTPRO   %Address of RT description.
SEC          %Seconds.
MIN          %Minutes.
HOUR         %Hour of day.
...

RTPRO, ...
SEC, ...
MIN, ...
HOUR, ...


ND-100 and ND-500 - User RT and user SYSTEM - RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 489*

[↑ Back to Top](#table-of-contents)

---

### 103B - StartupInterval (INTV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Prepares an RT program for periodic execution. The interval between the executions can be specified in hours, minutes, seconds, and basic time units. A basic time unit is 1/50th of a second.

- The RT program is not started. Use StartRTProgram, @RT, or similar, to start it.
- StopRTProgram, Disconnect, @ABORT or @DSCNT cancel this monitor call.
- One execution may be unfinished when it is time for the next execution, in which case the program's restart flag is set. If the delay becomes as long as two intervals, one execution is lost.
- The interval replaces any earlier specified intervals.
- AdjustClock and @CLADJ do not affect the interval.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Address of an RT description.` | UNKNOWN | I | 0 means calling program. GetRtAddress gives RT description addresses. |
| `Number of time units between executions of the program.` | UNKNOWN | I |  |
| `The type of time units.` | UNKNOWN | I | 1 = basic time units, i.e. 1/50th of a second, 2 = seconds, 3 = minutes, 4 = hours. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[ExactInterval](#130b-exactinterval-dintv) and @INTV. ExactInterval allows you to specify intervals between 0 and 4294967647 basic time units.

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, Time, Units : INTEGER2;
...
StartInterval (RTProgram, Time, Units); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 Time COMP.
01 Units COMP.
...
MONITOR-CALL "StartupInterval" USING RTProgram, Time, Units.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, Time, Units
...
Monitor_Call('StartupInterval', RTProgram, Time, Units)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, Time, Units
...
Monitor_Call('StartupInterval', RTProgram, Time, Units)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
Time : W BLOCK 1
Units : W BLOCK 1
StartupInterval : EQU 37B9 + 103B
...
CALLG StartupInterval, 3, RTProgram, Time, Units
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR      %Load register A with address of parameter list.
MON 103       %Monitor call StartupInterval.
...
PAR, RTPRO    %Address of RT description.
TIME          %Interval between each execution of program.
UNIT          %Time units.
...
RTPRO, ...
UNIT, ...
BASE, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 104B - SuspendProgram (HOLD)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Suspends the execution of your program for a given time. The execution then continues after the time specified by the monitor call.

#### See Also

[TimeOut](#267b-timeout-tmout), WaitForRestart, @HOLD

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
TimeUnits, UnitType : INTEGER2;
...
SuspendProgram(TimeUnits, UnitType);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 TimeUnits COMP.
01 UnitType COMP.
...
MONITOR-CALL "SuspendProgram" USING TimeUnits, UnitType.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER TimeUnits, UnitType
...
Monitor_Call('SuspendProgram', TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : TimeUnits, UnitType
...
Monitor_Call('SuspendProgram', TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
TimeUnits : W BLOCK 1
UnitType : W BLOCK 1
SuspendProgram : EQU 37B9 + 104B
...
CALLG SuspendProgram, 2, TimeUnits, UnitType
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDA    | (PAR          | %Load register A with address of parameter list. |
|--------|---------------|--------------------------------------------------|
| MON    | 104           | %Monitor call SuspendProgram.                    |
| PAR,   | TIME          | %Number of time units the program has to wait.   |
| BASE   |               | %Base time units.                                |
| TIME,  | ...           |                                                  |
| BASE,  | ...           |                                                  |


ND-100 and ND-500 | All users | All programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 497*

[↑ Back to Top](#table-of-contents)

---

### 105B - StopRTProgram (ABORT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Stops an RT program. It is removed from the time or execution queue. All reserved devices and files are released. RT programs schedules for periodic execution are also stopped.

- Nothing happens if the RT program is already stopped.
- The RT program restarts immediately if its restart flag is set.

#### See Also

[ExitRTProgram](#134b-exitrtprogram-rtext), @ABORT

#### Examples

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
MONITOR-CALL "StopRTProgram" USING RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram
Monitor_Call('StopRTProgram', RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram
Monitor_Call('StopRTProgram', RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTDescr : W BLOCK 1
...
StopRTProgram : EQU 37B9 + 105B
CALLG StopRTProgram, 1, RTDescr
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR   %Load register A with address of parameter list.
MON 105    %Monitor call StopRTProgram
...
PAR, RTPRO %Address of RT description.
...
RTPRO, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 495*

[↑ Back to Top](#table-of-contents)

---

### 106B - StartOnInterrupt (CONCT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

StartOnInterrupt connects an RT program to interrupts from a device. The RT program starts when an interrupt occurs. Use either WaitForRestart or SuspendProgram to make the program wait.

- You can remove this connection with NoInterruptStart.
- Several devices may be connected to one program.
- It is impossible to connect to some devices. Connection can be made if SINTRAN III has been generated with a connect driver routine for the device.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of RT description. Use 0 for your own RT description address. |
| `Param2` | UNKNOWN | I | Logical device number. See appendix B. |

#### See Also

[NoInterruptStart](#107b-nointerruptstart-dscnt), @CONCT

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, DeviceNumber : INTEGER2;
...
StartOnInterrupt(RTProgram, DeviceNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 DeviceNumber COMP.
...
MONITOR-CALL "StartOnInterrupt" USING RTProgram, DeviceNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, DeviceNumber
...
Monitor_Call('StartOnInterrupt', RTProgram, DeviceNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, DeviceNumber
...
Monitor_Call('StartOnInterrupt', RTProgram, DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
DeviceNumber : W BLOCK 1
StartOnInterrupt : EQU 3789 + 106B
...
CALLG StartOnInterrupt, 2, RTProgram, DeviceNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 106 %Monitor call StartOnInterrupt.
...


| PAR    | RTPRO       | %Address of RT description. |
|--------|-------------|-----------------------------|
| DEVNO  |             | %Logical device number.     |
| ...    | RTPRO, ...  |                             |
| DEVNO, | ...         |                             |


ND-100 and ND-500 | User RT and user SYSTEM | RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 481*

[↑ Back to Top](#table-of-contents)

---

### 107B - NoInterruptStart (DSCNT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

StartOnInterrupt connects an RT program to interrupts from a device. You remove this connection with NoInterruptStart.

- The program may be in the time queue. It is then removed. Periodic execution is prevented.
- Reserved resources are not released.
- The program is not removed from the execution queue.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of RT description. |

#### See Also

[StartOnInterrupt](#106b-startoninterrupt-conct), @DSCNT

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram : INTEGER2;
...
NoInterruptStart(RTProgram);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
...
MONITOR-CALL "NoInterruptStart" USING RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram
...
Monitor_Call('NoInterruptStart', RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
NoInterruptStart : EQU 37B9 + 107B
...
CALLG NoInterruptStart, 1, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (PAR       %Load register A with address of parameter list.
MON  107        %Monitor call NoInterruptStart.
...
PAR, RTPRO     %Address of RT description.
...
RTPRO, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 355*

[↑ Back to Top](#table-of-contents)

---

### 110B - SetRTPriority (PRIOR)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the priority of an RT program. RT programs may be given priorities from 0 to 255. SINTRAN III executes the RT program with the highest priority.

- The priority of background programs vary between 20 and 60.
- Programs with priority 0 will never start. You may use this to suspend programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. You may use 0 for your own program. |
| `Param2` | UNKNOWN | I | Priority. |
| `Param3` | UNKNOWN | I | The previous priority. |

#### See Also

SetND500Priority, @PRIOR

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, PriorityLevel, OldPriority : INTEGER2;
...
SetRTPriority(RTProgram, PriorityLevel, OldPriority);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 PriorLevel COMP.
01 OldPriority COMP.
...
MONITOR-CALL "SetRTPriority" USING RTProgram, PriorLevel, OldPriority.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, PriorityLevel, OldPriority
...
Monitor_Call('SetRTPriority', RTProgram, PriorityLevel, OldPriority)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, PriorityLevel, OldPriority
...
Monitor_Call('SetRTPriority', RTProgram, PriorityLevel, OldPriority)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
PriorityLevel : W BLOCK 1
OldPriority : W BLOCK 1
SetRTPriority : EQU 37B9 + 110B



CALLG SetRTPriority, 2, RTProgram, PriorityLevel
Wl =: OldPriority %Result is returned in Wl register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 110 %Monitor call SetRTPriority.
STA OLDP %Store old priority returned.
...
OLDP, 0
PAR, RTPRO %Address of RT description.
PRIOR %New priority.
...
RTPRO, ...
PRIOR, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 467*

[↑ Back to Top](#table-of-contents)

---

### 111B - SetClock (UPDAT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gives new values to the computer's clock and calendar. If the computer panel has a clock, it is updated.

- The startup time for RT programs can be set by StartupTime. Such RT programs start according to the new time.
- Illegal time values, e.g. 61 minutes, stop the program and output a message.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Minutes. |
| `Param2` | UNKNOWN | I | Hours. |
| `Param3` | UNKNOWN | I | Days. |
| `Param4` | UNKNOWN | I | Months. |
| `Param5` | UNKNOWN | I | Years. |

#### See Also

[AdjustClock](#112b-adjustclock-cladj), [GetCurrentTime](#113b-getcurrenttime-clock), and GetBasicTime

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Minute, Hour, Day, Month, Year : INTEGER2;
...
SetClock(Minute, Hour, Day, Month, Year);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Minute COMP.
01 Hour COMP.
01 Day COMP.
01 Month COMP.
01 Year COMP.
...
MONITOR-CALL "SetClock" USING Minute, Hour, Day, Month, Year.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Minute, Hour, Day, Month, Year
...
Monitor_Call('SetClock', Minute, Hour, Day, Month, Year)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Minute, Hour, Day, Month, Year
...
Monitor_Call('SetClock', Minute, Hour, Day, Month, Year)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Minute | :W BLOCK 1 |
| --- | --- |
| Hour | :W BLOCK 1 |
| Day | :W BLOCK 1 |
| Month | :W BLOCK 1 |
| Year | :W BLOCK 1 |

SetClock : EQU 37B9 + 111B
...
CALLG SetClock, 5, Minute, Hour, Day, Month, Year
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 111 %Monitor call SetClock.


| PAR, | MIN | %New minutes of hour. |
| --- | --- | --- |
| | HOUR | %New hour of day. |
| | DAY | %New day of month. |
| | MONTH | %New month of year. |
| | YEAR | %New value for year. |


MIN, ...
HOUR, ...
DAY, ...
MONTH, ...
YEAR, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 437*

[↑ Back to Top](#table-of-contents)

---

### 112B - AdjustClock (CLADJ)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the computer's clock (i.e. the current system time) forward or back. If the operator panel has a clock, it is also adjusted.

- The startup time for RT programs can be set by StartupTime. When the current system time is modified, the next startup times of periodic programs started by StartupTime are also affected. Other scheduling times are not affected.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Number of time units the clock will be adjusted by. Negative values make the clo |
| `Param2` | UNKNOWN | I | The type of time units. 1 = basic time units, i.e. 1/50th of a second, 2 = secon |

#### See Also

[SetClock](#111b-setclock-updat), [GetCurrentTime](#113b-getcurrenttime-clock), [StartupTime](#102b-startuptime-abset), [GetBasicTime](#11b-getbasictime-time)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
TimeUnits, UnitType : INTEGER2;
...
AdjustClock(TimeUnits, UnitType);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 TimeUnits COMP.
01 UnitType COMP.
...
MONITOR-CALL "AdjustClock" USING TimeUnits, UnitType.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER TimeUnits, UnitType
...
Monitor_Call('AdjustClock', TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : TimeUnits, UnitType
...
Monitor_Call('AdjustClock', TimeUnits, UnitType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
TimeUnits : W BLOCK 1
UnitType : W BLOCK 1
ErrCode : W BLOCK 1
AdjustClock : EQU 3789 + 112B
...
CALLG AdjustClock, 2, TimeUnits, UnitType
IF K GO ERROR
...
Error : W1 =: ErrCode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 112 %Monitor call AdjustClock.
...
PAR,  TIME  %Number of time units the clock is adjusted by.
BASE        %Time units.
...
TIME, ...
BASE, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 75*

[↑ Back to Top](#table-of-contents)

---

### 113B - GetCurrentTime (CLOCK)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the current system time and date.

- The current system time is returned as basic time units, seconds, minutes, hours, day, month, and year.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `0:1` | basic time units | 0:3 on the ND-500 |  |
| `2:3` | seconds | 4:7 on the ND-500 |  |
| `4:5` | minutes | 8:11 on the ND-500 |  |
| `6:7` | hours | 12:15 on the ND-500 |  |
| `8:9` | day | 16:19 on the ND-500 |  |
| `10:11` | month | 20:23 on the ND-500 |  |
| `12:13` | year | 24:27 on the ND-500 |  |

#### See Also

[GetBasicTime](#11b-getbasictime-time), [AdjustClock](#112b-adjustclock-cladj), [SetClock](#111b-setclock-updat), and @DATCL

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
TimeBuffer : record;
...
GetCurrentTime(TimeBuffer);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 TimeBuffer.
   02 array COMP OCCURS 7 TIMES.
   ...
MONITOR-CALL "GetCurrentTime" USING TimeBuffer.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER TimeBuffer(7)
...
Monitor_Call('GetCurrentTime', TimeBuffer(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER ARRAY : TimeBuffer(0:6)
...
Monitor_Call('GetCurrentTime', TimeBuffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
TimeBuffer : W BLOCK 7
GetCurrentTime : EQU 37B9 + 113B
...
CALLG GetCurrentTime, 1, TimeBuffer
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Parameter | Comment                                      |
|-------------|-----------|----------------------------------------------|
| LDA         | (PAR      | %Load register A with address of parameter list. |
| MON         | 113       | %Monitor call GetCurrentTime.                 |
| ...         |           |                                              |
| PAR,        | CLOK      |                                              |
| ...         |           |                                              |
| CLOK, 0     |           | %Basic time units.                           |
|             | 0         | %Seconds.                                    |
|             | 0         | %Minutes.                                    |
|             | 0         | %Hours.                                      |
|             | 0         | %Day.                                        |
|             | 0         | %Month.                                      |
|             | 0         | %Year.                                       |


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 227*

[↑ Back to Top](#table-of-contents)

---

### 114B - GetTimeUsed (TUSED)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the time you have used the CPU since you logged in. In batch jobs, you get the time since you entered the job.

- The CPU time used is given in basic time units. A basic time unit is 1/50th of a second.
- Can also be used from RT-programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The CPU time used. |

#### See Also

@TIME-USED

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
TimeUsed : LONGINT;
...
GetTimeUsed(TimeUsed);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 TimeUsed COMP PIC S9(10).
...
MONITOR-CALL "GetTimeUsed" USING TimeUsed.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER*4 TimeUsed
...
Monitor_Call('GetTimeUsed', TimeUsed)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER4 : TimeUsed
...
Monitor_Call('GetTimeUsed', TimeUsed)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
TimeUsed : W BLOCK 1
GetTimeUsed : EQU 37B9 + 114B
...
CALLG GetTimeUsed, 0
W1 =: TimeUsed           %Result is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON     114   %Monitor call GetTimeUsed.
STD     TIME  %Store CPU time.
...
TIME, 0       %A double word.
0             %



| **ND-100 and ND-500** | **All users**   | **Background programs** |
|-----------------------|-----------------|------------------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 293*

[↑ Back to Top](#table-of-contents)

---

### 115B - FixScattered (FIX)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Place a segment in physical memory. Its pages will no longer be swapped to the disk. The segment must be non demand. Its pages will be scattered in physical memory. You may, for example, use this function for time critical operations or for allocating DMA buffers.

- Use UnFixSegment or @UNFIX to allow the RT LOADER to clear the segment.
- Your program terminates if you refer to a nonexistent segment or a demand segment. An error message is output on the error device.
- Only a limited number of pages can be fixed. This limit is defined when SINTRAN III is generated. You may change it with the command CHANGE-VARIABLE in the SINTRAN-SERVICE-PROGRAM.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number to be fixed in memory. |

#### See Also

UnFixSegment, [FixContiguous](#160b-fixcontiguous-fixc), FixInMemory, [MemoryAllocation](#61b-memoryallocation-fixcs), [FixIOArea](#404b-fixioarea-iofix), and @FIXC

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
...
FixScattered(SegmentNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
...
MONITOR-CALL "FixScattered" USING SegmentNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
...
Monitor_Call('FixScattered', SegmentNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber
...
Monitor_Call('FixScattered', SegmentNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNumber : W BLOCK 1
FixScattered : EQU 37B9 + 115B
...
CALLG FixScattered, 1, SegmentNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (PAR   %Load register A with address of parameter list.
MON  115   %Monitor call FixScattered
...
PAR,  SEGNO  %Segment number to be fixed in memory.
...
SEGNO, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
|-------------------|-------------------------|-------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 207*

[↑ Back to Top](#table-of-contents)

---

### 116B - UnfixSegment (UNFIX)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Releases a fixed segment and removes it from the Page Index Table (PIT). Its pages may then be swapped from physical memory to the disk. Use FixScattered or FixContiguous to fix the segment.

- You must use UnFixSegment or @UNFIX to allow the RT-LOADER to clear the segment.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number to be released. |

#### See Also

FixInMemory, [MemoryAllocation](#61b-memoryallocation-fixcs), [FixIOArea](#404b-fixioarea-iofix), and @UNFIX

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
...
UnFixSegment(SegmentNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
...
MONITOR-CALL "UnfixSegment" USING SegmentNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
...
Monitor_Call('UnfixSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber

...

Monitor_Call('UnfixSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNumber : W BLOCK 1
UnfixSegment : EQU 37B9 + 116B
...

CALLG UnfixSegment, 1, SegmentNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDA            | (PAR         | %Load register A with address of parameter list.  |
| MON 116        |              | %Monitor call UnfixSegment.                       |
| ...            |              |                                                   |
| PAR, SEGNO     |              | %Segment number to be unfixed.                    |
| ...            |              |                                                   |
| SEGNO, ...     |              |                                                   |


| ND-100 and ND-500  | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 553*

[↑ Back to Top](#table-of-contents)

---

### 117B - ReadFromFile (RFILE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads any number of bytes from a file. The read operation must start at the beginning of a block. The file must be opened for random read access.

- The standard block size is 512 bytes. You can change this with SetBlockSize. The first block is number 0.
- You may use access code D for direct transfer. Then the block size must be a multiple of the page size. The number of bytes to transfer must be a multiple of the block size.
- Peripheral files are always read sequentially.
- Data transfer across segment or RT common limits is illegal as this would be likely to result in inconsistent data.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number` | See Openfile. | I |  |
| `Wait flag` | Use 0 to suspend the program until the transfer is completed. Other values make the program continue. You may check that the transfer is completed by AwaitFileTransfer. | I |  |
| `Transferred data` |  | I |  |
| `Block number` | To start the read operation. Use -1 to read the next block. | I |  |
| `Number of bytes` | To read. | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[SetStartBlock](#77b-setstartblock-setbl), [SetBlockSize](#76b-setblocksize-setbs), [ReadDiskPage](#270b-readdiskpage-rdpag), [ReadBlock](#7b-readblock-rpage), [WriteBlock](#10b-writeblock-wpage), and WriteToFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, WaitFlag, BlockNo : INTEGER2;
Buff : ARRAY [0..15] OF RECORD...END;
NoOfBytes : LONGINT;
...
ReadFromFile(FileNo, WaitFlag, Buff, BlockNo, NoOfBytes);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 WaitFlag COMP.
01 Buff.
   02 array COMP OCCURS 256 TIMES.
01 BlockNo COMP.
01 NoOfBytes COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "ReadFromFile" USING FileNo, WaitFlag, Buff,
                                BlockNo, NoOfBytes.

CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, WaitFlag, BlockNo
INTEGER Buff(256)
INTEGER*4 NoOfBytes
...
Monitor_Call('ReadFromFile', FileNo, WaitFlag, Buff(1),
                               BlockNo, NoOfBytes)
C  IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo, RetFlag, BlockNo
BYTES : Buff(0:511)
INTEGER4 : NoOfBytes
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('ReadfromFile', FileNo, RetFlag, Buff(0), BlockNo, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
WaitFlag : W BLOCK 1
Buff : W BLOCK 256 %Must start on an even byte address.
BlockNo : W BLOCK 1
NoOfBytes : W BLOCK 1
ErrCode : W BLOCK 1
ReadFromFile : EQU 37B9 + 117B

...
CALLG ReadfromFile, 5, FileNo, WaitFlag, Buff, BlockNo, NoOfBytes
IF K GO ERROR
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 117 %Monitor call ReadFromFile.
JAF ERROR %Handle error if register A is non-zero.
...
ERROR, ... %Error number in register A.
...
PAR, FILNO %File number returned from earlier call to OpenFile.
RETUR %Return flag.
BUFF %Buffer to receive data.
BLKNO %Block number in file where data starts.
COUNT %Number of words to be read.
...
FILNO, ...
RETUR, 0
BUFF, 0
*+400/ %Make a buffer of 256 words.
BLKNO, ...
COUNT, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 399*

[↑ Back to Top](#table-of-contents)

---

### 120B - WriteToFile (WFILE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Writes any number of bytes to a file. The read operation must start at the beginning of a block. The file must be opened for random write access.

- The standard block size is 512 bytes. You can change this with SetBlockSize. The first block is number 0.
- You may use access code D for direct transfer. Then the block size must be a multiple of the page size. The number of bytes to transfer must be a multiple of the block size. The data must be fixed contiguously in memory.
- Peripheral files are always written to sequentially.
- Data transfer across segment or RT common limits is illegal as this would be likely to result in inconsistent data.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See OpenFile. |
| `Param2` | UNKNOWN | I | Wait flag. Use 0 to suspend the program until the transfer is completed. Other v |
| `Param3` | UNKNOWN | I | Data to be transferred. |
| `Param4` | UNKNOWN | I | Block number to start writing from. Use -1 to write to the next block. |
| `Param5` | UNKNOWN | I | Number of bytes to be written. |
| `Param6` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SetStartBlock](#77b-setstartblock-setbl), [SetBlockSize](#76b-setblocksize-setbs), [WriteDiskPage](#271b-writediskpage-wdpag), [WriteBlock](#10b-writeblock-wpage), and ReadFromFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, ReturnFlag, BlockNo : INTEGER;
NoOfBytes : LONGINT;
Buff : ARRAY [0..15] OF RECORD...END;
...
WriteToFile(FileNo, ReturnFlag, Buff, BlockNo, NoOfBytes);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP. 01 ReturnFlag COMP.
01 Buff.
   02 array COMP OCCURS 256 TIMES.
01 BlockNo COMP. 01 NoOfBytes COMP PIC S9(10).
01 ErrCode COMP.
...
MONITOR-CALL "WriteToFile" USING FileNo, ReturnFlag, Buff,
                                BlockNo, NoOfBytes.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, ReturnFlag, BlockNo
INTEGER Buff(256)
INTEGER*4 NoOfBytes
...
Monitor_Call('WriteToFile', FileNo, ReturnFlag, Buff(1),
                             BlockNo, NoOfBytes)
C IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo, ReturnFlag, BlockNo
BYTES : Buff(0:511)
INTEGER4 : NoOfBytes

...

ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON

Monitor_Call('WriteToFile', FileNo, ReturnFlag, Buff(0), BlockNo, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
ReturnFlag : W BLOCK 1
Buff : W BLOCK 256
BlockNo : W BLOCK 1
NoOfBytes : W BLOCK 1
ErrCode : W BLOCK 1
WriteToFile : EQU 3789 + 120B

...

CALLG WriteToFile, 5, FileNo, ReturnFlag, Buff, BlockNo, NoOfBytes
IF K GO ERROR

...

ERROR : W1 =: ErrCode       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR       %Load register A with address of parameter list.
MON 120        %Monitor call WriteToFile.
JAF ERROR      %Do error handling if register A is non-zero.

ERROR, ...     %Error number in register A.

...

PAR, FILNO     %File number returned from earlier open.
    FLAG       %Wait flag.
    BUFF       %Buffer containing data to be written.
    BLKNO      %Block number where writing is started.
    COUNT      %Number of words to transfer.

...

FILNO, ...
FLAG, ...
BUFF, 0
    *+1000/    %Make a buffer of 512 words, 4 blocks.
BLKNO ...
COUNT, 0
    1000       %Transfer 512 words.



ND-100 and ND-500 All users Background programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 569*

[↑ Back to Top](#table-of-contents)

---

### 121B - AwaitFileTransfer (WAITF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Checks that a data transfer to or from a mass-storage file is completed. The monitor call is relevant to ReadFromFile and WriteToFile operations. These data transfers are carried out independently of the CPU.

#### See Also

[AwaitTransfer](#431b-awaittransfer-mwaitf)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, ReturnFlag, Status : INTEGER2;
...
AwaitFileTransfer(FileNumber, ReturnFlag, Status);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 ReturnFlag COMP.
01 Status COMP.
...
MONITOR-CALL "AwaitFileTransfer" USING FileNumber, ReturnFlag, Status.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, ReturnFlag, Status
...
Monitor_Call('AwaitfileTransfer', FileNumber, ReturnFlag, Status)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, ReturnFlag, Status
...
Monitor_Call('AwaitFileTransfer', FileNumber, ReturnFlag, Status)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
ReturnFlag : W BLOCK 1
Status : W BLOCK 1
AwaitFileTransfer : EQU 37B9 + 121B
...
CALLG AwaitFileTransfer, 2, FileNumber, ReturnFlag
IF K GO Error
...
Error, W1 =: Status %Status is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LOA (PAR %Load register A with address of parameter list.
MON 121 %Monitor call AwaitFileTransfer.
STA STAT %Store returned status.
JAP ERROR %Handle error if STAT is greater than 0.
...
ERROR, ... %Handle the error with error number in STAT.
...
STAT, 0
PAR, FILNO %File number.
FLAG %Return flag.
...
FILNO, ...
FLAG, 0


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 87*

[↑ Back to Top](#table-of-contents)

---

### 122B - ReserveResource (RESRV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reserves a device or file for your program only. You release it with ReleaseResource. Some devices, e.g. terminals, have both an input and output part. You can only reserve one part with each ReserveResource call.

- A normal termination of an RT program releases all resources.
- Release the device with ReleaseResource or ForceRelease.
- A background program does not release a resource when you press the ESCAPE key.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Input or output flag. Use 0 for the input part and 1 for the output part. |
| `Param3` | UNKNOWN | I | Wait flag. Use 0 to make the program wait if the resource is already reserved. U |
| `Param4` | UNKNOWN | I | Return status. Only used if the wait flag is 1. A negative value is returned if  |

#### See Also

[ForceReserve](#124b-forcereserve-prsrv), [ReserveDir](#246b-reservedir-redir), @RESRV, @RESERVE-FILE, and @RESERVE-DEVICE-UNIT

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, IOFlag, WaitFlag, ReturnStatus : INTEGER2;
...
ReserveResource(DeviceNo, IOFlag, WaitFlag, ReturnStatus);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo COMP.
01 IOFlag COMP.
01 WaitFlag COMP.
01 RetStatus COMP.
...
MONITOR-CALL "ReserveResource" USING DevNo, IOFlag, WaitFlag, RetStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DevNo, IOFlag, WaitFlag, ReturnStatus
...
Monitor_Call('ReserveResource', DevNo, IOFlag, WaitFlag, ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER: DeviceNo, IOFlag, WaitFlag, ReturnStatus
...
Monitor_Call('ReserveResource', DeviceNo, IOFlag, WaitFlag, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
IOFlag : W BLOCK 1
WaitFlag : W BLOCK 1
Status : W BLOCK 1
ReserveResource : EQU 37B9 + 122B
...
CALLG ReserveResource, 3, DeviceNo, IOFlag, WaitFlag
W1 =: Status
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 122 %Monitor call ReserveResource.
...
PAR, DEVNO %Logical device number.
IOF %Input/output flag.
WFLAG %Wait flag
STAT %Status

| DEVNO, ... |   |
|------------|---|
| IOF, ...   |   |
| WFLAG, ... |   |
| STAT, ...  |   |

ND-100 and ND-500 | All users | All programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 419*

[↑ Back to Top](#table-of-contents)

---

### 123B - ReleaseResource (RELES)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Releases a reserved device or file. The resource can then be used by another program. You reserve a device or opened file with ReserveResource. Some devices, e.g. terminals, have both an input and output part. You can only release one part with each ReleaseResource.

- A normal termination of an RT program release all resources.
- Reserve the device with ReserveResource or ForceReserve.
- CloseFile or @CLOSE-FILE releases reserved files.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Input or output flag. Use 0 for the input part and 1 for the output part. |

#### See Also

[ForceRelease](#125b-forcerelease-prlrs), [ReserveResource](#122b-reserveresource-resrv), [ReleaseDir](#247b-releasedir-rldir), @RESRV, @RELEASE-FILE, and @RELEASE-DEVICE-UNIT

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, IOFlag : INTEGER2;
...
ReleaseResource(DeviceNumber, IOFlag);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 IOFlag COMP.
...
MONITOR-CALL "ReleaseResource" USING DeviceNumber, IOFlag.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, IOFlag
...
Monitor_Call('ReleaseResource', DeviceNumber, IOFlag)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, IOFlag
...
Monitor_Call('ReleaseResource', DeviceNumber, IOFlag)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
IOFlag : W BLOCK 1
ReleaseResource : EQU 3789 + 123B
...
CALLG ReleaseResource, 2, DeviceNumber, IOFlag
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR   %Load register A with address of parameter list.
MON 123    %Monitor call ReleaseResource.
...
PAR, DEVNO %Logical device number.
IOF        %Input/output flag.
DEVNO, ...
IOF, ...

| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 411*

[↑ Back to Top](#table-of-contents)

---

### 124B - ForceReserve (PRSRV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reserves a device for an RT program other than that which is calling. Use ForceRelease if the device is already reserved.

- You can only release peripheral devices, such as terminals and printers, and semaphores in this way.
- Programs in a waiting queue may reserve the device between your ForceRelease and ForceReserve calls if they have higher priority than your program.
- The SINTRAN III Real Time Guide (ND-860133) describes this in more detail.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Input or output flag. Use 0 for the input part and 1 for the output part. |
| `Param3` | UNKNOWN | I | RT description address of the RT program to reserve the device. Use 0 for your o |
| `Param4` | UNKNOWN | I | Return status. 0 means OK. A negative value is returned if the device already wa |

#### See Also

[ForceRelease](#125b-forcerelease-prlrs), [ReserveResource](#122b-reserveresource-resrv), and @PRSRV

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, IOflag, RTProgram, Stat : INTEGER2;
...

PrivReserve(DeviceNo, IOflag, RTProgram, Stat); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 IOFlag COMP.
01 RTProgram COMP.
01 Stat COMP.
...
MONITOR-CALL "ForceReserve" USING DeviceNo, IOFlag, RTProgram, Stat.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, IOFlag, RTProgram, Stat
...
Monitor_Call('ForceReserve', DeviceNo, IOFlag, RTProgram, Stat)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, IOFlag, RTProgram, Stat
...
Monitor_Call('ForceReserve', DeviceNo, IOFlag, RTProgram, Stat)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
IOFlag : W BLOCK 1
RTProgram : W BLOCK 1
ForceReserve : EQU 3789 + 124B
...
CALLG ForceReserve, 3, DeviceNo, IOFlag, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR  %Load register A with address of parameter list.
MON 124  %Monitor call ForceReserve.
STA STAT %Store status returned.
...
STAT, 0
PAR, DEVNO  %Logical device number.
IOF  %Input or Output flag.
RTPRO %Address of RT description.
...
DEVNO, ...
IOF, ...
RTPRO, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 211*

[↑ Back to Top](#table-of-contents)

---

### 125B - ForceRelease (PRLRS)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Releases a device reserved by an RT program other than that which is calling. You can then reserve the device for your own RT program. Some devices, such as terminals, have both an input and output part. You can only release one part with each ForceRelease.

- Use ReservationInfo to get the RT description address of the reserving RT program. You may then give the device back with ForceReserve.
- Programs in a waiting queue may reserve the device between your ForceRelease and ReserveResource calls if they have higher priority than your program.
- The SINTRAN III Real Time Guide (ND-860133) describes this in more detail.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Input or output flag. Use 0 for the input part and 1 for the output part. |

#### See Also

[ForceReserve](#124b-forcereserve-prsrv), [ReleaseResource](#123b-releaseresource-reles), and @PRLRS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, IOflag: INTEGER2;
...
PrivRelease(DeviceNumber, IOflag);   [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber     COMP.
01 IOflag           COMP.
...
MONITOR-CALL "ForceRelease" USING DeviceNumber, IOflag.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, IOflag
...
Monitor_Call('ForceRelease', DeviceNumber, IOflag)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, IOflag
...
Monitor_Call('ForceRelease', DeviceNumber, IOflag)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
IOflag : W BLOCK 1
ForceRelease : EQU 37B9 + 125B
...
CALLG ForceRelease, 2, DeviceNumber, IOflag
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDA | (PAR | %Load register A with address of parameter list. |
| MON | 125  | %Monitor call ForceRelease.                      |
| ... |      |                                                  |
| PAR, DEVNO | %Logical device number.                          |
| IOFL | %    | Input or Output flag.                           |
| ... |      |                                                  |

DEVNO, ...
IOFL, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 209*

[↑ Back to Top](#table-of-contents)

---

### 126B - ExactDelayStart (DSET)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets an RT program to start after a given period. It is then moved from the time queue to the execution queue. The period is specified in basic time units. A basic time unit is 1/50th of a second. The period may be from 1 to 4294967647 basic time units.

- The program may already be in the time queue. It is then reinserted according to the new specifications.
- A period less than or equal to 0 transfers the RT program to the execution queue the next time the basic time unit counter is incremented.
- SetClock, AdjustClock and @CLADJ do not affect the interval.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. Use 0 for the calling program. |
| `Param2` | UNKNOWN | I | The number of basic time units to stay in the time queue. |

#### See Also

[SetClock](#111b-setclock-updat), [AdjustClock](#112b-adjustclock-cladj), [DelayStart](#101b-delaystart-set)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, BasicTimeUnits : INTEGER2;
...
ExactDelayStart(RTProgram, BasicTimeUnits);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 BasicTimeUnits COMP.
...
MONITOR-CALL "ExactDelayStart" USING RTProgram, BasicTimeUnits.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, BasicTimeUnits
...
Monitor_Call('ExactDelayStart', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, BasicTimeUnits
...
Monitor_Call('ExactDelayStart', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
BasicTimeUnits : W BLOCK 1
ExactDelayStart : EQU 37B9 + 126B
...
CALLG ExactDelayStart, 2, RTProgram, BasicTimeUnits
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (PAR   %Load register A with address of parameter list.
MON  126    %Monitor call ExactDelayStart.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 127B - ExactStartup (DABST)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Starts an RT program at a specific time. The time is given in basic time units. A basic time unit is 1/50th of a second. The RT program is moved from the time queue to the execution queue at the specified time.

- It may already be later than the time specified. The RT program is then scheduled for the next day.
- The RT program may already be in the time queue. It is then reinserted according to the new time specifications.
- AdjustClock and @CLADJ affect the startup. The RT program starts according to the new time.
- Use GetBasicTime to read the internal time in basic time units.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of an RT description. 0 means calling program. GetRtAddress gives RT des |
| `Param2` | UNKNOWN | I | StartupTime in basic time units. |

#### See Also

StartupTime. StartupTime allows you to specify the time in hours, minutes or seconds

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, BasicTimeUnits : INTEGER2;
...
ExactStartup(RTProgram, BasicTimeUnits);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 BasicTimeUnits COMP.
...
MONITOR-CALL "ExactStartup" USING RTProgram, BasicTimeUnits.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, BasicTimeUnits
...
Monitor_Call('ExactStartup', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, BasicTimeUnits
...
Monitor_Call('ExactStartup', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
BasicTimeUnits : W BLOCK 1
ExactStartup : EQU 3789 + 127B
...
CALLG ExactStartup, 2, RTProgram, BasicTimeUnits
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDA | (PAR | %Load register A with address of parameter list. |
| MON | 127 | %Monitor call ExactStartup. |
| ... | | |
| PAR, RTProgram | %Address of RT description. |
| TIME | %Time when the program is to be executed. |
| ... | | |
| RTPRO, ... | %A double word giving time in basic time units. |
| TIME, ... | % |


ND-100 and ND-500 | User RT and user SYSTEM | RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 179*

[↑ Back to Top](#table-of-contents)

---

### 130B - ExactInterval (DINTV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Prepares an RT program for periodic execution. The interval between the executions may be from 1 to 4294967647 basic time units. A basic time unit is 1/50th of a second.

- The RT program is not started. Use, for example, StartRTProgram or @RT to start it.
- StopRTProgram, Disconnect, @ABORT or @DSCNT cancel this monitor call.
- One execution may be unfinished when it is time for the next execution. In this case, the program's restart flag is set. If the delay becomes as long as two intervals, one execution is lost.
- The interval replaces any earlier specified intervals.
- AdjustClock and @CLADJ do not affect the interval.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of an RT description. 0 means calling program. GetRtAddress gives RT des |
| `Param2` | UNKNOWN | I | Period between executions in basic time units. |

#### See Also

StartupInterval, @DINTV. StartupInterval allows you to specify intervals in seconds, minutes or hours

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, BasicTimeUnits : INTEGER2;
...
ExactInterval(RTProgram, BasicTimeUnits);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 BasicTimeUnits COMP.
...
MONITOR-CALL "ExactInterval" USING RTProgram, BasicTimeUnits.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, BasicTimeUnits
...
Monitor_Call('ExactInterval', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, BasicTimeUnits
...
Monitor_Call('ExactInterval', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
BasicTimeUnits : W BLOCK 1
ExactInterval : EQU 37B9 + 130B
...
CALLG ExactInterval, 2, RTProgram, BasicTimeUnits
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA  (PAR      %Load register A with address of parameter list.
MON  130       %Monitor call ExactInterval.
...
PAR, RTPRO    %Address of RT description.
TIME          %Time of interval between each execution.
...
RTPRO, ...
TIME, ...      %A double word giving number of basic time units.
...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 177*

[↑ Back to Top](#table-of-contents)

---

### 131B - DataTransfer (ABSTR)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Transfers data between physical memory and a mass-storage device, e.g. a disk or magnetic tape. You may perform various device control functions. This monitor call is mainly used by the operating system itself. For more details, refer to the ND-100 SCSI Reference Guide, ND-12.048.

- With SINTRAN III/VSX, the monitor call and the parameters must reside on a fixed segment on protection ring 2. With SINTRAN III/VSE, the monitor call and parameters must reside in resident memory.
- The physical memory area must be contiguous. Older versions of magnetic tapes or disk controllers cannot cross physical memory bank boundaries of 128 Kbytes. These magnetic tapes have ND numbers less than ND-537. The disks have ND numbers less than ND-559.
- If you write code to be independent of whether it is run on SINTRAN III VSE or VSX, you are advised to use TransferData (EXABS, mon 335).

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Function code. See the tables on the following pages. |
| `Param3` | UNKNOWN | I | Physical memory address. |
| `Param4` | UNKNOWN | I | Block address on the disk. See the tables on the following pages. |
| `Param5` | UNKNOWN | I | Number of blocks to transfer. |
| `Param6` | UNKNOWN | I | Error code. Negative if error. Contains a hardware status. |

#### See Also

TransferData, [ReadFromFile](#117b-readfromfile-rfile), WriteToFile. TransferData allows you to use any page table

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, Func, BlockAddr, NoOfBlocks, Stat : INTEGER2;
MemoryAddr : LONGINT;
...
DataTransfer(DeviceNo, Func, MemoryAddr, BlockAddr, NoOfBlocks, Stat);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 Func COMP.
01 BlockAddr COMP.
01 NoOfBlocks COMP.
01 Stat COMP.
01 MemoryAddr COMP PIC S9(10).
...
MONITOR-CALL "DataTransfer" USING DeviceNo, Func, MemoryAddr, BlockAddr, NoOfBlocks, Stat.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, Func, BlockAddr, NoOfBlocks, Stat
INTEGER*4 MemoryAddr
...
Monitor_Call('DataTransfer', DeviceNo, Func, MemoryAddr, BlockAddr, NoOfBlocks, Stat)



Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, Func, BlockAddr, NoOfBlocks, Stat
INTEGER4 : MemoryAddr

Monitor_Call('DataTransfer', DeviceNo, Func, MemoryAddr, & BlockAddr, NoOfBlocks, Stat)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
Func : W BLOCK 1
MemoryAddr : W BLOCK 1  %ND-100 memory address.
BlockAddr : W BLOCK 1
NoOfBlocks : W BLOCK 1
Stat : W BLOCK 1
DataTransfer : EQU 37B9 + 131B

CALLG DataTransfer, 5, DeviceNo, Func, MemoryAddr, &
BlockAddr, NoOfBlocks

IF K GO Error

Error, W1 =: Stat
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Code  | Description                                  |
|-------|----------------------------------------------|
| LDT   | DEVNO %Logical device number.                |
| LDA   | (PAR %Load register A with address of parameter list. |
| MON   | 131 %Monitor call DataTransfer.              |
| JAN   | ERROR %Error if register A is negative.      |

%Continue with processing.
%Error number in register A.

| Variable | Description                           |
|----------|---------------------------------------|
| DEVNO    | ...                                   |
| PAR      | FUNC %Function code etc.              |
| DMEM     | %Memory address.                      |
| BLOCK    | %Block address.                       |
| NOBLK    | %Number of blocks to transfer.        |

% ... indicates continuation of descriptions, more variables, or other details.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 127*

[↑ Back to Top](#table-of-contents)

---

### 132B - JumpToSegment (MCALL)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Calls a routine on another segment in the ND-100. You can divide an ND-100 RT program between various segments. This monitor call switches one or both of the current segments according to what you specify in parameter 2. ChangeSegment may be used instead of JumpToSegment and ExitFromSegment. The segment numbers are restricted to 8-bits (values 0-255). Use SegmentFunction (MON 341) with version K of SINTRAN III.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of routine. |
| `Param2` | UNKNOWN | I | New segments. The most significant byte identifies the first segment. The least  |

#### See Also

[ExitFromSegment](#133b-exitfromsegment-mexit), [ChangeSegment](#337b-changesegment-spchg), [SegmentFunction](#341b-segmentfunction-sgmty)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SubroutineAddr, NewSegment : INTEGER2;
...
JumpToSegment(SubroutineAddr, NewSegment);
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDT (PAR      | %Load register T with address of parameter list.    |
|---------------|-----------------------------------------------------|
| MON 132       | %Monitor call JumpToSegment.                        |
| ...           | %Return after ExitFromSegment.                      |
| PAR, SUBR     | %Address of subroutine.                             |
| SEG           | %New segment to be loaded.                          |
| ...           |                                                     |
| SUBR, ...     |                                                     |
| SEG, 10030    | %New segments are 20B and 30B in this example.      |
|               | %ExitFromProgram shows the code on segment 30.      |


| ND-100  | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 329*

[↑ Back to Top](#table-of-contents)

---

### 133B - ExitFromSegment (MEXIT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Exchanges one or both current segments. Commonly used to return after the monitor call JumpToSegment. ChangeSegment may be used instead of JumpToSegment and ExitFromSegment. The segment numbers are restricted to 8-bits (values 0-255). Use SegmentFunction (MON 341) with version K of SINTRAN III.

#### See Also

[JumpToSegment](#132b-jumptosegment-mcall), [ChangeSegment](#337b-changesegment-spchg), [SegmentFunction](#341b-segmentfunction-sgmty)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
...
ExitFromSegment(SegmentNumber);
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Command | Description |
|---------|-------------|
| ...     | %See JumpToSegment. |
| ...     | %This code is on segment 30B. |
| SUBR, STT SEGNO | %Entry point after JumpToSegment. |
| COPY SL DT | %Save T and L registers. |
| STT RETUR | %L register contains return address. |
| ...     | %Any processing on the segment. |
| ...     | |
| LDT RETUR | %Restore return address in L register. |
| COPY ST DL | |
| LDT SEGNO | %Restore calling segment number. |
| MON 133 | %Monitor call ExitFromSegment. |
| SEGNO, ... | %Segment number. |
| RETUR, ... | %Return address. |

ND-100 | User RT and user SYSTEM | RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 187*

[↑ Back to Top](#table-of-contents)

---

### 134B - ExitRTProgram (RTEXT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Terminates the calling RT or background program. Releases all reserved resources. The monitor call has the same effect as exit for interactive background programs.

- Batch jobs are aborted.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Pascal` | UNKNOWN | I |  |
| `ExitRTProgram;` | UNKNOWN | I |  |
| `COBOL` | UNKNOWN | I |  |
| `MONITOR-CALL "ExitRTProgram".` | UNKNOWN | I |  |
| `FORTRAN` | UNKNOWN | I |  |
| `Monitor_Call('ExitRTProgram')` | UNKNOWN | I |  |

#### See Also

[ExitFromProgram](#0b-exitfromprogram-leave), [StopRTProgram](#105b-stoprtprogram-abort)

#### Examples

<details>
<summary><strong>PLANC</strong></summary>

```planc
Monitor_Call('ExitRTProgram')
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ExitRTProgram: EQU 37B9 + 134B
    ...
    CALLG ExitRTProgram, 0
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON     134     %Monitor call ExitRTProgram.


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 189*

[↑ Back to Top](#table-of-contents)

---

### 135B - WAITFORRESTART (RTWT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the RT program in a waiting state. It is restarted by StartRTProgram or @RT. Execution continues after the monitor call.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 136B - EnableRTStart (RTON)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

RTON RT programs cannot be started after DisableRTStart has been executed. Use EnableRTStart to do this.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. 0 means calling program. |

#### See Also

[DisableRTStart](#137b-disablertstart-rtoff), and @RTON

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram : INTEGER2;
...
EnableRTStart(RTProgram);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram  COMP.
...
MONITOR-CALL "EnableRTStart" USING RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram
...
Monitor_Call('EnableRTStart', RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram
    ...
    Monitor_Call('EnableRTStart', RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
    EnableRTStart : EQU 37B9 + 136B
    ...
    CALLG EnableRTStart, 1:, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
    MON 136 %Monitor call EnableRTStart.
    ...
    PAR, RTPRO %Address of RT description.
    ...
    RTPRO, ...

| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 169*

[↑ Back to Top](#table-of-contents)

---

### 137B - DisableRTStart (RTOFF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Disables start of RT programs. No RT program can be started before EnableRTStart is executed.

- RT programs in the time queue will not start. Other active RT programs are not affected.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Address of the RT description. 0 means calling program. |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[EnableRTStart](#136b-enablertstart-rton), and @RTOFF

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram : INTEGER2;

    DisableRTStart(RTProgram);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.

    MONITOR-CALL "DisableRTStart" USING RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram

    Monitor_Call('DisableRTStart', RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram
...
Monitor_Call('DisableRTStart', RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
DisableRTStart : EQU 37B9 + 137B
...
CALLG DisableRTStart, 1, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 137 %Monitor call DisableRTStart.
...
PAR, RTPRO %Address of RT description.
...
RTPRO, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
|-------------------|-------------------------|-------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 157*

[↑ Back to Top](#table-of-contents)

---

### 140B - ReservationInfo (WHDEV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Checks that a device is not reserved. If it is reserved, you will receive information about which RT program that reserves it.

- Some devices have both an input and an output part. You have to use ReservationInfo for each part.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. |
| `Param2` | UNKNOWN | I | Input or output flag. 0 means the input part. 1 means the output part. |
| `Param3` | UNKNOWN | I | RT description address of reserving RT program. 0 means not reserved. Errors in  |

#### See Also

[ReserveResource](#122b-reserveresource-resrv), [ReleaseResource](#123b-releaseresource-reles), and @LIST-DEVICE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, IOFlag, ReturnValue : INTEGER2;
...
ReservationInfo(DeviceNumber, IOFlag, ReturnValue);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 IOFlag COMP.
01 ReturnValue COMP.
...
MONITOR-CALL "ReservationInfo" USING DeviceNumber, IOFlag, ReturnValue.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, IOFlag, ReturnValue
...
Monitor_Call('ReservationInfo', DeviceNo, IOFlag, ReturnValue)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, IOFlag, ReturnValue
...
Monitor_Call('ReservationInfo', DeviceNumber, IOFlag, ReturnValue)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
IOFlag : W BLOCK 1
ReturnValue : W BLOCK 1
ReservationInfo : EQU 37B9 + 140B
...
CALLG ReservationInfo, 2, DeviceNumber, IOFlag
W1 =: ReturnValue %Result is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA   (PAR   %Load register A with address of parameter list.
MON   140    %Monitor call ReservationInfo.
STA   STAT   %Store status returned.
AAA   -1     %Test if -1. JAN not applicable because of RT
JAZ   ERROR  %addresses above 100000.
...
...
ERROR, ...
STAT, 0      %RT description address if any.
PAR, DEVNO  %Logical device number.
IOF         %Input or output flag.
DEVNO, ...
IOF, ...


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 415*

[↑ Back to Top](#table-of-contents)

---

### 141B - DeviceControl (IOSET)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets control information for a character device, e.g. a terminal or a printer. The control information depends on the device.

- The device must be reserved. See ReserveResource.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. You cannot use 1 for your own terminal. U |
| `Param2` | UNKNOWN | I | Input or output part of the device. Use 0 for input and 1 for output. |
| `Param3` | UNKNOWN | I | Address of RT description of reserving program. Use 0 for the calling program. |
| `Param4` | UNKNOWN | I | Control flag. Use -2 to empty the TAD output buffer. Use -1 to reset the device. |
| `Param5` | UNKNOWN | I | Return status. 0 means no errors. An illegal RT description address returns -1. |

#### See Also

[DeviceFunction](#144b-devicefunction-magtp), @IOSET

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, IOFlag, RTProgram, CtrlFlag, Status : INTEGER2;
...
DeviceControl(DeviceNo, IOFlag, RTProgram, CtrlFlag, Status);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo       COMP.
01 IOFlag      COMP.
01 RTProgram   COMP.
01 CtrlFlag    COMP.
01 ReturnStatus COMP.
...
MONITOR-CALL "DeviceControl" USING DevNo, IOFlag, RTProgram, CtrlFlag,
                                 ReturnStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, IOFlag, RTProgram, CtrlFlag, ReturnStatus
...
Monitor_Call('DeviceControl', DeviceNo, IOFlag, RTProgram,
             CtrlFlag, ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, IOFlag, RTProgram, CtrlFlag, ReturnStatus
...
Monitor_Call('DeviceControl', DeviceNo, IOFlag, RTProgram, CtrlFlag, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
IOFlag : W BLOCK 1
RTProgram : W BLOCK 1
CtrlFlag : W BLOCK 1
ReturnStatus : W BLOCK 1
DeviceControl : EQU 37B9 + 141B
...
CALLG DeviceControl, 4, DeviceNo, IOFlag, RTProgram, CtrlFlag
W1 =: ReturnStatus %Status is returned in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 141 %Monitor call DeviceControl.
JAN ERROR %Error if register A is negative.
...
ERROR, ... %Handle the error.
...
PAR, DEVNO %Logical device number.
IOF %Input or Output flag.
PROG %RealTime (program) description.
CTRL %Control flag.
...
DEVNO, ...
IOF, ...
PROG, ...
CTRL, ...

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 141*

[↑ Back to Top](#table-of-contents)

---

### 142B - ToErrorDevice (ERMON)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Outputs a user-defined, real-time error. The error message is output on the error device, i.e. normally the console. The following is an example of such a message: 23.10.59 ERROR 59 AT XPROG AT 134562, USER ERROR, SUBERROR 4. See appendix A.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Error number. You may use error numbers 50 to 69. This number is output followin |
| `Param2` | UNKNOWN | I | Suberror number. |

#### See Also

[WarningMessage](#64b-warningmessage-ermsg), [ErrorMessage](#65b-errormessage-qerms), and OutMessage

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrorNumber, SubErrorNumber : INTEGER2;
...
ToErrorDevice(ErrorNumber, SubErrorNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrorNumber COMP.
01 SubErrorNumber COMP.
...
MONITOR-CALL "ToErrorDevice" USING ErrorNumber, SubErrorNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrorNumber, SubErrorNumber
...
Monitor_Call('ToErrorDevice', ErrorNumber, SubErrorNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrorNumber, SubErrorNumber
...
Monitor_Call('ToErrorDevice', ErrorNumber, SubErrorNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrorNumber : W BLOCK  1                      %In ASCII characters.
SubErrorNumber : W BLOCK  1

ToErrorDevice : EQU 378B9 + 142B
...
CALLG ToErrorDevice, 2, ErrorNumber, SubErrorNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA     ERRNO            %Error number.
LDT     SUBER            %Sub-error number.
MON     142              %Monitor call ToErrorDevice.
...

ERRNO, ...                %Error number as two ASCII characters.
SUBER, ...                %Suberror number as two ASCII characters.


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 543*

[↑ Back to Top](#table-of-contents)

---

### 143B - ExecutionInfo (RSIO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about the execution of the calling program. You are told whether the program executes interactively, as a batch or mode job, or as an RT program. The monitor call returns some additional information for non-RT programs, consisting of the command input file, the command output file, and the directory index and user index of the program's owner.

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ExecutionMode, InputDev, OutputDev, UserIndex : INTEGER2;
...
ExecutionInfo(ExecutionMode, InputDev, OutputDev, UserIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ExecutionMode COMP.
01 InputDev COMP.
01 OutputDev COMP.
01 UserIndex COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ExecutionInfo" USING ExecutionMode, InputDev,
              OutputDev, UserIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ExecutionMode, InputDev, OutputDev, UserIndex
Monitor_Call('ExecutionInfo', ExecutionMode, InputDev,
             OutputDev, UserIndex)
C IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ExecutMode  : W BLOCK 1
InputDev    : W BLOCK 1
OutputDev   : W BLOCK 1
UserIndex   : W BLOCK 1
ExecutionInfo : EQU 37B9 + 143B
...
    CALLG ExecutionInfo, 4, ExecutMode, InputDev, OutputDev, UserIndex
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| MON  | 143  | %Monitor call ExecutionInfo.                    |
|------|------|-----------------------------------------------|
| STA  | EXMOD| %Store execution mode.                        |
| STT  | IFILE| %Store file number of command input file.     |
| COPY | SD DA|                                               |
| STA  | OFILE| %Store file number of command output file.    |
| STX  | INDEX| %Store directory and user indexes.            |
| ...  |      |                                               |


EXMOD, 0
IFILE, 0
OFILE, 0
INDEX, 0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 183*

[↑ Back to Top](#table-of-contents)

---

### 144B - DeviceFunction (MAGTP)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Performs various operations on floppy disks, magnetic tapes, Versatec plotters, and SCSI streamers.

- The parameter values depend on the device.
- If the function code is in the range 5B to 24B, except 23B, the parameter buffer and the two device dependent parameters are dummies.
- If the function code is in the range 20B to 24B, except 23B, the hardware status is returned.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Function code. See the following pages. |
| `Param2` | UNKNOWN | I | Buffer used for data transfer to and from the device. |
| `Param3` | UNKNOWN | I | Logical device number (or open-file number). See appendix B. |
| `Param4` | UNKNOWN | I | First device dependent parameter. See the following pages. |
| `Param5` | UNKNOWN | I | Second device dependent parameter. See the following pages. |
| `Param6` | UNKNOWN | I | Device dependent status code. See the following pages. |

#### See Also

[DeviceControl](#141b-devicecontrol-ioset), @DEVICE-FUNCTION

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DevNo, Func, Param1, Param2 : INTEGER2;
Buff : ARRAY [0..63] OF RECORD...END;
...
DeviceFunction(Func, Buff, DevNo, Param1, Param2);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo COMP.
01 Func COMP.
01 Param1 COMP.
01 Param2 COMP.
01 Buff.
   02 array COMP OCCURS 1024 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "DeviceFunction" USING Func, Buff, DevNo, Param1, Param2.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DevNo, Func, Param1, Param2
INTEGER Buff(1024)
...
Monitor Call('DeviceFunction', Func, Buff(1), DevNo, Param1, Param2)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DevNo, Func, Param1, Param2
BYTES : Buff(0:2047)
...
ON ROUTINEERROR DO
   IF ErrCode >‹ 0 THEN ...
ENDON
Monitor_Call(‘DeviceFunction‘, Func, Buff(0), DevNo, Param1, Param2)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DevNo : W BLOCK 1
Func : W BLOCK 1
Param1 : W BLOCK 1
Param2 : W BLOCK 1
Buff : W BLOCK 1024
ErrCode : W BLOCK 1
DeviceFunction : EQU 3789 + 144B
...
CALLG DeviceFunction, 5, Func, Buff, DevNo, Param1, Param2
IF K GO ERROR
...
ERROR : W1 =: ErrCode                      %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR                             %Load register A with address of parameter list.
MON 144                              %Monitor call DeviceFunction.
STA STAT                             %Store status returned.
...
STAT, 0
PAR, FUNC                            %Function to be performed.
BUFF                                 %Address of buffer used for data transfer.
DEVNO                                %Logical device number.
PARA1                                %Device dependent parameter.
PARA2                                %Device dependent parameter.
...

FUNC, ...
BUFF, 0
**+120/                              %Make a buffer of 80 words.
DEVNO, ...
PARA1, ...
PARA2, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 143*

[↑ Back to Top](#table-of-contents)

---

### 146B - PrivInstruction (IPRIV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Executes a privileged machine instruction on the ND-100. Privileged instructions may, for example, turn the paging and interrupt mechanisms on and off.

- The instruction uses the register contents of the calling program. The registers may be changed.

#### See Also

[IOInstruction](#31b-ioinstruction-exiox)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Instruction : INTEGER2;
...
PrivInstruction(Instruction);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Instruction COMP.
...
MONITOR-CALL "PrivInstruction" USING Instruction.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Instruction
...
Monitor_Call('PrivInstruction', Instruction)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Instruction
...
Monitor_Call('PrivInstruction', Instruction)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
TReg : W BLOCK 1
AReg : W BLOCK 1
DReg : W BLOCK 1
XReg : W BLOCK 1
ErrCode : W BLOCK 1
PrivInstruction : EQU 37B9 + 146B
...
CALLG PrivInstruction, 4, TReg, AReg, DReg, XReg
IF K GO Error
...
Error, W1 =: ErrCode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDT  | INSTR  | %Instruction to be executed.                   |
|------|--------|------------------------------------------------|
| MON  | 146    | %Monitor call PrivInstruction.                 |
| ...  |        |                                                |
| INSTR, | 150404 | %Turn off memory management system. Dangerous! |


ND-100 and ND-500   User RT and user SYSTEM   RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 391*

[↑ Back to Top](#table-of-contents)

---

### 147B - CAMACFunction (CAMAC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Operates the CAMAC, i.e. executes the NAF register. CAMAC is a standardized way to connect peripheral equipment to a computer. NAF data looks like this: ``` 15   13     9 8     5 4    0 ----------------------------- |  Q  X  | STATION | SUBADDRESS | FUNCTION | ----------------------------- ``` If Q=1 and/or X=1, then Q and X responses from the station are automatically checked. A Control/Status register (COST register) holds some extra information. If the Function parameter is 3 (clear), the contents of COST are returned in the Value parameter. If Function=0 (read), data is returned in Value. For both these functions, parameter 2 must be ≥0 on entry. If Function=1 (write) parameter 2 must be <0 before the call. If bit 15 in Crate Number is set to 1, Value is treated as an integer instead of floating point.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 150B - CAMACGLRegister (GL)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Read the CAMAC GL (Graded LAM -"look at me") register or the last CAMAC identification number. See under CAMACfunction (mon 147) for more general information. When a program requests interrupt, it generates a LAM to the control station. Up to 23 such signals may arrive, but for efficient handling in the computer, these must be reduced to 16 lines called Graded LAMs. The GL register, which may be read by the program, holds a bit mask giving the current LAM requests (bit=1 indicates LAM). The different graded LAM lines have individual IDENT codes (Bits 0-3: Graded LAM code. Bits 4-7: crate code. Bit 8=1 : CAMAC). A LAM Mask Register is used to enable (bit=1) or disable (bit=0) any of the 16 graded LAMs. Each time an IDENT code is read, the associated bit in the MASK register is cleared and must be set by the program to enable a new interrupt.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 151B - GetRTAddress (GRTDA)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the address of an RT description. You specify the name of the RT program. See the SINTRAN III Real Time Guide (ND-860133) for further information.

- In SINTRAN III, version J and older, this monitor call is only available to RT programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `RT-program name.` | UNKNOWN | I | The name must be given in capital letters. It must end with an apostrophe if less than 7 characters long (applies to high-level languages). When using a high-level language together with NPL or MAC, it is advisable to use an RT-program name of not more than 5 characters. |
| `RT program address.` | UNKNOWN | O | Non-existent RT-program names return -1. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[GetRTDescr](#27b-getrtdescr-rtdsc), [GetRTName](#152b-getrtname-grtna), and @LIST-RT-PROGRAM

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgramName: PACKED ARRAY [0..6] OF CHAR;
RTProgram : INTEGER2;
...
GetRTAddress (RTProgramName, RTProgram);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgramName PIC X(7).
01 RTProgram COMP.
...
MONITOR-CALL "GetRTAddress" USING RTProgramName, RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER RTProgramName*7
INTEGER RTProgram
...
Monitor_Call('GetRTAddress', RTProgramName(1:7), RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : RTProgramName(0:6)
INTEGER : RTProgram
...
Monitor_Call('GetRTAddress', RTProgramName, RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgramName : STRINGDATA 'SIB2A'''
RTProgram : W BLOCK 1
GetRTAddress : EQU 37B9 + 151B
...
%Get address of process SIB2A.
CALLG GetRTAddress, 1, RTProgramName
    IF K GO Error
...
W1 := RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR      %Load register A with address of parameter list.
MON 151       %Monitor call GetRTAddress.
STA RTPRO     %Store address of RT description.
...
RTPRO, 0
...
PAR, RTNAM    %String containing name of RT program.
...
RTNAM, 'SIB2A' %Get address of RT description of SIB2A.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 152B - GetRTName (GRTNA)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the name of an RT program. You specify the RT description address.

- This monitor call is only available to background programs in SINTRAN III VSX, version K.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The address of the RT description. Use 0 for the calling program. |
| `Param2` | UNKNOWN | I | Name of the RT program. The name is returned with a terminal apostrophe if less  |

#### See Also

[GetOwnRTAddress](#30b-getownrtaddress-getrt), GetRTAddress, [GetRTDescr](#27b-getrtdescr-rtdsc), @GET-RT-NAME, and @LIST-RT-Programs

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram : INTEGER2;
RTProgramName : PACKED ARRAY [0..6] OF CHAR;
...
GetRTName(RTProgram, RTProgramName);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 RTProgramName PIC X(7).
...
MONITOR-CALL "GetRTName" USING RTProgram, RTProgramName.
IF RTProgramName = "'"
...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram
CHARACTER RTProgramName*6
...
Monitor_Call1('GetRTName', RTProgram, RTProgramName(1:7))
IF RTProgramName(1) = '
...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram
BYTES : RTProgramName(0:6)
...
Monitor_Call('GetRTName', RTProgram, RTProgramName)
IF RTProgramName(0)=#1 THEN
...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
RTProgramName : BY BLOCK 7
GetRTName : EQU 37B9 + 152B
...
CALLG GetRTName, 2, RTProgram, RTProgramName
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR  %Load register A with address of parameter list.
MON 152   %Monitor call GetRTName.
SKP IF DD UEQ 0
JMP ERR
STT RTNAM   %Store 1st word of packed RT name.
STD RTNAM+1 %Store 2nd two words of packed RT name.
...
ERR, ...
PAR, RTPRO %Address of RT description.
...
RTPRO, ...
RTNAM, 0
*+4/      %Make a string of 7 bytes.


| ND-100 and ND-500 | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 273*

[↑ Back to Top](#table-of-contents)

---

### 153B - CAMACIOInstruction (IOXIN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Executes a single IOX instruction for CAMAC. See under CAMACfunction (mon 147) for general information.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 154B - AssignCAMACLAM (ASSIG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Assigns a graded LAM in the CAMAC identification table to a logical device number in the logical number table. See CAMACFunction and CAMACGLRegister for more details.

- Remove the LAM by specifying logical device number -1.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 155B - GRAPHICFUNCTION (GRAPH)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Executes various functions on a graphic peripheral, such as a NORDCOM terminal, a pen plotter, or a Textronix display.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 157B - SegmentToPageTable (ENTSG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Enters a routine as a direct task or as a device driver, and "remembers" which segments have been entered (up to 24 segments). These are reentered at restart following a power failure. The routines are connected to the interrupt system. They are loaded with the RT LOADER or by DMAC.

- The segment where the routine resides must be fixed in memory. See FixScattered.
- The segments can be removed from the selected PIT by using function 4 of ChangeActiveSegments (MON 341).
- Unfixing the segment (using UnFixSegment, MON 116), removes it from the Page Index Table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number where the routine resides. |
| `Param2` | UNKNOWN | I | Page table to use for the segment. In practice, this should be 3. For SINTRAN II |
| `Param3` | UNKNOWN | I | The interrupt level where the direct task should run. You must specify one of th |
| `Param4` | UNKNOWN | I | Start address of the routine (entry point). |
| `Param5` | UNKNOWN | I | Error code in A-register = 0 : OK |
| `Code` | Explanation | I |  |
| `-1` | attempt to enter too many segments | I |  |
| `-2` | illegal segment number | I |  |
| `-3` | illegal Page Index Table | I |  |
| `-4` | segment is not fixed | I |  |
| `-5` | illegal interrupt level | I |  |
| `-6` | PIT already in use | I |  |

#### See Also

@ENTSEG

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNo, PageTable, InterruptLevel, StartAddr : INTEGER2;
... [Note routine name.]
EnterSegment(SegmentNo, PageTable, InterruptLevel);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNo COMP.
01 PageTable COMP.
01 InterruptLevel COMP.
01 StartAddress COMP.
...
MONITOR-CALL "SegmentToPageTable" USING SegmentNo, PageTable,
             InterruptLevel, StartAddress.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNo, PageTable
INTEGER InterLevel, StartAddress
...
Monitor_Call('SegmentToPageTable', SegmentNo, PageTable,
C            InterLevel, StartAddress)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegNo, PageTable, InterruptLevel, StartAddress
...
Monitor_Call('SegmentToPageTable', SegNo, PageTable, &
InterruptLevel, StartAddress)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegNo : W BLOCK 1
PageTable : W BLOCK 1
InterLevel : W BLOCK 1
StartAddr : W BLOCK 1
SegmentToPageTable : EQU 37B9 + 157B
...
CALLG SegmentToPageTable, 4, SegNo, PageTable, InterLevel, StartAddr
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 157 %Monitor call SegmentToPageTable.
...
PAR, SEG %The segment where the routine resides.
PAGE %Page table to be used.
INTR %Interrupt level where the direct task will run.
ENTRY %Entry point, start address of direct task.
...
SEG, ...
PAGE, ...
INTR, ...
ENTRY, ...


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 431*

[↑ Back to Top](#table-of-contents)

---

### 160B - FixContiguous (FIXC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Places a segment in physical memory. Its pages will no longer be swapped to the disk. The segment is placed in a contiguous area of physical memory. This function is useful for time-critical operations.

- The segment must be created with the RT LOADER as a non-demand segment.
- Use UnfixSegment or @UNFIX to allow the RT LOADER to clear the segment.
- Your program normally terminates if you refer to a nonexistent segment or a demand segment. An error message is output on the error device. See parameters 1 and 3 to get a status value instead.
- Only a limited number of pages can be fixed. This limit is defined when SINTRAN III is generated. You may change it with the command CHANGE-VARIABLE in the SINTRAN-SERVICE-PROGRAM.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number to be fixed. Set bit 15 to 1 if you want a return status. |
| `Param2` | UNKNOWN | I | First physical page number to be used. |
| `Param3` | UNKNOWN | I | Return status if bit 15 of the segment number is set. Dummy on the ND-500. 0 mea |

#### See Also

[UnfixSegment](#116b-unfixsegment-unfix), [FixScattered](#115b-fixscattered-fix), FixInMemory, [MemoryAllocation](#61b-memoryallocation-fixcs), [FixIOArea](#404b-fixioarea-iofix), and @FIXC

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNo, PageNumber, Stat : INTEGER2;
...
FixContiguous(SegmentNo, PageNumber, Stat);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNo COMP.
01 PageNumber COMP.
01 Stat COMP.
...
MONITOR-CALL "FixContiguous" USING SegmentNo, PageNumber, Stat.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNo, PageNumber, Stat
...
Monitor_Call('FixContiguous', SegmentNo, PageNumber, Stat)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNo, PageNumber, Stat
...
Monitor_Call('FixContiguous', SegmentNo, PageNumber, Stat)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNo : W BLOCK 1
PageNumber : W BLOCK 1
FixContiguous : EQU 37B9 + 160B
...
CALLG FixContiguous, 2, SegmentNo, PageNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR   %Load register A with address of parameter list.
MON 160    %Monitor call FixContiguous.
STA STAT   %Returned status.
...
STAT, 0
PAR, SEGNO %Segment number to be fixed.
PAGE       %First physical page number.
...
SEGNO, ...
PAGE, ...



| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 201*

[↑ Back to Top](#table-of-contents)

---

### 161B - INSTRING (INSTR)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads a string of characters from a peripheral device, e.g. a terminal.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `DeviceNo` | Logical device number of a peripheral device. See appendix B. | I |  |
| `TextRead` | String of characters. | I |  |
| `NoOfBytes` | Maximum number of characters to be read. | I |  |
| `Terminator` | Terminating character. Input stops when this character is read. | I |  |
| `ReturnStatus` | A 16-bit integer status. Errors in parameters return -1. If bit 15:14 is 0, the maximum number of characters is read. If 1, a termination character is read. 2 means not terminated. This applies only to RT program. 3 means device error. Bit 7:0 contains the error number. If bit 15:14 are 0, 1 or 2, the number of characters read is returned in bit 13:0. | I |  |

#### See Also

[InputString](#503b-inputstring-dvinst), [In8AndFlag](#310b-in8andflag-tbin8), [InUpTo8Bytes](#21b-inupto8bytes-m8inb), [In8Bytes](#23b-in8bytes-b8inb), [In4x2Bytes](#63b-in4x2bytes-b41nw), and OutString

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
pascal
DeviceNo, NoOfBytes, Terminator, ReturnStatus : INTEGER2;
TextRead : PACKED ARRAY [0..255] OF CHAR;
...
InString(DeviceNo, TextRead, NoOfBytes, Terminator, ReturnStatus);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 TextRead PIC X(256).
01 NoOfBytes COMP.
01 Terminator COMP.
01 ReturnStatus COMP.
...
MONITOR-CALL "InString" USING DeviceNo, TextRead, NoOfBytes,
                          Terminator, ReturnStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
fortran
INTEGER DeviceNo, NoOfBytes, Terminator, ReturnStatus
CHARACTER TextRead*256
...
Monitor_Call('InString', DeviceNo, TextRead(1:256), NoOfBytes,
 C                       Terminator, ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, NoOfBytes, Terminator, ReturnStatus
BYTES : TextRead(0:255)

Monitor_Call(‘InString’. DeviceNo, TextRead, NoOfBytes, &
Terminator, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| DeviceNo   | : W BLOCK 1     |
|------------|-----------------|
| TextRead   | : STRING 4000B  |
| NoOfBytes  | : W BLOCK 1     |
| Terminator | : W BLOCK 1     |
| ReturnStatus | : W BLOCK 1   |
| InString   | : EQU 37B9 + 161B |

CALLG InString, 4, DeviceNo, TextRead, NoOfBytes, Terminator
IF K GO Error
W1 := ReturnStatus %Status is returned in W1 register.

Error.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR  %Load register A with address of parameter list.
MON 161  %Monitor call InString.
STA STAT %Store status returned.

| STAT. 0 |                       |
|---------|-----------------------|
| PAR,    | DEVNO %Logical device number |
| TEXT    | %String read.         |
| COUNT   | %Maximum number of characters to be read. |
| TERM    | %Terminator character. |

DEVNO,
TEXT, 0 \
**+50/ %Make a string of 80 characters.
COUNT, 120 %To read maximum 80 characters.
TERM, @' %Terminate reading when @ is read.


ND-100 and ND-500 All users All programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 323*

[↑ Back to Top](#table-of-contents)

---

### 162B - OutString (OUTST)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Writes a string of characters to a peripheral file, e.g., a terminal or a printer.

- You cannot use this monitor call for mass-storage files.
- The output buffer of the device may be too small. Then the program waits until the required buffer space becomes available.
- Parameters are fetched and returned through the alternative page table.
- The maximum string length is 2048 bytes (as in OutputString).
- For performance reasons, it is inadvisable to use this call from the ND-500(0). Use OutputString instead.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical device number. See appendix B. You cannot use 1 for your own terminal. U |
| `Param2` | UNKNOWN | I | Character string to be output. |
| `Param3` | UNKNOWN | I | Number of characters to be output. |
| `Param4` | UNKNOWN | I | Return status: |

#### See Also

[OutMessage](#32b-outmessage-msg), [OutUpTo8Bytes](#22b-outupto8bytes-m8out), [Out8Bytes](#24b-out8bytes-b8out), [OutputString](#504b-outputstring-dvouts), [OutNumber](#35b-outnumber-iout), [OutByte](#2b-outbyte-outbt), and InString

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, NoOfBytes, ReturnStatus : INTEGER2;
TextWrite : PACKED ARRAY [0..79] OF CHAR;
...
OutString(DeviceNo, TextWrite, NoOfBytes, ReturnStatus);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo COMP.
01 NoOfBytes COMP.
01 RetStatus COMP.
01 TextWrite PIC X(100).
...
MONITOR-CALL "OutString" USING DevNo, TextWrite, NoOfBytes, RetStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, NoOfBytes, RetStatus
CHARACTER TextWrite*80
...
Monitor_Call('OutString', DeviceNo, TextWrite(1:80), NoOfBytes, RetStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, NoOfBytes, ReturnStatus
BYTES : TextWrite(0:79)
...
Monitor_Call('OutString', DeviceNo, TextWrite, NoOfBytes, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
TextWrite : STRINGDATA 'This is a text...'
ReturnStatus : W BLOCK 1
OutString : EQU 37B9 + 162B
...
CALLG OutString, 2, DeviceNo, TextWrite
IF K GO Error
W1 := ReturnStatus      % Status is returned in W1
...
Error, ...
register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LOA (PAR         % Load register A with address of parameter list.
MON 162          % Monitor call OutString.
STA STAT         % Store status returned.
...
STAT, 0
PAR, DEVNO       % Logical device number.
TEXT             % String to be written.
COUNT            % Number of characters to be written.
...

DEVNO, ...
TEXT, 'THIS IS A TEST'
COUNT, 16        % Write 14 characters.



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 385*

[↑ Back to Top](#table-of-contents)

---

### 164B - SaveSegment (WSEG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Saves a segment in the ND-100. All pages in physical memory which have been changed, are written back to the disk.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number. |

#### See Also

[SaveND500Segment](#416b-savend500segment-wsegn)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
...
SaveSegment(SegmentNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
...
MONITOR-CALL "SaveSegment" USING SegmentNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
...
Monitor_Call('SaveSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber
...
Monitor_Call('SaveSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNumber : W BLOCK 1
SaveSegment : EQU 3789 + 164B
...
CALLG SaveSegment, 1, SegmentNumber
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction |  | Comment |
|-------------|--|---------|
| LDA | {PAR | %Load register A with address of parameter list. |
| MON | 164 | %Monitor call SaveSegment. |
| ... | | |
| PAR, SEGNO | | %Segment number. |
| ... | | |
| SEGNO, ... | | |


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 423*

[↑ Back to Top](#table-of-contents)

---

### 165B - GETINREGISTERS (DIW)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads the device interface registers.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 167B - AttachSegment (REENT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Attaches a reentrant segment to your two current segments. The address areas of the segments may overlap. Pages in the reentrant segment may be accessed for reading, writing, or fetching instructions. When written to, a page loses its reentrancy. It is stored on one of your current overlapping segments.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 170B - UserDef0 (US0)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 171B - UserDef1 (US1)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 172B - UserDef2 (US2)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 173B - UserDef3 (US3)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 174B - UserDef4 (US4)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 175B - UserDef5 (US5)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 176B - UserDef6 (US6)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 177B - UserDef7 (US7)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

User-defined monitor call. You can implement up to 8 monitor calls yourself. These are named UserDef0, UserDef1,... UserDef7. The short names are US0, US1,...US7. To do this you need good knowledge of SINTRAN III. See the SINTRAN III listing parts one and two.

- Use the DMAC subsystem or the @LOOK-AT command to patch in the MAC instructions of your monitor call. Place the code in physical memory after the SINTRAN III symbol 7ENDC. You ought to do this from a mode file. The monitor call is lost in a warm start.
- Use the SINTRAN-SERVICE-PROGRAM command DEFINE-USER-MONITOR-CALL to define the entry point of the monitor call.
- There are other ways of implementing user-defined monitor calls.
- Some monitor calls are no longer in use. You may use these for user-defined monitor calls.
- If the subsystems TPS is installed, the user-defined monitor calls 170 to 175 may not be used.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 200B - XMSGFunction (XMSG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Performs various data communication functions. All types of programs may communicate through this monitor call. The programs may be in different computers in a network.

- The monitor call operates by sending and receiving messages.
- The COSMOS Programmer Guide (ND-860164) describes XMSGFunction.
- The COSMOS Programmer Guide (ND-860164) describes the communication facilities offered to high-level languages.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 201B - HDLCfunction (MHDLC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Performs various HDLC functions. A HDLC is a high-level data link to another computer. You may send data, receive data, and control HDLC. Data is sent as Driver Control Blocks (DCB). This monitor call is also used by X.21 to transfer DCBs between the user programs and the X.21 driver. The system uses the Logical Device Number to distinguish between HDLC and X.21.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Function code: Use 0 to send DCB to driver and 1 to receive DCB from driver |
| `Param2` | UNKNOWN | I | Logical device number: There are different logical device numbers for the input  |
| `Param3` | UNKNOWN | I | Address of driver control block: The first word in the DCB contains command info |
| `Param4` | UNKNOWN | I | Size of the used part of the driver control block in bytes. |
| `Param5` | UNKNOWN | I | Depends on the function code: maximum size of the driver control block in bytes  |
| `Param6` | UNKNOWN | I | HDLC error code: |
| `Param7` | UNKNOWN | I | 1: LDN not reserved by calling program. |
| `Param8` | UNKNOWN | I | 2: Illegal LDN. Not known by SINTRAN. |
| `Param9` | UNKNOWN | I | 3: No DCB in receiver queue. |
| `Param10` | UNKNOWN | I | 4: No vacant buffer for DCB. |
| `Param11` | UNKNOWN | I | 5: Illegal used DCB size (parameter 4). |
| `Param12` | UNKNOWN | I | 6: Illegal LDN. Not permitted from this monitor call. |
| `Param13` | UNKNOWN | I | 7: Max. DCB size (parameter 5) is less than used DCB size. |
| `Param14` | UNKNOWN | I | 10: Illegal function. |
| `Param15` | UNKNOWN | I | 11: Fatal error. The table is inconsistent. |

#### Examples

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SendRecDCB, LDN, Buffer, DCBusize, DCBmsize
INTEGER Buff(1024)
...
Monitor_Call('HDLCFunction', SendRecDCB, LDN, Buffer(1),
    C DCBusize, DCBmsizeXcoor


Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Func   : W BLOCK 1
DevNo  : W BLOCK 1
Buffer : W BLOCK 1024 %Even byte address.
USize  : W BLOCK 1
MSize  : W BLOCK 1
Status : W BLOCK 1
HDLCfunction : EQU 37B9 + 201B


CALLG HDLCfunction, 5, Func, DevNo, Buffer, USize, MSize,
IF K GO Error
W1 =: Status

Error,
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR      %Load address of parameter list into reg.A
MON 201       %Monitor call HDLCfunction.
JMP ERROR
JMP OK


ERROR, ...  %A-reg contains error code.
PAR, ...

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 307*

[↑ Back to Top](#table-of-contents)

---

### 206B - TerminationHandling (EDTMP)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Switches termination handling on and off.

- Termination handling for RT programs is either on or off. Background programs may have termination handling on or off for either user break or fatal errors.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 207B - GetErrorInfo (RERRP)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about the last real-time error. The monitor call returns the error, the RT program responsible for the error, and the program address where it occurred. A flag tells whether the RT program was aborted or not.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | A 12 byte error information. It has the meaning below. |
| `0:1` | Error number as two ASCII characters. Parity bits are not set. See appendix A and F for error codes together with an ASCII table. | I |  |
| `2:3` | Pointer to the user program address where the error occurred. | I |  |
| `4:5` | Additional error information. See appendix A. | I |  |
| `6:7` | Additional error information. See appendix A. | I |  |
| `8:9` | RT description address of the RT program causing the error. | I |  |
| `10:11` | Flag. 0 if SINTRAN III aborted the RT program. | I |  |
| `Param8` | UNKNOWN | I | Returned status. 0 means OK, 153B means illegal output buffer. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buffer : RECORD...END;
ReturnStatus : INTEGER2;
...
ReadErrorParam(Buffer, ReturnStatus); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buffer.
02 array COMP OCCURS 6 TIMES.
01 ReturnStatus COMP.
...
MONITOR-CALL "GetErrorInfo" USING Buffer, ReturnStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buffer(6)
INTEGER ReturnStatus
...
Monitor_Call('GetErrorInfo', Buffer(1), ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Buffer(0:11)
INTEGER : ReturnStatus

    Monitor_Call('GetErrorInfo', Buffer(0), ReturnStatus)
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDA   | (PAR)  | %Load register A with address of parameter list.   |
|-------|--------|---------------------------------------------------|
| MON   | 207    | %Monitor call GetErrorInfo.                       |
| JAF   | ERROR  | %Do error handling if register A is non-zero.     |

    ERROR, ...    %Error number in register A.

    PAR, BUFF     %Buffer for returned error information.

    BUFF, 0
    0
    0
    0
    0
    0


| ND-100 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 241*

[↑ Back to Top](#table-of-contents)

---

### 212B - ReentrantSegment (SREEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Connects a reentrant segment to your two current segments. All modified pages of your current segments are written to the segment file before the reentrant segment is fetched. This is almost equivalent to SaveSegment followed by AttachSegment. However, ReentrantSegment is more efficient. Only the modified pages overlapping the reentrant segment are written back.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number to attach. |

#### See Also

[AttachSegment](#167b-attachsegment-reent)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
...
ReentrantSegment(SegmentNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
...
MONITOR-CALL "ReentrantSegment" USING SegmentNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
...
Monitor_Call('ReentrantSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber
...
Monitor_Call('ReentrantSegment', SegmentNumber)
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Parameter | Description                             |
|-------------|-----------|-----------------------------------------|
| LDA         | (PAR      | %Load register A with address of parameter list. |
| MON         | 212       | %Monitor call ReentrantSegment.         |
| PAR,        | SEGNO     | %Segment number.                        |
| ...         | SEGNO,    | ...                                     |


| ND-100 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 407*

[↑ Back to Top](#table-of-contents)

---

### 213B - GetDirUserIndexes (MUIDI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets a directory index and a user index. You have to specify a directory name and a user name.

- Use ExecutionInfo to get the user index and the directory index of the user executing a program.

#### See Also

GetDirNameIndexes, GetUserIndex, and GetAllFileIndexes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
UserName : PACKED ARRAY [0..15] OF CHAR;
DirIndex, UserIndex : INTEGER2;
...
GetDirUserIndexes(UserName, DirIndex, UserIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01  UserName  PIC X(16).
01  DirIndex  COMP.
01  UserIndex COMP.
01  ErrCode   COMP.
...
MONITOR-CALL "GetDirUserIndexes" USING UserName, DirIndex, UserIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER  UserName*16
INTEGER    DirIndex, UserIndex
...
Monitor Call('GetDirUserIndexes',UserName(1:16),DirIndex,UserIndex)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : UserName(0:15)
INTEGER : DirIndex, UserIndex
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('GetDirUserIndexes', UserName, DirIndex, UserIndex)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
UserName : STRINGDATA 'A-HANSEN'''
DirIndex : W BLOCK 1
UserIndex : W BLOCK 1
ErrCode : W BLOCK 1
GetDirUserIndexes : EQU 37B9 + 213B

CALLG GetDirUserIndexes, 3, UserName, DirIndex, UserIndex
IF K GO ERROR

ERROR : W1 =: ErrCode
%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (USER
%Address of string with directory and user name.
MON 213
%Monitor call GetDirUserIndexes.
JMP ERROR
%Error return from monitor call.
STT INDEX
%Normal return, store indexes.

ERROR, ...
%Error number in register A.

USER, 'A-HANSEN'
%Obtain indexes for A-HANSEN.
INDEX, 0
%Left byte: dir index. Right byte: user index.


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 237*

[↑ Back to Top](#table-of-contents)

---

### 214B - GetUserName (GUSNA)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the name of a user. The user may be on a remote computer if the COSMOS network is installed. The remote system name is then returned.

- RT programs return the name of user RT.

#### See Also

[GetUserEntry](#44b-getuserentry-ruser), [ExecutionInfo](#143b-executioninfo-rsio)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirectoryIndex, UserIndex : INTEGER2;
UserName : PACKED ARRAY [0..15] OF CHAR;
...
FindUserName(UserName, DirectoryIndex, UserIndex); [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirectoryIndex COMP.
01 UserIndex COMP.
01 UserName PIC X(16).
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "GetUserName" USING UserName, DirectoryIndex, UserIndex,
              RemoteFlag, RemoteSystem.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, UserIndex, RemoteFlag
CHARACTER UserName*16, RemoteSystem*64
...
Monitor_Call('GetUserName', UserName(1:16), DirIndex, UserIndex,
             RemoteFlag, RemoteSystem(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirectoryIndex, UserIndex, RemoteFlag
BYTES : UserName(0:15), RemoteSystem(0:63)
...
ON ROUTINEERROR DO
  IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('GetUserName', UserName, DirectoryIndex, UserIndex,&
  RemoteFlag, RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirectoryIndex : W BLOCK 1
UserIndex : W BLOCK 1
UserName : STRING 16
SysId : STRING 16   %Optional parameter for remote system.
ErrCode : W BLOCK 1
GetUserName : EQU 37B9 + 214B
...
CALLG GetUserName, 3, UserName, DirectoryIndex, UserIndex
  IF K GO ERROR
...
ERROR : W1 =: ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (USER           %Address of string to receive user name.
LDX INDEX           %Left byte: Dir index. Right byte: User index.
LDT (REMID          %Remote identification if bit 15 in register X.
MON 214             %Monitor call GetUserName.
JMP ERROR           %Error return from monitor call.
...                 %Normal return.
ERROR, ...          %Error number in register A.
...

USER, 0             %A string of 16 characters.
*+10/
INDEX, ...          %Set bit 15 if remote user.
REMID, 0            %Remote system identification string.
*+32/


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 299*

[↑ Back to Top](#table-of-contents)

---

### 215B - GetObjectEntry (DROBJ)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about a file. An object entry describes each file. It contains the file name, the access rights, the date last opened for read and write, the size, and more. See the file system description in appendix C. You specify the directory index, the user index, and the object index.

- There is one object entry for each version of a file.
- Only user SYSTEM can get the object entry of a file without read or write access to the file.
- You may access files on remote computer systems. The computers must be connected through a COSMOS network.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The 64 byte object entry. |
| `Param2` | UNKNOWN | I | The directory index. See GetAllFileIndexes. |
| `Param3` | UNKNOWN | I | The user index. See GetAllFileIndexes. |
| `Param4` | UNKNOWN | I | The object index. See GetAllFileIndexes. |
| `Param5` | UNKNOWN | I | Remote flag. Use 0 for the local computer and 1 for a remote computer. |
| `Param6` | UNKNOWN | I | Remote system identification if remote flag is 1. |
| `Param7` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[ReadObjectEntry](#41b-readobjectentry-robje), [SetObjectEntry](#216b-setobjectentry-dwobj), and GetAllFileIndexes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirIndex, UserIndex, ObjectIndex : INTEGER2;
Buffer : ARRAY [0..1] OF RECORD...END;
...
GetObjectEntry(Buffer, DirIndex, UserIndex, ObjectIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirIndex COMP.
01 UserIndex COMP.
01 ObjectIndex COMP.
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 Buffer.
   02 array COMP OCCURS 32 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetObjectEntry" USING Buffer, DirIndex, UserIndex,
   ObjectIndex, RemoteFlag, RemoteSystem)
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, UserIndex, ObjectIndex, RemoteFlag
CHARACTER RemoteSystem*64
INTEGER Buffer(32)
...
Monitor_Call('GetObjectEntry', Buffer(1), DirIndex, UserIndex,
   ObjectIndex, RemoteFlag, RemoteSystem(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirIndex, UserIndex, ObjIndex, RemoteFlag
BYTES : RemoteSystem(0:63)
BYTES : Buffer(0:63)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetObjectEntry', Buffer(0), DirIndex, UserIndex, ObjIndex,&
             RemoteFlag, RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirIndex : W BLOCK 1
UserIndex : W BLOCK 1
ObjectIndex : W BLOCK 1
Buffer : STRING 208
SysId : STRING 16 %Optional parameter if bit 7 in DirIndex set.
ErrCode : W BLOCK 1
GetObjectEntry : EQU 3789 + 215B

...
CALLG GetObjectEntry, 4, Buffer, DirIndex, UserIndex ObjectIndex
IF K GO ERROR
...

ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (REMID      %Remote identification in register D.
COPY SA DD      %Set bit 15 in register T if remote file.
LDA (BUFF       %Address of buffer for returned object entry.
LDT INDEX       %Left byte: Dir index. Right byte: User index.
LDX OBJIX       %Object index.
MON 215         %Monitor call GetObjectEntry.
JMP ERROR       %Error return from monitor call.
...             %Normal return.
ERROR, ...      %Error number in register A.
  ...
BUFF, 0
*+40/           %Make a buffer of 32 words.
INDEX, ...      %Set bit 15 if remote object entry.
OBJIX, ...
REMID, 0        %Remote identification string..
*+40/           %Make a buffer of 32 words.



| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|

Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 261*

[↑ Back to Top](#table-of-contents)

---

### 216B - SetObjectEntry (DWOBJ)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Changes the description of a file. An object entry describes each file. It contains the file name, the access rights, the date it was last opened for read and write, the size, and more. You may use GetObjectEntry to read an object entry, change parts of it, then write it back with SetObjectEntry.

- You specify the directory index, the user index, and the object index.
- There is one object entry for each version of a file.
- Only user SYSTEM can change the object entry of a file without read or write access to the file.
- On the ND-100 you may access files on remote computer systems if the computers are connected through a COSMOS network.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The 64 byte object entry. See appendix C. |
| `Param2` | UNKNOWN | I | The directory index. See GetAllFileIndexes. |
| `Param3` | UNKNOWN | I | The user index. See GetAllFileIndexes. |
| `Param4` | UNKNOWN | I | The object index. See GetAllFileIndexes. |
| `Param5` | UNKNOWN | I | Remote flag. Use 0 for the local computer and 1 for a remote computer. |
| `Param6` | UNKNOWN | I | Remote system identification if remote flag is 1. |
| `Param7` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetObjectEntry](#215b-getobjectentry-drobj), [GetAllFileIndexes](#217b-getallfileindexes-guioi), and @CHANGE-OBJECT-ENTRY

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buff : ARRAY [0..1] OF RECORD...END;
DirIndex, UserIndex, ObjectIndex : INTEGER2;
...
SetObjectEntry(Buff, DirIndex, UserIndex, ObjectIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buff.
   02 array COMP OCCURS 32 TIMES.
01 DirIndex COMP.
01 UserIndex COMP.
01 ObjectIndex COMP.
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "SetObjectEntry" USING Buff, DirIndex, UserIndex, ObjIndex,
                                     RemoteFlag, RemoteSystem.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buff(32)
INTEGER DirIndex, UserIndex, ObjIndex, RemoteFlag
CHARACTER RemoteSystem*64
...
Monitor_Call('SetObjectEntry', Buff(1), DirIndex, UserIndex, ObjIndex,
             RemoteFlag, RemoteSystem(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Buff(0:63)
INTEGER : DirIndex, UserIndex, ObjIndex, RemoteFlag
BYTES : RemoteSystem(0:63)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetObjectEntry', Buff(0), DirIndex, UserIndex, ObjIndex,&
  RemoteFlag, RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buff : H BLOCK 32
DirIndex : W BLOCK 1      %SysId used if bit 7 is set to 1.
UserIndex : W BLOCK 1
ObjIndex : W BLOCK 1
SysId : STRING 20B        %Dummy if not remote system
ErrCode : W BLOCK 1
SetObjectEntry : EQU 3789 + 216B
...
CALLG SetObjectEntry, 5, Buff, DirIndex, UserIndex, ObjIndex, SysId
IF K GO ERROR
...
ERROR : W1 =: ErrCode     %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA   (REMID)             %Remote identification in register D.
COPY  SA DD               %Used if bit 15 in register X is set.
LDA   (BUFF)              %Address of buffer containing source object entry.
LDT   INDEX               %Left byte: Dir. index, right byte: User index.
LDX   OBJIX               %Object index.
MON   216                 %Monitor call SetObjectEntry.
JMP   ERROR               %Error return from monitor call.
...                       %Normal return.
ERROR, ...                %Error number in register A.
...
BUFF, 0                   %
*+40/                     %Make a buffer of 32 words.
INDEX, ...                %Set bit 15 if remote file.
OBJIX, ...
REMID, 0                  %Remote system identification string.
*+32/                     %Space for the string.



| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 453*

[↑ Back to Top](#table-of-contents)

---

### 217B - GetAllFileIndexes (GUIOI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the directory index, the user index, and the object index of a file. These are indexes in the SINTRAN III File System. Appendix C describes the File System.

- The file must be open.
- You may get the information if the file is on your local computer, or on a remote computer system if the computers are connected through a COSMOS network.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number` | See Openfile. | I |  |
| `Directory index` | The directory index. | I |  |
| `User index` | The user index. | I |  |
| `Object index` | The object index. | I |  |
| `Remote flag` | Set to 0 if the file is on the local computer. A file on a remote computer returns 1. | I |  |
| `Remote system identification` | If the remote flag is set to 1. It includes information needed to log in, and is terminated by an apostrophe, e.g. SNURRE(P-HANSEN[&&&&&])'. The password, if present, is coded as a sequence of 6 control characters. | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[GetDirUserIndexes](#213b-getdiruserindexes-muidi), [GetDirNameIndex](#243b-getdirnameindex-fdina), [GetFileIndexes](#274b-getfileindexes-fobjn), [GetDirEntry](#244b-getdirentry-gdien), [GetUserEntry](#44b-getuserentry-ruser), [GetObjectEntry](#215b-getobjectentry-drobj), and GetDefaultDir

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, DirIndex, UserIndex, ObjectIndex : INTEGER2;
...
GetAllFileIndexes(FileNo, DirIndex, UserIndex, ObjectIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 DirIndex COMP.
01 UserIndex COMP.
01 ObjectIndex COMP.
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "GetAllFileIndexes" USING FileNo, DirIndex, UserIndex,
  ObjectIndex, RemoteFlag, RemoteSystem.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, DirIndex, UserIndex, ObjectIndex, RemoteFlag
CHARACTER RemoteSystem*64
...
Monitor_Call('GetAllFileIndexes', FileNo, DirIndex, UserIndex,
  ObjectIndex, RemoteFlag, RemoteSystem(1:64))
IF (ErrCode .NE. 0) THEN ...


Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo, DirIndex, UserIndex, ObjectIndex, RemoteFlag
BYTES : RemoteSystem(0:63)
...
ON ROUTINEERROR DO
&emsp;IF ErrCode >< 0 THEN ...
ENDON

Monitor_Call('GetAllFileIndexes', FileNo, DirIndex, UserIndex, ObjectIndex,&
&emsp;&emsp;RemoteFlag, RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Variable      | Width | Description |
|---------------|-------|-------------|
| FileNo        | W BLOCK 1 | |
| DirIndex      | W BLOCK 1 | %If bit 7 is set, SysId is a 5th parameter. |
| UserIndex     | W BLOCK 1 | |
| ObjectIndex   | W BLOCK 1 | |
| SysId         | STRING 16 | %Remote system name as an optional 5th parameter. |
| ErrCode       | W BLOCK 1 | |

GetAllFileIndexes : EQU 37B9 + 217B
...
CALLG GetAllFileIndexes, 4, FileNo, DirIndex, UserIndex, ObjectIndex
% This call is for a local file. For a remote file, the
% format would be:
% CALLG GetAllFileIndexes, 5, FN, DI, UI, OI, SysID
IF K GO ERROR
...

ERROR : WI =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (BUFF %Buffer in which to return remote system
COPY SA DD % identification
LDA FILNO %File number returned from earlier OpenFile.
MON 217 %Monitor call GetAllFileIndexes.
JMP ERROR %Error return from monitor call.
STT INDEX %Normal return, store directory and user index.
STX OBJIX %Store object index.
COPY SD DA %Remote identification in D register if bit 15
STA REMID % in T register is set.
...
ERROR, ... %Error number in register A.
...
FILNO, ...
INDEX, 0 %Dir index: left byte. User index: right byte.
OBJIX, 0 %Object index.
REMID, 0 %Address of remote identification string.
BUFF, 0
*+40/


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 221*

[↑ Back to Top](#table-of-contents)

---

### 220B - DirectOpen (DOPEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Opens a file. Files must be opened before they can be accessed. For public users this monitor call is identical to OpenFile. User SYSTEM and user RT are given the same access rights as the owner of a file.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. |
| `Param2` | UNKNOWN | I | Access code. The legal values are shown below. |
| `0` | Sequential write. | I |  |
| `1` | Sequential read. | I |  |
| `2` | Random read or write. | I |  |
| `3` | Random read only. | I |  |
| `4` | Sequential read or write. | I |  |
| `5` | Sequential write append. | I |  |
| `6` | Random read or write common on contiguous files. | I |  |
| `7` | Random read common on contiguous files. | I |  |
| `8` | Random read or write on contiguous files. Direct transfer for ReadFromFile, WriteToFile and DeviceFunction in RT programs. | I |  |
| `9` | Random read, write append for WriteToFile. | I |  |
| `Param13` | UNKNOWN | I | File name. |
| `Param14` | UNKNOWN | I | File type. Do not include the colon. Default is 'SYMB'. |
| `Param15` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[OpenFile](#50b-openfile-open), [CloseFile](#43b-closefile-close)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, AccCode : INTEGER2;
FileName : PACKED ARRAY [0..63] OF CHAR;
FileType : PACKED ARRAY [0..3] OF CHAR;
...
DirectOpen(FileNumber, AccCode, FileName, FileType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 AccCode COMP.
01 FileName PIC X(64).
01 FileType PIC X(4).
01 ErrCode COMP.
...
MONITOR-CALL "DirectOpen" USING FileNumber, AccCode, FileName, FileType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, AccCode
CHARACTER FileName*64, FileType*4
...
Monitor_Call('DirectOpen', FileNumber, AccCode, FileName(1:64), FileType(1:4))
C IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, AccCode
BYTES : FileName(0:63), FileType(0:3)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('DirectOpen', FileNumber, AccCode, FileName, FileType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
AccCode : W BLOCK 1
FileName : STRINGDATA 'EXAMPLE'
FileType : STRINGDATA 'SYMB'
ErrCode : W BLOCK 1
DirectOpen : EQU 3789 + 220B
...
    CALLG DirectOpen, 4, FileNumber, AccCode, FileName, FileType
    IF K GO ERROR
...
ERROR : W1 =: ErrCode         %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (FILE        %Address of file name string.
LDA (TYPE        %Address of file type string.
LDT ACCES        %Access code.
MON 220          %Monitor call DirectOpen.
JMP ERROR        %Error return from monitor call.
STA FILNO        %Normal return, store the filenumber returned.
...
ERROR, ...       %Error number in register A.
...
FILNO, 0
ACCES, ...
FILE, 'EXAMPLE'  %Open EXAMPLE:SYMB
TYPE, 'SYMB'     %


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 151*

[↑ Back to Top](#table-of-contents)

---

### 221B - CreateFile (CRALF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Creates a file. The file may be indexed, contiguous, or allocated. Most files are indexed. The size of indexed files expands automatically when written to. Contiguous and allocated files have shorter access time.

- You need directory access to the user who owns the file.
- User SYSTEM and RT always have the owner's access rights.
- An indexed file not yet written to may be converted to a contiguous file. Use ExpandFile or @EXPAND-FILE.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File name. Default file type is :DATA. |
| `Param2` | UNKNOWN | I | Start address in the directory. Use 0 if you want to create a contiguous or inde |
| `Param3` | UNKNOWN | I | Length of the file in pages. Use 0 if you want to create an indexed file. |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

@CREATE-FILE, @ALLOCATE-FILE, [NewFileVersion](#253b-newfileversion-craln), and ExpandFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
StartAddress, NoofPages : LONGINT;
FileName : PACKED ARRAY [0..63] OF CHAR;
...
CreateFile(FileName, StartAddress, NoofPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 StartAddress COMP PIC S9(10).
01 NoofPages COMP PIC S9(10).
01 FileName PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "CreateFile" USING FileName, StartAddress, NoofPages.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER*4 StartAddress, NoofPages
CHARACTER FileName*64
...
Monitor_Call('CreateFile', FileName(1:64), StartAddress, NoofPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER4 : StartAddress, NoOfPages
BYTES : FileName(0:63)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('CreateFile', FileName, StartAddress, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
StartAddress : W BLOCK 1
NoOfPages : W BLOCK 1
FileName : STRINGDATA 'EXAMPLE:SYMB'...
ErrCode : W BLOCK 1
CreateFile : EQU 37B9 + 221B
...
CALLG CreateFile, 3, FileName, StartAddress, NoOfPages
IF K GO ERROR
...
ERROR : W1 =: ErrCode        %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX  (FILE         %Address of file name string.
LDD  START         %Load register AD with start address.
LDT  (SIZE         %Address of double word with number of pages.
MON  221           %Monitor call CreateFile.
JMP  ERROR         %Error return from monitor call.
...               %Normal return.
ERROR, ...         %Error number in register A.
...
FILE, 'EXAMPLE:SYMB' %Create EXAMPLE:SYMB.
START, ...          %Start address, ie. page address in directory.
...               %A double word.
SIZE, ...           %File size as a double word.
...               %


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 125*

[↑ Back to Top](#table-of-contents)

---

### 222B - GetAddressArea (GBSIZ)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the size of your address area. Your address area may consist of one or two 128 Kbyte areas. This depends on the size of the background segments:

- The size of the background segment is defined when SINTRAN III is generated for your computer.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 227B - SetEscLocalChars (MSDAE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

You can terminate most programs with the ESCAPE key. A LOCAL key has a similar function. It terminates a connection to a remote computer in a network. This monitor call allows you to select other keys for these functions.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 230B - GEtEscLocalChars (MGDAE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets ESCAPE and LOCAL characters. You can terminate most programs with the ESCAPE key. A LOCAL key has a similar function. It terminates a connection to a remote computer in a COSMOS network. The system supervisor may select other keys for these functions. This monitor call tells you which keys to use.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 231B - ExpandFile (EXPFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Expands the file size. You use this monitor call to increase the size of contiguous and allocated files. The space following the file on the disk must be free.

- Indexed files created with 0 pages may be expanded.
- Public users must have directory access to the file. User RT and SYSTEM can expand any file.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➙ File name. It may be abbreviated, but this slows down execution.` | UNKNOWN | I |  |
| `➙ Number of additional pages.` | UNKNOWN | I |  |
| `➙ Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

[CreateFile](#221b-createfile-cralf), @EXPAND-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
NoOfPages : LONGINT;
FileName : PACKED ARRAY [0..63] OF CHAR;
...
ExpandFile(FileName, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NoOfPages COMP PIC S9(10).
01 FileName PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "ExpandFile" USING FileName, NoOfPages.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER*4 NoOfPages
CHARACTER FileName*64
...
Monitor_Call('ExpandFile', FileName(1:64), NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER4 : NoOfPages
BYTES : FileName(0:63)
...
ON ROUTINEERROR DO
    IF ErrCode >‹ O THEN ...
ENDON
Monitor_Call('ExpandFile', FileName, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NoOfPages : W BLOCK 1
FileName : STRING 64
ErrCode : W BLOCK 1
ExpandFile : EQU 37B9 + 231B
...
    CALLG ExpandFile, 2, FileName, NoOfPages
    IF K GO ERROR
    ...
ERROR : W1 =: ErrCode          %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX    (FILE    %Address of file name string.
LDT    (PAGES   %Address of double word with number of pages.
MON    231      %Monitor call ExpandFile.
JMP    ERROR    %Error return from monitor call.
...             %Normal return.
ERROR, ...      %Error number in register A.
...
FILE,  'EXAMPLE:SYMB'  %File name.
PAGES, ...             %A double word.
...                    %


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 191*

[↑ Back to Top](#table-of-contents)

---

### 232B - RenameFile (MRNFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

See also @RENAME-FILE.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Old file name. |
| `Param2` | UNKNOWN | I | New file name with file type, e.g. ADDRESS-LIST:TEXT. Do not use the directory n |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

@RENAME-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
OldFileName, NewFileName : PACKED ARRAY [0..63] OF CHAR;
...
RenameFile(OldFileName, NewFileName);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 OldFileName PIC X(64).
01 NewFileName PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "RenameFile" USING OldFileName, NewFileName.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER OldFileName*64, NewFileName*64
...
Monitor_Call('RenameFile', OldFileName(1:64), NewFileName(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : OldFileName(0:63), NewFileName(0:63)
...
ON ROUTINEERROR DO
&emsp; IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('RenameFile', OldFileName, NewFileName)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
OldFileName : STRINGDATA 'TEXT:TEXT'
NewFileName : STRINGDATA 'EXAMPLE:SYMB'
ErrCode : W BLOCK 1
RenameFile : EQU 37B9 + 232B
...
CALLG RenameFile, 2, OldFileName, NewFileName
IF K GO ERROR
...
ERROR : W1 =: ErrCode &emsp;&emsp;&emsp; %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX &emsp;(OLDFI) &emsp;%Address of string with old file name.
LDA &emsp;(NEWFI) &emsp;%Address of string with new file name.
MON &emsp; 232 &emsp;&emsp; %Monitor call RenameFile.
JMP &emsp; ERROR &emsp;&emsp; %Error return from monitor call.
... &emsp;&emsp;&emsp;&emsp;&emsp;&emsp; %Normal return.
ERROR, ... &emsp;&emsp; %Error number in register A.

OLDFI, 'TEXT:TEXT' &emsp; %Change name of file TEXT:TEXT to EXAMPLE:SYMB.
NEWFI, 'EXAMPLE:SYMB'

| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 413*

[↑ Back to Top](#table-of-contents)

---

### 233B - SetTemporaryFile (STEFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Defines a file to store information temporarily. The file can be read once. When it is closed, its contents are deleted. The empty file will still exist.

- GetObjectEntry and @FILE-STATISTICS shows whether a file is temporary or not.
- Files to be printed are commonly defined as temporary. Their contents exist until they have been printed.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `→ File name.` |  | I |  |
| `← Standard Error Code.` | See appendix A. | I |  |

#### See Also

[CreateFile](#221b-createfile-cralf), @SET-TEMPORARY-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
...
SetTemporaryFile(FileName);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "SetTemporaryFile" USING FileName.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER FileName*64
...
Monitor_Call('SetTemporaryFile', FileName(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : FileName(0:63)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetTemporaryFile', FileName)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRINGDATA 'TEMP-FILE:SYMB''
ErrCode  : W BLOCK 1
SetTemporaryFile : EQU 37B9 + 233B
...
    CALLG SetTemporaryFile, 1, FileName
    IF K GO ERROR
...
ERROR : W1 =: ErrCode    %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX  (FILE          %Address of string with file name.
MON 233            %Monitor call SetTemporaryFile.
JMP ERROR          %Error return from monitor call.
...                %Normal return.
ERROR, ...         %Error number in register A.
...
FILE, 'TEMP-FILE:SYMB' %Treat TEMP-FILE:SYMB as a temporary file.


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 473*

[↑ Back to Top](#table-of-contents)

---

### 234B - SetPeripheralName (SPEFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Defines a peripheral file, e.g. a printer. You connect a file name to the logical device number of the peripheral.

- The file name should exist in advance, but with no file type. Otherwise you may include the file name in double quotes ("..."). An empty file type is default.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞ File name for the peripheral. See appendix G.` | UNKNOWN | I |  |
| `➞ Logical device number. See appendix B.` | UNKNOWN | I |  |
| `⬅ Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

SetTerminalFile, @SET-PERIPHERAL-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
DeviceNumber : INTEGER2;
...
SetPeripheralName(FileName, DeviceNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName PIC X(64).
01 DeviceNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SetPeripheralName" USING FileName, DeviceNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER FileName*64
INTEGER DeviceNumber
...
Monitor_Call('SetPeripheralName', FileName(1:64), DeviceNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : FileName(0:63)
INTEGER : DeviceNumber

...

ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetPeripheralName', FileName, DeviceNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRINGDATA 'LINE-PRINTER'''   %Use LINE-PRINTER as name of dev.
DeviceNumber : W DATA 5                  % no. 5 (line printer no. 1).
ErrCode : W BLOCK 1
SetPeripheralName : EQU 37B9 + 234B

...

CALLG SetPeripheralName, 2, FileName, DeviceNumber
IF K GO ERROR

...

ERROR : W1 =: ErrCode                    %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX    (FILE          %Address of string with file name.
LDA    DEVNO          %Logical device number.
MON    234            %Monitor call SetPeripheralFile.
JMP    ERROR          %Error return from monitor call.
...                  %Normal return.
ERROR, ...           %Error number in register A.
...
FILE, 'LINE-PRINTER'  %Use LINE-PRINTER as name of device number 5.
DEVNO, 5              %Device number of line printer no. 1.



| ND-100 and ND-500 | User SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 457*

[↑ Back to Top](#table-of-contents)

---

### 235B - ScratchOpen (SCROP)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Opens a file as a scratch file. A maximum of 64 pages of the file is kept when you close the file. Use SET-CLOSED-FILE-SIZE in the SINTRAN-SERVICE-PROGRAM to change this.

- The scratch file is closed like any other opened file.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number.` | UNKNOWN | O | Used by monitor calls for input and output. |
| `Access code.` | UNKNOWN | I | 0 = Sequential write, 1 = Sequential read, 2 = Random read or write, 3 = Random read only, 4 = Sequential read or write, 5 = Sequential write append, 6 = Random read or write common on contiguous files, 7 = Random read common on contiguous files, 8 = Random read or write on contiguous files. Direct transfer for ReadFromFile, WriteToFile and DeviceFunction in RT programs, 9 = Random read, write append for WriteToFile and ReadFromFile. |
| `File name.` | UNKNOWN | I | The name of the file to be opened. |
| `Default file type.` | UNKNOWN | I | Specifies the file type. The colon should not be included. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[OpenFile](#50b-openfile-open), [SetPermanentOpen](#236b-setpermanentopen-sperd), [DirectOpen](#220b-directopen-dopen), [CloseFile](#43b-closefile-close), and @SCRATCH-OPEN

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, AccessCode : INTEGER2;
FileName : PACKED ARRAY [0..63] OF CHAR;
FileType : PACKED ARRAY [0..3] OF CHAR;
...
ScratchOpen (FileNo, AccessCode, FileName, FileType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 AccessCode COMP.
01 FileName PIC X(64).
01 FileType PIC X(4).
01 ErrCode COMP.
...
MONITOR-CALL "ScratchOpen" USING FileNo, AccessCode, FileName, FileType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, AccessCode
CHARACTER FileName*64, FileType*4
...
Monitor_Call('ScratchOpen', FileNo, AccessCode, FileName(1:64), FileType(1:4))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo, AccessCode
BYTES : FileName(0:63), FileType(0:3)
...
ON ROUTINEERROR DO
    IF ErrCode <> 0 THEN ...
ENDON
Monitor_Call('ScratchOpen', FileNo, AccessCode, FileName, FileType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
AccessCode : W BLOCK 1
FileName : STRINGDATA 'EXAMPLE'''
FileType : STRINGDATA 'SYMB'''
ErrCode : W BLOCK 1
ScratchOpen : EQU 37B9 + 235B
...
CALLG ScratchOpen, 4, FileNo, AccessCode, FileName, FileType
    IF K GO ERROR
...
ERROR : W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (FILE      %Address of file name string.
LDA (TYPE      %Address of default file type string.
LDT ACODE      %Access code.
MON 235        %Monitor call ScratchOpen.
JMP ERROR      %Error return from monitor call.
STA FILNO      %Normal return, store file number.
...
ERROR, ...     %Error number in register A.
...
FILNO, 0
ACODE, ...
FILE, 'EXAMPLE' %Open EXAMPLE:SYMB as a scratch file.
TYPE, 'SYMB'


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 236B - SetPermanentOpen (SPERD)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets a file permanently open. The file is not closed by CloseFile with -1 as file number. You have to specify the file number or -2.

- The file must already be open.
- Only mass-storage files can be set permanently open.
- The file is not closed when your program terminates.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number` | See OpenFile. | I |  |
| `Standard Error` | Code. See appendix A. | I |  |

#### See Also

[OpenFile](#50b-openfile-open), [CloseFile](#43b-closefile-close), and @SET-PERMANENT-OPEN

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber : INTEGER2;
...
SetPermanentOpen(FileNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SetPermanentOpen" USING FileNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber
...
Monitor_Call('SetPermanentOpen', FileNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber
...
ON ROUTINEERROR DO
     IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetPermanentOpen', FileNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
ErrCode : W BLOCK 1
SetPermanentOpen : EQU 3789 + 236B
...
CALLG SetPermanentOpen, 1, FileNumber
     IF K GO ERROR
...
ERROR : W1 =: ErrCode       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT     FILNO       %File number returned from earlier open.
MON     236         %Monitor call SetPermanentOpen.
JMP     ERROR       %Error return from monitor call.
...                 %Normal return.
ERROR, ...          %Error number in register A.
...
FILNO, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 459*

[↑ Back to Top](#table-of-contents)

---

### 237B - SetFileAccess (SFACC)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the access protection for a file. You should specify the access for yourself, friends, and other users. The default file access for yourself is full access. Your friends have read access only. Other users have no access.

- You need directory access to a file to change the file access. User SYSTEM and RT may set the access protection for any files.
- Use the characters R, W, A, C, D, and N to specify the legal file access. R means Read. W means Write. A means Append to the end of a file. C means Common, i.e. more than one user may access the file at a time. D means Directory access, i.e. the file may be deleted, new versions created, etc. N means No access.
- Use @FILE-STATISTICS to check the file access.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File name. It is most efficient to use unabbreviated file names, e.g. EXAMPLE:TE |
| `Param2` | UNKNOWN | I | Public access. Use N or a combination of R, W, A, C, and D. Public access is typ |
| `Param3` | UNKNOWN | I | Friend access. Use N or a combination of R, W, A, C, and D. Friend access is typ |
| `Param4` | UNKNOWN | I | Own access. Use N or a combination of R, W, A, C, and D. Own access is typically |
| `Param5` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

CreateFriend, [SetObjectEntry](#216b-setobjectentry-dwobj), @SET-FILE-ACCESS, and @SET-DEFAULT-FILE-ACCESS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
PubAccess, FriendAccess, OwnAccess : PACKED ARRAY [0..4] OF CHAR;
...
SetFileAccess(FileName, PubAccess, FriendAccess, OwnAccess);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName PIC X(64).
01 PubAcc PIC X(5).
01 FriendAcc PIC X(5).
01 OwnAcc PIC X(5).
01 ErrCode COMP.
...
MONITOR-CALL "SetFileAccess" USING FileName, PubAcc, FriendAcc, OwnAcc.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER FileName*64, PubAcc*5, FriendAcc*5, OwnAcc*5
...
Monitor_Call('SetFileAccess', FileName(1:64), PubAcc(1:5),
             FriendAcc(1:5), OwnAcc(1:5))
IF (ErrCode .NE. 0) THEN ...


Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : FileName(0:63), PubAccess(0:5), FriendAccess(0:5), OwnAccess(0:5)
...
ON ROUTINEERROR DO
&nbsp; &nbsp; IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetFileAccess', FileName, PubAccess, FriendAccess, OwnAccess)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRINGDATA 'EXAMPLE:SYMB''' &nbsp; &nbsp; %Set access of file EXAMPLE:SYMB to
PubAccess : STRINGDATA 'N''' &nbsp; &nbsp; % no public access
FriendAccess : STRINGDATA 'RA''' &nbsp; &nbsp; % read, append access for friends
OwnAccess : STRINGDATA 'RWACD''' &nbsp; &nbsp; % full own access
ErrCode : W BLOCK 1
SetFileAccess : EQU 37B9 + 237B
...
&nbsp; &nbsp; CALLG SetFileAccess, 4, FileName, PubAccess, FriendAccess, OwnAccess
&nbsp; &nbsp; IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (FILE &nbsp; &nbsp; %Address of string with file name.
LDT (PUBAC &nbsp; &nbsp; %Address of string with public access characters.
LDA (FRIAC &nbsp; &nbsp; %Address of string with own access characters.
COPY SA DD
LDA (OWNAC &nbsp; &nbsp; %Address of string with friend access characters.
MON 237 &nbsp; &nbsp; %Monitor call SetFileAccess.
JMP ERROR &nbsp; &nbsp; %Error return from monitor call.
... &nbsp; &nbsp; %Normal return.
ERROR, ... &nbsp; &nbsp; %Error number in register A.
...
FILE, 'EXAMPLE:SYMB' &nbsp; %Change access for EXAMPLE:SYMB.
PUBAC, 'N' &nbsp; %No public access.
FRIAC, 'RA' &nbsp; %Read write append access for friend.
OWNAC, 'RWACD' &nbsp; %Full own access.

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 447*

[↑ Back to Top](#table-of-contents)

---

### 240B - AppendSpooling (APSPE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Prints a file. The printer has a queue of files waiting to be output. The file is appended to this queue. One or more copies can be printed.

- You may connect a text, e.g. INVOICE, to the job. The operator uses the text to select all files to be printed on special paper.
- SINTRAN III, version K, allows both the file and the printer to be on remote systems. The complete standard syntax for remote file specification is as follows:
`system(user(password:project)).(directory:user)file:type;version`

#### See Also

[CloseSpoolingFile](#40b-closespoolingfile-spclo), @APPEND-SPOOLING-FILE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName, PrinterName : PACKED ARRAY [0..63] OF CHAR;
NoOfCopies : INTEGER2;
UserText : PACKED ARRAY [0..127] OF CHAR;
...
AppendSpooling(FileName, PrinterName, NoOfCopies, UserText);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NoOfCopies COMP.
01 FileName PIC X(64).
01 PrinterName PIC X(64).
01 UserText PIC X(128).
01 ErrCode COMP.
...
>>> Comment: If UserText is present, add 32768 to NumberOfCopies
ADD 32768 TO NoOfCopies.
MONITOR-CALL "AppendSpooling" USING FileName, PrinterName, NoOfCopies, UserText.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER NoOfCopies
CHARACTER FileName*64, PrinterName*64, UserText*128
...
C If UserText is present, add 100000B to NoOfCopies
NoOfCopies=NoOfCopies+100000B
CALL Monitor_Call('AppendSpooling', FileName(1:64), PrinterName(1:64),
& NoOfCopies, UserText(1:128))
C IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : NoOfCopies
BYTES : FileName(0:63), PrinterName(0:63), UserText(0:127)
...
% If UserText is present, set bit 15 in NoOfCopies to 1
TRUE =: Bit(NoOfCopies,15)
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('AppendSpooling', FileName, PrinterName, NoOfCopies, UserText)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NoOfCopies : W BLOCK 1
FileName : STRINGDATA 'EXAMPLE:SYMB'...
PrinterName : STRINGDATA 'LINE-PRINTER'...
UserText : STRINGDATA 'File queued'...
ErrCode : W BLOCK 1
AppendSpooling : EQU 3789 + 240B
...
CALLG AppendSpooling, 4, FileName, PrinterName, NoOfCopies, UserText
IF K GO ERROR
...
ERROR : W1 =: ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX    (FILE     %Memory address of file name.
LDT    NOCOP   %Number of copies to be printed in bit 0:14.
LDA    (TEXT    %Memory address of message to the error device.
COPY   SA DO
LDA    (DEV     %Memory address of name of spooling device.
MON    240      %Monitor call AppendSpooling.
JMP    ERROR    %Error return from monitor call.
...            %Normal return.
ERROR, ...      %Error number in register A.
...
FILE, 'EXAMPLE:SYMB'   %Send EXAMPLE:SYMB to a printer.
DEV, 'LINE-PRINTER'    %Print file on the device LINE-PRINTER.
NOCOP, ...             %Bit 15 set to 1 means print message.
TEXT, 'GUMMED LABELS'  %Message to be send to error device.


`ND-100 and ND-500` | `All users` | `All programs`
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 79*

[↑ Back to Top](#table-of-contents)

---

### 241B - NewUser (SUSCN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Switches the user name you are logged in under. The command is similar to logging out and then logging in as another user. Your program continues under this user name.

- Restore the old user name with OldUser.
- OldUser always resets the first user name. From the ND-100 you may execute NewUser more than once without OldUser in between. This is not the case from the ND-500.
- If originally logged in as user RT, it is not possible to log in as user SYSTEM.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | New user name. |
| `Param2` | UNKNOWN | I | Password. Use the contents of the password location in the user entry. |
| `Param3` | UNKNOWN | I | Project password. |
| `Param4` | UNKNOWN | I | Return status. Public users return 0. User SYSTEM returns 1. User RT returns 2. |
| `Param5` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[OldUser](#242b-olduser-ruscn), @ENTER

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
UserName, ProjPasswd : PACKED ARRAY [0..15] OF CHAR;
UserPasswd : INTEGER2;
ReturnStatus : INTEGER2;
...
NewUser(UserName, UserPasswd, ProjPasswd, ReturnStatus);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 UserName PIC X(16).
01 UserPasswd COMP.
01 ProjPasswd PIC X(16).
01 RetStat COMP.
01 ErrCode COMP.
...
MONITOR-CALL "NewUser" USING UserName, UserPasswd, ProjPasswd, RetStat.

CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ReturnStatus, UserPasswd
CHARACTER UserName*16, ProjPasswd*16
...
Monitor_Call1('NewUser', UserName(1:16), UserPasswd,
C ProjPasswd(1:16), ReturnStatus)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
UserType : W BLOCK 1
UserName : STRINGDATA 'A-HANSEN'''
UserPasswd : W BLOCK 1
ProjPasswd : STRING 40B
ErrCode : W BLOCK 1
NewUser : EQU 37B9 + 241B

...

CALLG NewUser, 4, UserName, UserPasswd, ProjPasswd, UserType
IF K GO ERROR

...

ERROR : W1 =: ErrCode      %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT (PROJP     %Address of project password.
LDX (USER     %Address of string containing user name.
LDA (PASSW    %User password coded as integer.
MON 241       %Monitor call NewUser.
JMP ERROR     %Error return from monitor call.
STA STAT      %Normal return, store status returned.

...


ERROR, ...     %Error number in register A.
USER, 'A-HANSEN'     %Use A-HANSEN as user name.
PASSW, ...     %Password as represented in the user entry.
PROJP, ...
STAT, 0


| ND-100 and ND-500 | User RT and user SYSTEM | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 353*

[↑ Back to Top](#table-of-contents)

---

### 242B - OldUser (RUSCN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Switches back to the user name you were logged in under before NewUser. The command is similar to logging out and then log in as another user. Your program continues under the old user.

- The monitor call has no function if NewUser has not been executed.
- You may execute NewUser more than once without OldUser in between. OldUser always reset the first user name.

#### See Also

[NewUser](#241b-newuser-suscn), QENTER

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
OldUser;
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrCode COMP.
...
MONITOR-CALL "OldUser"
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
Monitor Call('OldUser')
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
ON ROUTINEERROR DO
  IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('OldUser')
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrCode : W BLOCK 1
OldUser : EQU 37B9 + 242B
...
CALLG OldUser, 0
IF K GO ERROR
...
ERROR : W1 =: ErrCode     %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON 242       %Monitor call OldUser.
JMP ERROR     %Error return from monitor call.
              %Normal return.
ERROR, ...    %Error number in register A.
...


| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 365*

[↑ Back to Top](#table-of-contents)

---

### 243B - GetDirNameIndex (FDINA)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets directory index and name index. The name index identifies the device description of the disk. You have to specify the directory name.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 244B - GetDirEntry (GDIEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about a directory. The directory entry is returned. Appendix C describes the file system in more detail.

- You may access directories on remote systems. The computers must be connected through a COSMOS network.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The directory index. See GetDirUserIndexes. |
| `Param2` | UNKNOWN | I | The 42-byte directory entry. See appendix C. |
| `Param3` | UNKNOWN | I | Flag indicating whether or not the disk has spare-track allocation. |
| `Param4` | UNKNOWN | I | Remote flag. Use 0 for the local computer and 1 for a remote computer. |
| `Param5` | UNKNOWN | I | Remote system identification if remote flag is 1. |
| `Param6` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetDirUserIndexes](#213b-getdiruserindexes-muidi), [WriteDirEntry](#311b-writedirentry-wdien), [GetUserEntry](#44b-getuserentry-ruser), and GetObjectEntry

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirectoryIndex, Flag : INTEGER2;
DirEntry : ARRAY [0..1] OF RECORD...END;
...
GetDirEntry(DirectoryIndex, DirEntry, Flag);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirectoryIndex COMP.
01 Flag COMP.
01 DirEntry.
   02 array COMP OCCURS 24 TIMES.
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "GetDirEntry" USING DirectoryIndex, DirEntry, Flag,
                                 RemoteFlag, RemoteSystem)
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirectoryIndex, Flag, RemoteFlag
CHARACTER RemoteSystem*64
INTEGER DirEntry(24)
...
Monitor_Call('GetDirEntry', DirectoryIndex, DirEntry(1), Flag,
             RemoteFlag, RemoteSystem(1:16))
C    IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirectoryIndex, Flag, RemoteFlag
BYTES : DirEntry(0:47)
BYTES : RemoteSystem(0:63)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetDirEntry', DirectoryIndex, DirEntry(0), Flag, RemoteFlag, &
RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirectoryIndex : W BLOCK 1    %Bit 7 set if SysId is supplied.
Flag : W BLOCK 1
DirEntry : W ARRAY 25B
SysId : STRING 16  %Remote system name as an optional 4th parameter.
ErrCode : W BLOCK 1
GetDirEntry : EQU 37B9 + 244B
...
CALLG GetDirEntry, 3, DirectoryIndex, DirEntry, Flag
IF K GO ERROR
...
ERROR : W1 =: ErrCode         %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX        %Directory index.
LDX (BUFF       %Buffer to receive directory entry.
LDA (REMID      %Remote system identification in D register.
COPY SA DD      %Used only if bit 15 in X register is set.
MON 244         %Monitor call GetDirEntry.
JMP ERROR       %Error return from monitor call.
...
                %Normal return.
                %A-reg.=1 or 5 : disk has spare-track allocation.
                %A-reg.=3 : no spare-track allocation.
ERROR, ...      %Error number in register A.
...
DIRIX, ...      %Set bit 15 if remote system.
BUFF, 0         %
*+30/           %24 word long buffer.
REMID, 0        %Remote identification string.
*+40/           %32 words for string.



| ND-100 and ND-500 | User RT and user SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 233*

[↑ Back to Top](#table-of-contents)

---

### 245B - GetNameEntry (GNAEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about devices, e.g. disks and floppy disks. The monitor call returns the name entry of a device. You specify the name index.

- GetDirNameIndex provides the name index.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The name index of the device. |
| `Param2` | UNKNOWN | I | The 28 byte name entry. It contains the following: |
| `0:15` | Device name. | I |  |
| `16:19` | Storage capacity in pages (PAVA1 and PAVA2) | I |  |
| `20:21` | Sector size of the disk. | I |  |
| `22:23` | Various flags. | I |  |
| `Bit 15` | Cartridge disk. | I |  |
| `Bit 14` | Not used. | I |  |
| `Bit 13` | Single user device, e.g. floppy disk or magnetic tape. | I |  |
| `Bit 12` | Not used. | I |  |
| `Bit 11` | Magnetic tape station. | I |  |
| `Bit 10` | EEC disks. | I |  |
| `Bit 9` | Not used. | I |  |
| `Bit 8` | Floppy disk. | I |  |
| `Bit 7` | Phoenix disk. | I |  |
| `Bit 6` | Little Winchester disk. | I |  |
| `Bit 5` | SCSI streamer. | I |  |
| `Bit 4` | SCSI disk. | I |  |
| `Bit 3` | Read-only WORM disk (optical). | I |  |
| `Bit 2:0` | Maximum number of subunits on this device (000<sub>2</sub> means 8 subunits, 001<sub>2</sub>-111<sub>2</sub> means maximum number of subunits). | I |  |
| `24:25` | Address of data transfer routine in SINTRAN III. | I |  |
| `26:27` | Logical device number of the disk's semaphore. | I |  |
| `Param23` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetDirEntry](#244b-getdirentry-gdien)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
NameIndex : INTEGER2;
NameTableEntry : RECORD...END;
...
GetNameEntry(NameIndex, NameTableEntry);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NameIndex COMP.
01 NameTableEntry.
    02 array COMP OCCURS 14 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetNameEntry" USING NameIndex, NameTableEntry.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER NameIndex
INTEGER NameTableEntry(14)
...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : NameIndex
BYTES : NameTableEntry(0:27)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetNameEntry', NameIndex, NameTableEntry(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NameIndex : W BLOCK 1
NameTableEntry : BY BLOCK 34B
ErrCode : W BLOCK 1
GetNameEntry : EQU 37B9 + 245B
...
CALLG GetNameEntry, 2, NameIndex, NameTableEntry
  IF K GO ERROR
...
ERROR : W1 =: ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT NAMIX          %Name index.
LDX (BUFF          %Address of buffer to receive name table entry.
MON 245            %Monitor call GetNameEntry.
JMP ERROR          %Error return from monitor call.
...                %Normal return.
ERROR, ...         %Error number in register A.
...
NAMIX, ...
BUFF, 0
+*16/              %Make a buffer of 14 words.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 257*

[↑ Back to Top](#table-of-contents)

---

### 246B - ReserveDir (REDIR)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reserves a directory for special use. The directory must be entered. Other users will not be able to open files on a reserved directory.

- All files in the directory must be closed.
- Only user RT and the current user may be logged in if main directory.
- Use ReleaseDir to release the directory.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Directory index. Use @LIST-DIRECTORIES to find the directory index. |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[ReleaseDir](#247b-releasedir-rldir)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirectoryIndex : INTEGER2;
...
ResDirectory(DirectoryIndex);   [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirectoryIndex COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ReserveDir" USING DirectoryIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirectoryIndex
...
Monitor_Call('ReserveDir', DirectoryIndex)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirectoryIndex
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('ReserveDir', DirectoryIndex)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| DirectoryIndex : W BLOCK 1 |
|----------------------------|
| ErrCode : W BLOCK 1        |
| ReserveDir : EQU 3789 + 246B  |
| ...                        |
| CALLG ReserveDir, 1, DirectoryIndex |
| IF K GO ERROR              |
| ...                        |
| ERROR : W1 =: ErrCode      |

%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX %Directory index.
MON 246 %Monitor call ReserveDir.
JMP ERROR %Error return from monitor call.
... %Normal return.
ERROR, ... %Error number in register A.
...
DIRIX, ...


ND-100 and ND-500 | User RT and user SYSTEM | All programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 417*

[↑ Back to Top](#table-of-contents)

---

### 247B - ReleaseDir (RLDIR)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Releases a directory. The directory must have been reserved with ReserveDir.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Directory index. Use @LIST-DIRECTORIES to find the directory index. |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[ReleaseResource](#123b-releaseresource-reles)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirectoryIndex : INTEGER2;
...
RelDirectory(DirectoryIndex);      [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirectoryIndex COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ReleaseDir" USING DirectoryIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirectoryIndex
...
Monitor_Call('ReleaseDir', DirectoryIndex)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirectoryIndex : W BLOCK 1
ErrCode : W BLOCK 1
ReleaseDir : EQU 3789 + 247B
...
CALLG ReleaseDir, 1, DirectoryIndex
IF K GO ERROR
...
ERROR : W1 =: ErrCode       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX      %Directory index.
MON 247        %Monitor call ReleaseDir.
JMP ERROR      %Error return from monitor call.
...            %Normal return.
ERROR, ...     %Error number in register A.
...
DIRIX, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 409*

[↑ Back to Top](#table-of-contents)

---

### 250B - GetDefaultDir (FDFDI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the user’s default directory. The directory index and the user index are returned.

- Use ExecutionInfo to get the user index and the directory index of the user executing the program.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞` | The user name. A user in a remote system may be identified. | I |  |
| `➞` | The directory index. | I |  |
| `➞` | The user index in the directory. | I |  |
| `➞` | Standard Error Code. See appendix A. | I |  |

#### See Also

[GetDirUserIndexes](#213b-getdiruserindexes-muidi), [GetDirNameIndex](#243b-getdirnameindex-fdina), [GetFileIndexes](#274b-getfileindexes-fobjn), [GetDirEntry](#244b-getdirentry-gdien), [GetUserEntry](#44b-getuserentry-ruser), [GetObjectEntry](#215b-getobjectentry-drobj), and GetAllFileIndexes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirectoryIndex, UserIndex : INTEGER2;
UserName : PACKED ARRAY [0..15] OF CHAR;
...
GetDefaultDir(UserName, DirectoryIndex, UserIndex);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirectoryIndex COMP.
01 UserIndex COMP.
01 UserName PIC X(16).
01 ErrCode COMP.
...
MONITOR-CALL "GetDefaultDir" USING UserName, DirectoryIndex, UserIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, UserIndex
CHARACTER UserName*16
...
Monitor_Call('GetDefaultDir', UserName(1:16), DirIndex, UserIndex)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirectoryIndex, UserIndex
BYTES : UserName(0:15)
...

ON ROUTINEERROR DO
  IF ErrCode >0 THEN ...
ENDON

Monitor_Call('GetDefaultDir', UserName, DirectoryIndex, UserIndex)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirectoryIndex : W BLOCK 1
UserIndex : W BLOCK 1
UserName : STRINGDATA 'A-HANSEN'''
ErrCode : W BLOCK 1
GetDefaultDir : EQU 37B9 + 250B
...

CALLG GetDefaultDir, 3, UserName, DirectoryIndex, UserIndex
IF K GO ERROR
...

ERROR : W1 =: ErrCode    %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (USER      %Address of string containing user name.
MON 250        %Monitor call GetDefaultDir.
JMP ERROR      %Error return from monitor call.
STT DIRIX      %Normal return, store directory index.
STX USRIX      %Store user index in default directory.
...

ERROR, ...     %Error number in register A.
...

USER, 'A-HANSEN'   %To find default directory of user A-HANSEN.
DIRIX, 0
USRIX, 0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 229*

[↑ Back to Top](#table-of-contents)

---

### 251B - CopyPage (COPAG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Copies file pages between two opened files. One of the files may be a magnetic tape or floppy disk with volume.

- This is a special monitor call used by the BACKUP-SYSTEM. No high-level language interface exists. The use of the X and D registers is tailored for reading labels on magnetic tape.
- Copying stops at end-of-file, a non-existent page, or if a short magnetic tape record is found.
- CopyPage is only used for sequential copying, i.e., a sequence of CopyPage calls must start with page number zero.
- Files should be opened for random read or write.
- Both files must be local files.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 252B - BackupClose (BCLOS)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Closes a file. The version number and the last date accessed are unchanged. The number of pages in temporary files and spooling files is not affected.

- This monitor call is mainly used by the BACKUP-SYSTEM.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File number of the opened file. See OpenFile.` | UNKNOWN | I |  |
| `Modified flag.` | UNKNOWN | I | If 0, the file is not marked as modified. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[CloseFile](#43b-closefile-close)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, Flag : INTEGER2;
...
BackupClose(FileNumber, Flag);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 Flag COMP.
01 ErrCode COMP.
...
MONITOR-CALL "BackupClose" USING FileNumber, Flag.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, Flag
...
Monitor_Call ('BackupClose', FileNumber, Flag)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, Flag
...
ON ROUTINEERROR DO
    IF ErrCode <> 0 THEN ...
ENDON
Monitor_Call('BackupClose', FileNumber, Flag)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
Flag : W BLOCK 1
ErrCode : W BLOCK 1
BackupClose : EQU 37B9 + 252B
...
CALLG BackupClose, 2, FileNumber, Flag
    IF K GO ERROR
...
ERROR : W1 =: ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO      %File number returned from earlier open.
LDA FLAG       %Reset modified flag.
MON 252        %Monitor call BackupClose.
JMP ERROR      %Error return from monitor call.
...            %Normal return.
ERROR, ...     %Error number in register A.
...
FILNO, ...
FLAG, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 253B - NewFileVersion (CRALN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Creates new versions of a file. You may create new versions for both indexed, contiguous and allocated files.

- You must have directory access to the user area where you create the file. User SYSTEM and RT get the owners access rights.
- The number following the semicolon in a file name is the version number. For example, TEST:SYMB:4 version 4 of the file.
- The file must exist in advance.
- Use DeleteFile to delete file versions.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The file name including the version number. The version number defines the total |
| `Param2` | UNKNOWN | I | Start address of the first new version. Use 0 for contiguous and indexed files. |
| `Param3` | UNKNOWN | I | File size in pages. Use 0 for indexed files. |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[CreateFile](#221b-createfile-cralf), @CREATE-NEW-VERSIONS, and @ALLOCATE-NEW-VERSIONS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
FirstPage, NoOfPages : LONGINT;
...
NewFileVersion(FileName, FirstPage, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName       PIC X(64).
01 FirstPage      COMP PIC S9(10).
01 NoOfPages      COMP PIC S9(10).
01 ErrCode        COMP.
...
MONITOR-CALL "NewFileVersion" USING FileName, FirstPage, NoOfPages.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER FileName*64
INTEGER*4 FirstPage, NoOfPages
...
Monitor_Call('NewFileVersion', FileName(1:64), FirstPage, NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER4 : FirstPage, NoOfPages
BYTES : FileName(0:63)
...
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('NewFileVersion', FileName, FirstPage, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRINGDATA 'EXAMPLE:SYMB;2'''  %Make a 2nd version of ...
FirstPage : W BLOCK 1
NoOfPages : W BLOCK 1
ErrCode : W BLOCK 1                     % EXAMPLE:SYMB.
NewFileVersion : EQU 37B9 + 253B
...
  CALLG NewFileVersion, 3, FileName, FirstPage, NoOfPages
  IF K GO ERROR
...
ERROR : W1 =: ErrCode                %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (FILE                       %Address of string containing file name.
LDD PAGNO                       %Page number of first page.
LDT (PAGES                      %Address of number of pages.
MON 253                         %Monitor call NewFileVersion.
JMP ERROR                       %Error return from monitor call.
...                             %Normal return.
ERROR, ...                      %Error number in register A.

FILE, 'EXAMPLE:SYMB;2'          %Create 2nd version of file EXAMPLE:SYMB
PAGNO, ...                      %
...                             %A double word.
PAGES, ...                      %
...                             %A double word.



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 351*

[↑ Back to Top](#table-of-contents)

---

### 254B - GetErrorDevice (GERDV)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the logical device number of the error device. The error device may be reserved by an RT program. If this is the case, the monitor call returns the address of the RT description. The error device is the terminal which outputs system errors and RT program messages. The error device is normally the console.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The logical device number of the error device. |
| `Param2` | UNKNOWN | I | RT description address of reserving RT program. 0 means unreserved. |

#### See Also

@GET-ERROR-DEVICE, @SET-ERROR-DEVICE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrorDevice, RTProgram : INTEGER2;
...
FindErrorDevice(ErrorDevice, RTProgram);   [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrorDevice COMP.
01 RTProgram COMP.
...
MONITOR-CALL "GetErrorDevice" USING ErrorDevice, RTProgram.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrorDevice, RTProgram
...
Monitor_Call('GetErrorDevice', ErrorDevice, RTProgram)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrorDevice, RTProgram
...
Monitor_Call('GetErrorDevice', ErrorDevice, RTProgram)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrorDevice : W BLOCK 1
RTProgram : W BLOCK 1
GetErrorDevice : EQU 37B9 + 254B
...
CALLG GetErrorDevice, 2, ErrorDevice, RTProgram
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
MON 254 %Monitor call GetErrorDevice.
STA ERDEV %Store logical number of error device.
COPY SD DA
STA RTPRO %Store address of RT description.
...
ERDEV, 0
RTPRO, 0


| ND-100 and ND-500 | User SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 239*

[↑ Back to Top](#table-of-contents)

---

### 255B - PIOCCFunction (PIOCM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

PIOCC (Programmed Input/Output Control Channel) function monitor call.

WARNING: This monitor call is not documented in the ND-860228.2 EN manual.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 256B - FullFileName (DEABF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Returns a complete file name from an abbreviated one. The directory, the user, the file name, the file type, and the version are returned.

- You must have read access to the file.
- The abbreviation must be unambiguous.
- SINTRAN III, version K, allows remote file names. If the abbreviated file name contains a remote specification, the name of the remote system cannot be abbreviated.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Abbreviated File Name.` | UNKNOWN | I | The abbreviated file name. May include a file type. |
| `Full File Name.` | UNKNOWN | O | The complete file name, terminated by an apostrophe ('). |
| `Default File Type.` | UNKNOWN | I | The default file type (do not include the colon). This parameter is used on ND-100 only and is ignored by the ND-500. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[GetFileName](#273b-getfilename-gfn)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
AbbrevFileName, FileName : PACKED ARRAY [0..63] OF CHAR;
FileType : PACKED ARRAY [0..3] OF CHAR;
...
FullFileName(AbbrevFileName, FileName, FileType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 AbbrevFileName PIC X(64).
01 FileName       PIC X(64).
01 FileType       PIC X(4).
01 ErrCode        COMP.
...
MONITOR-CALL "FullFileName" USING AbbrevFileName, FileName, FileType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER AbbrevFileName*64, FileName*64, FileType*4
...
Monitor_Call('FullFileName', AbbrevFileName(1:64), FileName(1:64),
C FileType(1:4))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : AbbrevFileName(0:63), FileName(0:63), FileType(0:3)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('FullFileName', AbbrevFileName, FileName, FileType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
AbbrevFileName : STRINGDATA 'EX'''
FileName : STRING 64
FileType : STRINGDATA 'SYMB''' %Default file type.
ErrCode : W BLOCK 1
FullFileName : EQU 37B9 + 256B
...
CALLG FullFileName, 3, AbbrevFileName, FileName, FileType
    IF K GO ERROR
...
ERROR : W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX (ABBR)      %Address of abbreviated file name string.
LDA (FILE)      %Address of string to receive full file name.
LDT (TYPE)      %Address of default file type string.
MON 256         %Monitor call FullFileName.
JMP ERROR       %Error return from monitor call.
...             %Normal return.
ERROR, ...      %Error number in register A.
...
ABBR, 'EX'      %Find full file name of EX.
FILE, 0         %Empty string. (A and X may be identical.)
*+76/           %Make space to receive full file name.
TYPE, 'SYMB'    %Default file type SYMB.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 257B - OpenFileInfo (FOPEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Monitor call 257B - OpenFileInfo

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 262B - GetSystemInfo (CPUST)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets various system information. The system number, the CPU type, the SINTRAN III version, the instruction set, the patch indicator, and the system generation time are returned.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | A number. It should always be 0. |
| `Param2` | UNKNOWN | I | The i2-word (24 byte) array containing system information. Details are given on  |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Number : INTEGER2;
Buff : RECORD...END;
...
GetSystemInfo(Number, Buff);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Number COMP.
01 Buff.
  02 array COMP OCCURS 12 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "GetSystemInfo" USING Number, Buff.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Number
INTEGER Buff(12)
...
Monitor_Call('GetSystemInfo', Number, Buff(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Number
BYTES : Buff(0:23)
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('GetSystemInfo', Number, Buff(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Number : W BLOCK 1
Buff : H BLOCK 14B
ErrCode : W BLOCK 1
GetSystemInfo : EQU 3789 + 262B
...
    CALLG GetSystemInfo, 2. Number, Buff
    IF K GO ERROR
...
ERROR : W1 =: ErrCode                     %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
SAA  0         %Load register A with zero.
LDX  (BUFF     %Address of a 12-word long buffer.
MON  262       %Monitor call GetSystemInfo.
JMP  ERROR     %Error return from monitor call. (If A not 0.)
...            %Normal return.
ERROR, ...     %Error number in register A.
...

BUFF, 0
*+14/          %A 12-word long buffer.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 285*

[↑ Back to Top](#table-of-contents)

---

### 263B - GetDeviceType (GDEVT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the device type, e.g. terminal, floppy disk, mass-storage file, etc. The monitor call also provides information on how to handle the device.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 267B - TimeOut (TMOUT)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Suspends the execution of your program for a given time. The execution then continues after the monitor call. The restart cause is indicated. Avoid using Time Out from ND-500, use ND500TimeOut instead.

- No reserved files or devices are released.
- The execution continues immediately if the program has its restart flag set.
- You may use NoWaitSwitch. Then the program restarts when a break occurs.
- If a program has been rescheduled by the monitor call SET (mon 101, DelayStart) or ABSET (mon 102, StartupTime), this rescheduling will be destroyed when you use TimeOut.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Number of time units to suspend the program. |
| `Param2` | UNKNOWN | I | The type of time units. 1 = basic time units, i.e. 1/50th of a second, 2 = secon |
| `Param3` | UNKNOWN | I | Restart cause. 0 means that the defined time has elapsed. 1 means that a break r |

#### See Also

[SuspendProgram](#104b-suspendprogram-hold), WaitforRestart, and ND500TimeOut

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
NoTimeUnits, UnitType, ReturnStatus : INTEGER2;
...
TimeOut(NoTimeUnits, UnitType, ReturnStatus);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NoTimeUnits  COMP.
01 UnitType     COMP.
01 ReturnStatus COMP.
01 ErrCode      COMP.
...
MONITOR-CALL "TimeOut" USING NoTimeUnits, UnitType, ReturnStatus.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER  NoTimeUnits, UnitType, ReturnStatus
...
Monitor_Call('TimeOut', NoTimeUnits, UnitType, ReturnStatus)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : NoTimeUnits, UnitType, ReturnStatus
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('TimeOut', NoTimeUnits, UnitType, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NoTimeUnits : W BLOCK 1
UnitType : W BLOCK 1
RestartReason : W BLOCK 1
ErrCode : W BLOCK 1
TimeOut : EQU 37B9 + 267B
...
CALLG TimeOut, 3, NoTimeUnits, UnitType, RestartReason
IF K GO ERROR
...
ERROR : W1 =: ErrCode                          %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR                 %Load register A with address of parameter list.
MON 267                  %Monitor call TimeOut.
STA STAT                 %Store status returned.
...
STAT, 0
PAR, TIME                %Number of time units.
BASE                     %Unit base type.
...
TIME, ...
BASE, ...



| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|


Scanned by Jonny Oddene for Sintran Data © 2020
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 541*

[↑ Back to Top](#table-of-contents)

---

### 270B - ReadDiskPage (RDPAG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Reads one or more directory pages. Any page can be read.

- The directory must be reserved with ReserveDir.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Directory index. See GetDirUserIndexes. |
| `Param2` | UNKNOWN | I | Buffer to receive pages. |
| `Param3` | UNKNOWN | I | Address of the destination pages on the disk. |
| `Param4` | UNKNOWN | I | Number of pages to transfer. Each page is 2048 bytes. |

#### See Also

[WriteDiskPage](#271b-writediskpage-wdpag)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirIndex, NoOfPages : INTEGER2;
PageAddr : LONGINT;
Buffer : ARRAY [0..63] OF RECORD...END;
...
ReadDiskPage(DirIndex, Buffer, PageAddr, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirIndex COMP.
01 NoOfPages COMP.
01 PageAddr COMP PIC S9(10).
01 Buffer.
   02 array COMP OCCURS 1024 TIMES.
01 ErrCode COMP.
   ...
MONITOR-CALL "ReadDiskPage" USING DirIndex, Buffer, PageAddr, NoOfPages.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, NoOfPages
INTEGER*4 PageAddr
INTEGER Buffer(1024)
...
Monitor_Call('ReadDiskPage', DirIndex, Buffer(1), PageAddr, NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirIndex, NoOfPages
INTEGER4 : PageAddr
BYTES : Buffer(0:2047)
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('ReadDiskPage', DirIndex, Buffer(0), PageAddr, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirIndex : W BLOCK 1
PageAddr : W BLOCK 1
NoOfPages : W BLOCK 1
Buffer : H BLOCK 1024 % Must start on an even byte address.
ErrCode : W BLOCK 1
ReadDiskPage : EQU 37B9 + 270B

...
CALLG ReadDiskPage, 4,. DirIndex, Buffer, PageAddr, NoOfPages
IF K GO ERROR
...

ERROR : W1 =: ErrCode % ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX % Directory index.
LDX (BUFF % Address of buffer to receive data read.
LDA COUNT % Number of pages to transfer.
COPY SA DD
LDA (PAGNO % Address of double word with disk page address.
MON 270 % Monitor call ReadDiskPage.
JMP ERROR % Error return from monitor call.
... % Normal return.
ERROR, ... % Error number in register A.
...
DIRIX, ...
BUFF, 0
*+4000/      % Make a buffer of 2048 words.
PAGNO, ...  % A double word.
...
COUNT, 2    % Read 2 pages of 1024 words.



| ND-100 and ND-500 | User RT and user SYSTEM | Background programs |
|-------------------|-------------------------|---------------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 397*

[↑ Back to Top](#table-of-contents)

---

### 271B - WriteDiskPage (WDPAG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes to one or more pages in a directory. Any page can be written to.

- The directory must be reserved with ReseveDir.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Directory index. See GetDirUserIndexes. |
| `Param2` | UNKNOWN | I | Address of buffer with pages to transfer. |
| `Param3` | UNKNOWN | I | Address of the destination pages on the disk. |
| `Param4` | UNKNOWN | I | Number of pages to transfer. Each page is 2048 bytes. |

#### See Also

[ReadDiskPage](#270b-readdiskpage-rdpag)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirIndex, NoOfPages : INTEGER2;
Buffer : ARRAY [0..63] OF RECORD...END;
PageAddr : LONGINT;
...
WriteDiskPage(DirIndex, Buffer, PageAddr, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirIndex COMP.
01 NoOfPages COMP.
01 PageAddr COMP PIC S9(10).
01 Buffer.
   02 array COMP OCCURS 1024 TIMES.
01 ErrCode COMP.
   ...
   MONITOR-CALL "WriteDiskPage" USING DirIndex, Buffer, PageAddr, NoOfPages.
   CALL "CbError" USING ErrCode.
   IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, NoOfPages
INTEGER*4 PageAddr
INTEGER Buffer(1024)
...
Monitor__Call('WriteDiskPage', DirIndex, Buffer(1), PageAddr, NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirIndex, NoOfPages
INTEGER4 : PageAddr
BYTES : Buffer(0:2047)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('WriteDiskPage', DirIndex, Buffer(0), PageAddr, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirIndex : W BLOCK 1
PageAddr : W BLOCK 1
NoofPages : W BLOCK 1
Buffer : W BLOCK 1024 %Must start on an even byte address.
ErrCode : W BLOCK 1
WriteDiskPage : EQU 37B9 + 271B
...
CALLG WriteDiskPage, 4, DirIndex, Buffer, PageAddr, NoOfPages
IF K GO ERROR
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX %Directory index.
LDX (BUFF %Address of buffer containing data to be written.
LDA PAGES %Number of pages to transfer.
COPY SA DD
LDA (PAGNO %Address of double word with disk page address.
MON 271 %Monitor call WriteDiskPage.
JMP ERROR %Error return from monitor call.
... %Normal return.
... %Error number in register A.
...
DIRIX, ...
BUFF, 0
*+4000/ %Make a buffer of 2048 words, 2 pages.
PAGNO, ... %A double word.
... %
PAGES, 2 %Transfer 2 pages.



| ND-100 and ND-500 | User RT and user SYSTEM | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 565*

[↑ Back to Top](#table-of-contents)

---

### 272B - DeletePage (DELPG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Deletes pages from a file. Pages between two page numbers are removed.

- The file must be opened.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. |
| `Param2` | UNKNOWN | I | First page to be deleted. |
| `Param3` | UNKNOWN | I | Last page to be deleted. The value -1 means delete to end of the file. |
| `Param4` | UNKNOWN | I | Number of pages deleted. |
| `Param5` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[DeleteFile](#54b-deletefile-mdlfi)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo : INTEGER2;
FirstPage, LastPage, NoOfPages : LONGINT;
...
DeletePage(FileNo, FirstPage, LastPage, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 FirstPage COMP PIC S9(10).
01 LastPage COMP PIC S9(10).
01 NoOfPages COMP PIC S9(10).
    ErrCode COMP.
    ...
    MONITOR-CALL "DeletePage" USING FileNo, FirstPage, LastPage, NoOfPages.
    CALL "CbError" USING ErrCode.
    IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo
INTEGER*4 FirstPage, LastPage, NoOfPages
...
Monitor Call1('DeletePage', FileNo, FirstPage, LastPage, NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNo
INTEGER4 : FirstPage, LastPage, NoOfPages
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('DeletePage', FileNo, FirstPage, LastPage, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
FirstPage : W BLOCK 1
LastPage : W BLOCK 1
NoOfPages : W BLOCK 1
ErrCode : W BLOCK 1
DeletePage : EQU 37B9 + 272B
...
CALLG DeletePage, 4, FileNo, FirstPage, LastPage, NoOfPages
  IF K GO ERROR
...
ERROR : W1 =: ErrCode           %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT FILNO               %File number.
LDA (FIRST              %Address of double word with first page.
LDX (LAST               %Address of double word with last page.
MON 272                 %Monitor call DeletePage.
JMP ERROR               %Error return from monitor call.
STD NODEL               %Normal return, store the number of pages deleted.

ERROR, ...              %Error number in register A.
...
FILNO, ...
FIRST, ...             %A double word.
    ...                %
LAST, ...              %A double word.
    ...                %
NODEL, 0               %A double word.
0                      %


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 139*

[↑ Back to Top](#table-of-contents)

---

### 273B - GetFileName (MGFIL)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the name of a file. You specify the directory index, the user index, and the object index. The file name, the file type, and the version are returned.

- The file need not be open.
- On the ND-100 you may specify a file on a remote computer system. The computers must be connected through a COSMOS network.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Directory index. |
| `Param2` | UNKNOWN | I | User index. |
| `Param3` | UNKNOWN | I | Object index. |
| `Param4` | UNKNOWN | I | File name. |
| `Param5` | UNKNOWN | I | Remote flag. Use 0 for a file on the local computer. Use 1 for a file on a remot |
| `Param6` | UNKNOWN | I | Remote system identification if remote flag is 1. Not returned by ND-500. |
| `Param7` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

FullFileName

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirIndex, UserIndex, ObjectIndex : INTEGER2;
FileName : PACKED ARRAY [0..63] OF CHAR;
...
GetFileName(DirIndex, UserIndex, ObjectIndex, FileName);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirIndex COMP.
01 UserIndex COMP.
01 ObjectIndex COMP.
01 FileName PIC X(64).
01 RemoteFlag COMP.
01 RemoteSystem PIC X(64).
01 ErrCode COMP.
...
MONITOR-CALL "GetFileName" USING DirIndex, UserIndex, ObjectIndex,
                FileName, RemoteFlag, RemoteSystem.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex, UserIndex, ObjIndex, RemoteFlag
CHARACTER FileName*64, RemoteSystem*64
...
Monitor_Call('GetFileName', DirIndex, UserIndex, ObjIndex,
              FileName(1:64), RemoteFlag, RemoteSystem(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirIndex, UserIndex, ObjectIndex, RemoteFlag
BYTES : FileName(0:63), RemoteSystem(0:63)
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetFileName', DirIndex, UserIndex, ObjectIndex, FileName,&
&nbsp;&nbsp;&nbsp;&nbsp;RemoteFlag, RemoteSystem)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirIndex : W BLOCK 1
UserIndex : W BLOCK 1
ObjectIndex : W BLOCK 1
FileName : STRING 64
SysId : STRING 20 %Optional parameter if bit 7 in DirIndex is set.
ErrCode : W BLOCK 1
GetFileName : EQU 37B9 + 273B
...
CALLG GetFileName, 4, DirIndex, UserIndex, ObjectIndex, FileName
IF K GO ERROR
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT INDEX %Left byte: dir index. Right byte: user index.
LDA (REMID) %Remote system identification in register D.
COPY SA DD %Used only if bit 15 in register A is set.
LDA OBJIX %Object index.
LDX (BUFF) %Address of buffer to receive file name.
MON 273 %Monitor call GetFileName.
JMP ERROR %Error return from monitor call.
...  %Normal return.
ERROR, ... %Error number in register A.

INDEX, ... %Set bit 15 if remote file.
OBJIX, ...
BUFF, 0 %
*+32/ %Make a buffer big enough to receive file name.
REMID, 0 %Remote identification string.
*+40/  %32 words for string.


ND-100 and ND-500 | All users | All programs
---|---|---
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 249*

[↑ Back to Top](#table-of-contents)

---

### 274B - GetFileIndexes (FOBJN)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the directory index, the user index, and the object index of a file. These are indexes in the file system. See the SINTRAN III System Supervisor {ND-830003} for more details.

- The file need not be open.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `File name` | Abbreviated file names are less efficient. | I |  |
| `File type` | Not used on the ND-100. Do not include the colon. | I |  |
| `Directory index` | - | I |  |
| `User index` | - | I |  |
| `Object index` | - | I |  |
| `Object index of the next file version` | Equal to object index if no more versions exist. | I |  |
| `Standard Error Code` | See appendix A. | I |  |

#### See Also

[GetAllFileIndexes](#217b-getallfileindexes-guioi), [GetDirUserIndexes](#213b-getdiruserindexes-muidi), [GetDirNameIndex](#243b-getdirnameindex-fdina), [GetDirEntry](#244b-getdirentry-gdien), [GetUserEntry](#44b-getuserentry-ruser), [GetObjectEntry](#215b-getobjectentry-drobj), and GetDefaultDir

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileName : PACKED ARRAY [0..63] OF CHAR;
FileType : PACKED ARRAY [0..3] OF CHAR;
DirIndex, UserIndex, ObjectIndex, NextObjectIndex : INTEGER2;
...
FindFileIndexes(FileName, FileType, DirIndex, UserIndex,
  ObjectIndex, NextObjectIndex); [Note routine name.]

IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileName PIC X(64).
01 FileType PIC X(4).
01 DirIndex COMP.
01 UserIndex COMP.
01 ObjectIndex COMP.
01 NextObjectIndex COMP.
01 ErrCode COMP.
...
MONITOR-CALL "GetfileIndexes" USING FileName, FileType, DirIndex,
  UserIndex, ObjectIndex, NextObjectIndex.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER  FileName*64, FileType*4
INTEGER DirIndex, UserIndex, ObjectIndex, NextObjectIndex
...
Monitor_Call('GetFileIndexes', FileName(1:64), FileType(1:4),
 C      DirIndex, UserIndex, ObjectIndex, NextObjectIndex)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : FileName(0:63), FileType(0:3)
INTEGER : DirIndex, UserIndex, ObjectIndex, NextObjectIndex
...
ON ROUTINEERROR DO
  IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('GetFileIndexes', FileName, FileType, DirIndex, &
             UserIndex, ObjectIndex, NextObjectIndex)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileName : STRING 64
FileType : STRING 4
DirIndex : W BLOCK 1
UserIndex : W BLOCK 1
ObjectIndex : W BLOCK 1
NextObjectIndex : W BLOCK 1
ErrCode : W BLOCK 1
GetFileIndexes : EQU 37B9 + 274B
...
  CALLG GetFileIndexes, 6, FileName, FileType, DirIndex, &
        UserIndex, ObjectIndex, NextObjectIndex
  IF K GO ERROR
...

ERROR : W1 =: ErrCode                     %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDX  (FILE            %Address of file name string.
MON  274              %Monitor call GetFileIndexes.
JMP  ERROR            %Error return from monitor call.
STT  INDEX            %Normal return, store obtained indexes.
STA  OBJIX            %Store object index.
COPY SD DA
STA  NEXTO            %Store object index of next version.
...
ERROR, ...            %Error number in register A.

FILE,   'EXAMPLE:SYMB' %Obtain object index of file EXAMPLE:SYMB.
INDEX,  0             %Directory index in left byte. User index right.
OBJIX,  0
NEXTO,  0



ND-100 and ND-500 | All users | All programs
--- | --- | ---
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 247*

[↑ Back to Top](#table-of-contents)

---

### 275B - SetTerminalName (STRFI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Defines the file name to be used for terminals. This is normally `TERMINAL:`. Background users identify their own terminal with this file name.

- You may use this monitor call more than once. Each file name will identify the terminals.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File name. |
| `Param2` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SetPeripheralName](#234b-setperipheralname-spefi), @SET-TERMINAL-FILE

#### Examples

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 TerminalName  PIC X(64).
01 ErrCode       COMP.
...
MONITOR-CALL "SetTerminalName" USING TerminalName.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER TerminalName*64
...
Monitor_Call('SetTerminalName', TerminalName(1:64))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : TerminalName(0:63)

assemble
ON ROUTINEERROR DO
  IF ErrCode <> 0 THEN ...
ENDON
Monitor_Call('SetTerminalName', TerminalName)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
assemble
FileName : STRINGDATA 'TERMINAL'...
ErrCode : W BLOCK 1
...

SetTerminalName : EQU 37B9 + 275B
  CALLG SetTerminalName, 1, FileName
  IF K GO ERROR
...

ERROR : W1 =. ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
assemble
LDX   (NAME                        %Address of string containing terminal name.
MON   275                          %Monitor call SetTerminalName.
...
NAME, 'TERMINAL'                   %Set terminal name to TERMINAL.


| ND-100 and ND-500 | User SYSTEM | All programs |
|-------------------|-------------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 475*

[↑ Back to Top](#table-of-contents)

---

### 276B - EnableLocal (ELOFU)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

You may log in on remote computers through the COSMOS data network. A key on the terminal returns you to your local computer. This local function can be disabled. You enable it again with EnableLocal.

- You disable the key with DisableLocal.
- The key is disabled when a program terminates.
- The COSMOS CONNECT-TO program tells you which key to use as the LOCAL key.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 277B - DISABLELOCAL (DLOFU)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

You may log in on remote computers through the COSMOS data network. A key on the terminal returns you to your local computer. This monitor call disables the function of this key.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Standard Error Code. See appendix A. |
| `PASCAL` | UNKNOWN | I |  |
| `LocalDisable; [Note routine name.]` | UNKNOWN | I |  |
| `IF ErrCode <> 0 THEN ...` | UNKNOWN | I |  |
| `COBOL` | UNKNOWN | I |  |
| `01 ErrCode COMP.` | UNKNOWN | I |  |
| `...` | UNKNOWN | I |  |
| `MONITOR-CALL "DisableLocal".` | UNKNOWN | I |  |
| `CALL "CbError" USING ErrCode.` | UNKNOWN | I |  |
| `IF ErrCode NOT = 0 GO ...` | UNKNOWN | I |  |
| `FORTRAN` | UNKNOWN | I |  |
| `Monitor_Call('DisableLocal')` | UNKNOWN | I |  |
| `IF (ErrCode .NE. 0) THEN ...` | UNKNOWN | I |  |

#### See Also

[EnableLocal](#276b-enablelocal-elofu)

#### Examples

<details>
<summary><strong>PLANC</strong></summary>

```planc
ON ROUTINEERROR DO
IF ErrCode > 0 THEN ...

ENDON
Monitor_Call('DisableLocal')
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
|        |     |                                 |
|--------|-----|---------------------------------|
| MON    | 277 | %Monitor call DisableLocal.     |
| JMP    | ERROR | %Error return from monitor call. |
|        |     | %Normal return.                 |
| ERROR, | ... | %Error number in register A.    |
|        |     |                                 |


ND-100 | All users | All programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 155*

[↑ Back to Top](#table-of-contents)

---

### 300B - SetEscapeHandling (EUSEL)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Enables user-defined escape handling. When the ESCAPE key is pressed, execution continues at the specified address in your program.

- Disable the user-defined escape handling with StopEscapeHandling.
- The normal escape handling is reset when the program aborts.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 301B - StopEscapeHandling (DUSEL)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Disables user-defined escape handling. The ESCAPE key terminates the program as normal. StartEscapeHandling starts user-defined escape handling.

#### See Also

StartEscapeHandling, DisableEscape

#### Examples

<details>
<summary><strong>COBOL</strong></summary>

```cobol
MONITOR-CALL "StopEscapeHandling".
CALL "CbError" USING ErrCode.
IF ErrCode NOT 0 GO ....
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
Monitor_Call ('StopEscapeHandling')
IF (ErrCode .NE. 0) THEN...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
ON ROUTINEERROR DO
   IF ErrCode <> 0 THEN...
ENDON
...
Monitor_Call('StopEscapeHandling')
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| Command | Code | Description                        |
|---------|------|------------------------------------|
| MON     | 301  | %Monitor call StopEscapeHandling.  |
| JMP     | ERROR| %Error return from monitor call.   |
| ...     | ...  | %Normal return.                    |
| ERROR,  | ...  | %Error number in register A.       |
| ...     | ...  |                                    |


| ND-100 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 491*

[↑ Back to Top](#table-of-contents)

---

### 302B - ONESCLOCALFUNCTION (ELON)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Enables delayed escape and local functions for your terminal. The ESCAPE key then terminates a program unless it is disabled. The key with the local function will terminate connections to remote computers.

#### See Also

[SetEscapeHandling](#300b-setescapehandling-eusel), [OffEscLocalFunction](#303b-offesclocalfunction-eloff), [EnableEscape](#72b-enableescape-eescf), and EnableLocal

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
OnEscLocalFunction;
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrCode COMP.
...
MONITOR-CALL "OnEscLocalFunction".
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
Monitor Call('OnEscLocalFunction')
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
ON ROUTINEERROR DO
IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('OnEscLocalFunction')
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
|         |       |                                           |
|---------|-------|-------------------------------------------|
| MON     | 302   | %Monitor call OnEscLocalFunction.         |
| JMP     | ERROR | %Error return from monitor call.          |
| ...     |       | %Normal return.                           |
| ERROR,  | ...   | %Error number in register A.              |


ND-100 All users Background programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 367*

[↑ Back to Top](#table-of-contents)

---

### 303B - OffEscLocalFunction (ELOFF)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Delays the escape and local functions for your terminal. Then the ESCAPE key or LOCAL key does not terminate a program or remote connection immediately. Their functions are delayed until OnEscLocalFunction is executed.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SetEscapeHandling](#300b-setescapehandling-eusel), OnEscLocalFunction, DisableEscape, and DisableLocal

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
OffEscLocalFunction;
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrCode COMP.
...
MONITOR-CALL "OffEscLocalFunction".
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
Monitor_Call('OffEscLocalFunction')
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('OffEscLocalFunction')
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
|     |     |     |
| --- | --- | --- |
| MON | 303 | %Monitor call OffEscLocalFunction. |
| JMP | ERROR | %Error return from monitor call. |
| ... | ... | %Normal return. |
| ERROR, | ... | %Error number in register A. |


ND-100 | All users | Background programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 363*

[↑ Back to Top](#table-of-contents)

---

### 306B - GetTerminalMode (GTMOD)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the terminal mode. The terminal mode tells how the terminal function, i.e. if all letters are converted to uppercase, if output stops when a full page is displayed, etc.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The logical device number of the terminal. See appendix B. |
| `Param2` | UNKNOWN | I | The terminal mode. The numbers below are used. |
| `Terminal mode` | Capital letters? | Delay after return? | Stop on full page? |
| `0` | No | No | No |
| `1` | Yes | No | No |
| `2` | No | Yes | No |
| `3` | Yes | Yes | No |
| `4` | No | No | Yes |
| `5` | Yes | No | Yes |
| `6` | No | Yes | Yes |
| `7` | Yes | Yes | Yes |
| `8` | No | No | No |
| `9` | Yes | No | No |
| `10` | No | Yes | No |
| `11` | Yes | Yes | No |
| `12` | No | No | Yes |
| `13` | Yes | No | Yes |
| `14` | No | Yes | Yes |
| `15` | Yes | Yes | Yes |
| `Param20` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[TerminalMode](#52b-terminalmode-termo), @TERMINAL-MODE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, TerminalMode : INTEGER2;
...
GetTermMode(DeviceNumber, TerminalMode);  [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber  COMP.
01 TerminalMode  COMP.
01 ErrCode       COMP.
...
MONITOR-CALL "GetTerminalMode" USING DeviceNumber, TerminalMode.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, TerminalMode
...
Monitor_Call('GetTerminalMode', DeviceNumber, TerminalMode)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, TerminalMode
...
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('GetTerminalMode', DeviceNumber, TerminalMode)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Devno        : W DATA 1     % Own terminal
TermMode     : W BLOCK 1
ErrCode      : W BLOCK 1
GetTerminalMode: EQU 37B9+306B

...
CALLG GetTerminalMode,2,DevNo,TerMode
IF K GO Error
...

Error: W1:=ErrCode %ErrorCode in I1 register
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO    %Logical device number.
MON 306      %Monitor call GetTerminalMode.
JMP ERROR    %Error return from monitor call.
STA TMODE    %Normal return, store terminal mode number.
...
ERROR, ...
...
DEVNO, ...
TMODE, 0


| ND-100 and ND-500 | All users | All programs |
|-------------------|-----------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 289*

[↑ Back to Top](#table-of-contents)

---

### 307B - TerminalNoWait (TNOWAI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Switches No Wait on and off. No Wait is useful for input from, and output to, character devices, e.g. terminals. In No Wait, the program does not wait for input or output. Monitor calls like InByte return the error code 3 instead.

- SuspendProgram or WaitForRestart may passivate the program afterwards. The program then restarts when the device detects a break.
- The input buffer must be emptied before passivating the program.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `DevNo` | Logical device number of a character device. See appendix B. | I |  |
| `IOFlag` | Input or output flag. Use 0 for input and 1 for output. | I |  |
| `NoWaitFlag` | No Wait flag. Use 0 to switch No Wait off, and any other number to switch it on. | I |  |
| `RetStatus` | Return status. | I |  |

#### See Also

[InByte](#1b-inbyte-inbt), [OutByte](#2b-outbyte-outbt), and NoWaitSwitch

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DevNo, IOFlag, NoWaitFlag, RetStatus.: INTEGER2;
...
TermNoWait(DevNo, IOFlag, NoWaitFlag, RetStatus); [Note routine name.]
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo       COMP.
01 IOFlag      COMP.
01 NoWaitFlag  COMP.
01 RetStat     COMP.
...
MONITOR-CALL "TerminalNoWait" USING DevNo, IOFlag, NoWaitFlag, RetStat.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER	DevNo, IOFlag, NoWaitFlag, RetStatus
...
Monitor_Call('TerminalNoWait', DevNo, IOFlag, NoWaitFlag, RetStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DevNo, IOFlag, NoWaitFlag, RetStatus
...
Monitor_Call('TerminalNoWait', DevNo, IOFlag, NoWaitFlag, RetStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
IOFlag : W BLOCK 1
NoWaitFlag : W BLOCK 1
TerminalNoWait : EQU 3789 + 307B
...
CALLG TerminalNoWait, 3, DeviceNumber, IOFlag, NoWaitFlag
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR        %Load register A with address of parameter list.
MON 307         %Monitor call TerminalNoWait.
STA STAT        %Store status returned.
JAF ERROR       %Handle error if register A is non-zero.
...
ERROR, ...      %Error: illegal input.
...
STAT, 0
PAR, DEVNO      %Logical device number of a terminal.
IOF             %Input/output flag.
FLAG            %No Wait flag.
...
DEVNO, ...
IOF, ...
FLAG, ...



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 535*

[↑ Back to Top](#table-of-contents)

---

### 310B - In8AndFlag (TBIN8)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Reads 8 bytes from a device, e.g., a terminal. The monitor call applies to the defined echo and break setting. See SetEcho and SetBreak.

- Input of a break character stops the reading. The number of characters read are output. It is output as a negative number if a break character has been read. That is, bit 15 is set to 1.
- This monitor call can be used together with SetBreak and SetEcho.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number. See appendix B.` | UNKNOWN | I |  |
| `Number of bytes read. A negative number means that a break character is read.` | UNKNOWN | I |  |
| `The string of bytes read.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

[In8Bytes](#23b-in8bytes-b8inb), [InUpTo8Bytes](#21b-inupto8bytes-m8inb), [InByte](#1b-inbyte-inbt), InString, [InputString](#503b-inputstring-dvinst), [In4x2Bytes](#63b-in4x2bytes-b41nw), and Out8Bytes

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoOfBytes : INTEGER2;
Buffer : PACKED ARRAY [0..7] OF CHAR;
...
In8AndFlag(DeviceNumber, NoOfBytes, Buffer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber  COMP.
01 NoOfBytes     COMP.
01 Buffer        PIC X(8).
01 ErrCode       COMP.
...
MONITOR-CALL "In8AndFlag" USING DeviceNumber, NoOfBytes, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoOfBytes
CHARACTER Buffer*8
...
Monitor_Call('In8AndFlag', DeviceNumber, NoOfBytes, Buffer(1:8))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoOfBytes
BYTES : Buffer(0:7)
...
ON ROUTINEERROR DO
   IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('In8AndFlag', DeviceNumber, NoOfBytes, Buffer)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoOfBytes : W BLOCK 1
Buffer : STRING 8
In8AndFlag : EQU 37B9 + 310B
...
CALLG In8AndFlag, 3, DeviceNumber, NoOfBytes, Buffer
IF K GO Error,
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT  DEVNO         %Logical device number.
MON  310           %Monitor call In8AndFlag.
JMP  ERROR         %Error return.
STD  BYTES         %Store first 4 bytes read.
COPY SL DA
STA  BYTES+2       %Store next 2 bytes read.
STX  BYTES+3       %Store last 2 bytes read.
STT  COUNT         %Store number of bytes read.
...
ERROR, ...         %Error handling.
...
DEVNO, ...
COUNT, 0
BYTES, 0
0
0
0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 311*

[↑ Back to Top](#table-of-contents)

---

### 311B - WriteDirEntry (WDIEN)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Changes the information about a directory. The complete contents of the directory entry is set. The SINTRAN III System Supervisor (ND-830003) describes the file system in more detail.

- The directory must be entered.
- The directory must be reserved.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | The directory index. See GetDirUserIndexes. |
| `Param2` | UNKNOWN | I | The 48 byte directory entry. See Appendix C. |
| `Param3` | UNKNOWN | I | Standard Error Code. See Appendix A. |

#### See Also

[GetDirUserIndexes](#213b-getdiruserindexes-muidi), and GetDirEntry

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DirIndex : INTEGER2;
DirEntry : ARRAY [0..1] OF RECORD...END;
...
SetDirEntry(DirIndex, DirEntry);    [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DirIndex COMP.
01 DirEntry.
   02 array COMP OCCURS 24 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "WriteDirEntry" USING DirIndex, DirEntry.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DirIndex
INTEGER DirEntry(24)
...
Monitor_Call('WriteDirEntry', DirIndex, DirEntry(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DirIndex
BYTES : DirEntry(0:47)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('WriteDirEntry', DirIndex, DirEntry(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DirIndex : W BLOCK 1
DirEntry : W ARRAY 24
ErrCode : W BLOCK 1
WriteDirEntry : EQU 37B9 + 311B
...
CALLG WriteDirEntry, 2, DirIndex, DirEntry
IF K GO ERROR
...
ERROR : W1 =: ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DIRIX     %Directory index.
LDX (ENTRY)   %Address of buffer containing directory entry.
MON 311       %Monitor call WriteDirEntry.
JMP ERROR     %Error return from monitor call.
...           %Normal return.
ERROR, ...    %Error number in register A.
...
DIRIX, ...
ENTRY, ...    %
...           %A buffer of 24 words.



| ND-100 and ND-500 | User SYSTEM | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 563*

[↑ Back to Top](#table-of-contents)

---

### 312B - CheckMonCall (MOINF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Some monitor calls are optional or only available in later versions of SINTRAN III. This monitor call checks if a monitor call exists in your particular SINTRAN III system. Optional monitor calls are included or left out when SINTRAN III is generated.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Monitor-call number. |
| `Param2` | UNKNOWN | I | Address of the monitor call entry. 0 means not implemented. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
MonCallNumber, MonCallEntry : INTEGER2;
...
CheckMonCall(MonCallNumber, MonCallEntry);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 MonCallNumber COMP.
01 MonCallEntry COMP.
...
MONITOR-CALL "CheckMonCall" USING MonCallNumber, MonCallEntry.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER MonCallNumber, MonCallEntry
...
Monitor_Call('CheckMonCall', MonCallNumber, MonCallEntry)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : MonCallNumber, MonCallEntry
...
Monitor_Call('CheckMonCall', MonCallNumber, MonCallEntry)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
MonCallNumber : W BLOCK 1
MonCallEntry : W BLOCK 1
CheckMonCall : EQU 3789 + 312B
...
CALLG CheckMonCall, 2, MonCallNumber, MonCallEntry
...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA MONNO %Load register A with monitor call number.
MON 312 %Monitor call CheckMonCall.
...
%Return: Monitor call not implemented in system.
STA ENTRY %Skipreturn: Monitor call is implemented.
...

MONNO, ... %Monitor call number.
ENTRY, 0 %Monitor call entry returned if implemented.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 109*

[↑ Back to Top](#table-of-contents)

---

### 313B - InBufferState (IBRISZ)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about an input buffer. The current number of bytes in it, and the number of bytes until a break character, are returned.

- Use ExecutionInfo to get the logical device number for terminals. You can specify 1 for your own terminal.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number.` | See appendix B. | I |  |
| `Number of bytes in the buffer.` |  | I |  |
| `Number of bytes before break` | (zero if no break character in the buffer). | I |  |
| `Standard Error Code.` | See appendix A. | I |  |

#### See Also

[InBufferSpace](#66b-inbufferspace-isize), [ClearInBuffer](#13b-clearinbuffer-cibuf), [OutBufferSpace](#67b-outbufferspace-osize)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, NoInBuffer, NoUntilBreak : INTEGER2;
...
InBufferState(DeviceNumber, NoInBuffer, NoUntilBreak);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNo COMP.
01 NoInBuffer COMP.
01 NoUntilBreak COMP.
01 ErrCode COMP.
...
MONITOR-CALL "InBufferState" USING DeviceNo, NoInBuffer, NoUntilBreak.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, NoInBuffer, NoUntilBreak
...
Monitor_Call('InBufferState', DeviceNumber, NoInBuffer, NoUntilBreak)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, NoInBuffer, NoUntilBreak
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('InBufferState', DeviceNumber, NoInBuffer, NoUntilBreak)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
NoInBuffer : W BLOCK 1
NoUntilBreak : W BLOCK 1
ErrCode : W BLOCK 1
InBufferState : EQU 3789 + 313B
...
    CALLG InBufferState, 2, DeviceNumber, NoUntilBreak
    IF K GO ERROR
    W1 := NoOfBytes
...
ERROR : W1 =: ErrCode                    %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT DEVNO %Logical device number.
MON 313 %Monitor call InBufferState.
JMP ERROR %Error return from monitor call.
STA COUNT %Normal return, store number of bytes in inbuffer.
STX NOBRK %Store number of bytes in inbuffer until break.
...
ERROR, ... %Error number in register A.
...
DEVNO, ...
COUNT, 0
NOBRK, 0



| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 317*

[↑ Back to Top](#table-of-contents)

---

### 314B - DefaultRemoteSystem (SRUSI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets default values for COSMOS remote file access. You can specify the default remote system, the remote user, and the remote user's passwords. The specified values are used when you omit values in a remote file access.

- Empty parameters remove previous default values. Default values are then the local user's name and passwords.
- SetRemoteMode switches the remote search on and off.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞ Remote system name.` |  | I |  |
| `➞ User owning the files in the remote system.` |  | I |  |
| `➞ The user's password.` |  | I |  |
| `➞ The user's project password.` |  | I |  |
| `➞ Standard Error Code. See appendix A.` |  | I |  |

#### See Also

@SET-DEFAULT-REMOTE-SYSTEM, @RESET-DEFAULT-REMOTE-SYSTEM

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SystemName: PACKED ARRAY [0..15] OF CHAR;
UserName, Password, ProjPassword : PACKED ARRAY [0..15] OF CHAR;
...

DefaultRemoteSystem(SystemName, UserName, Password, ProjPassword);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SystemName PIC X(16).
01 UserName   PIC X(16).
01 Password   PIC X(16).
01 ProjPassword PIC X(16).
01 ErrCode COMP.
...

MONITOR-CALL "DefaultRemoteSystem" USING SystemName, UserName,
                                      Password, ProjPassword.

CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER SystemName*16
CHARACTER UserName*16, Password*16, ProjPassword*16
...
Monitor_Call('DefaultRemoteSystem', SystemName(1:16),
             UserName(1:16), Password(1:16), ProjPassword(1:16))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : SystemName(0:15), UserName(0:15), Password(0:15), ProjPassword(0:15)
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('DefaultRemoteSystem', SystemName, UserName, &
&nbsp;&nbsp;&nbsp;&nbsp;Password, ProjPassword)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SystemName : STRINGDATA 'STORE'...
UserName : STRINGDATA 'A-HANSEN'''
Password : STRINGDATA 'MAY'''
ProjPassword : STRINGDATA 'CHEESE'''
ErrCode : W BLOCK 1
DefaultRemoteSystem : EQU 37B9 + 314B
...
&nbsp;&nbsp;&nbsp;&nbsp;CALLG DefaultRemoteSystem, 4, SystemName, UserName, &
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;Password, ProjPassword

&nbsp;&nbsp;&nbsp;&nbsp;IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
| LDX  | (SYS)   | %Address of remote system name. |
| ---  | ---     | ---                              |
| LDT  | (USER)  | %Address of remote user identifier string. |
| LDA  | (PROJP) | %Address of remote project password string. |
| COPY | SA DD   | |
| LDA  | (PASSW) | %Address of remote user password string. |
| MON  | 314     | %Monitor call DefaultRemoteSystem. |
| JMP  | ERROR   | %Error return from monitor call. |
| ...  |         | %Normal return. |
| ERROR, ... |   | %Error number in register A. |

SYS, 'STORE' &nbsp;&nbsp;&nbsp;&nbsp;%Set up STORE as default remote system.
USER, 'A-HANSEN' &nbsp;&nbsp;&nbsp;&nbsp;%Set up A-HANSEN as default remote user.
PASSW, 'MAY' &nbsp;&nbsp;&nbsp;&nbsp;%Set up MAY as default remote user password.
PROJP, 'CHEESE' &nbsp;&nbsp;&nbsp;&nbsp;%Set up CHEESE as default remote project password.


| ND-100 and ND-500 | All users | All programs |
| ---               | ---       | ---          |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 133*

[↑ Back to Top](#table-of-contents)

---

### 315B - LAMUFunction (MLAMU)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Performs various functions on the LAMU system. A LAMU is a logically addressed memory unit. The LAMU system is an extension to the ND-100 segment structure. Programs may address more space than provided by the 3 available segments.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | **Function code.** |
| `Param2` | UNKNOWN | I | 1 means create a LAMU. |
| `Param3` | UNKNOWN | I | 2 means delete a LAMU. |
| `Param4` | UNKNOWN | I | 3 means connect a LAMU to the calling program or another program. |
| `Param5` | UNKNOWN | I | 4 means disconnect a particular LAMU or all LAMUs from the calling program or an |
| `Param6` | UNKNOWN | I | 7 means protect LAMU. |
| `Param7` | UNKNOWN | I | 8 means get LAMU information. |
| `Param8` | UNKNOWN | I | 9 means create system-LAMU. |
| `Param9` | UNKNOWN | I | 10 means create temporary system LAMU and connect it to the calling program. |
| `Param10` | UNKNOWN | I | Function-dependent parameter 2. See the following pages. |
| `Param11` | UNKNOWN | I | Function-dependent parameter 3. See the following pages. |
| `Param12` | UNKNOWN | I | Function-dependent parameter 4. See the following pages. |
| `Param13` | UNKNOWN | I | A Standard Error Code is returned in the A register. See appendix A. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Func, Para2, Para3, Para4 : INTEGER2;
...
LAMUfunction(Func, Para2, Para3, Para4);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Func COMP.
01 Para2 COMP.
01 Para3 COMP.
01 Para4 COMP.
...
MONITOR-CALL "LAMUfunction" USING Func, Para2, Para3, Para4.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Func, Para2, Para3, Para4
...
Monitor_Call('LAMUfunction', Func, Para2, Para3, Para4)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER . Func. Para2, Para3, Para4
ON ROUTINEERROR DO
  [IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('LAMUfunction', Func. Para2, Para3, Para4
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA   {PAR      % Load register A with address of parameter list.
MON   315       % Monitor call LAMUfunction.
JMP   ERROR     % Error return from monitor call.
               % Normal return.
ERROR           % Error number in register A.

PAR, FUNC       % Function.
PARA2
PARA3
PARA4

FUNC,
PARA2,
PARA3,
PARA4,


| ND-100 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 331*

[↑ Back to Top](#table-of-contents)

---

### 316B - SetRemoteAccess (SRLMO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Switches remote file access on and off. The COSMOS network allows you to access files in remote computers directly. Use DefaultRemoteSystem or QSET-DEFAULT-REMOTE-SYSTEM to specify a default remote system. If a file does not exist in the local system, the default remote system is searched. SetRemoteAccess switches this function on and off.

- You may include a remote system identification in the file name. Only the specified system is searched.
- This monitor call is only available with COSMOS.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞` | Remote mode flag. Use 0 to switch remote mode off. Switch it on with 1. | I |  |
| `⬅` | Standard Error Code. See appendix A. | I |  |

#### See Also

QSET-LOCAL-MODE or QSET-REMOTE-MODE

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Mode : INTEGER2;
...
SetRemoteAccess(Mode);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Mode COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SetRemoteAccess" USING Mode.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Mode
...
Monitor Call('SetRemoteAccess', Mode)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Mode
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SetRemoteAccess', Mode)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Mode : W BLOCK 1
SetRemoteAccess : EQU 37B9 + 316B
...
CALLG SetRemoteAccess, 1, Mode
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA | MODE | %Local / Remote Flag.
--- | --- | ---
MON | 316 | %Monitor call SetRemoteAccess.
... | ... | ...

| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 465*

[↑ Back to Top](#table-of-contents)

---

### 317B - ExecuteCommand (UECOM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Executes a SINTRAN III command. Specify the command name and the parameters as a text string.

- An error message is output if an error occurs. The program does not terminate.
- Some commands may destroy your program. Commands which affect your program’s memory area should be used with care.
- Some commands have output, e.g. @LIST-FILES. This is displayed on the terminal.
- Use SuspendProgram to wait a second between two ExecuteCommands which depend on each other, e.g. CreateFile and OpenFile.
- It may be advisable to use @enable-escape before this call, to avoid having problems terminating some commands.

#### See Also

[CallCommand](#70b-callcommand-commnd), SetCommandBuffer. CallCommand terminates the program if an error occurs

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Command : PACKED ARRAY [0..34] OF CHAR;
...
ExecuteCommand(Command);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Command    PIC X(35).
...
MONITOR-CALL "ExecuteCommand" USING Command.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER Command*35
...
Monitor_Call('ExecuteCommand', Command(1:35))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Command(0:35)
...
Monitor_Call('ExecuteCommand', Command)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Command : STRING 35
ExecuteCommand : EQU 37B9 + 317B
...
CALLG ExecuteCommand, 1, Command
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
|        |                         |
|--------|-------------------------|
| LDA    | (CMND                 | %Address of string with SINTRAN command. |
| MON    | 317                   | %Monitor call ExecuteCommand.            |
| ...    |                       |                                          |
| CMND,  | 'CLOSE-FILE 102'      | %Execute CLOSE-FILE 102.                 |


| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 181*

[↑ Back to Top](#table-of-contents)

---

### 322B - GetSegmentNo (GSGNO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the number of a segment in the ND-100. You specify the segment name. Segment names are created with the RT LOADER or when a program is dumped reentrant. See @DUMP-PROGRAM-REENTRANT.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment name. |
| `Param2` | UNKNOWN | I | Segment number. Nonexistent segment names return a negative value. |

#### See Also

[GetSegmentEntry](#53b-getsegmententry-rsegm)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNumber : INTEGER2;
SegmentName : PACKED ARRAY [0..5] OF CHAR;
...
GetSegmentNo(SegmentName, SegmentNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNumber COMP.
01 SegmentName PIC X(6).
...
MONITOR-CALL "GetSegmentNo" USING SegmentName, SegmentNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNumber
CHARACTER SegmentName*6
...
Monitor_Call('GetSegmentNo', SegmentName(1:6), SegmentNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNumber
BYTES : SegmentName(0:5)
...
Monitor_Call('GetSegmentNo', SegmentName, SegmentNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNumber : W BLOCK 1
SegmentName : STRINGDATA 'EXSEG' %Get number of segment EXSEG.
ErrCode : W BLOCK 1
GetSegmentNo : EQU 37B9 + 322B
...
CALLG GetSegmentNo, 1, SegmentName
IF K GO ERROR
W1 =: SegmentNumber
...
ERROR : W1 =: ErrCode %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR %Load register A with address of parameter list.
MON 322 %Monitor call GetSegmentNo.
JAN ERROR %Handle error if register A is negative.
STA SEGNO %Store segment number.
...
ERROR, ... %No segment has EXSEG as name.
...
SEGNO, 0
PAR, SEGNAM %String containing segment name.
...
SEGNAM, 'EXSEG' %Obtain segment number of EXSEG.


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 279*

[↑ Back to Top](#table-of-contents)

---

### 323B - SegmentOverlay (SPLRE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green)

Used to build multisegment programs in the ND-100. It is mainly for internal use. A new reentrant segment and two address areas in this segment are specified.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Segment number. The segment must be reentrant. |
| `Param2` | UNKNOWN | I | Page number of first page in address-area 1. Normally in the program bank. |
| `Param3` | UNKNOWN | I | Number of pages in address-area 1. |
| `Param4` | UNKNOWN | I | Page number of first page in address-area 2. Normally in the data bank. |
| `Param5` | UNKNOWN | I | Number of pages in address-area 2. |
| `Param6` | UNKNOWN | I | Clear flag. If not 0, the earlier specified overlay areas are cleared. Use 0 the |
| `Param7` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNo, Page1A1, NoPageA1, Page1A2, NoPageA2, ClearFlag : INTEGER2;
...
SegmentOverlay(SegmentNo, Page1A1, NoPageA1, Page1A2, NoPageA2, ClearFlag);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNo COMP.
01 Page1A1 COMP.
01 NoPageA1 COMP.
01 Page1A2 COMP.
01 NoPageA2 COMP.
01 ClearFlag COMP.
...
MONITOR-CALL "SegmentOverlay" USING SegmentNo, Page1A1, NoPageA1,
                                  Page1A2, NoPageA2, ClearFlag.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNo, Page1A1, NoPageA1, Page1A2, NoPageA2, ClearFlag
...
Monitor_Call('SegmentOverlay', SegmentNo, Page1A1, NoPageA1,
                               Page1A2, NoPageA2, ClearFlag)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNo, Page1A1, NoPageA1, Page1A2, NoPageA2, Clearflag
...
Monitor_Call('SegmentOverlay', SegmentNo, Page1A1, NoPageA1, &
             Page1A2, NoPageA2, Clearflag)
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA   (PAR  %Load register A with address of parameter list.
MON   323   %Monitor call SegmentOverlay.
...
PAR,  SEGNO %Segment number.
AREA1       %First page in area 1.
NUM1        %Number of pages in area 1.
AREA2       %First page in area 2.
NUM2        %Number of pages in area 2.
CLEAR       %Clear flag.
...
SEGNO, ...
AREA1, ...
NUM1, ...
AREA2, ...
NUM2, ...
CLEAR, ...


| ND-100   | All users      | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 429*

[↑ Back to Top](#table-of-contents)

---

### 324B - OctobusFunction (OCTIO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Performs various functions on an old Octobus (earlier than version 3).

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | **Function number:** |
| `Param2` | UNKNOWN | I | 0 means kick. |
| `Param3` | UNKNOWN | I | 1 means wait for kick. |
| `Param4` | UNKNOWN | I | 5 means read Octobus status. |
| `Param5` | UNKNOWN | I | 6 means "Who am I". |
| `Param6` | UNKNOWN | I | **Logical device number.** |
| `Param7` | UNKNOWN | I | **Function dependent parameter.** |
| `Param8` | UNKNOWN | I | Function 0 returns the destination station. |
| `Param9` | UNKNOWN | I | Function 5 and 6 return a status value. |
| `Param10` | UNKNOWN | I | The last transmit status is returned in bit 31:16. |
| `Param11` | UNKNOWN | I | The hardware status is in bit 15:0. |
| `Param12` | UNKNOWN | I | Function 1 does not use this parameter. |

#### Examples

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Parameter   | Type     | Block | Description                                     |
|-------------|----------|-------|-------------------------------------------------|
| Func        | W        | BLOCK 1 |                                                 |
| DevNo       | W        | BLOCK 1 |                                                 |
| Par3        | W        | BLOCK 1 | May be destination station or return value.     |
| Status      | W        | BLOCK 1 |                                                 |
| OctobusFunction | EQU  | 37B9 + 324B |                                           |

CALLG OctobusFunction, 3, Func, DevNo, Par3
IF K GO Error
W1 =: Status
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR
%Load register A with address of parameter list.
MON 324
%Monitor call OctobusFunction.

PAR, FUNC
LDN
STAT

FUNC, O
%Function.
LDN, ...
%Logical device number.
STAT, ...
%Status.


| ND-100 and ND-500 | All users    | All programs |
|-------------------|--------------|--------------|
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 361*

[↑ Back to Top](#table-of-contents)

---

### 325B - BATCHMODEECHO (MBECH)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Controls echo of input and output if the program is executed in a batch or mode job. The purpose is to allow the program to communicate with the terminal in mode jobs.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 326B - LogInStart (MLOGI)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Allowed in RT programs (ring ≥ 1), and in all programs run by users RT and SYSTEM.

- Parameter 1: Function code.
- Parameter 2: LAMU ID (identification) number.
- Specify the LAMU to get information about.
- Parameter 3: LAMU information. Three word array containing the following information:
- Word 1: the first physical page used by the LAMU
- Word 2: the number of pages in the LAMU
- Word 3: the LAMU protection information
- Parameter 4: Dummy.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 327B - FileSystemFunction (FSMTY)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Multifunction monitor call to make sure that an uncontrolled system stop does not leave the file system inconsistent. The file index block of an open file is written back to the disk.

- This monitor call is particularly useful for SIBAS and ISAM applications.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 330B - TerminalStatus (TERST)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets information about a terminal. The user logged in, the time logged in, the CPU time used, the job being executed, and more is returned.

- You may use the monitor call for batch jobs.
- This call can only be used from background programs, not RT programs.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | ⟶ Logical device number of a terminal. Use 1 for your own terminal. |
| `Param2` | UNKNOWN | I | ⟵ Information about the terminal. The 44 bytes are used as follows: |
| `0:15` | Name of user logged in. Terminated by '.' if less than 16 bytes. | I |  |
| `16:17` | Mode. 1 means command. 2 means program running. | I |  |
| `18:19` | State. -1 means no one logged in. 0 means idle batch processor. 1 means active terminal. | I |  |
| `20:21` | CPU time used in minutes. | I |  |
| `22:23` | Time logged in in minutes. | I |  |
| `24:43` | The last command executed. Terminated by an ' (apostrophe). | I |  |
| `Param9` | UNKNOWN | I | ⟵ Standard Error Code. See appendix A. |

#### See Also

@TERMINAL-STATUS

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber : INTEGER2;
Buffer : ARRAY [0..1] OF RECORD...END;
...
TermStatus(DeviceNumber, Buffer);   [Note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 Buffer.
   02 array COMP OCCURS 22 TIMES.
01 ErrCode COMP.
...
MONITOR-CALL "TerminalStatus" USING DeviceNumber, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber
INTEGER Buffer(22)
...
Monitor_Call('TerminalStatus', DeviceNumber, Buffer(1))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber
BYTES : Buffer(0:43)
...
ON ROUTINEERROR DO
    IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('TerminalStatus', DeviceNumber, Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNumber : W BLOCK 1
Buffer : H BLOCK 25B
ErrCode : W BLOCK 1
TerminalStatus : EQU 37B9 + 330B
...
CALLG TerminalStatus, 2, DeviceNumber, Buffer
IF K GO ERROR
...
ERROR : W1 =: ErrCode
       %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT LDN       %Load register T with the logical device number.
LDA (BUF      %Load register A with address of return buffer.
MON 330       %Monitor call TerminalStatus.
STA STAT      %Returned status.
...
LDN, 1
BUF, 0
*+26/         %Buffer of 44 bytes.
STAT, 0


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 537*

[↑ Back to Top](#table-of-contents)

---

### 332B - TerminalLineInfo (TREPP)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about a terminal line. You may also enable programs to continue in spite of errors on the terminal line.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | **Function code.** Use 1 to enable program to continue after line errors. 0 disa |
| `Param2` | UNKNOWN | I | **Logical device number of a terminal.** Use 1 for your own terminal. |
| `Param3` | UNKNOWN | I | **Terminal line information.** Dummy for function code 0 and 1. The following bi |
| `Param4` | UNKNOWN | I | Bit 0: Set if the terminal line does not function. |
| `Param5` | UNKNOWN | I | 1: Set if logout waits for ExitFromProgram. |
| `Param6` | UNKNOWN | I | 2: Overflow in input buffer. Some characters are lost. |
| `Param7` | UNKNOWN | I | 3: Parity error on input. |
| `Param8` | UNKNOWN | I | 4: Framing error on input. |
| `Param9` | UNKNOWN | I | **Standard Error Code.** See appendix A. |

#### See Also

[TerminalStatus](#330b-terminalstatus-terst)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FuncCode, DeviceNo, ReturnInfo : INTEGER2

TermLineInfo(FuncCode, DeviceNo, ReturnInfo); [note routine name.]
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FuncCode COMP.
01 DeviceNo COMP.
01 ReturnInfo COMP.
01 ErrCode COMP.

MONITOR-CALL "TerminalLineInfo" USING FuncCode, DeviceNo, ReturnInfo.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FuncCode, DeviceNo, ReturnInfo

Monitor Call('TerminalLineInfo', FuncCode, DeviceNo, ReturnInfo)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FuncCode, DeviceNo, ReturnInfo
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('TerminalLineInfo', FuncCode, DeviceNo, ReturnInfo)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FuncCode : W BLOCK 1
DeviceNo : W BLOCK 1
ReturnInfo : W BLOCK 1
ErrCode : W BLOCK 1
TerminalLineInfo : EQU 37B9 + 332B
...
&nbsp;&nbsp;&nbsp;&nbsp;CALLG TerminalLineInfo, 2, FuncCode, DeviceNo
&nbsp;&nbsp;&nbsp;&nbsp;IF K GO ERROR
&nbsp;&nbsp;&nbsp;&nbsp;W1 := ReturnInfo
...
ERROR : W1 =: ErrCode


%ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT LDN %Load register T with the logical device number.
LDA FUNC %Load register A with function code.
MON 332 %Monitor call TerminalStatus.
STA STAT %Returned status. Standard Error Code.
STA LINFO %Returns here if function code = 2 and no
         %errors have occurred.
...
LDN, 1
FUNC, 0
LINFO, 0
STAT, 0


| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 531*

[↑ Back to Top](#table-of-contents)

---

### 333B - DMAFunction (UDMA)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Monitor call 333B - DMAFunction

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 334B - GetErrorMessage (GETXM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets a SINTRAN III error message text. Appendix A shows the messages connected to each error number. The error number is input. The program continues.

- This monitor call is convenient for advanced use of the terminal screen. For example, you may output the error message in inverse video at the bottom line.
- Do not input error number 0.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞ Error number of the message to be printed. Use octal numbers.` |  | I |  |
| `➞ Error message text.` |  | I |  |
| `← Standard Error Code. See appendix A.` |  | I |  |

#### See Also

[WarningMessage](#64b-warningmessage-ermsg), ErrorMessage. ErrorMessage writes out the error message, terminates the program

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrorNo : INTEGER2;
Buffer : PACKED ARRAY [0..127] OF CHAR;
...
GetErrorMessage(ErrorNo, Buffer);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrCode COMP.
01 Buffer PIC X(128).
01 ErrorNo COMP.
...
MONITOR-CALL "GetErrorMessage" USING ErrorNo, Buffer.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrorNo
CHARACTER Buffer*128
...
Monitor_Call('GetErrorMessage', ErrorNo, Buffer(1:128))
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrorNo
BYTES : Buffer(0:127)
...
ON ROUTINEERROR DO
    IF ErrCode > < 0 THEN ...
ENDON
Monitor_Call('GetErrorMessage', ErrorNo, Buffer)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrCode : W BLOCK 1
Buffer : W BLOCK 100
ErrorNo : W BLOCK 1
GetErrorMessage : EQU 37B9 + 334B
...
CALLG GetErrorMessage, 2, ErrorNo, Buffer
IF K GO ERROR
...
ERROR : W1 =: ErrCode  %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA ERRNO   %Error number of error message to be printed.
LDX (BUF    %Address of buffer to receive the error message.
MON 334     %Monitor call GetErrorMessage.
STA STAT    %Returns here if error occurs.
...         %Normal return here.
ERRNO, ...
BUF, ...
*+100/
STAT, ...


| ND-100 and ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 243*

[↑ Back to Top](#table-of-contents)

---

### 335B - TRANSFERDATA (EXABS)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Transfers data between physical memory and a mass-storage device, e.g. a disk. You may perform various device control functions. This monitor call is mainly used by the operating system itself.

#### See Also

[DataTransfer](#131b-datatransfer-abstr), [ReadFromFile](#117b-readfromfile-rfile), and WriteToFile

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNumber, Func, RetStatus : INTEGER2;
MemAddr, BlockAddr, NoOfBlocks : LONGINT;
...
TransferData(DeviceNumber, Func, MemAddr, BlockAddr, NoOfBlocks, RetStatus);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DeviceNumber COMP.
01 Func COMP.
01 ReturnStatus COMP.
01 MemAddr COMP PIC S9(10).
01 BlockAddr COMP PIC S9(10).
01 NoOfBlocks COMP PIC S9(10).
...
MONITOR-CALL "TransferData" USING DeviceNumber, Func, MemAddr,
                           BlockAddr, NoOfBlocks, ReturnStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNumber, Func, ReturnStatus
INTEGER*4 MemAddr, BlockAddr, NoOfBlocks
...
Monitor_Call('TransferData', DeviceNumber, Func, MemAddr,
                          BlockAddr, NoOfBlocks, ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNumber, Func, ReturnStatus
INTEGERA : MemAddr, BlockAddr, NoOfBlocks
...
Monitor_Call('TransferData', DeviceNumber, Func, MemAddr, &
&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;BlockAddr, NoOfBlocks, ReturnStatus)
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDT     DEVNO     %Logical device number.
LDA     (PAR      %Load register A with address of parameter list.
MON     335       %Monitor call DataTransfer.
JAN     ERROR     %Error if register A is negative.
...               %Continue with processing.
ERROR, ...        %Error number in register A.


| DEVNO, ... |       |
|------------|-------|
| PAR, FUNC  | %Function code etc. |
|            | DMEM   %Memory address. |
|            | BLOCK  %Block address. |
|            | NOBLK  %Number of blocks to transfer. |

| FUNC, ... |       |
|-----------|-------|
| DMEM, 0:0 | %These three parameters are 32-bit long. Use |
| BLOCK, 0:0| %the most significant word for 16-bit parameters. |
| NOBLK, 0:0| % |


ND-100 | User RT and user SYSTEM | RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 545*

[↑ Back to Top](#table-of-contents)

---

### 336B - TERMINAL (IOMTY)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

This I/O multifunction monitor call is used to change the attributes of terminal and terminal access device (TAD) input/output. It is also used to configure NET/One interfaces and SCSI disks.

This monitor call needs a varying number of input and output parameters depending upon function. All parameters are therefore placed in an array.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Function code (more details on page 496). |
| `Param2` | UNKNOWN | I | Length of function parameter array (must be greater than or equal number of inpu |
| `Param3` | UNKNOWN | I | Function parameter array. (More details are given on page 496.) |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SetBreak](#4b-setbreak-brkm), [SetEcho](#3b-setecho-echom)

#### Examples

<details>
<summary><strong>MAC</strong></summary>

```mac
| Instruction | Operand | Comment |
|-------------|---------|---------|
| LDT         | NTERM   | %Load register T with new terminal. |
| SAA         | 0       |  |
| COPY        | SA DL   | %Copy function code 0 to the L register |
| LDA         | 0       | %No translation to uppercase letters. |
| MON         | 336     | %TerminalFunction with function code 0. |
| STA         | ERROR   | %Returns here if errors. |

- **NTERM, ...**
  %Logical device number of a terminal.

- **ERROR, ...**

| ND-100 and ND-500 | All users | Background programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 503*

[↑ Back to Top](#table-of-contents)

---

### 337B - ChangeSegment (SPCHG)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Changes the segment and the page table your program uses. The monitor call is similar to JumpToSegment and ExitFromSegment. In addition, you may change the two page tables in use. The segment numbers are restricted to 8-bits (values 0-255). SegmentFunction (MON 341) is the equivalent monitor call for version K of SINTRAN III.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Pascal` | Cobol | Fortran |  |
| `Not available.` | Not available. | Not available. |  |

#### See Also

[JumpToSegment](#132b-jumptosegment-mcall), [ExitFromSegment](#133b-exitfromsegment-mexit), and SegmentFunction

#### Examples

<details>
<summary><strong>MAC</strong></summary>

```mac
%Code on initial segment. JumpToSegment is performed.

| Instruction   | Description                                       |
|---------------|---------------------------------------------------|
| SAA 3         | %Page table 1 and 2 in A register.                |
| COPY SA DD    | %Copy to D register.                              |
| LDT (PAR      | %Address of parameter list in T register.         |
| MON 337       | %ChangeSegment with bit 15 in D register = 0.     |
| CONT, ...     | %Execution continues here after ExitFromSegment.  |
| PAR, SUBRO    | %Start address on the new segment.                |
| 100201        | %Segment 200B and 201B.                           |

%Code on new segment. ExitFromSegment is performed.

| Instruction     | Description                                   |
|-----------------|-----------------------------------------------|
| SUBRO, STT SAVET| %Save T, L, and D registers.                  |
| COPY SL DT      |                                               |
| STT SAVEL       |                                               |
| COPY SD DT      |                                               |
| STT SAVED       |                                               |
| ...             |                                               |
| LDT SAVED       | %Restore D register                           |
| COPY ST DD      |                                               |
| BSET ONE DD 170 | %Set bit 15 for ExitFromSegment function.     |
| LDT SAVEL       | %Restore L and T registers.                   |
| COPY ST DL      |                                               |
| LDT SAVET       |                                               |
| MON 337         | %ChangeSegment to return to original segment. |


| ND-100 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 107*

[↑ Back to Top](#table-of-contents)

---

### 340B - READSYSTEMRECORD (RSREC)

![RT](https://img.shields.io/badge/RT-Yes-blue)

Used to read the system record into a buffer.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Record type (A-reg.): 1 = RT-description. |
| `Param2` | UNKNOWN | I | RT-description address or segment number (T-reg.). |
| `Param3` | UNKNOWN | I | Buffer address (X-reg.). Address of buffer to receive system record. This buffer |
| `Param4` | UNKNOWN | I | FØrmat (only for ND-500): 0 = return information on 16-bit integer format |
| `Param5` | UNKNOWN | I | If parameter 1 =1, then A-register returns the number of devices connected with  |
| `Param6` | UNKNOWN | I | Standard Error Code. See appendix A. |
| `PASCAL` |  | I |  |
| `Not available.` |  | I |  |
| `COBOL` |  | I |  |
| `Not available.` |  | I |  |
| `FORTRAN` |  | I |  |
| `Not available.` |  | I |  |

#### Examples

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| RECTYPE | W BLOCK 1  | % RT-description/segment entry            |
|---------|------------|-------------------------------------------|
| ADDNO   | W BLOCK 1  | % address of RT-descr/ND-100 segment      |
| ENTRY   | H BLOCK 38 | % buffer, 38 words                        |
| FORMAT  | W BLOCK 1  | % format (16/32-bit integer)              |
| RSREC   | EQU 37000000340B | % RSREC = MON 340                   |

CALLG RSREC,4,RECTYPE,ADDNO,ENTRY,FORMAT % MON RSREC with 4 parameters
IF K GO ERROR % on error return, W1 = error code
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
- LDA RECTP  %Load A register with the record type.
- LDT RTADR  %Load T register with RT address or segment no.
- LDX (BUFFR %Load X register with address of the buffer.
- MON 340    %Monitor call ReadSystemRecord.
- JMP ERROR  %Error return.
  ...        %Normal return.

| RECTP. | 1    |
|--------|------|
| RTADR. | 54214|
| BUFFR. | 0 *+46/ %Reserve 46 (38 words). |


ND-100 and ND-500  User RT and user SYSTEM  RT programs
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 405*

[↑ Back to Top](#table-of-contents)

---

### 341B - SegmentFunction (SGMTY)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

This is a multifunction monitor call used to change the active segments of a program, or the page index tables used by a program.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Function code. |
| `Param2` | UNKNOWN | I | 0 = JumpToSegment (MCALL) without PIT change |
| `Param3` | UNKNOWN | I | 1 = ExitFromSegment (MEXIT) without PIT change |
| `Param4` | UNKNOWN | I | 2 = JumpToSegment (MCALL) with PIT change |
| `Param5` | UNKNOWN | I | 3 = ExitFromSegment (MEXIT) with PIT change |
| `Param6` | UNKNOWN | I | 4 = REMSG (remove segment entered by ENTSEG) |
| `Param7` | UNKNOWN | I | Start/return address |
| `Param8` | UNKNOWN | I | New segment 1 |
| `Param9` | UNKNOWN | I | New segment 2 |
| `Param10` | UNKNOWN | I | New page index tables |
| `Function` | Register | I |  |
| `Functions 0 and 2` | (MCALL): | I |  |
| `T` | Old segment 1 | I |  |
| `D` | Old segment 2 | I |  |
| `L` | Return address | I |  |
| `X` | Old PITs if PITs changed (function 2) | I |  |
| `Functions 1 and 3` | (MEXIT): No register change | I |  |
| `Function 4` | (REMSG): A  = 0 : ok | I |  |
| `Param19` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[ChangeSegment](#337b-changesegment-spchg), [JumpToSegment](#132b-jumptosegment-mcall), [ExitFromSegment](#133b-exitfromsegment-mexit)

#### Examples

<details>
<summary><strong>MAC</strong></summary>

```mac
| Command | Description |
|---------|-------------|
| LDT (PARLI | %Load T register with address of parameter list. |
| MON 341 | %Monitor call SegmentFunction. |
| JMP ERROR | %Error return. |
| ... | %Normal return. |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 427*

[↑ Back to Top](#table-of-contents)

---

### 400B - ErrorReturn (MACROE)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Terminates the program and sets an error code. The error code can be tested by the commands IF-ERROR-MACRO-STOP and IF-ERROR-FULL-STOP commands in the ND-500 Monitor. See the manual ND Linker User Guide and Reference Manual (ND-860289) for details about macros.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `PASCAL` |  | I |  |
| `COBOL` | UNKNOWN | I |  |
| `MONITOR-CALL "ErrorReturn".` | UNKNOWN | I |  |
| `FORTRAN` | UNKNOWN | I |  |
| `Monitor_Call('ErrorReturn')` | UNKNOWN | I |  |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
RTProgram, BasicTimeUnits: INTEGER2;
...
ExactDelayStart(RTProgram, BasicTimeUnits);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 RTProgram COMP.
01 BasicTimeUnits COMP.
...
MONITOR-CALL "ExactDelayStart" USING RTProgram, BasicTimeUnits.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER RTProgram, BasicTimeUnits
...
Monitor_Call('ExactDelayStart', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : RTProgram, BasicTimeUnits
...
Monitor_Call('ExactDelayStart', RTProgram, BasicTimeUnits)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
RTProgram : W BLOCK 1
BasicTimeUnits : W BLOCK 1
ExactDelayStart : EQU 37B9 + 126B
...
CALLG ExactDelayStart, 2, RTProgram, BasicTimeUnits
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
LDA (PAR   %Load register A with address of parameter list.
MON 126    %Monitor call ExactDelayStart.
...
PAR, RTPRO %Address of RT description.
TIME       %Number of basic time units the program
...        % is to stay in the time queue.
RTPRO, ...
TIME, ...  %A double word.
...        %


| ND-100 and ND-500 | User RT and user SYSTEM | RT programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 173*

[↑ Back to Top](#table-of-contents)

---

### 401B - DisAssemble (DIASS)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Disassembles one machine instruction on the ND-500. Output is the instruction in ASSEMBLY-500 language. See the manual ND-500 ASSEMBLER Reference Manual (ND-860113).

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Program address. |
| `Param2` | UNKNOWN | I | The assembly instruction. |
| `Param3` | UNKNOWN | I | Maximum number of characters in the assembly instruction. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProgPointer, MaxNoOfChar : LONGINT;
ReturnString : PACKED ARRAY [0..79] OF CHAR;
...
DisAssemble(ProgPointer, ReturnString, MaxNoOfChar);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProgPointer COMP.
01 MaxNoOfChar COMP.
01 ReturnString PIC X(100).
...
MONITOR-CALL "DisAssemble" USING ProgPointer, ReturnString, MaxNoOfChar.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ProgPointer, MaxChar
CHARACTER RetString*80
...
Monitor_Call('DisAssemble', ProgPointer, RetString(1:80), MaxChar)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ProgPointer, MaxNoOfChar
BYTES : ReturnString(0:79)
...
Monitor_Call('DisAssemble', ProgPointer, ReturnString, MaxNoOfChar)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProgPointer : W BLOCK 1
MaxNoOfChar : W BLOCK 1
ReturnString : STRINGDATA
DisAssemble : EQU 37B9 + 401B
...
CALLG DisAssemble, 3, ProgPointer, ReturnString, MaxNoOfChar
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 159*

[↑ Back to Top](#table-of-contents)

---

### 402B - GetInputFlags (RFLAG)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

ND-100 and ND-500 programs may communicate through two 32-bit flag arrays. You can use the flags as you wish. GetInputFlags reads the input flags. The ND-100 sets these flags with the monitor call ND500Function. See the manual ND Linker User Guide and Reference Manual (ND-860289).

- You get the last values written to the flags. There is no queue.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Flag values as a 32-bit integer. |

#### See Also

[SetOutputFlags](#403b-setoutputflags-wflag), ND500Function

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Value : LONGINT;
...
GetInputFlags(Value);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Value COMP.
...
MONITOR-CALL "GetInputFlags" USING Value.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Value
...
Monitor_Call('GetInputFlags', Value)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Value
...
Monitor_Call('GetInputFlags', Value)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Value : W BLOCK 1
GetInputFlags : EQU 37B9 + 402B
...
CALLG GetInputFlags, 1, Value
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 251*

[↑ Back to Top](#table-of-contents)

---

### 403B - SetOutputFlags (WFLAG)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

ND-100 and ND-500 programs may communicate through two 32-bit flag arrays. You can use the flags as you want. SetOutputFlags writes to the output flags. The ND-100 reads these flags with the monitor call ND500Function. See the manual ND Linker User Guide and Reference Manual (ND-860289).

- You store the last values written to the flags. There is no queue.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `➞` | Flag values as a 32-bit integer. | I |  |

#### See Also

[SetOutputFlags](#403b-setoutputflags-wflag)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Value : LONGINT;
...
SetOutputFlags(Value);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Value COMP.
...
MONITOR-CALL "SetOutputFlags" USING Value.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Value
...
Monitor_Call('SetOutputFlags', Value)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Value
...
Monitor_Call('SetOutputFlags', Value)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Value : W BLOCK 1
SetOutputFlags : EQU 37B9 + 403B
...
CALLG SetOutputFlags, 1, Value
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 455*

[↑ Back to Top](#table-of-contents)

---

### 404B - FixIOArea (IOFIX)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Fixes an address area in a domain in physical memory. The memory area can be used for later input and output monitor calls, e.g. ReadFromFile or WriteTofile.

- Use MemoryUnFix to release the pages.
- The pages are released when the domain terminates.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Start address in the domain. |
| `Param2` | UNKNOWN | I | Number of bytes to fix. |

#### See Also

[FixScattered](#115b-fixscattered-fix), [FixContiguous](#160b-fixcontiguous-fixc), FixInMemory, and MemoryAllocation

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FirstAddress, SizeOfArea : LONGINT;
...
FixIOArea(FirstAddress, SizeOfArea);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FirstAddress COMP.
01 SizeOfArea COMP.
...
MONITOR-CALL "FixIOArea" USING FirstAddress, SizeOfArea.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FirstAddress, SizeOfArea
...
Monitor_Call('FixIOArea', FirstAddress, SizeOfArea)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FirstAddress, SizeOfArea
...
Monitor_Call('FixIOArea', FirstAddress, SizeOfArea)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buffer : W BLOCK 1000B
SizeOfArea : W BLOCK 1
FixIOArea : EQU 37B9 + 404B
...
CALLG FixIOArea, 2, Buffer, SizeOfArea
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 205*

[↑ Back to Top](#table-of-contents)

---

### 405B - SwitchUserBreak (USTRK)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Switches user-defined escape handling on and off. The user-defined escape handling transfers control to a routine when you press the ESCAPE key.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | On/off flag. Use 1 for on and 0 for off. |
| `Param2` | UNKNOWN | I | Program address to start at when you press the ESCAPE key. |

#### See Also

[GetUserRegisters](#420b-getuserregisters-grblk)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Func, Address : LONGINT;
...
SwitchUserBreak(Func, Address);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Func COMP.
01 Address COMP.
...
MONITOR-CALL "SwitchUserBreak" USING Func, Address.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Func, Address
...
Monitor_Call('SwitchUserBreak', Func, Address)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Func, Address
...
Monitor_Call('SwitchUserBreak', Func, Address)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Func : W BLOCK 1
Address : W BLOCK 1
SwitchUserBreak : EQU 37B9 + 405B
...
    CALLG SwitchUserBreak, 2, Func, Address
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 501*

[↑ Back to Top](#table-of-contents)

---

### 406B - AccessRTCommon (RWRTC)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads from or writes to RT common from an ND-500 program. RT common is an area in physical memory where RT programs may exchange data.

- Some SINTRAN III systems are generated without RT common. The size of RT common is defined at the time of system generation. The size may be increased by up to 8 pages by the SINTRAN-SERVICE-PROGRAM command DEFINE-RTCOMMON-SIZE (see the SINTRAN III Commands Reference Manual, ND-860128, for more information).

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Function. Use 0 to read, and 1 to write. |
| `Param2` | UNKNOWN | I | RT common address. This is a logical ND-100 address in the logical address area  |
| `Param3` | UNKNOWN | I | Number of bytes to read or write. |
| `Param4` | UNKNOWN | I | Buffer of data to be read or written. |

#### See Also

[GetInputFlags](#402b-getinputflags-rflag), [SetOutputFlags](#403b-setoutputflags-wflag)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Func, RTCommon, NoOfBytes : LONGINT;
Buffer : ARRAY [0..6] OF RECORD...END;

    AccessRTCommon(Func, RTCommon, NoOfBytes, Buffer);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Func COMP.
01 RTCommon COMP.
01 NoOfBytes COMP.
01 Buffer.
   02 array COMP OCCURS 100 TIMES.
...
MONITOR-CALL "AccessRTCommon" USING Func, RTCommon, NoOfBytes, Buffer.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Func, RTCommon, NoOfBytes
INTEGER Buffer(100)
...
Monitor_Call('AccessRTCommon', Func, RTCommon, NoOfBytes, Buffer(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Func, RTCommon, NoOfBytes
BYTES : Buffer(0:199)

Monitor_Call('AccessRTCommon', Func, RTCommon, NoOfBytes, Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Func : W BLOCK 1
RTCommon : W BLOCK 1
NoOfBytes : W BLOCK 1
Buffer : W ARRAY 100
ErrCode : W BLOCK 1
AccessRTCommon : EQU 37B9 + 406B

CALLG AccessRTCommon, 4, Func, RTCommon, NoOfBytes, Buffer
IF K GO ERROR


Error : W1 = ErrCode  %Error code in the W1 register.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 73*

[↑ Back to Top](#table-of-contents)

---

### 410B - FixInMemory (FIXMEM)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Fixes a logical segment (either whole or in part) of a user's domain in physical memory. This action is intended to speed up access to the segment and is also useful for segment sharing.

- Only data segments can be fixed.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `FixType.` | UNKNOWN | I | 0 = Fixes pages that are scattered (non-contiguous). 1 = Fixes pages contiguously and returns the starting address. 2 = Fixes pages at a given memory address. |
| `FirstAddr.` | UNKNOWN | I | The starting address within the user's domain. This should be a 32-bit address, including the segment number. |
| `Length.` | UNKNOWN | I | The length of the segment to fix, specified in bytes. A value of -1 can be used to fix the remaining part of the segment. |
| `ND100Addr.` | UNKNOWN | IO | The physical memory address in the ND-100 system. This parameter is used if FixType is 1 or 2. The address provided or returned is the start of a physical page. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[MemoryUnfix](#411b-memoryunfix-unfixm)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FixType, FirstAddr, Length, ND100Addr : LONGINT;
...
FixInMemory (FixType, FirstAddr, Length, ND100Addr);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FixType COMP.
01 FirstAddr COMP.
01 Length COMP.
01 ND100Addr COMP.
...
MONITOR-CALL "FixInMemory" USING FixType, FirstAddr, Length, ND100Addr.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FixType, FirstAddr, Length, ND100Addr
...
Monitor_Call('FixInMemory', FixType, FirstAddr, Length, ND100Addr)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FixType, FirstAddr, Length, ND100Addr
...
Monitor_Call('FixInMemory', FixType, FirstAddr, Length, ND100Addr)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FixType : W BLOCK 1
FirstAddr : W BLOCK 1
Length : W BLOCK 1
ND100Addr : W BLOCK 1
FixInMemory : EQU 37B9 + 410B
...
CALLG FixInMemory, 4, FixType, FirstAddr, Length, ND100Addr
    IF K GO Error
...
Error, ...
```

</details>

<details>
<summary><strong>MAC</strong></summary>

Not available.

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 411B - MemoryUnfix (UNFIXM)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Releases a fixed segment in your domain from physical memory. A fixed segment has all its pages fixed in physical memory. After MemoryUnfix the pages may be swapped between the disk and physical memory.

#### See Also

FixInMemory

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Address : LONGINT;
...
MemoryUnfix(Address);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Address COMP.
...
MONITOR-CALL "MemoryUnfix" USING Address.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Address
...
Monitor_Call('MemoryUnfix', Address)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : Address
...
Monitor_Call('MemoryUnFix', Address)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Address : W BLOCK 1
MemoryUnfix : EQU 37B9 + 411B
...
CALLG MemoryUnfix, 1, Address
IF K GO error,
...
Error, ...
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 347*

[↑ Back to Top](#table-of-contents)

---

### 412B - FileAsSegment (FSCNT)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Connects a file as a segment to your domain. You can then access the file as a logical segment. This reduces the access time.

- The file must be open. The access must be specified in the OpenFile call.
- The file is disconnected when it is closed.
- A file may be connected to several processes simultaneously. It is your responsibility to synchronize simultaneous accesses.
- You may not use ReadFromFile (mon 117) or WriteToFile (mon 120) on a file which is connected to a segment. Refer to these monitor calls for further details.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See OpenFile. |
| `Param2` | UNKNOWN | I | Logical segment number in the domain. The segment number must be free. Use 0 to  |
| `Param3` | UNKNOWN | I | Access type. Use 0 if the file contains initial data. 1 means uninitialized, emp |
| `Param4` | UNKNOWN | I | Logical segment number selected if you give 0 in the second parameter. |
| `Param5` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

FileNotAsSegment which disconnects the file

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNo, LogSegmentNo, Type, SegmentNo : LONGINT;
...
FileAsSegment(FileNo, LogSegmentNo, Type, SegmentNo);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNo COMP.
01 LogSegmentNo COMP.
01 Type COMP.
01 SegNo COMP.
01 ErrCode COMP.
...
MONITOR-CALL "FileAsSegment" USING FileNo, LogSegmentNo, Type, SegNo.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNo, LogSegmentNo, Type, SegmentNo
...
Monitor_Call('FileAsSegment', FileNo, LogSegmentNo, Type, SegmentNo)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNo : W BLOCK 1
LogSegmentNo : W BLOCK 1
Type : W BLOCK 1
SegmentNo : W BLOCK 1
ErrCode : W BLOCK 1
FileAsSegment : EQU 37B9 + 412B

...
CALLG FileAsSegment, 4, FileNo, LogSegmentNo, Type, SegmentNo
IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp;&nbsp;&nbsp;&nbsp;%ErrorCode in W1 register.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 193*

[↑ Back to Top](#table-of-contents)

---

### 413B - FileNotAsSegment (FSCDNT)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Disconnects a file as a segment in your domain. FileAsSegment allows files to be accessed as segments. This monitor call disconnects the file.

- The file is not closed.
- The file is automatically disconnected by CloseFile.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See OpenFile. |
| `Param2` | UNKNOWN | I | Segment number (optional parameter). |

#### See Also

[FileAsSegment](#412b-fileassegment-fscnt)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, LogSegNumber: LONGINT;
...
FileNotAsSegment(FileNumber, LogSegNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
02 LogSegNumber COMP.
...
MONITOR-CALL "FileNotAsSegment" USING FileNumber, LogSegNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, LogSegNumber
...
Monitor_Call('FileNotAsSegment', FileNumber, LogSegNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, LogSegNumber
...
Monitor_Call('FileNotAsSegment', FileNumber, LogSegNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
LogSegNumber : W BLOCK 1
FileNotAsSegment : EQU 37B9 + 413B
...
CALLG FileNotAsSegment, 2, FileNumber, LogSegNumber
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 195*

[↑ Back to Top](#table-of-contents)

---

### 414B - BCNAFCAMAC (BCNAF)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Special CAMAC function on the ND-500. (Same as mon 156 TRACB.)

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 415B - BCNAF1CAMAC (BCNAF1)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Special CAMAC monitor call for the ND-500. (Same as mon 176 - user-defined monitor call.)

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 416B - SaveND500Segment (WSEGN)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes all modified pages of a segment back to the disk.

- Not allowed when fixed in memory.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical segment number in the domain. If 0, the segment number is retrieved from |
| `Param2` | UNKNOWN | I | First logical page in the segment. |
| `Param3` | UNKNOWN | I | Last logical page in the segment. |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[SaveSegment](#164b-savesegment-wseg)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
LogSegmentNo, FirstPage, LastPage : LONGINT;
...
SaveND500Segment(LogicalSegmentNo, FirstPage, LastPage);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 LogSegmentNo COMP.
01 FirstPage COMP.
01 LastPage COMP.
01 ErrCode COMP.
...
MONITOR-CALL "SaveND500Segment" USING LogSegmentNo, FirstPage, LastPage.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER LogSegmentNo, FirstPage, LastPage
...
Monitor_Call('SaveND500Segment', LogSegmentNo, FirstPage, LastPage)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : LogSegmentNo, FirstPage, LastPage
...
ON ROUTINEERROR DO
  IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('SaveND500Segment', LogSegmentNo, FirstPage, LastPage)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
LogSegmentNo : W BLOCK 1
FirstPage : W BLOCK 1
LastPage : W BLOCK 1
ErrCode : W BLOCK 1
SaveND500Segment : EQU 37B9 + 416B
...
  CALLG SaveND500Segment, 3, LogSegmentNo, FirstPage, LastPage
  IF K GO ERROR
...
ERROR : W1 =: ErrCode                    %ErrorCode in W1 register.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 421*

[↑ Back to Top](#table-of-contents)

---

### 417B - MaxPagesInMemory (MXPISG)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Sets the maximum number of pages a segment may have in physical memory at a time.

- This monitor call applies to ND-500 logical segments only.
- The segment must be in use.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Logical segment number in your domain. If you specify 0, a segment number is fou |
| `Param2` | UNKNOWN | I | Segment type. Use 0 for data segments and 1 for program segments. |
| `Param3` | UNKNOWN | I | Number of pages. |
| `Param4` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

FixInMemory

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SegmentNo, SegType, NoOfPages : LONGINT;
...
MaxPagesInMemory(SegmentNo, SegType, NoOfPages);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SegmentNo COMP.
01 SegType COMP.
01 NoOfPages COMP.
01 ErrCode COMP.
...
MONITOR-CALL "MaxPagesInMemory" USING SegmentNo, SegType, NoOfPages.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SegmentNo, SegType, NoOfPages
...
Monitor Call('MaxPagesInMemory', SegmentNo, SegType, NoOfPages)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SegmentNo, SegType, NoOfPages
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('MaxPagesInMemory', SegmentNo, SegType, NoOfPages)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SegmentNo : W BLOCK 1
SegType : W BLOCK 1
NoOfPages : W BLOCK 1
MaxPagesInMemory : EQU 37B9 + 417B
...
&nbsp;&nbsp;&nbsp;&nbsp;CALLG MaxPagesInMemory, 3, SegmentNo, SegType, NoOfPages
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 341*

[↑ Back to Top](#table-of-contents)

---

### 420B - GetUserRegisters (GRBLK)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

SwitchUserBreak allows you to save the registers when you terminate an ND-500 program with the ESCAPE key. You can get the contents of the registers with GetUserRegisters.

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 421B - GetActiveSegment (GASGM)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Gets the name of the segments in your domain. A 2048 byte buffer is returned. It contains 32 pointers to segment names in the buffer. Each pointer consists of 12 bytes. The first four is an address. The second four is the offset from this address to the start of the segment name. The last four are the offset to the end of the segment name.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | A 2048 byte buffer, i.e. 1 page, containing pointers to segment names. |

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buffer : ARRAY [0..31] OF RECORD...END;
...
GetActiveSegment(Buffer);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buffer.
   02 array COMP OCCURS 1024 TIMES.
   ...
MONITOR-CALL "GetActiveSegment" USING Buffer.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buffer(1024)
...
Monitor_Call('GetActiveSegment', Buffer(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Buffer(0:2047)
...
Monitor_Call('GetActiveSegment', Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buffer : W BLOCK 1024
GetActiveSegment : EQU 37B9 + 421B
...
CALLG GetActiveSegment, 1, Buffer
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 217*

[↑ Back to Top](#table-of-contents)

---

### 422B - GetScratchSegment (GSWSP)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Connects an empty data segment to the user's domain and reserves space for it on the swap file. The segment is assigned the default name "SCRATCH-SEGMENT:DSEG".

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Segment size in bytes.` | UNKNOWN | I | Specifies the desired size of the segment. |
| `Logical segment number.` | UNKNOWN | I | The segment number to be used. It must be free. Using 0 will cause the system to select the first available free segment. |
| `Selected logical segment number.` | UNKNOWN | O | This parameter returns the logical segment number that was actually selected, especially if 0 was specified for the input logical segment number. |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[ClearCapability](#424b-clearcapability-capcle)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SizeInBytes, LogSegmentNo, RetLogSegmentNo : LONGINT;
...
GetScratchSegment(SizeInBytes, LogSegmentNo, RetLogSegmentNo);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 SizeInBytes COMP.
01 LogSegmentNo COMP.
01 RetLogSegmentNo COMP.
01 ErrCode COMP.
...
MONITOR-CALL "GetScratchSegment" USING SizeInBytes, LogSegmentNo, RetLogSegmentNo.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER SizeInBytes, LogSegmentNo, RetLogSegmentNo
...
Monitor_Call('GetScratchSegment', SizeInBytes, LogSegmentNo, RetLogSegmentNo)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SizeInBytes, LogSegmentNo, RetLogSegNo
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('GetScratchSegment', SizeInBytes, LogSegmentNo, RetLogSegNo)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
SizeInBytes : W BLOCK 1
LogSegmentNo : W BLOCK 1
RetLogSegmentNo : W BLOCK 1
ErrCode : W BLOCK 1
GetScratchSegment : EQU 37B9 + 422B
...
CALLG GetScratchSegment, 3, SizeInBytes, LogSegmentNo, RetLogSegmentNo
    IF K GO ERROR
...
ERROR : W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

Not available.

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 423B - CopyCapability (CAPCOP)

![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Copies a capability for a segment. The segment itself is also copied. A capability describes each logical segment in a domain.

- The destination segment number must be unused.

#### See Also

[ClearCapability](#424b-clearcapability-capcle)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
SourceSegNo, SourceType, DestSegNo, DestType, AccCode, RetSegNo : LONGINT;
...
CopyCapability(SourceSegNo, SourceType, DestSegNo,
               DestType, AccCode, RetSegNo)
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01  SourceSegNo  COMP.
01  SourceType   COMP.
01  DestSegNo    COMP.
01  DestType     COMP.
01  AccCode      COMP.
01  RetSegNo     COMP.
01  ErrCode      COMP.
...
MONITOR-CALL "CopyCapability" USING SourceSegNo, SourceType,
                             DestSegNo, DestType, AccCode, RetSegNo.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER  SourceSegNo, SourceType, DestSegNo, DestType
INTEGER  AccCode, RetSegNo
...
Monitor_Call('CopyCapability', SourceSegNo, SourceType,
             DestSegNo, DestType, AccCode, RetSegNo)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : SourceSegNo, SourceType, DestSegNo, DestType, AccCode, RetSegNo
...
ON ROUTINEERROR DO
&emsp; IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('CopyCapability', SourceSegNo, SourceType, DestSegNo, &
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;DestType, AccCode, RetSegNo)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
| Parameter     | Type   |
|---------------|--------|
| SourceSegNo   | W BLOCK 1 |
| SourceType    | W BLOCK 1 |
| DestSegNo     | W BLOCK 1 |
| DestType      | W BLOCK 1 |
| AccCode       | W BLOCK 1 |
| RetSegNo      | W BLOCK 1 |
| ErrCode       | W BLOCK 1 |

CopyCapability : EQU 37B9 + 423B
...
CALLG CopyCapability, 6, SourceSegNo, SourceType, DestSegNo, &
&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;&emsp;DestType, AccCode, RetSegNo

IF K GO ERROR
...
ERROR : W1 =: ErrCode &emsp;&emsp;%ErrorCode in W1 register.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 121*

[↑ Back to Top](#table-of-contents)

---

### 424B - ClearCapability (CAPCLE)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Clears a capability. A capability describes each logical segment in a domain. The protection of the segment is removed. See the manual ND Linker User Guide and Reference Manual (ND-60289).

- The logical segment is available for other physical segments.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical segment number in your domain.` | Logical segment number in your domain. | I |  |
| `Segment type. Use 0 for data segments, and 1 for program segments.` | Segment type. Use 0 for data segments, and 1 for program segments. | I |  |
| `Standard Error Code. See appendix A.` | Standard Error Code. See appendix A. | I |  |

#### See Also

[CopyCapability](#423b-copycapability-capcop)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
LogicalSegmentNo, SegType : LONGINT;
...
ClearCapability(LogicalSegmentNo, SegType);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 LogicalSegmentNo COMP.
01 SegType COMP.
01 ErrCode COMP.
...
MONITOR-CALL "ClearCapability" USING LogicalSegmentNo, SegType.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER LogicalSegmentNo, SegType
...
Monitor_Call('ClearCapability', LogicalSegmentNo, SegType)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : LogicalSegmentNo; SegType
...
ON ROUTINEERROR DO
&nbsp;&nbsp;&nbsp;&nbsp;IF ErrCode > 0 THEN ...
ENDON
Monitor_Call('ClearCapability', LogicalSegmentNo, SegType)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
LogicalSegmentNo : W BLOCK 1
SegType : W BLOCK 1
ErrCode : W BLOCK 1
ClearCapability : EQU 37B9 + 424B
...
&nbsp;&nbsp;&nbsp;&nbsp;CALLG ClearCapability, 2, LogicalSegmentNo, SegType
&nbsp;&nbsp;&nbsp;&nbsp;IF K GO ERROR
...
ERROR : W1 =: ErrCode &nbsp;&nbsp;&nbsp;&nbsp;%ErrorCode in W1 register.
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 111*

[↑ Back to Top](#table-of-contents)

---

### 425B - SetProcessName (SPRNAM)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Defines a new name for your process.

- Process names may be up to 16 characters and contain an additional user name, e.g. (P-HANSEN)WP-PROCESS.

#### See Also

[GetProcessNo](#426b-getprocessno-gprnam), [GetOwnProcessInfo](#427b-getownprocessinfo-gprnme)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProcessName : PACKED ARRAY [0..33] OF CHAR;
...
SetProcessName(ProcessName);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProcessName PIC X(34).
...
MONITOR-CALL "SetProcessName" USING ProcessName.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER ProcessName*34
...
Monitor_Call('SetProcessName', ProcessName(1:34))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : ProcessName(0:33).
...
Monitor_Call('SetProcessName', ProcessName)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessName : STRINGDATA 'WP-PROCESS'...
SetProcessName : EQU 37B9 + 425B
...
CALLG SetProcessName, 1, ProcessName
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 461*

[↑ Back to Top](#table-of-contents)

---

### 426B - GetProcessNo (GPRNAM)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the number of a process in the ND-500. You specify the process name. The process number is assigned when you start the ND-500 Monitor.

- The process name may not be abbreviated.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Process name. You may include a user name, e.g. (P-HANSEN)WP-PROCESS. |
| `Param2` | UNKNOWN | I | Process number in bit 31:16 and magic number in bit 15:0. |

#### See Also

[GetOwnProcessInfo](#427b-getownprocessinfo-gprnme), [SetProcessName](#425b-setprocessname-sprnam)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProcessName : PACKED ARRAY [0..33] OF CHAR;
ProcessNumber : LONGINT;
...
GetProcessNo(ProcessName, ProcessNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProcessName PIC X(34).
01 ProcessNumber COMP.
...
MONITOR-CALL "GetProcessNo" USING ProcessName, ProcessNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER ProcessName*34
INTEGER ProcessNumber
...
Monitor_Call('GetProcessNo', ProcessName(1:34), ProcessNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : ProcessName(0:33)
INTEGER : ProcessNumber
...
Monitor_Call('GetProcessNo', ProcessName, ProcessNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessName : STRING 44B
ProcessNumber : W BLOCK 1
GetProcessNo : EQU 37B9 + 426B
...
CALLG GetProcessNo, 2, ProcessName, ProcessNumber
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 267*

[↑ Back to Top](#table-of-contents)

---

### 427B - GetOwnProcessInfo (GPRNME)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the name and number of your own process in the ND-500. You get a process each time you enter the ND-500 Monitor.

- The default process names are TERMINAL-xx where xx is a number.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Process name, maximum 34 bytes. |
| `Param2` | UNKNOWN | I | Process number in bit 31:16 and magic number in bit 15:0. |
| `Param3` | UNKNOWN | I | Standard Error Code. See appendix A. |

#### See Also

[GetProcessNo](#426b-getprocessno-gprnam), [SetProcessName](#425b-setprocessname-sprnam)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProcessName : PACKED ARRAY [0..33] OF CHAR;
ProcessNumber : LONGINT;
...
GetOwnProcessInfo(ProcessName, ProcessNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProcessName PIC X(34).
01 ProcessNumber COMP.
...
MONITOR-CALL "GetOwnProcessInfo" USING ProcessName, ProcessNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
CHARACTER ProcessName*34
INTEGER ProcessNumber
...
Monitor_Call('GetOwnProcessInfo', ProcessName(1:34), ProcessNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : ProcessName(0:33)
INTEGER : ProcessNumber
...
Monitor_Call('GetOwnProcessInfo', ProcessName, ProcessNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessName : STRING 44B
ProcessNumber : W BLOCK 1
GetOwnProcessInfo : EQU 37B9 + 427B
...
CALLG GetOwnProcessInfo, 2, ProcessName, ProcessNumber
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 263*

[↑ Back to Top](#table-of-contents)

---

### 430B - TranslateAddress (ADRIOO)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

The call formats are marked from a-i. All formats give status on return. See appendix A. The return status >=0 means the call terminated correctly. Parameters which do not affect the call format are called "dummy" below. All call formats contain logical unit number (0-3) in function code bits 6-8. Status=EXABS(logical device number, Unit/function code, dummy, dummy, dummy) The hardware status is returned as error code. The device must be reserved. Otherwise a positive error code may not be the correct hardware status. Status=EXABS(logical device number, Unit/function code, dummy, dummy, dummy) Status is Standard Error Code. Status=EXABS(logical device number, Unit/function code, physical memory address, sector address, no of sectors to read) Status=EXABS(logical device number, Unit/function code, physical memory address, sector address, no of sectors to write) Status=EXABS(logical device number, Unit/function code, dummy, dummy, floppy format number) Status=EXABS(logical device number, Unit/function code, dummy, dummy, sector address) Status=EXABS(logical device number, Unit/function code, dummy, dummy, destination unit number) Status=EXABS(logical device number, Unit/function code, dummy, dummy, first sector address of track)

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 431B - AwaitTransfer (MWAITF)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Checks that a data transfer to or from a mass-storage file is completed. The monitor call is relevant to DeviceFunction, ReadFromFile and WriteToFile operations. These are carried out independently of the CPU. The number of bytes read or written is returned.

- You may specify that the program should wait if the transfer is not ready. It is set in the I/O wait state.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | File number. See Openfile. |
| `Param2` | UNKNOWN | I | Wait flag. If 0, the program waits until the data transfer is completed. Other v |
| `Param3` | UNKNOWN | I | Number of bytes transferred. |

#### See Also

[AwaitFileTransfer](#121b-awaitfiletransfer-waitf)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
FileNumber, WaitFlag, NoOfBytes : LONGINT;
...
AwaitTransfer(FileNumber, WaitFlag, NoOfBytes);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 FileNumber COMP.
01 WaitFlag COMP.
01 NoOfBytes COMP.
...
MONITOR-CALL "AwaitTransfer" USING FileNumber, WaitFlag, NoOfBytes.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER FileNumber, WaitFlag, NoOfBytes
...
Monitor_Call('AwaitTransfer', FileNumber, WaitFlag, NoOfBytes)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : FileNumber, WaitFlag, NoOfBytes
...
Monitor_Call('AwaitTransfer', FileNumber, WaitFlag, NoOfBytes)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
FileNumber : W BLOCK 1
WaitFlag : W BLOCK 1
NoOfBytes : W BLOCK 1
ErrCode : W BLOCK 1
AwaitTransfer : EQU 37B9 + 431B
...
CALLG AwaitTransfer, 3, FileNumber, WaitFlag, NoOfBytes
IF K GO Error
...
Error, Wl =: ErrCode
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 89*

[↑ Back to Top](#table-of-contents)

---

### 435B - ForceTrap (PRT)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Forces a programmed trap to occur in another ND-500 process. The trap handler in this process is started.

- The trapped process gets your process number through GetTrapReason. It is stored in the upper half of the error code. The lower half contains the reason code you specify as a parameter.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Process number to be trapped. |
| `Param2` | UNKNOWN | I | Reason code. Bit 31:16 contains the process number and bit 15:0 the magic number |
| `Language` | Availability | I |  |
| `PASCAL` | Not available. | I |  |
| `COBOL` | Not available. | I |  |
| `FORTRAN` | Not available. | I |  |

#### See Also

[GetTrapReason](#505b-gettrapreason-gerrcod)

#### Examples

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessNumber : W BLOCK 1
ReasonCode : W BLOCK 1
ForceTrap : EQU 37B9 + 435B
...
CALLG ForceTrap, 2, ProcessNumber, ReasonCode
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 213*

[↑ Back to Top](#table-of-contents)

---

### 436B - SetND500Param (5PASET)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets information about an ND-500 program. Use GetND500Param to read the 5 parameters when a program is terminated.

- SINTRAN III sets some of the parameter values if you give the command @ENABLE-TERMINATION-HANDLING first.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `1` | Bit 24:16 contains the user index. Bit 15:0 contains the directory index. | I |  |
| `2` | Logical device number of the terminal. | I |  |
| `3` | Fatal error or the monitor call ErrorMessage returns the error number. If ESCAPE was pressed, -1 is returned. | I |  |
| `4` | Set by SetND500Param. | I |  |
| `5` | Set by SetND500Param. | I |  |

#### See Also

[TerminationHandling](#206b-terminationhandling-edtmp), [GetUserParam](#57b-getuserparam-pagei), [SetUserParam](#56b-setuserparam-paset), and GetND500Param

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buffer : RECORD...END;
...
SetND500Param(Buffer);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buffer.
   02 array COMP OCCURS 5 TIMES.
   ...
   MONITOR-CALL "SetND500Param" USING Buffer.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buffer(5)
...
Monitor_Call('SetND500Param', Buffer(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER ARRAY : Buffer(0:4)
...
Monitor_Call('SetND500Param', Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buffer : W BLOCK 5
SetND500Param : EQU 37B9 + 436B
...
CALLG SetND500Param, 1, Buffer
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
This monitor call is not available on the ND-100. See SetUserParam.

| ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 451*

[↑ Back to Top](#table-of-contents)

---

### 437B - GetND500Param (5PAGET)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets information about why the last ND-500 program terminated. There are five parameters for each background user. These can be set by SINTRAN III or your background program.

- Use SetND500Param to set the parameter values.
- SINTRAN III sets some of the parameter values if you give the command @ENABLE-TERMINATION-HANDLING first.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `1` | Bit 24:16 contain the user index. Bit 7:0 contains the directory index. | I |  |
| `2` | Logical device number of the terminal. | I |  |
| `3` | Fatal error or the monitor call ErrorMessage returns the error number. If escape was pressed, -1 is returned. | I |  |
| `4` | User defined. | I |  |
| `5` | User defined. | I |  |

#### See Also

[TerminationHandling](#206b-terminationhandling-edtmp), [SetND500Param](#436b-setnd500param-5paset), [GetUserParam](#57b-getuserparam-pagei), and SetUserParam

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
Buffer : RECORD...END;
...
GetND500Param(Buffer);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 Buffer.
   02 array COMP OCCURS 5 TIMES.
   ...
   MONITOR-CALL "GetND500Param" USING Buffer.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER Buffer(5)
...
Monitor_Call('GetND500Param', Buffer(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
BYTES : Buffer(0:9)
...
Monitor_Call('GetND500Param', Buffer(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
Buffer : W BLOCK 5
GetND500Param : EQU 37B9 + 437B
...
CALLG GetND500Param, 1, Buffer
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 259*

[↑ Back to Top](#table-of-contents)

---

### 440B - Attach500Segment (ATSGM)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Maps a logical ND-500 data segment onto shared ND-100/ND-500(0) physical memory (multiport memory). Example: ``` @SINTRAN-SERVICE-PROGRAM *CHANGE-TABLE TABLE: MEMORY-AREA-INVISIBLE-FOR-THIS-SYSTEM FUNCTION: INSERT-ELEMENT IMAGE OR SAVE AREA (DEFAULT IS IMAGE)? IMAGE FIRST PAGE (OCT): 10000 LAST PAGE (OCT): 13777 FUNCTION: EXIT *EXIT ```

- The specified physical memory area must be defined in the "Not initialize page" table by use of the *CHANGE-TABLE command in the SINTRAN Service Program (follow this command by a warm start to put the change into effect). Note that you should not use the first pages of the multiport memory (starting at "ND-500 page 0") for this.

*Source: ND-860228.2 EN, Page N/A*

[↑ Back to Top](#table-of-contents)

---

### 500B - StartProcess (STARTP)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Starts a process in the ND-500. You identify the process with the process number.

- The process may be active. The process then restarts as soon as it terminates.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Process number.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[StopProcess](#501b-stopprocess-stoppr) and [SwitchProcess](#502b-switchprocess-switchp)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProcessNumber : LONGINT;
...
StartProcess(ProcessNumber);
IF ErrCode <> 0 THEN ...
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProcessNumber COMP.
01 ErrCode COMP.
...
MONITOR-CALL "StartProcess" USING ProcessNumber.
CALL "CbError" USING ErrCode.
IF ErrCode NOT = 0 GO ...
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ProcessNumber
...
Monitor_Call('StartProcess', ProcessNumber)
IF (ErrCode .NE. 0) THEN ...
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ProcessNumber
...
ON ROUTINEERROR DO
    IF ErrCode >< 0 THEN ...
ENDON
Monitor_Call('StartProcess', ProcessNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessNumber : W BLOCK 1
ErrCode : W BLOCK 1
StartProcess : EQU 37B9 + 500B
...
CALLG StartProcess, 1, ProcessNumber
    IF K GO ERROR
...
ERROR : W1 := ErrCode              %ErrorCode in W1 register.
```

</details>

<details>
<summary><strong>MAC</strong></summary>

Not available.

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 501B - StopProcess (STOPPR)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the current process in a wait state. StartProcess restarts the process. Execution continues after the monitor call. The ESCAPE key terminates the waiting program.

- The process restarts immediately if it is scheduled for repeated execution.
- Use ExitFromProgram to terminate the execution.

#### See Also

StartProcess, [SwitchProcess](#502b-switchprocess-switchp)

#### Examples

<details>
<summary><strong>COBOL</strong></summary>

```cobol
MONITOR-CALL "StopProcess"
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
Monitor_Call('StopProcess')
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
Monitor_Call('StopProcess')
```

</details>

<details>
<summary><strong>MAC</strong></summary>

```mac
StopProcess : EQU 37B9 + 501B
CALLG StopProcess

| ND-500 | All users | All programs |
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 493*

[↑ Back to Top](#table-of-contents)

---

### 502B - SwitchProcess (SWITCHP)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the current process in a wait state. Restarts another process. This is similar to executing a StartProcess followed by a StopProcess.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Process number to start. |

#### See Also

StartProcess, [StopProcess](#501b-stopprocess-stoppr)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ProcessNumber : LONGINT;
...
SwitchProcess(ProcessNumber);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ProcessNumber COMP.
...
MONITOR-CALL "SwitchProcess" USING ProcessNumber.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ProcessNumber
...
Monitor_Call('SwitchProcess', ProcessNumber)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ProcessNumber
...
Monitor_Call('SwitchProcess', ProcessNumber)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ProcessNumber : W BLOCK 1
SwitchProcess : EQU 37B9 + 502B
...
CALLG SwitchProcess, 1, ProcessNumber
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 499*

[↑ Back to Top](#table-of-contents)

---

### 503B - InputString (DVINST)

![ND-100](https://img.shields.io/badge/ND--100-Yes-green) ![ND-500](https://img.shields.io/badge/ND--500-Yes-green) ![User](https://img.shields.io/badge/User-Yes-blue) ![RT](https://img.shields.io/badge/RT-Yes-blue)

Reads a string from a device, e.g. a terminal or an opened file. This monitor call provide a fast input to ND-500 programs.

- Only the first four parameters are used for file input.
- An ASCII table is given in Appendix G.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number.` | UNKNOWN | I | See appendix B. Use 1 for your own terminal. The open file number is obtained by opening a file and specifying connection number 0. See the manual ND Linker User Guide and Reference Manual (ND-860289). |
| `Maximum number of bytes to read before break.` | UNKNOWN | I |  |
| `Number of bytes read.` | UNKNOWN | O |  |
| `Buffer to receive input.` | UNKNOWN | I |  |
| `Break setting.` | UNKNOWN | I | See SetBreak. Use 8 for the last user-defined break table. |
| `Echo setting.` | UNKNOWN | I | See SetEcho. Use 8 for the last user-defined break table. |
| `Break table, bit 0:31.` | UNKNOWN | I | Bits set to 1 cause break on the character. |
| `Break table, bit 32:63.` | UNKNOWN | I |  |
| `Break table, bit 64:95.` | UNKNOWN | I |  |
| `Break table, bit 96:127.` | UNKNOWN | I |  |
| `Echo table, bit 0:31.` | UNKNOWN | I | Bits set to 0 cause echo on the character. |
| `Echo table, bit 32:63.` | UNKNOWN | I |  |
| `Echo table, bit 64:95.` | UNKNOWN | I |  |
| `Echo table, bit 96:127.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | O |  |

#### See Also

[InByte](#1b-inbyte-inbt), InString, and [OutputString](#504b-outputstring-dvouts)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DevNo, MaxNo, NoOfBytesRet, BreakStrat, EchoStrat, BreakT1: LONGINT;
BreakT2, BreakT3, BreakT4, EchoT1, EchoT2, EchoT3, EchoT4: LONGINT;
Buff : ARRAY [0..8] OF RECORD...END;
...
InputString(DevNo, MaxNo, NoOfBytesRet, Buff, BreakStrat, EchoStrat,
            BreakT1, BreakT2, BreakT3, BreakT4, EchoT1, EchoT2, EchoT3, EchoT4);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 DevNo COMP.
01 MaxNo COMP.
01 NoOfBytesRet COMP.
01 BreakStrat COMP.
01 EchoStrat COMP.
01 BreakT1 COMP.
01 BreakT2 COMP.
01 BreakT3 COMP.
01 BreakT4 COMP.
01 EchoT1 COMP.
01 EchoT2 COMP.
01 EchoT3 COMP.
01 EchoT4 COMP.
01 Buff.
   02 array COMP OCCURS 100 TIMES.
...
MONITOR-CALL "InputString" USING DevNo, MaxNo, NoOfBytesRet, Buff,
                 BreakStrat, EchoStrat, BreakT1, BreakT2, BreakT3,
                 BreakT4, EchoT1, EchoT2, EchoT3, EchoT4.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DevNo, MaxNo, NoOfBytesRet, BreakStrat, EchoStrat, BreakT1
INTEGER BreakT2, BreakT3, BreakT4, EchoT1, EchoT2, EchoT3, EchoT4
INTEGER Buff(100)
...
Monitor_Call('InputString', DevNo, MaxNo, NoOfBytesRet, Buff(1),
             BreakStrat, EchoStrat, BreakT1, BreakT2, BreakT3,
             BreakT4, EchoT1, EchoT2, EchoT3, EchoT4)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DevNo, MaxNo, NoOfBytesRet, BreakStrat, EchoStrat, BreakT1
INTEGER : BreakT2, BreakT3, BreakT4, EchoT1, EchoT2, EchoT3, EchoT4
BYTES : Buff(0:199)
...
Monitor_Call('InputString', DevNo, MaxNo, NoOfBytesRet, Buff(1),
             BreakStrat, EchoStrat, BreakT1, BreakT2, BreakT3,
             BreakT4, EchoT1, EchoT2, EchoT3, EchoT4)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DevNo : W BLOCK 1
MaxNo : W BLOCK 1
NoOfBytesRet: W BLOCK 1
BreakStrat: W BLOCK 1
EchoStrat: W BLOCK 1
BreakT1 : W BLOCK 1
BreakT2: W BLOCK 1
BreakT3: W BLOCK 1
BreakT4 : W BLOCK 1
EchoT1: W BLOCK 1
EchoT2: W BLOCK 1
EchoT3: W BLOCK 1
EchoT4 : W BLOCK 1
Buff : BY BLOCK 400B
InputString: EQU 37B9 + 503B
...
Monitor_Call('InputString', DevNo, MaxNo, NoOfBytesRet, Buff(0), &
             BreakStrat, EchoStrat, BreakT1, BreakT2, BreakT3, &
             BreakT4, EchoT1, EchoT2, EchoT3, EchoT4)
...
CALLG InputString, 14, DevNo, MaxNo, NoOfBytesRet, Buff, &
      BreakStrat, EchoStrat, BreakT1, BreakT2, &
      BreakT3, BreakT4, EchoT1, EchoT2, EchoT3, EchoT4
```

</details>

<details>
<summary><strong>MAC</strong></summary>

Not available.

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 0*

[↑ Back to Top](#table-of-contents)

---

### 504B - OutputString (DVOUTS)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Writes a string to a device, e.g. a terminal or an opened file.

- This is the most efficient way to output strings on the ND-500.
- The maximum string length is 2048 bytes.
- Appendix F contains an ASCII table.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Logical device number, e.g. a file number. See appendix B. You may use 1 for your own terminal. Use the SINTRAN III open file number if output to a file. The ND-500 open file number will not function.` | UNKNOWN | I |  |
| `Number of bytes to write.` | UNKNOWN | I |  |
| `String to be output.` | UNKNOWN | I |  |
| `Standard Error Code. See appendix A.` | UNKNOWN | I |  |

#### See Also

[OutMessage](#32b-outmessage-msg), [OutUpTo8Bytes](#22b-outupto8bytes-m8out), [Out8Bytes](#24b-out8bytes-b8out), [OutString](#162b-outstring-outst), [OutNumber](#35b-outnumber-iout), [OutByte](#2b-outbyte-outbt), and InputString

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
DeviceNo, NoOfBytes : LONGINT;
Buff : ARRAY [0..8] OF RECORD...END;
...
OutputString(DeviceNo, NoOfBytes, Buff);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01  DeviceNo  COMP.
01  NoOfBytes COMP.
01  Buff.
    02  array COMP OCCURS 100 TIMES.
    ...
MONITOR-CALL "OutputString" USING DeviceNo, NoOfBytes, Buff.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER DeviceNo, NoOfBytes
INTEGER Buff(100)
...
Monitor_Call('OutputString', DeviceNo, NoOfBytes, Buff(1))
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : DeviceNo, NoOfBytes
BYTES : Buff(0:199)
...
Monitor_Call('OutputString', DeviceNo, NoOfBytes, Buff(0))
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
DeviceNo : W BLOCK 1
NoOfBytes : W BLOCK 1
Buff : BY BLOCK 4000B
Status : W BLOCK 1
OutputString : EQU 37B9 + 504B
...
CALLG OutputString, 3, DeviceNo, NoOfBytes, Buff
IF K GO Error
W1 =: Status
...
Error, ...
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 383*

[↑ Back to Top](#table-of-contents)

---

### 505B - GetTrapReason (GERRCOD)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Gets the error code from the swapper process. This is only relevant to programmed trap handlers. The swapper process starts the trap handler when it detects a fatal error, e.g. address outside segment. Use this monitor call to get the error code.

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
ErrorCode : LONGINT;
...
GetTrapReason(ErrorCode);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 ErrorCode COMP.
...
MONITOR-CALL "GetTrapReason" USING ErrorCode.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER ErrorCode
...
Monitor_Call('GetTrapReason', ErrorCode)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : ErrorCode
...
Monitor_Call('GetTrapReason', ErrorCode)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
ErrorCode : W BLOCK 1
GetTrapReason : EQU 37B9 + 505B
...
CALLG GetTrapReason, 1, ErrorCode
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 295*

[↑ Back to Top](#table-of-contents)

---

### 507B - SetProcessPriority (SPRIO)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Sets the priority for a process in the ND-500. The priorities vary from 0 to 255. The process with the highest priority is executed first.

- The priorities of background programs normally vary between 20 and 64. SINTRAN III modifies the priorities all the time. This is done to allow several jobs to share the CPU. Specify 0 to execute a process in the same way.
- With SetProcessPriority, you may fix the priority.

#### See Also

[SetRTPriority](#110b-setrtpriority-prior)

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
NewPriority : LONGINT;
...
SetProcessPriority(NewPriority);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NewPriority COMP.
...
MONITOR-CALL "SetProcessPriority" USING NewPriority.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER NewPriority
...
Monitor_Call('SetProcessPriority', NewPriority)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : NewPriority
...
Monitor_Call('SetProcessPriority', NewPriority)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NewPriority : W BLOCK 1
SetProcessPriority : EQU 37B9 + 507B
...
CALLG SetProcessPriority, 1, NewPriority
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 463*

[↑ Back to Top](#table-of-contents)

---

### 514B - ND500TimeOut (5TMOUT)

![ND-500](https://img.shields.io/badge/ND--500-Yes-green)

Suspends the execution of an ND-500 program for a given time. The execution then continues after the monitor call. The program is placed in a time queue in the ND-500, not the ND-100.

- No reserved files or devices are released.
- Avoid using TimeOut (mon 267) from ND-500, use ND500TimeOut instead. Use TimeOut from ND-100.

#### Parameters

| Name | Type | I/O | Description |
|------|------|-----|-------------|
| `Param1` | UNKNOWN | I | Number of time units to suspend the program. Use 0 to restart programs immediate |
| `Param2` | UNKNOWN | I | The type of time units. 1 = basic time units, i.e. 1/50th of a second, 2 = secon |
| `Param3` | UNKNOWN | I | Restart cause. 0 means that the defined time has elapsed. 1 means that an interr |

#### See Also

[TimeOut](#267b-timeout-tmout), [SuspendProgram](#104b-suspendprogram-hold), WaitForRestart

#### Examples

<details>
<summary><strong>Pascal</strong></summary>

```pascal
NoOfTimeUnits, TimeUnit, ReturnStatus : LONGINT;
...
ND500TimeOut(NoOfTimeUnits, TimeUnit, ReturnStatus);
```

</details>

<details>
<summary><strong>COBOL</strong></summary>

```cobol
01 NoOfTimeUnits COMP.
01 TimeUnit COMP.
01 ReturnStatus COMP.
...
MONITOR-CALL "ND500TimeOut" USING NoOfTimeUnits, TimeUnit, ReturnStatus.
```

</details>

<details>
<summary><strong>Fortran</strong></summary>

```fortran
INTEGER NoOfTimeUnits, TimeUnit, ReturnStatus
...
Monitor_Call('ND500TimeOut', NoOfTimeUnits, TimeUnit, ReturnStatus)
```

</details>

<details>
<summary><strong>PLANC</strong></summary>

```planc
INTEGER : NoOfTimeUnits, TimeUnit, ReturnStatus
...
Monitor_Call('ND500TimeOut', NoOfTimeUnits, TimeUnit, ReturnStatus)
```

</details>

<details>
<summary><strong>ASSEMBLY-500</strong></summary>

```asm
NoOfTimeUnits : W BLOCK 1
TimeUnit : W BLOCK 1
ReturnStatus : W BLOCK 1
ND500TimeOut : EQU 3789 + 514B
...
CALLG ND500TimeOut, 3, NoOfTimeUnits, TimeUnit, ReturnStatus
```

</details>

*Source: SINTRAN III Monitor Calls (ND-860228.2 EN), Page 349*

[↑ Back to Top](#table-of-contents)

---

## About This Document

This documentation was automatically generated from YAML files extracted from the
SINTRAN III Monitor Calls reference manual (ND-860228.2 EN).

- **Total Monitor Calls:** 230
- **Calls with Short Names:** 230
- **Extraction Date:** 2025-01-06

For technical details about the extraction process, see:
- `FINAL-EXTRACTION-REPORT.md`
- `SCHEMA-CLEANUP-ANALYSIS.md`
