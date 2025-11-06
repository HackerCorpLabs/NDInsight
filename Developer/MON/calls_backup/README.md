# SINTRAN III Monitor Calls - YAML Files

**Extraction Date:** 2025-01-06  
**Source Document:** Monitor Calls.md (ND-860228.2 EN)  
**Format:** YAML with JSON Schema validation

---

## 📊 Statistics

- **Total Monitor Calls:** 177 unique files
- **Sections Extracted:** 204 (some octals have multiple sections)
- **OCR Corrections:** 75 headers reconstructed
- **Success Rate:** 100%

---

## 📂 File Naming Convention

Files are named: `<OCTAL>_<MonitorCallName>.yaml`

Examples:
- `000B_ExitFromProgram.yaml`
- `112B_AdjustClock.yaml`
- `131B_DataTransfer.yaml` *(see example)*
- `231B_EXPANDFILE.yaml` *(see example)*
- `406B_AccessRTCommon.yaml`

---

## 🗂️ Complete Inventory

1B_InByte.yaml
2B_OutByte.yaml
3B_SETECHO.yaml
4B_SetBreak.yaml
5B_ReadScratchFile.yaml
6B_WRITESCRATCHFILE.yaml
11B_GetBasicTime.yaml
12B_SetCommandBuffer.yaml
13B_ClearInBuffer.yaml
14B_CLEAROUTBUFFER.yaml
16B_GetTerminalType.yaml
17B_SetTerminalType.yaml
22B_OutUpTo8Bytes.yaml
23B_In8Bytes.yaml
24B_Out8Bytes.yaml
26B_GetLastByte.yaml
31B_IOInstruction.yaml
32B_OutMessage.yaml
33B_ALTPAGETABLE.yaml
35B_OutNumber.yaml
36B_NoWaitSwitch.yaml
37B_ReadADChannel.yaml
40B_CloseSpoolingFile.yaml
41B_ReadObjectEntry.yaml
43B_CloseFile.yaml
44B_GetUserEntry.yaml
50B_OpenFile.yaml
53B_GetSegmentEntry.yaml
54B_DeleteFile.yaml
55B_GetSpoolingEntry.yaml
57B_GetUserParam.yaml
62B_GETBYTESINFILE.yaml
63B_In4x2Bytes.yaml
64B_WarningMessage.yaml
65B_ErrorMessage.yaml
66B_InBufferSpace.yaml
67B_OutBufferSpace.yaml
70B_CallCommand.yaml
71B_DISABLEESCAPE.yaml
72B_ENABLEESCAPE.yaml
73B_SetMaxBytes.yaml
74B_SetStartByte.yaml
75B_GetStartByte.yaml
76B_SetBlockSize.yaml
77B_SetStartBlock.yaml
100B_StartRTProgram.yaml
101B_DelayStart.yaml
102B_StartupTime.yaml
103B_STARTUPINTERVAL.yaml
104B_SuspendProgram.yaml
105B_StopRTProgram.yaml
106B_StartOnInterrupt.yaml
107B_NoInterruptStart.yaml
110B_SetRTPriority.yaml
111B_SetClock.yaml
112B_AdjustClock.yaml
113B_GetCurrentTime.yaml
114B_GETTIMEUSED.yaml
115B_FixScatter.yaml
117B_ReadFromFile.yaml
121B_AwaitFileTransfer.yaml
122B_ReserveResource.yaml
123B_ReleaseResource.yaml
124B_FORCE.yaml
125B_ForceRelease.yaml
127B_ExactStartup.yaml
130B_ExactInterval.yaml
132B_JumpToSegment.yaml
133B_ExitFromSegment.yaml
134B_ExitRTProgram.yaml
135B_WAITFORRESTART.yaml
136B_EnableRTStart.yaml
137B_DisableRTStart.yaml
140B_ReservationInfo.yaml
141B_DeviceControl.yaml
142B_ToErrorDevice.yaml
143B_ExecutionInfo.yaml
144B_DEVICEFUNCTION.yaml
146B_PrivInstruction.yaml
151B_GETRTADDRESS.yaml
155B_GRAPHICFUNCTION.yaml
157B_SegmentToPageTable.yaml
160B_FIXCONTIGUOUS.yaml
161B_INSTRING.yaml
162B_OutString.yaml
164B_SAVESEGMENT.yaml
165B_GETINREGISTERS.yaml
167B_AttachSegment.yaml
207B_GetErrorInfo.yaml
212B_ReentrantSegment.yaml
213B_GetDirUserIndexes.yaml
214B_GetUserName.yaml
215B_GetObjectEntry.yaml
216B_SetObjectEntry.yaml
217B_GetAllFileIndexes.yaml
220B_DirectOpen.yaml
221B_CreateFile.yaml
227B_SetEscLocalChars.yaml
230B_GEtEscLocalChars.yaml
231B_EXPANDFILE.yaml
233B_SetTemporaryFile.yaml
234B_SetPeripheralName.yaml
235B_SCRATCHOPEN.yaml
236B_SetPermanentOpen.yaml
237B_SetFileAccess.yaml
240B_AppendSpooling.yaml
241B_NewUser.yaml
242B_OldUser.yaml
244B_GetDirEntry.yaml
245B_GetNameEntry.yaml
246B_ReserveDir.yaml
250B_GetDefaultDir.yaml
252B_BACKUPCLOSE.yaml
253B_NewFileVersion.yaml
254B_GetErrorDevice.yaml
256B_FULLFILENAME.yaml
262B_GetSystemInfo.yaml
270B_ReadDiskPage.yaml
273B_GetFileName.yaml
274B_GetFileIndexes.yaml
275B_SetTerminalName.yaml
277B_DISABLELOCAL.yaml
301B_StopEscapeHandling.yaml
302B_ONESCLOCALFUNCTION.yaml
303B_OffEscLocalFunction.yaml
306B_GetTerminalMode.yaml
306B_TerminalMode.yaml
307B_TerminalNoWait.yaml
311B_WriteDirEntry.yaml
312B_CheckMonCall.yaml
313B_InBufferState.yaml
314B_DefaultRemoteSystem.yaml
315B_LAMUFunction.yaml
316B_SetRemoteAccess.yaml
317B_ExecuteCommand.yaml
322B_GETSEGMENTNO.yaml
323B_SegmentOverlay.yaml
324B_OctobusFunction.yaml
325B_BATCHMODEECHO.yaml
330B_TerminalStatus.yaml
332B_TerminalLineInfo.yaml
334B_GetErrorMessage.yaml
335B_TRANSFERDATA.yaml
336B_TERMINAL.yaml
337B_ChangeSegment.yaml
340B_READSYSTEMRECORD.yaml
341B_SegmentFunction.yaml
400B_ERRORRETURN.yaml
401B_DiAssemble.yaml
402B_GetInputFlags.yaml
403B_SetOutputFlags.yaml
405B_SwitchUserBreak.yaml
406B_AccessRTCommon.yaml
410B_FIXINMEMORY.yaml
411B_MemoryUnfix.yaml
412B_FileAsSegment.yaml
413B_FileNotAsSegment.yaml
416B_SaveND500Segment.yaml
417B_MaxPagesInMemory.yaml
420B_GetUserRegisters.yaml
421B_GETACTIVESEGMENT.yaml
422B_GETSCRATCHSEGMENT.yaml
423B_CopyCapability.yaml
424B_ClearCapability.yaml
425B_SetProcessName.yaml
426B_GetProcessNo.yaml
427B_GetOwnProcessInfo.yaml
431B_AwaitTransfer.yaml
436B_SetND500Param.yaml
500B_STARTPROCESS.yaml
501B_StopProcess.yaml
502B_SwitchProcess.yaml
504B_OutputString.yaml
505B_GetTrapReason.yaml
507B_SetProcessPriority.yaml
514B_ND500TimeOut.yaml
514B_TimeOut.yaml


---

## 📋 Schema & Validation

- **Schema Documentation:** 
- **JSON Schema Validator:** 

### Validate a file:
```bash
check-jsonschema --schemafile ../mon-call.schema.json 112B_AdjustClock.yaml
```

---

## 🔍 YAML Structure

Each YAML file contains:

- **Metadata:** octal, name, short_names
- **Description:** detailed explanation with notes
- **Parameters:** ordered list with types and I/O direction
- **Examples:** code in 6 languages (Pascal, COBOL, Fortran, PLANC, Assembly-500, MAC)
- **Compatibility:** platform, user, and program compatibility
- **Source:** document page and line references
- **Extraction:** metadata about OCR corrections

---

## 📚 References

- [Extraction Plan](../EXTRACTION_PLAN.md)
- [Extraction Report](../extraction-report.md)
- [Schema Documentation](../mon-call-schema.yaml)

---

**Generated:** 2025-11-06 08:16:51

