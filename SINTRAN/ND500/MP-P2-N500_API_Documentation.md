# MP-P2-N500.NPL API Documentation

**File:** Z:\NorskData\Source Code\Sintran L\NPL\MP-P2-N500.NPL
**Total Lines:** ~3808
**Total APIs/Subroutines:** 47+

---

## API List by Line Number

### 1. **5RRTWT, 5XACTRT, 5SWACTRT** (Lines 21-46)
- **Line Range:** 21-46
- **Description:** Routines to remove message from execution queue and restart ND-500 shadow program in ND-100
- **Execution Level:** Driver level (when called from other levels must be called with interrupt off)
- **Entry Parameters:**
  - X = Actual message
- **Exit Parameters:**
  - T, A, D registers destroyed
- **Comments:** Lines 3-20
  - 5RRTWT: Main entry point
  - 5XACTRT: Message of process to restart, not in EX-queue
  - 5SWACTRT: Start 5SWAP

---

### 2. **ESC500, SYSABORT** (Lines 55-91)
- **Line Range:** 55-91
- **Description:** ESC500 - SYSABORT routine for escape handling
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - X = Addr of input datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 49-54
- **Purpose:** Handle escape/abort sequence for ND-500 processes

---

### 3. **RL5PDESC, 1RL5PDESC** (Lines 108-134)
- **Line Range:** 108-134
- **Description:** Release the ND-500 process description after the ND-500 process has been terminated
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - T = Monitor level routine to execute after the process is terminated
  - X = RT-description addr (when RL5PDESC)
- **Exit Parameters:** Return to STUPR if the program has reserved an ND-500 proc, else exit
- **Comments:** Lines 94-107
- **Purpose:** Called when an error is detected by SINTRAN III monitor (in MON.CALL etc.)

---

### 4. **RTBAK, APLRSTART, 5FRTBAK** (Lines 144-196)
- **Line Range:** 144-196
- **Description:** Monitor level routine to remove ND-100 program from I/O-wait
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - X = Process description
- **Exit Parameters:** Not specified
- **Comments:** Lines 137-143
- **Purpose:** RTBAK - remove from I/O wait; APLRSTART - activate ND-100 application waiting for SIBAS in ND-500

---

### 5. **500HIST** (Lines 209-345)
- **Line Range:** 209-345
- **Description:** Level 2 routine called each basic time unit
- **Execution Level:** Level 2
- **Entry Parameters:** None
- **Exit Parameters:** All registers are destroyed
- **Comments:** Lines 199-208
- **Purpose:** Called from ICLCK in IOF and return in IOF but has ION sequences inside. Handles histogram and process logging

---

### 6. **N500C** (Lines 358-644)
- **Line Range:** 358-644
- **Description:** Level 2 (monitor level) routine to execute commands to ND-500
- **Execution Level:** Monitor level (Level 2)
- **Entry Parameters:**
  - X = Process description
- **Exit Parameters:** Returns to MONEN/STUPR
- **Comments:** Lines 348-357
- **Purpose:** Main command dispatcher for ND-500 operations

---

### 7. **ITRAPDECODER** (Line 647-649)
- **Line Range:** 647-649
- **Description:** Empty stub for trap decoder
- **Execution Level:** Not specified
- **Entry Parameters:** Not specified
- **Exit Parameters:** Not specified

---

### 8. **5STDRIV, N500, XN500, NXTMSG, CALLID12** (Lines 656-721)
- **Line Range:** 656-721
- **Description:** Level 12 - ND-500 communication driver kernel
- **Execution Level:** Level 12 (Driver level)
- **Entry Parameters:** Not specified in comments
- **Exit Parameters:** Not specified
- **Comments:** Lines 650-655
- **Purpose:** Main driver kernel for ND-500 communication

---

### 9. **CHN5STATUS** (Lines 730-759)
- **Line Range:** 730-759
- **Description:** Check and handle ND-500 status
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Message address
- **Exit Parameters:** Not specified
- **Comments:** Lines 724-729
- **Purpose:** Decode ND-500 status and route to appropriate handler

---

### 10. **HISTSAMPLE** (Lines 768-787)
- **Line Range:** 768-787
- **Description:** Handle histogram sampling
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = HIMESS
- **Exit Parameters:** Not specified
- **Comments:** Lines 762-767
- **Purpose:** Update histogram when process is active

---

### 11. **DECOMESS** (Lines 803-819)
- **Line Range:** 803-819
- **Description:** Decode "ANSWER-MESSAGE" from N500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Actual message (=MESSAGE)
  - B = N500 CPU-datafield
- **Exit Parameters:** Returns to NXTMSG in driver kernel N500
- **Comments:** Lines 790-802
- **Purpose:** Route answer messages to appropriate handlers

---

### 12. **DECOERRMESS** (Lines 835-844)
- **Line Range:** 835-844
- **Description:** Decode "ERROR-ANSWER" messages from N500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Actual message
  - B = N500 CPU-datafield
- **Exit Parameters:** Returns to NXTMSG in driver kernel N500
- **Comments:** Lines 822-834
- **Purpose:** Handle error answers from ND-500

---

### 13. **TRAPDECODER, ITRAPDECODER** (Lines 859-895)
- **Line Range:** 859-895
- **Description:** Decode "TRAP-MESSAGES" from N500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = "Trapped" message
  - B = N500 CPU-datafield
- **Exit Parameters:** Returns to NXTMSG in driver kernel N500
- **Comments:** Lines 847-858
- **Purpose:** Handle trap conditions from ND-500 (including page faults)

---

### 14. **SWPDECODER, 5RDTRANSFER** (Lines 912-1241)
- **Line Range:** 912-1241
- **Description:** Swapper message decoder
- **Execution Level:** ND-500 driver level (level 12)
- **Entry Parameters:**
  - X = Swapper message
  - B = ND-500 cpu-datafield
- **Exit Parameters:** Returns to ND-500 driver kernel N500
- **Comments:** Lines 898-911
- **Purpose:** Message demanding swap is marked "swapwait" in status. The swapper is activated with page-fault info and swapped process is marked "swapping"

---

### 15. **N5FUD, STAPROC, NSTOPROC, SWITPROC, NINSTR, NOUTSTR, GERRC, 5SIBMO, SPRIO, SWMC, DVIO, A5XMSG, B5XMSG, M5TMOUT, 5MTRANS** (Lines 1246-1248)
- **Line Range:** 1246-1248
- **Description:** Forward declarations for multiple driver-level subroutines
- **Execution Level:** Various (Driver level mostly)
- **Purpose:** Function declarations for subsequent implementations

---

### 16. **MCHANDEL, NORMMC, M516, M517, M520, M521, M522, M523** (Lines 1267-1406)
- **Line Range:** 1267-1406
- **Description:** Decoding monitor call messages from N500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Actual message (=MESSAGE)
  - T = Stop-reason
  - B = ND-500 CPU datafield
- **Exit Parameters:** Returns to NXTMSG in ND-500 driver kernel N500
- **Comments:** Lines 1251-1266
- **Purpose:** Main monitor call handler with special treatment for calls requiring level 12 handling (L12MIN=500 to L12MAX=523)

---

### 17. **N5FUD, COAF, UDFUNC, RIOWA** (Lines 1426-1518)
- **Line Range:** 1426-1518
- **Description:** Activated when MON 333 (UDMA) from ND-500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - B = ND-500 CPU datafield
  - X = Monitor call message in 5MBBANK
- **Exit Parameters:**
  - EXIT: Error, try again on level 1
  - EXIT+1: OK, UDRxx RT-PROG started
- **Comments:** Lines 1415-1425
- **Purpose:** Handle UDMA (User DMA) monitor call from ND-500

---

### 18. **NSTOPROC, STAPROC, SWITPROC** (Lines 1527-1600)
- **Line Range:** 1527-1600
- **Description:** Process control: start, stop, and switch processes
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 cpu datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1521-1532
- **Purpose:** STAPROC - start process; SWITPROC - switch process (stop current and restart another); NSTOPROC - stop process

---

### 19. **M5TMOUT** (Lines 1609-1678)
- **Line Range:** 1609-1678
- **Description:** Monitor call M5TMOUT - timeout handling
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 cpu datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1603-1612
- **Purpose:** Handle ND-500 timeout monitor call, insert message in time queue

---

### 20. **NINSTR, XNINSTR** (Lines 1680-1682, 1817-1929)
- **Line Range:** 1680-1682 (declaration), 1817-1929 (implementation)
- **Description:** Monitor call DVIO and DVINST - device input string
- **Execution Level:** Level 12 (Driver level)
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1807-1816
- **Purpose:** Handle device input string operations

---

### 21. **DVIO, NOUTSTR, OSTRS, PT5RST** (Lines 1688-1804)
- **Line Range:** 1688-1804
- **Description:** Device output and terminal driver routines
- **Execution Level:** Driver level (Level 12)
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1682-1687
- **Purpose:** DVIO/NOUTSTR - output string to device; OSTRS - output string restart; PT5RST - restart ND-500 from terminal output driver (Level 10)

---

### 22. **GERRC** (Lines 1940-1954)
- **Line Range:** 1940-1954
- **Description:** Get error code after a programmed trap
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1932-1939
- **Purpose:** Used after a programmed trap to retrieve error code

---

### 23. **5SIBMO** (Lines 1965-1991)
- **Line Range:** 1965-1991
- **Description:** Special monitor call from SIBAS server in ND-500
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1957-1964
- **Purpose:** Handle SIBAS (database) server monitor calls

---

### 24. **SPRIO** (Lines 2001-2038)
- **Line Range:** 2001-2038
- **Description:** Set priority
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 1994-2000
- **Purpose:** Change process priority, handle timeslicing

---

### 25. **SWMC** (Lines 2047-2051)
- **Line Range:** 2047-2051
- **Description:** Monitor call to the swapper
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message (=MESSAGE)
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 2041-2046
- **Purpose:** Activate swapper with trap information

---

### 26. **XMWK** (Line 2054)
- **Line Range:** 2054
- **Description:** XMWK stub
- **Execution Level:** Not specified
- **Purpose:** XMSG wakeup routine (implemented later)

---

### 27. **A5XMSG, B5XMSG** (Lines 2062-2414)
- **Line Range:** 2062-2414
- **Description:** XMSG interface routines
- **Execution Level:** Level 12 (Driver level)
- **Entry Parameters:**
  - X = Current message
  - B = ND-500 CPU datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 2055-2061
- **Purpose:** Handle XMSG (message system) function calls from ND-500. Supports functions 0-57 including: get/release buffers, read/write headers, open/close ports, send/receive messages, etc.

---

### 28. **5MTRANS** (Lines 2440-2833)
- **Line Range:** 2440-2833
- **Description:** Monitor call executed on driver level for disk transfers
- **Execution Level:** Driver level
- **Entry Parameters:**
  - X = Current message
  - B = ND-500 cpu datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 2418-2438
- **Purpose:** Handle 8 different disk transfer and process control functions:
  1. Disk transfer, return immediately
  2. Disk transfer, wait until finished
  3. Check event, return immediately
  4. Check event, wait
  5. Check event, nowait
  6. Check event, wait
  7. Start process, return immediately
  8. Start process and wait

---

### 29. **CHDISKADDR** (Lines 2715-2745)
- **Line Range:** 2715-2745
- **Description:** Check if disk address is within file
- **Execution Level:** Not specified (subroutine)
- **Entry Parameters:**
  - B-reg: Pointer to disk access queue element
- **Exit Parameters:** Not specified
- **Comments:** Lines 2705-2710
- **Purpose:** Validate disk address range for file operations

---

### 30. **5MRDTRANS** (Lines 2759-2833)
- **Line Range:** 2759-2833
- **Description:** Driver level routine activated after transfer is finished
- **Execution Level:** Driver level
- **Entry Parameters:**
  - B = Disk access queue element (removed from acc queue)
- **Exit Parameters:** Not specified
- **Comments:** Lines 2751-2758
- **Purpose:** Handle completion of disk transfers, restart waiting processes

---

### 31. **5ACTSWAPPER** (Lines 2851-2908)
- **Line Range:** 2851-2908
- **Description:** Activate the ND-500 swapper process
- **Execution Level:** Must be called in IOF when called from levels other than driver level
- **Entry Parameters:**
  - X = Message requiring service from swapper
- **Exit Parameters:**
  - T, A, D regs are destroyed
- **Comments:** Lines 2839-2850
- **Purpose:** Activate swapper for page faults and swap operations

---

### 32. **XTER500** (Lines 2923-2962)
- **Line Range:** 2923-2962
- **Description:** Routine to terminate (stop) ND-500
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - X = -1 (no actual message) or X><-1 (actual message)
- **Exit Parameters:**
  - Error: ND-500 not terminated
  - Exit+1: OK, ND-500 stopped
- **Comments:** Lines 2911-2922
- **Purpose:** Stop ND-500 CPU, wait for termination or timeout

---

### 33. **XACTRDY** (Lines 2974-3040)
- **Line Range:** 2974-3040
- **Description:** Activate ND-500 with this message if any CPU is idle or priority higher than current running ones
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - X = Message
- **Exit Parameters:** OK
- **Comments:** Lines 2964-2973
- **Purpose:** Schedule ND-500 message for execution based on priority

---

### 34. **XACT500** (Lines 3052-3099)
- **Line Range:** 3052-3099
- **Description:** Activate the ND-500
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - B = ND-500 cpu-datafield
  - X = Message
- **Exit Parameters:**
  - T, A, D registers are destroyed
- **Comments:** Lines 3043-3051
- **Purpose:** Start ND-500 with first waiting message

---

### 35. **XRSTARTALL** (Lines 3113-3240)
- **Line Range:** 3113-3240
- **Description:** Restart all SINTRAN III programs with messages in the N500 execution queue with an error message
- **Execution Level:** Level 12 (Driver level)
- **Entry Parameters:**
  - A = Error status
  - B = CPU df
- **Exit Parameters:** Not specified
- **Comments:** Lines 3102-3112
- **Purpose:** Error recovery - restart all processes with error status (WM-400)

---

### 36. **CHACTIVEQ** (Lines 3252-3267)
- **Line Range:** 3252-3267
- **Description:** Check number of active CPUs in the system
- **Execution Level:** Monitor level
- **Entry Parameters:** None
- **Exit Parameters:**
  - Exit: No active CPU
  - Exit+1: One or more active CPUs
- **Comments:** Lines 3243-3251
- **Purpose:** Determine if any ND-500 CPUs are active

---

### 37. **XKICK500** (Lines 3278-3316)
- **Line Range:** 3278-3316
- **Description:** Kick the ND-500 via octobus
- **Execution Level:** Level 12 (must be called in IOF if called from other levels)
- **Entry Parameters:**
  - A = Kick type (in CKICKTYPE)
- **Exit Parameters:** Not specified
- **Comments:** Lines 3269-3277
- **Purpose:** Send octobus kick message to ND-500

---

### 38. **XRS5CPU** (Lines 3328-3343)
- **Line Range:** 3328-3343
- **Description:** Send octobus "Reset CPU" multibyte message
- **Execution Level:** Monitor level
- **Entry Parameters:**
  - B = CPU df
- **Exit Parameters:**
  - Exit: Error
  - Exit+1: OK
- **Comments:** Lines 3319-3327
- **Purpose:** Reset ND-500 CPU via octobus

---

### 39. **RS5CPU** (Lines 3352-3369)
- **Line Range:** 3352-3369
- **Description:** Reset all active CPUs
- **Execution Level:** Level 1 (5pit)
- **Entry Parameters:** None
- **Exit Parameters:** Not specified
- **Comments:** Lines 3346-3351
- **Purpose:** Reset all active ND-500 CPUs in system

---

### 40. **5OMBREAD, I5OMBR** (Lines 3453-3551)
- **Line Range:** 3453-3551
- **Description:** Read octobus message from the ACCP
- **Execution Level:** Monitor level
- **Entry Parameters:** None (activated by multibyte message received on reserved OMD number)
- **Exit Parameters:** Not specified
- **Comments:** Lines 3372-3452 (extensive documentation of error record structures)
- **Purpose:** Handle error messages from:
  - ACCP (memory errors)
  - 5000-MP (microprogram errors)
  - MF-controller (controller errors)

---

### 41. **CON5OMD** (Lines 3567-3572)
- **Line Range:** 3567-3572
- **Description:** Connect octobus OMD
- **Execution Level:** Level 1 (5pit)
- **Entry Parameters:**
  - B = Working field
- **Exit Parameters:** Not specified
- **Comments:** Lines 3554-3566
- **Purpose:** Connect octobus OMD number to receive multibyte error messages from MF controller, ACCP, and 5000 microprogram

---

### 42. **MFPREPARE** (Lines 3586-3598)
- **Line Range:** 3586-3598
- **Description:** Send OMD number to MF-controller
- **Execution Level:** Level 1 (5pit)
- **Entry Parameters:**
  - B = Working field
  - A = Destination octobus station number
- **Exit Parameters:** Not specified
- **Comments:** Lines 3575-3585
- **Purpose:** Inform MF-controller which OMD N100 can receive messages on. Ack/Nack answer handled by 5OMBREAD

---

### 43. **CON5IDENT** (Lines 3614-3634)
- **Line Range:** 3614-3634
- **Description:** Connect octobus Ident
- **Execution Level:** Level 1 (5pit)
- **Entry Parameters:**
  - B = CPU datafield
  - X = Working datafield
- **Exit Parameters:** Not specified
- **Comments:** Lines 3601-3613
- **Purpose:** Connect octobus Ident to receive multibyte messages/kicks from ACCP/Samson. Send 'alive' message to verify ACCP presence. Ack/Nack handled by 5OMBREAD

---

### 44. **MBSUSPROC** (Lines 3648-3657)
- **Line Range:** 3648-3657
- **Description:** Check if a process should be suspended
- **Execution Level:** Called from MCHANDLE
- **Entry Parameters:**
  - B = ND-500 CPU datafield
  - N5MESSAGE = Addr of actual message
- **Exit Parameters:** If suspended, returns to NXTMSG in main ND-500 driver
- **Comments:** Lines 3637-3647
- **Purpose:** Suspend process if system spends too much time on higher interrupt levels (more than 1 second)

---

### 45. **5GTDF** (Lines 3670-3693)
- **Line Range:** 3670-3693
- **Description:** Get terminal data field
- **Execution Level:** Not specified
- **Entry Parameters:**
  - X = ND-500 message addr
- **Exit Parameters:**
  - Exit: Not terminal or terminal not reserved by caller
  - Exit+1: A = Addr of terminal input datafield, D = Addr of terminal output datafield
- **Comments:** Lines 3660-3669
- **Purpose:** Validate and retrieve terminal datafield addresses

---

### 46. **INSMONCO** (Lines 3702-3713)
- **Line Range:** 3702-3713
- **Description:** Restart ND-500 process after input bytes written to data memory
- **Execution Level:** Driver level
- **Entry Parameters:** None (uses context from message)
- **Exit Parameters:** Not specified
- **Comments:** Lines 3696-3701
- **Purpose:** Handle completion of quick instring monitor call 504

---

### 47. **XMWK, N5WAKE** (Lines 3727-3739)
- **Line Range:** 3727-3739
- **Description:** Wake up ND-500 process
- **Execution Level:** Driver level
- **Entry Parameters:**
  - B = ND-500 message
- **Exit Parameters:** Not specified
- **Comments:** Lines 3716-3726
- **Purpose:**
  - XMWK: Started from xmsg when message arrives on port belonging to ND-500 process
  - N5WAKE: Started from monitor call handling on 5pit

---

### 48. **P12DCN** (Lines 3746-3749)
- **Line Range:** 3746-3749
- **Description:** Driver level routine to disconnect
- **Execution Level:** Driver level (Level 12)
- **Entry Parameters:** Not specified
- **Exit Parameters:** Not specified
- **Comments:** Lines 3742-3745
- **Purpose:** XMSG disconnect routine

---

### 49. **ST0PSYS** (Lines 3759-3802)
- **Line Range:** 3759-3802
- **Description:** Continuation on monitor level of STOP-SYSTEM command
- **Execution Level:** Monitor level
- **Entry Parameters:** None
- **Exit Parameters:** Not specified
- **Comments:** Lines 3752-3758
- **Purpose:** Terminate all ND-500s (if any) and simulate power failure. Clears caches, dumps data, resets CPUs, and triggers power fail sequence

---

## Summary Statistics

- **Total Subroutines Documented:** 49
- **Main Categories:**
  - Driver Level (Level 12): ~30 APIs
  - Monitor Level: ~12 APIs
  - Level 1/2 (Interrupt levels): ~7 APIs

- **Key Functional Areas:**
  1. Process Control (start, stop, switch, restart)
  2. Message Handling (execution queue, dispatch)
  3. Monitor Call Processing (MCHANDEL and related)
  4. Swapper Control (page faults, disk I/O)
  5. XMSG Interface (message system)
  6. Device I/O (terminal, disk)
  7. Error Handling (traps, timeouts, restarts)
  8. Octobus Communication (kicks, error messages)
  9. CPU Control (activate, terminate, reset)
  10. System Control (histogram, power fail, stop system)

---

## Notes

- Many routines require specific execution levels and interrupt states (IOF/ION)
- The file implements a complete ND-500 shadow processing system for ND-100
- Extensive use of assembly-like instructions (*LDATX, *STATX, etc.)
- Complex interaction between multiple privilege levels (1, 2, 10, 12, 14)
- Sophisticated error handling and recovery mechanisms
- Multi-CPU support with priority-based scheduling

---

**Generated:** 2025-10-13
**Source File Path:** Z:\NorskData\Source Code\Sintran L\NPL\MP-P2-N500.NPL
