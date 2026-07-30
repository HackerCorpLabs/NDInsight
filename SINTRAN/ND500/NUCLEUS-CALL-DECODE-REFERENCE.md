# NUCLEUS call decode reference (for decoding ND-5000 swapper/domain port traffic)

All facts VERIFIED from `Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md`
(Chapter 6 Overview + Chapter 7 Library), with page/section citations. Items not in the read text are
marked **[OPEN]** - NOT guessed.

## Model (verified, section 6.3, pages 193-194)
- Descriptions live in **physical memory shared between the CPUs** (the NUCLEUS kernel).
- **Message** = a physical DATA buffer + a header (buffer descriptor + link to other messages).
- **Port** = owner id + pointer to received messages; messages are queued FIFO (arrival order).
- **Home port**: supplied at create; default receive port + answer path (clients/servers).
- **Sender port**: supplied at send; who sent it (read via nkGetInfo). **Sendreference**: needed to
  send to a port; used for access checking.
- `message`, `port`, `sendreference` are DESCRIPTOR NUMBERS (indices), NOT addresses. Status is
  always an INTEGER4 outvalue; every parameter is INTEGER4 (bytes/data are BYTES POINTER).
- Slow services = create port/message; fast (reusable message) = send/receive/move (section 6.3 p194).
- **ND-5000 microcodes exactly nkMove, nkSend, nkReceive, nkGetInfo** (line 6192 / p192); all other
  nk* run in ND-100.

## The four microcoded calls (verified, sections 7.3.6-7.3.10, pages 214-222)

`nkCreMessage(0, bytes, homeport, =message)` [7.3.6 p214] - alloc contiguous physical buffer of
`bytes`; creator has EXCLUSIVE access until it is sent (then loses access); homeport 0 = dummy (msg
lost if sent home); returns the message NUMBER.

`nkMove(function, message, displacement, (=)data, =bytes)` [7.3.7 p216] - function 0=nkfRead,
1=nkfWrite, 2=nkfInsert (write but don't move byte pointer if shorter). Buffer = `Bytes(0:msglngth-1)`.
ND-100: `displacement` must be EVEN; byte-pointer min/max index range 0-64511. `=bytes` = actually
moved. Read stops at current byte counter; write/insert stops at max buffer.

`nkSend(0, port, sendreference, message)` [7.3.8 p218] - append `message` to the dest port's queue;
sender LOSES access. `port`=0 => sender port not set. `sendreference`=0 => send to the message's HOME
port. `message`=0 => do NOT send, just RESTART the destination port's process. If dest port empty, the
send activates the owning process (if requested at create). nke_PortClosed if receive port closed.

`nkReceive(0, port, =message, =bytes)` [7.3.9 p220] - receive FIRST queued message (FIFO); empty queue
=> `message`=0. Receiver gains read/write access. `=bytes` = bytes the sender wrote (<= message size).

`nkGetInfo(function, message|port|sendreference, =value)` [7.3.10 p221] info selector:
0 nkfSize (max size, msg), 1 nkfLength (used length, msg), 2 nkfHomeid (msg=home id / port=port id /
sendref=dest id; 64-bit, compare-only), 3 nkfLastid (last sender port, msg, 64-bit compare-only),
4 nkfBuffer (**message BUFFER ADDRESS in the NUCLEUS kernel** - the data location), 5 nkfQueue (port:
0=no msg / 1=has msg). Returns 32b for fn 0/1/4/5, 64b for 2/3 (future 128b).

## Subfunction + error tables (verified, Table 5 p201 + section 7.2.1 p202)
Multi-function calls: nkCrePort 0 nkfNoDelayAbort / 1 nkfDelayAbort; nkOpenReturnPort 0 nkfOpenHomePort
/ 1 nkfOpenLastPort; nkClose 0 nkfRemove / 1 nkfReject (messages only); nkVersion 0 nkfLibrary /
1 nkfKernel / 2 nkfStation. (nkGetInfo/nkMove selectors above.)

Errors (base `nke_ERROR_BASE = 101000b`): 101001 ILLPAR, 101002 ILLTYPE (bad port/msg/sendref),
101003 NOMESS (send: both port & msg zero), 101004 ILLNO (out of range), 101005 NOTLOCAL (receive from
remote), 101006 OUTSIDE (displacement outside buffer), 101007 DESCARRFULL, 101010 BUFFULL, 101011
NAMEFULL, 101012 NAMENOTFOUND, 101013 NAMEUSED, 101014 NOACCESS, 101015 ILLNETADDRESS, 101016
ILLKERNELNO, 101017 NETTABFULL, 101020 PROTOCOLERROR, 101021 REJECTED. Constants in `NK-ERRCODE:DEFS`.

## Kernel tables + descriptor/buffer layouts (VERIFIED field ORDER + meaning; section 7.4, Figs 23-27, pages 226-228)
Byte WIDTHS/offsets are NOT given by the figures - only field order + meaning. Do NOT assume widths.

- **Kernel tables (Fig 23):** MASTER BLOCK -> {descriptor table, hash array, hash mask, kick table,
  net-addr table, buffer-area start, buffer-area end}.
- **Descriptor type tag** = 2nd field of every descriptor: **2=Message, 3=Port, 4=Sendreference**
  (1st field is LOCK, used for TSET). This is the discriminator when decoding a descriptor slot.
- **MESSAGE descriptor (Fig 24, TYPE=2):** LOCK, 2, HEAD OWNER (access check), FREELINK, USER (quota),
  LINK (from receiving port), **BUFFERPOINTER (-> buffer record)**, **HOMEPORT (-> home port)**,
  HASHLINK (remote-msg id), COMSTAT, OWNINDEX (descriptor #), TRACECOND (0=no trace).
- **PORT descriptor (Fig 25, TYPE=3):** LOCK, 3, HEAD, OWNER (access check), FREELINK, USER,
  MESS HEAD (queue start), MESS TAIL (queue end), KICKLINK, KICK HEAD (kicktable queue head),
  **KICK DEST (= OCTOBUS station no)**, INQUEUE (0=not in kickqueue), KICK PROC (process to kick),
  EVENTS 1, EVENTS 2, OWNINDEX, PRANDOM (magic), NETTADDR, OPENCOUNT, NAMED. OWNID identifies the port.
  -> ties a NUCLEUS port directly to an octobus station + a to-be-kicked process.
- **SENDREF descriptor (Fig 26, TYPE=4):** LOCK, 4, HEAD OWNER, FREELINK, USER, DEST PORT ID,
  DESTINATION PORT (pointer).
- **MESSAGE BUFFER (Fig 27, in buffer area, what nkfBuffer points at):** PROTOCOL (NUCLEUS protocol
  version), MESSAGE STATUS (e.g. rejected), PORT ID (dest), MESSAGE ID (original), HOME ID (home
  receive port), LAST ID (last send port), SIZE (max bytes), LENGTH (bytes used), BUFFER(0:...) (user
  data start). -> the 8-field header precedes the user data the swapper reads/writes via nkMove.

## [OPEN] - NOT yet verified (do NOT assume)
- Exact BYTE WIDTHS/offsets of every descriptor+buffer field above (figures give order+meaning only),
  and whether fields are ND-100 words vs ND-500 32-bit.
- The mapping of nkMove/nkSend/nkReceive/nkGetInfo to their B30 microword CS routines - NOT carved;
  the swapper diagnostic's throwing-word Mpc trace can locate them once the swapper reaches these calls.
- On ND-5000 these four are microcoded; the exact octobus/mailbox message shape they emit vs. the
  ND-100 monitor-call path for the slow calls.

Source manual: `E:\Dev\Ronny\NDInsight\Reference-Manuals\500\ND-820026.1 EN DOMINO and NUCLEUS Software Guide.md`
