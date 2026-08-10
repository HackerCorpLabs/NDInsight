# APPEND-REMOTE-BATCH ACCEPTED over Ethernet (2026-08-09)

**CORRECTED.** The submission was accepted (`OK`) and the letter and reply are good.
The JOB ITSELF did not run: its output file came back EMPTY, because the job file was
mangled during terminal authoring - see the end of this file. `OK` means queued, not
executed.

`eth-batch-success.pcapng`, captured on `\Device\NPF_Loopback`, `tcp port 5010`.

This is the first captured ACCEPTED submission. The companion folder `ETH-BATCH-2026-08-09` holds the
failure case; together they separate the status byte from a constant.

## Prerequisites - what "remote batch" actually needs

From `ND-60.128.5` APPEND-BATCH, rules 5 and 6, and confirmed live:

 1. **A batch processor must be RUNNING on the remote.** D102 had five, all PASSIVE.
    `@BATCH` starts one and answers `BATCH NUMBER = 1`. Without this the submission cannot run.
 2. **The job file must begin with `@ENTER <user>,<password>,<cpu minutes>`** - it identifies
    the job's owner, and a batch job has no terminal session to inherit one from.
 3. **The job file must end with two consecutive ESC characters** (octal 33). Those correspond
    to `@LOGOUT` in direct mode.
 4. **The output file must already exist.** The submission names it and appends to it.

The job used here, on D102 as `ARBJOB3:SYMB`:

```
@ENTER SYSTEM,,1
@TIME
<ESC><ESC>
```

Submitted from D100:

```
@TRANSFER-FILE
F-T: APPEND-REMOTE-BATCH
  Batch system and user name? D102(SYSTEM)
  Input file?                ARBJOB3:SYMB
  Output file?               ARBOUT:SYMB
OK
```

`OK` - accepted.

## The exchange

```
100->102  F1=0A88  sub=0E  len 72  the *XFTRA letter
102->100  F1=0A88  sub=03  len  0  ack
100->102  F1=0A89  sub=0E  len  6  00 43 0000 0016
102->100  F1=0A89  sub=07  len  0
100->102  F1=0A8A  sub=03  len  0  ack
102->100  F1=0A8A  sub=0E  len 70  01 00 0000 FF 06 9000 "ARBJOB3'" ... "SYMB"
```

## THE STATUS BYTE - now separated from a constant

XROUT overwrites the request's service byte with a return status. Byte 1 of the reply:

| Run | Byte 1 | Outcome |
|---|---|---|
| `ETH-BATCH-2026-08-09` (input file absent) | `0x16` | failed, and parameter 1 carried 46 = NO SUCH FILE NAME |
| this one | `0x00` | accepted |

The failure write-up recorded `0x16` as UNKNOWN because one observation could not tell a
failure code from a constant. A second run with a different outcome settles it: **byte 1 is a
status, zero means accepted.**

## CORRECTED AGAIN: the job DID execute - its output went to the console

Ronny saw the batch output on the TDV console. So the remote job RAN. The output file named in
the submission stayed empty because the output went to the ERROR DEVICE - `@GET-ERROR-DEVICE`
on D102 answers `1`, the console - not because nothing happened.

Isolated afterwards by varying ONE thing at a time:

| Submission | Job runs? | Output lands in |
|---|---|---|
| local `APPEND-BATCH` | yes | the named output file |
| remote `APPEND-REMOTE-BATCH` | **yes** | the console |

The same job file was used for both, and a second local run WITHOUT `SET-ERROR-DEVICE` also
wrote its file correctly - so that command was not the difference either.

**Two wrong diagnoses, in opposite directions, before the right one:** first that the job file
was mangled (it was, initially, but a correct one behaved the same); then that the job never
ran. It ran. An empty output file is not evidence that nothing happened - it only says the
output is somewhere else.

## Superseded reasoning, kept because the mistake is instructive

`ARBOUT:SYMB` on D102 was read afterwards and is EMPTY, with batch processor 1 back at `IDLE`.
So the job was accepted, started and aborted.

The cause is the job file. Typing it over the terminal produced ONE line - `@TIMER SYSTEM,,1` -
because a bare CR returns the cursor without starting a new record, so `@TIME` overwrote the
start of `@ENTER SYSTEM,,,1`. CRLF and a raw LF behaved the same. With no valid `@ENTER` the
job has no owner and is aborted at once.

I had earlier concluded the opposite: that the readback display was misleading and the `OK`
proved the file was fine. **That was wrong.** `OK` means the submission was queued and says
nothing about the job's validity. The readback was accurate.

**The only evidence a job ran is its OUTPUT file.**

## Still not established
 - The layout of the success reply past byte 1. It carries the job file name and its type, and
   is 70 bytes against the failure's 8, but its parameter structure is not decoded here.
 - What the `sub=07` frame in the middle is doing. Subtype 0x07 is the network-error class
   elsewhere, which sits oddly inside a successful exchange, and one observation is not enough
   to say what it means.
