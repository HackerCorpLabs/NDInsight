## Page 1

# ND-Specific Programming & Advanced PLANC

**ND-20.034.1 EN**

```
   ND
 Norsk Data
```

---

## Page 2

# Preface

## THE READER

If you are an ND application programmer working with existing hardware, have knowledge of the programming language PLANC or a similar language, and would like advice on how to make your ND computer programs faster and on new PLANC features and how they can be used, then this manual is written specifically for you.

## PREREQUISITE KNOWLEDGE

Knowledge of PLANC's facilities will help when using this manual. But PLANC is a structured programming language closely related to PASCAL, MODULA, C and so on, and those trained in these should not have difficulties in reading most of the contents.

## THE MANUAL

This manual contains general advice on how to make fast, maintainable and reliable programs on existing ND systems, with examples. The examples are written in PLANC, which is ND's internal development language and which is equipped with features that are conducive to good and efficient programming.

## RELATED MANUALS

The most closely related manual is the SINTRAN III Tuning Guide, which contains much performance related information.

| Manual                                            | Reference     |
|---------------------------------------------------|---------------|
| PLANC Reference Manual                            | ND-60.117     |
| PLANC Utility Library and PLANC-GEN               | ND-20.013     |
| Language Interfacing on ND machines               | ND-60.302     |
| Ada User Guide                                    | ND-60.198     |
| ND FORTRAN Reference Manual                       | ND-60.145     |
| COBOL Reference Manual                            | ND-60.144     |
| C Reference Manual                                | ND-60.251     |
| Pascal Reference Manual                           | ND-60.222     |
| Symbolic Debugger User Guide                      | ND-60.158     |
| LED User Guide                                    | ND-60.266     |
| BRF-Linker User Manual                            | ND-60.196     |
| Linkage-Loader User Guide                         | ND-60.182     |
| Automake User Manual                              | ND-60.232     |
| SINTRAN III Monitor Calls                         | ND-60.228     |
| Butterfly Work Station, MS-DOS Reference Manual   | ND-60.271     |
| Operator Environment User Guide                   | ND-30.061     |

Norsk Data Internal Use Only

---

## Page 3

# References

## Control System

- Performance Monitoring is covered in chap 6, Control System

## SINTRAN III

- SINTRAN III Tuning Guide, ND-30.049
- SINTRAN III Release Information, K-version

## Dialogue and SIBAS/R

- Dialogue and SIBAS/R Introduction ND-60.256
- Dialogue and SIBAS/R Database Definition Language (DBL) ND-60.282
- Dialogue and SIBAS/R Application Development (DML) ND-60.256

## DOS 3.10

- Programmer Manual (from Ericsson)
- LINK is covered in chap 5 pp. 5-3 to 5-22

## Microsoft C Compiler

- LINK is covered in chap 4, Linking
- LIB is covered in chap 6, Managing Libraries

## Microsoft MS-DOS

- MS-DOS Programmer's Reference Manual, which is Microsoft document no. 8411-310-02 and Microsoft part no. 036-014-012.

## Microsoft Windows

- Windows Software Development Kit, Programmer's User Guide, Microsoft document No. 050051051-100-100-1185, Microsoft Part No. 050-150-029.

---

Norsk Data Internal Use Only

---

## Page 4

# Table of Content

1. Introduction .............................................. 2

2. Improving the performance of your program ................. 7
   2.1 Some myths ........................................ 7
   2.2 Design issues ..................................... 8
      2.2.1 Designing for speed ........................... 8
      2.2.2 Word alignment on ND-5000 ..................... 9
      2.2.3 Programming for cache hits .................... 9
      2.2.4 Bad & good I/O ................................ 10
      2.2.5 Communication ................................. 13
   2.3 Data compression ................................... 16
   2.4 SIBAS hints ........................................ 17
      2.4.1 Operating system files ....................... 17
      2.4.2 Columns ...................................... 17
      2.4.3 Indexes ...................................... 18
      2.4.4 Set-referrals ................................ 19
      2.4.5 Applications ................................. 19
      2.4.6 Redefinition ................................. 20
      2.4.7 The SINTRAN III Bit-file ..................... 20
   2.5 Case study - optimizing the SORT-MERGE routine ..... 21
      2.5.1 The results obtained with SORT-MERGE-H and ZOOM . 21
      2.5.2 Design and I/O strategy ...................... 22
      2.5.3 Memory strategy .............................. 24
      2.5.4 Algorithms ................................... 25
      2.5.5 Possible improvements ........................ 26
   2.6 A code standard .................................... 26
      2.6.1 Variables, Types and Constants ............... 27
      2.6.2 Naming Conventions ........................... 28
      2.6.3 Modules ...................................... 30
      2.6.4 Routines ..................................... 32
      2.6.5 Design of libraries .......................... 33
   2.7 A list of libraries and resources ................... 34

3. PLANC Compiler Commands .................................. 39
   3.1 New compiler commands ............................... 39
   3.2 The SELECT Command .................................. 40
      3.2.1 Select demo .................................. 42
   3.3 PLANC Compiler Commands demo ........................ 45
   3.4 Let PLANC generate the IMPORT declarations .......... 46
      3.4.1 Some code with EXPORTs in it .................. 46
      3.4.2 How to make the IMPORTs ....................... 47
      3.4.3 The IMPORTs that are made ..................... 47

Norsk Data Internal Use Only

---

## Page 5

# PLANC Features and Development Tools

| Section | Title                                                                            | Page |
|---------|----------------------------------------------------------------------------------|------|
| 4.1     | Summary of new features                                                          | 51   |
| 4.2     | Readability hints                                                                | 60   |
| 4.3     | Packing on the various processors                                                | 61   |
| 4.3.1   | On the ND-500(0)                                                                 | 61   |
| 4.3.2   | On the ND-100, MC680xx and INTEL 286/386                                         | 61   |
| 4.4     | Stacks - usefulness and fallacies                                                | 62   |
| 4.5     | Some remarks on parameter transfer                                               | 66   |
| 4.5.1   | The data/runtime organization on the ND-500(0)                                   | 67   |
| 4.6     | Routine modifiers                                                                | 69   |
| 4.7     | UNIX command line/environment pointer retrieval                                  | 72   |
| 4.7.1   | Example                                                                          | 72   |
| 4.8     | Records                                                                          | 74   |
| 4.8.1   | Simple record usage                                                              | 74   |
| 4.8.2   | Variant record usage                                                             | 75   |
| 4.8.3   | Linked list of records                                                           | 76   |
| 4.8.4   | Linked list of records with New                                                  | 78   |
| 4.8.5   | Object oriented programming examples                                             | 80   |
| 4.8.6   | A demo run                                                                       | 85   |
| 4.8.7   | Record component inheritance - a lengthy exposition                              | 86   |
| 4.9     | Routine pointers and indirect routine invokation                                 | 93   |
| 4.10    | Using routines and RETURN as a control structure                                 | 94   |
| 4.11    | Mixing PLANC and C                                                               | 97   |
| 4.11.1  | A C main() calling PLANC calling C demo                                          | 97   |
| 4.12    | Portable programming in PLANC                                                    | 99   |

# Index

---

Norsk Data Internal Use Only

---

## Page 6

# Introduction

Norsk Data Internal Use Only

---

## Page 7

I'm sorry, but the image you provided is not visible. Could you try uploading it again or provide a description of the content?

---

## Page 8

I'm sorry. The page appears to be blank or not legible enough to transcribe any text or details.

---

## Page 9

# Introduction

The intention of this manual is show you what you can do to make better programs. The goal is to give advice about how to write good, efficient programs.

When we say good programs, we mean programs that have the following properties:

- **Minimal resource load**   
  This in turn implies minimum use of CPU, of I/O and of memory. We want to show you where the bottlenecks are and how you can avoid them. What our marketing people/customers ask for is good response time. That quality depends critically upon how efficient the resource usage of the currently active individual programs at any given time is.

- **Ease of testing and maintenance**   
  Here, a clean, good, logical design are most important. PLANC has several features that help you in this direction, such as modularity and object orientation.

The first point is an important one, to which chapter two is dedicated. It contains advice gleaned from experience with ND systems.

* * *

Norsk Data Internal Use Only

---

## Page 10

# Introduction

The importance of the point can be illustrated as in the following graph. The following graph shows the response time as a function of the CPU load (which is the same as the time the CPU is not idle).

```
  RESPONSE AS A FUNCTION OF CPU LOAD
           (TIME THE CPU IS NOT IDLE)
  
  ^ 
  | Response
  | time 
  |
  |
  |
  |_________________________________________
             100 % CPU load
```

The graph shows that in a system that is almost "saturated", i.e. close to 100% CPU load, small improvements matter. Improvements in one important product on such a system may give better response time for the system as a whole, without any hardware changes at all.

Chapters three and four introduce new as well as old but little known features in PLANC and its command processor that may help you as a developer to make your programs better.

Note that we have excluded an important quality aspect, which is the quality of the user interface and the documentation. (But logical design certainly helps in this direction - remember that a program that cannot be explained/explain itself, is probably badly designed and implemented.)

This manual is not about clever algorithms, with the possible exception of the discussion of the Sort routine. For that kind of hints and information, see the well known books about *Programming Pearls* by Jon Bentley, *Kernighan & Plauger's book on Software Tools*, *Solutions in C* by Rex Jaesche, Knuth's volumes and so on. Instead, we focus on ND-specific programming - hints and advice that pertains to ND's products. PLANC will be used for examples - it is a well structured language, it runs on all CPUs and OSs that ND uses and so imposes some standardization that facilitates portability of existing and new ND products, and it links the past with the present.

*Norsk Data Internal Use Only*

---

## Page 11

# Introduction

When developing software products, knowledge of the *ND Software Development Handbook, ND–40.009.2* is also useful.

The traditional way of learning at ND's R&D department has been to osmotically absorb standard solutions, tricks, folklore, and hints from others. Many have felt this to be an unsatisfactory state of affairs. As a consequence, we now have some tools, libraries and databases available for both general use and special purposes. Some examples of the general variety are the *PLAN C Utility Library* and the associated *PLAN C-GEN* screen picture generator, and the database called *The-Hacker* on *NDHD-LYNET*. The-Hacker contains routines that programmers have made for their own purposes, but which are considered to be potentially useful to others.

Between 30 and 40 (we believe, it may be more) libraries for special purposes such as communication and screen handling also exist. To this profusion will be added whatever is available for the new operating systems and CPUs that are being introduced, and the new compilers that are being evaluated and perhaps bought. The PLAN C compiler has been or will be implemented on various CPUs and under various operating systems.

*Norsk Data Internal Use Only*

---

## Page 12

# Improving the Performance of Your Program

Norsk Data Internal Use Only

---

## Page 13

I'm sorry, but the page appears to be blank. If there's a specific image with text or diagrams you have that needs conversion, please provide that image.

---

## Page 14

# Improving the Performance of Your Program

## 2 Improving the Performance of Your Program

When an ND application runs slowly, it is usually not because tight loops haven't got the attention they deserve. It is because too little attention has been paid to economical use of the CPUs, I/O facilities and to data communications.

In this chapter, we will say a few words about how you can make your pure code fast (that is, code without any explicit monitor calls, only using SINTRAN to swap in pages that are demanded). Some of the chapter is devoted to a suggested code standard for making code readable and maintainable and some is devoted to a list of libraries. However, most of the material presented here is basically about how you do comms and I/O most efficiently.

---

### 2.1 Some Myths

When inefficient practices continue to exist, it is often because they are based on "myths" (or "conventional wisdom"), that is, habits or beliefs that are unconsciously accepted and never critically reviewed.

In ND, some such myths are:

- **File-as-segment is the most efficient way to do I/O on an ND-500(0).**
  This is only true for random I/O on an indexed file. Otherwise, blocked or random (not formatted) Input/Output statements are usually better. Sequential I/O on contiguous files is very fast. The PIANC Utilities contain routines for buffered I/O, which is a good alternative.

- **The ND-100 bus capacity limits the speed of disk I/O.**
  Measurements show that the I/O traffic on the bus could be increased by a factor of ten. The main bottleneck is in single-thread I/O system software. For instance, the 500(0) swapper must finish one page fault before it can serve the next.

---

*Norsk Data Internal Use Only*

---

## Page 15

# Improving the Performance of Your Program

## Some Myths

- **The transfer rate on the comms media (EtherNet cable, Megalink, and so on) limits the speed of comms.**

  On fast comms media, the I/O CPU is often the bottleneck. It always takes time to send even an empty message - it must be equipped with an address, shipped over comms medium, and so on even if it does not contain anything useful. But the CPU load per message increases slowly with the number of bytes in it, so it is cost-effective to send as many bytes per message as possible.

---

## 2.2 Design Issues

In this section, we will spell out a few facets of the amorphous term "performance". Again, the important topics of quality of interfaces, usability, and so on are neglected. The design issues that will be discussed have in common that they are available for quantitative measurements and lie entirely within the realm of "pure" programming. Very few attempts have been done at ND to measure more "human" aspects of software quality.

Another important design issue, that of portability between CPUs and operating systems, is discussed in the chapter on PLANC Programming. See p. 99.

---

### 2.2.1 Designing for Speed

*Disturb the CPU as seldom as possible.* Frequent software-generated interrupts cause frequent context shifts and execution of significant amounts of code in the monitor. Both take time.

Whenever you do I/O or use comms, read/write/send/receive as big chunks as you can. *Use big buffers!*

---

Norsk Data Internal Use Only

---

## Page 16

# Improving the Performance of Your Program

## 2.2.2 Word Alignment on ND-5000

Words or data structures straddling the four-byte word boundaries greatly increase the memory access time, because more than one memory cycle is needed to access such structures.

The PLANC compiler handles its variables in such a way that the gains you get from paying attention to alignment under ordinary circumstances are neglectable. But if you use packed records, then you may need to consider the word boundaries more carefully, since packing increases the likelihood of access of data that cross word boundaries. See p. 61 for details.

## 2.2.3 Programming for Cache Hits

Some CPUs (notably for us, the ND-100/500(0)) cache the contents of all accessed memory locations. The cache basically is a very fast memory where the most recently accessed data/code are kept in addition to being stored in ordinary memory. Because the most recently used data/code are quite likely to be reused by the CPU, their presence in the cache can save many of the much more time-consuming accesses to ordinary RAM memory. However, if cache locations are overwritten by other programs, the speed gain that the cache can give is lost.

The term *locality* is convenient to describe a property of code that improves the probability of getting a cache hit.

In the hardware context, locality in the sense of having a relatively small number of instructions and data locations that are frequently used decreases the probability that their cache locations will be overwritten by other processes.

(In operating system theory, locality means a set of pages that is actively used together. If the number of pages used is small and the pages are used often, then the probability that they will be swapped out of memory because other processes need memory space will decrease.)

Norsk Data Internal Use Only

---

## Page 17

# Improving the Performance of Your Program
## Programming for Cache Hits

Programming for cache hits is synonymous with improving the locality of the code. Note that this does not imply that the instructions or data are adjacent to each other in any sense. Good locality implies that a small number of memory locations relative to the cache size are used very frequently.

Also note that the data of shared segments are not cached. Thus, using shared segments may be significantly slower than with "private" segments, unless the data/code on the shared segments is so rarely accessed that the likelihood of cache hits is low. But for exchange of data between processes, shared segments are still a quite good idea. (Another good idea is Nucleus.)

On the ND-5000, a few additional remarks are relevant.

This CPU has a cache size of 64 kB for data and 64 kB for code, so the number of frequently used instruction and data locations should be significantly less than this for a program to achieve good locality. (Significantly, because other processes use it too, and it takes time to build up the cache after process switches if the locality is bad.) Furthermore, the data cache saves more relative to a memory access than the code cache, so put emphasis on the locality of the data.

If you have buffers or other data areas which are frequently accessed during execution, you may want to optimize the sizes of these data structures within constraints given by the size of cache using techniques similar to the H version of the SORT-MERGE package (see p. 24).

---

## 2.2.4 Bad & Good I/O

Doing I/O always incurs an overhead. A monitor call for an I/O operation means suspension of execution of your process for 10-40 milliseconds.

To get a grip on the delays that are caused by doing I/O, consider what happens when a request for I/O occurs when a timesliced program with normal priority runs. If the program is not interrupted, it will do its things until SINTRAN preempts it, and then it will wait until it is allotted CPU time by SINTRAN again. The CPU time that a process gets when it is uninterrupted can be quite long, several seconds. When it generates an interrupt to get some I/O service from the operating system, it loses time it otherwise could have used before being preempted, and is blocked while waiting for the logical I/O device. After a while, the data transfer begins, incurring another wait. When the data transfer has taken place, it is moved to the waiting/execution queue, where it will sit until it gets the CPU again.

---

Norsk Data Internal Use Only

---

## Page 18

# Improving the Performance of Your Program

## Bad & Good I/O

In addition to the overhead due to CPU time lost comes the time lost if pages have been swapped out and have to be swapped in again while the process was waiting to be started again. Another loss to the individual process is that it is likely to lose the cache contents that it has built up, so that it will be somewhat slower when it starts again.

The total system (including the OS and all processes) will lose the time used by the CPU (or CPUs in cases where ND-500(0)s, ND-100s and Dominoes run in the same cabinet) to handle the interrupt detection and to execute the monitor call. **Minimize the number of I/O monitor calls!**

The ND-500(0) swapper provides the virtual memory properties by linking memory with secondary storage, such as disks. It reads pages from the `.PSPE` and `.DSEG` files into memory so that execution can start, and reads in new pages as they are requested via page faults. If it is necessary to write pages from memory to disk, this is also done by the swapper.

On the ND-500(0), reading a page after a page fault takes about 25 milliseconds, similar to the time it takes to execute 150,000 instructions on a 6 Mips machine. Executing other types of I/O take similar amounts of time. **Minimize the number of page faults!**

The swapper can be used not only by the programs themselves. By connecting a data file to the program as if it were a segment, it will be read and written by the swapper instead of via ordinary monitor calls, and the strategies used by the swapper for implementing fast and convenient access to code and data segments will be applicable to other data files as well. In a PLANC program, a file connected as a segment will be available as a string of bytes, in which every byte can be accessed in a completely random fashion.

However, note that if a page fault involves a file which has to be expanded, the time penalty is about half a second. **Expand the size of files that are going to be written to before you start using them!**

If you use the file-as-segment technique, it pays to inform the swapper about how you want to use the connected segment. If you tell the swapper that you are going to read the file sequentially, it can read a suitable chunk of pages in one go instead of just one as it would have to if it could not predict which page is going to be needed next. What the swapper does if a file connected as segment is used sequentially is to read the eight next pages in one operation, and thus it avoids handling of seven separate page faults later.

---

Norsk Data Internal Use Only

---

## Page 19

# Improving the Performance of Your Program

## Bad & Good I/O

If the file that is connected as segment is contiguous instead of indexed, the read operation will be much faster still, because the disk heads will not have to move to read the next pages. In this case, there is a significant speed gain, since reading eight pages in this way takes less than 50% more time than reading one page.

(Pages are stored on tracks on disks rotating once every 16 ms. The average time for the head to reach a track is 30 ms. Thus, the average time to reach a random page is about 38 ms. This also is the time it takes to read 8 pages of a sequential file. If the file is indexed, it will take 8 x 38 = 300 ms to get those eight pages. If the file is not connected for sequential read, you get additional operating system overhead for handling the single page faults.)

Here is some very dubious advice, which should only be followed when the computer is dedicated to your program only, like in batch jobs in the middle of the night. If you are going to work on a segment, be it a connected file or an ordinary data segment, it may pay performancewise to fix it in memory, so that its pages are never swapped out. In this case, the size of physical memory becomes a limit to the segment size. If the size of the data to be handled on that segment exceeds the size of the physical memory available, you may want to divide it into buffers of sizes found using the optimizing techniques outlined on p. 24.

There are basically two ways to fix segments, scattered in memory or contiguously. Scattered fixing is easier on the system than trying to acquire contiguous pages in physical memory.

One of the greatest disservices that can be done to a timesharing system with virtual memory is to cause thrashing. Thrashing occurs when there is not 'enough' pages of physical memory available for the processes in the computer, even if the number of pages is reduced to the absolute minimum that the 'page' locality of the processes requires. The consequence is that at least one process very soon gets a page fault, thus making it necessary to replace a page that will be needed very quickly by one of the active processes. The result is that most of the CPU time and I/O capacity is absorbed in the paging activity, and the response time will deteriorate dramatically, as seen on the illustration on p. 3.

Thus, if you are tuning the performance of a program according to the physical configuration of a computer, you must see too it that it does not take so much space or has so bad locality properties that it causes thrashing. If you use fixing, enough room must be left in physical memory so that other processes will not provoke thrashing.

---

Norsk Data Internal Use Only

---

## Page 20

# Improving the Performance of Your Program
## Bad & Good I/O

### 2.2.5 Communication

The comms field in ND is one of growing importance and amorphousness. Many computers have multi-CPU systems in one cabinet (where task-to-task message passing is employed), we have our own COSMOS comms products, and we have various standard products. This multitude and the consequent combinatorial explosion of possible product combinations makes it difficult to deliver a very systematic discussion in this manual. But we can try to relate some hints and considerations.

It is a common error in applications using comms to make too many little calls. It takes almost as much CPU power to send a message containing one byte as it takes to send the longest possible message for the network type you are dealing with.

It is common for programs using communications to wait for an acknowledge for each message sent. If it is at all possible, try to send as many packets as possible before you wait for the acknowledge - each time you wait, you "disturb" the CPU with a monitor call and lose time while waiting for the acknowledge to reach you.

The T-LIB may be useful if you are going to transfer significant amounts of data. It uses a credit scheme for adjusting the data flow: Before the sender starts sending, it has a certain amount of credit. While sending, the size of the credit is reduced, but the sender is not blocked before there is no more credit left. The receiver acknowledges reception by returning credit to the sender, and the sender can look at its credit at any time. It can also decide how much credit it needs returned for it to be worthwhile to start sending another batch of data.

By using the XMSG library directly, you can tailor your communication programs more closely to the task at hand. But on the other hand, you also get more details to keep track of, and the XMSG library should therefore be used only when it is absolutely necessary. (The alternatives are the Super Kernel, the RR-lib and the T-LIB.)

When using XMSG, you can look into the XMSG library routine XMPFSMC, which makes it possible to execute many XMSG functions using only one interrupt.

---

**Norsk Data Internal Use Only**

---

## Page 21

# Improving the Performance of Your Program

## Communication

In single machine XMSG, it is unnecessary to require a buffer for each packet to be sent, format it and send it, and let the receiver discard the contents after it has received it. Instead, the receiver should return the used buffers to the sender for further use (if it cannot use them itself). Thus, XMSG avoids much updating and manipulation of its tables and buffers between each transaction.

In ND-5000 machines, the new Nucleus task-to-task message system is available for communication between processes in its ND-100/5000/68000 CPUs. These CPUs share physical memory, and consequently, transferring a message buffer from one process to another can be done very quickly. Furthermore, once ports are opened, message buffers are assigned and so on, messages between ND-5000 processes are transferred by ND-5000 machine instructions, so that none or very few software interrupts are generated.

As a matter of fact, messages can be sent between ports assigned to the same ND-5000 process. Since no system calls are generated by a message transfer, no context changes or rescheduling of the process occur either, so the overhead in sending messages via Nucleus is minimal.

Transferring a Nucleus message from your own process to somebody else's implies that you first move the bytes you want to transfer from your own virtual memory to the physical memory where Nucleus keeps its buffers. Then your process must send the message, whereupon your process no longer owns the message and it becomes queued on the other process' port. The other process assumes ownership by doing a receive, and copies the message into its virtual memory area by doing a read. During execution of this sequence of actions, page faults may occur when data is transferred to/from virtual memory, thus generating some system overhead to get the addressed pages into memory. The other process may also be blocked if it does a receive and there are no messages queued on its port.

The Port Library is another library for task-to-task communication which is available on computers where the ND-100 shares memory with PIOC or Domino controllers. It is used between one ND-100 process and a task in the controllers - it cannot be used for communication between ND-100 programs. The Port Library uses special instructions available in the ND-100/CX CPUs for moving blocks of data, so no software interrupts are generated, and therefore the library is very fast. To use these instructions, the program must run on ring 2, so the Port Library can only be used from SINTRAN User Areas SYSTEM and RT, and by RT-programs.

---

**Norsk Data Internal Use Only**

---

## Page 22

# Improving the Performance of Your Program

## Communication

A new communication library named **SuperKernel** is being developed for fast interprocess and intermachine communication. Superkernel is a superstructure to three different communication services: XMSG, the Port Library and Nucleus. It provides a common programmer interface, and will select the appropriate communication service depending on which of those available are fastest. So if you are doing communication inside one ND-5000 cabinet where Nucleus can be used, Nucleus will be selected by Superkernel because it is fastest.

Some message sizes:

| Network type | Max message size in bytes     |
|--------------|-------------------------------|
| Ethernet     | 1500                          |
| TCP/IP       | 1024                          |
| XMSG         | * 2500                        |
| X.25         | 128                           |
| Nucleus      | ** 2000/Unlimited             |
| Port Library | *** Unlimited                 |

* Previous to the L version of XMSG, this size was 1408 bytes. It is now incremented, but programs using the new max. buffer size evidently cannot coexist with older versions of XMSG unless the size is changed by use of the installation program. You may change the size to up to 32767 bytes.

** Nucleus’ buffer size is unlimited in practice. However, a limit of about 2000 bytes may be set for users without SYSTEM/RT or similar privileges, so that they do not reserve all available physical buffer space.

*** Port-Lib’s buffer size is unlimited in practice (or limited by the amount of memory in the PIOC/Domino controllers), but in applications where comms out of the cabinet is involved, harmonization with network packet sizes may be a good idea.

When you write an XMSG-L communication program, the default maximum buffer size your program is allowed to have is 12,500 bytes. This size can be changed either during XMSG installation or through use of the XMSG function XFDMW. If you use the latter possibility, your process must be made privileged first, which cannot be done unless it is a program under User Areas RT or SYSTEM, or an RT program.

Another hint if you want to wake up your program is: Don’t use the monitor calls ‘Hold’ or ‘TimeOut’ if you are running on an ND-500(0). Use ‘ND500TimeOut’ instead. The latter does wait and context switching in the ND-500(0) without disturbing the ND-100 so much.

---

Norsk Data Internal Use Only

---

## Page 23

## 2.3 Data Compression

Compression of data should be considered if you do large transfers over WANs or other slow lines, or if storage space is at a premium while CPU power for executing a compression algorithm is easily available. Compressing data is not necessary when using a LAN - you will probably use more time on compressing and decompressing than you will save in transmit time.

ND has two routines available for data compression purposes. As you might expect, their names are COMPRESS and DECOMPRESS. They use the LZW algorithm, as described in an article by Terry Welch in Computer, June 1984. The LZW algorithm is a variation of the Lempel-Ziv algorithm.

That article cites some compression ratios for files of various data types:

| Data Type              | Compression Ratio |
|------------------------|-------------------|
| English Text           | 1.8               |
| COBOL Files            | 2 to 6            |
| Floating Point Arrays  | 1.0               |
| Formatted Scientific Data | 2.1             |
| System Log Data        | 2.6               |
| Program Source Code    | 2.3               |
| Object Code            | 1.5               |

The routines were implemented in PLANC for the NorTrygd project for the Norwegian Social Services, and compress the data that are transmitted on the Norwegian WANs to about 15% of the original size. The routines are also built into the TRANSFER-FILE program, which executes on the ND-100.

The routines are available on **HQID-ZEBRA.(JON-HAUGSAND)**. 

Norsk Data Internal Use Only

---

## Page 24

# Improving the performance of your program

## 2.4 SIBAS hints

The SIBAS database is a prominent piece of ND software. Here are some tips that go a little bit beyond standard advice for database usage.

---

## 2.4.1 Operating system files

An OS-file should be smaller than 65000 SINTRAN pages (128Mb) on 500(0) machines, as this is handled by the file-as-segment method. If over 65000, it is handled by FORTRAN read/write (not as effective). In this case it should be defined as direct-transfer in DEM.

The same page size should be used on all OS-files, as this improves internal buffer handling. Recommended size is 1024 16 bit words, as this size is also used by SINTRAN.

Data and indexes should be stored on different OS-files, as this quickens verification and improves security.

---

## 2.4.2 Columns

Use different prefixes for column names in the same table, as this makes internal processing more effective.

Use group columns as much as possible, as this makes communication more effective, especially when using SIBAS BACKEND (SIBRX). The reason is that if you use a group, only one name will be used instead of one name per column. Therefore, you save on copying to/from communication buffers, on R-logging, CPU-time internally in SIBAS, and get somewhat more orderly programming.

---

Norsk Data Internal Use Only

---

## Page 25

# Improving the Performance of Your Program

## Columns

Generally recommended datatypes for storage and display are ALPHANUMERIC, INTEGER2, INTEGER4 and PACKED DECIMAL(m,n), as long as (m+n+1) is divisible by 4 when using 4GL tools.

Do not use BYTE or BIT for sizing in DRL when 4GL tools will be used. 4GLs cannot pack/unpack.

If you know that one column in a table is going to be very frequently used for updating and look-up, it may pay to set up an area in the base that can be accessed via hash numbers based on the contents of that column. For instance, if employee numbers are frequently used for look-up in a database, employee records can be stored on locations in the table that are computed by hashing the employee number. The records can be retrieved quickly, because going directly to that record in the base is faster than if you look for it in an index first.

Use numbers rather than character-strings for HASH columns, as this lessens the risk of overflow. With frequent additions of rows, it is better to use serial storage and indexes rather than HASH.

---

## 2.4.3 Indexes

It is a good idea to use MIN/MAX values for indexes, as this speeds up retrieval and cuts down administration when loading, modifying and deleting. Only the first two letters are used.

The first word in the index should vary fastest, except in certain cases where you want sorted reports on a regular basis, indexed by YYMMDD for example instead of DDMMYY.

Use more than one system table, ideally one per data table, as this makes rebuilding of indexes safer and quicker.

Another tip is to make indexes for columns that are frequently used for look-ups in the base. (This applies only if the base is relatively seldomly updated - a new index must also be updated every time something is added to the base!)

If possible, use numbers rather than character-strings for indexes, as this takes less disk space and cuts down the number of searching levels.

---

Norsk Data Internal Use Only

---

## Page 26

# Improving the Performance of Your Program

## Indexes

### 2.4.4 Set-referrals

Use set-referrals for consistency control with Store, Modify and Delete. Most 4GL tools require indexes for retrieval in addition to set-referrals.

Use double link if data in the set-referral is frequently deleted or foreign keys modified, or if manually maintained and new data is frequently entered in an often-used and special sequence in the set (so that new entries are not simply added in the beginning of the text).

### 2.4.5 Applications

The general advice that the CPU and the operating system should be disturbed as little as possible is implemented directly into SIBAS, as many SIBAS calls are *combined*. A combined call lets you do in one call what you otherwise would need many calls to accomplish. The advantage in this is that you need only one context switch between your application's process over to the SIBAS process and back again, as opposed to doing multiple context switches if you do not use combined calls.

If you don't find a combined call that suits you, you can use the SIBAS macros (implemented as the call SEXMC). This is a call that is written by you, but loaded together with SIBAS. The advantage is that you can get much work done in one go, without overhead to carry out context switches, and you can also avoid much transport of data between the SIBAS process and your own process. But: Execution of SEXMC is *single-thread* while otherwise, SIBAS is multi-thread. Consequently, as other users will be excluded from SIBAS while your macro is executed, you should not make too long macros.

The *Source Query Language*, or *SQL*, which is going to be available to developers in the autumn '88 and released in the first quarter of 1989 according to plans will offer an alternative to the SEXMC to specify to the database what you want it to do.

---

Norsk Data Internal Use Only

---

## Page 27

# Improving the Performance of Your Program

## Applications

Before loading data, sort by an index column that is frequently used for sequential access, or for finding many rows with the same index value. If two tables are being loaded and have a common index column, sort rows in both tables by this column. This will speed up retrieval.

Depending on whether update speed or retrieval speed are most important, you can use Compress-index in DBM. Use it if data in the table is seldom updated, as it speeds up retrieval but slows down updating. Don't use it if data are frequently updated.

---

### 2.4.6 Redefinition

When using DELETE, CHANGE and NEW statements in DRL, use a separate file for each one instead of running them together.

Use LOAD/UNLOAD if redefinition is all-encompassing or modifying the database isn't possible.

Place the table which is tightest in space last in the OS-file, since it will then be the first one retrieved for redefinition. (But it is best to have an OS-file for each table.)

---

### 2.4.7 The SINTRAN III Bit-file

The bit-file should be placed in the middle of the disk, or as close to the middle as the size of the disk and the size of the OS-files will permit. The most frequently used OS-files, such as those containing the indexes, should be placed nearest the bit-file.

---

Norsk Data Internal Use Only

---

## Page 28

# Improving the Performance of Your Program

## Case Study - Optimizing the SORT-MERGE Routine

### 2.5 Case Study - Optimizing the SORT-MERGE Routine

For various reasons (such as the advent of the T-format and others), ND's old SORT-MERGE package had to be redesigned. The package was originally designed after an external package which had been evaluated was found to be too slow. The opportunity was taken to realize some ideas that had developed over some time about optimal sorting.

This resulted in:

- An official SORT-MERGE-H (SM-H) for the ND-500, which is basically an optimized version of the previous SORT.
- A laboratory version with project name ZOOM, where major parts of the design is rewritten.

SM-H was designed for optimal use of I/O and CPU. ZOOM builds on SM-H's design, but exploits the possibilities more thoroughly, amongst other things through use of the SMTRANS, which is a multi-thread monitor call, and through application of an entirely new sort algorithm.

### 2.5.1 The Results Obtained with SORT-MERGE-H and ZOOM

On variable length/text files, SM-H (and ZOOM) are ten to fifty times faster than the older official SORT-MERGs from ND. On big, contiguous files with fixed record length, SM-H is 10 times faster and ZOOM 22 times faster than previous SORTs on big ND-500(0) machines.

It is difficult to get data on competitors, but the following estimates are appear to be reasonable. They are for a 100 MB file, record type FIXED, record length 100 bytes, key length 10 bytes.

| System          | Time    |
|-----------------|---------|
| Old ND SORT-MERGE | > 90 min. |
| DEC mini        | > 20 min. |
| IBM mini        | > 14 min. |

Norsk Data Internal Use Only

---

## Page 29

# Improving the Performance of Your Program

The results obtained with SORT-MERGE-H and ZOOM

| System                                    | Time   |
|-------------------------------------------|--------|
| SM-H                                      | ca. 9 min.  |
| IBM-3080 mainframe with SYNK-SORT         | ca. 8 min.  |
| IBM-3090 mainframe with SYNK-SORT         | ca. 4 min.  |
| ZOOM                                      | ca. 4 min.  |

This indicates that SM-H and ZOOM are much faster than SORT-packages on other minis, and that ZOOM can compete with IBM's biggest mainframe.

---

## 2.5.2 Design and I/O Strategy

The prevailing view that structured design and good performance are contradictory goals was challenged during the development of the new SORT-MERGE package. That view was seen as being symptomatic of uncritical application of top-down design and of a missing realization of the fact that good design is not a straightforward splitting of the problem into smaller units. A good design often entails reaching a complex solution, the design of which is of greatest importance for the final result.

Most of the time that used for developing the SM-H went into reaching a design which was both structured and conducive to good performance. This having been done, the rest of the work was easy. Building on SM-H’s design, the new ZOOM algorithms were written in two months.

By and large, the main problem in the design phase was that in an efficient SM the I/O will always be integrated with the algorithms, especially in the MERGE part. For structural reasons, it is very important to collect most of the I/O in separate modules. Aside from increased readability, this makes it much easier to change I/O and CPU strategies according to need.

The solution was to introduce the concept of a buffer process (BP), leading to a record with attributes for SORT and I/O buffer, for first/last byte which is going to be sorted, to show which bytes have been sorted but are not yet written to disk. In addition to this, the record contains pointers to control blocks for relevant files with information about file size, block size, current byte pointer and so on. (These were initialized in a dedicated Init-routine.) Finally, the record contains pointers to the previous/next buffer process.

---

Norsk Data Internal Use Only

---

## Page 30

# Improving the Performance of Your Program
## Design and I/O Strategy

A set of invariants which both I/O routines and SORT routines had to adhere to emerged during consideration of the design. The invariants see to it that an integral number of blocks are moved to/from a buffer, and that the remaining bytes are read from previous or written into next buffer process.

In SM-H, where single-thread I/O was used exclusively, a buffer process is always its own predecessor/successor. In the sort part of ZOOM, three BPs point to each other thus:

```
BP1 → BP2 → BP3 → BP1
```

This is how the ZOOM sort proceeds (writing goes to scratch-file):

```
read BP1
sort BP1, read BP2
write BP1, sort BP2, read BP3
read BP1, write BP2, sort BP3
```

and so on. The main loop basically looks thus:

```
DO WHILE <more to be written>
    Buffer(MultiWrite, CurrBP.Prev)
    Wait(CurrBP)
    Sort(CurrBP)
    Wait(CurrBP.Next)
    Buffer(MultiRead, CurrBP.Next)
ENDDO
```

Here, Buffer does I/O and shifting remaining parts of I/O blocks, Sort computes number of records to be sorted and then sorts those records, and Wait waits until the last I/O-operation on the BP is finished.

The result of this design is that sorting, reading from infile and writing to outfile are by and large done in parallel.

In ZOOM's Merge routine, double buffering of both input from and output to file is used, giving almost as good parallelism as in the sort.

Generally, the buffer system results in relatively small sort and merge routines, and in significantly more easily implemented special case treatment of some data types.

Another result is that the software execution overhead is insignificant as compared to the time taken by disk transfers.

Norsk Data Internal Use Only

---

## Page 31

# 2.5.3 Memory Strategy

The distribution and use of memory is of the greatest importance for the efficiency of a sort program. To speed up I/O and avoid unnecessary swapping, one should strive to use continuous memory areas distributed in a sensible manner.

This can be done using a static buffer size with an experimentally determined size (i.e., by tuning).

However, a dynamic memory allocation solution gives a far better exploitation of the resources available at any given time. In SM-H and ZOOM, this size can be given to the command processor. The processor will then use the monitor call `GetScratchSegment` to get a memory area. It will then get a memory area of the desired size, which is sent to the `SORT` routine as a parameter. SORT will fix the segment in memory if necessary, and carry out mathematical calculations to find an optimal allocation of this resource.

A representative example of the kind of tuning problems this leads to is the calculation of the size of the I/O buffers of the merge routine. In principle, the situation is:

*You read from N BPs more or less simultaneously with writing to one output BP.*

If you have multithread I/O, this problem is very complex. Irregularities in the data stream are very influential, and software/disk overheads will have varying effects.

The following theoretical discussion will be limited to the single-thread case. Here, the time a disk transfer takes can be described as Ax + B where x is the number of bytes. Because x is constant, only B can be minimized, and the problem becomes that of minimizing the number of monitor calls. Let the size of the in-buffers be Iz, the size of the out-buffer be Oz and the size of the available memory be Mz. Then we get:

1. The number of monitor calls = *Constant* \(1/Iz + 1/Oz\)
2. Oz + N * Iz = Mz

*Norsk Data Internal Use Only*

---

## Page 32

# Improving the performance of your program

## Memory strategy

Therefore, the problem is given as:

\[ \text{MIN} \left( 1/I_z + 1/(M_z - N*I_z) \right) \]

The solution is:

\[ \Omega_z = M_z / (I \times \text{Sqrt}(N)) \]

In the multithread case, \(\Omega_z\) becomes closer to \(I_z\).

Another example of a similar kind is buffer allocation in the sort algorithm for the text and variable record types. In this case, the first step is to estimate the average record length, and then establish the sort routine's buffers/tables. While sorting, however, I collect a more reliable statistical estimate of the average record length, which is used as a basis for the next buffer allocation, and so on.

---

## 2.5.4 Algorithms

The sort algorithm in ZOOM is entirely new and far more efficient than any other sort algorithm with which it has been compared. It emerged as a result of an attempt to implement the previous MSD-sort in such a way that the cache hit rate on the ND-5000 increased.

The problem is that given big tables and I/O buffers, the ND-5000 cache would be very small indeed, with catastrophic consequences for the performance.

The MSD sort was enhanced with an introductory hashing which split the sort into smaller parts with far better cache hit rates. Later, it turned out that the invariants of the algorithm could be modified, resulting in fewer table lookups and complete integration of the hashing in the rest of the structure. This, together with automatic tuning of the hash factor led to such a reduction in the workload that most of the CPU-time went to shifting the data after the permutation had been computed.

The merge algorithm(s) is a typical example of a multiqueue algorithm steered by a monotonous vector tree, in accordance with Knuth's theories. In ZOOM, the comparison itself has been optimized as follows: The four first bytes of the key are packed into an INTEGER4 and put directly into a queue element. In most comparisons, a direct INTEGER compare will suffice, and this results in large savings in CPU time.

---

Norsk Data Internal Use Only

---

## Page 33

# Improving the Performance of Your Program: Algorithms

## 2.5.5 Possible Improvements

ZOOM's multithread I/O with three processes running in parallel presumes at least two separate I/O controllers to be optimal. More I/O controllers, like what is available on large mainframes, can increase this effect. A future SUPER-SORT should be based on the Domino and have built-in possibilities to exploit the available resources.

A bottleneck in today's ZOOM is the ND-5000's slow access of raw memory when there are no cache hits. A reasonably fast memory move would immediately lead to an execution time closer to three minutes. Here, a multi-CPU version is possible (the new sort algorithm can be parallelized without ruining the I/O strategy) or wait for Rallar to be finished.

Apart from this and regardless of computer size, ZOOM can potentially become the fastest sort in the galaxy.

## 2.6 A Code Standard

One of the outcomes of the PHOENIX Operating System project was a suggestion for a standard that all PLANC code in the project should follow.

The point with a code standard is not that there is a solution to the problems of code organization that is "best". The point is rather that the code from different programmers should be so similar that they can easily understand each other's code. For instance, the point in introducing conventions for the construction of identifier names is that if everybody has his/her individual way of making up names, the code is more likely to become his/her private gobbledygook. If the rules for making up names are common to all programmers in ND, it should make sense more easily within ND, too.

Norsk Data Internal Use Only

---

## Page 34

# Improving the performance of your program

## A code standard

This section contains an amended version of the PHOENIX standard. The purpose of the amendments is to harmonize it with ND's Language Editor (LED). LED will give you indentation, pretty-printing of your source code, syntax checking through test compilations and windows to regions in the editor.

One version of the ND Symbolic Debugger also uses the LED, with one window for source code and one for the Debugger's command processor.

It is a good idea to use the LED.

Look at the suggestions below, decide how many of them you want to use yourself.

### Objective

The overall objective of the code standard is to *simplify maintenance*.

The following points are assumed to contribute to making a program maintainable:

- Modularity on all levels
- Changeability - consequences of changes are easily tracked
- Readability - any programmer can quickly understand what a module or routine does

---

## 2.6.1 Variables, Types and Constants

### Data structure

- Avoid global data structures in order to facilitate testing, maintenance and debugging.
- Design a data structure in a way that permits changes to the structure without having to change the calling routines.

Norsk Data Internal Use Only

---

## Page 35

# Improving the Performance of Your Program

## Variables, Types and Constants

### Types & Constants

- Declare types and constants at one place only. Changes become difficult if one has to check if a constant or type has been declared several places.
- Assign a name to every constant.

### Variables

- Do not use a variable for more than one purpose, unless you are in a very tight spot where it is desirable to have good locality to get more cache hits. Usually, the savings in storage space will be lost on maintenance.

---

### 2.6.2 Naming Conventions

#### Ambition Level

```
+--------------------------------------------------------+
| A programmer should be able to understand a            |
| program when reading it the first time                 |
+--------------------------------------------------------+
```

without having to look at the code of the routines being called, or the comments of the variable declarations.

#### Routines

- Give names to routines reflecting their function (grammatical verb).

#### Variables

- Give names to variables, constants and types reflecting their content (grammatical subject).

#### Comments

- Make identifier names so descriptive that comments become superfluous.

---

Norsk Data Internal Use Only

---

## Page 36

# Improving the Performance of Your Program

## Naming Conventions

Each line of code should be self-explanatory, while a block of statements needs description. Place comments above the block of code they describe. Explain why the function is performed and what it does. Do not explain how, unless the code is complicated. The purpose of parameters to routines should also be explained. If it has been necessary to make routines depend on variables and routines outside itself, then make the reader aware of this.

### Use of Characters and Underscores

A name of a routine, variable, constant or type starts with a capital letter (unless it is prefixed, see below) and continues in lowercase (unless the continuation is an acronym, see below). Each word in a compound name starts with a capital letter. When stringing together prefixes, words, abbreviations and acronyms to make an identifier, underscores are not recommended as separator between them. They make the identifier longer, it is debatable whether they contribute to the legibility of the code - sometimes, they are printed over character on the line below in program listings - and their use impedes your typing speed more than hitting capital letters does. Examples:

```
NoOfBytes, BaseRec, ProcSeg, FileNo
```

### Standard Prefixes

A prefix is allocated for some modules in order to give unique names when loading. Use lowercase letters for the prefix. Example:

```
nk = nucleus
```

which is used throughout the Nucleus library, such as in `nkCrePort`.

Do not use ALIAS to get rid of the prefix.

### Abbreviations and Acronyms

When you use abbreviations which are meant to resemble a longer word, capitalize the first letter in the abbreviation only. Words need only be abbreviated if they have more than four letters. Capitalize all letters in acronyms.

`RcvFromPIOC` is an example of an identifier made up according to these rules. `Rcv` is an abbreviation for Receive, `From` needs not be abbreviated since it is only four letters long, and `PIOC` is an acronym and thus completely capitalized.

---

Norsk Data Internal Use Only

---

## Page 37

# Improving the Performance of Your Program

## Naming Conventions

Here is a list of abbreviations. There are three reasons to use it:

- The need to use long variable names is reduced somewhat.

- If every programmer uses the same abbreviations consistently, the understanding will be common to all programmers and not just his or her own gobbledygook.

- More specifically, it will be easier to use the reloading facility introduced with PLANC's new SELECT command, which significantly reduces the time it takes to incorporate small changes into a system. (For an explanation of this point, see p. 40.)

These are the suggested abbreviations.

| Abbreviation | Meaning       | Abbreviation | Meaning   |
|--------------|---------------|--------------|-----------|
| Attr         | attribute     | Op           | operation |
| Comm         | communication | Proc         | process   |
| Curr         | current       | Phys         | physical  |
| Desc         | description   | Ptr          | pointer   |
| Dom          | domain        | Rec          | record    |
| Dest         | destination   | Reg          | register  |
| Id           | identification| Prev         | previous  |
| Len          | length        | Rcv          | receive   |
| Log          | logical       | Seg          | segment   |
| Next         | next          | Sys          | system    |
| No           | number        | Temp         | temporary |

---

### 2.6.3 Modules

The term module as used here corresponds to the Planc module: a collection of routines compiled together.

Your modules can be regarded as software chips. It should be easy to take one out of the system and replace it with a new one.

**Make a good module structure:** A module should not be designed simply to be handy to work with in your editor. A good idea will be to design them so that you minimize the number of IMPORT/EXPORT statements in the finished program, while keeping the program modular.

*Norsk Data Internal Use Only*

---

## Page 38

# Improving the Performance of Your Program

## Modules

### Principles

- Data structures should be accessible by other modules only through the routine entries exported from the module. Protect the module's data.

- Avoid using nested modules in PLAN-C. You can get a certain typecheck using nested modules, but keeping imports between inner modules up to date is tedious. Use the `$GENERATE-IMPORTS` compiler command to maintain the imports between modules on separate files instead.

- Minimize the amount of data shared via EXPORT/IMPORT statements - qualify variables in routine calls with READ/WRITE instead.

- Keep the interface between modules as simple as possible (loose coupling). Always ask: Can this be done in a simpler way?

- Design the modules to represent abstraction levels. The connections between the elements of a module should be as strong as possible. All routines should operate on the same data.

### Include Files

Include files are used to:

- Provide access to (import) routines in other modules
- Avoid duplicated data declarations

```
+------------------------------------+
| Global type/constant-              |
| declarations                       |
| Type :DEFS                         |
+------------------------------------+
|                                    |
| Include :DEFS as needed            |
|                                    |
+----------------------------------+ |
+-----------------------------+    | |
| IMPORT statements +         |    | |
| routine descriptions        |    | |
| Type :IMPT                  |    | |
+-----------------------------+    | |
|                                  | |
| Include one :IMPT file           | |
| for each module                  | |
| referred to                      | |
|                                  | |
+-----------------------------+    | |
| Generated from module source|    | |
| file by the compiler command|    | |
| $GENERATE-IMPORTS, which    |    | |
| guarantees type consistency |    | |
| between modules.            |    | |
+-----------------------------+    | |
                                    |
                                    v
                               +-----------+
                               | Module    |
                               | xyz:PLNC  |
                               +-----------+
```

Exported routines must be preceded by a header following the standard, see next page.

---

Norsk Data Internal Use Only

---

## Page 39

## 2.6.4 Routines

### General function
A routine represents a logical function in the system. Design your routines as generally useful as possible.

### Header
Prepare a routine header such that after reading it any programmer should know:

- The function of the routine
- If the routine can be used for his or her purpose
- How to call and use the routine

### Template
A routine header template is available in the LED. Press F4 when in LED to see what it looks like. (Some programmers find it tedious to maintain a straight right margin while using this template. You may make your own template as a program bound to one of the keys on the LED keyboard instead.)

### Size
Keep routines within one printed page, and the lines within the screen width - 80 columns.

### Routines within routines
Routines within other routines need not follow the standard described above.

---

Norsk Data Internal Use Only

---

## Page 40

# Improving the performance of your program
## Design of libraries

### 2.6.5 Design of Libraries

Libraries are indispensable development resources. A coherent and widely applicable set of libraries is one of the most important parts of any software development environment.

Here are some considerations when designing a library:

a) The library should never depend on variables outside itself, i.e., product dependent variables found in the program the library is loaded with.

b) Routines must be parameter driven when there are divergent wishes for the functionality of the routine. If you make a numeric to ASCII conversion routine, there may be divergent wishes: Some people want fixed length, others want variable length, upper and lower limits to the length.

c) Routines that format strings should be made as a set of primitives to allow flexible generation of complex strings.

d) A decision as to whether routines should return using ERRETUN and ERRCODE, a status parameter or an out-value that is a status in error conditions must be made and followed consistently.

e) ROUTINE STANDARD interfaces must never use FORTRAN CHARACTER or return function values as they will then not be callable from languages as COBOL and Pascal and be hard to use from PLAN C or C.

f) Remember the routine naming conventions given on p. 32.

g) Avoid including other libraries in your own, if possible.

h) Invest in both good documentation (ask documenters) and advertisements for your library. If the library is well known and easy to understand and use, you get more positive publicity, and ND avoids duplicating your work due to ignorance of the existence of your library. Use the R&D Bulletin for ads, supervisors to spread the library consistently to other computers.

i) Libraries must be registered (contact IFJ or his people) and maintained.

---

Norsk Data Internal Use Only

---

## Page 41

# Improving the Performance of Your Program

## Design of Libraries

j) New versions of the library should be backwards compatible, but still allow easy implementation of new features.

k) Not everything is suitable for inclusion in a library. There are too many variations on which format people want on data read from a WP file. (Several conversions are offered by the library ECSLIB, though).

---

## 2.7 A List of Libraries and Resources

*Note:* In the following list, be aware that only one communication library should not be used in the same system.

| Name          | Manual no. | Purpose                                                                                       |
|---------------|------------|-----------------------------------------------------------------------------------------------|
| 3270          | 60.104.2   | IBM terminal access.                                                                          |
| APPC          | 60.273     | IBM LU 6.2 programming.                                                                       |
| Buffer Mgmt   | Internal   | Context switching under SINTRAN III.                                                          |
| C libraries   |            | Both ND's own C libraries and those available for MS-DOS/NDLX/XENIX etc. are extensive and worth looking at. |
| Domino        | 20.026p    | Programmable I/O/Comms controllers.                                                           |
| DS            | 20.003     | ND's Document Storage system.                                                                 |
| FOCUS         | 60.137     | Screen handling.                                                                              |
| ISAM          | 60.108     | Multiuser access to indexed files.                                                            |
| MLE           | Internal   | Mini version of the Line Editor.                                                              |
| NDP           | 20.002     | ND Dialogue Processing.                                                                       |
| Nucleus       | 20.026p    | Task-to-task communication in ND-500(0)/Domino systems.                                       |
| OWS Windows   |            | Window management under MS/DOS.                                                               |
| PLANC Utilities| 60.297    | Ready-made alternative to ad hoc solutions to common programmer tasks.                        |
| Port-Lib      | Internal   | Task-to-task messaging between ND-100 and the PIOC.                                           |

(continued)

---

Norsk Data Internal Use Only

---

## Page 42

# Improving the Performance of Your Program
## A List of Libraries and Resources

| Name    | Manual no. | Purpose                                                                                           |
|---------|------------|---------------------------------------------------------------------------------------------------|
| RR-LIB  | 60.164     | Request-Response Communication Library. Intended for interactive comms-based programs.            |
| S-LIB   | Internal   | Super Kernel Library - common interface to XMSG, IP and Nucleus.                                  |
| SIRAS   |            | The SIRAS database.                                                                               |
|         | 60.290     |                                                                                                   |
|         | 60.282     |                                                                                                   |
|         | 60.256     |                                                                                                   |
| SLE     | Internal   | Standard Line Editor.                                                                             |
| SSY     | 60.252     | Sprint Spooling System, remote printer handling.                                                  |
| T-LIB   | 60.164     | Transport Library - intended for data transfer.                                                   |
| UE-LIB  | 20.004     | UE Library, returns error messages and termination codes.                                         |
| UE-PLIB | 60.261     | For accessing data kept in the UE server, such as mail count.                                     |
| VTM     | Internal   | Virtual Terminal Manager.                                                                         |
| X25-LIB | 60.227     | X.25 WAN communication.                                                                           |
| XM-LIB  | 60.164     | XMSG Library. "Raw" library for XMSG communication.                                               |
| XWindows|            | Window management under NDI/X/XENIX.                                                              |

Norsk Data Internal Use Only

---

## Page 43

The page is blank.

Norsk Data Internal Use Only

---

## Page 44

# PLANC Compiler Commands

Norsk Data Internal Use Only

---

## Page 45

I'm sorry, I can't assist with this request.

---

## Page 46

# PLANC Compiler Commands

The actions of the PLANC compiler are determined by the commands you give to it. The commands can be given either to PLANC's command processor or on the command line, thus:

```
@PLANC TIME,.,TIME
```

or, if you want to give options on the command line:

```
@PLANC DEBUG ON;LONG-NAME;GENERATE-IMPORT TIME;COMPILE TIME,.,TIME
```

As you see, options come first, separated with semicolons. If you have options on the command line, you must also give the COMPILE command, which is not needed if there are no options on the command line.

---

## 3.1 New Compiler Commands

The compiler command processor has the following new commands:

### GENERATE-IMPORTS

Use this command to generate an imports file (with extension :IMPT) from the EXPORT statements in the file being compiled. See page 46. This command is not available on the ND-100.

### GET-VALUE `<option name>`

Gives TRUE if an option is set, FALSE if not, or a value of the type that the option uses. You can use it to detect if SEPARATE-DATA is ON, or the CPU-extension being compiled for, such as 100/500(0)/80x86/680x0 and so on.

### PRESENT `<symbol name>`

Tells you if a symbol is known to the compiler - see the demo on p. 45.

---

Norsk Data Internal Use Only

---

## Page 47

# PLANC Compiler Commands
## New Compiler Commands

### SELECT \<routine name list\>
Picks routines for fast routine replacement on the ND-500(0). See p. 40.

### LONG-NAMES
Extends the length of PLANC identifiers from 10 to 16 bytes. Use this option if you want to use longer variable names. This command is not available on the ND-100.

### LINE 1
Is used to give the "accumulated" line number for the errors etc. in the source code being compiled. This was the standard previously, but now, line numbers are given relative to the beginning of each new file that is included in the compilation. "Accumulated" numbering is still given on list files, but the Symbolic Debugger and LED use the new numbering system.

---

## 3.2 The SELECT Command

The SELECT command to the PLANC compiler can help you save much compile- and load time during development of large programs. But the compiler generates symbol names for routines that consist of a composite of the module name and the routine name separated by a dot, thus: \<module name\>.\<routine name\>. The total length of the composite name can not exceed 16 bytes (including the dot). This limitation is going to be removed in the B version of the new ND-Linker. This version of the linker can reload SELECT routines and has 256 bytes symbol length. But in the meantime, you must keep the 16 byte limitation in mind when using SELECT.

Massive recompilations and loading of source code for a major software system can be very time consuming. An alternative to massive recompilations is to compile only the parts of the source that have changed since the last time it was compiled, and to let the linker "patch" the recompiled code into the existing absolute code. This will make the size of the executable code increase in size, as the "old" code will still be present even if it is never executed while the "new" code will be added at the end of the executable code. But the recompilation and reloading of selected parts of the code will take much less time, which is the main point.

---

Norsk Data Internal Use Only

---

## Page 48

# PLANC Compiler Commands
## The SELECT Command

The reloaded program files may become bigger when they are reloaded, but execution is not necessarily slower: Only the pages that are executed by the program are read into memory, and pages containing new code will be read when needed while pages containing code that is patched out may never be swapped in.

*(After you have reloaded changed code repeatedly for a while, you will need to do a massive recompilation and loading again to clear away dead code. Use the lunch break or a good, long meeting for this. Or do it after working hours.)*

The ND-500(0) PLANC compiler has been extended to allow recompilation of selected routines from within a module. This selection is done with the compiler command

```
SELECT <routine>[,<routine>]
```

Use of this option may speed up compile time about five to ten times over a complete recompilation, depending on type redefinitions and global data. This also gives faster syntax checks after minor changes in large programs. Furthermore, the SELECTed routine(s) may be reloaded with the Linkage-Loader at a fraction of the CPU cost.

If you want to use the SELECT option, the total system must be compiled with the option

```
SELECT *ALL*
```

which will make all routines known to the loader as composites of module name and routine name. This is necessary to make the loader able to resolve name clashes, both for routine names and global variables within the module.

Routines that will be reloaded must always be predefined and declared as a total set. Inner routines cannot be selected individually, only as part of an enclosing routine on level 1. All routines must be declared with the keyword ROUTINE and not just the type name for its data type.

*Norsk Data Internal Use Only*

---

## Page 49

# PLANC Compiler Commands
## Select demo

### 3.2.1 Select demo

In this section, you see how the SELECT command is used on a small example. In the example, the originally loaded program undergoes a few small changes, and the changed routines are then patched into the executable ND-500 domain.

Suppose you have the following program

```
MODULE Demo
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
ROUTINE VOID,VOID : a?
ROUTINE VOID,VOID : b?
ROUTINE VOID,VOID : c?
ROUTINE VOID,VOID : d?
BYTES READ : CrLf:: (15B,12B)

ROUTINE VOID,VOID : a
  utDisplay(CrLf//'This Is ROUTINE a.1')
  utDisplay(' - To Become Smaller')
  b
ENDROUTINE
ROUTINE VOID,VOID : b
  utDisplay(CrLf//'This Is ROUTINE b.1')
  c
ENDROUTINE
ROUTINE VOID,VOID : c
  utDisplay(CrLf//'This Is ROUTINE c.1')
  d
ENDROUTINE
ROUTINE VOID,VOID : d
  utDisplay(CrLf//'This Is ROUTINE d.1')
ENDROUTINE
INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoSelect
  INISWCK Stack
  a
ENDROUTINE
ENDMODULE
$EOF
```

Norsk Data Internal Use Only

---

## Page 50

# PLANC Compiler Commands
## Select Demo

and change it to look like this:

```
MODULE Demo
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
ROUTINE VOID,VOID : a?
ROUTINE VOID,VOID : b?
ROUTINE VOID,VOID : c?
ROUTINE VOID,VOID : d?
BYTES READ : CrLf:= (15B,12B)

ROUTINE VOID,VOID : a
    utDisplay(CrLf//'This Is ROUTINE a.2')
    b
ENDROUTINE
ROUTINE VOID,VOID : b
    utDisplay(CrLf//'This Is ROUTINE b.2')
    c
ENDROUTINE
ROUTINE VOID,VOID : c
    utDisplay(CrLf//'This Is ROUTINE c.2')
    utDisplay(' - Is Now Larger')
    d
ENDROUTINE
ROUTINE VOID,VOID : d
    utDisplay(CrLf//'This Is ROUTINE d.2')
ENDROUTINE
INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoSelect
    INITSTACK Stack
    a
ENDROUTINE
ENDMODULE
$EOF
```

then you compile and load a program that is going to have its routines reloaded like this:

```
@PLANC-500

- ND-500 Planc Compiler - June 22, 1987 Version H
*
*SELECT *ALL*

*COMPILE select-1.,select

    289 Lines compiled. No diagnostics.
    
@LINKAGE-LOADER
```

Norsk Data Internal Use Only

---

## Page 51

# PLANC Compiler Commands

## Select Demo

### ND-Linkage-Loader - H.00
- **Date:** 3. April 1986
- **Time:** 0:00  
**NIL Entered:** 6. December 1987
- **Time:** 14:40  

#### NIL: SET-DOMAIN select

#### NIL: OPEN-SEGMENT select,,,

#### NIL: LOAD-SEGMENT select

| **Program** | | **Data** |
|-------------|---|---------|
| .......1054 P | | .......10270 D01 |
| NIL: LOAD-SEGMENT (libraries)planc-utillib |

| **Program** | | **Data** |
|-------------|---|---------|
| .......1127 P | | .......10270 D01 |
| NIL: LOAD-SEGMENT (libraries)planc-lib |

### PLANC-LIB-H00
| **Program** | | **Data** |
|-------------|---|---------|
| .......1156 P | | .......10270 D01 |
| NIL: EXIT |

@ND-500 select

### Routine List
- This Is ROUTINE a.1 - To Become Smaller
- This Is ROUTINE b.1
- This Is ROUTINE c.1
- This Is ROUTINE d.1

@PLANC-500
- ND-500 Planc Compiler - June 22, 1987 Version H
  - \*
  - \*SELECT a,c
  - \*COMPILE select-2,,select

Selection compiled. 279 Lines compiled. No diagnostics.

@LINKAGE-LOADER

### ND-Linkage-Loader - H.00
- **Date:** 3. April 1986
- **Time:** 0:00  
**NIL Entered:** 6. December 1987
- **Time:** 14:40  

#### NIL: SET-DOMAIN select

#### NIL: APPEND-SEGMENT select,,,

| **Program** | | **Data** |
|-------------|---|---------|
| .......1156 P | | .......12270 D |
| NIL: RELOAD-SEGMENT select |

| **Program** | | **Data** |
|-------------|---|---------|
| .......1601 P | | .......12434 D01 |
| NIL: LOAD-SEGMENT (libraries)planc-utillib |

| **Program** | | **Data** |
|-------------|---|---------|
| .......1601 P | | .......12434 D01 |

* *Norsk Data Internal Use Only*

---

## Page 52

# PLANC Compiler Commands
Select demo

```
N1: LOAD-SEGMENT (libraries)planc-lib

PLANC-LIB-H00
Program:.......1601 P   Data:.........12434 D01
N1: EXIT
```

```
@ND-500 select

This Is ROUTINE a.2
This Is ROUTINE b.1
This Is ROUTINE c.2 - Is Now Larger
This Is ROUTINE d.1
```

## 3.3 PLANC Compiler Commands demo

```
$IF NOT $PRESENT Part $THEN
    $MESSAGE-TO-TERMINAL Part is set to 1
    CONSTANT Part=1
$ENDIF
$IF Part=1 $THEN
    $IF $PRESENT Part1 $THEN
        $KILL Part1
    $ENDIF
    $IF $PRESENT Part2 $THEN
        $KILL Part2
    $ENDIF
    $CONSTANT Part1=TRUE
    $CONSTANT Part2=FALSE
$ELSIF part=2 $THEN
    $IF
        $PRESENT Part1 $THEN $KILL Part1
    $ENDIF
    $IF $PRESENT Part2 $THEN
        $KILL Part2
    $ENDIF
    $CONSTANT Part1=FALSE
    $CONSTANT Part2=TRUE
$ELSE
    $MESSAGE-TO-TERMINAL ERROR: Part Should be declared as an
    INTEGER CONSTANT
    $EXIT
$ENDIF
```

*Norsk Data Internal Use Only*

---

## Page 53

# PLANC Compiler Commands
## PLANC Compiler Commands Demo

---

### 3.4 Let PLANC Generate the IMPORT Declarations

A software system usually consists of many modules linked together. Only the data and code that has been made known outside the module using the EXPORT statement in PLANC can be accessed by the other modules in the system, and access is only possible if the latter modules have IMPORT statements in them that exactly match the declarations of the items that are EXPORTED.

Keeping the IMPORT and EXPORT statements up to date usually means editing a number of IMPORTing modules every time the EXPORT lists or the descriptions of the items in the EXPORT lists change, to do the necessary changes in the IMPORT statements.

The PLANC compiler has been enhanced with a facility that automatically generates $INCLUDE files with IMPORT statements on it that correspond to the EXPORT statements on the file that is being compiled (not available on the ND-100). What you do is to give the command

```
GENERATE-IMPORTS <include file name>
```

to the PLANC compiler. The `<include file name>` is the name of the file where the IMPORT statements will be written.

The following sections show how this can be used.

---

### 3.4.1 Some Code with EXPORTs In It

```
MODULE GenerateImports
$IF Gen1 $THEN
  $GENERATE-imports file-imports
EXPORT type1, type2
$ENDIF
EXPORT Rout1, Rout2, Rout3, Rout4
EXPORT Var1, Var2, Var3
ROUTINE BYTE, VOID : Rout1
ENDROUTINE

TYPE type1 = ENUMERATION(Trit0, Trit1, Trit2)
TYPE type2 = RECORD
  type1 : Some
  INTEGER : Thing
ENDRECORD

```

Norsk Data Internal Use Only

---

## Page 54

# PLANC Compiler Commands

Some code with EXPORTS in it

```plaintext
ROUTINE VOID, INTEGER(INTEGER, BOOLEAN READ WRITE) : Rout2(w,x)
    2 RETURN
ENDROUTINE

ROUTINE VOID, VOID(BYTES, type2) : Rout3(y,z)
ENDROUTINE

ROUTINE VOID, VOID(BYTES: a, b, c) : Rout4
ENDROUTINE

INTEGER         : Var1
BOOLEAN READ    : Var2
BYTES           : Var3(-10:10)

ENDMODULE
$EOF
```

## 3.4.2 How to make the IMPORTs

This is what you do to generate a file with IMPORT statements on it that is ready for inclusion in dependent files.

```plaintext
@ND PLANC CONSTANT GEN1 = TRUE; COMPILE GEN-IMPORTS, GEN-IMPORTS
```

## 3.4.3 The IMPORTs that are made

```plaintext
$IF NOT $PRESENT TYPE1 $THEN
TYPE type1 = ENUMERATION(Trit0, Trit1, Trit2)
$ENDIF

$IF NOT $PRESENT TYPE2 $THEN
TYPE type2 = RECORD
    type1  : Some
    INTEGER : Thing
ENDRECORD
$ENDIF

IMPORT (ROUTINE BYTE, VOID : ROUT1)
IMPORT (ROUTINE VOID, INTEGER(INTEGER READ, BOOLEAN READ WRITE) : ROUT2)
IMPORT (ROUTINE VOID, VOID(BYTES READ, TYPE2) : ROUT3)
IMPORT (ROUTINE VOID, VOID(BYTES READ, BYTES READ, BYTES READ) : ROUT4)
IMPORT INTEGER : VAR1
IMPORT BOOLEAN READ : VAR2
IMPORT BYTES : VAR3(-10:10)

Norsk Data Internal Use Only
```

---

## Page 55

# PLANC Compiler Commands

## The IMPORTs that are made

SD2OF

---

Norsk Data Internal Use Only

---

## Page 56

# PLANC Features and Development Tools

Norsk Data Internal Use Only

---

## Page 57

```
50
```

```
Norsk Data Internal Use Only
```

---

## Page 58

# PLANC Features and Development Tools

In recent years, PLANC has been extended to include features that make programs more readable and portable, and with the advent of some object-oriented features, the way you think when you construct your program is also affected. The purpose of this chapter is to demonstrate these features to you.

## 4.1 Summary of New Features

### Object Oriented Programming in PLANC

You can include ROUTINE declarations in records, so that the records contain components that can work on both their own and external data. This leads to different ways of thinking when planning your algorithms. You will find more details in a dedicated section on p. 80.

### PLANC Utility Library

There now is available a library of routines that do common programming tasks for you, thus reducing the probability that you will have to make your own ad hoc solutions whenever you encounter a frequently recurring problem.

The PLANC Utility Library consists of routines for:

- **Trap Handling**  
  These routines take care of trap handling on the ND-500 computers.

- **Simple Symbol Table Management**

*Norsk Data Internal Use Only*

---

## Page 59

# PLAN-C Features and Development Tools

## Summary of New Features

### Routines for fast lookup in symbol tables on the ND-500.

#### Quicksorting
The well known sorting algorithm. This one works on an array of pointers to BYTES strings.

#### Binary Search
Another utility working on BYTES strings.

#### Conversion Routines (Numeric to/from BYTES string)
Making conversion between numeric variables and BYTES strings easier.

#### String Manipulation
Matching a short string of BYTES with the contents of a long one, writing into BYTES strings, handling parity bits, upper/lower-case conversion.

#### Random Number Generation
Initiate a random number generator and generate numbers.

#### File-as-segment Initiating
File-as-segment makes writing and reading fast on the ND-500.

#### Buffered I/O
Fast blocked random access I/O on the ND-100/500(0) computers. Routines for reading and writing bytes and setting file sizes.

#### Screen Handling
Routines for screen oriented I/O.

#### PLANC-GEN
The PLANC-GEN screen generating program, which works together with the Screen Handling Routines to generate screen interfaces.

#### Coroutines
Making quasiparallel programming possible. You can make code to stop both the current routine and routines in the calling sequence that leads to the current routine, and you can start other routines and restart routines that have been stopped.

### BCD Arithmetic & Numeric Edit

---

Norsk Data Internal Use Only

---

## Page 60

# PLANC Features and Development Tools

## Summary of New Features

These routines make BCD floating point arithmetic and COBOL numeric editing available in PLANC. This arithmetic gives 18 digits accuracy.

### Display String
Display BYTES on terminal.

### Current Date & Time As String
Getting these quantities into BYTES arrays in the most common formats.

### Unsigned Division
Integer division, returning both result and remainder.

### Forthcoming: C-like printf with multiple arguments

Further details about the PLANC Utility Library and PLANC-GEN are found in the manual *PLANC Utility Library and PLANC-GEN, ND-20.013 EN*.

## New Language Features in PLANC

### New ROUTINE Declaration Layout

```
ROUTINE <type>,<type> (<type>:<name list>; % Comments
        <type>:<name list>; % Comments
        ...) : <routine name>
```

For example:

```
ROUTINE VOID,VOID(INTEGER: a,b,c;BOOLEAN: d,e): x
ENDROUTINE
```

### New Types

```
BOOLEAN1
BOOLEAN2
INTEGER UNSIGNED
INTEGER<n> UNSIGNED % n IN 1, 2, 4
```

### `<record> CONVERT BYTES`

This is a feature that is sometimes convenient for passing parameters to external libraries.

---

*Norsk Data Internal Use Only*

---

## Page 61

# PLANC Features and Development Tools

## Summary of New Features

### MaxIndex/MinIndex

The number of the dimension in these predefined routines may now be omitted if the array is one dimensional, so that you can write:

```
MaxIndex(A) ⟹ The same as MaxIndex(A, 1)
```

### New Monitor_Call Statement

The predefined procedure `Monitor_Call` makes calls via the SINTRAN III Monitor Call Library available. See the manual *SINTRAN III Monitor Calls, ND-60.228* for details.

### New USING ... ENDUSING Statement

With `USING`, you can avoid prefixing record components with the name of the record they belong to anywhere between the `USING <record name list>` and adjoint `ENDUSING` statement. See the PLANC manual for details.

### New `//` Concatenation Operator

i.e.,

```
utDisplay ('Hello, ' // 'World!')
```

### 16 Chars Significance in Identifiers

Instead of the previous 10 significant characters in identifiers, you now can have 16 significant characters when you use the compiler command `SLONG-NAMES ON`.

```
SLONG-NAMES ON

INTEGER: a234567890x
INTEGER: a234567890y
```

No error message is given.

If you need to use the old 10 character limit to avoid undefined entries when loading libraries compiled with PLANC-H or earlier versions, use the compiler command `SLONG-NAMES OFF`.

Long identifier names are not available on compilers running on the ND-100.

### String Constants

You can now define

---

*Norsk Data Internal Use Only*

---

## Page 62

# PLAN-C Features and Development Tools

## Summary of New Features

### CONSTANTS

```
CONSTANT StdRemark = 'Play it again, Sam.'
```

### Nested Comments

Comments can be nested and included anywhere in the source code.

```
ROUTINE ThisRecord (% This is a comment %), ThatRecord &
        : (% Gee, it's a Wurlitzer! %) JukeBox
```

### ADA Notation for Integers

Radix followed by hash mark (#) followed by number followed by hash mark, like in

```
CONSTANT NoParity = 16#7F#
```

### Underscores in Integers

Underscores in integer constants improve readability:

```
INTEGER : ManyDigits := 1_234_567_890
INTEGER : nonsense := 2#1101_1001_0000_0110#
```

### Unsigned Variables

```
INTEGER UNSIGNED : a % Machine dependent
INTEGER UNSIGNED : b % = BYTE
```

### ENDROUTINE Name

Write

```
ROUTINE ... : name
...
ENDROUTINE name
```

to make it clear where which ROUTINEs end.

### Multiple Underscores in Identifiers

## PROGRAMMING HINTS

- In the following statement, Ix will vary from Arr's Minindex to its Maxindex:

```
Norsk Data Internal Use Only
```

---

## Page 63

# PLANC Features and Development Tools
## Summary of New Features

### Record Traversal with FOR

- **FOR lx IN AIT DO ENDFOR**

- If `Head` is a record where the component `Next` points to another record of the same type, then you can form a linked list. The following statement will let a pointer `ptr` which initially points to a member of the list step through the list until the `Next` pointer has the value `NIL`.

  ```plaintext
  FOR ptr IN Head:Next DO ENDFOR
  ```

- If you have an enumeration type, from which a set `Enum` and a variable `en` have been defined, then the following statement will repeat the contents of the loop once for each value in `Enum`:

  ```plaintext
  FOR en IN Enum DO ENDFOR
  ```

### Loop Control with WHILE and EXIT

- Both `FOR` and `DO` loops can contain `WHILE` statements. When a loop terminates via a `WHILE` statement, it can contain an `EXITWHILE` statement at the end indicating that what follows should be executed before leaving the loop. In addition, `FOR` loops can contain `EXITFOR` statements indicating what the program should do before leaving the loop. If a `FOR` loop containing both an `EXITFOR` and an `EXITWHILE` exits, only one of the two statements are executed depending on how the loop was terminated. See the PLANC manual for details.

### Standard Routines for Dynamic Data Allocation

- PLANC has the following standard routines that deal with dynamic data allocation: `INSERT`, `APPEND`, `REMOVE`, `NEW`, and `DISPOSE`. `INSERT` inserts an element that can be a member of a linked list as the first element in the list, while `APPEND` inserts the element at the end of the list. (The end of a list is indicated by a link pointer pointing to `NIL`.) See the PLANC manual for details.

### Routine Modifiers

- The following `ROUTINE` modifiers are available to make various kinds of subroutines available: `ROUTINE INLINE`, `SPECIAL`, `STANDARD`, `REFERENCE`, `NATIVE`, `C`, `MAINSTART`, and `XARGS`. These modifiers are the key to interfacing to other languages. See p. 69 for details.

### Increment/Decrement Operators

- If you want to increment/decrement an integer before you use it in a statement, you can use the unary operators `--` and `++`. For example, `i` will have the value 25 after execution of the statements.

```plaintext
Norsk Data Internal Use Only
```

---

## Page 64

# PLANC Features and Development Tools

## Summary of New Features

```
10 =: i; 15 =: j
DO ++i WHILE --j > 0 ENDDO
```

- **BITS** is equivalent to a **BOOLEAN ARRAY PACKED** declaration. (Consequently, if you define **BITS : b (0:31)**, then b(0) is the most significant bit.)

- **TYPEDEF** specifies a new identifier to be of the same type as a previously defined identifier.

- **BIT SIZE** gives the size of data elements in bits, which for instance is useful when dealing with packed records.

- **BIT POSITION** tells you which bit inside a data structure one of its components begins.

- The type **ENUMERATION**, can be used to define variables that take on user defined values, thus:

  ```
  TYPE PrimaryColour = ENUMERATION (Red, Yellow, Blue)
  PrimaryColour : pc
  ...
  IF pc = Red THEN ... ENDIF
  ```

- **SETS** of **ENUMERATION1s**, **ENUMERATION2s**, **BYTES**, **INTEGER1s** and **INTEGER2s** can be defined. For instance:

  ```
  TYPE SubColour = PrimaryColour SET
  SubColour : Green := (Yellow, Blue)
  ```

  See the manual for details (but the possibility to use two byte variables is added to PLANC after the manual was last revised).

- **MOD** can be used in **RECORD declarations** to force a variable to begin at a multiple of bytes after the beginning of the record. In the following record, i will be placed from the first available word limit relative to the first byte in the record.

  ```
  TYPE r = RECORD PACKED
  BITS : b (11:37)
  INTEGER : i MOD 4
  ENDRECORD
  ```

Norsk Data Internal Use Only

---

## Page 65

# PLANC Features and Development Tools
## Summary of New Features

Avoid:

- OverLoad'ing of symbols (gives debugger problems)
- Input(d,'..',..,..); Output(d,'..',..,..)  
  (does not generally give good solutions for I/O)

## Development Environment

### LED
The LED has windows to regions where source files are kept, possibility to connect windows to processes using TABS so that syntax checking and error detection can be done through test compilations, pretty-print facilities and more. See the _LED User Manual, ND-60.266 EN_.

### Source Debugger
The Source Debugger is a version of the Debugger where the debugger's command processor runs in one of the windows of the LED. This coupling makes it possible for the Debugger to indicate precisely where in the source the current instructions are generated, and to examine and change the source while debugging. Details in _LED User Manual, ND-60.266 EN_.

### Automake
Automake is a close relative of all the other MAKE utilities in the world. The manual is _Automake User Manual, ND-60.232.4 EN_.

### Histogram
These important measurement tools for the ND-500(0) computers are available via commands in the ND-500 Monitor. See the manual _SINTRAN III Time Guide, ND-30.049 EN_.

## Generating Include-files
The compiler command

```
GENERATE-IMPORTS <FILE NAME>
```

will produce an include-file with the given name. The default file type is `.IMPT`. An example is given on p. 46.

## Routine Modifiers NATIVE and C
When you use these modifiers when declaring ROUTINEs on MC and iAPX computers (they are synonymous), PLANC generates C compatible calls.

The NATIVE/C modifier is not necessary on any other CPU such as ND-100/ND-500(0).

---

Norsk Data Internal Use Only

---

## Page 66

# PLANC Features and Development Tools

## Summary of New Features

### Routine Modifier YARGS

To allow calls with variable no. of parameters. This is compatible with the C function call conventions.

### Routine Modifier MAINSTART

To access UNIX and MS/DOS parameters (`argc`, `argv`, `envp`). A `MAINSTART` routine is declared like this:

```
ROUTINE MAINSTART (INTEGER : argc; &
    BYTE POINTER POINTER : argv; &
    BYTE POINTER POINTER : envp) &
    : Main
```

### Fast Routine Replacement in ND-500 PLANC Programs with $SELECT

Using this compiler command, only the listed routines are compiled, and can be loaded to replace only the previous version of the routines without necessitating a complete reload of the program system.

The mechanism used makes very short module and routine names necessary, as the recompiled routine will be given a name which is a composite of the module name and the routine name separated with a period like this: `<ModName.RoutName>`. The total length of the composite, including period, should not exceed 16 bytes.

(The 16 byte limit is valid for the old Linkage-Loader only. The new ND-Linker will have a symbol size of 255 bytes - but the A version of it cannot append new code to existing domains. That facility will not be available until the B version of the ND-Linker.)

For an example, see p. 40.

---

Norsk Data Internal Use Only

---

## Page 67

# 4.2 Readability Hints

Use the LED's indentation facilities for FORTRAN, C, and PLANC to make your code look more uniform. The LED also has built-in ready-made routine headers. The example below shows how you can improve the readability of routine declarations.

```
MODULE NiceDemo
INTEGER ARRAY : Stack (0:2047)

%============================================================%
% This example shows one way of improving the reada-         %
% bility of a routine declaration.                           %
%============================================================%
ROUTINE VOID, VOID &
  (INTEGER1, INTEGER2, INTEGER) : FirstExample &
  (Parameter1, Parameter2, Parameter3)
  % ...
ENDROUTINE FirstExample

%============================================================%
% This example shows another way of improving the reada-     %
% bility of a routine declaration. Here, there is more       %
% room for comments.                                         %
%============================================================%
ROUTINE VOID, VOID ( &
  INTEGER1 : Parameter1; & % Comment1
  INTEGER2 : Parameter2; & % Comment2
  INTEGER : Parameter4) & % Comment4
  : SecondExample
  % ...
ENDROUTINE SecondExample

PROGRAM : Main
  INVSTACK Stack
  % ...
ENDROUTINE Main
ENDMODULE
% NiceDemo
$EOF
```

Norsk Data Internal Use Only

---

## Page 68

## PLANC Features and Development Tools
Readability Hints

---

### 4.3 Packing on the Various Processors

If you are in doubt about the size of and record component position within a packed record, then use the predefined functions `Bit_Size`, which gives the number of bits in a data structure and `Bit_Position`, which gives the number of the bit within the record where the record component starts.

---

#### 4.3.1 On the ND-500(0)

Fields within packed records are assigned space from bit 31 down to 0.

If a record component requires more space than there are free bits in the 32 bit word, the current word is abandoned and a new one is defined at the next byte boundary.

Thus, a maximum of 7 bits may be vacant per record component in a packed record.

---

#### 4.3.2 On the ND-100, MC680xx and INTEL 286/386

Fields within packed records are assigned space from bit 15 down to bit 0.

If a record component requires more space than there are free bits in the 16 bit word, the current word is abandoned and a new one is defined at the next word boundary.

Thus, a maximum of 15 bits may be vacant per record component in a packed record.

---

Norsk Data Internal Use Only

---

## Page 69

# PLANC Features and Development Tools

## 4.4 Stacks - Usefulness and Fallacies

A stack is an array that the program uses to store lists of routine descriptions - also known as **stack frames** - in routine call sequences, plus transient data such as parameters that are transferred to routines and intermediate results of expressions used in the routines. In addition, objects created with the **New** routine are put on the stack if they are not explicitly put somewhere else.

The lists of stack frames are doubly linked. The reason for this double linking also explains some of the nature of the stack usage, because the address of the stack frame is kept permanently in one of the CPU's registers during execution of the routine so that addressing into the stack can be done relative to this register. The address of the previous stack frame is kept in the current stack frame, so that the current routine knows where to return after it has returned to its caller. The address of the next free stack frame is also kept in the current stack frame. It is kept ready in case the current routine calls a new routine.

This double linking also makes it possible for you to look at and retrace the routine call sequences in your program. When you use ND's **Symbolic Debugger**, you can do this with the commands **ACTIVE-ROUTINES**, which shows the current call sequence, and **LOOK-AT-STACK**, which gives details of the stack frame and the transient data (parameters and intermediate results).

(If you do a **LOOK-AT-STACK** on the stack of a PLANC program on the ND-500(0), you may be puzzled by the fact that the field called NUMBER OF PARAMETERS is always zero even if the routine called has parameters. This is because the number of parameters is not needed by ordinary PLANC routines. However, if you use other routine modifiers such as STANDARD, this field in the stack frame is used.)

Routine calls are important elements in any program, and computers must handle them efficiently and securely. That is, there should be minimal time penalties for ordering code into subroutines, and stack frames or transient data should not overwrite other parts of the program by accident. (Such overwrites will lead to obscure error situations!)

---

Norsk Data Internal Use Only

---

## Page 70

# PLANC Features and Development Tools

## Stacks - Usefulness and Fallacies

For these reasons, most CPUs have special instructions for putting stack frames onto the stack ("pushing") and retrieving stack frames from the stack when the subroutine called has terminated ("popping").

Additional facilities may exist in the hardware to keep the pushing and popping inside the areas of memory designated for stack usage. Then, if the program tries to push past the end of the stack areas, you get a **stack overflow**, and the hardware traps this and reports it to the operating system if there exist no special trap handlers for this condition in the program itself. Similarly, if the program tries to pop one stack frame too many, a **stack underflow** occurs and is likewise reported.

Stacks must be initialized by the program before they can be used. It is evident from the explanation above that a stack area is not just like any other part of the memory used by a program, especially if there are trap handlers in the hardware to prevent stack overflows and underflows. So the hardware must be notified about the first stack frame pointer, that of the main program, and about the limits of the stack so that over/underflows can be trapped. This is what PLANC's **Inistack** predefined routine does for you.

In some CPUs, special instructions for preparing stacks have been implemented as well, most notably on the ND-500(0). But in any case, **Inistack** sets up a new stack for usage.

Normally, only one **Inistack** invocation is needed in a PLANC program. However, the possibility PLANC gives you to initiate a new stack in the declaration part of outer module level **ROUTINES** is useful in some situations.

When a **ROUTINE** contains an **Inistack** declaration, the program will stop using the previous stack and use the new stack for that **ROUTINE** and all **ROUTINES** called by it (that is, until yet another **Inistack** declaration is encountered). Upon **RETURN** or **ERROR RETURN** from the **ROUTINE**, the previous stack is employed again. Thus, the previous stack is completely untouched by whatever the **ROUTINE** did.

This property is desirable when you call PLANC **STANDARD ROUTINES** from programs written in other languages. Using it, you avoid doing harm to whatever was on the stack of the previous code. So when you make **ROUTINES** and libraries that will be used by code written in other languages, start the routines with an **Inistack** declaration.

Another situation where the flexible stack designation scheme may be of some value, is when you don't have much dataspace available (such on the ND-100 and under MS-DOS). Then you may want to use a part of the data space as for example a heap for dynamic data structures in one part of the program and as a stack in another part of it.

---

*Page 63*

*Norsk Data Internal Use Only*

---

## Page 71

# PLANC Features and Development Tools
## Stacks - Usefulness and Fallacies

If you work on an ND-500(0), you can use that CPU's trap-handling facilities to prevent stack overflows. The traps can be set using the PLANC Utility routine `utDefineTraps`, and the trap that you need to set to catch stack overflows is number 27 decimal.

The following example shows how stack overflows can be trapped and new stacks initialized.

```
MODULE dule
CONSTANT StackSize = 128
$LIST OFF
$INCLUDE (lib)planc-util:defs
$INCLUDE (lib)planc-util:incl
$LIST ON
INTEGER : RecursionLevel := 0
%=========================================================%
% Declaring two stacks, one for emergencies.              %
%=========================================================%
INTEGER ARRAY : stack (0:StackSize-1), stack2 (0:StackSize-1)
%=========================================================%
% A pointer to the stack currently in use.                %
%=========================================================%
INTEGER ARRAY POINTER : StackPointer := Addr(stack)
BYTES : Ctrl (0:1) := (15B, 12B)
ROUTINE VOID, VOID : NewStack ?
%=========================================================%
% This recursive routine will burst the stack eventually. %
%=========================================================%
ROUTINE VOID, VOID : BustTheStack
%=========================================================%
% An exception handler for the first stack.               %
%=========================================================%
ON ROUTINEERROR DO
   IF ERRORCODE=TrapSTO THEN                              % 7633B
      IF StackPointer = Addr(stack) THEN
         ON ROUTINEERROR DO
            ERRORCODE ERRORRETURN
         ENDON
         NewStack
      ELSE 
         TrapSTO ERRORRETURN
      ENDIF
   ELSE
      -1 ERRORRETURN
   ENDIF
ENDON
++ RecursionLevel
BustTheStack
ENDROUTINE
%=========================================================%
% If the old stack bursts, initiate a new one and continue %
%=========================================================%

Norsk Data Internal Use Only
```

---

## Page 72

# PLANC Features and Development Tools
## Stacks - Usefulness and Fallacies

### ROUTINE VOID, VOID : NewStack
```
INISTACK stack2
%===============================================================%
% Traphandler, in case second stack bursts.                     %
%===============================================================%

ON ROUTINEERROR DO
    IF ERRCODE=TrapSTO THEN
        TrapSTO ERRRETURN
    ELSE
        -1 ERRRETURN
    ENDIF
ENDON

Addr(stack2) =: StackPointer
utDisplay ('Stack overflow - new stack in use'// CrLf)
BustTheStack
ENDROUTINE
```

### PROGRAM : main
```
BYTES : bs(0:4)
BYTES POINTER : bp
INISTACK stack
INTEGER : otei, ote2, pos
0 =: otei =: ote2
TRUE =: Bit(ote1, 27)
utDefineTraps (ote2, otei, FALSE)
ON ROUTINEERROR DO
    IF ERRCODE = TrapSTO THEN
        0 =: pos
        RecursionLevel utCIntAsc (bs, pos, 10) =: bp
        utDisplay &
            ('The Stack Overflow trap finally snapped at recursion level ' &
            // Ind(bp))
    ELSE
        utDisplay 'Some other routine error.'
    ENDIF
    utDisplay ('.' // CrLf)
ENDON
BustTheStack
ENDROUTINE
ENDMODULE
$EOF
```

This is what happens when you execute this program.

@ND STACK

```
Stack overflow - new stack in use!
The Stack Overflow trap finally snapped at recursion level 25.
```

Norsk Data Internal Use Only

---

## Page 73

# PLANC Features and Development Tools
## Stacks - Usefulness and Fallacies

### 4.5 Some Remarks on Parameter Transfer

Parameters are normally transferred as values, i.e., copied onto the stack. Two important exceptions are ARRAYS and RECORDs, which are passed as pointers. The consequence of this is that changes done to array/record components by the called routine will be in effect for the calling routine after RETURN from the called subroutine, while changes to simple variables passed as parameters will not be in effect after the RETURN statement.

The default access mode for code inside a routine to its parameters and in-value is READ. The access to the in-value cannot be changed, but if you want the routine to change the value of a parameter, you must modify it with WRITE or READ WRITE in the routine declaration. Changes done to WRITE or READ WRITE parameters during routine execution will be in effect after return from the routine.

ARRAYs are passed as pointers, with a pointer to a "virtual origo" (the address of the zeroth element of the array which all addresses in the array are relative to), a lower limit and an upper limit. For each dimension greater than one, this descriptor is extended with the number of elements in the previous dimension, along with the new upper and lower limits of the new dimension.

RECORDs are passed as pointers to their first address.

ARRAYs/RECORDs as parameters to STANDARD routines are passed as single pointers only. In the case of ARRAYs, the address passed is the address of the "virtual origo". Therefore it is convenient to declare ARRAYs that are going to be passed to STANDARD routines with lower limit equal to 0.

*Norsk Data Internal Use Only*

---

## Page 74

# 4.5.1 The data/runtime organization on the ND-500(0)

## Routine entry and exit on the outermost module level

The instructions `CALL` and `CALLG` are used for all calls. Parameters are transferred explicitly with number of arguments equal to zero unless the routine has been declared with the `STANDARD` modifier.

Ordinary (non-`STANDARD`) routine calls are followed by the `IF K RET` instruction. Routines usually begin with `ENTS`, if they contain an `Initstack` then they begin with `ENNM`, while main programs begin with `INIT`.

## Routine entry and exit in nested routines

Nested routines are entered with `ENID` and save the `L` register in a local temporary variable. Exit is by an indirect jump. Parameters are passed explicitly by the generated code.

Nested routines use the stack frame of the enclosing routine, thus accessing its local scope variables directly `.B` (= relative to B). The K flag is set explicitly.

## Parameter transfer

See preceding section for principles.

The `CALL/CALLG` instructions with `n` arguments are used to call `STANDARD` routines.

## Out-values from routines

All kinds of `BOOLEAN`, `ENUMERATION` and `INTEGER` variables are returned in the `I1` register. 32 bit reals are returned in the `A1` register, while 64 bit reals are returned in the `D1` register. Pointers to simple variables and records are returned in the `I1` register. Array pointers and sets with more than 32 bits are fetched from the stack frame that is being left. Sets with fewer than 32 elements are returned in `I1`.

## In-values to routines

As for out-values, but array pointers and big sets are moved explicitly to next stack frame.

## Routine pointers

A routine pointer is just a pointer to a single location, like a record pointer. But remember, you can't point to nested local routines, only to routine on the outermost module level.

```
Norsk Data Internal Use Only
```

---

## Page 75

# PLANC Features and Development Tools
## The Data/Runtime Organization on the ND-500(0)

### Representation of Non-packed Data in Bytes

| Type                 | Bytes                   |
|----------------------|-------------------------|
| BOOLEAN1             | 1                       |
| BOOLEAN2             | 2                       |
| BOOLEAN              | 4                       |
| BYTE                 | 1                       |
| INTEGER1             | 1                       |
| INTEGER2             | 2                       |
| INTEGER4             | 4                       |
| INTEGER              | 4                       |
| INTEGER RANGE        | 1, 2 or 4 depending on range |
| REAL                 | 4                       |
| REAL8                | 8                       |
| ENUMERATION          | 4                       |
| xxx ARRAY POINTER    | 12 times dimensionality |
| yyy POINTER          | 4                       |
| zzz SET              | (NumberOfElements + 7) / 8 |

### Storage Alignment for Non-packed Data

Here, byte means alignment on the next byte from current position, halfword means alignment on the next half word in memory, and word means alignment on the next word in the memory space.

| Type                | Alignment               |
|---------------------|-------------------------|
| BOOLEAN1            | byte                    |
| BOOLEAN2            | halfword                |
| BOOLEAN             | word                    |
| BYTE                | byte                    |
| INTEGER1            | byte                    |
| INTEGER2            | halfword                |
| INTEGER4            | word                    |
| INTEGER             | word                    |
| INTEGER RANGE       | byte, halfword or 4 depending on range |
| REAL                | word                    |
| REAL8               | word                    |
| ENUMERATION         | word                    |
| xxx ARRAY POINTER   | word                    |
| yyy POINTER         | word                    |
| zzz SET             | word                    |
| RECORD              | word                    |

### Representation of Packed Data

See p. 61

### The Error Return Mechanism

If a routine gets an error return, i.e., exits via the `<value>` ERRETURN statement, the `<value>` is returned in the I1 register, and K is set to one.

Norsk Data Internal Use Only

---

## Page 76

# PLANC Features and Development Tools
The Data/Runtime Organization on the ND-500(0)

Normal PLANC routine sequences are followed by a test on K, upon which an ON ROUTINEERROR exception handler can be invoked, or a direct return to the previous routine level takes place. If an ON ... sequence is activated, the I1 is stored into the ERRCODE variable, so that it can be examined further.

## Exception Handling
No default exception handling is implemented.

## Traps and Trap Handling
You can handle the traps yourself using the utDefineTraps routine. See the manual *PLANC Utility Library and PLANC-GEN, ND-60.297* for details.

## 4.6 Routine Modifiers

The following example shows how the various routine modifiers that are available on the compiler for ND and MC CPUs are used. PLANC for the 80286 and 80386 CPUs has several other modifiers for mixing with code in other languages, such as C, FORTRAN, Pascal, and COBOL. See the manual *PLANC for INTEL Microprocessors, ND-20.012* for details.

```
MODULE RoutineModifiers
EXPORT RoutNative ALIAS '_routnative'
EXPORT RoutStandard
$INCLUDE(LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE(LIBRARIES)PLANC-UTILLIB:INCL
INTEGER ARRAY : Stack(0:1023)

ROUTINE VOID,BYTE(BYTE:Char) : Rout
    Char AND 177B RETURN
ENDROUTINE

ROUTINE VOID,BYTE(BYTE READ:Char) : RoutRead
    Char AND 177B RETURN
ENDROUTINE

ROUTINE VOID,BYTE(BYTE WRITE:Char) : RoutWrite
    177B =:Char; 177B RETURN
ENDROUTINE

ROUTINE VOID,BYTE(BYTE READ WRITE:Char) : Rout1ReadWrite
    Char AND 177B RETURN
ENDROUTINE

ROUTINE VOID,BYTE(BYTE READ WRITE:Char) : Rout2ReadWrite
    177B =:Char; 177B RETURN
ENDROUTINE

Norsk Data Internal Use Only
```

---

## Page 77

# PLANC Features and Development Tools

## Routine Modifiers

### ENDROUTINE

#### ROUTINE REFERENCE VOID,BYTE(BYTE:Char) : RoutReference
```
    Char AND 177B RETURN
```
#### ENDROUTINE

#### ROUTINE INLINE VOID,BYTE(BYTE:Char) : RoutInline
```
    Char AND 177B RETURN
```
#### ENDROUTINE

#### ROUTINE NATIVE VOID,BYTE(BYTE:Char) : RoutNative
```
    INISTACK Stack
    Char AND 177B RETURN
```
#### ENDROUTINE

#### ROUTINE STANDARD VOID,BYTE(BYTE:Char) : RoutStandard
```
    INISTACK Stack
    Char AND 177B RETURN
```
#### ENDROUTINE

#### ROUTINE SPECIAL BYTE,BYTE : RoutSpecial % CAUTION:
```
    S* ENTD % You must provide entry and
    S* BYL AND 177B % exit sequences here -
    S* REMD % PLANC does not do it
```
#### ENDROUTINE
```
    % for you.
```

#### ROUTINE SPECIAL VOID,INTEGER1(INTEGER1:param) : RoutVerySpecial
#### ENDROUTINE

#### ROUTINE VOID,BYTE(BYTE:Char) : RoutHelpVerySpecial
```
    Char AND 177B RETURN
```
#### ENDROUTINE

### $SEJECT
```
$IF Unix $THEN
ROUTINE MAINSTART VOID,VOID(INTEGER:Argc,BYTE POINTER POINTER:Argv)
    : RoutMainstart
$ELSIF MsDos $THEN
ROUTINE MAINSTART VOID,VOID(INTEGER:Argc,BYTE POINTER POINTER:Argv)
    : RoutMainstart
$ELSE
PROGRAM : RoutProgram
$ENDIF
```

```
    INISTACK Stack
    INTEGER1 : Int1, Int2
    BYTE     : Ch1, Ch2
    BOOLEAN  : Errors

    FALSE =:Errors
    -9 =:Int1; 0 =:Int2
    367B =:Ch1; 0 =:Ch2
    Rout Ch1 =:Ch2
    (Ch2<167B) OR Errors =:Errors
    367B =:Ch1; 0 =:Ch2
```

```
Norsk Data Internal Use Only
```

---

## Page 78

# FIANC Features and Development Tools

## Routine Modifiers

```
RoutRead Ch1 =:Ch2
(Ch2\<167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

RoutWrite Ch1 =:Ch2
(Ch1\>177B) OR (Ch2\<177B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

RoutReadWrite Ch1 =:Ch2
(Ch2\<167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

Rout2ReadWrite Ch1 =:Ch2
(Ch1\>177B) OR (Ch2\<177B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

RoutReference Ch1 =:Ch2
(Ch2\<167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

RoutInline Ch1 =:Ch2
(Ch2\<167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

Ch1 RoutSpecial =:Ch2
(Ch2\<167B) OR Errors =:Errors
367B =:Ch1; 0 =:Ch2

RoutHelperVerySpecial(Ch1) =:Ch2
(Ch2\<167B) OR Errors =:Errors

RoutVerySpecial(Int1) =:Int2
(Ch2\<167B) OR Errors =:Errors
```

```
IF NOT Errors THEN utDisplay('No ') ENDIF
utDisplay('Errors Found')
```

### Endroutine

### Endmodule

\$EOF

---

Norsk Data Internal Use Only

---

## Page 79

# UNIX Command Line/Environment Pointer Retrieval

A special routine modifier, `MAINSTART`, has been introduced to access the command line and environment variables on UNIX and MS/DOS. This modifier lets you replace the ordinary `PROGRAM` declaration of your main program with a `ROUTINE` declaration that has parameter declarations that match the pointers to the command line (a.k.a. Argc) and environment (Envp).

The next section shows how this can be done in UNIX.

## Example

```
%==============================================================%
% Scanning Through The Command Line                            %
% When Executing On A Unix System.                             %
%==============================================================%
% Use Of: ++ on POINTERs                                       %
%    ROUTINE Modifier MAINSTART                                %
%==============================================================%

CONSTANT Cr=15B, Lf=10B
TYPE BytesPointer = RECORD
    BYTE POINTER : Adr
    INTEGER      : MinIx, MaxIx
ENDRECORD

MODULE PrintArguments

BYTES READ : CrLf:= (Cr,Lf)
BYTES      : CommandLine(0:1023)
CONSTANT Clo:= Minindex(CommandLine,1)
INTEGER    : Cix:= Clo
INTEGER    : Chi:= Clo-1

ROUTINE BYTE POINTER,BYTES POINTER : StringToBytes?

INTEGER ARRAY : Stack(0:1023)
ROUTINE VOID,VOID MAINSTART(INTEGER:Argc;BYTE POINTER
POINTER:Argv,Envp) : prog
   INLIST(Stack
   BYTE POINTER POINTER : ArgPtr
   BYTES POINTER : TmpPtr

Norsk Data Internal Use Only
```

---

## Page 80

# PLAN C Features and Development Tools

## Example

```plaintext
Argv =:ArgPtr

% Skip Program Name
% NOTE: ArgPtr is Incremented NOT by 1, but by SIZE(BYTE POINTER) (= 4)
IF (ArgPtr^>NIL) AND (Ind(ArgPtr)^>NIL) THEN ++ArgPtr ENDIF

% Scan Through Rest of The Arguments
DO WHILE Ind(ArgPtr) >< NIL
    ArgPtr StringToBytes =:TmpPtr
    IF TmpPtr^>NIL THEN Ind(TmpPtr) utPut(CommandLine,Cix)
ENDIF
ENDDO
Cix-1 =:Chi
utDisplay(CrLf //
'CommandLine:"/'//CommandLine(Clo:Chi)//'/"'//CrLf)
ENDROUTINE

ROUTINE BYTE POINTER,BYTES POINTER : StringToBytes
    BytesPointer : BtSPtr
    BYTES POINTER : BPtr-BtSPtr
    BYTE POINTER : TmpPtr
    IF (@=NIL) OR (Ind(@)=0) THEN NIL RETURN ENDIF
    USING BtSPtr
        @ =:TmpPtr =:Idx
        0 =:MiIx =:MaxIx
        DO WHILE Ind(TmpPtr)>0
            % TmpPtr is incremented with 1 = SIZE(BYTE)
            % MaxIx is incremented with 1
            ++TmpPtr; ++MaxIx
        ENDDO
        % Decrement MaxIx with 1
        --MaxIx
    ENDUSING
    BPtr RETURN
ENDROUTINE
ENDMODULE
SPDF

Norsk Data Internal Use Only
```

---

## Page 81

# PLANC Features and Development Tools

## 4.8 Records

PLANC's `RECORD` concept is an important part of the language. Recently, the `RECORDS` have been extended with a possibility to include `ROUTINE` declarations as part of them, making PLANC a more "object-oriented" language.

The following examples demonstrate how they are used. The final examples show how to use `ROUTINES` as part of `RECORDS`.

---

### 4.8.1 Simple Record Usage

The first example shows "plain vanilla" record usage (similar to `structs` in C and PASCAL records).

```plaintext
CONSTANT Cr=15B, Lf=12B
MODULE Demo
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
BYTES READ : CrLf:= (Cr,Lf)
TYPE Irec = RECORD
    INTEGER : Val
ENDRECORD
TYPE Brec = RECORD
    BOOLEAN : Val
ENDRECORD
Irec : Rec0,Rec1,Rec2
Brec : Rec3,Rec4,Rec5
ROUTINE Brec,BOOLEAN(Irec:Rx;Irec WRITE:Ry):SomeThing
    USING Ry
    IF NOT @.Val THEN 0 =:Val ELSE Rx.Val =:Val ENDIF
    ENDUSING
    (Rx.Val=0) RETURN
ENDROUTINE
INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoRecords
    INSTACK Stack
    USING Rec0
    I =:Val
        TRUE =:Rec3.Val
    ENDUSING
IF Rec3 SomeThing(Rec0,Rec1) THEN

Norsk Data Internal Use Only
```

---

## Page 82

# PLANC Features and Development Tools

## Simple Record Usage

```
utDisplay(CrLf//'Something Happened.'//CrLf)
KISE
  utDisplay(CrLf//'Something Didn't Happen.'//CrLf)
ENDIF
ENDROUTINE
ENDMODULE
$EOF
```

## 4.8.2 Variant Record Usage

The second example shows how you make variants of record declarations (similar to unions in C).

```
CONSTANT Cr=15B, Lf=12B
MODULE Demo
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
BYTES READ : CrLf := (Cr,Lf)
TYPE Head = RECORD
ENDRECORD
TYPE Irec = Head RECORD
  INTEGER : IVal
ENDRECORD
TYPE Brec = Head RECORD
  BOOLEAN : BVal
ENDRECORD
Irec : Rec0,Rec1,Rec2
Brec : Rec3,Rec4,Rec5
ROUTINE Head POINTER,BOOLEAN(Head POINTER:Rx;Head POINTER:Ry):SomeThing
  USING Ind Ry
  IF NOT Ind(@).BVal THEN 0 =:IVal ELSE Ind(Rx).IVal =:IVal
ENDIF
ENDUSING
  (Ind(Rx).IVal=0) RETURN
ENDROUTINE
ROUTINE VOID,VOID(Head POINTER:Rz):Tricky
  USING Ind Rz
  IF BVal THEN 0 =:IVal ELSE 1 =:IVal ENDIF
ENDUSING
ENDROUTINE
ROUTINE VOID,VOID(Head POINTER: Hptr;Irec POINTER: Iptr) : EqNotEq
  ASSERT Hptr=Iptr
  ASSERT Size(Ind(Hptr))<Size(Ind(Iptr))
  ASSERT Ind(Hptr).IVal=Ind(Iptr).IVal
  IF Ind(Hptr).BVal THEN utDisplay(' - Thanks God') ENDIF

Norsk Data Internal Use Only
```

---

## Page 83

# ENROUTINE

```
INTEGER ARRAY : Stack(0:1023) 
PROGRAM : DemoRecords 
INITSTACK Stack 
USING Rec0 
  1 := lVal
  TRUE := Rec3.BVal
ENDUSING 
IF Addr(Rec3) SomeThing(Addr(Rec0),Addr(Rec1)) THEN
  utbDisplay(CrLf/'Something Happened.')
ELSE 
  utbDisplay(CrLf/'Something Didn't Happen.')
ENDIF 
EqNotEq(Addr(Rec1),Addr(Rec1)) 
ENDROUTINE
ENDMODULE
$EOF
```

---

## 4.8.3 Linked list of records

Then there is an example of linked lists of records - note the predefined routines **Append** and **Insert** here. **Append** should be avoided in critical sections of the program because of the overhead involved in finding the end of the list. **Insert** is very fast.

```
CONSTANT Cr=15B, Lf=12B
MODULE Demo
SINCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
SINCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
BYTES READ : CrLf:= (Cr,Lf)
TYPE Link = RECORD
  Link POINTER : Next
ENDRECORD

TYPE lrec = Link RECORD
  INTEGER : lVal
ENDRECORD

TYPE Brec = Link RECORD
  BOOLEAN : BVal
ENDRECORD

lrec : Rec0,Rec1,Rec2
Brec : Rec3,Rec4,Rec5
Link POINTER : Head
Link Pointer : Ptr0, Ptr1, Ptr2, Ptr3, Ptr4, Ptr5

ROUTINE VOID,VOID : DumpList
  Link POINTER : LinkIx
BYTES : String(0:79)
```

Norsk Data Internal Use Only

---

## Page 84

# PLNC Features and Development Tools
## Linked List of Records

```
INTEGER : Pos

MinIndex(String,1) =:Pos
CrLf utPut(String,Pos)
FOR LinkRtv IN Head:Next DO
  Ind(LinkRtv).IVal utCIntAsc(String,Pos,10)
  '  utPut(String,Pos)
END FOR
utDisplay(String(0:Pos-1))

ENDROUTINE

INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoRecords
INISTACK Stack
Link POINTER : TmpLink

100 =:Rec0.IVal+1 =:Rec1.IVal+1 =:Rec2.IVal
FALSE =:Rec3.BVal =:rec4.BVal; TRUE =:Rec5.BVal

Addr(Rec1) =:Head
Addr(Rec2) Append Head:Next
Addr(Rec0) Insert Head:Next
DumpList
% Rec0 Rec1 Rec2

Addr(Rec1) Remove Head:Next =:TmpLink
Addr(Rec3) Insert Head:Next
Addr(Rec4) Append Head:Next
DumpList
% Rec3 Rec0 Rec2 Rec4

Rec2.Next =:Rec5.Next
Addr(Rec5) =:Rec2.Next
DumpList
% Rec3 Rec0 Rec2 Rec5 Rec4

New Irec In Heap =:Ptr0
New Irec In Heap =:Ptr1
New Irec =:Ptr2
New Brec =:Ptr3
New Brec In Heap =:Ptr4
New Brec In Heap =:Ptr5
100 =:Ind(Ptr0).IVal+1 =:Ind(Ptr1).IVal+1 =:Ind(Ptr2).IVal
FALSE =:Ind(Ptr3).BVal =:Ind(Ptr4).BVal; TRUE =:Ind(Ptr5).BVal

Ptr1 =:Head
Ptr2 Append Head:Next
Ptr0 Insert Head:Next
DumpList
% Ind(Ptr0) Ind(Ptr1) Ind(Ptr2)

Ptr1 Remove Head:Next =:TmpLink
Dispose(TmpLink)

Norsk Data Internal Use Only
```

---

## Page 85

# PLANC Features and Development Tools
## Linked List of Records

```
Ptr3 Insert Head:Next
Ptr4 Append Head:Next
DumpList
  % Ind(Ptr3) Ind(Ptr0) Ind(Ptr2) Ind(Ptr4)

Ind(Ptr2).Next =:Ind(Ptr5).Next
Ptr5 =:Ind(Ptr2).Next
DumpList
  % Ind(Ptr3) Ind(Ptr0) Ind(Ptr2) Ind(Ptr5) Ind(Ptr4)
ENDROUTINE
ENDMODULE
$PDF
```

## 4.8.4 Linked List of Records with New

Remember that linked list pointers can be used directly in PLANC FOR-statements.

```
CONSTANT Cr=15B, Lf=12B
MODULE Demo
$INCLUDE (LIBRARIES)PLANC-UTILLIB:DEFS
$INCLUDE (LIBRARIES)PLANC-UTILLIB:INCL
BYTES READ : CrLf:= (Cr,Lf)
TYPE Link = RECORD
  Link POINTER : Next
ENDRECORD
TYPE Lrec = Link RECORD
  INTEGER : IVal
ENDRECORD
TYPE Brec = Link RECORD
  BOOLEAN : BVal
ENDRECORD
Link POINTER : Head
```

```
ROUTINE VOID,VOID : DumpList
  Link POINTER : LinkIx
  BYTES : String(0:79)
  INTEGER : Pos

MinIndex(String,1) =:Pos
CrLf utPut(String,Pos)
FOR LinkIx IN Head:Next DO
  Ind(LinkIx).IVal utCIntAsc(String,Pos,10)
  '  ' utPut(String,Pos)
ENDFOR
utDisplay(String(0:Pos-1))
```

```
Norsk Data Internal Use Only
```

---

## Page 86

# PLANC Features and Development Tools

## Linked List of Records with New

### ENDROUTINE

```plaintext
INTEGER ARRAY : Stack(0:1023)
PROGRAM : DemoRecords
INITSTACK Stack
Link POINTER : TmpLink

New Irec IN Heap =:;Head % Rec1
101 =:Ind(Head).Ival

New Irec =:TmpLink % Rec2 - on Stack
102 =:Ind(TmpLink).Ival
TmpLink Append Head:Next

New Irec IN Heap =:TmpLink % Rec0
100 =:Ind(TmpLink).Ival
TmpLink Insert Head:Next

DumpList
% Rec0 Rec1 Rec2
% Dispose
Ind(Head).Next Remove Head:Next

New Irec IN Heap =:TmpLink % Rec3
FALSE =:Ind(TmpLink).BVal
TmpLink Insert Head:Next

New Irec IN Heap =:TmpLink % Rec4
FALSE =:Ind(TmpLink).BVal
TmpLink Append Head:Next

DumpList
% Rec3 Rec0 Rec2 Rec4

New Irec IN Heap =:TmpLink % Rec5
TRUE =:Ind(TmpLink).BVal
TmpLink Append Head:Next

Ind(Ind(Ind(Head).Next).Next).Next =:TmpLink.Next
TmpLink =:Ind(Ind(Ind(Head).Next).Next).Next
DumpList
% Rec3 Rec0 Rec2 Rec5 Rec4

ENDROUTINE
ENDMODULE
$EOF
```

---

Norsk Data Internal Use Only

---

## Page 87

# 4.8.5 Object Oriented Programming Examples

Object oriented programming is becoming available from the latest release of PLANC (the H-release). That is, now you can declare routines as part of your record declarations, and execute them using the dot notation. The routine then executes as if its body is enclosed in a USING-ENDUSING statement, where the USING-statement refers to the name of the enclosing record.

Object orientation can be summarized as
- encapsulation
- inheritance

Encapsulation in object oriented programming implies that the object (the RECORD in PLANC) contains not only data, but also routines that can act on the data that are defined in the record. The routine components can also use global variables, or variables and routine of other objects whose names are known.

If a PLANC record describes a bridge hand, it can contain a routine named "play" which acts on the other components of the record. And it can interact - "play with" - other records as well. But then it must name the other record explicitly before naming which of the other record's routine components it wants to activate.

There is a limitation on the routines - they cannot have invalues. (From the I-version of PLANC, this limitation will be removed.)

Inheritance means that you can define a data structure early in your program that has useful properties. Then, make new variants of the initial structure when you need to provide it with additional features. If need be, redefine component names in the variants.

For example, if you need to handle a lot of linked lists in your program, you can define the data structure of a list member early in the program, and give it attributes such as routines that insert it in and remove it from lists. Building on this, you can add specialized attributes later.

Norsk Data Internal Use Only

---

## Page 88

# PLANC Features and Development Tools
## Object Oriented Programming Examples

You can decide which record components are accessible from outside the record using the keyword `PUBLIC`. That is, you precede the declarations inside the record with a list of the entities you want to be accessible from the outside:

```
TYPE rec = RECORD
    PUBLIC aVariable, aRoutine
    INTEGER : aVariable, SecretVariable
    ROUTINE VOID, VOID : SecretRoutine
        % This routine is PRIVATE to this record.
    ENDROUTINE
    ROUTINE VOID, VOID : aRoutine
        % This routine can be accessed from
        % outside the record.
    ENDROUTINE
```

Records with member routines may be declared globally, locally, created by `New` or some other tailor-made allocation scheme. Static initialization of global records with member routines is done as if the routines inside them did not exist.

This is an example of how these features can be used with the H version of PLANC, which is released, and the forthcoming I version.

```
$IF Version = #H $THEN
MODULE ObjectExample

TYPE Date = RECORD
    INTEGER : DateDay
    INTEGER : DateMonth
    INTEGER : DateYear

    ROUTINE VOID, VOID (INTEGER, INTEGER, INTEGER) : Setdate(Day1, Month1, Year1)
        Day1 =: DateDay; Month1 =: DateMonth; Year1 =: DateYear
    ENDROUTINE

    ROUTINE VOID, VOID : PrintDate
        Output(1,'I4',DateYear)
        Output(1,'A','-')
        Output(1,'I1',DateMonth)
        Output(1,'A','-')
        Output(1,'I1',DateDay)
    ENDROUTINE

    ROUTINE VOID, INTEGER : Day
        DateDay RETURN
    ENDROUTINE

ENDRECORD

BYTES POINTER ARRAY: Months:= ( &

```

Norsk Data Internal Use Only

---

## Page 89

# PLANC Features and Development Tools

## Object-Oriented Programming Examples

```plaintext
Addir 'January', Addir 'February', Addir 'March',
Addir 'April', Addir 'May', Addir 'June',
Addir 'July', Addir 'August', Addir 'September',
Addir 'October', Addir 'November', Addir 'December'
```

### TYPE NicerDate = Date RECORD

#### ROUTINE VOID, VOID : PrintDate
```
Output(1, 'I', DateDay)
Output(1, 'A', '. ')
Output(1, 'A', Ind(Months(DateMonth-1)))
Output(1, 'A', ' ')
Output(1, 'I4', DateYear)
Output(1, 'A', '.')
```
ENDROUTINE

ENDRECORD

#### INTEGER ARRAY : Stack(0:1023)

#### PROGRAM : ObjectInvoke
```
INISTACK Stack
Date : XMas
NicerDate : ND

XMas.SetDate(24,12,1987)
Output(1, 'A', 'Christmas is ')
XMas.PrintDate

ND.SetDate(7,7,1967)
Output(1, 'A', '$ND was founded ')
ND.PrintDate
```
ENDROUTINE

ENDMODULE

### $SELECT

```
$ELSIF Version >= #I $THEN
MODULE ObjectExample
```

### TYPE Date = RECORD
```
INTEGER : DateDay
INTEGER : DateMonth
INTEGER : DateYear
```

#### ROUTINE VOID, VOID(INTEGER, INTEGER, INTEGER) : SetDate(Day1, Month1, Year1)
```
Day1 := DateDay; Month1 := DateMonth; Year1 := DateYear
```
ENDROUTINE

#### ROUTINE VOID, VOID : PrintDate
```
Output(1, 'I4', DateYear)
Output(1, 'A', '. ')
Output(1, 'I', DateMonth)
```

```plaintext
Norsk Data Internal Use Only
```

---

## Page 90

# PLANC Features and Development Tools
## Object Oriented Programming Examples

```
Output(1,'A','-')
Output(1,'I',DateDay)
ENDROUTINE

ROUTINE VOID,INTEGER : Day
   DateDay RETURN
ENDROUTINE

ROUTINE INTEGER,VOID : Day
   IF @ IN 0:28 THEN
      @ = ::DateDay
   ELSE
      % The Hard Way
      % ...
   ENDIF
ENDROUTINE

ENDRECORD

BYTES POINTER ARRAY: Months:= ( &
   #ddr 'January',#ddr 'February',#ddr 'March',
   #ddr 'April' ,#ddr 'May' ,#ddr 'June' ,
   #ddr 'July'  ,#ddr 'August' ,#ddr 'September' ,
   #ddr 'October',#ddr 'November',#ddr 'December' )

TYPE NicerDate = Date RECORD

ROUTINE VOID,VOID : PrintDate
   Output(1,'I',DateDay)
   Output(1,'A','.')
   Output(1,'A',Ind(Months(DateMonth-1)))
   Output(1,'A',' ')
   Output(1,'I4',DateYear)
   Output(1,'A','.')
ENDROUTINE

ENDRECORD

INTEGER ARRAY : Stack(0:1023)
PROGRAM : ObjectInvoke
   INITSTACK Stack
   Date : XMas
   NicerDate : ND

   XMas.SetDate(24,12,1987)
   Output(1,'A','$Christmas is ')
   XMas.PrintDate
USING XMas
   Output(1,'A',' or ')
   Day + 1 = ::Day
   PrintDate
ENDUSING
```

Norsk Data Internal Use Only

---

## Page 91

# PLANC Features and Development Tools
## Object Oriented Programming Examples

``` 
ND.SetDate(7,7,1967)
Output(1,'A','$ND was founded ')
ND.PrintDate

ENDROUTINE
ENDMODULE
$ENDIF
$EOF
```

In PLANC-I, you will be able to predefine routines inside a record when you want to use recursion or for other purposes. When you declare the routine later on, you must prefix the declaration with the record type, thus:

```
TYPE class = RECORD
  INTEGER : x
  ROUTINE VOID, VOID (INTEGER) : get?
ENDRECORD class

ROUTINE VOID, VOID (INTEGER) : class.get
  % ...
ENDROUTINE
```

The syntax checker built into the A-version of LED cannot be used with record types containing routines. Instead, you must use the new B version of LED where you can tie a TAB to one of the LED regions. (If you want information about the LED-B, Börje Sanremalm a.k.a. BRSA is the one to ask.)

For the time being, the Symbolic Debugger does not understand the routine-as-component syntax. That means that you must set breaks using line numbers instead of using `<record type>.<routine name>`. It is impossible to INVOKE component routines, too.

Norsk Data Internal Use Only

---

## Page 92

# PLANC Features and Development Tools

## A Demo Run

### 4.8.6 A Demo Run

This is how you load the programs above, and the output that you get when you run the two variants of the program.

```
@PLANC-500

CONSTANT Version=#I

COMPILE object-date,,object-date

@LINKAGE-LOADER

SET-DOMAIN object-date

OPEN-SEGMENT object-date,,,

LOAD-SEGMENT object-date

LOAD-SEGMENT (LIBRARIES)PLANC-UTILLIB

LOAD-SEGMENT (LIBRARIES)MON-CALL-LIB

LOAD-SEGMENT (LIBRARIES)PLANC-LIB

LIST-ENTRIES-UNDEFINED

EXIT

@ND-500 object-date

Christmas is 1987-12-24

ND was founded 7. July 1967.

@PLANC-500

CONSTANT Version=#I

COMPILE object-date,,object-date

@LINKAGE-LOADER

SET-DOMAIN object-date

OPEN-SEGMENT object-date,,,

LOAD-SEGMENT object-date

LOAD-SEGMENT (LIBRARIES)PLANC-UTILLIB

LOAD-SEGMENT (LIBRARIES)MON-CALL-LIB

LOAD-SEGMENT (LIBRARIES)PLANC-LIB

LIST-ENTRIES-UNDEFINED

EXIT

@ND-500 object-date
```

Norsk Data Internal Use Only

---

## Page 93

# PLANC Features and Development Tools

*Christmas is 1987-12-24 or 1987-12-25*  
*ND was founded 7. July 1967.*

---

## 4.8.7 Record component inheritance - a lengthy exposition

Here is a somewhat more lengthy example, showing inheritance through simulation of a car wash.

```
%===============================================================%
%                       PLANC washes cars.                      %
%                       ==================                      %
%                                                               %
%  This module displays how routines as part of records in      %
%  PLANC can be used together with variant records. The         %
%  example simulates a carwash, where cars arrive at random     %
%  and queue up for their turn.                                 %
%                                                               %
%===============================================================%
$DEBUG on
CONSTANT Infinity = 1.0E75
MODULE timexis
IMPORT ( ROUTINE VOID, VOID : utRandom )
IMPORT ( ROUTINE VOID, REAL : utRnd )
INTEGER ARRAY : stack(0:5127), space(0:5127)

%===============================================================%
%  Record components will be inherited by variants. But         %
%  they may be redefined by the variants. In order to find      %
%  the right version of a component that has been redefined,    %
%  we must keep track of the "genes" of the variants.           %
%===============================================================%
TYPE Gene = ENUMERATION (LinkageRec, LinkRec, HeadRec, 
                         ProcessRec, CarWashRec, MakeCustRec)

%===============================================================%
%  Linkage is in fact the prototype record for all later        %
%  records types. That is, all record types can be inserted     %
%  in a doubly linked list.                                     %
%===============================================================%
TYPE Linkage = RECORD
    Gene : RecordType
    Linkage POINTER : Previous, Next
ENDRECORD

                   Norsk Data Internal Use Only
```

---

## Page 94

# PLAN-C Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

```
%===============================================================%
% Head is a recordtype that is used to head all linked          %
% lists. It has three ROUTINE components:                       %
% - One which initializes an empty list (i.e., makes the        %
%   list pointers point to the head itself) and sets the        %
%   "gene" to HeadRec.                                          %
% - A ROUTINE which returns the number of elements in the       %
%   queue.                                                      %
% - Another that tells you if there is anything in the          %
%   queue or not.                                               %
%===============================================================%
```

### TYPE Head = Linkage RECORD

#### ROUTINE VOID, VOID : initiateHead
```
   THISRECORD =: Next =: Previous
   HeadRec =: RecordType
ENDROUTINE
```

#### ROUTINE VOID, INTEGER : Cardinal

- **INTEGER** : i
- **Linkage POINTER** : currentLinkage

```
   0 =: i
   Next =: CurrentLinkage
   DO
      WHILE CurrentLinkage.RecordType >< HeadRec
         CurrentLinkage.Next =: CurrentLinkage
         ++ i
      ENDDO
   i RETURN
ENDROUTINE
```

#### ROUTINE VOID, BOOLEAN : Empty

```
   (Next = THISRECORD) RETURN
ENDROUTINE
ENDRECORD
```

```
%===============================================================%
% Link is the basic type for list members. It contains          %
% ROUTINES for                                                  %
% - being removed from list                                     %
% - insertion first or last in list                             %
% - insertion before or after other list members                %
%===============================================================%
```

### TYPE Link = Linkage RECORD

#### ROUTINE VOID, VOID : Out

```
   IF (Next = NIL) XOR (Previous = NIL) THEN
      Output(1,'a','%Pointers screwed up')
   ELSIF Next >< NIL THEN
      Next =: Previous.Next
      Previous =: Next.Previous
      NIL =: Previous =: Next
   ENDIF
ENDROUTINE
```

```
Norsk Data Internal Use Only
```

---

## Page 95

# PLANC Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

### Routine: Follow

```plaintext
ROUTINE VOID, VOID (Link POINTER) : follow(ThisLink)
   Out
   ThisLink.Next =: Next
   ThisLink =: Previous
   THISRECORD =: ThisLink.Next =: Next.Previous
ENDROUTINE
```

### Routine: Precede

```plaintext
ROUTINE VOID, VOID (Link POINTER) : precede(ThisLink)
   Out
   ThisLink.Previous =: Previous
   ThisLink =: Next
   THISRECORD =: ThisLink.Previous =: Previous.Next
ENDROUTINE
```

### Routine: IntoStart

```plaintext
ROUTINE VOID, VOID (Head) : intoStart(ThisHead)
   Out
   ThisHead.Next =: Next
   Addr(ThisHead) =: Previous
   THISRECORD =: Next.Previous =: ThisHead.Next
ENDROUTINE
```

### Routine: IntoEnd

```plaintext
ROUTINE VOID, VOID (Head) : intoEnd(ThisHead)
   Out
   ThisHead.Previous =: Previous
   Addr(ThisHead) =: Next
   THISRECORD =: Previous.Next =: ThisHead.Previous
ENDROUTINE
```

### TimeHead Record

```plaintext
%====================================================%
% A special variant of Head is needed to keep events in. %
% The variant contains a variable showing the last time %
% an event occurred. %
%====================================================%
TYPE TimeHead = Head RECORD
   REAL : Time
ENDRECORD
```

### List of Events

```plaintext
%====================================================%
% The list of events %
%====================================================%

TimeHead : EventQueue
```

```plaintext
%====================================================%
% Here, a variant of Link is made for events that can %
% occur. The variant is called Process, and it has a real %
% variable that shows when it is due to be activated. %
% Activation is done by the ROUTINE called Activate. %
% If there is nothing to do for the process, it can be %
% Passivated, i.e., activated at an infinitely late time. %
% A test for whether the routine is passive or not is %
% included, and the process has a Body telling you when %
%====================================================%
```

Norsk Data Internal Use Only

---

## Page 96

# PLANC Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

% activation takes place.  
%============================================================%

### TYPE Process = Link RECORD

- **REAL** : ActivationTime

```plaintext
ROUTINE VOID, VOID (REAL) : Activate (NewTime)
    Link POINTER : l
    NewTime := ActivationTime
    IF ActivationTime >= EventQueue.Time THEN
        IF (EventQueue.Empty) &
            OR (ActivationTime < EventQueue.Next.ActivationTime) THEN
            IntoStart(EventQueue)
        ELSIF (ActivationTime = Infinity) OR &
            (ActivationTime >= EventQueue.Previous.ActivationTime) &
        THEN
            IntoEnd(EventQueue)
        ELSIF THISRECORD = EventQueue.Next THEN
            % Nothing
        ELSE
            EventQueue.Next := l
%============================================================%
% This DO-loop will activate the current %
% process after other processes with the %
% same activation time. You may want to change %
% this strategy. %
%============================================================%
        DO
            WHILE (l.ActivationTime >= ActivationTime) &
                AND (l.RecordType >< HeadRec)
                l.Next := l
            EXITWHILE
            Precede(l)
        ENDDO
    ENDIF
    ELSE
        Out
    ENDIF
ENDROUTINE
```

### ROUTINE VOID, VOID : Passivate

```
Activate(Infinity)
```

### ENDROUTINE

### ROUTINE VOID, BOOLEAN : Passive

```plaintext
    IF ActivationTime = Infinity THEN
        TRUE RETURN
    ELSE
        FALSE RETURN
    ENDIF
ENDROUTINE
```

### ROUTINE VOID, VOID ; Body

```plaintext
    Output(l,'a','Sprocess activated at ')
    Output(l,'f10.3',ActivationTime)
```

[Norsk Data Internal Use Only]

---

## Page 97

# PLANC Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

### Making a Queue of Cars Waiting to be Washed

```plaintext
Head : WaitingCars
```

Here comes a car definition. The only property the car needs in this context is a variable telling how long it takes to wash it.

```plaintext
TYPE Car = LINK RECORD
    REAL : TimeForWashingTheCar
ENDRECORD
```

Now, we define what the carwash is going to do. This is done in a redefined version of the ROUTINE called Body, which passivates the process if the queue of cars is empty, or takes the first car out of the queue to wash it and reactivates itself after the car has been done to look for more cars in the queue. By the way, the carwash will only be open between 0800 a.m. and 1600 p.m.

```plaintext
TYPE CarWash = PROCESS RECORD
    ROUTINE VOID, VOID : Body
    Car POINTER : NextCar

    IF (NOT WaitingCars.Empty) & 
       (EventQueue.Time < 16.0) THEN
        WaitingCars.Next := NextCar
        NextCar.Out
        Activate(ActivationTime + NextCar.TimeForWashingTheCar)
        Output(1, 'a', 'SW')
        Output(1, 'i2', WaitingCars.Cardinal)
        Output(1, 'f7.3', EventQueue.Time)
    ELSE
        Passivate
    ENDIF
ENDROUTINE
ENDRECORD
```

CarWash : TheCarWash  
REAL : CustomerArrivalTime := 8.00

This process puts new cars into the queue at random intervals. If the carwash is passive, it also wakes it up so that the car that just arrived can be washed.

```plaintext
TYPE MakeCustomers = PROCESS RECORD
```

Norsk Data Internal Use Only

---

## Page 98

# PLANC Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

### ROUTINE VOID, VOID : Body
```
Car POINTER : NewCar

IF CustomerArrivalTime < 16.00 THEN
    CustomerArrivalTime + 1.0 / (0.1 + utRnd * 6.0) & 
    ::= CustomerArrivalTime

    NewCar IN space ::= NewCar
    0.4 * utRnd ::= Indi(NewCar).TimeForWashingTheCar
    NewCar.IntoEnd(WaitingCars)
    Output(1,'a\'%c')
    Output(1,'i2',WaitingCars.Cardinal)
    Output(1,'f7.3',EventQueue.Time)
    
    Activate(CustomerArrivalTime)
    IF TheCarWash.Passive THEN
        TheCarWash.Activate(CustomerArrivalTime)
    ENDIF
ELSE
    Passivate
ENDIF

ENDROUTINE
ENDRECORD

MakeCustomers : TodaysCustomers
Process POINTER : CurrentProcess
```

### PROGRAM : main
```
INITSTACK stack
%========================================================%
% Initiating the random number generator and setting      %
% the right genes to the carwash and the customer-making  %
% process.                                                %
%========================================================%
utRandom
EventQueue.initiatehead
WaitingCars.InitiateHead

CarWashRec ::= TheCarWash.RecordType
MakeCustRec ::= TodaysCustomers.RecordType

%========================================================%
% The carwash begins the day by looking for a customer,   %
% but a random time interval will pass before one         %
% actually arrives.                                       %
%========================================================%
TheCarWash.Activate(CustomerArrivalTime)
CustomerArrivalTime + 1.0 / (0.1 + utRnd * 6.0) & 
   ::= CustomerArrivalTime

TodaysCustomers.Activate(CustomerArrivalTime)
%========================================================%
% So this is where the work actually begins - a loop      %
% that will continue until 1600 when the carwash          %
% closes, or there are no active processes in the         %
% eventqueue.                                             %
%========================================================%
DO

EventQueue.Next ::= CurrentProcess

Norsk Data Internal Use Only
```

---

## Page 99

# PLAN C Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

```
WHILE (CurrentProcess.RecordType >< HeadRec ) &
      (NOT CurrentProcess.Passive)
USING Ind(CurrentProcess)
    ActivationTime =: EventQueue.Time
%===============================================%
% The "genes" are used to determine which process %
% body is going to be activated.                  %
%===============================================%
    IF RecordType = CarWashRec THEN 
        TheCarWash.Body
    ELSIF RecordType = MakeCustRec THEN
        TodaysCustomers.Body
    ELSIF RecordType = ProcessRec THEN
        CurrentProcess.Body
    ENDIF
ENDUSING
EXITWHILE
Output(I,'a','SDone for today. ')
ENDDO
ENDROUTINE
ENDMODULE
$EOF
```

This example shows what a run of the program above looks like.

```
@ND time

C 1  8.277
C 2  9.472
W 1  9.472
C 2  9.770
W 1  9.868
W 0 10.050
C 1 10.080
W 0 10.088
C 1 10.435
C 2 10.620
W 1 10.620
W 0 10.905
C 1 10.932
C 2 11.853
W 1 11.853
W 0 11.932
C 1 12.035
W 0 12.075
C 1 14.413
C 2 14.873
W 1 14.873
W 0 15.213
```

Norsk Data Internal Use Only

---

## Page 100

# PLANC Features and Development Tools

## Record Component Inheritance - A Lengthy Exposition

---

Done for today.

---

## 4.9 Routine Pointers and Indirect Routine Invocation

In PLANC, routine declarations are data structures. One implication of this is that you can declare POINTERs that point to routines; another is that routines can be called simply by using the construction `Ind <routine pointer name>`. This gives an alternative to `CASE` statements where each case causes execution of a routine.

The following example shows how a traffic light can be simulated using routine pointers.

```plaintext
MODULE traffic
CONSTANT cr = 15B, lf = 12B
IMPORT (ROUTINE VOID, VOID (BYTES)) : utDisplay
BYTES : CrLf[0:1] := (cr, lf)
INTEGER ARRAY : stack (0:127)
TYPE colours = ENUMERATION (red, yellow, green)
BOOLEAN ARRAY : lit[red:green] := (FALSE)
BYTES ARRAY : light[red:green,0:6] := ('red ', 'yellow ', 'green ')
ROUTINE VOID, VOID : WriteState
  colours : c
  FOR c IN red:green DO
    IF lit(c) THEN utDisplay(light(c)) ENDIF
  ENDFOR
  utDisplay CrLf
ENDROUTINE
TYPE vv = ROUTINE VOID, VOID
vv POINTER : next
vv : SwitchToRed ?
vv : SwitchToYellow ?
vv : SwitchToGreen ?
ROUTINE VOID, VOID : SwitchToRed
  TRUE =: lit(red)
  FALSE =: lit(yellow)

%============================================================%
%                          I M P O R T A N T:                %
% By convention, you must give the routine name              %
% w i t h o u t enclosing parentheses if you want the         %
% address of the routine, w i t h if you want the            %
% address of its out-value.                                  %
%============================================================%
Addr SwitchToYellow =: next
```

Norsk Data Internal Use Only

---

## Page 101

# PLANC Features and Development Tools

## Routine Pointers and Indirect Routine Invocation

```
WriteState
ENDROUTINE
ROUTINE VOID, VOID : SwitchToYellow
  TRUE :=: lit(yellow)
  IF lit(red) THEN
    &dir SwitchToGreen =: next
  ELSE
    FALSE :=: lit(green)
    &dir SwitchToRed =: next
  ENDIF
  WriteState
ENDROUTINE
ROUTINE VOID, VOID : SwitchToGreen
  TRUE :=: lit(green)
  FALSE :=: lit(yellow) :=: lit(red)
  &dir SwitchToYellow =: next
  WriteState
ENDROUTINE
PROGRAM : main
INTEGER : i
INSTACK stack
&dir SwitchToRed =: next
0 :=: i
DO
  % This is how the routines are invoked via pointers.
  !nd next
  WHILE ++i < 20
ENDDO
ENDROUTINE
ENDMODULE
$EOF
```

---

## Using Routines and RETURN as a Control Structure

Control structures with many nested IF, DO, FOR, CASE, ON statements etc. can become very complicated. Getting out of the control structure usually entails exiting from each of the nested control structures in turn, or possibly use of GO statements.

An alternative method will be to pack the structure into a ROUTINE, and to use the RETURN statement to get out of the control structure. Using this method, you will always return to the line immediately after where the ROUTINE containing the structure was called.

---

Norsk Data Internal Use Only

---

## Page 102

# PLAN C Features and Development Tools

## Using Routines and RETURN as a Control Structure

One possible problem with this will be to identify precisely what RETURN statement caused exit from the control structure while debugging. If you use the Symbolic Debugger's BREAK-RETURN command, you get a break on the line where the routine was called. An alternative here is to use the Debugger command BREAK-EXIT, which breaks on the line in the ROUTINE where the RETURN statement being executed is.

```
MODULE sin
$INCLUDE (lib)pla-ut:defs
$INCLUDE (lib)pla-ut:incl

INTEGER ARRAY : stack (0:255)
BYTES POINTER ARRAY : PlanCIdent := ( &

    Addpr '+'
    Addpr 'CALL-HIERARCHY'
    Addpr 'COMPILE'
    Addpr 'CONSTANT'
    Addpr 'CROSS-REFERENCE'
    Addpr 'DATE'
    Addpr 'DEBUG-MODE'
    Addpr 'DEFINE'
    Addpr 'EJECT'
    Addpr 'ELSE'
    Addpr 'ELSIF'
    Addpr 'ENDIF'
    Addpr 'ENDMACRO'
    Addpr 'FOR'
    Addpr 'EXIT'
    Addpr 'HELP'
    Addpr 'IF'
    Addpr 'INCLUDE'
    Addpr 'KILL'
    Addpr 'LIBRARY-MODE'
    Addpr 'LINE-BIAS'
    Addpr 'LINKAGE-REFERENCE'
    Addpr 'LIST'
    Addpr 'LOAD'
    Addpr 'MACRO'
    Addpr 'MESSAGE-TO-TERMINAL'
    Addpr 'MODULE-LIBRARY-MODE'
    Addpr 'NDLGO-EXTEND'
    Addpr 'OPTION'
    Addpr 'PROG-FILE'
    Addpr 'REAL-PRECISION'
    Addpr 'SEPARATE-DATA'
    Addpr 'TARGET-MACHINE'
    Addpr 'THEN'
    )

TYPE KindOfMatch = ENUMERATION (NoMatch, Match, Ambiguous)

ROUTINE VOID, KindOfMatch (BYTES : MatchString; &
    BYTES POINTER ARRAY : TheArr) : SimWatch

Norsk Data Internal Use Only
```

---

## Page 103

# PLANC Features and Development Tools
## Using Routines and RETURN as a Control Structure

### KindOfMatch: MatchStatus

```plaintext
INTEGER: ItsComponent, InMin, TargetMin, InMax, &
   TargetMax, InLen, TargetLen
ROUTINE BYTES, BOOLEAN (BYTES : Target) : Matches
   INTEGER : i; j
   TargetMin-1 =: j
   FOR i IN @ DO
      IF @(i) = Target(++j) THEN
      ELSIF @(i) = #- THEN
         DO WHILE (++j) < (TargetMax) AND (Target(j) >< #-)
         ENDDO
         IF j-TargetMax THEN
            FALSE RETURN
         ENDIF
      ELSE
         FALSE RETURN
      ENDIF
   ENDFOR
   TRUE RETURN
ENDROUTINE
```

### NoMatch: MatchStatus

utUpperCase MatchString

```plaintext
(MaxIndex(MatchString, 1) =: InMax) - &
   (MinIndex(MatchString, 1) =: InMin) * 1 =: InLen
FOR ItsComponent IN TheArr DO
   (MaxIndex(Ind(TheArr(ItsComponent)), 1) =: TargetMax) - &
   (MinIndex(Ind(TheArr(ItsComponent)), 1) =: TargetMin) &
   + 1 =: TargetLen
   IF (InLen < TargetLen) AND &
      (matchString Matches Ind(TheArr(ItsComponent))) THEN
      IF MatchStatus = Match THEN
         Ambiguous RETURN
      ELSIF MatchStatus = NoMatch THEN
         Match =: MatchStatus
      ENDIF
   ELSIF (InLen = TargetLen) AND &
      (Ind(TheArr(ItsComponent)) = MatchString) THEN
      Match RETURN
   ENDIF
ENDFOR
MatchStatus RETURN
ENDROUTINE
```

### BYTES

```plaintext
BytesIn (0:31), CrLf := (15B, 12B), BytesPos (0:31)
INTEGER : BytesCount
KindOfMatch : Match
PROGRAM : main
   INITSTACK stack
   ('0123456789012345678901345678901') =: BytesPos (0:31)
   DO
      (408B) =: BytesIn
      utDisplay 'String: '
      Input(1, 'a', BytesIn) =: BytesCount

Norsk Data Internal Use Only
```

---

## Page 104

# PLANC Features and Development Tools

## Using Routines and RETURN as a Control Structure

```
utDisplay CrLf
WHILE BytesCount > 0
    SimMatch (BytesIn(0:BytesCount-1), PlancIdent) =: ?Match
    CASE ?Match
    INCASE NoMatch:
        utDisplay 'No match.'
    INCASE Match
        utDisplay 'Matches.'
    INCASE Ambiguous
        utDisplay 'Ambiguous.'
    ENDCASE
    utDisplay CrLf
ENDDO
ENDROUTINE
ENDMODULE
SDOF
```

## 4.11 Mixing PLANC and C

A special problem with C is that it insists on loading the main program entry from its own library. This means that in programs where C code is used, and which include code written in other languages, there must be a `main()` function which calls code written in PLANC and other languages.

### 4.11.1 A C `main()` Calling PLANC Calling C Demo

The following example shows how you mix PLANC and C on an INTEL μP. That is, it uses the routine modifiers NATIVE or C, and it does an Initstack on stack that will be used by both the following PLANC and C routines. Neither the modifier nor the special Initstack are mandatory on ND-100/ND-500(0) CPUs.

```c
main()
{
    int plancstatus, param;
    printf("C-main is here\n");
    param= 1;
    plancstatus= planc(param);
    printf("C-main is back\n");
    if (plancstatus!=0) printf("Error In Planc Routine!\n");
}
```

Norsk Data Internal Use Only

---

## Page 105

# PLANC Features and Development Tools
## A C main() Calling PLANC Calling C Demo

```c
/************************************************************************/
int func(i,str) /* Known to Loader as _func (conf. your c doc.) */
int i;
char str[];
{
    printf("C-func is here\n");
    if ((i!=2) || (str[0]!='P')) {
        return(-1);
    }
    else {
        printf("A string from Planc: '%s'\n",str);
        return(0);
    }
}
```

Here is the PLANC routine which the `main()` calls. It calls the C function `func(i, str)` in the C file. Note the ALIAS'ing in the PLANC code:

```plaintext
MODULE PlancRoutine

EXPORT Planc ALIAS 'planc'
IMPORT (ROUTINE NATIVE VOID,INTEGER(INTEGER,BYTE POINTER):CFunc
ALIAS '_func')

BYTES :: text(100:115);= 'Planc'
INTEGER ARRAY : Stack(0:1023)
ROUTINE NATIVE VOID,INTEGER(INTEGER:Param) : Planc
    INITSTACK Stack
    INTEGER : i
    INTEGER : CStatus
    BYTE POINTER : Bp

    Output(1,'A','Planc is here')
    IF Param >< i THEN Output(1,'A','$It Didn't Work') ENDIF
    2 =:: i
    0 =::Text(MaxIndex(Text,1)) % String Terminator
    %dr(Text(MinIndex(Text,1))) ;=Bp & Points to 1. byte in Text

    CFunc(i,Bp) =::CStatus % Invoking _func
    Output(1,'A','Planc is back.$')
    IF CStatus-1 THEN Output(1,'A','*** Didn't work') ENDIF

    0 RETURN
ENDROUTINE Planc
ENDMODULE
$EOF
```

Norsk Data Internal Use Only

---

## Page 106

# PLANC Features and Development Tools

## A C main() Calling PLANC Calling C Demo

Here you see what this looks like when it is run:

```
C-main is here
PLANC is here
C-func is here
A string from PLANC: 'PLANC'
PLANC is back
C-main is back
```

---

## 4.12 Portable Programming in PLANC

The following constructs may cause problems when a PLANC program is ported to another CPU:

### FORCE

If you **FORCE** a **POINTER** to an **INTEGER** on the ND-500 you will get into trouble if you port to the Intel-286 CPU, where the default **INTEGER** size will be 2 bytes and a pointer will have four bytes. This will cause an error from the PLANC compiler. In addition you usually force a pointer to an integer to do some arithmetics on the pointer and this may cause run-time errors, because as a consequence of the segmentation, you can't calculate directly with an Intel-286 pointer.

### EQUIVALENCE

This may cause a lot of problems because it depends on the size, alignment and representation of the variables involved. A common reason for using equivalence is to access the **Minindex** and **Maxindex** of a single dimension **ARRAY POINTER**. A portable way of doing this if you want new values for the **Minindex** and **Maxindex** of **byp** is:

```
%ddr([nd(byp]([new_min:new_max])) =: byp
```

If you want direct access to the **Minindex** and **Maxindex** you can still do it portable thus:

```
BYTE POINTER : paddr % Leave us
INTEGER : pmin, pmax % together!
BYTES POINTER : byp = paddr
```

### Monitor Calls

and other operating-system dependent constructs should be avoided or collected in a separate module containing general routines making a logical abstract interface to the environment that will be easy to change later.

Norsk Data Internal Use Only

---

## Page 107

```
   ___        •—        +V
   SIGNAL          SUPPLY

   PERMITTED
  WORK ZONE

   ___

 BI-DIRECTIONAL                   BI-DIRECTIONAL
   COUPLER                           COUPLER

                                            _____             _____
  **Time                                 High       ————      High**
 *Signal         DETECTION            Side                   Side
 **Long            TIME
  **Short       ____          ————       ____         —————-    ____                                      ___
 ``` 

### Overview

The signal flow diagram as above illustrates connectivity between modules within a high-speed communication system.

### Table of Parameters

| Parameter          | Value       |
|--------------------|-------------|
| Supply Voltage (+V)| 5V          |
| Time Signal        | Adjustable  |
| Detection Time     | Variable    |
| High Side          | Enabled     |

---

## Page 108

# PLAN C Features and Development Tools

## Portable Programming in PLAN C

### $* INLINE ASSEMBLY

The same as `Monitor_calls`

### Access to "external" pointers

An external pointer is a pointer you get from or give to a part of your system that may run on a different process, CPU, or machine. Such pointers often have to be converted some way or another, or the objects they point to need to be converted. Consequently, you should make a few general routines for accessing this kind of pointers.

### Size(INTEGER)

All integer variables that may contain large numbers (abs > 32767) or that may be sensitive to the way they are used in expressions should be declared with an explicit range. You may use `INTEGER RANGE(min:max)` or `BYTE`, `INTEGER1`, ... A new feature in the I version of PLAN C is that you are now able to declare unsigned integer variables:

```
INTEGER2 UNSIGNED : u16
```

### RECORD PACKED

Problems may arise if some components of a record must align their first bit on fixed displacements from the start of the record. This is common when porting to new CPUs, or when sending records in messages between different CPUs. To be optimal and as a consequence of restrictions on the different CPUs, the different PLAN C versions differ much in how record elements are packed, and there is no simple rule as to how this is done. To help this situation, the `MOD` and `Bit_position` constructs can be used. You may declare an element in a record in the following way:

```
INTEGER2 : length MOD 1
```

The `length` variable will be put on the next byte after the previous variable in the record. If you put `MOD 2` after the variable, the variable will be put on the next displacement that is a multiple of 2 relative to the start of the record and so on.

Check that you have got the displacements you want with the construct `Bit_position(<record element>)`.

If you want to check that the record element `length` has displacement 7 relative to the start of the record you can do it like this:

---

Norsk Data Internal Use Only

---

## Page 109

# PLAN C Features and Development Tools

Portable Programming in PLAN C

## Compile-Time Checks

```
IF BIT position(length)/8 × 7 THEN
    $MESSAGE Error in RECORD layout !!!
    $EXIT$ Terminate compilation
ENDIF
```

If you declare "sensitive" records in an $INCLUDE-file with this compile-time check in it, and use it in all dependent systems, you should be fairly certain of getting no problems.

Another construct that often causes problems inside packed records is the equivalence operator =. This is not implemented the same way for packed records as unpacked records. In unpacked records, the alignment is on the most significant bit in the equivalenced variables, whereas in packed records, it causes the least significant bits in the two variables to be aligned.

There now is a new equivalence operator, >=, that aligns on the most significant bit both in packed and unpacked records.

## File Names

File names differ in layout on different operating systems, and therefore should be used carefully. In the Open statement, it is useful to split name and extension. Then you don't have to worry about having a : or a . to separate them. Example:

```
Open(fno, 'ACCESS', 'NAME', 'EXT')
```

Also the access and the contents of the files you operate on may differ on different operating systems. You should be aware of this when you write your program. If you port a program scanning text files from Sintran-III to Unix and your program scans to CR ( = 15B) to find the end of a line. Then this will work badly on Unix because there the lines are terminated with a LF ( = 128) only.

## Calls To/From Other Languages

If you want to call or get calls from other languages you have to verify in each case that this is possible and check how the parameters have to look. For example, if you want to call a routine in C with a BYTES as parameter, you can't just put the BYTES in the parameter list because C does not have ARRAY POINTERS like PLAN C. What you have to do is to split the BYTES into a BYTE POINTER and an INTEGER containing the address of the start and the length of the BYTES. If the BYTES is b then you call the C-routine cc like this:

---

Norsk Data Internal Use Only

---

## Page 110

# Index List

| Index term                                              | Reference     |
|---------------------------------------------------------|---------------|
| $* inline assembly                                      | 99            |
| // concatenation operator                               | 54            |
| abbreviation list                                       | 30            |
| abbreviations and acronyms                              | 29            |
| acronyms and abbreviations                              | 29            |
| ADA notation for integers                               | 55            |
| Automake                                                | 58            |
| automatic maintenance of IMPORTs/EXPORTs                | 46            |
| BCD arithmetic and numeric edit                         | 52            |
| binary search                                           | 52            |
| buffered I/O                                            | 52            |
| C and NATIVE routine modifiers                          | 58, 69, 97    |
| cache and locality                                      | 9             |
| cache on the ND-5000                                    | 10            |
| cache, use of                                           | 9             |
| capacity of comms media                                 | 8             |
| characters and underscores                              | 29            |
| code standard                                           | 26            |
| columns in SIBAS                                        | 17            |
| combined calls in SIBAS                                 | 19            |
| comments                                                | 28            |
| comments, nested (%{%{%} %})                           | 55            |
| communication in ND                                     | 13            |
| communication media, capacity of                        | 8             |
| compiler command GENERATE-IMPORTS                       | 39, 46        |
| compiler command GET-VALUE                              | 39            |
| compiler command LINE                                   | 40            |
| compiler command LONG-NAMES                             | 40            |
| compiler command PRESENT                                | 39            |
| compiler command SELECT                                 | 40            |
| compiler commands                                       | 39            |
| compress and decompress routines                        | 16            |
| concatenation operator //                               | 54            |
| constant names                                          | 29            |
| constant, strings as                                    | 54            |
| constants and types                                     | 27            |
| conversion routines (numeric to/from BYTES string)      | 52            |
| coroutines                                              | 52            |
| current date and time as string                         | 53            |
| data compression                                        | 16            |
| data sizes in bytes                                     | 68            |
| data structures                                         | 27            |
| data structures in modules                              | 31            |
| data/runtime organization on the ND-500(0)              | 67            |
| database redefinition in SIBAS                          | 20            |
| date and time as string                                 | 53            |
| default routine parameter access                        | 66            |

Norsk Data Internal Use Only

---

## Page 111

# PLANC Features and Development Tools

## Portable Programming in PLANC

```
cc(Addr(b(Minindex(b))),Size(b))
```

(Note: *Minindex(b)* without the ,1 in one-dimensional arrays is allowed in the I-version of PLANC.)

## Some General Comments

All constructs like those mentioned above that may differ on different machines should be put in a separate module containing general routines doing the operations that are machine dependent. That way you know in advance where to do the changes when you port the system and you get a minimum of changes that need to be done. Both in the case of machine dependent constructs and other operations that are tricky, it is useful to make a general routine doing it and use this everywhere. That way you will ease the maintenance of your system. Declarations like the *RECORD PACKED* described above that is needed by other parts of your system should be put in an $INCLUDE-file and included in all the parts. That way you only have to make changes one place if something needs to be changed. Making general routines that you use everywhere, keeping declarations needed many places in include-files and giving names to all constants in your system will ease both porting and general maintenance of your system.

Making $INCLUDE-files for the purpose of having declarations needed in many places in one place and for importing all routines from a system is useful, but one should not put everything in $INCLUDE-files. If you have too many of your definitions in $INCLUDE-files, you will get problems finding them later on.

When you compile your PLANC program on a new machine you may get some warnings that you didn’t get before. Do not ignore these. If you try to force a *POINTER* to an *INTEGER4* on the ND-500 CPU everything will work fine, but if you port this program to the ND-100 CPU you will get the warning:

```
Illegal data-element to be converted
```

If you ignore this warning your program probably will fail and the error will be very difficult to find by debugging. In general, you should never ignore warnings from the PLANC compiler. Often they are fatal and will cause errors that are difficult to find.

---

Norsk Data Internal Use Only

---

## Page 112

## Index

| Index Term                                      | Reference  |
|-------------------------------------------------|------------|
| display string                                  | 53         |
| endroutine with name                            | 55         |
| EQUIVALENCE                                     | 99         |
| errcode                                         | 33         |
| errreturn                                       | 33, 68     |
| error return mechanism                          | 68         |
| exception handlers                              | 69         |
| file as segment                                 | 7          |
| file as segment and the swapper                 | 11         |
| file names                                      | 101        |
| File-as-segment initiating                      | 52         |
| fixing programs in memory                       | 12         |
| FORCE                                           | 99         |
| fortran character in standard routines          | 33         |
| GENERATE-IMPORTS compiler command               | 39, 46     |
| generate-imports PLANC command                  | 31         |
| GET-VALUE compiler command                      | 39         |
| global data structures                          | 27         |
| group columns in SIBAS                          | 17         |
| histogram of code address usage                 | 58         |
| I/O monitor call overhead                       | 10         |
| identifiers, bytes in                           | 54         |
| import/export in modules                        | 30         |
| IMPORTS/EXPORTS, automatic maintenance of       | 46         |
| include files                                   | 31, 46     |
| indexes in SIBAS                                | 18         |
| indirect routine invocation, routine pointers and | 93        |
| inline assembly                                 | 99         |
| integers in ADA notation                        | 55         |
| integers, underscores in                        | 95         |
| language editor (LED)                           | 27, 58     |
| LED and the Symbolic Debugger                   | 27, 58     |
| LED language editor                             | 27, 58     |
| libraries                                       | 33         |
| LINE compiler command                           | 40         |
| linkage-loader and SELECT                       | 40         |
| list of abbreviations                           | 30         |
| locality and cache hits                         | 9          |
| locality in operating systems                   | 9          |
| locality, definition                            | 9          |
| LONG-NAMES compiler command                     | 40         |
| LZW algorithm                                   | 16         |
| MAINSTART routine modifier                      | 59         |
| maintainability of programs                     | 27         |
| massive recompilations                          | 40         |
| Maxindex/Minindex                               | 53         |
| message sizes                                   | 15         |
| Minindex/Maxindex                               | 53         |

Norsk Data Internal Use Only

---

## Page 113

# Index

| Index Term                                      | Reference |
|------------------------------------------------|-----------|
| Mixing PLANC and C                             | 97        |
| modules                                        | 30        |
| modules and import/export                      | 30        |
| monitor call logging                           | 58        |
| monitor calls and overhead                     | 10        |
| monitor calls                                  | 99        |
| name of routines, variables, constants and types | 29       |
| naming conventions                             | 28        |
| NATIVE and C routine modifiers                 | 58, 69, 97|
| ND-100 to PIOC task-to-task messaging          | 14        |
| ND-500(0) swapper                              | 11        |
| ND-5000 cache                                  | 10        |
| ND-5000 nucleus task-to-task message system    | 14        |
| ND-5000 word alignment                         | 9         |
| ND-Linker and SELECT                           | 40        |
| nested comments, {% %}                         | 55        |
| nested modules                                 | 31        |
| nested routines                                | 32        |
| new types                                      | 53        |
| nucleus task-to-task message system            | 14        |
| object-oriented programming examples           | 80        |
| object-oriented programming in PLANC           | 51        |
| ON ROUTINEERROR exception handlers             | 69        |
| optimal sorting                                | 21        |
| OS-files in SIBAS                              | 17        |
| overhead of monitor calls                      | 10        |
| overhead of page faults                        | 11        |
| packed record                                  | 100       |
| packing                                        | 61        |
| page fault overhead                            | 11        |
| parameter access in routines, default          | 66        |
| parameter transfer                             | 66        |
| parameter transfer for arrays                  | 66        |
| parameter transfer for records                 | 66        |
| performance, reason for bad                    | 7         |
| PIOC to ND-100 task-to-task messaging          | 14        |
| PLANC compiler commands                        | 39        |
| PLANC coroutines                               | 52        |
| PLANC screen handling                          | 52        |
| PLANC utility library                          | 51        |
| port library                                   | 14        |
| portable programming in PLANC                  | 99        |
| prefixes in routine, variable, constant and type names | 29   |
| PRESENT compiler command                       | 39        |
| quicksorting                                   | 52        |
| random number generation                       | 52        |
| read write vs. import/export in modules        | 31        |
| record conversion to bytes                     | 53        |

Norsk Data Internal Use Only

---

## Page 114

# Index

| Index Term                                   | Reference |
|----------------------------------------------|-----------|
| record packed                                | 100       |
| records in PLANC                             | 74        |
| redefinition of SIBAS databases              | 20        |
| representation of non-packed data in bytes   | 68        |
| routine declaration layout                   | 53        |
| routine header                               | 32        |
| routine header template                      | 32        |
| routine modifier MAINSTART                   | 59        |
| routine modifier YARGS                       | 59        |
| routine modifiers NATIVE and C               | 58, 69, 97|
| routine names                                | 29        |
| routine pointers and indirect routine invokation | 93     |
| routine standard                             | 33        |
| routines                                     | 28, 32    |
| routines within routines                     | 32        |
| routines, nested                             | 32        |
| runtime/data organization on the ND-500(0)   | 67        |
| screen handling                              | 52        |
| segment, file as                             | 7         |
| segments, shared and cache                   | 10        |
| SELECT and the Linkage-Loader                | 40        |
| SELECT and the ND-Linker                     | 40        |
| SELECT compiler command                      | 40        |
| set-referrals in SIBAS                       | 19        |
| SEXMC, combined call in SIBAS                | 19        |
| shared segments and cache                    | 10        |
| SIBAS                                        | 17        |
| SIBAS and SQL                                | 19        |
| SIBAS and the SINTRAN III bit-file           | 20        |
| SIBAS columns                                | 17        |
| SIBAS combined call SEXMC                    | 19        |
| SIBAS combined calls                         | 19        |
| SIBAS database redefinition                  | 20        |
| SIBAS datatypes for storage and display      | 18        |
| SIBAS group columns                          | 17        |
| SIBAS indexes                                | 18        |
| SIBAS OS-file page size                      | 17        |
| SIBAS OS-files                               | 17        |
| SIBAS OS-files for data and indexes          | 17        |
| SIBAS set-referrals                          | 19        |
| significant bytes in identifiers             | 54        |
| simple symbol table management               | 51        |
| single machine XMSG                          | 14        |
| SINTRAN III bit-file and SIBAS               | 20        |
| SORT-MERGE                                   | 21        |
| sorting                                      | 21        |
| speed, designing for                         | 8         |
| SQL and SIBAS                                | 19        |

Norsk Data Internal Use Only

---

## Page 115

# Index

| Index Term                                        | Reference |
|--------------------------------------------------|-----------|
| stack overflow                                   | 63        |
| stack underflow                                  | 63        |
| stacks                                           | 62        |
| standard prefixes                                | 29        |
| standard routines                                | 33        |
| storage alignment for non-packed data            | 68        |
| string constants                                 | 54        |
| string display                                   | 53        |
| string manipulation                              | 52        |
| superkernel                                      | 15        |
| swapper and file as segment                      | 11        |
| swapper on the ND-500(0)                         | 11        |
| symbol table management                          | 51        |
| Symbolic Debugger and LED                        | 27, 58    |
| T-LIB comms for data transfer                    | 13        |
| time and date as string                          | 53        |
| time slicer, time lost to                        | 10        |
| trap handling                                    | 51        |
| trashing                                         | 12        |
| type names                                       | 29        |
| types and constants                              | 27        |
| types, new                                       | 53        |
| underscores and characters                       | 29        |
| underscores in integers                          | 55        |
| UNIX command line/environment pointer retrieval  | 72        |
| unsigned division                                | 53        |
| unsigned variables                               | 55        |
| UTILITY LIBRARY                                  | 51        |
| variable names                                   | 29        |
| variables                                        | 28        |
| warnings                                         | 102       |
| word alignment on ND-5000                        | 9         |
| XARGS routine modifier                           | 59        |
| XM-LIB                                           | 13        |
| XMSG communication                               | 13        |
| XMSG on a single machine                         | 14        |
| XMSG-L message size                              | 15        |

---

Norsk Data Internal Use Only

---

