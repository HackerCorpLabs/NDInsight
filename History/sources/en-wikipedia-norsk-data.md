# Source: English Wikipedia, "Norsk Data"

- **Page**: <https://en.wikipedia.org/wiki/Norsk_Data>
- **Fetched**: 2026-08-27 via the MediaWiki API (`action=query&prop=extracts`),
  by Ronny's request.
- **Copy below**: the plain-text extract, complete, transliterated to ASCII.

**Status: SECONDARY SOURCE, an encyclopedia, and again uncited in the body** - the
References section is empty, though the prose around the Unix years reads as if
it was written from contemporary trade-press reports.

**It is nearly three times the length of the Norwegian article** (12,575
characters against 4,553) and is not a translation of it. The two disagree on
several points, and the English one carries whole subjects the Norwegian lacks.

## What this page adds that nothing else here had

- **The scale of the company at its peak.** In 1987 Norsk Data was the second
  largest company in Norway by stock value, behind only Norsk Hydro, and employed
  **over 4,500 people**.
- **Two pre-company machines at FFI Kjeller**: the **SAM** and the **SAM 2, also
  known as FLINK**. The Norwegian article names only SAM.
- **Lithuania.** In March 1991, shortly after the January Events, ND donated the
  first computer to the Lithuanian Institute of Mathematics and Informatics,
  starting **LITNET**. When the lines from Vilnius to Moscow were shut down later
  that year, further ND-donated hardware gave Lithuania its first satellite
  Internet connection at 9.6 kbit/s - the first Lithuanian communications line
  wholly independent of the Soviet Union.
- **The whole Unix strategy**, which no other source here mentions at all:
  collaboration with **DIAB** of Sweden from 1987; a 1988 agreement with the
  **Santa Cruz Operation** for System V on ND's Intel-based PCs; the **Uniline 33**
  range on Motorola 68030 in 1989; **NDIX**, ND's own Unix for its proprietary
  architecture, offered in Scandinavia while conventional Unix went to
  international customers; and Motorola **88000** systems planned to follow.
- **How it actually ended.** Hardware R&D was split off as **Dolphin Server
  Technology in 1989**, which supplied 88000-based systems back to its parent -
  the **Uniline 88** series, Scandinavia 1990, UK and Germany 1991. ND also agreed
  to resell **Data General's Aviion** 88000 line. Dolphin was sold to a Telenor
  subsidiary, and **the remaining parts of Norsk Data were bought by Telenor**.
  Dolphin later split again, the most successful piece being **Dolphin
  Interconnect Solutions**.
- **A long British afterlife.** Telenor kept the Norsk Data name in the UK for
  hardware support and maintenance, mainly for HMCG and local government; bought
  the ISP **CIX** and the Manchester host **XTML** for over GBP 50 million at the
  end of the dotcom boom; cycled through Nextra and Telenor Business Solutions
  before reverting to ND Norsk Data; resold CIX and XTML to Pipex reportedly for
  under 10% of the purchase price; and was itself acquired by **2e2 in 2003**.
- **More machines**: **ND-5400**, **ND-5900-2** and **ND-5900-3** (dual and triple
  CPU), and **ND-88000**, ND's own MC88000 RISC implementation for Unix/NDIX, 1987.
- **KPS (Knowledge Processing System)**, a joint venture with **Racal plc**, which
  it says pioneered a multi-user LISP machine environment.
- **NORD-100 described as the first single-board 16-bit minicomputer CPU**, and as
  a very early use of bit slicing in minicomputers.
- **XMSG dated and described**: "OSI based (X21 and X.25) communication system,
  integrated with SINTRAN, with support for both synchronous and asynchronous
  communication in 1974 and on. Full LU 6.2 support in 1982." That is directly
  relevant to the XMSG work in `SINTRAN/XMSG/` - and it is an uncited encyclopedia
  sentence, so treat it as a lead, not a fact.
- A claim worth flagging as **uncited and strong**: that NOTIS-QL was copied by
  Microsoft as Access, its internal name having been "Access-1".

## Where it contradicts sources we already hold

Recorded, not resolved.

- **Founding date: "August 8, 1967".** Both the Norwegian article and the
  norsk-data.com timeline say **7 July 1967**, and the timeline adds that the
  company became an aksjeselskap on 19 September 1967. That makes three dates in
  circulation. The Broennoeysund register is cited by the timeline for 7 July.
- **ND-505 has "29 bit addresses"** here. The Norwegian article calls it "en
  28-bits maskin". One of the two is wrong, and the difference matters because the
  number is the whole point of the machine - it was cut down to clear the CoCom
  embargo.
- **ND-5850 in 1987**, agreeing with the Norwegian article and disagreeing with
  the norsk-data.com timeline, which launches Rallar in **1990**. Note this page
  also puts the ND-5000 in 1987, so it has fourth and fifth generations arriving
  the same year.
- **NORD-50 as "second generation 32-bit supermini in 1975"** - the same claim as
  the other two secondary sources, and the same contradiction with the **primary**
  ND document `Reference-Manuals/10/NORD-10-Design-Goals.md`, which calls the
  NORD-50 a special-purpose **array processor introduced in 1973**. Three
  secondary sources repeating each other still do not outweigh one primary.
- **Only three founders**, as the Norwegian article has it. The norsk-data.com
  timeline adds Terje Mikalsen.

## Where it is more careful than our other sources

On the NORD-5 it says: the machine "in combination with NORD-1 and therefore a
16-bit minicomputer with a 32-bit attached processor, **was claimed to be, and
reported as**, a 32-bit minicomputer or superminicomputer". It also notes the
**Interdata 7/32** slightly followed it. That is a more honest framing than
ndwiki's flat "world's first 32-bit supermini", and it matches the architecture
Ronny described - an attached processor, not a standalone machine.

---

## Verbatim extract

Norsk Data was a minicomputer manufacturer located in Oslo, Norway. The company was founded in 1967 and dissolved in 1992. Its most active period was from the early 1970s to the late 1980s. At the company's peak in 1987, it was the second largest company in Norway and employed over 4,500 people.
Throughout its history Norsk Data produced a long string of extremely innovative systems, with a disproportionately large number of world firsts. Some examples of this are the NORD-1, the first minicomputer to have memory paging as a standard option, and the first machine to have floating-point instructions standard, the NORD-5, the world's first 32-bit minicomputer (beating the VAX, often claimed the first, by 6 years).


== Historical overview ==

The origins of Norsk Data go back to the development of digital computers at the Norwegian Defense Research Establishment at Kjeller, Norway, where several early computers had been designed, such as the SAM and the SAM 2, also known as the FLINK.
The success of this program resulted in the founding of A/S Nordata - Norsk Data Elektronikk on August 8, 1967, by Lars Monrad Krohn, Per Bjoerge and Rolf Skaar. The company became a significant supplier of minicomputers to many research projects, in particular to CERN in Geneva, Switzerland, where they were chosen to produce the computers for many projects, starting with the SPS Project, Norsk Data's international breakthrough contract. The other market segments Norsk Data succeeded in were process control, Norwegian municipal administration data centers, newspapers, as well as parts of the educational, health, and university sector.
In 1981-1982, Norsk Data became the first Norwegian company to be listed on both the London Stock Exchange and Nasdaq.
For a period in 1987, Norsk Data was the second largest company by stock value in Norway, second only to Norsk Hydro, and employed over 4,500 people.
In March 1991, shortly after the January Events, Norsk Data donated the first computer to Lithuanian Institute of Mathematics and Informatics. This donation started the development of LITNET, an academic and research network in Lithuania. Later that year, the network connection lines directly connecting Vilnius to Moscow were shut down. With the help of additional hardware donated by Norsk Data, Lithuania was able to use its first satellite-based Internet connection, which operated at 9.6 kbit/s. This was the first Lithuanian communications line that was totally independent from the former Soviet Union.
After a long period of exceptional success, the Norsk Data "empire" collapsed in the early 1990s, mostly due to not realizing the impact of the PC revolution as well as the growing competition from Unix-based systems. In 1987, Norsk Data sought to expand its collaboration with DIAB of Sweden to provide UNIX-based systems in Norsk Data's portfolio, to offer "a complete UNIX concept" together with the company's ND-5000 products. 1988 saw the company sign an agreement with the Santa Cruz Operation to offer SCO's System V product on its Intel-based personal computer systems. In 1989, alongside upgraded versions of the company's proprietary minicomputer range, notably the ND-5850, attempts were made to introduce Unix products such as the Uniline 33 range, based on Motorola system designs for the 68030 processor. Such conventional Unix systems were primarily aimed at international customers, whereas in Scandinavia the company reportedly sought to offer only its NDIX implementation of Unix for its own proprietary architecture. Systems based on Motorola's 88000 processor were planned to follow on from these new 68030-based products.
Efforts to restructure the company in 1990 were initially perceived as moderately successful, with executives and analysts expressing beliefs that such restructuring had put the company in a more favourable position than competitors who were yet "to swallow the same bitter pill" of refocusing and workforce reductions. Development of Norsk Data technology was continued by Dolphin Server Technology, with this spin-off company aiming to supply Motorola 88000-based systems to its parent. Indeed, Norsk Data introduced the Uniline 88 series of 88000-based systems, developed by Dolphin, initially in Scandinavia during 1990 and then in the UK and Germany during 1991. Norsk Data also announced an agreement with Data General to resell that company's Aviion line of 88000-based products.
Ultimately, having reorganised and rebranded its operations and having sold off numerous divisions, including Dolphin to a Telenor subsidiary, the remaining parts of Norsk Data were purchased by Telenor.


== Notable innovations ==
Throughout the times, Norsk Data produced a long string of innovative computers. Some examples of this include:

The NORD-1, the first minicomputer to have memory paging as a standard option, and the first machine to have floating-point instructions as standard.
The NORD-5, in combination with NORD-1 and therefore a 16-bit minicomputer with a 32-bit attached processor, was claimed to be, and reported as, a 32-bit minicomputer or superminicomputer, introduced in 1972. It slightly preceded the Interdata 7/32 and preceded the Digital Equipment Corporation VAX by a number of years, these being occasionally cited as the first of their kind.
The NORD-100, a very early application of bit slicing in minicomputers.
The KPS (Knowledge Processing System), developed in joint venture with Racal plc, a system which pioneered running a multi-user LISP machine environment.


== Post-breakup companies ==
Although the Norsk Data breakup caused a large number of layoffs, a large number of employees and intellectual property lived on in various smaller companies. Some went bankrupt quite quickly, some were bought for tax purposes.
The hardware research and development group was split off into Dolphin Server Technology in 1989. Dolphin later split off into a number of companies, by far the most successful of these being Dolphin Interconnect Solutions, a cluster interconnect hardware company.


=== Norsk Data UK ===
In the UK, Telenor kept the Norsk Data name for several years, focusing in on hardware support and maintenance contracts, mainly with HMCG and local governments.
At the tail end of the "dotcom boom" Telenor decided to try and expand the service by acquiring the ISP CIX  and XTML, a hosting company in Manchester, UK. The total expenditure on acquisitions was more than GBP 50 million.
The name and business focus of this group of companies changed several times in the early 21st Century. Initially combined with the acquired CIX and XTML to form the UK arm of Telenor's Nextra subsidiary, a "communications service provider", the group became Telenor Business Solutions before finally reverting to ND Norsk Data once CIX and XTML had been resold to Pipex, reportedly for less than 10% of the purchase price. Much of the loss in value of the acquired companies was put down to the astronomical "goodwill" payment included in the purchase price during the "dotcom boom".
In 2003, Norsk Data was eventually acquired by 2e2, an IT services business pursuing rapid growth through acquisition, joining various other established businesses including elements of PinkRoccade UK Group and ROCC Computers. This bolstered the hardware maintenance side of the company. The growth by acquisition trend continued with several smaller businesses being taken on, and many employees subsequently being laid off. Major losses of high earning contracts such as Thomas Cook, Woolworths, HMP, or Corus, were never replaced with similar-sized customers.


== Hardware ==
Significant Norsk Data computer models include:

NORD-1, 16-bit minicomputer launched in 1968, could run TSS (see below) from 1971
NORD-5, 32-bit supermini launched in 1972
NORD-9,
NORD-10, 16-bit mini launched in 1973
NORD-10/S, version of the Nord-10 with cache, paging, and other improvements
NORD-50, second generation 32-bit supermini in 1975
NORD-100, 16-bit, from 1978, later renamed ND-100. First single-board 16-bit minicomputer CPU.
ND-500, third generation 32-bit supermini in 1981
ND-505, a version of the ND-500 with 29 bit addresses allowed through the CoCom embargo of the Eastern bloc
ND-5000 ("Samson"), fourth generation 32-bit supermini in 1987 (5400, 5700, 5800)
ND-5850 ("Rallar"), fifth generation 32-bit supermini in 1987
ND-5900-2, ND-5900-3, and ND-5904, dual-, triple- and quad-CPU 5000 series machines.
ND-88000 - ND implementation of the Motorola MC88000 RISC for Unix/NDix - 1987


== Software ==
In addition to hardware, Norsk Data also produced a wide range of system and application software:

NORD-TSS - Nord Time Sharing System from 1971
SINTRAN - Operating system for Nord 10 and later models, version III from 1973, III/VS in 1974
XMSG - OSI based (X21 and X.25) communication system, integrated with SINTRAN, with support for both synchronous and asynchronous communication in 1974 and on. Full LU 6.2 support in 1982
SIBAS database based on the Codasyl database specifications was ported by the Central Institute for Industrial Research in 1975 SIBAS is owned by SRS.
FORTRAN compiler
ND-Paint Graphic editing - Windows based
BASIC compiler developed in Kiel and Muelheim an der Ruhr, Germany with the CAT-System (Common Abstract Tree-Language) using the Vienna Development Method, 1983
COBOL compiler
C compiler for ND-100/ND-500 developed by University of Luleaa, and IAR Systems AB, Sweden, in cooperation with Norsk Data A.S, 1984. A later C compiler developed in Kiel and Muelheim an der Ruhr, Germany with the CAT-System using the Vienna Development Method, 1987.
ND-COSMOS - computer networking system
PLANC compiler - PLANC was the system language of Norsk Data - a language "defined by its implementation", similar to C, but assignment left to right, as you read: A + 1 =: A same as A++
Pascal compiler developed by Prof. Dr. Hans Langmaack and his team at Kiel University in Germany with the CAT-System using the Vienna Development Method, 1987
PED - "Programmer's EDitor" Screen oriented text editor
LED - "Language-sensitive programmer's EDitor" Screen oriented text editor and debugger - complete Integrated Development Environment made for own use.
ND-NOTIS - Integrated, modular word processing and office application suite with ties to database and customer applications.
NORTEXT - typesetting system integrated with ND-NOTIS and SIBAS
Lisp Machine Lisp - MIT Lisp machine lisp developed in a joint venture Racal-Norsk (ZetaLisp).
Technovision - CAD system developed in Muelheim an der Ruhr, Germany. Technovision was a modular CAD/CAM system which was internationally considered to be one of the best on the market. It was in part designed by Norsk Data Dietz GmbH. A special workstation named the Technostation was designed specifically for running Technovision. It was extremely well received by international press, and even won a design award.
BIBDIA - Library system developed by Norsk Data Dietz GmbH in Muelheim an der Ruhr, Germany. BIBDIA was further developed by BiBer GmbH since 1992. The current WEB-based version is still running as a market leader in Germany and Switzerland.
In addition to the above: 

two batch languages, called JEC and XCOM. JEC were used primarily as a simple batch job controller, whereas XCOM was used for much more involved routines such as operating system patches etc. Most of the applications came in two different editions, one compiled for the NORD-10/ND-100 series and one compiled for the ND-500/ND-5000 series.
ND spun off NOTIS-WP and NOTIS-RG into NOTIS AS, which later changed its name to Maxware. NOTIS-QL was copied by Microsoft, where it is called Access (the internal name for NOTIS-QL was "Access-1") but the original was sold to Sysdeco and sold now with the name "QBEVision".


== Tim Berners-Lee connection ==
The ENQUIRE program, a predecessor of the World Wide Web from its creator, Tim Berners-Lee, was developed and run on Norsk Data machines running SINTRAN III at CERN. Written in Pascal and, in principle, portable to other systems, it saw no further use beyond that of its developer and the source code was eventually lost.


== References ==


== External links ==
Norsk Data Forum - a Norwegian site operated by ND ex-employees.
A commercial Norsk Data-related home page
Norsk Data Pakistan Private Limited
Computer-Archiv - Norsk Data
A Norsk Data page operated by Tore Bekkedal
Norsk Data's history by Jonny Oddene
NDWiki, The Norsk Data encyclopedia
BiBer GmbH - a German Company founded by ND ex-employees.
