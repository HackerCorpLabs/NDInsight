# Source: ndwiki article "ND-NOTIS"

- **Live page**: <https://www.ndwiki.org/wiki/ND-NOTIS>
- **Copy used here**: Wayback Machine snapshot, fetched 2026-08-27 by Ronny's
  request. The live ndwiki is behind an Anubis proof-of-work gate.

**Status: SECONDARY.** ND-NOTIS was ND's integrated word processing and office
suite, introduced 1979, tied into the database and into COSMOS.

It matters to the machine history for two reasons: the ND-COSMOS networking system
was built with tight NOTIS integration, and the **Butterfly** workstations existed
largely to run NOTIS software on a PC. English Wikipedia also carries the claim
that NOTIS-QL was copied by Microsoft as Access - uncited, and strong.

---

## Verbatim extract

ND-NOTIS was a tightly integrated yet modular office automation suite by Norsk Data introduced in the early 1980s, running on the SINTRAN III platform on both ND-100 and ND-500 architectures. It was also available on MS Windows running in networks of Norsk Data servers. The name NOTIS is short for "NOrsk Data Text og Informasjons System" and was chosen by Dag Spilde[1].

It was very successful, and was the main product line of the company for quite a while[citation needed], cementing its position in the Norwegian government office automation market.[citation needed] It was also very popular in Germany and in the UK (local municipality, DHSS etc)[citation needed] 

The NOTIS family of products was presented to the British Computing Society by Jeremy Salter. Roger Tagg et al. (BCS, End User SG, 1985) and endorsed as the BCS model for user interface.[citation needed] The same endorsement was awarded to NOTIS-IR as a model for information storage and retrieval.[citation needed] the European Commission published in 1985 NOTIS-IR as reference model for document and information search and retrieval.[citation needed]

Norsk Data also sold custom-made Tandberg Data TDV-2200 terminals as "NOTIS terminals" with special keys for text editing. Other terminals were "endorsed" and branded as "NOTIS Terminals" - including the Facit "Twist" - that would show a page standing.

### Contents

- 1 NOTIS components

- 2 General

- 3 Applications beside Text and Document Management

- 4 Sources

### NOTIS components
NOTIS-WP
NOTIS Word Processor, a full text editing environment optimized for word processing.
NOTIS-DS
NOTIS Document Store, a database of documents based on the SIBAS relational database.
NOTIS-BS
NOTIS Backup System, an advanced system used for automated and incremental backups of a DS document store.
NOTIS-ID
NOTIS mail system. Proprietary e-mail system. It was later interfaced to Notis-Mail (see below).
NOTIS-Mail
NOTIS full X.400 e-mail system, including a X.500 based directory service (implemented using SIBAS) and TCP/IP based SMTP mail.
NOTIS-TF
NOTIS Text Formatter, a text formatting system.
NOTIS-RG
NOTIS Report Generator, a powerful data extracting and modifying system. Often used together with database systems like Norsk Data SIBAS-II or Oracle.
NOTIS-RP
NOTIS Report Producer, closely integreated with NOTIS-RG.
NOTIS-CALC
NOTIS Spreadsheet program, similar to VisiCalc and later successors like Microsoft Excel
NOTIS-ENCRYPT
NOTIS Encryption software.
NOTIS-IR
NOTIS Information Retrieval, a document database with free text search allowing full multi-site search.
NOTIS-QUERY
NOTIS Database query and application generation program, similar to MS Access.

### General

NOTIS captured the notion of diffent user interfaces, or terminals; and managed a common user interface for all applications that used the platform. So a key on the keyboard would in all applications "mean" the same. 

It relied on an interface system "User Environment" to hold in one place all user profile and preferences. That is everything from log-in name and password, language preference, application skills and user rights to see, edit and change document - or data in applications. It came as a full "Document management" package, with full support for workflow - which was used by 3rd party application software. 

Another first was multi-lingual support, - also part of the user interface. Regardless of where you logged on, the system would know of your preferences, and allow you to resume last task. The system also supported full editing from right to left. All deliveries to the public sector required capability to use three language, and that in the same office, all three languages would be used. So to sell in its main market, it need multilingual support. That included all messages, error messages and user interaction. The error messages could also be adapter from "novice" to "expert".

An important "first" was the full support for SGML - or "S-code". This allow the text editor to be used to edit and view the first HTML documents created[citation needed] - on hardware running NOTIS. The alternate character set - "T-code" was the CCITT, now ITU T.56 standard character set - used in all television sets to show teletext/"Text TV". So, the systems had full support for semi-graphical input and display, but just a few terminals supported this.

The "back-end" to all these modules were also flexible. It had direct file system exposure - that included network mounted files.[citation needed] However, with NOTIS-DS it included "Document Storage" and management - a full Electronic Document Management System. The EDMS was based on a generic software interface, but only SIBAS was used commercially. This allowed fully localisation transparent document storage and retrieval. That of course demanded NOTIS-IR to search in all the documents.

NOTIS-ID was a "special" NOTIS-DS, with restricted functionalty, in that this would interface to "mail" exchanges only. Storing a document to an email-recipient functioned as sending an email. Likewise, Received email appear in the mail-count, and it was readable in NOTIS-WP like any other document.[citation needed]

Norsk Data needed NOTIS to avoid duplicating applications. So the software was used in professional text production systems, for newspaper and magazine production - by "NORTEXT". It was reviewed a number of times here and found to be "best of breed" by e.g. the Seybold Report on newspaper systems.[citation needed] 

### Applications beside Text and Document Management

The list here will become endless since NOTIS was linked to three application generators - beside the Query part. This was fully capable of making large applications system, that could also update databases. The most successful link was to "Unique" - an application package developed outside Norsk Data to support SIBAS but later was enhanced to interface to a number of RDBMS.[citation needed] The other platforms were "BIM" (Business Information Systems) and "ABM" (Application Building and Maintenance). A full interface was made to "Systemator" to provide full support to newspaper systems generated by this. Norsk Data marketed and sold the system as integrated with their offerings for the medical sector ("Infomedica") and hospital systems;- for local community in Scandinavia and the UK based on Unique (i.e.DIALOGUE-1); - for engineering documentation in Europe: CAD/CAM as Technovision and even to the F-16 Flight Simulator.[citation needed]

NOTIS-QUERY - also marketed by Norsk Data as "Access-1", is still commercially available as "QBEvision" and has been licensed under several names with full product sold by CA.[citation needed]

The NOTIS family was fully ported to Microsoft Windows, but was incredibly difficult to move with its huge customer base. For a time there were plans to include "Ami" into the family, to gain some market momentum.

The NOTIS family was ported to Norsk Data NDIX Unix line, but here suffered because Unix had problems with coping with the key sequences required (terminfo/termcap was incomplete).

### Sources

- This article was originally a copy of the English Wikipedia article ND-NOTIS in 19 August 2009.

- ^ Norsk Data - hva gikk galt?
