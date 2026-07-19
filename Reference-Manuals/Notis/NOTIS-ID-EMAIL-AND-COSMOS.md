# NOTIS: Electronic Mail (NOTIS-ID) and How It Delivers Over COSMOS

This document answers the question "what about email?" for Norsk Data's NOTIS
office suite on SINTRAN III / ND-100:

1. What **is** email in NOTIS, and which program provides it?
2. How does a user **write, send, and read** mail?
3. How does mail **actually get delivered** - the mailbox, the mail server, and
   the COSMOS network path?
4. What we **do** and **do not** have documented in this folder.

It is built from the NOTIS-ID manual in this folder; every non-obvious claim is
cited to a specific page/section. Where the manual is silent, that is stated
rather than guessed. Start point was [README.md](README.md).

Companion analysis doc: [NOTIS-PRINTING-AND-A4-PAGES.md](./NOTIS-PRINTING-AND-A4-PAGES.md)
(editing and printing). Related NDInsight material on the COSMOS network itself
lives under `../../Operations/` (COSMOS operator guides).

Primary source: `ND-63.011.2 EN NOTIS-ID User Guide.md` [ID] (version B,
ND 210192).

---

## 0. The one-line answer

Email in NOTIS is **NOTIS-ID** - "NOTIS Information Distribution". It is an
electronic mail system that "allows you to send mail to users on any computer
that is part of the same **COSMOS network** that your computer is connected to"
([ID] Preface). It is not a standalone product: it is **built on top of NOTIS-WP
and NOTIS-DS**, and it hands actual cross-machine delivery to a **mail server**
running over COSMOS.

---

## 1. The three programs behind one mailbox

NOTIS-ID reuses the rest of the suite rather than reimplementing it:

- **NOTIS-WP** - the editor. You write and edit the body of a letter with the
  same full-screen WP editor used for ordinary documents; the Answer and
  Letter/Write commands "provide you with the editor" ([ID] ch.1, Intray/Answer).
- **NOTIS-DS** - the archive. "As a user of NOTIS-ID, you are automatically a
  user of NOTIS-DS, which automatically files all of your mail in your mailbox"
  ([ID] Preface). Every letter you receive, write, or send is a DS document.
- **NOTIS-ID** - the mail layer on top: the command menu, the Intray/Outtray,
  the receiver/address handling, and the delivery options.

Consequence: a "letter" is just a NOTIS document with mail metadata (receivers,
mailing time, registered flag), stored in DS folders and moved around by the
mail server.

---

## 2. The user's view - the command menu

The NOTIS-ID screen presents a two-level command menu ([ID] ch.1, "The command
menu"). Top level:

```
ID:   Intray   Outtray   DS-folder   Letter   NOTIS
      List  Answer  Mail  Display-text  Receivers  Info
```

- **Intray** - incoming mailbox. All letters you receive are placed here; the
  screen shows a live **"mail count"** of unread letters, and the "Mail" area
  lights up when unread mail is present ([ID] ch.1, "The screen picture").
  An asterisk `*` marks an unread letter in the list ([ID] Intray/List).
- **Outtray** - a copy of every letter you write and/or send is stored here, so
  you can check whether it "has been stored, sent, received, etc." ([ID] ch.1,
  "Outtray").
- **DS-folder** - reach documents you have stored in NOTIS-DS and pull them into
  the mail flow (e.g. mail an existing document, or enclose one).
- **Letter** - Write / Mail / Store / Fetch / Continue: compose a new letter,
  send it, save a draft, retrieve a draft, or resume an interrupted one.
- **NOTIS** - jump into the underlying NOTIS/WP and NOTIS/DS commands.

Every leaf command has a two-letter shortcut formed from the two menu levels,
e.g. **IL** = Intray/List, **IA** = Intray/Answer, **IM** = Intray/Mail,
**LW** = Letter/Write, **OL** = Outtray/List ([ID] ch.1, command tables).

### 2.1 Reading and replying

- **Intray/List (IL)** lists your incoming letters in the work area; the display
  header shows sender, subject, number, date and mail count, e.g. the DS path
  `/MARY MACLYNN/MAILBOX/INTRAY/PARKING LOT-01-...` ([ID] Display-text example).
- **Intray/Answer (IA)** opens the editor pre-addressed to the original sender.
  When you choose Mail, the reply goes automatically to the sender - and,
  optionally, to *everyone else* who received the original, or to only *some* of
  them ([ID] ch.1, Intray/Answer; ch.8 p.121).
- **Display text** shows the body; **Receivers** lists who a letter went to (with
  their system names, e.g. `Tom Grouch / JEEVES`); **Info** shows the document
  profile.

---

## 3. Sending a letter - the mail options

When you choose **Mail**, NOTIS-ID prompts for delivery options ([ID] ch.2,
"Registered mail, enclosure and mailing time"; ch.7 p.100+):

| Field | Meaning |
|-------|---------|
| **Receiver** | The user name to deliver to. |
| **Address** | The receiver's *system address* (see section 4). Usually left blank. |
| **Registered mail? (Y/N)** | If Y, your Outtray tells you whether each receiver has read, deleted or moved the letter ([ID] "Registered Mail"). |
| **Enclosure? (Y/N)** | If Y, you are prompted for another document to attach before mailing ([ID] "Enclosure"). |
| **Mailing time** | Defaults to now; can be set to a future date/time for deferred delivery ([ID] "Mailing time"). |

---

## 4. How delivery actually works - addresses, mailing lists, the mail server

This is the part that answers "how does it reach another machine?".

### 4.1 The address field is usually blank

You fill in **Address** only when *both*: the receiver has a different address
than you, *and* the receiver is not on any mailing list ([ID] ch.2, "When You
Need to Fill in the Address Field"). You do **not** fill it in when the receiver
shares your address, or is on a mailing list.

### 4.2 The mail server resolves the address

Address resolution is done by the **mail server**, not by the user:

- There may be a **common mailing list** (set up by the system supervisor) that
  maps all user names to their addresses; the mail server consults it to find a
  receiver's address ([ID] ch.2, "Receiver is on a mailing list").
- If you keep **your own** user mailing lists, the mail server checks yours
  first ([ID] same section).

### 4.3 Cross-machine delivery over COSMOS, with retry

Delivery is store-and-forward over the COSMOS network:

> "Sometimes a letter cannot be delivered immediately. The receiver's computer,
> for example, may be temporarily inaccessible. The **mail server keeps trying to
> deliver the letter at regular intervals** until the letter is at its
> destination." ([ID] ch.1, delivery note)

So the model is: your letter is filed in DS, the mail server picks it up, resolves
the receiver's address (local list -> common list), and forwards it across COSMOS
to the destination machine's mailbox, retrying until it lands. **Registered mail**
is the read-receipt mechanism layered on top - the destination reports back to
your Outtray when the receiver reads/deletes/moves it.

---

## 5. Getting started (prerequisites)

- You must be **registered as a NOTIS-ID user** and know your user name and
  password(s); if not, the system supervisor sets this up ([ID] ch.1, "How to
  start using NOTIS-ID").
- You log in with the user name registered in **NOTIS User Environment
  (NOTIS-UE)** ([ID] same).
- The program may be invoked under a supervisor-chosen name - "Electronic Mail",
  "Mail", "ID", or similar ([ID] same).
- Recommended background: familiarity with NOTIS-UE and NOTIS-WP version M or
  later ([ID] "The Reader").

---

## 6. What this folder has - and what it does not

**Have (in this folder):**

- `ND-63.011.2 EN NOTIS-ID User Guide.md` - the end-user guide (version B). This
  is the sole NOTIS-ID manual present, and the basis for everything above.

**Referenced by the guide but NOT present in this folder** ([ID] "Related
Manuals"):

- **NOTIS-ID Supervisor Guide**, ND-30.062.1 EN - administration of the mail
  server, mailing lists, addresses and registration. *We do not have this file*,
  so the server-side/administrative mechanics (how the mail server is configured,
  how COSMOS routing is set up) are **not documented here** - only the user-facing
  behavior is.
- NOTIS-WP User Guide (ND-63.018.2 EN) and NOTIS-DS User Guide (ND-63.017.3 EN) -
  the EN editions the guide cross-references; this folder has other WP/DS editions
  but not these exact publication numbers.

**Not verified from source (flagged, not guessed):**

- The exact COSMOS protocol/transport NOTIS-ID mail rides on is not described in
  the user guide; it only says delivery is over the COSMOS network with retry.
  Do not assume a specific wire protocol from this document.

---

*Part of the [NDInsight](../../README.md) Norsk Data / SINTRAN III documentation
and preservation project. Source manual is a Norsk Data A.S publication,
reproduced for historical and technical reference.*
