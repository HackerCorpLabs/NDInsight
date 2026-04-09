# User Environment (UE) - Complete Guide

## What is User Environment?

**User Environment (UE)** is a software package delivered with Norsk Data computers that provides a menu-driven interface layer on top of SINTRAN III. It is designed to:

- Make the system accessible to non-expert users through menus instead of command-line
- Increase data security by controlling which SINTRAN user areas each person can access
- Provide a controlled login mechanism separate from the standard SINTRAN login
- Allow administrators to restrict what users can do

UE sits **outside** SINTRAN - it intercepts the login process on configured terminals and presents its own menus and authentication before the user reaches SINTRAN.

**Product number**: ND-210518 (includes File Manager)
**Primary reference manual**: ND-60.194 (User Environment Reference Manual) - NOT in this repository
**Security handbook**: ND-30.048 (Software Security Handbook)

### UE Versions

| UE Version | SINTRAN Version | Notes |
|:---:|:---:|---|
| B | K | Had to be modified for VSX compatibility |
| C | K, L | Highly improved performance. Recommended for L-version |
| D | M | Required for M-version |
| E | N | Enhanced security, ND-5000 support. Server moves to ND-5000 |

---

## How UE Login Works

### Normal SINTRAN Login (without UE)

1. User presses ESC on terminal
2. SINTRAN prompts: `ENTER <USERNAME>:`
3. User enters username
4. SINTRAN prompts: `PASSWORD:`
5. User enters SINTRAN password
6. User gets `@` prompt

### UE Login (with UE enabled on terminal)

1. User presses ESC on terminal
2. System **automatically** logs in as SINTRAN user `USER-ENVIRONMENT`
3. The **UE-LOGIN** program starts
4. UE prompts for **UE username** (can differ from SINTRAN username)
5. UE prompts for **UE password** (separate from SINTRAN password)
6. UE presents menus based on the user's profile
7. From the menus, the user can start programs, access SINTRAN, etc.

The key difference: with UE, the user never sees the raw SINTRAN login. UE controls which SINTRAN user areas the UE-user is allowed to access via `@UE-FUNC CHANGE-USER-AREA`.

---

## Installation

### Prerequisites

- SINTRAN III (version must match UE version - see table above)
- The `USER-ENVIRONMENT` SINTRAN user area must exist
- For UE version E (N-version): ND-5000 processor and XMSG must be available

### Install from Floppy

```
@(2:FLOPPY-USER)IN-UE-:.E
```

Follow the on-screen instructions. The installation program will prompt for disk changes.

### Files and User Areas

**SINTRAN user area: `USER-ENVIRONMENT`**

This is the primary user area where UE files are stored. Key files include:

| File | Purpose |
|------|---------|
| `(USER-ENVIRONMENT)UE-LOAD:MODE` | Mode file to load UE programs into segment files (cold start) |
| `(USER-ENVIRONMENT)UE-ENABLE:MODE` | Mode file to enable UE after loading |
| `(USER-ENVIRONMENT)UE-LOGIN:PROG` | The login program that runs when users press ESC |
| `(USER-ENVIRONMENT)UE-DATABASE:*` | UE database files (user profiles, passwords, menus) |

**SINTRAN user area: `UE-DATABASE-USER`** (Version E only)

In UE version E, the database is moved to a separate user area to allow it to run on the ND-5000:

| File | Purpose |
|------|---------|
| `(UE-DATABASE-USER)UE-DATABASE:*` | Moved from USER-ENVIRONMENT |

**Migration to Version E:**
1. Create the `UE-DATABASE-USER` user area
2. Allocate disk space for the database
3. Move files `UE-DATABASE::xxxx` from `USER-ENVIRONMENT` to `UE-DATABASE-USER`
4. Ensure `USER-ENVIRONMENT` user area has at least 500 pages free

### RT-Programs

| RT-Program | Octal ID | Purpose |
|------------|----------|---------|
| **UEXPS** | 61426₈ | UE Profile Server - the central UE server process |
| **UE-LOGIN** | -- | Login program started per terminal |

---

## System Startup Configuration

### Cold Start (HENT-MODE:MODE)

During a cold start, UE programs must be loaded into segment files. Add these lines to your `HENT-MODE:MODE`:

```
@CC ============================================================
@CC LOAD AND ENABLE USER ENVIRONMENT
@CC ============================================================
@CC
@MODE (USER-ENVIRONMENT)UE-LOAD:MODE,,,
@MODE (USER-ENVIRONMENT)UE-ENABLE:MODE,,,
```

These mode files are delivered with the UE product. `UE-LOAD:MODE` loads the UE programs onto the segment file. `UE-ENABLE:MODE` configures UE to be ready for use.

### Warm Start (LOAD-MODE:BATC)

During a warm start, UE needs to be started. Add to your `LOAD-MODE:BATC`:

**For UE versions B-D:**
```
@CC ============================================================
@CC START USER ENVIRONMENT.
@CC ============================================================
@CC
@UE-FUNC-EN START-PROFILE-MANAGER
```

**For UE version E (N-version) - must come AFTER ND-5000 and XMSG startup:**
```
@CC ============================================================
@CC START ND-5000.
@CC ============================================================
@CC
@ND-500-MONITOR
START-SWAPPER
EXIT
@CC
@CC ============================================================
@CC START XMSG.
@CC ============================================================
@CC
@MODE (UTILITY)XMSG-START:MODE,,,
@CC
@CC ============================================================
@CC START USER ENVIRONMENT (MUST BE AFTER ND-5000 AND XMSG).
@CC ============================================================
@CC
@UE-FUNC,START
@WAIT-FOR-UE
```

**IMPORTANT (Version E):** The `@UE-FUNC,START` line MUST be placed **after** the commands that start the ND-5000 and XMSG, because the UE server now runs on the ND-5000.

### Controlled Stop

When stopping the system, UE should be stopped before XMSG and ND-500. In your stop mode file:

```
@CC ============================================================
@CC STOP USER ENVIRONMENT.
@CC ============================================================
@CC
@UE-FUNC-EN STOP-PROFILE-MANAGER
@CC
@CC ============================================================
@CC STOP XMSG.
@CC ============================================================
@CC
@SINTRAN-SERVICE-PROGRAM
@STOP-XMSG
@EXIT
```

**NOTE on language:** `@UE-FUNC-EN` is the English version. If your system is configured for Norwegian, use `@UE-FUNC` (without `-EN`). The documentation examples use both forms.

---

## Terminal Configuration

### Enable UE on Terminals

To control which terminals require UE login, use the `@UE-AUTOMATIC-LOGIN` command (requires user SYSTEM):

**Enable on all terminals:**
```
@UE-AUTOMATIC-LOGIN Y,1
```

**Enable on a specific terminal:**
```
@UE-AUTOMATIC-LOGIN N,1,51
```

(Enables UE on terminal 51)

### Disable UE on Terminals

**Disable on all terminals:**
```
@UE-AUTOMATIC-LOGIN Y,0
```

**Disable on a specific terminal (e.g., console):**
```
@UE-AUTOMATIC-LOGIN N,0,1
```

### Disable UE in a Mode File

You can put this in LOAD-MODE:BATC to automatically disable UE on specific terminals during warm start. Place it **before** the `@UE-FUNC START-PROFILE-MANAGER` line:

```
@UE-AUTOMATIC-LOGIN N,0,1
@UE-AUTOMATIC-LOGIN N,0,2
@UE-FUNC-EN START-PROFILE-MANAGER
```

Or to disable on all terminals (effectively turning UE off):

```
@UE-AUTOMATIC-LOGIN Y,0
```

### Recommendation

Always keep **at least one terminal** (typically the console, terminal 1) **without UE**. This is essential because:
- You need a way to administer the system if UE fails
- For UE version E: if the ND-5000 is unavailable, UE cannot function at all
- The console is the only terminal that can access OPCOM

---

## UE-FUNC Commands

`@UE-FUNC` is **not a SINTRAN command** - it is an ADP (Application Development Program). The entire command must be specified on **one line**.

### Known UE-FUNC Subcommands

| Command | Purpose |
|---------|---------|
| `@UE-FUNC START-PROFILE-MANAGER` | Start the UE Profile Server (UEXPS RT-program) |
| `@UE-FUNC STOP-PROFILE-MANAGER` | Stop the UE Profile Server |
| `@UE-FUNC CHANGE-USER-AREA <user>` | Switch to a different SINTRAN user area within UE session |
| `@UE-FUNC TERMINAL-STATUS` | Display terminal status including UE info, IP address (version E) |
| `@UE-FUNC,START` | Short form to start UE (seen in N-version docs) |

**NOTE:** The `CHANGE-USER-AREA` subcommand is important - it is how UE switches between SINTRAN user areas for a logged-in UE user. This operation is logged by the file system audit trail (M-version and later).

### Starting UE Manually (Alternative Method)

Instead of `@UE-FUNC`, you can start the UEXPS RT-program directly:

```
@LIST-RT-DESCRIPTION UEXPS
@RT UEXPS
@LIST-RT-DESCRIPTION UEXPS
```

The first `@LIST-RT-DESCRIPTION` shows UEXPS in PASSIVE state. After `@RT UEXPS`, the second listing shows it has moved to a queue (running).

---

## User Management

### UE User Concepts

UE has its own user system **separate from** SINTRAN users:

| Concept | Description |
|---------|-------------|
| **UE-user** | A user identity in the UE system. Name can differ from SINTRAN username |
| **UE-password** | Separate password required for UE login (in addition to any SINTRAN password) |
| **UE-profile** | Defines what menus, programs, and SINTRAN user areas the UE-user can access |
| **SINTRAN user area** | A UE-user can be authorized to access ONE or MORE SINTRAN user areas |

### UE Profile Manager (UE-PMAN)

User profiles are managed through the **UE Profile Manager** (UE-PMAN). This tool allows the system administrator to:

- Create and delete UE users
- Set and change UE passwords
- Define which SINTRAN user areas each UE user can access
- Configure menu structures per user or user group
- Set access restrictions (terminal numbers, IP addresses in version E)

Detailed UE-PMAN procedures are documented in **ND-60.194** (User Environment Reference Manual) which is not in this repository.

### Password Security (Version E Enhancements)

UE version E adds these password security features:

| Feature | Description |
|---------|-------------|
| Old password required | Must enter current password when changing to a new one |
| Password history | Can prevent reuse of recent passwords |
| System-generated passwords | Option for automatic password generation |
| Minimum change interval | Can set minimum time between password changes |
| IP address logging | Login IP address recorded in activity log |
| IP access control | Can restrict login by IP address (similar to terminal restriction) |
| Remote system ID | TAD remote system ID logged in activity log |

---

## SINTRAN Commands Related to UE

### UE-Specific Commands

| Command | User | Description |
|---------|------|-------------|
| `@UE-AUTOMATIC-LOGIN` | SYSTEM | Enable/disable UE on terminals |
| `@UE-FUNC <subcommand>` | SYSTEM | ADP program for UE administration |
| `@WAIT-FOR-UE` | SYSTEM | Wait for UE server to be ready (Version E, in mode files) |

### User Area Commands (Used When Setting Up UE)

| Command | Description |
|---------|-------------|
| `@CREATE-USER <[directory:]username>` | Create a SINTRAN user area |
| `@GIVE-USER-SPACE <[directory:]username> <pages>` | Allocate disk space to user |
| `@DELETE-USER <[directory:]username>` | Remove a user area |
| `@SET-FILE-ACCESS` | Set file permissions |
| `@SET-INITIAL-FILE-ACCESS` | Set default permissions for new files |

---

## Monitor Calls for Programmers

### ExecuteCommand (UECOM - MON 317₈)

Execute a SINTRAN III command from within a program. This is the recommended way to call SINTRAN commands programmatically (preferred over the older CallCommand/COMND).

**Key features:**
- Program does **not** terminate if the command fails (unlike CallCommand)
- An error message is output if an error occurs
- Missing parameters are prompted for
- Maximum command buffer: 150 characters (M-version and later)

**Parameters:** Command string with parameters (do NOT include the `@` character)

**FORTRAN example:**
```fortran
CHARACTER Command*35
Command = 'LIST-FILES *'
Monitor_Call('ExecuteCommand', Command(1:35))
```

**PASCAL example:**
```pascal
Command : PACKED ARRAY [0..34] OF CHAR;
Command := 'LIST-FILES *';
ExecuteCommand(Command);
```

**PLANC example:**
```planc
BYTES : Command(0:35)
Command := 'LIST-FILES *'
Monitor_Call('ExecuteCommand', Command)
```

### Internal UE Monitor Calls

These are for UE internal use and should not be called directly by applications:

| Monitor Call | Number | Purpose |
|-------------|--------|---------|
| **UELogin** (UELOG) | 320₈ | UE login processing |
| **UEAdministrator** (UEADM) | 321₈ | UE administration functions |
| **UELAMUFunction** (ADP) | 342₈ | UE LAMU (Logical Application Management Unit) functions |

---

## Developing Programs for the UE Menu System

### ASSUMPTION: Limited Documentation Available

The detailed UE menu programming API is documented in **ND-60.194** (User Environment Reference Manual), which is **not available in this repository**. What follows is what can be determined from the available documentation.

### How Programs Integrate with UE

Programs appear in UE menus as configured through the UE Profile Manager. When a user selects a menu item, UE:

1. Switches to the appropriate SINTRAN user area (`@UE-FUNC CHANGE-USER-AREA`)
2. Starts the program

### Using ExecuteCommand (UECOM) from Programs

Programs running within a UE session can execute SINTRAN commands through the `ExecuteCommand` monitor call (MON 317₈). This is the primary programmatic interface.

```fortran
C     Execute a SINTRAN command from within a UE program
      CHARACTER CMD*150
      CMD = 'LIST-FILES *:PROG'
      CALL MON(317B, CMD)
```

### COSMOS TELNET Integration

Programs like the COSMOS TELNET Client can be started either from the UE menu or directly from SINTRAN:

```
@telnet-client
```

When started from UE, the program inherits the UE session context.

### Remote Login via COSMOS Scripts

UE can be accessed remotely through COSMOS scripts. A typical UE remote login script:

```
*SCRIPT /DEFAULT/
*INPUT: CONNECT-TO SCHOLAR
*MACRO: LOGIN-DEFAULT /UE/
*INPUT: SCRIPT-USER
*INPUT: SECRET-PASSWORD
*DISPLAY-ON:
*INPUT: CC END OF UE-LOGIN
*ENDSCRIPT:
```

**Requirement:** UE must be enabled on ALL TADs (Terminal Access Devices) on the remote system.

If UE is not enabled on the remote TAD, you get:
```
**User Environment not active on TAD no. nnn on remote system: xxxxxx**
```

Fix: User SYSTEM must run `@UE-AUTOMATIC-LOGIN` for the TADs on the remote system.

### For Full Menu Development

To develop programs that fully integrate with the UE menu system (creating custom menus, menu items, handling menu navigation), you need:

1. **ND-60.194** - User Environment Reference Manual (primary API documentation)
2. **ND-210518** - The UE/File Manager product package
3. Access to UE-PMAN for configuring menu structures

The UE menu system is configured through UE-PMAN, not through programming. Programs are *registered* in menus by the administrator, and UE handles the menu presentation and navigation.

---

## Error Reporting

### Watchdog Integration (Version E)

All UE error messages are sent to the SINTRAN III Watchdog. Error codes are in the range:

```
SEC range: 016100₈ to 016177₈ (User Environment system module)
```

These appear in the Watchdog error log and can be viewed with the Watchdog Manager Program.

---

## Quick Reference

### Start UE After Warm Start

```
@UE-FUNC-EN START-PROFILE-MANAGER
```

### Stop UE Before System Shutdown

```
@UE-FUNC-EN STOP-PROFILE-MANAGER
```

### Disable UE on Console (Terminal 1)

```
@UE-AUTOMATIC-LOGIN N,0,1
```

### Disable UE on All Terminals

```
@UE-AUTOMATIC-LOGIN Y,0
```

### Enable UE on All Terminals

```
@UE-AUTOMATIC-LOGIN Y,1
```

### Check if UEXPS is Running

```
@LIST-RT-DESCRIPTION UEXPS
```

If status shows PASSIVE, UE is not started. If it shows a queue position, UE is running.

### Start UE Directly (Without UE-FUNC)

```
@RT UEXPS
```

---

## Sources

- ND-30.003.7 EN SINTRAN III System Supervisor, Sections 1.4, 3.2, 3.3, Appendix H
- ND-860228-2 EN SINTRAN III Monitor Calls (UECOM 317₈, UELOG 320₈, UEADM 321₈)
- ND-860230-8 EN SINTRAN III Release Information N-version (UE Version E)
- ND-60.230.01 SINTRAN III J-version Release Information (UE-AUTOMATIC-LOGIN introduction)
- ND-60.230.5 SINTRAN III K-version Release Information
- ND-860230-7A EN SINTRAN III Release Information M-version (audit trail integration)
- ND-860289-2 EN ND Linker User Guide (UE-FUNC CHANGE-USER-AREA examples)

### Not Available in This Repository

- **ND-60.194** - User Environment Reference Manual (detailed UE-PMAN procedures, menu configuration, full API)
- **ND-30.048** - Software Security Handbook
