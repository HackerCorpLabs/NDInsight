# Drives a live SINTRAN terminal over a RetroCore TCP terminal port, in ONE connection.
#
# Why one connection: reconnecting while a program is running wedges the line. So the whole
# interaction - ESC, login, commands, logout - happens on a single socket opened once and closed
# once at the end.
#
# The port is a raw TCP line interface, not a real telnet server, so there is no option
# negotiation: write bytes, read bytes.
#
#   .\ndterm.ps1 -Port 9010 -User SYSTEM -Steps "LIST-FILES","D19999(sys).",""
#
# Each element of -Steps is sent as one line followed by CR, and whatever SINTRAN prints back is
# captured before the next line goes out. A blank element sends a bare CR, which is how you accept
# a default at a prompt (for example  OUTPUT FILE:  -> terminal).
#
# TIMING MATTERS. Some commands take many seconds and say so - START-NET-SERVER prints
# "wait 10 sec!" - and a fixed delay that is too short sends the next command INTO the busy one,
# which garbles both. -WaitFor solves this properly: it waits for a prompt to come back instead of
# guessing a duration. Use it for anything slow.
param(
    [Parameter(Mandatory = $true)][int]$Port,
    [string]$User = "SYSTEM",
    [string]$Password = "",
    [string[]]$Steps = @(),
    [int]$SettleMs = 900,
    [int]$LoginSettleMs = 1500,
    [int]$OpenWaitMs = 800,

    # When set, each step waits for this text to appear (the prompt) rather than for a fixed time.
    # Example: -WaitFor 'X-C:' for the XMSG command program, '@' for the SINTRAN command prompt.
    [string]$WaitFor = "",
    [int]$WaitForTimeoutMs = 120000,

    # ONE PROMPT PER STEP, parallel to -Steps. Use this when the prompts CHANGE from step to step,
    # which -WaitFor cannot express because it is a single string for the whole run.
    #
    # WHY IT EXISTS, measured on D100 2026-08-23: the RT-LOADER answers with a different prompt at
    # almost every step - "*" between loads, "CHATSER REPLACING?" when the description exists, then
    # RT-PROGRAM, PRIORITY, SEGMENT ONE and so on from CHANGE-RT-DESCRIPTION. With no prompt to wait
    # for, every step fell back to the fixed settle, the script ran AHEAD of the loader, and the
    # transcript shows the result: "LOAD LOAD PLANC-1BANK-F00" and "NO SUCH FILE NAME", MON-CALL
    # never loaded at all, and END-LOAD then eating EXIT-LOADER, RT and LOGOUT as answers to
    # "NEGLECTING REFERENCES?". That left the loader holding a terminal and the machine had to be
    # rebooted. A fixed delay is not a wait; it is a bet.
    #
    # An empty entry, or a step past the end of the array, falls back to the settle as before.
    # Several alternatives can be given for one step, separated by "|", because the loader asks
    # "REPLACING?" only when there is something to replace.
    [string[]]$StepWaits = @(),

    # How long ONE step waits for its own prompt. Shorter than WaitForTimeoutMs on purpose: a
    # loader prompt that has not appeared in this long is not slow, it is out of step, and the
    # useful thing then is to stop and print the transcript rather than to keep typing into it.
    [int]$StepWaitTimeoutMs = 60000,

    # How long the login waits for each of ITS OWN prompts (ENTER, then PASSWORD:). Short, because
    # a login that has not prompted within this long is wedged, not slow.
    [int]$LoginPromptTimeoutMs = 45000,

    [switch]$NoLogin,
    [switch]$NoLogout,
    [string]$RemoteHost = "127.0.0.1"
)

$client = New-Object System.Net.Sockets.TcpClient
$client.Connect($RemoteHost, $Port)
$client.NoDelay = $true
$stream = $client.GetStream()
$buffer = New-Object byte[] 65536

# Reads everything currently available, waiting first for the machine to produce it. The inner
# pause keeps reading while output is still trickling in - a SINTRAN reply arrives in several
# small writes, not one.
function Read-Available([int]$waitMs) {
    Start-Sleep -Milliseconds $waitMs
    $text = ""
    while ($stream.DataAvailable) {
        $n = $stream.Read($buffer, 0, $buffer.Length)
        if ($n -le 0) { break }
        $text += [System.Text.Encoding]::ASCII.GetString($buffer, 0, $n)
        Start-Sleep -Milliseconds 120
    }
    return $text
}

# Reads until $prompt appears or the timeout expires. Returns everything read either way, so a
# timeout still shows what the machine DID say - which is the useful part when a step misbehaves.
function Read-UntilPrompt([string]$prompt, [int]$timeoutMs) {
    $text = ""
    $waited = 0
    while ($waited -lt $timeoutMs) {
        Start-Sleep -Milliseconds 200
        $waited += 200
        while ($stream.DataAvailable) {
            $n = $stream.Read($buffer, 0, $buffer.Length)
            if ($n -le 0) { break }
            $text += [System.Text.Encoding]::ASCII.GetString($buffer, 0, $n)
        }
        # Only accept the prompt at the very end of what we have, so a prompt echoed in the
        # middle of a mode file's output does not end the wait early.
        if ($text.TrimEnd().EndsWith($prompt)) { return $text }
    }

    return $text + "`r`n*** ndterm: timed out waiting for '$prompt' ***"
}

# Reads until ANY of $prompts appears at the end of what we have, or the timeout expires. Returns
# the text read and which prompt matched ("" on timeout).
#
# Needed because after ESC the line can be in one of TWO states: freshly woken, showing the
# SINTRAN banner and ENTER; or ALREADY LOGGED IN from an earlier session, sitting at the @ command
# prompt. Waiting only for ENTER burns the full timeout in the second case - which happened on
# 2026-08-04 and wasted 90 seconds before the commands ran at all.
function Read-UntilAny([string[]]$prompts, [int]$timeoutMs) {
    $text = ""
    $waited = 0
    while ($waited -lt $timeoutMs) {
        Start-Sleep -Milliseconds 200
        $waited += 200
        while ($stream.DataAvailable) {
            $n = $stream.Read($buffer, 0, $buffer.Length)
            if ($n -le 0) { break }
            $text += [System.Text.Encoding]::ASCII.GetString($buffer, 0, $n)
        }
        $tail = $text.TrimEnd()
        foreach ($p in $prompts) {
            if ($tail.EndsWith($p)) {
                return [pscustomobject]@{ Text = $text; Matched = $p }
            }
        }
    }

    return [pscustomobject]@{ Text = $text + "`r`n*** ndterm: timed out waiting for any of: $($prompts -join ', ') ***"; Matched = "" }
}

function Send-Line([string]$line, [int]$waitMs, [string]$stepWait = "") {
    $bytes = [System.Text.Encoding]::ASCII.GetBytes($line + "`r")
    $stream.Write($bytes, 0, $bytes.Length)
    $stream.Flush()

    # A prompt named for THIS step wins over the run-wide -WaitFor: it is more specific, and it is
    # the only one that can follow a loader whose prompt changes as it goes.
    if ($stepWait -ne "") {
        $alternatives = $stepWait -split '\|'
        $got = Read-UntilAny $alternatives $StepWaitTimeoutMs
        if ($got.Matched -eq "") {
            # Say WHICH step lost the thread. Without this the transcript shows a pile of confused
            # output and nothing points at the line that caused it.
            return $got.Text + "`r`n*** ndterm: step [$line] never saw its prompt [$stepWait] - STOPPING ***"
        }

        return $got.Text
    }

    if ($WaitFor -ne "") {
        return (Read-UntilPrompt $WaitFor $WaitForTimeoutMs)
    }

    return (Read-Available $waitMs)
}

Write-Output "--- on connect ---"
Write-Output (Read-Available $OpenWaitMs)

# ESC first. A fresh connection shows only the RetroCore banner; ESC produces the SINTRAN banner
# and the ENTER prompt. ESC also recovers a wedged line.
$stream.Write([byte[]](0x1B), 0, 1)
$stream.Flush()

if (-not $NoLogin) {
    # LOGIN WAITS FOR ITS OWN PROMPTS. It does not honour -WaitFor (that names the COMMAND prompt,
    # which does not appear until after the login) and it does NOT use a fixed delay either.
    #
    # WHY, learned the hard way on 2026-08-04: with fixed delays this raced. D100 was busy right
    # after an XMSG restart, its banner arrived late, and the user name went out before the ENTER
    # prompt existed. Everything after that was one step out of phase - the password answered
    # ENTER, the first real command answered PASSWORD: - so the session never left the login loop
    # and all 13 X-C commands each burned their full 120s timeout. 28 minutes, no work done, and
    # the transcript looked like the ND was at fault when it was this script.
    Write-Output "--- after ESC, waiting for ENTER (or @ if already logged in) ---"
    $woke = Read-UntilAny @("ENTER", "@") $LoginPromptTimeoutMs
    Write-Output $woke.Text

    if ($woke.Matched -eq "@") {
        # Already logged in from an earlier session. Do NOT send the user name - it would be run as
        # a command. Just carry on at the command prompt.
        Write-Output "--- already logged in, skipping login ---"
    }
    else {
        $bytes = [System.Text.Encoding]::ASCII.GetBytes($User + "`r")
        $stream.Write($bytes, 0, $bytes.Length)
        $stream.Flush()
        Write-Output "--- login: $User ---"
        Write-Output (Read-UntilPrompt "PASSWORD:" $LoginPromptTimeoutMs)

        $bytes = [System.Text.Encoding]::ASCII.GetBytes($Password + "`r")
        $stream.Write($bytes, 0, $bytes.Length)
        $stream.Flush()
        Write-Output (Read-Available $LoginSettleMs)
    }
}
else {
    Write-Output "--- after ESC ---"
    Write-Output (Read-Available $OpenWaitMs)
}

for ($stepIndex = 0; $stepIndex -lt $Steps.Count; $stepIndex++) {
    $step = $Steps[$stepIndex]

    # A short -StepWaits is not an error: the steps past its end simply fall back to the settle.
    $stepWait = ""
    if ($stepIndex -lt $StepWaits.Count) { $stepWait = $StepWaits[$stepIndex] }

    Write-Output "===> [$step]"
    $answer = Send-Line $step $SettleMs $stepWait
    Write-Output $answer

    # STOP THE MOMENT A STEP LOSES ITS PROMPT. Carrying on is what turns one missed prompt into a
    # loader eating every following command as an answer - see the note on -StepWaits. The caller
    # gets the transcript up to the break, which is where the fault actually is.
    if ($answer -match 'STOPPING \*\*\*') { break }
}

if (-not $NoLogout) {
    Write-Output "===> [LOGOUT]"
    Write-Output (Send-Line "LOGOUT" $SettleMs)
}

$stream.Close()
$client.Close()
