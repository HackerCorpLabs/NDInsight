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

function Send-Line([string]$line, [int]$waitMs) {
    $bytes = [System.Text.Encoding]::ASCII.GetBytes($line + "`r")
    $stream.Write($bytes, 0, $bytes.Length)
    $stream.Flush()
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

foreach ($step in $Steps) {
    Write-Output "===> [$step]"
    Write-Output (Send-Line $step $SettleMs)
}

if (-not $NoLogout) {
    Write-Output "===> [LOGOUT]"
    Write-Output (Send-Line "LOGOUT" $SettleMs)
}

$stream.Close()
$client.Close()
