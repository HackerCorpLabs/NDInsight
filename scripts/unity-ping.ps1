# Is the Unity MCP bridge actually up? Answer in one call, with the REAL reason.
#
# Why this exists: on 2026-08-08 the bridge detached and `execute_code` returned
#     {"success":false,"message":null,"data":null}
# - no reason at all - about eight times over roughly 40 minutes before anyone
# thought to try a different tool. `read_console` is the one that returns a
# useful error:
#     "Unity session not ready for 'read_console' (ping not answered); please retry"
#
# So: ALWAYS run this before a batch of Unity work. It checks the three links in
# the chain separately, so the failure points at which one is down.
#
#   Claude  ->  HTTP 8088 (uvx python server)  ->  WebSocket  ->  Unity Editor
#
# Usage:  .\scripts\unity-ping.ps1

$ErrorActionPreference = 'Continue'
$url = 'http://127.0.0.1:8088/mcp'

# --- link 1: is the Unity Editor process even running? ---
# Note: there can be MORE THAN ONE Unity process (a restart, or a second
# project open), so force an array and report them all. Indexing a scalar as
# $u[0] is legal in PowerShell, but dividing an ARRAY is not - that bug bit on
# the first run of this script.
$unity = @(Get-Process Unity -ErrorAction SilentlyContinue)
if ($unity.Count -eq 0) {
    Write-Output "DOWN: the Unity Editor is not running."
    Write-Output "      Ask Ronny to open E:\Dev\Ronny\UnityDev\NorskData - never start or kill it yourself."
    exit 1
}
for ($i = 0; $i -lt $unity.Count; $i++) {
    Write-Output ("editor  : running, PID {0}, {1:N0} MB" -f $unity[$i].Id, ($unity[$i].WorkingSet64 / 1MB))
}

# --- link 2: is the python MCP server listening? ---
$tcp = Test-NetConnection -ComputerName 127.0.0.1 -Port 8088 -WarningAction SilentlyContinue
if (-not $tcp.TcpTestSucceeded) {
    Write-Output "DOWN: nothing listening on 127.0.0.1:8088 - the MCP server is not started."
    Write-Output "      Start it from the MCP for Unity window in the Editor, or run:"
    Write-Output '      C:\Users\ronny\.local\bin\uvx.exe --prerelease explicit --from "mcpforunityserver>=0.0.0a0" mcp-for-unity --transport http --http-url http://127.0.0.1:8088'
    exit 1
}
Write-Output "server  : listening on 127.0.0.1:8088"

# --- link 3: is the Editor attached to that server? ---
# read_console is used deliberately - it is the tool that reports the real
# reason when the session is not ready. execute_code just returns a bare false.
function Post-Json($body, $sessionId) {
    $req = [System.Net.HttpWebRequest]::Create($url)
    $req.Method = 'POST'
    $req.ContentType = 'application/json'
    $req.Accept = 'application/json, text/event-stream'
    $req.Timeout = 30000
    if ($sessionId) { $req.Headers.Add('mcp-session-id', $sessionId) }
    $bytes = [System.Text.Encoding]::UTF8.GetBytes($body)
    $req.ContentLength = $bytes.Length
    $s = $req.GetRequestStream(); $s.Write($bytes, 0, $bytes.Length); $s.Close()
    $resp = $req.GetResponse()
    $reader = New-Object System.IO.StreamReader($resp.GetResponseStream())
    $text = $reader.ReadToEnd(); $reader.Close()
    return @{ Body = $text; Session = $resp.Headers['mcp-session-id'] }
}

try {
    $init = '{"jsonrpc":"2.0","id":1,"method":"initialize","params":{"protocolVersion":"2024-11-05","capabilities":{},"clientInfo":{"name":"unity-ping","version":"1.0"}}}'
    $r1 = Post-Json $init $null
    $sid = $r1.Session
    if (-not $sid) { Write-Output "DOWN: server gave no mcp-session-id."; exit 1 }
    $null = Post-Json '{"jsonrpc":"2.0","method":"notifications/initialized"}' $sid

    $call = '{"jsonrpc":"2.0","id":2,"method":"tools/call","params":{"name":"read_console","arguments":{"action":"get","types":["error"],"count":1}}}'
    $r3 = Post-Json $call $sid
    $body = $r3.Body
}
catch {
    Write-Output "DOWN: HTTP call to the MCP server failed: $_"
    exit 1
}

if ($body -match 'ping not answered') {
    Write-Output "DOWN: the Editor is NOT attached to the server (ping not answered)."
    Write-Output "      Retrying will not fix it. Ask Ronny to open Window > MCP for Unity"
    Write-Output "      and press Connect - that is the ONLY thing that reattaches the Editor."
    Write-Output "      To stop this recurring, turn on 'Auto-Start Server on Editor Load'"
    Write-Output "      in that window's Advanced Settings."
    exit 1
}
if ($body -match '"success":\s*true' -or $body -match 'log entries') {
    Write-Output "bridge  : ATTACHED - Unity is answering."
    Write-Output ""
    Write-Output "all three links up. Safe to run Unity work."
    exit 0
}

Write-Output "UNCLEAR: unexpected reply from the server -"
Write-Output $body
exit 1
