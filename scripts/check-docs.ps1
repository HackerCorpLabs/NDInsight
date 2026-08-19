# Check markdown docs for the two rules that keep getting broken.
#
# 1. Absolute paths in markdown LINKS.
#    CLAUDE.md: "NEVER use absolute paths in markdown files, ALWAYS use relative
#    paths for internal links". This checks LINK TARGETS only - `[x](E:\...)` -
#    and deliberately does NOT flag absolute paths written in prose or code
#    blocks, because Ronny explicitly requires full paths when a document points
#    at a file on disk. Flagging those would fight a real requirement.
#
# 2. HTML entity encoding in text, e.g. "&gt;" instead of ">".
#    Called out directly: never use HTML encoding in comments or docs.
#
# Usage:
#   .\scripts\check-docs.ps1                  # whole repo, report only
#   .\scripts\check-docs.ps1 -Path SINTRAN    # a subtree
#   .\scripts\check-docs.ps1 -FailOnFind      # exit 1 if anything found (for hooks/CI)

param(
    [string]$Path = "",
    [switch]$FailOnFind,
    # Numeric entities like &#8323; (subscript 3) are usually genuine CONTENT in
    # an OCR'd manual, not an encoding mistake, so they are off by default.
    # The named ones (&gt; &lt; &amp; &quot;) are always wrong in our own text.
    [switch]$IncludeNumeric
)

$ErrorActionPreference = 'Stop'

$repo = Split-Path -Parent (Split-Path -Parent $MyInvocation.MyCommand.Path)
if ([string]::IsNullOrWhiteSpace($Path)) { $Path = $repo }
elseif (-not [System.IO.Path]::IsPathRooted($Path)) { $Path = Join-Path $repo $Path }

# A markdown link whose target starts with a drive letter or a unix root.
# Matches [text](E:\...), [text](C:/...), [text](/abs/path)
$linkAbs = [regex]'\[[^\]]*\]\(\s*(?<t>([A-Za-z]:[\\/]|/)[^)]*)\)'
# HTML entities that should be plain characters.
if ($IncludeNumeric) { $entity = [regex]'&(gt|lt|amp|quot|#\d+);' }
else                 { $entity = [regex]'&(gt|lt|amp|quot);' }

$files = Get-ChildItem -Path $Path -Filter *.md -Recurse -File -ErrorAction SilentlyContinue |
    Where-Object { $_.FullName -notmatch '\\\.git\\' -and $_.FullName -notmatch '\\node_modules\\' }

$absHits = 0
$entHits = 0
$fileCount = 0

foreach ($f in $files) {
    $fileCount++
    $lines = Get-Content -LiteralPath $f.FullName -Encoding UTF8 -ErrorAction SilentlyContinue
    if ($null -eq $lines) { continue }

    $inFence = $false
    for ($i = 0; $i -lt $lines.Count; $i++) {
        $line = $lines[$i]

        # Track fenced code blocks - a path inside a shell example is fine.
        if ($line -match '^\s*```') { $inFence = -not $inFence; continue }
        if ($inFence) { continue }

        $m = $linkAbs.Matches($line)
        if ($m.Count -gt 0) {
            foreach ($hit in $m) {
                $absHits++
                $rel = $f.FullName.Substring($repo.Length).TrimStart('\')
                Write-Output ("ABSOLUTE LINK  {0}:{1}" -f $rel, ($i + 1))
                Write-Output ("               -> {0}" -f $hit.Groups['t'].Value)
            }
        }

        $e = $entity.Matches($line)
        if ($e.Count -gt 0) {
            foreach ($hit in $e) {
                $entHits++
                $rel = $f.FullName.Substring($repo.Length).TrimStart('\')
                Write-Output ("HTML ENTITY    {0}:{1}  {2}" -f $rel, ($i + 1), $hit.Value)
            }
        }
    }
}

Write-Output ""
Write-Output ("markdown files scanned : {0}" -f $fileCount)
Write-Output ("absolute links         : {0}" -f $absHits)
Write-Output ("html entities          : {0}" -f $entHits)

if ($FailOnFind -and ($absHits + $entHits) -gt 0) { exit 1 }
exit 0
