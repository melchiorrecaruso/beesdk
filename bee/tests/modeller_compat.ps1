param(
    [Parameter(Mandatory=$true)][string]$InputFile,
    [string]$Table = 'default',
    [ValidateRange(0,9)][int]$Dictionary = 0,
    [ValidateSet('x86_64','i386')][string]$Architecture = 'x86_64'
)
$ErrorActionPreference = 'Stop'
$repo = (Resolve-Path "$PSScriptRoot/../..").Path
$inputPath = (Resolve-Path -LiteralPath $InputFile).Path
$tablePath = if ($Table -eq 'default') { 'default' } else { (Resolve-Path -LiteralPath $Table).Path }
$outputDir = Join-Path $repo ('build/modeller-compat-' + [Guid]::NewGuid().ToString('N'))
New-Item -ItemType Directory "$outputDir/units" | Out-Null
$original = @(git -C $repo show '5c2c183d1801b67ee3c43485025299eea39ba748:bee/source/bee_modeller.pas')
if ($LASTEXITCODE -ne 0) { throw 'Cannot read Bee 0.7.9 reference from Git' }
$reference = [string]::Join("`r`n", $original) + "`r`n"
$reference = $reference.Replace('unit Bee_Modeller;', 'unit Bee_Modeller_079;')
$reference = $reference.Replace('{$I compiler.inc}', '{$MODE DELPHI}' + "`r`n" + '{$POINTERMATH ON}' + "`r`n" + '{$I compiler.inc}')
# On Win64 the original four-byte pointer move must follow pointer width.
$reference = $reference.Replace('MoveCardinalUnchecked(List[1], List[0], ListCount - 1)', 'MovePointerUnchecked(List[1], List[0], ListCount - 1)')
[IO.File]::WriteAllText("$outputDir/bee_modeller_079.pas", $reference, [Text.Encoding]::ASCII)
$target = if ($Architecture -eq 'i386') { 'win32' } else { 'win64' }
& fpc '-B' "-P$Architecture" "-T$target" '-MObjFPC' '-Sh' '-O4' "-Fi$repo/bee/source/bee-include" "-Fu$repo/bee/source" "-Fu$outputDir" "-FU$outputDir/units" "-FE$outputDir" "$PSScriptRoot/modeller_compat.lpr" > "$outputDir/compile.log"
if ($LASTEXITCODE -ne 0) { throw "Compilation failed; see $outputDir/compile.log" }
& "$outputDir/modeller_compat.exe" $inputPath $tablePath $Dictionary
if ($LASTEXITCODE -ne 0) { throw 'Compatibility test failed' }
Write-Output "Test artifacts: $outputDir"
