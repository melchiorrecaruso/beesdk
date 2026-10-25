@echo off
setlocal
pushd "%~dp0"

set "BEEOPT_FPC=fpc.exe"
where "%BEEOPT_FPC%" >nul 2>nul
if errorlevel 1 set "BEEOPT_FPC=C:\FPC\3.2.2\bin\i386-win32\fpc.exe"

if not exist "%BEEOPT_FPC%" (
  where "%BEEOPT_FPC%" >nul 2>nul
  if errorlevel 1 (
    echo Free Pascal Compiler not found.
    popd
    exit /b 1
  )
)

set "BEEOPT_LAZARUS=..\..\lazarus"
if not exist "%BEEOPT_LAZARUS%\lcl\units\x86_64-win64\forms.ppu" (
  echo Lazarus LCL Win64 units not found in %BEEOPT_LAZARUS%.
  popd
  exit /b 1
)

if not exist "..\build\units-gui" mkdir "..\build\units-gui"

"%BEEOPT_FPC%" -B -Px86_64 -Twin64 -WG -MObjFPC -Sh -O4 -XX -CX ^
  -Fi"..\bee\source\bee-include" ^
  -Fu"..\bee\source" ^
  -Fu"%BEEOPT_LAZARUS%\lcl\units\x86_64-win64" ^
  -Fu"%BEEOPT_LAZARUS%\lcl\units\x86_64-win64\win32" ^
  -Fu"%BEEOPT_LAZARUS%\components\lazutils\lib\x86_64-win64" ^
  -Fu"%BEEOPT_LAZARUS%\packager\units\x86_64-win64" ^
  -FU"..\build\units-gui" ^
  -FE"..\build" beeopt.lpr

set "BEEOPT_RESULT=%errorlevel%"
popd
exit /b %BEEOPT_RESULT%
