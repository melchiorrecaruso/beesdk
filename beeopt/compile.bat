@echo off
rem Builds beeopt.exe (x86_64) with lazbuild into ..\build.
rem LAZARUS may point to the Lazarus folder; otherwise lazbuild.exe is taken
rem from PATH or from ..\..\fpcup\lazarus.
setlocal
pushd "%~dp0"

if not defined LAZARUS set "LAZARUS=%~dp0..\..\fpcup\lazarus"
set "BEEOPT_LAZBUILD=%LAZARUS%\lazbuild.exe"
if not exist "%BEEOPT_LAZBUILD%" (
  where lazbuild.exe >nul 2>nul || (
    echo lazbuild not found: set LAZARUS.
    goto fail
  )
  set "BEEOPT_LAZBUILD=lazbuild.exe"
  set "LAZARUS="
)

if defined LAZARUS (
  "%BEEOPT_LAZBUILD%" --lazarusdir="%LAZARUS%" --cpu=x86_64 --os=win64 --build-mode=Release beeopt.lpi
) else (
  "%BEEOPT_LAZBUILD%" --cpu=x86_64 --os=win64 --build-mode=Release beeopt.lpi
)
if errorlevel 1 goto fail

popd
exit /b 0

:fail
popd
exit /b 1
