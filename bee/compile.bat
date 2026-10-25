@echo off
rem Builds bee.exe with lazbuild into ..\build (x86_64) or ..\build\i386.
rem   compile.bat [x86_64 | i386]       (default: x86_64)
rem LAZARUS may point to the Lazarus folder; otherwise lazbuild.exe is taken
rem from ..\..\fpcup\lazarus or from PATH.
setlocal
pushd "%~dp0source"

if /i "%~1"=="" (
  set "BEE_MODE=--cpu=x86_64 --os=win64 --build-mode=Release"
) else if /i "%~1"=="x86_64" (
  set "BEE_MODE=--cpu=x86_64 --os=win64 --build-mode=Release"
) else if /i "%~1"=="i386" (
  set "BEE_MODE=--build-mode=Release-i386"
) else (
  echo Unknown target %~1: use x86_64 or i386.
  goto fail
)

if not defined LAZARUS set "LAZARUS=%~dp0..\..\fpcup\lazarus"
set "BEE_LAZBUILD=%LAZARUS%\lazbuild.exe"
if not exist "%BEE_LAZBUILD%" (
  where lazbuild.exe >nul 2>nul || (
    echo lazbuild not found: set LAZARUS.
    goto fail
  )
  set "BEE_LAZBUILD=lazbuild.exe"
  set "LAZARUS="
)

if defined LAZARUS (
  "%BEE_LAZBUILD%" --lazarusdir="%LAZARUS%" %BEE_MODE% bee.lpi
) else (
  "%BEE_LAZBUILD%" %BEE_MODE% bee.lpi
)
if errorlevel 1 goto fail

popd
exit /b 0

:fail
popd
exit /b 1
