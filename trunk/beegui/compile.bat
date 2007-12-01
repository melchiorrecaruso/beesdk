@echo off

  call clear.bat
  del  temp\BeeGui.exe
  del  distribution\BeeGui.exe
  
@echo @ 
@echo @ Compile BeeGui ...
@echo @ 

  cd source
  bds -b -ns BeeGui.dpr
  cd ..

  move temp\BeeGui.exe distribution\BeeGui.exe

  call stripreloc.bat  
  call clear.bat
  