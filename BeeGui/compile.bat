@echo off

  call clear.bat
  del  temp\BeeGui.exe
  del  distribution\BeeGui.exe
  
@echo @ 
@echo @ Compile BeeGui ...
@echo @ 

  cd source
  dcc32 BeeGui.dpr -K4194304
  cd ..

  move temp\BeeGui.exe distribution\BeeGui.exe
  
  call clear.bat
  