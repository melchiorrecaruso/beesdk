@echo off

  call clear.bat
  del  temp\BeeOpt.exe
  del  distribution\BeeOpt.exe
  
echo *** 
echo *** Compile BeeOpt.exe ...
echo *** 

  cd source
  dcc32 BeeOpt.dpr
  cd ..

  copy temp\BeeOpt.exe distribution\BeeOpt.exe
  
  call clear.bat
  