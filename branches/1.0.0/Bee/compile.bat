@echo off

  call clear.bat
  del  temp\Bee.exe
  del  distribution\Bee.exe
  
echo *** 
echo *** Compile Bee ...
echo *** 

  cd source
  dcc32 Bee.dpr -K4194304
  cd ..

  move temp\Bee.exe distribution\Bee.exe
  
  call clear.bat
  