@echo off

  call clear.bat
  del  temp\bee.exe
  del  distribution\bee.exe
  
echo *** 
echo *** Compile Bee ...
echo *** 

  cd source
  bds -b -ns bee.dpr
  cd ..

  move temp\bee.exe distribution\bee.exe
  
  call stripreloc.bat
  call clear.bat
  