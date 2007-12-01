@echo off

  call clear.bat
  del  temp\BeeSfx.exe
  del  ..\..\BeeGui\distribution\BeeSfx.bin

  
@echo @ 
@echo @ Compile BeeSfx.exe ...
@echo @ 
  
  call resbuild.bat
  cd source
  bds -b -ns BeeSfx.dpr
  cd .. 

  stripreloc /B temp\BeeSfx.exe

  move temp\BeeSfx.exe ..\..\BeeGui\distribution\BeeSfx.bin

  upx -9 ..\..\BeeGui\distribution\BeeSfx.bin    

  call clear.bat
  