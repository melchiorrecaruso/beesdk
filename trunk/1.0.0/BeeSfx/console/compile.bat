@echo off

  call clear.bat
  del  temp\BeeSfx.exe
  del  ..\..\Bee\distribution\BeeSfx.bin
  
@echo @ 
@echo @ Compile BeeSfx.exe ...
@echo @ 
  
  cd source
  dcc32 BeeSfx.dpr -K4194304
  cd .. 

  stripreloc /B temp\BeeSfx.exe

  move temp\BeeSfx.exe ..\..\Bee\distribution\BeeSfx.bin  

  upx -9 ..\..\Bee\distribution\BeeSfx.bin   

  call clear.bat
  