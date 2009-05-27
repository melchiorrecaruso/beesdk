@echo off

  call clear.bat
  del  temp\beesfx.exe
  del  ..\..\bee\distribution\beeSfx.bin
  
@echo @ 
@echo @ Compile BeeSfx.exe ...
@echo @ 
  
  cd source
  bds -b -ns beesfx.dpr -K4194304
  cd .. 

  stripreloc /B temp\beesfx.exe

  move temp\beesfx.exe ..\..\bee\distribution\beeSfx.bin  

  upx -9 ..\..\bee\distribution\beesfx.bin   

  call clear.bat
  