@echo off

  call clear.bat
  del  temp\ContextMenu.dll
  del  distribution\ContextMenu.dll
  
echo *** 
echo *** Compile ContextMenu.dll ...
echo *** 

  cd source
  bds -b -ns ContextMenu.dpr
  cd ..

  move temp\ContextMenu.dll distribution\ContextMenu.dll
    
  call clear.bat
  