@echo off

echo ***
echo *** Deleting temporary files ...
echo ***

  del *.~*  /S
  del *.dcu /S
  del *.drc /S
  del *.dsm /S
  del *.obj /S
  del *.map /S
  del *.$$$ /S
  del *.bee /S
  del *.ppu /S
  del *.o   /S
  del *.a   /S
  del *.bak /S
  del *.err /S
 
  del *.local      /S  
  del *.compiled   /S
  del *.identcache /S

  del link.res /S
  del ppas.bat /s