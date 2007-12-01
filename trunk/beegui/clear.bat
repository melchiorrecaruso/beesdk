@Echo Off

@echo @ 
@echo @ Deleting temporary files ...
@echo @ 

  del *.~*  /S >nul
  del *.dcu /S >nul
  del *.drc /S >nul
  del *.dsm /S >nul
  del *.obj /S >nul
  del *.map /S >nul
  del *.$$$ /S >nul
  del *.bee /S >nul
  del *.o   /S >nul
  del *.ppu /S >nul
  
  del *.err        /S >nul
  del *.local      /S >nul
  del *.identcache /S >nul


