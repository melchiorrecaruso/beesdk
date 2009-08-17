@echo Off

@echo @ 
@echo @ Clear BeeGui temp files...
@echo @ 

del beegui\temp\*.exe       /S >nul
del beegui\temp\*.~*        /S >nul
del beegui\temp\*.bak       /S >nul
del beegui\temp\*.o         /S >nul
del beegui\temp\*.or        /S >nul
del beegui\temp\*.ppu       /S >nul
del beegui\temp\*.a         /S >nul
del beegui\temp\*.compiled  /S >nul 
del beegui\temp\*.lsr       /S >nul 
del beegui\temp\*.res       /S >nul


@echo @ 
@echo @ Clear BeeLib temp files...
@echo @ 

del beelib\temp\*.dll       /S >nul
del beelib\temp\*.~*        /S >nul
del beelib\temp\*.bak       /S >nul
del beelib\temp\*.o         /S >nul
del beelib\temp\*.or        /S >nul
del beelib\temp\*.ppu       /S >nul
del beelib\temp\*.a         /S >nul
del beelib\temp\*.compiled  /S >nul 
del beelib\temp\*.lsr       /S >nul 
del beelib\temp\*.res       /S >nul


@echo @ 
@echo @ Clear Bee temp files...
@echo @   

del bee\temp\*.exe       /S >nul
del bee\temp\*.~*        /S >nul
del bee\temp\*.bak       /S >nul
del bee\temp\*.o         /S >nul
del bee\temp\*.or        /S >nul
del bee\temp\*.ppu       /S >nul
del bee\temp\*.a         /S >nul
del bee\temp\*.compiled  /S >nul 
del bee\temp\*.lsr       /S >nul 
del bee\temp\*.res       /S >nul


@echo @ 
@echo @ Clear BeeOpt temp files...
@echo @  

del beeopt\temp\*.exe       /S >nul
del beeopt\temp\*.~*        /S >nul
del beeopt\temp\*.bak       /S >nul
del beeopt\temp\*.o         /S >nul
del beeopt\temp\*.or        /S >nul
del beeopt\temp\*.ppu       /S >nul
del beeopt\temp\*.a         /S >nul
del beeopt\temp\*.compiled  /S >nul 
del beeopt\temp\*.lsr       /S >nul 
del beeopt\temp\*.res       /S >nul


@echo @ 
@echo @ Clear BeeSfx temp files...
@echo @  

del beesfx\console\temp\*.exe       /S >nul
del beesfx\console\temp\*.~*        /S >nul
del beesfx\console\temp\*.bak       /S >nul
del beesfx\console\temp\*.o         /S >nul
del beesfx\console\temp\*.or        /S >nul
del beesfx\console\temp\*.ppu       /S >nul
del beesfx\console\temp\*.a         /S >nul
del beesfx\console\temp\*.compiled  /S >nul 
del beesfx\console\temp\*.lsr       /S >nul 
del beesfx\console\temp\*.res       /S >nul

del beesfx\gui\temp\*.exe       /S >nul
del beesfx\gui\temp\*.~*        /S >nul
del beesfx\gui\temp\*.bak       /S >nul
del beesfx\gui\temp\*.o         /S >nul
del beesfx\gui\temp\*.or        /S >nul
del beesfx\gui\temp\*.ppu       /S >nul
del beesfx\gui\temp\*.a         /S >nul
del beesfx\gui\temp\*.compiled  /S >nul 
del beesfx\gui\temp\*.lsr       /S >nul 
del beesfx\gui\temp\*.res       /S >nul