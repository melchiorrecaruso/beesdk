@echo Off

@echo @ 
@echo @ Clear BeeGui distibution files...
@echo @ 

del beegui\distribution\*.exe       /S >nul
del beegui\distribution\*.~*        /S >nul
del beegui\distribution\*.dll       /S >nul
del beegui\distribution\*.sfx       /S >nul


@echo @ 
@echo @ Clear BeeLib distibution files...
@echo @ 

del beelib\distribution\*.exe       /S >nul
del beelib\distribution\*.~*        /S >nul
del beelib\distribution\*.dll       /S >nul
del beelib\distribution\*.sfx       /S >nul


@echo @ 
@echo @ Clear Bee distibution files...
@echo @   

del bee\distribution\*.exe       /S >nul
del bee\distribution\*.~*        /S >nul
del bee\distribution\*.dll       /S >nul
del bee\distribution\*.sfx       /S >nul


@echo @ 
@echo @ Clear BeeOpt distibution files...
@echo @  

del beeopt\distribution\*.exe       /S >nul
del beeopt\distribution\*.~*        /S >nul
del beeopt\distribution\*.dll       /S >nul
del beeopt\distribution\*.sfx       /S >nul


@echo @ 
@echo @ Clear BeeSfx distibution files...
@echo @  

del beesfx\console\distribution\*.exe       /S >nul
del beesfx\console\distribution\*.~*        /S >nul
del beesfx\console\distribution\*.dll       /S >nul
del beesfx\console\distribution\*.sfx       /S >nul


del beesfx\gui\distribution\*.exe       /S >nul
del beesfx\gui\distribution\*.~*        /S >nul
del beesfx\gui\distribution\*.dll       /S >nul
del beesfx\gui\distribution\*.sfx       /S >nul
