@echo Off

@echo @ 
@echo @ Create BeeGui distribution...
@echo @ 

strip.exe    beegui\temp\beegui.exe
move         beegui\temp\beegui.exe  beegui\distribution\beegui.exe


@echo @ 
@echo @ Create BeeLib distribution...
@echo @ 

copy         beelib\temp\beelib.dll  beegui\distribution\beelib.dll
move         beelib\temp\beelib.dll  beelib\distribution\beelib.dll


@echo @ 
@echo @ Create Bee distribution...
@echo @ 

strip.exe    bee\temp\bee.exe
move         bee\temp\bee.exe  bee\distribution\bee.exe


@echo @ 
@echo @ Create BeeOpt distribution...
@echo @ 

strip.exe    beeopt\temp\beeopt.exe
move         beeopt\temp\beeopt.exe  beeopt\distribution\beeopt.exe


@echo @ 
@echo @ Create BeeSfx distribution...
@echo @ 

strip.exe    beesfx\console\temp\beesfx.exe
upx       -9 beesfx\console\temp\beesfx.exe
copy         beesfx\console\temp\beesfx.exe  bee\distribution\bee.sfx
move         beesfx\console\temp\beesfx.exe  beegui\distribution\bee.sfx

strip.exe    beesfx\gui\temp\beesfx.exe
upx       -9 beesfx\gui\temp\beesfx.exe
copy         beesfx\gui\temp\beesfx.exe  bee\distribution\beegui.sfx
move         beesfx\gui\temp\beesfx.exe  beegui\distribution\beegui.sfx