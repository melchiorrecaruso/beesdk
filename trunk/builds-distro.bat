@echo Off

@echo @ 
@echo @ Create BeeGui distribution...
@echo @ 

move    beegui\temp\beegui.exe  beegui\distribution\beegui.exe


@echo @ 
@echo @ Create BeeLib distribution...
@echo @ 

move    bee\temp\beelib.dll  beegui\distribution\beelib.dll

@echo @ 
@echo @ Create Bee distribution...
@echo @ 

move    bee\temp\bee.exe  bee\distribution\bee.exe


@echo @ 
@echo @ Create BeeOpt distribution...
@echo @ 

move    beeopt\temp\beeopt.exe  beeopt\distribution\beeopt.exe


@echo @ 
@echo @ Create BeeSfx distribution...
@echo @ 

upx  -9 beesfx\console\temp\beesfx.exe
copy    beesfx\console\temp\beesfx.exe  bee\distribution\bee.sfx
move    beesfx\console\temp\beesfx.exe  beegui\distribution\bee.sfx

upx  -9 beesfx\gui\temp\beesfx.exe
copy    beesfx\gui\temp\beesfx.exe  bee\distribution\beegui.sfx
move    beesfx\gui\temp\beesfx.exe  beegui\distribution\beegui.sfx