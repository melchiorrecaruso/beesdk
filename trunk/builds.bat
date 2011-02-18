@echo Off

@echo @ 
@echo @ Compile BeeGUI ...
@echo @

lazbuild -B beegui\source\beegui.lpi

delp beegui\distribution
delp beegui\temp


@echo @ 
@echo @ Compile Bee\BeeLIB ...
@echo @

lazbuild -B bee\source\bee.lpi
lazbuild -B bee\source\beelib.lpi

delp bee\distribution
delp bee\temp


@echo @ 
@echo @ Compile BeeOPT ...
@echo @

lazbuild beeopt\source\beeopt.lpi

delp beeopt\source\distribution
delp beeopt\source\temp


@echo @ 
@echo @ Compile BeeSFX...
@echo @

gorc /r beesfx\gui\source\main.rc

lazbuild -B beesfx\console\source\beesfx.lpi
lazbuild -B beesfx\gui\source\beesfx.lpi

delp beesfx\console\distribution
delp beesfx\gui\distribution
delp beesfx\console\temp
delp beesfx\gui\temp