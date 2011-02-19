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

gorc /r beesfx\gui\source\main-gui.rc

lazbuild -B beesfx\source\beesfx-console.lpi
lazbuild -B beesfx\source\beesfx-gui.lpi

delp beesfx\distribution
delp beesfx\temp