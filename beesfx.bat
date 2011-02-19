@echo @ 
@echo @ Compile BeeSFX...
@echo @

gorc /r beesfx\source\main-gui.rc

lazbuild -B beesfx\source\beesfx-console.lpi
lazbuild -B beesfx\source\beesfx-gui.lpi

delp beesfx\distribution
delp beesfx\temp