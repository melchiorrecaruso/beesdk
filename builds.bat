@echo Off

@echo @ 
@echo @ Compile BeeGui..
@echo @

cd beegui\source

lazbuild -B beegui.lpi

cd ..
cd ..


@echo @ 
@echo @ Compile Bee\BeeLib...
@echo @

cd bee\source

lazbuild -B bee.lpi
lazbuild -B beelib.lpi

cd ..
cd ..


@echo @ 
@echo @ Compile BeeOpt...
@echo @

cd beeopt\source

lazbuild beeopt.lpi

cd ..
cd ..


@echo @ 
@echo @ Compile BeeSfx...
@echo @

cd beesfx\console\source

lazbuild beesfx.lpi

cd ..
cd ..
cd ..

cd beesfx\gui\source

gorc /r main.rc

lazbuild beesfx.lpi

cd ..
cd ..
cd ..