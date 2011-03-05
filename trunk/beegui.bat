@echo @ 
@echo @ Compile BeeGUI ...
@echo @

lazbuild -B beegui\source\beegui.lpi

delp beegui\distribution
delp beegui\temp