@echo @ 
@echo @ Compile Bee\BeeLib ...
@echo @

lazbuild -B bee\source\bee.lpi
lazbuild -B bee\source\beelib.lpi

delp bee\temp
delp bee\distribution
