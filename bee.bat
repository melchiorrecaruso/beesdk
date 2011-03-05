@echo @ 
@echo @ Compile Bee\BeeLIB ...
@echo @

lazbuild -B bee\source\bee.lpi
lazbuild -B bee\source\beelib.lpi

delp bee\distribution
delp bee\temp