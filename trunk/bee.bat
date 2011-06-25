@echo @ 
@echo @ Compile Bee\BeeLIB2 ...
@echo @

lazbuild -B bee\source\bee.lpi
lazbuild -B bee\source\beelib.lpi

delp bee\temp
delp bee\distribution
