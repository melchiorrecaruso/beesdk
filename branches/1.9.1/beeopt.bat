@echo @ 
@echo @ Compile BeeOPT ...
@echo @

lazbuild -B beeopt\source\beeopt.lpi

delp beeopt\distribution
delp beeopt\temp