@Echo Off

@echo @ 
@echo @ Compile BeeGui...
@echo @

cd beegui\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -WG -vewnhi -Fiforms\forms.include\ -Fipackages\packages.include\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\win32\ -Fu..\..\..\..\lazarus\packager\units\i386-win32\ -Fu. -FU..\temp\ -FE..\temp\ -obeegui.exe -dLCL -dLCLwin32 beegui.lpr

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vewnhi -l -Fiforms\forms.include\ -Fipackages\packages.include\ -Fi..\temp\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeelib.dll beelib.dpr

cd ..
cd ..

@echo @ 
@echo @ Create BeeGui distribution...
@echo @ 

strip.exe    beegui\temp\beegui.exe
move         beegui\temp\beegui.exe  beegui\distribution\beegui.exe
move         beegui\temp\beelib.dll  beegui\distribution\beelib.dll

@echo @ 
@echo @ Deleting temporary files...
@echo @  

del beegui\temp\*.~*        /S >nul
del beegui\temp\*.bak       /S >nul
del beegui\temp\*.o         /S >nul
del beegui\temp\*.or        /S >nul
del beegui\temp\*.ppu       /S >nul
del beegui\temp\*.a         /S >nul
del beegui\temp\*.compiled  /S >nul 
del beegui\temp\*.lsr       /S >nul 
del beegui\temp\*.res       /S >nul


@echo @ 
@echo @ Compile Bee...
@echo @

cd bee\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -OpPENTIUMM -Pi386 -XX -vewnh -Fibee.include\ -Fu. -FU..\temp\ -FE..\temp\ -obee.exe bee.dpr

cd ..
cd ..

@echo @ 
@echo @ Create Bee distribution...
@echo @ 

strip.exe    bee\temp\bee.exe
move         bee\temp\bee.exe  bee\distribution\bee.exe

@echo @ 
@echo @ Deleting temporary files...
@echo @  

del bee\temp\*.~*        /S >nul
del bee\temp\*.bak       /S >nul
del bee\temp\*.o         /S >nul
del bee\temp\*.or        /S >nul
del bee\temp\*.ppu       /S >nul
del bee\temp\*.a         /S >nul
del bee\temp\*.compiled  /S >nul 
del bee\temp\*.lsr       /S >nul 
del bee\temp\*.res       /S >nul

@echo @ 
@echo @ Compile BeeOpt...
@echo @

cd beeopt\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vewnhi -l -Fibeeopt.include\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeeopt.exe beeopt.dpr

cd ..
cd ..

@echo @ 
@echo @ Create BeeOpt distribution...
@echo @ 

strip.exe    beeopt\temp\beeopt.exe
move         beeopt\temp\beeopt.exe  beeopt\distribution\beeopt.exe

@echo @ 
@echo @ Deleting temporary files...
@echo @  

del beeopt\temp\*.~*        /S >nul
del beeopt\temp\*.bak       /S >nul
del beeopt\temp\*.o         /S >nul
del beeopt\temp\*.or        /S >nul
del beeopt\temp\*.ppu       /S >nul
del beeopt\temp\*.a         /S >nul
del beeopt\temp\*.compiled  /S >nul 
del beeopt\temp\*.lsr       /S >nul 
del beeopt\temp\*.res       /S >nul

@echo @ 
@echo @ Compile BeeSfx...
@echo @

cd beesfx\console\source

fpc -MDelphi -Sh -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -vewnhi -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..

cd beesfx\gui\source

gorc /r main.rc

fpc -MDelphi -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -WG -vewnhi -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..

@echo @ 
@echo @ Create BeeSfx distribution...
@echo @ 

strip.exe    beesfx\console\temp\beesfx.exe
upx       -9 beesfx\console\temp\beesfx.exe
copy         beesfx\console\temp\beesfx.exe  bee\distribution\bee.sfx
move         beesfx\console\temp\beesfx.exe  beegui\distribution\bee.sfx

strip.exe    beesfx\gui\temp\beesfx.exe
upx       -9 beesfx\gui\temp\beesfx.exe
copy         beesfx\gui\temp\beesfx.exe  bee\distribution\beegui.sfx
move         beesfx\gui\temp\beesfx.exe  beegui\distribution\beegui.sfx

@echo @ 
@echo @ Deleting temporary files...
@echo @  

del beesfx\console\temp\*.~*        /S >nul
del beesfx\console\temp\*.bak       /S >nul
del beesfx\console\temp\*.o         /S >nul
del beesfx\console\temp\*.or        /S >nul
del beesfx\console\temp\*.ppu       /S >nul
del beesfx\console\temp\*.a         /S >nul
del beesfx\console\temp\*.compiled  /S >nul 
del beesfx\console\temp\*.lsr       /S >nul 
del beesfx\console\temp\*.res       /S >nul

del beesfx\gui\temp\*.~*        /S >nul
del beesfx\gui\temp\*.bak       /S >nul
del beesfx\gui\temp\*.o         /S >nul
del beesfx\gui\temp\*.or        /S >nul
del beesfx\gui\temp\*.ppu       /S >nul
del beesfx\gui\temp\*.a         /S >nul
del beesfx\gui\temp\*.compiled  /S >nul 
del beesfx\gui\temp\*.lsr       /S >nul 
del beesfx\gui\temp\*.res       /S >nul

