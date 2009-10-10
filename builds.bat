@echo Off

@echo @ 
@echo @ Compile BeeGui...
@echo @

cd beegui\source

fpc -FE..\temp\ -Fiforms\forms.include\ -Fipackages\packages.include\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu..\..\beelib\source\packages\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\win32\ -Fu..\..\..\..\lazarus\packager\units\i386-win32\ -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew -dLCL -dLCLwin32 beegui.lpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeLib...
@echo @

cd beelib\source

fpc -FE..\temp\ -Fiforms\forms.include\ -Fipackages\packages.include\ -Fi..\temp\ -Fu..\..\beegui\source\packages\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vew -obeelib.dll beelib.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile Bee...
@echo @

cd bee\source

fpc -FE..\temp\ -Fibee.include\ -FU..\temp\ -MDelphi -CX -XX -O3 -OoUNCERTAIN -OoREGVAR -vew bee.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeOpt...
@echo @

cd beeopt\source

fpc -FE..\temp\ -Fibeeopt.include\ -Fu..\..\bee\source\ -FU..\temp\ -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vew -obeeopt.exe beeopt.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeSfx...
@echo @

cd beesfx\console\source

fpc -FE..\temp\ -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -MDelphi -Sh -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -vew -l -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..

cd beesfx\gui\source

gorc /r main.rc

fpc -FE..\temp\ -Fibeesfx.include\ -Fu..\..\..\bee\source\ -FU..\temp\ -MDelphi -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..