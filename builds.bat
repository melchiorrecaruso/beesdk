@echo Off

@echo @ 
@echo @ Compile BeeGui...
@echo @

cd beegui\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew -Fiforms\forms.include\ -Fipackages\packages.include\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\win32\ -Fu..\..\..\..\lazarus\packager\units\i386-win32\ -Fu. -FU..\temp\ -FE..\temp\ -obeegui.exe -dLCL -dLCLwin32 beegui.lpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeLib...
@echo @

cd beelib\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX  -l -vew -Fiforms\forms.include\ -Fipackages\packages.include\ -Fi..\temp\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeelib.dll beelib.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile Bee...
@echo @

cd bee\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -OpPENTIUMM -Pi386 -XX -vew -Fibee.include\ -Fu. -FU..\temp\ -FE..\temp\ -obee.exe bee.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeOpt...
@echo @

cd beeopt\source

fpc -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vew -l -Fibeeopt.include\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeeopt.exe beeopt.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeSfx...
@echo @

cd beesfx\console\source

fpc -MDelphi -Sh -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -vew -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..

cd beesfx\gui\source

gorc /r main.rc

fpc -MDelphi -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..