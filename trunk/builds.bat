@echo Off

@echo @ 
@echo @ Compile BeeGui...
@echo @

cd beegui\source

ppc386 -Fuforms\ -Fiforms\forms.include\ -Fupackages\ -Fipackages\packages.include\ -Fu..\..\bee\source\ -Fu..\..\beelib\source\packages\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\ -Fu..\..\..\..\lazarus\lcl\units\i386-win32\win32\ -Fu..\..\..\..\lazarus\packager\units\i386-win32\ -FE..\temp\ -dLCL -dLCLwin32 -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew beegui.lpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeLib...
@echo @

cd beelib\source

ppc386 -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -l -vew -Fu..\..\beegui\source\packages\ -Fiforms\forms.include\ -Fipackages\packages.include\ -Fi..\temp\ -Fuforms\ -Fupackages\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeelib.dll beelib.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile Bee...
@echo @

cd bee\source

ppc386 -FW..\temp\B1.wpo -OWdevirtcalls                                  -Fibee.include\ -FE..\temp\ -FU..\temp\ -MDelphi -CX -XX -Xs- -vew -al bee.dpr
ppc386 -FW..\temp\B2.wpo -OWdevirtcalls -Fw..\temp\B1.wpo -OWdevirtcalls -Fibee.include\ -FE..\temp\ -FU..\temp\ -MDelphi -CX -XX -Xs- -vew -al bee.dpr
ppc386 -FW..\temp\B2.wpo -OWdevirtcalls                                  -Fibee.include\ -FE..\temp\ -FU..\temp\ -MDelphi -CX -XX -Xs- -vew -al bee.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeOpt...
@echo @

cd beeopt\source

ppc386 -MDelphi -CX -O3 -OoUNCERTAIN -OoREGVAR -XX -vew -l -Fibeeopt.include\ -Fu..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeeopt.exe beeopt.dpr

cd ..
cd ..


@echo @ 
@echo @ Compile BeeSfx...
@echo @

cd beesfx\console\source

ppc386 -MDelphi -Sh -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -vew -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..

cd beesfx\gui\source

gorc /r main.rc

ppc386 -MDelphi -CX -Os3 -OoUNCERTAIN -OoREGVAR -XX -WG -vew -l -Fibeesfx.include\ -Fu..\..\..\bee\source\ -Fu. -FU..\temp\ -FE..\temp\ -obeesfx.exe beesfx.dpr

cd ..
cd ..
cd ..