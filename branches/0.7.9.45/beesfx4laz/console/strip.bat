call clear.bat

cd distribution

strip beesfx.exe
upx -9 beesfx.exe

copy beesfx.exe beesfx.bin

del beesfx.exe