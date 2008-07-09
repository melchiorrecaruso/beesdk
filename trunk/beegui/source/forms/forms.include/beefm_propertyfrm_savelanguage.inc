
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('propertyfrm.ini'));
    begin
      Storage.SaveString('TForm', 'Caption', Caption);

      Storage.SaveString('TTabSheet', 'APage.Caption', APage.Caption);
      Storage.SaveString('TTabSheet', 'FPage.Caption', FPage.Caption);

      Storage.SaveString('TLabel', 'AName.Caption',     AName.Caption);
      Storage.SaveString('TLabel', 'AVersion.Caption',  AVersion.Caption);
      Storage.SaveString('TLabel', 'AFiles.Caption',    AFiles.Caption);
      Storage.SaveString('TLabel', 'ASize.Caption',     ASize.Caption);
      Storage.SaveString('TLabel', 'APacked.Caption',   APacked.Caption);
      Storage.SaveString('TLabel', 'ARatio.Caption',    ARatio.Caption);
      Storage.SaveString('TLabel', 'ACrypted.Caption',  ACrypted.Caption);
      Storage.SaveString('TLabel', 'AArcSize.Caption',  AArcSize.Caption);
      Storage.SaveString('TLabel', 'AModified.Caption', AModified.Caption);
      
      Storage.SaveString('TLabel', 'FName.Caption',      FName.Caption);
      Storage.SaveString('TLabel', 'FVersion.Caption',   FVersion.Caption);
      Storage.SaveString('TLabel', 'FSize.Caption',      FSize.Caption);
      Storage.SaveString('TLabel', 'FPacked.Caption',    FPacked.Caption);
      Storage.SaveString('TLabel', 'FRatio.Caption',     FRatio.Caption);
      Storage.SaveString('TLabel', 'FAttribute.Caption', FAttribute.Caption);
      Storage.SaveString('TLabel', 'FPassword.Caption',  FPassword.Caption);
      Storage.SaveString('TLabel', 'FMethod.Caption',    FMethod.Caption);
      Storage.SaveString('TLabel', 'FModified.Caption',  FModified.Caption);
      
      Storage.SaveString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    end;
    Storage.Free;
  end;
