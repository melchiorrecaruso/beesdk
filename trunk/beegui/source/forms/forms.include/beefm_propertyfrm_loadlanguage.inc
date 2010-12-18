
  procedure TInfoFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('propertyfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);

        AName    .Caption := Storage.ReadString('TLabel', 'AName.Caption',     AName.Caption);
        AVersion .Caption := Storage.ReadString('TLabel', 'AVersion.Caption',  AVersion.Caption);
        AFiles   .Caption := Storage.ReadString('TLabel', 'AFiles.Caption',    AFiles.Caption);
        ASize    .Caption := Storage.ReadString('TLabel', 'ASize.Caption',     ASize.Caption);
        APacked  .Caption := Storage.ReadString('TLabel', 'APacked.Caption',   APacked.Caption);
        ARatio   .Caption := Storage.ReadString('TLabel', 'ARatio.Caption',    ARatio.Caption);
        ACrypted .Caption := Storage.ReadString('TLabel', 'ACrypted.Caption',  ACrypted.Caption);
        AArcSize .Caption := Storage.ReadString('TLabel', 'AArcSize.Caption',  AArcSize.Caption);
        AModified.Caption := Storage.ReadString('TLabel', 'AModified.Caption', AModified.Caption);
      
        FName     .Caption := Storage.ReadString('TLabel', 'FName.Caption',      FName.Caption);
        FVersion  .Caption := Storage.ReadString('TLabel', 'FVersion.Caption',   FVersion.Caption);
        FSize     .Caption := Storage.ReadString('TLabel', 'FSize.Caption',      FSize.Caption);
        FPacked   .Caption := Storage.ReadString('TLabel', 'FPacked.Caption',    FPacked.Caption);
        FRatio    .Caption := Storage.ReadString('TLabel', 'FRatio.Caption',     FRatio.Caption);
        FAttribute.Caption := Storage.ReadString('TLabel', 'FAttribute.Caption', FAttribute.Caption);
        FPassword .Caption := Storage.ReadString('TLabel', 'FPassword.Caption',  FPassword.Caption);
        FMethod   .Caption := Storage.ReadString('TLabel', 'FMethod.Caption',    FMethod.Caption);
        FModified .Caption := Storage.ReadString('TLabel', 'FModified.Caption',  FModified.Caption);
      
        BtnOk.Caption := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
