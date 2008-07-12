
  procedure TInfoFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('propertyfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);

        if Assigned(APage) then Storage.WriteString('TTabSheet', 'APage.Caption', APage.Caption);
        if Assigned(FPage) then Storage.WriteString('TTabSheet', 'FPage.Caption', FPage.Caption);
      
        Storage.WriteString('TLabel', 'AName.Caption',     AName.Caption);
        Storage.WriteString('TLabel', 'AVersion.Caption',  AVersion.Caption);
        Storage.WriteString('TLabel', 'AFiles.Caption',    AFiles.Caption);
        Storage.WriteString('TLabel', 'ASize.Caption',     ASize.Caption);
        Storage.WriteString('TLabel', 'APacked.Caption',   APacked.Caption);
        Storage.WriteString('TLabel', 'ARatio.Caption',    ARatio.Caption);
        Storage.WriteString('TLabel', 'ACrypted.Caption',  ACrypted.Caption);
        Storage.WriteString('TLabel', 'AArcSize.Caption',  AArcSize.Caption);
        Storage.WriteString('TLabel', 'AModified.Caption', AModified.Caption);
      
        Storage.WriteString('TLabel', 'FName.Caption',      FName.Caption);
        Storage.WriteString('TLabel', 'FVersion.Caption',   FVersion.Caption);
        Storage.WriteString('TLabel', 'FSize.Caption',      FSize.Caption);
        Storage.WriteString('TLabel', 'FPacked.Caption',    FPacked.Caption);
        Storage.WriteString('TLabel', 'FRatio.Caption',     FRatio.Caption);
        Storage.WriteString('TLabel', 'FAttribute.Caption', FAttribute.Caption);
        Storage.WriteString('TLabel', 'FPassword.Caption',  FPassword.Caption);
        Storage.WriteString('TLabel', 'FMethod.Caption',    FMethod.Caption);
        Storage.WriteString('TLabel', 'FModified.Caption',  FModified.Caption);

        Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
