  procedure TAboutFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);
  	Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
        Storage.WriteString('TBitBtn', 'BtnLicense.Caption', BtnLicense.Caption);
      end;
      Storage.Free;
    end;
  end;

