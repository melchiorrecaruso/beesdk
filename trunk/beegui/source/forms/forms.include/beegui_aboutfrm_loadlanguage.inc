
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));
    begin
	  Caption            := Storage.ReadString('TForm', 'Caption', Caption);
      BtnLicense.Caption := Storage.ReadString('TBitBtn', 'BtnLicense.Caption', BtnLicense.Caption);
      BtnOk.Caption      := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    end;
    Storage.Free;
  end;