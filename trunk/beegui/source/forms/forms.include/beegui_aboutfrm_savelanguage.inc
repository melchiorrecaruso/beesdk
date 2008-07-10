
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
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

