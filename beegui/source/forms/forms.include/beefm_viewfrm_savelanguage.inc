  
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('viewfrm.ini'));
    begin
      Storage.WriteString('TForm', 'Caption', Caption);

      Storage.WriteString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
      Storage.WriteString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
      Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    end;
    Storage.Free;
  end;

