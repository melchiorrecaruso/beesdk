
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
    begin
      Storage.WriteString('TForm','Caption', Caption);
	  
      Storage.WriteString('TLabel','FromFNLabel', FromFNLabel.Caption);
      Storage.WriteString('TLabel','ToFNLabel', ToFNLabel.Caption);
	  
      Storage.WriteString('TBitBtn','BtnCancel', BtnCancel.Caption);
      Storage.WriteString('TBitBtn','BtnOk', BtnOk.Caption);
    end;
    Storage.Free;
  end;

