
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));

    Storage.WriteString('TLabel','From', FromFNLabel.Caption);
    Storage.WriteString('TLabel','To', ToFNLabel.Caption);

    Storage.WriteString('TBitBtn','BtnCancel', BtnCancel.Caption);
    Storage.WriteString('TBitBtn','BtnOk', BtnOk.Caption);

    Storage.Free;
  end;

