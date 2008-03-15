
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));

    // ---
    
    Storage.WriteString('TLabel','From', FromFN.Caption);
    Storage.WriteString('TLabel','To', ToFN.Caption);

    Storage.WriteString('TBitBtn','BtnCancel', BtnCancel.Caption);
    Storage.WriteString('TBitBtn','BtnOk', BtnOk.Caption);

    // ---

    Storage.Free;
  end;

