
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));

    // ---
   
    FromFNLabel.Caption := Storage.ReadString('TLabel','From', FromFN.Caption);
    ToFNLabel.Caption   := Storage.ReadString('TLabel','To', ToFN.Caption);
    
    BtnCancel.Caption := Storage.ReadString('TBitBtn','BtnCancel', BtnCancel.Caption);
    BtnOk.Caption     := Storage.ReadString('TBitBtn','BtnOk', BtnOk.Caption);
   
    // ---

    Storage.Free;
  end;

