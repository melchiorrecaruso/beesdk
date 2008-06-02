
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
    
    Caption := Storage.ReadString('TForm','Caption', Caption);

    FromFNLabel.Caption := Storage.ReadString('TLabel','From', FromFNLabel.Caption);
    ToFNLabel.Caption   := Storage.ReadString('TLabel','To', ToFNLabel.Caption);
    
    BtnCancel.Caption := Storage.ReadString('TBitBtn','BtnCancel', BtnCancel.Caption);
    BtnOk.Caption     := Storage.ReadString('TBitBtn','BtnOk', BtnOk.Caption);
   
    Storage.Free;
  end;

