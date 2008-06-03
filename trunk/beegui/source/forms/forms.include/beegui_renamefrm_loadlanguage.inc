
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
    begin
      Caption := Storage.ReadString('TForm','Caption', Caption);

      FromFNLabel.Caption := Storage.ReadString('TLabel','FromFNLabel', FromFNLabel.Caption);
      ToFNLabel.Caption   := Storage.ReadString('TLabel','ToFNLabel', ToFNLabel.Caption);    
    
	  BtnCancel.Caption := Storage.ReadString('TBitBtn','BtnCancel', BtnCancel.Caption);
      BtnOk.Caption     := Storage.ReadString('TBitBtn','BtnOk', BtnOk.Caption);
    end;
    Storage.Free;
  end;

