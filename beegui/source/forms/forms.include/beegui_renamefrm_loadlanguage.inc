
  procedure TRenameFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
      begin
        Caption := Storage.ReadString('TForm','Caption', Caption);

        FromFNLabel.Caption := Storage.ReadString('TLabel','FromFNLabel', FromFNLabel.Caption);
        ToFNLabel.Caption   := Storage.ReadString('TLabel','ToFNLabel', ToFNLabel.Caption);

        BtnAbort.Caption  := Storage.ReadString('TBitBtn','BtnAbort', BtnAbort.Caption);
        BtnCancel.Caption := Storage.ReadString('TBitBtn','BtnCancel', BtnCancel.Caption);
        BtnOk.Caption     := Storage.ReadString('TBitBtn','BtnOk', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

