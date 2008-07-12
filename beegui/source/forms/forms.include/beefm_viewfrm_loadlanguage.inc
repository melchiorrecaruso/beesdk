
  procedure TViewFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('viewfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);

        BtnFont.Caption := Storage.ReadString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
        BtnSave.Caption := Storage.ReadString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
        BtnOk.Caption   := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
