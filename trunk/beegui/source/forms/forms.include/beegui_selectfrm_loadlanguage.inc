  procedure TSelectFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('selectfrm.ini'));
      begin
        Caption            := Storage.ReadString('TForm', 'Caption', Caption);

        // ---

        BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
        BtnOk.Caption      := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
