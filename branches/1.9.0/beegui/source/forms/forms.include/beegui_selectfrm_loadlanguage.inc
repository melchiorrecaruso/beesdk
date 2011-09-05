  procedure TSelectFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('selectfrm.ini'));
      begin
        MaskLabel.Caption := Storage.ReadString('TLabel',  'MaskLabel.Caption', MaskLabel.Caption);

        BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
        BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption',     BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
