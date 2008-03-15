
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));

    // ---

    Storage.WriteString('TForm', 'PasswordFrm.Caption', Caption);

    // ---

    Storage.WriteString('TLabel', 'KeyLabel.Caption', KeyLabel.Caption);
    Storage.WriteString('TLabel', 'ConfirmKeyLabel.Caption', ConfirmKeyLabel.Caption);
    
    Storage.WriteString('TCheckBox', 'MaskKey.Caption', MaskKey.Caption);
    
    Storage.WriteString('TBitBtn', 'BtnClear.Caption', BtnClear.Caption);
    Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);

    // ---

    Storage.Free;
  end;

