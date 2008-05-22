
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));

    // ---

    Storage.WriteString('TForm', 'PasswordFrm.Caption', Caption);

    // ---

    Storage.WriteString('TLabel', 'PasswordLabel.Caption', PasswordLabel.Caption);
    Storage.WriteString('TLabel', 'ConfirmPasswordLabel.Caption', ConfirmPasswordLabel.Caption);
    
    Storage.WriteString('TCheckBox', 'MaskPassword.Caption', MaskPassword.Caption);
    
    Storage.WriteString('TBitBtn', 'BtnClear.Caption', BtnClear.Caption);
    Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);

    // ---

    Storage.Free;
  end;

