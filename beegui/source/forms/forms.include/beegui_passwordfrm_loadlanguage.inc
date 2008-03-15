
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));

    // ---

    Caption := Storage.ReadString('TForm', 'PasswordFrm.Caption', Caption);

    // ---

    KeyLabel.Caption        := Storage.ReadString('TLabel', 'KeyLabel.Caption', KeyLabel.Caption);
    ConfirmKeyLabel.Caption := Storage.ReadString('TLabel', 'ConfirmKeyLabel.Caption', ConfirmKeyLabel.Caption);

    MaskKey.Caption         := Storage.ReadString('TCheckBox', 'MaskKey.Caption', MaskKey.Caption);

    BtnClear.Caption  := Storage.ReadString('TBitBtn', 'BtnClear.Caption', BtnClear.Caption);
    BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);

    // ---

    Storage.Free;
  end;

