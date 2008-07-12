
  procedure TPasswordFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);

        PasswordLabel.Caption        := Storage.ReadString('TLabel', 'PasswordLabel.Caption', PasswordLabel.Caption);
        ConfirmPasswordLabel.Caption := Storage.ReadString('TLabel', 'ConfirmPasswordLabel.Caption', ConfirmPasswordLabel.Caption);
        MaskPassword.Caption         := Storage.ReadString('TCheckBox', 'MaskPassword.Caption', MaskPassword.Caption);
      
        BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
        BtnClear.Caption  := Storage.ReadString('TBitBtn', 'BtnClear.Caption', BtnClear.Caption);
        BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

