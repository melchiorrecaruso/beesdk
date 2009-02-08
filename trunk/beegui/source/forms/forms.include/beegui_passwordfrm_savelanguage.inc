
  procedure tPasswordFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);

        Storage.WriteString('TLabel', 'PasswordLabel.Caption', PasswordLabel.Caption);
        Storage.WriteString('TLabel', 'ConfirmPasswordLabel.Caption', ConfirmPasswordLabel.Caption);
        Storage.WriteString('TCheckBox', 'MaskPassword.Caption', MaskPassword.Caption);

        Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
        Storage.WriteString('TBitBtn', 'BtnClear.Caption', BtnClear.Caption);
        Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
