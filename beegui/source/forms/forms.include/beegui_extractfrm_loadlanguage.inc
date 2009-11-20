
  procedure TExtractFrm.LoadLanguage;
  var
    F: string;
    Storage: TMemIniFile;
  begin
    F := IncludeTrailingBackSlash(GetApplicationLanguageDir);;
    if ForceDirectories(F) then
    begin
      Storage := TMemIniFile.Create(F + ('extractfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);
        FolderLabel.Caption := Storage.ReadString('TLabel', 'FolderLabel.Caption', FolderLabel.Caption);
      
        oOptionLabel.Caption := Storage.ReadString('TLabel', 'oOptionLabel.Caption', oOptionLabel.Caption);
        oOption.Items[0]     := Storage.ReadString('TLabel', 'oOption[0]', oOption.Items[0]);
        oOption.Items[1]     := Storage.ReadString('TLabel', 'oOption[1]', oOption.Items[1]);
        oOption.Items[2]     := Storage.ReadString('TLabel', 'oOption[2]', oOption.Items[2]);

        Options.Caption       := Storage.ReadString('TGroupBox', 'Options.Caption', Options.Caption);
        xCommand.Caption      := Storage.ReadString('TCheckBox', 'xCommand.Caption', xCommand.Caption);
        cdOptionCheck.Caption := Storage.ReadString('TCheckBox', 'cdOption.Caption', cdOptionCheck.Caption);

        BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
	BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
