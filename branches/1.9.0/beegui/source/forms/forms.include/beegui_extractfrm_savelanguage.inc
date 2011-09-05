
  procedure TExtractFrm.SaveLanguage;
  var
    F: string;
    Storage: TMemIniFile;
  begin
    F := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(F) then
    begin
      Storage := TMemIniFile.Create(F + ('extractfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);
        Storage.WriteString('TLabel', 'FolderLabel.Caption', FolderLabel.Caption);

        Storage.WriteString('TLabel', 'uOptionLabel.Caption', uOptionLabel.Caption);
        Storage.WriteString('TComboBox', 'uOption[0]', uOption.Items[0]);
        Storage.WriteString('TComboBox', 'uOption[1]', uOption.Items[1]);
        Storage.WriteString('TComboBox', 'uOption[2]', uOption.Items[2]);
        Storage.WriteString('TComboBox', 'uOption[3]', uOption.Items[3]);
        Storage.WriteString('TComboBox', 'uOption[4]', uOption.Items[4]);
        Storage.WriteString('TComboBox', 'uOption[5]', uOption.Items[5]);
    
        Storage.WriteString('TGroupBox', 'Options.Caption', Options.Caption);
        Storage.WriteString('TCheckBox', 'xCommand.Caption', xCommand.Caption);
        Storage.WriteString('TCheckBox', 'cdOption.Caption', cdOptionCheck.Caption);

        Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
        Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

