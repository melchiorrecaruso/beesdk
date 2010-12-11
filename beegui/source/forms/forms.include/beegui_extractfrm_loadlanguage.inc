
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
      
        uOptionLabel.Caption := Storage.ReadString('TLabel', 'uOptionLabel.Caption', uOptionLabel.Caption);
        uOption.Items[0] := Storage.ReadString('TComboBox', 'uOption[0]', uOption.Items[0]);
        uOption.Items[1] := Storage.ReadString('TComboBox', 'uOption[1]', uOption.Items[1]);
        uOption.Items[2] := Storage.ReadString('TComboBox', 'uOption[2]', uOption.Items[2]);
        uOption.Items[3] := Storage.ReadString('TComboBox', 'uOption[3]', uOption.Items[3]);
        uOption.Items[4] := Storage.ReadString('TComboBox', 'uOption[4]', uOption.Items[4]);
        uOption.Items[5] := Storage.ReadString('TComboBox', 'uOption[5]', uOption.Items[5]);

        Options.Caption       := Storage.ReadString('TGroupBox', 'Options.Caption', Options.Caption);
        xCommand.Caption      := Storage.ReadString('TCheckBox', 'xCommand.Caption', xCommand.Caption);
        cdOptionCheck.Caption := Storage.ReadString('TCheckBox', 'cdOption.Caption', cdOptionCheck.Caption);

        BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
	BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

