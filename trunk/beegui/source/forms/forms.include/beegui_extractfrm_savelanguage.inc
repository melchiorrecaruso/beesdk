
  // ---

  F := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));

    // ---

    Storage.WriteString('TForm', 'ExtractFrm.Caption', Caption);

    // ---
    
    Storage.WriteString('TLabel', 'FolderLabel.Caption', FolderLabel.Caption);

    Storage.WriteString('TLabel', 'oOptionLabel.Caption', oOptionLabel.Caption);
    Storage.WriteString('TLabel', 'oOption[0]', oOption.Items[0]);
    Storage.WriteString('TLabel', 'oOption[1]', oOption.Items[1]);
    Storage.WriteString('TLabel', 'oOption[2]', oOption.Items[2]);
    
    Storage.WriteString('TGroupBox', 'Options.Caption', Options.Caption);
    Storage.WriteString('TCheckBox', 'xCommand.Caption', xCommand.Caption);
    Storage.WriteString('TCheckBox', 'cdOption.Caption', cdOption.Caption);

    // ---

    Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
    
    // ---

    Storage.Free;
  end;

