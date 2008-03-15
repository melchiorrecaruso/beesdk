
  // ---

  F := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(F) then
  begin
    Storage := TMemIniFile.Create(F + ('extractfrm.ini'));

    // ---

    Caption := Storage.ReadString('TForm', 'ExtractFrm.Caption', Caption);

    // ---
    
    FolderLabel.Caption := Storage.ReadString('TLabel', 'FolderLabel.Caption', FolderLabel.Caption);

    oOptionLabel.Caption := Storage.ReadString('TLabel', 'oOptionLabel.Caption', oOptionLabel.Caption);
    oOption.Items[0] := Storage.ReadString('TLabel', 'oOption[0]', oOption.Items[0]);
    oOption.Items[1] := Storage.ReadString('TLabel', 'oOption[1]', oOption.Items[1]);
    oOption.Items[2] := Storage.ReadString('TLabel', 'oOption[2]', oOption.Items[2]);

    Options.Caption  := Storage.ReadString('TGroupBox', 'Options.Caption', Options.Caption);
    xCommand.Caption := Storage.ReadString('TCheckBox', 'xCommand.Caption', xCommand.Caption);
    cdOptionCheck.Caption := Storage.ReadString('TCheckBox', 'cdOption.Caption', cdOptionCheck.Caption);

    // ---

    BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
   
    // ---

    Storage.Free;
  end;

