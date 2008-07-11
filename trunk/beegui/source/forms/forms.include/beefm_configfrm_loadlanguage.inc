
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('configfrm.ini'));
    begin
      Caption := Storage.ReadString('TForm', 'Caption', Caption);

      Tree.Items[0].Text := Storage.ReadString('TTreeNode', 'Tree.Items[0]' ,Tree.Items[0].Text);
      Tree.Items[1].Text := Storage.ReadString('TTreeNode', 'Tree.Items[1]' ,Tree.Items[1].Text);
      
      AddPage.Caption := Storage.ReadString('TGroupBox', 'AddPage.Caption' ,AddPage.Caption);
      AddingOptions.Caption := Storage.ReadString('TGroupBox', 'AddingsOption.Caption' ,AddingOptions.Caption);
      
      ExtractPage.Caption := Storage.ReadString('TGroupBox', 'ExtractPage.Caption' ,ExtractPage.Caption);
      ExtractingOptions.Caption := Storage.ReadString('TGroupBox', 'ExtractingOptions.Caption' ,ExtractingOptions.Caption);

      mOption.Caption  := Storage.ReadString('TLabel', 'mOption.Caption' ,mOptionLabel.Caption);
      mOption.Items[0] := Storage.ReadString('TComboBox', 'mOption[0]' ,mOption.Items[0]);
      mOption.Items[1] := Storage.ReadString('TComboBox', 'mOption[1]' ,mOption.Items[1]);
      mOption.Items[2] := Storage.ReadString('TComboBox', 'mOption[2]' ,mOption.Items[2]);
      mOption.Items[3] := Storage.ReadString('TComboBox', 'mOption[3]' ,mOption.Items[3]);

      dOption.Caption  := Storage.ReadString('TLabel', 'dOption.Caption' ,dOptionLabel.Caption);
      dOption.Items[0] := Storage.ReadString('TComboBox', 'dOption[0]' ,dOption.Items[0]);
      dOption.Items[1] := Storage.ReadString('TComboBox', 'dOption[1]' ,dOption.Items[1]);
      dOption.Items[2] := Storage.ReadString('TComboBox', 'dOption[2]' ,dOption.Items[2]);
      dOption.Items[3] := Storage.ReadString('TComboBox', 'dOption[3]' ,dOption.Items[3]);
      dOption.Items[4] := Storage.ReadString('TComboBox', 'dOption[4]' ,dOption.Items[4]);
      dOption.Items[5] := Storage.ReadString('TComboBox', 'dOption[5]' ,dOption.Items[5]);
      dOption.Items[6] := Storage.ReadString('TComboBox', 'dOption[6]' ,dOption.Items[6]);
      dOption.Items[7] := Storage.ReadString('TComboBox', 'dOption[7]' ,dOption.Items[7]);
      dOption.Items[8] := Storage.ReadString('TComboBox', 'dOption[8]' ,dOption.Items[8]);
      dOption.Items[9] := Storage.ReadString('TComboBox', 'dOption[9]' ,dOption.Items[9]);
      
      oOption.Caption := Storage.ReadString('TComboBox', 'oOption.Caption' ,oOptionLabel.Caption);
      oOption.Items[0] := Storage.ReadString('TComboBox', 'oOption[0]' ,oOption.Items[0]);
      oOption.Items[1] := Storage.ReadString('TComboBox', 'oOption[1]' ,oOption.Items[1]);
      oOption.Items[2] := Storage.ReadString('TComboBox', 'oOption[2]' ,oOption.Items[2]);
      
      rOption.Caption := Storage.ReadString('TCheckBox', 'rOption.Caption',   rOption.Caption);
      sOption.Caption := Storage.ReadString('TCheckBox', 'sOption.Caption',   sOption.Caption);
      kOption.Caption := Storage.ReadString('TCheckBox', 'kOption.Caption',   kOption.Caption);
      tOption.Caption := Storage.ReadString('TCheckBox', 'tOption.Caption',   tOption.Caption);
      lOption.Caption := Storage.ReadString('TCheckBox', 'lOption.Caption',   lOption.Caption);
      cdAOption.Caption := Storage.ReadString('TCheckBox', 'cdAOption.Caption', cdAOption.Caption);
      xCommand.Caption := Storage.ReadString('TCheckBox', 'xCommand.Caption',  xCommand.Caption);
      cdEOption.Caption := Storage.ReadString('TCheckBox', 'cdEOption.Caption', cdEOption.Caption);
    end;
    Storage.Free;
  end;

