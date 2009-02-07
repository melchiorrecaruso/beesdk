
  procedure TConfigFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('configfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);

        Storage.WriteString('TTreeNode', 'Tree.Items[0]' ,Tree.Items[0].Text);
        Storage.WriteString('TTreeNode', 'Tree.Items[1]' ,Tree.Items[1].Text);
        Storage.WriteString('TTreeNode', 'Tree.Items[2]' ,Tree.Items[2].Text);
      
        Storage.WriteString('TGroupBox', 'AddPage.Caption' ,AddPage.Caption);
        Storage.WriteString('TGroupBox', 'AddingsOption.Caption' ,AddingOptions.Caption);
      
        Storage.WriteString('TGroupBox', 'ExtractPage.Caption' ,ExtractPage.Caption);
        Storage.WriteString('TGroupBox', 'ExtractingOptions.Caption' ,ExtractingOptions.Caption);

        Storage.WriteString('TGroupBox', 'GeneralPage.Caption' ,GeneralPage.Caption);
        Storage.WriteString('TGroupBox', 'Buttons.Caption' ,Buttons.Caption);

        Storage.WriteString('TLabel', 'mOption.Caption' ,mOptionLabel.Caption);
        Storage.WriteString('TComboBox', 'mOption[0]' ,mOption.Items[0]);
        Storage.WriteString('TComboBox', 'mOption[1]' ,mOption.Items[1]);
        Storage.WriteString('TComboBox', 'mOption[2]' ,mOption.Items[2]);
        Storage.WriteString('TComboBox', 'mOption[3]' ,mOption.Items[3]);

        Storage.WriteString('TLabel', 'dOption.Caption' ,dOptionLabel.Caption);
        Storage.WriteString('TComboBox', 'dOption[0]' ,dOption.Items[0]);
        Storage.WriteString('TComboBox', 'dOption[1]' ,dOption.Items[1]);
        Storage.WriteString('TComboBox', 'dOption[2]' ,dOption.Items[2]);
        Storage.WriteString('TComboBox', 'dOption[3]' ,dOption.Items[3]);
        Storage.WriteString('TComboBox', 'dOption[4]' ,dOption.Items[4]);
        Storage.WriteString('TComboBox', 'dOption[5]' ,dOption.Items[5]);
        Storage.WriteString('TComboBox', 'dOption[6]' ,dOption.Items[6]);
        Storage.WriteString('TComboBox', 'dOption[7]' ,dOption.Items[7]);
        Storage.WriteString('TComboBox', 'dOption[8]' ,dOption.Items[8]);
        Storage.WriteString('TComboBox', 'dOption[9]' ,dOption.Items[9]);
      
        Storage.WriteString('TComboBox', 'oOption.Caption' ,oOptionLabel.Caption);
        Storage.WriteString('TComboBox', 'oOption[0]' ,oOption.Items[0]);
        Storage.WriteString('TComboBox', 'oOption[1]' ,oOption.Items[1]);
        Storage.WriteString('TComboBox', 'oOption[2]' ,oOption.Items[2]);
      
        Storage.WriteString('TCheckBox', 'rOption.Caption',   rOption.Caption);
        Storage.WriteString('TCheckBox', 'sOption.Caption',   sOption.Caption);
        Storage.WriteString('TCheckBox', 'kOption.Caption',   kOption.Caption);
        Storage.WriteString('TCheckBox', 'tOption.Caption',   tOption.Caption);
        Storage.WriteString('TCheckBox', 'cdAOption.Caption', cdAOption.Caption);
        Storage.WriteString('TCheckBox', 'xCommand.Caption',  xCommand.Caption);
        Storage.WriteString('TCheckBox', 'cdEOption.Caption', cdEOption.Caption);

        Storage.WriteString('TCheckBox', 'HideWithTickFrmOption.Caption', HideWithTickFrmOption.Caption);
        Storage.WriteString('TCheckBox', 'HideWithAddFrmOrExtractFrmOption.Caption', HideWithAddFrmOrExtractFrmOption.Caption);
        Storage.WriteString('TCheckBox', 'UpBtnCloseOption.Caption', UpBtnCloseOption.Caption);
      
        Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

