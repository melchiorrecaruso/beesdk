
  procedure TConfigFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('configfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);

        Tree.Items[0].Text := Storage.ReadString('TTreeNode', 'Tree.Items[0]' ,Tree.Items[0].Text);
        Tree.Items[1].Text := Storage.ReadString('TTreeNode', 'Tree.Items[1]' ,Tree.Items[1].Text);
        Tree.Items[2].Text := Storage.ReadString('TTreeNode', 'Tree.Items[2]' ,Tree.Items[2].Text);
      
        AddPage.Caption := Storage.ReadString('TGroupBox', 'AddPage.Caption' ,AddPage.Caption);
        AddingOptions.Caption := Storage.ReadString('TGroupBox', 'AddingsOption.Caption' ,AddingOptions.Caption);
      
        ExtractPage.Caption := Storage.ReadString('TGroupBox', 'ExtractPage.Caption' ,ExtractPage.Caption);
        ExtractingOptions.Caption := Storage.ReadString('TGroupBox', 'ExtractingOptions.Caption' ,ExtractingOptions.Caption);

        GeneralPage.Caption := Storage.ReadString('TGroupBox', 'GeneralPage.Caption' ,GeneralPage.Caption);
        Buttons.Caption := Storage.ReadString('TGroupBox', 'Buttons.Caption' ,Buttons.Caption);

        mOptionLabel.Caption  := Storage.ReadString('TLabel', 'mOption.Caption' ,mOptionLabel.Caption);
        mOption.Items[0] := Storage.ReadString('TComboBox', 'mOption[0]' ,mOption.Items[0]);
        mOption.Items[1] := Storage.ReadString('TComboBox', 'mOption[1]' ,mOption.Items[1]);
        mOption.Items[2] := Storage.ReadString('TComboBox', 'mOption[2]' ,mOption.Items[2]);
        mOption.Items[3] := Storage.ReadString('TComboBox', 'mOption[3]' ,mOption.Items[3]);

        dOptionLabel.Caption  := Storage.ReadString('TLabel', 'dOption.Caption' ,dOptionLabel.Caption);
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

        uAOptionLabel.Caption := Storage.ReadString('TLabel', 'uAOption.Caption' ,uAOptionLabel.Caption);
        uAOption.Items[0] := Storage.ReadString('TComboBox', 'uAOption[0]' ,uAOption.Items[0]);
        uAOption.Items[1] := Storage.ReadString('TComboBox', 'uAOption[1]' ,uAOption.Items[1]);
        uAOption.Items[2] := Storage.ReadString('TComboBox', 'uAOption[2]' ,uAOption.Items[2]);
        uAOption.Items[3] := Storage.ReadString('TComboBox', 'uAOption[3]' ,uAOption.Items[3]);
        uAOption.Items[4] := Storage.ReadString('TComboBox', 'uAOption[4]' ,uAOption.Items[4]);
        uAOption.Items[5] := Storage.ReadString('TComboBox', 'uAOption[5]' ,uAOption.Items[5]);

        uEOptionLabel.Caption := Storage.ReadString('TLabel', 'uEOption.Caption' ,uEOptionLabel.Caption);
        uEOption.Items[0] := Storage.ReadString('TComboBox', 'uEOption[0]' ,uEOption.Items[0]);
        uEOption.Items[1] := Storage.ReadString('TComboBox', 'uEOption[1]' ,uEOption.Items[1]);
        uEOption.Items[2] := Storage.ReadString('TComboBox', 'uEOption[2]' ,uEOption.Items[2]);
        uEOption.Items[3] := Storage.ReadString('TComboBox', 'uEOption[3]' ,uEOption.Items[3]);
        uEOption.Items[4] := Storage.ReadString('TComboBox', 'uEOption[4]' ,uEOption.Items[4]);
        uEOption.Items[5] := Storage.ReadString('TComboBox', 'uEOption[5]' ,uEOption.Items[5]);

        rOption.Caption := Storage.ReadString('TCheckBox', 'rOption.Caption',   rOption.Caption);
        sOption.Caption := Storage.ReadString('TCheckBox', 'sOption.Caption',   sOption.Caption);
        tOption.Caption := Storage.ReadString('TCheckBox', 'tOption.Caption',   tOption.Caption);
        cdAOption.Caption := Storage.ReadString('TCheckBox', 'cdAOption.Caption', cdAOption.Caption);
        xCommand.Caption := Storage.ReadString('TCheckBox', 'xCommand.Caption',  xCommand.Caption);
        cdEOption.Caption := Storage.ReadString('TCheckBox', 'cdEOption.Caption', cdEOption.Caption);
      
        HideWithTickFrmOption.Caption := Storage.ReadString('TCheckBox',
          'HideWithTickFrmOption.Caption', HideWithTickFrmOption.Caption);

        HideWithAddFrmOrExtractFrmOption .Caption := Storage.ReadString('TCheckBox',
          'HideWithAddFrmOrExtractFrmOption.Caption', HideWithAddFrmOrExtractFrmOption.Caption);

        UpBtnCloseOption .Caption := Storage.ReadString('TCheckBox', 'UpBtnCloseOption.Caption', UpBtnCloseOption.Caption);

        BtnOk.Caption := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;
