
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));
    begin
      Caption              := Storage.ReadString('TForm', 'Caption', Caption);
      PageGeneral.Caption  := Storage.ReadString('TPages', 'PageGeneral.Caption', PageGeneral.Caption);
      PageFiles.Caption    := Storage.ReadString('TPages', 'PageFiles.Caption', PageFiles.Caption);
      PageAdvanced.Caption := Storage.ReadString('TPages', 'PageAdvanced.Caption', PageAdvanced.Caption);

      ArchiveNameLabel.Caption := Storage.ReadString('TLabel', 'ArchiveName', ArchiveNameLabel.Caption);
      BtnSave.Caption          := Storage.ReadString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
    
	  ufOptionLabel.Caption := Storage.ReadString('TLabel', 'ufOption', ufOptionLabel.Caption);
      ufOption.Items[0] := Storage.ReadString('TComboBox', 'ufOption[0]', ufOption.Items[0]);
      ufOption.Items[1] := Storage.ReadString('TComboBox', 'ufOption[1]', ufOption.Items[1]);
      ufOption.Items[2] := Storage.ReadString('TComboBox', 'ufOption[2]', ufOption.Items[2]);

      mOptionLabel.Caption := Storage.ReadString('TLabel', 'mOption', mOptionLabel.Caption);
      mOption.Items[0] := Storage.ReadString('TComboBox', 'mOption[0]', mOption.Items[0]);
      mOption.Items[1] := Storage.ReadString('TComboBox', 'mOption[1]', mOption.Items[1]);
      mOption.Items[2] := Storage.ReadString('TComboBox', 'mOption[2]', mOption.Items[2]);
      mOption.Items[3] := Storage.ReadString('TComboBox', 'mOption[3]', mOption.Items[3]);

      dOptionLabel.Caption := Storage.ReadString('TLabel', 'dOption', dOptionLabel.Caption);
      dOption.Items[0] := Storage.ReadString('TComboBox', 'dOption[0]', dOption.Items[0]);
      dOption.Items[1] := Storage.ReadString('TComboBox', 'dOption[1]', dOption.Items[1]);
      dOption.Items[2] := Storage.ReadString('TComboBox', 'dOption[2]', dOption.Items[2]);
      dOption.Items[3] := Storage.ReadString('TComboBox', 'dOption[3]', dOption.Items[3]);
      dOption.Items[4] := Storage.ReadString('TComboBox', 'dOption[4]', dOption.Items[4]);
      dOption.Items[5] := Storage.ReadString('TComboBox', 'dOption[5]', dOption.Items[5]);
      dOption.Items[6] := Storage.ReadString('TComboBox', 'dOption[6]', dOption.Items[6]);
      dOption.Items[7] := Storage.ReadString('TComboBox', 'dOption[7]', dOption.Items[7]);
      dOption.Items[8] := Storage.ReadString('TComboBox', 'dOption[8]', dOption.Items[8]);
      dOption.Items[9] := Storage.ReadString('TComboBox', 'dOption[9]', dOption.Items[9]);

      Options.Caption := Storage.ReadString('TGroupBox', 'Options.Caption', Options.Caption);
      rOption.Caption := Storage.ReadString('TCheckBox', 'rOption.Caption', rOption.Caption);
      sOption.Caption := Storage.ReadString('TCheckBox', 'sOption.Caption', sOption.Caption);
      tOption.Caption := Storage.ReadString('TCheckBox', 'tOption.Caption', tOption.Caption);
      kOption.Caption := Storage.ReadString('TCheckBox', 'kOption.Caption', kOption.Caption);
      lOption.Caption := Storage.ReadString('TCheckBox', 'lOption.Caption', lOption.Caption);

      eOptionLabel.Caption := Storage.ReadString('TLabel', 'eOption', eOptionLabel.Caption);

      FilesLabel.Caption := Storage.ReadString('TLabel', 'Files', FilesLabel.Caption);
      RootLabel.Caption  := Storage.ReadString('TLabel', 'Root', RootLabel.Caption);

      BtnFolder.Caption    := Storage.ReadString('TBitBtn', 'BtnFolder.Caption', BtnFolder.Caption);
      BtnFiles.Caption     := Storage.ReadString('TBitBtn', 'BtnFiles.Caption', BtnFiles.Caption);
      BtnView.Caption      := Storage.ReadString('TBitBtn', 'BtnView.Caption', BtnView.Caption);
      BtnPlusMinus.Caption := Storage.ReadString('TBitBtn', 'BtnPlusMinus.Caption', BtnPlusMinus.Caption);
      BtnModify.Caption    := Storage.ReadString('TBitBtn', 'BtnModify.Caption', BtnModify.Caption);
      BtnDelete.Caption    := Storage.ReadString('TBitBtn', 'BtnDelete.Caption', BtnDelete.Caption);

      aOptionCheck.Caption := Storage.ReadString('TCheckBox', 'aOption.Caption', aOptionCheck.Caption);
      aOption.Items[0] := Storage.ReadString('TComboBox', 'aOption[0]', aOption.Items[0]);
      aOption.Items[1] := Storage.ReadString('TComboBox', 'aOption[1]', aOption.Items[1]);
      aOption.Items[2] := Storage.ReadString('TComboBox', 'aOption[2]', aOption.Items[2]);

      cdOptionCheck.Caption := Storage.ReadString('TLabel', 'cdOption', cdOptionCheck.Caption);

      cfgOptionLabel.Caption := Storage.ReadString('TLabel', 'cfgOption', cfgOptionLabel.Caption);
      cfgOptionBtn.Caption   := Storage.ReadString('TBitBtn', 'cfgOptionBtn.Caption', cfgOptionBtn.Caption);

      yOptionLabel.Caption := Storage.ReadString('TLabel', 'yOption', yOptionLabel.Caption);
      yOptionBtn.Caption   := Storage.ReadString('TBitBtn', 'yOptionBtn.Caption', yOptionBtn.Caption);

      BtnCancel.Caption := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
      BtnOk.Caption     := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    end;
    Storage.Free;
  end;

