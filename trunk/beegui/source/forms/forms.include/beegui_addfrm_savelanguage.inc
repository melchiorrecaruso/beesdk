
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('addfrm.ini'));
    begin
      Storage.WriteString('TForm', 'Caption', Caption);    
      Storage.WriteString('TPages', 'PageGeneral.Caption', PageGeneral.Caption);
	  Storage.WriteString('TPages', 'PageFiles.Caption', PageFiles.Caption);
	  Storage.WriteString('TPages', 'PageAdvanced.Caption', PageAdvanced.Caption);

      Storage.WriteString('TLabel', 'ArchiveName.Caption', ArchiveNameLabel.Caption);
      
      Storage.WriteString('TLabel', 'ufOption.Caption', ufOptionLabel.Caption);
      Storage.WriteString('TComboBox', 'ufOption[0]', ufOption.Items[0]);
      Storage.WriteString('TComboBox', 'ufOption[1]', ufOption.Items[1]);
      Storage.WriteString('TComboBox', 'ufOption[2]', ufOption.Items[2]);

      Storage.WriteString('TLabel', 'mOption.Caption', mOptionLabel.Caption);
      Storage.WriteString('TComboBox', 'mOption[0]', mOption.Items[0]);
      Storage.WriteString('TComboBox', 'mOption[1]', mOption.Items[1]);
      Storage.WriteString('TComboBox', 'mOption[2]', mOption.Items[2]);
      Storage.WriteString('TComboBox', 'mOption[3]', mOption.Items[3]);

      Storage.WriteString('TLabel', 'dOption.Caption', dOptionLabel.Caption);
      Storage.WriteString('TComboBox', 'dOption[0]', dOption.Items[0]);
      Storage.WriteString('TComboBox', 'dOption[1]', dOption.Items[1]);
      Storage.WriteString('TComboBox', 'dOption[2]', dOption.Items[2]);
      Storage.WriteString('TComboBox', 'dOption[3]', dOption.Items[3]);
      Storage.WriteString('TComboBox', 'dOption[4]', dOption.Items[4]);
      Storage.WriteString('TComboBox', 'dOption[5]', dOption.Items[5]);
      Storage.WriteString('TComboBox', 'dOption[6]', dOption.Items[6]);
      Storage.WriteString('TComboBox', 'dOption[7]', dOption.Items[7]);
      Storage.WriteString('TComboBox', 'dOption[8]', dOption.Items[8]);
      Storage.WriteString('TComboBox', 'dOption[9]', dOption.Items[9]);

      Storage.WriteString('TGroupBox', 'Options.Caption', Options.Caption);
      Storage.WriteString('TCheckBox', 'rOption.Caption', rOption.Caption);
      Storage.WriteString('TCheckBox', 'sOption.Caption', sOption.Caption);
      Storage.WriteString('TCheckBox', 'tOption.Caption', tOption.Caption);
      Storage.WriteString('TCheckBox', 'kOption.Caption', kOption.Caption);
      Storage.WriteString('TCheckBox', 'lOption.Caption', lOption.Caption); 

      Storage.WriteString('TLabel', 'eOption.Caption', eOptionLabel.Caption);	  

      Storage.WriteString('TLabel', 'Files', FilesLabel.Caption);
      Storage.WriteString('TLabel', 'Root', RootLabel.Caption);
      
      Storage.WriteString('TCheckBox', 'aOption.Caption', aOptionCheck.Caption);
      Storage.WriteString('TComboBox', 'aOption[0]', aOption.Items[0]);
      Storage.WriteString('TComboBox', 'aOption[1]', aOption.Items[1]);
      Storage.WriteString('TComboBox', 'aOption[2]', aOption.Items[2]);

      Storage.WriteString('TLabel', 'cdOption.Caption', cdOptionCheck.Caption);

      Storage.WriteString('TLabel', 'cfgOption.Caption', cfgOptionLabel.Caption);
      Storage.WriteString('TBitBtn', 'cfgOptionBtn.Caption', cfgOptionBtn.Caption);

      Storage.WriteString('TLabel', 'yOption.Caption', yOptionLabel.Caption);
      Storage.WriteString('TBitBtn', 'yOptionBtn.Caption', yOptionBtn.Caption);
	  
      Storage.WriteString('TBitBtn', 'BtnFolder.Caption', BtnFolder.Caption);
      Storage.WriteString('TBitBtn', 'BtnFiles.Caption', BtnFiles.Caption);
      Storage.WriteString('TBitBtn', 'BtnView.Caption', BtnView.Caption);
      Storage.WriteString('TBitBtn', 'BtnPlusMinus.Caption', BtnPlusMinus.Caption);
      Storage.WriteString('TBitBtn', 'BtnModify.Caption', BtnModify.Caption);
      Storage.WriteString('TBitBtn', 'BtnDelete.Caption', BtnDelete.Caption);	  
	  Storage.WriteString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
      Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
      Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    end;
    Storage.Free;
  end;

