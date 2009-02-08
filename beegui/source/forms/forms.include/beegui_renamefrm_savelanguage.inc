
  procedure TRenameFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
      begin
        Storage.WriteString('TForm','Caption', Caption);
	  
        Storage.WriteString('TLabel','FromFNLabel', FromFNLabel.Caption);
        Storage.WriteString('TLabel','ToFNLabel', ToFNLabel.Caption);
	  
        Storage.WriteString('TBitBtn','BtnCancel', BtnCancel.Caption);
        Storage.WriteString('TBitBtn','BtnOk', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

