  procedure TSelectFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('selectfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);

        // ---

        Storage.WriteString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
  	Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
      end;
      Storage.Free;
    end;
  end;

