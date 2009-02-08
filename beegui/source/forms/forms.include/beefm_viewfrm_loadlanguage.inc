
  procedure TViewFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('viewfrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);

        BtnFont.Caption  := Storage.ReadString('TBitBtn', 'BtnFont.Caption',  BtnFont.Caption);
        BtnSave.Caption  := Storage.ReadString('TBitBtn', 'BtnSave.Caption',  BtnSave.Caption);
        BtnClose.Caption := Storage.ReadString('TBitBtn', 'BtnClose.Caption', BtnClose.Caption);
      end;
      Storage.Free;
    end;
  end;


