
  procedure TViewFrm.SaveLanguage;
  var
    I: integer;
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('viewfrm.ini'));
      begin
        Storage.WriteString('TForm', 'Caption', Caption);

        for I := 0 to PMenu.Items.Count -1 do with PMenu.Items[I] do
          Storage.WriteString('TMenuItem', 'PMenu.Items[' + IntToStr(I) + '].Caption', Caption);

        Storage.WriteString('TBitBtn', 'BtnFont.Caption',  BtnFont.Caption);
        Storage.WriteString('TBitBtn', 'BtnSave.Caption',  BtnSave.Caption);
        Storage.WriteString('TBitBtn', 'BtnClose.Caption', BtnClose.Caption);
      end;
      Storage.Free;
    end;
  end;


