
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));

    // ---

    Storage.WriteString('TForm', 'AboutFrm.Caption', Caption);

    // ---

    Storage.WriteString('TLabel', 'Version.Caption', Version.Caption);
    Storage.WriteString('TLabel', 'Build.Caption', Build.Caption);

    Storage.WriteString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    Storage.WriteString('TBitBtn', 'BtnLicense.Caption', BtnLicense.Caption);

    // ---

    Storage.Free;
  end;

