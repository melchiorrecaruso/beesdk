
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('aboutfrm.ini'));

    // ---

    Caption := Storage.ReadString('TForm', 'AboutFrm.Caption', Caption);

    // ---

    Version.Caption := Storage.ReadString('TLabel', 'Version.Caption', Version.Caption);
    Build.Caption   := Storage.ReadString('TLabel', 'Build.Caption', Build.Caption);

    BtnOk.Caption      := Storage.ReadString('TBitBtn', 'BtnOk.Caption', BtnOk.Caption);
    BtnLicense.Caption := Storage.ReadString('TBitBtn', 'BtnLicense.Caption', BtnLicense.Caption);

    // ---

    Storage.Free;
  end;

