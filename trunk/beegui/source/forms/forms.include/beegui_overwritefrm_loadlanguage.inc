
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));

    // ---

    Caption := Storage.ReadString('TForm', 'OverwriteFrm.Caption', Caption);

    // ---

    TheFolder.Caption := Storage.ReadString('TLabel', 'TheFolder.Caption', TheFolder.Caption);
    WouldYou.Caption  := Storage.ReadString('TLabel', 'WouldYou.Caption', WouldYou.Caption);
    WithFile.Caption  := Storage.ReadString('TLabel', 'WithFile.Caption', WithFile.Caption);

    OldSize.Caption := Storage.ReadString('TLabel', 'OldSize.Caption', OldSize.Caption);
    OldDate.Caption := Storage.ReadString('TLabel', 'OldDate.Caption', OldDate.Caption);
    NewSize.Caption := Storage.ReadString('TLabel', 'NewSize.Caption', NewSize.Caption);
    NewDate.Caption := Storage.ReadString('TLabel', 'NewDate.Caption', NewDate.Caption);

    BtnYes.Caption    := Storage.ReadString('TBitBtn', 'BtnYes.Caption', BtnYes.Caption);
    BtnYesAll.Caption := Storage.ReadString('TBitBtn', 'BtnYesAll.Caption', BtnYesAll.Caption);

    BtnNo.Caption    := Storage.ReadString('TBitBtn', 'BtnNo.Caption', BtnNo.Caption);
    BtnNoAll.Caption := Storage.ReadString('TBitBtn', 'BtnNoAll.Caption', BtnNoAll.Caption);

    BtnAbort.Caption := Storage.ReadString('TBitBtn', 'BtnAbort.Caption', BtnAbort.Caption);

    // ---

    Storage.Free;
  end;

