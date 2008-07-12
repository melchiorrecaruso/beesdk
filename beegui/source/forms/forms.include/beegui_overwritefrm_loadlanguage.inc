
  procedure TOverWriteFrm.LoadLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));
      begin
        Caption := Storage.ReadString('TForm', 'Caption', Caption);
      
	TheFolder.Caption := Storage.ReadString('TLabel', 'TheFolder.Caption', TheFolder.Caption);
        WouldYou.Caption  := Storage.ReadString('TLabel', 'WouldYou.Caption', WouldYou.Caption);
        WithFile.Caption  := Storage.ReadString('TLabel', 'WithFile.Caption', WithFile.Caption);
        OldSize.Caption   := Storage.ReadString('TLabel', 'OldSize.Caption', OldSize.Caption);
        OldTime.Caption   := Storage.ReadString('TLabel', 'OldDate.Caption', OldTime.Caption);
        NewSize.Caption   := Storage.ReadString('TLabel', 'NewSize.Caption', NewSize.Caption);
        NewTime.Caption   := Storage.ReadString('TLabel', 'NewDate.Caption', NewTime.Caption);

        BtnYes.Caption    := Storage.ReadString('TBitBtn', 'BtnYes.Caption', BtnYes.Caption);
        BtnYesAll.Caption := Storage.ReadString('TBitBtn', 'BtnYesAll.Caption', BtnYesAll.Caption);
        BtnNo.Caption     := Storage.ReadString('TBitBtn', 'BtnNo.Caption', BtnNo.Caption);
        BtnNoAll.Caption  := Storage.ReadString('TBitBtn', 'BtnNoAll.Caption', BtnNoAll.Caption);
        BtnAbort.Caption  := Storage.ReadString('TBitBtn', 'BtnAbort.Caption', BtnAbort.Caption);
      end;
      Storage.Free;
    end;
  end;
