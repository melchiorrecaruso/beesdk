
  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));   
    begin
      Storage.WriteString('TForm', 'Caption', Caption);
      
	  Storage.WriteString('TLabel', 'TheFolder.Caption', TheFolder.Caption);
      Storage.WriteString('TLabel', 'WouldYou.Caption', WouldYou.Caption);
      Storage.WriteString('TLabel', 'WithFile.Caption', WithFile.Caption);    
      Storage.WriteString('TLabel', 'OldSize.Caption', OldSize.Caption);
      Storage.WriteString('TLabel', 'OldTime.Caption', OldTime.Caption);
      Storage.WriteString('TLabel', 'NewSize.Caption', NewSize.Caption);
      Storage.WriteString('TLabel', 'NewTime.Caption', NewTime.Caption);
    
      Storage.WriteString('TBitBtn', 'BtnYes.Caption', BtnYes.Caption);
      Storage.WriteString('TBitBtn', 'BtnYesAll.Caption', BtnYesAll.Caption);    
      Storage.WriteString('TBitBtn', 'BtnNo.Caption', BtnNo.Caption);
      Storage.WriteString('TBitBtn', 'BtnNoAll.Caption', BtnNoAll.Caption);
      Storage.WriteString('TBitBtn', 'BtnAbort.Caption', BtnAbort.Caption);
    end;
    Storage.Free;
  end;

