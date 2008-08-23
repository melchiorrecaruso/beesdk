
  procedure TOverWriteFrm.LoadProperty;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('overwritefrm.ini'));
      begin
        Top  := Storage.ReadInteger('TForm', 'Top', Top);
        Left := Storage.ReadInteger('TForm', 'Left', Left);
      end;
      Storage.Free;
    end;
  end;
