
  procedure TRenameFrm.SaveProperty;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));
      begin
        Storage.WriteInteger('TForm', 'Top', Top);
        Storage.WriteInteger('TForm', 'Left', Left);
      end;
      Storage.Free;
    end;
  end;