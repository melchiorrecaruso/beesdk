
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('passwordfrm.ini'));        
    begin
      Top  := Storage.ReadInteger('TForm', 'Top', Top);
      Left := Storage.ReadInteger('TForm', 'Left', Left);      
    end;     
    Storage.Free;
  end;

