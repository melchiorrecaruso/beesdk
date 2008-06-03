
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('BeeGui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('renamefrm.ini'));    
    begin
      Top  := Storage.ReadInteger('TForm', 'Top', Top);
      Left := Storage.ReadInteger('TForm', 'Left', Left);     
    end;    
    Storage.Free;
  end;

