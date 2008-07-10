
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));
    begin
      if WindowState = wsNormal then
      begin
        Top  := Storage.ReadInteger('TForm', 'Top', Top);
        Left := Storage.ReadInteger('TForm', 'Left', Left);        
      end;      

      Report.Font.CharSet := TFontCharSet(Storage.ReadInteger('TFont', 'Report.Font.Charset',    (Report.Font.CharSet)));
      Report.Font.Color   := TColor      (Storage.ReadInteger('TFont', 'Report.Font.Color'  ,    (Report.Font.Color)));
      Report.Font.Name    :=              Storage.ReadString ('TFont', 'Report.Font.Name'   ,     Report.Font.Name);
      Report.Font.Height  :=              Storage.ReadInteger('TFont', 'Report.Font.Height' ,     Report.Font.Height);
      Report.Font.Pitch   := TFontPitch  (Storage.ReadInteger('TFont', 'Report.Font.Pitch'  , Ord(Report.Font.Pitch)));
      Report.Font.Size    :=              Storage.ReadInteger('TFont', 'Report.Font.Size'   ,     Report.Font.Size);

      if Storage.ReadBool('TFont', 'Report.Font.Style[fsBold]', False) then
        Report.Font.Style := Report.Font.Style + [fsBold]
      else
        Report.Font.Style := Report.Font.Style - [fsBold];

      if Storage.ReadBool ('TFont', 'Report.Font.Style[fsItalic]', False) then
        Report.Font.Style := Report.Font.Style + [fsItalic]
      else
        Report.Font.Style := Report.Font.Style - [fsItalic];

      if Storage.ReadBool ('TFont', 'Report.Font.Style[fsUnderline]', False) then
        Report.Font.Style := Report.Font.Style + [fsUnderline]
      else
        Report.Font.Style := Report.Font.Style - [fsUnderline];

      if Storage.ReadBool ('TFont', 'Report.Font.Style[fsStrikeOut]', False) then
        Report.Font.Style := Report.Font.Style + [fsStrikeOut]
      else
        Report.Font.Style := Report.Font.Style - [fsStrikeOut];
    end;
    Storage.Free;
  end;

