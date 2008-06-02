
  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));

    if WindowState = wsNormal then
    begin
      Storage.WriteInteger('TForm', 'TickFrm.Top', Top);
      Storage.WriteInteger('TForm', 'TickFrm.Left', Left);
      Storage.WriteInteger('TForm', 'TickFrm.Width', Width);
      Storage.WriteInteger('TForm', 'TickFrm.Height', Height);
    end;
    Storage.WriteInteger('TForm', 'TickFrm.WindowState', Ord(WindowState));
    
    Storage.WriteInteger('TFont', 'Report.Font.Charset',    (Report.Font.CharSet));
    Storage.WriteInteger('TFont', 'Report.Font.Color'  ,    (Report.Font.Color));
    Storage.WriteString ('TFont', 'Report.Font.Name'   ,     Report.Font.Name);
    Storage.WriteInteger('TFont', 'Report.Font.Height' ,     Report.Font.Height);
    Storage.WriteInteger('TFont', 'Report.Font.Pitch'  , Ord(Report.Font.Pitch));
    Storage.WriteInteger('TFont', 'Report.Font.Size'   ,     Report.Font.Size);

    if (fsBold in Report.Font.Style) then
      Storage.WriteBool('TFont', 'Report.Font.Style[fsBold]', True)
    else
      Storage.WriteBool('TFont', 'Report.Font.Style[fsBold]', False);

    if (fsItalic in Report.Font.Style) then
      Storage.WriteBool('TFont', 'Report.Font.Style[fsItalic]', True)
    else
      Storage.WriteBool('TFont', 'Report.Font.Style[fsItalic]', False);

    if (fsUnderline in Report.Font.Style) then
      Storage.WriteBool('TFont', 'Report.Font.Style[fsUnderline]', True)
    else
      Storage.WriteBool('TFont', 'Report.Font.Style[fsUnderline]', False);

    if (fsStrikeOut in Report.Font.Style) then
      Storage.WriteBool('TFont', 'Report.Font.Style[fsStrikeOut]', True)
    else
      Storage.WriteBool('TFont', 'Report.Font.Style[fsStrikeOut]', False);

    Storage.Free;
  end;
