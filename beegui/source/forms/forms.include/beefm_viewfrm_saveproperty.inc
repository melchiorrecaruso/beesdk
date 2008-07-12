
  procedure TViewFrm.SaveProperty;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationConfigDir(cApplicationName));
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('viewfrm.ini'));

      if WindowState = wsNormal then
      begin
        Storage.WriteInteger('TForm', 'ViewFrm.Top', Top);
        Storage.WriteInteger('TForm', 'ViewFrm.Left', Left);
        Storage.WriteInteger('TForm', 'ViewFrm.Width', Width);
        Storage.WriteInteger('TForm', 'ViewFrm.Height', Height);
      end;
      Storage.WriteInteger('TForm', 'ViewFrm.WindowState', Ord(WindowState));

      Storage.WriteInteger('TFont', 'FontDialog.Charset', Ord(FontDialog.Font.CharSet));
      Storage.WriteInteger('TFont', 'FontDialog.Color'  , Ord(FontDialog.Font.Color));
      Storage.WriteString ('TFont', 'FontDialog.Name'   ,     FontDialog.Font.Name);
      Storage.WriteInteger('TFont', 'FontDialog.Height' ,     FontDialog.Font.Height);
      Storage.WriteInteger('TFont', 'FontDialog.Pitch'  , Ord(FontDialog.Font.Pitch));
      Storage.WriteInteger('TFont', 'FontDialog.Size'   ,     FontDialog.Font.Size);

      if (fsBold in FontDialog.Font.Style) then
        Storage.WriteBool('TFont', 'FontDialog.Style[fsBold]', True)
      else
        Storage.WriteBool('TFont', 'FontDialog.Style[fsBold]', False);

      if (fsItalic in FontDialog.Font.Style) then
        Storage.WriteBool('TFont', 'FontDialog.Style[fsItalic]', True)
      else
        Storage.WriteBool('TFont', 'FontDialog.Style[fsItalic]', False);

      if (fsUnderline in FontDialog.Font.Style) then
        Storage.WriteBool('TFont', 'FontDialog.Style[fsUnderline]', True)
      else
        Storage.WriteBool('TFont', 'FontDialog.Style[fsUnderline]', False);

      if (fsStrikeOut in FontDialog.Font.Style) then
        Storage.WriteBool('TFont', 'FontDialog.Style[fsStrikeOut]', True)
      else
        Storage.WriteBool('TFont', 'FontDialog.Style[fsStrikeOut]', False);

      Storage.Free;
    end;
  end;
