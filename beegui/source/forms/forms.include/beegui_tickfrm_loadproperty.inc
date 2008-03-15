
  // ---

  Folder := IncludeTrailingBackSlash(GetApplicationConfigDir('beegui'));
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));

    // ---

    if WindowState = wsNormal then
    begin
      Top    := Storage.ReadInteger('TForm', 'TickFrm.Top', Top);
      Left   := Storage.ReadInteger('TForm', 'TickFrm.Left', Left);
      Width  := Storage.ReadInteger('TForm', 'TickFrm.Width', Width);
      Height := Storage.ReadInteger('TForm', 'TickFrm.Height', Height);
    end;
    WindowState := TWindowState(Storage.ReadInteger('TForm', 'TickFrm.WindowState', Ord(WindowState)));

    // ---

    FontDialog.Font.CharSet := TFontCharSet(Storage.ReadInteger('TFont', 'FontDialog.Charset', Ord(FontDialog.Font.CharSet)));
    FontDialog.Font.Color   := TColor      (Storage.ReadInteger('TFont', 'FontDialog.Color'  , Ord(FontDialog.Font.Color)));
    FontDialog.Font.Name    :=              Storage.ReadString ('TFont', 'FontDialog.Name'   ,     FontDialog.Font.Name);
    FontDialog.Font.Height  :=              Storage.ReadInteger('TFont', 'FontDialog.Height' ,     FontDialog.Font.Height);
    FontDialog.Font.Pitch   := TFontPitch  (Storage.ReadInteger('TFont', 'FontDialog.Pitch'  , Ord(FontDialog.Font.Pitch)));
    FontDialog.Font.Size    :=              Storage.ReadInteger('TFont', 'FontDialog.Size'   ,     FontDialog.Font.Size);

    if Storage.ReadBool('TFont', 'FontDialog.Style[fsBold]', False) then
      FontDialog.Font.Style := FontDialog.Font.Style + [fsBold]
    else
      FontDialog.Font.Style := FontDialog.Font.Style - [fsBold];

    if Storage.ReadBool ('TFont', 'FontDialog.Style[fsItalic]', False) then
      FontDialog.Font.Style := FontDialog.Font.Style + [fsItalic]
    else
      FontDialog.Font.Style := FontDialog.Font.Style - [fsItalic];

    if Storage.ReadBool ('TFont', 'FontDialog.Style[fsUnderline]', False) then
      FontDialog.Font.Style := FontDialog.Font.Style + [fsUnderline]
    else
      FontDialog.Font.Style := FontDialog.Font.Style - [fsUnderline];

    if Storage.ReadBool ('TFont', 'FontDialog.Style[fsStrikeOut]', False) then
      FontDialog.Font.Style := FontDialog.Font.Style + [fsStrikeOut]
    else
      FontDialog.Font.Style := FontDialog.Font.Style - [fsStrikeOut];

    // ---

    Storage.Free;
  end;

