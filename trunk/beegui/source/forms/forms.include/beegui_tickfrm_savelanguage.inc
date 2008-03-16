
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');;
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));

    // ---
    
    Storage.WriteString('TPage', 'GeneralPage.Caption', GeneralPage.Caption);
    Storage.WriteString('TPage', 'ReportPage.Caption', ReportPage.Caption);
    
    Storage.WriteString('TLabel', 'TimeLabel.Caption', TimeLabel.Caption);
    Storage.WriteString('TLabel', 'RemainingTimeLabel.Caption', RemainingTimeLabel.Caption);
    Storage.WriteString('TLabel', 'GeneralSizeLabel.Caption', GeneralSizeLabel.Caption);
    Storage.WriteString('TLabel', 'ProcessedSizeLabel.Caption', ProcessedSizeLabel.Caption);
    Storage.WriteString('TLabel', 'SpeedLabel.Caption', SpeedLabel.Caption);

    Storage.WriteString('TBitBtn', 'BtnBackGround.Caption', rsBtnBackGroundCaption);
    Storage.WriteString('TBitBtn', 'BtnForeGround.Caption', rsBtnForeGroundCaption);
    Storage.WriteString('TBitBtn', 'BtnPause.Caption', rsBtnPauseCaption);
    Storage.WriteString('TBitBtn', 'BtnRun.Caption', rsBtnRunCaption);
    Storage.WriteString('TBitBtn', 'BtnCancelCaption.Caption', rsBtnCancelCaption);
    Storage.WriteString('TBitBtn', 'BtnClose.Caption', rsBtnCloseCaption);

    Storage.WriteString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
    Storage.WriteString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
    
    // ---

    Storage.Free;
  end;
