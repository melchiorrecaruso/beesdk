
  // ---

  Folder := ExtractFilePath(ParamStr(0)) + IncludeTrailingBackSlash('language');;
  if ForceDirectories(Folder) then
  begin
    Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));

    // ---
    
    GeneralPage.Caption := Storage.ReadString('TPage', 'GeneralPage.Caption', GeneralPage.Caption);
    ReportPage.Caption  := Storage.ReadString('TPage', 'ReportPage.Caption', ReportPage.Caption);
    
    TimeLabel.Caption          := Storage.ReadString('TLabel', 'TimeLabel.Caption', TimeLabel.Caption);
    RemainingTimeLabel.Caption := Storage.ReadString('TLabel', 'RemainingTimeLabel.Caption', RemainingTimeLabel.Caption);
    GeneralSizeLabel.Caption   := Storage.ReadString('TLabel', 'GeneralSizeLabel.Caption', GeneralSizeLabel.Caption);
    ProcessedSizeLabel.Caption := Storage.ReadString('TLabel', 'ProcessedSizeLabel.Caption', ProcessedSizeLabel.Caption);
    SpeedLabel.Caption         := Storage.ReadString('TLabel', 'SpeedLabel.Caption', SpeedLabel.Caption);

    rsBtnBackGroundCaption     := Storage.ReadString('TBitBtn', 'BtnBackGround.Caption', rsBtnBackGroundCaption);
    rsBtnForeGroundCaption     := Storage.ReadString('TBitBtn', 'BtnForeGround.Caption', rsBtnForeGroundCaption);
    rsBtnPauseCaption          := Storage.ReadString('TBitBtn', 'BtnPause.Caption', rsBtnPauseCaption);
    rsBtnRunCaption            := Storage.ReadString('TBitBtn', 'BtnRun.Caption', rsBtnRunCaption);
    BtnCancel.Caption          := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', BtnCancel.Caption);
    BtnSave.Caption            := Storage.ReadString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
    BtnFont.Caption            := Storage.ReadString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
    
    // ---

    Storage.Free;
  end;
