
  procedure TTickFrm.SaveLanguage;
  var
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));
      begin
        Storage.WriteString('TPage', 'GeneralPage.Caption', GeneralPage.Caption);
        Storage.WriteString('TPage', 'ReportPage.Caption', ReportPage.Caption);
    
        Storage.WriteString('TLabel', 'TimeLabel.Caption', TimeLabel.Caption);
        Storage.WriteString('TLabel', 'RemainingTimeLabel.Caption', RemainingTimeLabel.Caption);
        Storage.WriteString('TLabel', 'GeneralSizeLabel.Caption', GeneralSizeLabel.Caption);
        Storage.WriteString('TLabel', 'ProcessedSizeLabel.Caption', ProcessedSizeLabel.Caption);
        Storage.WriteString('TLabel', 'SpeedLabel.Caption', SpeedLabel.Caption);

        Storage.WriteString('TBitBtn', 'BtnPause.Caption', rsBtnPauseCaption);
        Storage.WriteString('TBitBtn', 'BtnRun.Caption', rsBtnRunCaption);
        Storage.WriteString('TBitBtn', 'BtnCancel.Caption', rsBtnCancelCaption);
        Storage.WriteString('TBitBtn', 'BtnClose.Caption', rsBtnCloseCaption);
        Storage.WriteString('TBitBtn', 'BtnPriority.Caption', BtnPriority.Caption);
        Storage.WriteString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
        Storage.WriteString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
      end;
      Storage.Free;
    end;
  end;
