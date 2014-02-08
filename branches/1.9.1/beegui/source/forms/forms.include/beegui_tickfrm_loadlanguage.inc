
  procedure TTickFrm.LoadLanguage;
  var
    I: integer;
    Folder: string;
    Storage: TMemIniFile;
  begin
    Folder := IncludeTrailingBackSlash(GetApplicationLanguageDir);
    if ForceDirectories(Folder) then
    begin
      Storage := TMemIniFile.Create(Folder + ('tickfrm.ini'));
      begin
        //GeneralPage.Caption := Storage.ReadString('TPage', 'GeneralPage.Caption', GeneralPage.Caption);
        //ReportPage.Caption  := Storage.ReadString('TPage', 'ReportPage.Caption', ReportPage.Caption);
    
        ElapsedTimeLabel.Caption   := Storage.ReadString('TLabel', 'ElapsedTimeLabel.Caption', ElapsedTimeLabel.Caption);
        RemainingTimeLabel.Caption := Storage.ReadString('TLabel', 'RemainingTimeLabel.Caption', RemainingTimeLabel.Caption);
        GeneralSizeLabel.Caption   := Storage.ReadString('TLabel', 'GeneralSizeLabel.Caption', GeneralSizeLabel.Caption);
        ProcessedSizeLabel.Caption := Storage.ReadString('TLabel', 'ProcessedSizeLabel.Caption', ProcessedSizeLabel.Caption);
        SpeedLabel.Caption         := Storage.ReadString('TLabel', 'SpeedLabel.Caption', SpeedLabel.Caption);

        for I := 0 to Popup.Items.Count -1 do with Popup.Items[I] do
          Caption := Storage.ReadString('TMenuItem', 'Popup.Items[' + IntToStr(I) + '].Caption', Caption);

        rsBtnPauseCaption   := Storage.ReadString('TBitBtn', 'BtnPause.Caption', rsBtnPauseCaption);
        rsBtnRunCaption     := Storage.ReadString('TBitBtn', 'BtnRun.Caption', rsBtnRunCaption);
        rsBtnCancelCaption  := Storage.ReadString('TBitBtn', 'BtnCancel.Caption', rsBtnCancelCaption);
        rsBtnCloseCaption   := Storage.ReadString('TBitBtn', 'BtnClose.Caption', rsBtnCloseCaption);

        BtnPriority.Caption := Storage.ReadString('TBitBtn', 'BtnPriority.Caption', BtnPriority.Caption);
        BtnSave.Caption     := Storage.ReadString('TBitBtn', 'BtnSave.Caption', BtnSave.Caption);
        BtnFont.Caption     := Storage.ReadString('TBitBtn', 'BtnFont.Caption', BtnFont.Caption);
      end;
      Storage.Free;
    end;
  end;