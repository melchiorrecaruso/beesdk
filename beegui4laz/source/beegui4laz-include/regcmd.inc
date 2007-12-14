
  if (ParamCount = 1) then
  begin
    if (ParamStr(1) = 'register:file') then
    begin
      RegisterFileType('bee', ParamStr(0));
      Exit;
    end;
    if (ParamStr(1) = 'unregister:file') then
    begin
      UnRegisterFileType('bee', ParamStr(0));
      Exit;
    end;
  end;
