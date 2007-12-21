{ Library Exports for External Synchronize support }

procedure LibCheckSynchronize(TimeOut : longint = 0);
begin
  CheckSynchronize(TimeOut);
end;

procedure LibSetWakeMainThread(WakeProc: TNotifyEvent);
begin
  WakeMainThread := WakeProc;
end;

exports LibCheckSynchronize;
exports LibSetWakeMainThread;

