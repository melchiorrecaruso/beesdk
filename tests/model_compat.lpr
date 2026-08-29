program ModelCompat;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Bee_Configuration, Bee_Codec, Bee_Modeller,
  Bee_Model, Bee_RangeCoder;

var
  Input: TFileStream;
  LegacyStream, NewStream: TMemoryStream;
  LegacyCodec: TSecondaryEncoder;
  LegacyModel: TBaseCoder;
  NewCoder: TRangeCoder;
  NewModel: TBeeNibbleModel;
  LegacyTable: Bee_Configuration.TTableParameters;
  NewTable: Bee_Model.TBeeTableParameters;
  I: Integer;
  Symbol: Byte;
begin
  if ParamCount <> 1 then
  begin
    WriteLn('usage: model_compat <sample-file>');
    Halt(1);
  end;
  NewTable := BeeCreateDefaultTable(4);
  for I := 1 to SizeOf(LegacyTable) do LegacyTable[I] := NewTable[I - 1];
  Input := TFileStream.Create(ParamStr(1), fmOpenRead or fmShareDenyWrite);
  LegacyStream := TMemoryStream.Create;
  NewStream := TMemoryStream.Create;
  LegacyCodec := TSecondaryEncoder.Create(LegacyStream);
  LegacyModel := TBaseCoder.Create(LegacyCodec);
  NewCoder := TRangeCoder.Create(NewStream);
  NewModel := TBeeNibbleModel.Create;
  try
    LegacyModel.SetTable(LegacyTable);
    LegacyModel.SetDictionary(3);
    NewModel.SetTable(NewTable);
    NewModel.SetDictionaryLevel(3);
    LegacyCodec.Start;
    NewCoder.StartEncode;
    while Input.Read(Symbol, 1) = 1 do
    begin
      LegacyModel.UpdateModel(Symbol);
      NewModel.EncodeByte(Symbol, NewCoder);
    end;
    LegacyCodec.Flush;
    NewCoder.FinishEncode;
    if LegacyStream.Size <> NewStream.Size then
      raise Exception.CreateFmt('size mismatch: legacy=%d new=%d',
        [LegacyStream.Size, NewStream.Size]);
    if (LegacyStream.Size > 0) and
       (CompareByte(LegacyStream.Memory^, NewStream.Memory^, LegacyStream.Size) <> 0) then
      raise Exception.Create('bitstream mismatch');
    WriteLn('compatible', #9, 'yes');
    WriteLn('input_bytes', #9, Input.Size);
    WriteLn('packed_bytes', #9, NewStream.Size);
  finally
    NewModel.Free;
    NewCoder.Free;
    LegacyModel.Free;
    LegacyCodec.Free;
    NewStream.Free;
    LegacyStream.Free;
    Input.Free;
  end;
end.
