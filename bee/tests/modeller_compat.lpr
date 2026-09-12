program ModellerCompat;
{$mode objfpc}{$H+}
uses Classes, SysUtils, Bee_Configuration, Bee_Codec, Bee_RangeCoder,
  Bee_Modeller, Bee_Modeller_079, Bee_Model;
var
  Data: TBytes;
  Table: Bee_Model.TBeeTableParameters;
  LegacyTable: Bee_Configuration.TTableParameters;
  Dictionary: Cardinal;

function Compress(Kind: Integer): TMemoryStream;
var
  Encoder: TSecondaryEncoder;
  Original: Bee_Modeller_079.TBaseCoder;
  Current: Bee_Modeller.TBaseCoder;
  Adapter: TBeeModel;
  I: Integer;
begin
  Result := TMemoryStream.Create;
  Encoder := TSecondaryEncoder.Create(Result);
  Original := nil; Current := nil; Adapter := nil;
  try
    case Kind of
      0: begin
        Original := Bee_Modeller_079.TBaseCoder.Create(Encoder);
        Original.SetDictionary(Dictionary); Original.SetTable(LegacyTable);
      end;
      1: begin
        Current := Bee_Modeller.TBaseCoder.Create(Encoder);
        Current.SetDictionary(Dictionary); Current.SetTable(LegacyTable);
      end;
      2: begin
        Adapter := TBeeModel.Create;
        Adapter.SetDictionaryLevel(Dictionary); Adapter.SetTable(Table);
      end;
    end;
    Encoder.Start;
    for I := 0 to High(Data) do
      case Kind of
        0: Original.UpdateModel(Data[I]);
        1: Current.UpdateModel(Data[I]);
        2: Adapter.EncodeByte(Data[I], Encoder);
      end;
    Encoder.Flush;
  finally
    Original.Free; Current.Free; Adapter.Free; Encoder.Free;
  end;
end;

procedure Verify(Stream: TMemoryStream);
var Model: TBeeModel; Decoder: TRangeCoder; I: Integer;
begin
  Stream.Position := 0;
  Model := TBeeModel.Create; Decoder := TRangeCoder.Create(Stream);
  try
    Model.SetDictionaryLevel(Dictionary); Model.SetTable(Table);
    Decoder.StartDecode;
    for I := 0 to High(Data) do
      if Model.DecodeByte(Decoder) <> Data[I] then
        raise Exception.CreateFmt('Decode mismatch at %d', [I]);
    Decoder.FinishDecode;
  finally Model.Free; Decoder.Free; end;
end;

var Input: TFileStream; Reference, Candidate: TMemoryStream; I, Kind: Integer;
begin
  try
    if ParamCount <> 3 then raise Exception.Create('Usage: modeller_compat input table dictionary');
    Dictionary := StrToInt(ParamStr(3));
    BeeLoadTable(ParamStr(2), Table);
    for I := 1 to SizeOf(LegacyTable) do LegacyTable[I] := Table[I - 1];
    Input := TFileStream.Create(ParamStr(1), fmOpenRead);
    try SetLength(Data, Input.Size); if Length(Data) > 0 then Input.ReadBuffer(Data[0], Length(Data));
    finally Input.Free; end;
    Reference := Compress(0);
    try
      Verify(Reference);
      for Kind := 1 to 2 do begin
        Candidate := Compress(Kind);
        try
          if (Candidate.Size <> Reference.Size) or
            not CompareMem(Candidate.Memory, Reference.Memory, Reference.Size) then
            raise Exception.CreateFmt('Bitstream mismatch, implementation %d', [Kind]);
          Verify(Candidate);
        finally Candidate.Free; end;
      end;
      WriteLn('OK original079=current=adapter bytes=', Reference.Size,
        ' input=', Length(Data), ' d=', Dictionary);
    finally Reference.Free; end;
  except on E: Exception do begin WriteLn(StdErr, E.Message); Halt(1); end; end;
end.
