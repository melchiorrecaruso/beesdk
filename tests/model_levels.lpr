program ModelLevels;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Bee_Model, Bee_RangeCoder;

function CreateModel(Bits: Cardinal): TBeeModel;
begin
  case Bits of
    1: Result := TBeeOneBitModel.Create;
    2: Result := TBeeTwoBitModel.Create;
    4: Result := TBeeNibbleModel.Create;
    8: Result := TBeeByteModel.Create;
  else
    raise ERangeError.Create('invalid test model');
  end;
end;

procedure TestLevel(Bits: Cardinal; const Input: TBytes);
var
  PackedStream: TMemoryStream;
  Encoder, Decoder: TRangeCoder;
  EncodeModel, DecodeModel: TBeeModel;
  Table: TBeeTableParameters;
  I: Integer;
  Value: Byte;
begin
  Table := BeeCreateDefaultTable(Bits);
  Table[0] := BeeMaxTableLevel;
  PackedStream := TMemoryStream.Create;
  Encoder := TRangeCoder.Create(PackedStream);
  EncodeModel := CreateModel(Bits);
  try
    EncodeModel.SetTable(Table);
    EncodeModel.SetDictionaryLevel(1);
    Encoder.StartEncode;
    for I := 0 to High(Input) do
      EncodeModel.EncodeByte(Input[I], Encoder);
    Encoder.FinishEncode;
  finally
    EncodeModel.Free;
    Encoder.Free;
  end;

  PackedStream.Position := 0;
  Decoder := TRangeCoder.Create(PackedStream);
  DecodeModel := CreateModel(Bits);
  try
    DecodeModel.SetTable(Table);
    DecodeModel.SetDictionaryLevel(1);
    Decoder.StartDecode;
    for I := 0 to High(Input) do
    begin
      Value := DecodeModel.DecodeByte(Decoder);
      if Value <> Input[I] then
        raise Exception.CreateFmt('%d-bit mismatch at byte %d', [Bits, I]);
    end;
    Decoder.FinishDecode;
  finally
    DecodeModel.Free;
    Decoder.Free;
    PackedStream.Free;
  end;
  WriteLn(Bits, '-bit level 15 round-trip: yes');
end;

var
  Input: TBytes;
  I: Integer;
begin
  SetLength(Input, 65536);
  for I := 0 to High(Input) do
    Input[I] := Byte((I * 37 + (I shr 3) * 11) and $FF);
  TestLevel(1, Input);
  TestLevel(2, Input);
  TestLevel(4, Input);
  TestLevel(8, Input);
end.
