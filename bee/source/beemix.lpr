program BeeMix;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, Bee_Model, Bee_Mixer, Bee_RangeCoder;

const
  BeeMixMagic = $32584D42; { BMX2 }

type
  TOptions = record
    Command, InputName, OutputName: string;
    Dictionary: Byte;
    TableNames: array[0..BeeExpertCount - 1] of string;
  end;

procedure Usage;
begin
  WriteLn('usage: beemix c <input> <archive> [--dictionary=0..9] [--table1=f] [--table2=f] [--table4=f] [--table8=f]');
  WriteLn('       beemix d <archive> <output>');
end;

function StartsWith(const Value, Prefix: string): Boolean;
begin
  Result := Copy(Value, 1, Length(Prefix)) = Prefix;
end;

procedure ParseOptions(out Options: TOptions);
var
  I, TableIndex, Bits: Integer;
  Argument, Value: string;
begin
  FillChar(Options, SizeOf(Options), 0);
  Options.Dictionary := 3;
  for I := 0 to BeeExpertCount - 1 do Options.TableNames[I] := 'default';
  if ParamCount < 3 then begin Usage; Halt(1); end;
  Options.Command := LowerCase(ParamStr(1));
  Options.InputName := ParamStr(2);
  Options.OutputName := ParamStr(3);
  for I := 4 to ParamCount do
  begin
    Argument := ParamStr(I);
    if StartsWith(Argument, '--dictionary=') then
      Options.Dictionary := StrToInt(Copy(Argument, 14, MaxInt))
    else if StartsWith(Argument, '--table') then
    begin
      Bits := StrToInt(Copy(Argument, 8, Pos('=', Argument) - 8));
      Value := Copy(Argument, Pos('=', Argument) + 1, MaxInt);
      case Bits of
        1: TableIndex := 0;
        2: TableIndex := 1;
        4: TableIndex := 2;
        8: TableIndex := 3;
      else
        raise EArgumentException.CreateFmt('Invalid table size: %d', [Bits]);
      end;
      Options.TableNames[TableIndex] := Value;
    end
    else raise EArgumentException.CreateFmt('Unknown option: %s', [Argument]);
  end;
  if not (Options.Dictionary in [0..9]) then
    raise EArgumentException.Create('Dictionary must be between 0 and 9');
end;

procedure Compress(const Options: TOptions);
var
  InputStream, OutputStream: TFileStream;
  RangeCoder: TRangeCoder;
  Mixer: TBeeMixer;
  Tables: TBeeTableSet;
  Magic: Cardinal;
  OriginalSize: Int64;
  I: Integer;
  Symbol: Byte;
begin
  for I := 0 to BeeExpertCount - 1 do
    BeeLoadTable(Options.TableNames[I], Cardinal(1) shl I, Tables[I]);
  InputStream := TFileStream.Create(Options.InputName, fmOpenRead or fmShareDenyWrite);
  try
    OutputStream := TFileStream.Create(Options.OutputName, fmCreate);
    try
      Magic := BeeMixMagic;
      OriginalSize := InputStream.Size;
      OutputStream.WriteBuffer(Magic, SizeOf(Magic));
      OutputStream.WriteBuffer(OriginalSize, SizeOf(OriginalSize));
      OutputStream.WriteBuffer(Options.Dictionary, SizeOf(Options.Dictionary));
      for I := 0 to BeeExpertCount - 1 do
        OutputStream.WriteBuffer(Tables[I][0], Length(Tables[I]));
      RangeCoder := TRangeCoder.Create(OutputStream);
      Mixer := TBeeMixer.Create(Tables, Options.Dictionary);
      try
        RangeCoder.StartEncode;
        while InputStream.Read(Symbol, 1) = 1 do Mixer.EncodeByte(Symbol, RangeCoder);
        RangeCoder.FinishEncode;
      finally
        Mixer.Free;
        RangeCoder.Free;
      end;
      WriteLn('input_bytes', #9, OriginalSize);
      WriteLn('archive_bytes', #9, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

procedure Decompress(const Options: TOptions);
var
  InputStream, OutputStream: TFileStream;
  RangeCoder: TRangeCoder;
  Mixer: TBeeMixer;
  Tables: TBeeTableSet;
  Magic: Cardinal;
  OriginalSize, Remaining: Int64;
  Dictionary: Byte;
  Symbol: Byte;
  I: Integer;
begin
  InputStream := TFileStream.Create(Options.InputName, fmOpenRead or fmShareDenyWrite);
  try
    InputStream.ReadBuffer(Magic, SizeOf(Magic));
    if Magic <> BeeMixMagic then raise EStreamError.Create('Not a BeeMix archive');
    InputStream.ReadBuffer(OriginalSize, SizeOf(OriginalSize));
    InputStream.ReadBuffer(Dictionary, SizeOf(Dictionary));
    for I := 0 to BeeExpertCount - 1 do
    begin
      SetLength(Tables[I], BeeTableByteSize(Cardinal(1) shl I));
      InputStream.ReadBuffer(Tables[I][0], Length(Tables[I]));
    end;
    OutputStream := TFileStream.Create(Options.OutputName, fmCreate);
    try
      RangeCoder := TRangeCoder.Create(InputStream);
      Mixer := TBeeMixer.Create(Tables, Dictionary);
      try
        RangeCoder.StartDecode;
        Remaining := OriginalSize;
        while Remaining > 0 do
        begin
          Symbol := Mixer.DecodeByte(RangeCoder);
          OutputStream.WriteBuffer(Symbol, 1);
          Dec(Remaining);
        end;
        RangeCoder.FinishDecode;
      finally
        Mixer.Free;
        RangeCoder.Free;
      end;
      WriteLn('output_bytes', #9, OutputStream.Size);
    finally
      OutputStream.Free;
    end;
  finally
    InputStream.Free;
  end;
end;

var
  Options: TOptions;
begin
  try
    ParseOptions(Options);
    if Options.Command = 'c' then Compress(Options)
    else if Options.Command = 'd' then Decompress(Options)
    else begin Usage; Halt(1); end;
  except
    on E: Exception do
    begin
      WriteLn(StdErr, 'error: ', E.Message);
      Halt(1);
    end;
  end;
end.
