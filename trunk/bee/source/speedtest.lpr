program speedtest;

{$I compiler.inc}

uses
  Math,
  Classes,
  SysUtils,
  BufStream,
  Bee_Files,
  Bee_Common;


type
  TEntroper = class
  private
    FCount: longint;
    FBytes: array [0..255] of byte;
  public
    constructor Create;
    function Entropy: double;
    procedure Update(Symbol: byte);
    property Count: longint read FCount;
  end;



var
  Symbol: byte;

  X: double;

  Input: TStream;
  Output: TStream;
  InputBuff: TStream;
  OutputBuff: TStream;


  E: TEntroper;


constructor TEntroper.Create;
begin
  FillChar(FBytes, SizeOf(FBytes), 0);
  FCount := 0;
end;

procedure TEntroper.Update(Symbol: byte);
begin
  Inc(FBytes[Symbol]);
  Inc(FCount);
end;

function TEntroper.Entropy: double;
var
  I: integer;
begin
  Result := 0;
  for I := 0 to 255 do
  begin
    if FBytes[I] <> 0 then
      Result := Result - (FBytes[I]/FCount) * log2(FBytes[I]/FCount);
  end;
  FillChar(FBytes, SizeOf(FBytes), 0);
  FCount := 0;
end;

procedure Copy1;
begin
  X := Now;
  if FileExists(ParamStr(1)) then
  begin
    Input      := TFileStream.Create(ParamStr(1), fmOpenRead);
    InputBuff  := TReadBufStream.Create(Input);
    TReadBufStream(InputBuff).Capacity := 128;

    Output     := TFileStream.Create(ParamStr(2), fmCreate);
    OutputBuff := TWriteBufStream.Create(Output);
    TWriteBufStream(InputBuff).Capacity := 128;

    Writeln('SCRITTURA');
    while InputBuff.Read(Symbol, 1) <> 0 do
    begin
      OutputBuff.Write(Symbol, 1);
    end;

    OutputBuff.Free;
    Output.Free;

    InputBuff.Free;
    Input.Free;
  end;
  Writeln('Everything went ok - ' + TimeDifference(X) + ' seconds.');
end;

procedure Copy3;
var
  I: cardinal;
begin
  X := Now;
  if FileExists(ParamStr(1)) then
  begin
    Input      := TFileReader.Create(ParamStr(1), fmOpenRead);
    Output     := TFileWriter.Create(ParamStr(2), fmCreate);

    Writeln('SCRITTURA');
    while  Input.Read(I, SizeOf(I)) <> 0 do
    begin
      Output.Write(I, SizeOf(I));
    end;

    Output.Free;
    Input.Free;
  end;
  Writeln('Everything went ok - ' + TimeDifference(X) + ' seconds.');
end;


begin
  Copy3;
  Copy1;
end.

