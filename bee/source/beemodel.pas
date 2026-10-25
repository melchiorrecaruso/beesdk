unit BeeModel;

{$MODE OBJFPC}
{$H+}
{$R-}{$Q-}

interface

uses
  Classes, SysUtils,
  BeeCodec, BeeConfiguration, BeeModeller, BeeRangeCoder;

const
  BeeBits = BitChain;
  BeeMaxSymbol = MaxSymbol;
  BeeIncrement = Increment;
  BeeMaxTableLevel = 15;
  BeeTableColumnCount = TableCols;
  BeeTableColumnSize = TableSize + 1;
  BeeTableParameterCount = SizeOf(TTableParameters);

type
  TBeeTableParameters = array of Byte;
  TBeeFrequency = TFreq;

const
  BeeLegacyDefaultTable: array[0..42] of Byte =
    (3, 163, 157, 65, 93, 117, 135, 109, 126, 252, 172, 252, 152,
     227, 249, 249, 253, 196, 27, 82, 93, 74, 182, 245, 40, 67, 77,
     143, 133, 135, 128, 155, 207, 177, 225, 251, 253, 248, 73, 35,
     15, 107, 143);
type
  { Adapter for byte tables and the stand-alone range coder.
    The PPM algorithm itself remains in the original TBaseCoder. }
  TBeeModel = class(TBaseCoder)
  public
    constructor Create;
    procedure SetDictionaryLevel(Level: Cardinal);
    procedure SetTable(const Parameters: TBeeTableParameters); reintroduce;
    procedure EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
    function DecodeByte(RangeCoder: TRangeCoder): Byte;
  end;

function BeeCreateDefaultTable: TBeeTableParameters;
function BeeDictionaryMemoryBytes(Level: Cardinal): Int64;
{ A table is the hexadecimal string of its bytes, as in bee.ini. }
function BeeTableToHex(const Table: TBeeTableParameters): string;
function BeeHexToTable(const Hex: string; out Table: TBeeTableParameters): Boolean;

{ Reads a table from a file holding bee.ini lines (".ext=<hex>") or a bare
  hexadecimal string; 'default' gives the default table. }
procedure BeeLoadTable(const FileName: string; out Table: TBeeTableParameters);

implementation

uses
  BeeCommon;

function BeeDictionaryMemoryBytes(Level: Cardinal): Int64;
begin
  if Level > 9 then
    raise ERangeError.Create('Dictionary level must be between 0 and 9');
  Result := (Int64(1) shl (17 + Level)) * SizeOf(TNode);
end;

function BeeCreateDefaultTable: TBeeTableParameters;
begin
  SetLength(Result, BeeTableParameterCount);
  Move(BeeLegacyDefaultTable[0], Result[0], BeeTableParameterCount);
end;

function BeeTableToHex(const Table: TBeeTableParameters): string;
begin
  if Length(Table) <> BeeTableParameterCount then
    raise ERangeError.CreateFmt('A Bee table must contain %d values',
      [BeeTableParameterCount]);
  Result := BeeCommon.Hex(Table[0], BeeTableParameterCount);
end;

function BeeHexToTable(const Hex: string; out Table: TBeeTableParameters): Boolean;
begin
  SetLength(Table, BeeTableParameterCount);
  Result := (Length(Hex) = 2 * BeeTableParameterCount) and
    BeeCommon.HexToData(UpperCase(Hex), Table[0], BeeTableParameterCount);
  if not Result then Table := nil;
end;

procedure BeeLoadTable(const FileName: string; out Table: TBeeTableParameters);
var
  Lines: TStringList;
  I, Separator: Integer;
  Line, Key: string;
begin
  if SameText(FileName, 'default') then
  begin
    Table := BeeCreateDefaultTable;
    Exit;
  end;
  Lines := TStringList.Create;
  try
    Lines.LoadFromFile(FileName);
    for I := 0 to Lines.Count - 1 do
    begin
      Line := Trim(Lines[I]);
      Separator := Pos('=', Line);
      if Separator > 0 then
      begin
        Key := Copy(Line, 1, Separator - 1);
        if SameText(Copy(Key, Length(Key) - 4, 5), '.Size') then Continue;
        Line := Copy(Line, Separator + 1, MaxInt);
      end;
      if BeeHexToTable(Line, Table) then Exit;
    end;
  finally
    Lines.Free;
  end;
  raise EStreamError.CreateFmt('No %d-byte hexadecimal table in %s',
    [BeeTableParameterCount, FileName]);
end;

constructor TBeeModel.Create;
begin
  inherited Create(nil);
  SetTable(BeeCreateDefaultTable);
  SetDictionaryLevel(3);
end;

procedure TBeeModel.SetDictionaryLevel(Level: Cardinal);
begin
  if Level > 9 then
    raise ERangeError.Create('Dictionary level must be between 0 and 9');
  SetDictionary(Level);
end;

procedure TBeeModel.SetTable(const Parameters: TBeeTableParameters);
var
  OriginalTable: BeeConfiguration.TTableParameters;
  I: Integer;
begin
  if Length(Parameters) <> BeeTableParameterCount then
    raise ERangeError.CreateFmt('The Bee model requires a %d-byte table',
      [BeeTableParameterCount]);
  for I := 1 to BeeTableParameterCount do
    OriginalTable[I] := Parameters[I - 1];
  inherited SetTable(OriginalTable);
end;

procedure TBeeModel.EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
var
  PartIndex, Symbol: Cardinal;
  CumFreq, TotalFreq, I: Cardinal;
begin
  for PartIndex := 0 to BeeTableColumnCount - 1 do
  begin
    PrepareSymbol(PartIndex);
    if PartIndex = 0 then Symbol := Value shr 4
    else Symbol := Value and BeeMaxSymbol;
    CumFreq := 0;
    if Symbol > 0 then
      for I := 0 to Symbol - 1 do
        Inc(CumFreq, Frequencies[I]);
    TotalFreq := CumFreq;
    for I := Symbol to BeeMaxSymbol do
      Inc(TotalFreq, Frequencies[I]);
    RangeCoder.Encode(CumFreq, Frequencies[Symbol], TotalFreq);
    LearnSymbol(Symbol);
  end;
  FinishByte;
end;

function TBeeModel.DecodeByte(RangeCoder: TRangeCoder): Byte;
var
  PartIndex, Symbol: Cardinal;
  CumFreq, TotalFreq, Target: Cardinal;
begin
  Result := 0;
  for PartIndex := 0 to BeeTableColumnCount - 1 do
  begin
    PrepareSymbol(PartIndex);
    TotalFreq := 0;
    for Symbol := 0 to BeeMaxSymbol do
      Inc(TotalFreq, Frequencies[Symbol]);
    Target := RangeCoder.GetFreq(TotalFreq);
    CumFreq := 0;
    Symbol := 0;
    while Target >= CumFreq + Frequencies[Symbol] do
    begin
      Inc(CumFreq, Frequencies[Symbol]);
      Inc(Symbol);
    end;
    RangeCoder.Decode(CumFreq, Frequencies[Symbol], TotalFreq);
    if PartIndex = 0 then Result := Byte(Symbol shl 4)
    else Result := Result or Byte(Symbol);
    LearnSymbol(Symbol);
  end;
  FinishByte;
end;

end.
