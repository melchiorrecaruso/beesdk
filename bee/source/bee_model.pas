unit Bee_Model;

{$MODE OBJFPC}
{$H+}
{$R-}{$Q-}

interface

uses
  Classes, SysUtils, IniFiles,
  Bee_Codec, Bee_Configuration, Bee_Modeller, Bee_RangeCoder;

const
  BeeBits = BitChain;
  BeeMaxSymbol = MaxSymbol;
  BeeIncrement = Increment;
  BeeMaxTableLevel = 15;
  BeeTableColumnCount = TableCols;
  BeeTableColumnSize = TableSize + 1;
  BeeTableParameterCount = SizeOf(TTableParameters);
  BeeTableTextFormat = 'BEE-TABLE';
  BeeTableTextVersion = 1;

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
  { Adapter for the text-table format and the stand-alone range coder.
    The PPM algorithm itself remains in the original TBaseCoder. }
  TBeeModel = class(TBaseCoder)
  public
    constructor Create;
    procedure SetDictionaryLevel(Level: Cardinal);
    procedure SetTable(const Parameters: TBeeTableParameters); reintroduce;
    procedure PrepareMixerSymbol(PartIndex: Cardinal);
    procedure LearnMixerSymbol(PartIndex, Symbol: Cardinal);
    procedure FinishMixerByte(Value: Byte);
    procedure EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
    function DecodeByte(RangeCoder: TRangeCoder): Byte;
  end;

  TBeeNibbleModel = TBeeModel;

function BeeCreateDefaultTable: TBeeTableParameters;
function BeeDictionaryMemoryBytes(Level: Cardinal): Int64;
procedure BeeLoadTable(const FileName: string; out Table: TBeeTableParameters);
procedure BeeSaveTable(const FileName: string;
  const Table: TBeeTableParameters);

implementation

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

function BeeTableValuesToText(const Table: TBeeTableParameters): string;
var
  I: Integer;
begin
  Result := '';
  for I := 0 to High(Table) do
  begin
    if I > 0 then Result := Result + ',';
    Result := Result + IntToStr(Table[I]);
  end;
end;

procedure BeeTextToTable(const Value: string; out Table: TBeeTableParameters);
var
  Values: TStringList;
  I, Parameter: Integer;
begin
  Values := TStringList.Create;
  try
    Values.StrictDelimiter := True;
    Values.Delimiter := ',';
    Values.DelimitedText := Value;
    if Values.Count <> BeeTableParameterCount then
      raise EStreamError.CreateFmt('Table contains %d values; expected %d',
        [Values.Count, BeeTableParameterCount]);
    SetLength(Table, BeeTableParameterCount);
    for I := 0 to BeeTableParameterCount - 1 do
    begin
      Parameter := StrToInt(Trim(Values[I]));
      if (Parameter < 0) or (Parameter > 255) then
        raise EStreamError.CreateFmt('Table value %d is outside 0..255', [I]);
      Table[I] := Parameter;
    end;
  finally
    Values.Free;
  end;
end;

procedure BeeLoadTable(const FileName: string; out Table: TBeeTableParameters);
var
  Ini: TMemIniFile;
  StoredBits, StoredSize, Version: Integer;
  Format, Values: string;
begin
  if SameText(FileName, 'default') then
  begin
    Table := BeeCreateDefaultTable;
    Exit;
  end;
  Ini := TMemIniFile.Create(FileName);
  try
    Format := Ini.ReadString('BeeTable', 'Format', '');
    Version := Ini.ReadInteger('BeeTable', 'Version', 0);
    StoredBits := Ini.ReadInteger('BeeTable', 'Bits', 0);
    StoredSize := Ini.ReadInteger('BeeTable', 'Size', 0);
    Values := Ini.ReadString('BeeTable', 'Values', '');
    if (Format <> BeeTableTextFormat) or (Version <> BeeTableTextVersion) then
      raise EStreamError.Create('Unsupported Bee table text format');
    if (StoredBits <> BeeBits) or
      (StoredSize <> BeeTableParameterCount) then
      raise EStreamError.Create('Bee table geometry does not match the model');
    BeeTextToTable(Values, Table);
  finally
    Ini.Free;
  end;
end;

procedure BeeSaveTable(const FileName: string;
  const Table: TBeeTableParameters);
var
  Lines: TStringList;
begin
  if Length(Table) <> BeeTableParameterCount then
    raise ERangeError.CreateFmt('A Bee table must contain %d values',
      [BeeTableParameterCount]);
  Lines := TStringList.Create;
  try
    Lines.Add('[BeeTable]');
    Lines.Add('Format=' + BeeTableTextFormat);
    Lines.Add('Version=' + IntToStr(BeeTableTextVersion));
    Lines.Add('Bits=' + IntToStr(BeeBits));
    Lines.Add('Size=' + IntToStr(Length(Table)));
    Lines.Add('Values=' + BeeTableValuesToText(Table));
    Lines.SaveToFile(FileName);
  finally
    Lines.Free;
  end;
end;

constructor TBeeModel.Create;
begin
  inherited Create(nil);
  SetTable(BeeCreateDefaultTable);
  SetDictionaryLevel(3);
end;

procedure TBeeModel.PrepareMixerSymbol(PartIndex: Cardinal);
begin
  PrepareSymbol(PartIndex);
end;

procedure TBeeModel.LearnMixerSymbol(PartIndex, Symbol: Cardinal);
begin
  LearnSymbol(Symbol);
end;

procedure TBeeModel.FinishMixerByte(Value: Byte);
begin
  FinishByte;
end;

procedure TBeeModel.SetDictionaryLevel(Level: Cardinal);
begin
  if Level > 9 then
    raise ERangeError.Create('Dictionary level must be between 0 and 9');
  SetDictionary(Level);
end;

procedure TBeeModel.SetTable(const Parameters: TBeeTableParameters);
var
  OriginalTable: Bee_Configuration.TTableParameters;
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
