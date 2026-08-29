unit Bee_Mixer;

{$mode objfpc}{$H+}
{$R-}{$Q-}

interface

uses
  Classes, SysUtils, Bee_Model, Bee_RangeCoder;

const
  BeeExpertCount = 4;
  BeeMatchExpertIndex = 4;
  BeeOrderTapCount = 3;
  BeeMixerExpertCount = 5 + BeeExpertCount * BeeOrderTapCount;
  BeeMixerContextCount = 17 * 4 * 8;
  BeeProbabilityScale = 4096;
  BeeMixerTotal = 32768;
  BeeMatchContextBytes = 4;
  BeeMatchLengthLimit = 64;
  BeeOrderTapStages: array[0..BeeOrderTapCount - 1] of Cardinal = (1, 2, 4);

type
  TBeeTableSet = array[0..BeeExpertCount - 1] of TBeeTableParameters;

  TBeeExpert = class
  private
    FModel: TBeeModel;
    FBitsRead, FChunkValue: Cardinal;
  public
    constructor Create(AModel: TBeeModel; const Table: TBeeTableParameters;
      DictionaryLevel: Cardinal);
    destructor Destroy; override;
    procedure PrepareBit(BitPosition: Cardinal);
    procedure GetBitFrequencies(out ZeroFrequency, OneFrequency: Cardinal);
    function GetOrderBitFrequencies(OrderIndex: Cardinal;
      out ZeroFrequency, OneFrequency: Cardinal): Boolean;
    function OrderCount: Cardinal;
    procedure LearnBit(Bit: Cardinal);
    procedure FinishByte;
  end;

  TBeeMixer = class
  private
    FExperts: array[0..BeeExpertCount - 1] of TBeeExpert;
    FWeights: array[0..BeeMixerContextCount - 1,
      0..BeeMixerExpertCount - 1] of LongInt;
    FApm: array[0..7, 0..32] of LongInt;
    FLastPredictions: array[0..BeeMixerExpertCount - 1] of LongInt;
    FPredictionActive: array[0..BeeMixerExpertCount - 1] of Boolean;
    FRawProbability: LongInt;
    FLastSlot: Integer;
    FMatchHeads: array of LongInt;
    FHistory: TBytes;
    FHistorySize: Integer;
    FMatchPredictedByte, FCurrentByte, FPreviousByte: Byte;
    FMatchLength: Integer;
    FMatchValid, FHavePreviousByte: Boolean;
    procedure AppendHistory(Value: Byte);
    procedure PrepareMatch;
    function MatchProbability(BitPosition: Integer): Integer;
    function MixerSlot(BitPosition: Integer): Integer;
    procedure LearnMatch(BitPosition, Bit: Integer);
    function Predict(BitPosition: Integer): Cardinal;
    procedure Learn(BitPosition, Bit: Integer);
    function Calibrate(BitPosition, Probability: Integer): Integer;
    procedure LearnCalibration(BitPosition, Bit, Probability: Integer);
  public
    constructor Create(const Tables: TBeeTableSet; DictionaryLevel: Cardinal);
    destructor Destroy; override;
    procedure EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
    function DecodeByte(RangeCoder: TRangeCoder): Byte;
  end;

implementation

function Clamp(Value, Minimum, Maximum: Int64): LongInt; inline;
begin
  if Value < Minimum then Result := Minimum
  else if Value > Maximum then Result := Maximum
  else Result := Value;
end;

constructor TBeeExpert.Create(AModel: TBeeModel;
  const Table: TBeeTableParameters; DictionaryLevel: Cardinal);
begin
  inherited Create;
  FModel := AModel;
  FModel.SetTable(Table);
  FModel.SetDictionaryLevel(DictionaryLevel);
  FBitsRead := 0;
  FChunkValue := 0;
end;

destructor TBeeExpert.Destroy;
begin
  FModel.Free;
  inherited Destroy;
end;

procedure TBeeExpert.PrepareBit(BitPosition: Cardinal);
var
  PartIndex: Cardinal;
begin
  if FBitsRead <> 0 then Exit;
  if FModel.Bits = 4 then PartIndex := BitPosition div 4 else PartIndex := 0;
  FModel.PrepareSymbol(PartIndex);
end;

procedure TBeeExpert.GetBitFrequencies(out ZeroFrequency, OneFrequency: Cardinal);
var
  Remaining, FirstSymbol, MiddleSymbol, LastSymbol, Symbol: Cardinal;
begin
  Remaining := FModel.Bits - FBitsRead;
  FirstSymbol := FChunkValue shl Remaining;
  MiddleSymbol := FirstSymbol + (Cardinal(1) shl (Remaining - 1));
  LastSymbol := FirstSymbol + (Cardinal(1) shl Remaining) - 1;
  ZeroFrequency := 0;
  OneFrequency := 0;
  for Symbol := FirstSymbol to MiddleSymbol - 1 do
    Inc(ZeroFrequency, FModel.Frequencies[Symbol]);
  for Symbol := MiddleSymbol to LastSymbol do
    Inc(OneFrequency, FModel.Frequencies[Symbol]);
end;

function TBeeExpert.GetOrderBitFrequencies(OrderIndex: Cardinal;
  out ZeroFrequency, OneFrequency: Cardinal): Boolean;
begin
  Result := FModel.GetOrderBitFrequencies(OrderIndex, FChunkValue, FBitsRead,
    ZeroFrequency, OneFrequency);
end;

function TBeeExpert.OrderCount: Cardinal;
begin
  Result := FModel.OrderCount;
end;

procedure TBeeExpert.LearnBit(Bit: Cardinal);
begin
  FChunkValue := (FChunkValue shl 1) or (Bit and 1);
  Inc(FBitsRead);
  if FBitsRead = FModel.Bits then
  begin
    FModel.LearnSymbol(FChunkValue);
    FBitsRead := 0;
    FChunkValue := 0;
  end;
end;

procedure TBeeExpert.FinishByte;
begin
  if FBitsRead <> 0 then
    raise EInvalidOpException.Create('Incomplete Bee expert symbol');
  FModel.FinishByte;
end;

constructor TBeeMixer.Create(const Tables: TBeeTableSet;
  DictionaryLevel: Cardinal);
var
  BitPosition, ExpertIndex, BinIndex, Slot: Integer;
begin
  inherited Create;
  FExperts[0] := TBeeExpert.Create(TBeeOneBitModel.Create, Tables[0], DictionaryLevel);
  FExperts[1] := TBeeExpert.Create(TBeeTwoBitModel.Create, Tables[1], DictionaryLevel);
  FExperts[2] := TBeeExpert.Create(TBeeNibbleModel.Create, Tables[2], DictionaryLevel);
  FExperts[3] := TBeeExpert.Create(TBeeByteModel.Create, Tables[3], DictionaryLevel);
  for Slot := 0 to BeeMixerContextCount - 1 do
  begin
    for ExpertIndex := 0 to BeeMixerExpertCount - 1 do
      if ExpertIndex < BeeExpertCount then
        FWeights[Slot, ExpertIndex] := 256
      else if ExpertIndex = BeeMatchExpertIndex then
        FWeights[Slot, ExpertIndex] := 64
      else
        FWeights[Slot, ExpertIndex] := 16;
  end;
  for BitPosition := 0 to 7 do
    for BinIndex := 0 to 32 do
      FApm[BitPosition, BinIndex] := BinIndex * BeeProbabilityScale div 32;
  SetLength(FMatchHeads, 1 shl 18);
  FillChar(FMatchHeads[0], Length(FMatchHeads) * SizeOf(FMatchHeads[0]), $FF);
  SetLength(FHistory, 4096);
end;

destructor TBeeMixer.Destroy;
var
  I: Integer;
begin
  for I := 0 to BeeExpertCount - 1 do FExperts[I].Free;
  inherited Destroy;
end;

procedure TBeeMixer.AppendHistory(Value: Byte);
var
  NewCapacity: Integer;
begin
  if FHistorySize = Length(FHistory) then
  begin
    NewCapacity := Length(FHistory) * 2;
    if NewCapacity < 4096 then NewCapacity := 4096;
    SetLength(FHistory, NewCapacity);
  end;
  FHistory[FHistorySize] := Value;
  Inc(FHistorySize);
end;

procedure TBeeMixer.PrepareMatch;
var
  HashValue: Cardinal;
  HashIndex, Candidate, I: Integer;
begin
  FMatchValid := False;
  FMatchLength := 0;
  if FHistorySize < BeeMatchContextBytes then Exit;
  HashValue := 2166136261;
  for I := FHistorySize - BeeMatchContextBytes to FHistorySize - 1 do
    HashValue := (HashValue xor FHistory[I]) * 16777619;
  HashIndex := HashValue and (Length(FMatchHeads) - 1);
  Candidate := FMatchHeads[HashIndex];
  FMatchHeads[HashIndex] := FHistorySize;
  if (Candidate < BeeMatchContextBytes) or (Candidate >= FHistorySize) then Exit;
  for I := 1 to BeeMatchContextBytes do
    if FHistory[FHistorySize - I] <> FHistory[Candidate - I] then Exit;
  FMatchLength := BeeMatchContextBytes;
  while (FMatchLength < BeeMatchLengthLimit) and
        (FHistorySize > FMatchLength) and (Candidate > FMatchLength) and
        (FHistory[FHistorySize - FMatchLength - 1] =
         FHistory[Candidate - FMatchLength - 1]) do
    Inc(FMatchLength);
  FMatchPredictedByte := FHistory[Candidate];
  FMatchValid := True;
end;

function TBeeMixer.MatchProbability(BitPosition: Integer): Integer;
var
  CorrectProbability, PredictedBit: Integer;
begin
  if not FMatchValid then Exit(BeeProbabilityScale div 2);
  CorrectProbability := BeeProbabilityScale -
    BeeProbabilityScale div (FMatchLength + 2);
  CorrectProbability := Clamp(CorrectProbability,
    BeeProbabilityScale div 2, BeeProbabilityScale - 1);
  PredictedBit := (FMatchPredictedByte shr (7 - BitPosition)) and 1;
  if PredictedBit = 1 then Result := CorrectProbability
  else Result := BeeProbabilityScale - CorrectProbability;
end;

function TBeeMixer.MixerSlot(BitPosition: Integer): Integer;
var
  PreviousContext, MatchBucket: Integer;
begin
  if BitPosition = 0 then PrepareMatch;
  if FHavePreviousByte then PreviousContext := 1 + (FPreviousByte shr 4)
  else PreviousContext := 0;
  if not FMatchValid then MatchBucket := 0
  else if FMatchLength <= 7 then MatchBucket := 1
  else if FMatchLength <= 15 then MatchBucket := 2
  else MatchBucket := 3;
  Result := (MatchBucket * 17 + PreviousContext) * 8 + BitPosition;
end;

procedure TBeeMixer.LearnMatch(BitPosition, Bit: Integer);
var
  PredictedBit: Integer;
begin
  FCurrentByte := (FCurrentByte shl 1) or Byte(Bit and 1);
  if FMatchValid then
  begin
    PredictedBit := (FMatchPredictedByte shr (7 - BitPosition)) and 1;
    if PredictedBit <> Bit then FMatchValid := False;
  end;
  if BitPosition = 7 then
  begin
    AppendHistory(FCurrentByte);
    FPreviousByte := FCurrentByte;
    FHavePreviousByte := True;
    FCurrentByte := 0;
    FMatchValid := False;
  end;
end;

function TBeeMixer.Calibrate(BitPosition, Probability: Integer): Integer;
var
  Coordinate, LowerBin, Fraction, Calibrated: Integer;
begin
  Coordinate := Probability * 32;
  LowerBin := Coordinate div BeeProbabilityScale;
  if LowerBin >= 32 then Exit(BeeProbabilityScale - 1);
  Fraction := Coordinate mod BeeProbabilityScale;
  Calibrated := (FApm[BitPosition, LowerBin] * (BeeProbabilityScale - Fraction) +
    FApm[BitPosition, LowerBin + 1] * Fraction) div BeeProbabilityScale;
  Result := Clamp((Probability + Calibrated) div 2, 1, BeeProbabilityScale - 1);
end;

procedure TBeeMixer.LearnCalibration(BitPosition, Bit, Probability: Integer);
var
  Coordinate, LowerBin, Fraction, Target, Error: Integer;
begin
  Coordinate := Probability * 32;
  LowerBin := Coordinate div BeeProbabilityScale;
  if LowerBin >= 32 then LowerBin := 31;
  Fraction := Coordinate mod BeeProbabilityScale;
  Target := Bit * BeeProbabilityScale;
  Error := Target - FApm[BitPosition, LowerBin];
  Inc(FApm[BitPosition, LowerBin],
    (Int64(Error) * (BeeProbabilityScale - Fraction)) div (BeeProbabilityScale * 32));
  Error := Target - FApm[BitPosition, LowerBin + 1];
  Inc(FApm[BitPosition, LowerBin + 1],
    (Int64(Error) * Fraction) div (BeeProbabilityScale * 32));
end;

function TBeeMixer.Predict(BitPosition: Integer): Cardinal;
var
  I, TapIndex, PredictionIndex: Integer;
  ZeroFrequency, OneFrequency: Cardinal;
  Weighted, WeightTotal: Int64;
begin
  FillChar(FPredictionActive, SizeOf(FPredictionActive), 0);
  FLastSlot := MixerSlot(BitPosition);
  Weighted := 0;
  WeightTotal := 0;
  for I := 0 to BeeExpertCount - 1 do
  begin
    FExperts[I].PrepareBit(BitPosition);
    FExperts[I].GetBitFrequencies(ZeroFrequency, OneFrequency);
    FLastPredictions[I] :=
      (Int64(OneFrequency) * BeeProbabilityScale) div (ZeroFrequency + OneFrequency);
    FPredictionActive[I] := True;
  end;
  if FMatchValid then
  begin
    FLastPredictions[BeeMatchExpertIndex] := MatchProbability(BitPosition);
    FPredictionActive[BeeMatchExpertIndex] := True;
  end;
  for I := 0 to BeeExpertCount - 1 do
    for TapIndex := 0 to BeeOrderTapCount - 1 do
    begin
      PredictionIndex := 5 + I * BeeOrderTapCount + TapIndex;
      if (FExperts[I].OrderCount > BeeOrderTapStages[TapIndex] + 1) and
         FExperts[I].GetOrderBitFrequencies(BeeOrderTapStages[TapIndex],
           ZeroFrequency, OneFrequency) then
      begin
        FLastPredictions[PredictionIndex] :=
          (Int64(OneFrequency) * BeeProbabilityScale) div
          (ZeroFrequency + OneFrequency);
        FPredictionActive[PredictionIndex] := True;
      end;
    end;
  for I := 0 to BeeMixerExpertCount - 1 do
    if FPredictionActive[I] then
    begin
      Inc(Weighted, Int64(FWeights[FLastSlot, I]) * FLastPredictions[I]);
      Inc(WeightTotal, FWeights[FLastSlot, I]);
    end;
  if WeightTotal = 0 then
    FRawProbability := BeeProbabilityScale div 2
  else
    FRawProbability := Clamp(Weighted div WeightTotal, 1, BeeProbabilityScale - 1);
  Result := Calibrate(BitPosition, FRawProbability);
end;

procedure TBeeMixer.Learn(BitPosition, Bit: Integer);
var
  I: Integer;
  Error, Delta: Int64;
begin
  Error := Int64(Bit * BeeProbabilityScale) - FRawProbability;
  for I := 0 to BeeMixerExpertCount - 1 do
    if FPredictionActive[I] then
    begin
      Delta := Error * (FLastPredictions[I] - FRawProbability) div
        (Int64(BeeProbabilityScale) * 64);
      FWeights[FLastSlot, I] := Clamp(FWeights[FLastSlot, I] + Delta, 16, 4096);
    end;
  for I := 0 to BeeExpertCount - 1 do FExperts[I].LearnBit(Bit);
  LearnMatch(BitPosition, Bit);
  LearnCalibration(BitPosition, Bit, FRawProbability);
end;

procedure TBeeMixer.EncodeByte(Value: Byte; RangeCoder: TRangeCoder);
var
  BitPosition, Bit: Integer;
  Probability, OneFrequency, ZeroFrequency: Cardinal;
begin
  for BitPosition := 0 to 7 do
  begin
    Probability := Predict(BitPosition);
    OneFrequency := Clamp((Int64(Probability) * BeeMixerTotal) div
      BeeProbabilityScale, 1, BeeMixerTotal - 1);
    ZeroFrequency := BeeMixerTotal - OneFrequency;
    Bit := (Value shr (7 - BitPosition)) and 1;
    if Bit = 0 then RangeCoder.Encode(0, ZeroFrequency, BeeMixerTotal)
    else RangeCoder.Encode(ZeroFrequency, OneFrequency, BeeMixerTotal);
    Learn(BitPosition, Bit);
  end;
  for Bit := 0 to BeeExpertCount - 1 do FExperts[Bit].FinishByte;
end;

function TBeeMixer.DecodeByte(RangeCoder: TRangeCoder): Byte;
var
  BitPosition, Bit, I: Integer;
  Probability, OneFrequency, ZeroFrequency, Target: Cardinal;
begin
  Result := 0;
  for BitPosition := 0 to 7 do
  begin
    Probability := Predict(BitPosition);
    OneFrequency := Clamp((Int64(Probability) * BeeMixerTotal) div
      BeeProbabilityScale, 1, BeeMixerTotal - 1);
    ZeroFrequency := BeeMixerTotal - OneFrequency;
    Target := RangeCoder.GetFreq(BeeMixerTotal);
    if Target < ZeroFrequency then
    begin
      Bit := 0;
      RangeCoder.Decode(0, ZeroFrequency, BeeMixerTotal);
    end
    else
    begin
      Bit := 1;
      RangeCoder.Decode(ZeroFrequency, OneFrequency, BeeMixerTotal);
    end;
    Result := (Result shl 1) or Bit;
    Learn(BitPosition, Bit);
  end;
  for I := 0 to BeeExpertCount - 1 do FExperts[I].FinishByte;
end;

end.
