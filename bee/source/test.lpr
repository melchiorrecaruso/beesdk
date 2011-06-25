program Test;

{$I compiler.inc}
{$DEFINE REGCALL}

uses
  Classes,
  SysUtils,
  //Bee_Files,
  Bee_Common;
  //Bee_MainPacker,
  //Bee_Configuration;

const
  CCOUNT = 10;

var
  X: double;
  I: longint;
  A: array [0.. $FFFFFFF]of byte;
  // FTable: TTableParameters;
  // SrcStream: TFileReader;
  // DstStream: TFileWriter;
  // FStreamCoder: TStreamCoder;
  // FConfiguration: TConfiguration;

begin
  (*
  X := Now;
  FConfiguration := TConfiguration.Create;
  if FileExists('Bee.ini') then
    FConfiguration.LoadFromFile('Bee.ini')
  else
    Writeln('ConfigurationError');

  { load method and dictionary level }
  FConfiguration.Selector('\main');
  FConfiguration.CurrentSection.Values['Method']     := IntToStr(3);
  FConfiguration.CurrentSection.Values['Dictionary'] := IntToStr(7);

  SrcStream := TFileReader.Create('FP.log', fmOpenRead);
  DstStream := TFileWriter.Create('FP.bee', fmCreate);

  FStreamCoder := TStreamCoder.Create(DstStream);
  FStreamCoder.SetDictionary(7);

  FConfiguration.GetTable('.log', FTable);
  FStreamCoder.SetTable(FTable);

  FStreamCoder.FreshFlexible;
  FStreamCoder.Encode(SrcStream, SrcStream.Size);

  FStreamCoder.Free;;

  SrcStream.Free;
  DstStream.Free;

  FConfiguration.Free;
  *)

  Writeln('SPEED-TEST');

  X := Now;
  for I := 0 to CCOUNT do
  begin
    WRITELN('FillChar(', I,')');
    MyFillChar(A, Length(A), Char(I));
  end;
  WRITELN('END');

  Writeln(TimeDifference(X));
end.

