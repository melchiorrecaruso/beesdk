program Test;

{$I compiler.inc}

uses
  Classes,
  SysUtils,
  Bee_Files,
  Bee_Common,
  Bee_MainPacker2,
  Bee_Configuration;

var
  X: double;
  FTable: TTableParameters;
  SrcStream: TFileReader;
  DstStream: TFileWriter;
  FStreamCoder: TStreamCoder;
  FConfiguration: TConfiguration;

begin
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

  Writeln(TimeDifference(X));

  FConfiguration.Free;
end.

