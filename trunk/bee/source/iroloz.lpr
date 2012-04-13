program iroloz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

type
  TDictionary = class
  private

  public
    constructor Create(PrefixSize: longint; OffsetLen: longint);

  end;

  constructor TDictionary.Create(PrefixSize: longint; OffsetLen: longint);
  begin
  end;

begin
end.

