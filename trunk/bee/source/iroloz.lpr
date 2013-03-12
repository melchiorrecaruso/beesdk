program iroloz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes,
  SysUtils;

type
  TDictionary = class
  private
    m_last_position_lookup: array of longint;
    m_self_addressed_dictionary: array of longint;
    m_prefix_mask: longint;
    m_buffer_mask: longint;
    m_context: longint;
    m_index: longint;
  public
    constructor Create(PrefixSize: longint; OffsetLen: longint);
    destructor Destroy; override;
    procedure UpdateDictionary(Symbol: byte);
    function GetNextPosition(Position: longint): longint;
    procedure EraseData;
  end;

  constructor TDictionary.Create(PrefixSize: longint; OffsetLen: longint);
  begin
    case PrefixSize of
      1: m_prefix_mask := $FF;
      2: m_prefix_mask := $FFFF;
    else m_prefix_mask := $FFFFFF;
    end;
    m_buffer_mask := (1 shl OffsetLen) - 1;
    SetLength(m_last_position_lookup, m_prefix_mask + 1);
    SetLength(m_self_addressed_dictionary, m_buffer_mask + 1);
    EraseData;
  end;

  destructor TDictionary.Destroy;
  begin
    m_self_addressed_dictionary := nil;
    m_last_position_lookup      := nil;
  end;

  procedure TDictionary.UpdateDictionary(Symbol: byte);
  begin
    m_context := m_context shl 8;
    m_context := m_context or Symbol;
    m_context := m_context and m_prefix_mask;
    m_self_addressed_dictionary[m_index and m_buffer_mask] := m_last_position_lookup[m_context];
    m_last_position_lookup[m_context] := m_index;
    Inc(m_index);
  end;

  function TDictionary.GetNextPosition(Position: longint): longint;
  begin
    result := m_self_addressed_dictionary[position and m_buffer_mask];
  end;

  procedure TDictionary.EraseData;
  var
    I: longint;
  begin
    for I := 0 to m_prefix_mask do m_last_position_lookup     [I] := 0;
    for I := 0 to m_buffer_mask do m_self_addressed_dictionary[I] := 0;

    m_context := 0;
    m_index   := 0;
  end;

// Several following functions and classes are taken from open source
// http://balz.sourceforge.net/ and written by Ilia Muraviev. I found
// it very convenient. I added only some cosmetic changes.
(*
procedure ExeTransform(var Data; y: longint; n: longint);
var
  Buf: array [0..$FFFFFFF] of byte absolute Data;
  I, E: longint;
  P: ^longint;
  Addr: ^longint;
begin
  E := n - 8;

  // search for pe file header
  I := 0;
  repeat
    P := @Buf[I];
    if (P^ = $4550) then Break;

    Inc(I);
  until (I < E);

  // perform call/jmp address translation
  while (I < E) do
  begin
    Inc(I);
    if (Buf[I - 1] and 254) = $e8 then
    begin
      Addr := @Buf[I];

      if (boolean(y)) then
      begin
        if (Addr^ >= -1) and (Addr^ < (n-1)) then Inc(Addr^, I)
        else if (Addr^ > 0) and (Addr^ < n) then Dec(Addr^, n);
      end else
      begin
        if (Addr^ < 0) then
        begin
          if ((Addr^ + I) >= 0) then Inc(Addr^, n);
        end else
          if (Addr^ < n) then Dec(Addr^, I);
      end;
      Inc(I, 4);
    end;
  end;
end;

*)

//source: http://balz.sourceforge.net/

type
  TPredictor = class
  private
    p1: word;
    p2: word;
  public
    constructor Create;
    function P: longint;
    procedure Update(Bit: longint);
  end;

  constructor TPredictor.Create;
  begin
    p1 := 1 shl 15;
    p2 := 1 shl 15;
  end;

  function TPredictor.P: longint;
  begin
    Result :=  p1 + p2;
  end;

  procedure TPredictor.Update(Bit: longint);
  begin
    if boolean(Bit) then
    begin
      p1 := p1 + word((not p1) shr 3);
      p2 := p2 + word((not p2) shr 6);
    end else
    begin
      p1 := p1 - word(p1 shr 3);
      p2 := p2 - word(p2 shr 6);
    end;
  end;

// This coder Ilia Muraviev derived from Mahoney's fpaq0
// source: http://balz.sourceforge.net/

type
  TEncoder = class
  private
    x1: longword;
    x2: longword;
  public
    constructor Create;
    procedure Encode(P: longint; Bit: longint);
    procedure Flush;
  end;

  constructor TEncoder.Create;
  begin
    x1 :=  0;
    x2 := longword(-1);
  end;

  procedure TEncoder.Encode(P: longint; Bit: longint);
  var
    xmid: longword;
  begin
    xmid := x1 + ((x2 - x1) * P) shr 17;
    if Boolean(Bit) then
      x2 := xmid
    else
      x1 := xmid + 1;

    while (x1 xor x2) < (1 shl 24) do
    begin
      // writeByte(x2 >> 24);
      x1 := x1 shl 8;
      x2 := x2 shl 8 + 255;
    end;
  end;

  procedure TEncoder.Flush;
  var
    I: longint;
  begin
    for I := 0 to 3 do
    begin
      // writeByte(x2 >> 24);
      x2 := x2 shl 8;
    end;
  end;

type
  TDecoder = class
  private
    x:  longword;
    x1: longword;
    x2: longword;
  public
    constructor Create;
    procedure Init;


  end;

  constructor TDecoder.Create;
  begin
    x1 := 0;
    x2 := longword(-1);
  end;

  procedure TDecoder.Init;
  var
    I: longint;
  begin



  end;



const

  DataSize = 50;

  Prefix_Size = 2; // can be 1, 2 or 3. It means how many elements we compare.

var
  index: longint;
  Position: longint;
  New_Position: longint;
  Dictionary: TDictionary;

  current_word: longint;
  offset_word: longint;

  k: longint;

  Data: array of byte;

begin

  // 5 is a bitlength of circular history buffer. Since it is circular buffer it looks back on
  // 64 elements, twice longer than max 5 bit number. If you change it to 4 it start looking back on 32 elements.

  Dictionary := TDictionary.Create(Prefix_Size, 5);

  SetLength(Data, DataSize);
  for k := 0 to DataSize - 1 do
  begin
    Data[k] := (k + 1) mod 10;
  end;

  index := 0;
  repeat
    Dictionary.UpdateDictionary(Data[index]);
    position := index;
    while true do
    begin
      New_Position := Dictionary.getNextPosition(Position);

      if (new_position >= position) then Break; // positions should only going back in history
      if (new_position < prefix_size - 1) then Break; // should not go to negative array index

      // next word match verification is optional, it works without it since in encoding
      // and decoding operations are going in the same way
      current_word := 0;
      offset_word  := 0;

      for k := 0 to prefix_size - 1 do
      begin
        current_word := current_word shl 8;
        current_word := current_word or Data[index - k];
        offset_word  := offset_word  shl 8;
        offset_word  := offset_word  or Data[new_position - k];
      end;

      if (current_word <> offset_word) then Break;
      // end of optional part

      Writeln(Format('%8u %8u %8u %8u', [current_word, offset_word, new_position, index]));
      position := new_position;
    end;
    Writeln('-----------------');
    Inc(Index);
  until Index = DataSize;
  Dictionary.Destroy;
end.

