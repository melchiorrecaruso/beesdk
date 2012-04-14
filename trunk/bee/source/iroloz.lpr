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
  end;

  constructor TDictionary.Create(PrefixSize: longint; OffsetLen: longint);
  var
    I: longint;
  begin

    case PrefixSize of
      1: m_prefix_mask := $FF;
      2: m_prefix_mask := $FFFF;
    else m_prefix_mask := $FFFFFF;
    end;

    SetLength(m_last_position_lookup, m_prefix_mask + 1);
    for I := 0 to m_prefix_mask do
    begin
      m_last_position_lookup[I] := 0;
    end;

    m_buffer_mask := (1 shl OffsetLen) - 1;

    SetLength(m_self_addressed_dictionary, m_buffer_mask + 1);
    for I := 0 to m_buffer_mask do
    begin
      m_self_addressed_dictionary[I] := 0;
    end;

    m_context := 0;
    m_index   := 0;
  end;

  destructor TDictionary.Destroy;
  begin
    m_self_addressed_dictionary := nil;
    m_last_position_lookup      := nil;
  end;

  procedure TDictionary.UpdateDictionary(Symbol: byte);
  begin
    Writeln('TDictionary.UpdateDictionary::START');

    m_context := m_context shl 8;
    m_context := m_context or Symbol;
    m_context := m_context and m_prefix_mask;

    m_self_addressed_dictionary[m_index and m_buffer_mask] := m_last_position_lookup[m_context];
    m_last_position_lookup[m_context] := m_index;

    Inc(m_index);



    Writeln(m_context);
    Writeln('TDictionary.UpdateDictionary::END');
  end;

  function TDictionary.GetNextPosition(Position: longint): longint;
  begin
    result := m_self_addressed_dictionary[position and m_buffer_mask];
  end;

const
  Data     = '12345678901234567890123456789012345678901234567890';
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

begin

  // 5 is a bitlength of circular history buffer. Since it is circular buffer it looks back on
  // 64 elements, twice longer than max 5 bit number. If you change it to 4 it start looking back on 32 elements.

  Dictionary := TDictionary.Create(Prefix_Size, 5);

  index := 0;

  repeat
    Dictionary.UpdateDictionary(byte(Data[index]));
    position := index;

    while True do
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
        current_word := current_word or byte(Data[index - k]);

        offset_word  := offset_word  shl 8;
        offset_word  := offset_word  or byte(Data[new_position - k]);
      end;

      if (current_word <> offset_word) then Break;
      // end of optional part

      Writeln(Format('%u %u %D %D', [current_word, offset_word, new_position, index]));
      position := new_position;
    end;
  Writeln('-------------------');
  Inc(Index);
  until Index < DataSize;
  Dictionary.Destroy;
end.

