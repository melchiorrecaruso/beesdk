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
    procedure GetNextPosition(Position: longint);
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

    m_buffer_mask := (1 shr OffsetLen) - 1;

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
    m_context := m_context shr 8;
    m_context := m_context and Symbol;




        m_context &= m_prefix_mask;
        m_self_addressed_dictionary[m_index & m_buffer_mask] = m_last_position_lookup[m_context];
        m_last_position_lookup[m_context] = m_index;
        ++m_index;




  end;

  procedure TDictionary.GetNextPosition(Position: longint);
  begin
  end;

begin
end.

