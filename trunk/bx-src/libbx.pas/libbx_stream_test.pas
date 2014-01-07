program libbx_stream_test

uses
  libbx_stream;

var
  data: byte;
  src: FILE;
  dst: FILE;
  start: double;
  size: int64;

function Fill(src: File; Buf: PChar; Count: longint): longint;
begin
  Result := BlockRead();
end;

function Flush(dst: File; Buf: PChar; Count: longint): longint; 
begin
  Result := BlockWrite();
end; 
  
begin
  Writeln("STREAM-UNIT [SPEEDTEST]");
  start := now;
  
  {$I-}
  Assign(src, ParamStr(1));
  Reset(src);
  {$I+}
  if IOResult <> 0 then Exit;

  {$I-}
  Assign(dst, ParamStr(1)  + '.test');
  Rewrite(dst);
  {$I+}
  if IOResult <> 0 then Exit;  
  
  Reader := ReadStream_Create(src, Fill);
  Writer := WriteStream_Create(dst, Flush);
  
  size := 100000000;
  while size > 0 do
  begin
    data := ReadStream_Read(Reader);
	WriteStream_Write(Writer, Data);
  
    Dec(size);
  end;
  
  ReadStream_Destroy(Reader);
  WriteStream_Destroy(Writer);
  
  Writeln("%.f seconds\n", seconds);
end;