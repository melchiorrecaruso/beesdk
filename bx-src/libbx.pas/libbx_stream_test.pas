program libbx_stream_test;

uses
  libbx_stream,
  SysUtils;

function Fill(Src: Pointer; Buf: PByte; Count: longint): longint;
begin
  Fill := FileRead(THandle(Src), Buf, Count);
end;

function Flush(Dst: Pointer; Buf: PByte; Count: longint): longint; 
begin
   Flush := FileWrite(THandle(Dst), Buf, Count);
end; 

var
  data: byte;
  Src: THandle;
  dst: THandle;
  start: double;
  size: int64;
  reader: PReadStream;
  writer: PWriteStream;
  
begin
  Writeln('PASCAL-STREAM-UNIT [SPEEDTEST]');
  Start := Now;
  
  src := FileOpen(ParamStr(1), fmOpenRead);
  if src = THandle(-1) then Halt;
  
  dst := FileCreate(ParamStr(1)  + '.TEST');
  if dst = THandle(-1) then Halt;  
  
  reader := ReadStream_Create(Pointer(Src), @Fill);
  writer := WriteStream_Create(Pointer(Dst), @Flush);
  
  size := 1000000000;
  while size > 0 do
  begin
    data := ReadStream_Read(reader);
	WriteStream_Write(writer, data);
  
    Dec(size);
  end;
  
  ReadStream_Destroy(reader);
  WriteStream_Destroy(writer);
  
  FileClose(src);
  FileClose(dst);
  
  Writeln('seconds', Format('%0.2f', [(Now - Start) * (24 * 60 * 60)]));
end.