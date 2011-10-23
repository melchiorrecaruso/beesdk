{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit beelib; 

interface

uses
  BeeLib_Assembler, BeeLib_Codec, BeeLib_Configuration, BeeLib_Crc, 
  BeeLib_Interface, BeeLib_Modeller, BeeLib_RangeCoder, BeeLib_Stream, 
  BeeLib_StreamCoder, LazarusPackageIntf;

implementation

procedure Register; 
begin
end; 

initialization
  RegisterPackage('beelib', @Register); 
end.
