{ This file was automatically created by Lazarus. Do not edit!
  This source is only used to compile and install the package.
 }

unit beesdk;

interface

uses
  BeeSDK_Archive, Bee_BufStream, Bee_Files, LazarusPackageIntf;

implementation

procedure Register;
begin
end;

initialization
  RegisterPackage('beesdk', @Register);
end.
