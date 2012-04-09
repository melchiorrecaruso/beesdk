program iroloz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes
  { you can add units after this };

const
  // Can be changed if understood
  PREFIX_SIZE           =   3; // must be 1, 2 or 3
  HISTORY_BUFFER_BITLEN =  18; // trade between time and compression ratio
  SUFFICIENT_MATCH      =  64; // does not make much difference
  MINIMUM_MATCH         =   4; // must be 1 or larger
  LONGEST_COUNT         = 255; // must be one byte value
  BIT_LEN_FOR_STEPS     =   5; // these are offset indexes, typically 5 bits is enough, limited to 8 bits



begin
end.

