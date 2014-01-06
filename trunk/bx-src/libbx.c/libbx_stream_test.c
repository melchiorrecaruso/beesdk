
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include "libbx_stream.h"

int32_t Fill(void* Stream, void* Buf, int32_t Count) {

 return fread (Buf, 1, Count, Stream);
}

int32_t Flush(void* Stream, void* Buf, int32_t Count) {

 return fwrite (Buf, 1, Count, Stream);
}

int main(int argc, char** argv) {

  printf ("STREAM-UNIT [SPEEDTEST]\n");

  time_t now;
  time(&now);

  FILE* src = fopen ( "enwik9" , "rb" );
  if (src==NULL) {
    fputs ("File error", stderr);
    exit (1);
  }

  FILE* dst = fopen ( "enwik9.TEST" , "wb" );
  if (dst==NULL) {
    fputs ("File error", stderr);
    exit (1);
  }

  int64_t Count = 1000000000;

   PReadStream  ReadStream =  ReadStream_Create(src, Fill);
  PWriteStream WriteStream = WriteStream_Create(dst, Flush);

  uint8_t Data;
  while (Count > 0) {

    Data = ReadStream_Read(ReadStream);
    WriteStream_Write(WriteStream, Data);

    Count--;
  }

  ReadStream_Destroy(ReadStream);
  fclose (src);

  WriteStream_Destroy(WriteStream);
  fclose (dst);

  time_t later;
  time(&later);
  double seconds = difftime(later, now);
  printf("%.f seconds\n", seconds);

  return 0;
}
