#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>
#include <time.h>
#include "libbx_stream.h"
#include "libbx_bee_rangecoder.h"
#include "libbx_bee_modeller.h"
#include "libbx_bee_common.h"

int32_t Fill(void* Stream, void* Buf, int32_t Count) {

 return fread (Buf, 1, Count, Stream);
}

int32_t Flush(void* Stream, void* Buf, int32_t Count) {

 return fwrite (Buf, 1, Count, Stream);
}

int main(int argc, char** argv) {

  printf ("BX1 TEST-SPEED\n");

  time_t now;
  time(&now);

  FILE* src = fopen ( "enwik8" , "rb" );
  if (src==NULL) {
    fputs ("File error", stderr);
    exit (1);
  }

  FILE* dst = fopen ( "enwik8.bx1" , "wb" );
  if (dst==NULL) {
    fputs ("File error", stderr);
    exit (1);
  }

  PBeeRangeEnc REncoder = BeeRangeEnc_Create(dst, Flush);
  PBeeModeller Modeller = BeeModeller_Create(REncoder);

  BeeModeller_SetTableParameters(Modeller, &DefaultTableParameters);
  BeeModeller_SetDictionaryLevel(Modeller, 7);
  BeeModeller_FreshFlexible(Modeller);

  BeeRangeEnc_StartEncode(REncoder);

  uint8_t* Buf = (uint8_t*) malloc (sizeof(uint8_t) * 4096);

  uint32_t Readed = fread(Buf, 1, 4096, src);
  while (Readed>0) {
    BeeModeller_Encode(Modeller, Buf, Readed);
    // printf ("Encoded: %d\n", Readed);
    Readed = fread(Buf, 1, 4096, src);
  }
  BeeRangeEnc_FinishEncode(REncoder);

  BeeModeller_Destroy(Modeller);
  BeeRangeEnc_Destroy(REncoder);
  fclose (src);

  int dstsize = ftell (dst);
  printf("%d bytes\n", dstsize);

  fclose (dst);

  time_t later;
  time(&later);
  double seconds = difftime(later, now);

  printf("%.f seconds\n", seconds);

  return 0;
}
