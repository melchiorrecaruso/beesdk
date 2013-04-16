#include "bxlib.h"
#include "bxlib_ppmd7.h"

PPpmdRangeEnc PpmdRangeEnc_Create (void *aStream, PStreamWrite aStreamWrite)
{
  PPpmdRangeEnc Self = malloc(sizeof(CPpmd7z_RangeEnc));
  Self->Stream = WriteStream_Create(aStream, aStreamWrite);
  return Self;
};

void PpmdRangeEnc_Destroy(PPpmdRangeEnc Self)
{
  WriteStream_Destroy(Self->Stream);
  free(Self);
};

void PpmdRangeEnc_StartEncode (PPpmdRangeEnc Self)
{
  Ppmd7z_RangeEnc_Init(Self);
};

void PpmdRangeEnc_FinishEncode(PPpmdRangeEnc Self)
{
  Ppmd7z_RangeEnc_FlushData(Self);
  WriteStream_FlushBuffer(Self->Stream);
};

PPpmdRangeDec PpmdRangeDec_Create (void *aStream, PStreamRead aStreamRead)
{
  PPpmdRangeDec Self = malloc(sizeof(CPpmd7z_RangeDec));
  Self->Stream = ReadStream_Create(aStream, aStreamRead);
  Ppmd7z_RangeDec_CreateVTable(Self);
  return Self;
};

void PpmdRangeDec_Destroy(PPpmdRangeDec Self)
{
  ReadStream_Destroy(Self->Stream);
  free(Self);
};

void PpmdRangeDec_StartDecode(PPpmdRangeDec Self)
{
  Ppmd7z_RangeDec_Init(Self);
};

void PpmdRangeDec_FinishDecode(PPpmdRangeDec Self)
{
  ReadStream_ClearBuffer(Self->Stream);
};

PPpmdModeller PpmdModeller_Create()
{
  PPpmdModeller Self = malloc(sizeof(CPpmd7));
  Ppmd7_Construct(Self);
  return Self;
};

void PpmdModeller_Destroy(PPpmdModeller Self)
{
  Ppmd7_Free(Self);
  free(Self);
};

void PpmdModeller_Init(PPpmdModeller Self, uint32_t MemLev, uint32_t ModOrd)
{
  Ppmd7_Alloc(Self, MemLev);
  Ppmd7_Init(Self, ModOrd);
};

inline int32_t PpmdModeller_Encode(PPpmdModeller Self, PPpmdRangeEnc RangeEnc, uint8_t *Buffer, int32_t BufSize)
{
  int32_t I;
  for (I = 0; I < BufSize; I++)
  {
    Ppmd7_EncodeSymbol(Self, RangeEnc, Buffer[I]);
  }
  return I;
};

inline int32_t PpmdModeller_Decode(PPpmdModeller Self, PPpmdRangeDec RangeDec, uint8_t *Buffer, int32_t BufSize)
{
  int32_t I;
  for (I = 0; I < BufSize; I++)
  {
   Buffer[I] = Ppmd7_DecodeSymbol(Self, &(RangeDec->p));
  }
  return I;
};

