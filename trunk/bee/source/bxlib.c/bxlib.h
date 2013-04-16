#ifndef PPMDLIB_MAIN_H
#define PPMDLIB_MAIN_H

#include "bxlib_stream.h"
#include "bxlib_ppmd7.h"

typedef CPpmd7z_RangeEnc *PPpmdRangeEnc;
typedef CPpmd7z_RangeDec *PPpmdRangeDec;
typedef CPpmd7           *PPpmdModeller;

  PPpmdRangeEnc PpmdRangeEnc_Create      (void *aStream, PStreamWrite aStreamWrite);
           void PpmdRangeEnc_Destroy     (PPpmdRangeEnc Self);
           void PpmdRangeEnc_StartEncode (PPpmdRangeEnc Self);
           void PpmdRangeEnc_FinishEncode(PPpmdRangeEnc Self);

  PPpmdRangeDec PpmdRangeDec_Create      (void *aStream, PStreamRead aStreamRead);
           void PpmdRangeDec_Destroy     (PPpmdRangeDec Self);
           void PpmdRangeDec_StartDecode (PPpmdRangeDec Self);
           void PpmdRangeDec_FinishDecode(PPpmdRangeDec Self);

  PPpmdModeller PpmdModeller_Create      ();
           void PpmdModeller_Destroy     (PPpmdModeller Self);
           void PpmdModeller_Init        (PPpmdModeller Self, uint32_t MemLev, uint32_t ModOrd);

        int32_t PpmdModeller_Encode      (PPpmdModeller Self, PPpmdRangeEnc RangeEnc, uint8_t *Buffer, int32_t BufSize);
        int32_t PpmdModeller_Decode      (PPpmdModeller Self, PPpmdRangeDec RangeDec, uint8_t *Buffer, int32_t BufSize);

#endif
