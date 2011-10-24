#include "beelib_main.h"

#include "beelib_crc.h"
#include "beelib_codec.h"
#include "beelib_types.h"
#include "beelib_stream.h"
#include "beelib_modeller.h"
#include "beelib_assembler.h"

struct TStreamCoder
{
  void* Tick;
  TTickEvent OnTick;
  TStream* Stream;
  TFillEvent OnFill;
  TFlushEvent OnFlush;
  TBaseCoder* PPM;
  TSecondaryCodec* SecondaryCodec;
};


LIB_API unsigned int DllVersion()
{
  return 107;
}

LIB_API void SetDictionaryLevel(void* Handle, signed int Value)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->SetDictionary(Value);
}

LIB_API void SetTableParameters(void* Handle, const TTableParameters& Value)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->SetTable(Value);
}

LIB_API void FreshFlexible(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->FreshFlexible();
}

LIB_API void FreshSolid(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->FreshSolid();
}

LIB_API void* CreateEncoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv)
{
  TStreamCoder* StreamCoder = new TStreamCoder;

  StreamCoder->Tick    = TickPtr;
  StreamCoder->OnTick  = OnTickEv;

  StreamCoder->Stream  = new TWriteStream(StrmPtr, OnFlushEv);
  StreamCoder->OnFill  = OnFillEv;
  StreamCoder->OnFlush = OnFlushEv;

  StreamCoder->SecondaryCodec = new TSecondaryEncoder(StreamCoder->Stream);
  StreamCoder->PPM            = new TBaseCoder(StreamCoder->SecondaryCodec);

  return StreamCoder;
}

LIB_API int64 Encode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC)
{
	               CRC = (unsigned int)-1;
          int64 result = 0;
  unsigned char Symbol = 0;

  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;

  TReadStream* Source = new TReadStream(StrmPtr, StreamCoder->OnFill);

  StreamCoder->Stream->ClearBuffer();
  StreamCoder->SecondaryCodec->Start();
  while (result < Size)
  {
    Symbol = Source->Read();
    StreamCoder->PPM->UpdateModel(Symbol);
    UpdCrc32(CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
      if (StreamCoder->OnTick != NULL)
	    if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  StreamCoder->SecondaryCodec->Flush();
  StreamCoder->Stream->FlushBuffer();
  (*Source).~TReadStream();

  return result;
}

LIB_API void* CreateDecoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv)
{
  TStreamCoder* StreamCoder = new TStreamCoder;

  StreamCoder->Tick    = TickPtr;
  StreamCoder->OnTick  = OnTickEv;

  StreamCoder->Stream  = new TReadStream(StrmPtr, OnFillEv);
  StreamCoder->OnFill  = OnFillEv;
  StreamCoder->OnFlush = OnFlushEv;

  StreamCoder->SecondaryCodec = new TSecondaryDecoder(StreamCoder->Stream);
  StreamCoder->PPM            = new TBaseCoder(StreamCoder->SecondaryCodec);

  return StreamCoder;
}

LIB_API int64 Decode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC)
{
                   CRC = (unsigned int)-1;
          int64 result = 0;
  unsigned char Symbol = 0;

  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;

  TWriteStream* Dest = new TWriteStream(StrmPtr, StreamCoder->OnFlush);

  StreamCoder->Stream->ClearBuffer();
  StreamCoder->SecondaryCodec->Start();
  while (result < Size)
  {
    Symbol = StreamCoder->PPM->UpdateModel(0);
	Dest->Write(Symbol);
    UpdCrc32(CRC, Symbol);
    result++;

    if ((result & DefaultTickStepSize) == 0)
	  if (StreamCoder->OnTick != NULL)
	    if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  StreamCoder->SecondaryCodec->Flush();
  (*Dest).~TWriteStream();
  return result;
}

LIB_API void DestroyCoder(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;

  StreamCoder->PPM->~TBaseCoder();
  StreamCoder->SecondaryCodec->~TSecondaryCodec();
  StreamCoder->Stream->~TStream();

  delete StreamCoder;
}
