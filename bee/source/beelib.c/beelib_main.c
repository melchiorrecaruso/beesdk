#include "beelib_main.h"

#include "beelib_crc.h"
#include "beelib_codec.h"
#include "beelib_types.h"
#include "beelib_stream.h"
#include "beelib_modeller.h"
#include "beelib_assembler.h"

unsigned int DllVersion()
{
  return 110;
};

/* TStreamEncoder struct/methods implementation */

struct TStreamEncoder {
       PWriteStream Stream;
  PSecondaryEncoder SecondaryEncoder;
         PBaseCoder BaseCoder;
};

PStreamEncoder StreamEncoder_Malloc(PStream aStream, PFillBuffer aFillBuffer, PFlushBuffer aFlushBuffer)
{
  result = malloc(sizeof(struct TStreamEncoder));
  WriteStream_Initialize(result->Stream)


  result->Stream



  result->Stream = aStream;
  result->

  return result;
};


PStreamDecoder StreamDecoder_Malloc(PReadStream aStream, PStreamFill aStreamFill, PStreamFlush aStreamFlush)
{
  return malloc(sizeof(struct TStreamDecoder));
};



           void StreamEncoder_Free              (PStreamEncoder Self);
           void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, unsigned int Value);
           void StreamEncoder_SetTableParameters(PStreamEncoder Self, TTableParameters *Value);
           void StreamEncoder_FreshFlexible     (PStreamEncoder Self);
           void StreamEncoder_FreshSolid        (PStreamEncoder Self);
  long long int StreamEncoder_Encode            (PStreamEncoder Self, PStream aStream, long long int Size, unsigned int *CRC);


void StreamEncoder_SetDictionaryLevel(PStreamEncoder Self, signed int Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
  return;
};


/* TStreamDecoder struct/methods implementation */

struct TStreamDecoder {
        PReadStream Stream;
       PStreamFlush StreamFlush;
  PSecondaryDecoder SecondaryDecoder;
         PBaseCoder BaseCoder;
};

void StreamDecoder_SetDictionaryLevel(PStreamDecoder Self, signed int Value)
{
  BaseCoder_SetDictionary(Self->BaseCoder, Value);
  return;
};

























void SetTableParameters(void* Handle, const TTableParameters& Value)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->SetTable(Value);
  return;
};

void FreshFlexible(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->FreshFlexible();
  return;
};

void FreshSolid(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;
  (*StreamCoder).PPM->FreshSolid();
  return;
};

void* CreateEncoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv)
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
};

int64 Encode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC)
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
      if (StreamCoder->OnTick != 0)
	    if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  StreamCoder->SecondaryCodec->Flush();
  StreamCoder->Stream->FlushBuffer();
  (*Source).~TReadStream();

  return result;
};

void* CreateDecoder(void* StrmPtr, TFillEvent OnFillEv, TFlushEvent OnFlushEv, void* TickPtr, TTickEvent OnTickEv)
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
};

int64 Decode(void* Handle, void* StrmPtr, const int64 Size, unsigned int& CRC)
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
	  if (StreamCoder->OnTick != 0)
	    if (StreamCoder->OnTick(StreamCoder->Tick)) break;
  }
  StreamCoder->SecondaryCodec->Flush();
  (*Dest).~TWriteStream();
  return result;
};

void DestroyCoder(void* Handle)
{
  TStreamCoder* StreamCoder = (TStreamCoder*) Handle;

  StreamCoder->PPM->~TBaseCoder();
  StreamCoder->SecondaryCodec->~TSecondaryCodec();
  StreamCoder->Stream->~TStream();

  delete StreamCoder;
  return;
};