
#define DefaultLXStubSize     128
extern byte DefaultLXStub [DefaultLXStubSize];

extern void WriteLX (char * name, int debug, int dll);

/* --------------- for DebugInfo writing (HLL4, EDIF) --------------- */

extern dword LX_getOffset(dword vadr, word object);
extern word  LX_getObjectNumber(dword vadr);
