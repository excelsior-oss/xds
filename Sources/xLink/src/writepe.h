
#define DefaultPEStubSize     128
extern byte DefaultPEStub [DefaultPEStubSize];

extern void WritePE (char * name);

/* --------------- for DebugInfo writing (HLL4, EDIF) --------------- */

extern dword PE_getOffset(dword vadr, word object);
extern word  PE_getObjectNumber(dword vadr);
