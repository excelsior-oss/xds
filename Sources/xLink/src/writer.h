
extern dword   StubSize;
extern byte  * Stub;

extern const char * StrippedOutputFileName;

extern void GetStub();
extern void WriteOutFile();

extern dword getOffset(dword vadr, word object);
extern word getObjectNumber(dword vadr);
