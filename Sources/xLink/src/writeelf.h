
extern void WriteELF (char * name);

/* --------------- for DebugInfo writing (HLL4, EDIF) --------------- */

extern dword ELF_getOffset(dword vadr, word object);
extern word  ELF_getObjectNumber(dword vadr);
