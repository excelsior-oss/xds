/*----------------------------------------------------------------------------*/
/*                           DLL files reader                                 */
/*----------------------------------------------------------------------------*/

extern void InitDLLReader  (void);
extern void ClearDLLReader (void);

extern void ReadDLL (byte * rawdata, dword size, const char * filename);

extern void ReadDLLForAutoImageBase (const char * filename);

extern int IsDLL (byte * rawdata);
