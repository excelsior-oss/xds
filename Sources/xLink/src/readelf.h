/*----------------------------------------------------------------------------*/
/*                           ELF files reader                                 */
/*----------------------------------------------------------------------------*/

extern void InitELFReader  (void);
extern void ClearELFReader (void);

extern void ReadELF (byte * rawdata, dword size, char * filename);

extern Bool IsELF (byte * rawdata, dword size);
