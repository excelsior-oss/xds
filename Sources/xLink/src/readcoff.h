/*----------------------------------------------------------------------------*/
/*             COFF object files & libraries reader                           */
/*----------------------------------------------------------------------------*/

#include "xpe.h"

extern void InitCOFF        (void);
extern void ClearCOFF       (void);

extern void ReadCOFF        (byte * rawdata, long size, char * filename);
extern void ReadCOFFLibrary (byte * rawdata, long size, char * libname);

inline int  IsCOFF (byte * rawdata)
{
    return  (((PIMAGE_FILE_HEADER) rawdata) -> Machine == IMAGE_FILE_MACHINE_I386) &&
            (((PIMAGE_FILE_HEADER) rawdata) -> SizeOfOptionalHeader == 0);
}

inline int  IsCOFFLibrary (byte * rawdata)
{
    return (! memcmp (rawdata, IMAGE_ARCHIVE_START, IMAGE_ARCHIVE_START_SIZE));
}
