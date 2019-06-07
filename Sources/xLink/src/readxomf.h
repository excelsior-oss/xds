/*----------------------------------------------------------------------------*/
/*             XOMF object files & libraries reader                           */
/*----------------------------------------------------------------------------*/

#include "xomf.h"

extern void InitXOMF  (void);
extern void ClearXOMF (void);

extern void ReadXOMF  (byte * rawdata, long size, char * filename);

inline int  IsXOMF (byte * rawdata)
{
    return (*((dword *)rawdata) == XOMF_SIGNATURE) || (*rawdata == XOMF_THEADR);
}

inline int  IsOldXOMF (byte * rawdata)
{
    return (*rawdata == OLD_XOMF_THEADR) || (*rawdata == XOMF_LIBHDR);
}
