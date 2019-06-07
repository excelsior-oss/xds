/*----------------------------------------------------------------------------*/
/*             OMF object files & libraries reader                            */
/*----------------------------------------------------------------------------*/

#include "omf.h"

extern void InitOMF  (void);
extern void ClearOMF (void);

extern void ReadOMF  (byte * rawdata, long size, char * filename);

inline int  IsOMF (byte * rawdata)
{
    return ((*rawdata == THEADR) || (*rawdata == LIBHDR));
}
