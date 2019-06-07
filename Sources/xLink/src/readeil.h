
#include "eil.h"

extern void ReadEIL (byte * rawdata, long size, char * filename);

inline int  IsEIL (byte * rawdata)
{
    return (*rawdata == EIL_SIGNATURE);
}
