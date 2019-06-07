#include <string.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "messages.h"
#include "xdebug.h"
#include "xmem.h"

#include "rdos2res.h"

/*----------------------------------------------------------------------------*/

static byte * ReadIdOrName (byte * rawdata, word * Id, char ** Str)
{
        int      len;

        * Id  = 0;
        * Str = NULL;
        if (*rawdata == 0xFF) {          /* Next 2 bytes are ID */
                * Id   = *((word *) (rawdata+1));
                return rawdata + 3;
        }
        /* ASCIIZ string */
        len = 0;
        while ( *(rawdata + len) )
                len ++;
        len++;             /* Include terminating 0x00 */
        *Str = (char *) xalloc (len);
        memcpy (*Str, rawdata, len);
        return rawdata + len;
}

/*----------------------------------------------------------------------------*/

static dword ReadOneResource (byte * rawdata, char * filename) {
        dword  DataSize;
        word   ResTypeId,    ResNameId;
        char * ResTypeStr, * ResNameStr;
        word   MemoryFlags = 0;

        byte * cur_ptr = rawdata;

        cur_ptr = ReadIdOrName (cur_ptr, &ResTypeId, &ResTypeStr);
        cur_ptr = ReadIdOrName (cur_ptr, &ResNameId, &ResNameStr);
        /* Question: why in OS/2 resources ASCII type/name is error? */
        if(ResTypeStr || ResNameStr)
                Message(xFATAL, msgINVALID_FILE_FORMAT, filename);
        /* --------------------------------------------------------- */
        MemoryFlags  = * (word  *) cur_ptr;
        cur_ptr     += 2;
        DataSize     = * (dword *) cur_ptr;
        cur_ptr     += 4;

        ResourceDirectory * rd = getResourceDirectory (NULL, ResTypeId);
        if (! rd->addResource (NULL, ResNameId, DataSize, cur_ptr, 0, MemoryFlags, 0, 0, 0, RES_OS_TYPE_OS2))
            Message(xFATAL, msgDUP_RES_NAME, filename);

        return (cur_ptr + DataSize) - rawdata;
}

/*----------------------------------------------------------------------------*/

void ReadOS2Resource (byte * rawdata, dword size, char * filename)
{
    dword res_size;
    do {
        res_size = ReadOneResource (rawdata, filename);
        rawdata += res_size;
        size    -= res_size;
    } while (size > 0);
}

close_namespace

