
#include <string.h>

#include "xdefs.h"

open_namespace

#include "struct.h"
#include "messages.h"
#include "xmem.h"
#include "xpe.h"

#include "rdntres.h"

/*----------------------------------------------------------------------------*/

static byte * ReadIdOrName (byte * rawdata, word * Id, word ** Str)
{
        int      len;

        * Id  = 0;
        * Str = NULL;
        if (* (word *) rawdata == 0xFFFF) {          /* Next 2 bytes are ID */
                * Id   = *((word *) (rawdata+2));
                return rawdata + 4;
        }
        /* Unicode string */
        len = 0;
        while ( *( ((word *)rawdata) + len) )
                len ++;
        len = (len+1)*2;            /* Include terminating 0x0000, in bytes */
        *Str = (word *) xalloc (len);
        memcpy (*Str, rawdata, len);
        if(len & 3)                 /* Padding */
          len += 2;
        return rawdata + len;
}

/*----------------------------------------------------------------------------*/

static dword ReadOneRes (byte * rawdata, dword size, char * filename)
{
        dword DataSize, HeaderSize;
        word   ResTypeId,    ResNameId;
        word * ResTypeStr, * ResNameStr;
        dword  DataVersion = 0;
        word   MemoryFlags = 0;
        word   LanguageId  = 0;
        dword  Version     = 0;
        dword  Characteristics = 0;

        byte * cur_ptr = rawdata;

        if (size < sizeof (dword) *  2) {
                Message(xERROR, msgILLEGAL_RES_FILE, filename);
                return size;
        }
        DataSize   = * (dword *) cur_ptr;
        cur_ptr   += 4;
        HeaderSize = * (dword *) cur_ptr;
        cur_ptr   += 4;
        if ((size < DataSize + HeaderSize) || (HeaderSize < 32)) {
                Message(xERROR, msgILLEGAL_RES_FILE, filename);
                return size;
        }
        if (DataSize == 0)
                return HeaderSize;
        cur_ptr = ReadIdOrName (cur_ptr, &ResTypeId, &ResTypeStr);
        cur_ptr = ReadIdOrName (cur_ptr, &ResNameId, &ResNameStr);
        DataVersion  = * (dword *) cur_ptr;
        cur_ptr     += 4;
        MemoryFlags  = * (word  *) cur_ptr;
        cur_ptr     += 2;
        LanguageId   = * (word  *) cur_ptr;
        cur_ptr     += 2;
        Version      = * (dword *) cur_ptr;
        cur_ptr     += 4;
        Characteristics = * (dword *) cur_ptr;
        cur_ptr     += 4;
        if( (dword)(cur_ptr - rawdata) != HeaderSize ) {
                Message(xERROR, msgILLEGAL_RES_FILE, filename);
                return size;
        }

        ResourceDirectory * rd = getResourceDirectory (ResTypeStr, ResTypeId);
        if (! rd->addResource (ResNameStr, ResNameId, DataSize, cur_ptr, DataVersion, MemoryFlags, LanguageId, Version, Characteristics, RES_OS_TYPE_NT))
            Message(xFATAL, msgDUP_RES_NAME, filename);

        return (cur_ptr + DataSize) - rawdata;
}

/*----------------------------------------------------------------------------*/
/*                    Win32 Resource file (.RES) reader                       */
/*----------------------------------------------------------------------------*/

void ReadNTResource (byte * rawdata, dword size, char * filename)
{
        dword res_size;

        do {
                res_size = ReadOneRes (rawdata, size, filename);
                res_size = (res_size + 3) & ~ 3;
                rawdata += res_size;
                size    -= res_size;
        } while (size > 0);
}

/*----------------------------------------------------------------------------*/
/*                    Icon Resource file (.ICO) reader                        */
/*----------------------------------------------------------------------------*/

#define ICON_RESOURCE_TYPE        0x0003
#define GROUP_ICON_RESOURCE_TYPE  0x000E
#define RES_ICON                  1

struct NEWHEADER { 
    WORD Reserved; 
    WORD ResType; 
    WORD ResCount; 
};

struct RESDIR { 
    BYTE  Width;
    BYTE  Height;
    BYTE  ColorCount;
    BYTE  reserved;

    WORD  Planes;
    WORD  BitCount;
    DWORD BytesInRes;
}; 

struct TARGET_RESDIR {
    struct RESDIR r;
    WORD          IconId;
};


struct SOURCE_RESDIR {
    struct RESDIR r;
    DWORD         rawIconOffset;
};

static word IconNumber = 1;
static word GroupIconNumber = 1;

void ReadICOResource (byte * rawdata, dword size, char *filename)
{
    struct NEWHEADER * header = (struct NEWHEADER *)rawdata;
    if ( (size < sizeof (struct NEWHEADER)) ||
         (header->ResType != RES_ICON) || (header->ResCount == 0) ) {
        Message(xERROR, msgILLEGAL_FILE_FORMAT, filename);
        return;
    }

    struct SOURCE_RESDIR * sresdir = (struct SOURCE_RESDIR *) (rawdata + sizeof(struct NEWHEADER));

    if (size < sizeof (struct NEWHEADER) + sizeof (struct RESDIR) * header->ResCount) {
        Message(xERROR, msgILLEGAL_FILE_FORMAT, filename);
        return;
    }

    byte * resourceData = (byte *) xalloc ( sizeof(struct NEWHEADER) + sizeof (struct TARGET_RESDIR) * header->ResCount );
    memcpy (resourceData, header, sizeof(struct NEWHEADER));
    struct TARGET_RESDIR * tresdir = (struct TARGET_RESDIR *) (resourceData + sizeof(struct NEWHEADER));

    ResourceDirectory * rd = getResourceDirectory (NULL, ICON_RESOURCE_TYPE);
    for (int i = 0; i < header->ResCount; i++) {
        rd->addResource (NULL, IconNumber, sresdir[i].r.BytesInRes, rawdata + sresdir[i].rawIconOffset, 0, 0, 0, 0, 0, RES_OS_TYPE_NT);
        memcpy (tresdir + i, sresdir + i, sizeof (struct TARGET_RESDIR));
        tresdir [i].IconId = IconNumber++;
    }
    rd = getResourceDirectory (NULL, GROUP_ICON_RESOURCE_TYPE);

    rd->addResource (NULL, GroupIconNumber++, sizeof (struct NEWHEADER) + sizeof (struct TARGET_RESDIR) * header->ResCount,
                     resourceData, 0, 0, 0, 0, 0, RES_OS_TYPE_NT);
}

close_namespace

