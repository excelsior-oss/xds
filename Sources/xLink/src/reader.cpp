#include <stdio.h>
#include <ctype.h>

#include <vector>

#include "xdefs.h"

open_namespace

#include "xos.h"

#include "messages.h"
#include "xmem.h"
#include "args.h"
#include "idents.h"

#include "reader.h"

/* Readers */
#include "omf.h"
#include "readomf.h"
#include "readxomf.h"
#include "readeil.h"
#include "readcoff.h"
#include "readdll.h"
#include "rdntres.h"
#include "rdos2res.h"
#include "readelf.h"

#ifdef ENABLE_PACKEDDATA_INPUT
#include "res/packed_data.h"
#endif
#include "zlib/zlib.h"
#include "minizip/unzip.h"

/*----------------------------------------------------------------------------*/

static Bool IsExtension (char * filename, char *extension)
{
    char * ext = strrchr (filename, '.');
    return ext && !strcmpIC (ext, extension);
}

/*----------------------------------------------------------------------------*/

void ReadRawData (byte * rawdata, long size, char * filename)
{
    if      ( IsXOMF(rawdata) )
                ReadXOMF(rawdata, size, filename);
    else if ( IsOMF(rawdata) )
                ReadOMF(rawdata, size, filename);
    else if ( IsEIL(rawdata) )
                ReadEIL(rawdata, size, filename);
    else if ( IsDLL(rawdata) )
                ReadDLL(rawdata, size, filename);
    else if ( IsCOFF(rawdata) )
                ReadCOFF(rawdata, size, filename);
    else if ( IsCOFFLibrary(rawdata) )
                ReadCOFFLibrary(rawdata, size, filename);
    else if ( IsELF(rawdata, size) )
                ReadELF(rawdata, size, filename);
    else if (IsExtension (filename, ".RES")) {
            if (! memcmp (rawdata, "\0\0\0\0\x20\0\0\0", 8)) {
               ReadNTResource (rawdata, size, filename);
            }else{
               ReadOS2Resource (rawdata, size, filename);
            }
    } else if (IsExtension (filename, ".ICO")) {
         ReadICOResource (rawdata, size, filename);
    } else if (IsOldXOMF (rawdata)) {
         Message(xERROR, msgFILE_FORMAT_OUTDATED, filename);
    } else
         Message(xERROR, msgILLEGAL_FILE_FORMAT, filename);
}

//----------------------------------------------------------------------------

Bool IsZIP(char * filename)
{
    return IsExtension(filename,".ZIP");
}

Bool ZipEntryIsFile(char * filename, uLong size)
{
    ASSERT(size > 0);
    return (filename[size-1] != '/');
}

//----------------------------------------------------------------------------

void ReadZipFile (const char * filename)
{
    int res = 0;
    unz_file_info pfile_info;
    char *szFileName = 0;
    uLong fileNameBufferSize = 0;
    unz_global_info global_info;
    std::vector<char> buffer(65536);

    VerboseMessage (INFO_ZIPREAD, "[ZIP] Reading zip file %s\n", filename);

    unzFile zipFile = unzOpen (OS->GetFullPath(filename));
    if (zipFile == NULL) {
        VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzOpen() failed\n", filename);
        Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
        return;
    }

    res = unzGetGlobalInfo(zipFile, &global_info);
    if (res != UNZ_OK) {
        VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGetGlobalInfo() failed with code %d\n", filename, res);
        Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
        return;
    }

    if (global_info.number_entry > 0) {
        int getFileResult = unzGoToFirstFile(zipFile);
        if (getFileResult != UNZ_OK) {
            VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGoToFirstFile() failed with code %d\n", filename, getFileResult);
            Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
            return;
        }

        do {
            res = unzOpenCurrentFile (zipFile);
            if (res != UNZ_OK) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzOpenCurrentFile() failed with code %d\n", filename, res);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            res = unzGetCurrentFileInfo (zipFile,&pfile_info,0,0,0,0,0,0);
            if (res != UNZ_OK) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGetCurrentFileInfo() failed with code %d\n", filename, res);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            if (pfile_info.size_filename == 0) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGetCurrentFileInfo() failed, pfile_info.size_filename=0\n", filename);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            fileNameBufferSize = pfile_info.size_filename+1;
            szFileName = new char[fileNameBufferSize];
            memset (szFileName, 0, fileNameBufferSize);

            res = unzGetCurrentFileInfo (zipFile,&pfile_info,szFileName,fileNameBufferSize,0,0,0,0);
            if (res != UNZ_OK) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGetCurrentFileInfo() failed with code %d\n", filename, res);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            if (ZipEntryIsFile(szFileName,fileNameBufferSize-1)) {
                unsigned length = pfile_info.uncompressed_size;
                if (length > buffer.size()) {
                    buffer.clear();
                    buffer.resize(length);
                }
                memset (&buffer[0], 0, length);
                res = unzReadCurrentFile (zipFile, (voidp)&buffer[0], length);
                if (res != length) {
                    VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzReadCurrentFile() failed on entry %s\n", filename, szFileName);
                    Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                    return;
                }
                ReadRawData((byte*)&buffer[0], length, szFileName);
            }

            res = unzCloseCurrentFile (zipFile);
            if (res != UNZ_OK) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzCloseCurrentFile() failed with code %d\n", filename, res);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            getFileResult = unzGoToNextFile(zipFile);
            if ((getFileResult != UNZ_OK) && (getFileResult != UNZ_END_OF_LIST_OF_FILE)) {
                VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzGoToNextFile() failed with code %d\n", filename, getFileResult);
                Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
                return;
            }

            delete[] szFileName;

        } while (getFileResult != UNZ_END_OF_LIST_OF_FILE);
    }

    res = unzClose (zipFile);
    if (res != UNZ_OK) {
        VerboseMessage (INFO_ZIPREAD, "[ZIP] %s: unzClose() failed with code %d\n", filename, res);
        Message (xERROR, msgUNABLE_TO_READ_FILE, filename);
        return;
    }
}

//----------------------------------------------------------------------------

void ReadFileParameter (char * filename)
{
#if defined (DEBUG)
    fputc ('.', stderr);
    fflush (stderr);
#endif

    if (!IsZIP(filename)) {
        OSFile * file = OS -> File ();

        byte * rawdata = NULL;
        unsigned long size = 0;

        if (! file -> OpenRead (filename, rawdata, size))
            return;

        ReadRawData (rawdata, size, filename);

        delete file;
    } else {
        ReadZipFile(filename);
    }
}

#ifdef ENABLE_PACKEDDATA_INPUT

void ReadResourceParameter (char * resourcename)
{
    PackedFile* f = &(PackedData[0]);

    while (f->name != 0) {
        if (!strcmp(f->name, resourcename + 1))
        {
            ASSERT (f -> compressed_data != 0);
            unsigned long uncompressed_size = f -> uncompressed_len;

            // uncompress

            byte* uncompressed_data = (byte *) xalloc (uncompressed_size);
            memset (uncompressed_data, 0, uncompressed_size);

            int err = uncompress (uncompressed_data,
                                  &uncompressed_size,
                                  (const Bytef*) (f -> compressed_data),
                                  f -> compressed_len);

            if (err != Z_OK) {
                Message (xERROR, msgUNABLE_LOAD_RESOURCE, resourcename, err + 16000);
                return;
            }

            ReadRawData (uncompressed_data, uncompressed_size, resourcename + 1);

            xfree (uncompressed_data);

            return;
        }
        f += 1;
    }
    Message (xERROR, msgUNABLE_LOAD_RESOURCE, resourcename, 0);
}

#else

void ReadResourceParameter (char * resourcename) {
    Message (xERROR, msgUNABLE_LOAD_RESOURCE, resourcename, -1);
}

#endif

void InitializeReaders()
{
    InitOMF  ();
    InitXOMF ();
    InitCOFF ();
    InitDLLReader ();
    InitELFReader ();
}

void FinalizeReaders()
{
    ClearOMF  ();
    ClearXOMF ();
    ClearCOFF ();
    ClearDLLReader ();
    ClearELFReader ();
}

close_namespace

